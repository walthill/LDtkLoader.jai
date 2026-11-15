#import "Basic";
#import "File";
#import "String";

#scope_module

WHITESPACE_CHARS :: " \t\n\r";

LDtk_Json_Value :: struct {
    type: LDtk_Json_Type;
    data: *void;
}

LDtk_Json_Type :: enum {
    NULL;
    BOOL;
    NUMBER;
    STRING;
    ARRAY;
    OBJECT;
}

LDtk_Json_Object :: struct {
    keys: [..] string;
    values: [..] LDtk_Json_Value;
}

LDtk_Json_Array :: struct {
    items: [..] LDtk_Json_Value;
}

ldtk_json_parse_string :: (content: string) -> success: bool, value: LDtk_Json_Value {
    if !content return false, LDtk_Json_Value.{type = .NULL};
    rem, suc, val := ldtk_parse_value(content);
    if !suc return false, LDtk_Json_Value.{type = .NULL};
    rem = trim_left(rem, WHITESPACE_CHARS);
    return true, val;
}

ldtk_parse_value :: (str: string) -> remainder: string, success: bool, value: LDtk_Json_Value {
    trimmed := trim_left(str, WHITESPACE_CHARS);
    if !trimmed return str, false, LDtk_Json_Value.{type = .NULL};

    first_char := trimmed[0];

    if first_char == #char "{" {
        rem, suc, val := ldtk_parse_object(trimmed);
        return rem, suc, val;
    } else if first_char == #char "[" {
        rem, suc, val := ldtk_parse_array(trimmed);
        return rem, suc, val;
    } else if first_char == #char "\"" {
        res, rem, suc := ldtk_parse_string_value(trimmed);
        if !suc return str, false, LDtk_Json_Value.{type = .NULL};
        str_ptr := cast(*string)alloc(size_of(string));
        Initialize(str_ptr);
        str_ptr.* = res;
        value := LDtk_Json_Value.{type = .STRING, data = cast(*void)str_ptr};
        return rem, true, value;
    } else if first_char == #char "t" && starts_with(trimmed, "true") {
        bool_ptr := cast(*bool)alloc(size_of(bool));
        Initialize(bool_ptr);
        bool_ptr.* = true;
        value := LDtk_Json_Value.{type = .BOOL, data = cast(*void)bool_ptr};
        return advance(trimmed, 4), true, value;
    } else if first_char == #char "f" && starts_with(trimmed, "false") {
        bool_ptr := cast(*bool)alloc(size_of(bool));
        Initialize(bool_ptr);
        bool_ptr.* = false;
        value := LDtk_Json_Value.{type = .BOOL, data = cast(*void)bool_ptr};
        return advance(trimmed, 5), true, value;
    } else if first_char == #char "n" && starts_with(trimmed, "null") {
        value := LDtk_Json_Value.{type = .NULL};
        return advance(trimmed, 4), true, value;
    } else if first_char == #char "-" || (first_char >= #char "0" && first_char <= #char "9") {
        rem, suc, val := ldtk_parse_number(trimmed);
        return rem, suc, val;
    }

    return str, false, LDtk_Json_Value.{type = .NULL};
}

ldtk_parse_string_value :: (str: string) -> string, string, bool {
    if str[0] != #char "\"" return "", str, false;
    inside := advance(str);
    if !inside return "", str, false;

    needs_unescape := false;
    while inside[0] != #char "\"" {
        if inside.count < 2 return "", str, false;
        if inside[0] == #char "\\" {
            needs_unescape = true;
            advance(*inside);
        }
        advance(*inside);
    }

    length := inside.data - str.data - 1;
    res := slice(str, 1, length);

    if needs_unescape {
        unescaped, suc := ldtk_unescape_string(res);
        if !suc return "", str, false;
        res = unescaped;
    } else {
        res = copy_string(res);
    }

    rem := slice(str, length + 2, str.count - length - 2);
    return res, rem, true;
}

ldtk_unescape_string :: (str: string) -> string, bool {
    res := alloc_string(str.count);
    rc := 0;
    suc := true;
    for i: 0..str.count-1 {
        if str[i] != #char "\\" {
            if str[i] < 0x20 {
                suc = false;
                break;
            }
            res[rc] = str[i];
            rc += 1;
        } else {
            if i == str.count - 1 {
                suc = false;
                break;
            }
            i += 1;
            if str[i] == {
                case #char "\""; #through;
                case #char "/"; #through;
                case #char "\\";
                    res[rc] = str[i];
                    rc += 1;
                case #char "n";
                    res[rc] = #char "\n";
                    rc += 1;
                case #char "r";
                    res[rc] = #char "\r";
                    rc += 1;
                case #char "t";
                    res[rc] = #char "\t";
                    rc += 1;
                case;
                    suc = false;
                    break;
            }
        }
    }
    if !suc {
        free(res);
        return "", false;
    }
    res.count = rc;
    return res, true;
}

ldtk_parse_number :: (str: string) -> string, bool, LDtk_Json_Value {
    end := 0;
    while end < str.count && ((str[end] >= #char "0" && str[end] <= #char "9") || str[end] == #char "." || str[end] == #char "-" || str[end] == #char "e" || str[end] == #char "E" || str[end] == #char "+") {
        end += 1;
    }

    num_str := slice(str, 0, end);
    num_value: float64 = 0;

    negative := false;
    i := 0;
    if i < num_str.count && num_str[i] == #char "-" {
        negative = true;
        i += 1;
    }

    integer_part := 0;
    while i < num_str.count && num_str[i] >= #char "0" && num_str[i] <= #char "9" {
        integer_part = integer_part * 10 + (num_str[i] - #char "0");
        i += 1;
    }

    num_value = cast(float64)integer_part;

    if i < num_str.count && num_str[i] == #char "." {
        i += 1;
        fraction := 0.0;
        divisor := 1.0;
        while i < num_str.count && num_str[i] >= #char "0" && num_str[i] <= #char "9" {
            fraction = fraction * 10 + (num_str[i] - #char "0");
            divisor *= 10.0;
            i += 1;
        }
        num_value += fraction / divisor;
    }

    if negative num_value = -num_value;

    if i < num_str.count && (num_str[i] == #char "e" || num_str[i] == #char "E") {
        i += 1;
        exp_negative := false;
        if i < num_str.count && (num_str[i] == #char "+" || num_str[i] == #char "-") {
            if num_str[i] == #char "-" exp_negative = true;
            i += 1;
        }
        exp := 0;
        while i < num_str.count && num_str[i] >= #char "0" && num_str[i] <= #char "9" {
            exp = exp * 10 + (num_str[i] - #char "0");
            i += 1;
        }
        if exp_negative {
            for 0..exp-1 num_value /= 10.0;
        } else {
            for 0..exp-1 num_value *= 10.0;
        }
    }

    num_ptr := cast(*float64)alloc(size_of(float64));
    Initialize(num_ptr);
    num_ptr.* = num_value;

    value := LDtk_Json_Value.{type = .NUMBER, data = cast(*void)num_ptr};
    rem := slice(str, end, str.count - end);
    return rem, true, value;
}

ldtk_parse_object :: (str: string) -> string, bool, LDtk_Json_Value {
    if str[0] != #char "{" return str, false, LDtk_Json_Value.{type = .NULL};
    inside := advance(str);

    obj_ptr := cast(*LDtk_Json_Object)alloc(size_of(LDtk_Json_Object));
    Initialize(obj_ptr);
    suc := true;
    defer if !suc {
        for i: 0..obj_ptr.keys.count-1 free(obj_ptr.keys[i]);
        for i: 0..obj_ptr.values.count-1 ldtk_free_json_value(obj_ptr.values[i]);
        array_free(obj_ptr.keys);
        array_free(obj_ptr.values);
        free(obj_ptr);
    };

    inside = trim_left(inside, WHITESPACE_CHARS);

    if inside.count > 0 && inside[0] == #char "}" {
        value := LDtk_Json_Value.{type = .OBJECT, data = cast(*void)obj_ptr};
        return advance(inside), true, value;
    }

    while true {
        inside = trim_left(inside, WHITESPACE_CHARS);

        key, new_inside, key_success := ldtk_parse_string_value(inside);
        if !key_success {
            suc = false;
            return str, false, LDtk_Json_Value.{type = .NULL};
        }
        inside = new_inside;

        inside = trim_left(inside, WHITESPACE_CHARS);
        if inside.count < 1 || inside[0] != #char ":" {
            suc = false;
            return str, false, LDtk_Json_Value.{type = .NULL};
        }
        inside = advance(inside);

        p_rem, p_suc, val_result := ldtk_parse_value(inside);
        if !p_suc {
            suc = false;
            return str, false, LDtk_Json_Value.{type = .NULL};
        }
        inside = p_rem;

        array_add(*obj_ptr.keys, key);
        array_add(*obj_ptr.values, val_result);

        inside = trim_left(inside, WHITESPACE_CHARS);

        if inside.count > 0 && inside[0] == #char "," {
            inside = advance(inside);
            continue;
        }

        if inside.count > 0 && inside[0] == #char "}" {
            inside = advance(inside);
            break;
        }

        suc = false;
        return str, false, LDtk_Json_Value.{type = .NULL};
    }

    value := LDtk_Json_Value.{type = .OBJECT, data = cast(*void)obj_ptr};
    return inside, true, value;
}

ldtk_parse_array :: (str: string) -> string, bool, LDtk_Json_Value {
    if str[0] != #char "[" return str, false, LDtk_Json_Value.{type = .NULL};
    inside := advance(str);

    arr_ptr := cast(*LDtk_Json_Array)alloc(size_of(LDtk_Json_Array));
    Initialize(arr_ptr);
    suc := true;
    defer if !suc {
        for i: 0..arr_ptr.items.count-1 ldtk_free_json_value(arr_ptr.items[i]);
        array_free(arr_ptr.items);
        free(arr_ptr);
    };

    inside = trim_left(inside, WHITESPACE_CHARS);

    if inside.count > 0 && inside[0] == #char "]" {
        value := LDtk_Json_Value.{type = .ARRAY, data = cast(*void)arr_ptr};
        return advance(inside), true, value;
    }

    while true {
        p_rem, p_suc, val_result := ldtk_parse_value(inside);
        if !p_suc {
            suc = false;
            return str, false, LDtk_Json_Value.{type = .NULL};
        }
        inside = p_rem;

        array_add(*arr_ptr.items, val_result);

        inside = trim_left(inside, WHITESPACE_CHARS);

        if inside.count > 0 && inside[0] == #char "," {
            inside = advance(inside);
            continue;
        }

        if inside.count > 0 && inside[0] == #char "]" {
            inside = advance(inside);
            break;
        }

        suc = false;
        return str, false, LDtk_Json_Value.{type = .NULL};
    }

    value := LDtk_Json_Value.{type = .ARRAY, data = cast(*void)arr_ptr};
    return inside, true, value;
}

ldtk_free_json_value :: (val: LDtk_Json_Value) {
    if val.type == {
        case .STRING;
            str_ptr := cast(*string)val.data;
            free(str_ptr.data);
            free(str_ptr);
        case .OBJECT;
            obj := cast(*LDtk_Json_Object)val.data;
            for i: 0..obj.keys.count-1 free(obj.keys[i]);
            for i: 0..obj.values.count-1 ldtk_free_json_value(obj.values[i]);
            array_free(obj.keys);
            array_free(obj.values);
            free(obj);
        case .ARRAY;
            arr := cast(*LDtk_Json_Array)val.data;
            for i: 0..arr.items.count-1 ldtk_free_json_value(arr.items[i]);
            array_free(arr.items);
            free(arr);
        case .NUMBER;
            num := cast(*float64)val.data;
            free(num);
        case .BOOL;
            bool_ptr := cast(*bool)val.data;
            free(bool_ptr);
        case .NULL;
    }
}

ldtk_json_object :: (val: LDtk_Json_Value) -> *LDtk_Json_Object {
    if val.type != .OBJECT return null;
    return cast(*LDtk_Json_Object)val.data;
}

ldtk_json_array :: (val: LDtk_Json_Value) -> *LDtk_Json_Array {
    if val.type != .ARRAY return null;
    return cast(*LDtk_Json_Array)val.data;
}

ldtk_json_object_get :: (obj: *LDtk_Json_Object, key: string) -> LDtk_Json_Value {
    for i: 0..obj.keys.count-1 {
        if equal(obj.keys[i], key) {
            return obj.values[i];
        }
    }
    return LDtk_Json_Value.{type = .NULL};
}

ldtk_json_object_get_string :: (obj: *LDtk_Json_Object, key: string) -> string {
    val := ldtk_json_object_get(obj, key);
    if val.type != .STRING return "";
    return (cast(*string)val.data).*;
}

ldtk_json_object_get_number :: (obj: *LDtk_Json_Object, key: string) -> float64 {
    val := ldtk_json_object_get(obj, key);
    if val.type != .NUMBER return 0.0;
    return (cast(*float64)val.data).*;
}

ldtk_json_object_get_bool :: (obj: *LDtk_Json_Object, key: string) -> bool {
    val := ldtk_json_object_get(obj, key);
    if val.type != .BOOL return false;
    return (cast(*bool)val.data).*;
}

ldtk_json_array_get_count :: (arr: *LDtk_Json_Array) -> s64 {
    return arr.items.count;
}

ldtk_json_array_get :: (arr: *LDtk_Json_Array, index: s64) -> LDtk_Json_Value {
    if index < 0 || index >= arr.items.count return LDtk_Json_Value.{type = .NULL};
    return arr.items[index];
}

ldtk_json_array_get_object :: (arr: *LDtk_Json_Array, index: s64) -> *LDtk_Json_Object {
    val := ldtk_json_array_get(arr, index);
    if val.type != .OBJECT return null;
    return cast(*LDtk_Json_Object)val.data;
}

ldtk_json_array_get_number :: (arr: *LDtk_Json_Array, index: s64) -> float64 {
    val := ldtk_json_array_get(arr, index);
    if val.type != .NUMBER return 0.0;
    return (cast(*float64)val.data).*;
}

ldtk_json_array_get_string :: (arr: *LDtk_Json_Array, index: s64) -> string {
    val := ldtk_json_array_get(arr, index);
    if val.type != .STRING return "";
    return (cast(*string)val.data).*;
}

ldtk_json_array_get_bool :: (arr: *LDtk_Json_Array, index: s64) -> bool {
    val := ldtk_json_array_get(arr, index);
    if val.type != .BOOL return false;
    return (cast(*bool)val.data).*;
}

ldtk_json_object_dotget :: (obj: *LDtk_Json_Object, path: string) -> LDtk_Json_Value {
    parts := split(path, ".");
    defer array_free(parts);

    current := LDtk_Json_Value.{type = .OBJECT, data = cast(*void)obj};
    for part: parts {
        if current.type != .OBJECT return LDtk_Json_Value.{type = .NULL};
        cur_obj := cast(*LDtk_Json_Object)current.data;
        current = ldtk_json_object_get(cur_obj, part);
    }
    return current;
}

ldtk_json_object_dotget_array :: (obj: *LDtk_Json_Object, path: string) -> *LDtk_Json_Array {
    val := ldtk_json_object_dotget(obj, path);
    if val.type != .ARRAY return null;
    return cast(*LDtk_Json_Array)val.data;
}

// ============================================================
// LDtk Data Structures
// ============================================================

LDtk_Tileset :: struct {
    identifier: string;
    uid: int;
    relPath: string;
    pxWid: int;
    pxHei: int;
    tileGridSize: int;
    spacing: int;
    padding: int;
}

LDtk_Enum_Detail :: struct {
    id: string;
    tileId: int;
    tileSrcRect: [4] int;
}

LDtk_Enum :: struct {
    identifier: string;
    uid: int;
    iconTilesetUid: int;
    details: [..] LDtk_Enum_Detail;
}

LDtk_IntGrid :: struct {
    count: int;
    coordId: int;
    v: int;
}

LDtk_AutoTile :: struct {
    count: int;
    x: int;
    y: int;
    srcX: int;
    srcY: int;
    f: int;
}

LDtk_GridTile :: struct {
    count: int;
    x: int;
    y: int;
    srcX: int;
    srcY: int;
    f: int;
}

LDtk_FieldInstance_Points :: struct {
    cx: int;
    cy: int;
    check: u8;
    size: int;
}

LDtk_FieldInstance_Floats :: struct {
    floatValue: float32;
    check: u8;
    size: int;
}

LDtk_FieldInstance_Ints :: struct {
    intValue: int;
    check: u8;
    size: int;
}

LDtk_FieldInstance_Strings :: struct {
    strValue: string;
    check: u8;
    size: int;
}

LDtk_FieldInstance_Tiles :: struct {
    tileId: int;
    srcX: int;
    srcY: int;
    width: int;
    height: int;
}

LDtk_FieldInstance :: struct {
    identifier: string;
    type: string;
    points: [..] LDtk_FieldInstance_Points;
    floats: [..] LDtk_FieldInstance_Floats;
    ints: [..] LDtk_FieldInstance_Ints;
    strings: [..] LDtk_FieldInstance_Strings;
    tiles: [..] LDtk_FieldInstance_Tiles;
    defUid: int;
    size: int;
}

LDtk_EntityInstance :: struct {
    identifier: string;
    cx: int;
    cy: int;
    x: int;
    y: int;
    defUid: int;
    fieldInstanceCount: int;
    fieldInstances: [..] LDtk_FieldInstance;
}

LDtk_LayerInstance :: struct {
    identifier: string;
    type: string;
    cWid: int;
    cHei: int;
    gridSize: int;
    levelId: int;
    layerDefUid: int;
    pxOffsetX: int;
    pxOffsetY: int;
    autoTiles: [..] LDtk_AutoTile;
    gridTiles: [..] LDtk_GridTile;
    entityInstances: [..] LDtk_EntityInstance;
    intGrid: [..] LDtk_IntGrid;
    intGridCsv: [..] int;
}

LDtk_Level_Neighbor :: struct {
    dir: u8;
    uid: int;
}

LDtk_Level :: struct {
    identifier: string;
    uid: int;
    pxWid: int;
    pxHei: int;
    worldX: int;
    worldY: int;
    firstIntFieldInst: int;
    neighbors: [..] LDtk_Level_Neighbor;
    layers: [..] LDtk_LayerInstance;
    tilesets: [..] LDtk_Tileset;
    enums: [..] LDtk_Enum;
}

LDtk_File_Detail :: struct {
    jsonVersion: string;
    defaultPivotX: float64;
    defaultPivotY: float64;
    defaultGridSize: int;
    bgColor: string;
    nextUid: int;
    levels: [..] LDtk_Level;
    tilesets: [..] LDtk_Tileset;
    enums: [..] LDtk_Enum;
}

LDTK_FIELD_TYPE_INTEGER       :: "Int";
LDTK_FIELD_TYPE_FLOAT         :: "Float";
LDTK_FIELD_TYPE_BOOL          :: "Bool";
LDTK_FIELD_TYPE_STRING        :: "String";
LDTK_FIELD_TYPE_LOCAL_ENUM    :: "LocalEnum";
LDTK_FIELD_TYPE_COLOR         :: "Color";
LDTK_FIELD_TYPE_POINT         :: "Point";
LDTK_FIELD_TYPE_TILE          :: "Tile";
LDTK_FIELD_TYPE_FILE_PATH     :: "FilePath";
LDTK_FIELD_TYPE_ARRAY_INT     :: "Array<Int>";
LDTK_FIELD_TYPE_ARRAY_FLOAT   :: "Array<Float>";
LDTK_FIELD_TYPE_ARRAY_BOOL    :: "Array<Bool>";
LDTK_FIELD_TYPE_ARRAY_STRING  :: "Array<String>";
LDTK_FIELD_TYPE_ARRAY_ENUM    :: "Array<LocalEnum";
LDTK_FIELD_TYPE_ARRAY_POINT   :: "Array<Point>";
LDTK_FIELD_TYPE_ARRAY_COLOR   :: "Array<Color>";
LDTK_FIELD_TYPE_ARRAY_FILE    :: "Array<FilePath>";

// ============================================================
// LDtk Loader State
// ============================================================

_LDtk_State :: struct {
    schema: LDtk_Json_Value;
    user_data: LDtk_Json_Value;
    file_detail: LDtk_File_Detail;
    initialized: bool;
}

state: _LDtk_State;

// ============================================================
// LDtk Loader Functions
// ============================================================

load_ldtk_file :: (schema_str: string, filename: string) -> bool {
    success, schema_val := ldtk_json_parse_string(schema_str);
    if !success return false;
    state.schema = schema_val;

    file_data, read_success := read_entire_file(filename);
    if !read_success {
        print("Failed to read file: %\n", filename);
        return false;
    }
    defer free(file_data);

    user_val: LDtk_Json_Value;
    success, user_val = ldtk_json_parse_string(file_data);
    if !success {
        print("Failed to parse JSON from file: %\n", filename);
        return false;
    }
    state.user_data = user_val;
    state.initialized = true;
    return true;
}

import_file_specs :: () {
    if !state.initialized return;
    obj := ldtk_json_object(state.user_data);
    if !obj return;

    state.file_detail.jsonVersion = ldtk_json_object_get_string(obj, "jsonVersion");
    state.file_detail.defaultPivotX = ldtk_json_object_get_number(obj, "defaultPivotX");
    state.file_detail.defaultPivotY = ldtk_json_object_get_number(obj, "defaultPivotY");
    state.file_detail.defaultGridSize = cast(int)ldtk_json_object_get_number(obj, "defaultGridSize");
    state.file_detail.bgColor = ldtk_json_object_get_string(obj, "bgColor");
    state.file_detail.nextUid = cast(int)ldtk_json_object_get_number(obj, "nextUid");
}

import_tilesets :: () {
    if !state.initialized return;

    obj := ldtk_json_object(state.user_data);
    if !obj return;

    tilesets_array := ldtk_json_object_dotget_array(obj, "defs.tilesets");
    if !tilesets_array return;

    for i: 0..ldtk_json_array_get_count(tilesets_array)-1 {
        t_obj := ldtk_json_array_get_object(tilesets_array, i);
        if !t_obj continue;

        ts: LDtk_Tileset;
        ts.identifier = ldtk_json_object_get_string(t_obj, "identifier");
        ts.uid = cast(int)ldtk_json_object_get_number(t_obj, "uid");
        ts.relPath = ldtk_json_object_get_string(t_obj, "relPath");
        ts.pxWid = cast(int)ldtk_json_object_get_number(t_obj, "pxWid");
        ts.pxHei = cast(int)ldtk_json_object_get_number(t_obj, "pxHei");
        ts.tileGridSize = cast(int)ldtk_json_object_get_number(t_obj, "tileGridSize");
        ts.spacing = cast(int)ldtk_json_object_get_number(t_obj, "spacing");
        ts.padding = cast(int)ldtk_json_object_get_number(t_obj, "padding");

        array_add(*state.file_detail.tilesets, ts);
    }
}

import_enums :: () {
    if !state.initialized return;

    obj := ldtk_json_object(state.user_data);
    if !obj return;

    enums_array := ldtk_json_object_dotget_array(obj, "defs.enums");
    if !enums_array return;

    for i: 0..ldtk_json_array_get_count(enums_array)-1 {
        e_obj := ldtk_json_array_get_object(enums_array, i);
        if !e_obj continue;

        et: LDtk_Enum;
        et.identifier = ldtk_json_object_get_string(e_obj, "identifier");
        et.uid = cast(int)ldtk_json_object_get_number(e_obj, "uid");
        et.iconTilesetUid = cast(int)ldtk_json_object_get_number(e_obj, "iconTilesetUid");

        values_array := ldtk_json_object_get(e_obj, "values");
        if values_array.type == .ARRAY {
            v_arr := ldtk_json_array(values_array);

            for j: 0..ldtk_json_array_get_count(v_arr)-1 {
                v_obj := ldtk_json_array_get_object(v_arr, j);
                if !v_obj continue;

                detail: LDtk_Enum_Detail;
                detail.id = ldtk_json_object_get_string(v_obj, "id");
                detail.tileId = cast(int)ldtk_json_object_get_number(v_obj, "tileId");

                rect_array := ldtk_json_object_get(v_obj, "__tileSrcRect");
                if rect_array.type == .ARRAY {
                    r_arr := ldtk_json_array(rect_array);
                    for k: 0..3 {
                        detail.tileSrcRect[k] = cast(int)ldtk_json_array_get_number(r_arr, k);
                    }
                }

                array_add(*et.details, detail);
            }
        }

        array_add(*state.file_detail.enums, et);
    }
}

import_levels :: () {
    if !state.initialized return;

    obj := ldtk_json_object(state.user_data);
    if !obj return;

    levels_array := ldtk_json_object_get(obj, "levels");
    if levels_array.type != .ARRAY return;

    l_arr := ldtk_json_array(levels_array);

    for i: 0..ldtk_json_array_get_count(l_arr)-1 {
        l_obj := ldtk_json_array_get_object(l_arr, i);
        if !l_obj continue;

        level: LDtk_Level;
        level.identifier = ldtk_json_object_get_string(l_obj, "identifier");
        level.uid = cast(int)ldtk_json_object_get_number(l_obj, "uid");
        level.pxWid = cast(int)ldtk_json_object_get_number(l_obj, "pxWid");
        level.pxHei = cast(int)ldtk_json_object_get_number(l_obj, "pxHei");
        level.worldX = cast(int)ldtk_json_object_get_number(l_obj, "worldX");
        level.worldY = cast(int)ldtk_json_object_get_number(l_obj, "worldY");

        field_inst_array := ldtk_json_object_get(l_obj, "fieldInstances");
        if field_inst_array.type == .ARRAY {
            fi_arr := ldtk_json_array(field_inst_array);
            if ldtk_json_array_get_count(fi_arr) > 0 {
                first := ldtk_json_array_get_object(fi_arr, 0);
                if first {
                    level.firstIntFieldInst = cast(int)ldtk_json_object_get_number(first, "__value");
                }
            }
        }

        neighbors_array := ldtk_json_object_get(l_obj, "__neighbours");
        if neighbors_array.type == .ARRAY {
            n_arr := ldtk_json_array(neighbors_array);

            for j: 0..ldtk_json_array_get_count(n_arr)-1 {
                n_obj := ldtk_json_array_get_object(n_arr, j);
                if !n_obj continue;

                nb: LDtk_Level_Neighbor;
                dir_str := ldtk_json_object_get_string(n_obj, "dir");
                nb.dir = ifx dir_str.count > 0 then dir_str[0] else 0;
                nb.uid = cast(int)ldtk_json_object_get_number(n_obj, "levelUid");

                array_add(*level.neighbors, nb);
            }
        }

        layer_inst_array := ldtk_json_object_get(l_obj, "layerInstances");
        if layer_inst_array.type == .ARRAY {
            li_arr := ldtk_json_array(layer_inst_array);

            for j: 0..ldtk_json_array_get_count(li_arr)-1 {
                li_obj := ldtk_json_array_get_object(li_arr, j);
                if !li_obj continue;

                layer: LDtk_LayerInstance;
                layer.identifier = ldtk_json_object_get_string(li_obj, "__identifier");
                layer.type = ldtk_json_object_get_string(li_obj, "__type");
                layer.cWid = cast(int)ldtk_json_object_get_number(li_obj, "__cWid");
                layer.cHei = cast(int)ldtk_json_object_get_number(li_obj, "__cHei");
                layer.gridSize = cast(int)ldtk_json_object_get_number(li_obj, "__gridSize");
                layer.levelId = cast(int)ldtk_json_object_get_number(li_obj, "levelId");
                layer.layerDefUid = cast(int)ldtk_json_object_get_number(li_obj, "layerDefUid");
                layer.pxOffsetX = cast(int)ldtk_json_object_get_number(li_obj, "pxOffsetX");
                layer.pxOffsetY = cast(int)ldtk_json_object_get_number(li_obj, "pxOffsetY");

                int_grid_csv := ldtk_json_object_get(li_obj, "intGridCsv");
                if int_grid_csv.type == .ARRAY {
                    ig_arr := ldtk_json_array(int_grid_csv);

                    for k: 0..ldtk_json_array_get_count(ig_arr)-1 {
                        array_add(*layer.intGridCsv, cast(int)ldtk_json_array_get_number(ig_arr, k));
                    }
                }

                auto_tiles_array := ldtk_json_object_get(li_obj, "autoLayerTiles");
                if auto_tiles_array.type == .ARRAY {
                    at_arr := ldtk_json_array(auto_tiles_array);

                    for k: 0..ldtk_json_array_get_count(at_arr)-1 {
                        at_obj := ldtk_json_array_get_object(at_arr, k);
                        if !at_obj continue;

                        at: LDtk_AutoTile;
                        px_array := ldtk_json_object_get(at_obj, "px");
                        if px_array.type == .ARRAY {
                            px_arr := ldtk_json_array(px_array);
                            at.x = cast(int)ldtk_json_array_get_number(px_arr, 0);
                            at.y = cast(int)ldtk_json_array_get_number(px_arr, 1);
                        }
                        src_array := ldtk_json_object_get(at_obj, "src");
                        if src_array.type == .ARRAY {
                            src_arr := ldtk_json_array(src_array);
                            at.srcX = cast(int)ldtk_json_array_get_number(src_arr, 0);
                            at.srcY = cast(int)ldtk_json_array_get_number(src_arr, 1);
                        }
                        at.f = cast(int)ldtk_json_object_get_number(at_obj, "f");
                        at.count = k + 1;

                        array_add(*layer.autoTiles, at);
                    }
                }

                grid_tiles_array := ldtk_json_object_get(li_obj, "gridTiles");
                if grid_tiles_array.type == .ARRAY {
                    gt_arr := ldtk_json_array(grid_tiles_array);

                    for k: 0..ldtk_json_array_get_count(gt_arr)-1 {
                        gt_obj := ldtk_json_array_get_object(gt_arr, k);
                        if !gt_obj continue;

                        gt: LDtk_GridTile;
                        px_array := ldtk_json_object_get(gt_obj, "px");
                        if px_array.type == .ARRAY {
                            px_arr := ldtk_json_array(px_array);
                            gt.x = cast(int)ldtk_json_array_get_number(px_arr, 0);
                            gt.y = cast(int)ldtk_json_array_get_number(px_arr, 1);
                        }
                        src_array := ldtk_json_object_get(gt_obj, "src");
                        if src_array.type == .ARRAY {
                            src_arr := ldtk_json_array(src_array);
                            gt.srcX = cast(int)ldtk_json_array_get_number(src_arr, 0);
                            gt.srcY = cast(int)ldtk_json_array_get_number(src_arr, 1);
                        }
                        gt.f = cast(int)ldtk_json_object_get_number(gt_obj, "f");
                        gt.count = k + 1;

                        array_add(*layer.gridTiles, gt);
                    }
                }

                if equal(layer.type, "Entities") {
                    entity_array := ldtk_json_object_get(li_obj, "entityInstances");
                    if entity_array.type == .ARRAY {
                        e_arr := ldtk_json_array(entity_array);

                        for k: 0..ldtk_json_array_get_count(e_arr)-1 {
                            e_obj := ldtk_json_array_get_object(e_arr, k);
                            if !e_obj continue;

                            ent: LDtk_EntityInstance;
                            ent.identifier = ldtk_json_object_get_string(e_obj, "__identifier");

                            grid_array := ldtk_json_object_get(e_obj, "__grid");
                            if grid_array.type == .ARRAY {
                                g_arr := ldtk_json_array(grid_array);
                                ent.cx = cast(int)ldtk_json_array_get_number(g_arr, 0);
                                ent.cy = cast(int)ldtk_json_array_get_number(g_arr, 1);
                            }

                            px_array := ldtk_json_object_get(e_obj, "px");
                            if px_array.type == .ARRAY {
                                p_arr := ldtk_json_array(px_array);
                                ent.x = cast(int)ldtk_json_array_get_number(p_arr, 0);
                                ent.y = cast(int)ldtk_json_array_get_number(p_arr, 1);
                            }

                            ent.defUid = cast(int)ldtk_json_object_get_number(e_obj, "defUid");

                            field_array := ldtk_json_object_get(e_obj, "fieldInstances");
                            if field_array.type == .ARRAY {
                                f_arr := ldtk_json_array(field_array);
                                ent.fieldInstanceCount = cast(int)ldtk_json_array_get_count(f_arr);

                                for m: 0..ldtk_json_array_get_count(f_arr)-1 {
                                    f_obj := ldtk_json_array_get_object(f_arr, m);
                                    if !f_obj continue;

                                    fi: LDtk_FieldInstance;
                                    fi.identifier = ldtk_json_object_get_string(f_obj, "__identifier");
                                    fi.type = ldtk_json_object_get_string(f_obj, "__type");
                                    fi.defUid = cast(int)ldtk_json_object_get_number(f_obj, "defUid");

                                    if equal(fi.type, LDTK_FIELD_TYPE_ARRAY_POINT) {
                                        value_array := ldtk_json_object_get(f_obj, "__value");
                                        if value_array.type == .ARRAY {
                                            v_arr := ldtk_json_array(value_array);
                                            fi.size = cast(int)ldtk_json_array_get_count(v_arr);

                                            for n: 0..ldtk_json_array_get_count(v_arr)-1 {
                                                p_obj := ldtk_json_array_get_object(v_arr, n);
                                                if !p_obj continue;
                                                pt: LDtk_FieldInstance_Points;
                                                pt.cx = layer.gridSize * cast(int)ldtk_json_object_get_number(p_obj, "cx");
                                                pt.cy = layer.gridSize * cast(int)ldtk_json_object_get_number(p_obj, "cy");
                                                pt.check = 1;
                                                pt.size = n + 1;
                                                array_add(*fi.points, pt);
                                            }
                                        }
                                    } else if equal(fi.type, LDTK_FIELD_TYPE_POINT) {
                                        value_obj := ldtk_json_object_get(f_obj, "__value");
                                        if value_obj.type == .OBJECT {
                                            v_obj := ldtk_json_object(value_obj);
                                            fi.size = 1;
                                            pt: LDtk_FieldInstance_Points;
                                            pt.cx = layer.gridSize * cast(int)ldtk_json_object_get_number(v_obj, "cx");
                                            pt.cy = layer.gridSize * cast(int)ldtk_json_object_get_number(v_obj, "cy");
                                            pt.check = 1;
                                            pt.size = 1;
                                            array_add(*fi.points, pt);
                                        }
                                    } else if equal(fi.type, LDTK_FIELD_TYPE_TILE) { 
                                        value_obj := ldtk_json_object_get(f_obj, "__value");
                                        if value_obj.type == .OBJECT {
                                            v_obj := ldtk_json_object(value_obj);
                                            fi.size = 1;
                                            tile: LDtk_FieldInstance_Tiles;
                                            tile.tileId = cast(int)ldtk_json_object_get_number(v_obj, "tilesetUid");
                                            tile.srcX = cast(int)ldtk_json_object_get_number(v_obj, "x");
                                            tile.srcY = cast(int)ldtk_json_object_get_number(v_obj, "y");
                                            tile.width = cast(int)ldtk_json_object_get_number(v_obj, "w");
                                            tile.height = cast(int)ldtk_json_object_get_number(v_obj, "h");
                                            array_add(*fi.tiles, tile);
                                        }
                                    } else if equal(fi.type, LDTK_FIELD_TYPE_STRING) || equal(fi.type, LDTK_FIELD_TYPE_COLOR) || equal(fi.type, LDTK_FIELD_TYPE_FILE_PATH) || (fi.type.count >= 9 && equal(slice(fi.type, 0, 9), LDTK_FIELD_TYPE_LOCAL_ENUM)) {
                                        fi.size = 1;
                                        str_val: LDtk_FieldInstance_Strings;
                                        str_val.strValue = ldtk_json_object_get_string(f_obj, "__value");
                                        str_val.check = 1;
                                        str_val.size = 1;
                                        array_add(*fi.strings, str_val);
                                    } else if equal(fi.type, LDTK_FIELD_TYPE_ARRAY_INT) {
                                        value_array := ldtk_json_object_get(f_obj, "__value");
                                        if value_array.type == .ARRAY {
                                            v_arr := ldtk_json_array(value_array);
                                            fi.size = cast(int)ldtk_json_array_get_count(v_arr);

                                            for n: 0..ldtk_json_array_get_count(v_arr)-1 {
                                                int_val: LDtk_FieldInstance_Ints;
                                                int_val.intValue = cast(int)ldtk_json_array_get_number(v_arr, n);
                                                int_val.check = 1;
                                                int_val.size = n + 1;
                                                array_add(*fi.ints, int_val);
                                            }
                                        }
                                    } else if equal(fi.type, LDTK_FIELD_TYPE_ARRAY_FLOAT) {
                                        value_array := ldtk_json_object_get(f_obj, "__value");
                                        if value_array.type == .ARRAY {
                                            v_arr := ldtk_json_array(value_array);
                                            fi.size = cast(int)ldtk_json_array_get_count(v_arr);

                                            for n: 0..ldtk_json_array_get_count(v_arr)-1 {
                                                fl_val: LDtk_FieldInstance_Floats;
                                                fl_val.floatValue = cast(float32)ldtk_json_array_get_number(v_arr, n);
                                                fl_val.check = 1;
                                                fl_val.size = n + 1;
                                                array_add(*fi.floats, fl_val);
                                            }
                                        }
                                    } else if equal(fi.type, LDTK_FIELD_TYPE_ARRAY_BOOL) {
                                        value_array := ldtk_json_object_get(f_obj, "__value");
                                        if value_array.type == .ARRAY {
                                            v_arr := ldtk_json_array(value_array);
                                            fi.size = cast(int)ldtk_json_array_get_count(v_arr);

                                            for n: 0..ldtk_json_array_get_count(v_arr)-1 {
                                                int_val: LDtk_FieldInstance_Ints;
                                                int_val.intValue = cast(int)ldtk_json_array_get_bool(v_arr, n);
                                                int_val.check = 1;
                                                int_val.size = n + 1;
                                                array_add(*fi.ints, int_val);
                                            }
                                        }
                                    } else if equal(fi.type, LDTK_FIELD_TYPE_INTEGER) || equal(fi.type, LDTK_FIELD_TYPE_BOOL) {
                                        fi.size = 1;
                                        int_val: LDtk_FieldInstance_Ints;
                                        if equal(fi.type, LDTK_FIELD_TYPE_INTEGER) {
                                            int_val.intValue = cast(int)ldtk_json_object_get_number(f_obj, "__value");
                                        } else {
                                            int_val.intValue = cast(int)ldtk_json_object_get_bool(f_obj, "__value");
                                        }
                                        int_val.check = 1;
                                        int_val.size = 1;
                                        array_add(*fi.ints, int_val);
                                    } else if equal(fi.type, LDTK_FIELD_TYPE_FLOAT) {
                                        fi.size = 1;
                                        fl_val: LDtk_FieldInstance_Floats;
                                        fl_val.floatValue = cast(float32)ldtk_json_object_get_number(f_obj, "__value");
                                        fl_val.check = 1;
                                        fl_val.size = 1;
                                        array_add(*fi.floats, fl_val);
                                    } else if fi.type.count >= 15 && equal(slice(fi.type, 0, 15), LDTK_FIELD_TYPE_ARRAY_ENUM) || equal(fi.type, LDTK_FIELD_TYPE_ARRAY_FILE) || equal(fi.type, LDTK_FIELD_TYPE_ARRAY_STRING) || equal(fi.type, LDTK_FIELD_TYPE_ARRAY_COLOR) {
                                        value_array := ldtk_json_object_get(f_obj, "__value");
                                        if value_array.type == .ARRAY {
                                            v_arr := ldtk_json_array(value_array);
                                            fi.size = cast(int)ldtk_json_array_get_count(v_arr);

                                            for n: 0..ldtk_json_array_get_count(v_arr)-1 {
                                                str_val: LDtk_FieldInstance_Strings;
                                                str_val.strValue = ldtk_json_array_get_string(v_arr, n);
                                                str_val.check = 1;
                                                str_val.size = n + 1;
                                                array_add(*fi.strings, str_val);
                                            }
                                        }
                                    }

                                    array_add(*ent.fieldInstances, fi);
                                }
                            }

                            array_add(*layer.entityInstances, ent);
                        }
                    }
                }

                if equal(layer.type, "IntGrid") {
                    int_grid_array := ldtk_json_object_get(li_obj, "intGrid");
                    if int_grid_array.type == .ARRAY {
                        ig_arr := ldtk_json_array(int_grid_array);

                        for k: 0..ldtk_json_array_get_count(ig_arr)-1 {
                            ig_obj := ldtk_json_array_get_object(ig_arr, k);
                            if !ig_obj continue;

                            ig: LDtk_IntGrid;
                            ig.coordId = cast(int)ldtk_json_object_get_number(ig_obj, "coordId");
                            ig.v = cast(int)ldtk_json_object_get_number(ig_obj, "v");
                            ig.count = k + 1;
                            array_add(*layer.intGrid, ig);
                        }
                    }
                }

                array_add(*level.layers, layer);
            }
        }

        array_add(*state.file_detail.levels, level);
    }
}

import_map_data :: () {
    import_file_specs();
    import_tilesets();
    import_enums();
    import_levels();
}

get_level_by_name :: (name: string) -> *LDtk_Level {
    for i: 0..state.file_detail.levels.count-1 {
        if equal(state.file_detail.levels[i].identifier, name) return state.file_detail.levels.data + i;
    }
    return null;
}

get_level_by_uid :: (uid: int) -> *LDtk_Level {
    for i: 0..state.file_detail.levels.count-1 {
        if state.file_detail.levels[i].uid == uid return state.file_detail.levels.data + i;
    }
    return null;
}

get_entity :: (entity_name: string, level_id: int) -> [..] LDtk_EntityInstance {
    result: [..] LDtk_EntityInstance;

    level := get_level_by_uid(level_id);
    if level == null return result;

    for layer: level.layers {
        if !equal(layer.type, "Entities") continue;

        for ent: layer.entityInstances {
            if equal(ent.identifier, entity_name) {
                return layer.entityInstances;
            }
        }
    }
    return result;
}

get_layer :: (layer_name: string, level_id: int) -> *LDtk_LayerInstance {
    level := get_level_by_uid(level_id);
    if level == null return null;

    for i: 0..level.layers.count-1 {
        if equal(level.layers[i].identifier, layer_name) return level.layers.data + i;
    }
    return null;
}

free_ldtk_data :: () {
    array_free(state.file_detail.levels);
    array_free(state.file_detail.tilesets);
    array_free(state.file_detail.enums);

    ldtk_free_json_value(state.schema);
    ldtk_free_json_value(state.user_data);

    state.initialized = false;
}
