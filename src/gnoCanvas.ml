type items_properties = [ 
  | `pixbuf of GdkPixbuf.pixbuf
  | `width of float
  | `height of float
  | `bpath of GnomeCanvas.PathDef.t
  | `anchor of Gtk.Tags.anchor_type
  | `cap_style of Gdk.GC.gdkCapStyle
  | `smooth of bool
  | `first_arrowhead of bool
  | `last_arrowhead of bool
  | `arrow_shape_a of float
  | `arrow_shape_b of float
  | `arrow_shape_c of float
  | `points of float array
  | `fill_color of string
  | `fill_color_rgba of int32
  | `font of string
  | `outline_color of string
  | `size of int
  | `text of string
  | `width_units of float
  | `width_pixels of int
  | `x of float
  | `x1 of float
  | `x2 of float
  | `y of float
  | `y1 of float
  | `y2 of float] 

let propertize p =
  let name, g_fund, g_val = match p with
  | `x v -> "x", `DOUBLE, `FLOAT v
  | `y v -> "y", `DOUBLE, `FLOAT v
  | `x1 v -> "x1", `DOUBLE, `FLOAT v
  | `y1 v -> "y1", `DOUBLE, `FLOAT v
  | `x2 v -> "x2", `DOUBLE, `FLOAT v
  | `y2 v -> "y2", `DOUBLE, `FLOAT v
  | `outline_color "" -> "outline_color", `STRING, `STRING None
  | `outline_color c -> "outline_color", `STRING, `STRING (Some c)
  | `fill_color "" -> "fill_color", `STRING, `STRING None
  | `fill_color c -> "fill_color", `STRING, `STRING (Some c)
  | `fill_color_rgba c -> "fill_color_rgba", `UINT, `INT32 c
  (* | `fill_color_stipple (c : Gdk.bitmap) -> "fill_stipple", `OBJECT, `OBJECT (Some (d :> [`gtk] Gobject.obj)) *)
  | `width_units v -> "width_units", `DOUBLE, `FLOAT v
  | `width_pixels v -> "width_pixels", `UINT, `INT v
  (* | `text "" -> "text", `STRING, `STRING None *)
  | `text t -> "text", `STRING, `STRING (Some t)
  | `font t -> "font", `STRING, `STRING (Some t)
  | `size i -> "size", `INT, `INT i
  | `points p -> "points", `GVAL (GnomeCanvas.convert_points p), `BOOL false
  | `arrow_shape_a v -> "arrow_shape_a", `DOUBLE, `FLOAT v
  | `arrow_shape_b v -> "arrow_shape_b", `DOUBLE, `FLOAT v
  | `arrow_shape_c v -> "arrow_shape_c", `DOUBLE, `FLOAT v
  | `first_arrowhead b -> "first_arrowhead", `BOOLEAN, `BOOL b
  | `last_arrowhead  b -> "last_arrowhead", `BOOLEAN, `BOOL b
  | `anchor a -> "anchor", `INT, `INT (GnomeCanvas.convert_tags (GnomeCanvas.ANCHOR a))
  | `cap_style c -> "cap_style", `INT, `INT (GnomeCanvas.convert_tags (GnomeCanvas.CAPSTYLE c))
  | `bpath p -> "bpath", `POINTER , `POINTER (Some p)
  | `smooth b -> "smooth", `BOOLEAN, `BOOL b
  | `pixbuf (p : GdkPixbuf.pixbuf) -> "pixbuf", `OBJECT, `OBJECT (Some p)
  | `width v -> "width", `DOUBLE, `FLOAT v
  | `height v -> "height", `DOUBLE, `FLOAT v
  in
  let v = match g_fund with
  | `GVAL v -> v
  | #Gobject.Tags.fundamental_type as f -> 
      let v = Gobject.Value.create (Gobject.Type.of_fundamental f)  in
      Gobject.Value.set v g_val ; v
  in
  name, v

class item_signals ?after obj = object
  inherit GObj.gtkobj_signals ?after obj
  method event : callback:(GdkEvent.any -> bool) -> GtkSignal.id =
    GtkSignal.connect ~sgn:GnomeCanvas.Item.Signals.event ~after obj
end

class ['p] item obj = object
  inherit GObj.gtkobj obj
  method connect = new item_signals (obj :> GnomeCanvas.item Gtk.obj)
  method set_raw = GnomeCanvas.Item.set obj
  method set (p : 'p list) =
    GnomeCanvas.Item.set obj (List.map propertize p)
  method as_item = (obj :> GnomeCanvas.item Gtk.obj)
  method canvas = GnomeCanvas.Item.canvas obj
  method parent = GnomeCanvas.Item.parent obj
  method xform = GnomeCanvas.Item.xform obj
  method affine_relative = GnomeCanvas.Item.affine_relative obj
  method affine_absolute = GnomeCanvas.Item.affine_absolute obj
  method move = GnomeCanvas.Item.move obj
  method raise = GnomeCanvas.Item.raise obj
  method lower = GnomeCanvas.Item.lower obj
  method raise_to_top = GnomeCanvas.Item.raise_to_top obj
  method lower_to_bottom = GnomeCanvas.Item.lower_to_bottom obj
  method show = GnomeCanvas.Item.show obj
  method hide = GnomeCanvas.Item.hide obj
  method grab = GnomeCanvas.Item.grab obj
  method ungrab = GnomeCanvas.Item.ungrab obj
  method w2i = GnomeCanvas.Item.w2i obj
  method i2w = GnomeCanvas.Item.i2w obj
  method reparent grp = GnomeCanvas.Item.reparent obj grp
  method grab_focus = GnomeCanvas.Item.grab_focus obj
  method get_bounds = GnomeCanvas.Item.get_bounds obj
end

class group grp_obj = object
  inherit [GnomeCanvas.Types.group_p] item grp_obj
  method as_group = (grp_obj : GnomeCanvas.group Gtk.obj)
  method get_items =
    (* List.map (fun i -> new item i) *)
      (GnomeCanvas.Group.get_items grp_obj)
end


class canvas obj = object
  inherit GObj.widget_full (obj : GnomeCanvas.canvas Gtk.obj)
  method event = new GObj.event_ops obj

  method root = new group (GnomeCanvas.Canvas.root obj)
  method aa =
    let v = Gobject.Value.create (Gobject.Type.of_fundamental `BOOLEAN) in
    Gobject.get_property obj "aa" v ;
    match Gobject.Value.get v with
    | `BOOL b -> b
    | _ -> failwith "unexpected type for property"
  method set_scroll_region = GnomeCanvas.Canvas.set_scroll_region obj
  method get_scroll_region = GnomeCanvas.Canvas.get_scroll_region obj
  method set_center_scroll_region = GnomeCanvas.Canvas.set_center_scroll_region obj
  method get_center_scroll_region = GnomeCanvas.Canvas.get_center_scroll_region obj
  method set_pixels_per_unit = GnomeCanvas.Canvas.set_pixels_per_unit obj
  method scroll_to = GnomeCanvas.Canvas.scroll_to obj
  method get_scroll_offsets = GnomeCanvas.Canvas.get_scroll_offsets obj
  method update_now () = GnomeCanvas.Canvas.update_now obj
  method get_item_at ~x ~y = GnomeCanvas.Canvas.get_item_at obj ~x ~y
  method w2c_affine = GnomeCanvas.Canvas.w2c_affine obj
  method w2c = GnomeCanvas.Canvas.w2c obj
  method w2c_d = GnomeCanvas.Canvas.w2c_d obj
  method c2w = GnomeCanvas.Canvas.c2w obj
  method window_to_world = GnomeCanvas.Canvas.window_to_world obj
  method world_to_window   = GnomeCanvas.Canvas.world_to_window obj
end

let canvas ?(aa=false) ?border_width ?width ?height ?packing ?show () =
  let w = if aa
  then GnomeCanvas.Canvas.new_canvas_aa ()
  else GnomeCanvas.Canvas.new_canvas () in
  GtkBase.Container.set w ?border_width ?width ?height ;
  GObj.pack_return (new canvas w) ~packing ~show

let wrap_item o (typ : (_, 'p) GnomeCanvas.Types.t) =
  if not (GnomeCanvas.Types.is_a o typ)
  then failwith "type error" ;
  (new item o : 'p item)

let item (typ : (_, 'p) GnomeCanvas.Types.t) ?(props=[]) parent =
  let i = GnomeCanvas.Item.new_item parent#as_group typ in
  let o = (new item i : 'p item) in
  if props <> [] then o#set props ;
  o

let unoption_list ?(rest=[]) l =
  List.rev_append
    (List.fold_left (fun acc -> function Some v -> v :: acc | None -> acc) [] l)
    rest

let group ?x ?y parent =
  let i = GnomeCanvas.Item.new_item parent#as_group GnomeCanvas.Types.group in
  let g = new group i in
  let props = unoption_list
      [ ( match x with None -> None | Some v -> Some (`x v) ) ;
	( match y with None -> None | Some v -> Some (`y v) ) ; ] in
  if props <> [] then g#set props ;
  g

type rect = GnomeCanvas.Types.re_p item
let rect ?props p = item GnomeCanvas.Types.rect ?props p
type ellipse = GnomeCanvas.Types.re_p item
let ellipse ?props p = item GnomeCanvas.Types.ellipse ?props p
type text = GnomeCanvas.Types.text_p item
let text ?props p = item GnomeCanvas.Types.text ?props p
type line = GnomeCanvas.Types.line_p item
let line ?props p = item GnomeCanvas.Types.line ?props p
type bpath = GnomeCanvas.Types.bpath_p item
let bpath ?props p = item GnomeCanvas.Types.bpath ?props p

type pixbuf = GnomeCanvas.Types.pixbuf_p item
let pixbuf ?x ?y ?pixbuf ?(props=[]) p =
  let props = unoption_list ~rest:props
      [ ( match x with None -> None | Some v -> Some (`x v) ) ;
	( match y with None -> None | Some v -> Some (`y v) ) ;
	( match pixbuf with None -> None | Some v -> Some (`pixbuf v) ) ;
      ] in
  item GnomeCanvas.Types.pixbuf ~props p

type polygon = GnomeCanvas.Types.polygon_p item
let polygon ?points ?(props=[]) p =
  let props = 
    match points with None -> props | Some p -> `points p :: props in
  item GnomeCanvas.Types.polygon ~props p

let parent i =
  new group (i#parent)
