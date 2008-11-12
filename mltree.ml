#!/usr/bin/env ocaml
(* {{{ COPYING *)
(*
 * +--------------------------------------------------------------------+
 * | Copyright (C) 2005 2006 Florent Monnier                            |
 * +--------------------------------------------------------------------+
 * | This is a small implementation of the 'tree' command-line utility. |
 * +--------------------------------------------------------------------+
 * |                                                                    |
 * | This program is free software; you can redistribute it and/or      |
 * | modify it under the terms of the GNU General Public License        |
 * | as published by the Free Software Foundation; either version 2     |
 * | of the License, or (at your option) any later version.             |
 * |                                                                    |
 * | This program is distributed in the hope that it will be useful,    |
 * | but WITHOUT ANY WARRANTY; without even the implied warranty of     |
 * | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      |
 * | GNU General Public License for more details.                       |
 * |                                                                    |
 * | http://www.fsf.org/licensing/licenses/gpl.html                     |
 * |                                                                    |
 * | You should have received a copy of the GNU General Public License  |
 * | along with this program; if not,                                   |
 * | write to the Free Software Foundation, Inc.,                       |
 * | 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA       |
 * |                                                                    |
 * +--------------------------------------------------------------------+
 * | Author: Florent Monnier <fmonnier(¤)linux-nantes.fr.eu.org>        |
 * +--------------------------------------------------------------------+
 *
 * }}} *)
(* {{{ Bugs & comments *)
(*
 * This script is bugged!
 * But calling it on /home or /usr
 * with the option --colors
 * outputs a nice display on screen :)
 *
)* }}} *)
(* {{{ utils/types/inits *)

#load "unix.cma"

type options = {
  colors: bool;
  hide: bool;
  mt: bool;
  at: bool;
  ct: bool;
}

let concat = Filename.concat ;;

let ( +^ ) = Int64.add ;;

(* }}} *)
(* {{{ usage *)

let usage() =
  print_endline "
  display file times:
    -mt  --modification-time
    -at  --last-access-time
    -ct  --status-change-time

  display with colors:
    -c  --colors
";
  exit(1);
;;
(* }}} *)
(* {{{ round *)

let round nb dec =
  let rec _loop level =
    if level <= 0
    then 1
    else 10 * _loop (pred level)
  in
  let mult = float(_loop dec) in
  let nb = nb *. mult in
  let nb_floor = floor nb
  and nb_ceil  = ceil nb
  in
  if (nb -. nb_floor) < (nb_ceil -. nb)
  then nb_floor /. mult
  else nb_ceil  /. mult
;;
(* }}} *)
(* {{{ string_of_round *)

let string_of_round_nopad nb dec =
  let rounded = round nb dec in
  if rounded = (floor nb)
  then string_of_int (int_of_float nb)
  else string_of_float rounded
;;

let string_of_round nb dec =
  let rounded = round nb dec in
  if rounded = (floor nb)
  then Printf.sprintf "%4d" (int_of_float nb)
  else Printf.sprintf "%4.f" rounded
;;
(* }}} *)
(* {{{ human_size *)

let string_of_size =
  let round f = int_of_float (floor(f +. 0.5)) in
  fun ~size ->
  let s = Int64.to_float size in
  if s < 1000. then string_of_int(truncate s) else
  let k = s /. 1024. in
  if k < 10.0 then Printf.sprintf "%.1fK" k else
  if k < 1000. then Printf.sprintf "%dK" (round k) else
  let m = k /. 1024. in
  if m < 10.0 then Printf.sprintf "%.1fM" m else
  if m < 1000. then Printf.sprintf "%dM" (round m) else
  let g = m /. 1024. in
  if g < 10.0 then Printf.sprintf "%.1fG" g else
  if g < 1000. then Printf.sprintf "%dG" (round g) else
  let t = g /. 1024. in
  if t < 10.0 then Printf.sprintf "%.1fT" t else
  if t < 1000. then Printf.sprintf "%dT" (round t) else
  let p = t /. 1024. in
  if p < 10.0 then Printf.sprintf "%.1fP" p else
  if p < 1000. then Printf.sprintf "%dP" (round p) else
  let e = p /. 1024. in
  if e < 10.0 then Printf.sprintf "%.1fE" e else
  if e < 1000. then Printf.sprintf "%dE" (round e) else
  Printf.sprintf "%dE" (round e)
;;

let human_size ~size =
  let str = string_of_size ~size in
  match String.length str with
  | 1 -> "   " ^ str
  | 2 ->  "  " ^ str
  | 3 ->   " " ^ str
  | _ -> str
;;

(* }}} *)
(* {{{ padding *)

let padding ~last ~depth =
  let rec aux  depth last pad_acc =
    if depth <= 0
    then pad_acc
    else begin
      let branch =
        match last with
        | false :: _ -> "|   "
        | true  :: _ -> "    "
        | [] -> "xxxx"  (* this case should never occur *)
      in
      aux (pred depth) (List.tl last) (branch ^ pad_acc)
    end
  in
  let last =
    match last with
    | head::tail -> tail  (* the head is printed from branch_mark *)
    | _ -> []
  in
  aux depth last ""
;;


let right_pad filenames =
  let rec pass_1 width rev = function [] -> (rev, width)
    | (filename,s)::t ->
        let len = String.length filename in
        pass_1 (max width len) ((filename,len,s)::rev) t
  in
  let rev, width = pass_1 0 [] filenames in

  let rec pass_2 padded = function [] -> padded
    | (filename,len,s)::t ->
        let n = width - len in
        let pad = String.make n ' ' in
        let str = (filename ^ pad) in
        pass_2 ((str,s)::padded) t
  in
  pass_2 [] rev
;;

(* }}} *)
(* {{{ branch_mark *)

let branch_mark ~last =
  match last with
  | false :: _ -> "|--"
  | true  :: _ -> "`--"
  | [] -> "" (* only from boot_dump for base_dir *)
;;

let branch_dir_mark ~last =
  match last with
  | false :: _ -> "+-- "
  | true  :: _ -> "`-- "
  | [] -> "" (* only from boot_dump for base_dir *)
;;
(* }}} *)
(* {{{ color *)
let g = char_of_int 27 ;;
(*    red  [31;49m Normal [0m
    green  [32;49m Normal [0m
   yellow  [33;49m Normal [0m
     blue  [34;49m Normal [0m
  magenta  [35;49m Normal [0m
     cyan  [36;49m Normal [0m
    white  [37;49m Normal [0m
  default  [39;49m Normal [0m
*)
let color color_name ?(label="") str () =
  let col_code = match color_name with
  | `blue_dir  -> "01;34"
  (*
  | `yellow    -> "01;33"
  *)
  | `dark_red  -> "02;31"
  | `purple    -> "03;35"
  | `dark_grey -> "01;30"
  | `test      -> "01;32"

  |     `red -> "31;49"
  |   `green -> "32;49"
  |  `yellow -> "33;49"
  |    `blue -> "34;49"
  | `magenta -> "35;49"
  |    `cyan -> "36;49"
  |   `white -> "37;49"
  | `default -> "39;49"
  in
  Printf.sprintf "%c[%sm%s%s%c[00m" g col_code  label str  g
;;
(* }}} *)
(* {{{ human_perms *)

let human_perms ~perms =
  let string_of_octal = function
    | '1' -> "--x"
    | '2' -> "-w-"
    | '3' -> "-wx"
    | '4' -> "r--"
    | '5' -> "r-x"
    | '6' -> "rw-"
    | '7' -> "rwx"
    |  _  -> "---"
  in
  let octal_str =
    Printf.sprintf "%04o" perms
  in
  let u = string_of_octal  octal_str.[1]
  and g = string_of_octal  octal_str.[2]
  and o = string_of_octal  octal_str.[3]
  in
  (u ^ g ^ o)
;;
(* }}} *)
(* {{{ dump_total_size *)

let dump_total_size ~size ~depth ~last ~options =
  if size = 0L then () else begin
    let pad = padding ~last:(true::last) ~depth in
    let h_size = human_size ~size in

    if options.colors then begin
      Printf.printf "%s" (color `yellow (pad ^ "o--->  ") ());
      Printf.printf "%s" (color `dark_red h_size ());
    end else begin
      Printf.printf "%s" (pad ^ "o--->  ");
      Printf.printf "%s" h_size;
    end;
    Printf.printf "\n";
  end
;;
(* }}} *)
(* {{{ human_time *)

let human_time time =
  (* Unix.gmtime  UTC (Coordinated Universal Time), aka GMT *)
  let t = Unix.localtime time in (* local time zone *)
  Printf.sprintf "%d-%02d-%02d/%02d:%02d"
     (t.Unix.tm_year + 1900)
     (t.Unix.tm_mon + 1)
      t.Unix.tm_mday
      t.Unix.tm_hour
      t.Unix.tm_min
;;
(* }}} *)
(* {{{ dump_file *)

let dump_file ~name:(file_name) ~stats ~depth ~last ~options =

  let perms = stats.Unix.LargeFile.st_perm
  and size  = stats.Unix.LargeFile.st_size
  in
  let atime = stats.Unix.LargeFile.st_atime   (* Last access time *)
  and mtime = stats.Unix.LargeFile.st_mtime   (* Last modification time *)
  and ctime = stats.Unix.LargeFile.st_ctime   (* Last status change time *)
  in
  let h_size = human_size ~size in
  let pad = padding ~last ~depth in

  let show_times = true in

  if options.colors then begin
    Printf.printf "%s" (color `yellow (pad ^ (branch_mark ~last)) ());
    Printf.printf " %s" (color `purple ~label:"-" (human_perms ~perms) ());
  (*Printf.printf "%s " (color `purple ~label:"perms:" (Printf.sprintf "%03o" perms) ()); (* Octal *) *)
    Printf.printf " %s"  (color `dark_red h_size ());
    Printf.printf " %s" (Filename.basename file_name);
    if show_times then begin
      if options.mt then Printf.printf "  %s" (color `dark_grey ~label:"mt:" (human_time mtime) ());
      if options.at then Printf.printf "  %s" (color `dark_grey ~label:"at:" (human_time atime) ());
      if options.ct then Printf.printf "  %s" (color `dark_grey ~label:"ct:" (human_time ctime) ());
    end;
  end else begin
    Printf.printf "%s" (pad ^ (branch_mark ~last));
    Printf.printf " -%s" (human_perms ~perms);
  (*Printf.printf "perms:%s " (Printf.sprintf "%03o" perms); (* Octal *) *)
    Printf.printf " %s"  (h_size);
    Printf.printf " %s" (Filename.basename file_name);
    if show_times then begin
      if options.mt then Printf.printf "  %s" ("mt:" ^ (human_time mtime));
      if options.at then Printf.printf "  %s" ("at:" ^ (human_time atime));
      if options.ct then Printf.printf "  %s" ("ct:" ^ (human_time ctime));
    end;
  end;
  Printf.printf "\n";
  (size)
;;
(* Unix.stats.Unix.LargeFile.st_mtime *)
(* }}} *)
(* {{{ dump_file_list *)

let dump_file_list  ~files ~dirs ~parent_dir ~depth ~last ~options =

  let rec file_loop ~files ~dirs size_acc =
    match files, dirs with
    | [], [] | [], _ ->   (* this case matches a directory containing sub-directories *)
        dump_total_size ~size:(size_acc) ~depth ~last ~options;
        (size_acc)

    | (file_name, stats)::[], [] ->  (* this case matches a file leaf *)
        let file_size = dump_file
          ~name:(concat parent_dir file_name) ~stats ~depth ~last:(true::last) ~options
        in
        let total_size = file_size +^ size_acc in
        dump_total_size ~size:(total_size) ~depth ~last ~options;
        (total_size)

    | (file_name, stats)::tail_files, dirs ->  (* iteration loop *)
        let file_size = dump_file
          ~name:(concat parent_dir file_name) ~stats ~depth ~last:(false::last) ~options
        in
        file_loop ~files:tail_files ~dirs (file_size +^ size_acc)
  in
  file_loop ~files ~dirs 0L;
;;

(* }}} *)
(* {{{ dump_dir[_content] *)

let rec dump_dir ~name ~stats ~depth ~last ~options =
  let parent_dir = name
  (* and parent_dir_stats = stats *)
  in

  let pad = padding ~last ~depth:(pred depth) in

  if options.colors then
    Printf.printf "%s%s\n"  (* with colors *)
        (color `yellow (pad ^ (branch_dir_mark ~last)) ())
        (color `blue_dir (parent_dir ^ "/") ())
  else
    Printf.printf "%s%s%s/\n" pad (branch_dir_mark ~last) parent_dir;  (* without colors *)

  let contents = Sys.readdir parent_dir in
  let contents = Array.to_list contents in
  let rec sort c ~reg_acc ~dir_acc =
    match c with
    | [] -> (reg_acc, dir_acc)
    | name :: tl ->
        if options.hide && name.[0] = '.' then
          sort tl ~dir_acc ~reg_acc
        else
          let stats = Unix.LargeFile.lstat (concat parent_dir name) in
          match stats.Unix.LargeFile.st_kind with
          (* Accumulate directories and regular files in 2 different lists: dir_acc & reg_acc *)
          | Unix.S_REG -> sort tl ~dir_acc ~reg_acc:((name,stats) :: reg_acc)
          | Unix.S_DIR -> sort tl ~reg_acc ~dir_acc:((name,stats) :: dir_acc)
          | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK ->
              sort tl ~dir_acc ~reg_acc  (* TODO: handle these *)
  in
  let (files, dirs) = sort contents ~reg_acc:[] ~dir_acc:[] in
  let files = List.sort compare files
  and dirs = List.sort compare dirs in
  let files = right_pad files in

  (* For a better readablility, the files are printed before directories contents. *)
  let files_size = dump_file_list ~files ~dirs ~parent_dir ~depth ~last ~options in

  (* {{{ print directories *)
  let rec dir_loop dirs size_acc =
    match dirs with
    | [] -> size_acc
    | (dir_name, stats) :: [] ->   (* this case matches the last directory *)
        let content_size = dump_dir
          ~name:(concat parent_dir dir_name) ~stats ~depth:(succ depth) ~last:(true::last) ~options;
        in
        (content_size +^ size_acc)

    | (dir_name, stats) :: tail_dirs ->
        let content_size = dump_dir
          ~name:(concat parent_dir dir_name) ~stats ~depth:(succ depth) ~last:(false::last) ~options;
        in
        dir_loop tail_dirs (content_size +^ size_acc)
  in
  let rec_content_size = dir_loop dirs files_size in
  (rec_content_size)
  (* }}} *)
;;

(* }}} *)
(* {{{ TODO *(

 Add options to print the timestamps:
  --date-format      "%Y-%m-%d"
  --datetime-format  "%Y-%m-%d %H:%i:%s"

)* }}} *)
(* {{{ main *)

let strip_slash name =
  let last = pred(String.length name) in
  if name.[last] = '/'
  then String.sub name 0 last
  else name
;;

let boot_dump  base_dir ~options =
  let stats = Unix.LargeFile.lstat base_dir in
  let base_dir = strip_slash base_dir in
  let all_size = dump_dir ~name:base_dir ~stats ~depth:0 ~last:[] ~options in
  if options.colors then
    Printf.printf "\n%s\n" (color `green ~label:"Total size:" (human_size all_size) ())
  else
    Printf.printf "\nTotal size:%s\n" (human_size all_size);
;;

(* {{{ set options *)

let options_set_colors ~options =
  { colors = true;
    hide = options.hide;
    mt = options.mt;
    at = options.at;
    ct = options.ct }
;;

let options_set_mt ~options =
  { colors = options.colors;
    hide = options.hide;
    mt = true;
    at = options.at;
    ct = options.ct }
;;

let options_set_at ~options =
  { colors = options.colors;
    hide = options.hide;
    mt = options.mt;
    at = true;
    ct = options.ct }
;;

let options_set_ct ~options =
  { colors = options.colors;
    hide = options.hide;
    mt = options.mt;
    at = options.at;
    ct = true }
;;

let options_set_hide ~options =
  { colors = options.colors;
    hide = true;
    mt = options.mt;
    at = options.at;
    ct = options.ct }
;;

(* }}} *)

let () =
  let argc = Array.length Sys.argv
  and blank_options =
    {colors=false; hide=false; mt=false; at=false; ct=false}
  in

  let this_dir d yet options =
    if Sys.file_exists d
    then (boot_dump d ~options; print_newline(); succ yet, options)
    else
      match d with
      | "-mt" | "-mtime" | "--modification-time"  -> (yet, options_set_mt ~options)
      | "-at" | "-atime" | "--last-access-time"   -> (yet, options_set_at ~options)
      | "-ct" | "-ctime" | "--status-change-time" -> (yet, options_set_ct ~options)
      | "-c" | "--colors" -> (yet, options_set_colors ~options)
      | "-l" -> (yet, options_set_hide ~options)
      | "--" -> (yet, blank_options)
      | "-h" | "--help" -> usage();
      | _ -> Printf.fprintf stderr
          "Warning: '%s' is not an existing directory, nor an option\n%!" d;
          (yet, options)
  in
  let rec parse_arg argi yet options =
    if argi < argc then
      let (yet, options) = this_dir Sys.argv.(argi) yet options in
      parse_arg (succ argi) yet options
    else if yet = 0 then 
      ignore(this_dir "." 0 options) (* Sys.getcwd () *)
  in
  parse_arg 1 0 blank_options;
;;

(* }}} *)

(* vim:cindent sw=2 sts=2 ts=2 et fdm=marker
 *)
