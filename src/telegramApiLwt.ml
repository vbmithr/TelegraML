open Lwt.Infix
open TelegramApi

module Make (B : BOT) = struct
  module IO = Cohttp_lwt_unix_io
  open Cohttp_lwt_unix
  open Command

  let make_helper { name; description } = "/" ^ name ^ " - " ^ description

  let rec make_help = function
    | [] -> ""
    | cmd::cmds -> "\n" ^ make_helper cmd ^ make_help cmds

  let url = Uri.of_string "https://api.telegram.org"

  let with_path p = Uri.with_path url (Printf.sprintf "/bot/%s/%s" B.token p)

  let rec commands =
    let open Chat in
    let open Message in
    {name = "help"; description = "Show this message"; enabled = true; run = function
         (* Don't wake up users just to show a help message *)
         | {chat} -> SendMessage (chat.id, "Commands:" ^ make_help commands, true, None, `Null)} :: B.commands
  let inline = B.inline
  let callback = B.callback

  type return_obj = {
    ok: bool;
    result: Yojson.Safe.json [@default `Null];
    description: string [@default ""];
  } [@@deriving yojson]

  let get ?content_type ?buf ~f_result path =
    let headers = match content_type with
      | None -> Cohttp.Header.init ()
      | Some ct -> Cohttp.Header.init_with "Content-Type" ct
    in
    Client.get ~headers (with_path path) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json_str ->
    match return_obj_of_yojson @@ Yojson.Safe.from_string ?buf json_str with
    | Error msg -> Lwt.fail_with msg
    | Ok { ok=false; description } -> Lwt.fail_with description
    | Ok { result } -> Lwt.return @@ f_result result

  let post ?content_type ?buf ~f_result ~body path =
    let headers = match content_type with
      | None -> Cohttp.Header.init ()
      | Some ct -> Cohttp.Header.init_with "Content-Type" ct
    in
    let body = Cohttp_lwt_body.of_string body in
    Client.post ~headers ~body (with_path path) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json_str ->
    match return_obj_of_yojson @@ Yojson.Safe.from_string ?buf json_str with
    | Error msg -> Lwt.fail_with msg
    | Ok { ok=false; description } -> Lwt.fail_with description
    | Ok { result } -> Lwt.return @@ f_result result

  let get_me ?buf () = get ?buf "getMe" ~f_result:User.of_yojson

  let send_message ?buf msg =
    let body = Yojson.Safe.to_string ?buf @@ Message.Send.to_yojson msg in
    post ~content_type:"application/json" ?buf "sendMessage" ~f_result:ignore ~body

  let forward_message ?buf msg =
    let body = Yojson.Safe.to_string ?buf @@ Message.Forward.to_yojson msg in
    post ~content_type:"application/json" ?buf "forwardMessage" ~f_result:ignore ~body

  let send_chat_action ?buf ~chat_id ~action =
    let json = `Assoc ["chat_id", `Int chat_id; "action", `String action] in
    let body = Yojson.Safe.to_string ?buf json in
    post ~content_type:"application/json" ?buf "sendChatAction" ~f_result:ignore ~body

  let send_location ?buf msg =
    let body = Yojson.Safe.to_string ?buf @@ Location.Out.to_yojson msg in
    post ~content_type:"application/json" ?buf "sendLocation" ~f_result:ignore ~body

  let send_venue ?buf msg =
    let body = Yojson.Safe.to_string ?buf @@ Venue.Out.to_yojson msg in
    post ~content_type:"application/json" ?buf "sendVenue" ~f_result:ignore ~body

  let send_contact ?buf msg =
    let body = Yojson.Safe.to_string ?buf @@ Contact.Out.to_yojson msg in
    post ~content_type:"application/json" ?buf "sendContact" ~f_result:ignore ~body

  let buffer = Buffer.create 4096

  let send_photo ?(buffer=buffer) ?buf photo =
    let json = Photo.Out.to_yojson photo in
    let content_type, body = try
        let { InputFile.boundary; contents } = InputFile.multipart_body_exn ?buf ~buffer ~file_field:"photo" json in
        "multipart/form-data; boundary=" ^ boundary, contents
      with _ ->
        "application/json", Yojson.Safe.to_string ?buf @@ Photo.Out.to_yojson photo
    in
    post ~content_type ?buf "sendPhoto" ~f_result:Photo.Out.of_yojson ~body

  let send_audio ?(buffer=buffer) ?buf audio =
    let json = Audio.Out.to_yojson audio in
    let content_type, body = try
        let { InputFile.boundary; contents } = InputFile.multipart_body_exn ?buf ~buffer ~file_field:"audio" json in
        "multipart/form-data; boundary=" ^ boundary, contents
      with _ ->
        "application/json", Yojson.Safe.to_string ?buf @@ Audio.Out.to_yojson audio
    in
    post ~content_type ?buf "sendAudio" ~f_result:Audio.Out.of_yojson ~body

  let send_document ?(buffer=buffer) ?buf doc =
    let json = Document.Out.to_yojson doc in
    let content_type, body = try
        let { InputFile.boundary; contents } = InputFile.multipart_body_exn ?buf ~buffer ~file_field:"document" json in
        "multipart/form-data; boundary=" ^ boundary, contents
      with _ ->
        "application/json", Yojson.Safe.to_string ?buf @@ Document.Out.to_yojson doc
    in
    post ~content_type ?buf "sendDocument" ~f_result:Document.Out.of_yojson ~body

  let send_sticker ?(buffer=buffer) ?buf sticker =
    let json = Sticker.Out.to_yojson sticker in
    let content_type, body = try
        let { InputFile.boundary; contents } = InputFile.multipart_body_exn ?buf ~buffer ~file_field:"sticker" json in
        "multipart/form-data; boundary=" ^ boundary, contents
      with _ ->
        "application/json", Yojson.Safe.to_string ?buf @@ Sticker.Out.to_yojson sticker
    in
    post ~content_type ?buf "sendSticker" ~f_result:Sticker.Out.of_yojson ~body

  let send_video ?(buffer=buffer) ?buf video =
    let json = Video.Out.to_yojson video in
    let content_type, body = try
        let { InputFile.boundary; contents } = InputFile.multipart_body_exn ?buf ~buffer ~file_field:"video" json in
        "multipart/form-data; boundary=" ^ boundary, contents
      with _ ->
        "application/json", Yojson.Safe.to_string ?buf @@ Video.Out.to_yojson video
    in
    post ~content_type ?buf "sendVideo" ~f_result:Video.Out.of_yojson ~body

  let send_voice ?(buffer=buffer) ?buf voice =
    let json = Voice.Out.to_yojson voice in
    let content_type, body = try
        let { InputFile.boundary; contents } = InputFile.multipart_body_exn ?buf ~buffer ~file_field:"video" json in
        "multipart/form-data; boundary=" ^ boundary, contents
      with _ ->
        "application/json", Yojson.Safe.to_string ?buf @@ Voice.Out.to_yojson voice
    in
    post ~content_type ?buf "sendVideo" ~f_result:Voice.Out.of_yojson ~body

  type user_profile_photos_args = {
    user_id: int;
    offset: int [@default 0];
    limit: int [@default 0];
  } [@@deriving create, yojson]

  let get_user_profile_photos ?buf ?offset ?limit user_id =
    let body = create_user_profile_photos_args ?offset ?limit ~user_id () |> user_profile_photos_args_to_yojson |> Yojson.Safe.to_string ?buf in
    post ~content_type:"application/json" ?buf "getUserProfilePhotos" ~f_result:UserProfilePhotos.of_yojson ~body

  let get_file ?buf file_id =
    let body = `Assoc ["file_id", `String file_id] |> Yojson.Safe.to_string ?buf in
    post ~content_type:"application/json" ?buf "getFile" ~f_result:File.of_yojson ~body

  let kick_chat_member ?buf ~chat_id ~user_id () =
    let body = `Assoc ["chat_id", `Int chat_id; "user_id", `Int user_id] |> Yojson.Safe.to_string ?buf in
    post ~content_type:"application/json" ?buf "kickChatMember" ~f_result:ignore ~body

  let leave_chat ?buf chat_id =
    let body = `Assoc ["chat_id", `Int chat_id] |> Yojson.Safe.to_string ?buf in
    post ~content_type:"application/json" ?buf "leaveChat" ~f_result:ignore ~body

  let unban_chat_member ?buf ~chat_id ~user_id () =
    let body = `Assoc ["chat_id", `Int chat_id; "user_id", `Int user_id] |> Yojson.Safe.to_string ?buf in
    post ~content_type:"application/json" ?buf "unbanChatMember" ~f_result:ignore ~body

  let get_chat ?buf chat_id =
    let body = `Assoc ["chat_id", `Int chat_id] |> Yojson.Safe.to_string ?buf in
    post ~content_type:"application/json" ?buf "getChat" ~f_result:Chat.of_yojson ~body

  let get_chat_administrators ?buf chat_id =
    let body = `Assoc ["chat_id", `Int chat_id] |> Yojson.Safe.to_string ?buf in
    let f_result = function
      | `List members -> Ok (ListLabels.fold_left members ~init:[] ~f:(fun a m -> match ChatMember.of_yojson m with Ok m -> m::a | Error _ -> a))
      | #Yojson.Safe.json -> Error "f_result"
    in
    post ~content_type:"application/json" ?buf "getChatAdministrators" ~f_result ~body

  let get_chat_members_count ?buf chat_id =
    let body = `Assoc ["chat_id", `Int chat_id] |> Yojson.Safe.to_string ?buf in
    let f_result = function `Int i -> Ok i | #Yojson.Safe.json -> Error "f_result" in
    post ~content_type:"application/json" ?buf "getChatMembersCount" ~f_result ~body

  let get_chat_member ?buf ~chat_id ~user_id () =
    let body = `Assoc ["chat_id", `Int chat_id; "user_id", `Int user_id] |> Yojson.Safe.to_string ?buf in
    post ~content_type:"application/json" ?buf "getChatMember" ~f_result:ChatMember.of_yojson ~body

  type answer_callback_query_args = {
    callback_query_id: int;
    show_alert: bool [@default false];
    text: string [@default ""];
  } [@@deriving create, yojson]

  let answer_callback_query ?buf ?text ?show_alert callback_query_id =
    let body = create_answer_callback_query_args ?text ?show_alert ~callback_query_id () |> answer_callback_query_args_to_yojson |> Yojson.Safe.to_string ?buf in
    post ~content_type:"application/json" ?buf "answerCallbackQuery" ~f_result:ignore ~body

  let answer_inline_query ~inline_query_id ~results ?(cache_time=None) ?(is_personal=None) ?(next_offset=None) () =
    let results' = List.map (fun result -> InlineQuery.Out.prepare result) results in
    let body = `Assoc ([("inline_query_id", `String inline_query_id);
                        ("results", `List results')] +? ("cache_time", this_int <$> cache_time)
                                                     +? ("is_personal", this_bool <$> is_personal)
                                                     +? ("next_offset", this_string <$> next_offset)) |> Yojson.Safe.to_string in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "answerInlineQuery")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    Lwt.return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success ()
    | _ -> Result.Failure ((fun x -> print_endline x; x) @@ the_string @@ get_field "description" obj)

  let edit_message_text ?(chat_id=None) ?(message_id=None) ?(inline_message_id=None) ~text ~parse_mode ~disable_web_page_preview ~reply_markup () =
    let ids = match chat_id, message_id, inline_message_id with
      | (Some c, Some m, _) -> ["chat_id", `String c; "message_id", `Int m]
      | (_, _, Some i) -> ["inline_message_id", `String i]
      | (_, _, _) -> raise (ApiException "editMessageText requires either a chat_id and message_id or inline_message_id") in
    let body = `Assoc ([("text", `String text);
                        ("disable_web_page_preview", `Bool disable_web_page_preview)] @ ids
                       +? ("parse_mode", this_string <$> (ParseMode.string_of_parse_mode <$> parse_mode))
                       +? ("reply_markup", ReplyMarkup.prepare <$> reply_markup)) |> Yojson.Safe.to_string in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "editMessageText")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    Lwt.return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success ()
    | _ -> Result.Failure ((fun x -> print_endline x; x) @@ the_string @@ get_field "description" obj)

  let edit_message_caption ?(chat_id=None) ?(message_id=None) ?(inline_message_id=None) ~caption ~reply_markup () =
    let id = match chat_id, message_id, inline_message_id with
      | (None, None, None) -> raise (ApiException "editMessageText requires either a chat_id, message_id, or inline_message_id")
      | (Some c, _, _) -> ("chat_id", `String c)
      | (_, Some m, _) -> ("message_id", `Int m)
      | (_, _, Some i) -> ("inline_message_id", `String i) in
    let body = `Assoc ([("caption", `String caption);
                        id] +? ("reply_markup", ReplyMarkup.prepare <$> reply_markup)) |> Yojson.Safe.to_string in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "editMessageCaption")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    Lwt.return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success ()
    | _ -> Result.Failure ((fun x -> print_endline x; x) @@ the_string @@ get_field "description" obj)

  let edit_message_reply_markup ?(chat_id=None) ?(message_id=None) ?(inline_message_id=None) ~reply_markup () =
    let id = match chat_id, message_id, inline_message_id with
      | (None, None, None) -> raise (ApiException "editMessageText requires either a chat_id, message_id, or inline_message_id")
      | (Some c, _, _) -> ("chat_id", `String c)
      | (_, Some m, _) -> ("message_id", `Int m)
      | (_, _, Some i) -> ("inline_message_id", `String i) in
    let body = `Assoc ([id] +? ("reply_markup", ReplyMarkup.prepare <$> reply_markup)) |> Yojson.Safe.to_string in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "editMessageReplyMarkup")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    Lwt.return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success ()
    | _ -> Result.Failure ((fun x -> print_endline x; x) @@ the_string @@ get_field "description" obj)

  let get_updates =
    Client.get (Uri.of_string (url ^ "getUpdates")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    Lwt.return @@ match get_field "ok" obj with
    | `Bool true -> Result.Success (List.map Update.read @@ the_list @@ get_field "result" obj)
    | _ -> Result.Failure (the_string @@ get_field "description" obj)

  let offset = ref 0
  let clear_update () =
    let json = `Assoc [("offset", `Int !offset);
                       ("limit", `Int 0)] in
    let body = Yojson.Safe.to_string json in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "getUpdates")) >>= fun _ ->
    Lwt.return ()

  let peek_update =
    let open Update in
    let json = `Assoc [("offset", `Int 0);
                       ("limit", `Int 1)] in
    let body = Yojson.Safe.to_string json in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "getUpdates")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    let open Result in
    Lwt.Lwt.return @@ match get_field "ok" obj with
    | `Bool true -> Update.read <$> (hd_ @@ the_list @@ get_field "result" obj)
    | _ -> Failure (the_string @@ get_field "description" obj)

  let rec pop_update ?(run_cmds=true) () =
    let open Update in
    let json = `Assoc [("offset", `Int !offset);
                       ("limit", `Int 1)] in
    let body = Yojson.Safe.to_string json in
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Client.post ~headers ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string (url ^ "getUpdates")) >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let obj = Yojson.Safe.from_string json in
    match get_field "ok" obj with
    | `Bool true -> begin
        let open Result in
        (* Get the update number for the latest message (the head of the list), if it exists *)
        let update = Update.read <$> (hd_ @@ the_list @@ get_field "result" obj) in
        (* Set the offset to either: the current offset OR the latest update + 1, if one exists *)
        offset := default !offset ((fun update -> get_id update + 1) <$> update);
        let open Lwt in
        (* Clear the last update and then *)
        clear_update () >>= fun () -> begin
          let open Message in
          (* Let's assume that only one of these events can ever be present in a message at once... *)
          begin match (run_cmds, update) with
            | (true, Result.Success (Message (_, {chat; new_chat_member = Some user}))) ->
              evaluator @@ B.new_chat_member chat user
            | (true, Result.Success (Message (_, {chat; left_chat_member = Some user}))) ->
              evaluator @@ B.left_chat_member chat user
            | (true, Result.Success (Message (_, {chat; new_chat_title = Some title}))) ->
              evaluator @@ B.new_chat_title chat title
            | (true, Result.Success (Message (_, {chat; new_chat_photo = Some photo}))) ->
              evaluator @@ B.new_chat_photo chat photo
            | (true, Result.Success (Message (_, {chat; delete_chat_photo = Some true}))) ->
              evaluator @@ B.delete_chat_photo chat
            | (true, Result.Success (Message (_, {chat; group_chat_created = Some true}))) ->
              evaluator @@ B.group_chat_created chat
            | (true, Result.Success (Message (_, {chat; supergroup_chat_created = Some true}))) ->
              evaluator @@ B.supergroup_chat_created chat
            | (true, Result.Success (Message (_, {chat; channel_chat_created = Some true}))) ->
              evaluator @@ B.channel_chat_created chat
            | (true, Result.Success (Message (_, {chat; migrate_to_chat_id = Some chat_id}))) ->
              evaluator @@ B.migrate_to_chat_id chat chat_id
            | (true, Result.Success (Message (_, {chat; migrate_from_chat_id = Some chat_id}))) ->
              evaluator @@ B.migrate_from_chat_id chat chat_id
            | (true, Result.Success (Message (_, {chat; pinned_message = Some message}))) ->
              evaluator @@ B.pinned_message chat message
            | _ -> Lwt.return ()
          end |> ignore;
          (* If command execution is enabled: if there's an update and it has an inline_query field *)
          match run_cmds, update with
          | (true, Result.Success (InlineQuery (id, inline_query) as update)) -> begin
              (* Run the evaluator on the inline_query of the update and throw away the result *)
              ignore @@ evaluator @@ inline inline_query;
              (* And then return just the ID of the last update if it succeeded *)
              Lwt.return @@ Result.Success update
            end
          | (true, Result.Success (CallbackQuery (id, callback_query) as update)) -> begin
              (* Run the evaluator on the inline_query of the update and throw away the result *)
              ignore @@ evaluator @@ callback callback_query;
              (* And then return just the ID of the last update if it succeeded *)
              Lwt.return @@ Result.Success update
            end
          (* If command execution is enabled: if there's an update and it's a command... *)
          | (true, Result.Success update) when Command.is_command update -> begin
              (* Run the evaluator on the result of the command, if the update exists *)
              ignore @@ evaluator @@ Command.read_update B.command_postfix update commands;
              (* And then return just the ID of the last update if it succeeded *)
              Lwt.return @@ Result.Success update
            end
          | _ -> Lwt.return update (* Otherwise, return the last update *)
        end
      end
    | _ -> Lwt.return @@ Result.Failure (the_string @@ get_field "description" obj)

  and evaluator =
    let (>>) x y = x >>= fun _ -> y in
    let dispose x = x >> Lwt.return ()
    and eval f x = x >>= fun y -> evaluator (f y)
    and identify : 'a. (?chat_id:string option -> ?message_id:int option -> ?inline_message_id:string option -> 'a) -> [`ChatMessageId of string * int | `InlineMessageId of string] -> 'a =
      fun f id -> match id with
        | `ChatMessageId (c, m) -> f ~chat_id:(Some c) ~message_id:(Some m) ~inline_message_id:None
        | `InlineMessageId i -> f ~chat_id:None ~message_id:None ~inline_message_id:(Some i) in
    function
    | Nothing -> Lwt.return ()
    | GetMe f -> get_me |> eval f
    | SendMessage (chat_id, text, disable_notification, reply_to, reply_markup) -> send_message ~chat_id ~text ~disable_notification ~reply_to ~reply_markup |> dispose
    | ForwardMessage (chat_id, from_chat_id, disable_notification, message_id) -> forward_message ~chat_id ~from_chat_id ~disable_notification ~message_id |> dispose
    | SendChatAction (chat_id, action) -> send_chat_action ~chat_id ~action |> dispose
    | SendPhoto (chat_id, photo, caption, disable_notification, reply_to, reply_markup, f) -> send_photo ~chat_id ~photo ~caption ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendPhoto (chat_id, photo, caption, disable_notification, reply_to, reply_markup) -> resend_photo ~chat_id ~photo ~caption ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendAudio (chat_id, audio, performer, title, disable_notification, reply_to, reply_markup, f) -> send_audio ~chat_id ~audio ~performer ~title ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendAudio (chat_id, audio, performer, title, disable_notification, reply_to, reply_markup) -> resend_audio ~chat_id ~audio ~performer ~title ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendDocument (chat_id, document, disable_notification, reply_to, reply_markup, f) -> send_document ~chat_id ~document ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendDocument (chat_id, document, disable_notification, reply_to, reply_markup) -> resend_document ~chat_id ~document ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendSticker (chat_id, sticker, disable_notification, reply_to, reply_markup, f) -> send_sticker ~chat_id ~sticker ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendSticker (chat_id, sticker, disable_notification, reply_to, reply_markup) -> resend_sticker ~chat_id ~sticker ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendVideo (chat_id, video, duration, caption, disable_notification, reply_to, reply_markup, f) -> send_video ~chat_id ~video ~duration ~caption ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendVideo (chat_id, video, duration, caption, disable_notification, reply_to, reply_markup) -> resend_video ~chat_id ~video ~duration ~caption ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendVoice (chat_id, voice, disable_notification, reply_to, reply_markup, f) -> send_voice ~chat_id ~voice ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendVoice (chat_id, voice, disable_notification, reply_to, reply_markup) -> resend_voice ~chat_id ~voice ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendLocation (chat_id, latitude, longitude, disable_notification, reply_to, reply_markup) -> send_location ~chat_id ~latitude ~longitude ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendVenue (chat_id, latitude, longitude, title, address, foursquare_id, disable_notification, reply_to, reply_markup) -> send_venue ~chat_id ~latitude ~longitude ~title ~address ~foursquare_id ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendContact (chat_id, phone_number, first_name, last_name, disable_notification, reply_to, reply_markup) -> send_contact ~chat_id ~phone_number ~first_name ~last_name ~disable_notification ~reply_to ~reply_markup |> dispose
    | GetUserProfilePhotos (user_id, offset, limit, f) -> get_user_profile_photos ~user_id ~offset ~limit |> eval f
    | GetFile (file_id, f) -> get_file ~file_id |> eval f
    | GetFile' (file_id, f) -> get_file' ~file_id |> eval f
    | DownloadFile (file, f) -> download_file ~file |> eval f
    | KickChatMember (chat_id, user_id) -> kick_chat_member ~chat_id ~user_id |> dispose
    | LeaveChat chat_id -> leave_chat ~chat_id |> dispose
    | UnbanChatMember (chat_id, user_id) -> unban_chat_member ~chat_id ~user_id |> dispose
    | GetChat (chat_id, f) -> get_chat ~chat_id |> eval f
    | GetChatAdministrators (chat_id, f) -> get_chat_administrators ~chat_id |> eval f
    | GetChatMembersCount (chat_id, f) -> get_chat_members_count ~chat_id |> eval f
    | GetChatMember (chat_id, user_id, f) -> get_chat_member ~chat_id ~user_id |> eval f
    | AnswerCallbackQuery (callback_query_id, text, show_alert) -> answer_callback_query ~callback_query_id ~text ~show_alert () |> dispose
    | AnswerInlineQuery (inline_query_id, results, cache_time, is_personal, next_offset) -> answer_inline_query ~inline_query_id ~results ~cache_time ~is_personal ~next_offset () |> dispose
    | EditMessageText (id, text, parse_mode, disable_web_page_preview, reply_markup) ->
      (identify edit_message_text id) ~text ~parse_mode ~disable_web_page_preview ~reply_markup () |> dispose
    | EditMessageCaption (id, caption, reply_markup) ->
      (identify edit_message_caption id) ~caption ~reply_markup () |> dispose
    | EditMessageReplyMarkup (id, reply_markup) ->
      (identify edit_message_reply_markup id) ~reply_markup () |> dispose
    | GetUpdates f -> get_updates |> eval f
    | PeekUpdate f -> peek_update |> eval f
    | PopUpdate (run_cmds, f) -> pop_update ~run_cmds () |> eval f
    | Chain (first, second) -> evaluator first >> evaluator second

  let run ?(log=true) () =
    let process = function
      | Result.Success _ -> Lwt.return ()
      | Result.Failure e ->
        if log && e <> "Could not get head" then (* Ignore spam *)
          Lwt_io.printl e
        else Lwt.return () in
    let rec loop () =
      pop_update ~run_cmds:true () >>= process >>= loop in
    while true do (* Recover from errors if an exception is thrown *)
      try Lwt_main.run @@ loop ()
      with _ -> ()
    done
end
