open Rresult
open Astring
open Bos.OS

module InputFile = struct
  type t = {
    boundary: string;
    contents: string;
  } [@@deriving create]

  let multipart_body_exn ?(boundary="9e7a1f74-cfff-4c5a-af8a-251e7acd58cf") ?(buffer=Buffer.create 4096) ?buf ~file_field json =
    let boundary = "--" ^ boundary in
    let ending = boundary ^ "--" in
    let break = "\r\n" in
    let add_field_block name value =
      Buffer.add_string buffer boundary;
      Buffer.add_string buffer break;
      Buffer.add_string buffer "Content-Disposition: form-data; name=\"";
      Buffer.add_string buffer name;
      Buffer.add_string buffer "\"";
      Buffer.add_string buffer break;
      Buffer.add_string buffer break;
      Buffer.add_string buffer value;
      Buffer.add_string buffer break
    in
    let add_file_block name path_str file_contents =
      Buffer.add_string buffer boundary;
      Buffer.add_string buffer break;
      Buffer.add_string buffer "Content-Disposition: form-data; name=\"";
      Buffer.add_string buffer name;
      Buffer.add_string buffer "\"; filename=\"";
      Buffer.add_string buffer path_str;
      Buffer.add_string buffer "\"";
      Buffer.add_string buffer break;
      Buffer.add_string buffer "Content-Type: ";
      Buffer.add_string buffer @@ Magic_mime.lookup path_str;
      Buffer.add_string buffer break;
      Buffer.add_string buffer break;
      Buffer.add_string buffer file_contents;
      Buffer.add_string buffer break
    in
    match json with
    | `Assoc fields ->
      Buffer.clear buffer;
      let fields = ListLabels.map fields ~f:(fun (k, v) -> k, Yojson.Safe.to_string ?buf v) in
      ListLabels.iter fields ~f:begin fun (k, v) ->
        if k = file_field then begin
          let open R.Infix in
          match
            Fpath.of_string v >>= File.read >>| fun file_contents ->
            add_file_block k v file_contents
          with
          | Ok () -> ()
          | _ -> invalid_arg "multipart_body: invalid file"
        end
        else add_field_block k v
      end;
      Buffer.add_string buffer ending;
      create ~boundary ~contents:(Buffer.contents buffer) ()
    | #Yojson.Safe.json -> invalid_arg "multipart_body: invalid json"
end

module User = struct
  type t = {
    id         : int;
    first_name : string;
    last_name  : string [@default ""];
    username   : string [@default ""];
  } [@@deriving create, yojson]
end

module Chat = struct
  type t = {
    id         : int;
    typ : string [@key "type"];
    title      : string [@default ""];
    username   : string [@default ""];
    first_name : string [@default ""];
    last_name  : string [@default ""];
  } [@@deriving create, yojson]
end

module MessageEntity = struct
  type t = {
    typ : string [@key "type"];
    offset : int;
    length : int;
    url: string [@default ""];
    user: User.t option [@default None];
  } [@@deriving create, yojson]
end

module KeyboardButton = struct
  type t = {
    text             : string;
    request_contact  : bool [@default false];
    request_location : bool [@default false];
  } [@@deriving create, yojson]
end

module InlineKeyboardButton = struct
  type t = {
    text                : string;
    url                 : string [@default ""];
    callback_data       : string [@default ""];
    switch_inline_query : string [@default ""];
  } [@@deriving create, yojson]
end

module ReplyMarkup = struct
  type reply_keyboard_markup = {
    keyboard          : KeyboardButton.t list list;
    resize_keyboard   : bool [@default false];
    one_time_keyboard : bool [@default false];
    selective         : bool [@default false];
  } [@@deriving create, yojson]

  type inline_keyboard_markup = {
    inline_keyboard : InlineKeyboardButton.t list list
  } [@@deriving create, yojson]

  type selective = {
    force_reply: bool [@default false];
    hide_keyboard: bool [@default false];
    selective : bool;
  } [@@deriving create, yojson]
end

module Photo = struct
  type t = {
    file_id   : string;
    width     : int;
    height    : int;
    file_size : int [@default 0];
  } [@@deriving create, yojson]

  module Out = struct
    type t = {
      chat_id              : int;
      photo                : string;
      caption              : string [@default ""];
      disable_notification : bool [@default false];
      reply_to_message_id  : int option [@default None];
      reply_markup         : Yojson.Safe.json [@default `Null];
    } [@@deriving create, yojson]
  end
end

module Audio = struct
  type t = {
    file_id   : string;
    duration  : int;
    performer : string;
    title     : string;
    mime_type : string;
    file_size : int;
  } [@@deriving create, yojson]

  module Out = struct
    type t = {
      chat_id              : int;
      audio                : string;
      duration             : int [@default 0];
      performer            : string [@default ""];
      title                : string [@default ""];
      disable_notification : bool [@default false];
      reply_to_message_id  : int option [@default None];
      reply_markup         : Yojson.Safe.json [@default `Null];
    } [@@deriving create, yojson]
  end
end

module Document = struct
  type t = {
    file_id   : string;
    thumb     : Photo.t option [@default None];
    file_name : string [@default ""];
    mime_type : string [@default ""];
    file_size : int [@default 0];
  } [@@deriving create, yojson]

  module Out = struct
    type t = {
      chat_id              : int;
      document             : string;
      disable_notification : bool [@default false];
      reply_to_message_id  : int option [@default None];
      reply_markup         : Yojson.Safe.json [@default `Null]
    } [@@deriving create, yojson]
  end
end

module Sticker = struct
  type t = {
    file_id   : string;
    width     : int;
    height    : int;
    thumb     : Photo.t option [@default None];
    emoji     : string [@default ""];
    file_size : int [@default 0];
  } [@@deriving create, yojson]

  module Out = struct
    type t = {
      chat_id              : int;
      sticker              : string;
      disable_notification : bool [@default false];
      reply_to_message_id  : int option [@default None];
      reply_markup         : Yojson.Safe.json [@default `Null]
    } [@@deriving create, yojson]
  end
end

module Video = struct
  type t = {
    file_id   : string;
    width     : int;
    height    : int;
    duration  : int;
    thumb     : Photo.t option;
    mime_type : string [@default ""];
    file_size : int [@default 0];
  } [@@deriving create, yojson]

  module Out = struct
    type t = {
      chat_id              : int;
      video                : string;
      duration             : int [@default 0];
      caption              : string [@default ""];
      disable_notification : bool [@default false];
      reply_to_message_id  : int option [@default None];
      reply_markup         : Yojson.Safe.json [@default `Null]
    } [@@deriving create, yojson]
  end
end

module Voice = struct
  type t = {
    file_id   : string;
    duration  : int;
    mime_type : string [@default ""];
    file_size : int [@default 0];
  } [@@deriving create, yojson]

  module Out = struct
    type t = {
      chat_id              : int;
      voice                : string;
      duration             : int [@default 0];
      disable_notification : bool [@default false];
      reply_to_message_id  : int option [@default None];
      reply_markup         : Yojson.Safe.json [@default `Null]
    } [@@deriving create, yojson]
  end
end

module Contact = struct
  type t = {
    phone_number : string;
    first_name   : string;
    last_name    : string [@default ""];
    user_id      : int [@default 0];
  } [@@deriving create, yojson]

  module Out = struct
    type t = {
      chat_id              : int;
      phone_number         : string;
      first_name           : string;
      last_name            : string [@default ""];
      disable_notification : bool [@default false];
      reply_to_message_id  : int option [@default None];
      reply_markup         : Yojson.Safe.json [@default `Null]
    } [@@deriving create, yojson]
  end
end

module Location = struct
  type t = {
    longitude : float;
    latitude  : float
  } [@@deriving create, yojson]

  module Out = struct
    type t = {
      chat_id              : int;
      latitude             : float;
      longitude            : float;
      disable_notification : bool [@default false];
      reply_to_message_id  : int option [@default None];
      reply_markup         : Yojson.Safe.json [@default `Null]
    } [@@deriving create, yojson]
  end
end

module Venue = struct
  type t = {
    location      : Location.t;
    title         : string;
    address       : string;
    foursquare_id : string [@default ""];
  } [@@deriving create, yojson]

  module Out = struct
    type t = {
      chat_id              : int;
      latitude             : float;
      longitude            : float;
      title                : string;
      address              : string;
      foursquare_id        : string [@default ""];
      disable_notification : bool [@default false];
      reply_to_message_id  : int option [@default None];
      reply_markup         : Yojson.Safe.json [@default `Null]
    } [@@deriving create, yojson]
  end
end

module UserProfilePhotos = struct
  type t = {
    total_count : int;
    photos      : Photo.t list list
  } [@@deriving create, yojson]
end

module Message = struct
  type t = {
    message_id              : int;
    from                    : User.t option [@default None];
    date                    : int;
    chat                    : Chat.t;
    forward_from            : User.t option [@default None];
    forward_from_chat       : Chat.t option [@default None];
    forward_date            : int [@default 0];
    reply_to_message        : t option [@default None];
    edit_date               : int [@default 0];
    text                    : string [@default ""];
    entities                : MessageEntity.t list [@default []];
    audio                   : Audio.t option [@default None];
    document                : Document.t option [@default None];
    photo                   : Photo.t list [@default []];
    sticker                 : Sticker.t option [@default None];
    video                   : Video.t option [@default None];
    voice                   : Voice.t option [@default None];
    caption                 : string [@default ""];
    contact                 : Contact.t option [@default None];
    location                : Location.t option [@default None];
    venue                   : Venue.t option [@default None];
    new_chat_member         : User.t option [@default None];
    left_chat_member        : User.t option [@default None];
    new_chat_title          : string [@default ""];
    new_chat_photo          : Photo.t list [@default []];
    delete_chat_photo       : bool [@default false];
    group_chat_created      : bool [@default false];
    supergroup_chat_created : bool [@default false];
    channel_chat_created    : bool [@default false];
    migrate_to_chat_id      : int option [@default None];
    migrate_from_chat_id    : int option [@default None];
    pinned_message          : t option [@default None];
  } [@@deriving create, yojson]

  module Send = struct
    type t = {
      chat_id: int;
      text: string;
      parse_mode: string [@default ""];
      disable_web_page_preview: bool [@default false];
      disable_notification: bool [@default false];
      reply_to_msg_id: int option [@default None];
      reply_markup: Yojson.Safe.json [@default `Null];
    } [@@deriving create, yojson]
  end

  module Forward = struct
    type t = {
      chat_id: int;
      from_chat_id: int;
      message_id: int;
      disable_notification: bool [@default false];
    } [@@deriving create, yojson]
  end
end

module File = struct
  type t = {
    file_id   : string;
    file_size : int [@default 0];
    file_path : string [@default ""];
  } [@@deriving create, yojson]
end

module CallbackQuery = struct
  type t = {
    id                : string;
    from              : User.t;
    message           : Message.t option;
    inline_message_id : string [@default ""];
    data              : string;
  } [@@deriving create, yojson]
end

module ChatMember = struct
  type t = {
    user : User.t;
    status : string;
  } [@@deriving create, yojson]
end

module InputTextMessageContent = struct
  type t = {
    message_text             : string;
    parse_mode               : string [@default ""];
    disable_web_page_preview : bool [@default false];
  } [@@deriving create, yojson]
end

module Inline = struct
  type query = {
    id       : string;
    from     : User.t;
    location : Location.t option [@default None];
    query    : string;
    offset   : string;
  } [@@deriving create, yojson]

  type result = {
    result_id     : string;
    from          : User.t;
    location      : Location.t option [@default None];
    inline_msg_id : string [@default ""];
    query         : string
  } [@@deriving create, yojson]

  module Out = struct
    type article = {
      id                       : string;
      title                    : string;
      input_message_content    : Yojson.Safe.json [@default `Null];
      reply_markup             : Yojson.Safe.json [@default `Null];
      url                      : string [@default ""];
      hide_url                 : bool [@default false];
      description              : string [@default ""];
      thumb_url                : string [@default ""];
      thumb_width              : int [@default 0];
      thumb_height             : int [@default 0];
    }

    type photo = {
      id                       : string;
      photo_url                : string;
      thumb_url                : string;
      photo_width              : int [@default 0];
      photo_height             : int [@default 0];
      title                    : string [@default ""];
      description              : string [@default ""];
      caption                  : string [@default ""];
      reply_markup             : Yojson.Safe.json [@default `Null];
      input_message_content    : Yojson.Safe.json [@default `Null];
    }

    type gif = {
      id                       : string;
      gif_url                  : string;
      gif_width                : int [@default 0];
      gif_height               : int [@default 0];
      thumb_url                : string;
      title                    : string [@default ""];
      caption                  : string [@default ""];
      reply_markup             : Yojson.Safe.json [@default `Null];
      input_message_content    : Yojson.Safe.json [@default `Null];
    }

    type mpeg4gif = {
      id                       : string;
      mpeg4_url                : string;
      mpeg4_width              : int [@default 0];
      mpeg4_height             : int [@default 0];
      thumb_url                : string;
      title                    : string [@default ""];
      caption                  : string [@default ""];
      reply_markup             : Yojson.Safe.json [@default `Null];
      input_message_content    : Yojson.Safe.json [@default `Null];
    }

    type video = {
      id                       : string;
      video_url                : string;
      mime_type                : string;
      thumb_url                : string;
      title                    : string;
      caption                  : string [@default ""];
      video_width              : int [@default 0];
      video_height             : int [@default 0];
      video_duration           : int [@default 0];
      description              : string [@default ""];
      reply_markup             : Yojson.Safe.json [@default `Null];
      input_message_content    : Yojson.Safe.json [@default `Null];
    }

    type audio = {
      id                    : string;
      audio_url             : string;
      title                 : string;
      performer             : string [@default ""];
      audio_duration        : int [@default 0];
      reply_markup          : Yojson.Safe.json [@default `Null];
      input_message_content : Yojson.Safe.json [@default `Null];
    }

    type voice = {
      id                    : string;
      voice_url             : string;
      title                 : string;
      voice_duration        : int [@default 0];
      reply_markup          : Yojson.Safe.json [@default `Null];
      input_message_content : Yojson.Safe.json [@default `Null];
    }

    type document = {
      id                    : string;
      title                 : string;
      caption               : string [@default ""];
      document_url          : string;
      mime_type             : string;
      description           : string [@default ""];
      reply_markup          : Yojson.Safe.json [@default `Null];
      input_message_content : Yojson.Safe.json [@default `Null];
      thumb_url             : string [@default ""];
      thumb_width           : int [@default 0];
      thumb_height          : int [@default 0];
    }

    type location = {
      id                    : string;
      latitude              : float;
      longitude             : float;
      title                 : string;
      reply_markup          : Yojson.Safe.json [@default `Null];
      input_message_content : Yojson.Safe.json [@default `Null];
      thumb_url             : string [@default ""];
      thumb_width           : int [@default 0];
      thumb_height          : int [@default 0];
    }

    type venue = {
      id                    : string;
      latitude              : float;
      longitude             : float;
      title                 : string;
      address               : string;
      foursquare_id         : string [@default ""];
      reply_markup          : Yojson.Safe.json [@default `Null];
      input_message_content : Yojson.Safe.json [@default `Null];
      thumb_url             : string [@default ""];
      thumb_width           : int [@default 0];
      thumb_height          : int [@default 0];
    }

    type contact = {
      id                    : string;
      phone_number          : string;
      first_name            : string;
      last_name             : string [@default ""];
      reply_markup          : Yojson.Safe.json [@default `Null];
      input_message_content : Yojson.Safe.json [@default `Null];
      thumb_url             : string [@default ""];
      thumb_width           : int [@default 0];
      thumb_height          : int [@default 0];
    }

    type cached_photo = {
      id                       : string;
      photo_file_id            : string;
      title                    : string [@default ""];
      description              : string [@default ""];
      caption                  : string [@default ""];
      reply_markup             : Yojson.Safe.json [@default `Null];
      input_message_content    : Yojson.Safe.json [@default `Null];
    }

    type cached_gif = {
      id                       : string;
      gif_file_id              : string;
      title                    : string [@default ""];
      caption                  : string [@default ""];
      reply_markup             : Yojson.Safe.json [@default `Null];
      input_message_content    : Yojson.Safe.json [@default `Null];
    }

    type cached_mpeg4gif = {
      id                       : string;
      mpeg4_file_id            : string;
      title                    : string [@default ""];
      caption                  : string [@default ""];
      reply_markup             : Yojson.Safe.json [@default `Null];
      input_message_content    : Yojson.Safe.json [@default `Null];
    }

    type cached_sticker = {
      id                    : string;
      sticker_file_id       : string;
      reply_markup          : Yojson.Safe.json [@default `Null];
      input_message_content : Yojson.Safe.json [@default `Null];
    }

    type cached_document = {
      id                    : string;
      title                 : string;
      document_file_id      : string;
      description           : string [@default ""];
      caption               : string [@default ""];
      reply_markup          : Yojson.Safe.json [@default `Null];
      input_message_content : Yojson.Safe.json [@default `Null];
    }

    type cached_video = {
      id                       : string;
      video_file_id            : string;
      title                    : string;
      description              : string [@default ""];
      caption                  : string [@default ""];
      reply_markup             : Yojson.Safe.json [@default `Null];
      input_message_content    : Yojson.Safe.json [@default `Null];
    }

    type cached_voice = {
      id                    : string;
      voice_file_id         : string;
      title                 : string;
      reply_markup          : Yojson.Safe.json [@default `Null];
      input_message_content : Yojson.Safe.json [@default `Null];
    }

    type cached_audio = {
      id                    : string;
      audio_file_id         : string;
      reply_markup          : Yojson.Safe.json [@default `Null];
      input_message_content : Yojson.Safe.json [@default `Null];
    }
  end
end

module ChatAction = struct
  type action =
    | Typing
    | UploadPhoto
    | RecordVideo
    | UploadVideo
    | RecordAudio
    | UploadAudio
    | UploadDocument
    | FindLocation

  let to_string = function
    | Typing -> "typing"
    | UploadPhoto -> "upload_photo"
    | RecordVideo -> "record_video"
    | UploadVideo -> "upload_video"
    | RecordAudio -> "record_audio"
    | UploadAudio -> "upload_audio"
    | UploadDocument -> "upload_document"
    | FindLocation -> "find_location"
end

module Update = struct
  type t = {
    update_id: int;
    message: Message.t option [@default None];
    edited_message: Message.t option [@default None];
    inline_query: Inline.query option [@default None];
    chosen_inline_result: Inline.result option [@default None];
    callback_query: CallbackQuery.t option [@default None];
  } [@@deriving yojson]
end

type 'a or_error = ('a, string) R.t

module Command = struct
  type action =
    | Nothing
    | GetMe of (User.t or_error -> action)
    | SendMessage of int * string * bool * int option * Yojson.Safe.json
    | ForwardMessage of int * int * bool * int
    | SendChatAction of int * ChatAction.action
    | SendPhoto of int * string * string option * bool * int option * Yojson.Safe.json * (string or_error -> action)
    | ResendPhoto of int * string * string option * bool * int option * Yojson.Safe.json
    | SendAudio of int * string * string * string * bool * int option * Yojson.Safe.json * (string or_error -> action)
    | ResendAudio of int * string * string * string * bool * int option * Yojson.Safe.json
    | SendDocument of int * string * bool * int option * Yojson.Safe.json * (string or_error -> action)
    | ResendDocument of int * string * bool * int option * Yojson.Safe.json
    | SendSticker of int * string * bool * int option * Yojson.Safe.json * (string or_error -> action)
    | ResendSticker of int * string * bool * int option * Yojson.Safe.json
    | SendVideo of int * string * int option * string option * bool * int option * Yojson.Safe.json * (string or_error -> action)
    | ResendVideo of int * string * int option * string option * bool * int option * Yojson.Safe.json
    | SendVoice of int * string * bool * int option * Yojson.Safe.json * (string or_error -> action)
    | ResendVoice of int * string * bool * int option * Yojson.Safe.json
    | SendLocation of int * float * float * bool * int option * Yojson.Safe.json
    | SendVenue of int * float * float * string * string * string option * bool * int option * Yojson.Safe.json
    | SendContact of int * string * string * string option * bool * int option * Yojson.Safe.json
    | GetUserProfilePhotos of int * int option * int option * (UserProfilePhotos.t or_error -> action)
    | GetFile of string * (File.t or_error -> action)
    | GetFile' of string * (string option -> action)
    | DownloadFile of File.t * (string option -> action)
    | KickChatMember of int * int
    | LeaveChat of int
    | UnbanChatMember of int * int
    | GetChat of int * (Chat.t or_error -> action)
    | GetChatAdministrators of int * (ChatMember.t list or_error -> action)
    | GetChatMembersCount of int * (int or_error -> action)
    | GetChatMember of int * int * (ChatMember.t or_error -> action)
    | AnswerCallbackQuery of string * string option * bool
    | AnswerInlineQuery of string * Inline.result list * int option * bool option * string option
    | EditMessageText of [`ChatMessageId of string * int | `InlineMessageId of string] * string * string option * bool * Yojson.Safe.json
    | EditMessageCaption of [`ChatMessageId of string * int | `InlineMessageId of string] * string * Yojson.Safe.json
    | EditMessageReplyMarkup of [`ChatMessageId of string * int | `InlineMessageId of string] * Yojson.Safe.json
    | GetUpdates of (Update.t list or_error -> action)
    | PeekUpdate of (Update.t or_error -> action)
    | PopUpdate of bool * (Update.t or_error -> action)
    | Chain of action * action

  type command = {
    name            : string;
    description     : string;
    mutable enabled : bool;
    run             : Message.t -> action
  }

  let is_command u = match u.Update.message with None -> false | Some msg -> String.get msg.Message.text 0 = '/'

  let rec read_command username msg cmds = match msg.Message.text with
    | "" -> Nothing
    | txt -> begin
        let cmp str cmd =
          match String.cuts str ~sep:" " with
          | [] -> false
          | command::_ -> begin
              match username, String.cuts command ~sep:"@" with
              | _, [] -> false
              | Some username, base::bot::_ -> base = cmd && bot = username
              | _, base::_ -> base = cmd (* If no set prefix OR if no @postfix on the command itself *)
            end in
        match cmds with
        | [] -> Nothing
        | cmd::_ when cmp txt ("/" ^ cmd.name) && cmd.enabled -> cmd.run msg
        | _::cmds -> read_command username msg cmds
      end

  let read_update username u = match u.Update.message with
    | Some msg -> read_command username msg
    | None -> fun _ -> Nothing

  let tokenize msg = List.tl @@ String.cuts msg ~sep:" "

  let with_auth ~command msg = match msg.Message.from with
    | None -> Nothing
    | Some user ->
      let is_member u = List.exists (fun { ChatMember.user } -> user.User.id = u.User.id) in
      GetChatAdministrators (msg.Message.chat.Chat.id, function
          | Ok members when is_member user members -> command msg
          | _ -> Nothing
        )
end

module type BOT = sig
  val token : string
  val command_postfix : string option

  val commands : Command.command list
  val inline : Inline.query -> Command.action
  val callback : CallbackQuery.t -> Command.action

  val new_chat_member : Chat.t -> User.t -> Command.action
  val left_chat_member : Chat.t -> User.t -> Command.action
  val new_chat_title : Chat.t -> string -> Command.action
  val new_chat_photo : Chat.t -> Photo.t list -> Command.action
  val delete_chat_photo : Chat.t -> Command.action
  val group_chat_created : Chat.t -> Command.action
  val supergroup_chat_created : Chat.t -> Command.action
  val channel_chat_created : Chat.t -> Command.action
  val migrate_to_chat_id : Chat.t -> int -> Command.action
  val migrate_from_chat_id : Chat.t -> int -> Command.action
  val pinned_message : Chat.t -> Message.t -> Command.action
end

module type TELEGRAM_BOT = sig
  module IO : Cohttp.S.IO

  val url : string
  val commands : Command.command list
  val inline : Inline.query -> Command.action
  val callback : CallbackQuery.t -> Command.action

  val get_me : User.t or_error IO.t
  val send_message : chat_id:int -> text:string -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> unit or_error IO.t
  val forward_message : chat_id:int -> from_chat_id:int -> ?disable_notification:bool -> message_id:int -> unit or_error IO.t
  val send_chat_action : chat_id:int -> action:ChatAction.action -> unit or_error IO.t
  val send_photo : chat_id:int -> photo:string -> ?caption:string option -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> string or_error IO.t
  val resend_photo : chat_id:int -> photo:string -> ?caption:string option -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> unit or_error IO.t
  val send_audio : chat_id:int -> audio:string -> performer:string -> title:string -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> string or_error IO.t
  val resend_audio : chat_id:int -> audio:string -> performer:string -> title:string -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> unit or_error IO.t
  val send_document : chat_id:int -> document:string -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> string or_error IO.t
  val resend_document : chat_id:int -> document:string -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> unit or_error IO.t
  val send_sticker : chat_id:int -> sticker:string -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> string or_error IO.t
  val resend_sticker : chat_id:int -> sticker:string -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> unit or_error IO.t
  val send_video : chat_id:int -> video:string -> ?duration:int option -> ?caption:string option -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> string or_error IO.t
  val resend_video : chat_id:int -> video:string -> ?duration:int option -> ?caption:string option -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> unit or_error IO.t
  val send_voice : chat_id:int -> voice:string -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> string or_error IO.t
  val resend_voice : chat_id:int -> voice:string -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> unit or_error IO.t
  val send_location : chat_id:int -> latitude:float -> longitude:float -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> unit or_error IO.t
  val send_venue : chat_id:int -> latitude:float -> longitude:float -> title:string -> address:string -> foursquare_id:string option -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> unit or_error IO.t
  val send_contact : chat_id:int -> phone_number:string -> first_name:string -> last_name:string option -> ?disable_notification:bool -> reply_to:int option -> reply_markup:Yojson.Safe.json -> unit or_error IO.t
  val get_user_profile_photos : user_id:int -> offset:int option -> limit:int option -> UserProfilePhotos.t or_error IO.t
  val get_file : file_id:string -> File.t or_error IO.t
  val get_file' : file_id:string -> string option IO.t
  val download_file : file:File.t -> string option IO.t
  val kick_chat_member : chat_id:int -> user_id:int -> unit or_error IO.t
  val leave_chat : chat_id:int -> unit or_error IO.t
  val unban_chat_member : chat_id:int -> user_id:int -> unit or_error IO.t
  val get_chat : chat_id:int -> Chat.t or_error IO.t
  val get_chat_administrators : chat_id:int -> ChatMember.t list or_error IO.t
  val get_chat_members_count : chat_id:int -> int or_error IO.t
  val get_chat_member : chat_id:int -> user_id:int -> ChatMember.t or_error IO.t
  val answer_callback_query : callback_query_id:string -> ?text:string option -> ?show_alert:bool -> unit -> unit or_error IO.t
  val answer_inline_query : inline_query_id:string -> results:Inline.result list -> ?cache_time:int option -> ?is_personal:bool option -> ?next_offset:string option -> unit -> unit or_error IO.t
  val edit_message_text : ?chat_id:string option -> ?message_id:int option -> ?inline_message_id:string option -> text:string -> parse_mode:string option -> disable_web_page_preview:bool -> reply_markup:Yojson.Safe.json -> unit -> unit or_error IO.t
  val edit_message_caption : ?chat_id:string option -> ?message_id:int option -> ?inline_message_id:string option -> caption:string -> reply_markup:Yojson.Safe.json -> unit -> unit or_error IO.t
  val edit_message_reply_markup : ?chat_id:string option -> ?message_id:int option -> ?inline_message_id:string option -> reply_markup:Yojson.Safe.json -> unit -> unit or_error IO.t
  val get_updates : Update.t list or_error IO.t
  val peek_update : Update.t or_error IO.t
  val pop_update : ?run_cmds:bool -> unit -> Update.t or_error IO.t

  val run : ?log:bool -> unit -> unit
end

