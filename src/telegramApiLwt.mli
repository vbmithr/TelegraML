(** Generate a bot's interface to allow for direct calls to functions *)
module Make : functor (B : TelegramApi.BOT) -> TelegramApi.TELEGRAM_BOT
