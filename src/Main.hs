{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when, void)
import qualified Data.Text.IO as TIO
  ( readFile,
    putStrLn )
import Discord
  ( runDiscord,
    def,
    restCall,
    DiscordHandler,
    RunDiscordOpts(discordToken, discordOnEvent) )
import Discord.Types
    ( messageAuthor,
      messageId,
      Event(MessageCreate),
      Message (messageChannel),
      User (userIsBot) )
import qualified Discord.Requests as R
  ( ChannelRequest(CreateReaction) )

main :: IO ()
main = do
  token <- TIO.readFile "token.secret"
  t <- runDiscord $ def {
    discordToken = token,
    discordOnEvent = eventHandler
  }
  TIO.putStrLn t

eventHandler :: Event -> DiscordHandler ()
eventHandler e =
  case e of
    MessageCreate m -> when (not (fromBot m) && True) $ do
      void $ restCall (R.CreateReaction (messageChannel m, messageId m) "horseyping")
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor