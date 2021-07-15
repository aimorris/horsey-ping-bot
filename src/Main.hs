{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless, forM_, void)

import UnliftIO (liftIO)
import qualified Data.Text.IO as TIO
  ( readFile,
    putStrLn )
import Discord
  ( runDiscord,
    def,
    restCall,
    DiscordHandler,
    RunDiscordOpts(discordToken, discordOnEvent, discordOnEnd) )
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
    discordOnEvent = eventHandler,
    discordOnEnd = liftIO $ putStrLn "Ended"
  }
  TIO.putStrLn t

eventHandler :: Event -> DiscordHandler ()
eventHandler e =
  case e of
    MessageCreate m -> unless (fromBot m) $ do
      void $ restCall (R.CreateReaction (messageChannel m, messageId m) "horseyping")
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor