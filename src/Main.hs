{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when, forM_, void)

import UnliftIO (liftIO)
import qualified Data.Text.IO as TIO
  ( readFile,
    putStrLn )
import Discord
  ( runDiscord,
    def,
    restCall,
    sendCommand,
    DiscordHandler,
    RunDiscordOpts(discordToken, discordOnEvent, discordOnEnd, discordOnLog) )
import Discord.Types
    ( messageAuthor,
      messageMentions,
      messageId,
      updateStatusOptsSince,
      updateStatusOptsGame,
      updateStatusOptsNewStatus,
      updateStatusOptsAFK,
      UpdateStatusType(UpdateStatusOnline),
      GatewaySendable(UpdateStatus),
      Activity(activityName, activityType, activityUrl, Activity),
      ActivityType(ActivityTypeGame),
      Event(MessageCreate),
      Message (messageChannel),
      User (userIsBot), UpdateStatusOpts (UpdateStatusOpts) )
import qualified Discord.Requests as R
  ( ChannelRequest(CreateReaction, CreateMessage),
    UserRequest (GetCurrentUserGuilds) )

main :: IO ()
main = do
  token <- TIO.readFile "token.secret"
  t <- runDiscord $ def {
    discordToken = token,
    discordOnEvent = eventHandler,
    discordOnEnd = liftIO $ putStrLn "Ended",
    discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
  }
  TIO.putStrLn t

startHandler :: DiscordHandler ()
startHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds
  let activity = Activity {
    activityName = "horsey-ping",
    activityType = ActivityTypeGame,
    activityUrl = Nothing
  }
  let opts = UpdateStatusOpts {
    updateStatusOptsSince = Nothing,
    updateStatusOptsGame = Just activity,
    updateStatusOptsNewStatus = UpdateStatusOnline,
    updateStatusOptsAFK = False
  }
  sendCommand (UpdateStatus opts)

eventHandler :: Event -> DiscordHandler ()
eventHandler e =
  case e of
    MessageCreate m ->
      when (not (fromBot m) && pingsUser m)
      $ void
      $ restCall (R.CreateReaction (messageChannel m, messageId m) "horseyping:857042585029050368")
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

pingsUser :: Message -> Bool
pingsUser = not . null . messageMentions