module Server
  ( server
  , Env(..)
  , module X
  ) where

import Mig.Json.IO
import Server.GetMessage qualified as GetMessage
import Server.ListTag    qualified as ListTag
import Server.Save       qualified as Save
import Server.ToggleLog  qualified as ToggleLog
import Types
import Api as X

-- | Service environment by methods
data Env = Env
  { save        :: Save.Env
  , getMessage  :: GetMessage.Env
  , listTag     :: ListTag.Env
  , toggleLogs  :: ToggleLog.Env
  }

-- | Mig server for the app
server :: Env -> Server IO
server env =
  "api/v1" /.
    mconcat
      [ "save" /. handleSave
      , "get/message" /. handleGetById
      , "list/tag" /. handleGetByTag
      , "toggle-logs" /. handleToggleLogs
      ]
  where
    handleSave :: Body SaveRequest -> Post (Resp SaveResponse)
    handleSave (Body req) = Send $ fmap ok $ Save.handle env.save req

    handleGetById :: Capture "message-id" MessageId -> Get (RespOr ApiError Message)
    handleGetById (Capture messageId) = Send $
      either (bad status400) ok <$> GetMessage.handle env.getMessage messageId

    handleGetByTag :: Capture "tag" Tag -> Get (Resp [Message])
    handleGetByTag (Capture tag) = Send $ fmap ok $ ListTag.handle env.listTag tag

    handleToggleLogs :: Post (Resp ())
    handleToggleLogs = Send $ fmap ok $ ToggleLog.handle env.toggleLogs
