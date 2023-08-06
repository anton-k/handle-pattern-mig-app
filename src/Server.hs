module Server
  ( server
  , Env(..)
  , module X
  ) where

import Mig.Json.IO
import Data.Bifunctor
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
  "api" /. "v1" /.
    mconcat
      [ "save" /. handleSave
      , "get" /. "message" /. handleGetById
      , "list" /. "tag" /. handleGetByTag
      , "toggle-logs" /. handleToggleLogs
      ]
  where
    handleSave :: Body SaveRequest -> Post SaveResponse
    handleSave (Body req) = Post $ Save.handle env.save req

    handleGetById :: Capture MessageId -> Get (Either (Error ApiError) Message)
    handleGetById (Capture messageId) = Get $
      first (Error status400) <$> GetMessage.handle env.getMessage messageId

    handleGetByTag :: Capture Tag -> Get [Message]
    handleGetByTag (Capture tag) = Get $ ListTag.handle env.listTag tag

    handleToggleLogs :: Post ()
    handleToggleLogs = Post $ ToggleLog.handle env.toggleLogs
