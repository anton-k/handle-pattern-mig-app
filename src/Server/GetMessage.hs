-- | Get message by id handler
module Server.GetMessage
  ( Env(..)
  , Db(..)
  , handle
  ) where

import DI.Log
import Types

-----------------------------------------
-- Env

data Env = Env
  { db  :: Db
  , log :: Log
  }

data Db = Db
  { getMessage :: MessageId -> IO (Maybe Message)
  }

-----------------------------------------
-- Handler

handle :: Env -> MessageId -> IO (Either ApiError Message)
handle (Env Db{..} Log{..}) messageId = do
  logInfo $ "get by id call: " <> display messageId
  mMsg <- getMessage messageId
  case mMsg of
    Just msg -> pure $ Right msg
    Nothing  -> do
      logError $ "Message not found by id: " <> display messageId
      pure $ Left $ ApiError "Message not found"
