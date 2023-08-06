-- | Domain types
module Types
  ( Url
  , MessageId(..)
  , Tag(..)
  , Message(..)
  , ApiError (..)
  , module X
  ) where

import Data.Time as X (UTCTime)
import Data.Text as X (Text)
import Deriving.Aeson
import Deriving.Aeson.Stock
import Mig (FromText)

type Url = String

newtype MessageId = MessageId { unMessageId :: Int }
  deriving newtype (ToJSON, FromJSON, Show, Eq, Ord, FromText)

newtype Tag = Tag { unTag :: Text }
  deriving newtype (ToJSON, FromJSON, Show, Eq, FromText)

data Message = Message
  { content :: Text
  , tags    :: [Tag]
  , time    :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Vanilla Message

newtype ApiError = ApiError Text
  deriving newtype (ToJSON, FromJSON, Show, Eq, FromText)
