-- | API for the service.
-- For simplicity it is in single module, but will be split on parts
-- by methods in real project.
module Api
  ( -- Api
    SaveRequest(..)
  , SaveResponse(..)
  ) where

import Deriving.Aeson
import Deriving.Aeson.Stock
import Types
import Mig (ToSchema)

data SaveRequest = SaveRequest
  { message :: Text
  , tags    :: [Tag]
  }
  deriving stock (Show, Generic)
  deriving (ToSchema)
  deriving (FromJSON, ToJSON) via Vanilla SaveRequest

newtype SaveResponse = SaveResponse MessageId
  deriving newtype (ToJSON, FromJSON, ToSchema, Show, Eq)
