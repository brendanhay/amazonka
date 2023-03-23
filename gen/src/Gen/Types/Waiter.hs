{-# LANGUAGE TemplateHaskell #-}

module Gen.Types.Waiter where

import qualified Control.Lens as Lens
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import GHC.Generics ()
import Gen.Prelude
import Gen.TH
import Gen.Types.Id
import Gen.Types.Notation

data Match
  = Path
  | PathAll
  | PathAny
  | Status
  | Error
  deriving stock (Eq, Show, Generic)

instance FromJSON Match where
  parseJSON = gParseJSON' camel

data Criteria
  = Retry
  | Success
  | Failure
  deriving stock (Eq, Show, Generic)

instance FromJSON Criteria where
  parseJSON = gParseJSON' lower

data Expect
  = Status' !Integer
  | Textual !Text
  | Boolean !Bool
  deriving stock (Eq, Show)

instance FromJSON Expect where
  parseJSON = \case
    Aeson.String s -> pure (Textual s)
    Aeson.Bool b -> pure (Boolean b)
    o -> Status' <$> Aeson.parseJSON o

data Accept a = Accept
  { _acceptExpect :: Expect,
    _acceptMatch :: Match,
    _acceptCriteria :: Criteria,
    _acceptArgument :: Maybe (Notation a)
  }
  deriving stock (Eq, Show)

$(Lens.makeLenses ''Accept)

instance FromJSON (Accept Id) where
  parseJSON =
    Aeson.withObject "acceptor" $ \o ->
      Accept
        <$> o .: "expected"
        <*> o .: "matcher"
        <*> o .: "state"
        <*> o .:? "argument"

data Waiter a = Waiter
  { _waitDelay :: Integer,
    _waitAttempts :: Integer,
    _waitOperation :: Id,
    _waitAcceptors :: [Accept a]
  }
  deriving stock (Eq, Show)

$(Lens.makeLenses ''Waiter)

instance FromJSON (Waiter Id) where
  parseJSON =
    Aeson.withObject "waiter" $ \o ->
      Waiter
        <$> o .: "delay"
        <*> o .: "maxAttempts"
        <*> o .: "operation"
        <*> o .: "acceptors"
