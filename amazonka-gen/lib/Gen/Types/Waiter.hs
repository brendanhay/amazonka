{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Gen.Types.Waiter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject Lens.to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.Waiter where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Value (..), (.:), (.:?))
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import Gen.Prelude
import qualified Gen.TH
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
  parseJSON = Gen.TH.genericParseJSON Gen.TH.camel

data Criteria
  = Retry
  | Success
  | Failure
  deriving stock (Eq, Show, Generic)

instance FromJSON Criteria where
  parseJSON = Gen.TH.genericParseJSON Gen.TH.lower

data Expect
  = Status' !Integer
  | Textual !Text
  | Boolean !Bool
  deriving stock (Eq, Show)

instance FromJSON Expect where
  parseJSON = \case
    String s -> pure (Textual s)
    Bool b -> pure (Boolean b)
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
  parseJSON = Aeson.withObject "acceptor" $ \o ->
    Accept
      <$> o .: "expected"
      <*> o .: "matcher"
      <*> o .: "state"
      <*> o .:? "argument"

data Waiter a = Waiter
  { _waitDelay :: !Integer,
    _waitAttempts :: !Integer,
    _waitOperation :: Id,
    _waitAcceptors :: [Accept a]
  }
  deriving stock (Show, Eq)

$(Lens.makeLenses ''Waiter)

instance FromJSON (Waiter Id) where
  parseJSON = Aeson.withObject "waiter" $ \o ->
    Waiter
      <$> o .: "delay"
      <*> o .: "maxAttempts"
      <*> o .: "operation"
      <*> o .: "acceptors"
