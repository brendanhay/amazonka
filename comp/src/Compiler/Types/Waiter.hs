{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Compiler.Types.Waiter
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.Waiter where

import           Compiler.TH
import           Compiler.Types.Id
import           Compiler.Types.Notation
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec.Text    (Parser, parseOnly)
import qualified Data.Attoparsec.Text    as A
import           Data.Ord
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           GHC.Generics

data Match
    = Path
    | PathAll
    | PathAny
    | Status
    | Error
      deriving (Eq, Show, Generic)

instance FromJSON Match where
    parseJSON = gParseJSON' camel

data Criteria
    = Retry
    | Success
    | Failure
      deriving (Eq, Show, Generic)

instance FromJSON Criteria where
    parseJSON = gParseJSON' lower

data Expect
    = Status' !Integer
    | Textual !Text
    | Boolean !Bool
      deriving (Eq, Show)

instance FromJSON Expect where
    parseJSON = \case
        String s -> pure (Textual s)
        Bool   b -> pure (Boolean b)
        o        -> Status' <$> parseJSON o

data Accept a = Accept
    { _acceptExpect   :: Expect
    , _acceptMatch    :: Match
    , _acceptCriteria :: Criteria
    , _acceptArgument :: Maybe (Notation a)
    } deriving (Eq, Show)

makeLenses ''Accept

instance FromJSON (Accept Id) where
    parseJSON = withObject "acceptor" $ \o -> Accept
        <$> o .:  "expected"
        <*> o .:  "matcher"
        <*> o .:  "state"
        <*> o .:? "argument"

data Waiter a = Waiter
    { _waitDelay     :: !Integer
    , _waitAttempts  :: !Integer
    , _waitOperation :: Id
    , _waitAcceptors :: [Accept a]
    } deriving (Show, Eq)

makeLenses ''Waiter

instance FromJSON (Waiter Id) where
    parseJSON = withObject "waiter" $ \o -> Waiter
        <$> o .: "delay"
        <*> o .: "maxAttempts"
        <*> o .: "operation"
        <*> o .: "acceptors"
