{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.Model.Retrier
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Model.Retrier where

import           Control.Lens

import           Data.HashMap.Strict (HashMap)
import           Data.Scientific     (Scientific)
import           Data.Text           (Text)

data When
    = WhenStatus (Maybe Text) Int
    | WhenCRC32  Text
      deriving (Eq, Show)

data Policy
    = ApplyRef  Text
    | ApplySock [Text]
    | ApplyWhen When
      deriving (Eq, Show)

data Delay = Delay
    { _delayType         :: Text
    , _delayBase         :: Scientific
    , _delayGrowthFactor :: Int
    } deriving (Eq, Show)

data Retry = Retry
    { _retryMaxAttempts :: Int
    , _retryDelay       :: Delay
    , _retryPolicies    :: HashMap Text Policy
    } deriving (Eq, Show)

makeLenses ''Retry

data Retrier = Retrier
    { _retrierDefinitions :: HashMap Text Policy
    , _retrierRetries     :: HashMap Text Retry
    , _retrierDefault     :: Retry
    } deriving (Eq, Show)

makeLenses ''Retrier
