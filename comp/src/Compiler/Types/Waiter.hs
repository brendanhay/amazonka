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
import           Data.Foldable           (foldl')
import           Data.Ord
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           GHC.Generics

-- data When
--     = WhenStatus (Maybe Text) !Int
--     | WhenCRC32  !Text
--       deriving (Eq, Show)

-- instance FromJSON When where
--     parseJSON = withObject "when" (\o -> status o <|> crc o)
--       where
--         status o = WhenStatus
--              <$> o .:? "service_error_code"
--              <*> o .:  "http_status_code"

--         crc = fmap WhenCRC32 . (.: "crc32body")

-- data Policy
--     = ApplyRef  !Text
--     | ApplySock [Text]
--     | ApplyWhen When
--       deriving (Eq, Show)

-- instance FromJSON Policy where
--     parseJSON = withObject "ref" (\o -> sock o <|> resp o <|> ref o)
--       where
--         sock = fmap ApplySock . ((.: "applies_when") >=> (.: "socket_errors"))
--         resp = fmap ApplyWhen . ((.: "applies_when") >=> (.: "response"))
--         ref  = fmap ApplyRef  . (.: "$ref")

-- newtype Base = Factor Scientific
--     deriving (Eq, Show, A.ToJSON)

-- instance FromJSON Base where
--     parseJSON = \case
--        String "rand" -> pure (Factor 0.05)
--        o             -> Factor <$> parseJSON o

-- data Delay = Delay
--     { _dType         :: !Text
--     , _dBase         :: !Base
--     , _dGrowthFactor :: !Int
--     } deriving (Eq, Show, Generic)



-- data Retry = Retry
--     { _rMaxAttempts :: !Int
--     , _rDelay       :: !Delay
--     , _rPolicies    :: HashMap Text Policy
--     } deriving (Eq, Show)

-- makeLenses ''Retry

-- instance FromJSON Retry where
--     parseJSON = withObject "retry" $ \x ->
--         case lookup "__default__" (unObject x) of
--             Nothing -> go x
--             Just  y -> withObject "default_retry" go y
--       where
--         go o = Retry
--             <$> o .: "max_attempts"
--             <*> o .: "delay"
--             <*> o .: "policies"

-- data Retries = Retries
--     { _rDefinitions :: HashMap Text Policy
--     , _rRetries     :: HashMap Text Retry
--     , _rDefault     :: Retry
--     } deriving (Eq, Show)

-- makeLenses ''Retries

-- instance FromJSON Retries where
--     parseJSON = withObject "retries" $ \o -> do
--         r <- o .: "retry"
--         m <- maybe (fail $ "Missing: " ++ show def) return (Map.lookup def r)
--         Retries <$> o .: "definitions"
--                 <*> mergeAll r m
--                 <*> parseJSON m
--       where
--         mergeAll r m = traverse (\x -> parseJSON (go x p)) n
--           where
--             n = Map.delete def r
--             p = toJSON (Map.singleton def m)

--         go (Object x) (Object y) = Object $ merge [x, y]
--         go _          _          = error "Expected two objects"

--         def = "__default__"

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
