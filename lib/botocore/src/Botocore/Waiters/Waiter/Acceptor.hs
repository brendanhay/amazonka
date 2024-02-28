{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Waiters.Waiter.Acceptor
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Waiters.Waiter.Acceptor where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser,
    bool,
    enum,
    field,
    int,
    optional,
    orElse,
    record,
    text,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (map)

data State = Failure | Retry | Success
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data Matcher = Error | Path | PathAll | PathAny | Status
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data Expected = ExpectedBool Bool | ExpectedInt Int | ExpectedText Text
  deriving (Eq, Show, Generic)

$( passthroughBareB
     [d|
       data Acceptor = Acceptor
         { state :: State,
           matcher :: Matcher,
           expected :: Expected,
           argument :: Maybe Text
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Acceptor
parse =
  record
    Acceptor
      { state = field "state" . enum $ \case
          Failure -> "failure"
          Retry -> "retry"
          Success -> "success",
        matcher = field "matcher" . enum $ \case
          Error -> "error"
          Path -> "path"
          PathAll -> "pathAll"
          PathAny -> "pathAny"
          Status -> "status",
        expected =
          field "expected" $
            (ExpectedBool <$> bool)
              `orElse` (ExpectedInt <$> int)
              `orElse` (ExpectedText <$> text),
        argument = optional $ field "argument" text
      }
