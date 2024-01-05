{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Waiters.Waiter
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Waiters.Waiter where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Waiters.Waiter.Acceptor (Acceptor)
import Botocore.Waiters.Waiter.Acceptor qualified as Acceptor
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser,
    enum,
    field,
    int,
    nonEmpty,
    optional,
    record,
    text,
  )
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (map)

data Type = Api
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

$( passthroughBareB
     [d|
       data Waiter = Waiter
         { acceptors :: NonEmpty Acceptor,
           delay :: Int,
           maxAttempts :: Int,
           operation :: Text,
           description :: Maybe Text,
           type_ :: Maybe Type
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Waiter
parse =
  record
    Waiter
      { acceptors = field "acceptors" $ nonEmpty Acceptor.parse,
        delay = field "delay" int,
        maxAttempts = field "maxAttempts" int,
        operation = field "operation" text,
        description = optional $ field "description" text,
        type_ = optional . field "type" . enum $ \case
          Api -> "api"
      }
