{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.ClientContextParam
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.ClientContextParam where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    enum,
    field,
    record,
    text,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Type = Boolean
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

$( passthroughBareB
     [d|
       data ClientContextParam = ClientContextParam
         { documentation :: Text,
           type_ :: Type
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e ClientContextParam
parse =
  record
    ClientContextParam
      { documentation = field "documentation" text,
        type_ = field "type" . enum $ \case
          Boolean -> "boolean"
      }
