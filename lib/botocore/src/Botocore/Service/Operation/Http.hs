{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Operation.Http
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Operation.Http where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    enum,
    field,
    int,
    optional,
    record,
    text,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Method
  = Get
  | Post
  | Head
  | Put
  | Delete
  | Patch
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)

$( passthroughBareB
     [d|
       data Http = Http
         { method :: Method,
           requestUri :: Text,
           responseCode :: Maybe Int
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Http
parse =
  record
    Http
      { method = field "method" . enum $ \case
          Get -> "GET"
          Post -> "POST"
          Head -> "HEAD"
          Put -> "PUT"
          Delete -> "DELETE"
          Patch -> "PATCH",
        requestUri = field "requestUri" text,
        responseCode = optional $ field "responseCode" int
      }
