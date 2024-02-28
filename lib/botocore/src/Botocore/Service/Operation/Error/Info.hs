{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Operation.Error.Info
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Operation.Error.Info where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    bool,
    field,
    int,
    optional,
    record,
    text,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (error)

$( passthroughBareB
     [d|
       data Info = Info
         { code :: Maybe Text,
           httpStatusCode :: Int,
           senderFault :: Maybe Bool
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Info
parse =
  record
    Info
      { code = optional $ field "code" text,
        httpStatusCode = field "httpStatusCode" int,
        senderFault = optional $ field "senderFault" bool
      }
