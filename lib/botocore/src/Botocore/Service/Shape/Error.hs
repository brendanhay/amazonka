{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Shape.Error
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Shape.Error where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser,
    bool,
    field,
    int,
    optional,
    record,
    text,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

$( passthroughBareB
     [d|
       data Error = Error
         { httpStatusCode :: Int,
           code :: Maybe Text,
           fault :: Maybe Bool,
           senderFault :: Maybe Bool
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Error
parse =
  record
    Error
      { httpStatusCode = field "httpStatusCode" int,
        code = optional $ field "code" text,
        fault = optional $ field "fault" bool,
        senderFault = optional $ field "senderFault" bool
      }
