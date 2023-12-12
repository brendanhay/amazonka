{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Operation.Output
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Operation.Output where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Service.Types (ShapeName, shapeName)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    bool,
    field,
    optional,
    record,
    text,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

$( passthroughBareB
     [d|
       data Output = Output
         { shape :: ShapeName,
           documentation :: Maybe Text,
           locationName :: Maybe Text,
           wrapper :: Maybe Bool,
           resultWrapper :: Maybe Text
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Output
parse =
  record
    Output
      { shape = field "shape" shapeName,
        documentation = optional $ field "documentation" text,
        locationName = optional $ field "locationName" text,
        wrapper = optional $ field "wrapper" bool,
        resultWrapper = optional $ field "resultWrapper" text
      }
