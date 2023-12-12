{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Operation.Input
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Operation.Input where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Service.Operation.Input.XmlNamespace (XmlNamespace)
import Botocore.Service.Operation.Input.XmlNamespace qualified as XmlNamespace
import Botocore.Service.Types (ShapeName, shapeName)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    field,
    optional,
    record,
    text,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

$( passthroughBareB
     [d|
       data Input = Input
         { shape :: ShapeName,
           documentation :: Maybe Text,
           locationName :: Maybe ShapeName,
           xmlNamespace :: Maybe XmlNamespace
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Input
parse =
  record
    Input
      { shape = field "shape" shapeName,
        documentation = optional $ field "documentation" text,
        locationName = optional $ field "locationName" shapeName,
        xmlNamespace = optional $ field "xmlNamespace" XmlNamespace.parse
      }
