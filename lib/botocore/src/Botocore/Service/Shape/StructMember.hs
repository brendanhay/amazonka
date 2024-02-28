{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Shape.StructMember
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Shape.StructMember where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Service.Shape.StructMember.ContextParam (ContextParam)
import Botocore.Service.Shape.StructMember.ContextParam qualified as ContextParam
import Botocore.Service.Types (ShapeName, XmlNamespace, shapeName)
import Botocore.Service.Types qualified as Types
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser,
    bool,
    field,
    nonEmpty,
    optional,
    record,
    text,
  )
import Data.Aeson.Decoding.Tokens.Direct qualified as Types
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)

data Location
  = Uri
  | QueryString
  | Header
  | Headers
  | StatusCode
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

$( passthroughBareB
     [d|
       data StructMember = StructMember
         { shape :: ShapeName,
           box :: Maybe Bool,
           contextParam :: Maybe ContextParam,
           deprecated :: Maybe Bool,
           deprecatedMessage :: Maybe Text,
           documentation :: Maybe Text,
           enum :: Maybe (NonEmpty Text),
           eventpayload :: Maybe Bool,
           flattened :: Maybe Bool,
           hostLabel :: Maybe Bool,
           idempotencyToken :: Maybe Bool,
           jsonvalue :: Maybe Bool,
           location :: Maybe Location,
           locationName :: Maybe Text,
           pattern :: Maybe Text,
           queryName :: Maybe Text,
           streaming :: Maybe Bool,
           xmlAttribute :: Maybe Bool,
           xmlNamespace :: Maybe XmlNamespace
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e StructMember
parse =
  record
    StructMember
      { shape = field "shape" shapeName,
        box = optional $ field "box" bool,
        contextParam = optional $ field "contextParam" ContextParam.parse,
        deprecated = optional $ field "deprecated" bool,
        deprecatedMessage = optional $ field "deprecatedMessage" text,
        documentation = optional $ field "documentation" text,
        enum = optional . field "enum" $ nonEmpty text,
        eventpayload = optional $ field "eventpayload" bool,
        flattened = optional $ field "flattened" bool,
        hostLabel = optional $ field "hostLabel" bool,
        idempotencyToken = optional $ field "idempotencyToken" bool,
        jsonvalue = optional $ field "jsonvalue" bool,
        location = optional . field "location" . Types.enum $ \case
          Uri -> "uri"
          QueryString -> "querystring"
          Header -> "header"
          Headers -> "headers"
          StatusCode -> "statusCode",
        locationName = optional $ field "locationName" text,
        pattern = optional $ field "pattern" text,
        queryName = optional $ field "queryName" text,
        streaming = optional $ field "streaming" bool,
        xmlAttribute = optional $ field "xmlAttribute" bool,
        xmlNamespace = optional $ field "xmlNamespace" Types.xmlNamespace
      }
