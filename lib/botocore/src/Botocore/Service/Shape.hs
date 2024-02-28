{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Shape
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Shape where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Service.Shape.Error (Error)
import Botocore.Service.Shape.Error qualified as Error
import Botocore.Service.Shape.Key (Key)
import Botocore.Service.Shape.Key qualified as Key
import Botocore.Service.Shape.ListMember (ListMember)
import Botocore.Service.Shape.ListMember qualified as ListMember
import Botocore.Service.Shape.Retryable (Retryable)
import Botocore.Service.Shape.Retryable qualified as Retryable
import Botocore.Service.Shape.StructMember (StructMember)
import Botocore.Service.Shape.StructMember qualified as StructMember
import Botocore.Service.Shape.Value (Value)
import Botocore.Service.Shape.Value qualified as Value
import Botocore.Service.Types (ShapeName, XmlNamespace, shapeName)
import Botocore.Service.Types qualified as Types
import Data.Aeson.Decoding.Tokens (Tokens)
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    alist,
    bool,
    field,
    list,
    nonEmpty,
    optional,
    record,
    scientific,
    text,
  )
import Data.Aeson.Decoding.Tokens.Direct qualified as Types
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (error, map, max, min)

data Type
  = Blob
  | Boolean
  | Double
  | Float
  | Integer
  | List
  | Long
  | Map
  | String
  | Structure
  | Timestamp
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data TimestampFormat
  = Iso8601
  | Rfc822
  | UnixTimestamp
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

$( passthroughBareB
     [d|
       data Shape = Shape
         { type_ :: Type,
           box :: Maybe Bool,
           deprecated :: Maybe Bool,
           deprecatedMessage :: Maybe Text,
           document :: Maybe Bool,
           documentation :: Maybe Text,
           -- Names of valid strings for 'String's which are enums.
           enum :: Maybe (NonEmpty Text),
           error :: Maybe Error,
           event :: Maybe Bool,
           eventstream :: Maybe Bool,
           exception :: Maybe Bool,
           fault :: Maybe Bool,
           flattened :: Maybe Bool,
           key :: Maybe Key,
           locationName :: Maybe Text,
           -- 'List' member shape.
           member :: Maybe ListMember,
           -- Ordered list of fields within a 'Structure'.
           members :: Maybe [(Text, StructMember)],
           min :: Maybe Scientific,
           max :: Maybe Scientific,
           pattern :: Maybe Text,
           payload :: Maybe ShapeName,
           required :: Maybe [ShapeName],
           requiresLength :: Maybe Bool,
           retryable :: Maybe Retryable,
           sensitive :: Maybe Bool,
           streaming :: Maybe Bool,
           synthetic :: Maybe Bool,
           timestampFormat :: Maybe TimestampFormat,
           union :: Maybe Bool,
           value :: Maybe Value,
           wrapper :: Maybe Bool,
           -- List of field names when generating XML?
           xmlOrder :: Maybe (NonEmpty ShapeName),
           xmlNamespace :: Maybe XmlNamespace
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Shape
parse =
  record
    Shape
      { type_ = field "type" . Types.enum $ \case
          Blob -> "blob"
          Boolean -> "boolean"
          Double -> "double"
          Float -> "float"
          Integer -> "integer"
          List -> "list"
          Long -> "long"
          Map -> "map"
          String -> "string"
          Structure -> "structure"
          Timestamp -> "timestamp",
        box = optional $ field "box" bool,
        deprecated = optional $ field "deprecated" bool,
        deprecatedMessage = optional $ field "deprecatedMessage" text,
        document = optional $ field "document" bool,
        documentation = optional $ field "documentation" text,
        enum = optional . field "enum" $ nonEmpty text,
        error = optional . field "error" $ Error.parse,
        event = optional $ field "event" bool,
        eventstream = optional $ field "eventstream" bool,
        exception = optional $ field "exception" bool,
        fault = optional $ field "fault" bool,
        flattened = optional $ field "flattened" bool,
        key = optional $ field "key" Key.parse,
        locationName = optional $ field "locationName" text,
        member = optional $ field "member" ListMember.parse,
        members =
          optional . field "members" $ alist StructMember.parse,
        min = optional $ field "min" scientific,
        max = optional $ field "max" scientific,
        pattern = optional $ field "pattern" text,
        payload = optional $ field "payload" shapeName,
        required = optional . field "required" $ list shapeName,
        requiresLength = optional $ field "requiresLength" bool,
        retryable = optional $ field "retryable" Retryable.parse,
        sensitive = optional $ field "sensitive" bool,
        streaming = optional $ field "streaming" bool,
        synthetic = optional $ field "synthetic" bool,
        timestampFormat = optional . field "timestampFormat" $
          Types.enum $ \case
            Iso8601 -> "iso8601"
            Rfc822 -> "rfc822"
            UnixTimestamp -> "unixTimestamp",
        union = optional $ field "union" bool,
        value = optional $ field "value" Value.parse,
        wrapper = optional $ field "wrapper" bool,
        xmlOrder = optional . field "xmlOrder" $ nonEmpty shapeName,
        xmlNamespace = optional $ field "xmlNamespace" Types.xmlNamespace
      }
