{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Metadata
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Metadata where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Service.Metadata.ProtocolSettings (ProtocolSettings)
import Botocore.Service.Metadata.ProtocolSettings qualified as ProtocolSettings
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    enum,
    field,
    optional,
    record,
    text,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data ChecksumFormat = Md5 | Sha256
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data JsonVersion = V1_0 | V1_1
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data Protocol = Json | RestJson | RestXml | Query | Ec2 | ApiGateway
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data SignatureVersion = Bearer | S3 | S3V4 | V2 | V4
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

$( passthroughBareB
     [d|
       data Metadata = Metadata
         { apiVersion :: Text,
           checksumFormat :: Maybe ChecksumFormat,
           endpointPrefix :: Text,
           globalEndpoint :: Maybe Text,
           jsonVersion :: Maybe JsonVersion,
           protocol :: Protocol,
           protocolSettings :: Maybe ProtocolSettings,
           serviceAbbreviation :: Maybe Text,
           serviceFullName :: Text,
           serviceId :: Text,
           signatureVersion :: SignatureVersion,
           signingName :: Maybe Text,
           targetPrefix :: Maybe Text,
           uid :: Maybe Text,
           xmlNamespace :: Maybe Text
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Metadata
parse =
  record
    Metadata
      { apiVersion = field "apiVersion" text,
        checksumFormat = optional . field "checksumFormat" . enum $ \case
          Md5 -> "md5"
          Sha256 -> "sha256",
        endpointPrefix = field "endpointPrefix" text,
        globalEndpoint = optional $ field "globalEndpoint" text,
        jsonVersion = optional . field "jsonVersion" . enum $ \case
          V1_0 -> "1.0"
          V1_1 -> "1.1",
        protocol = field "protocol" . enum $ \case
          Json -> "json"
          RestJson -> "rest-json"
          RestXml -> "rest-xml"
          Query -> "query"
          Ec2 -> "ec2"
          ApiGateway -> "api-gateway",
        protocolSettings =
          optional $ field "protocolSettings" ProtocolSettings.parse,
        serviceAbbreviation = optional $ field "serviceAbbreviation" text,
        serviceFullName = field "serviceFullName" text,
        serviceId = field "serviceId" text,
        signatureVersion = field "signatureVersion" . enum $ \case
          Bearer -> "bearer"
          S3 -> "s3"
          S3V4 -> "s3v4"
          V2 -> "v2"
          V4 -> "v4",
        signingName = optional $ field "signingName" text,
        targetPrefix = optional $ field "targetPrefix" text,
        uid = optional $ field "uid" text,
        xmlNamespace = optional $ field "xmlNamespace" text
      }
