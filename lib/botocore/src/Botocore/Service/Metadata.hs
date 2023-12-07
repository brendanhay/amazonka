{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
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

import Botocore.Service.Metadata.ProtocolSettings (ProtocolSettings)
import Botocore.Service.Metadata.ProtocolSettings qualified as ProtocolSettings
import Data.Aeson.Decoding.ByteString.Lazy
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Error,
    FieldParser,
    Parser (..),
    enum,
    execParser,
    field,
    optional,
    recordB,
    text,
  )
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable
import Data.Functor ((<&>))
import Data.Functor.Barbie.Extended (FunctorB, Rec (..), TraversableB)
import Data.Functor.Barbie.Record (RecordB (..))
import Data.Functor.Identity (Identity)
import Data.Some (Some (..))
import Data.Text (Text)
import Data.Traversable
import GHC.Generics (Generic)
import System.Directory
import System.IO

data Metadata f = Metadata
  { apiVersion :: f Text,
    checksumFormat :: f (Maybe ChecksumFormat),
    endpointPrefix :: f Text,
    globalEndpoint :: f (Maybe Text),
    jsonVersion :: f (Maybe JsonVersion),
    protocol :: f Protocol,
    protocolSettings :: f (Maybe (ProtocolSettings Identity)),
    serviceAbbreviation :: f (Maybe Text),
    serviceFullName :: f Text,
    serviceId :: f Text,
    signatureVersion :: f SignatureVersion,
    signingName :: f (Maybe Text),
    targetPrefix :: f (Maybe Text),
    uid :: f (Maybe Text),
    xmlNamespace :: f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, TraversableB)

deriving stock instance (forall a. (Eq a) => Eq (f a)) => Eq (Metadata f)

deriving stock instance (forall a. (Show a) => Show (f a)) => Show (Metadata f)

instance RecordB Metadata where
  data Field Metadata a where
    ApiVersion :: Field Metadata Text
    ChecksumFormat :: Field Metadata (Maybe ChecksumFormat)
    EndpointPrefix :: Field Metadata Text
    GlobalEndpoint :: Field Metadata (Maybe Text)
    JsonVersion :: Field Metadata (Maybe JsonVersion)
    Protocol :: Field Metadata Protocol
    ProtocolSettings :: Field Metadata (Maybe (ProtocolSettings Identity))
    ServiceAbbreviation :: Field Metadata (Maybe Text)
    ServiceFullName :: Field Metadata Text
    ServiceId :: Field Metadata Text
    SignatureVersion :: Field Metadata SignatureVersion
    SigningName :: Field Metadata (Maybe Text)
    TargetPrefix :: Field Metadata (Maybe Text)
    Uid :: Field Metadata (Maybe Text)
    XmlNamespace :: Field Metadata (Maybe Text)

  allFields =
    [ Some ApiVersion,
      Some ChecksumFormat,
      Some EndpointPrefix,
      Some GlobalEndpoint,
      Some JsonVersion,
      Some Protocol,
      Some ProtocolSettings,
      Some ServiceAbbreviation,
      Some ServiceFullName,
      Some ServiceId,
      Some SignatureVersion,
      Some SigningName,
      Some TargetPrefix,
      Some Uid,
      Some XmlNamespace
    ]

  fieldLens i f = case i of
    ApiVersion -> \metadata ->
      f (apiVersion metadata) <&> \apiVersion' ->
        metadata {apiVersion = apiVersion'}
    ChecksumFormat -> \metadata ->
      f (checksumFormat metadata) <&> \checksumFormat' ->
        metadata {checksumFormat = checksumFormat'}
    EndpointPrefix -> \metadata ->
      f (endpointPrefix metadata) <&> \endpointPrefix' ->
        metadata {endpointPrefix = endpointPrefix'}
    GlobalEndpoint -> \metadata ->
      f (globalEndpoint metadata) <&> \globalEndpoint' ->
        metadata {globalEndpoint = globalEndpoint'}
    JsonVersion -> \metadata ->
      f (jsonVersion metadata) <&> \jsonVersion' ->
        metadata {jsonVersion = jsonVersion'}
    Protocol -> \metadata ->
      f (protocol metadata) <&> \protocol' ->
        metadata {protocol = protocol'}
    ProtocolSettings -> \metadata ->
      f (protocolSettings metadata) <&> \protocolSettings' ->
        metadata {protocolSettings = protocolSettings'}
    ServiceAbbreviation -> \metadata ->
      f (serviceAbbreviation metadata) <&> \serviceAbbreviation' ->
        metadata {serviceAbbreviation = serviceAbbreviation'}
    ServiceFullName -> \metadata ->
      f (serviceFullName metadata) <&> \serviceFullName' ->
        metadata {serviceFullName = serviceFullName'}
    ServiceId -> \metadata ->
      f (serviceId metadata) <&> \serviceId' ->
        metadata {serviceId = serviceId'}
    SignatureVersion -> \metadata ->
      f (signatureVersion metadata) <&> \signatureVersion' ->
        metadata {signatureVersion = signatureVersion'}
    SigningName -> \metadata ->
      f (signingName metadata) <&> \signingName' ->
        metadata {signingName = signingName'}
    TargetPrefix -> \metadata ->
      f (targetPrefix metadata) <&> \targetPrefix' ->
        metadata {targetPrefix = targetPrefix'}
    Uid -> \metadata ->
      f (uid metadata) <&> \uid' ->
        metadata {uid = uid'}
    XmlNamespace -> \metadata ->
      f (xmlNamespace metadata) <&> \xmlNamespace' ->
        metadata {xmlNamespace = xmlNamespace'}

parse :: Parser Tokens k e (Metadata Identity)
parse =
  recordB $
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

data ChecksumFormat = Md5 | Sha256
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data JsonVersion = V1_0 | V1_1
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data Protocol = Json | RestJson | RestXml | Query | Ec2 | ApiGateway
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

data SignatureVersion = Bearer | S3 | S3V4 | V2 | V4
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

test :: IO ()
test = do
  let dir = "../../scraps"
  files <- listDirectory dir
  for_ files $ \file -> do
    contents <- LBS.readFile $ dir ++ "/" ++ file
    either print (const $ pure ()) $ execParser parse $ lbsToTokens contents
