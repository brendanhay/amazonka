{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Metadata.Metadata
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Metadata where

import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Error,
    FieldParser (..),
    decodeRecordB,
    withText,
  )
import Data.Functor ((<&>))
import Data.Functor.Barbie.Extended (FunctorB, Rec (..), TraversableB)
import Data.Functor.Barbie.Record (RecordB (..))
import Data.Functor.Identity (Identity)
import Data.Some (Some (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data Metadata f = Metadata
  { apiVersion :: f Text,
    endpointPrefix :: f Text,
    protocol :: f Text,
    serviceFullName :: f Text,
    serviceId :: f Text,
    signatureVersion :: f Text,
    uid :: f Text
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, TraversableB)

deriving stock instance (forall a. (Eq a) => Eq (f a)) => Eq (Metadata f)

deriving stock instance (forall a. (Show a) => Show (f a)) => Show (Metadata f)

instance RecordB Metadata where
  data Field Metadata a where
    ApiVersion :: Field Metadata Text
    EndpointPrefix :: Field Metadata Text
    Protocol :: Field Metadata Text
    ServiceFullName :: Field Metadata Text
    ServiceId :: Field Metadata Text
    SignatureVersion :: Field Metadata Text
    Uid :: Field Metadata Text

  allFields =
    [ Some ApiVersion,
      Some EndpointPrefix,
      Some Protocol,
      Some ServiceFullName,
      Some ServiceId,
      Some SignatureVersion,
      Some Uid
    ]

  fieldName = \case
    ApiVersion -> "apiVersion"
    EndpointPrefix -> "endpointPrefix"
    Protocol -> "protocol"
    ServiceFullName -> "serviceFullName"
    ServiceId -> "serviceId"
    SignatureVersion -> "signatureVersion"
    Uid -> "uid"

  fieldLens i f = case i of
    ApiVersion -> \metadata ->
      f (apiVersion metadata) <&> \apiVersion' ->
        metadata {apiVersion = apiVersion'}
    EndpointPrefix -> \metadata ->
      f (endpointPrefix metadata) <&> \endpointPrefix' ->
        metadata {endpointPrefix = endpointPrefix'}
    Protocol -> \metadata ->
      f (protocol metadata) <&> \protocol' ->
        metadata {protocol = protocol'}
    ServiceFullName -> \metadata ->
      f (serviceFullName metadata) <&> \serviceFullName' ->
        metadata {serviceFullName = serviceFullName'}
    ServiceId -> \metadata ->
      f (serviceId metadata) <&> \serviceId' ->
        metadata {serviceId = serviceId'}
    SignatureVersion -> \metadata ->
      f (signatureVersion metadata) <&> \signatureVersion' ->
        metadata {signatureVersion = signatureVersion'}
    Uid -> \metadata ->
      f (uid metadata) <&> \uid' ->
        metadata {uid = uid'}

parse :: Tokens k e -> Either (Error e) (k, Metadata Identity)
parse =
  decodeRecordB $
    Metadata
      { apiVersion = FieldParser $ withText Right,
        endpointPrefix = FieldParser $ withText Right,
        protocol = FieldParser $ withText Right,
        serviceFullName = FieldParser $ withText Right,
        serviceId = FieldParser $ withText Right,
        signatureVersion = FieldParser $ withText Right,
        uid = FieldParser $ withText Right
      }
