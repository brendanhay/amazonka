{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service where

import Botocore.Service.Metadata qualified as Metadata
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Error,
    FieldParser (..),
    decodeRecordB,
    withText,
  )
import Data.Functor ((<&>))
import Data.Functor.Barbie.Extended (FunctorB (..), Rec (..), TraversableB (..))
import Data.Functor.Barbie.Record
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import Data.Some (Some (..))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A service definition, as described by a @service-2.json@ file.
data Service f = Service
  { version :: f Text,
    metadata :: f (Metadata.Metadata Identity),
    operations :: f (Map Text Operation),
    shapes :: f (Map Text Shape),
    documentation :: f Text
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, TraversableB)

deriving stock instance (forall a. (Eq a) => Eq (f a)) => Eq (Service f)

deriving stock instance (forall a. (Show a) => Show (f a)) => Show (Service f)

instance RecordB Service where
  data Field Service a where
    Version :: Field Service Text
    Metadata :: Field Service (Metadata.Metadata Identity)
    Operations :: Field Service (Map Text Operation)
    Shapes :: Field Service (Map Text Shape)
    Documentation :: Field Service Text

  allFields =
    [ Some Version,
      Some Metadata,
      Some Operations,
      Some Shapes,
      Some Documentation
    ]

  fieldName = \case
    Version -> "version"
    Metadata -> "metadata"
    Operations -> "operations"
    Shapes -> "shapes"
    Documentation -> "documentation"

  fieldLens i f = case i of
    Version -> \service ->
      f (version service) <&> \version' -> service {version = version'}
    Metadata -> \service ->
      f (metadata service) <&> \metadata' -> service {metadata = metadata'}
    Operations -> \service ->
      f (operations service) <&> \operations' -> service {operations = operations'}
    Shapes -> \service ->
      f (shapes service) <&> \shapes' -> service {shapes = shapes'}
    Documentation -> \service ->
      f (documentation service) <&> \documentation' -> service {documentation = documentation'}

parse :: Tokens k e -> Either (Error e) (k, Service Identity)
parse =
  decodeRecordB $
    Service
      { version = FieldParser $ withText Right,
        metadata = FieldParser $ Metadata.parse,
        documentation = FieldParser $ withText Right
      }

data Operation = MkOperation deriving (Eq, Show, Generic)

data Shape = MkShape deriving (Eq, Show, Generic)
