{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Metadata.ProtocolSettings
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Metadata.ProtocolSettings where

import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct (Parser, field, recordB, text)
import Data.Functor ((<&>))
import Data.Functor.Barbie.Extended (FunctorB, Rec (..), TraversableB)
import Data.Functor.Barbie.Record (RecordB (..))
import Data.Functor.Identity (Identity)
import Data.Some (Some (..))
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ProtocolSettings f = ProtocolSettings {h2 :: f Text}
  deriving stock (Generic)
  deriving anyclass (FunctorB, TraversableB)

deriving stock instance
  (forall a. (Eq a) => Eq (f a)) => Eq (ProtocolSettings f)

deriving stock instance
  (forall a. (Show a) => Show (f a)) => Show (ProtocolSettings f)

instance RecordB ProtocolSettings where
  data Field ProtocolSettings a where
    H2 :: Field ProtocolSettings Text

  allFields = [Some H2]

  fieldLens i f = case i of
    H2 -> \protocolSettings ->
      f (h2 protocolSettings) <&> \h2' -> protocolSettings {h2 = h2'}

parse :: Parser Tokens k e (ProtocolSettings Identity)
parse = recordB $ ProtocolSettings {h2 = field "h2" text}
