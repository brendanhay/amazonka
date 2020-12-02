{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.WrappingKeySpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.WrappingKeySpec where

import Network.AWS.Prelude

data WrappingKeySpec = Rsa2048
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText WrappingKeySpec where
  parser =
    takeLowerText >>= \case
      "rsa_2048" -> pure Rsa2048
      e ->
        fromTextError $
          "Failure parsing WrappingKeySpec from value: '" <> e
            <> "'. Accepted values: rsa_2048"

instance ToText WrappingKeySpec where
  toText = \case
    Rsa2048 -> "RSA_2048"

instance Hashable WrappingKeySpec

instance NFData WrappingKeySpec

instance ToByteString WrappingKeySpec

instance ToQuery WrappingKeySpec

instance ToHeader WrappingKeySpec

instance ToJSON WrappingKeySpec where
  toJSON = toJSONText
