{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsKeyProviderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsKeyProviderType where

import Network.AWS.Prelude

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
data HlsKeyProviderType
  = HKPTSpeke
  | HKPTStaticKey
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

instance FromText HlsKeyProviderType where
  parser =
    takeLowerText >>= \case
      "speke" -> pure HKPTSpeke
      "static_key" -> pure HKPTStaticKey
      e ->
        fromTextError $
          "Failure parsing HlsKeyProviderType from value: '" <> e
            <> "'. Accepted values: speke, static_key"

instance ToText HlsKeyProviderType where
  toText = \case
    HKPTSpeke -> "SPEKE"
    HKPTStaticKey -> "STATIC_KEY"

instance Hashable HlsKeyProviderType

instance NFData HlsKeyProviderType

instance ToByteString HlsKeyProviderType

instance ToQuery HlsKeyProviderType

instance ToHeader HlsKeyProviderType

instance ToJSON HlsKeyProviderType where
  toJSON = toJSONText

instance FromJSON HlsKeyProviderType where
  parseJSON = parseJSONText "HlsKeyProviderType"
