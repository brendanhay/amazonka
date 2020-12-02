{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafKeyProviderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafKeyProviderType where

import Network.AWS.Prelude

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
data CmafKeyProviderType
  = Speke
  | StaticKey
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

instance FromText CmafKeyProviderType where
  parser =
    takeLowerText >>= \case
      "speke" -> pure Speke
      "static_key" -> pure StaticKey
      e ->
        fromTextError $
          "Failure parsing CmafKeyProviderType from value: '" <> e
            <> "'. Accepted values: speke, static_key"

instance ToText CmafKeyProviderType where
  toText = \case
    Speke -> "SPEKE"
    StaticKey -> "STATIC_KEY"

instance Hashable CmafKeyProviderType

instance NFData CmafKeyProviderType

instance ToByteString CmafKeyProviderType

instance ToQuery CmafKeyProviderType

instance ToHeader CmafKeyProviderType

instance ToJSON CmafKeyProviderType where
  toJSON = toJSONText

instance FromJSON CmafKeyProviderType where
  parseJSON = parseJSONText "CmafKeyProviderType"
