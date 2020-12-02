{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsEncryptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsEncryptionType where

import Network.AWS.Prelude

-- | Hls Encryption Type
data HlsEncryptionType
  = AES128
  | SampleAES
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

instance FromText HlsEncryptionType where
  parser =
    takeLowerText >>= \case
      "aes128" -> pure AES128
      "sample_aes" -> pure SampleAES
      e ->
        fromTextError $
          "Failure parsing HlsEncryptionType from value: '" <> e
            <> "'. Accepted values: aes128, sample_aes"

instance ToText HlsEncryptionType where
  toText = \case
    AES128 -> "AES128"
    SampleAES -> "SAMPLE_AES"

instance Hashable HlsEncryptionType

instance NFData HlsEncryptionType

instance ToByteString HlsEncryptionType

instance ToQuery HlsEncryptionType

instance ToHeader HlsEncryptionType

instance ToJSON HlsEncryptionType where
  toJSON = toJSONText

instance FromJSON HlsEncryptionType where
  parseJSON = parseJSONText "HlsEncryptionType"
