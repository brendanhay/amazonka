{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsEncryptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsEncryptionType where

import Network.AWS.Prelude

-- | Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
data HlsEncryptionType
  = HETAES128
  | HETSampleAES
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
      "aes128" -> pure HETAES128
      "sample_aes" -> pure HETSampleAES
      e ->
        fromTextError $
          "Failure parsing HlsEncryptionType from value: '" <> e
            <> "'. Accepted values: aes128, sample_aes"

instance ToText HlsEncryptionType where
  toText = \case
    HETAES128 -> "AES128"
    HETSampleAES -> "SAMPLE_AES"

instance Hashable HlsEncryptionType

instance NFData HlsEncryptionType

instance ToByteString HlsEncryptionType

instance ToQuery HlsEncryptionType

instance ToHeader HlsEncryptionType

instance ToJSON HlsEncryptionType where
  toJSON = toJSONText

instance FromJSON HlsEncryptionType where
  parseJSON = parseJSONText "HlsEncryptionType"
