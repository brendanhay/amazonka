{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.EncryptionMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.EncryptionMethod where

import Network.AWS.Prelude

data EncryptionMethod
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

instance FromText EncryptionMethod where
  parser =
    takeLowerText >>= \case
      "aes_128" -> pure AES128
      "sample_aes" -> pure SampleAES
      e ->
        fromTextError $
          "Failure parsing EncryptionMethod from value: '" <> e
            <> "'. Accepted values: aes_128, sample_aes"

instance ToText EncryptionMethod where
  toText = \case
    AES128 -> "AES_128"
    SampleAES -> "SAMPLE_AES"

instance Hashable EncryptionMethod

instance NFData EncryptionMethod

instance ToByteString EncryptionMethod

instance ToQuery EncryptionMethod

instance ToHeader EncryptionMethod

instance ToJSON EncryptionMethod where
  toJSON = toJSONText

instance FromJSON EncryptionMethod where
  parseJSON = parseJSONText "EncryptionMethod"
