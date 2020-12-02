{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafEncryptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafEncryptionType where

import Network.AWS.Prelude

-- | Specify the encryption scheme that you want the service to use when encrypting your CMAF segments. Choose AES-CBC subsample (SAMPLE-AES) or AES_CTR (AES-CTR).
data CmafEncryptionType
  = AESCtr
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

instance FromText CmafEncryptionType where
  parser =
    takeLowerText >>= \case
      "aes_ctr" -> pure AESCtr
      "sample_aes" -> pure SampleAES
      e ->
        fromTextError $
          "Failure parsing CmafEncryptionType from value: '" <> e
            <> "'. Accepted values: aes_ctr, sample_aes"

instance ToText CmafEncryptionType where
  toText = \case
    AESCtr -> "AES_CTR"
    SampleAES -> "SAMPLE_AES"

instance Hashable CmafEncryptionType

instance NFData CmafEncryptionType

instance ToByteString CmafEncryptionType

instance ToQuery CmafEncryptionType

instance ToHeader CmafEncryptionType

instance ToJSON CmafEncryptionType where
  toJSON = toJSONText

instance FromJSON CmafEncryptionType where
  parseJSON = parseJSONText "CmafEncryptionType"
