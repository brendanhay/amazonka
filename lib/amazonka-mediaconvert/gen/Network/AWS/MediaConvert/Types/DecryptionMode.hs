{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DecryptionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DecryptionMode where

import Network.AWS.Prelude

-- | Specify the encryption mode that you used to encrypt your input files.
data DecryptionMode
  = DMAESCbc
  | DMAESCtr
  | DMAESGCM
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

instance FromText DecryptionMode where
  parser =
    takeLowerText >>= \case
      "aes_cbc" -> pure DMAESCbc
      "aes_ctr" -> pure DMAESCtr
      "aes_gcm" -> pure DMAESGCM
      e ->
        fromTextError $
          "Failure parsing DecryptionMode from value: '" <> e
            <> "'. Accepted values: aes_cbc, aes_ctr, aes_gcm"

instance ToText DecryptionMode where
  toText = \case
    DMAESCbc -> "AES_CBC"
    DMAESCtr -> "AES_CTR"
    DMAESGCM -> "AES_GCM"

instance Hashable DecryptionMode

instance NFData DecryptionMode

instance ToByteString DecryptionMode

instance ToQuery DecryptionMode

instance ToHeader DecryptionMode

instance ToJSON DecryptionMode where
  toJSON = toJSONText

instance FromJSON DecryptionMode where
  parseJSON = parseJSONText "DecryptionMode"
