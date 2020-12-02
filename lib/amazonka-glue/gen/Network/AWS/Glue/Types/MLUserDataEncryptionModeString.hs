{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MLUserDataEncryptionModeString
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MLUserDataEncryptionModeString where

import Network.AWS.Prelude

data MLUserDataEncryptionModeString
  = MLUDEMSDisabled
  | MLUDEMSSseKMS
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

instance FromText MLUserDataEncryptionModeString where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MLUDEMSDisabled
      "sse-kms" -> pure MLUDEMSSseKMS
      e ->
        fromTextError $
          "Failure parsing MLUserDataEncryptionModeString from value: '" <> e
            <> "'. Accepted values: disabled, sse-kms"

instance ToText MLUserDataEncryptionModeString where
  toText = \case
    MLUDEMSDisabled -> "DISABLED"
    MLUDEMSSseKMS -> "SSE-KMS"

instance Hashable MLUserDataEncryptionModeString

instance NFData MLUserDataEncryptionModeString

instance ToByteString MLUserDataEncryptionModeString

instance ToQuery MLUserDataEncryptionModeString

instance ToHeader MLUserDataEncryptionModeString

instance ToJSON MLUserDataEncryptionModeString where
  toJSON = toJSONText

instance FromJSON MLUserDataEncryptionModeString where
  parseJSON = parseJSONText "MLUserDataEncryptionModeString"
