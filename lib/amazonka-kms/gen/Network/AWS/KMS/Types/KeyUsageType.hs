{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyUsageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyUsageType where

import Network.AWS.Prelude

data KeyUsageType
  = EncryptDecrypt
  | SignVerify
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

instance FromText KeyUsageType where
  parser =
    takeLowerText >>= \case
      "encrypt_decrypt" -> pure EncryptDecrypt
      "sign_verify" -> pure SignVerify
      e ->
        fromTextError $
          "Failure parsing KeyUsageType from value: '" <> e
            <> "'. Accepted values: encrypt_decrypt, sign_verify"

instance ToText KeyUsageType where
  toText = \case
    EncryptDecrypt -> "ENCRYPT_DECRYPT"
    SignVerify -> "SIGN_VERIFY"

instance Hashable KeyUsageType

instance NFData KeyUsageType

instance ToByteString KeyUsageType

instance ToQuery KeyUsageType

instance ToHeader KeyUsageType

instance ToJSON KeyUsageType where
  toJSON = toJSONText

instance FromJSON KeyUsageType where
  parseJSON = parseJSONText "KeyUsageType"
