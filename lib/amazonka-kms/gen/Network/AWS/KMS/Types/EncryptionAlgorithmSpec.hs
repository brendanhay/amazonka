{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.EncryptionAlgorithmSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.EncryptionAlgorithmSpec where

import Network.AWS.Prelude

data EncryptionAlgorithmSpec
  = RsaesOaepSha1
  | RsaesOaepSha256
  | SymmetricDefault
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

instance FromText EncryptionAlgorithmSpec where
  parser =
    takeLowerText >>= \case
      "rsaes_oaep_sha_1" -> pure RsaesOaepSha1
      "rsaes_oaep_sha_256" -> pure RsaesOaepSha256
      "symmetric_default" -> pure SymmetricDefault
      e ->
        fromTextError $
          "Failure parsing EncryptionAlgorithmSpec from value: '" <> e
            <> "'. Accepted values: rsaes_oaep_sha_1, rsaes_oaep_sha_256, symmetric_default"

instance ToText EncryptionAlgorithmSpec where
  toText = \case
    RsaesOaepSha1 -> "RSAES_OAEP_SHA_1"
    RsaesOaepSha256 -> "RSAES_OAEP_SHA_256"
    SymmetricDefault -> "SYMMETRIC_DEFAULT"

instance Hashable EncryptionAlgorithmSpec

instance NFData EncryptionAlgorithmSpec

instance ToByteString EncryptionAlgorithmSpec

instance ToQuery EncryptionAlgorithmSpec

instance ToHeader EncryptionAlgorithmSpec

instance ToJSON EncryptionAlgorithmSpec where
  toJSON = toJSONText

instance FromJSON EncryptionAlgorithmSpec where
  parseJSON = parseJSONText "EncryptionAlgorithmSpec"
