{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.SigningAlgorithmSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.SigningAlgorithmSpec where

import Network.AWS.Prelude

data SigningAlgorithmSpec
  = EcdsaSha256
  | EcdsaSha384
  | EcdsaSha512
  | RsassaPKCS1V15Sha256
  | RsassaPKCS1V15Sha384
  | RsassaPKCS1V15Sha512
  | RsassaPssSha256
  | RsassaPssSha384
  | RsassaPssSha512
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

instance FromText SigningAlgorithmSpec where
  parser =
    takeLowerText >>= \case
      "ecdsa_sha_256" -> pure EcdsaSha256
      "ecdsa_sha_384" -> pure EcdsaSha384
      "ecdsa_sha_512" -> pure EcdsaSha512
      "rsassa_pkcs1_v1_5_sha_256" -> pure RsassaPKCS1V15Sha256
      "rsassa_pkcs1_v1_5_sha_384" -> pure RsassaPKCS1V15Sha384
      "rsassa_pkcs1_v1_5_sha_512" -> pure RsassaPKCS1V15Sha512
      "rsassa_pss_sha_256" -> pure RsassaPssSha256
      "rsassa_pss_sha_384" -> pure RsassaPssSha384
      "rsassa_pss_sha_512" -> pure RsassaPssSha512
      e ->
        fromTextError $
          "Failure parsing SigningAlgorithmSpec from value: '" <> e
            <> "'. Accepted values: ecdsa_sha_256, ecdsa_sha_384, ecdsa_sha_512, rsassa_pkcs1_v1_5_sha_256, rsassa_pkcs1_v1_5_sha_384, rsassa_pkcs1_v1_5_sha_512, rsassa_pss_sha_256, rsassa_pss_sha_384, rsassa_pss_sha_512"

instance ToText SigningAlgorithmSpec where
  toText = \case
    EcdsaSha256 -> "ECDSA_SHA_256"
    EcdsaSha384 -> "ECDSA_SHA_384"
    EcdsaSha512 -> "ECDSA_SHA_512"
    RsassaPKCS1V15Sha256 -> "RSASSA_PKCS1_V1_5_SHA_256"
    RsassaPKCS1V15Sha384 -> "RSASSA_PKCS1_V1_5_SHA_384"
    RsassaPKCS1V15Sha512 -> "RSASSA_PKCS1_V1_5_SHA_512"
    RsassaPssSha256 -> "RSASSA_PSS_SHA_256"
    RsassaPssSha384 -> "RSASSA_PSS_SHA_384"
    RsassaPssSha512 -> "RSASSA_PSS_SHA_512"

instance Hashable SigningAlgorithmSpec

instance NFData SigningAlgorithmSpec

instance ToByteString SigningAlgorithmSpec

instance ToQuery SigningAlgorithmSpec

instance ToHeader SigningAlgorithmSpec

instance ToJSON SigningAlgorithmSpec where
  toJSON = toJSONText

instance FromJSON SigningAlgorithmSpec where
  parseJSON = parseJSONText "SigningAlgorithmSpec"
