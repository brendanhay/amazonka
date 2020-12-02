{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.KeyAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.KeyAlgorithm where

import Network.AWS.Prelude

data KeyAlgorithm
  = EcPRIME256V1
  | EcSECP384R1
  | EcSECP521R1
  | Rsa1024
  | Rsa2048
  | Rsa4096
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

instance FromText KeyAlgorithm where
  parser =
    takeLowerText >>= \case
      "ec_prime256v1" -> pure EcPRIME256V1
      "ec_secp384r1" -> pure EcSECP384R1
      "ec_secp521r1" -> pure EcSECP521R1
      "rsa_1024" -> pure Rsa1024
      "rsa_2048" -> pure Rsa2048
      "rsa_4096" -> pure Rsa4096
      e ->
        fromTextError $
          "Failure parsing KeyAlgorithm from value: '" <> e
            <> "'. Accepted values: ec_prime256v1, ec_secp384r1, ec_secp521r1, rsa_1024, rsa_2048, rsa_4096"

instance ToText KeyAlgorithm where
  toText = \case
    EcPRIME256V1 -> "EC_prime256v1"
    EcSECP384R1 -> "EC_secp384r1"
    EcSECP521R1 -> "EC_secp521r1"
    Rsa1024 -> "RSA_1024"
    Rsa2048 -> "RSA_2048"
    Rsa4096 -> "RSA_4096"

instance Hashable KeyAlgorithm

instance NFData KeyAlgorithm

instance ToByteString KeyAlgorithm

instance ToQuery KeyAlgorithm

instance ToHeader KeyAlgorithm

instance ToJSON KeyAlgorithm where
  toJSON = toJSONText

instance FromJSON KeyAlgorithm where
  parseJSON = parseJSONText "KeyAlgorithm"
