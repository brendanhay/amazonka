{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.DataKeyPairSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.DataKeyPairSpec where

import Network.AWS.Prelude

data DataKeyPairSpec
  = DKPSEccNistP256
  | DKPSEccNistP384
  | DKPSEccNistP521
  | DKPSEccSecgP256K1
  | DKPSRsa2048
  | DKPSRsa3072
  | DKPSRsa4096
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

instance FromText DataKeyPairSpec where
  parser =
    takeLowerText >>= \case
      "ecc_nist_p256" -> pure DKPSEccNistP256
      "ecc_nist_p384" -> pure DKPSEccNistP384
      "ecc_nist_p521" -> pure DKPSEccNistP521
      "ecc_secg_p256k1" -> pure DKPSEccSecgP256K1
      "rsa_2048" -> pure DKPSRsa2048
      "rsa_3072" -> pure DKPSRsa3072
      "rsa_4096" -> pure DKPSRsa4096
      e ->
        fromTextError $
          "Failure parsing DataKeyPairSpec from value: '" <> e
            <> "'. Accepted values: ecc_nist_p256, ecc_nist_p384, ecc_nist_p521, ecc_secg_p256k1, rsa_2048, rsa_3072, rsa_4096"

instance ToText DataKeyPairSpec where
  toText = \case
    DKPSEccNistP256 -> "ECC_NIST_P256"
    DKPSEccNistP384 -> "ECC_NIST_P384"
    DKPSEccNistP521 -> "ECC_NIST_P521"
    DKPSEccSecgP256K1 -> "ECC_SECG_P256K1"
    DKPSRsa2048 -> "RSA_2048"
    DKPSRsa3072 -> "RSA_3072"
    DKPSRsa4096 -> "RSA_4096"

instance Hashable DataKeyPairSpec

instance NFData DataKeyPairSpec

instance ToByteString DataKeyPairSpec

instance ToQuery DataKeyPairSpec

instance ToHeader DataKeyPairSpec

instance ToJSON DataKeyPairSpec where
  toJSON = toJSONText

instance FromJSON DataKeyPairSpec where
  parseJSON = parseJSONText "DataKeyPairSpec"
