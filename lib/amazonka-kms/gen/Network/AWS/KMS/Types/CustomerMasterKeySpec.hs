{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.CustomerMasterKeySpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.CustomerMasterKeySpec where

import Network.AWS.Prelude

data CustomerMasterKeySpec
  = CMKSEccNistP256
  | CMKSEccNistP384
  | CMKSEccNistP521
  | CMKSEccSecgP256K1
  | CMKSRsa2048
  | CMKSRsa3072
  | CMKSRsa4096
  | CMKSSymmetricDefault
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

instance FromText CustomerMasterKeySpec where
  parser =
    takeLowerText >>= \case
      "ecc_nist_p256" -> pure CMKSEccNistP256
      "ecc_nist_p384" -> pure CMKSEccNistP384
      "ecc_nist_p521" -> pure CMKSEccNistP521
      "ecc_secg_p256k1" -> pure CMKSEccSecgP256K1
      "rsa_2048" -> pure CMKSRsa2048
      "rsa_3072" -> pure CMKSRsa3072
      "rsa_4096" -> pure CMKSRsa4096
      "symmetric_default" -> pure CMKSSymmetricDefault
      e ->
        fromTextError $
          "Failure parsing CustomerMasterKeySpec from value: '" <> e
            <> "'. Accepted values: ecc_nist_p256, ecc_nist_p384, ecc_nist_p521, ecc_secg_p256k1, rsa_2048, rsa_3072, rsa_4096, symmetric_default"

instance ToText CustomerMasterKeySpec where
  toText = \case
    CMKSEccNistP256 -> "ECC_NIST_P256"
    CMKSEccNistP384 -> "ECC_NIST_P384"
    CMKSEccNistP521 -> "ECC_NIST_P521"
    CMKSEccSecgP256K1 -> "ECC_SECG_P256K1"
    CMKSRsa2048 -> "RSA_2048"
    CMKSRsa3072 -> "RSA_3072"
    CMKSRsa4096 -> "RSA_4096"
    CMKSSymmetricDefault -> "SYMMETRIC_DEFAULT"

instance Hashable CustomerMasterKeySpec

instance NFData CustomerMasterKeySpec

instance ToByteString CustomerMasterKeySpec

instance ToQuery CustomerMasterKeySpec

instance ToHeader CustomerMasterKeySpec

instance ToJSON CustomerMasterKeySpec where
  toJSON = toJSONText

instance FromJSON CustomerMasterKeySpec where
  parseJSON = parseJSONText "CustomerMasterKeySpec"
