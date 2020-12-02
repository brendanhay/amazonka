{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.KeyUsageName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.KeyUsageName where

import Network.AWS.Prelude

data KeyUsageName
  = KUNAny
  | KUNCertificateSigning
  | KUNCrlSigning
  | KUNCustom
  | KUNDataEncipherment
  | KUNDecipherOnly
  | KUNDigitalSignature
  | KUNEncipherOnly
  | KUNKeyAgreement
  | KUNKeyEncipherment
  | KUNNonRepudiation
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

instance FromText KeyUsageName where
  parser =
    takeLowerText >>= \case
      "any" -> pure KUNAny
      "certificate_signing" -> pure KUNCertificateSigning
      "crl_signing" -> pure KUNCrlSigning
      "custom" -> pure KUNCustom
      "data_encipherment" -> pure KUNDataEncipherment
      "decipher_only" -> pure KUNDecipherOnly
      "digital_signature" -> pure KUNDigitalSignature
      "encipher_only" -> pure KUNEncipherOnly
      "key_agreement" -> pure KUNKeyAgreement
      "key_encipherment" -> pure KUNKeyEncipherment
      "non_repudiation" -> pure KUNNonRepudiation
      e ->
        fromTextError $
          "Failure parsing KeyUsageName from value: '" <> e
            <> "'. Accepted values: any, certificate_signing, crl_signing, custom, data_encipherment, decipher_only, digital_signature, encipher_only, key_agreement, key_encipherment, non_repudiation"

instance ToText KeyUsageName where
  toText = \case
    KUNAny -> "ANY"
    KUNCertificateSigning -> "CERTIFICATE_SIGNING"
    KUNCrlSigning -> "CRL_SIGNING"
    KUNCustom -> "CUSTOM"
    KUNDataEncipherment -> "DATA_ENCIPHERMENT"
    KUNDecipherOnly -> "DECIPHER_ONLY"
    KUNDigitalSignature -> "DIGITAL_SIGNATURE"
    KUNEncipherOnly -> "ENCIPHER_ONLY"
    KUNKeyAgreement -> "KEY_AGREEMENT"
    KUNKeyEncipherment -> "KEY_ENCIPHERMENT"
    KUNNonRepudiation -> "NON_REPUDIATION"

instance Hashable KeyUsageName

instance NFData KeyUsageName

instance ToByteString KeyUsageName

instance ToQuery KeyUsageName

instance ToHeader KeyUsageName

instance ToJSON KeyUsageName where
  toJSON = toJSONText

instance FromJSON KeyUsageName where
  parseJSON = parseJSONText "KeyUsageName"
