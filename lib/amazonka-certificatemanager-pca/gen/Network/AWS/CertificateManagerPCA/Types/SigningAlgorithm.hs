{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm where

import Network.AWS.Prelude

data SigningAlgorithm
  = SHA256WITHECDSA
  | SHA256WITHRSA
  | SHA384WITHECDSA
  | SHA384WITHRSA
  | SHA512WITHECDSA
  | SHA512WITHRSA
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

instance FromText SigningAlgorithm where
  parser =
    takeLowerText >>= \case
      "sha256withecdsa" -> pure SHA256WITHECDSA
      "sha256withrsa" -> pure SHA256WITHRSA
      "sha384withecdsa" -> pure SHA384WITHECDSA
      "sha384withrsa" -> pure SHA384WITHRSA
      "sha512withecdsa" -> pure SHA512WITHECDSA
      "sha512withrsa" -> pure SHA512WITHRSA
      e ->
        fromTextError $
          "Failure parsing SigningAlgorithm from value: '" <> e
            <> "'. Accepted values: sha256withecdsa, sha256withrsa, sha384withecdsa, sha384withrsa, sha512withecdsa, sha512withrsa"

instance ToText SigningAlgorithm where
  toText = \case
    SHA256WITHECDSA -> "SHA256WITHECDSA"
    SHA256WITHRSA -> "SHA256WITHRSA"
    SHA384WITHECDSA -> "SHA384WITHECDSA"
    SHA384WITHRSA -> "SHA384WITHRSA"
    SHA512WITHECDSA -> "SHA512WITHECDSA"
    SHA512WITHRSA -> "SHA512WITHRSA"

instance Hashable SigningAlgorithm

instance NFData SigningAlgorithm

instance ToByteString SigningAlgorithm

instance ToQuery SigningAlgorithm

instance ToHeader SigningAlgorithm

instance ToJSON SigningAlgorithm where
  toJSON = toJSONText

instance FromJSON SigningAlgorithm where
  parseJSON = parseJSONText "SigningAlgorithm"
