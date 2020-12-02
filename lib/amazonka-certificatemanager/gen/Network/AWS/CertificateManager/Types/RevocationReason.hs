{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.RevocationReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.RevocationReason where

import Network.AWS.Prelude

data RevocationReason
  = AACompromise
  | AffiliationChanged
  | CaCompromise
  | CertificateHold
  | CessationOfOperation
  | KeyCompromise
  | PrivilegeWithdrawn
  | RemoveFromCrl
  | Superceded
  | Unspecified
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

instance FromText RevocationReason where
  parser =
    takeLowerText >>= \case
      "a_a_compromise" -> pure AACompromise
      "affiliation_changed" -> pure AffiliationChanged
      "ca_compromise" -> pure CaCompromise
      "certificate_hold" -> pure CertificateHold
      "cessation_of_operation" -> pure CessationOfOperation
      "key_compromise" -> pure KeyCompromise
      "privilege_withdrawn" -> pure PrivilegeWithdrawn
      "remove_from_crl" -> pure RemoveFromCrl
      "superceded" -> pure Superceded
      "unspecified" -> pure Unspecified
      e ->
        fromTextError $
          "Failure parsing RevocationReason from value: '" <> e
            <> "'. Accepted values: a_a_compromise, affiliation_changed, ca_compromise, certificate_hold, cessation_of_operation, key_compromise, privilege_withdrawn, remove_from_crl, superceded, unspecified"

instance ToText RevocationReason where
  toText = \case
    AACompromise -> "A_A_COMPROMISE"
    AffiliationChanged -> "AFFILIATION_CHANGED"
    CaCompromise -> "CA_COMPROMISE"
    CertificateHold -> "CERTIFICATE_HOLD"
    CessationOfOperation -> "CESSATION_OF_OPERATION"
    KeyCompromise -> "KEY_COMPROMISE"
    PrivilegeWithdrawn -> "PRIVILEGE_WITHDRAWN"
    RemoveFromCrl -> "REMOVE_FROM_CRL"
    Superceded -> "SUPERCEDED"
    Unspecified -> "UNSPECIFIED"

instance Hashable RevocationReason

instance NFData RevocationReason

instance ToByteString RevocationReason

instance ToQuery RevocationReason

instance ToHeader RevocationReason

instance FromJSON RevocationReason where
  parseJSON = parseJSONText "RevocationReason"
