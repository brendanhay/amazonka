{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.RevocationReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.RevocationReason where

import Network.AWS.Prelude

data RevocationReason
  = AACompromise
  | AffiliationChanged
  | CertificateAuthorityCompromise
  | CessationOfOperation
  | KeyCompromise
  | PrivilegeWithdrawn
  | Superseded
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
      "certificate_authority_compromise" -> pure CertificateAuthorityCompromise
      "cessation_of_operation" -> pure CessationOfOperation
      "key_compromise" -> pure KeyCompromise
      "privilege_withdrawn" -> pure PrivilegeWithdrawn
      "superseded" -> pure Superseded
      "unspecified" -> pure Unspecified
      e ->
        fromTextError $
          "Failure parsing RevocationReason from value: '" <> e
            <> "'. Accepted values: a_a_compromise, affiliation_changed, certificate_authority_compromise, cessation_of_operation, key_compromise, privilege_withdrawn, superseded, unspecified"

instance ToText RevocationReason where
  toText = \case
    AACompromise -> "A_A_COMPROMISE"
    AffiliationChanged -> "AFFILIATION_CHANGED"
    CertificateAuthorityCompromise -> "CERTIFICATE_AUTHORITY_COMPROMISE"
    CessationOfOperation -> "CESSATION_OF_OPERATION"
    KeyCompromise -> "KEY_COMPROMISE"
    PrivilegeWithdrawn -> "PRIVILEGE_WITHDRAWN"
    Superseded -> "SUPERSEDED"
    Unspecified -> "UNSPECIFIED"

instance Hashable RevocationReason

instance NFData RevocationReason

instance ToByteString RevocationReason

instance ToQuery RevocationReason

instance ToHeader RevocationReason

instance ToJSON RevocationReason where
  toJSON = toJSONText
