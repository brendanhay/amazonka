{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateFailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateFailureReason where

import Network.AWS.Prelude

data LoadBalancerTLSCertificateFailureReason
  = AdditionalVerificationRequired
  | DomainNotAllowed
  | InvalidPublicDomain
  | NoAvailableContacts
  | Other
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

instance FromText LoadBalancerTLSCertificateFailureReason where
  parser =
    takeLowerText >>= \case
      "additional_verification_required" -> pure AdditionalVerificationRequired
      "domain_not_allowed" -> pure DomainNotAllowed
      "invalid_public_domain" -> pure InvalidPublicDomain
      "no_available_contacts" -> pure NoAvailableContacts
      "other" -> pure Other
      e ->
        fromTextError $
          "Failure parsing LoadBalancerTLSCertificateFailureReason from value: '" <> e
            <> "'. Accepted values: additional_verification_required, domain_not_allowed, invalid_public_domain, no_available_contacts, other"

instance ToText LoadBalancerTLSCertificateFailureReason where
  toText = \case
    AdditionalVerificationRequired -> "ADDITIONAL_VERIFICATION_REQUIRED"
    DomainNotAllowed -> "DOMAIN_NOT_ALLOWED"
    InvalidPublicDomain -> "INVALID_PUBLIC_DOMAIN"
    NoAvailableContacts -> "NO_AVAILABLE_CONTACTS"
    Other -> "OTHER"

instance Hashable LoadBalancerTLSCertificateFailureReason

instance NFData LoadBalancerTLSCertificateFailureReason

instance ToByteString LoadBalancerTLSCertificateFailureReason

instance ToQuery LoadBalancerTLSCertificateFailureReason

instance ToHeader LoadBalancerTLSCertificateFailureReason

instance FromJSON LoadBalancerTLSCertificateFailureReason where
  parseJSON = parseJSONText "LoadBalancerTLSCertificateFailureReason"
