{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode where

import Network.AWS.Prelude

data DeviceStatusDetailCode
  = AssociationRejection
  | AuthenticationFailure
  | CertificateAuthorityAccessDenied
  | CertificateIssuingLimitExceeded
  | CredentialsAccessFailure
  | DHCPFailure
  | DNSFailure
  | DeviceSoftwareUpdateNeeded
  | DeviceWasOffline
  | InternetUnavailable
  | InvalidCertificateAuthority
  | InvalidPasswordState
  | NetworkProfileNotFound
  | PasswordManagerAccessDenied
  | PasswordNotFound
  | TLSVersionMismatch
  | UnknownFailure
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

instance FromText DeviceStatusDetailCode where
  parser =
    takeLowerText >>= \case
      "association_rejection" -> pure AssociationRejection
      "authentication_failure" -> pure AuthenticationFailure
      "certificate_authority_access_denied" -> pure CertificateAuthorityAccessDenied
      "certificate_issuing_limit_exceeded" -> pure CertificateIssuingLimitExceeded
      "credentials_access_failure" -> pure CredentialsAccessFailure
      "dhcp_failure" -> pure DHCPFailure
      "dns_failure" -> pure DNSFailure
      "device_software_update_needed" -> pure DeviceSoftwareUpdateNeeded
      "device_was_offline" -> pure DeviceWasOffline
      "internet_unavailable" -> pure InternetUnavailable
      "invalid_certificate_authority" -> pure InvalidCertificateAuthority
      "invalid_password_state" -> pure InvalidPasswordState
      "network_profile_not_found" -> pure NetworkProfileNotFound
      "password_manager_access_denied" -> pure PasswordManagerAccessDenied
      "password_not_found" -> pure PasswordNotFound
      "tls_version_mismatch" -> pure TLSVersionMismatch
      "unknown_failure" -> pure UnknownFailure
      e ->
        fromTextError $
          "Failure parsing DeviceStatusDetailCode from value: '" <> e
            <> "'. Accepted values: association_rejection, authentication_failure, certificate_authority_access_denied, certificate_issuing_limit_exceeded, credentials_access_failure, dhcp_failure, dns_failure, device_software_update_needed, device_was_offline, internet_unavailable, invalid_certificate_authority, invalid_password_state, network_profile_not_found, password_manager_access_denied, password_not_found, tls_version_mismatch, unknown_failure"

instance ToText DeviceStatusDetailCode where
  toText = \case
    AssociationRejection -> "ASSOCIATION_REJECTION"
    AuthenticationFailure -> "AUTHENTICATION_FAILURE"
    CertificateAuthorityAccessDenied -> "CERTIFICATE_AUTHORITY_ACCESS_DENIED"
    CertificateIssuingLimitExceeded -> "CERTIFICATE_ISSUING_LIMIT_EXCEEDED"
    CredentialsAccessFailure -> "CREDENTIALS_ACCESS_FAILURE"
    DHCPFailure -> "DHCP_FAILURE"
    DNSFailure -> "DNS_FAILURE"
    DeviceSoftwareUpdateNeeded -> "DEVICE_SOFTWARE_UPDATE_NEEDED"
    DeviceWasOffline -> "DEVICE_WAS_OFFLINE"
    InternetUnavailable -> "INTERNET_UNAVAILABLE"
    InvalidCertificateAuthority -> "INVALID_CERTIFICATE_AUTHORITY"
    InvalidPasswordState -> "INVALID_PASSWORD_STATE"
    NetworkProfileNotFound -> "NETWORK_PROFILE_NOT_FOUND"
    PasswordManagerAccessDenied -> "PASSWORD_MANAGER_ACCESS_DENIED"
    PasswordNotFound -> "PASSWORD_NOT_FOUND"
    TLSVersionMismatch -> "TLS_VERSION_MISMATCH"
    UnknownFailure -> "UNKNOWN_FAILURE"

instance Hashable DeviceStatusDetailCode

instance NFData DeviceStatusDetailCode

instance ToByteString DeviceStatusDetailCode

instance ToQuery DeviceStatusDetailCode

instance ToHeader DeviceStatusDetailCode

instance FromJSON DeviceStatusDetailCode where
  parseJSON = parseJSONText "DeviceStatusDetailCode"
