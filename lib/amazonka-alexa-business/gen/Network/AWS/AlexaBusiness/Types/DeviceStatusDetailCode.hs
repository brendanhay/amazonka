{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode
  ( DeviceStatusDetailCode
      ( DeviceStatusDetailCode',
        AssociationRejection,
        AuthenticationFailure,
        CertificateAuthorityAccessDenied,
        CertificateIssuingLimitExceeded,
        CredentialsAccessFailure,
        DHCPFailure,
        DNSFailure,
        DeviceSoftwareUpdateNeeded,
        DeviceWasOffline,
        InternetUnavailable,
        InvalidCertificateAuthority,
        InvalidPasswordState,
        NetworkProfileNotFound,
        PasswordManagerAccessDenied,
        PasswordNotFound,
        TLSVersionMismatch,
        UnknownFailure
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeviceStatusDetailCode = DeviceStatusDetailCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AssociationRejection :: DeviceStatusDetailCode
pattern AssociationRejection = DeviceStatusDetailCode' "ASSOCIATION_REJECTION"

pattern AuthenticationFailure :: DeviceStatusDetailCode
pattern AuthenticationFailure = DeviceStatusDetailCode' "AUTHENTICATION_FAILURE"

pattern CertificateAuthorityAccessDenied :: DeviceStatusDetailCode
pattern CertificateAuthorityAccessDenied = DeviceStatusDetailCode' "CERTIFICATE_AUTHORITY_ACCESS_DENIED"

pattern CertificateIssuingLimitExceeded :: DeviceStatusDetailCode
pattern CertificateIssuingLimitExceeded = DeviceStatusDetailCode' "CERTIFICATE_ISSUING_LIMIT_EXCEEDED"

pattern CredentialsAccessFailure :: DeviceStatusDetailCode
pattern CredentialsAccessFailure = DeviceStatusDetailCode' "CREDENTIALS_ACCESS_FAILURE"

pattern DHCPFailure :: DeviceStatusDetailCode
pattern DHCPFailure = DeviceStatusDetailCode' "DHCP_FAILURE"

pattern DNSFailure :: DeviceStatusDetailCode
pattern DNSFailure = DeviceStatusDetailCode' "DNS_FAILURE"

pattern DeviceSoftwareUpdateNeeded :: DeviceStatusDetailCode
pattern DeviceSoftwareUpdateNeeded = DeviceStatusDetailCode' "DEVICE_SOFTWARE_UPDATE_NEEDED"

pattern DeviceWasOffline :: DeviceStatusDetailCode
pattern DeviceWasOffline = DeviceStatusDetailCode' "DEVICE_WAS_OFFLINE"

pattern InternetUnavailable :: DeviceStatusDetailCode
pattern InternetUnavailable = DeviceStatusDetailCode' "INTERNET_UNAVAILABLE"

pattern InvalidCertificateAuthority :: DeviceStatusDetailCode
pattern InvalidCertificateAuthority = DeviceStatusDetailCode' "INVALID_CERTIFICATE_AUTHORITY"

pattern InvalidPasswordState :: DeviceStatusDetailCode
pattern InvalidPasswordState = DeviceStatusDetailCode' "INVALID_PASSWORD_STATE"

pattern NetworkProfileNotFound :: DeviceStatusDetailCode
pattern NetworkProfileNotFound = DeviceStatusDetailCode' "NETWORK_PROFILE_NOT_FOUND"

pattern PasswordManagerAccessDenied :: DeviceStatusDetailCode
pattern PasswordManagerAccessDenied = DeviceStatusDetailCode' "PASSWORD_MANAGER_ACCESS_DENIED"

pattern PasswordNotFound :: DeviceStatusDetailCode
pattern PasswordNotFound = DeviceStatusDetailCode' "PASSWORD_NOT_FOUND"

pattern TLSVersionMismatch :: DeviceStatusDetailCode
pattern TLSVersionMismatch = DeviceStatusDetailCode' "TLS_VERSION_MISMATCH"

pattern UnknownFailure :: DeviceStatusDetailCode
pattern UnknownFailure = DeviceStatusDetailCode' "UNKNOWN_FAILURE"

{-# COMPLETE
  AssociationRejection,
  AuthenticationFailure,
  CertificateAuthorityAccessDenied,
  CertificateIssuingLimitExceeded,
  CredentialsAccessFailure,
  DHCPFailure,
  DNSFailure,
  DeviceSoftwareUpdateNeeded,
  DeviceWasOffline,
  InternetUnavailable,
  InvalidCertificateAuthority,
  InvalidPasswordState,
  NetworkProfileNotFound,
  PasswordManagerAccessDenied,
  PasswordNotFound,
  TLSVersionMismatch,
  UnknownFailure,
  DeviceStatusDetailCode'
  #-}
