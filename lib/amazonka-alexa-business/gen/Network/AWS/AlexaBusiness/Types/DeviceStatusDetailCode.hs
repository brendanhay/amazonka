{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode
  ( DeviceStatusDetailCode
    ( DeviceStatusDetailCode'
    , DeviceStatusDetailCodeDeviceSoftwareUpdateNeeded
    , DeviceStatusDetailCodeDeviceWasOffline
    , DeviceStatusDetailCodeCredentialsAccessFailure
    , DeviceStatusDetailCodeTlsVersionMismatch
    , DeviceStatusDetailCodeAssociationRejection
    , DeviceStatusDetailCodeAuthenticationFailure
    , DeviceStatusDetailCodeDhcpFailure
    , DeviceStatusDetailCodeInternetUnavailable
    , DeviceStatusDetailCodeDnsFailure
    , DeviceStatusDetailCodeUnknownFailure
    , DeviceStatusDetailCodeCertificateIssuingLimitExceeded
    , DeviceStatusDetailCodeInvalidCertificateAuthority
    , DeviceStatusDetailCodeNetworkProfileNotFound
    , DeviceStatusDetailCodeInvalidPasswordState
    , DeviceStatusDetailCodePasswordNotFound
    , DeviceStatusDetailCodePasswordManagerAccessDenied
    , DeviceStatusDetailCodeCertificateAuthorityAccessDenied
    , fromDeviceStatusDetailCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DeviceStatusDetailCode = DeviceStatusDetailCode'{fromDeviceStatusDetailCode
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern DeviceStatusDetailCodeDeviceSoftwareUpdateNeeded :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeDeviceSoftwareUpdateNeeded = DeviceStatusDetailCode' "DEVICE_SOFTWARE_UPDATE_NEEDED"

pattern DeviceStatusDetailCodeDeviceWasOffline :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeDeviceWasOffline = DeviceStatusDetailCode' "DEVICE_WAS_OFFLINE"

pattern DeviceStatusDetailCodeCredentialsAccessFailure :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeCredentialsAccessFailure = DeviceStatusDetailCode' "CREDENTIALS_ACCESS_FAILURE"

pattern DeviceStatusDetailCodeTlsVersionMismatch :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeTlsVersionMismatch = DeviceStatusDetailCode' "TLS_VERSION_MISMATCH"

pattern DeviceStatusDetailCodeAssociationRejection :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeAssociationRejection = DeviceStatusDetailCode' "ASSOCIATION_REJECTION"

pattern DeviceStatusDetailCodeAuthenticationFailure :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeAuthenticationFailure = DeviceStatusDetailCode' "AUTHENTICATION_FAILURE"

pattern DeviceStatusDetailCodeDhcpFailure :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeDhcpFailure = DeviceStatusDetailCode' "DHCP_FAILURE"

pattern DeviceStatusDetailCodeInternetUnavailable :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeInternetUnavailable = DeviceStatusDetailCode' "INTERNET_UNAVAILABLE"

pattern DeviceStatusDetailCodeDnsFailure :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeDnsFailure = DeviceStatusDetailCode' "DNS_FAILURE"

pattern DeviceStatusDetailCodeUnknownFailure :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeUnknownFailure = DeviceStatusDetailCode' "UNKNOWN_FAILURE"

pattern DeviceStatusDetailCodeCertificateIssuingLimitExceeded :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeCertificateIssuingLimitExceeded = DeviceStatusDetailCode' "CERTIFICATE_ISSUING_LIMIT_EXCEEDED"

pattern DeviceStatusDetailCodeInvalidCertificateAuthority :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeInvalidCertificateAuthority = DeviceStatusDetailCode' "INVALID_CERTIFICATE_AUTHORITY"

pattern DeviceStatusDetailCodeNetworkProfileNotFound :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeNetworkProfileNotFound = DeviceStatusDetailCode' "NETWORK_PROFILE_NOT_FOUND"

pattern DeviceStatusDetailCodeInvalidPasswordState :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeInvalidPasswordState = DeviceStatusDetailCode' "INVALID_PASSWORD_STATE"

pattern DeviceStatusDetailCodePasswordNotFound :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodePasswordNotFound = DeviceStatusDetailCode' "PASSWORD_NOT_FOUND"

pattern DeviceStatusDetailCodePasswordManagerAccessDenied :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodePasswordManagerAccessDenied = DeviceStatusDetailCode' "PASSWORD_MANAGER_ACCESS_DENIED"

pattern DeviceStatusDetailCodeCertificateAuthorityAccessDenied :: DeviceStatusDetailCode
pattern DeviceStatusDetailCodeCertificateAuthorityAccessDenied = DeviceStatusDetailCode' "CERTIFICATE_AUTHORITY_ACCESS_DENIED"

{-# COMPLETE 
  DeviceStatusDetailCodeDeviceSoftwareUpdateNeeded,

  DeviceStatusDetailCodeDeviceWasOffline,

  DeviceStatusDetailCodeCredentialsAccessFailure,

  DeviceStatusDetailCodeTlsVersionMismatch,

  DeviceStatusDetailCodeAssociationRejection,

  DeviceStatusDetailCodeAuthenticationFailure,

  DeviceStatusDetailCodeDhcpFailure,

  DeviceStatusDetailCodeInternetUnavailable,

  DeviceStatusDetailCodeDnsFailure,

  DeviceStatusDetailCodeUnknownFailure,

  DeviceStatusDetailCodeCertificateIssuingLimitExceeded,

  DeviceStatusDetailCodeInvalidCertificateAuthority,

  DeviceStatusDetailCodeNetworkProfileNotFound,

  DeviceStatusDetailCodeInvalidPasswordState,

  DeviceStatusDetailCodePasswordNotFound,

  DeviceStatusDetailCodePasswordManagerAccessDenied,

  DeviceStatusDetailCodeCertificateAuthorityAccessDenied,
  DeviceStatusDetailCode'
  #-}
