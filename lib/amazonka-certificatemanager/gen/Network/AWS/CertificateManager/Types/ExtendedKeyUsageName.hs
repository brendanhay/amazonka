{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.ExtendedKeyUsageName where

import Network.AWS.Prelude

data ExtendedKeyUsageName
  = Any
  | CodeSigning
  | Custom
  | EmailProtection
  | IPsecEndSystem
  | IPsecTunnel
  | IPsecUser
  | None
  | OcspSigning
  | TLSWebClientAuthentication
  | TLSWebServerAuthentication
  | TimeStamping
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

instance FromText ExtendedKeyUsageName where
  parser =
    takeLowerText >>= \case
      "any" -> pure Any
      "code_signing" -> pure CodeSigning
      "custom" -> pure Custom
      "email_protection" -> pure EmailProtection
      "ipsec_end_system" -> pure IPsecEndSystem
      "ipsec_tunnel" -> pure IPsecTunnel
      "ipsec_user" -> pure IPsecUser
      "none" -> pure None
      "ocsp_signing" -> pure OcspSigning
      "tls_web_client_authentication" -> pure TLSWebClientAuthentication
      "tls_web_server_authentication" -> pure TLSWebServerAuthentication
      "time_stamping" -> pure TimeStamping
      e ->
        fromTextError $
          "Failure parsing ExtendedKeyUsageName from value: '" <> e
            <> "'. Accepted values: any, code_signing, custom, email_protection, ipsec_end_system, ipsec_tunnel, ipsec_user, none, ocsp_signing, tls_web_client_authentication, tls_web_server_authentication, time_stamping"

instance ToText ExtendedKeyUsageName where
  toText = \case
    Any -> "ANY"
    CodeSigning -> "CODE_SIGNING"
    Custom -> "CUSTOM"
    EmailProtection -> "EMAIL_PROTECTION"
    IPsecEndSystem -> "IPSEC_END_SYSTEM"
    IPsecTunnel -> "IPSEC_TUNNEL"
    IPsecUser -> "IPSEC_USER"
    None -> "NONE"
    OcspSigning -> "OCSP_SIGNING"
    TLSWebClientAuthentication -> "TLS_WEB_CLIENT_AUTHENTICATION"
    TLSWebServerAuthentication -> "TLS_WEB_SERVER_AUTHENTICATION"
    TimeStamping -> "TIME_STAMPING"

instance Hashable ExtendedKeyUsageName

instance NFData ExtendedKeyUsageName

instance ToByteString ExtendedKeyUsageName

instance ToQuery ExtendedKeyUsageName

instance ToHeader ExtendedKeyUsageName

instance ToJSON ExtendedKeyUsageName where
  toJSON = toJSONText

instance FromJSON ExtendedKeyUsageName where
  parseJSON = parseJSONText "ExtendedKeyUsageName"
