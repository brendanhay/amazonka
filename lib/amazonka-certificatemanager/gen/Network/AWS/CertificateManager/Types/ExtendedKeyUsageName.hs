{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
  ( ExtendedKeyUsageName
    ( ExtendedKeyUsageName'
    , ExtendedKeyUsageNameTlsWebServerAuthentication
    , ExtendedKeyUsageNameTlsWebClientAuthentication
    , ExtendedKeyUsageNameCodeSigning
    , ExtendedKeyUsageNameEmailProtection
    , ExtendedKeyUsageNameTimeStamping
    , ExtendedKeyUsageNameOcspSigning
    , ExtendedKeyUsageNameIpsecEndSystem
    , ExtendedKeyUsageNameIpsecTunnel
    , ExtendedKeyUsageNameIpsecUser
    , ExtendedKeyUsageNameAny
    , ExtendedKeyUsageNameNone
    , ExtendedKeyUsageNameCustom
    , fromExtendedKeyUsageName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ExtendedKeyUsageName = ExtendedKeyUsageName'{fromExtendedKeyUsageName
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern ExtendedKeyUsageNameTlsWebServerAuthentication :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameTlsWebServerAuthentication = ExtendedKeyUsageName' "TLS_WEB_SERVER_AUTHENTICATION"

pattern ExtendedKeyUsageNameTlsWebClientAuthentication :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameTlsWebClientAuthentication = ExtendedKeyUsageName' "TLS_WEB_CLIENT_AUTHENTICATION"

pattern ExtendedKeyUsageNameCodeSigning :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameCodeSigning = ExtendedKeyUsageName' "CODE_SIGNING"

pattern ExtendedKeyUsageNameEmailProtection :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameEmailProtection = ExtendedKeyUsageName' "EMAIL_PROTECTION"

pattern ExtendedKeyUsageNameTimeStamping :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameTimeStamping = ExtendedKeyUsageName' "TIME_STAMPING"

pattern ExtendedKeyUsageNameOcspSigning :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameOcspSigning = ExtendedKeyUsageName' "OCSP_SIGNING"

pattern ExtendedKeyUsageNameIpsecEndSystem :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameIpsecEndSystem = ExtendedKeyUsageName' "IPSEC_END_SYSTEM"

pattern ExtendedKeyUsageNameIpsecTunnel :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameIpsecTunnel = ExtendedKeyUsageName' "IPSEC_TUNNEL"

pattern ExtendedKeyUsageNameIpsecUser :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameIpsecUser = ExtendedKeyUsageName' "IPSEC_USER"

pattern ExtendedKeyUsageNameAny :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameAny = ExtendedKeyUsageName' "ANY"

pattern ExtendedKeyUsageNameNone :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameNone = ExtendedKeyUsageName' "NONE"

pattern ExtendedKeyUsageNameCustom :: ExtendedKeyUsageName
pattern ExtendedKeyUsageNameCustom = ExtendedKeyUsageName' "CUSTOM"

{-# COMPLETE 
  ExtendedKeyUsageNameTlsWebServerAuthentication,

  ExtendedKeyUsageNameTlsWebClientAuthentication,

  ExtendedKeyUsageNameCodeSigning,

  ExtendedKeyUsageNameEmailProtection,

  ExtendedKeyUsageNameTimeStamping,

  ExtendedKeyUsageNameOcspSigning,

  ExtendedKeyUsageNameIpsecEndSystem,

  ExtendedKeyUsageNameIpsecTunnel,

  ExtendedKeyUsageNameIpsecUser,

  ExtendedKeyUsageNameAny,

  ExtendedKeyUsageNameNone,

  ExtendedKeyUsageNameCustom,
  ExtendedKeyUsageName'
  #-}
