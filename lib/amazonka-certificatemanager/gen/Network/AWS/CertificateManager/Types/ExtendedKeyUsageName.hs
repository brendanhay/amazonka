-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
  ( ExtendedKeyUsageName
      ( ExtendedKeyUsageName',
        Any,
        CodeSigning,
        Custom,
        EmailProtection,
        IPsecEndSystem,
        IPsecTunnel,
        IPsecUser,
        None,
        OcspSigning,
        TLSWebClientAuthentication,
        TLSWebServerAuthentication,
        TimeStamping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ExtendedKeyUsageName = ExtendedKeyUsageName' Lude.Text
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

pattern Any :: ExtendedKeyUsageName
pattern Any = ExtendedKeyUsageName' "ANY"

pattern CodeSigning :: ExtendedKeyUsageName
pattern CodeSigning = ExtendedKeyUsageName' "CODE_SIGNING"

pattern Custom :: ExtendedKeyUsageName
pattern Custom = ExtendedKeyUsageName' "CUSTOM"

pattern EmailProtection :: ExtendedKeyUsageName
pattern EmailProtection = ExtendedKeyUsageName' "EMAIL_PROTECTION"

pattern IPsecEndSystem :: ExtendedKeyUsageName
pattern IPsecEndSystem = ExtendedKeyUsageName' "IPSEC_END_SYSTEM"

pattern IPsecTunnel :: ExtendedKeyUsageName
pattern IPsecTunnel = ExtendedKeyUsageName' "IPSEC_TUNNEL"

pattern IPsecUser :: ExtendedKeyUsageName
pattern IPsecUser = ExtendedKeyUsageName' "IPSEC_USER"

pattern None :: ExtendedKeyUsageName
pattern None = ExtendedKeyUsageName' "NONE"

pattern OcspSigning :: ExtendedKeyUsageName
pattern OcspSigning = ExtendedKeyUsageName' "OCSP_SIGNING"

pattern TLSWebClientAuthentication :: ExtendedKeyUsageName
pattern TLSWebClientAuthentication = ExtendedKeyUsageName' "TLS_WEB_CLIENT_AUTHENTICATION"

pattern TLSWebServerAuthentication :: ExtendedKeyUsageName
pattern TLSWebServerAuthentication = ExtendedKeyUsageName' "TLS_WEB_SERVER_AUTHENTICATION"

pattern TimeStamping :: ExtendedKeyUsageName
pattern TimeStamping = ExtendedKeyUsageName' "TIME_STAMPING"

{-# COMPLETE
  Any,
  CodeSigning,
  Custom,
  EmailProtection,
  IPsecEndSystem,
  IPsecTunnel,
  IPsecUser,
  None,
  OcspSigning,
  TLSWebClientAuthentication,
  TLSWebServerAuthentication,
  TimeStamping,
  ExtendedKeyUsageName'
  #-}
