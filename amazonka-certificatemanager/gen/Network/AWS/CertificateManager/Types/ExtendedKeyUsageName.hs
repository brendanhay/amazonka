{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
  ( ExtendedKeyUsageName
      ( ..,
        ExtendedKeyUsageName_ANY,
        ExtendedKeyUsageName_CODE_SIGNING,
        ExtendedKeyUsageName_CUSTOM,
        ExtendedKeyUsageName_EMAIL_PROTECTION,
        ExtendedKeyUsageName_IPSEC_END_SYSTEM,
        ExtendedKeyUsageName_IPSEC_TUNNEL,
        ExtendedKeyUsageName_IPSEC_USER,
        ExtendedKeyUsageName_NONE,
        ExtendedKeyUsageName_OCSP_SIGNING,
        ExtendedKeyUsageName_TIME_STAMPING,
        ExtendedKeyUsageName_TLS_WEB_CLIENT_AUTHENTICATION,
        ExtendedKeyUsageName_TLS_WEB_SERVER_AUTHENTICATION
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ExtendedKeyUsageName = ExtendedKeyUsageName'
  { fromExtendedKeyUsageName ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ExtendedKeyUsageName_ANY :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_ANY = ExtendedKeyUsageName' "ANY"

pattern ExtendedKeyUsageName_CODE_SIGNING :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_CODE_SIGNING = ExtendedKeyUsageName' "CODE_SIGNING"

pattern ExtendedKeyUsageName_CUSTOM :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_CUSTOM = ExtendedKeyUsageName' "CUSTOM"

pattern ExtendedKeyUsageName_EMAIL_PROTECTION :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_EMAIL_PROTECTION = ExtendedKeyUsageName' "EMAIL_PROTECTION"

pattern ExtendedKeyUsageName_IPSEC_END_SYSTEM :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_IPSEC_END_SYSTEM = ExtendedKeyUsageName' "IPSEC_END_SYSTEM"

pattern ExtendedKeyUsageName_IPSEC_TUNNEL :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_IPSEC_TUNNEL = ExtendedKeyUsageName' "IPSEC_TUNNEL"

pattern ExtendedKeyUsageName_IPSEC_USER :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_IPSEC_USER = ExtendedKeyUsageName' "IPSEC_USER"

pattern ExtendedKeyUsageName_NONE :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_NONE = ExtendedKeyUsageName' "NONE"

pattern ExtendedKeyUsageName_OCSP_SIGNING :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_OCSP_SIGNING = ExtendedKeyUsageName' "OCSP_SIGNING"

pattern ExtendedKeyUsageName_TIME_STAMPING :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_TIME_STAMPING = ExtendedKeyUsageName' "TIME_STAMPING"

pattern ExtendedKeyUsageName_TLS_WEB_CLIENT_AUTHENTICATION :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_TLS_WEB_CLIENT_AUTHENTICATION = ExtendedKeyUsageName' "TLS_WEB_CLIENT_AUTHENTICATION"

pattern ExtendedKeyUsageName_TLS_WEB_SERVER_AUTHENTICATION :: ExtendedKeyUsageName
pattern ExtendedKeyUsageName_TLS_WEB_SERVER_AUTHENTICATION = ExtendedKeyUsageName' "TLS_WEB_SERVER_AUTHENTICATION"

{-# COMPLETE
  ExtendedKeyUsageName_ANY,
  ExtendedKeyUsageName_CODE_SIGNING,
  ExtendedKeyUsageName_CUSTOM,
  ExtendedKeyUsageName_EMAIL_PROTECTION,
  ExtendedKeyUsageName_IPSEC_END_SYSTEM,
  ExtendedKeyUsageName_IPSEC_TUNNEL,
  ExtendedKeyUsageName_IPSEC_USER,
  ExtendedKeyUsageName_NONE,
  ExtendedKeyUsageName_OCSP_SIGNING,
  ExtendedKeyUsageName_TIME_STAMPING,
  ExtendedKeyUsageName_TLS_WEB_CLIENT_AUTHENTICATION,
  ExtendedKeyUsageName_TLS_WEB_SERVER_AUTHENTICATION,
  ExtendedKeyUsageName'
  #-}
