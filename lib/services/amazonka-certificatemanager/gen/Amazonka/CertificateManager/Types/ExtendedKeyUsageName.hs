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
-- Module      : Amazonka.CertificateManager.Types.ExtendedKeyUsageName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types.ExtendedKeyUsageName
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExtendedKeyUsageName = ExtendedKeyUsageName'
  { fromExtendedKeyUsageName ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
