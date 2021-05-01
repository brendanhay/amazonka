{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.ExtendedKeyUsageType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.ExtendedKeyUsageType
  ( ExtendedKeyUsageType
      ( ..,
        ExtendedKeyUsageType_CERTIFICATE_TRANSPARENCY,
        ExtendedKeyUsageType_CLIENT_AUTH,
        ExtendedKeyUsageType_CODE_SIGNING,
        ExtendedKeyUsageType_DOCUMENT_SIGNING,
        ExtendedKeyUsageType_EMAIL_PROTECTION,
        ExtendedKeyUsageType_OCSP_SIGNING,
        ExtendedKeyUsageType_SERVER_AUTH,
        ExtendedKeyUsageType_SMART_CARD_LOGIN,
        ExtendedKeyUsageType_TIME_STAMPING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ExtendedKeyUsageType = ExtendedKeyUsageType'
  { fromExtendedKeyUsageType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ExtendedKeyUsageType_CERTIFICATE_TRANSPARENCY :: ExtendedKeyUsageType
pattern ExtendedKeyUsageType_CERTIFICATE_TRANSPARENCY = ExtendedKeyUsageType' "CERTIFICATE_TRANSPARENCY"

pattern ExtendedKeyUsageType_CLIENT_AUTH :: ExtendedKeyUsageType
pattern ExtendedKeyUsageType_CLIENT_AUTH = ExtendedKeyUsageType' "CLIENT_AUTH"

pattern ExtendedKeyUsageType_CODE_SIGNING :: ExtendedKeyUsageType
pattern ExtendedKeyUsageType_CODE_SIGNING = ExtendedKeyUsageType' "CODE_SIGNING"

pattern ExtendedKeyUsageType_DOCUMENT_SIGNING :: ExtendedKeyUsageType
pattern ExtendedKeyUsageType_DOCUMENT_SIGNING = ExtendedKeyUsageType' "DOCUMENT_SIGNING"

pattern ExtendedKeyUsageType_EMAIL_PROTECTION :: ExtendedKeyUsageType
pattern ExtendedKeyUsageType_EMAIL_PROTECTION = ExtendedKeyUsageType' "EMAIL_PROTECTION"

pattern ExtendedKeyUsageType_OCSP_SIGNING :: ExtendedKeyUsageType
pattern ExtendedKeyUsageType_OCSP_SIGNING = ExtendedKeyUsageType' "OCSP_SIGNING"

pattern ExtendedKeyUsageType_SERVER_AUTH :: ExtendedKeyUsageType
pattern ExtendedKeyUsageType_SERVER_AUTH = ExtendedKeyUsageType' "SERVER_AUTH"

pattern ExtendedKeyUsageType_SMART_CARD_LOGIN :: ExtendedKeyUsageType
pattern ExtendedKeyUsageType_SMART_CARD_LOGIN = ExtendedKeyUsageType' "SMART_CARD_LOGIN"

pattern ExtendedKeyUsageType_TIME_STAMPING :: ExtendedKeyUsageType
pattern ExtendedKeyUsageType_TIME_STAMPING = ExtendedKeyUsageType' "TIME_STAMPING"

{-# COMPLETE
  ExtendedKeyUsageType_CERTIFICATE_TRANSPARENCY,
  ExtendedKeyUsageType_CLIENT_AUTH,
  ExtendedKeyUsageType_CODE_SIGNING,
  ExtendedKeyUsageType_DOCUMENT_SIGNING,
  ExtendedKeyUsageType_EMAIL_PROTECTION,
  ExtendedKeyUsageType_OCSP_SIGNING,
  ExtendedKeyUsageType_SERVER_AUTH,
  ExtendedKeyUsageType_SMART_CARD_LOGIN,
  ExtendedKeyUsageType_TIME_STAMPING,
  ExtendedKeyUsageType'
  #-}
