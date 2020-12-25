{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.KeyUsageName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.KeyUsageName
  ( KeyUsageName
      ( KeyUsageName',
        KeyUsageNameDigitalSignature,
        KeyUsageNameNonRepudiation,
        KeyUsageNameKeyEncipherment,
        KeyUsageNameDataEncipherment,
        KeyUsageNameKeyAgreement,
        KeyUsageNameCertificateSigning,
        KeyUsageNameCrlSigning,
        KeyUsageNameEncipherOnly,
        KeyUsageNameDecipherOnly,
        KeyUsageNameAny,
        KeyUsageNameCustom,
        fromKeyUsageName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype KeyUsageName = KeyUsageName' {fromKeyUsageName :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern KeyUsageNameDigitalSignature :: KeyUsageName
pattern KeyUsageNameDigitalSignature = KeyUsageName' "DIGITAL_SIGNATURE"

pattern KeyUsageNameNonRepudiation :: KeyUsageName
pattern KeyUsageNameNonRepudiation = KeyUsageName' "NON_REPUDIATION"

pattern KeyUsageNameKeyEncipherment :: KeyUsageName
pattern KeyUsageNameKeyEncipherment = KeyUsageName' "KEY_ENCIPHERMENT"

pattern KeyUsageNameDataEncipherment :: KeyUsageName
pattern KeyUsageNameDataEncipherment = KeyUsageName' "DATA_ENCIPHERMENT"

pattern KeyUsageNameKeyAgreement :: KeyUsageName
pattern KeyUsageNameKeyAgreement = KeyUsageName' "KEY_AGREEMENT"

pattern KeyUsageNameCertificateSigning :: KeyUsageName
pattern KeyUsageNameCertificateSigning = KeyUsageName' "CERTIFICATE_SIGNING"

pattern KeyUsageNameCrlSigning :: KeyUsageName
pattern KeyUsageNameCrlSigning = KeyUsageName' "CRL_SIGNING"

pattern KeyUsageNameEncipherOnly :: KeyUsageName
pattern KeyUsageNameEncipherOnly = KeyUsageName' "ENCIPHER_ONLY"

pattern KeyUsageNameDecipherOnly :: KeyUsageName
pattern KeyUsageNameDecipherOnly = KeyUsageName' "DECIPHER_ONLY"

pattern KeyUsageNameAny :: KeyUsageName
pattern KeyUsageNameAny = KeyUsageName' "ANY"

pattern KeyUsageNameCustom :: KeyUsageName
pattern KeyUsageNameCustom = KeyUsageName' "CUSTOM"

{-# COMPLETE
  KeyUsageNameDigitalSignature,
  KeyUsageNameNonRepudiation,
  KeyUsageNameKeyEncipherment,
  KeyUsageNameDataEncipherment,
  KeyUsageNameKeyAgreement,
  KeyUsageNameCertificateSigning,
  KeyUsageNameCrlSigning,
  KeyUsageNameEncipherOnly,
  KeyUsageNameDecipherOnly,
  KeyUsageNameAny,
  KeyUsageNameCustom,
  KeyUsageName'
  #-}
