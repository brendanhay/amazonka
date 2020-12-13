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
        KUNDigitalSignature,
        KUNNonRepudiation,
        KUNKeyEncipherment,
        KUNDataEncipherment,
        KUNKeyAgreement,
        KUNCertificateSigning,
        KUNCrlSigning,
        KUNEncipherOnly,
        KUNDecipherOnly,
        KUNAny,
        KUNCustom
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype KeyUsageName = KeyUsageName' Lude.Text
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

pattern KUNDigitalSignature :: KeyUsageName
pattern KUNDigitalSignature = KeyUsageName' "DIGITAL_SIGNATURE"

pattern KUNNonRepudiation :: KeyUsageName
pattern KUNNonRepudiation = KeyUsageName' "NON_REPUDIATION"

pattern KUNKeyEncipherment :: KeyUsageName
pattern KUNKeyEncipherment = KeyUsageName' "KEY_ENCIPHERMENT"

pattern KUNDataEncipherment :: KeyUsageName
pattern KUNDataEncipherment = KeyUsageName' "DATA_ENCIPHERMENT"

pattern KUNKeyAgreement :: KeyUsageName
pattern KUNKeyAgreement = KeyUsageName' "KEY_AGREEMENT"

pattern KUNCertificateSigning :: KeyUsageName
pattern KUNCertificateSigning = KeyUsageName' "CERTIFICATE_SIGNING"

pattern KUNCrlSigning :: KeyUsageName
pattern KUNCrlSigning = KeyUsageName' "CRL_SIGNING"

pattern KUNEncipherOnly :: KeyUsageName
pattern KUNEncipherOnly = KeyUsageName' "ENCIPHER_ONLY"

pattern KUNDecipherOnly :: KeyUsageName
pattern KUNDecipherOnly = KeyUsageName' "DECIPHER_ONLY"

pattern KUNAny :: KeyUsageName
pattern KUNAny = KeyUsageName' "ANY"

pattern KUNCustom :: KeyUsageName
pattern KUNCustom = KeyUsageName' "CUSTOM"

{-# COMPLETE
  KUNDigitalSignature,
  KUNNonRepudiation,
  KUNKeyEncipherment,
  KUNDataEncipherment,
  KUNKeyAgreement,
  KUNCertificateSigning,
  KUNCrlSigning,
  KUNEncipherOnly,
  KUNDecipherOnly,
  KUNAny,
  KUNCustom,
  KeyUsageName'
  #-}
