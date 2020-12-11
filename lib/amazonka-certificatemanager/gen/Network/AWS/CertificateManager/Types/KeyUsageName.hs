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
        KUNAny,
        KUNCertificateSigning,
        KUNCrlSigning,
        KUNCustom,
        KUNDataEncipherment,
        KUNDecipherOnly,
        KUNDigitalSignature,
        KUNEncipherOnly,
        KUNKeyAgreement,
        KUNKeyEncipherment,
        KUNNonRepudiation
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

pattern KUNAny :: KeyUsageName
pattern KUNAny = KeyUsageName' "ANY"

pattern KUNCertificateSigning :: KeyUsageName
pattern KUNCertificateSigning = KeyUsageName' "CERTIFICATE_SIGNING"

pattern KUNCrlSigning :: KeyUsageName
pattern KUNCrlSigning = KeyUsageName' "CRL_SIGNING"

pattern KUNCustom :: KeyUsageName
pattern KUNCustom = KeyUsageName' "CUSTOM"

pattern KUNDataEncipherment :: KeyUsageName
pattern KUNDataEncipherment = KeyUsageName' "DATA_ENCIPHERMENT"

pattern KUNDecipherOnly :: KeyUsageName
pattern KUNDecipherOnly = KeyUsageName' "DECIPHER_ONLY"

pattern KUNDigitalSignature :: KeyUsageName
pattern KUNDigitalSignature = KeyUsageName' "DIGITAL_SIGNATURE"

pattern KUNEncipherOnly :: KeyUsageName
pattern KUNEncipherOnly = KeyUsageName' "ENCIPHER_ONLY"

pattern KUNKeyAgreement :: KeyUsageName
pattern KUNKeyAgreement = KeyUsageName' "KEY_AGREEMENT"

pattern KUNKeyEncipherment :: KeyUsageName
pattern KUNKeyEncipherment = KeyUsageName' "KEY_ENCIPHERMENT"

pattern KUNNonRepudiation :: KeyUsageName
pattern KUNNonRepudiation = KeyUsageName' "NON_REPUDIATION"

{-# COMPLETE
  KUNAny,
  KUNCertificateSigning,
  KUNCrlSigning,
  KUNCustom,
  KUNDataEncipherment,
  KUNDecipherOnly,
  KUNDigitalSignature,
  KUNEncipherOnly,
  KUNKeyAgreement,
  KUNKeyEncipherment,
  KUNNonRepudiation,
  KeyUsageName'
  #-}
