-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CertificateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CertificateStatus
  ( CertificateStatus
      ( CertificateStatus',
        CSExpired,
        CSFailed,
        CSInactive,
        CSIssued,
        CSPendingValidation,
        CSRevoked,
        CSValidationTimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CertificateStatus = CertificateStatus' Lude.Text
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

pattern CSExpired :: CertificateStatus
pattern CSExpired = CertificateStatus' "EXPIRED"

pattern CSFailed :: CertificateStatus
pattern CSFailed = CertificateStatus' "FAILED"

pattern CSInactive :: CertificateStatus
pattern CSInactive = CertificateStatus' "INACTIVE"

pattern CSIssued :: CertificateStatus
pattern CSIssued = CertificateStatus' "ISSUED"

pattern CSPendingValidation :: CertificateStatus
pattern CSPendingValidation = CertificateStatus' "PENDING_VALIDATION"

pattern CSRevoked :: CertificateStatus
pattern CSRevoked = CertificateStatus' "REVOKED"

pattern CSValidationTimedOut :: CertificateStatus
pattern CSValidationTimedOut = CertificateStatus' "VALIDATION_TIMED_OUT"

{-# COMPLETE
  CSExpired,
  CSFailed,
  CSInactive,
  CSIssued,
  CSPendingValidation,
  CSRevoked,
  CSValidationTimedOut,
  CertificateStatus'
  #-}
