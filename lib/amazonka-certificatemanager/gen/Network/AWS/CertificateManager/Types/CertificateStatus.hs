{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateStatus
  ( CertificateStatus
      ( CertificateStatus',
        CSPendingValidation,
        CSIssued,
        CSInactive,
        CSExpired,
        CSValidationTimedOut,
        CSRevoked,
        CSFailed
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

pattern CSPendingValidation :: CertificateStatus
pattern CSPendingValidation = CertificateStatus' "PENDING_VALIDATION"

pattern CSIssued :: CertificateStatus
pattern CSIssued = CertificateStatus' "ISSUED"

pattern CSInactive :: CertificateStatus
pattern CSInactive = CertificateStatus' "INACTIVE"

pattern CSExpired :: CertificateStatus
pattern CSExpired = CertificateStatus' "EXPIRED"

pattern CSValidationTimedOut :: CertificateStatus
pattern CSValidationTimedOut = CertificateStatus' "VALIDATION_TIMED_OUT"

pattern CSRevoked :: CertificateStatus
pattern CSRevoked = CertificateStatus' "REVOKED"

pattern CSFailed :: CertificateStatus
pattern CSFailed = CertificateStatus' "FAILED"

{-# COMPLETE
  CSPendingValidation,
  CSIssued,
  CSInactive,
  CSExpired,
  CSValidationTimedOut,
  CSRevoked,
  CSFailed,
  CertificateStatus'
  #-}
