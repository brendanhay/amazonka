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
        CertificateStatusPendingValidation,
        CertificateStatusIssued,
        CertificateStatusInactive,
        CertificateStatusExpired,
        CertificateStatusValidationTimedOut,
        CertificateStatusRevoked,
        CertificateStatusFailed,
        fromCertificateStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CertificateStatus = CertificateStatus'
  { fromCertificateStatus ::
      Core.Text
  }
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

pattern CertificateStatusPendingValidation :: CertificateStatus
pattern CertificateStatusPendingValidation = CertificateStatus' "PENDING_VALIDATION"

pattern CertificateStatusIssued :: CertificateStatus
pattern CertificateStatusIssued = CertificateStatus' "ISSUED"

pattern CertificateStatusInactive :: CertificateStatus
pattern CertificateStatusInactive = CertificateStatus' "INACTIVE"

pattern CertificateStatusExpired :: CertificateStatus
pattern CertificateStatusExpired = CertificateStatus' "EXPIRED"

pattern CertificateStatusValidationTimedOut :: CertificateStatus
pattern CertificateStatusValidationTimedOut = CertificateStatus' "VALIDATION_TIMED_OUT"

pattern CertificateStatusRevoked :: CertificateStatus
pattern CertificateStatusRevoked = CertificateStatus' "REVOKED"

pattern CertificateStatusFailed :: CertificateStatus
pattern CertificateStatusFailed = CertificateStatus' "FAILED"

{-# COMPLETE
  CertificateStatusPendingValidation,
  CertificateStatusIssued,
  CertificateStatusInactive,
  CertificateStatusExpired,
  CertificateStatusValidationTimedOut,
  CertificateStatusRevoked,
  CertificateStatusFailed,
  CertificateStatus'
  #-}
