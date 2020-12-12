{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateStatus
  ( LoadBalancerTLSCertificateStatus
      ( LoadBalancerTLSCertificateStatus',
        LBTCSExpired,
        LBTCSFailed,
        LBTCSInactive,
        LBTCSIssued,
        LBTCSPendingValidation,
        LBTCSRevoked,
        LBTCSUnknown,
        LBTCSValidationTimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoadBalancerTLSCertificateStatus = LoadBalancerTLSCertificateStatus' Lude.Text
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

pattern LBTCSExpired :: LoadBalancerTLSCertificateStatus
pattern LBTCSExpired = LoadBalancerTLSCertificateStatus' "EXPIRED"

pattern LBTCSFailed :: LoadBalancerTLSCertificateStatus
pattern LBTCSFailed = LoadBalancerTLSCertificateStatus' "FAILED"

pattern LBTCSInactive :: LoadBalancerTLSCertificateStatus
pattern LBTCSInactive = LoadBalancerTLSCertificateStatus' "INACTIVE"

pattern LBTCSIssued :: LoadBalancerTLSCertificateStatus
pattern LBTCSIssued = LoadBalancerTLSCertificateStatus' "ISSUED"

pattern LBTCSPendingValidation :: LoadBalancerTLSCertificateStatus
pattern LBTCSPendingValidation = LoadBalancerTLSCertificateStatus' "PENDING_VALIDATION"

pattern LBTCSRevoked :: LoadBalancerTLSCertificateStatus
pattern LBTCSRevoked = LoadBalancerTLSCertificateStatus' "REVOKED"

pattern LBTCSUnknown :: LoadBalancerTLSCertificateStatus
pattern LBTCSUnknown = LoadBalancerTLSCertificateStatus' "UNKNOWN"

pattern LBTCSValidationTimedOut :: LoadBalancerTLSCertificateStatus
pattern LBTCSValidationTimedOut = LoadBalancerTLSCertificateStatus' "VALIDATION_TIMED_OUT"

{-# COMPLETE
  LBTCSExpired,
  LBTCSFailed,
  LBTCSInactive,
  LBTCSIssued,
  LBTCSPendingValidation,
  LBTCSRevoked,
  LBTCSUnknown,
  LBTCSValidationTimedOut,
  LoadBalancerTLSCertificateStatus'
  #-}
