{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateStatus
  ( LoadBalancerTlsCertificateStatus
    ( LoadBalancerTlsCertificateStatus'
    , LoadBalancerTlsCertificateStatusPendingValidation
    , LoadBalancerTlsCertificateStatusIssued
    , LoadBalancerTlsCertificateStatusInactive
    , LoadBalancerTlsCertificateStatusExpired
    , LoadBalancerTlsCertificateStatusValidationTimedOut
    , LoadBalancerTlsCertificateStatusRevoked
    , LoadBalancerTlsCertificateStatusFailed
    , LoadBalancerTlsCertificateStatusUnknown
    , fromLoadBalancerTlsCertificateStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LoadBalancerTlsCertificateStatus = LoadBalancerTlsCertificateStatus'{fromLoadBalancerTlsCertificateStatus
                                                                             :: Core.Text}
                                             deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                             Core.Show, Core.Generic)
                                             deriving newtype (Core.IsString, Core.Hashable,
                                                               Core.NFData, Core.ToJSONKey,
                                                               Core.FromJSONKey, Core.ToJSON,
                                                               Core.FromJSON, Core.ToXML,
                                                               Core.FromXML, Core.ToText,
                                                               Core.FromText, Core.ToByteString,
                                                               Core.ToQuery, Core.ToHeader)

pattern LoadBalancerTlsCertificateStatusPendingValidation :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatusPendingValidation = LoadBalancerTlsCertificateStatus' "PENDING_VALIDATION"

pattern LoadBalancerTlsCertificateStatusIssued :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatusIssued = LoadBalancerTlsCertificateStatus' "ISSUED"

pattern LoadBalancerTlsCertificateStatusInactive :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatusInactive = LoadBalancerTlsCertificateStatus' "INACTIVE"

pattern LoadBalancerTlsCertificateStatusExpired :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatusExpired = LoadBalancerTlsCertificateStatus' "EXPIRED"

pattern LoadBalancerTlsCertificateStatusValidationTimedOut :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatusValidationTimedOut = LoadBalancerTlsCertificateStatus' "VALIDATION_TIMED_OUT"

pattern LoadBalancerTlsCertificateStatusRevoked :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatusRevoked = LoadBalancerTlsCertificateStatus' "REVOKED"

pattern LoadBalancerTlsCertificateStatusFailed :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatusFailed = LoadBalancerTlsCertificateStatus' "FAILED"

pattern LoadBalancerTlsCertificateStatusUnknown :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatusUnknown = LoadBalancerTlsCertificateStatus' "UNKNOWN"

{-# COMPLETE 
  LoadBalancerTlsCertificateStatusPendingValidation,

  LoadBalancerTlsCertificateStatusIssued,

  LoadBalancerTlsCertificateStatusInactive,

  LoadBalancerTlsCertificateStatusExpired,

  LoadBalancerTlsCertificateStatusValidationTimedOut,

  LoadBalancerTlsCertificateStatusRevoked,

  LoadBalancerTlsCertificateStatusFailed,

  LoadBalancerTlsCertificateStatusUnknown,
  LoadBalancerTlsCertificateStatus'
  #-}
