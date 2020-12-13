{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalStatus
  ( LoadBalancerTLSCertificateRenewalStatus
      ( LoadBalancerTLSCertificateRenewalStatus',
        LBTCRSPendingAutoRenewal,
        LBTCRSPendingValidation,
        LBTCRSSuccess,
        LBTCRSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoadBalancerTLSCertificateRenewalStatus = LoadBalancerTLSCertificateRenewalStatus' Lude.Text
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

pattern LBTCRSPendingAutoRenewal :: LoadBalancerTLSCertificateRenewalStatus
pattern LBTCRSPendingAutoRenewal = LoadBalancerTLSCertificateRenewalStatus' "PENDING_AUTO_RENEWAL"

pattern LBTCRSPendingValidation :: LoadBalancerTLSCertificateRenewalStatus
pattern LBTCRSPendingValidation = LoadBalancerTLSCertificateRenewalStatus' "PENDING_VALIDATION"

pattern LBTCRSSuccess :: LoadBalancerTLSCertificateRenewalStatus
pattern LBTCRSSuccess = LoadBalancerTLSCertificateRenewalStatus' "SUCCESS"

pattern LBTCRSFailed :: LoadBalancerTLSCertificateRenewalStatus
pattern LBTCRSFailed = LoadBalancerTLSCertificateRenewalStatus' "FAILED"

{-# COMPLETE
  LBTCRSPendingAutoRenewal,
  LBTCRSPendingValidation,
  LBTCRSSuccess,
  LBTCRSFailed,
  LoadBalancerTLSCertificateRenewalStatus'
  #-}
