{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRevocationReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRevocationReason
  ( LoadBalancerTlsCertificateRevocationReason
    ( LoadBalancerTlsCertificateRevocationReason'
    , LoadBalancerTlsCertificateRevocationReasonUnspecified
    , LoadBalancerTlsCertificateRevocationReasonKeyCompromise
    , LoadBalancerTlsCertificateRevocationReasonCaCompromise
    , LoadBalancerTlsCertificateRevocationReasonAffiliationChanged
    , LoadBalancerTlsCertificateRevocationReasonSuperceded
    , LoadBalancerTlsCertificateRevocationReasonCessationOfOperation
    , LoadBalancerTlsCertificateRevocationReasonCertificateHold
    , LoadBalancerTlsCertificateRevocationReasonRemoveFromCrl
    , LoadBalancerTlsCertificateRevocationReasonPrivilegeWithdrawn
    , LoadBalancerTlsCertificateRevocationReasonAACompromise
    , fromLoadBalancerTlsCertificateRevocationReason
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LoadBalancerTlsCertificateRevocationReason = LoadBalancerTlsCertificateRevocationReason'{fromLoadBalancerTlsCertificateRevocationReason
                                                                                                 ::
                                                                                                 Core.Text}
                                                       deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                       Core.Show, Core.Generic)
                                                       deriving newtype (Core.IsString,
                                                                         Core.Hashable, Core.NFData,
                                                                         Core.ToJSONKey,
                                                                         Core.FromJSONKey,
                                                                         Core.ToJSON, Core.FromJSON,
                                                                         Core.ToXML, Core.FromXML,
                                                                         Core.ToText, Core.FromText,
                                                                         Core.ToByteString,
                                                                         Core.ToQuery,
                                                                         Core.ToHeader)

pattern LoadBalancerTlsCertificateRevocationReasonUnspecified :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReasonUnspecified = LoadBalancerTlsCertificateRevocationReason' "UNSPECIFIED"

pattern LoadBalancerTlsCertificateRevocationReasonKeyCompromise :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReasonKeyCompromise = LoadBalancerTlsCertificateRevocationReason' "KEY_COMPROMISE"

pattern LoadBalancerTlsCertificateRevocationReasonCaCompromise :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReasonCaCompromise = LoadBalancerTlsCertificateRevocationReason' "CA_COMPROMISE"

pattern LoadBalancerTlsCertificateRevocationReasonAffiliationChanged :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReasonAffiliationChanged = LoadBalancerTlsCertificateRevocationReason' "AFFILIATION_CHANGED"

pattern LoadBalancerTlsCertificateRevocationReasonSuperceded :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReasonSuperceded = LoadBalancerTlsCertificateRevocationReason' "SUPERCEDED"

pattern LoadBalancerTlsCertificateRevocationReasonCessationOfOperation :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReasonCessationOfOperation = LoadBalancerTlsCertificateRevocationReason' "CESSATION_OF_OPERATION"

pattern LoadBalancerTlsCertificateRevocationReasonCertificateHold :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReasonCertificateHold = LoadBalancerTlsCertificateRevocationReason' "CERTIFICATE_HOLD"

pattern LoadBalancerTlsCertificateRevocationReasonRemoveFromCrl :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReasonRemoveFromCrl = LoadBalancerTlsCertificateRevocationReason' "REMOVE_FROM_CRL"

pattern LoadBalancerTlsCertificateRevocationReasonPrivilegeWithdrawn :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReasonPrivilegeWithdrawn = LoadBalancerTlsCertificateRevocationReason' "PRIVILEGE_WITHDRAWN"

pattern LoadBalancerTlsCertificateRevocationReasonAACompromise :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReasonAACompromise = LoadBalancerTlsCertificateRevocationReason' "A_A_COMPROMISE"

{-# COMPLETE 
  LoadBalancerTlsCertificateRevocationReasonUnspecified,

  LoadBalancerTlsCertificateRevocationReasonKeyCompromise,

  LoadBalancerTlsCertificateRevocationReasonCaCompromise,

  LoadBalancerTlsCertificateRevocationReasonAffiliationChanged,

  LoadBalancerTlsCertificateRevocationReasonSuperceded,

  LoadBalancerTlsCertificateRevocationReasonCessationOfOperation,

  LoadBalancerTlsCertificateRevocationReasonCertificateHold,

  LoadBalancerTlsCertificateRevocationReasonRemoveFromCrl,

  LoadBalancerTlsCertificateRevocationReasonPrivilegeWithdrawn,

  LoadBalancerTlsCertificateRevocationReasonAACompromise,
  LoadBalancerTlsCertificateRevocationReason'
  #-}
