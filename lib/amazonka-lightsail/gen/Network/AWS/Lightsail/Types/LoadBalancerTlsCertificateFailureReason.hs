{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateFailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateFailureReason
  ( LoadBalancerTlsCertificateFailureReason
    ( LoadBalancerTlsCertificateFailureReason'
    , LoadBalancerTlsCertificateFailureReasonNoAvailableContacts
    , LoadBalancerTlsCertificateFailureReasonAdditionalVerificationRequired
    , LoadBalancerTlsCertificateFailureReasonDomainNotAllowed
    , LoadBalancerTlsCertificateFailureReasonInvalidPublicDomain
    , LoadBalancerTlsCertificateFailureReasonOther
    , fromLoadBalancerTlsCertificateFailureReason
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LoadBalancerTlsCertificateFailureReason = LoadBalancerTlsCertificateFailureReason'{fromLoadBalancerTlsCertificateFailureReason
                                                                                           ::
                                                                                           Core.Text}
                                                    deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                    Core.Show, Core.Generic)
                                                    deriving newtype (Core.IsString, Core.Hashable,
                                                                      Core.NFData, Core.ToJSONKey,
                                                                      Core.FromJSONKey, Core.ToJSON,
                                                                      Core.FromJSON, Core.ToXML,
                                                                      Core.FromXML, Core.ToText,
                                                                      Core.FromText,
                                                                      Core.ToByteString,
                                                                      Core.ToQuery, Core.ToHeader)

pattern LoadBalancerTlsCertificateFailureReasonNoAvailableContacts :: LoadBalancerTlsCertificateFailureReason
pattern LoadBalancerTlsCertificateFailureReasonNoAvailableContacts = LoadBalancerTlsCertificateFailureReason' "NO_AVAILABLE_CONTACTS"

pattern LoadBalancerTlsCertificateFailureReasonAdditionalVerificationRequired :: LoadBalancerTlsCertificateFailureReason
pattern LoadBalancerTlsCertificateFailureReasonAdditionalVerificationRequired = LoadBalancerTlsCertificateFailureReason' "ADDITIONAL_VERIFICATION_REQUIRED"

pattern LoadBalancerTlsCertificateFailureReasonDomainNotAllowed :: LoadBalancerTlsCertificateFailureReason
pattern LoadBalancerTlsCertificateFailureReasonDomainNotAllowed = LoadBalancerTlsCertificateFailureReason' "DOMAIN_NOT_ALLOWED"

pattern LoadBalancerTlsCertificateFailureReasonInvalidPublicDomain :: LoadBalancerTlsCertificateFailureReason
pattern LoadBalancerTlsCertificateFailureReasonInvalidPublicDomain = LoadBalancerTlsCertificateFailureReason' "INVALID_PUBLIC_DOMAIN"

pattern LoadBalancerTlsCertificateFailureReasonOther :: LoadBalancerTlsCertificateFailureReason
pattern LoadBalancerTlsCertificateFailureReasonOther = LoadBalancerTlsCertificateFailureReason' "OTHER"

{-# COMPLETE 
  LoadBalancerTlsCertificateFailureReasonNoAvailableContacts,

  LoadBalancerTlsCertificateFailureReasonAdditionalVerificationRequired,

  LoadBalancerTlsCertificateFailureReasonDomainNotAllowed,

  LoadBalancerTlsCertificateFailureReasonInvalidPublicDomain,

  LoadBalancerTlsCertificateFailureReasonOther,
  LoadBalancerTlsCertificateFailureReason'
  #-}
