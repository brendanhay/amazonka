{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateFailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateFailureReason
  ( LoadBalancerTLSCertificateFailureReason
      ( LoadBalancerTLSCertificateFailureReason',
        AdditionalVerificationRequired,
        DomainNotAllowed,
        InvalidPublicDomain,
        NoAvailableContacts,
        Other
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoadBalancerTLSCertificateFailureReason = LoadBalancerTLSCertificateFailureReason' Lude.Text
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

pattern AdditionalVerificationRequired :: LoadBalancerTLSCertificateFailureReason
pattern AdditionalVerificationRequired = LoadBalancerTLSCertificateFailureReason' "ADDITIONAL_VERIFICATION_REQUIRED"

pattern DomainNotAllowed :: LoadBalancerTLSCertificateFailureReason
pattern DomainNotAllowed = LoadBalancerTLSCertificateFailureReason' "DOMAIN_NOT_ALLOWED"

pattern InvalidPublicDomain :: LoadBalancerTLSCertificateFailureReason
pattern InvalidPublicDomain = LoadBalancerTLSCertificateFailureReason' "INVALID_PUBLIC_DOMAIN"

pattern NoAvailableContacts :: LoadBalancerTLSCertificateFailureReason
pattern NoAvailableContacts = LoadBalancerTLSCertificateFailureReason' "NO_AVAILABLE_CONTACTS"

pattern Other :: LoadBalancerTLSCertificateFailureReason
pattern Other = LoadBalancerTLSCertificateFailureReason' "OTHER"

{-# COMPLETE
  AdditionalVerificationRequired,
  DomainNotAllowed,
  InvalidPublicDomain,
  NoAvailableContacts,
  Other,
  LoadBalancerTLSCertificateFailureReason'
  #-}
