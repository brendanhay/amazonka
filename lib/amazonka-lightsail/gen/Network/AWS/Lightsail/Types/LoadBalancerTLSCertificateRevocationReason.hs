{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRevocationReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRevocationReason
  ( LoadBalancerTLSCertificateRevocationReason
      ( LoadBalancerTLSCertificateRevocationReason',
        Unspecified,
        KeyCompromise,
        CaCompromise,
        AffiliationChanged,
        Superceded,
        CessationOfOperation,
        CertificateHold,
        RemoveFromCrl,
        PrivilegeWithdrawn,
        AACompromise
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoadBalancerTLSCertificateRevocationReason = LoadBalancerTLSCertificateRevocationReason' Lude.Text
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

pattern Unspecified :: LoadBalancerTLSCertificateRevocationReason
pattern Unspecified = LoadBalancerTLSCertificateRevocationReason' "UNSPECIFIED"

pattern KeyCompromise :: LoadBalancerTLSCertificateRevocationReason
pattern KeyCompromise = LoadBalancerTLSCertificateRevocationReason' "KEY_COMPROMISE"

pattern CaCompromise :: LoadBalancerTLSCertificateRevocationReason
pattern CaCompromise = LoadBalancerTLSCertificateRevocationReason' "CA_COMPROMISE"

pattern AffiliationChanged :: LoadBalancerTLSCertificateRevocationReason
pattern AffiliationChanged = LoadBalancerTLSCertificateRevocationReason' "AFFILIATION_CHANGED"

pattern Superceded :: LoadBalancerTLSCertificateRevocationReason
pattern Superceded = LoadBalancerTLSCertificateRevocationReason' "SUPERCEDED"

pattern CessationOfOperation :: LoadBalancerTLSCertificateRevocationReason
pattern CessationOfOperation = LoadBalancerTLSCertificateRevocationReason' "CESSATION_OF_OPERATION"

pattern CertificateHold :: LoadBalancerTLSCertificateRevocationReason
pattern CertificateHold = LoadBalancerTLSCertificateRevocationReason' "CERTIFICATE_HOLD"

pattern RemoveFromCrl :: LoadBalancerTLSCertificateRevocationReason
pattern RemoveFromCrl = LoadBalancerTLSCertificateRevocationReason' "REMOVE_FROM_CRL"

pattern PrivilegeWithdrawn :: LoadBalancerTLSCertificateRevocationReason
pattern PrivilegeWithdrawn = LoadBalancerTLSCertificateRevocationReason' "PRIVILEGE_WITHDRAWN"

pattern AACompromise :: LoadBalancerTLSCertificateRevocationReason
pattern AACompromise = LoadBalancerTLSCertificateRevocationReason' "A_A_COMPROMISE"

{-# COMPLETE
  Unspecified,
  KeyCompromise,
  CaCompromise,
  AffiliationChanged,
  Superceded,
  CessationOfOperation,
  CertificateHold,
  RemoveFromCrl,
  PrivilegeWithdrawn,
  AACompromise,
  LoadBalancerTLSCertificateRevocationReason'
  #-}
