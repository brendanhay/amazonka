-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.RenewalSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.RenewalSummary
  ( RenewalSummary (..),

    -- * Smart constructor
    mkRenewalSummary,

    -- * Lenses
    rsRenewalStatusReason,
    rsRenewalStatus,
    rsDomainValidationOptions,
    rsUpdatedAt,
  )
where

import Network.AWS.CertificateManager.Types.DomainValidation
import Network.AWS.CertificateManager.Types.FailureReason
import Network.AWS.CertificateManager.Types.RenewalStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the status of ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for the certificate. This structure exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- /See:/ 'mkRenewalSummary' smart constructor.
data RenewalSummary = RenewalSummary'
  { renewalStatusReason ::
      Lude.Maybe FailureReason,
    renewalStatus :: RenewalStatus,
    domainValidationOptions :: Lude.NonEmpty DomainValidation,
    updatedAt :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenewalSummary' with the minimum fields required to make a request.
--
-- * 'domainValidationOptions' - Contains information about the validation of each domain name in the certificate, as it pertains to ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> . This is different from the initial validation that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
-- * 'renewalStatus' - The status of ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> of the certificate.
-- * 'renewalStatusReason' - The reason that a renewal request was unsuccessful.
-- * 'updatedAt' - The time at which the renewal summary was last updated.
mkRenewalSummary ::
  -- | 'renewalStatus'
  RenewalStatus ->
  -- | 'domainValidationOptions'
  Lude.NonEmpty DomainValidation ->
  -- | 'updatedAt'
  Lude.Timestamp ->
  RenewalSummary
mkRenewalSummary
  pRenewalStatus_
  pDomainValidationOptions_
  pUpdatedAt_ =
    RenewalSummary'
      { renewalStatusReason = Lude.Nothing,
        renewalStatus = pRenewalStatus_,
        domainValidationOptions = pDomainValidationOptions_,
        updatedAt = pUpdatedAt_
      }

-- | The reason that a renewal request was unsuccessful.
--
-- /Note:/ Consider using 'renewalStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRenewalStatusReason :: Lens.Lens' RenewalSummary (Lude.Maybe FailureReason)
rsRenewalStatusReason = Lens.lens (renewalStatusReason :: RenewalSummary -> Lude.Maybe FailureReason) (\s a -> s {renewalStatusReason = a} :: RenewalSummary)
{-# DEPRECATED rsRenewalStatusReason "Use generic-lens or generic-optics with 'renewalStatusReason' instead." #-}

-- | The status of ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> of the certificate.
--
-- /Note:/ Consider using 'renewalStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRenewalStatus :: Lens.Lens' RenewalSummary RenewalStatus
rsRenewalStatus = Lens.lens (renewalStatus :: RenewalSummary -> RenewalStatus) (\s a -> s {renewalStatus = a} :: RenewalSummary)
{-# DEPRECATED rsRenewalStatus "Use generic-lens or generic-optics with 'renewalStatus' instead." #-}

-- | Contains information about the validation of each domain name in the certificate, as it pertains to ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> . This is different from the initial validation that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- /Note:/ Consider using 'domainValidationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsDomainValidationOptions :: Lens.Lens' RenewalSummary (Lude.NonEmpty DomainValidation)
rsDomainValidationOptions = Lens.lens (domainValidationOptions :: RenewalSummary -> Lude.NonEmpty DomainValidation) (\s a -> s {domainValidationOptions = a} :: RenewalSummary)
{-# DEPRECATED rsDomainValidationOptions "Use generic-lens or generic-optics with 'domainValidationOptions' instead." #-}

-- | The time at which the renewal summary was last updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsUpdatedAt :: Lens.Lens' RenewalSummary Lude.Timestamp
rsUpdatedAt = Lens.lens (updatedAt :: RenewalSummary -> Lude.Timestamp) (\s a -> s {updatedAt = a} :: RenewalSummary)
{-# DEPRECATED rsUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

instance Lude.FromJSON RenewalSummary where
  parseJSON =
    Lude.withObject
      "RenewalSummary"
      ( \x ->
          RenewalSummary'
            Lude.<$> (x Lude..:? "RenewalStatusReason")
            Lude.<*> (x Lude..: "RenewalStatus")
            Lude.<*> (x Lude..: "DomainValidationOptions")
            Lude.<*> (x Lude..: "UpdatedAt")
      )
