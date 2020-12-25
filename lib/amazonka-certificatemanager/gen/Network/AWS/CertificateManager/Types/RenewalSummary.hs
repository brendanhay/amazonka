{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    rsRenewalStatus,
    rsDomainValidationOptions,
    rsUpdatedAt,
    rsRenewalStatusReason,
  )
where

import qualified Network.AWS.CertificateManager.Types.DomainValidation as Types
import qualified Network.AWS.CertificateManager.Types.FailureReason as Types
import qualified Network.AWS.CertificateManager.Types.RenewalStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the status of ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for the certificate. This structure exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- /See:/ 'mkRenewalSummary' smart constructor.
data RenewalSummary = RenewalSummary'
  { -- | The status of ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> of the certificate.
    renewalStatus :: Types.RenewalStatus,
    -- | Contains information about the validation of each domain name in the certificate, as it pertains to ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> . This is different from the initial validation that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
    domainValidationOptions :: Core.NonEmpty Types.DomainValidation,
    -- | The time at which the renewal summary was last updated.
    updatedAt :: Core.NominalDiffTime,
    -- | The reason that a renewal request was unsuccessful.
    renewalStatusReason :: Core.Maybe Types.FailureReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RenewalSummary' value with any optional fields omitted.
mkRenewalSummary ::
  -- | 'renewalStatus'
  Types.RenewalStatus ->
  -- | 'domainValidationOptions'
  Core.NonEmpty Types.DomainValidation ->
  -- | 'updatedAt'
  Core.NominalDiffTime ->
  RenewalSummary
mkRenewalSummary renewalStatus domainValidationOptions updatedAt =
  RenewalSummary'
    { renewalStatus,
      domainValidationOptions,
      updatedAt,
      renewalStatusReason = Core.Nothing
    }

-- | The status of ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> of the certificate.
--
-- /Note:/ Consider using 'renewalStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRenewalStatus :: Lens.Lens' RenewalSummary Types.RenewalStatus
rsRenewalStatus = Lens.field @"renewalStatus"
{-# DEPRECATED rsRenewalStatus "Use generic-lens or generic-optics with 'renewalStatus' instead." #-}

-- | Contains information about the validation of each domain name in the certificate, as it pertains to ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> . This is different from the initial validation that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- /Note:/ Consider using 'domainValidationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsDomainValidationOptions :: Lens.Lens' RenewalSummary (Core.NonEmpty Types.DomainValidation)
rsDomainValidationOptions = Lens.field @"domainValidationOptions"
{-# DEPRECATED rsDomainValidationOptions "Use generic-lens or generic-optics with 'domainValidationOptions' instead." #-}

-- | The time at which the renewal summary was last updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsUpdatedAt :: Lens.Lens' RenewalSummary Core.NominalDiffTime
rsUpdatedAt = Lens.field @"updatedAt"
{-# DEPRECATED rsUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

-- | The reason that a renewal request was unsuccessful.
--
-- /Note:/ Consider using 'renewalStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRenewalStatusReason :: Lens.Lens' RenewalSummary (Core.Maybe Types.FailureReason)
rsRenewalStatusReason = Lens.field @"renewalStatusReason"
{-# DEPRECATED rsRenewalStatusReason "Use generic-lens or generic-optics with 'renewalStatusReason' instead." #-}

instance Core.FromJSON RenewalSummary where
  parseJSON =
    Core.withObject "RenewalSummary" Core.$
      \x ->
        RenewalSummary'
          Core.<$> (x Core..: "RenewalStatus")
          Core.<*> (x Core..: "DomainValidationOptions")
          Core.<*> (x Core..: "UpdatedAt")
          Core.<*> (x Core..:? "RenewalStatusReason")
