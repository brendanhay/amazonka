{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RenewalSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.RenewalSummary
  ( RenewalSummary (..)
  -- * Smart constructor
  , mkRenewalSummary
  -- * Lenses
  , rsDomainValidationRecords
  , rsRenewalStatus
  , rsRenewalStatusReason
  , rsUpdatedAt
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.DomainValidationRecord as Types
import qualified Network.AWS.Lightsail.Types.RenewalStatus as Types
import qualified Network.AWS.Lightsail.Types.RenewalStatusReason as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the status of a SSL/TLS certificate renewal managed by Amazon Lightsail.
--
-- /See:/ 'mkRenewalSummary' smart constructor.
data RenewalSummary = RenewalSummary'
  { domainValidationRecords :: Core.Maybe [Types.DomainValidationRecord]
    -- ^ An array of objects that describe the domain validation records of the certificate.
  , renewalStatus :: Core.Maybe Types.RenewalStatus
    -- ^ The renewal status of the certificate.
--
-- The following renewal status are possible:
--
--     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names of the certificate. No further action is required. 
--
--
--     * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names of the certificate. You must take action to validate these domain names or the certificate won't be renewed. Check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.
--
--
--     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required. 
--
--
--     * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
--
--
  , renewalStatusReason :: Core.Maybe Types.RenewalStatusReason
    -- ^ The reason for the renewal status of the certificate.
  , updatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the certificate was last updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RenewalSummary' value with any optional fields omitted.
mkRenewalSummary
    :: RenewalSummary
mkRenewalSummary
  = RenewalSummary'{domainValidationRecords = Core.Nothing,
                    renewalStatus = Core.Nothing, renewalStatusReason = Core.Nothing,
                    updatedAt = Core.Nothing}

-- | An array of objects that describe the domain validation records of the certificate.
--
-- /Note:/ Consider using 'domainValidationRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsDomainValidationRecords :: Lens.Lens' RenewalSummary (Core.Maybe [Types.DomainValidationRecord])
rsDomainValidationRecords = Lens.field @"domainValidationRecords"
{-# INLINEABLE rsDomainValidationRecords #-}
{-# DEPRECATED domainValidationRecords "Use generic-lens or generic-optics with 'domainValidationRecords' instead"  #-}

-- | The renewal status of the certificate.
--
-- The following renewal status are possible:
--
--     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names of the certificate. No further action is required. 
--
--
--     * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names of the certificate. You must take action to validate these domain names or the certificate won't be renewed. Check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.
--
--
--     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required. 
--
--
--     * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
--
--
--
-- /Note:/ Consider using 'renewalStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRenewalStatus :: Lens.Lens' RenewalSummary (Core.Maybe Types.RenewalStatus)
rsRenewalStatus = Lens.field @"renewalStatus"
{-# INLINEABLE rsRenewalStatus #-}
{-# DEPRECATED renewalStatus "Use generic-lens or generic-optics with 'renewalStatus' instead"  #-}

-- | The reason for the renewal status of the certificate.
--
-- /Note:/ Consider using 'renewalStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRenewalStatusReason :: Lens.Lens' RenewalSummary (Core.Maybe Types.RenewalStatusReason)
rsRenewalStatusReason = Lens.field @"renewalStatusReason"
{-# INLINEABLE rsRenewalStatusReason #-}
{-# DEPRECATED renewalStatusReason "Use generic-lens or generic-optics with 'renewalStatusReason' instead"  #-}

-- | The timestamp when the certificate was last updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsUpdatedAt :: Lens.Lens' RenewalSummary (Core.Maybe Core.NominalDiffTime)
rsUpdatedAt = Lens.field @"updatedAt"
{-# INLINEABLE rsUpdatedAt #-}
{-# DEPRECATED updatedAt "Use generic-lens or generic-optics with 'updatedAt' instead"  #-}

instance Core.FromJSON RenewalSummary where
        parseJSON
          = Core.withObject "RenewalSummary" Core.$
              \ x ->
                RenewalSummary' Core.<$>
                  (x Core..:? "domainValidationRecords") Core.<*>
                    x Core..:? "renewalStatus"
                    Core.<*> x Core..:? "renewalStatusReason"
                    Core.<*> x Core..:? "updatedAt"
