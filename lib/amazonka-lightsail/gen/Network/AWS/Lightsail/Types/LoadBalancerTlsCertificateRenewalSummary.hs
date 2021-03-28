{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
  ( LoadBalancerTlsCertificateRenewalSummary (..)
  -- * Smart constructor
  , mkLoadBalancerTlsCertificateRenewalSummary
  -- * Lenses
  , lbtcrsDomainValidationOptions
  , lbtcrsRenewalStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information about the status of Lightsail's managed renewal for the certificate.
--
-- The renewal status of the certificate.
-- The following renewal status are possible:
--
--     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names in the certificate. No further action is required. 
--
--
--     * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names in the certificate. You must take action to validate these domain names or the certificate won't be renewed. If you used DNS validation, check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.
--
--
--     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required. 
--
--
--     * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
--
--
--
-- /See:/ 'mkLoadBalancerTlsCertificateRenewalSummary' smart constructor.
data LoadBalancerTlsCertificateRenewalSummary = LoadBalancerTlsCertificateRenewalSummary'
  { domainValidationOptions :: Core.Maybe [Types.LoadBalancerTlsCertificateDomainValidationOption]
    -- ^ Contains information about the validation of each domain name in the certificate, as it pertains to Lightsail's managed renewal. This is different from the initial validation that occurs as a result of the RequestCertificate request.
  , renewalStatus :: Core.Maybe Types.LoadBalancerTlsCertificateRenewalStatus
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancerTlsCertificateRenewalSummary' value with any optional fields omitted.
mkLoadBalancerTlsCertificateRenewalSummary
    :: LoadBalancerTlsCertificateRenewalSummary
mkLoadBalancerTlsCertificateRenewalSummary
  = LoadBalancerTlsCertificateRenewalSummary'{domainValidationOptions
                                                = Core.Nothing,
                                              renewalStatus = Core.Nothing}

-- | Contains information about the validation of each domain name in the certificate, as it pertains to Lightsail's managed renewal. This is different from the initial validation that occurs as a result of the RequestCertificate request.
--
-- /Note:/ Consider using 'domainValidationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcrsDomainValidationOptions :: Lens.Lens' LoadBalancerTlsCertificateRenewalSummary (Core.Maybe [Types.LoadBalancerTlsCertificateDomainValidationOption])
lbtcrsDomainValidationOptions = Lens.field @"domainValidationOptions"
{-# INLINEABLE lbtcrsDomainValidationOptions #-}
{-# DEPRECATED domainValidationOptions "Use generic-lens or generic-optics with 'domainValidationOptions' instead"  #-}

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
lbtcrsRenewalStatus :: Lens.Lens' LoadBalancerTlsCertificateRenewalSummary (Core.Maybe Types.LoadBalancerTlsCertificateRenewalStatus)
lbtcrsRenewalStatus = Lens.field @"renewalStatus"
{-# INLINEABLE lbtcrsRenewalStatus #-}
{-# DEPRECATED renewalStatus "Use generic-lens or generic-optics with 'renewalStatus' instead"  #-}

instance Core.FromJSON LoadBalancerTlsCertificateRenewalSummary
         where
        parseJSON
          = Core.withObject "LoadBalancerTlsCertificateRenewalSummary" Core.$
              \ x ->
                LoadBalancerTlsCertificateRenewalSummary' Core.<$>
                  (x Core..:? "domainValidationOptions") Core.<*>
                    x Core..:? "renewalStatus"
