{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption
  ( LoadBalancerTlsCertificateDomainValidationOption (..)
  -- * Smart constructor
  , mkLoadBalancerTlsCertificateDomainValidationOption
  -- * Lenses
  , lbtcdvoDomainName
  , lbtcdvoValidationStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.DomainName as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information about the domain names on an SSL/TLS certificate that you will use to validate domain ownership.
--
-- /See:/ 'mkLoadBalancerTlsCertificateDomainValidationOption' smart constructor.
data LoadBalancerTlsCertificateDomainValidationOption = LoadBalancerTlsCertificateDomainValidationOption'
  { domainName :: Core.Maybe Types.DomainName
    -- ^ The fully qualified domain name in the certificate request.
  , validationStatus :: Core.Maybe Types.LoadBalancerTlsCertificateDomainStatus
    -- ^ The status of the domain validation. Valid values are listed below.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancerTlsCertificateDomainValidationOption' value with any optional fields omitted.
mkLoadBalancerTlsCertificateDomainValidationOption
    :: LoadBalancerTlsCertificateDomainValidationOption
mkLoadBalancerTlsCertificateDomainValidationOption
  = LoadBalancerTlsCertificateDomainValidationOption'{domainName =
                                                        Core.Nothing,
                                                      validationStatus = Core.Nothing}

-- | The fully qualified domain name in the certificate request.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvoDomainName :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationOption (Core.Maybe Types.DomainName)
lbtcdvoDomainName = Lens.field @"domainName"
{-# INLINEABLE lbtcdvoDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The status of the domain validation. Valid values are listed below.
--
-- /Note:/ Consider using 'validationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvoValidationStatus :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationOption (Core.Maybe Types.LoadBalancerTlsCertificateDomainStatus)
lbtcdvoValidationStatus = Lens.field @"validationStatus"
{-# INLINEABLE lbtcdvoValidationStatus #-}
{-# DEPRECATED validationStatus "Use generic-lens or generic-optics with 'validationStatus' instead"  #-}

instance Core.FromJSON
           LoadBalancerTlsCertificateDomainValidationOption
         where
        parseJSON
          = Core.withObject
              "LoadBalancerTlsCertificateDomainValidationOption"
              Core.$
              \ x ->
                LoadBalancerTlsCertificateDomainValidationOption' Core.<$>
                  (x Core..:? "domainName") Core.<*> x Core..:? "validationStatus"
