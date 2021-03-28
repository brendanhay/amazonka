{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateSummary
  ( LoadBalancerTlsCertificateSummary (..)
  -- * Smart constructor
  , mkLoadBalancerTlsCertificateSummary
  -- * Lenses
  , lbtcsIsAttached
  , lbtcsName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Prelude as Core

-- | Provides a summary of SSL/TLS certificate metadata.
--
-- /See:/ 'mkLoadBalancerTlsCertificateSummary' smart constructor.
data LoadBalancerTlsCertificateSummary = LoadBalancerTlsCertificateSummary'
  { isAttached :: Core.Maybe Core.Bool
    -- ^ When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name of the SSL/TLS certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancerTlsCertificateSummary' value with any optional fields omitted.
mkLoadBalancerTlsCertificateSummary
    :: LoadBalancerTlsCertificateSummary
mkLoadBalancerTlsCertificateSummary
  = LoadBalancerTlsCertificateSummary'{isAttached = Core.Nothing,
                                       name = Core.Nothing}

-- | When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
--
-- /Note:/ Consider using 'isAttached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcsIsAttached :: Lens.Lens' LoadBalancerTlsCertificateSummary (Core.Maybe Core.Bool)
lbtcsIsAttached = Lens.field @"isAttached"
{-# INLINEABLE lbtcsIsAttached #-}
{-# DEPRECATED isAttached "Use generic-lens or generic-optics with 'isAttached' instead"  #-}

-- | The name of the SSL/TLS certificate.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcsName :: Lens.Lens' LoadBalancerTlsCertificateSummary (Core.Maybe Types.ResourceName)
lbtcsName = Lens.field @"name"
{-# INLINEABLE lbtcsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON LoadBalancerTlsCertificateSummary where
        parseJSON
          = Core.withObject "LoadBalancerTlsCertificateSummary" Core.$
              \ x ->
                LoadBalancerTlsCertificateSummary' Core.<$>
                  (x Core..:? "isAttached") Core.<*> x Core..:? "name"
