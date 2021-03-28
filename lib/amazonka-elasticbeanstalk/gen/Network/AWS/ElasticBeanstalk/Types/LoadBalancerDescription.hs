{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
  ( LoadBalancerDescription (..)
  -- * Smart constructor
  , mkLoadBalancerDescription
  -- * Lenses
  , lbdDomain
  , lbdListeners
  , lbdLoadBalancerName
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.Listener as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the details of a LoadBalancer.
--
-- /See:/ 'mkLoadBalancerDescription' smart constructor.
data LoadBalancerDescription = LoadBalancerDescription'
  { domain :: Core.Maybe Core.Text
    -- ^ The domain name of the LoadBalancer.
  , listeners :: Core.Maybe [Types.Listener]
    -- ^ A list of Listeners used by the LoadBalancer.
  , loadBalancerName :: Core.Maybe Core.Text
    -- ^ The name of the LoadBalancer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancerDescription' value with any optional fields omitted.
mkLoadBalancerDescription
    :: LoadBalancerDescription
mkLoadBalancerDescription
  = LoadBalancerDescription'{domain = Core.Nothing,
                             listeners = Core.Nothing, loadBalancerName = Core.Nothing}

-- | The domain name of the LoadBalancer.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdDomain :: Lens.Lens' LoadBalancerDescription (Core.Maybe Core.Text)
lbdDomain = Lens.field @"domain"
{-# INLINEABLE lbdDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | A list of Listeners used by the LoadBalancer.
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdListeners :: Lens.Lens' LoadBalancerDescription (Core.Maybe [Types.Listener])
lbdListeners = Lens.field @"listeners"
{-# INLINEABLE lbdListeners #-}
{-# DEPRECATED listeners "Use generic-lens or generic-optics with 'listeners' instead"  #-}

-- | The name of the LoadBalancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdLoadBalancerName :: Lens.Lens' LoadBalancerDescription (Core.Maybe Core.Text)
lbdLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE lbdLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

instance Core.FromXML LoadBalancerDescription where
        parseXML x
          = LoadBalancerDescription' Core.<$>
              (x Core..@? "Domain") Core.<*>
                x Core..@? "Listeners" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "LoadBalancerName"
