{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.TagDescription
  ( TagDescription (..)
  -- * Smart constructor
  , mkTagDescription
  -- * Lenses
  , tdLoadBalancerName
  , tdTags
  ) where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.LoadBalancerName as Types
import qualified Network.AWS.ELB.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The tags associated with a load balancer.
--
-- /See:/ 'mkTagDescription' smart constructor.
data TagDescription = TagDescription'
  { loadBalancerName :: Core.Maybe Types.LoadBalancerName
    -- ^ The name of the load balancer.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagDescription' value with any optional fields omitted.
mkTagDescription
    :: TagDescription
mkTagDescription
  = TagDescription'{loadBalancerName = Core.Nothing,
                    tags = Core.Nothing}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdLoadBalancerName :: Lens.Lens' TagDescription (Core.Maybe Types.LoadBalancerName)
tdLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE tdLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTags :: Lens.Lens' TagDescription (Core.Maybe (Core.NonEmpty Types.Tag))
tdTags = Lens.field @"tags"
{-# INLINEABLE tdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML TagDescription where
        parseXML x
          = TagDescription' Core.<$>
              (x Core..@? "LoadBalancerName") Core.<*>
                x Core..@? "Tags" Core..<@> Core.parseXMLNonEmpty "member"
