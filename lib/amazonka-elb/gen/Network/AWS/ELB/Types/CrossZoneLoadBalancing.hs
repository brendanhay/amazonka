{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.CrossZoneLoadBalancing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.CrossZoneLoadBalancing
  ( CrossZoneLoadBalancing (..),

    -- * Smart constructor
    mkCrossZoneLoadBalancing,

    -- * Lenses
    czlbEnabled,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the @CrossZoneLoadBalancing@ attribute.
--
-- /See:/ 'mkCrossZoneLoadBalancing' smart constructor.
newtype CrossZoneLoadBalancing = CrossZoneLoadBalancing'
  { -- | Specifies whether cross-zone load balancing is enabled for the load balancer.
    enabled :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CrossZoneLoadBalancing' value with any optional fields omitted.
mkCrossZoneLoadBalancing ::
  -- | 'enabled'
  Core.Bool ->
  CrossZoneLoadBalancing
mkCrossZoneLoadBalancing enabled = CrossZoneLoadBalancing' {enabled}

-- | Specifies whether cross-zone load balancing is enabled for the load balancer.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
czlbEnabled :: Lens.Lens' CrossZoneLoadBalancing Core.Bool
czlbEnabled = Lens.field @"enabled"
{-# DEPRECATED czlbEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromXML CrossZoneLoadBalancing where
  parseXML x = CrossZoneLoadBalancing' Core.<$> (x Core..@ "Enabled")
