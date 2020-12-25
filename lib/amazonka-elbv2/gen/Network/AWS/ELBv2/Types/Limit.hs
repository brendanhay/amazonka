{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Limit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Limit
  ( Limit (..),

    -- * Smart constructor
    mkLimit,

    -- * Lenses
    lMax,
    lName,
  )
where

import qualified Network.AWS.ELBv2.Types.Max as Types
import qualified Network.AWS.ELBv2.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an Elastic Load Balancing resource limit for your AWS account.
--
-- /See:/ 'mkLimit' smart constructor.
data Limit = Limit'
  { -- | The maximum value of the limit.
    max :: Core.Maybe Types.Max,
    -- | The name of the limit. The possible values are:
    --
    --
    --     * application-load-balancers
    --
    --
    --     * condition-values-per-alb-rule
    --
    --
    --     * condition-wildcards-per-alb-rule
    --
    --
    --     * gateway-load-balancers
    --
    --
    --     * gateway-load-balancers-per-vpc
    --
    --
    --     * geneve-target-groups
    --
    --
    --     * listeners-per-application-load-balancer
    --
    --
    --     * listeners-per-network-load-balancer
    --
    --
    --     * network-load-balancers
    --
    --
    --     * rules-per-application-load-balancer
    --
    --
    --     * target-groups
    --
    --
    --     * target-groups-per-action-on-application-load-balancer
    --
    --
    --     * target-groups-per-action-on-network-load-balancer
    --
    --
    --     * target-groups-per-application-load-balancer
    --
    --
    --     * targets-per-application-load-balancer
    --
    --
    --     * targets-per-availability-zone-per-gateway-load-balancer
    --
    --
    --     * targets-per-availability-zone-per-network-load-balancer
    --
    --
    --     * targets-per-network-load-balancer
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Limit' value with any optional fields omitted.
mkLimit ::
  Limit
mkLimit = Limit' {max = Core.Nothing, name = Core.Nothing}

-- | The maximum value of the limit.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMax :: Lens.Lens' Limit (Core.Maybe Types.Max)
lMax = Lens.field @"max"
{-# DEPRECATED lMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | The name of the limit. The possible values are:
--
--
--     * application-load-balancers
--
--
--     * condition-values-per-alb-rule
--
--
--     * condition-wildcards-per-alb-rule
--
--
--     * gateway-load-balancers
--
--
--     * gateway-load-balancers-per-vpc
--
--
--     * geneve-target-groups
--
--
--     * listeners-per-application-load-balancer
--
--
--     * listeners-per-network-load-balancer
--
--
--     * network-load-balancers
--
--
--     * rules-per-application-load-balancer
--
--
--     * target-groups
--
--
--     * target-groups-per-action-on-application-load-balancer
--
--
--     * target-groups-per-action-on-network-load-balancer
--
--
--     * target-groups-per-application-load-balancer
--
--
--     * targets-per-application-load-balancer
--
--
--     * targets-per-availability-zone-per-gateway-load-balancer
--
--
--     * targets-per-availability-zone-per-network-load-balancer
--
--
--     * targets-per-network-load-balancer
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lName :: Lens.Lens' Limit (Core.Maybe Types.Name)
lName = Lens.field @"name"
{-# DEPRECATED lName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromXML Limit where
  parseXML x =
    Limit' Core.<$> (x Core..@? "Max") Core.<*> (x Core..@? "Name")
