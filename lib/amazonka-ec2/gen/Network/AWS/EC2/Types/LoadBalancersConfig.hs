{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadBalancersConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LoadBalancersConfig
  ( LoadBalancersConfig (..)
  -- * Smart constructor
  , mkLoadBalancersConfig
  -- * Lenses
  , lbcClassicLoadBalancersConfig
  , lbcTargetGroupsConfig
  ) where

import qualified Network.AWS.EC2.Types.ClassicLoadBalancersConfig as Types
import qualified Network.AWS.EC2.Types.TargetGroupsConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Classic Load Balancers and target groups to attach to a Spot Fleet request.
--
-- /See:/ 'mkLoadBalancersConfig' smart constructor.
data LoadBalancersConfig = LoadBalancersConfig'
  { classicLoadBalancersConfig :: Core.Maybe Types.ClassicLoadBalancersConfig
    -- ^ The Classic Load Balancers.
  , targetGroupsConfig :: Core.Maybe Types.TargetGroupsConfig
    -- ^ The target groups.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancersConfig' value with any optional fields omitted.
mkLoadBalancersConfig
    :: LoadBalancersConfig
mkLoadBalancersConfig
  = LoadBalancersConfig'{classicLoadBalancersConfig = Core.Nothing,
                         targetGroupsConfig = Core.Nothing}

-- | The Classic Load Balancers.
--
-- /Note:/ Consider using 'classicLoadBalancersConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbcClassicLoadBalancersConfig :: Lens.Lens' LoadBalancersConfig (Core.Maybe Types.ClassicLoadBalancersConfig)
lbcClassicLoadBalancersConfig = Lens.field @"classicLoadBalancersConfig"
{-# INLINEABLE lbcClassicLoadBalancersConfig #-}
{-# DEPRECATED classicLoadBalancersConfig "Use generic-lens or generic-optics with 'classicLoadBalancersConfig' instead"  #-}

-- | The target groups.
--
-- /Note:/ Consider using 'targetGroupsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbcTargetGroupsConfig :: Lens.Lens' LoadBalancersConfig (Core.Maybe Types.TargetGroupsConfig)
lbcTargetGroupsConfig = Lens.field @"targetGroupsConfig"
{-# INLINEABLE lbcTargetGroupsConfig #-}
{-# DEPRECATED targetGroupsConfig "Use generic-lens or generic-optics with 'targetGroupsConfig' instead"  #-}

instance Core.ToQuery LoadBalancersConfig where
        toQuery LoadBalancersConfig{..}
          = Core.maybe Core.mempty
              (Core.toQueryPair "ClassicLoadBalancersConfig")
              classicLoadBalancersConfig
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TargetGroupsConfig")
                targetGroupsConfig

instance Core.FromXML LoadBalancersConfig where
        parseXML x
          = LoadBalancersConfig' Core.<$>
              (x Core..@? "classicLoadBalancersConfig") Core.<*>
                x Core..@? "targetGroupsConfig"
