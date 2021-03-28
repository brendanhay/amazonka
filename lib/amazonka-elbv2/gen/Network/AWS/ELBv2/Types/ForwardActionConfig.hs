{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.ForwardActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.ForwardActionConfig
  ( ForwardActionConfig (..)
  -- * Smart constructor
  , mkForwardActionConfig
  -- * Lenses
  , facTargetGroupStickinessConfig
  , facTargetGroups
  ) where

import qualified Network.AWS.ELBv2.Types.TargetGroupStickinessConfig as Types
import qualified Network.AWS.ELBv2.Types.TargetGroupTuple as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a forward action.
--
-- /See:/ 'mkForwardActionConfig' smart constructor.
data ForwardActionConfig = ForwardActionConfig'
  { targetGroupStickinessConfig :: Core.Maybe Types.TargetGroupStickinessConfig
    -- ^ The target group stickiness for the rule.
  , targetGroups :: Core.Maybe [Types.TargetGroupTuple]
    -- ^ One or more target groups. For Network Load Balancers, you can specify a single target group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ForwardActionConfig' value with any optional fields omitted.
mkForwardActionConfig
    :: ForwardActionConfig
mkForwardActionConfig
  = ForwardActionConfig'{targetGroupStickinessConfig = Core.Nothing,
                         targetGroups = Core.Nothing}

-- | The target group stickiness for the rule.
--
-- /Note:/ Consider using 'targetGroupStickinessConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
facTargetGroupStickinessConfig :: Lens.Lens' ForwardActionConfig (Core.Maybe Types.TargetGroupStickinessConfig)
facTargetGroupStickinessConfig = Lens.field @"targetGroupStickinessConfig"
{-# INLINEABLE facTargetGroupStickinessConfig #-}
{-# DEPRECATED targetGroupStickinessConfig "Use generic-lens or generic-optics with 'targetGroupStickinessConfig' instead"  #-}

-- | One or more target groups. For Network Load Balancers, you can specify a single target group.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
facTargetGroups :: Lens.Lens' ForwardActionConfig (Core.Maybe [Types.TargetGroupTuple])
facTargetGroups = Lens.field @"targetGroups"
{-# INLINEABLE facTargetGroups #-}
{-# DEPRECATED targetGroups "Use generic-lens or generic-optics with 'targetGroups' instead"  #-}

instance Core.ToQuery ForwardActionConfig where
        toQuery ForwardActionConfig{..}
          = Core.maybe Core.mempty
              (Core.toQueryPair "TargetGroupStickinessConfig")
              targetGroupStickinessConfig
              Core.<>
              Core.toQueryPair "TargetGroups"
                (Core.maybe Core.mempty (Core.toQueryList "member") targetGroups)

instance Core.FromXML ForwardActionConfig where
        parseXML x
          = ForwardActionConfig' Core.<$>
              (x Core..@? "TargetGroupStickinessConfig") Core.<*>
                x Core..@? "TargetGroups" Core..<@> Core.parseXMLList "member"
