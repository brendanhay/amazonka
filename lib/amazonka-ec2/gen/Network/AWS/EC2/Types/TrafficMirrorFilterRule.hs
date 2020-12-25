{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorFilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorFilterRule
  ( TrafficMirrorFilterRule (..),

    -- * Smart constructor
    mkTrafficMirrorFilterRule,

    -- * Lenses
    tmfrDescription,
    tmfrDestinationCidrBlock,
    tmfrDestinationPortRange,
    tmfrProtocol,
    tmfrRuleAction,
    tmfrRuleNumber,
    tmfrSourceCidrBlock,
    tmfrSourcePortRange,
    tmfrTrafficDirection,
    tmfrTrafficMirrorFilterId,
    tmfrTrafficMirrorFilterRuleId,
  )
where

import qualified Network.AWS.EC2.Types.Description as Types
import qualified Network.AWS.EC2.Types.DestinationCidrBlock as Types
import qualified Network.AWS.EC2.Types.SourceCidrBlock as Types
import qualified Network.AWS.EC2.Types.TrafficDirection as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorFilterId as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorFilterRuleId as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorPortRange as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorRuleAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Traffic Mirror rule.
--
-- /See:/ 'mkTrafficMirrorFilterRule' smart constructor.
data TrafficMirrorFilterRule = TrafficMirrorFilterRule'
  { -- | The description of the Traffic Mirror rule.
    description :: Core.Maybe Types.Description,
    -- | The destination CIDR block assigned to the Traffic Mirror rule.
    destinationCidrBlock :: Core.Maybe Types.DestinationCidrBlock,
    -- | The destination port range assigned to the Traffic Mirror rule.
    destinationPortRange :: Core.Maybe Types.TrafficMirrorPortRange,
    -- | The protocol assigned to the Traffic Mirror rule.
    protocol :: Core.Maybe Core.Int,
    -- | The action assigned to the Traffic Mirror rule.
    ruleAction :: Core.Maybe Types.TrafficMirrorRuleAction,
    -- | The rule number of the Traffic Mirror rule.
    ruleNumber :: Core.Maybe Core.Int,
    -- | The source CIDR block assigned to the Traffic Mirror rule.
    sourceCidrBlock :: Core.Maybe Types.SourceCidrBlock,
    -- | The source port range assigned to the Traffic Mirror rule.
    sourcePortRange :: Core.Maybe Types.TrafficMirrorPortRange,
    -- | The traffic direction assigned to the Traffic Mirror rule.
    trafficDirection :: Core.Maybe Types.TrafficDirection,
    -- | The ID of the Traffic Mirror filter that the rule is associated with.
    trafficMirrorFilterId :: Core.Maybe Types.TrafficMirrorFilterId,
    -- | The ID of the Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Core.Maybe Types.TrafficMirrorFilterRuleId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficMirrorFilterRule' value with any optional fields omitted.
mkTrafficMirrorFilterRule ::
  TrafficMirrorFilterRule
mkTrafficMirrorFilterRule =
  TrafficMirrorFilterRule'
    { description = Core.Nothing,
      destinationCidrBlock = Core.Nothing,
      destinationPortRange = Core.Nothing,
      protocol = Core.Nothing,
      ruleAction = Core.Nothing,
      ruleNumber = Core.Nothing,
      sourceCidrBlock = Core.Nothing,
      sourcePortRange = Core.Nothing,
      trafficDirection = Core.Nothing,
      trafficMirrorFilterId = Core.Nothing,
      trafficMirrorFilterRuleId = Core.Nothing
    }

-- | The description of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrDescription :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.Description)
tmfrDescription = Lens.field @"description"
{-# DEPRECATED tmfrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The destination CIDR block assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrDestinationCidrBlock :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.DestinationCidrBlock)
tmfrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED tmfrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The destination port range assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationPortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrDestinationPortRange :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRange)
tmfrDestinationPortRange = Lens.field @"destinationPortRange"
{-# DEPRECATED tmfrDestinationPortRange "Use generic-lens or generic-optics with 'destinationPortRange' instead." #-}

-- | The protocol assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrProtocol :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Int)
tmfrProtocol = Lens.field @"protocol"
{-# DEPRECATED tmfrProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The action assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrRuleAction :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorRuleAction)
tmfrRuleAction = Lens.field @"ruleAction"
{-# DEPRECATED tmfrRuleAction "Use generic-lens or generic-optics with 'ruleAction' instead." #-}

-- | The rule number of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrRuleNumber :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Int)
tmfrRuleNumber = Lens.field @"ruleNumber"
{-# DEPRECATED tmfrRuleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead." #-}

-- | The source CIDR block assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourceCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrSourceCidrBlock :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.SourceCidrBlock)
tmfrSourceCidrBlock = Lens.field @"sourceCidrBlock"
{-# DEPRECATED tmfrSourceCidrBlock "Use generic-lens or generic-optics with 'sourceCidrBlock' instead." #-}

-- | The source port range assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourcePortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrSourcePortRange :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRange)
tmfrSourcePortRange = Lens.field @"sourcePortRange"
{-# DEPRECATED tmfrSourcePortRange "Use generic-lens or generic-optics with 'sourcePortRange' instead." #-}

-- | The traffic direction assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrTrafficDirection :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.TrafficDirection)
tmfrTrafficDirection = Lens.field @"trafficDirection"
{-# DEPRECATED tmfrTrafficDirection "Use generic-lens or generic-optics with 'trafficDirection' instead." #-}

-- | The ID of the Traffic Mirror filter that the rule is associated with.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrTrafficMirrorFilterId :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorFilterId)
tmfrTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# DEPRECATED tmfrTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The ID of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrTrafficMirrorFilterRuleId :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorFilterRuleId)
tmfrTrafficMirrorFilterRuleId = Lens.field @"trafficMirrorFilterRuleId"
{-# DEPRECATED tmfrTrafficMirrorFilterRuleId "Use generic-lens or generic-optics with 'trafficMirrorFilterRuleId' instead." #-}

instance Core.FromXML TrafficMirrorFilterRule where
  parseXML x =
    TrafficMirrorFilterRule'
      Core.<$> (x Core..@? "description")
      Core.<*> (x Core..@? "destinationCidrBlock")
      Core.<*> (x Core..@? "destinationPortRange")
      Core.<*> (x Core..@? "protocol")
      Core.<*> (x Core..@? "ruleAction")
      Core.<*> (x Core..@? "ruleNumber")
      Core.<*> (x Core..@? "sourceCidrBlock")
      Core.<*> (x Core..@? "sourcePortRange")
      Core.<*> (x Core..@? "trafficDirection")
      Core.<*> (x Core..@? "trafficMirrorFilterId")
      Core.<*> (x Core..@? "trafficMirrorFilterRuleId")
