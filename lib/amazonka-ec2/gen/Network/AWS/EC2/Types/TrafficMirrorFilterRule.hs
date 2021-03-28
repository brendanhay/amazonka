{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorFilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TrafficMirrorFilterRule
  ( TrafficMirrorFilterRule (..)
  -- * Smart constructor
  , mkTrafficMirrorFilterRule
  -- * Lenses
  , tmfrDescription
  , tmfrDestinationCidrBlock
  , tmfrDestinationPortRange
  , tmfrProtocol
  , tmfrRuleAction
  , tmfrRuleNumber
  , tmfrSourceCidrBlock
  , tmfrSourcePortRange
  , tmfrTrafficDirection
  , tmfrTrafficMirrorFilterId
  , tmfrTrafficMirrorFilterRuleId
  ) where

import qualified Network.AWS.EC2.Types.TrafficDirection as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorPortRange as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorRuleAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Traffic Mirror rule.
--
-- /See:/ 'mkTrafficMirrorFilterRule' smart constructor.
data TrafficMirrorFilterRule = TrafficMirrorFilterRule'
  { description :: Core.Maybe Core.Text
    -- ^ The description of the Traffic Mirror rule.
  , destinationCidrBlock :: Core.Maybe Core.Text
    -- ^ The destination CIDR block assigned to the Traffic Mirror rule.
  , destinationPortRange :: Core.Maybe Types.TrafficMirrorPortRange
    -- ^ The destination port range assigned to the Traffic Mirror rule.
  , protocol :: Core.Maybe Core.Int
    -- ^ The protocol assigned to the Traffic Mirror rule.
  , ruleAction :: Core.Maybe Types.TrafficMirrorRuleAction
    -- ^ The action assigned to the Traffic Mirror rule.
  , ruleNumber :: Core.Maybe Core.Int
    -- ^ The rule number of the Traffic Mirror rule.
  , sourceCidrBlock :: Core.Maybe Core.Text
    -- ^ The source CIDR block assigned to the Traffic Mirror rule.
  , sourcePortRange :: Core.Maybe Types.TrafficMirrorPortRange
    -- ^ The source port range assigned to the Traffic Mirror rule.
  , trafficDirection :: Core.Maybe Types.TrafficDirection
    -- ^ The traffic direction assigned to the Traffic Mirror rule.
  , trafficMirrorFilterId :: Core.Maybe Core.Text
    -- ^ The ID of the Traffic Mirror filter that the rule is associated with.
  , trafficMirrorFilterRuleId :: Core.Maybe Core.Text
    -- ^ The ID of the Traffic Mirror rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficMirrorFilterRule' value with any optional fields omitted.
mkTrafficMirrorFilterRule
    :: TrafficMirrorFilterRule
mkTrafficMirrorFilterRule
  = TrafficMirrorFilterRule'{description = Core.Nothing,
                             destinationCidrBlock = Core.Nothing,
                             destinationPortRange = Core.Nothing, protocol = Core.Nothing,
                             ruleAction = Core.Nothing, ruleNumber = Core.Nothing,
                             sourceCidrBlock = Core.Nothing, sourcePortRange = Core.Nothing,
                             trafficDirection = Core.Nothing,
                             trafficMirrorFilterId = Core.Nothing,
                             trafficMirrorFilterRuleId = Core.Nothing}

-- | The description of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrDescription :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Text)
tmfrDescription = Lens.field @"description"
{-# INLINEABLE tmfrDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The destination CIDR block assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrDestinationCidrBlock :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Text)
tmfrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE tmfrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The destination port range assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationPortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrDestinationPortRange :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRange)
tmfrDestinationPortRange = Lens.field @"destinationPortRange"
{-# INLINEABLE tmfrDestinationPortRange #-}
{-# DEPRECATED destinationPortRange "Use generic-lens or generic-optics with 'destinationPortRange' instead"  #-}

-- | The protocol assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrProtocol :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Int)
tmfrProtocol = Lens.field @"protocol"
{-# INLINEABLE tmfrProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The action assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrRuleAction :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorRuleAction)
tmfrRuleAction = Lens.field @"ruleAction"
{-# INLINEABLE tmfrRuleAction #-}
{-# DEPRECATED ruleAction "Use generic-lens or generic-optics with 'ruleAction' instead"  #-}

-- | The rule number of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrRuleNumber :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Int)
tmfrRuleNumber = Lens.field @"ruleNumber"
{-# INLINEABLE tmfrRuleNumber #-}
{-# DEPRECATED ruleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead"  #-}

-- | The source CIDR block assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourceCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrSourceCidrBlock :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Text)
tmfrSourceCidrBlock = Lens.field @"sourceCidrBlock"
{-# INLINEABLE tmfrSourceCidrBlock #-}
{-# DEPRECATED sourceCidrBlock "Use generic-lens or generic-optics with 'sourceCidrBlock' instead"  #-}

-- | The source port range assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourcePortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrSourcePortRange :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRange)
tmfrSourcePortRange = Lens.field @"sourcePortRange"
{-# INLINEABLE tmfrSourcePortRange #-}
{-# DEPRECATED sourcePortRange "Use generic-lens or generic-optics with 'sourcePortRange' instead"  #-}

-- | The traffic direction assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrTrafficDirection :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Types.TrafficDirection)
tmfrTrafficDirection = Lens.field @"trafficDirection"
{-# INLINEABLE tmfrTrafficDirection #-}
{-# DEPRECATED trafficDirection "Use generic-lens or generic-optics with 'trafficDirection' instead"  #-}

-- | The ID of the Traffic Mirror filter that the rule is associated with.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrTrafficMirrorFilterId :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Text)
tmfrTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# INLINEABLE tmfrTrafficMirrorFilterId #-}
{-# DEPRECATED trafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead"  #-}

-- | The ID of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrTrafficMirrorFilterRuleId :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Text)
tmfrTrafficMirrorFilterRuleId = Lens.field @"trafficMirrorFilterRuleId"
{-# INLINEABLE tmfrTrafficMirrorFilterRuleId #-}
{-# DEPRECATED trafficMirrorFilterRuleId "Use generic-lens or generic-optics with 'trafficMirrorFilterRuleId' instead"  #-}

instance Core.FromXML TrafficMirrorFilterRule where
        parseXML x
          = TrafficMirrorFilterRule' Core.<$>
              (x Core..@? "description") Core.<*>
                x Core..@? "destinationCidrBlock"
                Core.<*> x Core..@? "destinationPortRange"
                Core.<*> x Core..@? "protocol"
                Core.<*> x Core..@? "ruleAction"
                Core.<*> x Core..@? "ruleNumber"
                Core.<*> x Core..@? "sourceCidrBlock"
                Core.<*> x Core..@? "sourcePortRange"
                Core.<*> x Core..@? "trafficDirection"
                Core.<*> x Core..@? "trafficMirrorFilterId"
                Core.<*> x Core..@? "trafficMirrorFilterRuleId"
