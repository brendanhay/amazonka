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
    tmfrRuleNumber,
    tmfrTrafficDirection,
    tmfrRuleAction,
    tmfrProtocol,
    tmfrTrafficMirrorFilterId,
    tmfrTrafficMirrorFilterRuleId,
    tmfrDestinationPortRange,
    tmfrSourceCidrBlock,
    tmfrSourcePortRange,
    tmfrDescription,
    tmfrDestinationCidrBlock,
  )
where

import Network.AWS.EC2.Types.TrafficDirection
import Network.AWS.EC2.Types.TrafficMirrorPortRange
import Network.AWS.EC2.Types.TrafficMirrorRuleAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Traffic Mirror rule.
--
-- /See:/ 'mkTrafficMirrorFilterRule' smart constructor.
data TrafficMirrorFilterRule = TrafficMirrorFilterRule'
  { ruleNumber ::
      Lude.Maybe Lude.Int,
    trafficDirection ::
      Lude.Maybe TrafficDirection,
    ruleAction ::
      Lude.Maybe TrafficMirrorRuleAction,
    protocol :: Lude.Maybe Lude.Int,
    trafficMirrorFilterId ::
      Lude.Maybe Lude.Text,
    trafficMirrorFilterRuleId ::
      Lude.Maybe Lude.Text,
    destinationPortRange ::
      Lude.Maybe TrafficMirrorPortRange,
    sourceCidrBlock :: Lude.Maybe Lude.Text,
    sourcePortRange ::
      Lude.Maybe TrafficMirrorPortRange,
    description :: Lude.Maybe Lude.Text,
    destinationCidrBlock ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrafficMirrorFilterRule' with the minimum fields required to make a request.
--
-- * 'description' - The description of the Traffic Mirror rule.
-- * 'destinationCidrBlock' - The destination CIDR block assigned to the Traffic Mirror rule.
-- * 'destinationPortRange' - The destination port range assigned to the Traffic Mirror rule.
-- * 'protocol' - The protocol assigned to the Traffic Mirror rule.
-- * 'ruleAction' - The action assigned to the Traffic Mirror rule.
-- * 'ruleNumber' - The rule number of the Traffic Mirror rule.
-- * 'sourceCidrBlock' - The source CIDR block assigned to the Traffic Mirror rule.
-- * 'sourcePortRange' - The source port range assigned to the Traffic Mirror rule.
-- * 'trafficDirection' - The traffic direction assigned to the Traffic Mirror rule.
-- * 'trafficMirrorFilterId' - The ID of the Traffic Mirror filter that the rule is associated with.
-- * 'trafficMirrorFilterRuleId' - The ID of the Traffic Mirror rule.
mkTrafficMirrorFilterRule ::
  TrafficMirrorFilterRule
mkTrafficMirrorFilterRule =
  TrafficMirrorFilterRule'
    { ruleNumber = Lude.Nothing,
      trafficDirection = Lude.Nothing,
      ruleAction = Lude.Nothing,
      protocol = Lude.Nothing,
      trafficMirrorFilterId = Lude.Nothing,
      trafficMirrorFilterRuleId = Lude.Nothing,
      destinationPortRange = Lude.Nothing,
      sourceCidrBlock = Lude.Nothing,
      sourcePortRange = Lude.Nothing,
      description = Lude.Nothing,
      destinationCidrBlock = Lude.Nothing
    }

-- | The rule number of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrRuleNumber :: Lens.Lens' TrafficMirrorFilterRule (Lude.Maybe Lude.Int)
tmfrRuleNumber = Lens.lens (ruleNumber :: TrafficMirrorFilterRule -> Lude.Maybe Lude.Int) (\s a -> s {ruleNumber = a} :: TrafficMirrorFilterRule)
{-# DEPRECATED tmfrRuleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead." #-}

-- | The traffic direction assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrTrafficDirection :: Lens.Lens' TrafficMirrorFilterRule (Lude.Maybe TrafficDirection)
tmfrTrafficDirection = Lens.lens (trafficDirection :: TrafficMirrorFilterRule -> Lude.Maybe TrafficDirection) (\s a -> s {trafficDirection = a} :: TrafficMirrorFilterRule)
{-# DEPRECATED tmfrTrafficDirection "Use generic-lens or generic-optics with 'trafficDirection' instead." #-}

-- | The action assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrRuleAction :: Lens.Lens' TrafficMirrorFilterRule (Lude.Maybe TrafficMirrorRuleAction)
tmfrRuleAction = Lens.lens (ruleAction :: TrafficMirrorFilterRule -> Lude.Maybe TrafficMirrorRuleAction) (\s a -> s {ruleAction = a} :: TrafficMirrorFilterRule)
{-# DEPRECATED tmfrRuleAction "Use generic-lens or generic-optics with 'ruleAction' instead." #-}

-- | The protocol assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrProtocol :: Lens.Lens' TrafficMirrorFilterRule (Lude.Maybe Lude.Int)
tmfrProtocol = Lens.lens (protocol :: TrafficMirrorFilterRule -> Lude.Maybe Lude.Int) (\s a -> s {protocol = a} :: TrafficMirrorFilterRule)
{-# DEPRECATED tmfrProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The ID of the Traffic Mirror filter that the rule is associated with.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrTrafficMirrorFilterId :: Lens.Lens' TrafficMirrorFilterRule (Lude.Maybe Lude.Text)
tmfrTrafficMirrorFilterId = Lens.lens (trafficMirrorFilterId :: TrafficMirrorFilterRule -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorFilterId = a} :: TrafficMirrorFilterRule)
{-# DEPRECATED tmfrTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The ID of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrTrafficMirrorFilterRuleId :: Lens.Lens' TrafficMirrorFilterRule (Lude.Maybe Lude.Text)
tmfrTrafficMirrorFilterRuleId = Lens.lens (trafficMirrorFilterRuleId :: TrafficMirrorFilterRule -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorFilterRuleId = a} :: TrafficMirrorFilterRule)
{-# DEPRECATED tmfrTrafficMirrorFilterRuleId "Use generic-lens or generic-optics with 'trafficMirrorFilterRuleId' instead." #-}

-- | The destination port range assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationPortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrDestinationPortRange :: Lens.Lens' TrafficMirrorFilterRule (Lude.Maybe TrafficMirrorPortRange)
tmfrDestinationPortRange = Lens.lens (destinationPortRange :: TrafficMirrorFilterRule -> Lude.Maybe TrafficMirrorPortRange) (\s a -> s {destinationPortRange = a} :: TrafficMirrorFilterRule)
{-# DEPRECATED tmfrDestinationPortRange "Use generic-lens or generic-optics with 'destinationPortRange' instead." #-}

-- | The source CIDR block assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourceCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrSourceCidrBlock :: Lens.Lens' TrafficMirrorFilterRule (Lude.Maybe Lude.Text)
tmfrSourceCidrBlock = Lens.lens (sourceCidrBlock :: TrafficMirrorFilterRule -> Lude.Maybe Lude.Text) (\s a -> s {sourceCidrBlock = a} :: TrafficMirrorFilterRule)
{-# DEPRECATED tmfrSourceCidrBlock "Use generic-lens or generic-optics with 'sourceCidrBlock' instead." #-}

-- | The source port range assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourcePortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrSourcePortRange :: Lens.Lens' TrafficMirrorFilterRule (Lude.Maybe TrafficMirrorPortRange)
tmfrSourcePortRange = Lens.lens (sourcePortRange :: TrafficMirrorFilterRule -> Lude.Maybe TrafficMirrorPortRange) (\s a -> s {sourcePortRange = a} :: TrafficMirrorFilterRule)
{-# DEPRECATED tmfrSourcePortRange "Use generic-lens or generic-optics with 'sourcePortRange' instead." #-}

-- | The description of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrDescription :: Lens.Lens' TrafficMirrorFilterRule (Lude.Maybe Lude.Text)
tmfrDescription = Lens.lens (description :: TrafficMirrorFilterRule -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TrafficMirrorFilterRule)
{-# DEPRECATED tmfrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The destination CIDR block assigned to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrDestinationCidrBlock :: Lens.Lens' TrafficMirrorFilterRule (Lude.Maybe Lude.Text)
tmfrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: TrafficMirrorFilterRule -> Lude.Maybe Lude.Text) (\s a -> s {destinationCidrBlock = a} :: TrafficMirrorFilterRule)
{-# DEPRECATED tmfrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

instance Lude.FromXML TrafficMirrorFilterRule where
  parseXML x =
    TrafficMirrorFilterRule'
      Lude.<$> (x Lude..@? "ruleNumber")
      Lude.<*> (x Lude..@? "trafficDirection")
      Lude.<*> (x Lude..@? "ruleAction")
      Lude.<*> (x Lude..@? "protocol")
      Lude.<*> (x Lude..@? "trafficMirrorFilterId")
      Lude.<*> (x Lude..@? "trafficMirrorFilterRuleId")
      Lude.<*> (x Lude..@? "destinationPortRange")
      Lude.<*> (x Lude..@? "sourceCidrBlock")
      Lude.<*> (x Lude..@? "sourcePortRange")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "destinationCidrBlock")
