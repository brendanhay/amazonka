{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTrafficMirrorFilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Traffic Mirror rule.
--
-- @DestinationCidrBlock@ and @SourceCidrBlock@ must both be an IPv4 range or an IPv6 range.
module Network.AWS.EC2.ModifyTrafficMirrorFilterRule
  ( -- * Creating a request
    ModifyTrafficMirrorFilterRule (..),
    mkModifyTrafficMirrorFilterRule,

    -- ** Request lenses
    mtmfrRemoveFields,
    mtmfrRuleNumber,
    mtmfrTrafficDirection,
    mtmfrRuleAction,
    mtmfrProtocol,
    mtmfrTrafficMirrorFilterRuleId,
    mtmfrDestinationPortRange,
    mtmfrSourceCidrBlock,
    mtmfrSourcePortRange,
    mtmfrDescription,
    mtmfrDryRun,
    mtmfrDestinationCidrBlock,

    -- * Destructuring the response
    ModifyTrafficMirrorFilterRuleResponse (..),
    mkModifyTrafficMirrorFilterRuleResponse,

    -- ** Response lenses
    mtmfrrsTrafficMirrorFilterRule,
    mtmfrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyTrafficMirrorFilterRule' smart constructor.
data ModifyTrafficMirrorFilterRule = ModifyTrafficMirrorFilterRule'
  { -- | The properties that you want to remove from the Traffic Mirror filter rule.
    --
    -- When you remove a property from a Traffic Mirror filter rule, the property is set to the default.
    removeFields :: Lude.Maybe [TrafficMirrorFilterRuleField],
    -- | The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
    ruleNumber :: Lude.Maybe Lude.Int,
    -- | The type of traffic (@ingress@ | @egress@ ) to assign to the rule.
    trafficDirection :: Lude.Maybe TrafficDirection,
    -- | The action to assign to the rule.
    ruleAction :: Lude.Maybe TrafficMirrorRuleAction,
    -- | The protocol, for example TCP, to assign to the Traffic Mirror rule.
    protocol :: Lude.Maybe Lude.Int,
    -- | The ID of the Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Lude.Text,
    -- | The destination ports that are associated with the Traffic Mirror rule.
    destinationPortRange :: Lude.Maybe TrafficMirrorPortRangeRequest,
    -- | The source CIDR block to assign to the Traffic Mirror rule.
    sourceCidrBlock :: Lude.Maybe Lude.Text,
    -- | The port range to assign to the Traffic Mirror rule.
    sourcePortRange :: Lude.Maybe TrafficMirrorPortRangeRequest,
    -- | The description to assign to the Traffic Mirror rule.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The destination CIDR block to assign to the Traffic Mirror rule.
    destinationCidrBlock :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTrafficMirrorFilterRule' with the minimum fields required to make a request.
--
-- * 'removeFields' - The properties that you want to remove from the Traffic Mirror filter rule.
--
-- When you remove a property from a Traffic Mirror filter rule, the property is set to the default.
-- * 'ruleNumber' - The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
-- * 'trafficDirection' - The type of traffic (@ingress@ | @egress@ ) to assign to the rule.
-- * 'ruleAction' - The action to assign to the rule.
-- * 'protocol' - The protocol, for example TCP, to assign to the Traffic Mirror rule.
-- * 'trafficMirrorFilterRuleId' - The ID of the Traffic Mirror rule.
-- * 'destinationPortRange' - The destination ports that are associated with the Traffic Mirror rule.
-- * 'sourceCidrBlock' - The source CIDR block to assign to the Traffic Mirror rule.
-- * 'sourcePortRange' - The port range to assign to the Traffic Mirror rule.
-- * 'description' - The description to assign to the Traffic Mirror rule.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'destinationCidrBlock' - The destination CIDR block to assign to the Traffic Mirror rule.
mkModifyTrafficMirrorFilterRule ::
  -- | 'trafficMirrorFilterRuleId'
  Lude.Text ->
  ModifyTrafficMirrorFilterRule
mkModifyTrafficMirrorFilterRule pTrafficMirrorFilterRuleId_ =
  ModifyTrafficMirrorFilterRule'
    { removeFields = Lude.Nothing,
      ruleNumber = Lude.Nothing,
      trafficDirection = Lude.Nothing,
      ruleAction = Lude.Nothing,
      protocol = Lude.Nothing,
      trafficMirrorFilterRuleId = pTrafficMirrorFilterRuleId_,
      destinationPortRange = Lude.Nothing,
      sourceCidrBlock = Lude.Nothing,
      sourcePortRange = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing,
      destinationCidrBlock = Lude.Nothing
    }

-- | The properties that you want to remove from the Traffic Mirror filter rule.
--
-- When you remove a property from a Traffic Mirror filter rule, the property is set to the default.
--
-- /Note:/ Consider using 'removeFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrRemoveFields :: Lens.Lens' ModifyTrafficMirrorFilterRule (Lude.Maybe [TrafficMirrorFilterRuleField])
mtmfrRemoveFields = Lens.lens (removeFields :: ModifyTrafficMirrorFilterRule -> Lude.Maybe [TrafficMirrorFilterRuleField]) (\s a -> s {removeFields = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrRemoveFields "Use generic-lens or generic-optics with 'removeFields' instead." #-}

-- | The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrRuleNumber :: Lens.Lens' ModifyTrafficMirrorFilterRule (Lude.Maybe Lude.Int)
mtmfrRuleNumber = Lens.lens (ruleNumber :: ModifyTrafficMirrorFilterRule -> Lude.Maybe Lude.Int) (\s a -> s {ruleNumber = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrRuleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead." #-}

-- | The type of traffic (@ingress@ | @egress@ ) to assign to the rule.
--
-- /Note:/ Consider using 'trafficDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrTrafficDirection :: Lens.Lens' ModifyTrafficMirrorFilterRule (Lude.Maybe TrafficDirection)
mtmfrTrafficDirection = Lens.lens (trafficDirection :: ModifyTrafficMirrorFilterRule -> Lude.Maybe TrafficDirection) (\s a -> s {trafficDirection = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrTrafficDirection "Use generic-lens or generic-optics with 'trafficDirection' instead." #-}

-- | The action to assign to the rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrRuleAction :: Lens.Lens' ModifyTrafficMirrorFilterRule (Lude.Maybe TrafficMirrorRuleAction)
mtmfrRuleAction = Lens.lens (ruleAction :: ModifyTrafficMirrorFilterRule -> Lude.Maybe TrafficMirrorRuleAction) (\s a -> s {ruleAction = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrRuleAction "Use generic-lens or generic-optics with 'ruleAction' instead." #-}

-- | The protocol, for example TCP, to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrProtocol :: Lens.Lens' ModifyTrafficMirrorFilterRule (Lude.Maybe Lude.Int)
mtmfrProtocol = Lens.lens (protocol :: ModifyTrafficMirrorFilterRule -> Lude.Maybe Lude.Int) (\s a -> s {protocol = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The ID of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrTrafficMirrorFilterRuleId :: Lens.Lens' ModifyTrafficMirrorFilterRule Lude.Text
mtmfrTrafficMirrorFilterRuleId = Lens.lens (trafficMirrorFilterRuleId :: ModifyTrafficMirrorFilterRule -> Lude.Text) (\s a -> s {trafficMirrorFilterRuleId = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrTrafficMirrorFilterRuleId "Use generic-lens or generic-optics with 'trafficMirrorFilterRuleId' instead." #-}

-- | The destination ports that are associated with the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationPortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDestinationPortRange :: Lens.Lens' ModifyTrafficMirrorFilterRule (Lude.Maybe TrafficMirrorPortRangeRequest)
mtmfrDestinationPortRange = Lens.lens (destinationPortRange :: ModifyTrafficMirrorFilterRule -> Lude.Maybe TrafficMirrorPortRangeRequest) (\s a -> s {destinationPortRange = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrDestinationPortRange "Use generic-lens or generic-optics with 'destinationPortRange' instead." #-}

-- | The source CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourceCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrSourceCidrBlock :: Lens.Lens' ModifyTrafficMirrorFilterRule (Lude.Maybe Lude.Text)
mtmfrSourceCidrBlock = Lens.lens (sourceCidrBlock :: ModifyTrafficMirrorFilterRule -> Lude.Maybe Lude.Text) (\s a -> s {sourceCidrBlock = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrSourceCidrBlock "Use generic-lens or generic-optics with 'sourceCidrBlock' instead." #-}

-- | The port range to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourcePortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrSourcePortRange :: Lens.Lens' ModifyTrafficMirrorFilterRule (Lude.Maybe TrafficMirrorPortRangeRequest)
mtmfrSourcePortRange = Lens.lens (sourcePortRange :: ModifyTrafficMirrorFilterRule -> Lude.Maybe TrafficMirrorPortRangeRequest) (\s a -> s {sourcePortRange = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrSourcePortRange "Use generic-lens or generic-optics with 'sourcePortRange' instead." #-}

-- | The description to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDescription :: Lens.Lens' ModifyTrafficMirrorFilterRule (Lude.Maybe Lude.Text)
mtmfrDescription = Lens.lens (description :: ModifyTrafficMirrorFilterRule -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDryRun :: Lens.Lens' ModifyTrafficMirrorFilterRule (Lude.Maybe Lude.Bool)
mtmfrDryRun = Lens.lens (dryRun :: ModifyTrafficMirrorFilterRule -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The destination CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDestinationCidrBlock :: Lens.Lens' ModifyTrafficMirrorFilterRule (Lude.Maybe Lude.Text)
mtmfrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: ModifyTrafficMirrorFilterRule -> Lude.Maybe Lude.Text) (\s a -> s {destinationCidrBlock = a} :: ModifyTrafficMirrorFilterRule)
{-# DEPRECATED mtmfrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

instance Lude.AWSRequest ModifyTrafficMirrorFilterRule where
  type
    Rs ModifyTrafficMirrorFilterRule =
      ModifyTrafficMirrorFilterRuleResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyTrafficMirrorFilterRuleResponse'
            Lude.<$> (x Lude..@? "trafficMirrorFilterRule")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyTrafficMirrorFilterRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyTrafficMirrorFilterRule where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyTrafficMirrorFilterRule where
  toQuery ModifyTrafficMirrorFilterRule' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyTrafficMirrorFilterRule" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "RemoveField" Lude.<$> removeFields),
        "RuleNumber" Lude.=: ruleNumber,
        "TrafficDirection" Lude.=: trafficDirection,
        "RuleAction" Lude.=: ruleAction,
        "Protocol" Lude.=: protocol,
        "TrafficMirrorFilterRuleId" Lude.=: trafficMirrorFilterRuleId,
        "DestinationPortRange" Lude.=: destinationPortRange,
        "SourceCidrBlock" Lude.=: sourceCidrBlock,
        "SourcePortRange" Lude.=: sourcePortRange,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock
      ]

-- | /See:/ 'mkModifyTrafficMirrorFilterRuleResponse' smart constructor.
data ModifyTrafficMirrorFilterRuleResponse = ModifyTrafficMirrorFilterRuleResponse'
  { -- | Modifies a Traffic Mirror rule.
    trafficMirrorFilterRule :: Lude.Maybe TrafficMirrorFilterRule,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTrafficMirrorFilterRuleResponse' with the minimum fields required to make a request.
--
-- * 'trafficMirrorFilterRule' - Modifies a Traffic Mirror rule.
-- * 'responseStatus' - The response status code.
mkModifyTrafficMirrorFilterRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyTrafficMirrorFilterRuleResponse
mkModifyTrafficMirrorFilterRuleResponse pResponseStatus_ =
  ModifyTrafficMirrorFilterRuleResponse'
    { trafficMirrorFilterRule =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Modifies a Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrrsTrafficMirrorFilterRule :: Lens.Lens' ModifyTrafficMirrorFilterRuleResponse (Lude.Maybe TrafficMirrorFilterRule)
mtmfrrsTrafficMirrorFilterRule = Lens.lens (trafficMirrorFilterRule :: ModifyTrafficMirrorFilterRuleResponse -> Lude.Maybe TrafficMirrorFilterRule) (\s a -> s {trafficMirrorFilterRule = a} :: ModifyTrafficMirrorFilterRuleResponse)
{-# DEPRECATED mtmfrrsTrafficMirrorFilterRule "Use generic-lens or generic-optics with 'trafficMirrorFilterRule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrrsResponseStatus :: Lens.Lens' ModifyTrafficMirrorFilterRuleResponse Lude.Int
mtmfrrsResponseStatus = Lens.lens (responseStatus :: ModifyTrafficMirrorFilterRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyTrafficMirrorFilterRuleResponse)
{-# DEPRECATED mtmfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
