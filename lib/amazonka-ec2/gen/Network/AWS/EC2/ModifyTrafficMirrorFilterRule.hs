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
    mtmfrTrafficMirrorFilterRuleId,
    mtmfrDescription,
    mtmfrDestinationCidrBlock,
    mtmfrDestinationPortRange,
    mtmfrDryRun,
    mtmfrProtocol,
    mtmfrRemoveFields,
    mtmfrRuleAction,
    mtmfrRuleNumber,
    mtmfrSourceCidrBlock,
    mtmfrSourcePortRange,
    mtmfrTrafficDirection,

    -- * Destructuring the response
    ModifyTrafficMirrorFilterRuleResponse (..),
    mkModifyTrafficMirrorFilterRuleResponse,

    -- ** Response lenses
    mtmfrrrsTrafficMirrorFilterRule,
    mtmfrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTrafficMirrorFilterRule' smart constructor.
data ModifyTrafficMirrorFilterRule = ModifyTrafficMirrorFilterRule'
  { -- | The ID of the Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Types.TrafficMirrorFilterRuleId,
    -- | The description to assign to the Traffic Mirror rule.
    description :: Core.Maybe Types.String,
    -- | The destination CIDR block to assign to the Traffic Mirror rule.
    destinationCidrBlock :: Core.Maybe Types.String,
    -- | The destination ports that are associated with the Traffic Mirror rule.
    destinationPortRange :: Core.Maybe Types.TrafficMirrorPortRangeRequest,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The protocol, for example TCP, to assign to the Traffic Mirror rule.
    protocol :: Core.Maybe Core.Int,
    -- | The properties that you want to remove from the Traffic Mirror filter rule.
    --
    -- When you remove a property from a Traffic Mirror filter rule, the property is set to the default.
    removeFields :: Core.Maybe [Types.TrafficMirrorFilterRuleField],
    -- | The action to assign to the rule.
    ruleAction :: Core.Maybe Types.TrafficMirrorRuleAction,
    -- | The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
    ruleNumber :: Core.Maybe Core.Int,
    -- | The source CIDR block to assign to the Traffic Mirror rule.
    sourceCidrBlock :: Core.Maybe Types.String,
    -- | The port range to assign to the Traffic Mirror rule.
    sourcePortRange :: Core.Maybe Types.TrafficMirrorPortRangeRequest,
    -- | The type of traffic (@ingress@ | @egress@ ) to assign to the rule.
    trafficDirection :: Core.Maybe Types.TrafficDirection
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorFilterRule' value with any optional fields omitted.
mkModifyTrafficMirrorFilterRule ::
  -- | 'trafficMirrorFilterRuleId'
  Types.TrafficMirrorFilterRuleId ->
  ModifyTrafficMirrorFilterRule
mkModifyTrafficMirrorFilterRule trafficMirrorFilterRuleId =
  ModifyTrafficMirrorFilterRule'
    { trafficMirrorFilterRuleId,
      description = Core.Nothing,
      destinationCidrBlock = Core.Nothing,
      destinationPortRange = Core.Nothing,
      dryRun = Core.Nothing,
      protocol = Core.Nothing,
      removeFields = Core.Nothing,
      ruleAction = Core.Nothing,
      ruleNumber = Core.Nothing,
      sourceCidrBlock = Core.Nothing,
      sourcePortRange = Core.Nothing,
      trafficDirection = Core.Nothing
    }

-- | The ID of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrTrafficMirrorFilterRuleId :: Lens.Lens' ModifyTrafficMirrorFilterRule Types.TrafficMirrorFilterRuleId
mtmfrTrafficMirrorFilterRuleId = Lens.field @"trafficMirrorFilterRuleId"
{-# DEPRECATED mtmfrTrafficMirrorFilterRuleId "Use generic-lens or generic-optics with 'trafficMirrorFilterRuleId' instead." #-}

-- | The description to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDescription :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Types.String)
mtmfrDescription = Lens.field @"description"
{-# DEPRECATED mtmfrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The destination CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDestinationCidrBlock :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Types.String)
mtmfrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED mtmfrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The destination ports that are associated with the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationPortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDestinationPortRange :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRangeRequest)
mtmfrDestinationPortRange = Lens.field @"destinationPortRange"
{-# DEPRECATED mtmfrDestinationPortRange "Use generic-lens or generic-optics with 'destinationPortRange' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDryRun :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Bool)
mtmfrDryRun = Lens.field @"dryRun"
{-# DEPRECATED mtmfrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The protocol, for example TCP, to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrProtocol :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Int)
mtmfrProtocol = Lens.field @"protocol"
{-# DEPRECATED mtmfrProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The properties that you want to remove from the Traffic Mirror filter rule.
--
-- When you remove a property from a Traffic Mirror filter rule, the property is set to the default.
--
-- /Note:/ Consider using 'removeFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrRemoveFields :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe [Types.TrafficMirrorFilterRuleField])
mtmfrRemoveFields = Lens.field @"removeFields"
{-# DEPRECATED mtmfrRemoveFields "Use generic-lens or generic-optics with 'removeFields' instead." #-}

-- | The action to assign to the rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrRuleAction :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorRuleAction)
mtmfrRuleAction = Lens.field @"ruleAction"
{-# DEPRECATED mtmfrRuleAction "Use generic-lens or generic-optics with 'ruleAction' instead." #-}

-- | The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrRuleNumber :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Int)
mtmfrRuleNumber = Lens.field @"ruleNumber"
{-# DEPRECATED mtmfrRuleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead." #-}

-- | The source CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourceCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrSourceCidrBlock :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Types.String)
mtmfrSourceCidrBlock = Lens.field @"sourceCidrBlock"
{-# DEPRECATED mtmfrSourceCidrBlock "Use generic-lens or generic-optics with 'sourceCidrBlock' instead." #-}

-- | The port range to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourcePortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrSourcePortRange :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRangeRequest)
mtmfrSourcePortRange = Lens.field @"sourcePortRange"
{-# DEPRECATED mtmfrSourcePortRange "Use generic-lens or generic-optics with 'sourcePortRange' instead." #-}

-- | The type of traffic (@ingress@ | @egress@ ) to assign to the rule.
--
-- /Note:/ Consider using 'trafficDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrTrafficDirection :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Types.TrafficDirection)
mtmfrTrafficDirection = Lens.field @"trafficDirection"
{-# DEPRECATED mtmfrTrafficDirection "Use generic-lens or generic-optics with 'trafficDirection' instead." #-}

instance Core.AWSRequest ModifyTrafficMirrorFilterRule where
  type
    Rs ModifyTrafficMirrorFilterRule =
      ModifyTrafficMirrorFilterRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyTrafficMirrorFilterRule")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "TrafficMirrorFilterRuleId"
                            trafficMirrorFilterRuleId
                        )
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> ( Core.toQueryValue "DestinationCidrBlock"
                            Core.<$> destinationCidrBlock
                        )
                Core.<> ( Core.toQueryValue "DestinationPortRange"
                            Core.<$> destinationPortRange
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Protocol" Core.<$> protocol)
                Core.<> (Core.toQueryList "RemoveField" Core.<$> removeFields)
                Core.<> (Core.toQueryValue "RuleAction" Core.<$> ruleAction)
                Core.<> (Core.toQueryValue "RuleNumber" Core.<$> ruleNumber)
                Core.<> (Core.toQueryValue "SourceCidrBlock" Core.<$> sourceCidrBlock)
                Core.<> (Core.toQueryValue "SourcePortRange" Core.<$> sourcePortRange)
                Core.<> (Core.toQueryValue "TrafficDirection" Core.<$> trafficDirection)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTrafficMirrorFilterRuleResponse'
            Core.<$> (x Core..@? "trafficMirrorFilterRule")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyTrafficMirrorFilterRuleResponse' smart constructor.
data ModifyTrafficMirrorFilterRuleResponse = ModifyTrafficMirrorFilterRuleResponse'
  { -- | Modifies a Traffic Mirror rule.
    trafficMirrorFilterRule :: Core.Maybe Types.TrafficMirrorFilterRule,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorFilterRuleResponse' value with any optional fields omitted.
mkModifyTrafficMirrorFilterRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyTrafficMirrorFilterRuleResponse
mkModifyTrafficMirrorFilterRuleResponse responseStatus =
  ModifyTrafficMirrorFilterRuleResponse'
    { trafficMirrorFilterRule =
        Core.Nothing,
      responseStatus
    }

-- | Modifies a Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrrrsTrafficMirrorFilterRule :: Lens.Lens' ModifyTrafficMirrorFilterRuleResponse (Core.Maybe Types.TrafficMirrorFilterRule)
mtmfrrrsTrafficMirrorFilterRule = Lens.field @"trafficMirrorFilterRule"
{-# DEPRECATED mtmfrrrsTrafficMirrorFilterRule "Use generic-lens or generic-optics with 'trafficMirrorFilterRule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrrrsResponseStatus :: Lens.Lens' ModifyTrafficMirrorFilterRuleResponse Core.Int
mtmfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mtmfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
