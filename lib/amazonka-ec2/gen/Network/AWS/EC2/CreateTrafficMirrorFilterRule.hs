{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTrafficMirrorFilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Traffic Mirror filter rule.
--
-- A Traffic Mirror rule defines the Traffic Mirror source traffic to mirror.
-- You need the Traffic Mirror filter ID when you create the rule.
module Network.AWS.EC2.CreateTrafficMirrorFilterRule
  ( -- * Creating a request
    CreateTrafficMirrorFilterRule (..),
    mkCreateTrafficMirrorFilterRule,

    -- ** Request lenses
    ctmfrTrafficMirrorFilterId,
    ctmfrTrafficDirection,
    ctmfrRuleNumber,
    ctmfrRuleAction,
    ctmfrDestinationCidrBlock,
    ctmfrSourceCidrBlock,
    ctmfrClientToken,
    ctmfrDescription,
    ctmfrDestinationPortRange,
    ctmfrDryRun,
    ctmfrProtocol,
    ctmfrSourcePortRange,

    -- * Destructuring the response
    CreateTrafficMirrorFilterRuleResponse (..),
    mkCreateTrafficMirrorFilterRuleResponse,

    -- ** Response lenses
    ctmfrrrsClientToken,
    ctmfrrrsTrafficMirrorFilterRule,
    ctmfrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTrafficMirrorFilterRule' smart constructor.
data CreateTrafficMirrorFilterRule = CreateTrafficMirrorFilterRule'
  { -- | The ID of the filter that this rule is associated with.
    trafficMirrorFilterId :: Types.TrafficMirrorFilterId,
    -- | The type of traffic (@ingress@ | @egress@ ).
    trafficDirection :: Types.TrafficDirection,
    -- | The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
    ruleNumber :: Core.Int,
    -- | The action to take (@accept@ | @reject@ ) on the filtered traffic.
    ruleAction :: Types.TrafficMirrorRuleAction,
    -- | The destination CIDR block to assign to the Traffic Mirror rule.
    destinationCidrBlock :: Types.DestinationCidrBlock,
    -- | The source CIDR block to assign to the Traffic Mirror rule.
    sourceCidrBlock :: Types.SourceCidrBlock,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Core.Maybe Types.ClientToken,
    -- | The description of the Traffic Mirror rule.
    description :: Core.Maybe Types.Description,
    -- | The destination port range.
    destinationPortRange :: Core.Maybe Types.TrafficMirrorPortRangeRequest,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The protocol, for example UDP, to assign to the Traffic Mirror rule.
    --
    -- For information about the protocol value, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> on the Internet Assigned Numbers Authority (IANA) website.
    protocol :: Core.Maybe Core.Int,
    -- | The source port range.
    sourcePortRange :: Core.Maybe Types.TrafficMirrorPortRangeRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficMirrorFilterRule' value with any optional fields omitted.
mkCreateTrafficMirrorFilterRule ::
  -- | 'trafficMirrorFilterId'
  Types.TrafficMirrorFilterId ->
  -- | 'trafficDirection'
  Types.TrafficDirection ->
  -- | 'ruleNumber'
  Core.Int ->
  -- | 'ruleAction'
  Types.TrafficMirrorRuleAction ->
  -- | 'destinationCidrBlock'
  Types.DestinationCidrBlock ->
  -- | 'sourceCidrBlock'
  Types.SourceCidrBlock ->
  CreateTrafficMirrorFilterRule
mkCreateTrafficMirrorFilterRule
  trafficMirrorFilterId
  trafficDirection
  ruleNumber
  ruleAction
  destinationCidrBlock
  sourceCidrBlock =
    CreateTrafficMirrorFilterRule'
      { trafficMirrorFilterId,
        trafficDirection,
        ruleNumber,
        ruleAction,
        destinationCidrBlock,
        sourceCidrBlock,
        clientToken = Core.Nothing,
        description = Core.Nothing,
        destinationPortRange = Core.Nothing,
        dryRun = Core.Nothing,
        protocol = Core.Nothing,
        sourcePortRange = Core.Nothing
      }

-- | The ID of the filter that this rule is associated with.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrTrafficMirrorFilterId :: Lens.Lens' CreateTrafficMirrorFilterRule Types.TrafficMirrorFilterId
ctmfrTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# DEPRECATED ctmfrTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The type of traffic (@ingress@ | @egress@ ).
--
-- /Note:/ Consider using 'trafficDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrTrafficDirection :: Lens.Lens' CreateTrafficMirrorFilterRule Types.TrafficDirection
ctmfrTrafficDirection = Lens.field @"trafficDirection"
{-# DEPRECATED ctmfrTrafficDirection "Use generic-lens or generic-optics with 'trafficDirection' instead." #-}

-- | The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrRuleNumber :: Lens.Lens' CreateTrafficMirrorFilterRule Core.Int
ctmfrRuleNumber = Lens.field @"ruleNumber"
{-# DEPRECATED ctmfrRuleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead." #-}

-- | The action to take (@accept@ | @reject@ ) on the filtered traffic.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrRuleAction :: Lens.Lens' CreateTrafficMirrorFilterRule Types.TrafficMirrorRuleAction
ctmfrRuleAction = Lens.field @"ruleAction"
{-# DEPRECATED ctmfrRuleAction "Use generic-lens or generic-optics with 'ruleAction' instead." #-}

-- | The destination CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDestinationCidrBlock :: Lens.Lens' CreateTrafficMirrorFilterRule Types.DestinationCidrBlock
ctmfrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED ctmfrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The source CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourceCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrSourceCidrBlock :: Lens.Lens' CreateTrafficMirrorFilterRule Types.SourceCidrBlock
ctmfrSourceCidrBlock = Lens.field @"sourceCidrBlock"
{-# DEPRECATED ctmfrSourceCidrBlock "Use generic-lens or generic-optics with 'sourceCidrBlock' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrClientToken :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Types.ClientToken)
ctmfrClientToken = Lens.field @"clientToken"
{-# DEPRECATED ctmfrClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The description of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDescription :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Types.Description)
ctmfrDescription = Lens.field @"description"
{-# DEPRECATED ctmfrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The destination port range.
--
-- /Note:/ Consider using 'destinationPortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDestinationPortRange :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRangeRequest)
ctmfrDestinationPortRange = Lens.field @"destinationPortRange"
{-# DEPRECATED ctmfrDestinationPortRange "Use generic-lens or generic-optics with 'destinationPortRange' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDryRun :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Core.Bool)
ctmfrDryRun = Lens.field @"dryRun"
{-# DEPRECATED ctmfrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The protocol, for example UDP, to assign to the Traffic Mirror rule.
--
-- For information about the protocol value, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> on the Internet Assigned Numbers Authority (IANA) website.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrProtocol :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Core.Int)
ctmfrProtocol = Lens.field @"protocol"
{-# DEPRECATED ctmfrProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The source port range.
--
-- /Note:/ Consider using 'sourcePortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrSourcePortRange :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRangeRequest)
ctmfrSourcePortRange = Lens.field @"sourcePortRange"
{-# DEPRECATED ctmfrSourcePortRange "Use generic-lens or generic-optics with 'sourcePortRange' instead." #-}

instance Core.AWSRequest CreateTrafficMirrorFilterRule where
  type
    Rs CreateTrafficMirrorFilterRule =
      CreateTrafficMirrorFilterRuleResponse
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
            ( Core.pure ("Action", "CreateTrafficMirrorFilterRule")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "TrafficMirrorFilterId" trafficMirrorFilterId)
                Core.<> (Core.toQueryValue "TrafficDirection" trafficDirection)
                Core.<> (Core.toQueryValue "RuleNumber" ruleNumber)
                Core.<> (Core.toQueryValue "RuleAction" ruleAction)
                Core.<> (Core.toQueryValue "DestinationCidrBlock" destinationCidrBlock)
                Core.<> (Core.toQueryValue "SourceCidrBlock" sourceCidrBlock)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> ( Core.toQueryValue "DestinationPortRange"
                            Core.<$> destinationPortRange
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Protocol" Core.<$> protocol)
                Core.<> (Core.toQueryValue "SourcePortRange" Core.<$> sourcePortRange)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficMirrorFilterRuleResponse'
            Core.<$> (x Core..@? "clientToken")
            Core.<*> (x Core..@? "trafficMirrorFilterRule")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTrafficMirrorFilterRuleResponse' smart constructor.
data CreateTrafficMirrorFilterRuleResponse = CreateTrafficMirrorFilterRuleResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | The Traffic Mirror rule.
    trafficMirrorFilterRule :: Core.Maybe Types.TrafficMirrorFilterRule,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficMirrorFilterRuleResponse' value with any optional fields omitted.
mkCreateTrafficMirrorFilterRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTrafficMirrorFilterRuleResponse
mkCreateTrafficMirrorFilterRuleResponse responseStatus =
  CreateTrafficMirrorFilterRuleResponse'
    { clientToken =
        Core.Nothing,
      trafficMirrorFilterRule = Core.Nothing,
      responseStatus
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrrsClientToken :: Lens.Lens' CreateTrafficMirrorFilterRuleResponse (Core.Maybe Types.String)
ctmfrrrsClientToken = Lens.field @"clientToken"
{-# DEPRECATED ctmfrrrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrrsTrafficMirrorFilterRule :: Lens.Lens' CreateTrafficMirrorFilterRuleResponse (Core.Maybe Types.TrafficMirrorFilterRule)
ctmfrrrsTrafficMirrorFilterRule = Lens.field @"trafficMirrorFilterRule"
{-# DEPRECATED ctmfrrrsTrafficMirrorFilterRule "Use generic-lens or generic-optics with 'trafficMirrorFilterRule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrrsResponseStatus :: Lens.Lens' CreateTrafficMirrorFilterRuleResponse Core.Int
ctmfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctmfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
