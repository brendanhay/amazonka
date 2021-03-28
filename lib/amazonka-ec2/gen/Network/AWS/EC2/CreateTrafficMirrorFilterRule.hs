{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateTrafficMirrorFilterRule (..)
    , mkCreateTrafficMirrorFilterRule
    -- ** Request lenses
    , ctmfrTrafficMirrorFilterId
    , ctmfrTrafficDirection
    , ctmfrRuleNumber
    , ctmfrRuleAction
    , ctmfrDestinationCidrBlock
    , ctmfrSourceCidrBlock
    , ctmfrClientToken
    , ctmfrDescription
    , ctmfrDestinationPortRange
    , ctmfrDryRun
    , ctmfrProtocol
    , ctmfrSourcePortRange

    -- * Destructuring the response
    , CreateTrafficMirrorFilterRuleResponse (..)
    , mkCreateTrafficMirrorFilterRuleResponse
    -- ** Response lenses
    , ctmfrrrsClientToken
    , ctmfrrrsTrafficMirrorFilterRule
    , ctmfrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTrafficMirrorFilterRule' smart constructor.
data CreateTrafficMirrorFilterRule = CreateTrafficMirrorFilterRule'
  { trafficMirrorFilterId :: Types.TrafficMirrorFilterId
    -- ^ The ID of the filter that this rule is associated with.
  , trafficDirection :: Types.TrafficDirection
    -- ^ The type of traffic (@ingress@ | @egress@ ).
  , ruleNumber :: Core.Int
    -- ^ The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
  , ruleAction :: Types.TrafficMirrorRuleAction
    -- ^ The action to take (@accept@ | @reject@ ) on the filtered traffic.
  , destinationCidrBlock :: Core.Text
    -- ^ The destination CIDR block to assign to the Traffic Mirror rule.
  , sourceCidrBlock :: Core.Text
    -- ^ The source CIDR block to assign to the Traffic Mirror rule.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , description :: Core.Maybe Core.Text
    -- ^ The description of the Traffic Mirror rule.
  , destinationPortRange :: Core.Maybe Types.TrafficMirrorPortRangeRequest
    -- ^ The destination port range.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , protocol :: Core.Maybe Core.Int
    -- ^ The protocol, for example UDP, to assign to the Traffic Mirror rule.
--
-- For information about the protocol value, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> on the Internet Assigned Numbers Authority (IANA) website.
  , sourcePortRange :: Core.Maybe Types.TrafficMirrorPortRangeRequest
    -- ^ The source port range.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficMirrorFilterRule' value with any optional fields omitted.
mkCreateTrafficMirrorFilterRule
    :: Types.TrafficMirrorFilterId -- ^ 'trafficMirrorFilterId'
    -> Types.TrafficDirection -- ^ 'trafficDirection'
    -> Core.Int -- ^ 'ruleNumber'
    -> Types.TrafficMirrorRuleAction -- ^ 'ruleAction'
    -> Core.Text -- ^ 'destinationCidrBlock'
    -> Core.Text -- ^ 'sourceCidrBlock'
    -> CreateTrafficMirrorFilterRule
mkCreateTrafficMirrorFilterRule trafficMirrorFilterId
  trafficDirection ruleNumber ruleAction destinationCidrBlock
  sourceCidrBlock
  = CreateTrafficMirrorFilterRule'{trafficMirrorFilterId,
                                   trafficDirection, ruleNumber, ruleAction, destinationCidrBlock,
                                   sourceCidrBlock, clientToken = Core.Nothing,
                                   description = Core.Nothing, destinationPortRange = Core.Nothing,
                                   dryRun = Core.Nothing, protocol = Core.Nothing,
                                   sourcePortRange = Core.Nothing}

-- | The ID of the filter that this rule is associated with.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrTrafficMirrorFilterId :: Lens.Lens' CreateTrafficMirrorFilterRule Types.TrafficMirrorFilterId
ctmfrTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# INLINEABLE ctmfrTrafficMirrorFilterId #-}
{-# DEPRECATED trafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead"  #-}

-- | The type of traffic (@ingress@ | @egress@ ).
--
-- /Note:/ Consider using 'trafficDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrTrafficDirection :: Lens.Lens' CreateTrafficMirrorFilterRule Types.TrafficDirection
ctmfrTrafficDirection = Lens.field @"trafficDirection"
{-# INLINEABLE ctmfrTrafficDirection #-}
{-# DEPRECATED trafficDirection "Use generic-lens or generic-optics with 'trafficDirection' instead"  #-}

-- | The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrRuleNumber :: Lens.Lens' CreateTrafficMirrorFilterRule Core.Int
ctmfrRuleNumber = Lens.field @"ruleNumber"
{-# INLINEABLE ctmfrRuleNumber #-}
{-# DEPRECATED ruleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead"  #-}

-- | The action to take (@accept@ | @reject@ ) on the filtered traffic.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrRuleAction :: Lens.Lens' CreateTrafficMirrorFilterRule Types.TrafficMirrorRuleAction
ctmfrRuleAction = Lens.field @"ruleAction"
{-# INLINEABLE ctmfrRuleAction #-}
{-# DEPRECATED ruleAction "Use generic-lens or generic-optics with 'ruleAction' instead"  #-}

-- | The destination CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDestinationCidrBlock :: Lens.Lens' CreateTrafficMirrorFilterRule Core.Text
ctmfrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE ctmfrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The source CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourceCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrSourceCidrBlock :: Lens.Lens' CreateTrafficMirrorFilterRule Core.Text
ctmfrSourceCidrBlock = Lens.field @"sourceCidrBlock"
{-# INLINEABLE ctmfrSourceCidrBlock #-}
{-# DEPRECATED sourceCidrBlock "Use generic-lens or generic-optics with 'sourceCidrBlock' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrClientToken :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Core.Text)
ctmfrClientToken = Lens.field @"clientToken"
{-# INLINEABLE ctmfrClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The description of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDescription :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Core.Text)
ctmfrDescription = Lens.field @"description"
{-# INLINEABLE ctmfrDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The destination port range.
--
-- /Note:/ Consider using 'destinationPortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDestinationPortRange :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRangeRequest)
ctmfrDestinationPortRange = Lens.field @"destinationPortRange"
{-# INLINEABLE ctmfrDestinationPortRange #-}
{-# DEPRECATED destinationPortRange "Use generic-lens or generic-optics with 'destinationPortRange' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDryRun :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Core.Bool)
ctmfrDryRun = Lens.field @"dryRun"
{-# INLINEABLE ctmfrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The protocol, for example UDP, to assign to the Traffic Mirror rule.
--
-- For information about the protocol value, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> on the Internet Assigned Numbers Authority (IANA) website.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrProtocol :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Core.Int)
ctmfrProtocol = Lens.field @"protocol"
{-# INLINEABLE ctmfrProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The source port range.
--
-- /Note:/ Consider using 'sourcePortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrSourcePortRange :: Lens.Lens' CreateTrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRangeRequest)
ctmfrSourcePortRange = Lens.field @"sourcePortRange"
{-# INLINEABLE ctmfrSourcePortRange #-}
{-# DEPRECATED sourcePortRange "Use generic-lens or generic-optics with 'sourcePortRange' instead"  #-}

instance Core.ToQuery CreateTrafficMirrorFilterRule where
        toQuery CreateTrafficMirrorFilterRule{..}
          = Core.toQueryPair "Action"
              ("CreateTrafficMirrorFilterRule" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TrafficMirrorFilterId" trafficMirrorFilterId
              Core.<> Core.toQueryPair "TrafficDirection" trafficDirection
              Core.<> Core.toQueryPair "RuleNumber" ruleNumber
              Core.<> Core.toQueryPair "RuleAction" ruleAction
              Core.<>
              Core.toQueryPair "DestinationCidrBlock" destinationCidrBlock
              Core.<> Core.toQueryPair "SourceCidrBlock" sourceCidrBlock
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DestinationPortRange")
                destinationPortRange
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Protocol") protocol
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourcePortRange")
                sourcePortRange

instance Core.ToHeaders CreateTrafficMirrorFilterRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTrafficMirrorFilterRule where
        type Rs CreateTrafficMirrorFilterRule =
             CreateTrafficMirrorFilterRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateTrafficMirrorFilterRuleResponse' Core.<$>
                   (x Core..@? "clientToken") Core.<*>
                     x Core..@? "trafficMirrorFilterRule"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTrafficMirrorFilterRuleResponse' smart constructor.
data CreateTrafficMirrorFilterRuleResponse = CreateTrafficMirrorFilterRuleResponse'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , trafficMirrorFilterRule :: Core.Maybe Types.TrafficMirrorFilterRule
    -- ^ The Traffic Mirror rule.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficMirrorFilterRuleResponse' value with any optional fields omitted.
mkCreateTrafficMirrorFilterRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTrafficMirrorFilterRuleResponse
mkCreateTrafficMirrorFilterRuleResponse responseStatus
  = CreateTrafficMirrorFilterRuleResponse'{clientToken =
                                             Core.Nothing,
                                           trafficMirrorFilterRule = Core.Nothing, responseStatus}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrrsClientToken :: Lens.Lens' CreateTrafficMirrorFilterRuleResponse (Core.Maybe Core.Text)
ctmfrrrsClientToken = Lens.field @"clientToken"
{-# INLINEABLE ctmfrrrsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrrsTrafficMirrorFilterRule :: Lens.Lens' CreateTrafficMirrorFilterRuleResponse (Core.Maybe Types.TrafficMirrorFilterRule)
ctmfrrrsTrafficMirrorFilterRule = Lens.field @"trafficMirrorFilterRule"
{-# INLINEABLE ctmfrrrsTrafficMirrorFilterRule #-}
{-# DEPRECATED trafficMirrorFilterRule "Use generic-lens or generic-optics with 'trafficMirrorFilterRule' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrrsResponseStatus :: Lens.Lens' CreateTrafficMirrorFilterRuleResponse Core.Int
ctmfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctmfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
