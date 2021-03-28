{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyTrafficMirrorFilterRule (..)
    , mkModifyTrafficMirrorFilterRule
    -- ** Request lenses
    , mtmfrTrafficMirrorFilterRuleId
    , mtmfrDescription
    , mtmfrDestinationCidrBlock
    , mtmfrDestinationPortRange
    , mtmfrDryRun
    , mtmfrProtocol
    , mtmfrRemoveFields
    , mtmfrRuleAction
    , mtmfrRuleNumber
    , mtmfrSourceCidrBlock
    , mtmfrSourcePortRange
    , mtmfrTrafficDirection

    -- * Destructuring the response
    , ModifyTrafficMirrorFilterRuleResponse (..)
    , mkModifyTrafficMirrorFilterRuleResponse
    -- ** Response lenses
    , mtmfrrrsTrafficMirrorFilterRule
    , mtmfrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTrafficMirrorFilterRule' smart constructor.
data ModifyTrafficMirrorFilterRule = ModifyTrafficMirrorFilterRule'
  { trafficMirrorFilterRuleId :: Types.TrafficMirrorFilterRuleId
    -- ^ The ID of the Traffic Mirror rule.
  , description :: Core.Maybe Core.Text
    -- ^ The description to assign to the Traffic Mirror rule.
  , destinationCidrBlock :: Core.Maybe Core.Text
    -- ^ The destination CIDR block to assign to the Traffic Mirror rule.
  , destinationPortRange :: Core.Maybe Types.TrafficMirrorPortRangeRequest
    -- ^ The destination ports that are associated with the Traffic Mirror rule.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , protocol :: Core.Maybe Core.Int
    -- ^ The protocol, for example TCP, to assign to the Traffic Mirror rule.
  , removeFields :: Core.Maybe [Types.TrafficMirrorFilterRuleField]
    -- ^ The properties that you want to remove from the Traffic Mirror filter rule.
--
-- When you remove a property from a Traffic Mirror filter rule, the property is set to the default.
  , ruleAction :: Core.Maybe Types.TrafficMirrorRuleAction
    -- ^ The action to assign to the rule.
  , ruleNumber :: Core.Maybe Core.Int
    -- ^ The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
  , sourceCidrBlock :: Core.Maybe Core.Text
    -- ^ The source CIDR block to assign to the Traffic Mirror rule.
  , sourcePortRange :: Core.Maybe Types.TrafficMirrorPortRangeRequest
    -- ^ The port range to assign to the Traffic Mirror rule.
  , trafficDirection :: Core.Maybe Types.TrafficDirection
    -- ^ The type of traffic (@ingress@ | @egress@ ) to assign to the rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorFilterRule' value with any optional fields omitted.
mkModifyTrafficMirrorFilterRule
    :: Types.TrafficMirrorFilterRuleId -- ^ 'trafficMirrorFilterRuleId'
    -> ModifyTrafficMirrorFilterRule
mkModifyTrafficMirrorFilterRule trafficMirrorFilterRuleId
  = ModifyTrafficMirrorFilterRule'{trafficMirrorFilterRuleId,
                                   description = Core.Nothing, destinationCidrBlock = Core.Nothing,
                                   destinationPortRange = Core.Nothing, dryRun = Core.Nothing,
                                   protocol = Core.Nothing, removeFields = Core.Nothing,
                                   ruleAction = Core.Nothing, ruleNumber = Core.Nothing,
                                   sourceCidrBlock = Core.Nothing, sourcePortRange = Core.Nothing,
                                   trafficDirection = Core.Nothing}

-- | The ID of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrTrafficMirrorFilterRuleId :: Lens.Lens' ModifyTrafficMirrorFilterRule Types.TrafficMirrorFilterRuleId
mtmfrTrafficMirrorFilterRuleId = Lens.field @"trafficMirrorFilterRuleId"
{-# INLINEABLE mtmfrTrafficMirrorFilterRuleId #-}
{-# DEPRECATED trafficMirrorFilterRuleId "Use generic-lens or generic-optics with 'trafficMirrorFilterRuleId' instead"  #-}

-- | The description to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDescription :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Text)
mtmfrDescription = Lens.field @"description"
{-# INLINEABLE mtmfrDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The destination CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDestinationCidrBlock :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Text)
mtmfrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE mtmfrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The destination ports that are associated with the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationPortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDestinationPortRange :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRangeRequest)
mtmfrDestinationPortRange = Lens.field @"destinationPortRange"
{-# INLINEABLE mtmfrDestinationPortRange #-}
{-# DEPRECATED destinationPortRange "Use generic-lens or generic-optics with 'destinationPortRange' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrDryRun :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Bool)
mtmfrDryRun = Lens.field @"dryRun"
{-# INLINEABLE mtmfrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The protocol, for example TCP, to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrProtocol :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Int)
mtmfrProtocol = Lens.field @"protocol"
{-# INLINEABLE mtmfrProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The properties that you want to remove from the Traffic Mirror filter rule.
--
-- When you remove a property from a Traffic Mirror filter rule, the property is set to the default.
--
-- /Note:/ Consider using 'removeFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrRemoveFields :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe [Types.TrafficMirrorFilterRuleField])
mtmfrRemoveFields = Lens.field @"removeFields"
{-# INLINEABLE mtmfrRemoveFields #-}
{-# DEPRECATED removeFields "Use generic-lens or generic-optics with 'removeFields' instead"  #-}

-- | The action to assign to the rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrRuleAction :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorRuleAction)
mtmfrRuleAction = Lens.field @"ruleAction"
{-# INLINEABLE mtmfrRuleAction #-}
{-# DEPRECATED ruleAction "Use generic-lens or generic-optics with 'ruleAction' instead"  #-}

-- | The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrRuleNumber :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Int)
mtmfrRuleNumber = Lens.field @"ruleNumber"
{-# INLINEABLE mtmfrRuleNumber #-}
{-# DEPRECATED ruleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead"  #-}

-- | The source CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourceCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrSourceCidrBlock :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Text)
mtmfrSourceCidrBlock = Lens.field @"sourceCidrBlock"
{-# INLINEABLE mtmfrSourceCidrBlock #-}
{-# DEPRECATED sourceCidrBlock "Use generic-lens or generic-optics with 'sourceCidrBlock' instead"  #-}

-- | The port range to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourcePortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrSourcePortRange :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Types.TrafficMirrorPortRangeRequest)
mtmfrSourcePortRange = Lens.field @"sourcePortRange"
{-# INLINEABLE mtmfrSourcePortRange #-}
{-# DEPRECATED sourcePortRange "Use generic-lens or generic-optics with 'sourcePortRange' instead"  #-}

-- | The type of traffic (@ingress@ | @egress@ ) to assign to the rule.
--
-- /Note:/ Consider using 'trafficDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrTrafficDirection :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Types.TrafficDirection)
mtmfrTrafficDirection = Lens.field @"trafficDirection"
{-# INLINEABLE mtmfrTrafficDirection #-}
{-# DEPRECATED trafficDirection "Use generic-lens or generic-optics with 'trafficDirection' instead"  #-}

instance Core.ToQuery ModifyTrafficMirrorFilterRule where
        toQuery ModifyTrafficMirrorFilterRule{..}
          = Core.toQueryPair "Action"
              ("ModifyTrafficMirrorFilterRule" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TrafficMirrorFilterRuleId"
                trafficMirrorFilterRuleId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DestinationCidrBlock")
                destinationCidrBlock
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DestinationPortRange")
                destinationPortRange
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Protocol") protocol
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "RemoveField")
                removeFields
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RuleAction") ruleAction
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RuleNumber") ruleNumber
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceCidrBlock")
                sourceCidrBlock
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourcePortRange")
                sourcePortRange
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TrafficDirection")
                trafficDirection

instance Core.ToHeaders ModifyTrafficMirrorFilterRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyTrafficMirrorFilterRule where
        type Rs ModifyTrafficMirrorFilterRule =
             ModifyTrafficMirrorFilterRuleResponse
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
                 ModifyTrafficMirrorFilterRuleResponse' Core.<$>
                   (x Core..@? "trafficMirrorFilterRule") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyTrafficMirrorFilterRuleResponse' smart constructor.
data ModifyTrafficMirrorFilterRuleResponse = ModifyTrafficMirrorFilterRuleResponse'
  { trafficMirrorFilterRule :: Core.Maybe Types.TrafficMirrorFilterRule
    -- ^ Modifies a Traffic Mirror rule.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorFilterRuleResponse' value with any optional fields omitted.
mkModifyTrafficMirrorFilterRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyTrafficMirrorFilterRuleResponse
mkModifyTrafficMirrorFilterRuleResponse responseStatus
  = ModifyTrafficMirrorFilterRuleResponse'{trafficMirrorFilterRule =
                                             Core.Nothing,
                                           responseStatus}

-- | Modifies a Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrrrsTrafficMirrorFilterRule :: Lens.Lens' ModifyTrafficMirrorFilterRuleResponse (Core.Maybe Types.TrafficMirrorFilterRule)
mtmfrrrsTrafficMirrorFilterRule = Lens.field @"trafficMirrorFilterRule"
{-# INLINEABLE mtmfrrrsTrafficMirrorFilterRule #-}
{-# DEPRECATED trafficMirrorFilterRule "Use generic-lens or generic-optics with 'trafficMirrorFilterRule' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmfrrrsResponseStatus :: Lens.Lens' ModifyTrafficMirrorFilterRuleResponse Core.Int
mtmfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mtmfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
