{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTrafficMirrorTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a target for your Traffic Mirror session.
--
-- A Traffic Mirror target is the destination for mirrored traffic. The Traffic Mirror source and the Traffic Mirror target (monitoring appliances) can be in the same VPC, or in different VPCs connected via VPC peering or a transit gateway.
-- A Traffic Mirror target can be a network interface, or a Network Load Balancer.
-- To use the target in a Traffic Mirror session, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTrafficMirrorSession.htm CreateTrafficMirrorSession> .
module Network.AWS.EC2.CreateTrafficMirrorTarget
    (
    -- * Creating a request
      CreateTrafficMirrorTarget (..)
    , mkCreateTrafficMirrorTarget
    -- ** Request lenses
    , ctmtClientToken
    , ctmtDescription
    , ctmtDryRun
    , ctmtNetworkInterfaceId
    , ctmtNetworkLoadBalancerArn
    , ctmtTagSpecifications

    -- * Destructuring the response
    , CreateTrafficMirrorTargetResponse (..)
    , mkCreateTrafficMirrorTargetResponse
    -- ** Response lenses
    , ctmtrrsClientToken
    , ctmtrrsTrafficMirrorTarget
    , ctmtrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTrafficMirrorTarget' smart constructor.
data CreateTrafficMirrorTarget = CreateTrafficMirrorTarget'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , description :: Core.Maybe Core.Text
    -- ^ The description of the Traffic Mirror target.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId
    -- ^ The network interface ID that is associated with the target.
  , networkLoadBalancerArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the target.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to the Traffic Mirror target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficMirrorTarget' value with any optional fields omitted.
mkCreateTrafficMirrorTarget
    :: CreateTrafficMirrorTarget
mkCreateTrafficMirrorTarget
  = CreateTrafficMirrorTarget'{clientToken = Core.Nothing,
                               description = Core.Nothing, dryRun = Core.Nothing,
                               networkInterfaceId = Core.Nothing,
                               networkLoadBalancerArn = Core.Nothing,
                               tagSpecifications = Core.Nothing}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtClientToken :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe Core.Text)
ctmtClientToken = Lens.field @"clientToken"
{-# INLINEABLE ctmtClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The description of the Traffic Mirror target.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtDescription :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe Core.Text)
ctmtDescription = Lens.field @"description"
{-# INLINEABLE ctmtDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtDryRun :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe Core.Bool)
ctmtDryRun = Lens.field @"dryRun"
{-# INLINEABLE ctmtDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The network interface ID that is associated with the target.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtNetworkInterfaceId :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe Types.NetworkInterfaceId)
ctmtNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE ctmtNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the target.
--
-- /Note:/ Consider using 'networkLoadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtNetworkLoadBalancerArn :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe Core.Text)
ctmtNetworkLoadBalancerArn = Lens.field @"networkLoadBalancerArn"
{-# INLINEABLE ctmtNetworkLoadBalancerArn #-}
{-# DEPRECATED networkLoadBalancerArn "Use generic-lens or generic-optics with 'networkLoadBalancerArn' instead"  #-}

-- | The tags to assign to the Traffic Mirror target.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtTagSpecifications :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe [Types.TagSpecification])
ctmtTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ctmtTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateTrafficMirrorTarget where
        toQuery CreateTrafficMirrorTarget{..}
          = Core.toQueryPair "Action"
              ("CreateTrafficMirrorTarget" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NetworkInterfaceId")
                networkInterfaceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NetworkLoadBalancerArn")
                networkLoadBalancerArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateTrafficMirrorTarget where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTrafficMirrorTarget where
        type Rs CreateTrafficMirrorTarget =
             CreateTrafficMirrorTargetResponse
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
                 CreateTrafficMirrorTargetResponse' Core.<$>
                   (x Core..@? "clientToken") Core.<*>
                     x Core..@? "trafficMirrorTarget"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTrafficMirrorTargetResponse' smart constructor.
data CreateTrafficMirrorTargetResponse = CreateTrafficMirrorTargetResponse'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , trafficMirrorTarget :: Core.Maybe Types.TrafficMirrorTarget
    -- ^ Information about the Traffic Mirror target.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficMirrorTargetResponse' value with any optional fields omitted.
mkCreateTrafficMirrorTargetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTrafficMirrorTargetResponse
mkCreateTrafficMirrorTargetResponse responseStatus
  = CreateTrafficMirrorTargetResponse'{clientToken = Core.Nothing,
                                       trafficMirrorTarget = Core.Nothing, responseStatus}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtrrsClientToken :: Lens.Lens' CreateTrafficMirrorTargetResponse (Core.Maybe Core.Text)
ctmtrrsClientToken = Lens.field @"clientToken"
{-# INLINEABLE ctmtrrsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Information about the Traffic Mirror target.
--
-- /Note:/ Consider using 'trafficMirrorTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtrrsTrafficMirrorTarget :: Lens.Lens' CreateTrafficMirrorTargetResponse (Core.Maybe Types.TrafficMirrorTarget)
ctmtrrsTrafficMirrorTarget = Lens.field @"trafficMirrorTarget"
{-# INLINEABLE ctmtrrsTrafficMirrorTarget #-}
{-# DEPRECATED trafficMirrorTarget "Use generic-lens or generic-optics with 'trafficMirrorTarget' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtrrsResponseStatus :: Lens.Lens' CreateTrafficMirrorTargetResponse Core.Int
ctmtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctmtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
