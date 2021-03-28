{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVpcEndpointConnectionNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection notification for a specified VPC endpoint or VPC endpoint service. A connection notification notifies you of specific endpoint events. You must create an SNS topic to receive notifications. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Create a Topic> in the /Amazon Simple Notification Service Developer Guide/ .
--
-- You can create a connection notification for interface endpoints only.
module Network.AWS.EC2.CreateVpcEndpointConnectionNotification
    (
    -- * Creating a request
      CreateVpcEndpointConnectionNotification (..)
    , mkCreateVpcEndpointConnectionNotification
    -- ** Request lenses
    , cvecnConnectionNotificationArn
    , cvecnConnectionEvents
    , cvecnClientToken
    , cvecnDryRun
    , cvecnServiceId
    , cvecnVpcEndpointId

    -- * Destructuring the response
    , CreateVpcEndpointConnectionNotificationResponse (..)
    , mkCreateVpcEndpointConnectionNotificationResponse
    -- ** Response lenses
    , cvecnrrsClientToken
    , cvecnrrsConnectionNotification
    , cvecnrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateVpcEndpointConnectionNotification' smart constructor.
data CreateVpcEndpointConnectionNotification = CreateVpcEndpointConnectionNotification'
  { connectionNotificationArn :: Core.Text
    -- ^ The ARN of the SNS topic for the notifications.
  , connectionEvents :: [Core.Text]
    -- ^ One or more endpoint events for which to receive notifications. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , serviceId :: Core.Maybe Types.ServiceId
    -- ^ The ID of the endpoint service.
  , vpcEndpointId :: Core.Maybe Types.VpcEndpointId
    -- ^ The ID of the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpcEndpointConnectionNotification' value with any optional fields omitted.
mkCreateVpcEndpointConnectionNotification
    :: Core.Text -- ^ 'connectionNotificationArn'
    -> CreateVpcEndpointConnectionNotification
mkCreateVpcEndpointConnectionNotification connectionNotificationArn
  = CreateVpcEndpointConnectionNotification'{connectionNotificationArn,
                                             connectionEvents = Core.mempty,
                                             clientToken = Core.Nothing, dryRun = Core.Nothing,
                                             serviceId = Core.Nothing, vpcEndpointId = Core.Nothing}

-- | The ARN of the SNS topic for the notifications.
--
-- /Note:/ Consider using 'connectionNotificationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnConnectionNotificationArn :: Lens.Lens' CreateVpcEndpointConnectionNotification Core.Text
cvecnConnectionNotificationArn = Lens.field @"connectionNotificationArn"
{-# INLINEABLE cvecnConnectionNotificationArn #-}
{-# DEPRECATED connectionNotificationArn "Use generic-lens or generic-optics with 'connectionNotificationArn' instead"  #-}

-- | One or more endpoint events for which to receive notifications. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
--
-- /Note:/ Consider using 'connectionEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnConnectionEvents :: Lens.Lens' CreateVpcEndpointConnectionNotification [Core.Text]
cvecnConnectionEvents = Lens.field @"connectionEvents"
{-# INLINEABLE cvecnConnectionEvents #-}
{-# DEPRECATED connectionEvents "Use generic-lens or generic-optics with 'connectionEvents' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnClientToken :: Lens.Lens' CreateVpcEndpointConnectionNotification (Core.Maybe Core.Text)
cvecnClientToken = Lens.field @"clientToken"
{-# INLINEABLE cvecnClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnDryRun :: Lens.Lens' CreateVpcEndpointConnectionNotification (Core.Maybe Core.Bool)
cvecnDryRun = Lens.field @"dryRun"
{-# INLINEABLE cvecnDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the endpoint service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnServiceId :: Lens.Lens' CreateVpcEndpointConnectionNotification (Core.Maybe Types.ServiceId)
cvecnServiceId = Lens.field @"serviceId"
{-# INLINEABLE cvecnServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | The ID of the endpoint.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnVpcEndpointId :: Lens.Lens' CreateVpcEndpointConnectionNotification (Core.Maybe Types.VpcEndpointId)
cvecnVpcEndpointId = Lens.field @"vpcEndpointId"
{-# INLINEABLE cvecnVpcEndpointId #-}
{-# DEPRECATED vpcEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead"  #-}

instance Core.ToQuery CreateVpcEndpointConnectionNotification where
        toQuery CreateVpcEndpointConnectionNotification{..}
          = Core.toQueryPair "Action"
              ("CreateVpcEndpointConnectionNotification" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "ConnectionNotificationArn"
                connectionNotificationArn
              Core.<> Core.toQueryList "ConnectionEvents" connectionEvents
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ServiceId") serviceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VpcEndpointId")
                vpcEndpointId

instance Core.ToHeaders CreateVpcEndpointConnectionNotification
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateVpcEndpointConnectionNotification
         where
        type Rs CreateVpcEndpointConnectionNotification =
             CreateVpcEndpointConnectionNotificationResponse
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
                 CreateVpcEndpointConnectionNotificationResponse' Core.<$>
                   (x Core..@? "clientToken") Core.<*>
                     x Core..@? "connectionNotification"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateVpcEndpointConnectionNotificationResponse' smart constructor.
data CreateVpcEndpointConnectionNotificationResponse = CreateVpcEndpointConnectionNotificationResponse'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
  , connectionNotification :: Core.Maybe Types.ConnectionNotification
    -- ^ Information about the notification.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpcEndpointConnectionNotificationResponse' value with any optional fields omitted.
mkCreateVpcEndpointConnectionNotificationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateVpcEndpointConnectionNotificationResponse
mkCreateVpcEndpointConnectionNotificationResponse responseStatus
  = CreateVpcEndpointConnectionNotificationResponse'{clientToken =
                                                       Core.Nothing,
                                                     connectionNotification = Core.Nothing,
                                                     responseStatus}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnrrsClientToken :: Lens.Lens' CreateVpcEndpointConnectionNotificationResponse (Core.Maybe Core.Text)
cvecnrrsClientToken = Lens.field @"clientToken"
{-# INLINEABLE cvecnrrsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Information about the notification.
--
-- /Note:/ Consider using 'connectionNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnrrsConnectionNotification :: Lens.Lens' CreateVpcEndpointConnectionNotificationResponse (Core.Maybe Types.ConnectionNotification)
cvecnrrsConnectionNotification = Lens.field @"connectionNotification"
{-# INLINEABLE cvecnrrsConnectionNotification #-}
{-# DEPRECATED connectionNotification "Use generic-lens or generic-optics with 'connectionNotification' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnrrsResponseStatus :: Lens.Lens' CreateVpcEndpointConnectionNotificationResponse Core.Int
cvecnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cvecnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
