{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVpcEndpointConnectionNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a connection notification for VPC endpoint or VPC endpoint service. You can change the SNS topic for the notification, or the events for which to be notified. 
module Network.AWS.EC2.ModifyVpcEndpointConnectionNotification
    (
    -- * Creating a request
      ModifyVpcEndpointConnectionNotification (..)
    , mkModifyVpcEndpointConnectionNotification
    -- ** Request lenses
    , mvecnConnectionNotificationId
    , mvecnConnectionEvents
    , mvecnConnectionNotificationArn
    , mvecnDryRun

    -- * Destructuring the response
    , ModifyVpcEndpointConnectionNotificationResponse (..)
    , mkModifyVpcEndpointConnectionNotificationResponse
    -- ** Response lenses
    , mvecnrrsReturnValue
    , mvecnrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVpcEndpointConnectionNotification' smart constructor.
data ModifyVpcEndpointConnectionNotification = ModifyVpcEndpointConnectionNotification'
  { connectionNotificationId :: Types.ConnectionNotificationId
    -- ^ The ID of the notification.
  , connectionEvents :: Core.Maybe [Core.Text]
    -- ^ One or more events for the endpoint. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
  , connectionNotificationArn :: Core.Maybe Core.Text
    -- ^ The ARN for the SNS topic for the notification.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcEndpointConnectionNotification' value with any optional fields omitted.
mkModifyVpcEndpointConnectionNotification
    :: Types.ConnectionNotificationId -- ^ 'connectionNotificationId'
    -> ModifyVpcEndpointConnectionNotification
mkModifyVpcEndpointConnectionNotification connectionNotificationId
  = ModifyVpcEndpointConnectionNotification'{connectionNotificationId,
                                             connectionEvents = Core.Nothing,
                                             connectionNotificationArn = Core.Nothing,
                                             dryRun = Core.Nothing}

-- | The ID of the notification.
--
-- /Note:/ Consider using 'connectionNotificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnConnectionNotificationId :: Lens.Lens' ModifyVpcEndpointConnectionNotification Types.ConnectionNotificationId
mvecnConnectionNotificationId = Lens.field @"connectionNotificationId"
{-# INLINEABLE mvecnConnectionNotificationId #-}
{-# DEPRECATED connectionNotificationId "Use generic-lens or generic-optics with 'connectionNotificationId' instead"  #-}

-- | One or more events for the endpoint. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
--
-- /Note:/ Consider using 'connectionEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnConnectionEvents :: Lens.Lens' ModifyVpcEndpointConnectionNotification (Core.Maybe [Core.Text])
mvecnConnectionEvents = Lens.field @"connectionEvents"
{-# INLINEABLE mvecnConnectionEvents #-}
{-# DEPRECATED connectionEvents "Use generic-lens or generic-optics with 'connectionEvents' instead"  #-}

-- | The ARN for the SNS topic for the notification.
--
-- /Note:/ Consider using 'connectionNotificationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnConnectionNotificationArn :: Lens.Lens' ModifyVpcEndpointConnectionNotification (Core.Maybe Core.Text)
mvecnConnectionNotificationArn = Lens.field @"connectionNotificationArn"
{-# INLINEABLE mvecnConnectionNotificationArn #-}
{-# DEPRECATED connectionNotificationArn "Use generic-lens or generic-optics with 'connectionNotificationArn' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnDryRun :: Lens.Lens' ModifyVpcEndpointConnectionNotification (Core.Maybe Core.Bool)
mvecnDryRun = Lens.field @"dryRun"
{-# INLINEABLE mvecnDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ModifyVpcEndpointConnectionNotification where
        toQuery ModifyVpcEndpointConnectionNotification{..}
          = Core.toQueryPair "Action"
              ("ModifyVpcEndpointConnectionNotification" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "ConnectionNotificationId"
                connectionNotificationId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "ConnectionEvents")
                connectionEvents
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ConnectionNotificationArn")
                connectionNotificationArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ModifyVpcEndpointConnectionNotification
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVpcEndpointConnectionNotification
         where
        type Rs ModifyVpcEndpointConnectionNotification =
             ModifyVpcEndpointConnectionNotificationResponse
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
                 ModifyVpcEndpointConnectionNotificationResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVpcEndpointConnectionNotificationResponse' smart constructor.
data ModifyVpcEndpointConnectionNotificationResponse = ModifyVpcEndpointConnectionNotificationResponse'
  { returnValue :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcEndpointConnectionNotificationResponse' value with any optional fields omitted.
mkModifyVpcEndpointConnectionNotificationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyVpcEndpointConnectionNotificationResponse
mkModifyVpcEndpointConnectionNotificationResponse responseStatus
  = ModifyVpcEndpointConnectionNotificationResponse'{returnValue =
                                                       Core.Nothing,
                                                     responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'returnValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnrrsReturnValue :: Lens.Lens' ModifyVpcEndpointConnectionNotificationResponse (Core.Maybe Core.Bool)
mvecnrrsReturnValue = Lens.field @"returnValue"
{-# INLINEABLE mvecnrrsReturnValue #-}
{-# DEPRECATED returnValue "Use generic-lens or generic-optics with 'returnValue' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnrrsResponseStatus :: Lens.Lens' ModifyVpcEndpointConnectionNotificationResponse Core.Int
mvecnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mvecnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
