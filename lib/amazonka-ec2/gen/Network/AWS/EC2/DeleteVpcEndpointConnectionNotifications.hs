{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVpcEndpointConnectionNotifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more VPC endpoint connection notifications.
module Network.AWS.EC2.DeleteVpcEndpointConnectionNotifications
    (
    -- * Creating a request
      DeleteVpcEndpointConnectionNotifications (..)
    , mkDeleteVpcEndpointConnectionNotifications
    -- ** Request lenses
    , dvecnConnectionNotificationIds
    , dvecnDryRun

    -- * Destructuring the response
    , DeleteVpcEndpointConnectionNotificationsResponse (..)
    , mkDeleteVpcEndpointConnectionNotificationsResponse
    -- ** Response lenses
    , dvecnrrsUnsuccessful
    , dvecnrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteVpcEndpointConnectionNotifications' smart constructor.
data DeleteVpcEndpointConnectionNotifications = DeleteVpcEndpointConnectionNotifications'
  { connectionNotificationIds :: [Types.ConnectionNotificationId]
    -- ^ One or more notification IDs.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcEndpointConnectionNotifications' value with any optional fields omitted.
mkDeleteVpcEndpointConnectionNotifications
    :: DeleteVpcEndpointConnectionNotifications
mkDeleteVpcEndpointConnectionNotifications
  = DeleteVpcEndpointConnectionNotifications'{connectionNotificationIds
                                                = Core.mempty,
                                              dryRun = Core.Nothing}

-- | One or more notification IDs.
--
-- /Note:/ Consider using 'connectionNotificationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnConnectionNotificationIds :: Lens.Lens' DeleteVpcEndpointConnectionNotifications [Types.ConnectionNotificationId]
dvecnConnectionNotificationIds = Lens.field @"connectionNotificationIds"
{-# INLINEABLE dvecnConnectionNotificationIds #-}
{-# DEPRECATED connectionNotificationIds "Use generic-lens or generic-optics with 'connectionNotificationIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnDryRun :: Lens.Lens' DeleteVpcEndpointConnectionNotifications (Core.Maybe Core.Bool)
dvecnDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvecnDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteVpcEndpointConnectionNotifications
         where
        toQuery DeleteVpcEndpointConnectionNotifications{..}
          = Core.toQueryPair "Action"
              ("DeleteVpcEndpointConnectionNotifications" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryList "ConnectionNotificationId"
                connectionNotificationIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteVpcEndpointConnectionNotifications
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVpcEndpointConnectionNotifications
         where
        type Rs DeleteVpcEndpointConnectionNotifications =
             DeleteVpcEndpointConnectionNotificationsResponse
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
                 DeleteVpcEndpointConnectionNotificationsResponse' Core.<$>
                   (x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVpcEndpointConnectionNotificationsResponse' smart constructor.
data DeleteVpcEndpointConnectionNotificationsResponse = DeleteVpcEndpointConnectionNotificationsResponse'
  { unsuccessful :: Core.Maybe [Types.UnsuccessfulItem]
    -- ^ Information about the notifications that could not be deleted successfully.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcEndpointConnectionNotificationsResponse' value with any optional fields omitted.
mkDeleteVpcEndpointConnectionNotificationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteVpcEndpointConnectionNotificationsResponse
mkDeleteVpcEndpointConnectionNotificationsResponse responseStatus
  = DeleteVpcEndpointConnectionNotificationsResponse'{unsuccessful =
                                                        Core.Nothing,
                                                      responseStatus}

-- | Information about the notifications that could not be deleted successfully.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnrrsUnsuccessful :: Lens.Lens' DeleteVpcEndpointConnectionNotificationsResponse (Core.Maybe [Types.UnsuccessfulItem])
dvecnrrsUnsuccessful = Lens.field @"unsuccessful"
{-# INLINEABLE dvecnrrsUnsuccessful #-}
{-# DEPRECATED unsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnrrsResponseStatus :: Lens.Lens' DeleteVpcEndpointConnectionNotificationsResponse Core.Int
dvecnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvecnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
