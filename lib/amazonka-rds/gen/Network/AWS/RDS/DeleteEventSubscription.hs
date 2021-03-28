{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteEventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an RDS event notification subscription.
module Network.AWS.RDS.DeleteEventSubscription
    (
    -- * Creating a request
      DeleteEventSubscription (..)
    , mkDeleteEventSubscription
    -- ** Request lenses
    , desSubscriptionName

    -- * Destructuring the response
    , DeleteEventSubscriptionResponse (..)
    , mkDeleteEventSubscriptionResponse
    -- ** Response lenses
    , desrrsEventSubscription
    , desrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteEventSubscription' smart constructor.
newtype DeleteEventSubscription = DeleteEventSubscription'
  { subscriptionName :: Core.Text
    -- ^ The name of the RDS event notification subscription you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEventSubscription' value with any optional fields omitted.
mkDeleteEventSubscription
    :: Core.Text -- ^ 'subscriptionName'
    -> DeleteEventSubscription
mkDeleteEventSubscription subscriptionName
  = DeleteEventSubscription'{subscriptionName}

-- | The name of the RDS event notification subscription you want to delete.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desSubscriptionName :: Lens.Lens' DeleteEventSubscription Core.Text
desSubscriptionName = Lens.field @"subscriptionName"
{-# INLINEABLE desSubscriptionName #-}
{-# DEPRECATED subscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead"  #-}

instance Core.ToQuery DeleteEventSubscription where
        toQuery DeleteEventSubscription{..}
          = Core.toQueryPair "Action"
              ("DeleteEventSubscription" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "SubscriptionName" subscriptionName

instance Core.ToHeaders DeleteEventSubscription where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteEventSubscription where
        type Rs DeleteEventSubscription = DeleteEventSubscriptionResponse
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
          = Response.receiveXMLWrapper "DeleteEventSubscriptionResult"
              (\ s h x ->
                 DeleteEventSubscriptionResponse' Core.<$>
                   (x Core..@? "EventSubscription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
  { eventSubscription :: Core.Maybe Types.EventSubscription
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEventSubscriptionResponse' value with any optional fields omitted.
mkDeleteEventSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteEventSubscriptionResponse
mkDeleteEventSubscriptionResponse responseStatus
  = DeleteEventSubscriptionResponse'{eventSubscription =
                                       Core.Nothing,
                                     responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsEventSubscription :: Lens.Lens' DeleteEventSubscriptionResponse (Core.Maybe Types.EventSubscription)
desrrsEventSubscription = Lens.field @"eventSubscription"
{-# INLINEABLE desrrsEventSubscription #-}
{-# DEPRECATED eventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsResponseStatus :: Lens.Lens' DeleteEventSubscriptionResponse Core.Int
desrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE desrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
