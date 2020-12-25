{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteEventSubscription (..),
    mkDeleteEventSubscription,

    -- ** Request lenses
    desSubscriptionName,

    -- * Destructuring the response
    DeleteEventSubscriptionResponse (..),
    mkDeleteEventSubscriptionResponse,

    -- ** Response lenses
    desrrsEventSubscription,
    desrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteEventSubscription' smart constructor.
newtype DeleteEventSubscription = DeleteEventSubscription'
  { -- | The name of the RDS event notification subscription you want to delete.
    subscriptionName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEventSubscription' value with any optional fields omitted.
mkDeleteEventSubscription ::
  -- | 'subscriptionName'
  Types.String ->
  DeleteEventSubscription
mkDeleteEventSubscription subscriptionName =
  DeleteEventSubscription' {subscriptionName}

-- | The name of the RDS event notification subscription you want to delete.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desSubscriptionName :: Lens.Lens' DeleteEventSubscription Types.String
desSubscriptionName = Lens.field @"subscriptionName"
{-# DEPRECATED desSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

instance Core.AWSRequest DeleteEventSubscription where
  type Rs DeleteEventSubscription = DeleteEventSubscriptionResponse
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
            ( Core.pure ("Action", "DeleteEventSubscription")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "SubscriptionName" subscriptionName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteEventSubscriptionResult"
      ( \s h x ->
          DeleteEventSubscriptionResponse'
            Core.<$> (x Core..@? "EventSubscription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
  { eventSubscription :: Core.Maybe Types.EventSubscription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEventSubscriptionResponse' value with any optional fields omitted.
mkDeleteEventSubscriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteEventSubscriptionResponse
mkDeleteEventSubscriptionResponse responseStatus =
  DeleteEventSubscriptionResponse'
    { eventSubscription =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsEventSubscription :: Lens.Lens' DeleteEventSubscriptionResponse (Core.Maybe Types.EventSubscription)
desrrsEventSubscription = Lens.field @"eventSubscription"
{-# DEPRECATED desrrsEventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsResponseStatus :: Lens.Lens' DeleteEventSubscriptionResponse Core.Int
desrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED desrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
