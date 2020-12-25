{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteEventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS DMS event subscription.
module Network.AWS.DMS.DeleteEventSubscription
  ( -- * Creating a request
    DeleteEventSubscription (..),
    mkDeleteEventSubscription,

    -- ** Request lenses
    desSubscriptionName,

    -- * Destructuring the response
    DeleteEventSubscriptionResponse (..),
    mkDeleteEventSubscriptionResponse,

    -- ** Response lenses
    desrfrsEventSubscription,
    desrfrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteEventSubscription' smart constructor.
newtype DeleteEventSubscription = DeleteEventSubscription'
  { -- | The name of the DMS event notification subscription to be deleted.
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

-- | The name of the DMS event notification subscription to be deleted.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desSubscriptionName :: Lens.Lens' DeleteEventSubscription Types.String
desSubscriptionName = Lens.field @"subscriptionName"
{-# DEPRECATED desSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

instance Core.FromJSON DeleteEventSubscription where
  toJSON DeleteEventSubscription {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SubscriptionName" Core..= subscriptionName)]
      )

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
            ("X-Amz-Target", "AmazonDMSv20160101.DeleteEventSubscription")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEventSubscriptionResponse'
            Core.<$> (x Core..:? "EventSubscription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkDeleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
  { -- | The event subscription that was deleted.
    eventSubscription :: Core.Maybe Types.EventSubscription,
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

-- | The event subscription that was deleted.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrfrsEventSubscription :: Lens.Lens' DeleteEventSubscriptionResponse (Core.Maybe Types.EventSubscription)
desrfrsEventSubscription = Lens.field @"eventSubscription"
{-# DEPRECATED desrfrsEventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrfrsResponseStatus :: Lens.Lens' DeleteEventSubscriptionResponse Core.Int
desrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED desrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
