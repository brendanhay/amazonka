{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DeleteSubscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscriber.
--
-- /Important:/ Deleting the last subscriber to a notification also deletes the notification.
module Network.AWS.Budgets.DeleteSubscriber
  ( -- * Creating a request
    DeleteSubscriber (..),
    mkDeleteSubscriber,

    -- ** Request lenses
    dsAccountId,
    dsBudgetName,
    dsNotification,
    dsSubscriber,

    -- * Destructuring the response
    DeleteSubscriberResponse (..),
    mkDeleteSubscriberResponse,

    -- ** Response lenses
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DeleteSubscriber
--
-- /See:/ 'mkDeleteSubscriber' smart constructor.
data DeleteSubscriber = DeleteSubscriber'
  { -- | The @accountId@ that is associated with the budget whose subscriber you want to delete.
    accountId :: Types.AccountId,
    -- | The name of the budget whose subscriber you want to delete.
    budgetName :: Types.BudgetName,
    -- | The notification whose subscriber you want to delete.
    notification :: Types.Notification,
    -- | The subscriber that you want to delete.
    subscriber :: Types.Subscriber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubscriber' value with any optional fields omitted.
mkDeleteSubscriber ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'notification'
  Types.Notification ->
  -- | 'subscriber'
  Types.Subscriber ->
  DeleteSubscriber
mkDeleteSubscriber accountId budgetName notification subscriber =
  DeleteSubscriber'
    { accountId,
      budgetName,
      notification,
      subscriber
    }

-- | The @accountId@ that is associated with the budget whose subscriber you want to delete.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAccountId :: Lens.Lens' DeleteSubscriber Types.AccountId
dsAccountId = Lens.field @"accountId"
{-# DEPRECATED dsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget whose subscriber you want to delete.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsBudgetName :: Lens.Lens' DeleteSubscriber Types.BudgetName
dsBudgetName = Lens.field @"budgetName"
{-# DEPRECATED dsBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The notification whose subscriber you want to delete.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNotification :: Lens.Lens' DeleteSubscriber Types.Notification
dsNotification = Lens.field @"notification"
{-# DEPRECATED dsNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The subscriber that you want to delete.
--
-- /Note:/ Consider using 'subscriber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSubscriber :: Lens.Lens' DeleteSubscriber Types.Subscriber
dsSubscriber = Lens.field @"subscriber"
{-# DEPRECATED dsSubscriber "Use generic-lens or generic-optics with 'subscriber' instead." #-}

instance Core.FromJSON DeleteSubscriber where
  toJSON DeleteSubscriber {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("Notification" Core..= notification),
            Core.Just ("Subscriber" Core..= subscriber)
          ]
      )

instance Core.AWSRequest DeleteSubscriber where
  type Rs DeleteSubscriber = DeleteSubscriberResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSBudgetServiceGateway.DeleteSubscriber")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSubscriberResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Response of DeleteSubscriber
--
-- /See:/ 'mkDeleteSubscriberResponse' smart constructor.
newtype DeleteSubscriberResponse = DeleteSubscriberResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubscriberResponse' value with any optional fields omitted.
mkDeleteSubscriberResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteSubscriberResponse
mkDeleteSubscriberResponse responseStatus =
  DeleteSubscriberResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteSubscriberResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
