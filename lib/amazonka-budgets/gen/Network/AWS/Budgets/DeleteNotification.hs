{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DeleteNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a notification.
--
-- /Important:/ Deleting a notification also deletes the subscribers that are associated with the notification.
module Network.AWS.Budgets.DeleteNotification
  ( -- * Creating a request
    DeleteNotification (..),
    mkDeleteNotification,

    -- ** Request lenses
    dnAccountId,
    dnBudgetName,
    dnNotification,

    -- * Destructuring the response
    DeleteNotificationResponse (..),
    mkDeleteNotificationResponse,

    -- ** Response lenses
    dnrrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DeleteNotification
--
-- /See:/ 'mkDeleteNotification' smart constructor.
data DeleteNotification = DeleteNotification'
  { -- | The @accountId@ that is associated with the budget whose notification you want to delete.
    accountId :: Types.AccountId,
    -- | The name of the budget whose notification you want to delete.
    budgetName :: Types.BudgetName,
    -- | The notification that you want to delete.
    notification :: Types.Notification
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotification' value with any optional fields omitted.
mkDeleteNotification ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'notification'
  Types.Notification ->
  DeleteNotification
mkDeleteNotification accountId budgetName notification =
  DeleteNotification' {accountId, budgetName, notification}

-- | The @accountId@ that is associated with the budget whose notification you want to delete.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnAccountId :: Lens.Lens' DeleteNotification Types.AccountId
dnAccountId = Lens.field @"accountId"
{-# DEPRECATED dnAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget whose notification you want to delete.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnBudgetName :: Lens.Lens' DeleteNotification Types.BudgetName
dnBudgetName = Lens.field @"budgetName"
{-# DEPRECATED dnBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The notification that you want to delete.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnNotification :: Lens.Lens' DeleteNotification Types.Notification
dnNotification = Lens.field @"notification"
{-# DEPRECATED dnNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

instance Core.FromJSON DeleteNotification where
  toJSON DeleteNotification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("Notification" Core..= notification)
          ]
      )

instance Core.AWSRequest DeleteNotification where
  type Rs DeleteNotification = DeleteNotificationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSBudgetServiceGateway.DeleteNotification")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNotificationResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Response of DeleteNotification
--
-- /See:/ 'mkDeleteNotificationResponse' smart constructor.
newtype DeleteNotificationResponse = DeleteNotificationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotificationResponse' value with any optional fields omitted.
mkDeleteNotificationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteNotificationResponse
mkDeleteNotificationResponse responseStatus =
  DeleteNotificationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrrsResponseStatus :: Lens.Lens' DeleteNotificationResponse Core.Int
dnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
