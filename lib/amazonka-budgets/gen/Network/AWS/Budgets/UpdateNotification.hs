{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.UpdateNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notification.
module Network.AWS.Budgets.UpdateNotification
  ( -- * Creating a request
    UpdateNotification (..),
    mkUpdateNotification,

    -- ** Request lenses
    unAccountId,
    unBudgetName,
    unOldNotification,
    unNewNotification,

    -- * Destructuring the response
    UpdateNotificationResponse (..),
    mkUpdateNotificationResponse,

    -- ** Response lenses
    unrrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of UpdateNotification
--
-- /See:/ 'mkUpdateNotification' smart constructor.
data UpdateNotification = UpdateNotification'
  { -- | The @accountId@ that is associated with the budget whose notification you want to update.
    accountId :: Types.AccountId,
    -- | The name of the budget whose notification you want to update.
    budgetName :: Types.BudgetName,
    -- | The previous notification that is associated with a budget.
    oldNotification :: Types.Notification,
    -- | The updated notification to be associated with a budget.
    newNotification :: Types.Notification
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNotification' value with any optional fields omitted.
mkUpdateNotification ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'oldNotification'
  Types.Notification ->
  -- | 'newNotification'
  Types.Notification ->
  UpdateNotification
mkUpdateNotification
  accountId
  budgetName
  oldNotification
  newNotification =
    UpdateNotification'
      { accountId,
        budgetName,
        oldNotification,
        newNotification
      }

-- | The @accountId@ that is associated with the budget whose notification you want to update.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unAccountId :: Lens.Lens' UpdateNotification Types.AccountId
unAccountId = Lens.field @"accountId"
{-# DEPRECATED unAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget whose notification you want to update.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unBudgetName :: Lens.Lens' UpdateNotification Types.BudgetName
unBudgetName = Lens.field @"budgetName"
{-# DEPRECATED unBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The previous notification that is associated with a budget.
--
-- /Note:/ Consider using 'oldNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unOldNotification :: Lens.Lens' UpdateNotification Types.Notification
unOldNotification = Lens.field @"oldNotification"
{-# DEPRECATED unOldNotification "Use generic-lens or generic-optics with 'oldNotification' instead." #-}

-- | The updated notification to be associated with a budget.
--
-- /Note:/ Consider using 'newNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unNewNotification :: Lens.Lens' UpdateNotification Types.Notification
unNewNotification = Lens.field @"newNotification"
{-# DEPRECATED unNewNotification "Use generic-lens or generic-optics with 'newNotification' instead." #-}

instance Core.FromJSON UpdateNotification where
  toJSON UpdateNotification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("OldNotification" Core..= oldNotification),
            Core.Just ("NewNotification" Core..= newNotification)
          ]
      )

instance Core.AWSRequest UpdateNotification where
  type Rs UpdateNotification = UpdateNotificationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSBudgetServiceGateway.UpdateNotification")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotificationResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Response of UpdateNotification
--
-- /See:/ 'mkUpdateNotificationResponse' smart constructor.
newtype UpdateNotificationResponse = UpdateNotificationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNotificationResponse' value with any optional fields omitted.
mkUpdateNotificationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateNotificationResponse
mkUpdateNotificationResponse responseStatus =
  UpdateNotificationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unrrsResponseStatus :: Lens.Lens' UpdateNotificationResponse Core.Int
unrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED unrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
