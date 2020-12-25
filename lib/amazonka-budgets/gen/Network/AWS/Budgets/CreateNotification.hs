{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.CreateNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a notification. You must create the budget before you create the associated notification.
module Network.AWS.Budgets.CreateNotification
  ( -- * Creating a request
    CreateNotification (..),
    mkCreateNotification,

    -- ** Request lenses
    cnAccountId,
    cnBudgetName,
    cnNotification,
    cnSubscribers,

    -- * Destructuring the response
    CreateNotificationResponse (..),
    mkCreateNotificationResponse,

    -- ** Response lenses
    cnrrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of CreateNotification
--
-- /See:/ 'mkCreateNotification' smart constructor.
data CreateNotification = CreateNotification'
  { -- | The @accountId@ that is associated with the budget that you want to create a notification for.
    accountId :: Types.AccountId,
    -- | The name of the budget that you want AWS to notify you about. Budget names must be unique within an account.
    budgetName :: Types.BudgetName,
    -- | The notification that you want to create.
    notification :: Types.Notification,
    -- | A list of subscribers that you want to associate with the notification. Each notification can have one SNS subscriber and up to 10 email subscribers.
    subscribers :: Core.NonEmpty Types.Subscriber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNotification' value with any optional fields omitted.
mkCreateNotification ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'notification'
  Types.Notification ->
  -- | 'subscribers'
  Core.NonEmpty Types.Subscriber ->
  CreateNotification
mkCreateNotification accountId budgetName notification subscribers =
  CreateNotification'
    { accountId,
      budgetName,
      notification,
      subscribers
    }

-- | The @accountId@ that is associated with the budget that you want to create a notification for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnAccountId :: Lens.Lens' CreateNotification Types.AccountId
cnAccountId = Lens.field @"accountId"
{-# DEPRECATED cnAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget that you want AWS to notify you about. Budget names must be unique within an account.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnBudgetName :: Lens.Lens' CreateNotification Types.BudgetName
cnBudgetName = Lens.field @"budgetName"
{-# DEPRECATED cnBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The notification that you want to create.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnNotification :: Lens.Lens' CreateNotification Types.Notification
cnNotification = Lens.field @"notification"
{-# DEPRECATED cnNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | A list of subscribers that you want to associate with the notification. Each notification can have one SNS subscriber and up to 10 email subscribers.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnSubscribers :: Lens.Lens' CreateNotification (Core.NonEmpty Types.Subscriber)
cnSubscribers = Lens.field @"subscribers"
{-# DEPRECATED cnSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

instance Core.FromJSON CreateNotification where
  toJSON CreateNotification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("Notification" Core..= notification),
            Core.Just ("Subscribers" Core..= subscribers)
          ]
      )

instance Core.AWSRequest CreateNotification where
  type Rs CreateNotification = CreateNotificationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSBudgetServiceGateway.CreateNotification")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateNotificationResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Response of CreateNotification
--
-- /See:/ 'mkCreateNotificationResponse' smart constructor.
newtype CreateNotificationResponse = CreateNotificationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNotificationResponse' value with any optional fields omitted.
mkCreateNotificationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateNotificationResponse
mkCreateNotificationResponse responseStatus =
  CreateNotificationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnrrsResponseStatus :: Lens.Lens' CreateNotificationResponse Core.Int
cnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
