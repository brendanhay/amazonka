{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.CreateSubscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscriber. You must create the associated budget and notification before you create the subscriber.
module Network.AWS.Budgets.CreateSubscriber
  ( -- * Creating a request
    CreateSubscriber (..),
    mkCreateSubscriber,

    -- ** Request lenses
    csAccountId,
    csBudgetName,
    csNotification,
    csSubscriber,

    -- * Destructuring the response
    CreateSubscriberResponse (..),
    mkCreateSubscriberResponse,

    -- ** Response lenses
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of CreateSubscriber
--
-- /See:/ 'mkCreateSubscriber' smart constructor.
data CreateSubscriber = CreateSubscriber'
  { -- | The @accountId@ that is associated with the budget that you want to create a subscriber for.
    accountId :: Types.AccountId,
    -- | The name of the budget that you want to subscribe to. Budget names must be unique within an account.
    budgetName :: Types.BudgetName,
    -- | The notification that you want to create a subscriber for.
    notification :: Types.Notification,
    -- | The subscriber that you want to associate with a budget notification.
    subscriber :: Types.Subscriber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubscriber' value with any optional fields omitted.
mkCreateSubscriber ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'notification'
  Types.Notification ->
  -- | 'subscriber'
  Types.Subscriber ->
  CreateSubscriber
mkCreateSubscriber accountId budgetName notification subscriber =
  CreateSubscriber'
    { accountId,
      budgetName,
      notification,
      subscriber
    }

-- | The @accountId@ that is associated with the budget that you want to create a subscriber for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAccountId :: Lens.Lens' CreateSubscriber Types.AccountId
csAccountId = Lens.field @"accountId"
{-# DEPRECATED csAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget that you want to subscribe to. Budget names must be unique within an account.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csBudgetName :: Lens.Lens' CreateSubscriber Types.BudgetName
csBudgetName = Lens.field @"budgetName"
{-# DEPRECATED csBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The notification that you want to create a subscriber for.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNotification :: Lens.Lens' CreateSubscriber Types.Notification
csNotification = Lens.field @"notification"
{-# DEPRECATED csNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The subscriber that you want to associate with a budget notification.
--
-- /Note:/ Consider using 'subscriber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSubscriber :: Lens.Lens' CreateSubscriber Types.Subscriber
csSubscriber = Lens.field @"subscriber"
{-# DEPRECATED csSubscriber "Use generic-lens or generic-optics with 'subscriber' instead." #-}

instance Core.FromJSON CreateSubscriber where
  toJSON CreateSubscriber {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("Notification" Core..= notification),
            Core.Just ("Subscriber" Core..= subscriber)
          ]
      )

instance Core.AWSRequest CreateSubscriber where
  type Rs CreateSubscriber = CreateSubscriberResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSBudgetServiceGateway.CreateSubscriber")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateSubscriberResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Response of CreateSubscriber
--
-- /See:/ 'mkCreateSubscriberResponse' smart constructor.
newtype CreateSubscriberResponse = CreateSubscriberResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubscriberResponse' value with any optional fields omitted.
mkCreateSubscriberResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSubscriberResponse
mkCreateSubscriberResponse responseStatus =
  CreateSubscriberResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSubscriberResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
