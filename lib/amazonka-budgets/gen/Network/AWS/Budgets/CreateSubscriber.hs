{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    csrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of CreateSubscriber
--
-- /See:/ 'mkCreateSubscriber' smart constructor.
data CreateSubscriber = CreateSubscriber'
  { accountId :: Lude.Text,
    budgetName :: Lude.Text,
    notification :: Notification,
    subscriber :: Subscriber
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubscriber' with the minimum fields required to make a request.
--
-- * 'accountId' - The @accountId@ that is associated with the budget that you want to create a subscriber for.
-- * 'budgetName' - The name of the budget that you want to subscribe to. Budget names must be unique within an account.
-- * 'notification' - The notification that you want to create a subscriber for.
-- * 'subscriber' - The subscriber that you want to associate with a budget notification.
mkCreateSubscriber ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'notification'
  Notification ->
  -- | 'subscriber'
  Subscriber ->
  CreateSubscriber
mkCreateSubscriber
  pAccountId_
  pBudgetName_
  pNotification_
  pSubscriber_ =
    CreateSubscriber'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        notification = pNotification_,
        subscriber = pSubscriber_
      }

-- | The @accountId@ that is associated with the budget that you want to create a subscriber for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAccountId :: Lens.Lens' CreateSubscriber Lude.Text
csAccountId = Lens.lens (accountId :: CreateSubscriber -> Lude.Text) (\s a -> s {accountId = a} :: CreateSubscriber)
{-# DEPRECATED csAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget that you want to subscribe to. Budget names must be unique within an account.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csBudgetName :: Lens.Lens' CreateSubscriber Lude.Text
csBudgetName = Lens.lens (budgetName :: CreateSubscriber -> Lude.Text) (\s a -> s {budgetName = a} :: CreateSubscriber)
{-# DEPRECATED csBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The notification that you want to create a subscriber for.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNotification :: Lens.Lens' CreateSubscriber Notification
csNotification = Lens.lens (notification :: CreateSubscriber -> Notification) (\s a -> s {notification = a} :: CreateSubscriber)
{-# DEPRECATED csNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The subscriber that you want to associate with a budget notification.
--
-- /Note:/ Consider using 'subscriber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSubscriber :: Lens.Lens' CreateSubscriber Subscriber
csSubscriber = Lens.lens (subscriber :: CreateSubscriber -> Subscriber) (\s a -> s {subscriber = a} :: CreateSubscriber)
{-# DEPRECATED csSubscriber "Use generic-lens or generic-optics with 'subscriber' instead." #-}

instance Lude.AWSRequest CreateSubscriber where
  type Rs CreateSubscriber = CreateSubscriberResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateSubscriberResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSubscriber where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.CreateSubscriber" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSubscriber where
  toJSON CreateSubscriber' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName),
            Lude.Just ("Notification" Lude..= notification),
            Lude.Just ("Subscriber" Lude..= subscriber)
          ]
      )

instance Lude.ToPath CreateSubscriber where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSubscriber where
  toQuery = Lude.const Lude.mempty

-- | Response of CreateSubscriber
--
-- /See:/ 'mkCreateSubscriberResponse' smart constructor.
newtype CreateSubscriberResponse = CreateSubscriberResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubscriberResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateSubscriberResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSubscriberResponse
mkCreateSubscriberResponse pResponseStatus_ =
  CreateSubscriberResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateSubscriberResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateSubscriberResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSubscriberResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
