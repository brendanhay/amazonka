{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cnrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of CreateNotification
--
-- /See:/ 'mkCreateNotification' smart constructor.
data CreateNotification = CreateNotification'
  { accountId ::
      Lude.Text,
    budgetName :: Lude.Text,
    notification :: Notification,
    subscribers :: Lude.NonEmpty Subscriber
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNotification' with the minimum fields required to make a request.
--
-- * 'accountId' - The @accountId@ that is associated with the budget that you want to create a notification for.
-- * 'budgetName' - The name of the budget that you want AWS to notify you about. Budget names must be unique within an account.
-- * 'notification' - The notification that you want to create.
-- * 'subscribers' - A list of subscribers that you want to associate with the notification. Each notification can have one SNS subscriber and up to 10 email subscribers.
mkCreateNotification ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'notification'
  Notification ->
  -- | 'subscribers'
  Lude.NonEmpty Subscriber ->
  CreateNotification
mkCreateNotification
  pAccountId_
  pBudgetName_
  pNotification_
  pSubscribers_ =
    CreateNotification'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        notification = pNotification_,
        subscribers = pSubscribers_
      }

-- | The @accountId@ that is associated with the budget that you want to create a notification for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnAccountId :: Lens.Lens' CreateNotification Lude.Text
cnAccountId = Lens.lens (accountId :: CreateNotification -> Lude.Text) (\s a -> s {accountId = a} :: CreateNotification)
{-# DEPRECATED cnAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget that you want AWS to notify you about. Budget names must be unique within an account.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnBudgetName :: Lens.Lens' CreateNotification Lude.Text
cnBudgetName = Lens.lens (budgetName :: CreateNotification -> Lude.Text) (\s a -> s {budgetName = a} :: CreateNotification)
{-# DEPRECATED cnBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The notification that you want to create.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnNotification :: Lens.Lens' CreateNotification Notification
cnNotification = Lens.lens (notification :: CreateNotification -> Notification) (\s a -> s {notification = a} :: CreateNotification)
{-# DEPRECATED cnNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | A list of subscribers that you want to associate with the notification. Each notification can have one SNS subscriber and up to 10 email subscribers.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnSubscribers :: Lens.Lens' CreateNotification (Lude.NonEmpty Subscriber)
cnSubscribers = Lens.lens (subscribers :: CreateNotification -> Lude.NonEmpty Subscriber) (\s a -> s {subscribers = a} :: CreateNotification)
{-# DEPRECATED cnSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

instance Lude.AWSRequest CreateNotification where
  type Rs CreateNotification = CreateNotificationResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateNotificationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNotification where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.CreateNotification" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateNotification where
  toJSON CreateNotification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName),
            Lude.Just ("Notification" Lude..= notification),
            Lude.Just ("Subscribers" Lude..= subscribers)
          ]
      )

instance Lude.ToPath CreateNotification where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNotification where
  toQuery = Lude.const Lude.mempty

-- | Response of CreateNotification
--
-- /See:/ 'mkCreateNotificationResponse' smart constructor.
newtype CreateNotificationResponse = CreateNotificationResponse'
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

-- | Creates a value of 'CreateNotificationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateNotificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNotificationResponse
mkCreateNotificationResponse pResponseStatus_ =
  CreateNotificationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnrsResponseStatus :: Lens.Lens' CreateNotificationResponse Lude.Int
cnrsResponseStatus = Lens.lens (responseStatus :: CreateNotificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNotificationResponse)
{-# DEPRECATED cnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
