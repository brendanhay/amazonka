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
    dsSubscriber,
    dsNotification,
    dsAccountId,
    dsBudgetName,

    -- * Destructuring the response
    DeleteSubscriberResponse (..),
    mkDeleteSubscriberResponse,

    -- ** Response lenses
    dsrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of DeleteSubscriber
--
-- /See:/ 'mkDeleteSubscriber' smart constructor.
data DeleteSubscriber = DeleteSubscriber'
  { -- | The subscriber that you want to delete.
    subscriber :: Subscriber,
    -- | The notification whose subscriber you want to delete.
    notification :: Notification,
    -- | The @accountId@ that is associated with the budget whose subscriber you want to delete.
    accountId :: Lude.Text,
    -- | The name of the budget whose subscriber you want to delete.
    budgetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSubscriber' with the minimum fields required to make a request.
--
-- * 'subscriber' - The subscriber that you want to delete.
-- * 'notification' - The notification whose subscriber you want to delete.
-- * 'accountId' - The @accountId@ that is associated with the budget whose subscriber you want to delete.
-- * 'budgetName' - The name of the budget whose subscriber you want to delete.
mkDeleteSubscriber ::
  -- | 'subscriber'
  Subscriber ->
  -- | 'notification'
  Notification ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  DeleteSubscriber
mkDeleteSubscriber
  pSubscriber_
  pNotification_
  pAccountId_
  pBudgetName_ =
    DeleteSubscriber'
      { subscriber = pSubscriber_,
        notification = pNotification_,
        accountId = pAccountId_,
        budgetName = pBudgetName_
      }

-- | The subscriber that you want to delete.
--
-- /Note:/ Consider using 'subscriber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSubscriber :: Lens.Lens' DeleteSubscriber Subscriber
dsSubscriber = Lens.lens (subscriber :: DeleteSubscriber -> Subscriber) (\s a -> s {subscriber = a} :: DeleteSubscriber)
{-# DEPRECATED dsSubscriber "Use generic-lens or generic-optics with 'subscriber' instead." #-}

-- | The notification whose subscriber you want to delete.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNotification :: Lens.Lens' DeleteSubscriber Notification
dsNotification = Lens.lens (notification :: DeleteSubscriber -> Notification) (\s a -> s {notification = a} :: DeleteSubscriber)
{-# DEPRECATED dsNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The @accountId@ that is associated with the budget whose subscriber you want to delete.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAccountId :: Lens.Lens' DeleteSubscriber Lude.Text
dsAccountId = Lens.lens (accountId :: DeleteSubscriber -> Lude.Text) (\s a -> s {accountId = a} :: DeleteSubscriber)
{-# DEPRECATED dsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget whose subscriber you want to delete.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsBudgetName :: Lens.Lens' DeleteSubscriber Lude.Text
dsBudgetName = Lens.lens (budgetName :: DeleteSubscriber -> Lude.Text) (\s a -> s {budgetName = a} :: DeleteSubscriber)
{-# DEPRECATED dsBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Lude.AWSRequest DeleteSubscriber where
  type Rs DeleteSubscriber = DeleteSubscriberResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteSubscriberResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSubscriber where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.DeleteSubscriber" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSubscriber where
  toJSON DeleteSubscriber' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Subscriber" Lude..= subscriber),
            Lude.Just ("Notification" Lude..= notification),
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName)
          ]
      )

instance Lude.ToPath DeleteSubscriber where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSubscriber where
  toQuery = Lude.const Lude.mempty

-- | Response of DeleteSubscriber
--
-- /See:/ 'mkDeleteSubscriberResponse' smart constructor.
newtype DeleteSubscriberResponse = DeleteSubscriberResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSubscriberResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteSubscriberResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSubscriberResponse
mkDeleteSubscriberResponse pResponseStatus_ =
  DeleteSubscriberResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteSubscriberResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteSubscriberResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSubscriberResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
