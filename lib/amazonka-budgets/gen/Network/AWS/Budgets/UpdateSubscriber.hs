{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.UpdateSubscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a subscriber.
module Network.AWS.Budgets.UpdateSubscriber
  ( -- * Creating a request
    UpdateSubscriber (..),
    mkUpdateSubscriber,

    -- ** Request lenses
    usNotification,
    usOldSubscriber,
    usNewSubscriber,
    usAccountId,
    usBudgetName,

    -- * Destructuring the response
    UpdateSubscriberResponse (..),
    mkUpdateSubscriberResponse,

    -- ** Response lenses
    usrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of UpdateSubscriber
--
-- /See:/ 'mkUpdateSubscriber' smart constructor.
data UpdateSubscriber = UpdateSubscriber'
  { -- | The notification whose subscriber you want to update.
    notification :: Notification,
    -- | The previous subscriber that is associated with a budget notification.
    oldSubscriber :: Subscriber,
    -- | The updated subscriber that is associated with a budget notification.
    newSubscriber :: Subscriber,
    -- | The @accountId@ that is associated with the budget whose subscriber you want to update.
    accountId :: Lude.Text,
    -- | The name of the budget whose subscriber you want to update.
    budgetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSubscriber' with the minimum fields required to make a request.
--
-- * 'notification' - The notification whose subscriber you want to update.
-- * 'oldSubscriber' - The previous subscriber that is associated with a budget notification.
-- * 'newSubscriber' - The updated subscriber that is associated with a budget notification.
-- * 'accountId' - The @accountId@ that is associated with the budget whose subscriber you want to update.
-- * 'budgetName' - The name of the budget whose subscriber you want to update.
mkUpdateSubscriber ::
  -- | 'notification'
  Notification ->
  -- | 'oldSubscriber'
  Subscriber ->
  -- | 'newSubscriber'
  Subscriber ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  UpdateSubscriber
mkUpdateSubscriber
  pNotification_
  pOldSubscriber_
  pNewSubscriber_
  pAccountId_
  pBudgetName_ =
    UpdateSubscriber'
      { notification = pNotification_,
        oldSubscriber = pOldSubscriber_,
        newSubscriber = pNewSubscriber_,
        accountId = pAccountId_,
        budgetName = pBudgetName_
      }

-- | The notification whose subscriber you want to update.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usNotification :: Lens.Lens' UpdateSubscriber Notification
usNotification = Lens.lens (notification :: UpdateSubscriber -> Notification) (\s a -> s {notification = a} :: UpdateSubscriber)
{-# DEPRECATED usNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The previous subscriber that is associated with a budget notification.
--
-- /Note:/ Consider using 'oldSubscriber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usOldSubscriber :: Lens.Lens' UpdateSubscriber Subscriber
usOldSubscriber = Lens.lens (oldSubscriber :: UpdateSubscriber -> Subscriber) (\s a -> s {oldSubscriber = a} :: UpdateSubscriber)
{-# DEPRECATED usOldSubscriber "Use generic-lens or generic-optics with 'oldSubscriber' instead." #-}

-- | The updated subscriber that is associated with a budget notification.
--
-- /Note:/ Consider using 'newSubscriber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usNewSubscriber :: Lens.Lens' UpdateSubscriber Subscriber
usNewSubscriber = Lens.lens (newSubscriber :: UpdateSubscriber -> Subscriber) (\s a -> s {newSubscriber = a} :: UpdateSubscriber)
{-# DEPRECATED usNewSubscriber "Use generic-lens or generic-optics with 'newSubscriber' instead." #-}

-- | The @accountId@ that is associated with the budget whose subscriber you want to update.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAccountId :: Lens.Lens' UpdateSubscriber Lude.Text
usAccountId = Lens.lens (accountId :: UpdateSubscriber -> Lude.Text) (\s a -> s {accountId = a} :: UpdateSubscriber)
{-# DEPRECATED usAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget whose subscriber you want to update.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usBudgetName :: Lens.Lens' UpdateSubscriber Lude.Text
usBudgetName = Lens.lens (budgetName :: UpdateSubscriber -> Lude.Text) (\s a -> s {budgetName = a} :: UpdateSubscriber)
{-# DEPRECATED usBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Lude.AWSRequest UpdateSubscriber where
  type Rs UpdateSubscriber = UpdateSubscriberResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateSubscriberResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSubscriber where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.UpdateSubscriber" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSubscriber where
  toJSON UpdateSubscriber' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Notification" Lude..= notification),
            Lude.Just ("OldSubscriber" Lude..= oldSubscriber),
            Lude.Just ("NewSubscriber" Lude..= newSubscriber),
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName)
          ]
      )

instance Lude.ToPath UpdateSubscriber where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSubscriber where
  toQuery = Lude.const Lude.mempty

-- | Response of UpdateSubscriber
--
-- /See:/ 'mkUpdateSubscriberResponse' smart constructor.
newtype UpdateSubscriberResponse = UpdateSubscriberResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSubscriberResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateSubscriberResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSubscriberResponse
mkUpdateSubscriberResponse pResponseStatus_ =
  UpdateSubscriberResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateSubscriberResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateSubscriberResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSubscriberResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
