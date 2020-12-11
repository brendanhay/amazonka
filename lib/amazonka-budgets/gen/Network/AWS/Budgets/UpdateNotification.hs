{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    unrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of UpdateNotification
--
-- /See:/ 'mkUpdateNotification' smart constructor.
data UpdateNotification = UpdateNotification'
  { accountId ::
      Lude.Text,
    budgetName :: Lude.Text,
    oldNotification :: Notification,
    newNotification :: Notification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNotification' with the minimum fields required to make a request.
--
-- * 'accountId' - The @accountId@ that is associated with the budget whose notification you want to update.
-- * 'budgetName' - The name of the budget whose notification you want to update.
-- * 'newNotification' - The updated notification to be associated with a budget.
-- * 'oldNotification' - The previous notification that is associated with a budget.
mkUpdateNotification ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'oldNotification'
  Notification ->
  -- | 'newNotification'
  Notification ->
  UpdateNotification
mkUpdateNotification
  pAccountId_
  pBudgetName_
  pOldNotification_
  pNewNotification_ =
    UpdateNotification'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        oldNotification = pOldNotification_,
        newNotification = pNewNotification_
      }

-- | The @accountId@ that is associated with the budget whose notification you want to update.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unAccountId :: Lens.Lens' UpdateNotification Lude.Text
unAccountId = Lens.lens (accountId :: UpdateNotification -> Lude.Text) (\s a -> s {accountId = a} :: UpdateNotification)
{-# DEPRECATED unAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget whose notification you want to update.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unBudgetName :: Lens.Lens' UpdateNotification Lude.Text
unBudgetName = Lens.lens (budgetName :: UpdateNotification -> Lude.Text) (\s a -> s {budgetName = a} :: UpdateNotification)
{-# DEPRECATED unBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The previous notification that is associated with a budget.
--
-- /Note:/ Consider using 'oldNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unOldNotification :: Lens.Lens' UpdateNotification Notification
unOldNotification = Lens.lens (oldNotification :: UpdateNotification -> Notification) (\s a -> s {oldNotification = a} :: UpdateNotification)
{-# DEPRECATED unOldNotification "Use generic-lens or generic-optics with 'oldNotification' instead." #-}

-- | The updated notification to be associated with a budget.
--
-- /Note:/ Consider using 'newNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unNewNotification :: Lens.Lens' UpdateNotification Notification
unNewNotification = Lens.lens (newNotification :: UpdateNotification -> Notification) (\s a -> s {newNotification = a} :: UpdateNotification)
{-# DEPRECATED unNewNotification "Use generic-lens or generic-optics with 'newNotification' instead." #-}

instance Lude.AWSRequest UpdateNotification where
  type Rs UpdateNotification = UpdateNotificationResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateNotificationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateNotification where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.UpdateNotification" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateNotification where
  toJSON UpdateNotification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName),
            Lude.Just ("OldNotification" Lude..= oldNotification),
            Lude.Just ("NewNotification" Lude..= newNotification)
          ]
      )

instance Lude.ToPath UpdateNotification where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateNotification where
  toQuery = Lude.const Lude.mempty

-- | Response of UpdateNotification
--
-- /See:/ 'mkUpdateNotificationResponse' smart constructor.
newtype UpdateNotificationResponse = UpdateNotificationResponse'
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

-- | Creates a value of 'UpdateNotificationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateNotificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateNotificationResponse
mkUpdateNotificationResponse pResponseStatus_ =
  UpdateNotificationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unrsResponseStatus :: Lens.Lens' UpdateNotificationResponse Lude.Int
unrsResponseStatus = Lens.lens (responseStatus :: UpdateNotificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateNotificationResponse)
{-# DEPRECATED unrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
