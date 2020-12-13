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
    dnNotification,
    dnAccountId,
    dnBudgetName,

    -- * Destructuring the response
    DeleteNotificationResponse (..),
    mkDeleteNotificationResponse,

    -- ** Response lenses
    dnrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of DeleteNotification
--
-- /See:/ 'mkDeleteNotification' smart constructor.
data DeleteNotification = DeleteNotification'
  { -- | The notification that you want to delete.
    notification :: Notification,
    -- | The @accountId@ that is associated with the budget whose notification you want to delete.
    accountId :: Lude.Text,
    -- | The name of the budget whose notification you want to delete.
    budgetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotification' with the minimum fields required to make a request.
--
-- * 'notification' - The notification that you want to delete.
-- * 'accountId' - The @accountId@ that is associated with the budget whose notification you want to delete.
-- * 'budgetName' - The name of the budget whose notification you want to delete.
mkDeleteNotification ::
  -- | 'notification'
  Notification ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  DeleteNotification
mkDeleteNotification pNotification_ pAccountId_ pBudgetName_ =
  DeleteNotification'
    { notification = pNotification_,
      accountId = pAccountId_,
      budgetName = pBudgetName_
    }

-- | The notification that you want to delete.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnNotification :: Lens.Lens' DeleteNotification Notification
dnNotification = Lens.lens (notification :: DeleteNotification -> Notification) (\s a -> s {notification = a} :: DeleteNotification)
{-# DEPRECATED dnNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The @accountId@ that is associated with the budget whose notification you want to delete.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnAccountId :: Lens.Lens' DeleteNotification Lude.Text
dnAccountId = Lens.lens (accountId :: DeleteNotification -> Lude.Text) (\s a -> s {accountId = a} :: DeleteNotification)
{-# DEPRECATED dnAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget whose notification you want to delete.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnBudgetName :: Lens.Lens' DeleteNotification Lude.Text
dnBudgetName = Lens.lens (budgetName :: DeleteNotification -> Lude.Text) (\s a -> s {budgetName = a} :: DeleteNotification)
{-# DEPRECATED dnBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Lude.AWSRequest DeleteNotification where
  type Rs DeleteNotification = DeleteNotificationResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteNotificationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteNotification where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.DeleteNotification" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteNotification where
  toJSON DeleteNotification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Notification" Lude..= notification),
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName)
          ]
      )

instance Lude.ToPath DeleteNotification where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNotification where
  toQuery = Lude.const Lude.mempty

-- | Response of DeleteNotification
--
-- /See:/ 'mkDeleteNotificationResponse' smart constructor.
newtype DeleteNotificationResponse = DeleteNotificationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotificationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteNotificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteNotificationResponse
mkDeleteNotificationResponse pResponseStatus_ =
  DeleteNotificationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrsResponseStatus :: Lens.Lens' DeleteNotificationResponse Lude.Int
dnrsResponseStatus = Lens.lens (responseStatus :: DeleteNotificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteNotificationResponse)
{-# DEPRECATED dnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
