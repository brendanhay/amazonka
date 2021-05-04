{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DeleteNotification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a notification.
--
-- Deleting a notification also deletes the subscribers that are associated
-- with the notification.
module Network.AWS.Budgets.DeleteNotification
  ( -- * Creating a Request
    DeleteNotification (..),
    newDeleteNotification,

    -- * Request Lenses
    deleteNotification_accountId,
    deleteNotification_budgetName,
    deleteNotification_notification,

    -- * Destructuring the Response
    DeleteNotificationResponse (..),
    newDeleteNotificationResponse,

    -- * Response Lenses
    deleteNotificationResponse_httpStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DeleteNotification
--
-- /See:/ 'newDeleteNotification' smart constructor.
data DeleteNotification = DeleteNotification'
  { -- | The @accountId@ that is associated with the budget whose notification
    -- you want to delete.
    accountId :: Prelude.Text,
    -- | The name of the budget whose notification you want to delete.
    budgetName :: Prelude.Text,
    -- | The notification that you want to delete.
    notification :: Notification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteNotification_accountId' - The @accountId@ that is associated with the budget whose notification
-- you want to delete.
--
-- 'budgetName', 'deleteNotification_budgetName' - The name of the budget whose notification you want to delete.
--
-- 'notification', 'deleteNotification_notification' - The notification that you want to delete.
newDeleteNotification ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'notification'
  Notification ->
  DeleteNotification
newDeleteNotification
  pAccountId_
  pBudgetName_
  pNotification_ =
    DeleteNotification'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        notification = pNotification_
      }

-- | The @accountId@ that is associated with the budget whose notification
-- you want to delete.
deleteNotification_accountId :: Lens.Lens' DeleteNotification Prelude.Text
deleteNotification_accountId = Lens.lens (\DeleteNotification' {accountId} -> accountId) (\s@DeleteNotification' {} a -> s {accountId = a} :: DeleteNotification)

-- | The name of the budget whose notification you want to delete.
deleteNotification_budgetName :: Lens.Lens' DeleteNotification Prelude.Text
deleteNotification_budgetName = Lens.lens (\DeleteNotification' {budgetName} -> budgetName) (\s@DeleteNotification' {} a -> s {budgetName = a} :: DeleteNotification)

-- | The notification that you want to delete.
deleteNotification_notification :: Lens.Lens' DeleteNotification Notification
deleteNotification_notification = Lens.lens (\DeleteNotification' {notification} -> notification) (\s@DeleteNotification' {} a -> s {notification = a} :: DeleteNotification)

instance Prelude.AWSRequest DeleteNotification where
  type
    Rs DeleteNotification =
      DeleteNotificationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNotificationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNotification

instance Prelude.NFData DeleteNotification

instance Prelude.ToHeaders DeleteNotification where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSBudgetServiceGateway.DeleteNotification" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteNotification where
  toJSON DeleteNotification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Prelude..= accountId),
            Prelude.Just ("BudgetName" Prelude..= budgetName),
            Prelude.Just
              ("Notification" Prelude..= notification)
          ]
      )

instance Prelude.ToPath DeleteNotification where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteNotification where
  toQuery = Prelude.const Prelude.mempty

-- | Response of DeleteNotification
--
-- /See:/ 'newDeleteNotificationResponse' smart constructor.
data DeleteNotificationResponse = DeleteNotificationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteNotificationResponse_httpStatus' - The response's http status code.
newDeleteNotificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNotificationResponse
newDeleteNotificationResponse pHttpStatus_ =
  DeleteNotificationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteNotificationResponse_httpStatus :: Lens.Lens' DeleteNotificationResponse Prelude.Int
deleteNotificationResponse_httpStatus = Lens.lens (\DeleteNotificationResponse' {httpStatus} -> httpStatus) (\s@DeleteNotificationResponse' {} a -> s {httpStatus = a} :: DeleteNotificationResponse)

instance Prelude.NFData DeleteNotificationResponse
