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
-- Module      : Network.AWS.Budgets.DeleteSubscriber
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscriber.
--
-- Deleting the last subscriber to a notification also deletes the
-- notification.
module Network.AWS.Budgets.DeleteSubscriber
  ( -- * Creating a Request
    DeleteSubscriber (..),
    newDeleteSubscriber,

    -- * Request Lenses
    deleteSubscriber_accountId,
    deleteSubscriber_budgetName,
    deleteSubscriber_notification,
    deleteSubscriber_subscriber,

    -- * Destructuring the Response
    DeleteSubscriberResponse (..),
    newDeleteSubscriberResponse,

    -- * Response Lenses
    deleteSubscriberResponse_httpStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DeleteSubscriber
--
-- /See:/ 'newDeleteSubscriber' smart constructor.
data DeleteSubscriber = DeleteSubscriber'
  { -- | The @accountId@ that is associated with the budget whose subscriber you
    -- want to delete.
    accountId :: Core.Text,
    -- | The name of the budget whose subscriber you want to delete.
    budgetName :: Core.Text,
    -- | The notification whose subscriber you want to delete.
    notification :: Notification,
    -- | The subscriber that you want to delete.
    subscriber :: Subscriber
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSubscriber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteSubscriber_accountId' - The @accountId@ that is associated with the budget whose subscriber you
-- want to delete.
--
-- 'budgetName', 'deleteSubscriber_budgetName' - The name of the budget whose subscriber you want to delete.
--
-- 'notification', 'deleteSubscriber_notification' - The notification whose subscriber you want to delete.
--
-- 'subscriber', 'deleteSubscriber_subscriber' - The subscriber that you want to delete.
newDeleteSubscriber ::
  -- | 'accountId'
  Core.Text ->
  -- | 'budgetName'
  Core.Text ->
  -- | 'notification'
  Notification ->
  -- | 'subscriber'
  Subscriber ->
  DeleteSubscriber
newDeleteSubscriber
  pAccountId_
  pBudgetName_
  pNotification_
  pSubscriber_ =
    DeleteSubscriber'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        notification = pNotification_,
        subscriber = pSubscriber_
      }

-- | The @accountId@ that is associated with the budget whose subscriber you
-- want to delete.
deleteSubscriber_accountId :: Lens.Lens' DeleteSubscriber Core.Text
deleteSubscriber_accountId = Lens.lens (\DeleteSubscriber' {accountId} -> accountId) (\s@DeleteSubscriber' {} a -> s {accountId = a} :: DeleteSubscriber)

-- | The name of the budget whose subscriber you want to delete.
deleteSubscriber_budgetName :: Lens.Lens' DeleteSubscriber Core.Text
deleteSubscriber_budgetName = Lens.lens (\DeleteSubscriber' {budgetName} -> budgetName) (\s@DeleteSubscriber' {} a -> s {budgetName = a} :: DeleteSubscriber)

-- | The notification whose subscriber you want to delete.
deleteSubscriber_notification :: Lens.Lens' DeleteSubscriber Notification
deleteSubscriber_notification = Lens.lens (\DeleteSubscriber' {notification} -> notification) (\s@DeleteSubscriber' {} a -> s {notification = a} :: DeleteSubscriber)

-- | The subscriber that you want to delete.
deleteSubscriber_subscriber :: Lens.Lens' DeleteSubscriber Subscriber
deleteSubscriber_subscriber = Lens.lens (\DeleteSubscriber' {subscriber} -> subscriber) (\s@DeleteSubscriber' {} a -> s {subscriber = a} :: DeleteSubscriber)

instance Core.AWSRequest DeleteSubscriber where
  type
    AWSResponse DeleteSubscriber =
      DeleteSubscriberResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSubscriberResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSubscriber

instance Core.NFData DeleteSubscriber

instance Core.ToHeaders DeleteSubscriber where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.DeleteSubscriber" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteSubscriber where
  toJSON DeleteSubscriber' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("Notification" Core..= notification),
            Core.Just ("Subscriber" Core..= subscriber)
          ]
      )

instance Core.ToPath DeleteSubscriber where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSubscriber where
  toQuery = Core.const Core.mempty

-- | Response of DeleteSubscriber
--
-- /See:/ 'newDeleteSubscriberResponse' smart constructor.
data DeleteSubscriberResponse = DeleteSubscriberResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSubscriberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSubscriberResponse_httpStatus' - The response's http status code.
newDeleteSubscriberResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteSubscriberResponse
newDeleteSubscriberResponse pHttpStatus_ =
  DeleteSubscriberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSubscriberResponse_httpStatus :: Lens.Lens' DeleteSubscriberResponse Core.Int
deleteSubscriberResponse_httpStatus = Lens.lens (\DeleteSubscriberResponse' {httpStatus} -> httpStatus) (\s@DeleteSubscriberResponse' {} a -> s {httpStatus = a} :: DeleteSubscriberResponse)

instance Core.NFData DeleteSubscriberResponse
