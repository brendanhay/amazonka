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
-- Module      : Amazonka.Budgets.DeleteSubscriber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscriber.
--
-- Deleting the last subscriber to a notification also deletes the
-- notification.
module Amazonka.Budgets.DeleteSubscriber
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

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request of DeleteSubscriber
--
-- /See:/ 'newDeleteSubscriber' smart constructor.
data DeleteSubscriber = DeleteSubscriber'
  { -- | The @accountId@ that is associated with the budget whose subscriber you
    -- want to delete.
    accountId :: Prelude.Text,
    -- | The name of the budget whose subscriber you want to delete.
    budgetName :: Prelude.Text,
    -- | The notification whose subscriber you want to delete.
    notification :: Notification,
    -- | The subscriber that you want to delete.
    subscriber :: Subscriber
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
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
deleteSubscriber_accountId :: Lens.Lens' DeleteSubscriber Prelude.Text
deleteSubscriber_accountId = Lens.lens (\DeleteSubscriber' {accountId} -> accountId) (\s@DeleteSubscriber' {} a -> s {accountId = a} :: DeleteSubscriber)

-- | The name of the budget whose subscriber you want to delete.
deleteSubscriber_budgetName :: Lens.Lens' DeleteSubscriber Prelude.Text
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSubscriberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSubscriber where
  hashWithSalt _salt DeleteSubscriber' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` subscriber

instance Prelude.NFData DeleteSubscriber where
  rnf DeleteSubscriber' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf notification
      `Prelude.seq` Prelude.rnf subscriber

instance Data.ToHeaders DeleteSubscriber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.DeleteSubscriber" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSubscriber where
  toJSON DeleteSubscriber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName),
            Prelude.Just ("Notification" Data..= notification),
            Prelude.Just ("Subscriber" Data..= subscriber)
          ]
      )

instance Data.ToPath DeleteSubscriber where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSubscriber where
  toQuery = Prelude.const Prelude.mempty

-- | Response of DeleteSubscriber
--
-- /See:/ 'newDeleteSubscriberResponse' smart constructor.
data DeleteSubscriberResponse = DeleteSubscriberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteSubscriberResponse
newDeleteSubscriberResponse pHttpStatus_ =
  DeleteSubscriberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSubscriberResponse_httpStatus :: Lens.Lens' DeleteSubscriberResponse Prelude.Int
deleteSubscriberResponse_httpStatus = Lens.lens (\DeleteSubscriberResponse' {httpStatus} -> httpStatus) (\s@DeleteSubscriberResponse' {} a -> s {httpStatus = a} :: DeleteSubscriberResponse)

instance Prelude.NFData DeleteSubscriberResponse where
  rnf DeleteSubscriberResponse' {..} =
    Prelude.rnf httpStatus
