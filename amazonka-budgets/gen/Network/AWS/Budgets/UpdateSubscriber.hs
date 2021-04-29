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
-- Module      : Network.AWS.Budgets.UpdateSubscriber
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a subscriber.
module Network.AWS.Budgets.UpdateSubscriber
  ( -- * Creating a Request
    UpdateSubscriber (..),
    newUpdateSubscriber,

    -- * Request Lenses
    updateSubscriber_accountId,
    updateSubscriber_budgetName,
    updateSubscriber_notification,
    updateSubscriber_oldSubscriber,
    updateSubscriber_newSubscriber,

    -- * Destructuring the Response
    UpdateSubscriberResponse (..),
    newUpdateSubscriberResponse,

    -- * Response Lenses
    updateSubscriberResponse_httpStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of UpdateSubscriber
--
-- /See:/ 'newUpdateSubscriber' smart constructor.
data UpdateSubscriber = UpdateSubscriber'
  { -- | The @accountId@ that is associated with the budget whose subscriber you
    -- want to update.
    accountId :: Prelude.Text,
    -- | The name of the budget whose subscriber you want to update.
    budgetName :: Prelude.Text,
    -- | The notification whose subscriber you want to update.
    notification :: Notification,
    -- | The previous subscriber that is associated with a budget notification.
    oldSubscriber :: Subscriber,
    -- | The updated subscriber that is associated with a budget notification.
    newSubscriber' :: Subscriber
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubscriber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'updateSubscriber_accountId' - The @accountId@ that is associated with the budget whose subscriber you
-- want to update.
--
-- 'budgetName', 'updateSubscriber_budgetName' - The name of the budget whose subscriber you want to update.
--
-- 'notification', 'updateSubscriber_notification' - The notification whose subscriber you want to update.
--
-- 'oldSubscriber', 'updateSubscriber_oldSubscriber' - The previous subscriber that is associated with a budget notification.
--
-- 'newSubscriber'', 'updateSubscriber_newSubscriber' - The updated subscriber that is associated with a budget notification.
newUpdateSubscriber ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'notification'
  Notification ->
  -- | 'oldSubscriber'
  Subscriber ->
  -- | 'newSubscriber''
  Subscriber ->
  UpdateSubscriber
newUpdateSubscriber
  pAccountId_
  pBudgetName_
  pNotification_
  pOldSubscriber_
  pNewSubscriber_ =
    UpdateSubscriber'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        notification = pNotification_,
        oldSubscriber = pOldSubscriber_,
        newSubscriber' = pNewSubscriber_
      }

-- | The @accountId@ that is associated with the budget whose subscriber you
-- want to update.
updateSubscriber_accountId :: Lens.Lens' UpdateSubscriber Prelude.Text
updateSubscriber_accountId = Lens.lens (\UpdateSubscriber' {accountId} -> accountId) (\s@UpdateSubscriber' {} a -> s {accountId = a} :: UpdateSubscriber)

-- | The name of the budget whose subscriber you want to update.
updateSubscriber_budgetName :: Lens.Lens' UpdateSubscriber Prelude.Text
updateSubscriber_budgetName = Lens.lens (\UpdateSubscriber' {budgetName} -> budgetName) (\s@UpdateSubscriber' {} a -> s {budgetName = a} :: UpdateSubscriber)

-- | The notification whose subscriber you want to update.
updateSubscriber_notification :: Lens.Lens' UpdateSubscriber Notification
updateSubscriber_notification = Lens.lens (\UpdateSubscriber' {notification} -> notification) (\s@UpdateSubscriber' {} a -> s {notification = a} :: UpdateSubscriber)

-- | The previous subscriber that is associated with a budget notification.
updateSubscriber_oldSubscriber :: Lens.Lens' UpdateSubscriber Subscriber
updateSubscriber_oldSubscriber = Lens.lens (\UpdateSubscriber' {oldSubscriber} -> oldSubscriber) (\s@UpdateSubscriber' {} a -> s {oldSubscriber = a} :: UpdateSubscriber)

-- | The updated subscriber that is associated with a budget notification.
updateSubscriber_newSubscriber :: Lens.Lens' UpdateSubscriber Subscriber
updateSubscriber_newSubscriber = Lens.lens (\UpdateSubscriber' {newSubscriber'} -> newSubscriber') (\s@UpdateSubscriber' {} a -> s {newSubscriber' = a} :: UpdateSubscriber)

instance Prelude.AWSRequest UpdateSubscriber where
  type Rs UpdateSubscriber = UpdateSubscriberResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSubscriberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSubscriber

instance Prelude.NFData UpdateSubscriber

instance Prelude.ToHeaders UpdateSubscriber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSBudgetServiceGateway.UpdateSubscriber" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateSubscriber where
  toJSON UpdateSubscriber' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Prelude..= accountId),
            Prelude.Just ("BudgetName" Prelude..= budgetName),
            Prelude.Just
              ("Notification" Prelude..= notification),
            Prelude.Just
              ("OldSubscriber" Prelude..= oldSubscriber),
            Prelude.Just
              ("NewSubscriber" Prelude..= newSubscriber')
          ]
      )

instance Prelude.ToPath UpdateSubscriber where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateSubscriber where
  toQuery = Prelude.const Prelude.mempty

-- | Response of UpdateSubscriber
--
-- /See:/ 'newUpdateSubscriberResponse' smart constructor.
data UpdateSubscriberResponse = UpdateSubscriberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubscriberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSubscriberResponse_httpStatus' - The response's http status code.
newUpdateSubscriberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSubscriberResponse
newUpdateSubscriberResponse pHttpStatus_ =
  UpdateSubscriberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateSubscriberResponse_httpStatus :: Lens.Lens' UpdateSubscriberResponse Prelude.Int
updateSubscriberResponse_httpStatus = Lens.lens (\UpdateSubscriberResponse' {httpStatus} -> httpStatus) (\s@UpdateSubscriberResponse' {} a -> s {httpStatus = a} :: UpdateSubscriberResponse)

instance Prelude.NFData UpdateSubscriberResponse
