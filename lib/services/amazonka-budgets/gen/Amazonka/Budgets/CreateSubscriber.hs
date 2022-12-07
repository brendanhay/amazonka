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
-- Module      : Amazonka.Budgets.CreateSubscriber
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscriber. You must create the associated budget and
-- notification before you create the subscriber.
module Amazonka.Budgets.CreateSubscriber
  ( -- * Creating a Request
    CreateSubscriber (..),
    newCreateSubscriber,

    -- * Request Lenses
    createSubscriber_accountId,
    createSubscriber_budgetName,
    createSubscriber_notification,
    createSubscriber_subscriber,

    -- * Destructuring the Response
    CreateSubscriberResponse (..),
    newCreateSubscriberResponse,

    -- * Response Lenses
    createSubscriberResponse_httpStatus,
  )
where

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request of CreateSubscriber
--
-- /See:/ 'newCreateSubscriber' smart constructor.
data CreateSubscriber = CreateSubscriber'
  { -- | The @accountId@ that is associated with the budget that you want to
    -- create a subscriber for.
    accountId :: Prelude.Text,
    -- | The name of the budget that you want to subscribe to. Budget names must
    -- be unique within an account.
    budgetName :: Prelude.Text,
    -- | The notification that you want to create a subscriber for.
    notification :: Notification,
    -- | The subscriber that you want to associate with a budget notification.
    subscriber :: Subscriber
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'createSubscriber_accountId' - The @accountId@ that is associated with the budget that you want to
-- create a subscriber for.
--
-- 'budgetName', 'createSubscriber_budgetName' - The name of the budget that you want to subscribe to. Budget names must
-- be unique within an account.
--
-- 'notification', 'createSubscriber_notification' - The notification that you want to create a subscriber for.
--
-- 'subscriber', 'createSubscriber_subscriber' - The subscriber that you want to associate with a budget notification.
newCreateSubscriber ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'notification'
  Notification ->
  -- | 'subscriber'
  Subscriber ->
  CreateSubscriber
newCreateSubscriber
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

-- | The @accountId@ that is associated with the budget that you want to
-- create a subscriber for.
createSubscriber_accountId :: Lens.Lens' CreateSubscriber Prelude.Text
createSubscriber_accountId = Lens.lens (\CreateSubscriber' {accountId} -> accountId) (\s@CreateSubscriber' {} a -> s {accountId = a} :: CreateSubscriber)

-- | The name of the budget that you want to subscribe to. Budget names must
-- be unique within an account.
createSubscriber_budgetName :: Lens.Lens' CreateSubscriber Prelude.Text
createSubscriber_budgetName = Lens.lens (\CreateSubscriber' {budgetName} -> budgetName) (\s@CreateSubscriber' {} a -> s {budgetName = a} :: CreateSubscriber)

-- | The notification that you want to create a subscriber for.
createSubscriber_notification :: Lens.Lens' CreateSubscriber Notification
createSubscriber_notification = Lens.lens (\CreateSubscriber' {notification} -> notification) (\s@CreateSubscriber' {} a -> s {notification = a} :: CreateSubscriber)

-- | The subscriber that you want to associate with a budget notification.
createSubscriber_subscriber :: Lens.Lens' CreateSubscriber Subscriber
createSubscriber_subscriber = Lens.lens (\CreateSubscriber' {subscriber} -> subscriber) (\s@CreateSubscriber' {} a -> s {subscriber = a} :: CreateSubscriber)

instance Core.AWSRequest CreateSubscriber where
  type
    AWSResponse CreateSubscriber =
      CreateSubscriberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateSubscriberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSubscriber where
  hashWithSalt _salt CreateSubscriber' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` subscriber

instance Prelude.NFData CreateSubscriber where
  rnf CreateSubscriber' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf notification
      `Prelude.seq` Prelude.rnf subscriber

instance Data.ToHeaders CreateSubscriber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.CreateSubscriber" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSubscriber where
  toJSON CreateSubscriber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName),
            Prelude.Just ("Notification" Data..= notification),
            Prelude.Just ("Subscriber" Data..= subscriber)
          ]
      )

instance Data.ToPath CreateSubscriber where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSubscriber where
  toQuery = Prelude.const Prelude.mempty

-- | Response of CreateSubscriber
--
-- /See:/ 'newCreateSubscriberResponse' smart constructor.
data CreateSubscriberResponse = CreateSubscriberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSubscriberResponse_httpStatus' - The response's http status code.
newCreateSubscriberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSubscriberResponse
newCreateSubscriberResponse pHttpStatus_ =
  CreateSubscriberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createSubscriberResponse_httpStatus :: Lens.Lens' CreateSubscriberResponse Prelude.Int
createSubscriberResponse_httpStatus = Lens.lens (\CreateSubscriberResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriberResponse' {} a -> s {httpStatus = a} :: CreateSubscriberResponse)

instance Prelude.NFData CreateSubscriberResponse where
  rnf CreateSubscriberResponse' {..} =
    Prelude.rnf httpStatus
