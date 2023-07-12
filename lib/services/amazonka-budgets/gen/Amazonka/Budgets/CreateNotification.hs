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
-- Module      : Amazonka.Budgets.CreateNotification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a notification. You must create the budget before you create the
-- associated notification.
module Amazonka.Budgets.CreateNotification
  ( -- * Creating a Request
    CreateNotification (..),
    newCreateNotification,

    -- * Request Lenses
    createNotification_accountId,
    createNotification_budgetName,
    createNotification_notification,
    createNotification_subscribers,

    -- * Destructuring the Response
    CreateNotificationResponse (..),
    newCreateNotificationResponse,

    -- * Response Lenses
    createNotificationResponse_httpStatus,
  )
where

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request of CreateNotification
--
-- /See:/ 'newCreateNotification' smart constructor.
data CreateNotification = CreateNotification'
  { -- | The @accountId@ that is associated with the budget that you want to
    -- create a notification for.
    accountId :: Prelude.Text,
    -- | The name of the budget that you want Amazon Web Services to notify you
    -- about. Budget names must be unique within an account.
    budgetName :: Prelude.Text,
    -- | The notification that you want to create.
    notification :: Notification,
    -- | A list of subscribers that you want to associate with the notification.
    -- Each notification can have one SNS subscriber and up to 10 email
    -- subscribers.
    subscribers :: Prelude.NonEmpty Subscriber
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'createNotification_accountId' - The @accountId@ that is associated with the budget that you want to
-- create a notification for.
--
-- 'budgetName', 'createNotification_budgetName' - The name of the budget that you want Amazon Web Services to notify you
-- about. Budget names must be unique within an account.
--
-- 'notification', 'createNotification_notification' - The notification that you want to create.
--
-- 'subscribers', 'createNotification_subscribers' - A list of subscribers that you want to associate with the notification.
-- Each notification can have one SNS subscriber and up to 10 email
-- subscribers.
newCreateNotification ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'notification'
  Notification ->
  -- | 'subscribers'
  Prelude.NonEmpty Subscriber ->
  CreateNotification
newCreateNotification
  pAccountId_
  pBudgetName_
  pNotification_
  pSubscribers_ =
    CreateNotification'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        notification = pNotification_,
        subscribers = Lens.coerced Lens.# pSubscribers_
      }

-- | The @accountId@ that is associated with the budget that you want to
-- create a notification for.
createNotification_accountId :: Lens.Lens' CreateNotification Prelude.Text
createNotification_accountId = Lens.lens (\CreateNotification' {accountId} -> accountId) (\s@CreateNotification' {} a -> s {accountId = a} :: CreateNotification)

-- | The name of the budget that you want Amazon Web Services to notify you
-- about. Budget names must be unique within an account.
createNotification_budgetName :: Lens.Lens' CreateNotification Prelude.Text
createNotification_budgetName = Lens.lens (\CreateNotification' {budgetName} -> budgetName) (\s@CreateNotification' {} a -> s {budgetName = a} :: CreateNotification)

-- | The notification that you want to create.
createNotification_notification :: Lens.Lens' CreateNotification Notification
createNotification_notification = Lens.lens (\CreateNotification' {notification} -> notification) (\s@CreateNotification' {} a -> s {notification = a} :: CreateNotification)

-- | A list of subscribers that you want to associate with the notification.
-- Each notification can have one SNS subscriber and up to 10 email
-- subscribers.
createNotification_subscribers :: Lens.Lens' CreateNotification (Prelude.NonEmpty Subscriber)
createNotification_subscribers = Lens.lens (\CreateNotification' {subscribers} -> subscribers) (\s@CreateNotification' {} a -> s {subscribers = a} :: CreateNotification) Prelude.. Lens.coerced

instance Core.AWSRequest CreateNotification where
  type
    AWSResponse CreateNotification =
      CreateNotificationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateNotificationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNotification where
  hashWithSalt _salt CreateNotification' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` subscribers

instance Prelude.NFData CreateNotification where
  rnf CreateNotification' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf notification
      `Prelude.seq` Prelude.rnf subscribers

instance Data.ToHeaders CreateNotification where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.CreateNotification" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateNotification where
  toJSON CreateNotification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName),
            Prelude.Just ("Notification" Data..= notification),
            Prelude.Just ("Subscribers" Data..= subscribers)
          ]
      )

instance Data.ToPath CreateNotification where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateNotification where
  toQuery = Prelude.const Prelude.mempty

-- | Response of CreateNotification
--
-- /See:/ 'newCreateNotificationResponse' smart constructor.
data CreateNotificationResponse = CreateNotificationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createNotificationResponse_httpStatus' - The response's http status code.
newCreateNotificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNotificationResponse
newCreateNotificationResponse pHttpStatus_ =
  CreateNotificationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createNotificationResponse_httpStatus :: Lens.Lens' CreateNotificationResponse Prelude.Int
createNotificationResponse_httpStatus = Lens.lens (\CreateNotificationResponse' {httpStatus} -> httpStatus) (\s@CreateNotificationResponse' {} a -> s {httpStatus = a} :: CreateNotificationResponse)

instance Prelude.NFData CreateNotificationResponse where
  rnf CreateNotificationResponse' {..} =
    Prelude.rnf httpStatus
