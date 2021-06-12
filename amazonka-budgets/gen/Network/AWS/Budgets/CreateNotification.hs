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
-- Module      : Network.AWS.Budgets.CreateNotification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a notification. You must create the budget before you create the
-- associated notification.
module Network.AWS.Budgets.CreateNotification
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

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of CreateNotification
--
-- /See:/ 'newCreateNotification' smart constructor.
data CreateNotification = CreateNotification'
  { -- | The @accountId@ that is associated with the budget that you want to
    -- create a notification for.
    accountId :: Core.Text,
    -- | The name of the budget that you want AWS to notify you about. Budget
    -- names must be unique within an account.
    budgetName :: Core.Text,
    -- | The notification that you want to create.
    notification :: Notification,
    -- | A list of subscribers that you want to associate with the notification.
    -- Each notification can have one SNS subscriber and up to 10 email
    -- subscribers.
    subscribers :: Core.NonEmpty Subscriber
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- 'budgetName', 'createNotification_budgetName' - The name of the budget that you want AWS to notify you about. Budget
-- names must be unique within an account.
--
-- 'notification', 'createNotification_notification' - The notification that you want to create.
--
-- 'subscribers', 'createNotification_subscribers' - A list of subscribers that you want to associate with the notification.
-- Each notification can have one SNS subscriber and up to 10 email
-- subscribers.
newCreateNotification ::
  -- | 'accountId'
  Core.Text ->
  -- | 'budgetName'
  Core.Text ->
  -- | 'notification'
  Notification ->
  -- | 'subscribers'
  Core.NonEmpty Subscriber ->
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
        subscribers = Lens._Coerce Lens.# pSubscribers_
      }

-- | The @accountId@ that is associated with the budget that you want to
-- create a notification for.
createNotification_accountId :: Lens.Lens' CreateNotification Core.Text
createNotification_accountId = Lens.lens (\CreateNotification' {accountId} -> accountId) (\s@CreateNotification' {} a -> s {accountId = a} :: CreateNotification)

-- | The name of the budget that you want AWS to notify you about. Budget
-- names must be unique within an account.
createNotification_budgetName :: Lens.Lens' CreateNotification Core.Text
createNotification_budgetName = Lens.lens (\CreateNotification' {budgetName} -> budgetName) (\s@CreateNotification' {} a -> s {budgetName = a} :: CreateNotification)

-- | The notification that you want to create.
createNotification_notification :: Lens.Lens' CreateNotification Notification
createNotification_notification = Lens.lens (\CreateNotification' {notification} -> notification) (\s@CreateNotification' {} a -> s {notification = a} :: CreateNotification)

-- | A list of subscribers that you want to associate with the notification.
-- Each notification can have one SNS subscriber and up to 10 email
-- subscribers.
createNotification_subscribers :: Lens.Lens' CreateNotification (Core.NonEmpty Subscriber)
createNotification_subscribers = Lens.lens (\CreateNotification' {subscribers} -> subscribers) (\s@CreateNotification' {} a -> s {subscribers = a} :: CreateNotification) Core.. Lens._Coerce

instance Core.AWSRequest CreateNotification where
  type
    AWSResponse CreateNotification =
      CreateNotificationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateNotificationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateNotification

instance Core.NFData CreateNotification

instance Core.ToHeaders CreateNotification where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.CreateNotification" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateNotification where
  toJSON CreateNotification' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("Notification" Core..= notification),
            Core.Just ("Subscribers" Core..= subscribers)
          ]
      )

instance Core.ToPath CreateNotification where
  toPath = Core.const "/"

instance Core.ToQuery CreateNotification where
  toQuery = Core.const Core.mempty

-- | Response of CreateNotification
--
-- /See:/ 'newCreateNotificationResponse' smart constructor.
data CreateNotificationResponse = CreateNotificationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateNotificationResponse
newCreateNotificationResponse pHttpStatus_ =
  CreateNotificationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createNotificationResponse_httpStatus :: Lens.Lens' CreateNotificationResponse Core.Int
createNotificationResponse_httpStatus = Lens.lens (\CreateNotificationResponse' {httpStatus} -> httpStatus) (\s@CreateNotificationResponse' {} a -> s {httpStatus = a} :: CreateNotificationResponse)

instance Core.NFData CreateNotificationResponse
