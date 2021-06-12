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
-- Module      : Network.AWS.Budgets.DescribeSubscribersForNotification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the subscribers that are associated with a notification.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeSubscribersForNotification
  ( -- * Creating a Request
    DescribeSubscribersForNotification (..),
    newDescribeSubscribersForNotification,

    -- * Request Lenses
    describeSubscribersForNotification_nextToken,
    describeSubscribersForNotification_maxResults,
    describeSubscribersForNotification_accountId,
    describeSubscribersForNotification_budgetName,
    describeSubscribersForNotification_notification,

    -- * Destructuring the Response
    DescribeSubscribersForNotificationResponse (..),
    newDescribeSubscribersForNotificationResponse,

    -- * Response Lenses
    describeSubscribersForNotificationResponse_nextToken,
    describeSubscribersForNotificationResponse_subscribers,
    describeSubscribersForNotificationResponse_httpStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DescribeSubscribersForNotification
--
-- /See:/ 'newDescribeSubscribersForNotification' smart constructor.
data DescribeSubscribersForNotification = DescribeSubscribersForNotification'
  { -- | The pagination token that you include in your request to indicate the
    -- next set of results that you want to retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | An optional integer that represents how many entries a paginated
    -- response contains. The maximum is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The @accountId@ that is associated with the budget whose subscribers you
    -- want descriptions of.
    accountId :: Core.Text,
    -- | The name of the budget whose subscribers you want descriptions of.
    budgetName :: Core.Text,
    -- | The notification whose subscribers you want to list.
    notification :: Notification
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSubscribersForNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSubscribersForNotification_nextToken' - The pagination token that you include in your request to indicate the
-- next set of results that you want to retrieve.
--
-- 'maxResults', 'describeSubscribersForNotification_maxResults' - An optional integer that represents how many entries a paginated
-- response contains. The maximum is 100.
--
-- 'accountId', 'describeSubscribersForNotification_accountId' - The @accountId@ that is associated with the budget whose subscribers you
-- want descriptions of.
--
-- 'budgetName', 'describeSubscribersForNotification_budgetName' - The name of the budget whose subscribers you want descriptions of.
--
-- 'notification', 'describeSubscribersForNotification_notification' - The notification whose subscribers you want to list.
newDescribeSubscribersForNotification ::
  -- | 'accountId'
  Core.Text ->
  -- | 'budgetName'
  Core.Text ->
  -- | 'notification'
  Notification ->
  DescribeSubscribersForNotification
newDescribeSubscribersForNotification
  pAccountId_
  pBudgetName_
  pNotification_ =
    DescribeSubscribersForNotification'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        notification = pNotification_
      }

-- | The pagination token that you include in your request to indicate the
-- next set of results that you want to retrieve.
describeSubscribersForNotification_nextToken :: Lens.Lens' DescribeSubscribersForNotification (Core.Maybe Core.Text)
describeSubscribersForNotification_nextToken = Lens.lens (\DescribeSubscribersForNotification' {nextToken} -> nextToken) (\s@DescribeSubscribersForNotification' {} a -> s {nextToken = a} :: DescribeSubscribersForNotification)

-- | An optional integer that represents how many entries a paginated
-- response contains. The maximum is 100.
describeSubscribersForNotification_maxResults :: Lens.Lens' DescribeSubscribersForNotification (Core.Maybe Core.Natural)
describeSubscribersForNotification_maxResults = Lens.lens (\DescribeSubscribersForNotification' {maxResults} -> maxResults) (\s@DescribeSubscribersForNotification' {} a -> s {maxResults = a} :: DescribeSubscribersForNotification)

-- | The @accountId@ that is associated with the budget whose subscribers you
-- want descriptions of.
describeSubscribersForNotification_accountId :: Lens.Lens' DescribeSubscribersForNotification Core.Text
describeSubscribersForNotification_accountId = Lens.lens (\DescribeSubscribersForNotification' {accountId} -> accountId) (\s@DescribeSubscribersForNotification' {} a -> s {accountId = a} :: DescribeSubscribersForNotification)

-- | The name of the budget whose subscribers you want descriptions of.
describeSubscribersForNotification_budgetName :: Lens.Lens' DescribeSubscribersForNotification Core.Text
describeSubscribersForNotification_budgetName = Lens.lens (\DescribeSubscribersForNotification' {budgetName} -> budgetName) (\s@DescribeSubscribersForNotification' {} a -> s {budgetName = a} :: DescribeSubscribersForNotification)

-- | The notification whose subscribers you want to list.
describeSubscribersForNotification_notification :: Lens.Lens' DescribeSubscribersForNotification Notification
describeSubscribersForNotification_notification = Lens.lens (\DescribeSubscribersForNotification' {notification} -> notification) (\s@DescribeSubscribersForNotification' {} a -> s {notification = a} :: DescribeSubscribersForNotification)

instance
  Core.AWSPager
    DescribeSubscribersForNotification
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSubscribersForNotificationResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSubscribersForNotificationResponse_subscribers
              Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSubscribersForNotification_nextToken
          Lens..~ rs
          Lens.^? describeSubscribersForNotificationResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeSubscribersForNotification
  where
  type
    AWSResponse DescribeSubscribersForNotification =
      DescribeSubscribersForNotificationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubscribersForNotificationResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Subscribers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeSubscribersForNotification

instance
  Core.NFData
    DescribeSubscribersForNotification

instance
  Core.ToHeaders
    DescribeSubscribersForNotification
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.DescribeSubscribersForNotification" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeSubscribersForNotification
  where
  toJSON DescribeSubscribersForNotification' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("Notification" Core..= notification)
          ]
      )

instance
  Core.ToPath
    DescribeSubscribersForNotification
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeSubscribersForNotification
  where
  toQuery = Core.const Core.mempty

-- | Response of DescribeSubscribersForNotification
--
-- /See:/ 'newDescribeSubscribersForNotificationResponse' smart constructor.
data DescribeSubscribersForNotificationResponse = DescribeSubscribersForNotificationResponse'
  { -- | The pagination token in the service response that indicates the next set
    -- of results that you can retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of subscribers that are associated with a notification.
    subscribers :: Core.Maybe (Core.NonEmpty Subscriber),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSubscribersForNotificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSubscribersForNotificationResponse_nextToken' - The pagination token in the service response that indicates the next set
-- of results that you can retrieve.
--
-- 'subscribers', 'describeSubscribersForNotificationResponse_subscribers' - A list of subscribers that are associated with a notification.
--
-- 'httpStatus', 'describeSubscribersForNotificationResponse_httpStatus' - The response's http status code.
newDescribeSubscribersForNotificationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSubscribersForNotificationResponse
newDescribeSubscribersForNotificationResponse
  pHttpStatus_ =
    DescribeSubscribersForNotificationResponse'
      { nextToken =
          Core.Nothing,
        subscribers = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token in the service response that indicates the next set
-- of results that you can retrieve.
describeSubscribersForNotificationResponse_nextToken :: Lens.Lens' DescribeSubscribersForNotificationResponse (Core.Maybe Core.Text)
describeSubscribersForNotificationResponse_nextToken = Lens.lens (\DescribeSubscribersForNotificationResponse' {nextToken} -> nextToken) (\s@DescribeSubscribersForNotificationResponse' {} a -> s {nextToken = a} :: DescribeSubscribersForNotificationResponse)

-- | A list of subscribers that are associated with a notification.
describeSubscribersForNotificationResponse_subscribers :: Lens.Lens' DescribeSubscribersForNotificationResponse (Core.Maybe (Core.NonEmpty Subscriber))
describeSubscribersForNotificationResponse_subscribers = Lens.lens (\DescribeSubscribersForNotificationResponse' {subscribers} -> subscribers) (\s@DescribeSubscribersForNotificationResponse' {} a -> s {subscribers = a} :: DescribeSubscribersForNotificationResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSubscribersForNotificationResponse_httpStatus :: Lens.Lens' DescribeSubscribersForNotificationResponse Core.Int
describeSubscribersForNotificationResponse_httpStatus = Lens.lens (\DescribeSubscribersForNotificationResponse' {httpStatus} -> httpStatus) (\s@DescribeSubscribersForNotificationResponse' {} a -> s {httpStatus = a} :: DescribeSubscribersForNotificationResponse)

instance
  Core.NFData
    DescribeSubscribersForNotificationResponse
