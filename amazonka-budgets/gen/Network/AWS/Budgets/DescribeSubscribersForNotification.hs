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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DescribeSubscribersForNotification
--
-- /See:/ 'newDescribeSubscribersForNotification' smart constructor.
data DescribeSubscribersForNotification = DescribeSubscribersForNotification'
  { -- | The pagination token that you include in your request to indicate the
    -- next set of results that you want to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional integer that represents how many entries a paginated
    -- response contains. The maximum is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The @accountId@ that is associated with the budget whose subscribers you
    -- want descriptions of.
    accountId :: Prelude.Text,
    -- | The name of the budget whose subscribers you want descriptions of.
    budgetName :: Prelude.Text,
    -- | The notification whose subscribers you want to list.
    notification :: Notification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'notification'
  Notification ->
  DescribeSubscribersForNotification
newDescribeSubscribersForNotification
  pAccountId_
  pBudgetName_
  pNotification_ =
    DescribeSubscribersForNotification'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        notification = pNotification_
      }

-- | The pagination token that you include in your request to indicate the
-- next set of results that you want to retrieve.
describeSubscribersForNotification_nextToken :: Lens.Lens' DescribeSubscribersForNotification (Prelude.Maybe Prelude.Text)
describeSubscribersForNotification_nextToken = Lens.lens (\DescribeSubscribersForNotification' {nextToken} -> nextToken) (\s@DescribeSubscribersForNotification' {} a -> s {nextToken = a} :: DescribeSubscribersForNotification)

-- | An optional integer that represents how many entries a paginated
-- response contains. The maximum is 100.
describeSubscribersForNotification_maxResults :: Lens.Lens' DescribeSubscribersForNotification (Prelude.Maybe Prelude.Natural)
describeSubscribersForNotification_maxResults = Lens.lens (\DescribeSubscribersForNotification' {maxResults} -> maxResults) (\s@DescribeSubscribersForNotification' {} a -> s {maxResults = a} :: DescribeSubscribersForNotification)

-- | The @accountId@ that is associated with the budget whose subscribers you
-- want descriptions of.
describeSubscribersForNotification_accountId :: Lens.Lens' DescribeSubscribersForNotification Prelude.Text
describeSubscribersForNotification_accountId = Lens.lens (\DescribeSubscribersForNotification' {accountId} -> accountId) (\s@DescribeSubscribersForNotification' {} a -> s {accountId = a} :: DescribeSubscribersForNotification)

-- | The name of the budget whose subscribers you want descriptions of.
describeSubscribersForNotification_budgetName :: Lens.Lens' DescribeSubscribersForNotification Prelude.Text
describeSubscribersForNotification_budgetName = Lens.lens (\DescribeSubscribersForNotification' {budgetName} -> budgetName) (\s@DescribeSubscribersForNotification' {} a -> s {budgetName = a} :: DescribeSubscribersForNotification)

-- | The notification whose subscribers you want to list.
describeSubscribersForNotification_notification :: Lens.Lens' DescribeSubscribersForNotification Notification
describeSubscribersForNotification_notification = Lens.lens (\DescribeSubscribersForNotification' {notification} -> notification) (\s@DescribeSubscribersForNotification' {} a -> s {notification = a} :: DescribeSubscribersForNotification)

instance
  Pager.AWSPager
    DescribeSubscribersForNotification
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeSubscribersForNotificationResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeSubscribersForNotificationResponse_subscribers
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeSubscribersForNotification_nextToken
          Lens..~ rs
          Lens.^? describeSubscribersForNotificationResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeSubscribersForNotification
  where
  type
    Rs DescribeSubscribersForNotification =
      DescribeSubscribersForNotificationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubscribersForNotificationResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
              Prelude.<*> (x Prelude..?> "Subscribers")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSubscribersForNotification

instance
  Prelude.NFData
    DescribeSubscribersForNotification

instance
  Prelude.ToHeaders
    DescribeSubscribersForNotification
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSBudgetServiceGateway.DescribeSubscribersForNotification" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DescribeSubscribersForNotification
  where
  toJSON DescribeSubscribersForNotification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just ("AccountId" Prelude..= accountId),
            Prelude.Just ("BudgetName" Prelude..= budgetName),
            Prelude.Just
              ("Notification" Prelude..= notification)
          ]
      )

instance
  Prelude.ToPath
    DescribeSubscribersForNotification
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeSubscribersForNotification
  where
  toQuery = Prelude.const Prelude.mempty

-- | Response of DescribeSubscribersForNotification
--
-- /See:/ 'newDescribeSubscribersForNotificationResponse' smart constructor.
data DescribeSubscribersForNotificationResponse = DescribeSubscribersForNotificationResponse'
  { -- | The pagination token in the service response that indicates the next set
    -- of results that you can retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of subscribers that are associated with a notification.
    subscribers :: Prelude.Maybe (Prelude.NonEmpty Subscriber),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeSubscribersForNotificationResponse
newDescribeSubscribersForNotificationResponse
  pHttpStatus_ =
    DescribeSubscribersForNotificationResponse'
      { nextToken =
          Prelude.Nothing,
        subscribers = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token in the service response that indicates the next set
-- of results that you can retrieve.
describeSubscribersForNotificationResponse_nextToken :: Lens.Lens' DescribeSubscribersForNotificationResponse (Prelude.Maybe Prelude.Text)
describeSubscribersForNotificationResponse_nextToken = Lens.lens (\DescribeSubscribersForNotificationResponse' {nextToken} -> nextToken) (\s@DescribeSubscribersForNotificationResponse' {} a -> s {nextToken = a} :: DescribeSubscribersForNotificationResponse)

-- | A list of subscribers that are associated with a notification.
describeSubscribersForNotificationResponse_subscribers :: Lens.Lens' DescribeSubscribersForNotificationResponse (Prelude.Maybe (Prelude.NonEmpty Subscriber))
describeSubscribersForNotificationResponse_subscribers = Lens.lens (\DescribeSubscribersForNotificationResponse' {subscribers} -> subscribers) (\s@DescribeSubscribersForNotificationResponse' {} a -> s {subscribers = a} :: DescribeSubscribersForNotificationResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeSubscribersForNotificationResponse_httpStatus :: Lens.Lens' DescribeSubscribersForNotificationResponse Prelude.Int
describeSubscribersForNotificationResponse_httpStatus = Lens.lens (\DescribeSubscribersForNotificationResponse' {httpStatus} -> httpStatus) (\s@DescribeSubscribersForNotificationResponse' {} a -> s {httpStatus = a} :: DescribeSubscribersForNotificationResponse)

instance
  Prelude.NFData
    DescribeSubscribersForNotificationResponse
