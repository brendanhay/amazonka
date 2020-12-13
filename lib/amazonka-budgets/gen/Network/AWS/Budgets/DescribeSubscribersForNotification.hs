{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeSubscribersForNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the subscribers that are associated with a notification.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeSubscribersForNotification
  ( -- * Creating a request
    DescribeSubscribersForNotification (..),
    mkDescribeSubscribersForNotification,

    -- ** Request lenses
    dsfnNotification,
    dsfnAccountId,
    dsfnNextToken,
    dsfnBudgetName,
    dsfnMaxResults,

    -- * Destructuring the response
    DescribeSubscribersForNotificationResponse (..),
    mkDescribeSubscribersForNotificationResponse,

    -- ** Response lenses
    dsfnrsNextToken,
    dsfnrsSubscribers,
    dsfnrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of DescribeSubscribersForNotification
--
-- /See:/ 'mkDescribeSubscribersForNotification' smart constructor.
data DescribeSubscribersForNotification = DescribeSubscribersForNotification'
  { -- | The notification whose subscribers you want to list.
    notification :: Notification,
    -- | The @accountId@ that is associated with the budget whose subscribers you want descriptions of.
    accountId :: Lude.Text,
    -- | The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the budget whose subscribers you want descriptions of.
    budgetName :: Lude.Text,
    -- | An optional integer that represents how many entries a paginated response contains. The maximum is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSubscribersForNotification' with the minimum fields required to make a request.
--
-- * 'notification' - The notification whose subscribers you want to list.
-- * 'accountId' - The @accountId@ that is associated with the budget whose subscribers you want descriptions of.
-- * 'nextToken' - The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
-- * 'budgetName' - The name of the budget whose subscribers you want descriptions of.
-- * 'maxResults' - An optional integer that represents how many entries a paginated response contains. The maximum is 100.
mkDescribeSubscribersForNotification ::
  -- | 'notification'
  Notification ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  DescribeSubscribersForNotification
mkDescribeSubscribersForNotification
  pNotification_
  pAccountId_
  pBudgetName_ =
    DescribeSubscribersForNotification'
      { notification =
          pNotification_,
        accountId = pAccountId_,
        nextToken = Lude.Nothing,
        budgetName = pBudgetName_,
        maxResults = Lude.Nothing
      }

-- | The notification whose subscribers you want to list.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnNotification :: Lens.Lens' DescribeSubscribersForNotification Notification
dsfnNotification = Lens.lens (notification :: DescribeSubscribersForNotification -> Notification) (\s a -> s {notification = a} :: DescribeSubscribersForNotification)
{-# DEPRECATED dsfnNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The @accountId@ that is associated with the budget whose subscribers you want descriptions of.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnAccountId :: Lens.Lens' DescribeSubscribersForNotification Lude.Text
dsfnAccountId = Lens.lens (accountId :: DescribeSubscribersForNotification -> Lude.Text) (\s a -> s {accountId = a} :: DescribeSubscribersForNotification)
{-# DEPRECATED dsfnAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnNextToken :: Lens.Lens' DescribeSubscribersForNotification (Lude.Maybe Lude.Text)
dsfnNextToken = Lens.lens (nextToken :: DescribeSubscribersForNotification -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSubscribersForNotification)
{-# DEPRECATED dsfnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the budget whose subscribers you want descriptions of.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnBudgetName :: Lens.Lens' DescribeSubscribersForNotification Lude.Text
dsfnBudgetName = Lens.lens (budgetName :: DescribeSubscribersForNotification -> Lude.Text) (\s a -> s {budgetName = a} :: DescribeSubscribersForNotification)
{-# DEPRECATED dsfnBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | An optional integer that represents how many entries a paginated response contains. The maximum is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnMaxResults :: Lens.Lens' DescribeSubscribersForNotification (Lude.Maybe Lude.Natural)
dsfnMaxResults = Lens.lens (maxResults :: DescribeSubscribersForNotification -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeSubscribersForNotification)
{-# DEPRECATED dsfnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeSubscribersForNotification where
  page rq rs
    | Page.stop (rs Lens.^. dsfnrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsfnrsSubscribers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsfnNextToken Lens..~ rs Lens.^. dsfnrsNextToken

instance Lude.AWSRequest DescribeSubscribersForNotification where
  type
    Rs DescribeSubscribersForNotification =
      DescribeSubscribersForNotificationResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSubscribersForNotificationResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Subscribers")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSubscribersForNotification where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSBudgetServiceGateway.DescribeSubscribersForNotification" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSubscribersForNotification where
  toJSON DescribeSubscribersForNotification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Notification" Lude..= notification),
            Lude.Just ("AccountId" Lude..= accountId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("BudgetName" Lude..= budgetName),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeSubscribersForNotification where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSubscribersForNotification where
  toQuery = Lude.const Lude.mempty

-- | Response of DescribeSubscribersForNotification
--
-- /See:/ 'mkDescribeSubscribersForNotificationResponse' smart constructor.
data DescribeSubscribersForNotificationResponse = DescribeSubscribersForNotificationResponse'
  { -- | The pagination token in the service response that indicates the next set of results that you can retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of subscribers that are associated with a notification.
    subscribers :: Lude.Maybe (Lude.NonEmpty Subscriber),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSubscribersForNotificationResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token in the service response that indicates the next set of results that you can retrieve.
-- * 'subscribers' - A list of subscribers that are associated with a notification.
-- * 'responseStatus' - The response status code.
mkDescribeSubscribersForNotificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSubscribersForNotificationResponse
mkDescribeSubscribersForNotificationResponse pResponseStatus_ =
  DescribeSubscribersForNotificationResponse'
    { nextToken =
        Lude.Nothing,
      subscribers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token in the service response that indicates the next set of results that you can retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnrsNextToken :: Lens.Lens' DescribeSubscribersForNotificationResponse (Lude.Maybe Lude.Text)
dsfnrsNextToken = Lens.lens (nextToken :: DescribeSubscribersForNotificationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSubscribersForNotificationResponse)
{-# DEPRECATED dsfnrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of subscribers that are associated with a notification.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnrsSubscribers :: Lens.Lens' DescribeSubscribersForNotificationResponse (Lude.Maybe (Lude.NonEmpty Subscriber))
dsfnrsSubscribers = Lens.lens (subscribers :: DescribeSubscribersForNotificationResponse -> Lude.Maybe (Lude.NonEmpty Subscriber)) (\s a -> s {subscribers = a} :: DescribeSubscribersForNotificationResponse)
{-# DEPRECATED dsfnrsSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnrsResponseStatus :: Lens.Lens' DescribeSubscribersForNotificationResponse Lude.Int
dsfnrsResponseStatus = Lens.lens (responseStatus :: DescribeSubscribersForNotificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSubscribersForNotificationResponse)
{-# DEPRECATED dsfnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
