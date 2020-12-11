{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ListLogSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the active log subscriptions for the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.ListLogSubscriptions
  ( -- * Creating a request
    ListLogSubscriptions (..),
    mkListLogSubscriptions,

    -- ** Request lenses
    llsDirectoryId,
    llsNextToken,
    llsLimit,

    -- * Destructuring the response
    ListLogSubscriptionsResponse (..),
    mkListLogSubscriptionsResponse,

    -- ** Response lenses
    llsrsNextToken,
    llsrsLogSubscriptions,
    llsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListLogSubscriptions' smart constructor.
data ListLogSubscriptions = ListLogSubscriptions'
  { directoryId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLogSubscriptions' with the minimum fields required to make a request.
--
-- * 'directoryId' - If a /DirectoryID/ is provided, lists only the log subscription associated with that directory. If no /DirectoryId/ is provided, lists all log subscriptions associated with your AWS account. If there are no log subscriptions for the AWS account or the directory, an empty list will be returned.
-- * 'limit' - The maximum number of items returned.
-- * 'nextToken' - The token for the next set of items to return.
mkListLogSubscriptions ::
  ListLogSubscriptions
mkListLogSubscriptions =
  ListLogSubscriptions'
    { directoryId = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If a /DirectoryID/ is provided, lists only the log subscription associated with that directory. If no /DirectoryId/ is provided, lists all log subscriptions associated with your AWS account. If there are no log subscriptions for the AWS account or the directory, an empty list will be returned.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsDirectoryId :: Lens.Lens' ListLogSubscriptions (Lude.Maybe Lude.Text)
llsDirectoryId = Lens.lens (directoryId :: ListLogSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: ListLogSubscriptions)
{-# DEPRECATED llsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The token for the next set of items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsNextToken :: Lens.Lens' ListLogSubscriptions (Lude.Maybe Lude.Text)
llsNextToken = Lens.lens (nextToken :: ListLogSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLogSubscriptions)
{-# DEPRECATED llsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items returned.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsLimit :: Lens.Lens' ListLogSubscriptions (Lude.Maybe Lude.Natural)
llsLimit = Lens.lens (limit :: ListLogSubscriptions -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListLogSubscriptions)
{-# DEPRECATED llsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListLogSubscriptions where
  page rq rs
    | Page.stop (rs Lens.^. llsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. llsrsLogSubscriptions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& llsNextToken Lens..~ rs Lens.^. llsrsNextToken

instance Lude.AWSRequest ListLogSubscriptions where
  type Rs ListLogSubscriptions = ListLogSubscriptionsResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLogSubscriptionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "LogSubscriptions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLogSubscriptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.ListLogSubscriptions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListLogSubscriptions where
  toJSON ListLogSubscriptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DirectoryId" Lude..=) Lude.<$> directoryId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListLogSubscriptions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListLogSubscriptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListLogSubscriptionsResponse' smart constructor.
data ListLogSubscriptionsResponse = ListLogSubscriptionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    logSubscriptions ::
      Lude.Maybe [LogSubscription],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLogSubscriptionsResponse' with the minimum fields required to make a request.
--
-- * 'logSubscriptions' - A list of active 'LogSubscription' objects for calling the AWS account.
-- * 'nextToken' - The token for the next set of items to return.
-- * 'responseStatus' - The response status code.
mkListLogSubscriptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLogSubscriptionsResponse
mkListLogSubscriptionsResponse pResponseStatus_ =
  ListLogSubscriptionsResponse'
    { nextToken = Lude.Nothing,
      logSubscriptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsrsNextToken :: Lens.Lens' ListLogSubscriptionsResponse (Lude.Maybe Lude.Text)
llsrsNextToken = Lens.lens (nextToken :: ListLogSubscriptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLogSubscriptionsResponse)
{-# DEPRECATED llsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of active 'LogSubscription' objects for calling the AWS account.
--
-- /Note:/ Consider using 'logSubscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsrsLogSubscriptions :: Lens.Lens' ListLogSubscriptionsResponse (Lude.Maybe [LogSubscription])
llsrsLogSubscriptions = Lens.lens (logSubscriptions :: ListLogSubscriptionsResponse -> Lude.Maybe [LogSubscription]) (\s a -> s {logSubscriptions = a} :: ListLogSubscriptionsResponse)
{-# DEPRECATED llsrsLogSubscriptions "Use generic-lens or generic-optics with 'logSubscriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsrsResponseStatus :: Lens.Lens' ListLogSubscriptionsResponse Lude.Int
llsrsResponseStatus = Lens.lens (responseStatus :: ListLogSubscriptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLogSubscriptionsResponse)
{-# DEPRECATED llsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
