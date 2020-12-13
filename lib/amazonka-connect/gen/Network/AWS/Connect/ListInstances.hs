{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a list of instances which are in active state, creation-in-progress state, and failed state. Instances that aren't successfully created (they are in a failed state) are returned only for 24 hours after the CreateInstance API was invoked.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListInstances
  ( -- * Creating a request
    ListInstances (..),
    mkListInstances,

    -- ** Request lenses
    liNextToken,
    liMaxResults,

    -- * Destructuring the response
    ListInstancesResponse (..),
    mkListInstancesResponse,

    -- ** Response lenses
    lirsInstanceSummaryList,
    lirsNextToken,
    lirsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListInstances' smart constructor.
data ListInstances = ListInstances'
  { -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximimum number of results to return per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstances' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
-- * 'maxResults' - The maximimum number of results to return per page.
mkListInstances ::
  ListInstances
mkListInstances =
  ListInstances'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListInstances (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInstances)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListInstances (Lude.Maybe Lude.Natural)
liMaxResults = Lens.lens (maxResults :: ListInstances -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListInstances)
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListInstances where
  page rq rs
    | Page.stop (rs Lens.^. lirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsInstanceSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liNextToken Lens..~ rs Lens.^. lirsNextToken

instance Lude.AWSRequest ListInstances where
  type Rs ListInstances = ListInstancesResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInstancesResponse'
            Lude.<$> (x Lude..?> "InstanceSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListInstances where
  toPath = Lude.const "/instance"

instance Lude.ToQuery ListInstances where
  toQuery ListInstances' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | Information about the instances.
    instanceSummaryList :: Lude.Maybe [InstanceSummary],
    -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstancesResponse' with the minimum fields required to make a request.
--
-- * 'instanceSummaryList' - Information about the instances.
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
mkListInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInstancesResponse
mkListInstancesResponse pResponseStatus_ =
  ListInstancesResponse'
    { instanceSummaryList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the instances.
--
-- /Note:/ Consider using 'instanceSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsInstanceSummaryList :: Lens.Lens' ListInstancesResponse (Lude.Maybe [InstanceSummary])
lirsInstanceSummaryList = Lens.lens (instanceSummaryList :: ListInstancesResponse -> Lude.Maybe [InstanceSummary]) (\s a -> s {instanceSummaryList = a} :: ListInstancesResponse)
{-# DEPRECATED lirsInstanceSummaryList "Use generic-lens or generic-optics with 'instanceSummaryList' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsNextToken :: Lens.Lens' ListInstancesResponse (Lude.Maybe Lude.Text)
lirsNextToken = Lens.lens (nextToken :: ListInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInstancesResponse)
{-# DEPRECATED lirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListInstancesResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInstancesResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
