{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListEndpointConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists endpoint configurations.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListEndpointConfigs
  ( -- * Creating a request
    ListEndpointConfigs (..),
    mkListEndpointConfigs,

    -- ** Request lenses
    lecNameContains,
    lecCreationTimeAfter,
    lecNextToken,
    lecSortOrder,
    lecCreationTimeBefore,
    lecMaxResults,
    lecSortBy,

    -- * Destructuring the response
    ListEndpointConfigsResponse (..),
    mkListEndpointConfigsResponse,

    -- ** Response lenses
    lecrsNextToken,
    lecrsResponseStatus,
    lecrsEndpointConfigs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListEndpointConfigs' smart constructor.
data ListEndpointConfigs = ListEndpointConfigs'
  { nameContains ::
      Lude.Maybe Lude.Text,
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe OrderKey,
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe EndpointConfigSortKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEndpointConfigs' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - A filter that returns only endpoint configurations with a creation time greater than or equal to the specified time (timestamp).
-- * 'creationTimeBefore' - A filter that returns only endpoint configurations created before the specified time (timestamp).
-- * 'maxResults' - The maximum number of training jobs to return in the response.
-- * 'nameContains' - A string in the endpoint configuration name. This filter returns only endpoint configurations whose name contains the specified string.
-- * 'nextToken' - If the result of the previous @ListEndpointConfig@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoint configurations, use the token in the next request.
-- * 'sortBy' - The field to sort results by. The default is @CreationTime@ .
-- * 'sortOrder' - The sort order for results. The default is @Descending@ .
mkListEndpointConfigs ::
  ListEndpointConfigs
mkListEndpointConfigs =
  ListEndpointConfigs'
    { nameContains = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A string in the endpoint configuration name. This filter returns only endpoint configurations whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecNameContains :: Lens.Lens' ListEndpointConfigs (Lude.Maybe Lude.Text)
lecNameContains = Lens.lens (nameContains :: ListEndpointConfigs -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListEndpointConfigs)
{-# DEPRECATED lecNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only endpoint configurations with a creation time greater than or equal to the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecCreationTimeAfter :: Lens.Lens' ListEndpointConfigs (Lude.Maybe Lude.Timestamp)
lecCreationTimeAfter = Lens.lens (creationTimeAfter :: ListEndpointConfigs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListEndpointConfigs)
{-# DEPRECATED lecCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of the previous @ListEndpointConfig@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoint configurations, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecNextToken :: Lens.Lens' ListEndpointConfigs (Lude.Maybe Lude.Text)
lecNextToken = Lens.lens (nextToken :: ListEndpointConfigs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEndpointConfigs)
{-# DEPRECATED lecNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecSortOrder :: Lens.Lens' ListEndpointConfigs (Lude.Maybe OrderKey)
lecSortOrder = Lens.lens (sortOrder :: ListEndpointConfigs -> Lude.Maybe OrderKey) (\s a -> s {sortOrder = a} :: ListEndpointConfigs)
{-# DEPRECATED lecSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only endpoint configurations created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecCreationTimeBefore :: Lens.Lens' ListEndpointConfigs (Lude.Maybe Lude.Timestamp)
lecCreationTimeBefore = Lens.lens (creationTimeBefore :: ListEndpointConfigs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListEndpointConfigs)
{-# DEPRECATED lecCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of training jobs to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecMaxResults :: Lens.Lens' ListEndpointConfigs (Lude.Maybe Lude.Natural)
lecMaxResults = Lens.lens (maxResults :: ListEndpointConfigs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListEndpointConfigs)
{-# DEPRECATED lecMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecSortBy :: Lens.Lens' ListEndpointConfigs (Lude.Maybe EndpointConfigSortKey)
lecSortBy = Lens.lens (sortBy :: ListEndpointConfigs -> Lude.Maybe EndpointConfigSortKey) (\s a -> s {sortBy = a} :: ListEndpointConfigs)
{-# DEPRECATED lecSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListEndpointConfigs where
  page rq rs
    | Page.stop (rs Lens.^. lecrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lecrsEndpointConfigs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lecNextToken Lens..~ rs Lens.^. lecrsNextToken

instance Lude.AWSRequest ListEndpointConfigs where
  type Rs ListEndpointConfigs = ListEndpointConfigsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEndpointConfigsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "EndpointConfigs" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListEndpointConfigs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListEndpointConfigs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListEndpointConfigs where
  toJSON ListEndpointConfigs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListEndpointConfigs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEndpointConfigs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListEndpointConfigsResponse' smart constructor.
data ListEndpointConfigsResponse = ListEndpointConfigsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    endpointConfigs ::
      [EndpointConfigSummary]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEndpointConfigsResponse' with the minimum fields required to make a request.
--
-- * 'endpointConfigs' - An array of endpoint configurations.
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of endpoint configurations, use it in the subsequent request
-- * 'responseStatus' - The response status code.
mkListEndpointConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEndpointConfigsResponse
mkListEndpointConfigsResponse pResponseStatus_ =
  ListEndpointConfigsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      endpointConfigs = Lude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of endpoint configurations, use it in the subsequent request
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecrsNextToken :: Lens.Lens' ListEndpointConfigsResponse (Lude.Maybe Lude.Text)
lecrsNextToken = Lens.lens (nextToken :: ListEndpointConfigsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEndpointConfigsResponse)
{-# DEPRECATED lecrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecrsResponseStatus :: Lens.Lens' ListEndpointConfigsResponse Lude.Int
lecrsResponseStatus = Lens.lens (responseStatus :: ListEndpointConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEndpointConfigsResponse)
{-# DEPRECATED lecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | An array of endpoint configurations.
--
-- /Note:/ Consider using 'endpointConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecrsEndpointConfigs :: Lens.Lens' ListEndpointConfigsResponse [EndpointConfigSummary]
lecrsEndpointConfigs = Lens.lens (endpointConfigs :: ListEndpointConfigsResponse -> [EndpointConfigSummary]) (\s a -> s {endpointConfigs = a} :: ListEndpointConfigsResponse)
{-# DEPRECATED lecrsEndpointConfigs "Use generic-lens or generic-optics with 'endpointConfigs' instead." #-}
