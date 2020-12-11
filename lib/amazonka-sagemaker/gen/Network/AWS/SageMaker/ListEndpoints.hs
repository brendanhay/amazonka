{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists endpoints.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListEndpoints
  ( -- * Creating a request
    ListEndpoints (..),
    mkListEndpoints,

    -- ** Request lenses
    lesNameContains,
    lesLastModifiedTimeBefore,
    lesCreationTimeAfter,
    lesNextToken,
    lesSortOrder,
    lesLastModifiedTimeAfter,
    lesCreationTimeBefore,
    lesStatusEquals,
    lesMaxResults,
    lesSortBy,

    -- * Destructuring the response
    ListEndpointsResponse (..),
    mkListEndpointsResponse,

    -- ** Response lenses
    lesrsNextToken,
    lesrsResponseStatus,
    lesrsEndpoints,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { nameContains ::
      Lude.Maybe Lude.Text,
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe OrderKey,
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    statusEquals :: Lude.Maybe EndpointStatus,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe EndpointSortKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEndpoints' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - A filter that returns only endpoints with a creation time greater than or equal to the specified time (timestamp).
-- * 'creationTimeBefore' - A filter that returns only endpoints that were created before the specified time (timestamp).
-- * 'lastModifiedTimeAfter' - A filter that returns only endpoints that were modified after the specified timestamp.
-- * 'lastModifiedTimeBefore' - A filter that returns only endpoints that were modified before the specified timestamp.
-- * 'maxResults' - The maximum number of endpoints to return in the response.
-- * 'nameContains' - A string in endpoint names. This filter returns only endpoints whose name contains the specified string.
-- * 'nextToken' - If the result of a @ListEndpoints@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoints, use the token in the next request.
-- * 'sortBy' - Sorts the list of results. The default is @CreationTime@ .
-- * 'sortOrder' - The sort order for results. The default is @Descending@ .
-- * 'statusEquals' - A filter that returns only endpoints with the specified status.
mkListEndpoints ::
  ListEndpoints
mkListEndpoints =
  ListEndpoints'
    { nameContains = Lude.Nothing,
      lastModifiedTimeBefore = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      lastModifiedTimeAfter = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      statusEquals = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A string in endpoint names. This filter returns only endpoints whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesNameContains :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Text)
lesNameContains = Lens.lens (nameContains :: ListEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListEndpoints)
{-# DEPRECATED lesNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only endpoints that were modified before the specified timestamp.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesLastModifiedTimeBefore :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Timestamp)
lesLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListEndpoints -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListEndpoints)
{-# DEPRECATED lesLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only endpoints with a creation time greater than or equal to the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesCreationTimeAfter :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Timestamp)
lesCreationTimeAfter = Lens.lens (creationTimeAfter :: ListEndpoints -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListEndpoints)
{-# DEPRECATED lesCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of a @ListEndpoints@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoints, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesNextToken :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Text)
lesNextToken = Lens.lens (nextToken :: ListEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEndpoints)
{-# DEPRECATED lesNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesSortOrder :: Lens.Lens' ListEndpoints (Lude.Maybe OrderKey)
lesSortOrder = Lens.lens (sortOrder :: ListEndpoints -> Lude.Maybe OrderKey) (\s a -> s {sortOrder = a} :: ListEndpoints)
{-# DEPRECATED lesSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only endpoints that were modified after the specified timestamp.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesLastModifiedTimeAfter :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Timestamp)
lesLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListEndpoints -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListEndpoints)
{-# DEPRECATED lesLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only endpoints that were created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesCreationTimeBefore :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Timestamp)
lesCreationTimeBefore = Lens.lens (creationTimeBefore :: ListEndpoints -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListEndpoints)
{-# DEPRECATED lesCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only endpoints with the specified status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesStatusEquals :: Lens.Lens' ListEndpoints (Lude.Maybe EndpointStatus)
lesStatusEquals = Lens.lens (statusEquals :: ListEndpoints -> Lude.Maybe EndpointStatus) (\s a -> s {statusEquals = a} :: ListEndpoints)
{-# DEPRECATED lesStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of endpoints to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesMaxResults :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Natural)
lesMaxResults = Lens.lens (maxResults :: ListEndpoints -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListEndpoints)
{-# DEPRECATED lesMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Sorts the list of results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesSortBy :: Lens.Lens' ListEndpoints (Lude.Maybe EndpointSortKey)
lesSortBy = Lens.lens (sortBy :: ListEndpoints -> Lude.Maybe EndpointSortKey) (\s a -> s {sortBy = a} :: ListEndpoints)
{-# DEPRECATED lesSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListEndpoints where
  page rq rs
    | Page.stop (rs Lens.^. lesrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lesrsEndpoints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lesNextToken Lens..~ rs Lens.^. lesrsNextToken

instance Lude.AWSRequest ListEndpoints where
  type Rs ListEndpoints = ListEndpointsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEndpointsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "Endpoints" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListEndpoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListEndpoints" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListEndpoints where
  toJSON ListEndpoints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("LastModifiedTimeBefore" Lude..=) Lude.<$> lastModifiedTimeBefore,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("LastModifiedTimeAfter" Lude..=) Lude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("StatusEquals" Lude..=) Lude.<$> statusEquals,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListEndpoints where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEndpoints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    endpoints :: [EndpointSummary]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'endpoints' - An array or endpoint objects.
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
-- * 'responseStatus' - The response status code.
mkListEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEndpointsResponse
mkListEndpointsResponse pResponseStatus_ =
  ListEndpointsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      endpoints = Lude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrsNextToken :: Lens.Lens' ListEndpointsResponse (Lude.Maybe Lude.Text)
lesrsNextToken = Lens.lens (nextToken :: ListEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEndpointsResponse)
{-# DEPRECATED lesrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrsResponseStatus :: Lens.Lens' ListEndpointsResponse Lude.Int
lesrsResponseStatus = Lens.lens (responseStatus :: ListEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEndpointsResponse)
{-# DEPRECATED lesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | An array or endpoint objects.
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrsEndpoints :: Lens.Lens' ListEndpointsResponse [EndpointSummary]
lesrsEndpoints = Lens.lens (endpoints :: ListEndpointsResponse -> [EndpointSummary]) (\s a -> s {endpoints = a} :: ListEndpointsResponse)
{-# DEPRECATED lesrsEndpoints "Use generic-lens or generic-optics with 'endpoints' instead." #-}
