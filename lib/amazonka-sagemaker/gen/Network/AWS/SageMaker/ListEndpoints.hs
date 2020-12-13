{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    leNameContains,
    leLastModifiedTimeBefore,
    leCreationTimeAfter,
    leNextToken,
    leSortOrder,
    leLastModifiedTimeAfter,
    leCreationTimeBefore,
    leStatusEquals,
    leMaxResults,
    leSortBy,

    -- * Destructuring the response
    ListEndpointsResponse (..),
    mkListEndpointsResponse,

    -- ** Response lenses
    lersNextToken,
    lersEndpoints,
    lersResponseStatus,
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
  { -- | A string in endpoint names. This filter returns only endpoints whose name contains the specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | A filter that returns only endpoints that were modified before the specified timestamp.
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only endpoints with a creation time greater than or equal to the specified time (timestamp).
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the result of a @ListEndpoints@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoints, use the token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order for results. The default is @Descending@ .
    sortOrder :: Lude.Maybe OrderKey,
    -- | A filter that returns only endpoints that were modified after the specified timestamp.
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only endpoints that were created before the specified time (timestamp).
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only endpoints with the specified status.
    statusEquals :: Lude.Maybe EndpointStatus,
    -- | The maximum number of endpoints to return in the response.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Sorts the list of results. The default is @CreationTime@ .
    sortBy :: Lude.Maybe EndpointSortKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEndpoints' with the minimum fields required to make a request.
--
-- * 'nameContains' - A string in endpoint names. This filter returns only endpoints whose name contains the specified string.
-- * 'lastModifiedTimeBefore' - A filter that returns only endpoints that were modified before the specified timestamp.
-- * 'creationTimeAfter' - A filter that returns only endpoints with a creation time greater than or equal to the specified time (timestamp).
-- * 'nextToken' - If the result of a @ListEndpoints@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoints, use the token in the next request.
-- * 'sortOrder' - The sort order for results. The default is @Descending@ .
-- * 'lastModifiedTimeAfter' - A filter that returns only endpoints that were modified after the specified timestamp.
-- * 'creationTimeBefore' - A filter that returns only endpoints that were created before the specified time (timestamp).
-- * 'statusEquals' - A filter that returns only endpoints with the specified status.
-- * 'maxResults' - The maximum number of endpoints to return in the response.
-- * 'sortBy' - Sorts the list of results. The default is @CreationTime@ .
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
leNameContains :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Text)
leNameContains = Lens.lens (nameContains :: ListEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListEndpoints)
{-# DEPRECATED leNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only endpoints that were modified before the specified timestamp.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leLastModifiedTimeBefore :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Timestamp)
leLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListEndpoints -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListEndpoints)
{-# DEPRECATED leLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only endpoints with a creation time greater than or equal to the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCreationTimeAfter :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Timestamp)
leCreationTimeAfter = Lens.lens (creationTimeAfter :: ListEndpoints -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListEndpoints)
{-# DEPRECATED leCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of a @ListEndpoints@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoints, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Text)
leNextToken = Lens.lens (nextToken :: ListEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEndpoints)
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leSortOrder :: Lens.Lens' ListEndpoints (Lude.Maybe OrderKey)
leSortOrder = Lens.lens (sortOrder :: ListEndpoints -> Lude.Maybe OrderKey) (\s a -> s {sortOrder = a} :: ListEndpoints)
{-# DEPRECATED leSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only endpoints that were modified after the specified timestamp.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leLastModifiedTimeAfter :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Timestamp)
leLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListEndpoints -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListEndpoints)
{-# DEPRECATED leLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only endpoints that were created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCreationTimeBefore :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Timestamp)
leCreationTimeBefore = Lens.lens (creationTimeBefore :: ListEndpoints -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListEndpoints)
{-# DEPRECATED leCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only endpoints with the specified status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leStatusEquals :: Lens.Lens' ListEndpoints (Lude.Maybe EndpointStatus)
leStatusEquals = Lens.lens (statusEquals :: ListEndpoints -> Lude.Maybe EndpointStatus) (\s a -> s {statusEquals = a} :: ListEndpoints)
{-# DEPRECATED leStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of endpoints to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListEndpoints (Lude.Maybe Lude.Natural)
leMaxResults = Lens.lens (maxResults :: ListEndpoints -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListEndpoints)
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Sorts the list of results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leSortBy :: Lens.Lens' ListEndpoints (Lude.Maybe EndpointSortKey)
leSortBy = Lens.lens (sortBy :: ListEndpoints -> Lude.Maybe EndpointSortKey) (\s a -> s {sortBy = a} :: ListEndpoints)
{-# DEPRECATED leSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListEndpoints where
  page rq rs
    | Page.stop (rs Lens.^. lersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lersEndpoints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& leNextToken Lens..~ rs Lens.^. lersNextToken

instance Lude.AWSRequest ListEndpoints where
  type Rs ListEndpoints = ListEndpointsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEndpointsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Endpoints" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array or endpoint objects.
    endpoints :: [EndpointSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
-- * 'endpoints' - An array or endpoint objects.
-- * 'responseStatus' - The response status code.
mkListEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEndpointsResponse
mkListEndpointsResponse pResponseStatus_ =
  ListEndpointsResponse'
    { nextToken = Lude.Nothing,
      endpoints = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersNextToken :: Lens.Lens' ListEndpointsResponse (Lude.Maybe Lude.Text)
lersNextToken = Lens.lens (nextToken :: ListEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEndpointsResponse)
{-# DEPRECATED lersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array or endpoint objects.
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersEndpoints :: Lens.Lens' ListEndpointsResponse [EndpointSummary]
lersEndpoints = Lens.lens (endpoints :: ListEndpointsResponse -> [EndpointSummary]) (\s a -> s {endpoints = a} :: ListEndpointsResponse)
{-# DEPRECATED lersEndpoints "Use generic-lens or generic-optics with 'endpoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersResponseStatus :: Lens.Lens' ListEndpointsResponse Lude.Int
lersResponseStatus = Lens.lens (responseStatus :: ListEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEndpointsResponse)
{-# DEPRECATED lersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
