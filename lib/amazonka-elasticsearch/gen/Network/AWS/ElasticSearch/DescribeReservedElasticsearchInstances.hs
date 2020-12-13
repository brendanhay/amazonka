{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved Elasticsearch instances for this account.
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
  ( -- * Creating a request
    DescribeReservedElasticsearchInstances (..),
    mkDescribeReservedElasticsearchInstances,

    -- ** Request lenses
    dreiReservedElasticsearchInstanceId,
    dreiNextToken,
    dreiMaxResults,

    -- * Destructuring the response
    DescribeReservedElasticsearchInstancesResponse (..),
    mkDescribeReservedElasticsearchInstancesResponse,

    -- ** Response lenses
    dreirsReservedElasticsearchInstances,
    dreirsNextToken,
    dreirsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for parameters to @DescribeReservedElasticsearchInstances@
--
-- /See:/ 'mkDescribeReservedElasticsearchInstances' smart constructor.
data DescribeReservedElasticsearchInstances = DescribeReservedElasticsearchInstances'
  { -- | The reserved instance identifier filter value. Use this parameter to show only the reservation that matches the specified reserved Elasticsearch instance ID.
    reservedElasticsearchInstanceId :: Lude.Maybe Lude.Text,
    -- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Set this value to limit the number of results returned. If not specified, defaults to 100.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedElasticsearchInstances' with the minimum fields required to make a request.
--
-- * 'reservedElasticsearchInstanceId' - The reserved instance identifier filter value. Use this parameter to show only the reservation that matches the specified reserved Elasticsearch instance ID.
-- * 'nextToken' - NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
-- * 'maxResults' - Set this value to limit the number of results returned. If not specified, defaults to 100.
mkDescribeReservedElasticsearchInstances ::
  DescribeReservedElasticsearchInstances
mkDescribeReservedElasticsearchInstances =
  DescribeReservedElasticsearchInstances'
    { reservedElasticsearchInstanceId =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The reserved instance identifier filter value. Use this parameter to show only the reservation that matches the specified reserved Elasticsearch instance ID.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiReservedElasticsearchInstanceId :: Lens.Lens' DescribeReservedElasticsearchInstances (Lude.Maybe Lude.Text)
dreiReservedElasticsearchInstanceId = Lens.lens (reservedElasticsearchInstanceId :: DescribeReservedElasticsearchInstances -> Lude.Maybe Lude.Text) (\s a -> s {reservedElasticsearchInstanceId = a} :: DescribeReservedElasticsearchInstances)
{-# DEPRECATED dreiReservedElasticsearchInstanceId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceId' instead." #-}

-- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiNextToken :: Lens.Lens' DescribeReservedElasticsearchInstances (Lude.Maybe Lude.Text)
dreiNextToken = Lens.lens (nextToken :: DescribeReservedElasticsearchInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeReservedElasticsearchInstances)
{-# DEPRECATED dreiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Set this value to limit the number of results returned. If not specified, defaults to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiMaxResults :: Lens.Lens' DescribeReservedElasticsearchInstances (Lude.Maybe Lude.Int)
dreiMaxResults = Lens.lens (maxResults :: DescribeReservedElasticsearchInstances -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeReservedElasticsearchInstances)
{-# DEPRECATED dreiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeReservedElasticsearchInstances where
  page rq rs
    | Page.stop (rs Lens.^. dreirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dreirsReservedElasticsearchInstances) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dreiNextToken Lens..~ rs Lens.^. dreirsNextToken

instance Lude.AWSRequest DescribeReservedElasticsearchInstances where
  type
    Rs DescribeReservedElasticsearchInstances =
      DescribeReservedElasticsearchInstancesResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReservedElasticsearchInstancesResponse'
            Lude.<$> (x Lude..?> "ReservedElasticsearchInstances" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservedElasticsearchInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedElasticsearchInstances where
  toPath = Lude.const "/2015-01-01/es/reservedInstances"

instance Lude.ToQuery DescribeReservedElasticsearchInstances where
  toQuery DescribeReservedElasticsearchInstances' {..} =
    Lude.mconcat
      [ "reservationId" Lude.=: reservedElasticsearchInstanceId,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | Container for results from @DescribeReservedElasticsearchInstances@
--
-- /See:/ 'mkDescribeReservedElasticsearchInstancesResponse' smart constructor.
data DescribeReservedElasticsearchInstancesResponse = DescribeReservedElasticsearchInstancesResponse'
  { -- | List of reserved Elasticsearch instances.
    reservedElasticsearchInstances :: Lude.Maybe [ReservedElasticsearchInstance],
    -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedElasticsearchInstancesResponse' with the minimum fields required to make a request.
--
-- * 'reservedElasticsearchInstances' - List of reserved Elasticsearch instances.
-- * 'nextToken' - Provides an identifier to allow retrieval of paginated results.
-- * 'responseStatus' - The response status code.
mkDescribeReservedElasticsearchInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedElasticsearchInstancesResponse
mkDescribeReservedElasticsearchInstancesResponse pResponseStatus_ =
  DescribeReservedElasticsearchInstancesResponse'
    { reservedElasticsearchInstances =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of reserved Elasticsearch instances.
--
-- /Note:/ Consider using 'reservedElasticsearchInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreirsReservedElasticsearchInstances :: Lens.Lens' DescribeReservedElasticsearchInstancesResponse (Lude.Maybe [ReservedElasticsearchInstance])
dreirsReservedElasticsearchInstances = Lens.lens (reservedElasticsearchInstances :: DescribeReservedElasticsearchInstancesResponse -> Lude.Maybe [ReservedElasticsearchInstance]) (\s a -> s {reservedElasticsearchInstances = a} :: DescribeReservedElasticsearchInstancesResponse)
{-# DEPRECATED dreirsReservedElasticsearchInstances "Use generic-lens or generic-optics with 'reservedElasticsearchInstances' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreirsNextToken :: Lens.Lens' DescribeReservedElasticsearchInstancesResponse (Lude.Maybe Lude.Text)
dreirsNextToken = Lens.lens (nextToken :: DescribeReservedElasticsearchInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeReservedElasticsearchInstancesResponse)
{-# DEPRECATED dreirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreirsResponseStatus :: Lens.Lens' DescribeReservedElasticsearchInstancesResponse Lude.Int
dreirsResponseStatus = Lens.lens (responseStatus :: DescribeReservedElasticsearchInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedElasticsearchInstancesResponse)
{-# DEPRECATED dreirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
