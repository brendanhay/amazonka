{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved Elasticsearch instance offerings.
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
  ( -- * Creating a request
    DescribeReservedElasticsearchInstanceOfferings (..),
    mkDescribeReservedElasticsearchInstanceOfferings,

    -- ** Request lenses
    dreioReservedElasticsearchInstanceOfferingId,
    dreioNextToken,
    dreioMaxResults,

    -- * Destructuring the response
    DescribeReservedElasticsearchInstanceOfferingsResponse (..),
    mkDescribeReservedElasticsearchInstanceOfferingsResponse,

    -- ** Response lenses
    dreiorsReservedElasticsearchInstanceOfferings,
    dreiorsNextToken,
    dreiorsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for parameters to @DescribeReservedElasticsearchInstanceOfferings@
--
-- /See:/ 'mkDescribeReservedElasticsearchInstanceOfferings' smart constructor.
data DescribeReservedElasticsearchInstanceOfferings = DescribeReservedElasticsearchInstanceOfferings'
  { -- | The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
    reservedElasticsearchInstanceOfferingId :: Lude.Maybe Lude.Text,
    -- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Set this value to limit the number of results returned. If not specified, defaults to 100.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedElasticsearchInstanceOfferings' with the minimum fields required to make a request.
--
-- * 'reservedElasticsearchInstanceOfferingId' - The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
-- * 'nextToken' - NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
-- * 'maxResults' - Set this value to limit the number of results returned. If not specified, defaults to 100.
mkDescribeReservedElasticsearchInstanceOfferings ::
  DescribeReservedElasticsearchInstanceOfferings
mkDescribeReservedElasticsearchInstanceOfferings =
  DescribeReservedElasticsearchInstanceOfferings'
    { reservedElasticsearchInstanceOfferingId =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreioReservedElasticsearchInstanceOfferingId :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Lude.Maybe Lude.Text)
dreioReservedElasticsearchInstanceOfferingId = Lens.lens (reservedElasticsearchInstanceOfferingId :: DescribeReservedElasticsearchInstanceOfferings -> Lude.Maybe Lude.Text) (\s a -> s {reservedElasticsearchInstanceOfferingId = a} :: DescribeReservedElasticsearchInstanceOfferings)
{-# DEPRECATED dreioReservedElasticsearchInstanceOfferingId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferingId' instead." #-}

-- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreioNextToken :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Lude.Maybe Lude.Text)
dreioNextToken = Lens.lens (nextToken :: DescribeReservedElasticsearchInstanceOfferings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeReservedElasticsearchInstanceOfferings)
{-# DEPRECATED dreioNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Set this value to limit the number of results returned. If not specified, defaults to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreioMaxResults :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Lude.Maybe Lude.Int)
dreioMaxResults = Lens.lens (maxResults :: DescribeReservedElasticsearchInstanceOfferings -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeReservedElasticsearchInstanceOfferings)
{-# DEPRECATED dreioMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance
  Page.AWSPager
    DescribeReservedElasticsearchInstanceOfferings
  where
  page rq rs
    | Page.stop (rs Lens.^. dreiorsNextToken) = Lude.Nothing
    | Page.stop
        (rs Lens.^. dreiorsReservedElasticsearchInstanceOfferings) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dreioNextToken Lens..~ rs Lens.^. dreiorsNextToken

instance
  Lude.AWSRequest
    DescribeReservedElasticsearchInstanceOfferings
  where
  type
    Rs DescribeReservedElasticsearchInstanceOfferings =
      DescribeReservedElasticsearchInstanceOfferingsResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReservedElasticsearchInstanceOfferingsResponse'
            Lude.<$> ( x Lude..?> "ReservedElasticsearchInstanceOfferings"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DescribeReservedElasticsearchInstanceOfferings
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedElasticsearchInstanceOfferings where
  toPath = Lude.const "/2015-01-01/es/reservedInstanceOfferings"

instance
  Lude.ToQuery
    DescribeReservedElasticsearchInstanceOfferings
  where
  toQuery DescribeReservedElasticsearchInstanceOfferings' {..} =
    Lude.mconcat
      [ "offeringId" Lude.=: reservedElasticsearchInstanceOfferingId,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | Container for results from @DescribeReservedElasticsearchInstanceOfferings@
--
-- /See:/ 'mkDescribeReservedElasticsearchInstanceOfferingsResponse' smart constructor.
data DescribeReservedElasticsearchInstanceOfferingsResponse = DescribeReservedElasticsearchInstanceOfferingsResponse'
  { -- | List of reserved Elasticsearch instance offerings
    reservedElasticsearchInstanceOfferings :: Lude.Maybe [ReservedElasticsearchInstanceOffering],
    -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedElasticsearchInstanceOfferingsResponse' with the minimum fields required to make a request.
--
-- * 'reservedElasticsearchInstanceOfferings' - List of reserved Elasticsearch instance offerings
-- * 'nextToken' - Provides an identifier to allow retrieval of paginated results.
-- * 'responseStatus' - The response status code.
mkDescribeReservedElasticsearchInstanceOfferingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedElasticsearchInstanceOfferingsResponse
mkDescribeReservedElasticsearchInstanceOfferingsResponse
  pResponseStatus_ =
    DescribeReservedElasticsearchInstanceOfferingsResponse'
      { reservedElasticsearchInstanceOfferings =
          Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | List of reserved Elasticsearch instance offerings
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiorsReservedElasticsearchInstanceOfferings :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse (Lude.Maybe [ReservedElasticsearchInstanceOffering])
dreiorsReservedElasticsearchInstanceOfferings = Lens.lens (reservedElasticsearchInstanceOfferings :: DescribeReservedElasticsearchInstanceOfferingsResponse -> Lude.Maybe [ReservedElasticsearchInstanceOffering]) (\s a -> s {reservedElasticsearchInstanceOfferings = a} :: DescribeReservedElasticsearchInstanceOfferingsResponse)
{-# DEPRECATED dreiorsReservedElasticsearchInstanceOfferings "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferings' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiorsNextToken :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse (Lude.Maybe Lude.Text)
dreiorsNextToken = Lens.lens (nextToken :: DescribeReservedElasticsearchInstanceOfferingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeReservedElasticsearchInstanceOfferingsResponse)
{-# DEPRECATED dreiorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiorsResponseStatus :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse Lude.Int
dreiorsResponseStatus = Lens.lens (responseStatus :: DescribeReservedElasticsearchInstanceOfferingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedElasticsearchInstanceOfferingsResponse)
{-# DEPRECATED dreiorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
