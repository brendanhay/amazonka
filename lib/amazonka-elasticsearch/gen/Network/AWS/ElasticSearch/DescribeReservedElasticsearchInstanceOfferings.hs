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
    dreioMaxResults,
    dreioNextToken,
    dreioReservedElasticsearchInstanceOfferingId,

    -- * Destructuring the response
    DescribeReservedElasticsearchInstanceOfferingsResponse (..),
    mkDescribeReservedElasticsearchInstanceOfferingsResponse,

    -- ** Response lenses
    dreiorrsNextToken,
    dreiorrsReservedElasticsearchInstanceOfferings,
    dreiorrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for parameters to @DescribeReservedElasticsearchInstanceOfferings@
--
-- /See:/ 'mkDescribeReservedElasticsearchInstanceOfferings' smart constructor.
data DescribeReservedElasticsearchInstanceOfferings = DescribeReservedElasticsearchInstanceOfferings'
  { -- | Set this value to limit the number of results returned. If not specified, defaults to 100.
    maxResults :: Core.Maybe Core.Int,
    -- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
    reservedElasticsearchInstanceOfferingId :: Core.Maybe Types.ReservedElasticsearchInstanceOfferingId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedElasticsearchInstanceOfferings' value with any optional fields omitted.
mkDescribeReservedElasticsearchInstanceOfferings ::
  DescribeReservedElasticsearchInstanceOfferings
mkDescribeReservedElasticsearchInstanceOfferings =
  DescribeReservedElasticsearchInstanceOfferings'
    { maxResults =
        Core.Nothing,
      nextToken = Core.Nothing,
      reservedElasticsearchInstanceOfferingId =
        Core.Nothing
    }

-- | Set this value to limit the number of results returned. If not specified, defaults to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreioMaxResults :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Core.Maybe Core.Int)
dreioMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dreioMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreioNextToken :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Core.Maybe Types.NextToken)
dreioNextToken = Lens.field @"nextToken"
{-# DEPRECATED dreioNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreioReservedElasticsearchInstanceOfferingId :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Core.Maybe Types.ReservedElasticsearchInstanceOfferingId)
dreioReservedElasticsearchInstanceOfferingId = Lens.field @"reservedElasticsearchInstanceOfferingId"
{-# DEPRECATED dreioReservedElasticsearchInstanceOfferingId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferingId' instead." #-}

instance
  Core.AWSRequest
    DescribeReservedElasticsearchInstanceOfferings
  where
  type
    Rs DescribeReservedElasticsearchInstanceOfferings =
      DescribeReservedElasticsearchInstanceOfferingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath "/2015-01-01/es/reservedInstanceOfferings",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> ( Core.toQueryValue "offeringId"
                        Core.<$> reservedElasticsearchInstanceOfferingId
                    ),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservedElasticsearchInstanceOfferingsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "ReservedElasticsearchInstanceOfferings")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Pager.AWSPager
    DescribeReservedElasticsearchInstanceOfferings
  where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"reservedElasticsearchInstanceOfferings"
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Container for results from @DescribeReservedElasticsearchInstanceOfferings@
--
-- /See:/ 'mkDescribeReservedElasticsearchInstanceOfferingsResponse' smart constructor.
data DescribeReservedElasticsearchInstanceOfferingsResponse = DescribeReservedElasticsearchInstanceOfferingsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | List of reserved Elasticsearch instance offerings
    reservedElasticsearchInstanceOfferings :: Core.Maybe [Types.ReservedElasticsearchInstanceOffering],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedElasticsearchInstanceOfferingsResponse' value with any optional fields omitted.
mkDescribeReservedElasticsearchInstanceOfferingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeReservedElasticsearchInstanceOfferingsResponse
mkDescribeReservedElasticsearchInstanceOfferingsResponse
  responseStatus =
    DescribeReservedElasticsearchInstanceOfferingsResponse'
      { nextToken =
          Core.Nothing,
        reservedElasticsearchInstanceOfferings =
          Core.Nothing,
        responseStatus
      }

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiorrsNextToken :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse (Core.Maybe Types.NextToken)
dreiorrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dreiorrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of reserved Elasticsearch instance offerings
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiorrsReservedElasticsearchInstanceOfferings :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse (Core.Maybe [Types.ReservedElasticsearchInstanceOffering])
dreiorrsReservedElasticsearchInstanceOfferings = Lens.field @"reservedElasticsearchInstanceOfferings"
{-# DEPRECATED dreiorrsReservedElasticsearchInstanceOfferings "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiorrsResponseStatus :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse Core.Int
dreiorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dreiorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
