{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.GetProducts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all products that match the filter criteria.
--
-- This operation returns paginated results.
module Network.AWS.Pricing.GetProducts
  ( -- * Creating a request
    GetProducts (..),
    mkGetProducts,

    -- ** Request lenses
    gpFilters,
    gpFormatVersion,
    gpMaxResults,
    gpNextToken,
    gpServiceCode,

    -- * Destructuring the response
    GetProductsResponse (..),
    mkGetProductsResponse,

    -- ** Response lenses
    gprrsFormatVersion,
    gprrsNextToken,
    gprrsPriceList,
    gprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Pricing.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetProducts' smart constructor.
data GetProducts = GetProducts'
  { -- | The list of filters that limit the returned products. only products that match all filters are returned.
    filters :: Core.Maybe [Types.Filter],
    -- | The format version that you want the response to be in.
    --
    -- Valid values are: @aws_v1@
    formatVersion :: Core.Maybe Types.String,
    -- | The maximum number of results to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token that indicates the next set of results that you want to retrieve.
    nextToken :: Core.Maybe Types.String,
    -- | The code for the service whose products you want to retrieve.
    serviceCode :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProducts' value with any optional fields omitted.
mkGetProducts ::
  GetProducts
mkGetProducts =
  GetProducts'
    { filters = Core.Nothing,
      formatVersion = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      serviceCode = Core.Nothing
    }

-- | The list of filters that limit the returned products. only products that match all filters are returned.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpFilters :: Lens.Lens' GetProducts (Core.Maybe [Types.Filter])
gpFilters = Lens.field @"filters"
{-# DEPRECATED gpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpFormatVersion :: Lens.Lens' GetProducts (Core.Maybe Types.String)
gpFormatVersion = Lens.field @"formatVersion"
{-# DEPRECATED gpFormatVersion "Use generic-lens or generic-optics with 'formatVersion' instead." #-}

-- | The maximum number of results to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpMaxResults :: Lens.Lens' GetProducts (Core.Maybe Core.Natural)
gpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpNextToken :: Lens.Lens' GetProducts (Core.Maybe Types.String)
gpNextToken = Lens.field @"nextToken"
{-# DEPRECATED gpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The code for the service whose products you want to retrieve.
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpServiceCode :: Lens.Lens' GetProducts (Core.Maybe Types.String)
gpServiceCode = Lens.field @"serviceCode"
{-# DEPRECATED gpServiceCode "Use generic-lens or generic-optics with 'serviceCode' instead." #-}

instance Core.FromJSON GetProducts where
  toJSON GetProducts {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("FormatVersion" Core..=) Core.<$> formatVersion,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ServiceCode" Core..=) Core.<$> serviceCode
          ]
      )

instance Core.AWSRequest GetProducts where
  type Rs GetProducts = GetProductsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSPriceListService.GetProducts")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProductsResponse'
            Core.<$> (x Core..:? "FormatVersion")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "PriceList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetProducts where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"priceList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetProductsResponse' smart constructor.
data GetProductsResponse = GetProductsResponse'
  { -- | The format version of the response. For example, aws_v1.
    formatVersion :: Core.Maybe Types.FormatVersion,
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The list of products that match your filters. The list contains both the product metadata and the price information.
    priceList :: Core.Maybe [Types.PriceListItemJSON],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProductsResponse' value with any optional fields omitted.
mkGetProductsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetProductsResponse
mkGetProductsResponse responseStatus =
  GetProductsResponse'
    { formatVersion = Core.Nothing,
      nextToken = Core.Nothing,
      priceList = Core.Nothing,
      responseStatus
    }

-- | The format version of the response. For example, aws_v1.
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsFormatVersion :: Lens.Lens' GetProductsResponse (Core.Maybe Types.FormatVersion)
gprrsFormatVersion = Lens.field @"formatVersion"
{-# DEPRECATED gprrsFormatVersion "Use generic-lens or generic-optics with 'formatVersion' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsNextToken :: Lens.Lens' GetProductsResponse (Core.Maybe Types.NextToken)
gprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of products that match your filters. The list contains both the product metadata and the price information.
--
-- /Note:/ Consider using 'priceList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPriceList :: Lens.Lens' GetProductsResponse (Core.Maybe [Types.PriceListItemJSON])
gprrsPriceList = Lens.field @"priceList"
{-# DEPRECATED gprrsPriceList "Use generic-lens or generic-optics with 'priceList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetProductsResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
