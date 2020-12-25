{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.DescribeServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata for one service or a list of the metadata for all services. Use this without a service code to get the service codes for all services. Use it with a service code, such as @AmazonEC2@ , to get information specific to that service, such as the attribute names available for that service. For example, some of the attribute names available for EC2 are @volumeType@ , @maxIopsVolume@ , @operation@ , @locationType@ , and @instanceCapacity10xlarge@ .
--
-- This operation returns paginated results.
module Network.AWS.Pricing.DescribeServices
  ( -- * Creating a request
    DescribeServices (..),
    mkDescribeServices,

    -- ** Request lenses
    dsFormatVersion,
    dsMaxResults,
    dsNextToken,
    dsServiceCode,

    -- * Destructuring the response
    DescribeServicesResponse (..),
    mkDescribeServicesResponse,

    -- ** Response lenses
    dsrrsFormatVersion,
    dsrrsNextToken,
    dsrrsServices,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Pricing.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { -- | The format version that you want the response to be in.
    --
    -- Valid values are: @aws_v1@
    formatVersion :: Core.Maybe Types.String,
    -- | The maximum number of results that you want returned in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token that indicates the next set of results that you want to retrieve.
    nextToken :: Core.Maybe Types.String,
    -- | The code for the service whose information you want to retrieve, such as @AmazonEC2@ . You can use the @ServiceCode@ to filter the results in a @GetProducts@ call. To retrieve a list of all services, leave this blank.
    serviceCode :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServices' value with any optional fields omitted.
mkDescribeServices ::
  DescribeServices
mkDescribeServices =
  DescribeServices'
    { formatVersion = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      serviceCode = Core.Nothing
    }

-- | The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFormatVersion :: Lens.Lens' DescribeServices (Core.Maybe Types.String)
dsFormatVersion = Lens.field @"formatVersion"
{-# DEPRECATED dsFormatVersion "Use generic-lens or generic-optics with 'formatVersion' instead." #-}

-- | The maximum number of results that you want returned in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMaxResults :: Lens.Lens' DescribeServices (Core.Maybe Core.Natural)
dsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeServices (Core.Maybe Types.String)
dsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The code for the service whose information you want to retrieve, such as @AmazonEC2@ . You can use the @ServiceCode@ to filter the results in a @GetProducts@ call. To retrieve a list of all services, leave this blank.
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServiceCode :: Lens.Lens' DescribeServices (Core.Maybe Types.String)
dsServiceCode = Lens.field @"serviceCode"
{-# DEPRECATED dsServiceCode "Use generic-lens or generic-optics with 'serviceCode' instead." #-}

instance Core.FromJSON DescribeServices where
  toJSON DescribeServices {..} =
    Core.object
      ( Core.catMaybes
          [ ("FormatVersion" Core..=) Core.<$> formatVersion,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ServiceCode" Core..=) Core.<$> serviceCode
          ]
      )

instance Core.AWSRequest DescribeServices where
  type Rs DescribeServices = DescribeServicesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSPriceListService.DescribeServices")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServicesResponse'
            Core.<$> (x Core..:? "FormatVersion")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Services")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeServices where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"services" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { -- | The format version of the response. For example, @aws_v1@ .
    formatVersion :: Core.Maybe Types.String,
    -- | The pagination token for the next set of retreivable results.
    nextToken :: Core.Maybe Types.String,
    -- | The service metadata for the service or services in the response.
    services :: Core.Maybe [Types.PricingService],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServicesResponse' value with any optional fields omitted.
mkDescribeServicesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeServicesResponse
mkDescribeServicesResponse responseStatus =
  DescribeServicesResponse'
    { formatVersion = Core.Nothing,
      nextToken = Core.Nothing,
      services = Core.Nothing,
      responseStatus
    }

-- | The format version of the response. For example, @aws_v1@ .
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsFormatVersion :: Lens.Lens' DescribeServicesResponse (Core.Maybe Types.String)
dsrrsFormatVersion = Lens.field @"formatVersion"
{-# DEPRECATED dsrrsFormatVersion "Use generic-lens or generic-optics with 'formatVersion' instead." #-}

-- | The pagination token for the next set of retreivable results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsNextToken :: Lens.Lens' DescribeServicesResponse (Core.Maybe Types.String)
dsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The service metadata for the service or services in the response.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsServices :: Lens.Lens' DescribeServicesResponse (Core.Maybe [Types.PricingService])
dsrrsServices = Lens.field @"services"
{-# DEPRECATED dsrrsServices "Use generic-lens or generic-optics with 'services' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeServicesResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
