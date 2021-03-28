{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeServices (..)
    , mkDescribeServices
    -- ** Request lenses
    , dsFormatVersion
    , dsMaxResults
    , dsNextToken
    , dsServiceCode

    -- * Destructuring the response
    , DescribeServicesResponse (..)
    , mkDescribeServicesResponse
    -- ** Response lenses
    , dsrrsFormatVersion
    , dsrrsNextToken
    , dsrrsServices
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Pricing.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { formatVersion :: Core.Maybe Core.Text
    -- ^ The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@ 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results that you want returned in the response.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token that indicates the next set of results that you want to retrieve.
  , serviceCode :: Core.Maybe Core.Text
    -- ^ The code for the service whose information you want to retrieve, such as @AmazonEC2@ . You can use the @ServiceCode@ to filter the results in a @GetProducts@ call. To retrieve a list of all services, leave this blank.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServices' value with any optional fields omitted.
mkDescribeServices
    :: DescribeServices
mkDescribeServices
  = DescribeServices'{formatVersion = Core.Nothing,
                      maxResults = Core.Nothing, nextToken = Core.Nothing,
                      serviceCode = Core.Nothing}

-- | The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@ 
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFormatVersion :: Lens.Lens' DescribeServices (Core.Maybe Core.Text)
dsFormatVersion = Lens.field @"formatVersion"
{-# INLINEABLE dsFormatVersion #-}
{-# DEPRECATED formatVersion "Use generic-lens or generic-optics with 'formatVersion' instead"  #-}

-- | The maximum number of results that you want returned in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMaxResults :: Lens.Lens' DescribeServices (Core.Maybe Core.Natural)
dsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeServices (Core.Maybe Core.Text)
dsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The code for the service whose information you want to retrieve, such as @AmazonEC2@ . You can use the @ServiceCode@ to filter the results in a @GetProducts@ call. To retrieve a list of all services, leave this blank.
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServiceCode :: Lens.Lens' DescribeServices (Core.Maybe Core.Text)
dsServiceCode = Lens.field @"serviceCode"
{-# INLINEABLE dsServiceCode #-}
{-# DEPRECATED serviceCode "Use generic-lens or generic-optics with 'serviceCode' instead"  #-}

instance Core.ToQuery DescribeServices where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeServices where
        toHeaders DescribeServices{..}
          = Core.pure
              ("X-Amz-Target", "AWSPriceListService.DescribeServices")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeServices where
        toJSON DescribeServices{..}
          = Core.object
              (Core.catMaybes
                 [("FormatVersion" Core..=) Core.<$> formatVersion,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ServiceCode" Core..=) Core.<$> serviceCode])

instance Core.AWSRequest DescribeServices where
        type Rs DescribeServices = DescribeServicesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeServicesResponse' Core.<$>
                   (x Core..:? "FormatVersion") Core.<*> x Core..:? "NextToken"
                     Core.<*> x Core..:? "Services"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeServices where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"services" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { formatVersion :: Core.Maybe Core.Text
    -- ^ The format version of the response. For example, @aws_v1@ .
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token for the next set of retreivable results.
  , services :: Core.Maybe [Types.PricingService]
    -- ^ The service metadata for the service or services in the response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServicesResponse' value with any optional fields omitted.
mkDescribeServicesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeServicesResponse
mkDescribeServicesResponse responseStatus
  = DescribeServicesResponse'{formatVersion = Core.Nothing,
                              nextToken = Core.Nothing, services = Core.Nothing, responseStatus}

-- | The format version of the response. For example, @aws_v1@ .
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsFormatVersion :: Lens.Lens' DescribeServicesResponse (Core.Maybe Core.Text)
dsrrsFormatVersion = Lens.field @"formatVersion"
{-# INLINEABLE dsrrsFormatVersion #-}
{-# DEPRECATED formatVersion "Use generic-lens or generic-optics with 'formatVersion' instead"  #-}

-- | The pagination token for the next set of retreivable results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsNextToken :: Lens.Lens' DescribeServicesResponse (Core.Maybe Core.Text)
dsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The service metadata for the service or services in the response.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsServices :: Lens.Lens' DescribeServicesResponse (Core.Maybe [Types.PricingService])
dsrrsServices = Lens.field @"services"
{-# INLINEABLE dsrrsServices #-}
{-# DEPRECATED services "Use generic-lens or generic-optics with 'services' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeServicesResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
