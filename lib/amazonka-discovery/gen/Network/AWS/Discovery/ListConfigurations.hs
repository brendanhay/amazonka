{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.ListConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configuration items as specified by the value passed to the required parameter @configurationType@ . Optional filtering may be applied to refine search results.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.ListConfigurations
    (
    -- * Creating a request
      ListConfigurations (..)
    , mkListConfigurations
    -- ** Request lenses
    , lcConfigurationType
    , lcFilters
    , lcMaxResults
    , lcNextToken
    , lcOrderBy

    -- * Destructuring the response
    , ListConfigurationsResponse (..)
    , mkListConfigurationsResponse
    -- ** Response lenses
    , lcrrsConfigurations
    , lcrrsNextToken
    , lcrrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListConfigurations' smart constructor.
data ListConfigurations = ListConfigurations'
  { configurationType :: Types.ConfigurationItemType
    -- ^ A valid configuration identified by Application Discovery Service. 
  , filters :: Core.Maybe [Types.Filter]
    -- ^ You can filter the request using various logical operators and a /key/ -/value/ format. For example: 
--
-- @{"key": "serverType", "value": "webServer"}@ 
-- For a complete list of filter options and guidance about using them with this action, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action> in the /AWS Application Discovery Service User Guide/ .
  , maxResults :: Core.Maybe Core.Int
    -- ^ The total number of items to return. The maximum value is 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Token to retrieve the next set of results. For example, if a previous call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
  , orderBy :: Core.Maybe [Types.OrderByElement]
    -- ^ Certain filter criteria return output that can be sorted in ascending or descending order. For a list of output characteristics for each filter, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action> in the /AWS Application Discovery Service User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConfigurations' value with any optional fields omitted.
mkListConfigurations
    :: Types.ConfigurationItemType -- ^ 'configurationType'
    -> ListConfigurations
mkListConfigurations configurationType
  = ListConfigurations'{configurationType, filters = Core.Nothing,
                        maxResults = Core.Nothing, nextToken = Core.Nothing,
                        orderBy = Core.Nothing}

-- | A valid configuration identified by Application Discovery Service. 
--
-- /Note:/ Consider using 'configurationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcConfigurationType :: Lens.Lens' ListConfigurations Types.ConfigurationItemType
lcConfigurationType = Lens.field @"configurationType"
{-# INLINEABLE lcConfigurationType #-}
{-# DEPRECATED configurationType "Use generic-lens or generic-optics with 'configurationType' instead"  #-}

-- | You can filter the request using various logical operators and a /key/ -/value/ format. For example: 
--
-- @{"key": "serverType", "value": "webServer"}@ 
-- For a complete list of filter options and guidance about using them with this action, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action> in the /AWS Application Discovery Service User Guide/ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcFilters :: Lens.Lens' ListConfigurations (Core.Maybe [Types.Filter])
lcFilters = Lens.field @"filters"
{-# INLINEABLE lcFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The total number of items to return. The maximum value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListConfigurations (Core.Maybe Core.Int)
lcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Token to retrieve the next set of results. For example, if a previous call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListConfigurations (Core.Maybe Types.NextToken)
lcNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Certain filter criteria return output that can be sorted in ascending or descending order. For a list of output characteristics for each filter, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action> in the /AWS Application Discovery Service User Guide/ .
--
-- /Note:/ Consider using 'orderBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcOrderBy :: Lens.Lens' ListConfigurations (Core.Maybe [Types.OrderByElement])
lcOrderBy = Lens.field @"orderBy"
{-# INLINEABLE lcOrderBy #-}
{-# DEPRECATED orderBy "Use generic-lens or generic-optics with 'orderBy' instead"  #-}

instance Core.ToQuery ListConfigurations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListConfigurations where
        toHeaders ListConfigurations{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSPoseidonService_V2015_11_01.ListConfigurations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListConfigurations where
        toJSON ListConfigurations{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("configurationType" Core..= configurationType),
                  ("filters" Core..=) Core.<$> filters,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("orderBy" Core..=) Core.<$> orderBy])

instance Core.AWSRequest ListConfigurations where
        type Rs ListConfigurations = ListConfigurationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListConfigurationsResponse' Core.<$>
                   (x Core..:? "configurations") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListConfigurations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"configurations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListConfigurationsResponse' smart constructor.
data ListConfigurationsResponse = ListConfigurationsResponse'
  { configurations :: Core.Maybe [Core.HashMap Core.Text Core.Text]
    -- ^ Returns configuration details, including the configuration ID, attribute names, and attribute values.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Token to retrieve the next set of results. For example, if your call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConfigurationsResponse' value with any optional fields omitted.
mkListConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListConfigurationsResponse
mkListConfigurationsResponse responseStatus
  = ListConfigurationsResponse'{configurations = Core.Nothing,
                                nextToken = Core.Nothing, responseStatus}

-- | Returns configuration details, including the configuration ID, attribute names, and attribute values.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsConfigurations :: Lens.Lens' ListConfigurationsResponse (Core.Maybe [Core.HashMap Core.Text Core.Text])
lcrrsConfigurations = Lens.field @"configurations"
{-# INLINEABLE lcrrsConfigurations #-}
{-# DEPRECATED configurations "Use generic-lens or generic-optics with 'configurations' instead"  #-}

-- | Token to retrieve the next set of results. For example, if your call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListConfigurationsResponse (Core.Maybe Types.NextToken)
lcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListConfigurationsResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
