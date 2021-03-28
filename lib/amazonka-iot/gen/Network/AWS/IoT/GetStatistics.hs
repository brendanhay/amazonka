{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the count, average, sum, minimum, maximum, sum of squares, variance, and standard deviation for the specified aggregated field. If the aggregation field is of type @String@ , only the count statistic is returned.
module Network.AWS.IoT.GetStatistics
    (
    -- * Creating a request
      GetStatistics (..)
    , mkGetStatistics
    -- ** Request lenses
    , gsQueryString
    , gsAggregationField
    , gsIndexName
    , gsQueryVersion

    -- * Destructuring the response
    , GetStatisticsResponse (..)
    , mkGetStatisticsResponse
    -- ** Response lenses
    , gsrrsStatistics
    , gsrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetStatistics' smart constructor.
data GetStatistics = GetStatistics'
  { queryString :: Types.QueryString
    -- ^ The query used to search. You can specify "*" for the query string to get the count of all indexed things in your AWS account.
  , aggregationField :: Core.Maybe Types.AggregationField
    -- ^ The aggregation field name.
  , indexName :: Core.Maybe Types.IndexName
    -- ^ The name of the index to search. The default value is @AWS_Things@ .
  , queryVersion :: Core.Maybe Types.QueryVersion
    -- ^ The version of the query used to search.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetStatistics' value with any optional fields omitted.
mkGetStatistics
    :: Types.QueryString -- ^ 'queryString'
    -> GetStatistics
mkGetStatistics queryString
  = GetStatistics'{queryString, aggregationField = Core.Nothing,
                   indexName = Core.Nothing, queryVersion = Core.Nothing}

-- | The query used to search. You can specify "*" for the query string to get the count of all indexed things in your AWS account.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsQueryString :: Lens.Lens' GetStatistics Types.QueryString
gsQueryString = Lens.field @"queryString"
{-# INLINEABLE gsQueryString #-}
{-# DEPRECATED queryString "Use generic-lens or generic-optics with 'queryString' instead"  #-}

-- | The aggregation field name.
--
-- /Note:/ Consider using 'aggregationField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsAggregationField :: Lens.Lens' GetStatistics (Core.Maybe Types.AggregationField)
gsAggregationField = Lens.field @"aggregationField"
{-# INLINEABLE gsAggregationField #-}
{-# DEPRECATED aggregationField "Use generic-lens or generic-optics with 'aggregationField' instead"  #-}

-- | The name of the index to search. The default value is @AWS_Things@ .
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsIndexName :: Lens.Lens' GetStatistics (Core.Maybe Types.IndexName)
gsIndexName = Lens.field @"indexName"
{-# INLINEABLE gsIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | The version of the query used to search.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsQueryVersion :: Lens.Lens' GetStatistics (Core.Maybe Types.QueryVersion)
gsQueryVersion = Lens.field @"queryVersion"
{-# INLINEABLE gsQueryVersion #-}
{-# DEPRECATED queryVersion "Use generic-lens or generic-optics with 'queryVersion' instead"  #-}

instance Core.ToQuery GetStatistics where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetStatistics where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetStatistics where
        toJSON GetStatistics{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("queryString" Core..= queryString),
                  ("aggregationField" Core..=) Core.<$> aggregationField,
                  ("indexName" Core..=) Core.<$> indexName,
                  ("queryVersion" Core..=) Core.<$> queryVersion])

instance Core.AWSRequest GetStatistics where
        type Rs GetStatistics = GetStatisticsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/indices/statistics",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetStatisticsResponse' Core.<$>
                   (x Core..:? "statistics") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetStatisticsResponse' smart constructor.
data GetStatisticsResponse = GetStatisticsResponse'
  { statistics :: Core.Maybe Types.Statistics
    -- ^ The statistics returned by the Fleet Indexing service based on the query and aggregation field.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetStatisticsResponse' value with any optional fields omitted.
mkGetStatisticsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetStatisticsResponse
mkGetStatisticsResponse responseStatus
  = GetStatisticsResponse'{statistics = Core.Nothing, responseStatus}

-- | The statistics returned by the Fleet Indexing service based on the query and aggregation field.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsStatistics :: Lens.Lens' GetStatisticsResponse (Core.Maybe Types.Statistics)
gsrrsStatistics = Lens.field @"statistics"
{-# INLINEABLE gsrrsStatistics #-}
{-# DEPRECATED statistics "Use generic-lens or generic-optics with 'statistics' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetStatisticsResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
