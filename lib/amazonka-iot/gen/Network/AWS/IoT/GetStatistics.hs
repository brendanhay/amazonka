{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetStatistics (..),
    mkGetStatistics,

    -- ** Request lenses
    gsQueryString,
    gsAggregationField,
    gsIndexName,
    gsQueryVersion,

    -- * Destructuring the response
    GetStatisticsResponse (..),
    mkGetStatisticsResponse,

    -- ** Response lenses
    gsrrsStatistics,
    gsrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetStatistics' smart constructor.
data GetStatistics = GetStatistics'
  { -- | The query used to search. You can specify "*" for the query string to get the count of all indexed things in your AWS account.
    queryString :: Types.QueryString,
    -- | The aggregation field name.
    aggregationField :: Core.Maybe Types.AggregationField,
    -- | The name of the index to search. The default value is @AWS_Things@ .
    indexName :: Core.Maybe Types.IndexName,
    -- | The version of the query used to search.
    queryVersion :: Core.Maybe Types.QueryVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetStatistics' value with any optional fields omitted.
mkGetStatistics ::
  -- | 'queryString'
  Types.QueryString ->
  GetStatistics
mkGetStatistics queryString =
  GetStatistics'
    { queryString,
      aggregationField = Core.Nothing,
      indexName = Core.Nothing,
      queryVersion = Core.Nothing
    }

-- | The query used to search. You can specify "*" for the query string to get the count of all indexed things in your AWS account.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsQueryString :: Lens.Lens' GetStatistics Types.QueryString
gsQueryString = Lens.field @"queryString"
{-# DEPRECATED gsQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The aggregation field name.
--
-- /Note:/ Consider using 'aggregationField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsAggregationField :: Lens.Lens' GetStatistics (Core.Maybe Types.AggregationField)
gsAggregationField = Lens.field @"aggregationField"
{-# DEPRECATED gsAggregationField "Use generic-lens or generic-optics with 'aggregationField' instead." #-}

-- | The name of the index to search. The default value is @AWS_Things@ .
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsIndexName :: Lens.Lens' GetStatistics (Core.Maybe Types.IndexName)
gsIndexName = Lens.field @"indexName"
{-# DEPRECATED gsIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The version of the query used to search.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsQueryVersion :: Lens.Lens' GetStatistics (Core.Maybe Types.QueryVersion)
gsQueryVersion = Lens.field @"queryVersion"
{-# DEPRECATED gsQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

instance Core.FromJSON GetStatistics where
  toJSON GetStatistics {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("queryString" Core..= queryString),
            ("aggregationField" Core..=) Core.<$> aggregationField,
            ("indexName" Core..=) Core.<$> indexName,
            ("queryVersion" Core..=) Core.<$> queryVersion
          ]
      )

instance Core.AWSRequest GetStatistics where
  type Rs GetStatistics = GetStatisticsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/indices/statistics",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStatisticsResponse'
            Core.<$> (x Core..:? "statistics") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetStatisticsResponse' smart constructor.
data GetStatisticsResponse = GetStatisticsResponse'
  { -- | The statistics returned by the Fleet Indexing service based on the query and aggregation field.
    statistics :: Core.Maybe Types.Statistics,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetStatisticsResponse' value with any optional fields omitted.
mkGetStatisticsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetStatisticsResponse
mkGetStatisticsResponse responseStatus =
  GetStatisticsResponse' {statistics = Core.Nothing, responseStatus}

-- | The statistics returned by the Fleet Indexing service based on the query and aggregation field.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsStatistics :: Lens.Lens' GetStatisticsResponse (Core.Maybe Types.Statistics)
gsrrsStatistics = Lens.field @"statistics"
{-# DEPRECATED gsrrsStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetStatisticsResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
