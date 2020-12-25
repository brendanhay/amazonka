{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetPercentiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Groups the aggregated values that match the query into percentile groupings. The default percentile groupings are: 1,5,25,50,75,95,99, although you can specify your own when you call @GetPercentiles@ . This function returns a value for each percentile group specified (or the default percentile groupings). The percentile group "1" contains the aggregated field value that occurs in approximately one percent of the values that match the query. The percentile group "5" contains the aggregated field value that occurs in approximately five percent of the values that match the query, and so on. The result is an approximation, the more values that match the query, the more accurate the percentile values.
module Network.AWS.IoT.GetPercentiles
  ( -- * Creating a request
    GetPercentiles (..),
    mkGetPercentiles,

    -- ** Request lenses
    gpQueryString,
    gpAggregationField,
    gpIndexName,
    gpPercents,
    gpQueryVersion,

    -- * Destructuring the response
    GetPercentilesResponse (..),
    mkGetPercentilesResponse,

    -- ** Response lenses
    grsPercentiles,
    grsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPercentiles' smart constructor.
data GetPercentiles = GetPercentiles'
  { -- | The query string.
    queryString :: Types.QueryString,
    -- | The field to aggregate.
    aggregationField :: Core.Maybe Types.AggregationField,
    -- | The name of the index to search.
    indexName :: Core.Maybe Types.IndexName,
    -- | The percentile groups returned.
    percents :: Core.Maybe [Core.Double],
    -- | The query version.
    queryVersion :: Core.Maybe Types.QueryVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPercentiles' value with any optional fields omitted.
mkGetPercentiles ::
  -- | 'queryString'
  Types.QueryString ->
  GetPercentiles
mkGetPercentiles queryString =
  GetPercentiles'
    { queryString,
      aggregationField = Core.Nothing,
      indexName = Core.Nothing,
      percents = Core.Nothing,
      queryVersion = Core.Nothing
    }

-- | The query string.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpQueryString :: Lens.Lens' GetPercentiles Types.QueryString
gpQueryString = Lens.field @"queryString"
{-# DEPRECATED gpQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The field to aggregate.
--
-- /Note:/ Consider using 'aggregationField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpAggregationField :: Lens.Lens' GetPercentiles (Core.Maybe Types.AggregationField)
gpAggregationField = Lens.field @"aggregationField"
{-# DEPRECATED gpAggregationField "Use generic-lens or generic-optics with 'aggregationField' instead." #-}

-- | The name of the index to search.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpIndexName :: Lens.Lens' GetPercentiles (Core.Maybe Types.IndexName)
gpIndexName = Lens.field @"indexName"
{-# DEPRECATED gpIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The percentile groups returned.
--
-- /Note:/ Consider using 'percents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPercents :: Lens.Lens' GetPercentiles (Core.Maybe [Core.Double])
gpPercents = Lens.field @"percents"
{-# DEPRECATED gpPercents "Use generic-lens or generic-optics with 'percents' instead." #-}

-- | The query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpQueryVersion :: Lens.Lens' GetPercentiles (Core.Maybe Types.QueryVersion)
gpQueryVersion = Lens.field @"queryVersion"
{-# DEPRECATED gpQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

instance Core.FromJSON GetPercentiles where
  toJSON GetPercentiles {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("queryString" Core..= queryString),
            ("aggregationField" Core..=) Core.<$> aggregationField,
            ("indexName" Core..=) Core.<$> indexName,
            ("percents" Core..=) Core.<$> percents,
            ("queryVersion" Core..=) Core.<$> queryVersion
          ]
      )

instance Core.AWSRequest GetPercentiles where
  type Rs GetPercentiles = GetPercentilesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/indices/percentiles",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPercentilesResponse'
            Core.<$> (x Core..:? "percentiles") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetPercentilesResponse' smart constructor.
data GetPercentilesResponse = GetPercentilesResponse'
  { -- | The percentile values of the aggregated fields.
    percentiles :: Core.Maybe [Types.PercentPair],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPercentilesResponse' value with any optional fields omitted.
mkGetPercentilesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPercentilesResponse
mkGetPercentilesResponse responseStatus =
  GetPercentilesResponse'
    { percentiles = Core.Nothing,
      responseStatus
    }

-- | The percentile values of the aggregated fields.
--
-- /Note:/ Consider using 'percentiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsPercentiles :: Lens.Lens' GetPercentilesResponse (Core.Maybe [Types.PercentPair])
grsPercentiles = Lens.field @"percentiles"
{-# DEPRECATED grsPercentiles "Use generic-lens or generic-optics with 'percentiles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetPercentilesResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
