{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetCardinality
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the approximate count of unique values that match the query.
module Network.AWS.IoT.GetCardinality
  ( -- * Creating a request
    GetCardinality (..),
    mkGetCardinality,

    -- ** Request lenses
    gcQueryString,
    gcAggregationField,
    gcIndexName,
    gcQueryVersion,

    -- * Destructuring the response
    GetCardinalityResponse (..),
    mkGetCardinalityResponse,

    -- ** Response lenses
    gcrrsCardinality,
    gcrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCardinality' smart constructor.
data GetCardinality = GetCardinality'
  { -- | The search query.
    queryString :: Types.QueryString,
    -- | The field to aggregate.
    aggregationField :: Core.Maybe Types.AggregationField,
    -- | The name of the index to search.
    indexName :: Core.Maybe Types.IndexName,
    -- | The query version.
    queryVersion :: Core.Maybe Types.QueryVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCardinality' value with any optional fields omitted.
mkGetCardinality ::
  -- | 'queryString'
  Types.QueryString ->
  GetCardinality
mkGetCardinality queryString =
  GetCardinality'
    { queryString,
      aggregationField = Core.Nothing,
      indexName = Core.Nothing,
      queryVersion = Core.Nothing
    }

-- | The search query.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcQueryString :: Lens.Lens' GetCardinality Types.QueryString
gcQueryString = Lens.field @"queryString"
{-# DEPRECATED gcQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The field to aggregate.
--
-- /Note:/ Consider using 'aggregationField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcAggregationField :: Lens.Lens' GetCardinality (Core.Maybe Types.AggregationField)
gcAggregationField = Lens.field @"aggregationField"
{-# DEPRECATED gcAggregationField "Use generic-lens or generic-optics with 'aggregationField' instead." #-}

-- | The name of the index to search.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcIndexName :: Lens.Lens' GetCardinality (Core.Maybe Types.IndexName)
gcIndexName = Lens.field @"indexName"
{-# DEPRECATED gcIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcQueryVersion :: Lens.Lens' GetCardinality (Core.Maybe Types.QueryVersion)
gcQueryVersion = Lens.field @"queryVersion"
{-# DEPRECATED gcQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

instance Core.FromJSON GetCardinality where
  toJSON GetCardinality {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("queryString" Core..= queryString),
            ("aggregationField" Core..=) Core.<$> aggregationField,
            ("indexName" Core..=) Core.<$> indexName,
            ("queryVersion" Core..=) Core.<$> queryVersion
          ]
      )

instance Core.AWSRequest GetCardinality where
  type Rs GetCardinality = GetCardinalityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/indices/cardinality",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCardinalityResponse'
            Core.<$> (x Core..:? "cardinality") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCardinalityResponse' smart constructor.
data GetCardinalityResponse = GetCardinalityResponse'
  { -- | The approximate count of unique values that match the query.
    cardinality :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCardinalityResponse' value with any optional fields omitted.
mkGetCardinalityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCardinalityResponse
mkGetCardinalityResponse responseStatus =
  GetCardinalityResponse'
    { cardinality = Core.Nothing,
      responseStatus
    }

-- | The approximate count of unique values that match the query.
--
-- /Note:/ Consider using 'cardinality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsCardinality :: Lens.Lens' GetCardinalityResponse (Core.Maybe Core.Int)
gcrrsCardinality = Lens.field @"cardinality"
{-# DEPRECATED gcrrsCardinality "Use generic-lens or generic-optics with 'cardinality' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetCardinalityResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
