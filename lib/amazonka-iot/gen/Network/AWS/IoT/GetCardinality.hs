{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetCardinality (..)
    , mkGetCardinality
    -- ** Request lenses
    , gcQueryString
    , gcAggregationField
    , gcIndexName
    , gcQueryVersion

    -- * Destructuring the response
    , GetCardinalityResponse (..)
    , mkGetCardinalityResponse
    -- ** Response lenses
    , gcrrsCardinality
    , gcrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCardinality' smart constructor.
data GetCardinality = GetCardinality'
  { queryString :: Types.QueryString
    -- ^ The search query.
  , aggregationField :: Core.Maybe Types.AggregationField
    -- ^ The field to aggregate.
  , indexName :: Core.Maybe Types.IndexName
    -- ^ The name of the index to search.
  , queryVersion :: Core.Maybe Types.QueryVersion
    -- ^ The query version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCardinality' value with any optional fields omitted.
mkGetCardinality
    :: Types.QueryString -- ^ 'queryString'
    -> GetCardinality
mkGetCardinality queryString
  = GetCardinality'{queryString, aggregationField = Core.Nothing,
                    indexName = Core.Nothing, queryVersion = Core.Nothing}

-- | The search query.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcQueryString :: Lens.Lens' GetCardinality Types.QueryString
gcQueryString = Lens.field @"queryString"
{-# INLINEABLE gcQueryString #-}
{-# DEPRECATED queryString "Use generic-lens or generic-optics with 'queryString' instead"  #-}

-- | The field to aggregate.
--
-- /Note:/ Consider using 'aggregationField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcAggregationField :: Lens.Lens' GetCardinality (Core.Maybe Types.AggregationField)
gcAggregationField = Lens.field @"aggregationField"
{-# INLINEABLE gcAggregationField #-}
{-# DEPRECATED aggregationField "Use generic-lens or generic-optics with 'aggregationField' instead"  #-}

-- | The name of the index to search.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcIndexName :: Lens.Lens' GetCardinality (Core.Maybe Types.IndexName)
gcIndexName = Lens.field @"indexName"
{-# INLINEABLE gcIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | The query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcQueryVersion :: Lens.Lens' GetCardinality (Core.Maybe Types.QueryVersion)
gcQueryVersion = Lens.field @"queryVersion"
{-# INLINEABLE gcQueryVersion #-}
{-# DEPRECATED queryVersion "Use generic-lens or generic-optics with 'queryVersion' instead"  #-}

instance Core.ToQuery GetCardinality where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCardinality where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetCardinality where
        toJSON GetCardinality{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("queryString" Core..= queryString),
                  ("aggregationField" Core..=) Core.<$> aggregationField,
                  ("indexName" Core..=) Core.<$> indexName,
                  ("queryVersion" Core..=) Core.<$> queryVersion])

instance Core.AWSRequest GetCardinality where
        type Rs GetCardinality = GetCardinalityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/indices/cardinality",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCardinalityResponse' Core.<$>
                   (x Core..:? "cardinality") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCardinalityResponse' smart constructor.
data GetCardinalityResponse = GetCardinalityResponse'
  { cardinality :: Core.Maybe Core.Int
    -- ^ The approximate count of unique values that match the query.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCardinalityResponse' value with any optional fields omitted.
mkGetCardinalityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCardinalityResponse
mkGetCardinalityResponse responseStatus
  = GetCardinalityResponse'{cardinality = Core.Nothing,
                            responseStatus}

-- | The approximate count of unique values that match the query.
--
-- /Note:/ Consider using 'cardinality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsCardinality :: Lens.Lens' GetCardinalityResponse (Core.Maybe Core.Int)
gcrrsCardinality = Lens.field @"cardinality"
{-# INLINEABLE gcrrsCardinality #-}
{-# DEPRECATED cardinality "Use generic-lens or generic-optics with 'cardinality' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetCardinalityResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
