{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetInventorySchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a list of inventory type names for the account, or return a list of attribute names for a specific Inventory item type.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetInventorySchema
    (
    -- * Creating a request
      GetInventorySchema (..)
    , mkGetInventorySchema
    -- ** Request lenses
    , gisAggregator
    , gisMaxResults
    , gisNextToken
    , gisSubType
    , gisTypeName

    -- * Destructuring the response
    , GetInventorySchemaResponse (..)
    , mkGetInventorySchemaResponse
    -- ** Response lenses
    , gisrrsNextToken
    , gisrrsSchemas
    , gisrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetInventorySchema' smart constructor.
data GetInventorySchema = GetInventorySchema'
  { aggregator :: Core.Maybe Core.Bool
    -- ^ Returns inventory schemas that support aggregation. For example, this call returns the @AWS:InstanceInformation@ type, because it supports aggregation based on the @PlatformName@ , @PlatformType@ , and @PlatformVersion@ attributes.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  , subType :: Core.Maybe Core.Bool
    -- ^ Returns the sub-type schema for a specified inventory type.
  , typeName :: Core.Maybe Types.TypeName
    -- ^ The type of inventory item to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInventorySchema' value with any optional fields omitted.
mkGetInventorySchema
    :: GetInventorySchema
mkGetInventorySchema
  = GetInventorySchema'{aggregator = Core.Nothing,
                        maxResults = Core.Nothing, nextToken = Core.Nothing,
                        subType = Core.Nothing, typeName = Core.Nothing}

-- | Returns inventory schemas that support aggregation. For example, this call returns the @AWS:InstanceInformation@ type, because it supports aggregation based on the @PlatformName@ , @PlatformType@ , and @PlatformVersion@ attributes.
--
-- /Note:/ Consider using 'aggregator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisAggregator :: Lens.Lens' GetInventorySchema (Core.Maybe Core.Bool)
gisAggregator = Lens.field @"aggregator"
{-# INLINEABLE gisAggregator #-}
{-# DEPRECATED aggregator "Use generic-lens or generic-optics with 'aggregator' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisMaxResults :: Lens.Lens' GetInventorySchema (Core.Maybe Core.Natural)
gisMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gisMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisNextToken :: Lens.Lens' GetInventorySchema (Core.Maybe Types.NextToken)
gisNextToken = Lens.field @"nextToken"
{-# INLINEABLE gisNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Returns the sub-type schema for a specified inventory type.
--
-- /Note:/ Consider using 'subType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisSubType :: Lens.Lens' GetInventorySchema (Core.Maybe Core.Bool)
gisSubType = Lens.field @"subType"
{-# INLINEABLE gisSubType #-}
{-# DEPRECATED subType "Use generic-lens or generic-optics with 'subType' instead"  #-}

-- | The type of inventory item to return.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisTypeName :: Lens.Lens' GetInventorySchema (Core.Maybe Types.TypeName)
gisTypeName = Lens.field @"typeName"
{-# INLINEABLE gisTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

instance Core.ToQuery GetInventorySchema where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetInventorySchema where
        toHeaders GetInventorySchema{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.GetInventorySchema")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetInventorySchema where
        toJSON GetInventorySchema{..}
          = Core.object
              (Core.catMaybes
                 [("Aggregator" Core..=) Core.<$> aggregator,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SubType" Core..=) Core.<$> subType,
                  ("TypeName" Core..=) Core.<$> typeName])

instance Core.AWSRequest GetInventorySchema where
        type Rs GetInventorySchema = GetInventorySchemaResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetInventorySchemaResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Schemas" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetInventorySchema where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"schemas" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetInventorySchemaResponse' smart constructor.
data GetInventorySchemaResponse = GetInventorySchemaResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
  , schemas :: Core.Maybe [Types.InventoryItemSchema]
    -- ^ Inventory schemas returned by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInventorySchemaResponse' value with any optional fields omitted.
mkGetInventorySchemaResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetInventorySchemaResponse
mkGetInventorySchemaResponse responseStatus
  = GetInventorySchemaResponse'{nextToken = Core.Nothing,
                                schemas = Core.Nothing, responseStatus}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsNextToken :: Lens.Lens' GetInventorySchemaResponse (Core.Maybe Types.NextToken)
gisrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gisrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Inventory schemas returned by the request.
--
-- /Note:/ Consider using 'schemas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsSchemas :: Lens.Lens' GetInventorySchemaResponse (Core.Maybe [Types.InventoryItemSchema])
gisrrsSchemas = Lens.field @"schemas"
{-# INLINEABLE gisrrsSchemas #-}
{-# DEPRECATED schemas "Use generic-lens or generic-optics with 'schemas' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsResponseStatus :: Lens.Lens' GetInventorySchemaResponse Core.Int
gisrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gisrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
