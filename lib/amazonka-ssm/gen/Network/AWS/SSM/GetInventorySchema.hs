{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetInventorySchema (..),
    mkGetInventorySchema,

    -- ** Request lenses
    gisAggregator,
    gisMaxResults,
    gisNextToken,
    gisSubType,
    gisTypeName,

    -- * Destructuring the response
    GetInventorySchemaResponse (..),
    mkGetInventorySchemaResponse,

    -- ** Response lenses
    gisrrsNextToken,
    gisrrsSchemas,
    gisrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetInventorySchema' smart constructor.
data GetInventorySchema = GetInventorySchema'
  { -- | Returns inventory schemas that support aggregation. For example, this call returns the @AWS:InstanceInformation@ type, because it supports aggregation based on the @PlatformName@ , @PlatformType@ , and @PlatformVersion@ attributes.
    aggregator :: Core.Maybe Core.Bool,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | Returns the sub-type schema for a specified inventory type.
    subType :: Core.Maybe Core.Bool,
    -- | The type of inventory item to return.
    typeName :: Core.Maybe Types.TypeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInventorySchema' value with any optional fields omitted.
mkGetInventorySchema ::
  GetInventorySchema
mkGetInventorySchema =
  GetInventorySchema'
    { aggregator = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      subType = Core.Nothing,
      typeName = Core.Nothing
    }

-- | Returns inventory schemas that support aggregation. For example, this call returns the @AWS:InstanceInformation@ type, because it supports aggregation based on the @PlatformName@ , @PlatformType@ , and @PlatformVersion@ attributes.
--
-- /Note:/ Consider using 'aggregator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisAggregator :: Lens.Lens' GetInventorySchema (Core.Maybe Core.Bool)
gisAggregator = Lens.field @"aggregator"
{-# DEPRECATED gisAggregator "Use generic-lens or generic-optics with 'aggregator' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisMaxResults :: Lens.Lens' GetInventorySchema (Core.Maybe Core.Natural)
gisMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gisMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisNextToken :: Lens.Lens' GetInventorySchema (Core.Maybe Types.NextToken)
gisNextToken = Lens.field @"nextToken"
{-# DEPRECATED gisNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns the sub-type schema for a specified inventory type.
--
-- /Note:/ Consider using 'subType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisSubType :: Lens.Lens' GetInventorySchema (Core.Maybe Core.Bool)
gisSubType = Lens.field @"subType"
{-# DEPRECATED gisSubType "Use generic-lens or generic-optics with 'subType' instead." #-}

-- | The type of inventory item to return.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisTypeName :: Lens.Lens' GetInventorySchema (Core.Maybe Types.TypeName)
gisTypeName = Lens.field @"typeName"
{-# DEPRECATED gisTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

instance Core.FromJSON GetInventorySchema where
  toJSON GetInventorySchema {..} =
    Core.object
      ( Core.catMaybes
          [ ("Aggregator" Core..=) Core.<$> aggregator,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SubType" Core..=) Core.<$> subType,
            ("TypeName" Core..=) Core.<$> typeName
          ]
      )

instance Core.AWSRequest GetInventorySchema where
  type Rs GetInventorySchema = GetInventorySchemaResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetInventorySchema")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInventorySchemaResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Schemas")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetInventorySchema where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"schemas" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetInventorySchemaResponse' smart constructor.
data GetInventorySchemaResponse = GetInventorySchemaResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Inventory schemas returned by the request.
    schemas :: Core.Maybe [Types.InventoryItemSchema],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInventorySchemaResponse' value with any optional fields omitted.
mkGetInventorySchemaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInventorySchemaResponse
mkGetInventorySchemaResponse responseStatus =
  GetInventorySchemaResponse'
    { nextToken = Core.Nothing,
      schemas = Core.Nothing,
      responseStatus
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsNextToken :: Lens.Lens' GetInventorySchemaResponse (Core.Maybe Types.NextToken)
gisrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gisrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Inventory schemas returned by the request.
--
-- /Note:/ Consider using 'schemas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsSchemas :: Lens.Lens' GetInventorySchemaResponse (Core.Maybe [Types.InventoryItemSchema])
gisrrsSchemas = Lens.field @"schemas"
{-# DEPRECATED gisrrsSchemas "Use generic-lens or generic-optics with 'schemas' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsResponseStatus :: Lens.Lens' GetInventorySchemaResponse Core.Int
gisrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gisrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
