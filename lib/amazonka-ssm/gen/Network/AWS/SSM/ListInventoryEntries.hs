{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListInventoryEntries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of inventory items returned by the request.
module Network.AWS.SSM.ListInventoryEntries
  ( -- * Creating a request
    ListInventoryEntries (..),
    mkListInventoryEntries,

    -- ** Request lenses
    lieInstanceId,
    lieTypeName,
    lieFilters,
    lieMaxResults,
    lieNextToken,

    -- * Destructuring the response
    ListInventoryEntriesResponse (..),
    mkListInventoryEntriesResponse,

    -- ** Response lenses
    lierrsCaptureTime,
    lierrsEntries,
    lierrsInstanceId,
    lierrsNextToken,
    lierrsSchemaVersion,
    lierrsTypeName,
    lierrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkListInventoryEntries' smart constructor.
data ListInventoryEntries = ListInventoryEntries'
  { -- | The instance ID for which you want inventory information.
    instanceId :: Types.InstanceId,
    -- | The type of inventory item for which you want information.
    typeName :: Types.InventoryItemTypeName,
    -- | One or more filters. Use a filter to return a more specific list of results.
    filters :: Core.Maybe (Core.NonEmpty Types.InventoryFilter),
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInventoryEntries' value with any optional fields omitted.
mkListInventoryEntries ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'typeName'
  Types.InventoryItemTypeName ->
  ListInventoryEntries
mkListInventoryEntries instanceId typeName =
  ListInventoryEntries'
    { instanceId,
      typeName,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The instance ID for which you want inventory information.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lieInstanceId :: Lens.Lens' ListInventoryEntries Types.InstanceId
lieInstanceId = Lens.field @"instanceId"
{-# DEPRECATED lieInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The type of inventory item for which you want information.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lieTypeName :: Lens.Lens' ListInventoryEntries Types.InventoryItemTypeName
lieTypeName = Lens.field @"typeName"
{-# DEPRECATED lieTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lieFilters :: Lens.Lens' ListInventoryEntries (Core.Maybe (Core.NonEmpty Types.InventoryFilter))
lieFilters = Lens.field @"filters"
{-# DEPRECATED lieFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lieMaxResults :: Lens.Lens' ListInventoryEntries (Core.Maybe Core.Natural)
lieMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lieMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lieNextToken :: Lens.Lens' ListInventoryEntries (Core.Maybe Types.NextToken)
lieNextToken = Lens.field @"nextToken"
{-# DEPRECATED lieNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListInventoryEntries where
  toJSON ListInventoryEntries {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("TypeName" Core..= typeName),
            ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListInventoryEntries where
  type Rs ListInventoryEntries = ListInventoryEntriesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.ListInventoryEntries")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInventoryEntriesResponse'
            Core.<$> (x Core..:? "CaptureTime")
            Core.<*> (x Core..:? "Entries")
            Core.<*> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "SchemaVersion")
            Core.<*> (x Core..:? "TypeName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListInventoryEntriesResponse' smart constructor.
data ListInventoryEntriesResponse = ListInventoryEntriesResponse'
  { -- | The time that inventory information was collected for the instance(s).
    captureTime :: Core.Maybe Types.InventoryItemCaptureTime,
    -- | A list of inventory items on the instance(s).
    entries :: Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue],
    -- | The instance ID targeted by the request to query inventory information.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The inventory schema version used by the instance(s).
    schemaVersion :: Core.Maybe Types.InventoryItemSchemaVersion,
    -- | The type of inventory item returned by the request.
    typeName :: Core.Maybe Types.InventoryItemTypeName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInventoryEntriesResponse' value with any optional fields omitted.
mkListInventoryEntriesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListInventoryEntriesResponse
mkListInventoryEntriesResponse responseStatus =
  ListInventoryEntriesResponse'
    { captureTime = Core.Nothing,
      entries = Core.Nothing,
      instanceId = Core.Nothing,
      nextToken = Core.Nothing,
      schemaVersion = Core.Nothing,
      typeName = Core.Nothing,
      responseStatus
    }

-- | The time that inventory information was collected for the instance(s).
--
-- /Note:/ Consider using 'captureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lierrsCaptureTime :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe Types.InventoryItemCaptureTime)
lierrsCaptureTime = Lens.field @"captureTime"
{-# DEPRECATED lierrsCaptureTime "Use generic-lens or generic-optics with 'captureTime' instead." #-}

-- | A list of inventory items on the instance(s).
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lierrsEntries :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue])
lierrsEntries = Lens.field @"entries"
{-# DEPRECATED lierrsEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The instance ID targeted by the request to query inventory information.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lierrsInstanceId :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe Types.InstanceId)
lierrsInstanceId = Lens.field @"instanceId"
{-# DEPRECATED lierrsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lierrsNextToken :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe Types.NextToken)
lierrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lierrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The inventory schema version used by the instance(s).
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lierrsSchemaVersion :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe Types.InventoryItemSchemaVersion)
lierrsSchemaVersion = Lens.field @"schemaVersion"
{-# DEPRECATED lierrsSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The type of inventory item returned by the request.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lierrsTypeName :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe Types.InventoryItemTypeName)
lierrsTypeName = Lens.field @"typeName"
{-# DEPRECATED lierrsTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lierrsResponseStatus :: Lens.Lens' ListInventoryEntriesResponse Core.Int
lierrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lierrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
