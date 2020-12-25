{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ListGlobalTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all global tables that have a replica in the specified Region.
module Network.AWS.DynamoDB.ListGlobalTables
  ( -- * Creating a request
    ListGlobalTables (..),
    mkListGlobalTables,

    -- ** Request lenses
    lgtExclusiveStartGlobalTableName,
    lgtLimit,
    lgtRegionName,

    -- * Destructuring the response
    ListGlobalTablesResponse (..),
    mkListGlobalTablesResponse,

    -- ** Response lenses
    lgtrrsGlobalTables,
    lgtrrsLastEvaluatedGlobalTableName,
    lgtrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGlobalTables' smart constructor.
data ListGlobalTables = ListGlobalTables'
  { -- | The first global table name that this operation will evaluate.
    exclusiveStartGlobalTableName :: Core.Maybe Types.ExclusiveStartGlobalTableName,
    -- | The maximum number of table names to return, if the parameter is not specified DynamoDB defaults to 100.
    --
    -- If the number of global tables DynamoDB finds reaches this limit, it stops the operation and returns the table names collected up to that point, with a table name in the @LastEvaluatedGlobalTableName@ to apply in a subsequent operation to the @ExclusiveStartGlobalTableName@ parameter.
    limit :: Core.Maybe Core.Natural,
    -- | Lists the global tables in a specific Region.
    regionName :: Core.Maybe Types.RegionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGlobalTables' value with any optional fields omitted.
mkListGlobalTables ::
  ListGlobalTables
mkListGlobalTables =
  ListGlobalTables'
    { exclusiveStartGlobalTableName = Core.Nothing,
      limit = Core.Nothing,
      regionName = Core.Nothing
    }

-- | The first global table name that this operation will evaluate.
--
-- /Note:/ Consider using 'exclusiveStartGlobalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtExclusiveStartGlobalTableName :: Lens.Lens' ListGlobalTables (Core.Maybe Types.ExclusiveStartGlobalTableName)
lgtExclusiveStartGlobalTableName = Lens.field @"exclusiveStartGlobalTableName"
{-# DEPRECATED lgtExclusiveStartGlobalTableName "Use generic-lens or generic-optics with 'exclusiveStartGlobalTableName' instead." #-}

-- | The maximum number of table names to return, if the parameter is not specified DynamoDB defaults to 100.
--
-- If the number of global tables DynamoDB finds reaches this limit, it stops the operation and returns the table names collected up to that point, with a table name in the @LastEvaluatedGlobalTableName@ to apply in a subsequent operation to the @ExclusiveStartGlobalTableName@ parameter.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtLimit :: Lens.Lens' ListGlobalTables (Core.Maybe Core.Natural)
lgtLimit = Lens.field @"limit"
{-# DEPRECATED lgtLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Lists the global tables in a specific Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtRegionName :: Lens.Lens' ListGlobalTables (Core.Maybe Types.RegionName)
lgtRegionName = Lens.field @"regionName"
{-# DEPRECATED lgtRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Core.FromJSON ListGlobalTables where
  toJSON ListGlobalTables {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExclusiveStartGlobalTableName" Core..=)
              Core.<$> exclusiveStartGlobalTableName,
            ("Limit" Core..=) Core.<$> limit,
            ("RegionName" Core..=) Core.<$> regionName
          ]
      )

instance Core.AWSRequest ListGlobalTables where
  type Rs ListGlobalTables = ListGlobalTablesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.ListGlobalTables")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGlobalTablesResponse'
            Core.<$> (x Core..:? "GlobalTables")
            Core.<*> (x Core..:? "LastEvaluatedGlobalTableName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListGlobalTablesResponse' smart constructor.
data ListGlobalTablesResponse = ListGlobalTablesResponse'
  { -- | List of global table names.
    globalTables :: Core.Maybe [Types.GlobalTable],
    -- | Last evaluated global table name.
    lastEvaluatedGlobalTableName :: Core.Maybe Types.LastEvaluatedGlobalTableName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGlobalTablesResponse' value with any optional fields omitted.
mkListGlobalTablesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListGlobalTablesResponse
mkListGlobalTablesResponse responseStatus =
  ListGlobalTablesResponse'
    { globalTables = Core.Nothing,
      lastEvaluatedGlobalTableName = Core.Nothing,
      responseStatus
    }

-- | List of global table names.
--
-- /Note:/ Consider using 'globalTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtrrsGlobalTables :: Lens.Lens' ListGlobalTablesResponse (Core.Maybe [Types.GlobalTable])
lgtrrsGlobalTables = Lens.field @"globalTables"
{-# DEPRECATED lgtrrsGlobalTables "Use generic-lens or generic-optics with 'globalTables' instead." #-}

-- | Last evaluated global table name.
--
-- /Note:/ Consider using 'lastEvaluatedGlobalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtrrsLastEvaluatedGlobalTableName :: Lens.Lens' ListGlobalTablesResponse (Core.Maybe Types.LastEvaluatedGlobalTableName)
lgtrrsLastEvaluatedGlobalTableName = Lens.field @"lastEvaluatedGlobalTableName"
{-# DEPRECATED lgtrrsLastEvaluatedGlobalTableName "Use generic-lens or generic-optics with 'lastEvaluatedGlobalTableName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtrrsResponseStatus :: Lens.Lens' ListGlobalTablesResponse Core.Int
lgtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lgtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
