{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListGlobalTables (..)
    , mkListGlobalTables
    -- ** Request lenses
    , lgtExclusiveStartGlobalTableName
    , lgtLimit
    , lgtRegionName

    -- * Destructuring the response
    , ListGlobalTablesResponse (..)
    , mkListGlobalTablesResponse
    -- ** Response lenses
    , lgtrrsGlobalTables
    , lgtrrsLastEvaluatedGlobalTableName
    , lgtrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGlobalTables' smart constructor.
data ListGlobalTables = ListGlobalTables'
  { exclusiveStartGlobalTableName :: Core.Maybe Types.ExclusiveStartGlobalTableName
    -- ^ The first global table name that this operation will evaluate.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of table names to return, if the parameter is not specified DynamoDB defaults to 100.
--
-- If the number of global tables DynamoDB finds reaches this limit, it stops the operation and returns the table names collected up to that point, with a table name in the @LastEvaluatedGlobalTableName@ to apply in a subsequent operation to the @ExclusiveStartGlobalTableName@ parameter.
  , regionName :: Core.Maybe Types.RegionName
    -- ^ Lists the global tables in a specific Region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGlobalTables' value with any optional fields omitted.
mkListGlobalTables
    :: ListGlobalTables
mkListGlobalTables
  = ListGlobalTables'{exclusiveStartGlobalTableName = Core.Nothing,
                      limit = Core.Nothing, regionName = Core.Nothing}

-- | The first global table name that this operation will evaluate.
--
-- /Note:/ Consider using 'exclusiveStartGlobalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtExclusiveStartGlobalTableName :: Lens.Lens' ListGlobalTables (Core.Maybe Types.ExclusiveStartGlobalTableName)
lgtExclusiveStartGlobalTableName = Lens.field @"exclusiveStartGlobalTableName"
{-# INLINEABLE lgtExclusiveStartGlobalTableName #-}
{-# DEPRECATED exclusiveStartGlobalTableName "Use generic-lens or generic-optics with 'exclusiveStartGlobalTableName' instead"  #-}

-- | The maximum number of table names to return, if the parameter is not specified DynamoDB defaults to 100.
--
-- If the number of global tables DynamoDB finds reaches this limit, it stops the operation and returns the table names collected up to that point, with a table name in the @LastEvaluatedGlobalTableName@ to apply in a subsequent operation to the @ExclusiveStartGlobalTableName@ parameter.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtLimit :: Lens.Lens' ListGlobalTables (Core.Maybe Core.Natural)
lgtLimit = Lens.field @"limit"
{-# INLINEABLE lgtLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Lists the global tables in a specific Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtRegionName :: Lens.Lens' ListGlobalTables (Core.Maybe Types.RegionName)
lgtRegionName = Lens.field @"regionName"
{-# INLINEABLE lgtRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

instance Core.ToQuery ListGlobalTables where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListGlobalTables where
        toHeaders ListGlobalTables{..}
          = Core.pure ("X-Amz-Target", "DynamoDB_20120810.ListGlobalTables")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON ListGlobalTables where
        toJSON ListGlobalTables{..}
          = Core.object
              (Core.catMaybes
                 [("ExclusiveStartGlobalTableName" Core..=) Core.<$>
                    exclusiveStartGlobalTableName,
                  ("Limit" Core..=) Core.<$> limit,
                  ("RegionName" Core..=) Core.<$> regionName])

instance Core.AWSRequest ListGlobalTables where
        type Rs ListGlobalTables = ListGlobalTablesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListGlobalTablesResponse' Core.<$>
                   (x Core..:? "GlobalTables") Core.<*>
                     x Core..:? "LastEvaluatedGlobalTableName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListGlobalTablesResponse' smart constructor.
data ListGlobalTablesResponse = ListGlobalTablesResponse'
  { globalTables :: Core.Maybe [Types.GlobalTable]
    -- ^ List of global table names.
  , lastEvaluatedGlobalTableName :: Core.Maybe Types.LastEvaluatedGlobalTableName
    -- ^ Last evaluated global table name.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGlobalTablesResponse' value with any optional fields omitted.
mkListGlobalTablesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListGlobalTablesResponse
mkListGlobalTablesResponse responseStatus
  = ListGlobalTablesResponse'{globalTables = Core.Nothing,
                              lastEvaluatedGlobalTableName = Core.Nothing, responseStatus}

-- | List of global table names.
--
-- /Note:/ Consider using 'globalTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtrrsGlobalTables :: Lens.Lens' ListGlobalTablesResponse (Core.Maybe [Types.GlobalTable])
lgtrrsGlobalTables = Lens.field @"globalTables"
{-# INLINEABLE lgtrrsGlobalTables #-}
{-# DEPRECATED globalTables "Use generic-lens or generic-optics with 'globalTables' instead"  #-}

-- | Last evaluated global table name.
--
-- /Note:/ Consider using 'lastEvaluatedGlobalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtrrsLastEvaluatedGlobalTableName :: Lens.Lens' ListGlobalTablesResponse (Core.Maybe Types.LastEvaluatedGlobalTableName)
lgtrrsLastEvaluatedGlobalTableName = Lens.field @"lastEvaluatedGlobalTableName"
{-# INLINEABLE lgtrrsLastEvaluatedGlobalTableName #-}
{-# DEPRECATED lastEvaluatedGlobalTableName "Use generic-lens or generic-optics with 'lastEvaluatedGlobalTableName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgtrrsResponseStatus :: Lens.Lens' ListGlobalTablesResponse Core.Int
lgtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lgtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
