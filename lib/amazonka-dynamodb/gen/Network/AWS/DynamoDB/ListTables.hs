{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ListTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of table names associated with the current account and endpoint. The output from @ListTables@ is paginated, with each page returning a maximum of 100 table names.
--
-- This operation returns paginated results.
module Network.AWS.DynamoDB.ListTables
    (
    -- * Creating a request
      ListTables (..)
    , mkListTables
    -- ** Request lenses
    , ltExclusiveStartTableName
    , ltLimit

    -- * Destructuring the response
    , ListTablesResponse (..)
    , mkListTablesResponse
    -- ** Response lenses
    , ltrrsLastEvaluatedTableName
    , ltrrsTableNames
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListTables@ operation.
--
-- /See:/ 'mkListTables' smart constructor.
data ListTables = ListTables'
  { exclusiveStartTableName :: Core.Maybe Types.TableName
    -- ^ The first table name that this operation will evaluate. Use the value that was returned for @LastEvaluatedTableName@ in a previous operation, so that you can obtain the next page of results.
  , limit :: Core.Maybe Core.Natural
    -- ^ A maximum number of table names to return. If this parameter is not specified, the limit is 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTables' value with any optional fields omitted.
mkListTables
    :: ListTables
mkListTables
  = ListTables'{exclusiveStartTableName = Core.Nothing,
                limit = Core.Nothing}

-- | The first table name that this operation will evaluate. Use the value that was returned for @LastEvaluatedTableName@ in a previous operation, so that you can obtain the next page of results.
--
-- /Note:/ Consider using 'exclusiveStartTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltExclusiveStartTableName :: Lens.Lens' ListTables (Core.Maybe Types.TableName)
ltExclusiveStartTableName = Lens.field @"exclusiveStartTableName"
{-# INLINEABLE ltExclusiveStartTableName #-}
{-# DEPRECATED exclusiveStartTableName "Use generic-lens or generic-optics with 'exclusiveStartTableName' instead"  #-}

-- | A maximum number of table names to return. If this parameter is not specified, the limit is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLimit :: Lens.Lens' ListTables (Core.Maybe Core.Natural)
ltLimit = Lens.field @"limit"
{-# INLINEABLE ltLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

instance Core.ToQuery ListTables where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTables where
        toHeaders ListTables{..}
          = Core.pure ("X-Amz-Target", "DynamoDB_20120810.ListTables")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON ListTables where
        toJSON ListTables{..}
          = Core.object
              (Core.catMaybes
                 [("ExclusiveStartTableName" Core..=) Core.<$>
                    exclusiveStartTableName,
                  ("Limit" Core..=) Core.<$> limit])

instance Core.AWSRequest ListTables where
        type Rs ListTables = ListTablesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTablesResponse' Core.<$>
                   (x Core..:? "LastEvaluatedTableName") Core.<*>
                     x Core..:? "TableNames"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTables where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"lastEvaluatedTableName") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"tableNames" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"exclusiveStartTableName" Lens..~
                   rs Lens.^. Lens.field @"lastEvaluatedTableName")

-- | Represents the output of a @ListTables@ operation.
--
-- /See:/ 'mkListTablesResponse' smart constructor.
data ListTablesResponse = ListTablesResponse'
  { lastEvaluatedTableName :: Core.Maybe Types.LastEvaluatedTableName
    -- ^ The name of the last table in the current page of results. Use this value as the @ExclusiveStartTableName@ in a new request to obtain the next page of results, until all the table names are returned.
--
-- If you do not receive a @LastEvaluatedTableName@ value in the response, this means that there are no more table names to be retrieved.
  , tableNames :: Core.Maybe [Types.TableName]
    -- ^ The names of the tables associated with the current account at the current endpoint. The maximum size of this array is 100.
--
-- If @LastEvaluatedTableName@ also appears in the output, you can use this value as the @ExclusiveStartTableName@ parameter in a subsequent @ListTables@ request and obtain the next page of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTablesResponse' value with any optional fields omitted.
mkListTablesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTablesResponse
mkListTablesResponse responseStatus
  = ListTablesResponse'{lastEvaluatedTableName = Core.Nothing,
                        tableNames = Core.Nothing, responseStatus}

-- | The name of the last table in the current page of results. Use this value as the @ExclusiveStartTableName@ in a new request to obtain the next page of results, until all the table names are returned.
--
-- If you do not receive a @LastEvaluatedTableName@ value in the response, this means that there are no more table names to be retrieved.
--
-- /Note:/ Consider using 'lastEvaluatedTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsLastEvaluatedTableName :: Lens.Lens' ListTablesResponse (Core.Maybe Types.LastEvaluatedTableName)
ltrrsLastEvaluatedTableName = Lens.field @"lastEvaluatedTableName"
{-# INLINEABLE ltrrsLastEvaluatedTableName #-}
{-# DEPRECATED lastEvaluatedTableName "Use generic-lens or generic-optics with 'lastEvaluatedTableName' instead"  #-}

-- | The names of the tables associated with the current account at the current endpoint. The maximum size of this array is 100.
--
-- If @LastEvaluatedTableName@ also appears in the output, you can use this value as the @ExclusiveStartTableName@ parameter in a subsequent @ListTables@ request and obtain the next page of results.
--
-- /Note:/ Consider using 'tableNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTableNames :: Lens.Lens' ListTablesResponse (Core.Maybe [Types.TableName])
ltrrsTableNames = Lens.field @"tableNames"
{-# INLINEABLE ltrrsTableNames #-}
{-# DEPRECATED tableNames "Use generic-lens or generic-optics with 'tableNames' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTablesResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
