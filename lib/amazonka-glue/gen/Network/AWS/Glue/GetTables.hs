{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definitions of some or all of the tables in a given @Database@ .
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTables
    (
    -- * Creating a request
      GetTables (..)
    , mkGetTables
    -- ** Request lenses
    , gtsDatabaseName
    , gtsCatalogId
    , gtsExpression
    , gtsMaxResults
    , gtsNextToken

    -- * Destructuring the response
    , GetTablesResponse (..)
    , mkGetTablesResponse
    -- ** Response lenses
    , gtrgrsNextToken
    , gtrgrsTableList
    , gtrgrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTables' smart constructor.
data GetTables = GetTables'
  { databaseName :: Types.DatabaseName
    -- ^ The database in the catalog whose tables to list. For Hive compatibility, this name is entirely lowercase.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
  , expression :: Core.Maybe Types.Expression
    -- ^ A regular expression pattern. If present, only those tables whose names match the pattern are returned.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of tables to return in a single response.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, included if this is a continuation call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTables' value with any optional fields omitted.
mkGetTables
    :: Types.DatabaseName -- ^ 'databaseName'
    -> GetTables
mkGetTables databaseName
  = GetTables'{databaseName, catalogId = Core.Nothing,
               expression = Core.Nothing, maxResults = Core.Nothing,
               nextToken = Core.Nothing}

-- | The database in the catalog whose tables to list. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsDatabaseName :: Lens.Lens' GetTables Types.DatabaseName
gtsDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gtsDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsCatalogId :: Lens.Lens' GetTables (Core.Maybe Types.CatalogId)
gtsCatalogId = Lens.field @"catalogId"
{-# INLINEABLE gtsCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | A regular expression pattern. If present, only those tables whose names match the pattern are returned.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsExpression :: Lens.Lens' GetTables (Core.Maybe Types.Expression)
gtsExpression = Lens.field @"expression"
{-# INLINEABLE gtsExpression #-}
{-# DEPRECATED expression "Use generic-lens or generic-optics with 'expression' instead"  #-}

-- | The maximum number of tables to return in a single response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsMaxResults :: Lens.Lens' GetTables (Core.Maybe Core.Natural)
gtsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gtsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, included if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsNextToken :: Lens.Lens' GetTables (Core.Maybe Types.NextToken)
gtsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetTables where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTables where
        toHeaders GetTables{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetTables") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTables where
        toJSON GetTables{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  ("CatalogId" Core..=) Core.<$> catalogId,
                  ("Expression" Core..=) Core.<$> expression,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetTables where
        type Rs GetTables = GetTablesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTablesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "TableList" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetTables where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"tableList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetTablesResponse' smart constructor.
data GetTablesResponse = GetTablesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, present if the current list segment is not the last.
  , tableList :: Core.Maybe [Types.Table]
    -- ^ A list of the requested @Table@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTablesResponse' value with any optional fields omitted.
mkGetTablesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTablesResponse
mkGetTablesResponse responseStatus
  = GetTablesResponse'{nextToken = Core.Nothing,
                       tableList = Core.Nothing, responseStatus}

-- | A continuation token, present if the current list segment is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrgrsNextToken :: Lens.Lens' GetTablesResponse (Core.Maybe Types.NextToken)
gtrgrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtrgrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of the requested @Table@ objects.
--
-- /Note:/ Consider using 'tableList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrgrsTableList :: Lens.Lens' GetTablesResponse (Core.Maybe [Types.Table])
gtrgrsTableList = Lens.field @"tableList"
{-# INLINEABLE gtrgrsTableList #-}
{-# DEPRECATED tableList "Use generic-lens or generic-optics with 'tableList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrgrsResponseStatus :: Lens.Lens' GetTablesResponse Core.Int
gtrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
