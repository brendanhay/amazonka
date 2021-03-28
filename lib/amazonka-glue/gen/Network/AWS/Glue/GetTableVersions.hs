{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTableVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of strings that identify available versions of a specified table.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTableVersions
    (
    -- * Creating a request
      GetTableVersions (..)
    , mkGetTableVersions
    -- ** Request lenses
    , gtvsDatabaseName
    , gtvsTableName
    , gtvsCatalogId
    , gtvsMaxResults
    , gtvsNextToken

    -- * Destructuring the response
    , GetTableVersionsResponse (..)
    , mkGetTableVersionsResponse
    -- ** Response lenses
    , gtvrfrsNextToken
    , gtvrfrsTableVersions
    , gtvrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTableVersions' smart constructor.
data GetTableVersions = GetTableVersions'
  { databaseName :: Types.NameString
    -- ^ The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
  , tableName :: Types.NameString
    -- ^ The name of the table. For Hive compatibility, this name is entirely lowercase.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of table versions to return in one response.
  , nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token, if this is not the first call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTableVersions' value with any optional fields omitted.
mkGetTableVersions
    :: Types.NameString -- ^ 'databaseName'
    -> Types.NameString -- ^ 'tableName'
    -> GetTableVersions
mkGetTableVersions databaseName tableName
  = GetTableVersions'{databaseName, tableName,
                      catalogId = Core.Nothing, maxResults = Core.Nothing,
                      nextToken = Core.Nothing}

-- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsDatabaseName :: Lens.Lens' GetTableVersions Types.NameString
gtvsDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gtvsDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the table. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsTableName :: Lens.Lens' GetTableVersions Types.NameString
gtvsTableName = Lens.field @"tableName"
{-# INLINEABLE gtvsTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsCatalogId :: Lens.Lens' GetTableVersions (Core.Maybe Types.CatalogId)
gtvsCatalogId = Lens.field @"catalogId"
{-# INLINEABLE gtvsCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | The maximum number of table versions to return in one response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsMaxResults :: Lens.Lens' GetTableVersions (Core.Maybe Core.Natural)
gtvsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gtvsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is not the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvsNextToken :: Lens.Lens' GetTableVersions (Core.Maybe Types.Token)
gtvsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtvsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetTableVersions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTableVersions where
        toHeaders GetTableVersions{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetTableVersions") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTableVersions where
        toJSON GetTableVersions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  ("CatalogId" Core..=) Core.<$> catalogId,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetTableVersions where
        type Rs GetTableVersions = GetTableVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTableVersionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "TableVersions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetTableVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"tableVersions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetTableVersionsResponse' smart constructor.
data GetTableVersionsResponse = GetTableVersionsResponse'
  { nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token, if the list of available versions does not include the last one.
  , tableVersions :: Core.Maybe [Types.TableVersion]
    -- ^ A list of strings identifying available versions of the specified table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTableVersionsResponse' value with any optional fields omitted.
mkGetTableVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTableVersionsResponse
mkGetTableVersionsResponse responseStatus
  = GetTableVersionsResponse'{nextToken = Core.Nothing,
                              tableVersions = Core.Nothing, responseStatus}

-- | A continuation token, if the list of available versions does not include the last one.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrfrsNextToken :: Lens.Lens' GetTableVersionsResponse (Core.Maybe Types.Token)
gtvrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtvrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of strings identifying available versions of the specified table.
--
-- /Note:/ Consider using 'tableVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrfrsTableVersions :: Lens.Lens' GetTableVersionsResponse (Core.Maybe [Types.TableVersion])
gtvrfrsTableVersions = Lens.field @"tableVersions"
{-# INLINEABLE gtvrfrsTableVersions #-}
{-# DEPRECATED tableVersions "Use generic-lens or generic-optics with 'tableVersions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrfrsResponseStatus :: Lens.Lens' GetTableVersionsResponse Core.Int
gtvrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtvrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
