{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetColumnStatisticsForTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves table statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this operation is @GetTable@ .
module Network.AWS.Glue.GetColumnStatisticsForTable
    (
    -- * Creating a request
      GetColumnStatisticsForTable (..)
    , mkGetColumnStatisticsForTable
    -- ** Request lenses
    , gcsftDatabaseName
    , gcsftTableName
    , gcsftColumnNames
    , gcsftCatalogId

    -- * Destructuring the response
    , GetColumnStatisticsForTableResponse (..)
    , mkGetColumnStatisticsForTableResponse
    -- ** Response lenses
    , gcsftrrsColumnStatisticsList
    , gcsftrrsErrors
    , gcsftrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetColumnStatisticsForTable' smart constructor.
data GetColumnStatisticsForTable = GetColumnStatisticsForTable'
  { databaseName :: Types.NameString
    -- ^ The name of the catalog database where the partitions reside.
  , tableName :: Types.NameString
    -- ^ The name of the partitions' table.
  , columnNames :: [Types.NameString]
    -- ^ A list of the column names.
  , catalogId :: Core.Maybe Types.CatalogIdString
    -- ^ The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetColumnStatisticsForTable' value with any optional fields omitted.
mkGetColumnStatisticsForTable
    :: Types.NameString -- ^ 'databaseName'
    -> Types.NameString -- ^ 'tableName'
    -> GetColumnStatisticsForTable
mkGetColumnStatisticsForTable databaseName tableName
  = GetColumnStatisticsForTable'{databaseName, tableName,
                                 columnNames = Core.mempty, catalogId = Core.Nothing}

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftDatabaseName :: Lens.Lens' GetColumnStatisticsForTable Types.NameString
gcsftDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gcsftDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftTableName :: Lens.Lens' GetColumnStatisticsForTable Types.NameString
gcsftTableName = Lens.field @"tableName"
{-# INLINEABLE gcsftTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | A list of the column names.
--
-- /Note:/ Consider using 'columnNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftColumnNames :: Lens.Lens' GetColumnStatisticsForTable [Types.NameString]
gcsftColumnNames = Lens.field @"columnNames"
{-# INLINEABLE gcsftColumnNames #-}
{-# DEPRECATED columnNames "Use generic-lens or generic-optics with 'columnNames' instead"  #-}

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftCatalogId :: Lens.Lens' GetColumnStatisticsForTable (Core.Maybe Types.CatalogIdString)
gcsftCatalogId = Lens.field @"catalogId"
{-# INLINEABLE gcsftCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery GetColumnStatisticsForTable where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetColumnStatisticsForTable where
        toHeaders GetColumnStatisticsForTable{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetColumnStatisticsForTable")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetColumnStatisticsForTable where
        toJSON GetColumnStatisticsForTable{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  Core.Just ("ColumnNames" Core..= columnNames),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest GetColumnStatisticsForTable where
        type Rs GetColumnStatisticsForTable =
             GetColumnStatisticsForTableResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetColumnStatisticsForTableResponse' Core.<$>
                   (x Core..:? "ColumnStatisticsList") Core.<*> x Core..:? "Errors"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetColumnStatisticsForTableResponse' smart constructor.
data GetColumnStatisticsForTableResponse = GetColumnStatisticsForTableResponse'
  { columnStatisticsList :: Core.Maybe [Types.ColumnStatistics]
    -- ^ List of ColumnStatistics that failed to be retrieved.
  , errors :: Core.Maybe [Types.ColumnError]
    -- ^ List of ColumnStatistics that failed to be retrieved.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetColumnStatisticsForTableResponse' value with any optional fields omitted.
mkGetColumnStatisticsForTableResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetColumnStatisticsForTableResponse
mkGetColumnStatisticsForTableResponse responseStatus
  = GetColumnStatisticsForTableResponse'{columnStatisticsList =
                                           Core.Nothing,
                                         errors = Core.Nothing, responseStatus}

-- | List of ColumnStatistics that failed to be retrieved.
--
-- /Note:/ Consider using 'columnStatisticsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftrrsColumnStatisticsList :: Lens.Lens' GetColumnStatisticsForTableResponse (Core.Maybe [Types.ColumnStatistics])
gcsftrrsColumnStatisticsList = Lens.field @"columnStatisticsList"
{-# INLINEABLE gcsftrrsColumnStatisticsList #-}
{-# DEPRECATED columnStatisticsList "Use generic-lens or generic-optics with 'columnStatisticsList' instead"  #-}

-- | List of ColumnStatistics that failed to be retrieved.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftrrsErrors :: Lens.Lens' GetColumnStatisticsForTableResponse (Core.Maybe [Types.ColumnError])
gcsftrrsErrors = Lens.field @"errors"
{-# INLINEABLE gcsftrrsErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsftrrsResponseStatus :: Lens.Lens' GetColumnStatisticsForTableResponse Core.Int
gcsftrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcsftrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
