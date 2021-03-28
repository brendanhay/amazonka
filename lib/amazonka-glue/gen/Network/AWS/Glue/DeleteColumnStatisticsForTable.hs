{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteColumnStatisticsForTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves table statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this operation is @DeleteTable@ .
module Network.AWS.Glue.DeleteColumnStatisticsForTable
    (
    -- * Creating a request
      DeleteColumnStatisticsForTable (..)
    , mkDeleteColumnStatisticsForTable
    -- ** Request lenses
    , dcsftDatabaseName
    , dcsftTableName
    , dcsftColumnName
    , dcsftCatalogId

    -- * Destructuring the response
    , DeleteColumnStatisticsForTableResponse (..)
    , mkDeleteColumnStatisticsForTableResponse
    -- ** Response lenses
    , dcsftrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteColumnStatisticsForTable' smart constructor.
data DeleteColumnStatisticsForTable = DeleteColumnStatisticsForTable'
  { databaseName :: Types.NameString
    -- ^ The name of the catalog database where the partitions reside.
  , tableName :: Types.NameString
    -- ^ The name of the partitions' table.
  , columnName :: Types.NameString
    -- ^ The name of the column.
  , catalogId :: Core.Maybe Types.CatalogIdString
    -- ^ The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteColumnStatisticsForTable' value with any optional fields omitted.
mkDeleteColumnStatisticsForTable
    :: Types.NameString -- ^ 'databaseName'
    -> Types.NameString -- ^ 'tableName'
    -> Types.NameString -- ^ 'columnName'
    -> DeleteColumnStatisticsForTable
mkDeleteColumnStatisticsForTable databaseName tableName columnName
  = DeleteColumnStatisticsForTable'{databaseName, tableName,
                                    columnName, catalogId = Core.Nothing}

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftDatabaseName :: Lens.Lens' DeleteColumnStatisticsForTable Types.NameString
dcsftDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE dcsftDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftTableName :: Lens.Lens' DeleteColumnStatisticsForTable Types.NameString
dcsftTableName = Lens.field @"tableName"
{-# INLINEABLE dcsftTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The name of the column.
--
-- /Note:/ Consider using 'columnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftColumnName :: Lens.Lens' DeleteColumnStatisticsForTable Types.NameString
dcsftColumnName = Lens.field @"columnName"
{-# INLINEABLE dcsftColumnName #-}
{-# DEPRECATED columnName "Use generic-lens or generic-optics with 'columnName' instead"  #-}

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftCatalogId :: Lens.Lens' DeleteColumnStatisticsForTable (Core.Maybe Types.CatalogIdString)
dcsftCatalogId = Lens.field @"catalogId"
{-# INLINEABLE dcsftCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery DeleteColumnStatisticsForTable where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteColumnStatisticsForTable where
        toHeaders DeleteColumnStatisticsForTable{..}
          = Core.pure
              ("X-Amz-Target", "AWSGlue.DeleteColumnStatisticsForTable")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteColumnStatisticsForTable where
        toJSON DeleteColumnStatisticsForTable{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  Core.Just ("ColumnName" Core..= columnName),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest DeleteColumnStatisticsForTable where
        type Rs DeleteColumnStatisticsForTable =
             DeleteColumnStatisticsForTableResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteColumnStatisticsForTableResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteColumnStatisticsForTableResponse' smart constructor.
newtype DeleteColumnStatisticsForTableResponse = DeleteColumnStatisticsForTableResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteColumnStatisticsForTableResponse' value with any optional fields omitted.
mkDeleteColumnStatisticsForTableResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteColumnStatisticsForTableResponse
mkDeleteColumnStatisticsForTableResponse responseStatus
  = DeleteColumnStatisticsForTableResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftrrsResponseStatus :: Lens.Lens' DeleteColumnStatisticsForTableResponse Core.Int
dcsftrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcsftrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
