{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteColumnStatisticsForTable (..),
    mkDeleteColumnStatisticsForTable,

    -- ** Request lenses
    dcsftDatabaseName,
    dcsftTableName,
    dcsftColumnName,
    dcsftCatalogId,

    -- * Destructuring the response
    DeleteColumnStatisticsForTableResponse (..),
    mkDeleteColumnStatisticsForTableResponse,

    -- ** Response lenses
    dcsftrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteColumnStatisticsForTable' smart constructor.
data DeleteColumnStatisticsForTable = DeleteColumnStatisticsForTable'
  { -- | The name of the catalog database where the partitions reside.
    databaseName :: Types.NameString,
    -- | The name of the partitions' table.
    tableName :: Types.NameString,
    -- | The name of the column.
    columnName :: Types.NameString,
    -- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogIdString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteColumnStatisticsForTable' value with any optional fields omitted.
mkDeleteColumnStatisticsForTable ::
  -- | 'databaseName'
  Types.NameString ->
  -- | 'tableName'
  Types.NameString ->
  -- | 'columnName'
  Types.NameString ->
  DeleteColumnStatisticsForTable
mkDeleteColumnStatisticsForTable databaseName tableName columnName =
  DeleteColumnStatisticsForTable'
    { databaseName,
      tableName,
      columnName,
      catalogId = Core.Nothing
    }

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftDatabaseName :: Lens.Lens' DeleteColumnStatisticsForTable Types.NameString
dcsftDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED dcsftDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftTableName :: Lens.Lens' DeleteColumnStatisticsForTable Types.NameString
dcsftTableName = Lens.field @"tableName"
{-# DEPRECATED dcsftTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The name of the column.
--
-- /Note:/ Consider using 'columnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftColumnName :: Lens.Lens' DeleteColumnStatisticsForTable Types.NameString
dcsftColumnName = Lens.field @"columnName"
{-# DEPRECATED dcsftColumnName "Use generic-lens or generic-optics with 'columnName' instead." #-}

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftCatalogId :: Lens.Lens' DeleteColumnStatisticsForTable (Core.Maybe Types.CatalogIdString)
dcsftCatalogId = Lens.field @"catalogId"
{-# DEPRECATED dcsftCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON DeleteColumnStatisticsForTable where
  toJSON DeleteColumnStatisticsForTable {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("ColumnName" Core..= columnName),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest DeleteColumnStatisticsForTable where
  type
    Rs DeleteColumnStatisticsForTable =
      DeleteColumnStatisticsForTableResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSGlue.DeleteColumnStatisticsForTable")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteColumnStatisticsForTableResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteColumnStatisticsForTableResponse' smart constructor.
newtype DeleteColumnStatisticsForTableResponse = DeleteColumnStatisticsForTableResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteColumnStatisticsForTableResponse' value with any optional fields omitted.
mkDeleteColumnStatisticsForTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteColumnStatisticsForTableResponse
mkDeleteColumnStatisticsForTableResponse responseStatus =
  DeleteColumnStatisticsForTableResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsftrrsResponseStatus :: Lens.Lens' DeleteColumnStatisticsForTableResponse Core.Int
dcsftrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcsftrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
