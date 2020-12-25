{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteColumnStatisticsForPartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the partition column statistics of a column.
--
-- The Identity and Access Management (IAM) permission required for this operation is @DeletePartition@ .
module Network.AWS.Glue.DeleteColumnStatisticsForPartition
  ( -- * Creating a request
    DeleteColumnStatisticsForPartition (..),
    mkDeleteColumnStatisticsForPartition,

    -- ** Request lenses
    dcsfpDatabaseName,
    dcsfpTableName,
    dcsfpPartitionValues,
    dcsfpColumnName,
    dcsfpCatalogId,

    -- * Destructuring the response
    DeleteColumnStatisticsForPartitionResponse (..),
    mkDeleteColumnStatisticsForPartitionResponse,

    -- ** Response lenses
    dcsfprrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteColumnStatisticsForPartition' smart constructor.
data DeleteColumnStatisticsForPartition = DeleteColumnStatisticsForPartition'
  { -- | The name of the catalog database where the partitions reside.
    databaseName :: Types.NameString,
    -- | The name of the partitions' table.
    tableName :: Types.NameString,
    -- | A list of partition values identifying the partition.
    partitionValues :: [Types.ValueString],
    -- | Name of the column.
    columnName :: Types.NameString,
    -- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteColumnStatisticsForPartition' value with any optional fields omitted.
mkDeleteColumnStatisticsForPartition ::
  -- | 'databaseName'
  Types.NameString ->
  -- | 'tableName'
  Types.NameString ->
  -- | 'columnName'
  Types.NameString ->
  DeleteColumnStatisticsForPartition
mkDeleteColumnStatisticsForPartition
  databaseName
  tableName
  columnName =
    DeleteColumnStatisticsForPartition'
      { databaseName,
        tableName,
        partitionValues = Core.mempty,
        columnName,
        catalogId = Core.Nothing
      }

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfpDatabaseName :: Lens.Lens' DeleteColumnStatisticsForPartition Types.NameString
dcsfpDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED dcsfpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfpTableName :: Lens.Lens' DeleteColumnStatisticsForPartition Types.NameString
dcsfpTableName = Lens.field @"tableName"
{-# DEPRECATED dcsfpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of partition values identifying the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfpPartitionValues :: Lens.Lens' DeleteColumnStatisticsForPartition [Types.ValueString]
dcsfpPartitionValues = Lens.field @"partitionValues"
{-# DEPRECATED dcsfpPartitionValues "Use generic-lens or generic-optics with 'partitionValues' instead." #-}

-- | Name of the column.
--
-- /Note:/ Consider using 'columnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfpColumnName :: Lens.Lens' DeleteColumnStatisticsForPartition Types.NameString
dcsfpColumnName = Lens.field @"columnName"
{-# DEPRECATED dcsfpColumnName "Use generic-lens or generic-optics with 'columnName' instead." #-}

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfpCatalogId :: Lens.Lens' DeleteColumnStatisticsForPartition (Core.Maybe Types.CatalogId)
dcsfpCatalogId = Lens.field @"catalogId"
{-# DEPRECATED dcsfpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON DeleteColumnStatisticsForPartition where
  toJSON DeleteColumnStatisticsForPartition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("PartitionValues" Core..= partitionValues),
            Core.Just ("ColumnName" Core..= columnName),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest DeleteColumnStatisticsForPartition where
  type
    Rs DeleteColumnStatisticsForPartition =
      DeleteColumnStatisticsForPartitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSGlue.DeleteColumnStatisticsForPartition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteColumnStatisticsForPartitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteColumnStatisticsForPartitionResponse' smart constructor.
newtype DeleteColumnStatisticsForPartitionResponse = DeleteColumnStatisticsForPartitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteColumnStatisticsForPartitionResponse' value with any optional fields omitted.
mkDeleteColumnStatisticsForPartitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteColumnStatisticsForPartitionResponse
mkDeleteColumnStatisticsForPartitionResponse responseStatus =
  DeleteColumnStatisticsForPartitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsfprrsResponseStatus :: Lens.Lens' DeleteColumnStatisticsForPartitionResponse Core.Int
dcsfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcsfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
