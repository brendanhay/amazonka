{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetColumnStatisticsForPartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves partition statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this operation is @GetPartition@ .
module Network.AWS.Glue.GetColumnStatisticsForPartition
  ( -- * Creating a request
    GetColumnStatisticsForPartition (..),
    mkGetColumnStatisticsForPartition,

    -- ** Request lenses
    gcsfpDatabaseName,
    gcsfpTableName,
    gcsfpPartitionValues,
    gcsfpColumnNames,
    gcsfpCatalogId,

    -- * Destructuring the response
    GetColumnStatisticsForPartitionResponse (..),
    mkGetColumnStatisticsForPartitionResponse,

    -- ** Response lenses
    gcsfprrsColumnStatisticsList,
    gcsfprrsErrors,
    gcsfprrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetColumnStatisticsForPartition' smart constructor.
data GetColumnStatisticsForPartition = GetColumnStatisticsForPartition'
  { -- | The name of the catalog database where the partitions reside.
    databaseName :: Types.DatabaseName,
    -- | The name of the partitions' table.
    tableName :: Types.TableName,
    -- | A list of partition values identifying the partition.
    partitionValues :: [Types.ValueString],
    -- | A list of the column names.
    columnNames :: [Types.NameString],
    -- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetColumnStatisticsForPartition' value with any optional fields omitted.
mkGetColumnStatisticsForPartition ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'tableName'
  Types.TableName ->
  GetColumnStatisticsForPartition
mkGetColumnStatisticsForPartition databaseName tableName =
  GetColumnStatisticsForPartition'
    { databaseName,
      tableName,
      partitionValues = Core.mempty,
      columnNames = Core.mempty,
      catalogId = Core.Nothing
    }

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfpDatabaseName :: Lens.Lens' GetColumnStatisticsForPartition Types.DatabaseName
gcsfpDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED gcsfpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfpTableName :: Lens.Lens' GetColumnStatisticsForPartition Types.TableName
gcsfpTableName = Lens.field @"tableName"
{-# DEPRECATED gcsfpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of partition values identifying the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfpPartitionValues :: Lens.Lens' GetColumnStatisticsForPartition [Types.ValueString]
gcsfpPartitionValues = Lens.field @"partitionValues"
{-# DEPRECATED gcsfpPartitionValues "Use generic-lens or generic-optics with 'partitionValues' instead." #-}

-- | A list of the column names.
--
-- /Note:/ Consider using 'columnNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfpColumnNames :: Lens.Lens' GetColumnStatisticsForPartition [Types.NameString]
gcsfpColumnNames = Lens.field @"columnNames"
{-# DEPRECATED gcsfpColumnNames "Use generic-lens or generic-optics with 'columnNames' instead." #-}

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfpCatalogId :: Lens.Lens' GetColumnStatisticsForPartition (Core.Maybe Types.CatalogId)
gcsfpCatalogId = Lens.field @"catalogId"
{-# DEPRECATED gcsfpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON GetColumnStatisticsForPartition where
  toJSON GetColumnStatisticsForPartition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("PartitionValues" Core..= partitionValues),
            Core.Just ("ColumnNames" Core..= columnNames),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest GetColumnStatisticsForPartition where
  type
    Rs GetColumnStatisticsForPartition =
      GetColumnStatisticsForPartitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSGlue.GetColumnStatisticsForPartition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetColumnStatisticsForPartitionResponse'
            Core.<$> (x Core..:? "ColumnStatisticsList")
            Core.<*> (x Core..:? "Errors")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetColumnStatisticsForPartitionResponse' smart constructor.
data GetColumnStatisticsForPartitionResponse = GetColumnStatisticsForPartitionResponse'
  { -- | List of ColumnStatistics that failed to be retrieved.
    columnStatisticsList :: Core.Maybe [Types.ColumnStatistics],
    -- | Error occurred during retrieving column statistics data.
    errors :: Core.Maybe [Types.ColumnError],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetColumnStatisticsForPartitionResponse' value with any optional fields omitted.
mkGetColumnStatisticsForPartitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetColumnStatisticsForPartitionResponse
mkGetColumnStatisticsForPartitionResponse responseStatus =
  GetColumnStatisticsForPartitionResponse'
    { columnStatisticsList =
        Core.Nothing,
      errors = Core.Nothing,
      responseStatus
    }

-- | List of ColumnStatistics that failed to be retrieved.
--
-- /Note:/ Consider using 'columnStatisticsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfprrsColumnStatisticsList :: Lens.Lens' GetColumnStatisticsForPartitionResponse (Core.Maybe [Types.ColumnStatistics])
gcsfprrsColumnStatisticsList = Lens.field @"columnStatisticsList"
{-# DEPRECATED gcsfprrsColumnStatisticsList "Use generic-lens or generic-optics with 'columnStatisticsList' instead." #-}

-- | Error occurred during retrieving column statistics data.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfprrsErrors :: Lens.Lens' GetColumnStatisticsForPartitionResponse (Core.Maybe [Types.ColumnError])
gcsfprrsErrors = Lens.field @"errors"
{-# DEPRECATED gcsfprrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsfprrsResponseStatus :: Lens.Lens' GetColumnStatisticsForPartitionResponse Core.Int
gcsfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcsfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
