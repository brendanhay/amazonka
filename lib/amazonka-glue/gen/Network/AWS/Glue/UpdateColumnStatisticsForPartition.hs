{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateColumnStatisticsForPartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates partition statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this operation is @UpdatePartition@ .
module Network.AWS.Glue.UpdateColumnStatisticsForPartition
  ( -- * Creating a request
    UpdateColumnStatisticsForPartition (..),
    mkUpdateColumnStatisticsForPartition,

    -- ** Request lenses
    ucsfpDatabaseName,
    ucsfpTableName,
    ucsfpPartitionValues,
    ucsfpColumnStatisticsList,
    ucsfpCatalogId,

    -- * Destructuring the response
    UpdateColumnStatisticsForPartitionResponse (..),
    mkUpdateColumnStatisticsForPartitionResponse,

    -- ** Response lenses
    ucsfprrsErrors,
    ucsfprrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateColumnStatisticsForPartition' smart constructor.
data UpdateColumnStatisticsForPartition = UpdateColumnStatisticsForPartition'
  { -- | The name of the catalog database where the partitions reside.
    databaseName :: Types.NameString,
    -- | The name of the partitions' table.
    tableName :: Types.NameString,
    -- | A list of partition values identifying the partition.
    partitionValues :: [Types.ValueString],
    -- | A list of the column statistics.
    columnStatisticsList :: [Types.ColumnStatistics],
    -- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateColumnStatisticsForPartition' value with any optional fields omitted.
mkUpdateColumnStatisticsForPartition ::
  -- | 'databaseName'
  Types.NameString ->
  -- | 'tableName'
  Types.NameString ->
  UpdateColumnStatisticsForPartition
mkUpdateColumnStatisticsForPartition databaseName tableName =
  UpdateColumnStatisticsForPartition'
    { databaseName,
      tableName,
      partitionValues = Core.mempty,
      columnStatisticsList = Core.mempty,
      catalogId = Core.Nothing
    }

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfpDatabaseName :: Lens.Lens' UpdateColumnStatisticsForPartition Types.NameString
ucsfpDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED ucsfpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfpTableName :: Lens.Lens' UpdateColumnStatisticsForPartition Types.NameString
ucsfpTableName = Lens.field @"tableName"
{-# DEPRECATED ucsfpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of partition values identifying the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfpPartitionValues :: Lens.Lens' UpdateColumnStatisticsForPartition [Types.ValueString]
ucsfpPartitionValues = Lens.field @"partitionValues"
{-# DEPRECATED ucsfpPartitionValues "Use generic-lens or generic-optics with 'partitionValues' instead." #-}

-- | A list of the column statistics.
--
-- /Note:/ Consider using 'columnStatisticsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfpColumnStatisticsList :: Lens.Lens' UpdateColumnStatisticsForPartition [Types.ColumnStatistics]
ucsfpColumnStatisticsList = Lens.field @"columnStatisticsList"
{-# DEPRECATED ucsfpColumnStatisticsList "Use generic-lens or generic-optics with 'columnStatisticsList' instead." #-}

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfpCatalogId :: Lens.Lens' UpdateColumnStatisticsForPartition (Core.Maybe Types.CatalogId)
ucsfpCatalogId = Lens.field @"catalogId"
{-# DEPRECATED ucsfpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON UpdateColumnStatisticsForPartition where
  toJSON UpdateColumnStatisticsForPartition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("PartitionValues" Core..= partitionValues),
            Core.Just ("ColumnStatisticsList" Core..= columnStatisticsList),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest UpdateColumnStatisticsForPartition where
  type
    Rs UpdateColumnStatisticsForPartition =
      UpdateColumnStatisticsForPartitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSGlue.UpdateColumnStatisticsForPartition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateColumnStatisticsForPartitionResponse'
            Core.<$> (x Core..:? "Errors") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateColumnStatisticsForPartitionResponse' smart constructor.
data UpdateColumnStatisticsForPartitionResponse = UpdateColumnStatisticsForPartitionResponse'
  { -- | Error occurred during updating column statistics data.
    errors :: Core.Maybe [Types.ColumnStatisticsError],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateColumnStatisticsForPartitionResponse' value with any optional fields omitted.
mkUpdateColumnStatisticsForPartitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateColumnStatisticsForPartitionResponse
mkUpdateColumnStatisticsForPartitionResponse responseStatus =
  UpdateColumnStatisticsForPartitionResponse'
    { errors =
        Core.Nothing,
      responseStatus
    }

-- | Error occurred during updating column statistics data.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfprrsErrors :: Lens.Lens' UpdateColumnStatisticsForPartitionResponse (Core.Maybe [Types.ColumnStatisticsError])
ucsfprrsErrors = Lens.field @"errors"
{-# DEPRECATED ucsfprrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsfprrsResponseStatus :: Lens.Lens' UpdateColumnStatisticsForPartitionResponse Core.Int
ucsfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucsfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
