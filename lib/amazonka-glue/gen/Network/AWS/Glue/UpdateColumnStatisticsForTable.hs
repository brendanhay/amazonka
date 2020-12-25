{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateColumnStatisticsForTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates table statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this operation is @UpdateTable@ .
module Network.AWS.Glue.UpdateColumnStatisticsForTable
  ( -- * Creating a request
    UpdateColumnStatisticsForTable (..),
    mkUpdateColumnStatisticsForTable,

    -- ** Request lenses
    ucsftDatabaseName,
    ucsftTableName,
    ucsftColumnStatisticsList,
    ucsftCatalogId,

    -- * Destructuring the response
    UpdateColumnStatisticsForTableResponse (..),
    mkUpdateColumnStatisticsForTableResponse,

    -- ** Response lenses
    ucsftrrsErrors,
    ucsftrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateColumnStatisticsForTable' smart constructor.
data UpdateColumnStatisticsForTable = UpdateColumnStatisticsForTable'
  { -- | The name of the catalog database where the partitions reside.
    databaseName :: Types.NameString,
    -- | The name of the partitions' table.
    tableName :: Types.NameString,
    -- | A list of the column statistics.
    columnStatisticsList :: [Types.ColumnStatistics],
    -- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogIdString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateColumnStatisticsForTable' value with any optional fields omitted.
mkUpdateColumnStatisticsForTable ::
  -- | 'databaseName'
  Types.NameString ->
  -- | 'tableName'
  Types.NameString ->
  UpdateColumnStatisticsForTable
mkUpdateColumnStatisticsForTable databaseName tableName =
  UpdateColumnStatisticsForTable'
    { databaseName,
      tableName,
      columnStatisticsList = Core.mempty,
      catalogId = Core.Nothing
    }

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftDatabaseName :: Lens.Lens' UpdateColumnStatisticsForTable Types.NameString
ucsftDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED ucsftDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftTableName :: Lens.Lens' UpdateColumnStatisticsForTable Types.NameString
ucsftTableName = Lens.field @"tableName"
{-# DEPRECATED ucsftTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of the column statistics.
--
-- /Note:/ Consider using 'columnStatisticsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftColumnStatisticsList :: Lens.Lens' UpdateColumnStatisticsForTable [Types.ColumnStatistics]
ucsftColumnStatisticsList = Lens.field @"columnStatisticsList"
{-# DEPRECATED ucsftColumnStatisticsList "Use generic-lens or generic-optics with 'columnStatisticsList' instead." #-}

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftCatalogId :: Lens.Lens' UpdateColumnStatisticsForTable (Core.Maybe Types.CatalogIdString)
ucsftCatalogId = Lens.field @"catalogId"
{-# DEPRECATED ucsftCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON UpdateColumnStatisticsForTable where
  toJSON UpdateColumnStatisticsForTable {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("ColumnStatisticsList" Core..= columnStatisticsList),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest UpdateColumnStatisticsForTable where
  type
    Rs UpdateColumnStatisticsForTable =
      UpdateColumnStatisticsForTableResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSGlue.UpdateColumnStatisticsForTable")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateColumnStatisticsForTableResponse'
            Core.<$> (x Core..:? "Errors") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateColumnStatisticsForTableResponse' smart constructor.
data UpdateColumnStatisticsForTableResponse = UpdateColumnStatisticsForTableResponse'
  { -- | List of ColumnStatisticsErrors.
    errors :: Core.Maybe [Types.ColumnStatisticsError],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateColumnStatisticsForTableResponse' value with any optional fields omitted.
mkUpdateColumnStatisticsForTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateColumnStatisticsForTableResponse
mkUpdateColumnStatisticsForTableResponse responseStatus =
  UpdateColumnStatisticsForTableResponse'
    { errors = Core.Nothing,
      responseStatus
    }

-- | List of ColumnStatisticsErrors.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftrrsErrors :: Lens.Lens' UpdateColumnStatisticsForTableResponse (Core.Maybe [Types.ColumnStatisticsError])
ucsftrrsErrors = Lens.field @"errors"
{-# DEPRECATED ucsftrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsftrrsResponseStatus :: Lens.Lens' UpdateColumnStatisticsForTableResponse Core.Int
ucsftrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucsftrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
