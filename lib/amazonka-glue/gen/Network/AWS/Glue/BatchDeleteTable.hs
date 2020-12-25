{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchDeleteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes multiple tables at once.
module Network.AWS.Glue.BatchDeleteTable
  ( -- * Creating a request
    BatchDeleteTable (..),
    mkBatchDeleteTable,

    -- ** Request lenses
    bdtDatabaseName,
    bdtTablesToDelete,
    bdtCatalogId,

    -- * Destructuring the response
    BatchDeleteTableResponse (..),
    mkBatchDeleteTableResponse,

    -- ** Response lenses
    bdtrrsErrors,
    bdtrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDeleteTable' smart constructor.
data BatchDeleteTable = BatchDeleteTable'
  { -- | The name of the catalog database in which the tables to delete reside. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Types.DatabaseName,
    -- | A list of the table to delete.
    tablesToDelete :: [Types.NameString],
    -- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteTable' value with any optional fields omitted.
mkBatchDeleteTable ::
  -- | 'databaseName'
  Types.DatabaseName ->
  BatchDeleteTable
mkBatchDeleteTable databaseName =
  BatchDeleteTable'
    { databaseName,
      tablesToDelete = Core.mempty,
      catalogId = Core.Nothing
    }

-- | The name of the catalog database in which the tables to delete reside. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtDatabaseName :: Lens.Lens' BatchDeleteTable Types.DatabaseName
bdtDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED bdtDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A list of the table to delete.
--
-- /Note:/ Consider using 'tablesToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtTablesToDelete :: Lens.Lens' BatchDeleteTable [Types.NameString]
bdtTablesToDelete = Lens.field @"tablesToDelete"
{-# DEPRECATED bdtTablesToDelete "Use generic-lens or generic-optics with 'tablesToDelete' instead." #-}

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtCatalogId :: Lens.Lens' BatchDeleteTable (Core.Maybe Types.CatalogId)
bdtCatalogId = Lens.field @"catalogId"
{-# DEPRECATED bdtCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON BatchDeleteTable where
  toJSON BatchDeleteTable {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TablesToDelete" Core..= tablesToDelete),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest BatchDeleteTable where
  type Rs BatchDeleteTable = BatchDeleteTableResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.BatchDeleteTable")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteTableResponse'
            Core.<$> (x Core..:? "Errors") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchDeleteTableResponse' smart constructor.
data BatchDeleteTableResponse = BatchDeleteTableResponse'
  { -- | A list of errors encountered in attempting to delete the specified tables.
    errors :: Core.Maybe [Types.TableError],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteTableResponse' value with any optional fields omitted.
mkBatchDeleteTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchDeleteTableResponse
mkBatchDeleteTableResponse responseStatus =
  BatchDeleteTableResponse' {errors = Core.Nothing, responseStatus}

-- | A list of errors encountered in attempting to delete the specified tables.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtrrsErrors :: Lens.Lens' BatchDeleteTableResponse (Core.Maybe [Types.TableError])
bdtrrsErrors = Lens.field @"errors"
{-# DEPRECATED bdtrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtrrsResponseStatus :: Lens.Lens' BatchDeleteTableResponse Core.Int
bdtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bdtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
