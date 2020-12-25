{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeletePartitionIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified partition index from an existing table.
module Network.AWS.Glue.DeletePartitionIndex
  ( -- * Creating a request
    DeletePartitionIndex (..),
    mkDeletePartitionIndex,

    -- ** Request lenses
    dpiDatabaseName,
    dpiTableName,
    dpiIndexName,
    dpiCatalogId,

    -- * Destructuring the response
    DeletePartitionIndexResponse (..),
    mkDeletePartitionIndexResponse,

    -- ** Response lenses
    dpirrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePartitionIndex' smart constructor.
data DeletePartitionIndex = DeletePartitionIndex'
  { -- | Specifies the name of a database from which you want to delete a partition index.
    databaseName :: Types.DatabaseName,
    -- | Specifies the name of a table from which you want to delete a partition index.
    tableName :: Types.TableName,
    -- | The name of the partition index to be deleted.
    indexName :: Types.IndexName,
    -- | The catalog ID where the table resides.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePartitionIndex' value with any optional fields omitted.
mkDeletePartitionIndex ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'tableName'
  Types.TableName ->
  -- | 'indexName'
  Types.IndexName ->
  DeletePartitionIndex
mkDeletePartitionIndex databaseName tableName indexName =
  DeletePartitionIndex'
    { databaseName,
      tableName,
      indexName,
      catalogId = Core.Nothing
    }

-- | Specifies the name of a database from which you want to delete a partition index.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiDatabaseName :: Lens.Lens' DeletePartitionIndex Types.DatabaseName
dpiDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED dpiDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Specifies the name of a table from which you want to delete a partition index.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiTableName :: Lens.Lens' DeletePartitionIndex Types.TableName
dpiTableName = Lens.field @"tableName"
{-# DEPRECATED dpiTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The name of the partition index to be deleted.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiIndexName :: Lens.Lens' DeletePartitionIndex Types.IndexName
dpiIndexName = Lens.field @"indexName"
{-# DEPRECATED dpiIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The catalog ID where the table resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiCatalogId :: Lens.Lens' DeletePartitionIndex (Core.Maybe Types.CatalogId)
dpiCatalogId = Lens.field @"catalogId"
{-# DEPRECATED dpiCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON DeletePartitionIndex where
  toJSON DeletePartitionIndex {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("IndexName" Core..= indexName),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest DeletePartitionIndex where
  type Rs DeletePartitionIndex = DeletePartitionIndexResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.DeletePartitionIndex")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePartitionIndexResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeletePartitionIndexResponse' smart constructor.
newtype DeletePartitionIndexResponse = DeletePartitionIndexResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePartitionIndexResponse' value with any optional fields omitted.
mkDeletePartitionIndexResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeletePartitionIndexResponse
mkDeletePartitionIndexResponse responseStatus =
  DeletePartitionIndexResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpirrsResponseStatus :: Lens.Lens' DeletePartitionIndexResponse Core.Int
dpirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
