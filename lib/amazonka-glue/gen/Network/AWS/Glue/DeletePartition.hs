{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeletePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified partition.
module Network.AWS.Glue.DeletePartition
  ( -- * Creating a request
    DeletePartition (..),
    mkDeletePartition,

    -- ** Request lenses
    dpDatabaseName,
    dpTableName,
    dpPartitionValues,
    dpCatalogId,

    -- * Destructuring the response
    DeletePartitionResponse (..),
    mkDeletePartitionResponse,

    -- ** Response lenses
    dprrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePartition' smart constructor.
data DeletePartition = DeletePartition'
  { -- | The name of the catalog database in which the table in question resides.
    databaseName :: Types.NameString,
    -- | The name of the table that contains the partition to be deleted.
    tableName :: Types.NameString,
    -- | The values that define the partition.
    partitionValues :: [Types.ValueString],
    -- | The ID of the Data Catalog where the partition to be deleted resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePartition' value with any optional fields omitted.
mkDeletePartition ::
  -- | 'databaseName'
  Types.NameString ->
  -- | 'tableName'
  Types.NameString ->
  DeletePartition
mkDeletePartition databaseName tableName =
  DeletePartition'
    { databaseName,
      tableName,
      partitionValues = Core.mempty,
      catalogId = Core.Nothing
    }

-- | The name of the catalog database in which the table in question resides.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDatabaseName :: Lens.Lens' DeletePartition Types.NameString
dpDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED dpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table that contains the partition to be deleted.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpTableName :: Lens.Lens' DeletePartition Types.NameString
dpTableName = Lens.field @"tableName"
{-# DEPRECATED dpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The values that define the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPartitionValues :: Lens.Lens' DeletePartition [Types.ValueString]
dpPartitionValues = Lens.field @"partitionValues"
{-# DEPRECATED dpPartitionValues "Use generic-lens or generic-optics with 'partitionValues' instead." #-}

-- | The ID of the Data Catalog where the partition to be deleted resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpCatalogId :: Lens.Lens' DeletePartition (Core.Maybe Types.CatalogId)
dpCatalogId = Lens.field @"catalogId"
{-# DEPRECATED dpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON DeletePartition where
  toJSON DeletePartition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("PartitionValues" Core..= partitionValues),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest DeletePartition where
  type Rs DeletePartition = DeletePartitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.DeletePartition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePartitionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeletePartitionResponse' smart constructor.
newtype DeletePartitionResponse = DeletePartitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePartitionResponse' value with any optional fields omitted.
mkDeletePartitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeletePartitionResponse
mkDeletePartitionResponse responseStatus =
  DeletePartitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DeletePartitionResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
