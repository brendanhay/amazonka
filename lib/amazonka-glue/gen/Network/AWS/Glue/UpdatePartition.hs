{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdatePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a partition.
module Network.AWS.Glue.UpdatePartition
  ( -- * Creating a request
    UpdatePartition (..),
    mkUpdatePartition,

    -- ** Request lenses
    upDatabaseName,
    upTableName,
    upPartitionValueList,
    upPartitionInput,
    upCatalogId,

    -- * Destructuring the response
    UpdatePartitionResponse (..),
    mkUpdatePartitionResponse,

    -- ** Response lenses
    uprrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePartition' smart constructor.
data UpdatePartition = UpdatePartition'
  { -- | The name of the catalog database in which the table in question resides.
    databaseName :: Types.NameString,
    -- | The name of the table in which the partition to be updated is located.
    tableName :: Types.NameString,
    -- | List of partition key values that define the partition to update.
    partitionValueList :: [Types.ValueString],
    -- | The new partition object to update the partition to.
    --
    -- The @Values@ property can't be changed. If you want to change the partition key values for a partition, delete and recreate the partition.
    partitionInput :: Types.PartitionInput,
    -- | The ID of the Data Catalog where the partition to be updated resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdatePartition' value with any optional fields omitted.
mkUpdatePartition ::
  -- | 'databaseName'
  Types.NameString ->
  -- | 'tableName'
  Types.NameString ->
  -- | 'partitionInput'
  Types.PartitionInput ->
  UpdatePartition
mkUpdatePartition databaseName tableName partitionInput =
  UpdatePartition'
    { databaseName,
      tableName,
      partitionValueList = Core.mempty,
      partitionInput,
      catalogId = Core.Nothing
    }

-- | The name of the catalog database in which the table in question resides.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDatabaseName :: Lens.Lens' UpdatePartition Types.NameString
upDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED upDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table in which the partition to be updated is located.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTableName :: Lens.Lens' UpdatePartition Types.NameString
upTableName = Lens.field @"tableName"
{-# DEPRECATED upTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | List of partition key values that define the partition to update.
--
-- /Note:/ Consider using 'partitionValueList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPartitionValueList :: Lens.Lens' UpdatePartition [Types.ValueString]
upPartitionValueList = Lens.field @"partitionValueList"
{-# DEPRECATED upPartitionValueList "Use generic-lens or generic-optics with 'partitionValueList' instead." #-}

-- | The new partition object to update the partition to.
--
-- The @Values@ property can't be changed. If you want to change the partition key values for a partition, delete and recreate the partition.
--
-- /Note:/ Consider using 'partitionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPartitionInput :: Lens.Lens' UpdatePartition Types.PartitionInput
upPartitionInput = Lens.field @"partitionInput"
{-# DEPRECATED upPartitionInput "Use generic-lens or generic-optics with 'partitionInput' instead." #-}

-- | The ID of the Data Catalog where the partition to be updated resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upCatalogId :: Lens.Lens' UpdatePartition (Core.Maybe Types.CatalogId)
upCatalogId = Lens.field @"catalogId"
{-# DEPRECATED upCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON UpdatePartition where
  toJSON UpdatePartition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("PartitionValueList" Core..= partitionValueList),
            Core.Just ("PartitionInput" Core..= partitionInput),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest UpdatePartition where
  type Rs UpdatePartition = UpdatePartitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.UpdatePartition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePartitionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdatePartitionResponse' smart constructor.
newtype UpdatePartitionResponse = UpdatePartitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePartitionResponse' value with any optional fields omitted.
mkUpdatePartitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdatePartitionResponse
mkUpdatePartitionResponse responseStatus =
  UpdatePartitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdatePartitionResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
