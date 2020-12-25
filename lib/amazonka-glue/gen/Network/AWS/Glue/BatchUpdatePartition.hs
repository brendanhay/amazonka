{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchUpdatePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more partitions in a batch operation.
module Network.AWS.Glue.BatchUpdatePartition
  ( -- * Creating a request
    BatchUpdatePartition (..),
    mkBatchUpdatePartition,

    -- ** Request lenses
    bupDatabaseName,
    bupTableName,
    bupEntries,
    bupCatalogId,

    -- * Destructuring the response
    BatchUpdatePartitionResponse (..),
    mkBatchUpdatePartitionResponse,

    -- ** Response lenses
    buprrsErrors,
    buprrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchUpdatePartition' smart constructor.
data BatchUpdatePartition = BatchUpdatePartition'
  { -- | The name of the metadata database in which the partition is to be updated.
    databaseName :: Types.DatabaseName,
    -- | The name of the metadata table in which the partition is to be updated.
    tableName :: Types.TableName,
    -- | A list of up to 100 @BatchUpdatePartitionRequestEntry@ objects to update.
    entries :: Core.NonEmpty Types.BatchUpdatePartitionRequestEntry,
    -- | The ID of the catalog in which the partition is to be updated. Currently, this should be the AWS account ID.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchUpdatePartition' value with any optional fields omitted.
mkBatchUpdatePartition ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'tableName'
  Types.TableName ->
  -- | 'entries'
  Core.NonEmpty Types.BatchUpdatePartitionRequestEntry ->
  BatchUpdatePartition
mkBatchUpdatePartition databaseName tableName entries =
  BatchUpdatePartition'
    { databaseName,
      tableName,
      entries,
      catalogId = Core.Nothing
    }

-- | The name of the metadata database in which the partition is to be updated.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupDatabaseName :: Lens.Lens' BatchUpdatePartition Types.DatabaseName
bupDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED bupDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the metadata table in which the partition is to be updated.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupTableName :: Lens.Lens' BatchUpdatePartition Types.TableName
bupTableName = Lens.field @"tableName"
{-# DEPRECATED bupTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of up to 100 @BatchUpdatePartitionRequestEntry@ objects to update.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupEntries :: Lens.Lens' BatchUpdatePartition (Core.NonEmpty Types.BatchUpdatePartitionRequestEntry)
bupEntries = Lens.field @"entries"
{-# DEPRECATED bupEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The ID of the catalog in which the partition is to be updated. Currently, this should be the AWS account ID.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupCatalogId :: Lens.Lens' BatchUpdatePartition (Core.Maybe Types.CatalogId)
bupCatalogId = Lens.field @"catalogId"
{-# DEPRECATED bupCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON BatchUpdatePartition where
  toJSON BatchUpdatePartition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("Entries" Core..= entries),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest BatchUpdatePartition where
  type Rs BatchUpdatePartition = BatchUpdatePartitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.BatchUpdatePartition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdatePartitionResponse'
            Core.<$> (x Core..:? "Errors") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchUpdatePartitionResponse' smart constructor.
data BatchUpdatePartitionResponse = BatchUpdatePartitionResponse'
  { -- | The errors encountered when trying to update the requested partitions. A list of @BatchUpdatePartitionFailureEntry@ objects.
    errors :: Core.Maybe [Types.BatchUpdatePartitionFailureEntry],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchUpdatePartitionResponse' value with any optional fields omitted.
mkBatchUpdatePartitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchUpdatePartitionResponse
mkBatchUpdatePartitionResponse responseStatus =
  BatchUpdatePartitionResponse'
    { errors = Core.Nothing,
      responseStatus
    }

-- | The errors encountered when trying to update the requested partitions. A list of @BatchUpdatePartitionFailureEntry@ objects.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buprrsErrors :: Lens.Lens' BatchUpdatePartitionResponse (Core.Maybe [Types.BatchUpdatePartitionFailureEntry])
buprrsErrors = Lens.field @"errors"
{-# DEPRECATED buprrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buprrsResponseStatus :: Lens.Lens' BatchUpdatePartitionResponse Core.Int
buprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED buprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
