{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchDeletePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more partitions in a batch operation.
module Network.AWS.Glue.BatchDeletePartition
    (
    -- * Creating a request
      BatchDeletePartition (..)
    , mkBatchDeletePartition
    -- ** Request lenses
    , bdpDatabaseName
    , bdpTableName
    , bdpPartitionsToDelete
    , bdpCatalogId

    -- * Destructuring the response
    , BatchDeletePartitionResponse (..)
    , mkBatchDeletePartitionResponse
    -- ** Response lenses
    , bdprrsErrors
    , bdprrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDeletePartition' smart constructor.
data BatchDeletePartition = BatchDeletePartition'
  { databaseName :: Types.DatabaseName
    -- ^ The name of the catalog database in which the table in question resides.
  , tableName :: Types.TableName
    -- ^ The name of the table that contains the partitions to be deleted.
  , partitionsToDelete :: [Types.PartitionValueList]
    -- ^ A list of @PartitionInput@ structures that define the partitions to be deleted.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog where the partition to be deleted resides. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeletePartition' value with any optional fields omitted.
mkBatchDeletePartition
    :: Types.DatabaseName -- ^ 'databaseName'
    -> Types.TableName -- ^ 'tableName'
    -> BatchDeletePartition
mkBatchDeletePartition databaseName tableName
  = BatchDeletePartition'{databaseName, tableName,
                          partitionsToDelete = Core.mempty, catalogId = Core.Nothing}

-- | The name of the catalog database in which the table in question resides.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpDatabaseName :: Lens.Lens' BatchDeletePartition Types.DatabaseName
bdpDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE bdpDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the table that contains the partitions to be deleted.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpTableName :: Lens.Lens' BatchDeletePartition Types.TableName
bdpTableName = Lens.field @"tableName"
{-# INLINEABLE bdpTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | A list of @PartitionInput@ structures that define the partitions to be deleted.
--
-- /Note:/ Consider using 'partitionsToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpPartitionsToDelete :: Lens.Lens' BatchDeletePartition [Types.PartitionValueList]
bdpPartitionsToDelete = Lens.field @"partitionsToDelete"
{-# INLINEABLE bdpPartitionsToDelete #-}
{-# DEPRECATED partitionsToDelete "Use generic-lens or generic-optics with 'partitionsToDelete' instead"  #-}

-- | The ID of the Data Catalog where the partition to be deleted resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpCatalogId :: Lens.Lens' BatchDeletePartition (Core.Maybe Types.CatalogId)
bdpCatalogId = Lens.field @"catalogId"
{-# INLINEABLE bdpCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery BatchDeletePartition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchDeletePartition where
        toHeaders BatchDeletePartition{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.BatchDeletePartition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchDeletePartition where
        toJSON BatchDeletePartition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  Core.Just ("PartitionsToDelete" Core..= partitionsToDelete),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest BatchDeletePartition where
        type Rs BatchDeletePartition = BatchDeletePartitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchDeletePartitionResponse' Core.<$>
                   (x Core..:? "Errors") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchDeletePartitionResponse' smart constructor.
data BatchDeletePartitionResponse = BatchDeletePartitionResponse'
  { errors :: Core.Maybe [Types.PartitionError]
    -- ^ The errors encountered when trying to delete the requested partitions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeletePartitionResponse' value with any optional fields omitted.
mkBatchDeletePartitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchDeletePartitionResponse
mkBatchDeletePartitionResponse responseStatus
  = BatchDeletePartitionResponse'{errors = Core.Nothing,
                                  responseStatus}

-- | The errors encountered when trying to delete the requested partitions.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdprrsErrors :: Lens.Lens' BatchDeletePartitionResponse (Core.Maybe [Types.PartitionError])
bdprrsErrors = Lens.field @"errors"
{-# INLINEABLE bdprrsErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdprrsResponseStatus :: Lens.Lens' BatchDeletePartitionResponse Core.Int
bdprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bdprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
