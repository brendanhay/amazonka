{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetPartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves partitions in a batch request.
module Network.AWS.Glue.BatchGetPartition
    (
    -- * Creating a request
      BatchGetPartition (..)
    , mkBatchGetPartition
    -- ** Request lenses
    , bgpDatabaseName
    , bgpTableName
    , bgpPartitionsToGet
    , bgpCatalogId

    -- * Destructuring the response
    , BatchGetPartitionResponse (..)
    , mkBatchGetPartitionResponse
    -- ** Response lenses
    , bgprrsPartitions
    , bgprrsUnprocessedKeys
    , bgprrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetPartition' smart constructor.
data BatchGetPartition = BatchGetPartition'
  { databaseName :: Types.NameString
    -- ^ The name of the catalog database where the partitions reside.
  , tableName :: Types.NameString
    -- ^ The name of the partitions' table.
  , partitionsToGet :: [Types.PartitionValueList]
    -- ^ A list of partition values identifying the partitions to retrieve.
  , catalogId :: Core.Maybe Types.CatalogIdString
    -- ^ The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetPartition' value with any optional fields omitted.
mkBatchGetPartition
    :: Types.NameString -- ^ 'databaseName'
    -> Types.NameString -- ^ 'tableName'
    -> BatchGetPartition
mkBatchGetPartition databaseName tableName
  = BatchGetPartition'{databaseName, tableName,
                       partitionsToGet = Core.mempty, catalogId = Core.Nothing}

-- | The name of the catalog database where the partitions reside.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpDatabaseName :: Lens.Lens' BatchGetPartition Types.NameString
bgpDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE bgpDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the partitions' table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpTableName :: Lens.Lens' BatchGetPartition Types.NameString
bgpTableName = Lens.field @"tableName"
{-# INLINEABLE bgpTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | A list of partition values identifying the partitions to retrieve.
--
-- /Note:/ Consider using 'partitionsToGet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpPartitionsToGet :: Lens.Lens' BatchGetPartition [Types.PartitionValueList]
bgpPartitionsToGet = Lens.field @"partitionsToGet"
{-# INLINEABLE bgpPartitionsToGet #-}
{-# DEPRECATED partitionsToGet "Use generic-lens or generic-optics with 'partitionsToGet' instead"  #-}

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpCatalogId :: Lens.Lens' BatchGetPartition (Core.Maybe Types.CatalogIdString)
bgpCatalogId = Lens.field @"catalogId"
{-# INLINEABLE bgpCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery BatchGetPartition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetPartition where
        toHeaders BatchGetPartition{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.BatchGetPartition") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetPartition where
        toJSON BatchGetPartition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  Core.Just ("PartitionsToGet" Core..= partitionsToGet),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest BatchGetPartition where
        type Rs BatchGetPartition = BatchGetPartitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetPartitionResponse' Core.<$>
                   (x Core..:? "Partitions") Core.<*> x Core..:? "UnprocessedKeys"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetPartitionResponse' smart constructor.
data BatchGetPartitionResponse = BatchGetPartitionResponse'
  { partitions :: Core.Maybe [Types.Partition]
    -- ^ A list of the requested partitions.
  , unprocessedKeys :: Core.Maybe [Types.PartitionValueList]
    -- ^ A list of the partition values in the request for which partitions were not returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetPartitionResponse' value with any optional fields omitted.
mkBatchGetPartitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetPartitionResponse
mkBatchGetPartitionResponse responseStatus
  = BatchGetPartitionResponse'{partitions = Core.Nothing,
                               unprocessedKeys = Core.Nothing, responseStatus}

-- | A list of the requested partitions.
--
-- /Note:/ Consider using 'partitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprrsPartitions :: Lens.Lens' BatchGetPartitionResponse (Core.Maybe [Types.Partition])
bgprrsPartitions = Lens.field @"partitions"
{-# INLINEABLE bgprrsPartitions #-}
{-# DEPRECATED partitions "Use generic-lens or generic-optics with 'partitions' instead"  #-}

-- | A list of the partition values in the request for which partitions were not returned.
--
-- /Note:/ Consider using 'unprocessedKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprrsUnprocessedKeys :: Lens.Lens' BatchGetPartitionResponse (Core.Maybe [Types.PartitionValueList])
bgprrsUnprocessedKeys = Lens.field @"unprocessedKeys"
{-# INLINEABLE bgprrsUnprocessedKeys #-}
{-# DEPRECATED unprocessedKeys "Use generic-lens or generic-optics with 'unprocessedKeys' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprrsResponseStatus :: Lens.Lens' BatchGetPartitionResponse Core.Int
bgprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
