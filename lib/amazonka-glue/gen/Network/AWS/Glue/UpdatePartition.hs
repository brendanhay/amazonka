{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdatePartition (..)
    , mkUpdatePartition
    -- ** Request lenses
    , upDatabaseName
    , upTableName
    , upPartitionValueList
    , upPartitionInput
    , upCatalogId

    -- * Destructuring the response
    , UpdatePartitionResponse (..)
    , mkUpdatePartitionResponse
    -- ** Response lenses
    , uprrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePartition' smart constructor.
data UpdatePartition = UpdatePartition'
  { databaseName :: Types.NameString
    -- ^ The name of the catalog database in which the table in question resides.
  , tableName :: Types.NameString
    -- ^ The name of the table in which the partition to be updated is located.
  , partitionValueList :: [Types.ValueString]
    -- ^ List of partition key values that define the partition to update.
  , partitionInput :: Types.PartitionInput
    -- ^ The new partition object to update the partition to.
--
-- The @Values@ property can't be changed. If you want to change the partition key values for a partition, delete and recreate the partition.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog where the partition to be updated resides. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdatePartition' value with any optional fields omitted.
mkUpdatePartition
    :: Types.NameString -- ^ 'databaseName'
    -> Types.NameString -- ^ 'tableName'
    -> Types.PartitionInput -- ^ 'partitionInput'
    -> UpdatePartition
mkUpdatePartition databaseName tableName partitionInput
  = UpdatePartition'{databaseName, tableName,
                     partitionValueList = Core.mempty, partitionInput,
                     catalogId = Core.Nothing}

-- | The name of the catalog database in which the table in question resides.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDatabaseName :: Lens.Lens' UpdatePartition Types.NameString
upDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE upDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the table in which the partition to be updated is located.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTableName :: Lens.Lens' UpdatePartition Types.NameString
upTableName = Lens.field @"tableName"
{-# INLINEABLE upTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | List of partition key values that define the partition to update.
--
-- /Note:/ Consider using 'partitionValueList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPartitionValueList :: Lens.Lens' UpdatePartition [Types.ValueString]
upPartitionValueList = Lens.field @"partitionValueList"
{-# INLINEABLE upPartitionValueList #-}
{-# DEPRECATED partitionValueList "Use generic-lens or generic-optics with 'partitionValueList' instead"  #-}

-- | The new partition object to update the partition to.
--
-- The @Values@ property can't be changed. If you want to change the partition key values for a partition, delete and recreate the partition.
--
-- /Note:/ Consider using 'partitionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPartitionInput :: Lens.Lens' UpdatePartition Types.PartitionInput
upPartitionInput = Lens.field @"partitionInput"
{-# INLINEABLE upPartitionInput #-}
{-# DEPRECATED partitionInput "Use generic-lens or generic-optics with 'partitionInput' instead"  #-}

-- | The ID of the Data Catalog where the partition to be updated resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upCatalogId :: Lens.Lens' UpdatePartition (Core.Maybe Types.CatalogId)
upCatalogId = Lens.field @"catalogId"
{-# INLINEABLE upCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery UpdatePartition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdatePartition where
        toHeaders UpdatePartition{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.UpdatePartition") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdatePartition where
        toJSON UpdatePartition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  Core.Just ("PartitionValueList" Core..= partitionValueList),
                  Core.Just ("PartitionInput" Core..= partitionInput),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest UpdatePartition where
        type Rs UpdatePartition = UpdatePartitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdatePartitionResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdatePartitionResponse' smart constructor.
newtype UpdatePartitionResponse = UpdatePartitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePartitionResponse' value with any optional fields omitted.
mkUpdatePartitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdatePartitionResponse
mkUpdatePartitionResponse responseStatus
  = UpdatePartitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdatePartitionResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
