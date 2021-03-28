{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreatePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new partition.
module Network.AWS.Glue.CreatePartition
    (
    -- * Creating a request
      CreatePartition (..)
    , mkCreatePartition
    -- ** Request lenses
    , cpDatabaseName
    , cpTableName
    , cpPartitionInput
    , cpCatalogId

    -- * Destructuring the response
    , CreatePartitionResponse (..)
    , mkCreatePartitionResponse
    -- ** Response lenses
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePartition' smart constructor.
data CreatePartition = CreatePartition'
  { databaseName :: Types.DatabaseName
    -- ^ The name of the metadata database in which the partition is to be created.
  , tableName :: Types.TableName
    -- ^ The name of the metadata table in which the partition is to be created.
  , partitionInput :: Types.PartitionInput
    -- ^ A @PartitionInput@ structure defining the partition to be created.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The AWS account ID of the catalog in which the partition is to be created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreatePartition' value with any optional fields omitted.
mkCreatePartition
    :: Types.DatabaseName -- ^ 'databaseName'
    -> Types.TableName -- ^ 'tableName'
    -> Types.PartitionInput -- ^ 'partitionInput'
    -> CreatePartition
mkCreatePartition databaseName tableName partitionInput
  = CreatePartition'{databaseName, tableName, partitionInput,
                     catalogId = Core.Nothing}

-- | The name of the metadata database in which the partition is to be created.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDatabaseName :: Lens.Lens' CreatePartition Types.DatabaseName
cpDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE cpDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the metadata table in which the partition is to be created.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTableName :: Lens.Lens' CreatePartition Types.TableName
cpTableName = Lens.field @"tableName"
{-# INLINEABLE cpTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | A @PartitionInput@ structure defining the partition to be created.
--
-- /Note:/ Consider using 'partitionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPartitionInput :: Lens.Lens' CreatePartition Types.PartitionInput
cpPartitionInput = Lens.field @"partitionInput"
{-# INLINEABLE cpPartitionInput #-}
{-# DEPRECATED partitionInput "Use generic-lens or generic-optics with 'partitionInput' instead"  #-}

-- | The AWS account ID of the catalog in which the partition is to be created.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCatalogId :: Lens.Lens' CreatePartition (Core.Maybe Types.CatalogId)
cpCatalogId = Lens.field @"catalogId"
{-# INLINEABLE cpCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery CreatePartition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePartition where
        toHeaders CreatePartition{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.CreatePartition") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePartition where
        toJSON CreatePartition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  Core.Just ("PartitionInput" Core..= partitionInput),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest CreatePartition where
        type Rs CreatePartition = CreatePartitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreatePartitionResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePartitionResponse' smart constructor.
newtype CreatePartitionResponse = CreatePartitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePartitionResponse' value with any optional fields omitted.
mkCreatePartitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePartitionResponse
mkCreatePartitionResponse responseStatus
  = CreatePartitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePartitionResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
