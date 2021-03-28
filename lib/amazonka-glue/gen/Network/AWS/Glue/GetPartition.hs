{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetPartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified partition.
module Network.AWS.Glue.GetPartition
    (
    -- * Creating a request
      GetPartition (..)
    , mkGetPartition
    -- ** Request lenses
    , gpDatabaseName
    , gpTableName
    , gpPartitionValues
    , gpCatalogId

    -- * Destructuring the response
    , GetPartitionResponse (..)
    , mkGetPartitionResponse
    -- ** Response lenses
    , gprrsPartition
    , gprrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPartition' smart constructor.
data GetPartition = GetPartition'
  { databaseName :: Types.NameString
    -- ^ The name of the catalog database where the partition resides.
  , tableName :: Types.NameString
    -- ^ The name of the partition's table.
  , partitionValues :: [Types.ValueString]
    -- ^ The values that define the partition.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog where the partition in question resides. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPartition' value with any optional fields omitted.
mkGetPartition
    :: Types.NameString -- ^ 'databaseName'
    -> Types.NameString -- ^ 'tableName'
    -> GetPartition
mkGetPartition databaseName tableName
  = GetPartition'{databaseName, tableName,
                  partitionValues = Core.mempty, catalogId = Core.Nothing}

-- | The name of the catalog database where the partition resides.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpDatabaseName :: Lens.Lens' GetPartition Types.NameString
gpDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gpDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the partition's table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpTableName :: Lens.Lens' GetPartition Types.NameString
gpTableName = Lens.field @"tableName"
{-# INLINEABLE gpTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The values that define the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPartitionValues :: Lens.Lens' GetPartition [Types.ValueString]
gpPartitionValues = Lens.field @"partitionValues"
{-# INLINEABLE gpPartitionValues #-}
{-# DEPRECATED partitionValues "Use generic-lens or generic-optics with 'partitionValues' instead"  #-}

-- | The ID of the Data Catalog where the partition in question resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpCatalogId :: Lens.Lens' GetPartition (Core.Maybe Types.CatalogId)
gpCatalogId = Lens.field @"catalogId"
{-# INLINEABLE gpCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery GetPartition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPartition where
        toHeaders GetPartition{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetPartition") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetPartition where
        toJSON GetPartition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  Core.Just ("PartitionValues" Core..= partitionValues),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest GetPartition where
        type Rs GetPartition = GetPartitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPartitionResponse' Core.<$>
                   (x Core..:? "Partition") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPartitionResponse' smart constructor.
data GetPartitionResponse = GetPartitionResponse'
  { partition :: Core.Maybe Types.Partition
    -- ^ The requested information, in the form of a @Partition@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetPartitionResponse' value with any optional fields omitted.
mkGetPartitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPartitionResponse
mkGetPartitionResponse responseStatus
  = GetPartitionResponse'{partition = Core.Nothing, responseStatus}

-- | The requested information, in the form of a @Partition@ object.
--
-- /Note:/ Consider using 'partition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPartition :: Lens.Lens' GetPartitionResponse (Core.Maybe Types.Partition)
gprrsPartition = Lens.field @"partition"
{-# INLINEABLE gprrsPartition #-}
{-# DEPRECATED partition "Use generic-lens or generic-optics with 'partition' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetPartitionResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
