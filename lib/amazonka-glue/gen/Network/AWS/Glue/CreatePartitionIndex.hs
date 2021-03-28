{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreatePartitionIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a specified partition index in an existing table.
module Network.AWS.Glue.CreatePartitionIndex
    (
    -- * Creating a request
      CreatePartitionIndex (..)
    , mkCreatePartitionIndex
    -- ** Request lenses
    , cpiDatabaseName
    , cpiTableName
    , cpiPartitionIndex
    , cpiCatalogId

    -- * Destructuring the response
    , CreatePartitionIndexResponse (..)
    , mkCreatePartitionIndexResponse
    -- ** Response lenses
    , cpirrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePartitionIndex' smart constructor.
data CreatePartitionIndex = CreatePartitionIndex'
  { databaseName :: Types.NameString
    -- ^ Specifies the name of a database in which you want to create a partition index.
  , tableName :: Types.NameString
    -- ^ Specifies the name of a table in which you want to create a partition index.
  , partitionIndex :: Types.PartitionIndex
    -- ^ Specifies a @PartitionIndex@ structure to create a partition index in an existing table.
  , catalogId :: Core.Maybe Types.CatalogIdString
    -- ^ The catalog ID where the table resides.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePartitionIndex' value with any optional fields omitted.
mkCreatePartitionIndex
    :: Types.NameString -- ^ 'databaseName'
    -> Types.NameString -- ^ 'tableName'
    -> Types.PartitionIndex -- ^ 'partitionIndex'
    -> CreatePartitionIndex
mkCreatePartitionIndex databaseName tableName partitionIndex
  = CreatePartitionIndex'{databaseName, tableName, partitionIndex,
                          catalogId = Core.Nothing}

-- | Specifies the name of a database in which you want to create a partition index.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiDatabaseName :: Lens.Lens' CreatePartitionIndex Types.NameString
cpiDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE cpiDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | Specifies the name of a table in which you want to create a partition index.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiTableName :: Lens.Lens' CreatePartitionIndex Types.NameString
cpiTableName = Lens.field @"tableName"
{-# INLINEABLE cpiTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | Specifies a @PartitionIndex@ structure to create a partition index in an existing table.
--
-- /Note:/ Consider using 'partitionIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiPartitionIndex :: Lens.Lens' CreatePartitionIndex Types.PartitionIndex
cpiPartitionIndex = Lens.field @"partitionIndex"
{-# INLINEABLE cpiPartitionIndex #-}
{-# DEPRECATED partitionIndex "Use generic-lens or generic-optics with 'partitionIndex' instead"  #-}

-- | The catalog ID where the table resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiCatalogId :: Lens.Lens' CreatePartitionIndex (Core.Maybe Types.CatalogIdString)
cpiCatalogId = Lens.field @"catalogId"
{-# INLINEABLE cpiCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery CreatePartitionIndex where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePartitionIndex where
        toHeaders CreatePartitionIndex{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.CreatePartitionIndex")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePartitionIndex where
        toJSON CreatePartitionIndex{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  Core.Just ("PartitionIndex" Core..= partitionIndex),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest CreatePartitionIndex where
        type Rs CreatePartitionIndex = CreatePartitionIndexResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreatePartitionIndexResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePartitionIndexResponse' smart constructor.
newtype CreatePartitionIndexResponse = CreatePartitionIndexResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePartitionIndexResponse' value with any optional fields omitted.
mkCreatePartitionIndexResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePartitionIndexResponse
mkCreatePartitionIndexResponse responseStatus
  = CreatePartitionIndexResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpirrsResponseStatus :: Lens.Lens' CreatePartitionIndexResponse Core.Int
cpirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
