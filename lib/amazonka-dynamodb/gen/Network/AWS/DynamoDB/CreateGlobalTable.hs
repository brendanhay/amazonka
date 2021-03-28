{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.CreateGlobalTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a global table from an existing table. A global table creates a replication relationship between two or more DynamoDB tables with the same table name in the provided Regions. 
--
-- If you want to add a new replica table to a global table, each of the following conditions must be true:
--
--     * The table must have the same primary key as all of the other replicas.
--
--
--     * The table must have the same name as all of the other replicas.
--
--
--     * The table must have DynamoDB Streams enabled, with the stream containing both the new and the old images of the item.
--
--
--     * None of the replica tables in the global table can contain any data.
--
--
-- If global secondary indexes are specified, then the following conditions must also be met: 
--
--     * The global secondary indexes must have the same name. 
--
--
--     * The global secondary indexes must have the same hash key and sort key (if present). 
--
--
-- If local secondary indexes are specified, then the following conditions must also be met: 
--
--     * The local secondary indexes must have the same name. 
--
--
--     * The local secondary indexes must have the same hash key and sort key (if present). 
--
--
-- /Important:/ Write capacity settings should be set consistently across your replica tables and secondary indexes. DynamoDB strongly recommends enabling auto scaling to manage the write capacity settings for all of your global tables replicas and indexes. 
-- If you prefer to manage write capacity settings manually, you should provision equal replicated write capacity units to your replica tables. You should also provision equal replicated write capacity units to matching secondary indexes across your global table. 
module Network.AWS.DynamoDB.CreateGlobalTable
    (
    -- * Creating a request
      CreateGlobalTable (..)
    , mkCreateGlobalTable
    -- ** Request lenses
    , cgtGlobalTableName
    , cgtReplicationGroup

    -- * Destructuring the response
    , CreateGlobalTableResponse (..)
    , mkCreateGlobalTableResponse
    -- ** Response lenses
    , cgtrrsGlobalTableDescription
    , cgtrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGlobalTable' smart constructor.
data CreateGlobalTable = CreateGlobalTable'
  { globalTableName :: Types.GlobalTableName
    -- ^ The global table name.
  , replicationGroup :: [Types.Replica]
    -- ^ The Regions where the global table needs to be created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGlobalTable' value with any optional fields omitted.
mkCreateGlobalTable
    :: Types.GlobalTableName -- ^ 'globalTableName'
    -> CreateGlobalTable
mkCreateGlobalTable globalTableName
  = CreateGlobalTable'{globalTableName,
                       replicationGroup = Core.mempty}

-- | The global table name.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgtGlobalTableName :: Lens.Lens' CreateGlobalTable Types.GlobalTableName
cgtGlobalTableName = Lens.field @"globalTableName"
{-# INLINEABLE cgtGlobalTableName #-}
{-# DEPRECATED globalTableName "Use generic-lens or generic-optics with 'globalTableName' instead"  #-}

-- | The Regions where the global table needs to be created.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgtReplicationGroup :: Lens.Lens' CreateGlobalTable [Types.Replica]
cgtReplicationGroup = Lens.field @"replicationGroup"
{-# INLINEABLE cgtReplicationGroup #-}
{-# DEPRECATED replicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead"  #-}

instance Core.ToQuery CreateGlobalTable where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateGlobalTable where
        toHeaders CreateGlobalTable{..}
          = Core.pure ("X-Amz-Target", "DynamoDB_20120810.CreateGlobalTable")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON CreateGlobalTable where
        toJSON CreateGlobalTable{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GlobalTableName" Core..= globalTableName),
                  Core.Just ("ReplicationGroup" Core..= replicationGroup)])

instance Core.AWSRequest CreateGlobalTable where
        type Rs CreateGlobalTable = CreateGlobalTableResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateGlobalTableResponse' Core.<$>
                   (x Core..:? "GlobalTableDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateGlobalTableResponse' smart constructor.
data CreateGlobalTableResponse = CreateGlobalTableResponse'
  { globalTableDescription :: Core.Maybe Types.GlobalTableDescription
    -- ^ Contains the details of the global table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateGlobalTableResponse' value with any optional fields omitted.
mkCreateGlobalTableResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateGlobalTableResponse
mkCreateGlobalTableResponse responseStatus
  = CreateGlobalTableResponse'{globalTableDescription = Core.Nothing,
                               responseStatus}

-- | Contains the details of the global table.
--
-- /Note:/ Consider using 'globalTableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgtrrsGlobalTableDescription :: Lens.Lens' CreateGlobalTableResponse (Core.Maybe Types.GlobalTableDescription)
cgtrrsGlobalTableDescription = Lens.field @"globalTableDescription"
{-# INLINEABLE cgtrrsGlobalTableDescription #-}
{-# DEPRECATED globalTableDescription "Use generic-lens or generic-optics with 'globalTableDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgtrrsResponseStatus :: Lens.Lens' CreateGlobalTableResponse Core.Int
cgtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cgtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
