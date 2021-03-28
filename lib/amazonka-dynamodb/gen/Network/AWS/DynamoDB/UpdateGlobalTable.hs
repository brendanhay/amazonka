{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateGlobalTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes replicas in the specified global table. The global table must already exist to be able to use this operation. Any replica to be added must be empty, have the same name as the global table, have the same key schema, have DynamoDB Streams enabled, and have the same provisioned and maximum write capacity units.
--
-- If global secondary indexes are specified, then the following conditions must also be met: 
--
--     * The global secondary indexes must have the same name. 
--
--
--     * The global secondary indexes must have the same hash key and sort key (if present). 
--
--
--     * The global secondary indexes must have the same provisioned and maximum write capacity units. 
--
--
module Network.AWS.DynamoDB.UpdateGlobalTable
    (
    -- * Creating a request
      UpdateGlobalTable (..)
    , mkUpdateGlobalTable
    -- ** Request lenses
    , ugtGlobalTableName
    , ugtReplicaUpdates

    -- * Destructuring the response
    , UpdateGlobalTableResponse (..)
    , mkUpdateGlobalTableResponse
    -- ** Response lenses
    , ugtrrsGlobalTableDescription
    , ugtrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGlobalTable' smart constructor.
data UpdateGlobalTable = UpdateGlobalTable'
  { globalTableName :: Types.GlobalTableName
    -- ^ The global table name.
  , replicaUpdates :: [Types.ReplicaUpdate]
    -- ^ A list of Regions that should be added or removed from the global table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGlobalTable' value with any optional fields omitted.
mkUpdateGlobalTable
    :: Types.GlobalTableName -- ^ 'globalTableName'
    -> UpdateGlobalTable
mkUpdateGlobalTable globalTableName
  = UpdateGlobalTable'{globalTableName, replicaUpdates = Core.mempty}

-- | The global table name.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtGlobalTableName :: Lens.Lens' UpdateGlobalTable Types.GlobalTableName
ugtGlobalTableName = Lens.field @"globalTableName"
{-# INLINEABLE ugtGlobalTableName #-}
{-# DEPRECATED globalTableName "Use generic-lens or generic-optics with 'globalTableName' instead"  #-}

-- | A list of Regions that should be added or removed from the global table.
--
-- /Note:/ Consider using 'replicaUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtReplicaUpdates :: Lens.Lens' UpdateGlobalTable [Types.ReplicaUpdate]
ugtReplicaUpdates = Lens.field @"replicaUpdates"
{-# INLINEABLE ugtReplicaUpdates #-}
{-# DEPRECATED replicaUpdates "Use generic-lens or generic-optics with 'replicaUpdates' instead"  #-}

instance Core.ToQuery UpdateGlobalTable where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGlobalTable where
        toHeaders UpdateGlobalTable{..}
          = Core.pure ("X-Amz-Target", "DynamoDB_20120810.UpdateGlobalTable")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON UpdateGlobalTable where
        toJSON UpdateGlobalTable{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GlobalTableName" Core..= globalTableName),
                  Core.Just ("ReplicaUpdates" Core..= replicaUpdates)])

instance Core.AWSRequest UpdateGlobalTable where
        type Rs UpdateGlobalTable = UpdateGlobalTableResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateGlobalTableResponse' Core.<$>
                   (x Core..:? "GlobalTableDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateGlobalTableResponse' smart constructor.
data UpdateGlobalTableResponse = UpdateGlobalTableResponse'
  { globalTableDescription :: Core.Maybe Types.GlobalTableDescription
    -- ^ Contains the details of the global table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateGlobalTableResponse' value with any optional fields omitted.
mkUpdateGlobalTableResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateGlobalTableResponse
mkUpdateGlobalTableResponse responseStatus
  = UpdateGlobalTableResponse'{globalTableDescription = Core.Nothing,
                               responseStatus}

-- | Contains the details of the global table.
--
-- /Note:/ Consider using 'globalTableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtrrsGlobalTableDescription :: Lens.Lens' UpdateGlobalTableResponse (Core.Maybe Types.GlobalTableDescription)
ugtrrsGlobalTableDescription = Lens.field @"globalTableDescription"
{-# INLINEABLE ugtrrsGlobalTableDescription #-}
{-# DEPRECATED globalTableDescription "Use generic-lens or generic-optics with 'globalTableDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtrrsResponseStatus :: Lens.Lens' UpdateGlobalTableResponse Core.Int
ugtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ugtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
