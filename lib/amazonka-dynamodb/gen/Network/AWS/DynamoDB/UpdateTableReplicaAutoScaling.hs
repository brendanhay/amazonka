{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates auto scaling settings on your global tables at once.
module Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
    (
    -- * Creating a request
      UpdateTableReplicaAutoScaling (..)
    , mkUpdateTableReplicaAutoScaling
    -- ** Request lenses
    , utrasTableName
    , utrasGlobalSecondaryIndexUpdates
    , utrasProvisionedWriteCapacityAutoScalingUpdate
    , utrasReplicaUpdates

    -- * Destructuring the response
    , UpdateTableReplicaAutoScalingResponse (..)
    , mkUpdateTableReplicaAutoScalingResponse
    -- ** Response lenses
    , utrasrrsTableAutoScalingDescription
    , utrasrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTableReplicaAutoScaling' smart constructor.
data UpdateTableReplicaAutoScaling = UpdateTableReplicaAutoScaling'
  { tableName :: Types.TableName
    -- ^ The name of the global table to be updated.
  , globalSecondaryIndexUpdates :: Core.Maybe (Core.NonEmpty Types.GlobalSecondaryIndexAutoScalingUpdate)
    -- ^ Represents the auto scaling settings of the global secondary indexes of the replica to be updated.
  , provisionedWriteCapacityAutoScalingUpdate :: Core.Maybe Types.AutoScalingSettingsUpdate
  , replicaUpdates :: Core.Maybe (Core.NonEmpty Types.ReplicaAutoScalingUpdate)
    -- ^ Represents the auto scaling settings of replicas of the table that will be modified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTableReplicaAutoScaling' value with any optional fields omitted.
mkUpdateTableReplicaAutoScaling
    :: Types.TableName -- ^ 'tableName'
    -> UpdateTableReplicaAutoScaling
mkUpdateTableReplicaAutoScaling tableName
  = UpdateTableReplicaAutoScaling'{tableName,
                                   globalSecondaryIndexUpdates = Core.Nothing,
                                   provisionedWriteCapacityAutoScalingUpdate = Core.Nothing,
                                   replicaUpdates = Core.Nothing}

-- | The name of the global table to be updated.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasTableName :: Lens.Lens' UpdateTableReplicaAutoScaling Types.TableName
utrasTableName = Lens.field @"tableName"
{-# INLINEABLE utrasTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | Represents the auto scaling settings of the global secondary indexes of the replica to be updated.
--
-- /Note:/ Consider using 'globalSecondaryIndexUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasGlobalSecondaryIndexUpdates :: Lens.Lens' UpdateTableReplicaAutoScaling (Core.Maybe (Core.NonEmpty Types.GlobalSecondaryIndexAutoScalingUpdate))
utrasGlobalSecondaryIndexUpdates = Lens.field @"globalSecondaryIndexUpdates"
{-# INLINEABLE utrasGlobalSecondaryIndexUpdates #-}
{-# DEPRECATED globalSecondaryIndexUpdates "Use generic-lens or generic-optics with 'globalSecondaryIndexUpdates' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'provisionedWriteCapacityAutoScalingUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasProvisionedWriteCapacityAutoScalingUpdate :: Lens.Lens' UpdateTableReplicaAutoScaling (Core.Maybe Types.AutoScalingSettingsUpdate)
utrasProvisionedWriteCapacityAutoScalingUpdate = Lens.field @"provisionedWriteCapacityAutoScalingUpdate"
{-# INLINEABLE utrasProvisionedWriteCapacityAutoScalingUpdate #-}
{-# DEPRECATED provisionedWriteCapacityAutoScalingUpdate "Use generic-lens or generic-optics with 'provisionedWriteCapacityAutoScalingUpdate' instead"  #-}

-- | Represents the auto scaling settings of replicas of the table that will be modified.
--
-- /Note:/ Consider using 'replicaUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasReplicaUpdates :: Lens.Lens' UpdateTableReplicaAutoScaling (Core.Maybe (Core.NonEmpty Types.ReplicaAutoScalingUpdate))
utrasReplicaUpdates = Lens.field @"replicaUpdates"
{-# INLINEABLE utrasReplicaUpdates #-}
{-# DEPRECATED replicaUpdates "Use generic-lens or generic-optics with 'replicaUpdates' instead"  #-}

instance Core.ToQuery UpdateTableReplicaAutoScaling where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTableReplicaAutoScaling where
        toHeaders UpdateTableReplicaAutoScaling{..}
          = Core.pure
              ("X-Amz-Target", "DynamoDB_20120810.UpdateTableReplicaAutoScaling")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON UpdateTableReplicaAutoScaling where
        toJSON UpdateTableReplicaAutoScaling{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TableName" Core..= tableName),
                  ("GlobalSecondaryIndexUpdates" Core..=) Core.<$>
                    globalSecondaryIndexUpdates,
                  ("ProvisionedWriteCapacityAutoScalingUpdate" Core..=) Core.<$>
                    provisionedWriteCapacityAutoScalingUpdate,
                  ("ReplicaUpdates" Core..=) Core.<$> replicaUpdates])

instance Core.AWSRequest UpdateTableReplicaAutoScaling where
        type Rs UpdateTableReplicaAutoScaling =
             UpdateTableReplicaAutoScalingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateTableReplicaAutoScalingResponse' Core.<$>
                   (x Core..:? "TableAutoScalingDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTableReplicaAutoScalingResponse' smart constructor.
data UpdateTableReplicaAutoScalingResponse = UpdateTableReplicaAutoScalingResponse'
  { tableAutoScalingDescription :: Core.Maybe Types.TableAutoScalingDescription
    -- ^ Returns information about the auto scaling settings of a table with replicas.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTableReplicaAutoScalingResponse' value with any optional fields omitted.
mkUpdateTableReplicaAutoScalingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTableReplicaAutoScalingResponse
mkUpdateTableReplicaAutoScalingResponse responseStatus
  = UpdateTableReplicaAutoScalingResponse'{tableAutoScalingDescription
                                             = Core.Nothing,
                                           responseStatus}

-- | Returns information about the auto scaling settings of a table with replicas.
--
-- /Note:/ Consider using 'tableAutoScalingDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasrrsTableAutoScalingDescription :: Lens.Lens' UpdateTableReplicaAutoScalingResponse (Core.Maybe Types.TableAutoScalingDescription)
utrasrrsTableAutoScalingDescription = Lens.field @"tableAutoScalingDescription"
{-# INLINEABLE utrasrrsTableAutoScalingDescription #-}
{-# DEPRECATED tableAutoScalingDescription "Use generic-lens or generic-optics with 'tableAutoScalingDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasrrsResponseStatus :: Lens.Lens' UpdateTableReplicaAutoScalingResponse Core.Int
utrasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utrasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
