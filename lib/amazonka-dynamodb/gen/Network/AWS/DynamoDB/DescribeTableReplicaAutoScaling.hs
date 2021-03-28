{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeTableReplicaAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes auto scaling settings across replicas of the global table at once.
module Network.AWS.DynamoDB.DescribeTableReplicaAutoScaling
    (
    -- * Creating a request
      DescribeTableReplicaAutoScaling (..)
    , mkDescribeTableReplicaAutoScaling
    -- ** Request lenses
    , dtrasTableName

    -- * Destructuring the response
    , DescribeTableReplicaAutoScalingResponse (..)
    , mkDescribeTableReplicaAutoScalingResponse
    -- ** Response lenses
    , dtrasrrsTableAutoScalingDescription
    , dtrasrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTableReplicaAutoScaling' smart constructor.
newtype DescribeTableReplicaAutoScaling = DescribeTableReplicaAutoScaling'
  { tableName :: Types.TableName
    -- ^ The name of the table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTableReplicaAutoScaling' value with any optional fields omitted.
mkDescribeTableReplicaAutoScaling
    :: Types.TableName -- ^ 'tableName'
    -> DescribeTableReplicaAutoScaling
mkDescribeTableReplicaAutoScaling tableName
  = DescribeTableReplicaAutoScaling'{tableName}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrasTableName :: Lens.Lens' DescribeTableReplicaAutoScaling Types.TableName
dtrasTableName = Lens.field @"tableName"
{-# INLINEABLE dtrasTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

instance Core.ToQuery DescribeTableReplicaAutoScaling where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTableReplicaAutoScaling where
        toHeaders DescribeTableReplicaAutoScaling{..}
          = Core.pure
              ("X-Amz-Target",
               "DynamoDB_20120810.DescribeTableReplicaAutoScaling")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DescribeTableReplicaAutoScaling where
        toJSON DescribeTableReplicaAutoScaling{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TableName" Core..= tableName)])

instance Core.AWSRequest DescribeTableReplicaAutoScaling where
        type Rs DescribeTableReplicaAutoScaling =
             DescribeTableReplicaAutoScalingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTableReplicaAutoScalingResponse' Core.<$>
                   (x Core..:? "TableAutoScalingDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeTableReplicaAutoScalingResponse' smart constructor.
data DescribeTableReplicaAutoScalingResponse = DescribeTableReplicaAutoScalingResponse'
  { tableAutoScalingDescription :: Core.Maybe Types.TableAutoScalingDescription
    -- ^ Represents the auto scaling properties of the table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTableReplicaAutoScalingResponse' value with any optional fields omitted.
mkDescribeTableReplicaAutoScalingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTableReplicaAutoScalingResponse
mkDescribeTableReplicaAutoScalingResponse responseStatus
  = DescribeTableReplicaAutoScalingResponse'{tableAutoScalingDescription
                                               = Core.Nothing,
                                             responseStatus}

-- | Represents the auto scaling properties of the table.
--
-- /Note:/ Consider using 'tableAutoScalingDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrasrrsTableAutoScalingDescription :: Lens.Lens' DescribeTableReplicaAutoScalingResponse (Core.Maybe Types.TableAutoScalingDescription)
dtrasrrsTableAutoScalingDescription = Lens.field @"tableAutoScalingDescription"
{-# INLINEABLE dtrasrrsTableAutoScalingDescription #-}
{-# DEPRECATED tableAutoScalingDescription "Use generic-lens or generic-optics with 'tableAutoScalingDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrasrrsResponseStatus :: Lens.Lens' DescribeTableReplicaAutoScalingResponse Core.Int
dtrasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
