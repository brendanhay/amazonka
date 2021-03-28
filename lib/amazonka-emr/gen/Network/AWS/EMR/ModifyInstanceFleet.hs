{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ModifyInstanceFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the target On-Demand and target Spot capacities for the instance fleet with the specified InstanceFleetID within the cluster specified using ClusterID. The call either succeeds or fails atomically.
module Network.AWS.EMR.ModifyInstanceFleet
    (
    -- * Creating a request
      ModifyInstanceFleet (..)
    , mkModifyInstanceFleet
    -- ** Request lenses
    , mifClusterId
    , mifInstanceFleet

    -- * Destructuring the response
    , ModifyInstanceFleetResponse (..)
    , mkModifyInstanceFleetResponse
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyInstanceFleet' smart constructor.
data ModifyInstanceFleet = ModifyInstanceFleet'
  { clusterId :: Types.ClusterId
    -- ^ The unique identifier of the cluster.
  , instanceFleet :: Types.InstanceFleetModifyConfig
    -- ^ The unique identifier of the instance fleet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceFleet' value with any optional fields omitted.
mkModifyInstanceFleet
    :: Types.ClusterId -- ^ 'clusterId'
    -> Types.InstanceFleetModifyConfig -- ^ 'instanceFleet'
    -> ModifyInstanceFleet
mkModifyInstanceFleet clusterId instanceFleet
  = ModifyInstanceFleet'{clusterId, instanceFleet}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mifClusterId :: Lens.Lens' ModifyInstanceFleet Types.ClusterId
mifClusterId = Lens.field @"clusterId"
{-# INLINEABLE mifClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The unique identifier of the instance fleet.
--
-- /Note:/ Consider using 'instanceFleet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mifInstanceFleet :: Lens.Lens' ModifyInstanceFleet Types.InstanceFleetModifyConfig
mifInstanceFleet = Lens.field @"instanceFleet"
{-# INLINEABLE mifInstanceFleet #-}
{-# DEPRECATED instanceFleet "Use generic-lens or generic-optics with 'instanceFleet' instead"  #-}

instance Core.ToQuery ModifyInstanceFleet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyInstanceFleet where
        toHeaders ModifyInstanceFleet{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.ModifyInstanceFleet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyInstanceFleet where
        toJSON ModifyInstanceFleet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterId" Core..= clusterId),
                  Core.Just ("InstanceFleet" Core..= instanceFleet)])

instance Core.AWSRequest ModifyInstanceFleet where
        type Rs ModifyInstanceFleet = ModifyInstanceFleetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull ModifyInstanceFleetResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyInstanceFleetResponse' smart constructor.
data ModifyInstanceFleetResponse = ModifyInstanceFleetResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceFleetResponse' value with any optional fields omitted.
mkModifyInstanceFleetResponse
    :: ModifyInstanceFleetResponse
mkModifyInstanceFleetResponse = ModifyInstanceFleetResponse'
