{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ModifyInstanceGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- ModifyInstanceGroups modifies the number of nodes and configuration settings of an instance group. The input parameters include the new target instance count for the group and the instance group ID. The call will either succeed or fail atomically.
module Network.AWS.EMR.ModifyInstanceGroups
    (
    -- * Creating a request
      ModifyInstanceGroups (..)
    , mkModifyInstanceGroups
    -- ** Request lenses
    , migClusterId
    , migInstanceGroups

    -- * Destructuring the response
    , ModifyInstanceGroupsResponse (..)
    , mkModifyInstanceGroupsResponse
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Change the size of some instance groups.
--
-- /See:/ 'mkModifyInstanceGroups' smart constructor.
data ModifyInstanceGroups = ModifyInstanceGroups'
  { clusterId :: Core.Maybe Types.ClusterId
    -- ^ The ID of the cluster to which the instance group belongs.
  , instanceGroups :: Core.Maybe [Types.InstanceGroupModifyConfig]
    -- ^ Instance groups to change.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceGroups' value with any optional fields omitted.
mkModifyInstanceGroups
    :: ModifyInstanceGroups
mkModifyInstanceGroups
  = ModifyInstanceGroups'{clusterId = Core.Nothing,
                          instanceGroups = Core.Nothing}

-- | The ID of the cluster to which the instance group belongs.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
migClusterId :: Lens.Lens' ModifyInstanceGroups (Core.Maybe Types.ClusterId)
migClusterId = Lens.field @"clusterId"
{-# INLINEABLE migClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | Instance groups to change.
--
-- /Note:/ Consider using 'instanceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
migInstanceGroups :: Lens.Lens' ModifyInstanceGroups (Core.Maybe [Types.InstanceGroupModifyConfig])
migInstanceGroups = Lens.field @"instanceGroups"
{-# INLINEABLE migInstanceGroups #-}
{-# DEPRECATED instanceGroups "Use generic-lens or generic-optics with 'instanceGroups' instead"  #-}

instance Core.ToQuery ModifyInstanceGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyInstanceGroups where
        toHeaders ModifyInstanceGroups{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.ModifyInstanceGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyInstanceGroups where
        toJSON ModifyInstanceGroups{..}
          = Core.object
              (Core.catMaybes
                 [("ClusterId" Core..=) Core.<$> clusterId,
                  ("InstanceGroups" Core..=) Core.<$> instanceGroups])

instance Core.AWSRequest ModifyInstanceGroups where
        type Rs ModifyInstanceGroups = ModifyInstanceGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull ModifyInstanceGroupsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyInstanceGroupsResponse' smart constructor.
data ModifyInstanceGroupsResponse = ModifyInstanceGroupsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceGroupsResponse' value with any optional fields omitted.
mkModifyInstanceGroupsResponse
    :: ModifyInstanceGroupsResponse
mkModifyInstanceGroupsResponse = ModifyInstanceGroupsResponse'
