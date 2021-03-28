{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.AddInstanceGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more instance groups to a running cluster.
module Network.AWS.EMR.AddInstanceGroups
    (
    -- * Creating a request
      AddInstanceGroups (..)
    , mkAddInstanceGroups
    -- ** Request lenses
    , aigInstanceGroups
    , aigJobFlowId

    -- * Destructuring the response
    , AddInstanceGroupsResponse (..)
    , mkAddInstanceGroupsResponse
    -- ** Response lenses
    , aigrrsClusterArn
    , aigrrsInstanceGroupIds
    , aigrrsJobFlowId
    , aigrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to an AddInstanceGroups call.
--
-- /See:/ 'mkAddInstanceGroups' smart constructor.
data AddInstanceGroups = AddInstanceGroups'
  { instanceGroups :: [Types.InstanceGroupConfig]
    -- ^ Instance groups to add.
  , jobFlowId :: Types.JobFlowId
    -- ^ Job flow in which to add the instance groups.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddInstanceGroups' value with any optional fields omitted.
mkAddInstanceGroups
    :: Types.JobFlowId -- ^ 'jobFlowId'
    -> AddInstanceGroups
mkAddInstanceGroups jobFlowId
  = AddInstanceGroups'{instanceGroups = Core.mempty, jobFlowId}

-- | Instance groups to add.
--
-- /Note:/ Consider using 'instanceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigInstanceGroups :: Lens.Lens' AddInstanceGroups [Types.InstanceGroupConfig]
aigInstanceGroups = Lens.field @"instanceGroups"
{-# INLINEABLE aigInstanceGroups #-}
{-# DEPRECATED instanceGroups "Use generic-lens or generic-optics with 'instanceGroups' instead"  #-}

-- | Job flow in which to add the instance groups.
--
-- /Note:/ Consider using 'jobFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigJobFlowId :: Lens.Lens' AddInstanceGroups Types.JobFlowId
aigJobFlowId = Lens.field @"jobFlowId"
{-# INLINEABLE aigJobFlowId #-}
{-# DEPRECATED jobFlowId "Use generic-lens or generic-optics with 'jobFlowId' instead"  #-}

instance Core.ToQuery AddInstanceGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddInstanceGroups where
        toHeaders AddInstanceGroups{..}
          = Core.pure ("X-Amz-Target", "ElasticMapReduce.AddInstanceGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddInstanceGroups where
        toJSON AddInstanceGroups{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceGroups" Core..= instanceGroups),
                  Core.Just ("JobFlowId" Core..= jobFlowId)])

instance Core.AWSRequest AddInstanceGroups where
        type Rs AddInstanceGroups = AddInstanceGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AddInstanceGroupsResponse' Core.<$>
                   (x Core..:? "ClusterArn") Core.<*> x Core..:? "InstanceGroupIds"
                     Core.<*> x Core..:? "JobFlowId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Output from an AddInstanceGroups call.
--
-- /See:/ 'mkAddInstanceGroupsResponse' smart constructor.
data AddInstanceGroupsResponse = AddInstanceGroupsResponse'
  { clusterArn :: Core.Maybe Types.ClusterArn
    -- ^ The Amazon Resource Name of the cluster.
  , instanceGroupIds :: Core.Maybe [Types.XmlStringMaxLen256]
    -- ^ Instance group IDs of the newly created instance groups.
  , jobFlowId :: Core.Maybe Types.JobFlowId
    -- ^ The job flow ID in which the instance groups are added.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddInstanceGroupsResponse' value with any optional fields omitted.
mkAddInstanceGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddInstanceGroupsResponse
mkAddInstanceGroupsResponse responseStatus
  = AddInstanceGroupsResponse'{clusterArn = Core.Nothing,
                               instanceGroupIds = Core.Nothing, jobFlowId = Core.Nothing,
                               responseStatus}

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigrrsClusterArn :: Lens.Lens' AddInstanceGroupsResponse (Core.Maybe Types.ClusterArn)
aigrrsClusterArn = Lens.field @"clusterArn"
{-# INLINEABLE aigrrsClusterArn #-}
{-# DEPRECATED clusterArn "Use generic-lens or generic-optics with 'clusterArn' instead"  #-}

-- | Instance group IDs of the newly created instance groups.
--
-- /Note:/ Consider using 'instanceGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigrrsInstanceGroupIds :: Lens.Lens' AddInstanceGroupsResponse (Core.Maybe [Types.XmlStringMaxLen256])
aigrrsInstanceGroupIds = Lens.field @"instanceGroupIds"
{-# INLINEABLE aigrrsInstanceGroupIds #-}
{-# DEPRECATED instanceGroupIds "Use generic-lens or generic-optics with 'instanceGroupIds' instead"  #-}

-- | The job flow ID in which the instance groups are added.
--
-- /Note:/ Consider using 'jobFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigrrsJobFlowId :: Lens.Lens' AddInstanceGroupsResponse (Core.Maybe Types.JobFlowId)
aigrrsJobFlowId = Lens.field @"jobFlowId"
{-# INLINEABLE aigrrsJobFlowId #-}
{-# DEPRECATED jobFlowId "Use generic-lens or generic-optics with 'jobFlowId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigrrsResponseStatus :: Lens.Lens' AddInstanceGroupsResponse Core.Int
aigrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aigrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
