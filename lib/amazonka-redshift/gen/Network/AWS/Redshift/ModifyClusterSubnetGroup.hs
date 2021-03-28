{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a cluster subnet group to include the specified list of VPC subnets. The operation replaces the existing list of subnets with the new list of subnets.
module Network.AWS.Redshift.ModifyClusterSubnetGroup
    (
    -- * Creating a request
      ModifyClusterSubnetGroup (..)
    , mkModifyClusterSubnetGroup
    -- ** Request lenses
    , mcsgClusterSubnetGroupName
    , mcsgSubnetIds
    , mcsgDescription

    -- * Destructuring the response
    , ModifyClusterSubnetGroupResponse (..)
    , mkModifyClusterSubnetGroupResponse
    -- ** Response lenses
    , mcsgrrsClusterSubnetGroup
    , mcsgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyClusterSubnetGroup' smart constructor.
data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup'
  { clusterSubnetGroupName :: Core.Text
    -- ^ The name of the subnet group to be modified.
  , subnetIds :: [Core.Text]
    -- ^ An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
  , description :: Core.Maybe Core.Text
    -- ^ A text description of the subnet group to be modified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterSubnetGroup' value with any optional fields omitted.
mkModifyClusterSubnetGroup
    :: Core.Text -- ^ 'clusterSubnetGroupName'
    -> ModifyClusterSubnetGroup
mkModifyClusterSubnetGroup clusterSubnetGroupName
  = ModifyClusterSubnetGroup'{clusterSubnetGroupName,
                              subnetIds = Core.mempty, description = Core.Nothing}

-- | The name of the subnet group to be modified.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgClusterSubnetGroupName :: Lens.Lens' ModifyClusterSubnetGroup Core.Text
mcsgClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# INLINEABLE mcsgClusterSubnetGroupName #-}
{-# DEPRECATED clusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead"  #-}

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgSubnetIds :: Lens.Lens' ModifyClusterSubnetGroup [Core.Text]
mcsgSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE mcsgSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | A text description of the subnet group to be modified.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgDescription :: Lens.Lens' ModifyClusterSubnetGroup (Core.Maybe Core.Text)
mcsgDescription = Lens.field @"description"
{-# INLINEABLE mcsgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery ModifyClusterSubnetGroup where
        toQuery ModifyClusterSubnetGroup{..}
          = Core.toQueryPair "Action"
              ("ModifyClusterSubnetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ClusterSubnetGroupName" clusterSubnetGroupName
              Core.<>
              Core.toQueryPair "SubnetIds"
                (Core.toQueryList "SubnetIdentifier" subnetIds)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description

instance Core.ToHeaders ModifyClusterSubnetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyClusterSubnetGroup where
        type Rs ModifyClusterSubnetGroup = ModifyClusterSubnetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyClusterSubnetGroupResult"
              (\ s h x ->
                 ModifyClusterSubnetGroupResponse' Core.<$>
                   (x Core..@? "ClusterSubnetGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyClusterSubnetGroupResponse' smart constructor.
data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse'
  { clusterSubnetGroup :: Core.Maybe Types.ClusterSubnetGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterSubnetGroupResponse' value with any optional fields omitted.
mkModifyClusterSubnetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyClusterSubnetGroupResponse
mkModifyClusterSubnetGroupResponse responseStatus
  = ModifyClusterSubnetGroupResponse'{clusterSubnetGroup =
                                        Core.Nothing,
                                      responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrrsClusterSubnetGroup :: Lens.Lens' ModifyClusterSubnetGroupResponse (Core.Maybe Types.ClusterSubnetGroup)
mcsgrrsClusterSubnetGroup = Lens.field @"clusterSubnetGroup"
{-# INLINEABLE mcsgrrsClusterSubnetGroup #-}
{-# DEPRECATED clusterSubnetGroup "Use generic-lens or generic-optics with 'clusterSubnetGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrrsResponseStatus :: Lens.Lens' ModifyClusterSubnetGroupResponse Core.Int
mcsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
