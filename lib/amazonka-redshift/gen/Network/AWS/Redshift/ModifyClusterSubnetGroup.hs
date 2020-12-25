{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyClusterSubnetGroup (..),
    mkModifyClusterSubnetGroup,

    -- ** Request lenses
    mcsgClusterSubnetGroupName,
    mcsgSubnetIds,
    mcsgDescription,

    -- * Destructuring the response
    ModifyClusterSubnetGroupResponse (..),
    mkModifyClusterSubnetGroupResponse,

    -- ** Response lenses
    mcsgrrsClusterSubnetGroup,
    mcsgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkModifyClusterSubnetGroup' smart constructor.
data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup'
  { -- | The name of the subnet group to be modified.
    clusterSubnetGroupName :: Types.String,
    -- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
    subnetIds :: [Types.String],
    -- | A text description of the subnet group to be modified.
    description :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterSubnetGroup' value with any optional fields omitted.
mkModifyClusterSubnetGroup ::
  -- | 'clusterSubnetGroupName'
  Types.String ->
  ModifyClusterSubnetGroup
mkModifyClusterSubnetGroup clusterSubnetGroupName =
  ModifyClusterSubnetGroup'
    { clusterSubnetGroupName,
      subnetIds = Core.mempty,
      description = Core.Nothing
    }

-- | The name of the subnet group to be modified.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgClusterSubnetGroupName :: Lens.Lens' ModifyClusterSubnetGroup Types.String
mcsgClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# DEPRECATED mcsgClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgSubnetIds :: Lens.Lens' ModifyClusterSubnetGroup [Types.String]
mcsgSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED mcsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | A text description of the subnet group to be modified.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgDescription :: Lens.Lens' ModifyClusterSubnetGroup (Core.Maybe Types.String)
mcsgDescription = Lens.field @"description"
{-# DEPRECATED mcsgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.AWSRequest ModifyClusterSubnetGroup where
  type Rs ModifyClusterSubnetGroup = ModifyClusterSubnetGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyClusterSubnetGroup")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterSubnetGroupName" clusterSubnetGroupName)
                Core.<> ( Core.toQueryValue
                            "SubnetIds"
                            (Core.toQueryList "SubnetIdentifier" subnetIds)
                        )
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyClusterSubnetGroupResult"
      ( \s h x ->
          ModifyClusterSubnetGroupResponse'
            Core.<$> (x Core..@? "ClusterSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyClusterSubnetGroupResponse' smart constructor.
data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse'
  { clusterSubnetGroup :: Core.Maybe Types.ClusterSubnetGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterSubnetGroupResponse' value with any optional fields omitted.
mkModifyClusterSubnetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyClusterSubnetGroupResponse
mkModifyClusterSubnetGroupResponse responseStatus =
  ModifyClusterSubnetGroupResponse'
    { clusterSubnetGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrrsClusterSubnetGroup :: Lens.Lens' ModifyClusterSubnetGroupResponse (Core.Maybe Types.ClusterSubnetGroup)
mcsgrrsClusterSubnetGroup = Lens.field @"clusterSubnetGroup"
{-# DEPRECATED mcsgrrsClusterSubnetGroup "Use generic-lens or generic-optics with 'clusterSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrrsResponseStatus :: Lens.Lens' ModifyClusterSubnetGroupResponse Core.Int
mcsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mcsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
