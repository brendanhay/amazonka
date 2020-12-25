{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Redshift subnet group. You must provide a list of one or more subnets in your existing Amazon Virtual Private Cloud (Amazon VPC) when creating Amazon Redshift subnet group.
--
-- For information about subnet groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-cluster-subnet-groups.html Amazon Redshift Cluster Subnet Groups> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CreateClusterSubnetGroup
  ( -- * Creating a request
    CreateClusterSubnetGroup (..),
    mkCreateClusterSubnetGroup,

    -- ** Request lenses
    ccsgClusterSubnetGroupName,
    ccsgDescription,
    ccsgSubnetIds,
    ccsgTags,

    -- * Destructuring the response
    CreateClusterSubnetGroupResponse (..),
    mkCreateClusterSubnetGroupResponse,

    -- ** Response lenses
    ccsgrrsClusterSubnetGroup,
    ccsgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateClusterSubnetGroup' smart constructor.
data CreateClusterSubnetGroup = CreateClusterSubnetGroup'
  { -- | The name for the subnet group. Amazon Redshift stores the value as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must contain no more than 255 alphanumeric characters or hyphens.
    --
    --
    --     * Must not be "Default".
    --
    --
    --     * Must be unique for all subnet groups that are created by your AWS account.
    --
    --
    -- Example: @examplesubnetgroup@
    clusterSubnetGroupName :: Types.String,
    -- | A description for the subnet group.
    description :: Types.String,
    -- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
    subnetIds :: [Types.String],
    -- | A list of tag instances.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterSubnetGroup' value with any optional fields omitted.
mkCreateClusterSubnetGroup ::
  -- | 'clusterSubnetGroupName'
  Types.String ->
  -- | 'description'
  Types.String ->
  CreateClusterSubnetGroup
mkCreateClusterSubnetGroup clusterSubnetGroupName description =
  CreateClusterSubnetGroup'
    { clusterSubnetGroupName,
      description,
      subnetIds = Core.mempty,
      tags = Core.Nothing
    }

-- | The name for the subnet group. Amazon Redshift stores the value as a lowercase string.
--
-- Constraints:
--
--     * Must contain no more than 255 alphanumeric characters or hyphens.
--
--
--     * Must not be "Default".
--
--
--     * Must be unique for all subnet groups that are created by your AWS account.
--
--
-- Example: @examplesubnetgroup@
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgClusterSubnetGroupName :: Lens.Lens' CreateClusterSubnetGroup Types.String
ccsgClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# DEPRECATED ccsgClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

-- | A description for the subnet group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgDescription :: Lens.Lens' CreateClusterSubnetGroup Types.String
ccsgDescription = Lens.field @"description"
{-# DEPRECATED ccsgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgSubnetIds :: Lens.Lens' CreateClusterSubnetGroup [Types.String]
ccsgSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED ccsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgTags :: Lens.Lens' CreateClusterSubnetGroup (Core.Maybe [Types.Tag])
ccsgTags = Lens.field @"tags"
{-# DEPRECATED ccsgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateClusterSubnetGroup where
  type Rs CreateClusterSubnetGroup = CreateClusterSubnetGroupResponse
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
            ( Core.pure ("Action", "CreateClusterSubnetGroup")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterSubnetGroupName" clusterSubnetGroupName)
                Core.<> (Core.toQueryValue "Description" description)
                Core.<> ( Core.toQueryValue
                            "SubnetIds"
                            (Core.toQueryList "SubnetIdentifier" subnetIds)
                        )
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateClusterSubnetGroupResult"
      ( \s h x ->
          CreateClusterSubnetGroupResponse'
            Core.<$> (x Core..@? "ClusterSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateClusterSubnetGroupResponse' smart constructor.
data CreateClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse'
  { clusterSubnetGroup :: Core.Maybe Types.ClusterSubnetGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterSubnetGroupResponse' value with any optional fields omitted.
mkCreateClusterSubnetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateClusterSubnetGroupResponse
mkCreateClusterSubnetGroupResponse responseStatus =
  CreateClusterSubnetGroupResponse'
    { clusterSubnetGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgrrsClusterSubnetGroup :: Lens.Lens' CreateClusterSubnetGroupResponse (Core.Maybe Types.ClusterSubnetGroup)
ccsgrrsClusterSubnetGroup = Lens.field @"clusterSubnetGroup"
{-# DEPRECATED ccsgrrsClusterSubnetGroup "Use generic-lens or generic-optics with 'clusterSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgrrsResponseStatus :: Lens.Lens' CreateClusterSubnetGroupResponse Core.Int
ccsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
