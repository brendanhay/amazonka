{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Redshift parameter group.
--
-- Creating parameter groups is independent of creating clusters. You can associate a cluster with a parameter group when you create the cluster. You can also associate an existing cluster with a parameter group after the cluster is created by using 'ModifyCluster' .
-- Parameters in the parameter group define specific behavior that applies to the databases you create on the cluster. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CreateClusterParameterGroup
  ( -- * Creating a request
    CreateClusterParameterGroup (..),
    mkCreateClusterParameterGroup,

    -- ** Request lenses
    ccpgParameterGroupName,
    ccpgParameterGroupFamily,
    ccpgDescription,
    ccpgTags,

    -- * Destructuring the response
    CreateClusterParameterGroupResponse (..),
    mkCreateClusterParameterGroupResponse,

    -- ** Response lenses
    ccpgrrsClusterParameterGroup,
    ccpgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateClusterParameterGroup' smart constructor.
data CreateClusterParameterGroup = CreateClusterParameterGroup'
  { -- | The name of the cluster parameter group.
    --
    -- Constraints:
    --
    --     * Must be 1 to 255 alphanumeric characters or hyphens
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Cannot end with a hyphen or contain two consecutive hyphens.
    --
    --
    --     * Must be unique withing your AWS account.
    parameterGroupName :: Types.String,
    -- | The Amazon Redshift engine version to which the cluster parameter group applies. The cluster engine version determines the set of parameters.
    --
    -- To get a list of valid parameter group family names, you can call 'DescribeClusterParameterGroups' . By default, Amazon Redshift returns a list of all the parameter groups that are owned by your AWS account, including the default parameter groups for each Amazon Redshift engine version. The parameter group family names associated with the default parameter groups provide you the valid values. For example, a valid family name is "redshift-1.0".
    parameterGroupFamily :: Types.String,
    -- | A description of the parameter group.
    description :: Types.String,
    -- | A list of tag instances.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterParameterGroup' value with any optional fields omitted.
mkCreateClusterParameterGroup ::
  -- | 'parameterGroupName'
  Types.String ->
  -- | 'parameterGroupFamily'
  Types.String ->
  -- | 'description'
  Types.String ->
  CreateClusterParameterGroup
mkCreateClusterParameterGroup
  parameterGroupName
  parameterGroupFamily
  description =
    CreateClusterParameterGroup'
      { parameterGroupName,
        parameterGroupFamily,
        description,
        tags = Core.Nothing
      }

-- | The name of the cluster parameter group.
--
-- Constraints:
--
--     * Must be 1 to 255 alphanumeric characters or hyphens
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique withing your AWS account.
--
--
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgParameterGroupName :: Lens.Lens' CreateClusterParameterGroup Types.String
ccpgParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED ccpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | The Amazon Redshift engine version to which the cluster parameter group applies. The cluster engine version determines the set of parameters.
--
-- To get a list of valid parameter group family names, you can call 'DescribeClusterParameterGroups' . By default, Amazon Redshift returns a list of all the parameter groups that are owned by your AWS account, including the default parameter groups for each Amazon Redshift engine version. The parameter group family names associated with the default parameter groups provide you the valid values. For example, a valid family name is "redshift-1.0".
--
-- /Note:/ Consider using 'parameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgParameterGroupFamily :: Lens.Lens' CreateClusterParameterGroup Types.String
ccpgParameterGroupFamily = Lens.field @"parameterGroupFamily"
{-# DEPRECATED ccpgParameterGroupFamily "Use generic-lens or generic-optics with 'parameterGroupFamily' instead." #-}

-- | A description of the parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgDescription :: Lens.Lens' CreateClusterParameterGroup Types.String
ccpgDescription = Lens.field @"description"
{-# DEPRECATED ccpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgTags :: Lens.Lens' CreateClusterParameterGroup (Core.Maybe [Types.Tag])
ccpgTags = Lens.field @"tags"
{-# DEPRECATED ccpgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateClusterParameterGroup where
  type
    Rs CreateClusterParameterGroup =
      CreateClusterParameterGroupResponse
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
            ( Core.pure ("Action", "CreateClusterParameterGroup")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ParameterGroupName" parameterGroupName)
                Core.<> (Core.toQueryValue "ParameterGroupFamily" parameterGroupFamily)
                Core.<> (Core.toQueryValue "Description" description)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateClusterParameterGroupResult"
      ( \s h x ->
          CreateClusterParameterGroupResponse'
            Core.<$> (x Core..@? "ClusterParameterGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateClusterParameterGroupResponse' smart constructor.
data CreateClusterParameterGroupResponse = CreateClusterParameterGroupResponse'
  { clusterParameterGroup :: Core.Maybe Types.ClusterParameterGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterParameterGroupResponse' value with any optional fields omitted.
mkCreateClusterParameterGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateClusterParameterGroupResponse
mkCreateClusterParameterGroupResponse responseStatus =
  CreateClusterParameterGroupResponse'
    { clusterParameterGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgrrsClusterParameterGroup :: Lens.Lens' CreateClusterParameterGroupResponse (Core.Maybe Types.ClusterParameterGroup)
ccpgrrsClusterParameterGroup = Lens.field @"clusterParameterGroup"
{-# DEPRECATED ccpgrrsClusterParameterGroup "Use generic-lens or generic-optics with 'clusterParameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgrrsResponseStatus :: Lens.Lens' CreateClusterParameterGroupResponse Core.Int
ccpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
