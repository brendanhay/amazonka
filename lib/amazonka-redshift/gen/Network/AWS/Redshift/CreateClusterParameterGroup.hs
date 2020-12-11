{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ccpgTags,
    ccpgParameterGroupName,
    ccpgParameterGroupFamily,
    ccpgDescription,

    -- * Destructuring the response
    CreateClusterParameterGroupResponse (..),
    mkCreateClusterParameterGroupResponse,

    -- ** Response lenses
    ccpgrsClusterParameterGroup,
    ccpgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateClusterParameterGroup' smart constructor.
data CreateClusterParameterGroup = CreateClusterParameterGroup'
  { tags ::
      Lude.Maybe [Tag],
    parameterGroupName :: Lude.Text,
    parameterGroupFamily :: Lude.Text,
    description :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClusterParameterGroup' with the minimum fields required to make a request.
--
-- * 'description' - A description of the parameter group.
-- * 'parameterGroupFamily' - The Amazon Redshift engine version to which the cluster parameter group applies. The cluster engine version determines the set of parameters.
--
-- To get a list of valid parameter group family names, you can call 'DescribeClusterParameterGroups' . By default, Amazon Redshift returns a list of all the parameter groups that are owned by your AWS account, including the default parameter groups for each Amazon Redshift engine version. The parameter group family names associated with the default parameter groups provide you the valid values. For example, a valid family name is "redshift-1.0".
-- * 'parameterGroupName' - The name of the cluster parameter group.
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
-- * 'tags' - A list of tag instances.
mkCreateClusterParameterGroup ::
  -- | 'parameterGroupName'
  Lude.Text ->
  -- | 'parameterGroupFamily'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  CreateClusterParameterGroup
mkCreateClusterParameterGroup
  pParameterGroupName_
  pParameterGroupFamily_
  pDescription_ =
    CreateClusterParameterGroup'
      { tags = Lude.Nothing,
        parameterGroupName = pParameterGroupName_,
        parameterGroupFamily = pParameterGroupFamily_,
        description = pDescription_
      }

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgTags :: Lens.Lens' CreateClusterParameterGroup (Lude.Maybe [Tag])
ccpgTags = Lens.lens (tags :: CreateClusterParameterGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateClusterParameterGroup)
{-# DEPRECATED ccpgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
ccpgParameterGroupName :: Lens.Lens' CreateClusterParameterGroup Lude.Text
ccpgParameterGroupName = Lens.lens (parameterGroupName :: CreateClusterParameterGroup -> Lude.Text) (\s a -> s {parameterGroupName = a} :: CreateClusterParameterGroup)
{-# DEPRECATED ccpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | The Amazon Redshift engine version to which the cluster parameter group applies. The cluster engine version determines the set of parameters.
--
-- To get a list of valid parameter group family names, you can call 'DescribeClusterParameterGroups' . By default, Amazon Redshift returns a list of all the parameter groups that are owned by your AWS account, including the default parameter groups for each Amazon Redshift engine version. The parameter group family names associated with the default parameter groups provide you the valid values. For example, a valid family name is "redshift-1.0".
--
-- /Note:/ Consider using 'parameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgParameterGroupFamily :: Lens.Lens' CreateClusterParameterGroup Lude.Text
ccpgParameterGroupFamily = Lens.lens (parameterGroupFamily :: CreateClusterParameterGroup -> Lude.Text) (\s a -> s {parameterGroupFamily = a} :: CreateClusterParameterGroup)
{-# DEPRECATED ccpgParameterGroupFamily "Use generic-lens or generic-optics with 'parameterGroupFamily' instead." #-}

-- | A description of the parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgDescription :: Lens.Lens' CreateClusterParameterGroup Lude.Text
ccpgDescription = Lens.lens (description :: CreateClusterParameterGroup -> Lude.Text) (\s a -> s {description = a} :: CreateClusterParameterGroup)
{-# DEPRECATED ccpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateClusterParameterGroup where
  type
    Rs CreateClusterParameterGroup =
      CreateClusterParameterGroupResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CreateClusterParameterGroupResult"
      ( \s h x ->
          CreateClusterParameterGroupResponse'
            Lude.<$> (x Lude..@? "ClusterParameterGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateClusterParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateClusterParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateClusterParameterGroup where
  toQuery CreateClusterParameterGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateClusterParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "ParameterGroupName" Lude.=: parameterGroupName,
        "ParameterGroupFamily" Lude.=: parameterGroupFamily,
        "Description" Lude.=: description
      ]

-- | /See:/ 'mkCreateClusterParameterGroupResponse' smart constructor.
data CreateClusterParameterGroupResponse = CreateClusterParameterGroupResponse'
  { clusterParameterGroup ::
      Lude.Maybe
        ClusterParameterGroup,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClusterParameterGroupResponse' with the minimum fields required to make a request.
--
-- * 'clusterParameterGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateClusterParameterGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateClusterParameterGroupResponse
mkCreateClusterParameterGroupResponse pResponseStatus_ =
  CreateClusterParameterGroupResponse'
    { clusterParameterGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgrsClusterParameterGroup :: Lens.Lens' CreateClusterParameterGroupResponse (Lude.Maybe ClusterParameterGroup)
ccpgrsClusterParameterGroup = Lens.lens (clusterParameterGroup :: CreateClusterParameterGroupResponse -> Lude.Maybe ClusterParameterGroup) (\s a -> s {clusterParameterGroup = a} :: CreateClusterParameterGroupResponse)
{-# DEPRECATED ccpgrsClusterParameterGroup "Use generic-lens or generic-optics with 'clusterParameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgrsResponseStatus :: Lens.Lens' CreateClusterParameterGroupResponse Lude.Int
ccpgrsResponseStatus = Lens.lens (responseStatus :: CreateClusterParameterGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateClusterParameterGroupResponse)
{-# DEPRECATED ccpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
