{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Redshift security group. You use security groups to control access to non-VPC clusters.
--
-- For information about managing security groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CreateClusterSecurityGroup
  ( -- * Creating a request
    CreateClusterSecurityGroup (..),
    mkCreateClusterSecurityGroup,

    -- ** Request lenses
    ccsgClusterSecurityGroupName,
    ccsgDescription,
    ccsgTags,

    -- * Destructuring the response
    CreateClusterSecurityGroupResponse (..),
    mkCreateClusterSecurityGroupResponse,

    -- ** Response lenses
    crsClusterSecurityGroup,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateClusterSecurityGroup' smart constructor.
data CreateClusterSecurityGroup = CreateClusterSecurityGroup'
  { -- | The name for the security group. Amazon Redshift stores the value as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must contain no more than 255 alphanumeric characters or hyphens.
    --
    --
    --     * Must not be "Default".
    --
    --
    --     * Must be unique for all security groups that are created by your AWS account.
    --
    --
    -- Example: @examplesecuritygroup@
    clusterSecurityGroupName :: Lude.Text,
    -- | A description for the security group.
    description :: Lude.Text,
    -- | A list of tag instances.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClusterSecurityGroup' with the minimum fields required to make a request.
--
-- * 'clusterSecurityGroupName' - The name for the security group. Amazon Redshift stores the value as a lowercase string.
--
-- Constraints:
--
--     * Must contain no more than 255 alphanumeric characters or hyphens.
--
--
--     * Must not be "Default".
--
--
--     * Must be unique for all security groups that are created by your AWS account.
--
--
-- Example: @examplesecuritygroup@
-- * 'description' - A description for the security group.
-- * 'tags' - A list of tag instances.
mkCreateClusterSecurityGroup ::
  -- | 'clusterSecurityGroupName'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  CreateClusterSecurityGroup
mkCreateClusterSecurityGroup
  pClusterSecurityGroupName_
  pDescription_ =
    CreateClusterSecurityGroup'
      { clusterSecurityGroupName =
          pClusterSecurityGroupName_,
        description = pDescription_,
        tags = Lude.Nothing
      }

-- | The name for the security group. Amazon Redshift stores the value as a lowercase string.
--
-- Constraints:
--
--     * Must contain no more than 255 alphanumeric characters or hyphens.
--
--
--     * Must not be "Default".
--
--
--     * Must be unique for all security groups that are created by your AWS account.
--
--
-- Example: @examplesecuritygroup@
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgClusterSecurityGroupName :: Lens.Lens' CreateClusterSecurityGroup Lude.Text
ccsgClusterSecurityGroupName = Lens.lens (clusterSecurityGroupName :: CreateClusterSecurityGroup -> Lude.Text) (\s a -> s {clusterSecurityGroupName = a} :: CreateClusterSecurityGroup)
{-# DEPRECATED ccsgClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

-- | A description for the security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgDescription :: Lens.Lens' CreateClusterSecurityGroup Lude.Text
ccsgDescription = Lens.lens (description :: CreateClusterSecurityGroup -> Lude.Text) (\s a -> s {description = a} :: CreateClusterSecurityGroup)
{-# DEPRECATED ccsgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgTags :: Lens.Lens' CreateClusterSecurityGroup (Lude.Maybe [Tag])
ccsgTags = Lens.lens (tags :: CreateClusterSecurityGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateClusterSecurityGroup)
{-# DEPRECATED ccsgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateClusterSecurityGroup where
  type
    Rs CreateClusterSecurityGroup =
      CreateClusterSecurityGroupResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CreateClusterSecurityGroupResult"
      ( \s h x ->
          CreateClusterSecurityGroupResponse'
            Lude.<$> (x Lude..@? "ClusterSecurityGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateClusterSecurityGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateClusterSecurityGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateClusterSecurityGroup where
  toQuery CreateClusterSecurityGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateClusterSecurityGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterSecurityGroupName" Lude.=: clusterSecurityGroupName,
        "Description" Lude.=: description,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCreateClusterSecurityGroupResponse' smart constructor.
data CreateClusterSecurityGroupResponse = CreateClusterSecurityGroupResponse'
  { clusterSecurityGroup :: Lude.Maybe ClusterSecurityGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClusterSecurityGroupResponse' with the minimum fields required to make a request.
--
-- * 'clusterSecurityGroup' -
-- * 'responseStatus' - The response status code.
mkCreateClusterSecurityGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateClusterSecurityGroupResponse
mkCreateClusterSecurityGroupResponse pResponseStatus_ =
  CreateClusterSecurityGroupResponse'
    { clusterSecurityGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsClusterSecurityGroup :: Lens.Lens' CreateClusterSecurityGroupResponse (Lude.Maybe ClusterSecurityGroup)
crsClusterSecurityGroup = Lens.lens (clusterSecurityGroup :: CreateClusterSecurityGroupResponse -> Lude.Maybe ClusterSecurityGroup) (\s a -> s {clusterSecurityGroup = a} :: CreateClusterSecurityGroupResponse)
{-# DEPRECATED crsClusterSecurityGroup "Use generic-lens or generic-optics with 'clusterSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateClusterSecurityGroupResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateClusterSecurityGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateClusterSecurityGroupResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
