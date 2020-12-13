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
    mcsgSubnetIds,
    mcsgClusterSubnetGroupName,
    mcsgDescription,

    -- * Destructuring the response
    ModifyClusterSubnetGroupResponse (..),
    mkModifyClusterSubnetGroupResponse,

    -- ** Response lenses
    mcsgrsClusterSubnetGroup,
    mcsgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyClusterSubnetGroup' smart constructor.
data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup'
  { -- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
    subnetIds :: [Lude.Text],
    -- | The name of the subnet group to be modified.
    clusterSubnetGroupName :: Lude.Text,
    -- | A text description of the subnet group to be modified.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterSubnetGroup' with the minimum fields required to make a request.
--
-- * 'subnetIds' - An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
-- * 'clusterSubnetGroupName' - The name of the subnet group to be modified.
-- * 'description' - A text description of the subnet group to be modified.
mkModifyClusterSubnetGroup ::
  -- | 'clusterSubnetGroupName'
  Lude.Text ->
  ModifyClusterSubnetGroup
mkModifyClusterSubnetGroup pClusterSubnetGroupName_ =
  ModifyClusterSubnetGroup'
    { subnetIds = Lude.mempty,
      clusterSubnetGroupName = pClusterSubnetGroupName_,
      description = Lude.Nothing
    }

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgSubnetIds :: Lens.Lens' ModifyClusterSubnetGroup [Lude.Text]
mcsgSubnetIds = Lens.lens (subnetIds :: ModifyClusterSubnetGroup -> [Lude.Text]) (\s a -> s {subnetIds = a} :: ModifyClusterSubnetGroup)
{-# DEPRECATED mcsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The name of the subnet group to be modified.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgClusterSubnetGroupName :: Lens.Lens' ModifyClusterSubnetGroup Lude.Text
mcsgClusterSubnetGroupName = Lens.lens (clusterSubnetGroupName :: ModifyClusterSubnetGroup -> Lude.Text) (\s a -> s {clusterSubnetGroupName = a} :: ModifyClusterSubnetGroup)
{-# DEPRECATED mcsgClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

-- | A text description of the subnet group to be modified.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgDescription :: Lens.Lens' ModifyClusterSubnetGroup (Lude.Maybe Lude.Text)
mcsgDescription = Lens.lens (description :: ModifyClusterSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ModifyClusterSubnetGroup)
{-# DEPRECATED mcsgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest ModifyClusterSubnetGroup where
  type Rs ModifyClusterSubnetGroup = ModifyClusterSubnetGroupResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ModifyClusterSubnetGroupResult"
      ( \s h x ->
          ModifyClusterSubnetGroupResponse'
            Lude.<$> (x Lude..@? "ClusterSubnetGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyClusterSubnetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyClusterSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyClusterSubnetGroup where
  toQuery ModifyClusterSubnetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyClusterSubnetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "SubnetIds" Lude.=: Lude.toQueryList "SubnetIdentifier" subnetIds,
        "ClusterSubnetGroupName" Lude.=: clusterSubnetGroupName,
        "Description" Lude.=: description
      ]

-- | /See:/ 'mkModifyClusterSubnetGroupResponse' smart constructor.
data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse'
  { clusterSubnetGroup :: Lude.Maybe ClusterSubnetGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterSubnetGroupResponse' with the minimum fields required to make a request.
--
-- * 'clusterSubnetGroup' -
-- * 'responseStatus' - The response status code.
mkModifyClusterSubnetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyClusterSubnetGroupResponse
mkModifyClusterSubnetGroupResponse pResponseStatus_ =
  ModifyClusterSubnetGroupResponse'
    { clusterSubnetGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrsClusterSubnetGroup :: Lens.Lens' ModifyClusterSubnetGroupResponse (Lude.Maybe ClusterSubnetGroup)
mcsgrsClusterSubnetGroup = Lens.lens (clusterSubnetGroup :: ModifyClusterSubnetGroupResponse -> Lude.Maybe ClusterSubnetGroup) (\s a -> s {clusterSubnetGroup = a} :: ModifyClusterSubnetGroupResponse)
{-# DEPRECATED mcsgrsClusterSubnetGroup "Use generic-lens or generic-optics with 'clusterSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrsResponseStatus :: Lens.Lens' ModifyClusterSubnetGroupResponse Lude.Int
mcsgrsResponseStatus = Lens.lens (responseStatus :: ModifyClusterSubnetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyClusterSubnetGroupResponse)
{-# DEPRECATED mcsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
