{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.ModifyMountTargetSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the set of security groups in effect for a mount target.
--
-- When you create a mount target, Amazon EFS also creates a new network interface. For more information, see 'CreateMountTarget' . This operation replaces the security groups in effect for the network interface associated with a mount target, with the @SecurityGroups@ provided in the request. This operation requires that the network interface of the mount target has been created and the lifecycle state of the mount target is not @deleted@ .
-- The operation requires permissions for the following actions:
--
--     * @elasticfilesystem:ModifyMountTargetSecurityGroups@ action on the mount target's file system.
--
--
--     * @ec2:ModifyNetworkInterfaceAttribute@ action on the mount target's network interface.
module Network.AWS.EFS.ModifyMountTargetSecurityGroups
  ( -- * Creating a request
    ModifyMountTargetSecurityGroups (..),
    mkModifyMountTargetSecurityGroups,

    -- ** Request lenses
    mmtsgSecurityGroups,
    mmtsgMountTargetId,

    -- * Destructuring the response
    ModifyMountTargetSecurityGroupsResponse (..),
    mkModifyMountTargetSecurityGroupsResponse,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyMountTargetSecurityGroups' smart constructor.
data ModifyMountTargetSecurityGroups = ModifyMountTargetSecurityGroups'
  { -- | An array of up to five VPC security group IDs.
    securityGroups :: Lude.Maybe [Lude.Text],
    -- | The ID of the mount target whose security groups you want to modify.
    mountTargetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyMountTargetSecurityGroups' with the minimum fields required to make a request.
--
-- * 'securityGroups' - An array of up to five VPC security group IDs.
-- * 'mountTargetId' - The ID of the mount target whose security groups you want to modify.
mkModifyMountTargetSecurityGroups ::
  -- | 'mountTargetId'
  Lude.Text ->
  ModifyMountTargetSecurityGroups
mkModifyMountTargetSecurityGroups pMountTargetId_ =
  ModifyMountTargetSecurityGroups'
    { securityGroups = Lude.Nothing,
      mountTargetId = pMountTargetId_
    }

-- | An array of up to five VPC security group IDs.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmtsgSecurityGroups :: Lens.Lens' ModifyMountTargetSecurityGroups (Lude.Maybe [Lude.Text])
mmtsgSecurityGroups = Lens.lens (securityGroups :: ModifyMountTargetSecurityGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: ModifyMountTargetSecurityGroups)
{-# DEPRECATED mmtsgSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The ID of the mount target whose security groups you want to modify.
--
-- /Note:/ Consider using 'mountTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmtsgMountTargetId :: Lens.Lens' ModifyMountTargetSecurityGroups Lude.Text
mmtsgMountTargetId = Lens.lens (mountTargetId :: ModifyMountTargetSecurityGroups -> Lude.Text) (\s a -> s {mountTargetId = a} :: ModifyMountTargetSecurityGroups)
{-# DEPRECATED mmtsgMountTargetId "Use generic-lens or generic-optics with 'mountTargetId' instead." #-}

instance Lude.AWSRequest ModifyMountTargetSecurityGroups where
  type
    Rs ModifyMountTargetSecurityGroups =
      ModifyMountTargetSecurityGroupsResponse
  request = Req.putJSON efsService
  response = Res.receiveNull ModifyMountTargetSecurityGroupsResponse'

instance Lude.ToHeaders ModifyMountTargetSecurityGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ModifyMountTargetSecurityGroups where
  toJSON ModifyMountTargetSecurityGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [("SecurityGroups" Lude..=) Lude.<$> securityGroups]
      )

instance Lude.ToPath ModifyMountTargetSecurityGroups where
  toPath ModifyMountTargetSecurityGroups' {..} =
    Lude.mconcat
      [ "/2015-02-01/mount-targets/",
        Lude.toBS mountTargetId,
        "/security-groups"
      ]

instance Lude.ToQuery ModifyMountTargetSecurityGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyMountTargetSecurityGroupsResponse' smart constructor.
data ModifyMountTargetSecurityGroupsResponse = ModifyMountTargetSecurityGroupsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyMountTargetSecurityGroupsResponse' with the minimum fields required to make a request.
mkModifyMountTargetSecurityGroupsResponse ::
  ModifyMountTargetSecurityGroupsResponse
mkModifyMountTargetSecurityGroupsResponse =
  ModifyMountTargetSecurityGroupsResponse'
