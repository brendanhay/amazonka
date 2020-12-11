{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a security group.
--
-- If you attempt to delete a security group that is associated with an instance, or is referenced by another security group, the operation fails with @InvalidGroup.InUse@ in EC2-Classic or @DependencyViolation@ in EC2-VPC.
module Network.AWS.EC2.DeleteSecurityGroup
  ( -- * Creating a request
    DeleteSecurityGroup (..),
    mkDeleteSecurityGroup,

    -- ** Request lenses
    dsgGroupId,
    dsgGroupName,
    dsgDryRun,

    -- * Destructuring the response
    DeleteSecurityGroupResponse (..),
    mkDeleteSecurityGroupResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSecurityGroup' smart constructor.
data DeleteSecurityGroup = DeleteSecurityGroup'
  { groupId ::
      Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSecurityGroup' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'groupId' - The ID of the security group. Required for a nondefault VPC.
-- * 'groupName' - [EC2-Classic, default VPC] The name of the security group. You can specify either the security group name or the security group ID.
mkDeleteSecurityGroup ::
  DeleteSecurityGroup
mkDeleteSecurityGroup =
  DeleteSecurityGroup'
    { groupId = Lude.Nothing,
      groupName = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the security group. Required for a nondefault VPC.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgGroupId :: Lens.Lens' DeleteSecurityGroup (Lude.Maybe Lude.Text)
dsgGroupId = Lens.lens (groupId :: DeleteSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: DeleteSecurityGroup)
{-# DEPRECATED dsgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | [EC2-Classic, default VPC] The name of the security group. You can specify either the security group name or the security group ID.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgGroupName :: Lens.Lens' DeleteSecurityGroup (Lude.Maybe Lude.Text)
dsgGroupName = Lens.lens (groupName :: DeleteSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: DeleteSecurityGroup)
{-# DEPRECATED dsgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDryRun :: Lens.Lens' DeleteSecurityGroup (Lude.Maybe Lude.Bool)
dsgDryRun = Lens.lens (dryRun :: DeleteSecurityGroup -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteSecurityGroup)
{-# DEPRECATED dsgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteSecurityGroup where
  type Rs DeleteSecurityGroup = DeleteSecurityGroupResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteSecurityGroupResponse'

instance Lude.ToHeaders DeleteSecurityGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSecurityGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSecurityGroup where
  toQuery DeleteSecurityGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteSecurityGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "GroupId" Lude.=: groupId,
        "GroupName" Lude.=: groupName,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteSecurityGroupResponse' smart constructor.
data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSecurityGroupResponse' with the minimum fields required to make a request.
mkDeleteSecurityGroupResponse ::
  DeleteSecurityGroupResponse
mkDeleteSecurityGroupResponse = DeleteSecurityGroupResponse'
