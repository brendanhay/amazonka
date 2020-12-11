{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteAutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Auto Scaling group.
--
-- If the group has instances or scaling activities in progress, you must specify the option to force the deletion in order for it to succeed.
-- If the group has policies, deleting the group deletes the policies, the underlying alarm actions, and any alarm that no longer has an associated action.
-- To remove instances from the Auto Scaling group before deleting it, call the 'DetachInstances' API with the list of instances and the option to decrement the desired capacity. This ensures that Amazon EC2 Auto Scaling does not launch replacement instances.
-- To terminate all instances before deleting the Auto Scaling group, call the 'UpdateAutoScalingGroup' API and set the minimum size and desired capacity of the Auto Scaling group to zero.
module Network.AWS.AutoScaling.DeleteAutoScalingGroup
  ( -- * Creating a request
    DeleteAutoScalingGroup (..),
    mkDeleteAutoScalingGroup,

    -- ** Request lenses
    dasgForceDelete,
    dasgAutoScalingGroupName,

    -- * Destructuring the response
    DeleteAutoScalingGroupResponse (..),
    mkDeleteAutoScalingGroupResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAutoScalingGroup' smart constructor.
data DeleteAutoScalingGroup = DeleteAutoScalingGroup'
  { forceDelete ::
      Lude.Maybe Lude.Bool,
    autoScalingGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAutoScalingGroup' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'forceDelete' - Specifies that the group is to be deleted along with all instances associated with the group, without waiting for all instances to be terminated. This parameter also deletes any lifecycle actions associated with the group.
mkDeleteAutoScalingGroup ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  DeleteAutoScalingGroup
mkDeleteAutoScalingGroup pAutoScalingGroupName_ =
  DeleteAutoScalingGroup'
    { forceDelete = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | Specifies that the group is to be deleted along with all instances associated with the group, without waiting for all instances to be terminated. This parameter also deletes any lifecycle actions associated with the group.
--
-- /Note:/ Consider using 'forceDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgForceDelete :: Lens.Lens' DeleteAutoScalingGroup (Lude.Maybe Lude.Bool)
dasgForceDelete = Lens.lens (forceDelete :: DeleteAutoScalingGroup -> Lude.Maybe Lude.Bool) (\s a -> s {forceDelete = a} :: DeleteAutoScalingGroup)
{-# DEPRECATED dasgForceDelete "Use generic-lens or generic-optics with 'forceDelete' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgAutoScalingGroupName :: Lens.Lens' DeleteAutoScalingGroup Lude.Text
dasgAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DeleteAutoScalingGroup -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DeleteAutoScalingGroup)
{-# DEPRECATED dasgAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest DeleteAutoScalingGroup where
  type Rs DeleteAutoScalingGroup = DeleteAutoScalingGroupResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull DeleteAutoScalingGroupResponse'

instance Lude.ToHeaders DeleteAutoScalingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAutoScalingGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAutoScalingGroup where
  toQuery DeleteAutoScalingGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteAutoScalingGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "ForceDelete" Lude.=: forceDelete,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkDeleteAutoScalingGroupResponse' smart constructor.
data DeleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAutoScalingGroupResponse' with the minimum fields required to make a request.
mkDeleteAutoScalingGroupResponse ::
  DeleteAutoScalingGroupResponse
mkDeleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse'
