{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeletePlacementGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified placement group. You must terminate all instances in the placement group before you can delete the placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DeletePlacementGroup
  ( -- * Creating a request
    DeletePlacementGroup (..),
    mkDeletePlacementGroup,

    -- ** Request lenses
    dpgDryRun,
    dpgGroupName,

    -- * Destructuring the response
    DeletePlacementGroupResponse (..),
    mkDeletePlacementGroupResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePlacementGroup' smart constructor.
data DeletePlacementGroup = DeletePlacementGroup'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    groupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePlacementGroup' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'groupName' - The name of the placement group.
mkDeletePlacementGroup ::
  -- | 'groupName'
  Lude.Text ->
  DeletePlacementGroup
mkDeletePlacementGroup pGroupName_ =
  DeletePlacementGroup'
    { dryRun = Lude.Nothing,
      groupName = pGroupName_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgDryRun :: Lens.Lens' DeletePlacementGroup (Lude.Maybe Lude.Bool)
dpgDryRun = Lens.lens (dryRun :: DeletePlacementGroup -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeletePlacementGroup)
{-# DEPRECATED dpgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The name of the placement group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgGroupName :: Lens.Lens' DeletePlacementGroup Lude.Text
dpgGroupName = Lens.lens (groupName :: DeletePlacementGroup -> Lude.Text) (\s a -> s {groupName = a} :: DeletePlacementGroup)
{-# DEPRECATED dpgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest DeletePlacementGroup where
  type Rs DeletePlacementGroup = DeletePlacementGroupResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeletePlacementGroupResponse'

instance Lude.ToHeaders DeletePlacementGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePlacementGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePlacementGroup where
  toQuery DeletePlacementGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeletePlacementGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "GroupName" Lude.=: groupName
      ]

-- | /See:/ 'mkDeletePlacementGroupResponse' smart constructor.
data DeletePlacementGroupResponse = DeletePlacementGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePlacementGroupResponse' with the minimum fields required to make a request.
mkDeletePlacementGroupResponse ::
  DeletePlacementGroupResponse
mkDeletePlacementGroupResponse = DeletePlacementGroupResponse'
