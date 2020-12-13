{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DeleteMountTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified mount target.
--
-- This operation forcibly breaks any mounts of the file system by using the mount target that is being deleted, which might disrupt instances or applications using those mounts. To avoid applications getting cut off abruptly, you might consider unmounting any mounts of the mount target, if feasible. The operation also deletes the associated network interface. Uncommitted writes might be lost, but breaking a mount target using this operation does not corrupt the file system itself. The file system you created remains. You can mount an EC2 instance in your VPC by using another mount target.
-- This operation requires permissions for the following action on the file system:
--
--     * @elasticfilesystem:DeleteMountTarget@
--
--
-- The operation also requires permissions for the following Amazon EC2 action on the mount target's network interface:
--
--     * @ec2:DeleteNetworkInterface@
module Network.AWS.EFS.DeleteMountTarget
  ( -- * Creating a request
    DeleteMountTarget (..),
    mkDeleteMountTarget,

    -- ** Request lenses
    dmtMountTargetId,

    -- * Destructuring the response
    DeleteMountTargetResponse (..),
    mkDeleteMountTargetResponse,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteMountTarget' smart constructor.
newtype DeleteMountTarget = DeleteMountTarget'
  { -- | The ID of the mount target to delete (String).
    mountTargetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMountTarget' with the minimum fields required to make a request.
--
-- * 'mountTargetId' - The ID of the mount target to delete (String).
mkDeleteMountTarget ::
  -- | 'mountTargetId'
  Lude.Text ->
  DeleteMountTarget
mkDeleteMountTarget pMountTargetId_ =
  DeleteMountTarget' {mountTargetId = pMountTargetId_}

-- | The ID of the mount target to delete (String).
--
-- /Note:/ Consider using 'mountTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtMountTargetId :: Lens.Lens' DeleteMountTarget Lude.Text
dmtMountTargetId = Lens.lens (mountTargetId :: DeleteMountTarget -> Lude.Text) (\s a -> s {mountTargetId = a} :: DeleteMountTarget)
{-# DEPRECATED dmtMountTargetId "Use generic-lens or generic-optics with 'mountTargetId' instead." #-}

instance Lude.AWSRequest DeleteMountTarget where
  type Rs DeleteMountTarget = DeleteMountTargetResponse
  request = Req.delete efsService
  response = Res.receiveNull DeleteMountTargetResponse'

instance Lude.ToHeaders DeleteMountTarget where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteMountTarget where
  toPath DeleteMountTarget' {..} =
    Lude.mconcat
      ["/2015-02-01/mount-targets/", Lude.toBS mountTargetId]

instance Lude.ToQuery DeleteMountTarget where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMountTargetResponse' smart constructor.
data DeleteMountTargetResponse = DeleteMountTargetResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMountTargetResponse' with the minimum fields required to make a request.
mkDeleteMountTargetResponse ::
  DeleteMountTargetResponse
mkDeleteMountTargetResponse = DeleteMountTargetResponse'
