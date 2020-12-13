{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified snapshot.
--
-- When you make periodic snapshots of a volume, the snapshots are incremental, and only the blocks on the device that have changed since your last snapshot are saved in the new snapshot. When you delete a snapshot, only the data not needed for any other snapshot is removed. So regardless of which prior snapshots have been deleted, all active snapshots will have access to all the information needed to restore the volume.
-- You cannot delete a snapshot of the root device of an EBS volume used by a registered AMI. You must first de-register the AMI before you can delete the snapshot.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-snapshot.html Deleting an Amazon EBS snapshot> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DeleteSnapshot
  ( -- * Creating a request
    DeleteSnapshot (..),
    mkDeleteSnapshot,

    -- ** Request lenses
    dshDryRun,
    dshSnapshotId,

    -- * Destructuring the response
    DeleteSnapshotResponse (..),
    mkDeleteSnapshotResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSnapshot' smart constructor.
data DeleteSnapshot = DeleteSnapshot'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The ID of the EBS snapshot.
    snapshotId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSnapshot' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'snapshotId' - The ID of the EBS snapshot.
mkDeleteSnapshot ::
  -- | 'snapshotId'
  Lude.Text ->
  DeleteSnapshot
mkDeleteSnapshot pSnapshotId_ =
  DeleteSnapshot' {dryRun = Lude.Nothing, snapshotId = pSnapshotId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dshDryRun :: Lens.Lens' DeleteSnapshot (Lude.Maybe Lude.Bool)
dshDryRun = Lens.lens (dryRun :: DeleteSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteSnapshot)
{-# DEPRECATED dshDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the EBS snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dshSnapshotId :: Lens.Lens' DeleteSnapshot Lude.Text
dshSnapshotId = Lens.lens (snapshotId :: DeleteSnapshot -> Lude.Text) (\s a -> s {snapshotId = a} :: DeleteSnapshot)
{-# DEPRECATED dshSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.AWSRequest DeleteSnapshot where
  type Rs DeleteSnapshot = DeleteSnapshotResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteSnapshotResponse'

instance Lude.ToHeaders DeleteSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSnapshot where
  toQuery DeleteSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "SnapshotId" Lude.=: snapshotId
      ]

-- | /See:/ 'mkDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSnapshotResponse' with the minimum fields required to make a request.
mkDeleteSnapshotResponse ::
  DeleteSnapshotResponse
mkDeleteSnapshotResponse = DeleteSnapshotResponse'
