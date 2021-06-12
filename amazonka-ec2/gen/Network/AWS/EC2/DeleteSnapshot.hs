{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified snapshot.
--
-- When you make periodic snapshots of a volume, the snapshots are
-- incremental, and only the blocks on the device that have changed since
-- your last snapshot are saved in the new snapshot. When you delete a
-- snapshot, only the data not needed for any other snapshot is removed. So
-- regardless of which prior snapshots have been deleted, all active
-- snapshots will have access to all the information needed to restore the
-- volume.
--
-- You cannot delete a snapshot of the root device of an EBS volume used by
-- a registered AMI. You must first de-register the AMI before you can
-- delete the snapshot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-snapshot.html Deleting an Amazon EBS snapshot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.DeleteSnapshot
  ( -- * Creating a Request
    DeleteSnapshot (..),
    newDeleteSnapshot,

    -- * Request Lenses
    deleteSnapshot_dryRun,
    deleteSnapshot_snapshotId,

    -- * Destructuring the Response
    DeleteSnapshotResponse (..),
    newDeleteSnapshotResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSnapshot' smart constructor.
data DeleteSnapshot = DeleteSnapshot'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the EBS snapshot.
    snapshotId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteSnapshot_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'snapshotId', 'deleteSnapshot_snapshotId' - The ID of the EBS snapshot.
newDeleteSnapshot ::
  -- | 'snapshotId'
  Core.Text ->
  DeleteSnapshot
newDeleteSnapshot pSnapshotId_ =
  DeleteSnapshot'
    { dryRun = Core.Nothing,
      snapshotId = pSnapshotId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteSnapshot_dryRun :: Lens.Lens' DeleteSnapshot (Core.Maybe Core.Bool)
deleteSnapshot_dryRun = Lens.lens (\DeleteSnapshot' {dryRun} -> dryRun) (\s@DeleteSnapshot' {} a -> s {dryRun = a} :: DeleteSnapshot)

-- | The ID of the EBS snapshot.
deleteSnapshot_snapshotId :: Lens.Lens' DeleteSnapshot Core.Text
deleteSnapshot_snapshotId = Lens.lens (\DeleteSnapshot' {snapshotId} -> snapshotId) (\s@DeleteSnapshot' {} a -> s {snapshotId = a} :: DeleteSnapshot)

instance Core.AWSRequest DeleteSnapshot where
  type
    AWSResponse DeleteSnapshot =
      DeleteSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteSnapshotResponse'

instance Core.Hashable DeleteSnapshot

instance Core.NFData DeleteSnapshot

instance Core.ToHeaders DeleteSnapshot where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSnapshot where
  toQuery DeleteSnapshot' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteSnapshot" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "SnapshotId" Core.=: snapshotId
      ]

-- | /See:/ 'newDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSnapshotResponse ::
  DeleteSnapshotResponse
newDeleteSnapshotResponse = DeleteSnapshotResponse'

instance Core.NFData DeleteSnapshotResponse
