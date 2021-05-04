{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StorageGateway.DeleteSnapshotSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a snapshot of a volume.
--
-- You can take snapshots of your gateway volumes on a scheduled or ad hoc
-- basis. This API action enables you to delete a snapshot schedule for a
-- volume. For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/backing-up-volumes.html Backing up your volumes>.
-- In the @DeleteSnapshotSchedule@ request, you identify the volume by
-- providing its Amazon Resource Name (ARN). This operation is only
-- supported in stored and cached volume gateway types.
--
-- To list or delete a snapshot, you must use the Amazon EC2 API. For more
-- information, go to
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeSnapshots.html DescribeSnapshots>
-- in the /Amazon Elastic Compute Cloud API Reference/.
module Network.AWS.StorageGateway.DeleteSnapshotSchedule
  ( -- * Creating a Request
    DeleteSnapshotSchedule (..),
    newDeleteSnapshotSchedule,

    -- * Request Lenses
    deleteSnapshotSchedule_volumeARN,

    -- * Destructuring the Response
    DeleteSnapshotScheduleResponse (..),
    newDeleteSnapshotScheduleResponse,

    -- * Response Lenses
    deleteSnapshotScheduleResponse_volumeARN,
    deleteSnapshotScheduleResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newDeleteSnapshotSchedule' smart constructor.
data DeleteSnapshotSchedule = DeleteSnapshotSchedule'
  { -- | The volume which snapshot schedule to delete.
    volumeARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSnapshotSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'deleteSnapshotSchedule_volumeARN' - The volume which snapshot schedule to delete.
newDeleteSnapshotSchedule ::
  -- | 'volumeARN'
  Prelude.Text ->
  DeleteSnapshotSchedule
newDeleteSnapshotSchedule pVolumeARN_ =
  DeleteSnapshotSchedule' {volumeARN = pVolumeARN_}

-- | The volume which snapshot schedule to delete.
deleteSnapshotSchedule_volumeARN :: Lens.Lens' DeleteSnapshotSchedule Prelude.Text
deleteSnapshotSchedule_volumeARN = Lens.lens (\DeleteSnapshotSchedule' {volumeARN} -> volumeARN) (\s@DeleteSnapshotSchedule' {} a -> s {volumeARN = a} :: DeleteSnapshotSchedule)

instance Prelude.AWSRequest DeleteSnapshotSchedule where
  type
    Rs DeleteSnapshotSchedule =
      DeleteSnapshotScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSnapshotScheduleResponse'
            Prelude.<$> (x Prelude..?> "VolumeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSnapshotSchedule

instance Prelude.NFData DeleteSnapshotSchedule

instance Prelude.ToHeaders DeleteSnapshotSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DeleteSnapshotSchedule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteSnapshotSchedule where
  toJSON DeleteSnapshotSchedule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("VolumeARN" Prelude..= volumeARN)]
      )

instance Prelude.ToPath DeleteSnapshotSchedule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteSnapshotSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSnapshotScheduleResponse' smart constructor.
data DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'
  { -- | The volume which snapshot schedule was deleted.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSnapshotScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'deleteSnapshotScheduleResponse_volumeARN' - The volume which snapshot schedule was deleted.
--
-- 'httpStatus', 'deleteSnapshotScheduleResponse_httpStatus' - The response's http status code.
newDeleteSnapshotScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSnapshotScheduleResponse
newDeleteSnapshotScheduleResponse pHttpStatus_ =
  DeleteSnapshotScheduleResponse'
    { volumeARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The volume which snapshot schedule was deleted.
deleteSnapshotScheduleResponse_volumeARN :: Lens.Lens' DeleteSnapshotScheduleResponse (Prelude.Maybe Prelude.Text)
deleteSnapshotScheduleResponse_volumeARN = Lens.lens (\DeleteSnapshotScheduleResponse' {volumeARN} -> volumeARN) (\s@DeleteSnapshotScheduleResponse' {} a -> s {volumeARN = a} :: DeleteSnapshotScheduleResponse)

-- | The response's http status code.
deleteSnapshotScheduleResponse_httpStatus :: Lens.Lens' DeleteSnapshotScheduleResponse Prelude.Int
deleteSnapshotScheduleResponse_httpStatus = Lens.lens (\DeleteSnapshotScheduleResponse' {httpStatus} -> httpStatus) (\s@DeleteSnapshotScheduleResponse' {} a -> s {httpStatus = a} :: DeleteSnapshotScheduleResponse)

instance
  Prelude.NFData
    DeleteSnapshotScheduleResponse
