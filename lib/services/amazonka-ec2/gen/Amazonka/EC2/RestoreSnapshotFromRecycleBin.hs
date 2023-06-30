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
-- Module      : Amazonka.EC2.RestoreSnapshotFromRecycleBin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a snapshot from the Recycle Bin. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/recycle-bin-working-with-snaps.html#recycle-bin-restore-snaps Restore snapshots from the Recycle Bin>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.RestoreSnapshotFromRecycleBin
  ( -- * Creating a Request
    RestoreSnapshotFromRecycleBin (..),
    newRestoreSnapshotFromRecycleBin,

    -- * Request Lenses
    restoreSnapshotFromRecycleBin_dryRun,
    restoreSnapshotFromRecycleBin_snapshotId,

    -- * Destructuring the Response
    RestoreSnapshotFromRecycleBinResponse (..),
    newRestoreSnapshotFromRecycleBinResponse,

    -- * Response Lenses
    restoreSnapshotFromRecycleBinResponse_description,
    restoreSnapshotFromRecycleBinResponse_encrypted,
    restoreSnapshotFromRecycleBinResponse_outpostArn,
    restoreSnapshotFromRecycleBinResponse_ownerId,
    restoreSnapshotFromRecycleBinResponse_progress,
    restoreSnapshotFromRecycleBinResponse_snapshotId,
    restoreSnapshotFromRecycleBinResponse_startTime,
    restoreSnapshotFromRecycleBinResponse_state,
    restoreSnapshotFromRecycleBinResponse_volumeId,
    restoreSnapshotFromRecycleBinResponse_volumeSize,
    restoreSnapshotFromRecycleBinResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreSnapshotFromRecycleBin' smart constructor.
data RestoreSnapshotFromRecycleBin = RestoreSnapshotFromRecycleBin'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the snapshot to restore.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreSnapshotFromRecycleBin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'restoreSnapshotFromRecycleBin_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'snapshotId', 'restoreSnapshotFromRecycleBin_snapshotId' - The ID of the snapshot to restore.
newRestoreSnapshotFromRecycleBin ::
  -- | 'snapshotId'
  Prelude.Text ->
  RestoreSnapshotFromRecycleBin
newRestoreSnapshotFromRecycleBin pSnapshotId_ =
  RestoreSnapshotFromRecycleBin'
    { dryRun =
        Prelude.Nothing,
      snapshotId = pSnapshotId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
restoreSnapshotFromRecycleBin_dryRun :: Lens.Lens' RestoreSnapshotFromRecycleBin (Prelude.Maybe Prelude.Bool)
restoreSnapshotFromRecycleBin_dryRun = Lens.lens (\RestoreSnapshotFromRecycleBin' {dryRun} -> dryRun) (\s@RestoreSnapshotFromRecycleBin' {} a -> s {dryRun = a} :: RestoreSnapshotFromRecycleBin)

-- | The ID of the snapshot to restore.
restoreSnapshotFromRecycleBin_snapshotId :: Lens.Lens' RestoreSnapshotFromRecycleBin Prelude.Text
restoreSnapshotFromRecycleBin_snapshotId = Lens.lens (\RestoreSnapshotFromRecycleBin' {snapshotId} -> snapshotId) (\s@RestoreSnapshotFromRecycleBin' {} a -> s {snapshotId = a} :: RestoreSnapshotFromRecycleBin)

instance
  Core.AWSRequest
    RestoreSnapshotFromRecycleBin
  where
  type
    AWSResponse RestoreSnapshotFromRecycleBin =
      RestoreSnapshotFromRecycleBinResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RestoreSnapshotFromRecycleBinResponse'
            Prelude.<$> (x Data..@? "description")
            Prelude.<*> (x Data..@? "encrypted")
            Prelude.<*> (x Data..@? "outpostArn")
            Prelude.<*> (x Data..@? "ownerId")
            Prelude.<*> (x Data..@? "progress")
            Prelude.<*> (x Data..@? "snapshotId")
            Prelude.<*> (x Data..@? "startTime")
            Prelude.<*> (x Data..@? "status")
            Prelude.<*> (x Data..@? "volumeId")
            Prelude.<*> (x Data..@? "volumeSize")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreSnapshotFromRecycleBin
  where
  hashWithSalt _salt RestoreSnapshotFromRecycleBin' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData RestoreSnapshotFromRecycleBin where
  rnf RestoreSnapshotFromRecycleBin' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToHeaders RestoreSnapshotFromRecycleBin where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RestoreSnapshotFromRecycleBin where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreSnapshotFromRecycleBin where
  toQuery RestoreSnapshotFromRecycleBin' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RestoreSnapshotFromRecycleBin" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "SnapshotId" Data.=: snapshotId
      ]

-- | /See:/ 'newRestoreSnapshotFromRecycleBinResponse' smart constructor.
data RestoreSnapshotFromRecycleBinResponse = RestoreSnapshotFromRecycleBinResponse'
  { -- | The description for the snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the Outpost on which the snapshot is stored. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html Amazon EBS local snapshots on Outposts>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the EBS snapshot.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The progress of the snapshot, as a percentage.
    progress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The time stamp when the snapshot was initiated.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The state of the snapshot.
    state :: Prelude.Maybe SnapshotState,
    -- | The ID of the volume that was used to create the snapshot.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | The size of the volume, in GiB.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreSnapshotFromRecycleBinResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'restoreSnapshotFromRecycleBinResponse_description' - The description for the snapshot.
--
-- 'encrypted', 'restoreSnapshotFromRecycleBinResponse_encrypted' - Indicates whether the snapshot is encrypted.
--
-- 'outpostArn', 'restoreSnapshotFromRecycleBinResponse_outpostArn' - The ARN of the Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html Amazon EBS local snapshots on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'ownerId', 'restoreSnapshotFromRecycleBinResponse_ownerId' - The ID of the Amazon Web Services account that owns the EBS snapshot.
--
-- 'progress', 'restoreSnapshotFromRecycleBinResponse_progress' - The progress of the snapshot, as a percentage.
--
-- 'snapshotId', 'restoreSnapshotFromRecycleBinResponse_snapshotId' - The ID of the snapshot.
--
-- 'startTime', 'restoreSnapshotFromRecycleBinResponse_startTime' - The time stamp when the snapshot was initiated.
--
-- 'state', 'restoreSnapshotFromRecycleBinResponse_state' - The state of the snapshot.
--
-- 'volumeId', 'restoreSnapshotFromRecycleBinResponse_volumeId' - The ID of the volume that was used to create the snapshot.
--
-- 'volumeSize', 'restoreSnapshotFromRecycleBinResponse_volumeSize' - The size of the volume, in GiB.
--
-- 'httpStatus', 'restoreSnapshotFromRecycleBinResponse_httpStatus' - The response's http status code.
newRestoreSnapshotFromRecycleBinResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreSnapshotFromRecycleBinResponse
newRestoreSnapshotFromRecycleBinResponse pHttpStatus_ =
  RestoreSnapshotFromRecycleBinResponse'
    { description =
        Prelude.Nothing,
      encrypted = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      progress = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      state = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description for the snapshot.
restoreSnapshotFromRecycleBinResponse_description :: Lens.Lens' RestoreSnapshotFromRecycleBinResponse (Prelude.Maybe Prelude.Text)
restoreSnapshotFromRecycleBinResponse_description = Lens.lens (\RestoreSnapshotFromRecycleBinResponse' {description} -> description) (\s@RestoreSnapshotFromRecycleBinResponse' {} a -> s {description = a} :: RestoreSnapshotFromRecycleBinResponse)

-- | Indicates whether the snapshot is encrypted.
restoreSnapshotFromRecycleBinResponse_encrypted :: Lens.Lens' RestoreSnapshotFromRecycleBinResponse (Prelude.Maybe Prelude.Bool)
restoreSnapshotFromRecycleBinResponse_encrypted = Lens.lens (\RestoreSnapshotFromRecycleBinResponse' {encrypted} -> encrypted) (\s@RestoreSnapshotFromRecycleBinResponse' {} a -> s {encrypted = a} :: RestoreSnapshotFromRecycleBinResponse)

-- | The ARN of the Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html Amazon EBS local snapshots on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
restoreSnapshotFromRecycleBinResponse_outpostArn :: Lens.Lens' RestoreSnapshotFromRecycleBinResponse (Prelude.Maybe Prelude.Text)
restoreSnapshotFromRecycleBinResponse_outpostArn = Lens.lens (\RestoreSnapshotFromRecycleBinResponse' {outpostArn} -> outpostArn) (\s@RestoreSnapshotFromRecycleBinResponse' {} a -> s {outpostArn = a} :: RestoreSnapshotFromRecycleBinResponse)

-- | The ID of the Amazon Web Services account that owns the EBS snapshot.
restoreSnapshotFromRecycleBinResponse_ownerId :: Lens.Lens' RestoreSnapshotFromRecycleBinResponse (Prelude.Maybe Prelude.Text)
restoreSnapshotFromRecycleBinResponse_ownerId = Lens.lens (\RestoreSnapshotFromRecycleBinResponse' {ownerId} -> ownerId) (\s@RestoreSnapshotFromRecycleBinResponse' {} a -> s {ownerId = a} :: RestoreSnapshotFromRecycleBinResponse)

-- | The progress of the snapshot, as a percentage.
restoreSnapshotFromRecycleBinResponse_progress :: Lens.Lens' RestoreSnapshotFromRecycleBinResponse (Prelude.Maybe Prelude.Text)
restoreSnapshotFromRecycleBinResponse_progress = Lens.lens (\RestoreSnapshotFromRecycleBinResponse' {progress} -> progress) (\s@RestoreSnapshotFromRecycleBinResponse' {} a -> s {progress = a} :: RestoreSnapshotFromRecycleBinResponse)

-- | The ID of the snapshot.
restoreSnapshotFromRecycleBinResponse_snapshotId :: Lens.Lens' RestoreSnapshotFromRecycleBinResponse (Prelude.Maybe Prelude.Text)
restoreSnapshotFromRecycleBinResponse_snapshotId = Lens.lens (\RestoreSnapshotFromRecycleBinResponse' {snapshotId} -> snapshotId) (\s@RestoreSnapshotFromRecycleBinResponse' {} a -> s {snapshotId = a} :: RestoreSnapshotFromRecycleBinResponse)

-- | The time stamp when the snapshot was initiated.
restoreSnapshotFromRecycleBinResponse_startTime :: Lens.Lens' RestoreSnapshotFromRecycleBinResponse (Prelude.Maybe Prelude.UTCTime)
restoreSnapshotFromRecycleBinResponse_startTime = Lens.lens (\RestoreSnapshotFromRecycleBinResponse' {startTime} -> startTime) (\s@RestoreSnapshotFromRecycleBinResponse' {} a -> s {startTime = a} :: RestoreSnapshotFromRecycleBinResponse) Prelude.. Lens.mapping Data._Time

-- | The state of the snapshot.
restoreSnapshotFromRecycleBinResponse_state :: Lens.Lens' RestoreSnapshotFromRecycleBinResponse (Prelude.Maybe SnapshotState)
restoreSnapshotFromRecycleBinResponse_state = Lens.lens (\RestoreSnapshotFromRecycleBinResponse' {state} -> state) (\s@RestoreSnapshotFromRecycleBinResponse' {} a -> s {state = a} :: RestoreSnapshotFromRecycleBinResponse)

-- | The ID of the volume that was used to create the snapshot.
restoreSnapshotFromRecycleBinResponse_volumeId :: Lens.Lens' RestoreSnapshotFromRecycleBinResponse (Prelude.Maybe Prelude.Text)
restoreSnapshotFromRecycleBinResponse_volumeId = Lens.lens (\RestoreSnapshotFromRecycleBinResponse' {volumeId} -> volumeId) (\s@RestoreSnapshotFromRecycleBinResponse' {} a -> s {volumeId = a} :: RestoreSnapshotFromRecycleBinResponse)

-- | The size of the volume, in GiB.
restoreSnapshotFromRecycleBinResponse_volumeSize :: Lens.Lens' RestoreSnapshotFromRecycleBinResponse (Prelude.Maybe Prelude.Int)
restoreSnapshotFromRecycleBinResponse_volumeSize = Lens.lens (\RestoreSnapshotFromRecycleBinResponse' {volumeSize} -> volumeSize) (\s@RestoreSnapshotFromRecycleBinResponse' {} a -> s {volumeSize = a} :: RestoreSnapshotFromRecycleBinResponse)

-- | The response's http status code.
restoreSnapshotFromRecycleBinResponse_httpStatus :: Lens.Lens' RestoreSnapshotFromRecycleBinResponse Prelude.Int
restoreSnapshotFromRecycleBinResponse_httpStatus = Lens.lens (\RestoreSnapshotFromRecycleBinResponse' {httpStatus} -> httpStatus) (\s@RestoreSnapshotFromRecycleBinResponse' {} a -> s {httpStatus = a} :: RestoreSnapshotFromRecycleBinResponse)

instance
  Prelude.NFData
    RestoreSnapshotFromRecycleBinResponse
  where
  rnf RestoreSnapshotFromRecycleBinResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf httpStatus
