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
-- Module      : Amazonka.EC2.RestoreSnapshotTier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an archived Amazon EBS snapshot for use temporarily or
-- permanently, or modifies the restore period or restore type for a
-- snapshot that was previously temporarily restored.
--
-- For more information see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/working-with-snapshot-archiving.html#restore-archived-snapshot Restore an archived snapshot>
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/working-with-snapshot-archiving.html#modify-temp-restore-period modify the restore period or restore type for a temporarily restored snapshot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.RestoreSnapshotTier
  ( -- * Creating a Request
    RestoreSnapshotTier (..),
    newRestoreSnapshotTier,

    -- * Request Lenses
    restoreSnapshotTier_temporaryRestoreDays,
    restoreSnapshotTier_dryRun,
    restoreSnapshotTier_permanentRestore,
    restoreSnapshotTier_snapshotId,

    -- * Destructuring the Response
    RestoreSnapshotTierResponse (..),
    newRestoreSnapshotTierResponse,

    -- * Response Lenses
    restoreSnapshotTierResponse_restoreStartTime,
    restoreSnapshotTierResponse_snapshotId,
    restoreSnapshotTierResponse_restoreDuration,
    restoreSnapshotTierResponse_isPermanentRestore,
    restoreSnapshotTierResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreSnapshotTier' smart constructor.
data RestoreSnapshotTier = RestoreSnapshotTier'
  { -- | Specifies the number of days for which to temporarily restore an
    -- archived snapshot. Required for temporary restores only. The snapshot
    -- will be automatically re-archived after this period.
    --
    -- To temporarily restore an archived snapshot, specify the number of days
    -- and omit the __PermanentRestore__ parameter or set it to @false@.
    temporaryRestoreDays :: Prelude.Maybe Prelude.Int,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to permanently restore an archived snapshot. To
    -- permanently restore an archived snapshot, specify @true@ and omit the
    -- __RestoreSnapshotTierRequest$TemporaryRestoreDays__ parameter.
    permanentRestore :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the snapshot to restore.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreSnapshotTier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'temporaryRestoreDays', 'restoreSnapshotTier_temporaryRestoreDays' - Specifies the number of days for which to temporarily restore an
-- archived snapshot. Required for temporary restores only. The snapshot
-- will be automatically re-archived after this period.
--
-- To temporarily restore an archived snapshot, specify the number of days
-- and omit the __PermanentRestore__ parameter or set it to @false@.
--
-- 'dryRun', 'restoreSnapshotTier_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'permanentRestore', 'restoreSnapshotTier_permanentRestore' - Indicates whether to permanently restore an archived snapshot. To
-- permanently restore an archived snapshot, specify @true@ and omit the
-- __RestoreSnapshotTierRequest$TemporaryRestoreDays__ parameter.
--
-- 'snapshotId', 'restoreSnapshotTier_snapshotId' - The ID of the snapshot to restore.
newRestoreSnapshotTier ::
  -- | 'snapshotId'
  Prelude.Text ->
  RestoreSnapshotTier
newRestoreSnapshotTier pSnapshotId_ =
  RestoreSnapshotTier'
    { temporaryRestoreDays =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      permanentRestore = Prelude.Nothing,
      snapshotId = pSnapshotId_
    }

-- | Specifies the number of days for which to temporarily restore an
-- archived snapshot. Required for temporary restores only. The snapshot
-- will be automatically re-archived after this period.
--
-- To temporarily restore an archived snapshot, specify the number of days
-- and omit the __PermanentRestore__ parameter or set it to @false@.
restoreSnapshotTier_temporaryRestoreDays :: Lens.Lens' RestoreSnapshotTier (Prelude.Maybe Prelude.Int)
restoreSnapshotTier_temporaryRestoreDays = Lens.lens (\RestoreSnapshotTier' {temporaryRestoreDays} -> temporaryRestoreDays) (\s@RestoreSnapshotTier' {} a -> s {temporaryRestoreDays = a} :: RestoreSnapshotTier)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
restoreSnapshotTier_dryRun :: Lens.Lens' RestoreSnapshotTier (Prelude.Maybe Prelude.Bool)
restoreSnapshotTier_dryRun = Lens.lens (\RestoreSnapshotTier' {dryRun} -> dryRun) (\s@RestoreSnapshotTier' {} a -> s {dryRun = a} :: RestoreSnapshotTier)

-- | Indicates whether to permanently restore an archived snapshot. To
-- permanently restore an archived snapshot, specify @true@ and omit the
-- __RestoreSnapshotTierRequest$TemporaryRestoreDays__ parameter.
restoreSnapshotTier_permanentRestore :: Lens.Lens' RestoreSnapshotTier (Prelude.Maybe Prelude.Bool)
restoreSnapshotTier_permanentRestore = Lens.lens (\RestoreSnapshotTier' {permanentRestore} -> permanentRestore) (\s@RestoreSnapshotTier' {} a -> s {permanentRestore = a} :: RestoreSnapshotTier)

-- | The ID of the snapshot to restore.
restoreSnapshotTier_snapshotId :: Lens.Lens' RestoreSnapshotTier Prelude.Text
restoreSnapshotTier_snapshotId = Lens.lens (\RestoreSnapshotTier' {snapshotId} -> snapshotId) (\s@RestoreSnapshotTier' {} a -> s {snapshotId = a} :: RestoreSnapshotTier)

instance Core.AWSRequest RestoreSnapshotTier where
  type
    AWSResponse RestoreSnapshotTier =
      RestoreSnapshotTierResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RestoreSnapshotTierResponse'
            Prelude.<$> (x Core..@? "restoreStartTime")
            Prelude.<*> (x Core..@? "snapshotId")
            Prelude.<*> (x Core..@? "restoreDuration")
            Prelude.<*> (x Core..@? "isPermanentRestore")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreSnapshotTier where
  hashWithSalt _salt RestoreSnapshotTier' {..} =
    _salt `Prelude.hashWithSalt` temporaryRestoreDays
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` permanentRestore
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData RestoreSnapshotTier where
  rnf RestoreSnapshotTier' {..} =
    Prelude.rnf temporaryRestoreDays
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf permanentRestore
      `Prelude.seq` Prelude.rnf snapshotId

instance Core.ToHeaders RestoreSnapshotTier where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RestoreSnapshotTier where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreSnapshotTier where
  toQuery RestoreSnapshotTier' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RestoreSnapshotTier" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "TemporaryRestoreDays" Core.=: temporaryRestoreDays,
        "DryRun" Core.=: dryRun,
        "PermanentRestore" Core.=: permanentRestore,
        "SnapshotId" Core.=: snapshotId
      ]

-- | /See:/ 'newRestoreSnapshotTierResponse' smart constructor.
data RestoreSnapshotTierResponse = RestoreSnapshotTierResponse'
  { -- | The date and time when the snapshot restore process started.
    restoreStartTime :: Prelude.Maybe Core.ISO8601,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | For temporary restores only. The number of days for which the archived
    -- snapshot is temporarily restored.
    restoreDuration :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the snapshot is permanently restored. @true@ indicates
    -- a permanent restore. @false@ indicates a temporary restore.
    isPermanentRestore :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreSnapshotTierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restoreStartTime', 'restoreSnapshotTierResponse_restoreStartTime' - The date and time when the snapshot restore process started.
--
-- 'snapshotId', 'restoreSnapshotTierResponse_snapshotId' - The ID of the snapshot.
--
-- 'restoreDuration', 'restoreSnapshotTierResponse_restoreDuration' - For temporary restores only. The number of days for which the archived
-- snapshot is temporarily restored.
--
-- 'isPermanentRestore', 'restoreSnapshotTierResponse_isPermanentRestore' - Indicates whether the snapshot is permanently restored. @true@ indicates
-- a permanent restore. @false@ indicates a temporary restore.
--
-- 'httpStatus', 'restoreSnapshotTierResponse_httpStatus' - The response's http status code.
newRestoreSnapshotTierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreSnapshotTierResponse
newRestoreSnapshotTierResponse pHttpStatus_ =
  RestoreSnapshotTierResponse'
    { restoreStartTime =
        Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      restoreDuration = Prelude.Nothing,
      isPermanentRestore = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time when the snapshot restore process started.
restoreSnapshotTierResponse_restoreStartTime :: Lens.Lens' RestoreSnapshotTierResponse (Prelude.Maybe Prelude.UTCTime)
restoreSnapshotTierResponse_restoreStartTime = Lens.lens (\RestoreSnapshotTierResponse' {restoreStartTime} -> restoreStartTime) (\s@RestoreSnapshotTierResponse' {} a -> s {restoreStartTime = a} :: RestoreSnapshotTierResponse) Prelude.. Lens.mapping Core._Time

-- | The ID of the snapshot.
restoreSnapshotTierResponse_snapshotId :: Lens.Lens' RestoreSnapshotTierResponse (Prelude.Maybe Prelude.Text)
restoreSnapshotTierResponse_snapshotId = Lens.lens (\RestoreSnapshotTierResponse' {snapshotId} -> snapshotId) (\s@RestoreSnapshotTierResponse' {} a -> s {snapshotId = a} :: RestoreSnapshotTierResponse)

-- | For temporary restores only. The number of days for which the archived
-- snapshot is temporarily restored.
restoreSnapshotTierResponse_restoreDuration :: Lens.Lens' RestoreSnapshotTierResponse (Prelude.Maybe Prelude.Int)
restoreSnapshotTierResponse_restoreDuration = Lens.lens (\RestoreSnapshotTierResponse' {restoreDuration} -> restoreDuration) (\s@RestoreSnapshotTierResponse' {} a -> s {restoreDuration = a} :: RestoreSnapshotTierResponse)

-- | Indicates whether the snapshot is permanently restored. @true@ indicates
-- a permanent restore. @false@ indicates a temporary restore.
restoreSnapshotTierResponse_isPermanentRestore :: Lens.Lens' RestoreSnapshotTierResponse (Prelude.Maybe Prelude.Bool)
restoreSnapshotTierResponse_isPermanentRestore = Lens.lens (\RestoreSnapshotTierResponse' {isPermanentRestore} -> isPermanentRestore) (\s@RestoreSnapshotTierResponse' {} a -> s {isPermanentRestore = a} :: RestoreSnapshotTierResponse)

-- | The response's http status code.
restoreSnapshotTierResponse_httpStatus :: Lens.Lens' RestoreSnapshotTierResponse Prelude.Int
restoreSnapshotTierResponse_httpStatus = Lens.lens (\RestoreSnapshotTierResponse' {httpStatus} -> httpStatus) (\s@RestoreSnapshotTierResponse' {} a -> s {httpStatus = a} :: RestoreSnapshotTierResponse)

instance Prelude.NFData RestoreSnapshotTierResponse where
  rnf RestoreSnapshotTierResponse' {..} =
    Prelude.rnf restoreStartTime
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf restoreDuration
      `Prelude.seq` Prelude.rnf isPermanentRestore
      `Prelude.seq` Prelude.rnf httpStatus
