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
-- Module      : Amazonka.EC2.ModifySnapshotTier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Archives an Amazon EBS snapshot. When you archive a snapshot, it is
-- converted to a full snapshot that includes all of the blocks of data
-- that were written to the volume at the time the snapshot was created,
-- and moved from the standard tier to the archive tier. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshot-archive.html Archive Amazon EBS snapshots>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.ModifySnapshotTier
  ( -- * Creating a Request
    ModifySnapshotTier (..),
    newModifySnapshotTier,

    -- * Request Lenses
    modifySnapshotTier_dryRun,
    modifySnapshotTier_storageTier,
    modifySnapshotTier_snapshotId,

    -- * Destructuring the Response
    ModifySnapshotTierResponse (..),
    newModifySnapshotTierResponse,

    -- * Response Lenses
    modifySnapshotTierResponse_snapshotId,
    modifySnapshotTierResponse_tieringStartTime,
    modifySnapshotTierResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifySnapshotTier' smart constructor.
data ModifySnapshotTier = ModifySnapshotTier'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the storage tier. You must specify @archive@.
    storageTier :: Prelude.Maybe TargetStorageTier,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySnapshotTier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifySnapshotTier_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'storageTier', 'modifySnapshotTier_storageTier' - The name of the storage tier. You must specify @archive@.
--
-- 'snapshotId', 'modifySnapshotTier_snapshotId' - The ID of the snapshot.
newModifySnapshotTier ::
  -- | 'snapshotId'
  Prelude.Text ->
  ModifySnapshotTier
newModifySnapshotTier pSnapshotId_ =
  ModifySnapshotTier'
    { dryRun = Prelude.Nothing,
      storageTier = Prelude.Nothing,
      snapshotId = pSnapshotId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifySnapshotTier_dryRun :: Lens.Lens' ModifySnapshotTier (Prelude.Maybe Prelude.Bool)
modifySnapshotTier_dryRun = Lens.lens (\ModifySnapshotTier' {dryRun} -> dryRun) (\s@ModifySnapshotTier' {} a -> s {dryRun = a} :: ModifySnapshotTier)

-- | The name of the storage tier. You must specify @archive@.
modifySnapshotTier_storageTier :: Lens.Lens' ModifySnapshotTier (Prelude.Maybe TargetStorageTier)
modifySnapshotTier_storageTier = Lens.lens (\ModifySnapshotTier' {storageTier} -> storageTier) (\s@ModifySnapshotTier' {} a -> s {storageTier = a} :: ModifySnapshotTier)

-- | The ID of the snapshot.
modifySnapshotTier_snapshotId :: Lens.Lens' ModifySnapshotTier Prelude.Text
modifySnapshotTier_snapshotId = Lens.lens (\ModifySnapshotTier' {snapshotId} -> snapshotId) (\s@ModifySnapshotTier' {} a -> s {snapshotId = a} :: ModifySnapshotTier)

instance Core.AWSRequest ModifySnapshotTier where
  type
    AWSResponse ModifySnapshotTier =
      ModifySnapshotTierResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifySnapshotTierResponse'
            Prelude.<$> (x Core..@? "snapshotId")
            Prelude.<*> (x Core..@? "tieringStartTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifySnapshotTier where
  hashWithSalt _salt ModifySnapshotTier' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` storageTier
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData ModifySnapshotTier where
  rnf ModifySnapshotTier' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf storageTier
      `Prelude.seq` Prelude.rnf snapshotId

instance Core.ToHeaders ModifySnapshotTier where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifySnapshotTier where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifySnapshotTier where
  toQuery ModifySnapshotTier' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifySnapshotTier" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "StorageTier" Core.=: storageTier,
        "SnapshotId" Core.=: snapshotId
      ]

-- | /See:/ 'newModifySnapshotTierResponse' smart constructor.
data ModifySnapshotTierResponse = ModifySnapshotTierResponse'
  { -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the archive process was started.
    tieringStartTime :: Prelude.Maybe Core.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySnapshotTierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotId', 'modifySnapshotTierResponse_snapshotId' - The ID of the snapshot.
--
-- 'tieringStartTime', 'modifySnapshotTierResponse_tieringStartTime' - The date and time when the archive process was started.
--
-- 'httpStatus', 'modifySnapshotTierResponse_httpStatus' - The response's http status code.
newModifySnapshotTierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifySnapshotTierResponse
newModifySnapshotTierResponse pHttpStatus_ =
  ModifySnapshotTierResponse'
    { snapshotId =
        Prelude.Nothing,
      tieringStartTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the snapshot.
modifySnapshotTierResponse_snapshotId :: Lens.Lens' ModifySnapshotTierResponse (Prelude.Maybe Prelude.Text)
modifySnapshotTierResponse_snapshotId = Lens.lens (\ModifySnapshotTierResponse' {snapshotId} -> snapshotId) (\s@ModifySnapshotTierResponse' {} a -> s {snapshotId = a} :: ModifySnapshotTierResponse)

-- | The date and time when the archive process was started.
modifySnapshotTierResponse_tieringStartTime :: Lens.Lens' ModifySnapshotTierResponse (Prelude.Maybe Prelude.UTCTime)
modifySnapshotTierResponse_tieringStartTime = Lens.lens (\ModifySnapshotTierResponse' {tieringStartTime} -> tieringStartTime) (\s@ModifySnapshotTierResponse' {} a -> s {tieringStartTime = a} :: ModifySnapshotTierResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
modifySnapshotTierResponse_httpStatus :: Lens.Lens' ModifySnapshotTierResponse Prelude.Int
modifySnapshotTierResponse_httpStatus = Lens.lens (\ModifySnapshotTierResponse' {httpStatus} -> httpStatus) (\s@ModifySnapshotTierResponse' {} a -> s {httpStatus = a} :: ModifySnapshotTierResponse)

instance Prelude.NFData ModifySnapshotTierResponse where
  rnf ModifySnapshotTierResponse' {..} =
    Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf tieringStartTime
      `Prelude.seq` Prelude.rnf httpStatus
