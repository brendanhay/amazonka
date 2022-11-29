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
-- Module      : Amazonka.RedshiftServerLess.ConvertRecoveryPointToSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Converts a recovery point to a snapshot. For more information about
-- recovery points and snapshots, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/serverless-snapshots-recovery.html Working with snapshots and recovery points>.
module Amazonka.RedshiftServerLess.ConvertRecoveryPointToSnapshot
  ( -- * Creating a Request
    ConvertRecoveryPointToSnapshot (..),
    newConvertRecoveryPointToSnapshot,

    -- * Request Lenses
    convertRecoveryPointToSnapshot_retentionPeriod,
    convertRecoveryPointToSnapshot_recoveryPointId,
    convertRecoveryPointToSnapshot_snapshotName,

    -- * Destructuring the Response
    ConvertRecoveryPointToSnapshotResponse (..),
    newConvertRecoveryPointToSnapshotResponse,

    -- * Response Lenses
    convertRecoveryPointToSnapshotResponse_snapshot,
    convertRecoveryPointToSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newConvertRecoveryPointToSnapshot' smart constructor.
data ConvertRecoveryPointToSnapshot = ConvertRecoveryPointToSnapshot'
  { -- | How long to retain the snapshot.
    retentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier of the recovery point.
    recoveryPointId :: Prelude.Text,
    -- | The name of the snapshot.
    snapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConvertRecoveryPointToSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionPeriod', 'convertRecoveryPointToSnapshot_retentionPeriod' - How long to retain the snapshot.
--
-- 'recoveryPointId', 'convertRecoveryPointToSnapshot_recoveryPointId' - The unique identifier of the recovery point.
--
-- 'snapshotName', 'convertRecoveryPointToSnapshot_snapshotName' - The name of the snapshot.
newConvertRecoveryPointToSnapshot ::
  -- | 'recoveryPointId'
  Prelude.Text ->
  -- | 'snapshotName'
  Prelude.Text ->
  ConvertRecoveryPointToSnapshot
newConvertRecoveryPointToSnapshot
  pRecoveryPointId_
  pSnapshotName_ =
    ConvertRecoveryPointToSnapshot'
      { retentionPeriod =
          Prelude.Nothing,
        recoveryPointId = pRecoveryPointId_,
        snapshotName = pSnapshotName_
      }

-- | How long to retain the snapshot.
convertRecoveryPointToSnapshot_retentionPeriod :: Lens.Lens' ConvertRecoveryPointToSnapshot (Prelude.Maybe Prelude.Int)
convertRecoveryPointToSnapshot_retentionPeriod = Lens.lens (\ConvertRecoveryPointToSnapshot' {retentionPeriod} -> retentionPeriod) (\s@ConvertRecoveryPointToSnapshot' {} a -> s {retentionPeriod = a} :: ConvertRecoveryPointToSnapshot)

-- | The unique identifier of the recovery point.
convertRecoveryPointToSnapshot_recoveryPointId :: Lens.Lens' ConvertRecoveryPointToSnapshot Prelude.Text
convertRecoveryPointToSnapshot_recoveryPointId = Lens.lens (\ConvertRecoveryPointToSnapshot' {recoveryPointId} -> recoveryPointId) (\s@ConvertRecoveryPointToSnapshot' {} a -> s {recoveryPointId = a} :: ConvertRecoveryPointToSnapshot)

-- | The name of the snapshot.
convertRecoveryPointToSnapshot_snapshotName :: Lens.Lens' ConvertRecoveryPointToSnapshot Prelude.Text
convertRecoveryPointToSnapshot_snapshotName = Lens.lens (\ConvertRecoveryPointToSnapshot' {snapshotName} -> snapshotName) (\s@ConvertRecoveryPointToSnapshot' {} a -> s {snapshotName = a} :: ConvertRecoveryPointToSnapshot)

instance
  Core.AWSRequest
    ConvertRecoveryPointToSnapshot
  where
  type
    AWSResponse ConvertRecoveryPointToSnapshot =
      ConvertRecoveryPointToSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ConvertRecoveryPointToSnapshotResponse'
            Prelude.<$> (x Core..?> "snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ConvertRecoveryPointToSnapshot
  where
  hashWithSalt
    _salt
    ConvertRecoveryPointToSnapshot' {..} =
      _salt `Prelude.hashWithSalt` retentionPeriod
        `Prelude.hashWithSalt` recoveryPointId
        `Prelude.hashWithSalt` snapshotName

instance
  Prelude.NFData
    ConvertRecoveryPointToSnapshot
  where
  rnf ConvertRecoveryPointToSnapshot' {..} =
    Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf recoveryPointId
      `Prelude.seq` Prelude.rnf snapshotName

instance
  Core.ToHeaders
    ConvertRecoveryPointToSnapshot
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RedshiftServerless.ConvertRecoveryPointToSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ConvertRecoveryPointToSnapshot where
  toJSON ConvertRecoveryPointToSnapshot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("retentionPeriod" Core..=)
              Prelude.<$> retentionPeriod,
            Prelude.Just
              ("recoveryPointId" Core..= recoveryPointId),
            Prelude.Just ("snapshotName" Core..= snapshotName)
          ]
      )

instance Core.ToPath ConvertRecoveryPointToSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery ConvertRecoveryPointToSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newConvertRecoveryPointToSnapshotResponse' smart constructor.
data ConvertRecoveryPointToSnapshotResponse = ConvertRecoveryPointToSnapshotResponse'
  { -- | The snapshot converted from the recovery point.
    snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConvertRecoveryPointToSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'convertRecoveryPointToSnapshotResponse_snapshot' - The snapshot converted from the recovery point.
--
-- 'httpStatus', 'convertRecoveryPointToSnapshotResponse_httpStatus' - The response's http status code.
newConvertRecoveryPointToSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConvertRecoveryPointToSnapshotResponse
newConvertRecoveryPointToSnapshotResponse
  pHttpStatus_ =
    ConvertRecoveryPointToSnapshotResponse'
      { snapshot =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The snapshot converted from the recovery point.
convertRecoveryPointToSnapshotResponse_snapshot :: Lens.Lens' ConvertRecoveryPointToSnapshotResponse (Prelude.Maybe Snapshot)
convertRecoveryPointToSnapshotResponse_snapshot = Lens.lens (\ConvertRecoveryPointToSnapshotResponse' {snapshot} -> snapshot) (\s@ConvertRecoveryPointToSnapshotResponse' {} a -> s {snapshot = a} :: ConvertRecoveryPointToSnapshotResponse)

-- | The response's http status code.
convertRecoveryPointToSnapshotResponse_httpStatus :: Lens.Lens' ConvertRecoveryPointToSnapshotResponse Prelude.Int
convertRecoveryPointToSnapshotResponse_httpStatus = Lens.lens (\ConvertRecoveryPointToSnapshotResponse' {httpStatus} -> httpStatus) (\s@ConvertRecoveryPointToSnapshotResponse' {} a -> s {httpStatus = a} :: ConvertRecoveryPointToSnapshotResponse)

instance
  Prelude.NFData
    ConvertRecoveryPointToSnapshotResponse
  where
  rnf ConvertRecoveryPointToSnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus
