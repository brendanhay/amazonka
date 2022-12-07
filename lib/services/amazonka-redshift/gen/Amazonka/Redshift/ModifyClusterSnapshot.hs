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
-- Module      : Amazonka.Redshift.ModifyClusterSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a snapshot.
--
-- This exanmple modifies the manual retention period setting for a cluster
-- snapshot.
module Amazonka.Redshift.ModifyClusterSnapshot
  ( -- * Creating a Request
    ModifyClusterSnapshot (..),
    newModifyClusterSnapshot,

    -- * Request Lenses
    modifyClusterSnapshot_manualSnapshotRetentionPeriod,
    modifyClusterSnapshot_force,
    modifyClusterSnapshot_snapshotIdentifier,

    -- * Destructuring the Response
    ModifyClusterSnapshotResponse (..),
    newModifyClusterSnapshotResponse,

    -- * Response Lenses
    modifyClusterSnapshotResponse_snapshot,
    modifyClusterSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyClusterSnapshot' smart constructor.
data ModifyClusterSnapshot = ModifyClusterSnapshot'
  { -- | The number of days that a manual snapshot is retained. If the value is
    -- -1, the manual snapshot is retained indefinitely.
    --
    -- If the manual snapshot falls outside of the new retention period, you
    -- can specify the force option to immediately delete the snapshot.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A Boolean option to override an exception if the retention period has
    -- already passed.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the snapshot whose setting you want to modify.
    snapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manualSnapshotRetentionPeriod', 'modifyClusterSnapshot_manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- If the manual snapshot falls outside of the new retention period, you
-- can specify the force option to immediately delete the snapshot.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- 'force', 'modifyClusterSnapshot_force' - A Boolean option to override an exception if the retention period has
-- already passed.
--
-- 'snapshotIdentifier', 'modifyClusterSnapshot_snapshotIdentifier' - The identifier of the snapshot whose setting you want to modify.
newModifyClusterSnapshot ::
  -- | 'snapshotIdentifier'
  Prelude.Text ->
  ModifyClusterSnapshot
newModifyClusterSnapshot pSnapshotIdentifier_ =
  ModifyClusterSnapshot'
    { manualSnapshotRetentionPeriod =
        Prelude.Nothing,
      force = Prelude.Nothing,
      snapshotIdentifier = pSnapshotIdentifier_
    }

-- | The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- If the manual snapshot falls outside of the new retention period, you
-- can specify the force option to immediately delete the snapshot.
--
-- The value must be either -1 or an integer between 1 and 3,653.
modifyClusterSnapshot_manualSnapshotRetentionPeriod :: Lens.Lens' ModifyClusterSnapshot (Prelude.Maybe Prelude.Int)
modifyClusterSnapshot_manualSnapshotRetentionPeriod = Lens.lens (\ModifyClusterSnapshot' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@ModifyClusterSnapshot' {} a -> s {manualSnapshotRetentionPeriod = a} :: ModifyClusterSnapshot)

-- | A Boolean option to override an exception if the retention period has
-- already passed.
modifyClusterSnapshot_force :: Lens.Lens' ModifyClusterSnapshot (Prelude.Maybe Prelude.Bool)
modifyClusterSnapshot_force = Lens.lens (\ModifyClusterSnapshot' {force} -> force) (\s@ModifyClusterSnapshot' {} a -> s {force = a} :: ModifyClusterSnapshot)

-- | The identifier of the snapshot whose setting you want to modify.
modifyClusterSnapshot_snapshotIdentifier :: Lens.Lens' ModifyClusterSnapshot Prelude.Text
modifyClusterSnapshot_snapshotIdentifier = Lens.lens (\ModifyClusterSnapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@ModifyClusterSnapshot' {} a -> s {snapshotIdentifier = a} :: ModifyClusterSnapshot)

instance Core.AWSRequest ModifyClusterSnapshot where
  type
    AWSResponse ModifyClusterSnapshot =
      ModifyClusterSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyClusterSnapshotResult"
      ( \s h x ->
          ModifyClusterSnapshotResponse'
            Prelude.<$> (x Data..@? "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyClusterSnapshot where
  hashWithSalt _salt ModifyClusterSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` manualSnapshotRetentionPeriod
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` snapshotIdentifier

instance Prelude.NFData ModifyClusterSnapshot where
  rnf ModifyClusterSnapshot' {..} =
    Prelude.rnf manualSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf force
      `Prelude.seq` Prelude.rnf snapshotIdentifier

instance Data.ToHeaders ModifyClusterSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyClusterSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyClusterSnapshot where
  toQuery ModifyClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyClusterSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Data.=: manualSnapshotRetentionPeriod,
        "Force" Data.=: force,
        "SnapshotIdentifier" Data.=: snapshotIdentifier
      ]

-- | /See:/ 'newModifyClusterSnapshotResponse' smart constructor.
data ModifyClusterSnapshotResponse = ModifyClusterSnapshotResponse'
  { snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'modifyClusterSnapshotResponse_snapshot' - Undocumented member.
--
-- 'httpStatus', 'modifyClusterSnapshotResponse_httpStatus' - The response's http status code.
newModifyClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyClusterSnapshotResponse
newModifyClusterSnapshotResponse pHttpStatus_ =
  ModifyClusterSnapshotResponse'
    { snapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterSnapshotResponse_snapshot :: Lens.Lens' ModifyClusterSnapshotResponse (Prelude.Maybe Snapshot)
modifyClusterSnapshotResponse_snapshot = Lens.lens (\ModifyClusterSnapshotResponse' {snapshot} -> snapshot) (\s@ModifyClusterSnapshotResponse' {} a -> s {snapshot = a} :: ModifyClusterSnapshotResponse)

-- | The response's http status code.
modifyClusterSnapshotResponse_httpStatus :: Lens.Lens' ModifyClusterSnapshotResponse Prelude.Int
modifyClusterSnapshotResponse_httpStatus = Lens.lens (\ModifyClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterSnapshotResponse' {} a -> s {httpStatus = a} :: ModifyClusterSnapshotResponse)

instance Prelude.NFData ModifyClusterSnapshotResponse where
  rnf ModifyClusterSnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus
