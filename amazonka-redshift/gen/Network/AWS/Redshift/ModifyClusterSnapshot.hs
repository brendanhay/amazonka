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
-- Module      : Network.AWS.Redshift.ModifyClusterSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a snapshot.
--
-- This exanmple modifies the manual retention period setting for a cluster
-- snapshot.
module Network.AWS.Redshift.ModifyClusterSnapshot
  ( -- * Creating a Request
    ModifyClusterSnapshot (..),
    newModifyClusterSnapshot,

    -- * Request Lenses
    modifyClusterSnapshot_force,
    modifyClusterSnapshot_manualSnapshotRetentionPeriod,
    modifyClusterSnapshot_snapshotIdentifier,

    -- * Destructuring the Response
    ModifyClusterSnapshotResponse (..),
    newModifyClusterSnapshotResponse,

    -- * Response Lenses
    modifyClusterSnapshotResponse_snapshot,
    modifyClusterSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyClusterSnapshot' smart constructor.
data ModifyClusterSnapshot = ModifyClusterSnapshot'
  { -- | A Boolean option to override an exception if the retention period has
    -- already passed.
    force :: Core.Maybe Core.Bool,
    -- | The number of days that a manual snapshot is retained. If the value is
    -- -1, the manual snapshot is retained indefinitely.
    --
    -- If the manual snapshot falls outside of the new retention period, you
    -- can specify the force option to immediately delete the snapshot.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The identifier of the snapshot whose setting you want to modify.
    snapshotIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'modifyClusterSnapshot_force' - A Boolean option to override an exception if the retention period has
-- already passed.
--
-- 'manualSnapshotRetentionPeriod', 'modifyClusterSnapshot_manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- If the manual snapshot falls outside of the new retention period, you
-- can specify the force option to immediately delete the snapshot.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- 'snapshotIdentifier', 'modifyClusterSnapshot_snapshotIdentifier' - The identifier of the snapshot whose setting you want to modify.
newModifyClusterSnapshot ::
  -- | 'snapshotIdentifier'
  Core.Text ->
  ModifyClusterSnapshot
newModifyClusterSnapshot pSnapshotIdentifier_ =
  ModifyClusterSnapshot'
    { force = Core.Nothing,
      manualSnapshotRetentionPeriod = Core.Nothing,
      snapshotIdentifier = pSnapshotIdentifier_
    }

-- | A Boolean option to override an exception if the retention period has
-- already passed.
modifyClusterSnapshot_force :: Lens.Lens' ModifyClusterSnapshot (Core.Maybe Core.Bool)
modifyClusterSnapshot_force = Lens.lens (\ModifyClusterSnapshot' {force} -> force) (\s@ModifyClusterSnapshot' {} a -> s {force = a} :: ModifyClusterSnapshot)

-- | The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- If the manual snapshot falls outside of the new retention period, you
-- can specify the force option to immediately delete the snapshot.
--
-- The value must be either -1 or an integer between 1 and 3,653.
modifyClusterSnapshot_manualSnapshotRetentionPeriod :: Lens.Lens' ModifyClusterSnapshot (Core.Maybe Core.Int)
modifyClusterSnapshot_manualSnapshotRetentionPeriod = Lens.lens (\ModifyClusterSnapshot' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@ModifyClusterSnapshot' {} a -> s {manualSnapshotRetentionPeriod = a} :: ModifyClusterSnapshot)

-- | The identifier of the snapshot whose setting you want to modify.
modifyClusterSnapshot_snapshotIdentifier :: Lens.Lens' ModifyClusterSnapshot Core.Text
modifyClusterSnapshot_snapshotIdentifier = Lens.lens (\ModifyClusterSnapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@ModifyClusterSnapshot' {} a -> s {snapshotIdentifier = a} :: ModifyClusterSnapshot)

instance Core.AWSRequest ModifyClusterSnapshot where
  type
    AWSResponse ModifyClusterSnapshot =
      ModifyClusterSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyClusterSnapshotResult"
      ( \s h x ->
          ModifyClusterSnapshotResponse'
            Core.<$> (x Core..@? "Snapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyClusterSnapshot

instance Core.NFData ModifyClusterSnapshot

instance Core.ToHeaders ModifyClusterSnapshot where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyClusterSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery ModifyClusterSnapshot where
  toQuery ModifyClusterSnapshot' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyClusterSnapshot" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Force" Core.=: force,
        "ManualSnapshotRetentionPeriod"
          Core.=: manualSnapshotRetentionPeriod,
        "SnapshotIdentifier" Core.=: snapshotIdentifier
      ]

-- | /See:/ 'newModifyClusterSnapshotResponse' smart constructor.
data ModifyClusterSnapshotResponse = ModifyClusterSnapshotResponse'
  { snapshot :: Core.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyClusterSnapshotResponse
newModifyClusterSnapshotResponse pHttpStatus_ =
  ModifyClusterSnapshotResponse'
    { snapshot =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterSnapshotResponse_snapshot :: Lens.Lens' ModifyClusterSnapshotResponse (Core.Maybe Snapshot)
modifyClusterSnapshotResponse_snapshot = Lens.lens (\ModifyClusterSnapshotResponse' {snapshot} -> snapshot) (\s@ModifyClusterSnapshotResponse' {} a -> s {snapshot = a} :: ModifyClusterSnapshotResponse)

-- | The response's http status code.
modifyClusterSnapshotResponse_httpStatus :: Lens.Lens' ModifyClusterSnapshotResponse Core.Int
modifyClusterSnapshotResponse_httpStatus = Lens.lens (\ModifyClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterSnapshotResponse' {} a -> s {httpStatus = a} :: ModifyClusterSnapshotResponse)

instance Core.NFData ModifyClusterSnapshotResponse
