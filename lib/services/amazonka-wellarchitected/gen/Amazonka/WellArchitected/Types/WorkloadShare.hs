{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WellArchitected.Types.WorkloadShare
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.WorkloadShare where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.PermissionType
import Amazonka.WellArchitected.Types.ShareStatus

-- | A workload share return object.
--
-- /See:/ 'newWorkloadShare' smart constructor.
data WorkloadShare = WorkloadShare'
  { permissionType :: Prelude.Maybe PermissionType,
    sharedWith :: Prelude.Maybe Prelude.Text,
    workloadName :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ShareStatus,
    shareId :: Prelude.Maybe Prelude.Text,
    sharedBy :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkloadShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionType', 'workloadShare_permissionType' - Undocumented member.
--
-- 'sharedWith', 'workloadShare_sharedWith' - Undocumented member.
--
-- 'workloadName', 'workloadShare_workloadName' - Undocumented member.
--
-- 'status', 'workloadShare_status' - Undocumented member.
--
-- 'shareId', 'workloadShare_shareId' - Undocumented member.
--
-- 'sharedBy', 'workloadShare_sharedBy' - Undocumented member.
--
-- 'workloadId', 'workloadShare_workloadId' - Undocumented member.
newWorkloadShare ::
  WorkloadShare
newWorkloadShare =
  WorkloadShare'
    { permissionType = Prelude.Nothing,
      sharedWith = Prelude.Nothing,
      workloadName = Prelude.Nothing,
      status = Prelude.Nothing,
      shareId = Prelude.Nothing,
      sharedBy = Prelude.Nothing,
      workloadId = Prelude.Nothing
    }

-- | Undocumented member.
workloadShare_permissionType :: Lens.Lens' WorkloadShare (Prelude.Maybe PermissionType)
workloadShare_permissionType = Lens.lens (\WorkloadShare' {permissionType} -> permissionType) (\s@WorkloadShare' {} a -> s {permissionType = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_sharedWith :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_sharedWith = Lens.lens (\WorkloadShare' {sharedWith} -> sharedWith) (\s@WorkloadShare' {} a -> s {sharedWith = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_workloadName :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_workloadName = Lens.lens (\WorkloadShare' {workloadName} -> workloadName) (\s@WorkloadShare' {} a -> s {workloadName = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_status :: Lens.Lens' WorkloadShare (Prelude.Maybe ShareStatus)
workloadShare_status = Lens.lens (\WorkloadShare' {status} -> status) (\s@WorkloadShare' {} a -> s {status = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_shareId :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_shareId = Lens.lens (\WorkloadShare' {shareId} -> shareId) (\s@WorkloadShare' {} a -> s {shareId = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_sharedBy :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_sharedBy = Lens.lens (\WorkloadShare' {sharedBy} -> sharedBy) (\s@WorkloadShare' {} a -> s {sharedBy = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_workloadId :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_workloadId = Lens.lens (\WorkloadShare' {workloadId} -> workloadId) (\s@WorkloadShare' {} a -> s {workloadId = a} :: WorkloadShare)

instance Data.FromJSON WorkloadShare where
  parseJSON =
    Data.withObject
      "WorkloadShare"
      ( \x ->
          WorkloadShare'
            Prelude.<$> (x Data..:? "PermissionType")
            Prelude.<*> (x Data..:? "SharedWith")
            Prelude.<*> (x Data..:? "WorkloadName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "ShareId")
            Prelude.<*> (x Data..:? "SharedBy")
            Prelude.<*> (x Data..:? "WorkloadId")
      )

instance Prelude.Hashable WorkloadShare where
  hashWithSalt _salt WorkloadShare' {..} =
    _salt `Prelude.hashWithSalt` permissionType
      `Prelude.hashWithSalt` sharedWith
      `Prelude.hashWithSalt` workloadName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` shareId
      `Prelude.hashWithSalt` sharedBy
      `Prelude.hashWithSalt` workloadId

instance Prelude.NFData WorkloadShare where
  rnf WorkloadShare' {..} =
    Prelude.rnf permissionType
      `Prelude.seq` Prelude.rnf sharedWith
      `Prelude.seq` Prelude.rnf workloadName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf shareId
      `Prelude.seq` Prelude.rnf sharedBy
      `Prelude.seq` Prelude.rnf workloadId
