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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    shareId :: Prelude.Maybe Prelude.Text,
    sharedBy :: Prelude.Maybe Prelude.Text,
    sharedWith :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ShareStatus,
    workloadId :: Prelude.Maybe Prelude.Text,
    workloadName :: Prelude.Maybe Prelude.Text
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
-- 'shareId', 'workloadShare_shareId' - Undocumented member.
--
-- 'sharedBy', 'workloadShare_sharedBy' - Undocumented member.
--
-- 'sharedWith', 'workloadShare_sharedWith' - Undocumented member.
--
-- 'status', 'workloadShare_status' - Undocumented member.
--
-- 'workloadId', 'workloadShare_workloadId' - Undocumented member.
--
-- 'workloadName', 'workloadShare_workloadName' - Undocumented member.
newWorkloadShare ::
  WorkloadShare
newWorkloadShare =
  WorkloadShare'
    { permissionType = Prelude.Nothing,
      shareId = Prelude.Nothing,
      sharedBy = Prelude.Nothing,
      sharedWith = Prelude.Nothing,
      status = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      workloadName = Prelude.Nothing
    }

-- | Undocumented member.
workloadShare_permissionType :: Lens.Lens' WorkloadShare (Prelude.Maybe PermissionType)
workloadShare_permissionType = Lens.lens (\WorkloadShare' {permissionType} -> permissionType) (\s@WorkloadShare' {} a -> s {permissionType = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_shareId :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_shareId = Lens.lens (\WorkloadShare' {shareId} -> shareId) (\s@WorkloadShare' {} a -> s {shareId = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_sharedBy :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_sharedBy = Lens.lens (\WorkloadShare' {sharedBy} -> sharedBy) (\s@WorkloadShare' {} a -> s {sharedBy = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_sharedWith :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_sharedWith = Lens.lens (\WorkloadShare' {sharedWith} -> sharedWith) (\s@WorkloadShare' {} a -> s {sharedWith = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_status :: Lens.Lens' WorkloadShare (Prelude.Maybe ShareStatus)
workloadShare_status = Lens.lens (\WorkloadShare' {status} -> status) (\s@WorkloadShare' {} a -> s {status = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_workloadId :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_workloadId = Lens.lens (\WorkloadShare' {workloadId} -> workloadId) (\s@WorkloadShare' {} a -> s {workloadId = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_workloadName :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_workloadName = Lens.lens (\WorkloadShare' {workloadName} -> workloadName) (\s@WorkloadShare' {} a -> s {workloadName = a} :: WorkloadShare)

instance Data.FromJSON WorkloadShare where
  parseJSON =
    Data.withObject
      "WorkloadShare"
      ( \x ->
          WorkloadShare'
            Prelude.<$> (x Data..:? "PermissionType")
            Prelude.<*> (x Data..:? "ShareId")
            Prelude.<*> (x Data..:? "SharedBy")
            Prelude.<*> (x Data..:? "SharedWith")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "WorkloadId")
            Prelude.<*> (x Data..:? "WorkloadName")
      )

instance Prelude.Hashable WorkloadShare where
  hashWithSalt _salt WorkloadShare' {..} =
    _salt
      `Prelude.hashWithSalt` permissionType
      `Prelude.hashWithSalt` shareId
      `Prelude.hashWithSalt` sharedBy
      `Prelude.hashWithSalt` sharedWith
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` workloadName

instance Prelude.NFData WorkloadShare where
  rnf WorkloadShare' {..} =
    Prelude.rnf permissionType
      `Prelude.seq` Prelude.rnf shareId
      `Prelude.seq` Prelude.rnf sharedBy
      `Prelude.seq` Prelude.rnf sharedWith
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf workloadName
