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
-- Module      : Network.AWS.WellArchitected.Types.WorkloadShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WellArchitected.Types.WorkloadShare where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WellArchitected.Types.PermissionType
import Network.AWS.WellArchitected.Types.ShareStatus

-- | A workload share return object.
--
-- /See:/ 'newWorkloadShare' smart constructor.
data WorkloadShare = WorkloadShare'
  { status :: Prelude.Maybe ShareStatus,
    sharedBy :: Prelude.Maybe Prelude.Text,
    sharedWith :: Prelude.Maybe Prelude.Text,
    permissionType :: Prelude.Maybe PermissionType,
    workloadId :: Prelude.Maybe Prelude.Text,
    workloadName :: Prelude.Maybe Prelude.Text,
    shareId :: Prelude.Maybe Prelude.Text
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
-- 'status', 'workloadShare_status' - Undocumented member.
--
-- 'sharedBy', 'workloadShare_sharedBy' - Undocumented member.
--
-- 'sharedWith', 'workloadShare_sharedWith' - Undocumented member.
--
-- 'permissionType', 'workloadShare_permissionType' - Undocumented member.
--
-- 'workloadId', 'workloadShare_workloadId' - Undocumented member.
--
-- 'workloadName', 'workloadShare_workloadName' - Undocumented member.
--
-- 'shareId', 'workloadShare_shareId' - Undocumented member.
newWorkloadShare ::
  WorkloadShare
newWorkloadShare =
  WorkloadShare'
    { status = Prelude.Nothing,
      sharedBy = Prelude.Nothing,
      sharedWith = Prelude.Nothing,
      permissionType = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      workloadName = Prelude.Nothing,
      shareId = Prelude.Nothing
    }

-- | Undocumented member.
workloadShare_status :: Lens.Lens' WorkloadShare (Prelude.Maybe ShareStatus)
workloadShare_status = Lens.lens (\WorkloadShare' {status} -> status) (\s@WorkloadShare' {} a -> s {status = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_sharedBy :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_sharedBy = Lens.lens (\WorkloadShare' {sharedBy} -> sharedBy) (\s@WorkloadShare' {} a -> s {sharedBy = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_sharedWith :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_sharedWith = Lens.lens (\WorkloadShare' {sharedWith} -> sharedWith) (\s@WorkloadShare' {} a -> s {sharedWith = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_permissionType :: Lens.Lens' WorkloadShare (Prelude.Maybe PermissionType)
workloadShare_permissionType = Lens.lens (\WorkloadShare' {permissionType} -> permissionType) (\s@WorkloadShare' {} a -> s {permissionType = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_workloadId :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_workloadId = Lens.lens (\WorkloadShare' {workloadId} -> workloadId) (\s@WorkloadShare' {} a -> s {workloadId = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_workloadName :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_workloadName = Lens.lens (\WorkloadShare' {workloadName} -> workloadName) (\s@WorkloadShare' {} a -> s {workloadName = a} :: WorkloadShare)

-- | Undocumented member.
workloadShare_shareId :: Lens.Lens' WorkloadShare (Prelude.Maybe Prelude.Text)
workloadShare_shareId = Lens.lens (\WorkloadShare' {shareId} -> shareId) (\s@WorkloadShare' {} a -> s {shareId = a} :: WorkloadShare)

instance Core.FromJSON WorkloadShare where
  parseJSON =
    Core.withObject
      "WorkloadShare"
      ( \x ->
          WorkloadShare'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "SharedBy")
            Prelude.<*> (x Core..:? "SharedWith")
            Prelude.<*> (x Core..:? "PermissionType")
            Prelude.<*> (x Core..:? "WorkloadId")
            Prelude.<*> (x Core..:? "WorkloadName")
            Prelude.<*> (x Core..:? "ShareId")
      )

instance Prelude.Hashable WorkloadShare

instance Prelude.NFData WorkloadShare
