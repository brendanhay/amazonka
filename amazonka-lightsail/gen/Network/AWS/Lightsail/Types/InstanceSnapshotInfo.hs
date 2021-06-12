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
-- Module      : Network.AWS.Lightsail.Types.InstanceSnapshotInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceSnapshotInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DiskInfo

-- | Describes an instance snapshot.
--
-- /See:/ 'newInstanceSnapshotInfo' smart constructor.
data InstanceSnapshotInfo = InstanceSnapshotInfo'
  { -- | A list of objects describing the disks that were attached to the source
    -- instance.
    fromDiskInfo :: Core.Maybe [DiskInfo],
    -- | The bundle ID from which the source instance was created (e.g.,
    -- @micro_1_0@).
    fromBundleId :: Core.Maybe Core.Text,
    -- | The blueprint ID from which the source instance (e.g., @os_debian_8_3@).
    fromBlueprintId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceSnapshotInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromDiskInfo', 'instanceSnapshotInfo_fromDiskInfo' - A list of objects describing the disks that were attached to the source
-- instance.
--
-- 'fromBundleId', 'instanceSnapshotInfo_fromBundleId' - The bundle ID from which the source instance was created (e.g.,
-- @micro_1_0@).
--
-- 'fromBlueprintId', 'instanceSnapshotInfo_fromBlueprintId' - The blueprint ID from which the source instance (e.g., @os_debian_8_3@).
newInstanceSnapshotInfo ::
  InstanceSnapshotInfo
newInstanceSnapshotInfo =
  InstanceSnapshotInfo'
    { fromDiskInfo = Core.Nothing,
      fromBundleId = Core.Nothing,
      fromBlueprintId = Core.Nothing
    }

-- | A list of objects describing the disks that were attached to the source
-- instance.
instanceSnapshotInfo_fromDiskInfo :: Lens.Lens' InstanceSnapshotInfo (Core.Maybe [DiskInfo])
instanceSnapshotInfo_fromDiskInfo = Lens.lens (\InstanceSnapshotInfo' {fromDiskInfo} -> fromDiskInfo) (\s@InstanceSnapshotInfo' {} a -> s {fromDiskInfo = a} :: InstanceSnapshotInfo) Core.. Lens.mapping Lens._Coerce

-- | The bundle ID from which the source instance was created (e.g.,
-- @micro_1_0@).
instanceSnapshotInfo_fromBundleId :: Lens.Lens' InstanceSnapshotInfo (Core.Maybe Core.Text)
instanceSnapshotInfo_fromBundleId = Lens.lens (\InstanceSnapshotInfo' {fromBundleId} -> fromBundleId) (\s@InstanceSnapshotInfo' {} a -> s {fromBundleId = a} :: InstanceSnapshotInfo)

-- | The blueprint ID from which the source instance (e.g., @os_debian_8_3@).
instanceSnapshotInfo_fromBlueprintId :: Lens.Lens' InstanceSnapshotInfo (Core.Maybe Core.Text)
instanceSnapshotInfo_fromBlueprintId = Lens.lens (\InstanceSnapshotInfo' {fromBlueprintId} -> fromBlueprintId) (\s@InstanceSnapshotInfo' {} a -> s {fromBlueprintId = a} :: InstanceSnapshotInfo)

instance Core.FromJSON InstanceSnapshotInfo where
  parseJSON =
    Core.withObject
      "InstanceSnapshotInfo"
      ( \x ->
          InstanceSnapshotInfo'
            Core.<$> (x Core..:? "fromDiskInfo" Core..!= Core.mempty)
            Core.<*> (x Core..:? "fromBundleId")
            Core.<*> (x Core..:? "fromBlueprintId")
      )

instance Core.Hashable InstanceSnapshotInfo

instance Core.NFData InstanceSnapshotInfo
