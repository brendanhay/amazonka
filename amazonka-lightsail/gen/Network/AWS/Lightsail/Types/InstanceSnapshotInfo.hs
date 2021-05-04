{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DiskInfo
import qualified Network.AWS.Prelude as Prelude

-- | Describes an instance snapshot.
--
-- /See:/ 'newInstanceSnapshotInfo' smart constructor.
data InstanceSnapshotInfo = InstanceSnapshotInfo'
  { -- | A list of objects describing the disks that were attached to the source
    -- instance.
    fromDiskInfo :: Prelude.Maybe [DiskInfo],
    -- | The bundle ID from which the source instance was created (e.g.,
    -- @micro_1_0@).
    fromBundleId :: Prelude.Maybe Prelude.Text,
    -- | The blueprint ID from which the source instance (e.g., @os_debian_8_3@).
    fromBlueprintId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { fromDiskInfo =
        Prelude.Nothing,
      fromBundleId = Prelude.Nothing,
      fromBlueprintId = Prelude.Nothing
    }

-- | A list of objects describing the disks that were attached to the source
-- instance.
instanceSnapshotInfo_fromDiskInfo :: Lens.Lens' InstanceSnapshotInfo (Prelude.Maybe [DiskInfo])
instanceSnapshotInfo_fromDiskInfo = Lens.lens (\InstanceSnapshotInfo' {fromDiskInfo} -> fromDiskInfo) (\s@InstanceSnapshotInfo' {} a -> s {fromDiskInfo = a} :: InstanceSnapshotInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The bundle ID from which the source instance was created (e.g.,
-- @micro_1_0@).
instanceSnapshotInfo_fromBundleId :: Lens.Lens' InstanceSnapshotInfo (Prelude.Maybe Prelude.Text)
instanceSnapshotInfo_fromBundleId = Lens.lens (\InstanceSnapshotInfo' {fromBundleId} -> fromBundleId) (\s@InstanceSnapshotInfo' {} a -> s {fromBundleId = a} :: InstanceSnapshotInfo)

-- | The blueprint ID from which the source instance (e.g., @os_debian_8_3@).
instanceSnapshotInfo_fromBlueprintId :: Lens.Lens' InstanceSnapshotInfo (Prelude.Maybe Prelude.Text)
instanceSnapshotInfo_fromBlueprintId = Lens.lens (\InstanceSnapshotInfo' {fromBlueprintId} -> fromBlueprintId) (\s@InstanceSnapshotInfo' {} a -> s {fromBlueprintId = a} :: InstanceSnapshotInfo)

instance Prelude.FromJSON InstanceSnapshotInfo where
  parseJSON =
    Prelude.withObject
      "InstanceSnapshotInfo"
      ( \x ->
          InstanceSnapshotInfo'
            Prelude.<$> ( x Prelude..:? "fromDiskInfo"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "fromBundleId")
            Prelude.<*> (x Prelude..:? "fromBlueprintId")
      )

instance Prelude.Hashable InstanceSnapshotInfo

instance Prelude.NFData InstanceSnapshotInfo
