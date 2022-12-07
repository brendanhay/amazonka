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
-- Module      : Amazonka.Lightsail.Types.InstanceSnapshotInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstanceSnapshotInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.DiskInfo
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance snapshot.
--
-- /See:/ 'newInstanceSnapshotInfo' smart constructor.
data InstanceSnapshotInfo = InstanceSnapshotInfo'
  { -- | A list of objects describing the disks that were attached to the source
    -- instance.
    fromDiskInfo :: Prelude.Maybe [DiskInfo],
    -- | The blueprint ID from which the source instance (e.g., @os_debian_8_3@).
    fromBlueprintId :: Prelude.Maybe Prelude.Text,
    -- | The bundle ID from which the source instance was created (e.g.,
    -- @micro_1_0@).
    fromBundleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'fromBlueprintId', 'instanceSnapshotInfo_fromBlueprintId' - The blueprint ID from which the source instance (e.g., @os_debian_8_3@).
--
-- 'fromBundleId', 'instanceSnapshotInfo_fromBundleId' - The bundle ID from which the source instance was created (e.g.,
-- @micro_1_0@).
newInstanceSnapshotInfo ::
  InstanceSnapshotInfo
newInstanceSnapshotInfo =
  InstanceSnapshotInfo'
    { fromDiskInfo =
        Prelude.Nothing,
      fromBlueprintId = Prelude.Nothing,
      fromBundleId = Prelude.Nothing
    }

-- | A list of objects describing the disks that were attached to the source
-- instance.
instanceSnapshotInfo_fromDiskInfo :: Lens.Lens' InstanceSnapshotInfo (Prelude.Maybe [DiskInfo])
instanceSnapshotInfo_fromDiskInfo = Lens.lens (\InstanceSnapshotInfo' {fromDiskInfo} -> fromDiskInfo) (\s@InstanceSnapshotInfo' {} a -> s {fromDiskInfo = a} :: InstanceSnapshotInfo) Prelude.. Lens.mapping Lens.coerced

-- | The blueprint ID from which the source instance (e.g., @os_debian_8_3@).
instanceSnapshotInfo_fromBlueprintId :: Lens.Lens' InstanceSnapshotInfo (Prelude.Maybe Prelude.Text)
instanceSnapshotInfo_fromBlueprintId = Lens.lens (\InstanceSnapshotInfo' {fromBlueprintId} -> fromBlueprintId) (\s@InstanceSnapshotInfo' {} a -> s {fromBlueprintId = a} :: InstanceSnapshotInfo)

-- | The bundle ID from which the source instance was created (e.g.,
-- @micro_1_0@).
instanceSnapshotInfo_fromBundleId :: Lens.Lens' InstanceSnapshotInfo (Prelude.Maybe Prelude.Text)
instanceSnapshotInfo_fromBundleId = Lens.lens (\InstanceSnapshotInfo' {fromBundleId} -> fromBundleId) (\s@InstanceSnapshotInfo' {} a -> s {fromBundleId = a} :: InstanceSnapshotInfo)

instance Data.FromJSON InstanceSnapshotInfo where
  parseJSON =
    Data.withObject
      "InstanceSnapshotInfo"
      ( \x ->
          InstanceSnapshotInfo'
            Prelude.<$> (x Data..:? "fromDiskInfo" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "fromBlueprintId")
            Prelude.<*> (x Data..:? "fromBundleId")
      )

instance Prelude.Hashable InstanceSnapshotInfo where
  hashWithSalt _salt InstanceSnapshotInfo' {..} =
    _salt `Prelude.hashWithSalt` fromDiskInfo
      `Prelude.hashWithSalt` fromBlueprintId
      `Prelude.hashWithSalt` fromBundleId

instance Prelude.NFData InstanceSnapshotInfo where
  rnf InstanceSnapshotInfo' {..} =
    Prelude.rnf fromDiskInfo
      `Prelude.seq` Prelude.rnf fromBlueprintId
      `Prelude.seq` Prelude.rnf fromBundleId
