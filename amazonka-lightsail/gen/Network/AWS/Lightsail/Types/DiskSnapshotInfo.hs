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
-- Module      : Network.AWS.Lightsail.Types.DiskSnapshotInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskSnapshotInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a disk snapshot.
--
-- /See:/ 'newDiskSnapshotInfo' smart constructor.
data DiskSnapshotInfo = DiskSnapshotInfo'
  { -- | The size of the disk in GB (e.g., @32@).
    sizeInGb :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DiskSnapshotInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInGb', 'diskSnapshotInfo_sizeInGb' - The size of the disk in GB (e.g., @32@).
newDiskSnapshotInfo ::
  DiskSnapshotInfo
newDiskSnapshotInfo =
  DiskSnapshotInfo' {sizeInGb = Core.Nothing}

-- | The size of the disk in GB (e.g., @32@).
diskSnapshotInfo_sizeInGb :: Lens.Lens' DiskSnapshotInfo (Core.Maybe Core.Int)
diskSnapshotInfo_sizeInGb = Lens.lens (\DiskSnapshotInfo' {sizeInGb} -> sizeInGb) (\s@DiskSnapshotInfo' {} a -> s {sizeInGb = a} :: DiskSnapshotInfo)

instance Core.FromJSON DiskSnapshotInfo where
  parseJSON =
    Core.withObject
      "DiskSnapshotInfo"
      ( \x ->
          DiskSnapshotInfo' Core.<$> (x Core..:? "sizeInGb")
      )

instance Core.Hashable DiskSnapshotInfo

instance Core.NFData DiskSnapshotInfo
