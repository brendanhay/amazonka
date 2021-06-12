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
-- Module      : Network.AWS.Lightsail.Types.DiskInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a disk.
--
-- /See:/ 'newDiskInfo' smart constructor.
data DiskInfo = DiskInfo'
  { -- | The size of the disk in GB (e.g., @32@).
    sizeInGb :: Core.Maybe Core.Int,
    -- | The disk name.
    name :: Core.Maybe Core.Text,
    -- | The disk path.
    path :: Core.Maybe Core.Text,
    -- | A Boolean value indicating whether this disk is a system disk (has an
    -- operating system loaded on it).
    isSystemDisk :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DiskInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInGb', 'diskInfo_sizeInGb' - The size of the disk in GB (e.g., @32@).
--
-- 'name', 'diskInfo_name' - The disk name.
--
-- 'path', 'diskInfo_path' - The disk path.
--
-- 'isSystemDisk', 'diskInfo_isSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an
-- operating system loaded on it).
newDiskInfo ::
  DiskInfo
newDiskInfo =
  DiskInfo'
    { sizeInGb = Core.Nothing,
      name = Core.Nothing,
      path = Core.Nothing,
      isSystemDisk = Core.Nothing
    }

-- | The size of the disk in GB (e.g., @32@).
diskInfo_sizeInGb :: Lens.Lens' DiskInfo (Core.Maybe Core.Int)
diskInfo_sizeInGb = Lens.lens (\DiskInfo' {sizeInGb} -> sizeInGb) (\s@DiskInfo' {} a -> s {sizeInGb = a} :: DiskInfo)

-- | The disk name.
diskInfo_name :: Lens.Lens' DiskInfo (Core.Maybe Core.Text)
diskInfo_name = Lens.lens (\DiskInfo' {name} -> name) (\s@DiskInfo' {} a -> s {name = a} :: DiskInfo)

-- | The disk path.
diskInfo_path :: Lens.Lens' DiskInfo (Core.Maybe Core.Text)
diskInfo_path = Lens.lens (\DiskInfo' {path} -> path) (\s@DiskInfo' {} a -> s {path = a} :: DiskInfo)

-- | A Boolean value indicating whether this disk is a system disk (has an
-- operating system loaded on it).
diskInfo_isSystemDisk :: Lens.Lens' DiskInfo (Core.Maybe Core.Bool)
diskInfo_isSystemDisk = Lens.lens (\DiskInfo' {isSystemDisk} -> isSystemDisk) (\s@DiskInfo' {} a -> s {isSystemDisk = a} :: DiskInfo)

instance Core.FromJSON DiskInfo where
  parseJSON =
    Core.withObject
      "DiskInfo"
      ( \x ->
          DiskInfo'
            Core.<$> (x Core..:? "sizeInGb")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "path")
            Core.<*> (x Core..:? "isSystemDisk")
      )

instance Core.Hashable DiskInfo

instance Core.NFData DiskInfo
