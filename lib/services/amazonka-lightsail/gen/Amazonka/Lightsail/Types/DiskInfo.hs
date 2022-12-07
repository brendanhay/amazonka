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
-- Module      : Amazonka.Lightsail.Types.DiskInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DiskInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a disk.
--
-- /See:/ 'newDiskInfo' smart constructor.
data DiskInfo = DiskInfo'
  { -- | The disk name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The size of the disk in GB (e.g., @32@).
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The disk path.
    path :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether this disk is a system disk (has an
    -- operating system loaded on it).
    isSystemDisk :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiskInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'diskInfo_name' - The disk name.
--
-- 'sizeInGb', 'diskInfo_sizeInGb' - The size of the disk in GB (e.g., @32@).
--
-- 'path', 'diskInfo_path' - The disk path.
--
-- 'isSystemDisk', 'diskInfo_isSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an
-- operating system loaded on it).
newDiskInfo ::
  DiskInfo
newDiskInfo =
  DiskInfo'
    { name = Prelude.Nothing,
      sizeInGb = Prelude.Nothing,
      path = Prelude.Nothing,
      isSystemDisk = Prelude.Nothing
    }

-- | The disk name.
diskInfo_name :: Lens.Lens' DiskInfo (Prelude.Maybe Prelude.Text)
diskInfo_name = Lens.lens (\DiskInfo' {name} -> name) (\s@DiskInfo' {} a -> s {name = a} :: DiskInfo)

-- | The size of the disk in GB (e.g., @32@).
diskInfo_sizeInGb :: Lens.Lens' DiskInfo (Prelude.Maybe Prelude.Int)
diskInfo_sizeInGb = Lens.lens (\DiskInfo' {sizeInGb} -> sizeInGb) (\s@DiskInfo' {} a -> s {sizeInGb = a} :: DiskInfo)

-- | The disk path.
diskInfo_path :: Lens.Lens' DiskInfo (Prelude.Maybe Prelude.Text)
diskInfo_path = Lens.lens (\DiskInfo' {path} -> path) (\s@DiskInfo' {} a -> s {path = a} :: DiskInfo)

-- | A Boolean value indicating whether this disk is a system disk (has an
-- operating system loaded on it).
diskInfo_isSystemDisk :: Lens.Lens' DiskInfo (Prelude.Maybe Prelude.Bool)
diskInfo_isSystemDisk = Lens.lens (\DiskInfo' {isSystemDisk} -> isSystemDisk) (\s@DiskInfo' {} a -> s {isSystemDisk = a} :: DiskInfo)

instance Data.FromJSON DiskInfo where
  parseJSON =
    Data.withObject
      "DiskInfo"
      ( \x ->
          DiskInfo'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "sizeInGb")
            Prelude.<*> (x Data..:? "path")
            Prelude.<*> (x Data..:? "isSystemDisk")
      )

instance Prelude.Hashable DiskInfo where
  hashWithSalt _salt DiskInfo' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sizeInGb
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` isSystemDisk

instance Prelude.NFData DiskInfo where
  rnf DiskInfo' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf sizeInGb
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf isSystemDisk
