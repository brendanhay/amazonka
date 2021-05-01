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
-- Module      : Network.AWS.EC2.Types.DiskInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DiskType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the disk.
--
-- /See:/ 'newDiskInfo' smart constructor.
data DiskInfo = DiskInfo'
  { -- | The size of the disk in GB.
    sizeInGB :: Prelude.Maybe Prelude.Integer,
    -- | The number of disks with this configuration.
    count :: Prelude.Maybe Prelude.Int,
    -- | The type of disk.
    type' :: Prelude.Maybe DiskType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DiskInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInGB', 'diskInfo_sizeInGB' - The size of the disk in GB.
--
-- 'count', 'diskInfo_count' - The number of disks with this configuration.
--
-- 'type'', 'diskInfo_type' - The type of disk.
newDiskInfo ::
  DiskInfo
newDiskInfo =
  DiskInfo'
    { sizeInGB = Prelude.Nothing,
      count = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The size of the disk in GB.
diskInfo_sizeInGB :: Lens.Lens' DiskInfo (Prelude.Maybe Prelude.Integer)
diskInfo_sizeInGB = Lens.lens (\DiskInfo' {sizeInGB} -> sizeInGB) (\s@DiskInfo' {} a -> s {sizeInGB = a} :: DiskInfo)

-- | The number of disks with this configuration.
diskInfo_count :: Lens.Lens' DiskInfo (Prelude.Maybe Prelude.Int)
diskInfo_count = Lens.lens (\DiskInfo' {count} -> count) (\s@DiskInfo' {} a -> s {count = a} :: DiskInfo)

-- | The type of disk.
diskInfo_type :: Lens.Lens' DiskInfo (Prelude.Maybe DiskType)
diskInfo_type = Lens.lens (\DiskInfo' {type'} -> type') (\s@DiskInfo' {} a -> s {type' = a} :: DiskInfo)

instance Prelude.FromXML DiskInfo where
  parseXML x =
    DiskInfo'
      Prelude.<$> (x Prelude..@? "sizeInGB")
      Prelude.<*> (x Prelude..@? "count")
      Prelude.<*> (x Prelude..@? "type")

instance Prelude.Hashable DiskInfo

instance Prelude.NFData DiskInfo
