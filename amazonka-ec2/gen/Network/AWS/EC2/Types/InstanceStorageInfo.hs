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
-- Module      : Network.AWS.EC2.Types.InstanceStorageInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStorageInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DiskInfo
import Network.AWS.EC2.Types.EphemeralNvmeSupport
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the disks that are available for the instance type.
--
-- /See:/ 'newInstanceStorageInfo' smart constructor.
data InstanceStorageInfo = InstanceStorageInfo'
  { -- | Indicates whether non-volatile memory express (NVMe) is supported for
    -- instance store.
    nvmeSupport :: Prelude.Maybe EphemeralNvmeSupport,
    -- | The total size of the disks, in GB.
    totalSizeInGB :: Prelude.Maybe Prelude.Integer,
    -- | Describes the disks that are available for the instance type.
    disks :: Prelude.Maybe [DiskInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceStorageInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nvmeSupport', 'instanceStorageInfo_nvmeSupport' - Indicates whether non-volatile memory express (NVMe) is supported for
-- instance store.
--
-- 'totalSizeInGB', 'instanceStorageInfo_totalSizeInGB' - The total size of the disks, in GB.
--
-- 'disks', 'instanceStorageInfo_disks' - Describes the disks that are available for the instance type.
newInstanceStorageInfo ::
  InstanceStorageInfo
newInstanceStorageInfo =
  InstanceStorageInfo'
    { nvmeSupport = Prelude.Nothing,
      totalSizeInGB = Prelude.Nothing,
      disks = Prelude.Nothing
    }

-- | Indicates whether non-volatile memory express (NVMe) is supported for
-- instance store.
instanceStorageInfo_nvmeSupport :: Lens.Lens' InstanceStorageInfo (Prelude.Maybe EphemeralNvmeSupport)
instanceStorageInfo_nvmeSupport = Lens.lens (\InstanceStorageInfo' {nvmeSupport} -> nvmeSupport) (\s@InstanceStorageInfo' {} a -> s {nvmeSupport = a} :: InstanceStorageInfo)

-- | The total size of the disks, in GB.
instanceStorageInfo_totalSizeInGB :: Lens.Lens' InstanceStorageInfo (Prelude.Maybe Prelude.Integer)
instanceStorageInfo_totalSizeInGB = Lens.lens (\InstanceStorageInfo' {totalSizeInGB} -> totalSizeInGB) (\s@InstanceStorageInfo' {} a -> s {totalSizeInGB = a} :: InstanceStorageInfo)

-- | Describes the disks that are available for the instance type.
instanceStorageInfo_disks :: Lens.Lens' InstanceStorageInfo (Prelude.Maybe [DiskInfo])
instanceStorageInfo_disks = Lens.lens (\InstanceStorageInfo' {disks} -> disks) (\s@InstanceStorageInfo' {} a -> s {disks = a} :: InstanceStorageInfo) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML InstanceStorageInfo where
  parseXML x =
    InstanceStorageInfo'
      Prelude.<$> (x Prelude..@? "nvmeSupport")
      Prelude.<*> (x Prelude..@? "totalSizeInGB")
      Prelude.<*> ( x Prelude..@? "disks" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable InstanceStorageInfo

instance Prelude.NFData InstanceStorageInfo
