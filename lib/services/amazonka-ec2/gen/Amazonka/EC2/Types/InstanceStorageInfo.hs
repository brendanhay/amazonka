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
-- Module      : Amazonka.EC2.Types.InstanceStorageInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceStorageInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DiskInfo
import Amazonka.EC2.Types.EphemeralNvmeSupport
import Amazonka.EC2.Types.InstanceStorageEncryptionSupport
import qualified Amazonka.Prelude as Prelude

-- | Describes the instance store features that are supported by the instance
-- type.
--
-- /See:/ 'newInstanceStorageInfo' smart constructor.
data InstanceStorageInfo = InstanceStorageInfo'
  { -- | The total size of the disks, in GB.
    totalSizeInGB :: Prelude.Maybe Prelude.Integer,
    -- | Describes the disks that are available for the instance type.
    disks :: Prelude.Maybe [DiskInfo],
    -- | Indicates whether non-volatile memory express (NVMe) is supported.
    nvmeSupport :: Prelude.Maybe EphemeralNvmeSupport,
    -- | Indicates whether data is encrypted at rest.
    encryptionSupport :: Prelude.Maybe InstanceStorageEncryptionSupport
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceStorageInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalSizeInGB', 'instanceStorageInfo_totalSizeInGB' - The total size of the disks, in GB.
--
-- 'disks', 'instanceStorageInfo_disks' - Describes the disks that are available for the instance type.
--
-- 'nvmeSupport', 'instanceStorageInfo_nvmeSupport' - Indicates whether non-volatile memory express (NVMe) is supported.
--
-- 'encryptionSupport', 'instanceStorageInfo_encryptionSupport' - Indicates whether data is encrypted at rest.
newInstanceStorageInfo ::
  InstanceStorageInfo
newInstanceStorageInfo =
  InstanceStorageInfo'
    { totalSizeInGB =
        Prelude.Nothing,
      disks = Prelude.Nothing,
      nvmeSupport = Prelude.Nothing,
      encryptionSupport = Prelude.Nothing
    }

-- | The total size of the disks, in GB.
instanceStorageInfo_totalSizeInGB :: Lens.Lens' InstanceStorageInfo (Prelude.Maybe Prelude.Integer)
instanceStorageInfo_totalSizeInGB = Lens.lens (\InstanceStorageInfo' {totalSizeInGB} -> totalSizeInGB) (\s@InstanceStorageInfo' {} a -> s {totalSizeInGB = a} :: InstanceStorageInfo)

-- | Describes the disks that are available for the instance type.
instanceStorageInfo_disks :: Lens.Lens' InstanceStorageInfo (Prelude.Maybe [DiskInfo])
instanceStorageInfo_disks = Lens.lens (\InstanceStorageInfo' {disks} -> disks) (\s@InstanceStorageInfo' {} a -> s {disks = a} :: InstanceStorageInfo) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether non-volatile memory express (NVMe) is supported.
instanceStorageInfo_nvmeSupport :: Lens.Lens' InstanceStorageInfo (Prelude.Maybe EphemeralNvmeSupport)
instanceStorageInfo_nvmeSupport = Lens.lens (\InstanceStorageInfo' {nvmeSupport} -> nvmeSupport) (\s@InstanceStorageInfo' {} a -> s {nvmeSupport = a} :: InstanceStorageInfo)

-- | Indicates whether data is encrypted at rest.
instanceStorageInfo_encryptionSupport :: Lens.Lens' InstanceStorageInfo (Prelude.Maybe InstanceStorageEncryptionSupport)
instanceStorageInfo_encryptionSupport = Lens.lens (\InstanceStorageInfo' {encryptionSupport} -> encryptionSupport) (\s@InstanceStorageInfo' {} a -> s {encryptionSupport = a} :: InstanceStorageInfo)

instance Data.FromXML InstanceStorageInfo where
  parseXML x =
    InstanceStorageInfo'
      Prelude.<$> (x Data..@? "totalSizeInGB")
      Prelude.<*> ( x Data..@? "disks" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "nvmeSupport")
      Prelude.<*> (x Data..@? "encryptionSupport")

instance Prelude.Hashable InstanceStorageInfo where
  hashWithSalt _salt InstanceStorageInfo' {..} =
    _salt `Prelude.hashWithSalt` totalSizeInGB
      `Prelude.hashWithSalt` disks
      `Prelude.hashWithSalt` nvmeSupport
      `Prelude.hashWithSalt` encryptionSupport

instance Prelude.NFData InstanceStorageInfo where
  rnf InstanceStorageInfo' {..} =
    Prelude.rnf totalSizeInGB
      `Prelude.seq` Prelude.rnf disks
      `Prelude.seq` Prelude.rnf nvmeSupport
      `Prelude.seq` Prelude.rnf encryptionSupport
