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
-- Module      : Amazonka.FSx.Types.DiskIopsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DiskIopsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.DiskIopsConfigurationMode
import qualified Amazonka.Prelude as Prelude

-- | The SSD IOPS (input\/output operations per second) configuration for an
-- Amazon FSx for NetApp ONTAP or Amazon FSx for OpenZFS file system. The
-- default is 3 IOPS per GB of storage capacity, but you can provision
-- additional IOPS per GB of storage. The configuration consists of the
-- total number of provisioned SSD IOPS and how the amount was provisioned
-- (by the customer or by the system).
--
-- /See:/ 'newDiskIopsConfiguration' smart constructor.
data DiskIopsConfiguration = DiskIopsConfiguration'
  { -- | The total number of SSD IOPS provisioned for the file system.
    iops :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether the number of IOPS for the file system is using the
    -- system default (@AUTOMATIC@) or was provisioned by the customer
    -- (@USER_PROVISIONED@).
    mode :: Prelude.Maybe DiskIopsConfigurationMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiskIopsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iops', 'diskIopsConfiguration_iops' - The total number of SSD IOPS provisioned for the file system.
--
-- 'mode', 'diskIopsConfiguration_mode' - Specifies whether the number of IOPS for the file system is using the
-- system default (@AUTOMATIC@) or was provisioned by the customer
-- (@USER_PROVISIONED@).
newDiskIopsConfiguration ::
  DiskIopsConfiguration
newDiskIopsConfiguration =
  DiskIopsConfiguration'
    { iops = Prelude.Nothing,
      mode = Prelude.Nothing
    }

-- | The total number of SSD IOPS provisioned for the file system.
diskIopsConfiguration_iops :: Lens.Lens' DiskIopsConfiguration (Prelude.Maybe Prelude.Natural)
diskIopsConfiguration_iops = Lens.lens (\DiskIopsConfiguration' {iops} -> iops) (\s@DiskIopsConfiguration' {} a -> s {iops = a} :: DiskIopsConfiguration)

-- | Specifies whether the number of IOPS for the file system is using the
-- system default (@AUTOMATIC@) or was provisioned by the customer
-- (@USER_PROVISIONED@).
diskIopsConfiguration_mode :: Lens.Lens' DiskIopsConfiguration (Prelude.Maybe DiskIopsConfigurationMode)
diskIopsConfiguration_mode = Lens.lens (\DiskIopsConfiguration' {mode} -> mode) (\s@DiskIopsConfiguration' {} a -> s {mode = a} :: DiskIopsConfiguration)

instance Data.FromJSON DiskIopsConfiguration where
  parseJSON =
    Data.withObject
      "DiskIopsConfiguration"
      ( \x ->
          DiskIopsConfiguration'
            Prelude.<$> (x Data..:? "Iops") Prelude.<*> (x Data..:? "Mode")
      )

instance Prelude.Hashable DiskIopsConfiguration where
  hashWithSalt _salt DiskIopsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` mode

instance Prelude.NFData DiskIopsConfiguration where
  rnf DiskIopsConfiguration' {..} =
    Prelude.rnf iops `Prelude.seq` Prelude.rnf mode

instance Data.ToJSON DiskIopsConfiguration where
  toJSON DiskIopsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Iops" Data..=) Prelude.<$> iops,
            ("Mode" Data..=) Prelude.<$> mode
          ]
      )
