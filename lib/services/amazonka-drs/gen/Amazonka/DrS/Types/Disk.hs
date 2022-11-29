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
-- Module      : Amazonka.DrS.Types.Disk
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.Disk where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing a data storage device on a server.
--
-- /See:/ 'newDisk' smart constructor.
data Disk = Disk'
  { -- | The amount of storage on the disk in bytes.
    bytes :: Prelude.Maybe Prelude.Natural,
    -- | The disk or device name.
    deviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Disk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytes', 'disk_bytes' - The amount of storage on the disk in bytes.
--
-- 'deviceName', 'disk_deviceName' - The disk or device name.
newDisk ::
  Disk
newDisk =
  Disk'
    { bytes = Prelude.Nothing,
      deviceName = Prelude.Nothing
    }

-- | The amount of storage on the disk in bytes.
disk_bytes :: Lens.Lens' Disk (Prelude.Maybe Prelude.Natural)
disk_bytes = Lens.lens (\Disk' {bytes} -> bytes) (\s@Disk' {} a -> s {bytes = a} :: Disk)

-- | The disk or device name.
disk_deviceName :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_deviceName = Lens.lens (\Disk' {deviceName} -> deviceName) (\s@Disk' {} a -> s {deviceName = a} :: Disk)

instance Core.FromJSON Disk where
  parseJSON =
    Core.withObject
      "Disk"
      ( \x ->
          Disk'
            Prelude.<$> (x Core..:? "bytes")
            Prelude.<*> (x Core..:? "deviceName")
      )

instance Prelude.Hashable Disk where
  hashWithSalt _salt Disk' {..} =
    _salt `Prelude.hashWithSalt` bytes
      `Prelude.hashWithSalt` deviceName

instance Prelude.NFData Disk where
  rnf Disk' {..} =
    Prelude.rnf bytes
      `Prelude.seq` Prelude.rnf deviceName
