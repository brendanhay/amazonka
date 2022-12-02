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
-- Module      : Amazonka.DrS.Types.RecoveryInstanceDisk
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryInstanceDisk where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing a block storage device on the Recovery Instance.
--
-- /See:/ 'newRecoveryInstanceDisk' smart constructor.
data RecoveryInstanceDisk = RecoveryInstanceDisk'
  { -- | The amount of storage on the disk in bytes.
    bytes :: Prelude.Maybe Prelude.Natural,
    -- | The internal device name of this disk. This is the name that is visible
    -- on the machine itself and not from the EC2 console.
    internalDeviceName :: Prelude.Maybe Prelude.Text,
    -- | The EBS Volume ID of this disk.
    ebsVolumeID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryInstanceDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytes', 'recoveryInstanceDisk_bytes' - The amount of storage on the disk in bytes.
--
-- 'internalDeviceName', 'recoveryInstanceDisk_internalDeviceName' - The internal device name of this disk. This is the name that is visible
-- on the machine itself and not from the EC2 console.
--
-- 'ebsVolumeID', 'recoveryInstanceDisk_ebsVolumeID' - The EBS Volume ID of this disk.
newRecoveryInstanceDisk ::
  RecoveryInstanceDisk
newRecoveryInstanceDisk =
  RecoveryInstanceDisk'
    { bytes = Prelude.Nothing,
      internalDeviceName = Prelude.Nothing,
      ebsVolumeID = Prelude.Nothing
    }

-- | The amount of storage on the disk in bytes.
recoveryInstanceDisk_bytes :: Lens.Lens' RecoveryInstanceDisk (Prelude.Maybe Prelude.Natural)
recoveryInstanceDisk_bytes = Lens.lens (\RecoveryInstanceDisk' {bytes} -> bytes) (\s@RecoveryInstanceDisk' {} a -> s {bytes = a} :: RecoveryInstanceDisk)

-- | The internal device name of this disk. This is the name that is visible
-- on the machine itself and not from the EC2 console.
recoveryInstanceDisk_internalDeviceName :: Lens.Lens' RecoveryInstanceDisk (Prelude.Maybe Prelude.Text)
recoveryInstanceDisk_internalDeviceName = Lens.lens (\RecoveryInstanceDisk' {internalDeviceName} -> internalDeviceName) (\s@RecoveryInstanceDisk' {} a -> s {internalDeviceName = a} :: RecoveryInstanceDisk)

-- | The EBS Volume ID of this disk.
recoveryInstanceDisk_ebsVolumeID :: Lens.Lens' RecoveryInstanceDisk (Prelude.Maybe Prelude.Text)
recoveryInstanceDisk_ebsVolumeID = Lens.lens (\RecoveryInstanceDisk' {ebsVolumeID} -> ebsVolumeID) (\s@RecoveryInstanceDisk' {} a -> s {ebsVolumeID = a} :: RecoveryInstanceDisk)

instance Data.FromJSON RecoveryInstanceDisk where
  parseJSON =
    Data.withObject
      "RecoveryInstanceDisk"
      ( \x ->
          RecoveryInstanceDisk'
            Prelude.<$> (x Data..:? "bytes")
            Prelude.<*> (x Data..:? "internalDeviceName")
            Prelude.<*> (x Data..:? "ebsVolumeID")
      )

instance Prelude.Hashable RecoveryInstanceDisk where
  hashWithSalt _salt RecoveryInstanceDisk' {..} =
    _salt `Prelude.hashWithSalt` bytes
      `Prelude.hashWithSalt` internalDeviceName
      `Prelude.hashWithSalt` ebsVolumeID

instance Prelude.NFData RecoveryInstanceDisk where
  rnf RecoveryInstanceDisk' {..} =
    Prelude.rnf bytes
      `Prelude.seq` Prelude.rnf internalDeviceName
      `Prelude.seq` Prelude.rnf ebsVolumeID
