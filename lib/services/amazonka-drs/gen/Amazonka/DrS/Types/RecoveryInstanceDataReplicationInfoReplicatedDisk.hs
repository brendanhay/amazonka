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
-- Module      : Amazonka.DrS.Types.RecoveryInstanceDataReplicationInfoReplicatedDisk
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryInstanceDataReplicationInfoReplicatedDisk where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A disk that should be replicated.
--
-- /See:/ 'newRecoveryInstanceDataReplicationInfoReplicatedDisk' smart constructor.
data RecoveryInstanceDataReplicationInfoReplicatedDisk = RecoveryInstanceDataReplicationInfoReplicatedDisk'
  { -- | The amount of data to be rescanned in bytes.
    rescannedStorageBytes :: Prelude.Maybe Prelude.Natural,
    -- | The size of the replication backlog in bytes.
    backloggedStorageBytes :: Prelude.Maybe Prelude.Natural,
    -- | The name of the device.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The total amount of data to be replicated in bytes.
    totalStorageBytes :: Prelude.Maybe Prelude.Natural,
    -- | The amount of data replicated so far in bytes.
    replicatedStorageBytes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryInstanceDataReplicationInfoReplicatedDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rescannedStorageBytes', 'recoveryInstanceDataReplicationInfoReplicatedDisk_rescannedStorageBytes' - The amount of data to be rescanned in bytes.
--
-- 'backloggedStorageBytes', 'recoveryInstanceDataReplicationInfoReplicatedDisk_backloggedStorageBytes' - The size of the replication backlog in bytes.
--
-- 'deviceName', 'recoveryInstanceDataReplicationInfoReplicatedDisk_deviceName' - The name of the device.
--
-- 'totalStorageBytes', 'recoveryInstanceDataReplicationInfoReplicatedDisk_totalStorageBytes' - The total amount of data to be replicated in bytes.
--
-- 'replicatedStorageBytes', 'recoveryInstanceDataReplicationInfoReplicatedDisk_replicatedStorageBytes' - The amount of data replicated so far in bytes.
newRecoveryInstanceDataReplicationInfoReplicatedDisk ::
  RecoveryInstanceDataReplicationInfoReplicatedDisk
newRecoveryInstanceDataReplicationInfoReplicatedDisk =
  RecoveryInstanceDataReplicationInfoReplicatedDisk'
    { rescannedStorageBytes =
        Prelude.Nothing,
      backloggedStorageBytes =
        Prelude.Nothing,
      deviceName =
        Prelude.Nothing,
      totalStorageBytes =
        Prelude.Nothing,
      replicatedStorageBytes =
        Prelude.Nothing
    }

-- | The amount of data to be rescanned in bytes.
recoveryInstanceDataReplicationInfoReplicatedDisk_rescannedStorageBytes :: Lens.Lens' RecoveryInstanceDataReplicationInfoReplicatedDisk (Prelude.Maybe Prelude.Natural)
recoveryInstanceDataReplicationInfoReplicatedDisk_rescannedStorageBytes = Lens.lens (\RecoveryInstanceDataReplicationInfoReplicatedDisk' {rescannedStorageBytes} -> rescannedStorageBytes) (\s@RecoveryInstanceDataReplicationInfoReplicatedDisk' {} a -> s {rescannedStorageBytes = a} :: RecoveryInstanceDataReplicationInfoReplicatedDisk)

-- | The size of the replication backlog in bytes.
recoveryInstanceDataReplicationInfoReplicatedDisk_backloggedStorageBytes :: Lens.Lens' RecoveryInstanceDataReplicationInfoReplicatedDisk (Prelude.Maybe Prelude.Natural)
recoveryInstanceDataReplicationInfoReplicatedDisk_backloggedStorageBytes = Lens.lens (\RecoveryInstanceDataReplicationInfoReplicatedDisk' {backloggedStorageBytes} -> backloggedStorageBytes) (\s@RecoveryInstanceDataReplicationInfoReplicatedDisk' {} a -> s {backloggedStorageBytes = a} :: RecoveryInstanceDataReplicationInfoReplicatedDisk)

-- | The name of the device.
recoveryInstanceDataReplicationInfoReplicatedDisk_deviceName :: Lens.Lens' RecoveryInstanceDataReplicationInfoReplicatedDisk (Prelude.Maybe Prelude.Text)
recoveryInstanceDataReplicationInfoReplicatedDisk_deviceName = Lens.lens (\RecoveryInstanceDataReplicationInfoReplicatedDisk' {deviceName} -> deviceName) (\s@RecoveryInstanceDataReplicationInfoReplicatedDisk' {} a -> s {deviceName = a} :: RecoveryInstanceDataReplicationInfoReplicatedDisk)

-- | The total amount of data to be replicated in bytes.
recoveryInstanceDataReplicationInfoReplicatedDisk_totalStorageBytes :: Lens.Lens' RecoveryInstanceDataReplicationInfoReplicatedDisk (Prelude.Maybe Prelude.Natural)
recoveryInstanceDataReplicationInfoReplicatedDisk_totalStorageBytes = Lens.lens (\RecoveryInstanceDataReplicationInfoReplicatedDisk' {totalStorageBytes} -> totalStorageBytes) (\s@RecoveryInstanceDataReplicationInfoReplicatedDisk' {} a -> s {totalStorageBytes = a} :: RecoveryInstanceDataReplicationInfoReplicatedDisk)

-- | The amount of data replicated so far in bytes.
recoveryInstanceDataReplicationInfoReplicatedDisk_replicatedStorageBytes :: Lens.Lens' RecoveryInstanceDataReplicationInfoReplicatedDisk (Prelude.Maybe Prelude.Natural)
recoveryInstanceDataReplicationInfoReplicatedDisk_replicatedStorageBytes = Lens.lens (\RecoveryInstanceDataReplicationInfoReplicatedDisk' {replicatedStorageBytes} -> replicatedStorageBytes) (\s@RecoveryInstanceDataReplicationInfoReplicatedDisk' {} a -> s {replicatedStorageBytes = a} :: RecoveryInstanceDataReplicationInfoReplicatedDisk)

instance
  Data.FromJSON
    RecoveryInstanceDataReplicationInfoReplicatedDisk
  where
  parseJSON =
    Data.withObject
      "RecoveryInstanceDataReplicationInfoReplicatedDisk"
      ( \x ->
          RecoveryInstanceDataReplicationInfoReplicatedDisk'
            Prelude.<$> (x Data..:? "rescannedStorageBytes")
              Prelude.<*> (x Data..:? "backloggedStorageBytes")
              Prelude.<*> (x Data..:? "deviceName")
              Prelude.<*> (x Data..:? "totalStorageBytes")
              Prelude.<*> (x Data..:? "replicatedStorageBytes")
      )

instance
  Prelude.Hashable
    RecoveryInstanceDataReplicationInfoReplicatedDisk
  where
  hashWithSalt
    _salt
    RecoveryInstanceDataReplicationInfoReplicatedDisk' {..} =
      _salt `Prelude.hashWithSalt` rescannedStorageBytes
        `Prelude.hashWithSalt` backloggedStorageBytes
        `Prelude.hashWithSalt` deviceName
        `Prelude.hashWithSalt` totalStorageBytes
        `Prelude.hashWithSalt` replicatedStorageBytes

instance
  Prelude.NFData
    RecoveryInstanceDataReplicationInfoReplicatedDisk
  where
  rnf
    RecoveryInstanceDataReplicationInfoReplicatedDisk' {..} =
      Prelude.rnf rescannedStorageBytes
        `Prelude.seq` Prelude.rnf backloggedStorageBytes
        `Prelude.seq` Prelude.rnf deviceName
        `Prelude.seq` Prelude.rnf totalStorageBytes
        `Prelude.seq` Prelude.rnf replicatedStorageBytes
