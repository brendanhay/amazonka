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
-- Module      : Amazonka.MGN.Types.DataReplicationInfoReplicatedDisk
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.DataReplicationInfoReplicatedDisk where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Request to query disks replicated.
--
-- /See:/ 'newDataReplicationInfoReplicatedDisk' smart constructor.
data DataReplicationInfoReplicatedDisk = DataReplicationInfoReplicatedDisk'
  { -- | Request to query amount of data rescanned in bytes.
    rescannedStorageBytes :: Prelude.Maybe Prelude.Natural,
    -- | Request to query data replication backlog size in bytes.
    backloggedStorageBytes :: Prelude.Maybe Prelude.Natural,
    -- | Request to query device name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | Request to query total amount of data replicated in bytes.
    totalStorageBytes :: Prelude.Maybe Prelude.Natural,
    -- | Request to query amount of data replicated in bytes.
    replicatedStorageBytes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataReplicationInfoReplicatedDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rescannedStorageBytes', 'dataReplicationInfoReplicatedDisk_rescannedStorageBytes' - Request to query amount of data rescanned in bytes.
--
-- 'backloggedStorageBytes', 'dataReplicationInfoReplicatedDisk_backloggedStorageBytes' - Request to query data replication backlog size in bytes.
--
-- 'deviceName', 'dataReplicationInfoReplicatedDisk_deviceName' - Request to query device name.
--
-- 'totalStorageBytes', 'dataReplicationInfoReplicatedDisk_totalStorageBytes' - Request to query total amount of data replicated in bytes.
--
-- 'replicatedStorageBytes', 'dataReplicationInfoReplicatedDisk_replicatedStorageBytes' - Request to query amount of data replicated in bytes.
newDataReplicationInfoReplicatedDisk ::
  DataReplicationInfoReplicatedDisk
newDataReplicationInfoReplicatedDisk =
  DataReplicationInfoReplicatedDisk'
    { rescannedStorageBytes =
        Prelude.Nothing,
      backloggedStorageBytes = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      totalStorageBytes = Prelude.Nothing,
      replicatedStorageBytes = Prelude.Nothing
    }

-- | Request to query amount of data rescanned in bytes.
dataReplicationInfoReplicatedDisk_rescannedStorageBytes :: Lens.Lens' DataReplicationInfoReplicatedDisk (Prelude.Maybe Prelude.Natural)
dataReplicationInfoReplicatedDisk_rescannedStorageBytes = Lens.lens (\DataReplicationInfoReplicatedDisk' {rescannedStorageBytes} -> rescannedStorageBytes) (\s@DataReplicationInfoReplicatedDisk' {} a -> s {rescannedStorageBytes = a} :: DataReplicationInfoReplicatedDisk)

-- | Request to query data replication backlog size in bytes.
dataReplicationInfoReplicatedDisk_backloggedStorageBytes :: Lens.Lens' DataReplicationInfoReplicatedDisk (Prelude.Maybe Prelude.Natural)
dataReplicationInfoReplicatedDisk_backloggedStorageBytes = Lens.lens (\DataReplicationInfoReplicatedDisk' {backloggedStorageBytes} -> backloggedStorageBytes) (\s@DataReplicationInfoReplicatedDisk' {} a -> s {backloggedStorageBytes = a} :: DataReplicationInfoReplicatedDisk)

-- | Request to query device name.
dataReplicationInfoReplicatedDisk_deviceName :: Lens.Lens' DataReplicationInfoReplicatedDisk (Prelude.Maybe Prelude.Text)
dataReplicationInfoReplicatedDisk_deviceName = Lens.lens (\DataReplicationInfoReplicatedDisk' {deviceName} -> deviceName) (\s@DataReplicationInfoReplicatedDisk' {} a -> s {deviceName = a} :: DataReplicationInfoReplicatedDisk)

-- | Request to query total amount of data replicated in bytes.
dataReplicationInfoReplicatedDisk_totalStorageBytes :: Lens.Lens' DataReplicationInfoReplicatedDisk (Prelude.Maybe Prelude.Natural)
dataReplicationInfoReplicatedDisk_totalStorageBytes = Lens.lens (\DataReplicationInfoReplicatedDisk' {totalStorageBytes} -> totalStorageBytes) (\s@DataReplicationInfoReplicatedDisk' {} a -> s {totalStorageBytes = a} :: DataReplicationInfoReplicatedDisk)

-- | Request to query amount of data replicated in bytes.
dataReplicationInfoReplicatedDisk_replicatedStorageBytes :: Lens.Lens' DataReplicationInfoReplicatedDisk (Prelude.Maybe Prelude.Natural)
dataReplicationInfoReplicatedDisk_replicatedStorageBytes = Lens.lens (\DataReplicationInfoReplicatedDisk' {replicatedStorageBytes} -> replicatedStorageBytes) (\s@DataReplicationInfoReplicatedDisk' {} a -> s {replicatedStorageBytes = a} :: DataReplicationInfoReplicatedDisk)

instance
  Core.FromJSON
    DataReplicationInfoReplicatedDisk
  where
  parseJSON =
    Core.withObject
      "DataReplicationInfoReplicatedDisk"
      ( \x ->
          DataReplicationInfoReplicatedDisk'
            Prelude.<$> (x Core..:? "rescannedStorageBytes")
            Prelude.<*> (x Core..:? "backloggedStorageBytes")
            Prelude.<*> (x Core..:? "deviceName")
            Prelude.<*> (x Core..:? "totalStorageBytes")
            Prelude.<*> (x Core..:? "replicatedStorageBytes")
      )

instance
  Prelude.Hashable
    DataReplicationInfoReplicatedDisk
  where
  hashWithSalt
    _salt
    DataReplicationInfoReplicatedDisk' {..} =
      _salt `Prelude.hashWithSalt` rescannedStorageBytes
        `Prelude.hashWithSalt` backloggedStorageBytes
        `Prelude.hashWithSalt` deviceName
        `Prelude.hashWithSalt` totalStorageBytes
        `Prelude.hashWithSalt` replicatedStorageBytes

instance
  Prelude.NFData
    DataReplicationInfoReplicatedDisk
  where
  rnf DataReplicationInfoReplicatedDisk' {..} =
    Prelude.rnf rescannedStorageBytes
      `Prelude.seq` Prelude.rnf backloggedStorageBytes
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf totalStorageBytes
      `Prelude.seq` Prelude.rnf replicatedStorageBytes
