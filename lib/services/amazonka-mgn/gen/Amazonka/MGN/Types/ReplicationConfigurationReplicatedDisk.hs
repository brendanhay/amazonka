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
-- Module      : Amazonka.MGN.Types.ReplicationConfigurationReplicatedDisk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ReplicationConfigurationReplicatedDisk where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.ReplicationConfigurationReplicatedDiskStagingDiskType
import qualified Amazonka.Prelude as Prelude

-- | Replication Configuration replicated disk.
--
-- /See:/ 'newReplicationConfigurationReplicatedDisk' smart constructor.
data ReplicationConfigurationReplicatedDisk = ReplicationConfigurationReplicatedDisk'
  { -- | Replication Configuration replicated disk device name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | Replication Configuration replicated disk IOPs.
    iops :: Prelude.Maybe Prelude.Natural,
    -- | Replication Configuration replicated disk boot disk.
    isBootDisk :: Prelude.Maybe Prelude.Bool,
    -- | Replication Configuration replicated disk staging disk type.
    stagingDiskType :: Prelude.Maybe ReplicationConfigurationReplicatedDiskStagingDiskType,
    -- | Replication Configuration replicated disk throughput.
    throughput :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationConfigurationReplicatedDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceName', 'replicationConfigurationReplicatedDisk_deviceName' - Replication Configuration replicated disk device name.
--
-- 'iops', 'replicationConfigurationReplicatedDisk_iops' - Replication Configuration replicated disk IOPs.
--
-- 'isBootDisk', 'replicationConfigurationReplicatedDisk_isBootDisk' - Replication Configuration replicated disk boot disk.
--
-- 'stagingDiskType', 'replicationConfigurationReplicatedDisk_stagingDiskType' - Replication Configuration replicated disk staging disk type.
--
-- 'throughput', 'replicationConfigurationReplicatedDisk_throughput' - Replication Configuration replicated disk throughput.
newReplicationConfigurationReplicatedDisk ::
  ReplicationConfigurationReplicatedDisk
newReplicationConfigurationReplicatedDisk =
  ReplicationConfigurationReplicatedDisk'
    { deviceName =
        Prelude.Nothing,
      iops = Prelude.Nothing,
      isBootDisk = Prelude.Nothing,
      stagingDiskType = Prelude.Nothing,
      throughput = Prelude.Nothing
    }

-- | Replication Configuration replicated disk device name.
replicationConfigurationReplicatedDisk_deviceName :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe Prelude.Text)
replicationConfigurationReplicatedDisk_deviceName = Lens.lens (\ReplicationConfigurationReplicatedDisk' {deviceName} -> deviceName) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {deviceName = a} :: ReplicationConfigurationReplicatedDisk)

-- | Replication Configuration replicated disk IOPs.
replicationConfigurationReplicatedDisk_iops :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe Prelude.Natural)
replicationConfigurationReplicatedDisk_iops = Lens.lens (\ReplicationConfigurationReplicatedDisk' {iops} -> iops) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {iops = a} :: ReplicationConfigurationReplicatedDisk)

-- | Replication Configuration replicated disk boot disk.
replicationConfigurationReplicatedDisk_isBootDisk :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe Prelude.Bool)
replicationConfigurationReplicatedDisk_isBootDisk = Lens.lens (\ReplicationConfigurationReplicatedDisk' {isBootDisk} -> isBootDisk) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {isBootDisk = a} :: ReplicationConfigurationReplicatedDisk)

-- | Replication Configuration replicated disk staging disk type.
replicationConfigurationReplicatedDisk_stagingDiskType :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe ReplicationConfigurationReplicatedDiskStagingDiskType)
replicationConfigurationReplicatedDisk_stagingDiskType = Lens.lens (\ReplicationConfigurationReplicatedDisk' {stagingDiskType} -> stagingDiskType) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {stagingDiskType = a} :: ReplicationConfigurationReplicatedDisk)

-- | Replication Configuration replicated disk throughput.
replicationConfigurationReplicatedDisk_throughput :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe Prelude.Natural)
replicationConfigurationReplicatedDisk_throughput = Lens.lens (\ReplicationConfigurationReplicatedDisk' {throughput} -> throughput) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {throughput = a} :: ReplicationConfigurationReplicatedDisk)

instance
  Data.FromJSON
    ReplicationConfigurationReplicatedDisk
  where
  parseJSON =
    Data.withObject
      "ReplicationConfigurationReplicatedDisk"
      ( \x ->
          ReplicationConfigurationReplicatedDisk'
            Prelude.<$> (x Data..:? "deviceName")
            Prelude.<*> (x Data..:? "iops")
            Prelude.<*> (x Data..:? "isBootDisk")
            Prelude.<*> (x Data..:? "stagingDiskType")
            Prelude.<*> (x Data..:? "throughput")
      )

instance
  Prelude.Hashable
    ReplicationConfigurationReplicatedDisk
  where
  hashWithSalt
    _salt
    ReplicationConfigurationReplicatedDisk' {..} =
      _salt `Prelude.hashWithSalt` deviceName
        `Prelude.hashWithSalt` iops
        `Prelude.hashWithSalt` isBootDisk
        `Prelude.hashWithSalt` stagingDiskType
        `Prelude.hashWithSalt` throughput

instance
  Prelude.NFData
    ReplicationConfigurationReplicatedDisk
  where
  rnf ReplicationConfigurationReplicatedDisk' {..} =
    Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf isBootDisk
      `Prelude.seq` Prelude.rnf stagingDiskType
      `Prelude.seq` Prelude.rnf throughput

instance
  Data.ToJSON
    ReplicationConfigurationReplicatedDisk
  where
  toJSON ReplicationConfigurationReplicatedDisk' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deviceName" Data..=) Prelude.<$> deviceName,
            ("iops" Data..=) Prelude.<$> iops,
            ("isBootDisk" Data..=) Prelude.<$> isBootDisk,
            ("stagingDiskType" Data..=)
              Prelude.<$> stagingDiskType,
            ("throughput" Data..=) Prelude.<$> throughput
          ]
      )
