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
-- Module      : Amazonka.DrS.Types.ReplicationConfigurationReplicatedDisk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ReplicationConfigurationReplicatedDisk where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.ReplicationConfigurationReplicatedDiskStagingDiskType
import qualified Amazonka.Prelude as Prelude

-- | The configuration of a disk of the Source Server to be replicated.
--
-- /See:/ 'newReplicationConfigurationReplicatedDisk' smart constructor.
data ReplicationConfigurationReplicatedDisk = ReplicationConfigurationReplicatedDisk'
  { -- | The name of the device.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The requested number of I\/O operations per second (IOPS).
    iops :: Prelude.Maybe Prelude.Natural,
    -- | Whether to boot from this disk or not.
    isBootDisk :: Prelude.Maybe Prelude.Bool,
    -- | When @stagingDiskType@ is set to Auto, this field shows the current
    -- staging disk EBS volume type as it is constantly updated by the service.
    -- This is a read-only field.
    optimizedStagingDiskType :: Prelude.Maybe ReplicationConfigurationReplicatedDiskStagingDiskType,
    -- | The Staging Disk EBS volume type to be used during replication.
    stagingDiskType :: Prelude.Maybe ReplicationConfigurationReplicatedDiskStagingDiskType,
    -- | The throughput to use for the EBS volume in MiB\/s. This parameter is
    -- valid only for gp3 volumes.
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
-- 'deviceName', 'replicationConfigurationReplicatedDisk_deviceName' - The name of the device.
--
-- 'iops', 'replicationConfigurationReplicatedDisk_iops' - The requested number of I\/O operations per second (IOPS).
--
-- 'isBootDisk', 'replicationConfigurationReplicatedDisk_isBootDisk' - Whether to boot from this disk or not.
--
-- 'optimizedStagingDiskType', 'replicationConfigurationReplicatedDisk_optimizedStagingDiskType' - When @stagingDiskType@ is set to Auto, this field shows the current
-- staging disk EBS volume type as it is constantly updated by the service.
-- This is a read-only field.
--
-- 'stagingDiskType', 'replicationConfigurationReplicatedDisk_stagingDiskType' - The Staging Disk EBS volume type to be used during replication.
--
-- 'throughput', 'replicationConfigurationReplicatedDisk_throughput' - The throughput to use for the EBS volume in MiB\/s. This parameter is
-- valid only for gp3 volumes.
newReplicationConfigurationReplicatedDisk ::
  ReplicationConfigurationReplicatedDisk
newReplicationConfigurationReplicatedDisk =
  ReplicationConfigurationReplicatedDisk'
    { deviceName =
        Prelude.Nothing,
      iops = Prelude.Nothing,
      isBootDisk = Prelude.Nothing,
      optimizedStagingDiskType =
        Prelude.Nothing,
      stagingDiskType = Prelude.Nothing,
      throughput = Prelude.Nothing
    }

-- | The name of the device.
replicationConfigurationReplicatedDisk_deviceName :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe Prelude.Text)
replicationConfigurationReplicatedDisk_deviceName = Lens.lens (\ReplicationConfigurationReplicatedDisk' {deviceName} -> deviceName) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {deviceName = a} :: ReplicationConfigurationReplicatedDisk)

-- | The requested number of I\/O operations per second (IOPS).
replicationConfigurationReplicatedDisk_iops :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe Prelude.Natural)
replicationConfigurationReplicatedDisk_iops = Lens.lens (\ReplicationConfigurationReplicatedDisk' {iops} -> iops) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {iops = a} :: ReplicationConfigurationReplicatedDisk)

-- | Whether to boot from this disk or not.
replicationConfigurationReplicatedDisk_isBootDisk :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe Prelude.Bool)
replicationConfigurationReplicatedDisk_isBootDisk = Lens.lens (\ReplicationConfigurationReplicatedDisk' {isBootDisk} -> isBootDisk) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {isBootDisk = a} :: ReplicationConfigurationReplicatedDisk)

-- | When @stagingDiskType@ is set to Auto, this field shows the current
-- staging disk EBS volume type as it is constantly updated by the service.
-- This is a read-only field.
replicationConfigurationReplicatedDisk_optimizedStagingDiskType :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe ReplicationConfigurationReplicatedDiskStagingDiskType)
replicationConfigurationReplicatedDisk_optimizedStagingDiskType = Lens.lens (\ReplicationConfigurationReplicatedDisk' {optimizedStagingDiskType} -> optimizedStagingDiskType) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {optimizedStagingDiskType = a} :: ReplicationConfigurationReplicatedDisk)

-- | The Staging Disk EBS volume type to be used during replication.
replicationConfigurationReplicatedDisk_stagingDiskType :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe ReplicationConfigurationReplicatedDiskStagingDiskType)
replicationConfigurationReplicatedDisk_stagingDiskType = Lens.lens (\ReplicationConfigurationReplicatedDisk' {stagingDiskType} -> stagingDiskType) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {stagingDiskType = a} :: ReplicationConfigurationReplicatedDisk)

-- | The throughput to use for the EBS volume in MiB\/s. This parameter is
-- valid only for gp3 volumes.
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
            Prelude.<*> (x Data..:? "optimizedStagingDiskType")
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
      _salt
        `Prelude.hashWithSalt` deviceName
        `Prelude.hashWithSalt` iops
        `Prelude.hashWithSalt` isBootDisk
        `Prelude.hashWithSalt` optimizedStagingDiskType
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
      `Prelude.seq` Prelude.rnf optimizedStagingDiskType
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
            ("optimizedStagingDiskType" Data..=)
              Prelude.<$> optimizedStagingDiskType,
            ("stagingDiskType" Data..=)
              Prelude.<$> stagingDiskType,
            ("throughput" Data..=) Prelude.<$> throughput
          ]
      )
