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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ReplicationConfigurationReplicatedDisk where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types.ReplicationConfigurationReplicatedDiskStagingDiskType
import qualified Amazonka.Prelude as Prelude

-- | The configuration of a disk of the Source Server to be replicated.
--
-- /See:/ 'newReplicationConfigurationReplicatedDisk' smart constructor.
data ReplicationConfigurationReplicatedDisk = ReplicationConfigurationReplicatedDisk'
  { -- | Whether to boot from this disk or not.
    isBootDisk :: Prelude.Maybe Prelude.Bool,
    -- | The name of the device.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The Staging Disk EBS volume type to be used during replication when
    -- @stagingDiskType@ is set to Auto. This is a read-only field.
    optimizedStagingDiskType :: Prelude.Maybe ReplicationConfigurationReplicatedDiskStagingDiskType,
    -- | The Staging Disk EBS volume type to be used during replication.
    stagingDiskType :: Prelude.Maybe ReplicationConfigurationReplicatedDiskStagingDiskType,
    -- | The throughput to use for the EBS volume in MiB\/s. This parameter is
    -- valid only for gp3 volumes.
    throughput :: Prelude.Maybe Prelude.Natural,
    -- | The requested number of I\/O operations per second (IOPS).
    iops :: Prelude.Maybe Prelude.Natural
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
-- 'isBootDisk', 'replicationConfigurationReplicatedDisk_isBootDisk' - Whether to boot from this disk or not.
--
-- 'deviceName', 'replicationConfigurationReplicatedDisk_deviceName' - The name of the device.
--
-- 'optimizedStagingDiskType', 'replicationConfigurationReplicatedDisk_optimizedStagingDiskType' - The Staging Disk EBS volume type to be used during replication when
-- @stagingDiskType@ is set to Auto. This is a read-only field.
--
-- 'stagingDiskType', 'replicationConfigurationReplicatedDisk_stagingDiskType' - The Staging Disk EBS volume type to be used during replication.
--
-- 'throughput', 'replicationConfigurationReplicatedDisk_throughput' - The throughput to use for the EBS volume in MiB\/s. This parameter is
-- valid only for gp3 volumes.
--
-- 'iops', 'replicationConfigurationReplicatedDisk_iops' - The requested number of I\/O operations per second (IOPS).
newReplicationConfigurationReplicatedDisk ::
  ReplicationConfigurationReplicatedDisk
newReplicationConfigurationReplicatedDisk =
  ReplicationConfigurationReplicatedDisk'
    { isBootDisk =
        Prelude.Nothing,
      deviceName = Prelude.Nothing,
      optimizedStagingDiskType =
        Prelude.Nothing,
      stagingDiskType = Prelude.Nothing,
      throughput = Prelude.Nothing,
      iops = Prelude.Nothing
    }

-- | Whether to boot from this disk or not.
replicationConfigurationReplicatedDisk_isBootDisk :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe Prelude.Bool)
replicationConfigurationReplicatedDisk_isBootDisk = Lens.lens (\ReplicationConfigurationReplicatedDisk' {isBootDisk} -> isBootDisk) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {isBootDisk = a} :: ReplicationConfigurationReplicatedDisk)

-- | The name of the device.
replicationConfigurationReplicatedDisk_deviceName :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe Prelude.Text)
replicationConfigurationReplicatedDisk_deviceName = Lens.lens (\ReplicationConfigurationReplicatedDisk' {deviceName} -> deviceName) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {deviceName = a} :: ReplicationConfigurationReplicatedDisk)

-- | The Staging Disk EBS volume type to be used during replication when
-- @stagingDiskType@ is set to Auto. This is a read-only field.
replicationConfigurationReplicatedDisk_optimizedStagingDiskType :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe ReplicationConfigurationReplicatedDiskStagingDiskType)
replicationConfigurationReplicatedDisk_optimizedStagingDiskType = Lens.lens (\ReplicationConfigurationReplicatedDisk' {optimizedStagingDiskType} -> optimizedStagingDiskType) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {optimizedStagingDiskType = a} :: ReplicationConfigurationReplicatedDisk)

-- | The Staging Disk EBS volume type to be used during replication.
replicationConfigurationReplicatedDisk_stagingDiskType :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe ReplicationConfigurationReplicatedDiskStagingDiskType)
replicationConfigurationReplicatedDisk_stagingDiskType = Lens.lens (\ReplicationConfigurationReplicatedDisk' {stagingDiskType} -> stagingDiskType) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {stagingDiskType = a} :: ReplicationConfigurationReplicatedDisk)

-- | The throughput to use for the EBS volume in MiB\/s. This parameter is
-- valid only for gp3 volumes.
replicationConfigurationReplicatedDisk_throughput :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe Prelude.Natural)
replicationConfigurationReplicatedDisk_throughput = Lens.lens (\ReplicationConfigurationReplicatedDisk' {throughput} -> throughput) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {throughput = a} :: ReplicationConfigurationReplicatedDisk)

-- | The requested number of I\/O operations per second (IOPS).
replicationConfigurationReplicatedDisk_iops :: Lens.Lens' ReplicationConfigurationReplicatedDisk (Prelude.Maybe Prelude.Natural)
replicationConfigurationReplicatedDisk_iops = Lens.lens (\ReplicationConfigurationReplicatedDisk' {iops} -> iops) (\s@ReplicationConfigurationReplicatedDisk' {} a -> s {iops = a} :: ReplicationConfigurationReplicatedDisk)

instance
  Core.FromJSON
    ReplicationConfigurationReplicatedDisk
  where
  parseJSON =
    Core.withObject
      "ReplicationConfigurationReplicatedDisk"
      ( \x ->
          ReplicationConfigurationReplicatedDisk'
            Prelude.<$> (x Core..:? "isBootDisk")
            Prelude.<*> (x Core..:? "deviceName")
            Prelude.<*> (x Core..:? "optimizedStagingDiskType")
            Prelude.<*> (x Core..:? "stagingDiskType")
            Prelude.<*> (x Core..:? "throughput")
            Prelude.<*> (x Core..:? "iops")
      )

instance
  Prelude.Hashable
    ReplicationConfigurationReplicatedDisk
  where
  hashWithSalt
    _salt
    ReplicationConfigurationReplicatedDisk' {..} =
      _salt `Prelude.hashWithSalt` isBootDisk
        `Prelude.hashWithSalt` deviceName
        `Prelude.hashWithSalt` optimizedStagingDiskType
        `Prelude.hashWithSalt` stagingDiskType
        `Prelude.hashWithSalt` throughput
        `Prelude.hashWithSalt` iops

instance
  Prelude.NFData
    ReplicationConfigurationReplicatedDisk
  where
  rnf ReplicationConfigurationReplicatedDisk' {..} =
    Prelude.rnf isBootDisk
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf optimizedStagingDiskType
      `Prelude.seq` Prelude.rnf stagingDiskType
      `Prelude.seq` Prelude.rnf throughput
      `Prelude.seq` Prelude.rnf iops

instance
  Core.ToJSON
    ReplicationConfigurationReplicatedDisk
  where
  toJSON ReplicationConfigurationReplicatedDisk' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("isBootDisk" Core..=) Prelude.<$> isBootDisk,
            ("deviceName" Core..=) Prelude.<$> deviceName,
            ("optimizedStagingDiskType" Core..=)
              Prelude.<$> optimizedStagingDiskType,
            ("stagingDiskType" Core..=)
              Prelude.<$> stagingDiskType,
            ("throughput" Core..=) Prelude.<$> throughput,
            ("iops" Core..=) Prelude.<$> iops
          ]
      )
