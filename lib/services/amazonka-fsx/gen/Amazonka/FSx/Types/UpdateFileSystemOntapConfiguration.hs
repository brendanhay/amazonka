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
-- Module      : Amazonka.FSx.Types.UpdateFileSystemOntapConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.UpdateFileSystemOntapConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.DiskIopsConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration updates for an Amazon FSx for NetApp ONTAP file
-- system.
--
-- /See:/ 'newUpdateFileSystemOntapConfiguration' smart constructor.
data UpdateFileSystemOntapConfiguration = UpdateFileSystemOntapConfiguration'
  { weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
    -- | Specifies the throughput of an FSx for NetApp ONTAP file system,
    -- measured in megabytes per second (MBps). Valid values are 128, 256, 512,
    -- 1024, or 2048 MB\/s.
    throughputCapacity :: Prelude.Maybe Prelude.Natural,
    automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    -- | The ONTAP administrative password for the @fsxadmin@ user.
    fsxAdminPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The SSD IOPS (input\/output operations per second) configuration for an
    -- Amazon FSx for NetApp ONTAP file system. The default is 3 IOPS per GB of
    -- storage capacity, but you can provision additional IOPS per GB of
    -- storage. The configuration consists of an IOPS mode (@AUTOMATIC@ or
    -- @USER_PROVISIONED@), and in the case of @USER_PROVISIONED@ IOPS, the
    -- total number of SSD IOPS provisioned.
    diskIopsConfiguration :: Prelude.Maybe DiskIopsConfiguration,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFileSystemOntapConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'weeklyMaintenanceStartTime', 'updateFileSystemOntapConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
--
-- 'throughputCapacity', 'updateFileSystemOntapConfiguration_throughputCapacity' - Specifies the throughput of an FSx for NetApp ONTAP file system,
-- measured in megabytes per second (MBps). Valid values are 128, 256, 512,
-- 1024, or 2048 MB\/s.
--
-- 'automaticBackupRetentionDays', 'updateFileSystemOntapConfiguration_automaticBackupRetentionDays' - Undocumented member.
--
-- 'fsxAdminPassword', 'updateFileSystemOntapConfiguration_fsxAdminPassword' - The ONTAP administrative password for the @fsxadmin@ user.
--
-- 'diskIopsConfiguration', 'updateFileSystemOntapConfiguration_diskIopsConfiguration' - The SSD IOPS (input\/output operations per second) configuration for an
-- Amazon FSx for NetApp ONTAP file system. The default is 3 IOPS per GB of
-- storage capacity, but you can provision additional IOPS per GB of
-- storage. The configuration consists of an IOPS mode (@AUTOMATIC@ or
-- @USER_PROVISIONED@), and in the case of @USER_PROVISIONED@ IOPS, the
-- total number of SSD IOPS provisioned.
--
-- 'dailyAutomaticBackupStartTime', 'updateFileSystemOntapConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
newUpdateFileSystemOntapConfiguration ::
  UpdateFileSystemOntapConfiguration
newUpdateFileSystemOntapConfiguration =
  UpdateFileSystemOntapConfiguration'
    { weeklyMaintenanceStartTime =
        Prelude.Nothing,
      throughputCapacity = Prelude.Nothing,
      automaticBackupRetentionDays =
        Prelude.Nothing,
      fsxAdminPassword = Prelude.Nothing,
      diskIopsConfiguration = Prelude.Nothing,
      dailyAutomaticBackupStartTime =
        Prelude.Nothing
    }

-- | Undocumented member.
updateFileSystemOntapConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' UpdateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
updateFileSystemOntapConfiguration_weeklyMaintenanceStartTime = Lens.lens (\UpdateFileSystemOntapConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@UpdateFileSystemOntapConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: UpdateFileSystemOntapConfiguration)

-- | Specifies the throughput of an FSx for NetApp ONTAP file system,
-- measured in megabytes per second (MBps). Valid values are 128, 256, 512,
-- 1024, or 2048 MB\/s.
updateFileSystemOntapConfiguration_throughputCapacity :: Lens.Lens' UpdateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Natural)
updateFileSystemOntapConfiguration_throughputCapacity = Lens.lens (\UpdateFileSystemOntapConfiguration' {throughputCapacity} -> throughputCapacity) (\s@UpdateFileSystemOntapConfiguration' {} a -> s {throughputCapacity = a} :: UpdateFileSystemOntapConfiguration)

-- | Undocumented member.
updateFileSystemOntapConfiguration_automaticBackupRetentionDays :: Lens.Lens' UpdateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Natural)
updateFileSystemOntapConfiguration_automaticBackupRetentionDays = Lens.lens (\UpdateFileSystemOntapConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@UpdateFileSystemOntapConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: UpdateFileSystemOntapConfiguration)

-- | The ONTAP administrative password for the @fsxadmin@ user.
updateFileSystemOntapConfiguration_fsxAdminPassword :: Lens.Lens' UpdateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
updateFileSystemOntapConfiguration_fsxAdminPassword = Lens.lens (\UpdateFileSystemOntapConfiguration' {fsxAdminPassword} -> fsxAdminPassword) (\s@UpdateFileSystemOntapConfiguration' {} a -> s {fsxAdminPassword = a} :: UpdateFileSystemOntapConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The SSD IOPS (input\/output operations per second) configuration for an
-- Amazon FSx for NetApp ONTAP file system. The default is 3 IOPS per GB of
-- storage capacity, but you can provision additional IOPS per GB of
-- storage. The configuration consists of an IOPS mode (@AUTOMATIC@ or
-- @USER_PROVISIONED@), and in the case of @USER_PROVISIONED@ IOPS, the
-- total number of SSD IOPS provisioned.
updateFileSystemOntapConfiguration_diskIopsConfiguration :: Lens.Lens' UpdateFileSystemOntapConfiguration (Prelude.Maybe DiskIopsConfiguration)
updateFileSystemOntapConfiguration_diskIopsConfiguration = Lens.lens (\UpdateFileSystemOntapConfiguration' {diskIopsConfiguration} -> diskIopsConfiguration) (\s@UpdateFileSystemOntapConfiguration' {} a -> s {diskIopsConfiguration = a} :: UpdateFileSystemOntapConfiguration)

-- | Undocumented member.
updateFileSystemOntapConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' UpdateFileSystemOntapConfiguration (Prelude.Maybe Prelude.Text)
updateFileSystemOntapConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\UpdateFileSystemOntapConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@UpdateFileSystemOntapConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: UpdateFileSystemOntapConfiguration)

instance
  Prelude.Hashable
    UpdateFileSystemOntapConfiguration
  where
  hashWithSalt
    _salt
    UpdateFileSystemOntapConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime
        `Prelude.hashWithSalt` throughputCapacity
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` fsxAdminPassword
        `Prelude.hashWithSalt` diskIopsConfiguration
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime

instance
  Prelude.NFData
    UpdateFileSystemOntapConfiguration
  where
  rnf UpdateFileSystemOntapConfiguration' {..} =
    Prelude.rnf weeklyMaintenanceStartTime
      `Prelude.seq` Prelude.rnf throughputCapacity
      `Prelude.seq` Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf fsxAdminPassword
      `Prelude.seq` Prelude.rnf diskIopsConfiguration
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime

instance
  Data.ToJSON
    UpdateFileSystemOntapConfiguration
  where
  toJSON UpdateFileSystemOntapConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WeeklyMaintenanceStartTime" Data..=)
              Prelude.<$> weeklyMaintenanceStartTime,
            ("ThroughputCapacity" Data..=)
              Prelude.<$> throughputCapacity,
            ("AutomaticBackupRetentionDays" Data..=)
              Prelude.<$> automaticBackupRetentionDays,
            ("FsxAdminPassword" Data..=)
              Prelude.<$> fsxAdminPassword,
            ("DiskIopsConfiguration" Data..=)
              Prelude.<$> diskIopsConfiguration,
            ("DailyAutomaticBackupStartTime" Data..=)
              Prelude.<$> dailyAutomaticBackupStartTime
          ]
      )
