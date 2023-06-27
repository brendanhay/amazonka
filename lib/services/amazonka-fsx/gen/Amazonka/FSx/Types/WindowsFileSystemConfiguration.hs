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
-- Module      : Amazonka.FSx.Types.WindowsFileSystemConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.WindowsFileSystemConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.Alias
import Amazonka.FSx.Types.FileSystemMaintenanceOperation
import Amazonka.FSx.Types.SelfManagedActiveDirectoryAttributes
import Amazonka.FSx.Types.WindowsAuditLogConfiguration
import Amazonka.FSx.Types.WindowsDeploymentType
import qualified Amazonka.Prelude as Prelude

-- | The configuration for this Microsoft Windows file system.
--
-- /See:/ 'newWindowsFileSystemConfiguration' smart constructor.
data WindowsFileSystemConfiguration = WindowsFileSystemConfiguration'
  { -- | The ID for an existing Amazon Web Services Managed Microsoft Active
    -- Directory instance that the file system is joined to.
    activeDirectoryId :: Prelude.Maybe Prelude.Text,
    aliases :: Prelude.Maybe [Alias],
    -- | The configuration that Amazon FSx for Windows File Server uses to audit
    -- and log user accesses of files, folders, and file shares on the Amazon
    -- FSx for Windows File Server file system.
    auditLogConfiguration :: Prelude.Maybe WindowsAuditLogConfiguration,
    -- | The number of days to retain automatic backups. Setting this to 0
    -- disables automatic backups. You can retain automatic backups for a
    -- maximum of 90 days.
    automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    -- | A boolean flag indicating whether tags on the file system should be
    -- copied to backups. This value defaults to false. If it\'s set to true,
    -- all tags on the file system are copied to all automatic backups and any
    -- user-initiated backups where the user doesn\'t specify any tags. If this
    -- value is true, and you specify one or more tags, only the specified tags
    -- are copied to backups. If you specify one or more tags when creating a
    -- user-initiated backup, no tags are copied from the file system,
    -- regardless of this value.
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    -- | The preferred time to take daily automatic backups, in the UTC time
    -- zone.
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | Specifies the file system deployment type, valid values are the
    -- following:
    --
    -- -   @MULTI_AZ_1@ - Specifies a high availability file system that is
    --     configured for Multi-AZ redundancy to tolerate temporary
    --     Availability Zone (AZ) unavailability, and supports SSD and HDD
    --     storage.
    --
    -- -   @SINGLE_AZ_1@ - (Default) Specifies a file system that is configured
    --     for single AZ redundancy, only supports SSD storage.
    --
    -- -   @SINGLE_AZ_2@ - Latest generation Single AZ file system. Specifies a
    --     file system that is configured for single AZ redundancy and supports
    --     SSD and HDD storage.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html Single-AZ and Multi-AZ File Systems>.
    deploymentType :: Prelude.Maybe WindowsDeploymentType,
    -- | The list of maintenance operations in progress for this file system.
    maintenanceOperationsInProgress :: Prelude.Maybe [FileSystemMaintenanceOperation],
    -- | For @MULTI_AZ_1@ deployment types, the IP address of the primary, or
    -- preferred, file server.
    --
    -- Use this IP address when mounting the file system on Linux SMB clients
    -- or Windows SMB clients that are not joined to a Microsoft Active
    -- Directory. Applicable for all Windows file system deployment types. This
    -- IP address is temporarily unavailable when the file system is undergoing
    -- maintenance. For Linux and Windows SMB clients that are joined to an
    -- Active Directory, use the file system\'s DNSName instead. For more
    -- information on mapping and mounting file shares, see
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/accessing-file-shares.html Accessing File Shares>.
    preferredFileServerIp :: Prelude.Maybe Prelude.Text,
    -- | For @MULTI_AZ_1@ deployment types, it specifies the ID of the subnet
    -- where the preferred file server is located. Must be one of the two
    -- subnet IDs specified in @SubnetIds@ property. Amazon FSx serves traffic
    -- from this subnet except in the event of a failover to the secondary file
    -- server.
    --
    -- For @SINGLE_AZ_1@ and @SINGLE_AZ_2@ deployment types, this value is the
    -- same as that for @SubnetIDs@. For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html#single-multi-az-resources Availability and durability: Single-AZ and Multi-AZ file systems>.
    preferredSubnetId :: Prelude.Maybe Prelude.Text,
    -- | For @MULTI_AZ_1@ deployment types, use this endpoint when performing
    -- administrative tasks on the file system using Amazon FSx Remote
    -- PowerShell.
    --
    -- For @SINGLE_AZ_1@ and @SINGLE_AZ_2@ deployment types, this is the DNS
    -- name of the file system.
    --
    -- This endpoint is temporarily unavailable when the file system is
    -- undergoing maintenance.
    remoteAdministrationEndpoint :: Prelude.Maybe Prelude.Text,
    selfManagedActiveDirectoryConfiguration :: Prelude.Maybe SelfManagedActiveDirectoryAttributes,
    -- | The throughput of the Amazon FSx file system, measured in megabytes per
    -- second.
    throughputCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The preferred start time to perform weekly maintenance, formatted
    -- d:HH:MM in the UTC time zone. d is the weekday number, from 1 through 7,
    -- beginning with Monday and ending with Sunday.
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WindowsFileSystemConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryId', 'windowsFileSystemConfiguration_activeDirectoryId' - The ID for an existing Amazon Web Services Managed Microsoft Active
-- Directory instance that the file system is joined to.
--
-- 'aliases', 'windowsFileSystemConfiguration_aliases' - Undocumented member.
--
-- 'auditLogConfiguration', 'windowsFileSystemConfiguration_auditLogConfiguration' - The configuration that Amazon FSx for Windows File Server uses to audit
-- and log user accesses of files, folders, and file shares on the Amazon
-- FSx for Windows File Server file system.
--
-- 'automaticBackupRetentionDays', 'windowsFileSystemConfiguration_automaticBackupRetentionDays' - The number of days to retain automatic backups. Setting this to 0
-- disables automatic backups. You can retain automatic backups for a
-- maximum of 90 days.
--
-- 'copyTagsToBackups', 'windowsFileSystemConfiguration_copyTagsToBackups' - A boolean flag indicating whether tags on the file system should be
-- copied to backups. This value defaults to false. If it\'s set to true,
-- all tags on the file system are copied to all automatic backups and any
-- user-initiated backups where the user doesn\'t specify any tags. If this
-- value is true, and you specify one or more tags, only the specified tags
-- are copied to backups. If you specify one or more tags when creating a
-- user-initiated backup, no tags are copied from the file system,
-- regardless of this value.
--
-- 'dailyAutomaticBackupStartTime', 'windowsFileSystemConfiguration_dailyAutomaticBackupStartTime' - The preferred time to take daily automatic backups, in the UTC time
-- zone.
--
-- 'deploymentType', 'windowsFileSystemConfiguration_deploymentType' - Specifies the file system deployment type, valid values are the
-- following:
--
-- -   @MULTI_AZ_1@ - Specifies a high availability file system that is
--     configured for Multi-AZ redundancy to tolerate temporary
--     Availability Zone (AZ) unavailability, and supports SSD and HDD
--     storage.
--
-- -   @SINGLE_AZ_1@ - (Default) Specifies a file system that is configured
--     for single AZ redundancy, only supports SSD storage.
--
-- -   @SINGLE_AZ_2@ - Latest generation Single AZ file system. Specifies a
--     file system that is configured for single AZ redundancy and supports
--     SSD and HDD storage.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html Single-AZ and Multi-AZ File Systems>.
--
-- 'maintenanceOperationsInProgress', 'windowsFileSystemConfiguration_maintenanceOperationsInProgress' - The list of maintenance operations in progress for this file system.
--
-- 'preferredFileServerIp', 'windowsFileSystemConfiguration_preferredFileServerIp' - For @MULTI_AZ_1@ deployment types, the IP address of the primary, or
-- preferred, file server.
--
-- Use this IP address when mounting the file system on Linux SMB clients
-- or Windows SMB clients that are not joined to a Microsoft Active
-- Directory. Applicable for all Windows file system deployment types. This
-- IP address is temporarily unavailable when the file system is undergoing
-- maintenance. For Linux and Windows SMB clients that are joined to an
-- Active Directory, use the file system\'s DNSName instead. For more
-- information on mapping and mounting file shares, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/accessing-file-shares.html Accessing File Shares>.
--
-- 'preferredSubnetId', 'windowsFileSystemConfiguration_preferredSubnetId' - For @MULTI_AZ_1@ deployment types, it specifies the ID of the subnet
-- where the preferred file server is located. Must be one of the two
-- subnet IDs specified in @SubnetIds@ property. Amazon FSx serves traffic
-- from this subnet except in the event of a failover to the secondary file
-- server.
--
-- For @SINGLE_AZ_1@ and @SINGLE_AZ_2@ deployment types, this value is the
-- same as that for @SubnetIDs@. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html#single-multi-az-resources Availability and durability: Single-AZ and Multi-AZ file systems>.
--
-- 'remoteAdministrationEndpoint', 'windowsFileSystemConfiguration_remoteAdministrationEndpoint' - For @MULTI_AZ_1@ deployment types, use this endpoint when performing
-- administrative tasks on the file system using Amazon FSx Remote
-- PowerShell.
--
-- For @SINGLE_AZ_1@ and @SINGLE_AZ_2@ deployment types, this is the DNS
-- name of the file system.
--
-- This endpoint is temporarily unavailable when the file system is
-- undergoing maintenance.
--
-- 'selfManagedActiveDirectoryConfiguration', 'windowsFileSystemConfiguration_selfManagedActiveDirectoryConfiguration' - Undocumented member.
--
-- 'throughputCapacity', 'windowsFileSystemConfiguration_throughputCapacity' - The throughput of the Amazon FSx file system, measured in megabytes per
-- second.
--
-- 'weeklyMaintenanceStartTime', 'windowsFileSystemConfiguration_weeklyMaintenanceStartTime' - The preferred start time to perform weekly maintenance, formatted
-- d:HH:MM in the UTC time zone. d is the weekday number, from 1 through 7,
-- beginning with Monday and ending with Sunday.
newWindowsFileSystemConfiguration ::
  WindowsFileSystemConfiguration
newWindowsFileSystemConfiguration =
  WindowsFileSystemConfiguration'
    { activeDirectoryId =
        Prelude.Nothing,
      aliases = Prelude.Nothing,
      auditLogConfiguration = Prelude.Nothing,
      automaticBackupRetentionDays =
        Prelude.Nothing,
      copyTagsToBackups = Prelude.Nothing,
      dailyAutomaticBackupStartTime =
        Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      maintenanceOperationsInProgress =
        Prelude.Nothing,
      preferredFileServerIp = Prelude.Nothing,
      preferredSubnetId = Prelude.Nothing,
      remoteAdministrationEndpoint =
        Prelude.Nothing,
      selfManagedActiveDirectoryConfiguration =
        Prelude.Nothing,
      throughputCapacity = Prelude.Nothing,
      weeklyMaintenanceStartTime =
        Prelude.Nothing
    }

-- | The ID for an existing Amazon Web Services Managed Microsoft Active
-- Directory instance that the file system is joined to.
windowsFileSystemConfiguration_activeDirectoryId :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe Prelude.Text)
windowsFileSystemConfiguration_activeDirectoryId = Lens.lens (\WindowsFileSystemConfiguration' {activeDirectoryId} -> activeDirectoryId) (\s@WindowsFileSystemConfiguration' {} a -> s {activeDirectoryId = a} :: WindowsFileSystemConfiguration)

-- | Undocumented member.
windowsFileSystemConfiguration_aliases :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe [Alias])
windowsFileSystemConfiguration_aliases = Lens.lens (\WindowsFileSystemConfiguration' {aliases} -> aliases) (\s@WindowsFileSystemConfiguration' {} a -> s {aliases = a} :: WindowsFileSystemConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The configuration that Amazon FSx for Windows File Server uses to audit
-- and log user accesses of files, folders, and file shares on the Amazon
-- FSx for Windows File Server file system.
windowsFileSystemConfiguration_auditLogConfiguration :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe WindowsAuditLogConfiguration)
windowsFileSystemConfiguration_auditLogConfiguration = Lens.lens (\WindowsFileSystemConfiguration' {auditLogConfiguration} -> auditLogConfiguration) (\s@WindowsFileSystemConfiguration' {} a -> s {auditLogConfiguration = a} :: WindowsFileSystemConfiguration)

-- | The number of days to retain automatic backups. Setting this to 0
-- disables automatic backups. You can retain automatic backups for a
-- maximum of 90 days.
windowsFileSystemConfiguration_automaticBackupRetentionDays :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe Prelude.Natural)
windowsFileSystemConfiguration_automaticBackupRetentionDays = Lens.lens (\WindowsFileSystemConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@WindowsFileSystemConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: WindowsFileSystemConfiguration)

-- | A boolean flag indicating whether tags on the file system should be
-- copied to backups. This value defaults to false. If it\'s set to true,
-- all tags on the file system are copied to all automatic backups and any
-- user-initiated backups where the user doesn\'t specify any tags. If this
-- value is true, and you specify one or more tags, only the specified tags
-- are copied to backups. If you specify one or more tags when creating a
-- user-initiated backup, no tags are copied from the file system,
-- regardless of this value.
windowsFileSystemConfiguration_copyTagsToBackups :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe Prelude.Bool)
windowsFileSystemConfiguration_copyTagsToBackups = Lens.lens (\WindowsFileSystemConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@WindowsFileSystemConfiguration' {} a -> s {copyTagsToBackups = a} :: WindowsFileSystemConfiguration)

-- | The preferred time to take daily automatic backups, in the UTC time
-- zone.
windowsFileSystemConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe Prelude.Text)
windowsFileSystemConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\WindowsFileSystemConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@WindowsFileSystemConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: WindowsFileSystemConfiguration)

-- | Specifies the file system deployment type, valid values are the
-- following:
--
-- -   @MULTI_AZ_1@ - Specifies a high availability file system that is
--     configured for Multi-AZ redundancy to tolerate temporary
--     Availability Zone (AZ) unavailability, and supports SSD and HDD
--     storage.
--
-- -   @SINGLE_AZ_1@ - (Default) Specifies a file system that is configured
--     for single AZ redundancy, only supports SSD storage.
--
-- -   @SINGLE_AZ_2@ - Latest generation Single AZ file system. Specifies a
--     file system that is configured for single AZ redundancy and supports
--     SSD and HDD storage.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html Single-AZ and Multi-AZ File Systems>.
windowsFileSystemConfiguration_deploymentType :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe WindowsDeploymentType)
windowsFileSystemConfiguration_deploymentType = Lens.lens (\WindowsFileSystemConfiguration' {deploymentType} -> deploymentType) (\s@WindowsFileSystemConfiguration' {} a -> s {deploymentType = a} :: WindowsFileSystemConfiguration)

-- | The list of maintenance operations in progress for this file system.
windowsFileSystemConfiguration_maintenanceOperationsInProgress :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe [FileSystemMaintenanceOperation])
windowsFileSystemConfiguration_maintenanceOperationsInProgress = Lens.lens (\WindowsFileSystemConfiguration' {maintenanceOperationsInProgress} -> maintenanceOperationsInProgress) (\s@WindowsFileSystemConfiguration' {} a -> s {maintenanceOperationsInProgress = a} :: WindowsFileSystemConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | For @MULTI_AZ_1@ deployment types, the IP address of the primary, or
-- preferred, file server.
--
-- Use this IP address when mounting the file system on Linux SMB clients
-- or Windows SMB clients that are not joined to a Microsoft Active
-- Directory. Applicable for all Windows file system deployment types. This
-- IP address is temporarily unavailable when the file system is undergoing
-- maintenance. For Linux and Windows SMB clients that are joined to an
-- Active Directory, use the file system\'s DNSName instead. For more
-- information on mapping and mounting file shares, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/accessing-file-shares.html Accessing File Shares>.
windowsFileSystemConfiguration_preferredFileServerIp :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe Prelude.Text)
windowsFileSystemConfiguration_preferredFileServerIp = Lens.lens (\WindowsFileSystemConfiguration' {preferredFileServerIp} -> preferredFileServerIp) (\s@WindowsFileSystemConfiguration' {} a -> s {preferredFileServerIp = a} :: WindowsFileSystemConfiguration)

-- | For @MULTI_AZ_1@ deployment types, it specifies the ID of the subnet
-- where the preferred file server is located. Must be one of the two
-- subnet IDs specified in @SubnetIds@ property. Amazon FSx serves traffic
-- from this subnet except in the event of a failover to the secondary file
-- server.
--
-- For @SINGLE_AZ_1@ and @SINGLE_AZ_2@ deployment types, this value is the
-- same as that for @SubnetIDs@. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html#single-multi-az-resources Availability and durability: Single-AZ and Multi-AZ file systems>.
windowsFileSystemConfiguration_preferredSubnetId :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe Prelude.Text)
windowsFileSystemConfiguration_preferredSubnetId = Lens.lens (\WindowsFileSystemConfiguration' {preferredSubnetId} -> preferredSubnetId) (\s@WindowsFileSystemConfiguration' {} a -> s {preferredSubnetId = a} :: WindowsFileSystemConfiguration)

-- | For @MULTI_AZ_1@ deployment types, use this endpoint when performing
-- administrative tasks on the file system using Amazon FSx Remote
-- PowerShell.
--
-- For @SINGLE_AZ_1@ and @SINGLE_AZ_2@ deployment types, this is the DNS
-- name of the file system.
--
-- This endpoint is temporarily unavailable when the file system is
-- undergoing maintenance.
windowsFileSystemConfiguration_remoteAdministrationEndpoint :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe Prelude.Text)
windowsFileSystemConfiguration_remoteAdministrationEndpoint = Lens.lens (\WindowsFileSystemConfiguration' {remoteAdministrationEndpoint} -> remoteAdministrationEndpoint) (\s@WindowsFileSystemConfiguration' {} a -> s {remoteAdministrationEndpoint = a} :: WindowsFileSystemConfiguration)

-- | Undocumented member.
windowsFileSystemConfiguration_selfManagedActiveDirectoryConfiguration :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe SelfManagedActiveDirectoryAttributes)
windowsFileSystemConfiguration_selfManagedActiveDirectoryConfiguration = Lens.lens (\WindowsFileSystemConfiguration' {selfManagedActiveDirectoryConfiguration} -> selfManagedActiveDirectoryConfiguration) (\s@WindowsFileSystemConfiguration' {} a -> s {selfManagedActiveDirectoryConfiguration = a} :: WindowsFileSystemConfiguration)

-- | The throughput of the Amazon FSx file system, measured in megabytes per
-- second.
windowsFileSystemConfiguration_throughputCapacity :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe Prelude.Natural)
windowsFileSystemConfiguration_throughputCapacity = Lens.lens (\WindowsFileSystemConfiguration' {throughputCapacity} -> throughputCapacity) (\s@WindowsFileSystemConfiguration' {} a -> s {throughputCapacity = a} :: WindowsFileSystemConfiguration)

-- | The preferred start time to perform weekly maintenance, formatted
-- d:HH:MM in the UTC time zone. d is the weekday number, from 1 through 7,
-- beginning with Monday and ending with Sunday.
windowsFileSystemConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' WindowsFileSystemConfiguration (Prelude.Maybe Prelude.Text)
windowsFileSystemConfiguration_weeklyMaintenanceStartTime = Lens.lens (\WindowsFileSystemConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@WindowsFileSystemConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: WindowsFileSystemConfiguration)

instance Data.FromJSON WindowsFileSystemConfiguration where
  parseJSON =
    Data.withObject
      "WindowsFileSystemConfiguration"
      ( \x ->
          WindowsFileSystemConfiguration'
            Prelude.<$> (x Data..:? "ActiveDirectoryId")
            Prelude.<*> (x Data..:? "Aliases" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AuditLogConfiguration")
            Prelude.<*> (x Data..:? "AutomaticBackupRetentionDays")
            Prelude.<*> (x Data..:? "CopyTagsToBackups")
            Prelude.<*> (x Data..:? "DailyAutomaticBackupStartTime")
            Prelude.<*> (x Data..:? "DeploymentType")
            Prelude.<*> ( x
                            Data..:? "MaintenanceOperationsInProgress"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PreferredFileServerIp")
            Prelude.<*> (x Data..:? "PreferredSubnetId")
            Prelude.<*> (x Data..:? "RemoteAdministrationEndpoint")
            Prelude.<*> ( x
                            Data..:? "SelfManagedActiveDirectoryConfiguration"
                        )
            Prelude.<*> (x Data..:? "ThroughputCapacity")
            Prelude.<*> (x Data..:? "WeeklyMaintenanceStartTime")
      )

instance
  Prelude.Hashable
    WindowsFileSystemConfiguration
  where
  hashWithSalt
    _salt
    WindowsFileSystemConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` activeDirectoryId
        `Prelude.hashWithSalt` aliases
        `Prelude.hashWithSalt` auditLogConfiguration
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` copyTagsToBackups
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` deploymentType
        `Prelude.hashWithSalt` maintenanceOperationsInProgress
        `Prelude.hashWithSalt` preferredFileServerIp
        `Prelude.hashWithSalt` preferredSubnetId
        `Prelude.hashWithSalt` remoteAdministrationEndpoint
        `Prelude.hashWithSalt` selfManagedActiveDirectoryConfiguration
        `Prelude.hashWithSalt` throughputCapacity
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime

instance
  Prelude.NFData
    WindowsFileSystemConfiguration
  where
  rnf WindowsFileSystemConfiguration' {..} =
    Prelude.rnf activeDirectoryId
      `Prelude.seq` Prelude.rnf aliases
      `Prelude.seq` Prelude.rnf auditLogConfiguration
      `Prelude.seq` Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf maintenanceOperationsInProgress
      `Prelude.seq` Prelude.rnf preferredFileServerIp
      `Prelude.seq` Prelude.rnf preferredSubnetId
      `Prelude.seq` Prelude.rnf remoteAdministrationEndpoint
      `Prelude.seq` Prelude.rnf
        selfManagedActiveDirectoryConfiguration
      `Prelude.seq` Prelude.rnf throughputCapacity
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime
