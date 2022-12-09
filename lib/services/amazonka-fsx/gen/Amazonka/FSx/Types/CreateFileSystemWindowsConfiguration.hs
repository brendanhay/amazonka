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
-- Module      : Amazonka.FSx.Types.CreateFileSystemWindowsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CreateFileSystemWindowsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.SelfManagedActiveDirectoryConfiguration
import Amazonka.FSx.Types.WindowsAuditLogCreateConfiguration
import Amazonka.FSx.Types.WindowsDeploymentType
import qualified Amazonka.Prelude as Prelude

-- | The configuration object for the Microsoft Windows file system used in
-- @CreateFileSystem@ and @CreateFileSystemFromBackup@ operations.
--
-- /See:/ 'newCreateFileSystemWindowsConfiguration' smart constructor.
data CreateFileSystemWindowsConfiguration = CreateFileSystemWindowsConfiguration'
  { -- | The ID for an existing Amazon Web Services Managed Microsoft Active
    -- Directory (AD) instance that the file system should join when it\'s
    -- created.
    activeDirectoryId :: Prelude.Maybe Prelude.Text,
    -- | An array of one or more DNS alias names that you want to associate with
    -- the Amazon FSx file system. Aliases allow you to use existing DNS names
    -- to access the data in your Amazon FSx file system. You can associate up
    -- to 50 aliases with a file system at any time. You can associate
    -- additional DNS aliases after you create the file system using the
    -- AssociateFileSystemAliases operation. You can remove DNS aliases from
    -- the file system after it is created using the
    -- DisassociateFileSystemAliases operation. You only need to specify the
    -- alias name in the request payload.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-dns-aliases.html Working with DNS Aliases>
    -- and
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/walkthrough05-file-system-custom-CNAME.html Walkthrough 5: Using DNS aliases to access your file system>,
    -- including additional steps you must take to be able to access your file
    -- system using a DNS alias.
    --
    -- An alias name has to meet the following requirements:
    --
    -- -   Formatted as a fully-qualified domain name (FQDN),
    --     @hostname.domain@, for example, @accounting.example.com@.
    --
    -- -   Can contain alphanumeric characters, the underscore (_), and the
    --     hyphen (-).
    --
    -- -   Cannot start or end with a hyphen.
    --
    -- -   Can start with a numeric.
    --
    -- For DNS alias names, Amazon FSx stores alphabetic characters as
    -- lowercase letters (a-z), regardless of how you specify them: as
    -- uppercase letters, lowercase letters, or the corresponding letters in
    -- escape codes.
    aliases :: Prelude.Maybe [Prelude.Text],
    -- | The configuration that Amazon FSx for Windows File Server uses to audit
    -- and log user accesses of files, folders, and file shares on the Amazon
    -- FSx for Windows File Server file system.
    auditLogConfiguration :: Prelude.Maybe WindowsAuditLogCreateConfiguration,
    -- | The number of days to retain automatic backups. The default is to retain
    -- backups for 7 days. Setting this value to 0 disables the creation of
    -- automatic backups. The maximum retention period for backups is 90 days.
    automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    -- | A boolean flag indicating whether tags for the file system should be
    -- copied to backups. This value defaults to false. If it\'s set to true,
    -- all tags for the file system are copied to all automatic and
    -- user-initiated backups where the user doesn\'t specify tags. If this
    -- value is true, and you specify one or more tags, only the specified tags
    -- are copied to backups. If you specify one or more tags when creating a
    -- user-initiated backup, no tags are copied from the file system,
    -- regardless of this value.
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    -- | The preferred time to take daily automatic backups, formatted HH:MM in
    -- the UTC time zone.
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | Specifies the file system deployment type, valid values are the
    -- following:
    --
    -- -   @MULTI_AZ_1@ - Deploys a high availability file system that is
    --     configured for Multi-AZ redundancy to tolerate temporary
    --     Availability Zone (AZ) unavailability. You can only deploy a
    --     Multi-AZ file system in Amazon Web Services Regions that have a
    --     minimum of three Availability Zones. Also supports HDD storage type
    --
    -- -   @SINGLE_AZ_1@ - (Default) Choose to deploy a file system that is
    --     configured for single AZ redundancy.
    --
    -- -   @SINGLE_AZ_2@ - The latest generation Single AZ file system.
    --     Specifies a file system that is configured for single AZ redundancy
    --     and supports HDD storage type.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html Availability and Durability: Single-AZ and Multi-AZ File Systems>.
    deploymentType :: Prelude.Maybe WindowsDeploymentType,
    -- | Required when @DeploymentType@ is set to @MULTI_AZ_1@. This specifies
    -- the subnet in which you want the preferred file server to be located.
    -- For in-Amazon Web Services applications, we recommend that you launch
    -- your clients in the same Availability Zone (AZ) as your preferred file
    -- server to reduce cross-AZ data transfer costs and minimize latency.
    preferredSubnetId :: Prelude.Maybe Prelude.Text,
    selfManagedActiveDirectoryConfiguration :: Prelude.Maybe SelfManagedActiveDirectoryConfiguration,
    -- | The preferred start time to perform weekly maintenance, formatted
    -- d:HH:MM in the UTC time zone, where d is the weekday number, from 1
    -- through 7, beginning with Monday and ending with Sunday.
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
    -- | Sets the throughput capacity of an Amazon FSx file system, measured in
    -- megabytes per second (MB\/s), in 2 to the /n/th increments, between 2^3
    -- (8) and 2^11 (2048).
    throughputCapacity :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileSystemWindowsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryId', 'createFileSystemWindowsConfiguration_activeDirectoryId' - The ID for an existing Amazon Web Services Managed Microsoft Active
-- Directory (AD) instance that the file system should join when it\'s
-- created.
--
-- 'aliases', 'createFileSystemWindowsConfiguration_aliases' - An array of one or more DNS alias names that you want to associate with
-- the Amazon FSx file system. Aliases allow you to use existing DNS names
-- to access the data in your Amazon FSx file system. You can associate up
-- to 50 aliases with a file system at any time. You can associate
-- additional DNS aliases after you create the file system using the
-- AssociateFileSystemAliases operation. You can remove DNS aliases from
-- the file system after it is created using the
-- DisassociateFileSystemAliases operation. You only need to specify the
-- alias name in the request payload.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-dns-aliases.html Working with DNS Aliases>
-- and
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/walkthrough05-file-system-custom-CNAME.html Walkthrough 5: Using DNS aliases to access your file system>,
-- including additional steps you must take to be able to access your file
-- system using a DNS alias.
--
-- An alias name has to meet the following requirements:
--
-- -   Formatted as a fully-qualified domain name (FQDN),
--     @hostname.domain@, for example, @accounting.example.com@.
--
-- -   Can contain alphanumeric characters, the underscore (_), and the
--     hyphen (-).
--
-- -   Cannot start or end with a hyphen.
--
-- -   Can start with a numeric.
--
-- For DNS alias names, Amazon FSx stores alphabetic characters as
-- lowercase letters (a-z), regardless of how you specify them: as
-- uppercase letters, lowercase letters, or the corresponding letters in
-- escape codes.
--
-- 'auditLogConfiguration', 'createFileSystemWindowsConfiguration_auditLogConfiguration' - The configuration that Amazon FSx for Windows File Server uses to audit
-- and log user accesses of files, folders, and file shares on the Amazon
-- FSx for Windows File Server file system.
--
-- 'automaticBackupRetentionDays', 'createFileSystemWindowsConfiguration_automaticBackupRetentionDays' - The number of days to retain automatic backups. The default is to retain
-- backups for 7 days. Setting this value to 0 disables the creation of
-- automatic backups. The maximum retention period for backups is 90 days.
--
-- 'copyTagsToBackups', 'createFileSystemWindowsConfiguration_copyTagsToBackups' - A boolean flag indicating whether tags for the file system should be
-- copied to backups. This value defaults to false. If it\'s set to true,
-- all tags for the file system are copied to all automatic and
-- user-initiated backups where the user doesn\'t specify tags. If this
-- value is true, and you specify one or more tags, only the specified tags
-- are copied to backups. If you specify one or more tags when creating a
-- user-initiated backup, no tags are copied from the file system,
-- regardless of this value.
--
-- 'dailyAutomaticBackupStartTime', 'createFileSystemWindowsConfiguration_dailyAutomaticBackupStartTime' - The preferred time to take daily automatic backups, formatted HH:MM in
-- the UTC time zone.
--
-- 'deploymentType', 'createFileSystemWindowsConfiguration_deploymentType' - Specifies the file system deployment type, valid values are the
-- following:
--
-- -   @MULTI_AZ_1@ - Deploys a high availability file system that is
--     configured for Multi-AZ redundancy to tolerate temporary
--     Availability Zone (AZ) unavailability. You can only deploy a
--     Multi-AZ file system in Amazon Web Services Regions that have a
--     minimum of three Availability Zones. Also supports HDD storage type
--
-- -   @SINGLE_AZ_1@ - (Default) Choose to deploy a file system that is
--     configured for single AZ redundancy.
--
-- -   @SINGLE_AZ_2@ - The latest generation Single AZ file system.
--     Specifies a file system that is configured for single AZ redundancy
--     and supports HDD storage type.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html Availability and Durability: Single-AZ and Multi-AZ File Systems>.
--
-- 'preferredSubnetId', 'createFileSystemWindowsConfiguration_preferredSubnetId' - Required when @DeploymentType@ is set to @MULTI_AZ_1@. This specifies
-- the subnet in which you want the preferred file server to be located.
-- For in-Amazon Web Services applications, we recommend that you launch
-- your clients in the same Availability Zone (AZ) as your preferred file
-- server to reduce cross-AZ data transfer costs and minimize latency.
--
-- 'selfManagedActiveDirectoryConfiguration', 'createFileSystemWindowsConfiguration_selfManagedActiveDirectoryConfiguration' - Undocumented member.
--
-- 'weeklyMaintenanceStartTime', 'createFileSystemWindowsConfiguration_weeklyMaintenanceStartTime' - The preferred start time to perform weekly maintenance, formatted
-- d:HH:MM in the UTC time zone, where d is the weekday number, from 1
-- through 7, beginning with Monday and ending with Sunday.
--
-- 'throughputCapacity', 'createFileSystemWindowsConfiguration_throughputCapacity' - Sets the throughput capacity of an Amazon FSx file system, measured in
-- megabytes per second (MB\/s), in 2 to the /n/th increments, between 2^3
-- (8) and 2^11 (2048).
newCreateFileSystemWindowsConfiguration ::
  -- | 'throughputCapacity'
  Prelude.Natural ->
  CreateFileSystemWindowsConfiguration
newCreateFileSystemWindowsConfiguration
  pThroughputCapacity_ =
    CreateFileSystemWindowsConfiguration'
      { activeDirectoryId =
          Prelude.Nothing,
        aliases = Prelude.Nothing,
        auditLogConfiguration =
          Prelude.Nothing,
        automaticBackupRetentionDays =
          Prelude.Nothing,
        copyTagsToBackups = Prelude.Nothing,
        dailyAutomaticBackupStartTime =
          Prelude.Nothing,
        deploymentType = Prelude.Nothing,
        preferredSubnetId = Prelude.Nothing,
        selfManagedActiveDirectoryConfiguration =
          Prelude.Nothing,
        weeklyMaintenanceStartTime =
          Prelude.Nothing,
        throughputCapacity =
          pThroughputCapacity_
      }

-- | The ID for an existing Amazon Web Services Managed Microsoft Active
-- Directory (AD) instance that the file system should join when it\'s
-- created.
createFileSystemWindowsConfiguration_activeDirectoryId :: Lens.Lens' CreateFileSystemWindowsConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemWindowsConfiguration_activeDirectoryId = Lens.lens (\CreateFileSystemWindowsConfiguration' {activeDirectoryId} -> activeDirectoryId) (\s@CreateFileSystemWindowsConfiguration' {} a -> s {activeDirectoryId = a} :: CreateFileSystemWindowsConfiguration)

-- | An array of one or more DNS alias names that you want to associate with
-- the Amazon FSx file system. Aliases allow you to use existing DNS names
-- to access the data in your Amazon FSx file system. You can associate up
-- to 50 aliases with a file system at any time. You can associate
-- additional DNS aliases after you create the file system using the
-- AssociateFileSystemAliases operation. You can remove DNS aliases from
-- the file system after it is created using the
-- DisassociateFileSystemAliases operation. You only need to specify the
-- alias name in the request payload.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-dns-aliases.html Working with DNS Aliases>
-- and
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/walkthrough05-file-system-custom-CNAME.html Walkthrough 5: Using DNS aliases to access your file system>,
-- including additional steps you must take to be able to access your file
-- system using a DNS alias.
--
-- An alias name has to meet the following requirements:
--
-- -   Formatted as a fully-qualified domain name (FQDN),
--     @hostname.domain@, for example, @accounting.example.com@.
--
-- -   Can contain alphanumeric characters, the underscore (_), and the
--     hyphen (-).
--
-- -   Cannot start or end with a hyphen.
--
-- -   Can start with a numeric.
--
-- For DNS alias names, Amazon FSx stores alphabetic characters as
-- lowercase letters (a-z), regardless of how you specify them: as
-- uppercase letters, lowercase letters, or the corresponding letters in
-- escape codes.
createFileSystemWindowsConfiguration_aliases :: Lens.Lens' CreateFileSystemWindowsConfiguration (Prelude.Maybe [Prelude.Text])
createFileSystemWindowsConfiguration_aliases = Lens.lens (\CreateFileSystemWindowsConfiguration' {aliases} -> aliases) (\s@CreateFileSystemWindowsConfiguration' {} a -> s {aliases = a} :: CreateFileSystemWindowsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The configuration that Amazon FSx for Windows File Server uses to audit
-- and log user accesses of files, folders, and file shares on the Amazon
-- FSx for Windows File Server file system.
createFileSystemWindowsConfiguration_auditLogConfiguration :: Lens.Lens' CreateFileSystemWindowsConfiguration (Prelude.Maybe WindowsAuditLogCreateConfiguration)
createFileSystemWindowsConfiguration_auditLogConfiguration = Lens.lens (\CreateFileSystemWindowsConfiguration' {auditLogConfiguration} -> auditLogConfiguration) (\s@CreateFileSystemWindowsConfiguration' {} a -> s {auditLogConfiguration = a} :: CreateFileSystemWindowsConfiguration)

-- | The number of days to retain automatic backups. The default is to retain
-- backups for 7 days. Setting this value to 0 disables the creation of
-- automatic backups. The maximum retention period for backups is 90 days.
createFileSystemWindowsConfiguration_automaticBackupRetentionDays :: Lens.Lens' CreateFileSystemWindowsConfiguration (Prelude.Maybe Prelude.Natural)
createFileSystemWindowsConfiguration_automaticBackupRetentionDays = Lens.lens (\CreateFileSystemWindowsConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@CreateFileSystemWindowsConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: CreateFileSystemWindowsConfiguration)

-- | A boolean flag indicating whether tags for the file system should be
-- copied to backups. This value defaults to false. If it\'s set to true,
-- all tags for the file system are copied to all automatic and
-- user-initiated backups where the user doesn\'t specify tags. If this
-- value is true, and you specify one or more tags, only the specified tags
-- are copied to backups. If you specify one or more tags when creating a
-- user-initiated backup, no tags are copied from the file system,
-- regardless of this value.
createFileSystemWindowsConfiguration_copyTagsToBackups :: Lens.Lens' CreateFileSystemWindowsConfiguration (Prelude.Maybe Prelude.Bool)
createFileSystemWindowsConfiguration_copyTagsToBackups = Lens.lens (\CreateFileSystemWindowsConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@CreateFileSystemWindowsConfiguration' {} a -> s {copyTagsToBackups = a} :: CreateFileSystemWindowsConfiguration)

-- | The preferred time to take daily automatic backups, formatted HH:MM in
-- the UTC time zone.
createFileSystemWindowsConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' CreateFileSystemWindowsConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemWindowsConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\CreateFileSystemWindowsConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@CreateFileSystemWindowsConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: CreateFileSystemWindowsConfiguration)

-- | Specifies the file system deployment type, valid values are the
-- following:
--
-- -   @MULTI_AZ_1@ - Deploys a high availability file system that is
--     configured for Multi-AZ redundancy to tolerate temporary
--     Availability Zone (AZ) unavailability. You can only deploy a
--     Multi-AZ file system in Amazon Web Services Regions that have a
--     minimum of three Availability Zones. Also supports HDD storage type
--
-- -   @SINGLE_AZ_1@ - (Default) Choose to deploy a file system that is
--     configured for single AZ redundancy.
--
-- -   @SINGLE_AZ_2@ - The latest generation Single AZ file system.
--     Specifies a file system that is configured for single AZ redundancy
--     and supports HDD storage type.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html Availability and Durability: Single-AZ and Multi-AZ File Systems>.
createFileSystemWindowsConfiguration_deploymentType :: Lens.Lens' CreateFileSystemWindowsConfiguration (Prelude.Maybe WindowsDeploymentType)
createFileSystemWindowsConfiguration_deploymentType = Lens.lens (\CreateFileSystemWindowsConfiguration' {deploymentType} -> deploymentType) (\s@CreateFileSystemWindowsConfiguration' {} a -> s {deploymentType = a} :: CreateFileSystemWindowsConfiguration)

-- | Required when @DeploymentType@ is set to @MULTI_AZ_1@. This specifies
-- the subnet in which you want the preferred file server to be located.
-- For in-Amazon Web Services applications, we recommend that you launch
-- your clients in the same Availability Zone (AZ) as your preferred file
-- server to reduce cross-AZ data transfer costs and minimize latency.
createFileSystemWindowsConfiguration_preferredSubnetId :: Lens.Lens' CreateFileSystemWindowsConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemWindowsConfiguration_preferredSubnetId = Lens.lens (\CreateFileSystemWindowsConfiguration' {preferredSubnetId} -> preferredSubnetId) (\s@CreateFileSystemWindowsConfiguration' {} a -> s {preferredSubnetId = a} :: CreateFileSystemWindowsConfiguration)

-- | Undocumented member.
createFileSystemWindowsConfiguration_selfManagedActiveDirectoryConfiguration :: Lens.Lens' CreateFileSystemWindowsConfiguration (Prelude.Maybe SelfManagedActiveDirectoryConfiguration)
createFileSystemWindowsConfiguration_selfManagedActiveDirectoryConfiguration = Lens.lens (\CreateFileSystemWindowsConfiguration' {selfManagedActiveDirectoryConfiguration} -> selfManagedActiveDirectoryConfiguration) (\s@CreateFileSystemWindowsConfiguration' {} a -> s {selfManagedActiveDirectoryConfiguration = a} :: CreateFileSystemWindowsConfiguration)

-- | The preferred start time to perform weekly maintenance, formatted
-- d:HH:MM in the UTC time zone, where d is the weekday number, from 1
-- through 7, beginning with Monday and ending with Sunday.
createFileSystemWindowsConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' CreateFileSystemWindowsConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemWindowsConfiguration_weeklyMaintenanceStartTime = Lens.lens (\CreateFileSystemWindowsConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@CreateFileSystemWindowsConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: CreateFileSystemWindowsConfiguration)

-- | Sets the throughput capacity of an Amazon FSx file system, measured in
-- megabytes per second (MB\/s), in 2 to the /n/th increments, between 2^3
-- (8) and 2^11 (2048).
createFileSystemWindowsConfiguration_throughputCapacity :: Lens.Lens' CreateFileSystemWindowsConfiguration Prelude.Natural
createFileSystemWindowsConfiguration_throughputCapacity = Lens.lens (\CreateFileSystemWindowsConfiguration' {throughputCapacity} -> throughputCapacity) (\s@CreateFileSystemWindowsConfiguration' {} a -> s {throughputCapacity = a} :: CreateFileSystemWindowsConfiguration)

instance
  Prelude.Hashable
    CreateFileSystemWindowsConfiguration
  where
  hashWithSalt
    _salt
    CreateFileSystemWindowsConfiguration' {..} =
      _salt `Prelude.hashWithSalt` activeDirectoryId
        `Prelude.hashWithSalt` aliases
        `Prelude.hashWithSalt` auditLogConfiguration
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` copyTagsToBackups
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` deploymentType
        `Prelude.hashWithSalt` preferredSubnetId
        `Prelude.hashWithSalt` selfManagedActiveDirectoryConfiguration
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime
        `Prelude.hashWithSalt` throughputCapacity

instance
  Prelude.NFData
    CreateFileSystemWindowsConfiguration
  where
  rnf CreateFileSystemWindowsConfiguration' {..} =
    Prelude.rnf activeDirectoryId
      `Prelude.seq` Prelude.rnf aliases
      `Prelude.seq` Prelude.rnf auditLogConfiguration
      `Prelude.seq` Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf preferredSubnetId
      `Prelude.seq` Prelude.rnf selfManagedActiveDirectoryConfiguration
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime
      `Prelude.seq` Prelude.rnf throughputCapacity

instance
  Data.ToJSON
    CreateFileSystemWindowsConfiguration
  where
  toJSON CreateFileSystemWindowsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActiveDirectoryId" Data..=)
              Prelude.<$> activeDirectoryId,
            ("Aliases" Data..=) Prelude.<$> aliases,
            ("AuditLogConfiguration" Data..=)
              Prelude.<$> auditLogConfiguration,
            ("AutomaticBackupRetentionDays" Data..=)
              Prelude.<$> automaticBackupRetentionDays,
            ("CopyTagsToBackups" Data..=)
              Prelude.<$> copyTagsToBackups,
            ("DailyAutomaticBackupStartTime" Data..=)
              Prelude.<$> dailyAutomaticBackupStartTime,
            ("DeploymentType" Data..=)
              Prelude.<$> deploymentType,
            ("PreferredSubnetId" Data..=)
              Prelude.<$> preferredSubnetId,
            ("SelfManagedActiveDirectoryConfiguration" Data..=)
              Prelude.<$> selfManagedActiveDirectoryConfiguration,
            ("WeeklyMaintenanceStartTime" Data..=)
              Prelude.<$> weeklyMaintenanceStartTime,
            Prelude.Just
              ("ThroughputCapacity" Data..= throughputCapacity)
          ]
      )
