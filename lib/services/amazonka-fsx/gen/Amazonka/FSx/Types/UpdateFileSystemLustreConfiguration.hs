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
-- Module      : Amazonka.FSx.Types.UpdateFileSystemLustreConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.UpdateFileSystemLustreConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.AutoImportPolicyType
import Amazonka.FSx.Types.DataCompressionType
import Amazonka.FSx.Types.LustreLogCreateConfiguration
import Amazonka.FSx.Types.LustreRootSquashConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration object for Amazon FSx for Lustre file systems used in
-- the @UpdateFileSystem@ operation.
--
-- /See:/ 'newUpdateFileSystemLustreConfiguration' smart constructor.
data UpdateFileSystemLustreConfiguration = UpdateFileSystemLustreConfiguration'
  { -- | (Optional) When you create your file system, your existing S3 objects
    -- appear as file and directory listings. Use this property to choose how
    -- Amazon FSx keeps your file and directory listing up to date as you add
    -- or modify objects in your linked S3 bucket. @AutoImportPolicy@ can have
    -- the following values:
    --
    -- -   @NONE@ - (Default) AutoImport is off. Amazon FSx only updates file
    --     and directory listings from the linked S3 bucket when the file
    --     system is created. FSx does not update the file and directory
    --     listing for any new or changed objects after choosing this option.
    --
    -- -   @NEW@ - AutoImport is on. Amazon FSx automatically imports directory
    --     listings of any new objects added to the linked S3 bucket that do
    --     not currently exist in the FSx file system.
    --
    -- -   @NEW_CHANGED@ - AutoImport is on. Amazon FSx automatically imports
    --     file and directory listings of any new objects added to the S3
    --     bucket and any existing objects that are changed in the S3 bucket
    --     after you choose this option.
    --
    -- -   @NEW_CHANGED_DELETED@ - AutoImport is on. Amazon FSx automatically
    --     imports file and directory listings of any new objects added to the
    --     S3 bucket, any existing objects that are changed in the S3 bucket,
    --     and any objects that were deleted in the S3 bucket.
    --
    -- The @AutoImportPolicy@ parameter is not supported for Lustre file
    -- systems with the @Persistent_2@ deployment type. Instead, use to update
    -- a data repository association on your @Persistent_2@ file system.
    autoImportPolicy :: Prelude.Maybe AutoImportPolicyType,
    automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | Sets the data compression configuration for the file system.
    -- @DataCompressionType@ can have the following values:
    --
    -- -   @NONE@ - Data compression is turned off for the file system.
    --
    -- -   @LZ4@ - Data compression is turned on with the LZ4 algorithm.
    --
    -- If you don\'t use @DataCompressionType@, the file system retains its
    -- current data compression configuration.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/data-compression.html Lustre data compression>.
    dataCompressionType :: Prelude.Maybe DataCompressionType,
    -- | The Lustre logging configuration used when updating an Amazon FSx for
    -- Lustre file system. When logging is enabled, Lustre logs error and
    -- warning events for data repositories associated with your file system to
    -- Amazon CloudWatch Logs.
    logConfiguration :: Prelude.Maybe LustreLogCreateConfiguration,
    -- | The Lustre root squash configuration used when updating an Amazon FSx
    -- for Lustre file system. When enabled, root squash restricts root-level
    -- access from clients that try to access your file system as a root user.
    rootSquashConfiguration :: Prelude.Maybe LustreRootSquashConfiguration,
    -- | (Optional) The preferred start time to perform weekly maintenance,
    -- formatted d:HH:MM in the UTC time zone. d is the weekday number, from 1
    -- through 7, beginning with Monday and ending with Sunday.
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFileSystemLustreConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoImportPolicy', 'updateFileSystemLustreConfiguration_autoImportPolicy' - (Optional) When you create your file system, your existing S3 objects
-- appear as file and directory listings. Use this property to choose how
-- Amazon FSx keeps your file and directory listing up to date as you add
-- or modify objects in your linked S3 bucket. @AutoImportPolicy@ can have
-- the following values:
--
-- -   @NONE@ - (Default) AutoImport is off. Amazon FSx only updates file
--     and directory listings from the linked S3 bucket when the file
--     system is created. FSx does not update the file and directory
--     listing for any new or changed objects after choosing this option.
--
-- -   @NEW@ - AutoImport is on. Amazon FSx automatically imports directory
--     listings of any new objects added to the linked S3 bucket that do
--     not currently exist in the FSx file system.
--
-- -   @NEW_CHANGED@ - AutoImport is on. Amazon FSx automatically imports
--     file and directory listings of any new objects added to the S3
--     bucket and any existing objects that are changed in the S3 bucket
--     after you choose this option.
--
-- -   @NEW_CHANGED_DELETED@ - AutoImport is on. Amazon FSx automatically
--     imports file and directory listings of any new objects added to the
--     S3 bucket, any existing objects that are changed in the S3 bucket,
--     and any objects that were deleted in the S3 bucket.
--
-- The @AutoImportPolicy@ parameter is not supported for Lustre file
-- systems with the @Persistent_2@ deployment type. Instead, use to update
-- a data repository association on your @Persistent_2@ file system.
--
-- 'automaticBackupRetentionDays', 'updateFileSystemLustreConfiguration_automaticBackupRetentionDays' - Undocumented member.
--
-- 'dailyAutomaticBackupStartTime', 'updateFileSystemLustreConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
--
-- 'dataCompressionType', 'updateFileSystemLustreConfiguration_dataCompressionType' - Sets the data compression configuration for the file system.
-- @DataCompressionType@ can have the following values:
--
-- -   @NONE@ - Data compression is turned off for the file system.
--
-- -   @LZ4@ - Data compression is turned on with the LZ4 algorithm.
--
-- If you don\'t use @DataCompressionType@, the file system retains its
-- current data compression configuration.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/data-compression.html Lustre data compression>.
--
-- 'logConfiguration', 'updateFileSystemLustreConfiguration_logConfiguration' - The Lustre logging configuration used when updating an Amazon FSx for
-- Lustre file system. When logging is enabled, Lustre logs error and
-- warning events for data repositories associated with your file system to
-- Amazon CloudWatch Logs.
--
-- 'rootSquashConfiguration', 'updateFileSystemLustreConfiguration_rootSquashConfiguration' - The Lustre root squash configuration used when updating an Amazon FSx
-- for Lustre file system. When enabled, root squash restricts root-level
-- access from clients that try to access your file system as a root user.
--
-- 'weeklyMaintenanceStartTime', 'updateFileSystemLustreConfiguration_weeklyMaintenanceStartTime' - (Optional) The preferred start time to perform weekly maintenance,
-- formatted d:HH:MM in the UTC time zone. d is the weekday number, from 1
-- through 7, beginning with Monday and ending with Sunday.
newUpdateFileSystemLustreConfiguration ::
  UpdateFileSystemLustreConfiguration
newUpdateFileSystemLustreConfiguration =
  UpdateFileSystemLustreConfiguration'
    { autoImportPolicy =
        Prelude.Nothing,
      automaticBackupRetentionDays =
        Prelude.Nothing,
      dailyAutomaticBackupStartTime =
        Prelude.Nothing,
      dataCompressionType = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      rootSquashConfiguration =
        Prelude.Nothing,
      weeklyMaintenanceStartTime =
        Prelude.Nothing
    }

-- | (Optional) When you create your file system, your existing S3 objects
-- appear as file and directory listings. Use this property to choose how
-- Amazon FSx keeps your file and directory listing up to date as you add
-- or modify objects in your linked S3 bucket. @AutoImportPolicy@ can have
-- the following values:
--
-- -   @NONE@ - (Default) AutoImport is off. Amazon FSx only updates file
--     and directory listings from the linked S3 bucket when the file
--     system is created. FSx does not update the file and directory
--     listing for any new or changed objects after choosing this option.
--
-- -   @NEW@ - AutoImport is on. Amazon FSx automatically imports directory
--     listings of any new objects added to the linked S3 bucket that do
--     not currently exist in the FSx file system.
--
-- -   @NEW_CHANGED@ - AutoImport is on. Amazon FSx automatically imports
--     file and directory listings of any new objects added to the S3
--     bucket and any existing objects that are changed in the S3 bucket
--     after you choose this option.
--
-- -   @NEW_CHANGED_DELETED@ - AutoImport is on. Amazon FSx automatically
--     imports file and directory listings of any new objects added to the
--     S3 bucket, any existing objects that are changed in the S3 bucket,
--     and any objects that were deleted in the S3 bucket.
--
-- The @AutoImportPolicy@ parameter is not supported for Lustre file
-- systems with the @Persistent_2@ deployment type. Instead, use to update
-- a data repository association on your @Persistent_2@ file system.
updateFileSystemLustreConfiguration_autoImportPolicy :: Lens.Lens' UpdateFileSystemLustreConfiguration (Prelude.Maybe AutoImportPolicyType)
updateFileSystemLustreConfiguration_autoImportPolicy = Lens.lens (\UpdateFileSystemLustreConfiguration' {autoImportPolicy} -> autoImportPolicy) (\s@UpdateFileSystemLustreConfiguration' {} a -> s {autoImportPolicy = a} :: UpdateFileSystemLustreConfiguration)

-- | Undocumented member.
updateFileSystemLustreConfiguration_automaticBackupRetentionDays :: Lens.Lens' UpdateFileSystemLustreConfiguration (Prelude.Maybe Prelude.Natural)
updateFileSystemLustreConfiguration_automaticBackupRetentionDays = Lens.lens (\UpdateFileSystemLustreConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@UpdateFileSystemLustreConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: UpdateFileSystemLustreConfiguration)

-- | Undocumented member.
updateFileSystemLustreConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' UpdateFileSystemLustreConfiguration (Prelude.Maybe Prelude.Text)
updateFileSystemLustreConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\UpdateFileSystemLustreConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@UpdateFileSystemLustreConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: UpdateFileSystemLustreConfiguration)

-- | Sets the data compression configuration for the file system.
-- @DataCompressionType@ can have the following values:
--
-- -   @NONE@ - Data compression is turned off for the file system.
--
-- -   @LZ4@ - Data compression is turned on with the LZ4 algorithm.
--
-- If you don\'t use @DataCompressionType@, the file system retains its
-- current data compression configuration.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/data-compression.html Lustre data compression>.
updateFileSystemLustreConfiguration_dataCompressionType :: Lens.Lens' UpdateFileSystemLustreConfiguration (Prelude.Maybe DataCompressionType)
updateFileSystemLustreConfiguration_dataCompressionType = Lens.lens (\UpdateFileSystemLustreConfiguration' {dataCompressionType} -> dataCompressionType) (\s@UpdateFileSystemLustreConfiguration' {} a -> s {dataCompressionType = a} :: UpdateFileSystemLustreConfiguration)

-- | The Lustre logging configuration used when updating an Amazon FSx for
-- Lustre file system. When logging is enabled, Lustre logs error and
-- warning events for data repositories associated with your file system to
-- Amazon CloudWatch Logs.
updateFileSystemLustreConfiguration_logConfiguration :: Lens.Lens' UpdateFileSystemLustreConfiguration (Prelude.Maybe LustreLogCreateConfiguration)
updateFileSystemLustreConfiguration_logConfiguration = Lens.lens (\UpdateFileSystemLustreConfiguration' {logConfiguration} -> logConfiguration) (\s@UpdateFileSystemLustreConfiguration' {} a -> s {logConfiguration = a} :: UpdateFileSystemLustreConfiguration)

-- | The Lustre root squash configuration used when updating an Amazon FSx
-- for Lustre file system. When enabled, root squash restricts root-level
-- access from clients that try to access your file system as a root user.
updateFileSystemLustreConfiguration_rootSquashConfiguration :: Lens.Lens' UpdateFileSystemLustreConfiguration (Prelude.Maybe LustreRootSquashConfiguration)
updateFileSystemLustreConfiguration_rootSquashConfiguration = Lens.lens (\UpdateFileSystemLustreConfiguration' {rootSquashConfiguration} -> rootSquashConfiguration) (\s@UpdateFileSystemLustreConfiguration' {} a -> s {rootSquashConfiguration = a} :: UpdateFileSystemLustreConfiguration)

-- | (Optional) The preferred start time to perform weekly maintenance,
-- formatted d:HH:MM in the UTC time zone. d is the weekday number, from 1
-- through 7, beginning with Monday and ending with Sunday.
updateFileSystemLustreConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' UpdateFileSystemLustreConfiguration (Prelude.Maybe Prelude.Text)
updateFileSystemLustreConfiguration_weeklyMaintenanceStartTime = Lens.lens (\UpdateFileSystemLustreConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@UpdateFileSystemLustreConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: UpdateFileSystemLustreConfiguration)

instance
  Prelude.Hashable
    UpdateFileSystemLustreConfiguration
  where
  hashWithSalt
    _salt
    UpdateFileSystemLustreConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` autoImportPolicy
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` dataCompressionType
        `Prelude.hashWithSalt` logConfiguration
        `Prelude.hashWithSalt` rootSquashConfiguration
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime

instance
  Prelude.NFData
    UpdateFileSystemLustreConfiguration
  where
  rnf UpdateFileSystemLustreConfiguration' {..} =
    Prelude.rnf autoImportPolicy
      `Prelude.seq` Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf dataCompressionType
      `Prelude.seq` Prelude.rnf logConfiguration
      `Prelude.seq` Prelude.rnf rootSquashConfiguration
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime

instance
  Data.ToJSON
    UpdateFileSystemLustreConfiguration
  where
  toJSON UpdateFileSystemLustreConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoImportPolicy" Data..=)
              Prelude.<$> autoImportPolicy,
            ("AutomaticBackupRetentionDays" Data..=)
              Prelude.<$> automaticBackupRetentionDays,
            ("DailyAutomaticBackupStartTime" Data..=)
              Prelude.<$> dailyAutomaticBackupStartTime,
            ("DataCompressionType" Data..=)
              Prelude.<$> dataCompressionType,
            ("LogConfiguration" Data..=)
              Prelude.<$> logConfiguration,
            ("RootSquashConfiguration" Data..=)
              Prelude.<$> rootSquashConfiguration,
            ("WeeklyMaintenanceStartTime" Data..=)
              Prelude.<$> weeklyMaintenanceStartTime
          ]
      )
