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
-- Module      : Amazonka.FSx.Types.CreateFileSystemLustreConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CreateFileSystemLustreConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.AutoImportPolicyType
import Amazonka.FSx.Types.DataCompressionType
import Amazonka.FSx.Types.DriveCacheType
import Amazonka.FSx.Types.LustreDeploymentType
import Amazonka.FSx.Types.LustreLogCreateConfiguration
import Amazonka.FSx.Types.LustreRootSquashConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The Lustre configuration for the file system being created.
--
-- The following parameters are not supported for file systems with a data
-- repository association created with .
--
-- -   @AutoImportPolicy@
--
-- -   @ExportPath@
--
-- -   @ImportedChunkSize@
--
-- -   @ImportPath@
--
-- /See:/ 'newCreateFileSystemLustreConfiguration' smart constructor.
data CreateFileSystemLustreConfiguration = CreateFileSystemLustreConfiguration'
  { -- | (Optional) When you create your file system, your existing S3 objects
    -- appear as file and directory listings. Use this parameter to choose how
    -- Amazon FSx keeps your file and directory listings up to date as you add
    -- or modify objects in your linked S3 bucket. @AutoImportPolicy@ can have
    -- the following values:
    --
    -- -   @NONE@ - (Default) AutoImport is off. Amazon FSx only updates file
    --     and directory listings from the linked S3 bucket when the file
    --     system is created. FSx does not update file and directory listings
    --     for any new or changed objects after choosing this option.
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
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/older-deployment-types.html#legacy-auto-import-from-s3 Automatically import updates from your S3 bucket>.
    --
    -- This parameter is not supported for file systems with a data repository
    -- association.
    autoImportPolicy :: Prelude.Maybe AutoImportPolicyType,
    -- | The number of days to retain automatic backups. Setting this property to
    -- @0@ disables automatic backups. You can retain automatic backups for a
    -- maximum of 90 days. The default is @0@.
    automaticBackupRetentionDays :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) Not available for use with file systems that are linked to a
    -- data repository. A boolean flag indicating whether tags for the file
    -- system should be copied to backups. The default value is false. If
    -- @CopyTagsToBackups@ is set to true, all file system tags are copied to
    -- all automatic and user-initiated backups when the user doesn\'t specify
    -- any backup-specific tags. If @CopyTagsToBackups@ is set to true and you
    -- specify one or more backup tags, only the specified tags are copied to
    -- backups. If you specify one or more tags when creating a user-initiated
    -- backup, no tags are copied from the file system, regardless of this
    -- value.
    --
    -- (Default = @false@)
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/using-backups-fsx.html Working with backups>
    -- in the /Amazon FSx for Lustre User Guide/.
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    dailyAutomaticBackupStartTime :: Prelude.Maybe Prelude.Text,
    -- | Sets the data compression configuration for the file system.
    -- @DataCompressionType@ can have the following values:
    --
    -- -   @NONE@ - (Default) Data compression is turned off when the file
    --     system is created.
    --
    -- -   @LZ4@ - Data compression is turned on with the LZ4 algorithm.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/data-compression.html Lustre data compression>
    -- in the /Amazon FSx for Lustre User Guide/.
    dataCompressionType :: Prelude.Maybe DataCompressionType,
    -- | (Optional) Choose @SCRATCH_1@ and @SCRATCH_2@ deployment types when you
    -- need temporary storage and shorter-term processing of data. The
    -- @SCRATCH_2@ deployment type provides in-transit encryption of data and
    -- higher burst throughput capacity than @SCRATCH_1@.
    --
    -- Choose @PERSISTENT_1@ for longer-term storage and for throughput-focused
    -- workloads that aren’t latency-sensitive. @PERSISTENT_1@ supports
    -- encryption of data in transit, and is available in all Amazon Web
    -- Services Regions in which FSx for Lustre is available.
    --
    -- Choose @PERSISTENT_2@ for longer-term storage and for latency-sensitive
    -- workloads that require the highest levels of IOPS\/throughput.
    -- @PERSISTENT_2@ supports SSD storage, and offers higher
    -- @PerUnitStorageThroughput@ (up to 1000 MB\/s\/TiB). @PERSISTENT_2@ is
    -- available in a limited number of Amazon Web Services Regions. For more
    -- information, and an up-to-date list of Amazon Web Services Regions in
    -- which @PERSISTENT_2@ is available, see
    -- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/using-fsx-lustre.html#lustre-deployment-types File system deployment options for FSx for Lustre>
    -- in the /Amazon FSx for Lustre User Guide/.
    --
    -- If you choose @PERSISTENT_2@, and you set @FileSystemTypeVersion@ to
    -- @2.10@, the @CreateFileSystem@ operation fails.
    --
    -- Encryption of data in transit is automatically turned on when you access
    -- @SCRATCH_2@, @PERSISTENT_1@ and @PERSISTENT_2@ file systems from Amazon
    -- EC2 instances that support automatic encryption in the Amazon Web
    -- Services Regions where they are available. For more information about
    -- encryption in transit for FSx for Lustre file systems, see
    -- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/encryption-in-transit-fsxl.html Encrypting data in transit>
    -- in the /Amazon FSx for Lustre User Guide/.
    --
    -- (Default = @SCRATCH_1@)
    deploymentType :: Prelude.Maybe LustreDeploymentType,
    -- | The type of drive cache used by @PERSISTENT_1@ file systems that are
    -- provisioned with HDD storage devices. This parameter is required when
    -- storage type is HDD. Set this property to @READ@ to improve the
    -- performance for frequently accessed files by caching up to 20% of the
    -- total storage capacity of the file system.
    --
    -- This parameter is required when @StorageType@ is set to @HDD@.
    driveCacheType :: Prelude.Maybe DriveCacheType,
    -- | (Optional) Specifies the path in the Amazon S3 bucket where the root of
    -- your Amazon FSx file system is exported. The path must use the same
    -- Amazon S3 bucket as specified in ImportPath. You can provide an optional
    -- prefix to which new and changed data is to be exported from your Amazon
    -- FSx for Lustre file system. If an @ExportPath@ value is not provided,
    -- Amazon FSx sets a default export path,
    -- @s3:\/\/import-bucket\/FSxLustre[creation-timestamp]@. The timestamp is
    -- in UTC format, for example
    -- @s3:\/\/import-bucket\/FSxLustre20181105T222312Z@.
    --
    -- The Amazon S3 export bucket must be the same as the import bucket
    -- specified by @ImportPath@. If you specify only a bucket name, such as
    -- @s3:\/\/import-bucket@, you get a 1:1 mapping of file system objects to
    -- S3 bucket objects. This mapping means that the input data in S3 is
    -- overwritten on export. If you provide a custom prefix in the export
    -- path, such as @s3:\/\/import-bucket\/[custom-optional-prefix]@, Amazon
    -- FSx exports the contents of your file system to that export prefix in
    -- the Amazon S3 bucket.
    --
    -- This parameter is not supported for file systems with a data repository
    -- association.
    exportPath :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The path to the Amazon S3 bucket (including the optional
    -- prefix) that you\'re using as the data repository for your Amazon FSx
    -- for Lustre file system. The root of your FSx for Lustre file system will
    -- be mapped to the root of the Amazon S3 bucket you select. An example is
    -- @s3:\/\/import-bucket\/optional-prefix@. If you specify a prefix after
    -- the Amazon S3 bucket name, only object keys with that prefix are loaded
    -- into the file system.
    --
    -- This parameter is not supported for file systems with a data repository
    -- association.
    importPath :: Prelude.Maybe Prelude.Text,
    -- | (Optional) For files imported from a data repository, this value
    -- determines the stripe count and maximum amount of data per file (in MiB)
    -- stored on a single physical disk. The maximum number of disks that a
    -- single file can be striped across is limited by the total number of
    -- disks that make up the file system.
    --
    -- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
    -- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
    --
    -- This parameter is not supported for file systems with a data repository
    -- association.
    importedFileChunkSize :: Prelude.Maybe Prelude.Natural,
    -- | The Lustre logging configuration used when creating an Amazon FSx for
    -- Lustre file system. When logging is enabled, Lustre logs error and
    -- warning events for data repositories associated with your file system to
    -- Amazon CloudWatch Logs.
    logConfiguration :: Prelude.Maybe LustreLogCreateConfiguration,
    -- | Required with @PERSISTENT_1@ and @PERSISTENT_2@ deployment types,
    -- provisions the amount of read and write throughput for each 1 tebibyte
    -- (TiB) of file system storage capacity, in MB\/s\/TiB. File system
    -- throughput capacity is calculated by multiplying ﬁle system storage
    -- capacity (TiB) by the @PerUnitStorageThroughput@ (MB\/s\/TiB). For a
    -- 2.4-TiB ﬁle system, provisioning 50 MB\/s\/TiB of
    -- @PerUnitStorageThroughput@ yields 120 MB\/s of ﬁle system throughput.
    -- You pay for the amount of throughput that you provision.
    --
    -- Valid values:
    --
    -- -   For @PERSISTENT_1@ SSD storage: 50, 100, 200 MB\/s\/TiB.
    --
    -- -   For @PERSISTENT_1@ HDD storage: 12, 40 MB\/s\/TiB.
    --
    -- -   For @PERSISTENT_2@ SSD storage: 125, 250, 500, 1000 MB\/s\/TiB.
    perUnitStorageThroughput :: Prelude.Maybe Prelude.Natural,
    -- | The Lustre root squash configuration used when creating an Amazon FSx
    -- for Lustre file system. When enabled, root squash restricts root-level
    -- access from clients that try to access your file system as a root user.
    rootSquashConfiguration :: Prelude.Maybe LustreRootSquashConfiguration,
    -- | (Optional) The preferred start time to perform weekly maintenance,
    -- formatted d:HH:MM in the UTC time zone, where d is the weekday number,
    -- from 1 through 7, beginning with Monday and ending with Sunday.
    weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileSystemLustreConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoImportPolicy', 'createFileSystemLustreConfiguration_autoImportPolicy' - (Optional) When you create your file system, your existing S3 objects
-- appear as file and directory listings. Use this parameter to choose how
-- Amazon FSx keeps your file and directory listings up to date as you add
-- or modify objects in your linked S3 bucket. @AutoImportPolicy@ can have
-- the following values:
--
-- -   @NONE@ - (Default) AutoImport is off. Amazon FSx only updates file
--     and directory listings from the linked S3 bucket when the file
--     system is created. FSx does not update file and directory listings
--     for any new or changed objects after choosing this option.
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
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/older-deployment-types.html#legacy-auto-import-from-s3 Automatically import updates from your S3 bucket>.
--
-- This parameter is not supported for file systems with a data repository
-- association.
--
-- 'automaticBackupRetentionDays', 'createFileSystemLustreConfiguration_automaticBackupRetentionDays' - The number of days to retain automatic backups. Setting this property to
-- @0@ disables automatic backups. You can retain automatic backups for a
-- maximum of 90 days. The default is @0@.
--
-- 'copyTagsToBackups', 'createFileSystemLustreConfiguration_copyTagsToBackups' - (Optional) Not available for use with file systems that are linked to a
-- data repository. A boolean flag indicating whether tags for the file
-- system should be copied to backups. The default value is false. If
-- @CopyTagsToBackups@ is set to true, all file system tags are copied to
-- all automatic and user-initiated backups when the user doesn\'t specify
-- any backup-specific tags. If @CopyTagsToBackups@ is set to true and you
-- specify one or more backup tags, only the specified tags are copied to
-- backups. If you specify one or more tags when creating a user-initiated
-- backup, no tags are copied from the file system, regardless of this
-- value.
--
-- (Default = @false@)
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/using-backups-fsx.html Working with backups>
-- in the /Amazon FSx for Lustre User Guide/.
--
-- 'dailyAutomaticBackupStartTime', 'createFileSystemLustreConfiguration_dailyAutomaticBackupStartTime' - Undocumented member.
--
-- 'dataCompressionType', 'createFileSystemLustreConfiguration_dataCompressionType' - Sets the data compression configuration for the file system.
-- @DataCompressionType@ can have the following values:
--
-- -   @NONE@ - (Default) Data compression is turned off when the file
--     system is created.
--
-- -   @LZ4@ - Data compression is turned on with the LZ4 algorithm.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/data-compression.html Lustre data compression>
-- in the /Amazon FSx for Lustre User Guide/.
--
-- 'deploymentType', 'createFileSystemLustreConfiguration_deploymentType' - (Optional) Choose @SCRATCH_1@ and @SCRATCH_2@ deployment types when you
-- need temporary storage and shorter-term processing of data. The
-- @SCRATCH_2@ deployment type provides in-transit encryption of data and
-- higher burst throughput capacity than @SCRATCH_1@.
--
-- Choose @PERSISTENT_1@ for longer-term storage and for throughput-focused
-- workloads that aren’t latency-sensitive. @PERSISTENT_1@ supports
-- encryption of data in transit, and is available in all Amazon Web
-- Services Regions in which FSx for Lustre is available.
--
-- Choose @PERSISTENT_2@ for longer-term storage and for latency-sensitive
-- workloads that require the highest levels of IOPS\/throughput.
-- @PERSISTENT_2@ supports SSD storage, and offers higher
-- @PerUnitStorageThroughput@ (up to 1000 MB\/s\/TiB). @PERSISTENT_2@ is
-- available in a limited number of Amazon Web Services Regions. For more
-- information, and an up-to-date list of Amazon Web Services Regions in
-- which @PERSISTENT_2@ is available, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/using-fsx-lustre.html#lustre-deployment-types File system deployment options for FSx for Lustre>
-- in the /Amazon FSx for Lustre User Guide/.
--
-- If you choose @PERSISTENT_2@, and you set @FileSystemTypeVersion@ to
-- @2.10@, the @CreateFileSystem@ operation fails.
--
-- Encryption of data in transit is automatically turned on when you access
-- @SCRATCH_2@, @PERSISTENT_1@ and @PERSISTENT_2@ file systems from Amazon
-- EC2 instances that support automatic encryption in the Amazon Web
-- Services Regions where they are available. For more information about
-- encryption in transit for FSx for Lustre file systems, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/encryption-in-transit-fsxl.html Encrypting data in transit>
-- in the /Amazon FSx for Lustre User Guide/.
--
-- (Default = @SCRATCH_1@)
--
-- 'driveCacheType', 'createFileSystemLustreConfiguration_driveCacheType' - The type of drive cache used by @PERSISTENT_1@ file systems that are
-- provisioned with HDD storage devices. This parameter is required when
-- storage type is HDD. Set this property to @READ@ to improve the
-- performance for frequently accessed files by caching up to 20% of the
-- total storage capacity of the file system.
--
-- This parameter is required when @StorageType@ is set to @HDD@.
--
-- 'exportPath', 'createFileSystemLustreConfiguration_exportPath' - (Optional) Specifies the path in the Amazon S3 bucket where the root of
-- your Amazon FSx file system is exported. The path must use the same
-- Amazon S3 bucket as specified in ImportPath. You can provide an optional
-- prefix to which new and changed data is to be exported from your Amazon
-- FSx for Lustre file system. If an @ExportPath@ value is not provided,
-- Amazon FSx sets a default export path,
-- @s3:\/\/import-bucket\/FSxLustre[creation-timestamp]@. The timestamp is
-- in UTC format, for example
-- @s3:\/\/import-bucket\/FSxLustre20181105T222312Z@.
--
-- The Amazon S3 export bucket must be the same as the import bucket
-- specified by @ImportPath@. If you specify only a bucket name, such as
-- @s3:\/\/import-bucket@, you get a 1:1 mapping of file system objects to
-- S3 bucket objects. This mapping means that the input data in S3 is
-- overwritten on export. If you provide a custom prefix in the export
-- path, such as @s3:\/\/import-bucket\/[custom-optional-prefix]@, Amazon
-- FSx exports the contents of your file system to that export prefix in
-- the Amazon S3 bucket.
--
-- This parameter is not supported for file systems with a data repository
-- association.
--
-- 'importPath', 'createFileSystemLustreConfiguration_importPath' - (Optional) The path to the Amazon S3 bucket (including the optional
-- prefix) that you\'re using as the data repository for your Amazon FSx
-- for Lustre file system. The root of your FSx for Lustre file system will
-- be mapped to the root of the Amazon S3 bucket you select. An example is
-- @s3:\/\/import-bucket\/optional-prefix@. If you specify a prefix after
-- the Amazon S3 bucket name, only object keys with that prefix are loaded
-- into the file system.
--
-- This parameter is not supported for file systems with a data repository
-- association.
--
-- 'importedFileChunkSize', 'createFileSystemLustreConfiguration_importedFileChunkSize' - (Optional) For files imported from a data repository, this value
-- determines the stripe count and maximum amount of data per file (in MiB)
-- stored on a single physical disk. The maximum number of disks that a
-- single file can be striped across is limited by the total number of
-- disks that make up the file system.
--
-- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
-- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
--
-- This parameter is not supported for file systems with a data repository
-- association.
--
-- 'logConfiguration', 'createFileSystemLustreConfiguration_logConfiguration' - The Lustre logging configuration used when creating an Amazon FSx for
-- Lustre file system. When logging is enabled, Lustre logs error and
-- warning events for data repositories associated with your file system to
-- Amazon CloudWatch Logs.
--
-- 'perUnitStorageThroughput', 'createFileSystemLustreConfiguration_perUnitStorageThroughput' - Required with @PERSISTENT_1@ and @PERSISTENT_2@ deployment types,
-- provisions the amount of read and write throughput for each 1 tebibyte
-- (TiB) of file system storage capacity, in MB\/s\/TiB. File system
-- throughput capacity is calculated by multiplying ﬁle system storage
-- capacity (TiB) by the @PerUnitStorageThroughput@ (MB\/s\/TiB). For a
-- 2.4-TiB ﬁle system, provisioning 50 MB\/s\/TiB of
-- @PerUnitStorageThroughput@ yields 120 MB\/s of ﬁle system throughput.
-- You pay for the amount of throughput that you provision.
--
-- Valid values:
--
-- -   For @PERSISTENT_1@ SSD storage: 50, 100, 200 MB\/s\/TiB.
--
-- -   For @PERSISTENT_1@ HDD storage: 12, 40 MB\/s\/TiB.
--
-- -   For @PERSISTENT_2@ SSD storage: 125, 250, 500, 1000 MB\/s\/TiB.
--
-- 'rootSquashConfiguration', 'createFileSystemLustreConfiguration_rootSquashConfiguration' - The Lustre root squash configuration used when creating an Amazon FSx
-- for Lustre file system. When enabled, root squash restricts root-level
-- access from clients that try to access your file system as a root user.
--
-- 'weeklyMaintenanceStartTime', 'createFileSystemLustreConfiguration_weeklyMaintenanceStartTime' - (Optional) The preferred start time to perform weekly maintenance,
-- formatted d:HH:MM in the UTC time zone, where d is the weekday number,
-- from 1 through 7, beginning with Monday and ending with Sunday.
newCreateFileSystemLustreConfiguration ::
  CreateFileSystemLustreConfiguration
newCreateFileSystemLustreConfiguration =
  CreateFileSystemLustreConfiguration'
    { autoImportPolicy =
        Prelude.Nothing,
      automaticBackupRetentionDays =
        Prelude.Nothing,
      copyTagsToBackups = Prelude.Nothing,
      dailyAutomaticBackupStartTime =
        Prelude.Nothing,
      dataCompressionType = Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      driveCacheType = Prelude.Nothing,
      exportPath = Prelude.Nothing,
      importPath = Prelude.Nothing,
      importedFileChunkSize =
        Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      perUnitStorageThroughput =
        Prelude.Nothing,
      rootSquashConfiguration =
        Prelude.Nothing,
      weeklyMaintenanceStartTime =
        Prelude.Nothing
    }

-- | (Optional) When you create your file system, your existing S3 objects
-- appear as file and directory listings. Use this parameter to choose how
-- Amazon FSx keeps your file and directory listings up to date as you add
-- or modify objects in your linked S3 bucket. @AutoImportPolicy@ can have
-- the following values:
--
-- -   @NONE@ - (Default) AutoImport is off. Amazon FSx only updates file
--     and directory listings from the linked S3 bucket when the file
--     system is created. FSx does not update file and directory listings
--     for any new or changed objects after choosing this option.
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
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/older-deployment-types.html#legacy-auto-import-from-s3 Automatically import updates from your S3 bucket>.
--
-- This parameter is not supported for file systems with a data repository
-- association.
createFileSystemLustreConfiguration_autoImportPolicy :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe AutoImportPolicyType)
createFileSystemLustreConfiguration_autoImportPolicy = Lens.lens (\CreateFileSystemLustreConfiguration' {autoImportPolicy} -> autoImportPolicy) (\s@CreateFileSystemLustreConfiguration' {} a -> s {autoImportPolicy = a} :: CreateFileSystemLustreConfiguration)

-- | The number of days to retain automatic backups. Setting this property to
-- @0@ disables automatic backups. You can retain automatic backups for a
-- maximum of 90 days. The default is @0@.
createFileSystemLustreConfiguration_automaticBackupRetentionDays :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe Prelude.Natural)
createFileSystemLustreConfiguration_automaticBackupRetentionDays = Lens.lens (\CreateFileSystemLustreConfiguration' {automaticBackupRetentionDays} -> automaticBackupRetentionDays) (\s@CreateFileSystemLustreConfiguration' {} a -> s {automaticBackupRetentionDays = a} :: CreateFileSystemLustreConfiguration)

-- | (Optional) Not available for use with file systems that are linked to a
-- data repository. A boolean flag indicating whether tags for the file
-- system should be copied to backups. The default value is false. If
-- @CopyTagsToBackups@ is set to true, all file system tags are copied to
-- all automatic and user-initiated backups when the user doesn\'t specify
-- any backup-specific tags. If @CopyTagsToBackups@ is set to true and you
-- specify one or more backup tags, only the specified tags are copied to
-- backups. If you specify one or more tags when creating a user-initiated
-- backup, no tags are copied from the file system, regardless of this
-- value.
--
-- (Default = @false@)
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/using-backups-fsx.html Working with backups>
-- in the /Amazon FSx for Lustre User Guide/.
createFileSystemLustreConfiguration_copyTagsToBackups :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe Prelude.Bool)
createFileSystemLustreConfiguration_copyTagsToBackups = Lens.lens (\CreateFileSystemLustreConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@CreateFileSystemLustreConfiguration' {} a -> s {copyTagsToBackups = a} :: CreateFileSystemLustreConfiguration)

-- | Undocumented member.
createFileSystemLustreConfiguration_dailyAutomaticBackupStartTime :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemLustreConfiguration_dailyAutomaticBackupStartTime = Lens.lens (\CreateFileSystemLustreConfiguration' {dailyAutomaticBackupStartTime} -> dailyAutomaticBackupStartTime) (\s@CreateFileSystemLustreConfiguration' {} a -> s {dailyAutomaticBackupStartTime = a} :: CreateFileSystemLustreConfiguration)

-- | Sets the data compression configuration for the file system.
-- @DataCompressionType@ can have the following values:
--
-- -   @NONE@ - (Default) Data compression is turned off when the file
--     system is created.
--
-- -   @LZ4@ - Data compression is turned on with the LZ4 algorithm.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/data-compression.html Lustre data compression>
-- in the /Amazon FSx for Lustre User Guide/.
createFileSystemLustreConfiguration_dataCompressionType :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe DataCompressionType)
createFileSystemLustreConfiguration_dataCompressionType = Lens.lens (\CreateFileSystemLustreConfiguration' {dataCompressionType} -> dataCompressionType) (\s@CreateFileSystemLustreConfiguration' {} a -> s {dataCompressionType = a} :: CreateFileSystemLustreConfiguration)

-- | (Optional) Choose @SCRATCH_1@ and @SCRATCH_2@ deployment types when you
-- need temporary storage and shorter-term processing of data. The
-- @SCRATCH_2@ deployment type provides in-transit encryption of data and
-- higher burst throughput capacity than @SCRATCH_1@.
--
-- Choose @PERSISTENT_1@ for longer-term storage and for throughput-focused
-- workloads that aren’t latency-sensitive. @PERSISTENT_1@ supports
-- encryption of data in transit, and is available in all Amazon Web
-- Services Regions in which FSx for Lustre is available.
--
-- Choose @PERSISTENT_2@ for longer-term storage and for latency-sensitive
-- workloads that require the highest levels of IOPS\/throughput.
-- @PERSISTENT_2@ supports SSD storage, and offers higher
-- @PerUnitStorageThroughput@ (up to 1000 MB\/s\/TiB). @PERSISTENT_2@ is
-- available in a limited number of Amazon Web Services Regions. For more
-- information, and an up-to-date list of Amazon Web Services Regions in
-- which @PERSISTENT_2@ is available, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/using-fsx-lustre.html#lustre-deployment-types File system deployment options for FSx for Lustre>
-- in the /Amazon FSx for Lustre User Guide/.
--
-- If you choose @PERSISTENT_2@, and you set @FileSystemTypeVersion@ to
-- @2.10@, the @CreateFileSystem@ operation fails.
--
-- Encryption of data in transit is automatically turned on when you access
-- @SCRATCH_2@, @PERSISTENT_1@ and @PERSISTENT_2@ file systems from Amazon
-- EC2 instances that support automatic encryption in the Amazon Web
-- Services Regions where they are available. For more information about
-- encryption in transit for FSx for Lustre file systems, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/encryption-in-transit-fsxl.html Encrypting data in transit>
-- in the /Amazon FSx for Lustre User Guide/.
--
-- (Default = @SCRATCH_1@)
createFileSystemLustreConfiguration_deploymentType :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe LustreDeploymentType)
createFileSystemLustreConfiguration_deploymentType = Lens.lens (\CreateFileSystemLustreConfiguration' {deploymentType} -> deploymentType) (\s@CreateFileSystemLustreConfiguration' {} a -> s {deploymentType = a} :: CreateFileSystemLustreConfiguration)

-- | The type of drive cache used by @PERSISTENT_1@ file systems that are
-- provisioned with HDD storage devices. This parameter is required when
-- storage type is HDD. Set this property to @READ@ to improve the
-- performance for frequently accessed files by caching up to 20% of the
-- total storage capacity of the file system.
--
-- This parameter is required when @StorageType@ is set to @HDD@.
createFileSystemLustreConfiguration_driveCacheType :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe DriveCacheType)
createFileSystemLustreConfiguration_driveCacheType = Lens.lens (\CreateFileSystemLustreConfiguration' {driveCacheType} -> driveCacheType) (\s@CreateFileSystemLustreConfiguration' {} a -> s {driveCacheType = a} :: CreateFileSystemLustreConfiguration)

-- | (Optional) Specifies the path in the Amazon S3 bucket where the root of
-- your Amazon FSx file system is exported. The path must use the same
-- Amazon S3 bucket as specified in ImportPath. You can provide an optional
-- prefix to which new and changed data is to be exported from your Amazon
-- FSx for Lustre file system. If an @ExportPath@ value is not provided,
-- Amazon FSx sets a default export path,
-- @s3:\/\/import-bucket\/FSxLustre[creation-timestamp]@. The timestamp is
-- in UTC format, for example
-- @s3:\/\/import-bucket\/FSxLustre20181105T222312Z@.
--
-- The Amazon S3 export bucket must be the same as the import bucket
-- specified by @ImportPath@. If you specify only a bucket name, such as
-- @s3:\/\/import-bucket@, you get a 1:1 mapping of file system objects to
-- S3 bucket objects. This mapping means that the input data in S3 is
-- overwritten on export. If you provide a custom prefix in the export
-- path, such as @s3:\/\/import-bucket\/[custom-optional-prefix]@, Amazon
-- FSx exports the contents of your file system to that export prefix in
-- the Amazon S3 bucket.
--
-- This parameter is not supported for file systems with a data repository
-- association.
createFileSystemLustreConfiguration_exportPath :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemLustreConfiguration_exportPath = Lens.lens (\CreateFileSystemLustreConfiguration' {exportPath} -> exportPath) (\s@CreateFileSystemLustreConfiguration' {} a -> s {exportPath = a} :: CreateFileSystemLustreConfiguration)

-- | (Optional) The path to the Amazon S3 bucket (including the optional
-- prefix) that you\'re using as the data repository for your Amazon FSx
-- for Lustre file system. The root of your FSx for Lustre file system will
-- be mapped to the root of the Amazon S3 bucket you select. An example is
-- @s3:\/\/import-bucket\/optional-prefix@. If you specify a prefix after
-- the Amazon S3 bucket name, only object keys with that prefix are loaded
-- into the file system.
--
-- This parameter is not supported for file systems with a data repository
-- association.
createFileSystemLustreConfiguration_importPath :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemLustreConfiguration_importPath = Lens.lens (\CreateFileSystemLustreConfiguration' {importPath} -> importPath) (\s@CreateFileSystemLustreConfiguration' {} a -> s {importPath = a} :: CreateFileSystemLustreConfiguration)

-- | (Optional) For files imported from a data repository, this value
-- determines the stripe count and maximum amount of data per file (in MiB)
-- stored on a single physical disk. The maximum number of disks that a
-- single file can be striped across is limited by the total number of
-- disks that make up the file system.
--
-- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
-- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
--
-- This parameter is not supported for file systems with a data repository
-- association.
createFileSystemLustreConfiguration_importedFileChunkSize :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe Prelude.Natural)
createFileSystemLustreConfiguration_importedFileChunkSize = Lens.lens (\CreateFileSystemLustreConfiguration' {importedFileChunkSize} -> importedFileChunkSize) (\s@CreateFileSystemLustreConfiguration' {} a -> s {importedFileChunkSize = a} :: CreateFileSystemLustreConfiguration)

-- | The Lustre logging configuration used when creating an Amazon FSx for
-- Lustre file system. When logging is enabled, Lustre logs error and
-- warning events for data repositories associated with your file system to
-- Amazon CloudWatch Logs.
createFileSystemLustreConfiguration_logConfiguration :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe LustreLogCreateConfiguration)
createFileSystemLustreConfiguration_logConfiguration = Lens.lens (\CreateFileSystemLustreConfiguration' {logConfiguration} -> logConfiguration) (\s@CreateFileSystemLustreConfiguration' {} a -> s {logConfiguration = a} :: CreateFileSystemLustreConfiguration)

-- | Required with @PERSISTENT_1@ and @PERSISTENT_2@ deployment types,
-- provisions the amount of read and write throughput for each 1 tebibyte
-- (TiB) of file system storage capacity, in MB\/s\/TiB. File system
-- throughput capacity is calculated by multiplying ﬁle system storage
-- capacity (TiB) by the @PerUnitStorageThroughput@ (MB\/s\/TiB). For a
-- 2.4-TiB ﬁle system, provisioning 50 MB\/s\/TiB of
-- @PerUnitStorageThroughput@ yields 120 MB\/s of ﬁle system throughput.
-- You pay for the amount of throughput that you provision.
--
-- Valid values:
--
-- -   For @PERSISTENT_1@ SSD storage: 50, 100, 200 MB\/s\/TiB.
--
-- -   For @PERSISTENT_1@ HDD storage: 12, 40 MB\/s\/TiB.
--
-- -   For @PERSISTENT_2@ SSD storage: 125, 250, 500, 1000 MB\/s\/TiB.
createFileSystemLustreConfiguration_perUnitStorageThroughput :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe Prelude.Natural)
createFileSystemLustreConfiguration_perUnitStorageThroughput = Lens.lens (\CreateFileSystemLustreConfiguration' {perUnitStorageThroughput} -> perUnitStorageThroughput) (\s@CreateFileSystemLustreConfiguration' {} a -> s {perUnitStorageThroughput = a} :: CreateFileSystemLustreConfiguration)

-- | The Lustre root squash configuration used when creating an Amazon FSx
-- for Lustre file system. When enabled, root squash restricts root-level
-- access from clients that try to access your file system as a root user.
createFileSystemLustreConfiguration_rootSquashConfiguration :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe LustreRootSquashConfiguration)
createFileSystemLustreConfiguration_rootSquashConfiguration = Lens.lens (\CreateFileSystemLustreConfiguration' {rootSquashConfiguration} -> rootSquashConfiguration) (\s@CreateFileSystemLustreConfiguration' {} a -> s {rootSquashConfiguration = a} :: CreateFileSystemLustreConfiguration)

-- | (Optional) The preferred start time to perform weekly maintenance,
-- formatted d:HH:MM in the UTC time zone, where d is the weekday number,
-- from 1 through 7, beginning with Monday and ending with Sunday.
createFileSystemLustreConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' CreateFileSystemLustreConfiguration (Prelude.Maybe Prelude.Text)
createFileSystemLustreConfiguration_weeklyMaintenanceStartTime = Lens.lens (\CreateFileSystemLustreConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@CreateFileSystemLustreConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: CreateFileSystemLustreConfiguration)

instance
  Prelude.Hashable
    CreateFileSystemLustreConfiguration
  where
  hashWithSalt
    _salt
    CreateFileSystemLustreConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` autoImportPolicy
        `Prelude.hashWithSalt` automaticBackupRetentionDays
        `Prelude.hashWithSalt` copyTagsToBackups
        `Prelude.hashWithSalt` dailyAutomaticBackupStartTime
        `Prelude.hashWithSalt` dataCompressionType
        `Prelude.hashWithSalt` deploymentType
        `Prelude.hashWithSalt` driveCacheType
        `Prelude.hashWithSalt` exportPath
        `Prelude.hashWithSalt` importPath
        `Prelude.hashWithSalt` importedFileChunkSize
        `Prelude.hashWithSalt` logConfiguration
        `Prelude.hashWithSalt` perUnitStorageThroughput
        `Prelude.hashWithSalt` rootSquashConfiguration
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime

instance
  Prelude.NFData
    CreateFileSystemLustreConfiguration
  where
  rnf CreateFileSystemLustreConfiguration' {..} =
    Prelude.rnf autoImportPolicy
      `Prelude.seq` Prelude.rnf automaticBackupRetentionDays
      `Prelude.seq` Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf dailyAutomaticBackupStartTime
      `Prelude.seq` Prelude.rnf dataCompressionType
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf driveCacheType
      `Prelude.seq` Prelude.rnf exportPath
      `Prelude.seq` Prelude.rnf importPath
      `Prelude.seq` Prelude.rnf importedFileChunkSize
      `Prelude.seq` Prelude.rnf logConfiguration
      `Prelude.seq` Prelude.rnf perUnitStorageThroughput
      `Prelude.seq` Prelude.rnf rootSquashConfiguration
      `Prelude.seq` Prelude.rnf weeklyMaintenanceStartTime

instance
  Data.ToJSON
    CreateFileSystemLustreConfiguration
  where
  toJSON CreateFileSystemLustreConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoImportPolicy" Data..=)
              Prelude.<$> autoImportPolicy,
            ("AutomaticBackupRetentionDays" Data..=)
              Prelude.<$> automaticBackupRetentionDays,
            ("CopyTagsToBackups" Data..=)
              Prelude.<$> copyTagsToBackups,
            ("DailyAutomaticBackupStartTime" Data..=)
              Prelude.<$> dailyAutomaticBackupStartTime,
            ("DataCompressionType" Data..=)
              Prelude.<$> dataCompressionType,
            ("DeploymentType" Data..=)
              Prelude.<$> deploymentType,
            ("DriveCacheType" Data..=)
              Prelude.<$> driveCacheType,
            ("ExportPath" Data..=) Prelude.<$> exportPath,
            ("ImportPath" Data..=) Prelude.<$> importPath,
            ("ImportedFileChunkSize" Data..=)
              Prelude.<$> importedFileChunkSize,
            ("LogConfiguration" Data..=)
              Prelude.<$> logConfiguration,
            ("PerUnitStorageThroughput" Data..=)
              Prelude.<$> perUnitStorageThroughput,
            ("RootSquashConfiguration" Data..=)
              Prelude.<$> rootSquashConfiguration,
            ("WeeklyMaintenanceStartTime" Data..=)
              Prelude.<$> weeklyMaintenanceStartTime
          ]
      )
