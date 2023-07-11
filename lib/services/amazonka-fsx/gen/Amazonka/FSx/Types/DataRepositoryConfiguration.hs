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
-- Module      : Amazonka.FSx.Types.DataRepositoryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DataRepositoryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.AutoImportPolicyType
import Amazonka.FSx.Types.DataRepositoryFailureDetails
import Amazonka.FSx.Types.DataRepositoryLifecycle
import qualified Amazonka.Prelude as Prelude

-- | The data repository configuration object for Lustre file systems
-- returned in the response of the @CreateFileSystem@ operation.
--
-- This data type is not supported for file systems with the @Persistent_2@
-- deployment type. Instead, use .
--
-- /See:/ 'newDataRepositoryConfiguration' smart constructor.
data DataRepositoryConfiguration = DataRepositoryConfiguration'
  { -- | Describes the file system\'s linked S3 data repository\'s
    -- @AutoImportPolicy@. The AutoImportPolicy configures how Amazon FSx keeps
    -- your file and directory listings up to date as you add or modify objects
    -- in your linked S3 bucket. @AutoImportPolicy@ can have the following
    -- values:
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
    autoImportPolicy :: Prelude.Maybe AutoImportPolicyType,
    -- | The export path to the Amazon S3 bucket (and prefix) that you are using
    -- to store new and changed Lustre file system files in S3.
    exportPath :: Prelude.Maybe Prelude.Text,
    failureDetails :: Prelude.Maybe DataRepositoryFailureDetails,
    -- | The import path to the Amazon S3 bucket (and optional prefix) that
    -- you\'re using as the data repository for your FSx for Lustre file
    -- system, for example @s3:\/\/import-bucket\/optional-prefix@. If a prefix
    -- is specified after the Amazon S3 bucket name, only object keys with that
    -- prefix are loaded into the file system.
    importPath :: Prelude.Maybe Prelude.Text,
    -- | For files imported from a data repository, this value determines the
    -- stripe count and maximum amount of data per file (in MiB) stored on a
    -- single physical disk. The maximum number of disks that a single file can
    -- be striped across is limited by the total number of disks that make up
    -- the file system.
    --
    -- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
    -- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
    importedFileChunkSize :: Prelude.Maybe Prelude.Natural,
    -- | Describes the state of the file system\'s S3 durable data repository, if
    -- it is configured with an S3 repository. The lifecycle can have the
    -- following values:
    --
    -- -   @CREATING@ - The data repository configuration between the FSx file
    --     system and the linked S3 data repository is being created. The data
    --     repository is unavailable.
    --
    -- -   @AVAILABLE@ - The data repository is available for use.
    --
    -- -   @MISCONFIGURED@ - Amazon FSx cannot automatically import updates
    --     from the S3 bucket until the data repository configuration is
    --     corrected. For more information, see
    --     <https://docs.aws.amazon.com/fsx/latest/LustreGuide/troubleshooting.html#troubleshooting-misconfigured-data-repository Troubleshooting a Misconfigured linked S3 bucket>.
    --
    -- -   @UPDATING@ - The data repository is undergoing a customer initiated
    --     update and availability may be impacted.
    --
    -- -   @FAILED@ - The data repository is in a terminal state that cannot be
    --     recovered.
    lifecycle :: Prelude.Maybe DataRepositoryLifecycle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataRepositoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoImportPolicy', 'dataRepositoryConfiguration_autoImportPolicy' - Describes the file system\'s linked S3 data repository\'s
-- @AutoImportPolicy@. The AutoImportPolicy configures how Amazon FSx keeps
-- your file and directory listings up to date as you add or modify objects
-- in your linked S3 bucket. @AutoImportPolicy@ can have the following
-- values:
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
-- 'exportPath', 'dataRepositoryConfiguration_exportPath' - The export path to the Amazon S3 bucket (and prefix) that you are using
-- to store new and changed Lustre file system files in S3.
--
-- 'failureDetails', 'dataRepositoryConfiguration_failureDetails' - Undocumented member.
--
-- 'importPath', 'dataRepositoryConfiguration_importPath' - The import path to the Amazon S3 bucket (and optional prefix) that
-- you\'re using as the data repository for your FSx for Lustre file
-- system, for example @s3:\/\/import-bucket\/optional-prefix@. If a prefix
-- is specified after the Amazon S3 bucket name, only object keys with that
-- prefix are loaded into the file system.
--
-- 'importedFileChunkSize', 'dataRepositoryConfiguration_importedFileChunkSize' - For files imported from a data repository, this value determines the
-- stripe count and maximum amount of data per file (in MiB) stored on a
-- single physical disk. The maximum number of disks that a single file can
-- be striped across is limited by the total number of disks that make up
-- the file system.
--
-- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
-- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
--
-- 'lifecycle', 'dataRepositoryConfiguration_lifecycle' - Describes the state of the file system\'s S3 durable data repository, if
-- it is configured with an S3 repository. The lifecycle can have the
-- following values:
--
-- -   @CREATING@ - The data repository configuration between the FSx file
--     system and the linked S3 data repository is being created. The data
--     repository is unavailable.
--
-- -   @AVAILABLE@ - The data repository is available for use.
--
-- -   @MISCONFIGURED@ - Amazon FSx cannot automatically import updates
--     from the S3 bucket until the data repository configuration is
--     corrected. For more information, see
--     <https://docs.aws.amazon.com/fsx/latest/LustreGuide/troubleshooting.html#troubleshooting-misconfigured-data-repository Troubleshooting a Misconfigured linked S3 bucket>.
--
-- -   @UPDATING@ - The data repository is undergoing a customer initiated
--     update and availability may be impacted.
--
-- -   @FAILED@ - The data repository is in a terminal state that cannot be
--     recovered.
newDataRepositoryConfiguration ::
  DataRepositoryConfiguration
newDataRepositoryConfiguration =
  DataRepositoryConfiguration'
    { autoImportPolicy =
        Prelude.Nothing,
      exportPath = Prelude.Nothing,
      failureDetails = Prelude.Nothing,
      importPath = Prelude.Nothing,
      importedFileChunkSize = Prelude.Nothing,
      lifecycle = Prelude.Nothing
    }

-- | Describes the file system\'s linked S3 data repository\'s
-- @AutoImportPolicy@. The AutoImportPolicy configures how Amazon FSx keeps
-- your file and directory listings up to date as you add or modify objects
-- in your linked S3 bucket. @AutoImportPolicy@ can have the following
-- values:
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
dataRepositoryConfiguration_autoImportPolicy :: Lens.Lens' DataRepositoryConfiguration (Prelude.Maybe AutoImportPolicyType)
dataRepositoryConfiguration_autoImportPolicy = Lens.lens (\DataRepositoryConfiguration' {autoImportPolicy} -> autoImportPolicy) (\s@DataRepositoryConfiguration' {} a -> s {autoImportPolicy = a} :: DataRepositoryConfiguration)

-- | The export path to the Amazon S3 bucket (and prefix) that you are using
-- to store new and changed Lustre file system files in S3.
dataRepositoryConfiguration_exportPath :: Lens.Lens' DataRepositoryConfiguration (Prelude.Maybe Prelude.Text)
dataRepositoryConfiguration_exportPath = Lens.lens (\DataRepositoryConfiguration' {exportPath} -> exportPath) (\s@DataRepositoryConfiguration' {} a -> s {exportPath = a} :: DataRepositoryConfiguration)

-- | Undocumented member.
dataRepositoryConfiguration_failureDetails :: Lens.Lens' DataRepositoryConfiguration (Prelude.Maybe DataRepositoryFailureDetails)
dataRepositoryConfiguration_failureDetails = Lens.lens (\DataRepositoryConfiguration' {failureDetails} -> failureDetails) (\s@DataRepositoryConfiguration' {} a -> s {failureDetails = a} :: DataRepositoryConfiguration)

-- | The import path to the Amazon S3 bucket (and optional prefix) that
-- you\'re using as the data repository for your FSx for Lustre file
-- system, for example @s3:\/\/import-bucket\/optional-prefix@. If a prefix
-- is specified after the Amazon S3 bucket name, only object keys with that
-- prefix are loaded into the file system.
dataRepositoryConfiguration_importPath :: Lens.Lens' DataRepositoryConfiguration (Prelude.Maybe Prelude.Text)
dataRepositoryConfiguration_importPath = Lens.lens (\DataRepositoryConfiguration' {importPath} -> importPath) (\s@DataRepositoryConfiguration' {} a -> s {importPath = a} :: DataRepositoryConfiguration)

-- | For files imported from a data repository, this value determines the
-- stripe count and maximum amount of data per file (in MiB) stored on a
-- single physical disk. The maximum number of disks that a single file can
-- be striped across is limited by the total number of disks that make up
-- the file system.
--
-- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
-- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
dataRepositoryConfiguration_importedFileChunkSize :: Lens.Lens' DataRepositoryConfiguration (Prelude.Maybe Prelude.Natural)
dataRepositoryConfiguration_importedFileChunkSize = Lens.lens (\DataRepositoryConfiguration' {importedFileChunkSize} -> importedFileChunkSize) (\s@DataRepositoryConfiguration' {} a -> s {importedFileChunkSize = a} :: DataRepositoryConfiguration)

-- | Describes the state of the file system\'s S3 durable data repository, if
-- it is configured with an S3 repository. The lifecycle can have the
-- following values:
--
-- -   @CREATING@ - The data repository configuration between the FSx file
--     system and the linked S3 data repository is being created. The data
--     repository is unavailable.
--
-- -   @AVAILABLE@ - The data repository is available for use.
--
-- -   @MISCONFIGURED@ - Amazon FSx cannot automatically import updates
--     from the S3 bucket until the data repository configuration is
--     corrected. For more information, see
--     <https://docs.aws.amazon.com/fsx/latest/LustreGuide/troubleshooting.html#troubleshooting-misconfigured-data-repository Troubleshooting a Misconfigured linked S3 bucket>.
--
-- -   @UPDATING@ - The data repository is undergoing a customer initiated
--     update and availability may be impacted.
--
-- -   @FAILED@ - The data repository is in a terminal state that cannot be
--     recovered.
dataRepositoryConfiguration_lifecycle :: Lens.Lens' DataRepositoryConfiguration (Prelude.Maybe DataRepositoryLifecycle)
dataRepositoryConfiguration_lifecycle = Lens.lens (\DataRepositoryConfiguration' {lifecycle} -> lifecycle) (\s@DataRepositoryConfiguration' {} a -> s {lifecycle = a} :: DataRepositoryConfiguration)

instance Data.FromJSON DataRepositoryConfiguration where
  parseJSON =
    Data.withObject
      "DataRepositoryConfiguration"
      ( \x ->
          DataRepositoryConfiguration'
            Prelude.<$> (x Data..:? "AutoImportPolicy")
            Prelude.<*> (x Data..:? "ExportPath")
            Prelude.<*> (x Data..:? "FailureDetails")
            Prelude.<*> (x Data..:? "ImportPath")
            Prelude.<*> (x Data..:? "ImportedFileChunkSize")
            Prelude.<*> (x Data..:? "Lifecycle")
      )

instance Prelude.Hashable DataRepositoryConfiguration where
  hashWithSalt _salt DataRepositoryConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` autoImportPolicy
      `Prelude.hashWithSalt` exportPath
      `Prelude.hashWithSalt` failureDetails
      `Prelude.hashWithSalt` importPath
      `Prelude.hashWithSalt` importedFileChunkSize
      `Prelude.hashWithSalt` lifecycle

instance Prelude.NFData DataRepositoryConfiguration where
  rnf DataRepositoryConfiguration' {..} =
    Prelude.rnf autoImportPolicy
      `Prelude.seq` Prelude.rnf exportPath
      `Prelude.seq` Prelude.rnf failureDetails
      `Prelude.seq` Prelude.rnf importPath
      `Prelude.seq` Prelude.rnf importedFileChunkSize
      `Prelude.seq` Prelude.rnf lifecycle
