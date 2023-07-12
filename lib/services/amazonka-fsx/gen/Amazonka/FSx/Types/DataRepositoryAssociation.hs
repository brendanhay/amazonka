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
-- Module      : Amazonka.FSx.Types.DataRepositoryAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DataRepositoryAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.DataRepositoryFailureDetails
import Amazonka.FSx.Types.DataRepositoryLifecycle
import Amazonka.FSx.Types.NFSDataRepositoryConfiguration
import Amazonka.FSx.Types.S3DataRepositoryConfiguration
import Amazonka.FSx.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The configuration of a data repository association that links an Amazon
-- FSx for Lustre file system to an Amazon S3 bucket or an Amazon File
-- Cache resource to an Amazon S3 bucket or an NFS file system. The data
-- repository association configuration object is returned in the response
-- of the following operations:
--
-- -   @CreateDataRepositoryAssociation@
--
-- -   @UpdateDataRepositoryAssociation@
--
-- -   @DescribeDataRepositoryAssociations@
--
-- Data repository associations are supported only for an Amazon FSx for
-- Lustre file system with the @Persistent_2@ deployment type and for an
-- Amazon File Cache resource.
--
-- /See:/ 'newDataRepositoryAssociation' smart constructor.
data DataRepositoryAssociation = DataRepositoryAssociation'
  { -- | The system-generated, unique ID of the data repository association.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | A boolean flag indicating whether an import data repository task to
    -- import metadata should run after the data repository association is
    -- created. The task runs if this flag is set to @true@.
    --
    -- @BatchImportMetaDataOnCreate@ is not supported for data repositories
    -- linked to an Amazon File Cache resource.
    batchImportMetaDataOnCreate :: Prelude.Maybe Prelude.Bool,
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The path to the data repository that will be linked to the cache or file
    -- system.
    --
    -- -   For Amazon File Cache, the path can be an NFS data repository that
    --     will be linked to the cache. The path can be in one of two formats:
    --
    --     -   If you are not using the @DataRepositorySubdirectories@
    --         parameter, the path is to an NFS Export directory (or one of its
    --         subdirectories) in the format
    --         @nsf:\/\/nfs-domain-name\/exportpath@. You can therefore link a
    --         single NFS Export to a single data repository association.
    --
    --     -   If you are using the @DataRepositorySubdirectories@ parameter,
    --         the path is the domain name of the NFS file system in the format
    --         @nfs:\/\/filer-domain-name@, which indicates the root of the
    --         subdirectories specified with the @DataRepositorySubdirectories@
    --         parameter.
    --
    -- -   For Amazon File Cache, the path can be an S3 bucket or prefix in the
    --     format @s3:\/\/myBucket\/myPrefix\/@.
    --
    -- -   For Amazon FSx for Lustre, the path can be an S3 bucket or prefix in
    --     the format @s3:\/\/myBucket\/myPrefix\/@.
    dataRepositoryPath :: Prelude.Maybe Prelude.Text,
    -- | For Amazon File Cache, a list of NFS Exports that will be linked with an
    -- NFS data repository association. All the subdirectories must be on a
    -- single NFS file system. The Export paths are in the format
    -- @\/exportpath1@. To use this parameter, you must configure
    -- @DataRepositoryPath@ as the domain name of the NFS file system. The NFS
    -- file system domain name in effect is the root of the subdirectories.
    -- Note that @DataRepositorySubdirectories@ is not supported for S3 data
    -- repositories.
    dataRepositorySubdirectories :: Prelude.Maybe [Prelude.Text],
    failureDetails :: Prelude.Maybe DataRepositoryFailureDetails,
    -- | The globally unique ID of the Amazon File Cache resource.
    fileCacheId :: Prelude.Maybe Prelude.Text,
    -- | A path on the Amazon File Cache that points to a high-level directory
    -- (such as @\/ns1\/@) or subdirectory (such as @\/ns1\/subdir\/@) that
    -- will be mapped 1-1 with @DataRepositoryPath@. The leading forward slash
    -- in the path is required. Two data repository associations cannot have
    -- overlapping cache paths. For example, if a data repository is associated
    -- with cache path @\/ns1\/@, then you cannot link another data repository
    -- with cache path @\/ns1\/ns2@.
    --
    -- This path specifies the directory in your cache where files will be
    -- exported from. This cache directory can be linked to only one data
    -- repository (S3 or NFS) and no other data repository can be linked to the
    -- directory.
    --
    -- The cache path can only be set to root (\/) on an NFS DRA when
    -- @DataRepositorySubdirectories@ is specified. If you specify root (\/) as
    -- the cache path, you can create only one DRA on the cache.
    --
    -- The cache path cannot be set to root (\/) for an S3 DRA.
    fileCachePath :: Prelude.Maybe Prelude.Text,
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | A path on the Amazon FSx for Lustre file system that points to a
    -- high-level directory (such as @\/ns1\/@) or subdirectory (such as
    -- @\/ns1\/subdir\/@) that will be mapped 1-1 with @DataRepositoryPath@.
    -- The leading forward slash in the name is required. Two data repository
    -- associations cannot have overlapping file system paths. For example, if
    -- a data repository is associated with file system path @\/ns1\/@, then
    -- you cannot link another data repository with file system path
    -- @\/ns1\/ns2@.
    --
    -- This path specifies where in your file system files will be exported
    -- from or imported to. This file system directory can be linked to only
    -- one Amazon S3 bucket, and no other S3 bucket can be linked to the
    -- directory.
    --
    -- If you specify only a forward slash (@\/@) as the file system path, you
    -- can link only one data repository to the file system. You can only
    -- specify \"\/\" as the file system path for the first data repository
    -- associated with a file system.
    fileSystemPath :: Prelude.Maybe Prelude.Text,
    -- | For files imported from a data repository, this value determines the
    -- stripe count and maximum amount of data per file (in MiB) stored on a
    -- single physical disk. The maximum number of disks that a single file can
    -- be striped across is limited by the total number of disks that make up
    -- the file system or cache.
    --
    -- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
    -- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
    importedFileChunkSize :: Prelude.Maybe Prelude.Natural,
    -- | Describes the state of a data repository association. The lifecycle can
    -- have the following values:
    --
    -- -   @CREATING@ - The data repository association between the file system
    --     or cache and the data repository is being created. The data
    --     repository is unavailable.
    --
    -- -   @AVAILABLE@ - The data repository association is available for use.
    --
    -- -   @MISCONFIGURED@ - The data repository association is misconfigured.
    --     Until the configuration is corrected, automatic import and automatic
    --     export will not work (only for Amazon FSx for Lustre).
    --
    -- -   @UPDATING@ - The data repository association is undergoing a
    --     customer initiated update that might affect its availability.
    --
    -- -   @DELETING@ - The data repository association is undergoing a
    --     customer initiated deletion.
    --
    -- -   @FAILED@ - The data repository association is in a terminal state
    --     that cannot be recovered.
    lifecycle :: Prelude.Maybe DataRepositoryLifecycle,
    -- | The configuration for an NFS data repository linked to an Amazon File
    -- Cache resource with a data repository association.
    nfs :: Prelude.Maybe NFSDataRepositoryConfiguration,
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | The configuration for an Amazon S3 data repository linked to an Amazon
    -- FSx for Lustre file system with a data repository association.
    s3 :: Prelude.Maybe S3DataRepositoryConfiguration,
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataRepositoryAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'dataRepositoryAssociation_associationId' - The system-generated, unique ID of the data repository association.
--
-- 'batchImportMetaDataOnCreate', 'dataRepositoryAssociation_batchImportMetaDataOnCreate' - A boolean flag indicating whether an import data repository task to
-- import metadata should run after the data repository association is
-- created. The task runs if this flag is set to @true@.
--
-- @BatchImportMetaDataOnCreate@ is not supported for data repositories
-- linked to an Amazon File Cache resource.
--
-- 'creationTime', 'dataRepositoryAssociation_creationTime' - Undocumented member.
--
-- 'dataRepositoryPath', 'dataRepositoryAssociation_dataRepositoryPath' - The path to the data repository that will be linked to the cache or file
-- system.
--
-- -   For Amazon File Cache, the path can be an NFS data repository that
--     will be linked to the cache. The path can be in one of two formats:
--
--     -   If you are not using the @DataRepositorySubdirectories@
--         parameter, the path is to an NFS Export directory (or one of its
--         subdirectories) in the format
--         @nsf:\/\/nfs-domain-name\/exportpath@. You can therefore link a
--         single NFS Export to a single data repository association.
--
--     -   If you are using the @DataRepositorySubdirectories@ parameter,
--         the path is the domain name of the NFS file system in the format
--         @nfs:\/\/filer-domain-name@, which indicates the root of the
--         subdirectories specified with the @DataRepositorySubdirectories@
--         parameter.
--
-- -   For Amazon File Cache, the path can be an S3 bucket or prefix in the
--     format @s3:\/\/myBucket\/myPrefix\/@.
--
-- -   For Amazon FSx for Lustre, the path can be an S3 bucket or prefix in
--     the format @s3:\/\/myBucket\/myPrefix\/@.
--
-- 'dataRepositorySubdirectories', 'dataRepositoryAssociation_dataRepositorySubdirectories' - For Amazon File Cache, a list of NFS Exports that will be linked with an
-- NFS data repository association. All the subdirectories must be on a
-- single NFS file system. The Export paths are in the format
-- @\/exportpath1@. To use this parameter, you must configure
-- @DataRepositoryPath@ as the domain name of the NFS file system. The NFS
-- file system domain name in effect is the root of the subdirectories.
-- Note that @DataRepositorySubdirectories@ is not supported for S3 data
-- repositories.
--
-- 'failureDetails', 'dataRepositoryAssociation_failureDetails' - Undocumented member.
--
-- 'fileCacheId', 'dataRepositoryAssociation_fileCacheId' - The globally unique ID of the Amazon File Cache resource.
--
-- 'fileCachePath', 'dataRepositoryAssociation_fileCachePath' - A path on the Amazon File Cache that points to a high-level directory
-- (such as @\/ns1\/@) or subdirectory (such as @\/ns1\/subdir\/@) that
-- will be mapped 1-1 with @DataRepositoryPath@. The leading forward slash
-- in the path is required. Two data repository associations cannot have
-- overlapping cache paths. For example, if a data repository is associated
-- with cache path @\/ns1\/@, then you cannot link another data repository
-- with cache path @\/ns1\/ns2@.
--
-- This path specifies the directory in your cache where files will be
-- exported from. This cache directory can be linked to only one data
-- repository (S3 or NFS) and no other data repository can be linked to the
-- directory.
--
-- The cache path can only be set to root (\/) on an NFS DRA when
-- @DataRepositorySubdirectories@ is specified. If you specify root (\/) as
-- the cache path, you can create only one DRA on the cache.
--
-- The cache path cannot be set to root (\/) for an S3 DRA.
--
-- 'fileSystemId', 'dataRepositoryAssociation_fileSystemId' - Undocumented member.
--
-- 'fileSystemPath', 'dataRepositoryAssociation_fileSystemPath' - A path on the Amazon FSx for Lustre file system that points to a
-- high-level directory (such as @\/ns1\/@) or subdirectory (such as
-- @\/ns1\/subdir\/@) that will be mapped 1-1 with @DataRepositoryPath@.
-- The leading forward slash in the name is required. Two data repository
-- associations cannot have overlapping file system paths. For example, if
-- a data repository is associated with file system path @\/ns1\/@, then
-- you cannot link another data repository with file system path
-- @\/ns1\/ns2@.
--
-- This path specifies where in your file system files will be exported
-- from or imported to. This file system directory can be linked to only
-- one Amazon S3 bucket, and no other S3 bucket can be linked to the
-- directory.
--
-- If you specify only a forward slash (@\/@) as the file system path, you
-- can link only one data repository to the file system. You can only
-- specify \"\/\" as the file system path for the first data repository
-- associated with a file system.
--
-- 'importedFileChunkSize', 'dataRepositoryAssociation_importedFileChunkSize' - For files imported from a data repository, this value determines the
-- stripe count and maximum amount of data per file (in MiB) stored on a
-- single physical disk. The maximum number of disks that a single file can
-- be striped across is limited by the total number of disks that make up
-- the file system or cache.
--
-- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
-- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
--
-- 'lifecycle', 'dataRepositoryAssociation_lifecycle' - Describes the state of a data repository association. The lifecycle can
-- have the following values:
--
-- -   @CREATING@ - The data repository association between the file system
--     or cache and the data repository is being created. The data
--     repository is unavailable.
--
-- -   @AVAILABLE@ - The data repository association is available for use.
--
-- -   @MISCONFIGURED@ - The data repository association is misconfigured.
--     Until the configuration is corrected, automatic import and automatic
--     export will not work (only for Amazon FSx for Lustre).
--
-- -   @UPDATING@ - The data repository association is undergoing a
--     customer initiated update that might affect its availability.
--
-- -   @DELETING@ - The data repository association is undergoing a
--     customer initiated deletion.
--
-- -   @FAILED@ - The data repository association is in a terminal state
--     that cannot be recovered.
--
-- 'nfs', 'dataRepositoryAssociation_nfs' - The configuration for an NFS data repository linked to an Amazon File
-- Cache resource with a data repository association.
--
-- 'resourceARN', 'dataRepositoryAssociation_resourceARN' - Undocumented member.
--
-- 's3', 'dataRepositoryAssociation_s3' - The configuration for an Amazon S3 data repository linked to an Amazon
-- FSx for Lustre file system with a data repository association.
--
-- 'tags', 'dataRepositoryAssociation_tags' - Undocumented member.
newDataRepositoryAssociation ::
  DataRepositoryAssociation
newDataRepositoryAssociation =
  DataRepositoryAssociation'
    { associationId =
        Prelude.Nothing,
      batchImportMetaDataOnCreate = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      dataRepositoryPath = Prelude.Nothing,
      dataRepositorySubdirectories = Prelude.Nothing,
      failureDetails = Prelude.Nothing,
      fileCacheId = Prelude.Nothing,
      fileCachePath = Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      fileSystemPath = Prelude.Nothing,
      importedFileChunkSize = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      nfs = Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      s3 = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The system-generated, unique ID of the data repository association.
dataRepositoryAssociation_associationId :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe Prelude.Text)
dataRepositoryAssociation_associationId = Lens.lens (\DataRepositoryAssociation' {associationId} -> associationId) (\s@DataRepositoryAssociation' {} a -> s {associationId = a} :: DataRepositoryAssociation)

-- | A boolean flag indicating whether an import data repository task to
-- import metadata should run after the data repository association is
-- created. The task runs if this flag is set to @true@.
--
-- @BatchImportMetaDataOnCreate@ is not supported for data repositories
-- linked to an Amazon File Cache resource.
dataRepositoryAssociation_batchImportMetaDataOnCreate :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe Prelude.Bool)
dataRepositoryAssociation_batchImportMetaDataOnCreate = Lens.lens (\DataRepositoryAssociation' {batchImportMetaDataOnCreate} -> batchImportMetaDataOnCreate) (\s@DataRepositoryAssociation' {} a -> s {batchImportMetaDataOnCreate = a} :: DataRepositoryAssociation)

-- | Undocumented member.
dataRepositoryAssociation_creationTime :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe Prelude.UTCTime)
dataRepositoryAssociation_creationTime = Lens.lens (\DataRepositoryAssociation' {creationTime} -> creationTime) (\s@DataRepositoryAssociation' {} a -> s {creationTime = a} :: DataRepositoryAssociation) Prelude.. Lens.mapping Data._Time

-- | The path to the data repository that will be linked to the cache or file
-- system.
--
-- -   For Amazon File Cache, the path can be an NFS data repository that
--     will be linked to the cache. The path can be in one of two formats:
--
--     -   If you are not using the @DataRepositorySubdirectories@
--         parameter, the path is to an NFS Export directory (or one of its
--         subdirectories) in the format
--         @nsf:\/\/nfs-domain-name\/exportpath@. You can therefore link a
--         single NFS Export to a single data repository association.
--
--     -   If you are using the @DataRepositorySubdirectories@ parameter,
--         the path is the domain name of the NFS file system in the format
--         @nfs:\/\/filer-domain-name@, which indicates the root of the
--         subdirectories specified with the @DataRepositorySubdirectories@
--         parameter.
--
-- -   For Amazon File Cache, the path can be an S3 bucket or prefix in the
--     format @s3:\/\/myBucket\/myPrefix\/@.
--
-- -   For Amazon FSx for Lustre, the path can be an S3 bucket or prefix in
--     the format @s3:\/\/myBucket\/myPrefix\/@.
dataRepositoryAssociation_dataRepositoryPath :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe Prelude.Text)
dataRepositoryAssociation_dataRepositoryPath = Lens.lens (\DataRepositoryAssociation' {dataRepositoryPath} -> dataRepositoryPath) (\s@DataRepositoryAssociation' {} a -> s {dataRepositoryPath = a} :: DataRepositoryAssociation)

-- | For Amazon File Cache, a list of NFS Exports that will be linked with an
-- NFS data repository association. All the subdirectories must be on a
-- single NFS file system. The Export paths are in the format
-- @\/exportpath1@. To use this parameter, you must configure
-- @DataRepositoryPath@ as the domain name of the NFS file system. The NFS
-- file system domain name in effect is the root of the subdirectories.
-- Note that @DataRepositorySubdirectories@ is not supported for S3 data
-- repositories.
dataRepositoryAssociation_dataRepositorySubdirectories :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe [Prelude.Text])
dataRepositoryAssociation_dataRepositorySubdirectories = Lens.lens (\DataRepositoryAssociation' {dataRepositorySubdirectories} -> dataRepositorySubdirectories) (\s@DataRepositoryAssociation' {} a -> s {dataRepositorySubdirectories = a} :: DataRepositoryAssociation) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
dataRepositoryAssociation_failureDetails :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe DataRepositoryFailureDetails)
dataRepositoryAssociation_failureDetails = Lens.lens (\DataRepositoryAssociation' {failureDetails} -> failureDetails) (\s@DataRepositoryAssociation' {} a -> s {failureDetails = a} :: DataRepositoryAssociation)

-- | The globally unique ID of the Amazon File Cache resource.
dataRepositoryAssociation_fileCacheId :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe Prelude.Text)
dataRepositoryAssociation_fileCacheId = Lens.lens (\DataRepositoryAssociation' {fileCacheId} -> fileCacheId) (\s@DataRepositoryAssociation' {} a -> s {fileCacheId = a} :: DataRepositoryAssociation)

-- | A path on the Amazon File Cache that points to a high-level directory
-- (such as @\/ns1\/@) or subdirectory (such as @\/ns1\/subdir\/@) that
-- will be mapped 1-1 with @DataRepositoryPath@. The leading forward slash
-- in the path is required. Two data repository associations cannot have
-- overlapping cache paths. For example, if a data repository is associated
-- with cache path @\/ns1\/@, then you cannot link another data repository
-- with cache path @\/ns1\/ns2@.
--
-- This path specifies the directory in your cache where files will be
-- exported from. This cache directory can be linked to only one data
-- repository (S3 or NFS) and no other data repository can be linked to the
-- directory.
--
-- The cache path can only be set to root (\/) on an NFS DRA when
-- @DataRepositorySubdirectories@ is specified. If you specify root (\/) as
-- the cache path, you can create only one DRA on the cache.
--
-- The cache path cannot be set to root (\/) for an S3 DRA.
dataRepositoryAssociation_fileCachePath :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe Prelude.Text)
dataRepositoryAssociation_fileCachePath = Lens.lens (\DataRepositoryAssociation' {fileCachePath} -> fileCachePath) (\s@DataRepositoryAssociation' {} a -> s {fileCachePath = a} :: DataRepositoryAssociation)

-- | Undocumented member.
dataRepositoryAssociation_fileSystemId :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe Prelude.Text)
dataRepositoryAssociation_fileSystemId = Lens.lens (\DataRepositoryAssociation' {fileSystemId} -> fileSystemId) (\s@DataRepositoryAssociation' {} a -> s {fileSystemId = a} :: DataRepositoryAssociation)

-- | A path on the Amazon FSx for Lustre file system that points to a
-- high-level directory (such as @\/ns1\/@) or subdirectory (such as
-- @\/ns1\/subdir\/@) that will be mapped 1-1 with @DataRepositoryPath@.
-- The leading forward slash in the name is required. Two data repository
-- associations cannot have overlapping file system paths. For example, if
-- a data repository is associated with file system path @\/ns1\/@, then
-- you cannot link another data repository with file system path
-- @\/ns1\/ns2@.
--
-- This path specifies where in your file system files will be exported
-- from or imported to. This file system directory can be linked to only
-- one Amazon S3 bucket, and no other S3 bucket can be linked to the
-- directory.
--
-- If you specify only a forward slash (@\/@) as the file system path, you
-- can link only one data repository to the file system. You can only
-- specify \"\/\" as the file system path for the first data repository
-- associated with a file system.
dataRepositoryAssociation_fileSystemPath :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe Prelude.Text)
dataRepositoryAssociation_fileSystemPath = Lens.lens (\DataRepositoryAssociation' {fileSystemPath} -> fileSystemPath) (\s@DataRepositoryAssociation' {} a -> s {fileSystemPath = a} :: DataRepositoryAssociation)

-- | For files imported from a data repository, this value determines the
-- stripe count and maximum amount of data per file (in MiB) stored on a
-- single physical disk. The maximum number of disks that a single file can
-- be striped across is limited by the total number of disks that make up
-- the file system or cache.
--
-- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
-- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
dataRepositoryAssociation_importedFileChunkSize :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe Prelude.Natural)
dataRepositoryAssociation_importedFileChunkSize = Lens.lens (\DataRepositoryAssociation' {importedFileChunkSize} -> importedFileChunkSize) (\s@DataRepositoryAssociation' {} a -> s {importedFileChunkSize = a} :: DataRepositoryAssociation)

-- | Describes the state of a data repository association. The lifecycle can
-- have the following values:
--
-- -   @CREATING@ - The data repository association between the file system
--     or cache and the data repository is being created. The data
--     repository is unavailable.
--
-- -   @AVAILABLE@ - The data repository association is available for use.
--
-- -   @MISCONFIGURED@ - The data repository association is misconfigured.
--     Until the configuration is corrected, automatic import and automatic
--     export will not work (only for Amazon FSx for Lustre).
--
-- -   @UPDATING@ - The data repository association is undergoing a
--     customer initiated update that might affect its availability.
--
-- -   @DELETING@ - The data repository association is undergoing a
--     customer initiated deletion.
--
-- -   @FAILED@ - The data repository association is in a terminal state
--     that cannot be recovered.
dataRepositoryAssociation_lifecycle :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe DataRepositoryLifecycle)
dataRepositoryAssociation_lifecycle = Lens.lens (\DataRepositoryAssociation' {lifecycle} -> lifecycle) (\s@DataRepositoryAssociation' {} a -> s {lifecycle = a} :: DataRepositoryAssociation)

-- | The configuration for an NFS data repository linked to an Amazon File
-- Cache resource with a data repository association.
dataRepositoryAssociation_nfs :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe NFSDataRepositoryConfiguration)
dataRepositoryAssociation_nfs = Lens.lens (\DataRepositoryAssociation' {nfs} -> nfs) (\s@DataRepositoryAssociation' {} a -> s {nfs = a} :: DataRepositoryAssociation)

-- | Undocumented member.
dataRepositoryAssociation_resourceARN :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe Prelude.Text)
dataRepositoryAssociation_resourceARN = Lens.lens (\DataRepositoryAssociation' {resourceARN} -> resourceARN) (\s@DataRepositoryAssociation' {} a -> s {resourceARN = a} :: DataRepositoryAssociation)

-- | The configuration for an Amazon S3 data repository linked to an Amazon
-- FSx for Lustre file system with a data repository association.
dataRepositoryAssociation_s3 :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe S3DataRepositoryConfiguration)
dataRepositoryAssociation_s3 = Lens.lens (\DataRepositoryAssociation' {s3} -> s3) (\s@DataRepositoryAssociation' {} a -> s {s3 = a} :: DataRepositoryAssociation)

-- | Undocumented member.
dataRepositoryAssociation_tags :: Lens.Lens' DataRepositoryAssociation (Prelude.Maybe (Prelude.NonEmpty Tag))
dataRepositoryAssociation_tags = Lens.lens (\DataRepositoryAssociation' {tags} -> tags) (\s@DataRepositoryAssociation' {} a -> s {tags = a} :: DataRepositoryAssociation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DataRepositoryAssociation where
  parseJSON =
    Data.withObject
      "DataRepositoryAssociation"
      ( \x ->
          DataRepositoryAssociation'
            Prelude.<$> (x Data..:? "AssociationId")
            Prelude.<*> (x Data..:? "BatchImportMetaDataOnCreate")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DataRepositoryPath")
            Prelude.<*> ( x
                            Data..:? "DataRepositorySubdirectories"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FailureDetails")
            Prelude.<*> (x Data..:? "FileCacheId")
            Prelude.<*> (x Data..:? "FileCachePath")
            Prelude.<*> (x Data..:? "FileSystemId")
            Prelude.<*> (x Data..:? "FileSystemPath")
            Prelude.<*> (x Data..:? "ImportedFileChunkSize")
            Prelude.<*> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..:? "NFS")
            Prelude.<*> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "S3")
            Prelude.<*> (x Data..:? "Tags")
      )

instance Prelude.Hashable DataRepositoryAssociation where
  hashWithSalt _salt DataRepositoryAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` batchImportMetaDataOnCreate
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataRepositoryPath
      `Prelude.hashWithSalt` dataRepositorySubdirectories
      `Prelude.hashWithSalt` failureDetails
      `Prelude.hashWithSalt` fileCacheId
      `Prelude.hashWithSalt` fileCachePath
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` fileSystemPath
      `Prelude.hashWithSalt` importedFileChunkSize
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` nfs
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` s3
      `Prelude.hashWithSalt` tags

instance Prelude.NFData DataRepositoryAssociation where
  rnf DataRepositoryAssociation' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf batchImportMetaDataOnCreate
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataRepositoryPath
      `Prelude.seq` Prelude.rnf dataRepositorySubdirectories
      `Prelude.seq` Prelude.rnf failureDetails
      `Prelude.seq` Prelude.rnf fileCacheId
      `Prelude.seq` Prelude.rnf fileCachePath
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf fileSystemPath
      `Prelude.seq` Prelude.rnf importedFileChunkSize
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf nfs
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf tags
