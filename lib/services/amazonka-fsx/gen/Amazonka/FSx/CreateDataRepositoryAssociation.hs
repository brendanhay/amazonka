{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FSx.CreateDataRepositoryAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon FSx for Lustre data repository association (DRA). A
-- data repository association is a link between a directory on the file
-- system and an Amazon S3 bucket or prefix. You can have a maximum of 8
-- data repository associations on a file system. Data repository
-- associations are supported only for file systems with the @Persistent_2@
-- deployment type.
--
-- Each data repository association must have a unique Amazon FSx file
-- system directory and a unique S3 bucket or prefix associated with it.
-- You can configure a data repository association for automatic import
-- only, for automatic export only, or for both. To learn more about
-- linking a data repository to your file system, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/create-dra-linked-data-repo.html Linking your file system to an S3 bucket>.
--
-- @CreateDataRepositoryAssociation@ isn\'t supported on Amazon File Cache
-- resources. To create a DRA on Amazon File Cache, use the
-- @CreateFileCache@ operation.
module Amazonka.FSx.CreateDataRepositoryAssociation
  ( -- * Creating a Request
    CreateDataRepositoryAssociation (..),
    newCreateDataRepositoryAssociation,

    -- * Request Lenses
    createDataRepositoryAssociation_tags,
    createDataRepositoryAssociation_clientRequestToken,
    createDataRepositoryAssociation_s3,
    createDataRepositoryAssociation_importedFileChunkSize,
    createDataRepositoryAssociation_fileSystemPath,
    createDataRepositoryAssociation_batchImportMetaDataOnCreate,
    createDataRepositoryAssociation_fileSystemId,
    createDataRepositoryAssociation_dataRepositoryPath,

    -- * Destructuring the Response
    CreateDataRepositoryAssociationResponse (..),
    newCreateDataRepositoryAssociationResponse,

    -- * Response Lenses
    createDataRepositoryAssociationResponse_association,
    createDataRepositoryAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataRepositoryAssociation' smart constructor.
data CreateDataRepositoryAssociation = CreateDataRepositoryAssociation'
  { tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The configuration for an Amazon S3 data repository linked to an Amazon
    -- FSx Lustre file system with a data repository association. The
    -- configuration defines which file events (new, changed, or deleted files
    -- or directories) are automatically imported from the linked data
    -- repository to the file system or automatically exported from the file
    -- system to the data repository.
    s3 :: Prelude.Maybe S3DataRepositoryConfiguration,
    -- | For files imported from a data repository, this value determines the
    -- stripe count and maximum amount of data per file (in MiB) stored on a
    -- single physical disk. The maximum number of disks that a single file can
    -- be striped across is limited by the total number of disks that make up
    -- the file system.
    --
    -- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
    -- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
    importedFileChunkSize :: Prelude.Maybe Prelude.Natural,
    -- | A path on the file system that points to a high-level directory (such as
    -- @\/ns1\/@) or subdirectory (such as @\/ns1\/subdir\/@) that will be
    -- mapped 1-1 with @DataRepositoryPath@. The leading forward slash in the
    -- name is required. Two data repository associations cannot have
    -- overlapping file system paths. For example, if a data repository is
    -- associated with file system path @\/ns1\/@, then you cannot link another
    -- data repository with file system path @\/ns1\/ns2@.
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
    -- | Set to @true@ to run an import data repository task to import metadata
    -- from the data repository to the file system after the data repository
    -- association is created. Default is @false@.
    batchImportMetaDataOnCreate :: Prelude.Maybe Prelude.Bool,
    fileSystemId :: Prelude.Text,
    -- | The path to the Amazon S3 data repository that will be linked to the
    -- file system. The path can be an S3 bucket or prefix in the format
    -- @s3:\/\/myBucket\/myPrefix\/@. This path specifies where in the S3 data
    -- repository files will be imported from or exported to.
    dataRepositoryPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataRepositoryAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDataRepositoryAssociation_tags' - Undocumented member.
--
-- 'clientRequestToken', 'createDataRepositoryAssociation_clientRequestToken' - Undocumented member.
--
-- 's3', 'createDataRepositoryAssociation_s3' - The configuration for an Amazon S3 data repository linked to an Amazon
-- FSx Lustre file system with a data repository association. The
-- configuration defines which file events (new, changed, or deleted files
-- or directories) are automatically imported from the linked data
-- repository to the file system or automatically exported from the file
-- system to the data repository.
--
-- 'importedFileChunkSize', 'createDataRepositoryAssociation_importedFileChunkSize' - For files imported from a data repository, this value determines the
-- stripe count and maximum amount of data per file (in MiB) stored on a
-- single physical disk. The maximum number of disks that a single file can
-- be striped across is limited by the total number of disks that make up
-- the file system.
--
-- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
-- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
--
-- 'fileSystemPath', 'createDataRepositoryAssociation_fileSystemPath' - A path on the file system that points to a high-level directory (such as
-- @\/ns1\/@) or subdirectory (such as @\/ns1\/subdir\/@) that will be
-- mapped 1-1 with @DataRepositoryPath@. The leading forward slash in the
-- name is required. Two data repository associations cannot have
-- overlapping file system paths. For example, if a data repository is
-- associated with file system path @\/ns1\/@, then you cannot link another
-- data repository with file system path @\/ns1\/ns2@.
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
-- 'batchImportMetaDataOnCreate', 'createDataRepositoryAssociation_batchImportMetaDataOnCreate' - Set to @true@ to run an import data repository task to import metadata
-- from the data repository to the file system after the data repository
-- association is created. Default is @false@.
--
-- 'fileSystemId', 'createDataRepositoryAssociation_fileSystemId' - Undocumented member.
--
-- 'dataRepositoryPath', 'createDataRepositoryAssociation_dataRepositoryPath' - The path to the Amazon S3 data repository that will be linked to the
-- file system. The path can be an S3 bucket or prefix in the format
-- @s3:\/\/myBucket\/myPrefix\/@. This path specifies where in the S3 data
-- repository files will be imported from or exported to.
newCreateDataRepositoryAssociation ::
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'dataRepositoryPath'
  Prelude.Text ->
  CreateDataRepositoryAssociation
newCreateDataRepositoryAssociation
  pFileSystemId_
  pDataRepositoryPath_ =
    CreateDataRepositoryAssociation'
      { tags =
          Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        s3 = Prelude.Nothing,
        importedFileChunkSize = Prelude.Nothing,
        fileSystemPath = Prelude.Nothing,
        batchImportMetaDataOnCreate =
          Prelude.Nothing,
        fileSystemId = pFileSystemId_,
        dataRepositoryPath = pDataRepositoryPath_
      }

-- | Undocumented member.
createDataRepositoryAssociation_tags :: Lens.Lens' CreateDataRepositoryAssociation (Prelude.Maybe (Prelude.NonEmpty Tag))
createDataRepositoryAssociation_tags = Lens.lens (\CreateDataRepositoryAssociation' {tags} -> tags) (\s@CreateDataRepositoryAssociation' {} a -> s {tags = a} :: CreateDataRepositoryAssociation) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createDataRepositoryAssociation_clientRequestToken :: Lens.Lens' CreateDataRepositoryAssociation (Prelude.Maybe Prelude.Text)
createDataRepositoryAssociation_clientRequestToken = Lens.lens (\CreateDataRepositoryAssociation' {clientRequestToken} -> clientRequestToken) (\s@CreateDataRepositoryAssociation' {} a -> s {clientRequestToken = a} :: CreateDataRepositoryAssociation)

-- | The configuration for an Amazon S3 data repository linked to an Amazon
-- FSx Lustre file system with a data repository association. The
-- configuration defines which file events (new, changed, or deleted files
-- or directories) are automatically imported from the linked data
-- repository to the file system or automatically exported from the file
-- system to the data repository.
createDataRepositoryAssociation_s3 :: Lens.Lens' CreateDataRepositoryAssociation (Prelude.Maybe S3DataRepositoryConfiguration)
createDataRepositoryAssociation_s3 = Lens.lens (\CreateDataRepositoryAssociation' {s3} -> s3) (\s@CreateDataRepositoryAssociation' {} a -> s {s3 = a} :: CreateDataRepositoryAssociation)

-- | For files imported from a data repository, this value determines the
-- stripe count and maximum amount of data per file (in MiB) stored on a
-- single physical disk. The maximum number of disks that a single file can
-- be striped across is limited by the total number of disks that make up
-- the file system.
--
-- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
-- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
createDataRepositoryAssociation_importedFileChunkSize :: Lens.Lens' CreateDataRepositoryAssociation (Prelude.Maybe Prelude.Natural)
createDataRepositoryAssociation_importedFileChunkSize = Lens.lens (\CreateDataRepositoryAssociation' {importedFileChunkSize} -> importedFileChunkSize) (\s@CreateDataRepositoryAssociation' {} a -> s {importedFileChunkSize = a} :: CreateDataRepositoryAssociation)

-- | A path on the file system that points to a high-level directory (such as
-- @\/ns1\/@) or subdirectory (such as @\/ns1\/subdir\/@) that will be
-- mapped 1-1 with @DataRepositoryPath@. The leading forward slash in the
-- name is required. Two data repository associations cannot have
-- overlapping file system paths. For example, if a data repository is
-- associated with file system path @\/ns1\/@, then you cannot link another
-- data repository with file system path @\/ns1\/ns2@.
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
createDataRepositoryAssociation_fileSystemPath :: Lens.Lens' CreateDataRepositoryAssociation (Prelude.Maybe Prelude.Text)
createDataRepositoryAssociation_fileSystemPath = Lens.lens (\CreateDataRepositoryAssociation' {fileSystemPath} -> fileSystemPath) (\s@CreateDataRepositoryAssociation' {} a -> s {fileSystemPath = a} :: CreateDataRepositoryAssociation)

-- | Set to @true@ to run an import data repository task to import metadata
-- from the data repository to the file system after the data repository
-- association is created. Default is @false@.
createDataRepositoryAssociation_batchImportMetaDataOnCreate :: Lens.Lens' CreateDataRepositoryAssociation (Prelude.Maybe Prelude.Bool)
createDataRepositoryAssociation_batchImportMetaDataOnCreate = Lens.lens (\CreateDataRepositoryAssociation' {batchImportMetaDataOnCreate} -> batchImportMetaDataOnCreate) (\s@CreateDataRepositoryAssociation' {} a -> s {batchImportMetaDataOnCreate = a} :: CreateDataRepositoryAssociation)

-- | Undocumented member.
createDataRepositoryAssociation_fileSystemId :: Lens.Lens' CreateDataRepositoryAssociation Prelude.Text
createDataRepositoryAssociation_fileSystemId = Lens.lens (\CreateDataRepositoryAssociation' {fileSystemId} -> fileSystemId) (\s@CreateDataRepositoryAssociation' {} a -> s {fileSystemId = a} :: CreateDataRepositoryAssociation)

-- | The path to the Amazon S3 data repository that will be linked to the
-- file system. The path can be an S3 bucket or prefix in the format
-- @s3:\/\/myBucket\/myPrefix\/@. This path specifies where in the S3 data
-- repository files will be imported from or exported to.
createDataRepositoryAssociation_dataRepositoryPath :: Lens.Lens' CreateDataRepositoryAssociation Prelude.Text
createDataRepositoryAssociation_dataRepositoryPath = Lens.lens (\CreateDataRepositoryAssociation' {dataRepositoryPath} -> dataRepositoryPath) (\s@CreateDataRepositoryAssociation' {} a -> s {dataRepositoryPath = a} :: CreateDataRepositoryAssociation)

instance
  Core.AWSRequest
    CreateDataRepositoryAssociation
  where
  type
    AWSResponse CreateDataRepositoryAssociation =
      CreateDataRepositoryAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataRepositoryAssociationResponse'
            Prelude.<$> (x Data..?> "Association")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateDataRepositoryAssociation
  where
  hashWithSalt
    _salt
    CreateDataRepositoryAssociation' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` s3
        `Prelude.hashWithSalt` importedFileChunkSize
        `Prelude.hashWithSalt` fileSystemPath
        `Prelude.hashWithSalt` batchImportMetaDataOnCreate
        `Prelude.hashWithSalt` fileSystemId
        `Prelude.hashWithSalt` dataRepositoryPath

instance
  Prelude.NFData
    CreateDataRepositoryAssociation
  where
  rnf CreateDataRepositoryAssociation' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf importedFileChunkSize
      `Prelude.seq` Prelude.rnf fileSystemPath
      `Prelude.seq` Prelude.rnf batchImportMetaDataOnCreate
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf dataRepositoryPath

instance
  Data.ToHeaders
    CreateDataRepositoryAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.CreateDataRepositoryAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataRepositoryAssociation where
  toJSON CreateDataRepositoryAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("S3" Data..=) Prelude.<$> s3,
            ("ImportedFileChunkSize" Data..=)
              Prelude.<$> importedFileChunkSize,
            ("FileSystemPath" Data..=)
              Prelude.<$> fileSystemPath,
            ("BatchImportMetaDataOnCreate" Data..=)
              Prelude.<$> batchImportMetaDataOnCreate,
            Prelude.Just ("FileSystemId" Data..= fileSystemId),
            Prelude.Just
              ("DataRepositoryPath" Data..= dataRepositoryPath)
          ]
      )

instance Data.ToPath CreateDataRepositoryAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDataRepositoryAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataRepositoryAssociationResponse' smart constructor.
data CreateDataRepositoryAssociationResponse = CreateDataRepositoryAssociationResponse'
  { -- | The response object returned after the data repository association is
    -- created.
    association :: Prelude.Maybe DataRepositoryAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataRepositoryAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'association', 'createDataRepositoryAssociationResponse_association' - The response object returned after the data repository association is
-- created.
--
-- 'httpStatus', 'createDataRepositoryAssociationResponse_httpStatus' - The response's http status code.
newCreateDataRepositoryAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataRepositoryAssociationResponse
newCreateDataRepositoryAssociationResponse
  pHttpStatus_ =
    CreateDataRepositoryAssociationResponse'
      { association =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The response object returned after the data repository association is
-- created.
createDataRepositoryAssociationResponse_association :: Lens.Lens' CreateDataRepositoryAssociationResponse (Prelude.Maybe DataRepositoryAssociation)
createDataRepositoryAssociationResponse_association = Lens.lens (\CreateDataRepositoryAssociationResponse' {association} -> association) (\s@CreateDataRepositoryAssociationResponse' {} a -> s {association = a} :: CreateDataRepositoryAssociationResponse)

-- | The response's http status code.
createDataRepositoryAssociationResponse_httpStatus :: Lens.Lens' CreateDataRepositoryAssociationResponse Prelude.Int
createDataRepositoryAssociationResponse_httpStatus = Lens.lens (\CreateDataRepositoryAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateDataRepositoryAssociationResponse' {} a -> s {httpStatus = a} :: CreateDataRepositoryAssociationResponse)

instance
  Prelude.NFData
    CreateDataRepositoryAssociationResponse
  where
  rnf CreateDataRepositoryAssociationResponse' {..} =
    Prelude.rnf association
      `Prelude.seq` Prelude.rnf httpStatus
