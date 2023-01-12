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
-- Module      : Amazonka.CodeCommit.GetFile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the base-64 encoded contents of a specified file and its
-- metadata.
module Amazonka.CodeCommit.GetFile
  ( -- * Creating a Request
    GetFile (..),
    newGetFile,

    -- * Request Lenses
    getFile_commitSpecifier,
    getFile_repositoryName,
    getFile_filePath,

    -- * Destructuring the Response
    GetFileResponse (..),
    newGetFileResponse,

    -- * Response Lenses
    getFileResponse_httpStatus,
    getFileResponse_commitId,
    getFileResponse_blobId,
    getFileResponse_filePath,
    getFileResponse_fileMode,
    getFileResponse_fileSize,
    getFileResponse_fileContent,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFile' smart constructor.
data GetFile = GetFile'
  { -- | The fully quaified reference that identifies the commit that contains
    -- the file. For example, you can specify a full commit ID, a tag, a branch
    -- name, or a reference such as refs\/heads\/master. If none is provided,
    -- the head commit is used.
    commitSpecifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that contains the file.
    repositoryName :: Prelude.Text,
    -- | The fully qualified path to the file, including the full name and
    -- extension of the file. For example, \/examples\/file.md is the fully
    -- qualified path to a file named file.md in a folder named examples.
    filePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitSpecifier', 'getFile_commitSpecifier' - The fully quaified reference that identifies the commit that contains
-- the file. For example, you can specify a full commit ID, a tag, a branch
-- name, or a reference such as refs\/heads\/master. If none is provided,
-- the head commit is used.
--
-- 'repositoryName', 'getFile_repositoryName' - The name of the repository that contains the file.
--
-- 'filePath', 'getFile_filePath' - The fully qualified path to the file, including the full name and
-- extension of the file. For example, \/examples\/file.md is the fully
-- qualified path to a file named file.md in a folder named examples.
newGetFile ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'filePath'
  Prelude.Text ->
  GetFile
newGetFile pRepositoryName_ pFilePath_ =
  GetFile'
    { commitSpecifier = Prelude.Nothing,
      repositoryName = pRepositoryName_,
      filePath = pFilePath_
    }

-- | The fully quaified reference that identifies the commit that contains
-- the file. For example, you can specify a full commit ID, a tag, a branch
-- name, or a reference such as refs\/heads\/master. If none is provided,
-- the head commit is used.
getFile_commitSpecifier :: Lens.Lens' GetFile (Prelude.Maybe Prelude.Text)
getFile_commitSpecifier = Lens.lens (\GetFile' {commitSpecifier} -> commitSpecifier) (\s@GetFile' {} a -> s {commitSpecifier = a} :: GetFile)

-- | The name of the repository that contains the file.
getFile_repositoryName :: Lens.Lens' GetFile Prelude.Text
getFile_repositoryName = Lens.lens (\GetFile' {repositoryName} -> repositoryName) (\s@GetFile' {} a -> s {repositoryName = a} :: GetFile)

-- | The fully qualified path to the file, including the full name and
-- extension of the file. For example, \/examples\/file.md is the fully
-- qualified path to a file named file.md in a folder named examples.
getFile_filePath :: Lens.Lens' GetFile Prelude.Text
getFile_filePath = Lens.lens (\GetFile' {filePath} -> filePath) (\s@GetFile' {} a -> s {filePath = a} :: GetFile)

instance Core.AWSRequest GetFile where
  type AWSResponse GetFile = GetFileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "commitId")
            Prelude.<*> (x Data..:> "blobId")
            Prelude.<*> (x Data..:> "filePath")
            Prelude.<*> (x Data..:> "fileMode")
            Prelude.<*> (x Data..:> "fileSize")
            Prelude.<*> (x Data..:> "fileContent")
      )

instance Prelude.Hashable GetFile where
  hashWithSalt _salt GetFile' {..} =
    _salt `Prelude.hashWithSalt` commitSpecifier
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` filePath

instance Prelude.NFData GetFile where
  rnf GetFile' {..} =
    Prelude.rnf commitSpecifier
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf filePath

instance Data.ToHeaders GetFile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.GetFile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFile where
  toJSON GetFile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("commitSpecifier" Data..=)
              Prelude.<$> commitSpecifier,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("filePath" Data..= filePath)
          ]
      )

instance Data.ToPath GetFile where
  toPath = Prelude.const "/"

instance Data.ToQuery GetFile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFileResponse' smart constructor.
data GetFileResponse = GetFileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full commit ID of the commit that contains the content returned by
    -- GetFile.
    commitId :: Prelude.Text,
    -- | The blob ID of the object that represents the file content.
    blobId :: Prelude.Text,
    -- | The fully qualified path to the specified file. Returns the name and
    -- extension of the file.
    filePath :: Prelude.Text,
    -- | The extrapolated file mode permissions of the blob. Valid values include
    -- strings such as EXECUTABLE and not numeric values.
    --
    -- The file mode permissions returned by this API are not the standard file
    -- mode permission values, such as 100644, but rather extrapolated values.
    -- See the supported return values.
    fileMode :: FileModeTypeEnum,
    -- | The size of the contents of the file, in bytes.
    fileSize :: Prelude.Integer,
    -- | The base-64 encoded binary data object that represents the content of
    -- the file.
    fileContent :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getFileResponse_httpStatus' - The response's http status code.
--
-- 'commitId', 'getFileResponse_commitId' - The full commit ID of the commit that contains the content returned by
-- GetFile.
--
-- 'blobId', 'getFileResponse_blobId' - The blob ID of the object that represents the file content.
--
-- 'filePath', 'getFileResponse_filePath' - The fully qualified path to the specified file. Returns the name and
-- extension of the file.
--
-- 'fileMode', 'getFileResponse_fileMode' - The extrapolated file mode permissions of the blob. Valid values include
-- strings such as EXECUTABLE and not numeric values.
--
-- The file mode permissions returned by this API are not the standard file
-- mode permission values, such as 100644, but rather extrapolated values.
-- See the supported return values.
--
-- 'fileSize', 'getFileResponse_fileSize' - The size of the contents of the file, in bytes.
--
-- 'fileContent', 'getFileResponse_fileContent' - The base-64 encoded binary data object that represents the content of
-- the file.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newGetFileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'commitId'
  Prelude.Text ->
  -- | 'blobId'
  Prelude.Text ->
  -- | 'filePath'
  Prelude.Text ->
  -- | 'fileMode'
  FileModeTypeEnum ->
  -- | 'fileSize'
  Prelude.Integer ->
  -- | 'fileContent'
  Prelude.ByteString ->
  GetFileResponse
newGetFileResponse
  pHttpStatus_
  pCommitId_
  pBlobId_
  pFilePath_
  pFileMode_
  pFileSize_
  pFileContent_ =
    GetFileResponse'
      { httpStatus = pHttpStatus_,
        commitId = pCommitId_,
        blobId = pBlobId_,
        filePath = pFilePath_,
        fileMode = pFileMode_,
        fileSize = pFileSize_,
        fileContent = Data._Base64 Lens.# pFileContent_
      }

-- | The response's http status code.
getFileResponse_httpStatus :: Lens.Lens' GetFileResponse Prelude.Int
getFileResponse_httpStatus = Lens.lens (\GetFileResponse' {httpStatus} -> httpStatus) (\s@GetFileResponse' {} a -> s {httpStatus = a} :: GetFileResponse)

-- | The full commit ID of the commit that contains the content returned by
-- GetFile.
getFileResponse_commitId :: Lens.Lens' GetFileResponse Prelude.Text
getFileResponse_commitId = Lens.lens (\GetFileResponse' {commitId} -> commitId) (\s@GetFileResponse' {} a -> s {commitId = a} :: GetFileResponse)

-- | The blob ID of the object that represents the file content.
getFileResponse_blobId :: Lens.Lens' GetFileResponse Prelude.Text
getFileResponse_blobId = Lens.lens (\GetFileResponse' {blobId} -> blobId) (\s@GetFileResponse' {} a -> s {blobId = a} :: GetFileResponse)

-- | The fully qualified path to the specified file. Returns the name and
-- extension of the file.
getFileResponse_filePath :: Lens.Lens' GetFileResponse Prelude.Text
getFileResponse_filePath = Lens.lens (\GetFileResponse' {filePath} -> filePath) (\s@GetFileResponse' {} a -> s {filePath = a} :: GetFileResponse)

-- | The extrapolated file mode permissions of the blob. Valid values include
-- strings such as EXECUTABLE and not numeric values.
--
-- The file mode permissions returned by this API are not the standard file
-- mode permission values, such as 100644, but rather extrapolated values.
-- See the supported return values.
getFileResponse_fileMode :: Lens.Lens' GetFileResponse FileModeTypeEnum
getFileResponse_fileMode = Lens.lens (\GetFileResponse' {fileMode} -> fileMode) (\s@GetFileResponse' {} a -> s {fileMode = a} :: GetFileResponse)

-- | The size of the contents of the file, in bytes.
getFileResponse_fileSize :: Lens.Lens' GetFileResponse Prelude.Integer
getFileResponse_fileSize = Lens.lens (\GetFileResponse' {fileSize} -> fileSize) (\s@GetFileResponse' {} a -> s {fileSize = a} :: GetFileResponse)

-- | The base-64 encoded binary data object that represents the content of
-- the file.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getFileResponse_fileContent :: Lens.Lens' GetFileResponse Prelude.ByteString
getFileResponse_fileContent = Lens.lens (\GetFileResponse' {fileContent} -> fileContent) (\s@GetFileResponse' {} a -> s {fileContent = a} :: GetFileResponse) Prelude.. Data._Base64

instance Prelude.NFData GetFileResponse where
  rnf GetFileResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf commitId
      `Prelude.seq` Prelude.rnf blobId
      `Prelude.seq` Prelude.rnf filePath
      `Prelude.seq` Prelude.rnf fileMode
      `Prelude.seq` Prelude.rnf fileSize
      `Prelude.seq` Prelude.rnf fileContent
