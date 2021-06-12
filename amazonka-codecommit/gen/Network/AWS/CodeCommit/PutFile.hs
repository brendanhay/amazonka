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
-- Module      : Network.AWS.CodeCommit.PutFile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a file in a branch in an AWS CodeCommit repository, and
-- generates a commit for the addition in the specified branch.
module Network.AWS.CodeCommit.PutFile
  ( -- * Creating a Request
    PutFile (..),
    newPutFile,

    -- * Request Lenses
    putFile_parentCommitId,
    putFile_commitMessage,
    putFile_name,
    putFile_email,
    putFile_fileMode,
    putFile_repositoryName,
    putFile_branchName,
    putFile_fileContent,
    putFile_filePath,

    -- * Destructuring the Response
    PutFileResponse (..),
    newPutFileResponse,

    -- * Response Lenses
    putFileResponse_httpStatus,
    putFileResponse_commitId,
    putFileResponse_blobId,
    putFileResponse_treeId,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutFile' smart constructor.
data PutFile = PutFile'
  { -- | The full commit ID of the head commit in the branch where you want to
    -- add or update the file. If this is an empty repository, no commit ID is
    -- required. If this is not an empty repository, a commit ID is required.
    --
    -- The commit ID must match the ID of the head commit at the time of the
    -- operation. Otherwise, an error occurs, and the file is not added or
    -- updated.
    parentCommitId :: Core.Maybe Core.Text,
    -- | A message about why this file was added or updated. Although it is
    -- optional, a message makes the commit history for your repository more
    -- useful.
    commitMessage :: Core.Maybe Core.Text,
    -- | The name of the person adding or updating the file. Although it is
    -- optional, a name makes the commit history for your repository more
    -- useful.
    name :: Core.Maybe Core.Text,
    -- | An email address for the person adding or updating the file.
    email :: Core.Maybe Core.Text,
    -- | The file mode permissions of the blob. Valid file mode permissions are
    -- listed here.
    fileMode :: Core.Maybe FileModeTypeEnum,
    -- | The name of the repository where you want to add or update the file.
    repositoryName :: Core.Text,
    -- | The name of the branch where you want to add or update the file. If this
    -- is an empty repository, this branch is created.
    branchName :: Core.Text,
    -- | The content of the file, in binary object format.
    fileContent :: Core.Base64,
    -- | The name of the file you want to add or update, including the relative
    -- path to the file in the repository.
    --
    -- If the path does not currently exist in the repository, the path is
    -- created as part of adding the file.
    filePath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentCommitId', 'putFile_parentCommitId' - The full commit ID of the head commit in the branch where you want to
-- add or update the file. If this is an empty repository, no commit ID is
-- required. If this is not an empty repository, a commit ID is required.
--
-- The commit ID must match the ID of the head commit at the time of the
-- operation. Otherwise, an error occurs, and the file is not added or
-- updated.
--
-- 'commitMessage', 'putFile_commitMessage' - A message about why this file was added or updated. Although it is
-- optional, a message makes the commit history for your repository more
-- useful.
--
-- 'name', 'putFile_name' - The name of the person adding or updating the file. Although it is
-- optional, a name makes the commit history for your repository more
-- useful.
--
-- 'email', 'putFile_email' - An email address for the person adding or updating the file.
--
-- 'fileMode', 'putFile_fileMode' - The file mode permissions of the blob. Valid file mode permissions are
-- listed here.
--
-- 'repositoryName', 'putFile_repositoryName' - The name of the repository where you want to add or update the file.
--
-- 'branchName', 'putFile_branchName' - The name of the branch where you want to add or update the file. If this
-- is an empty repository, this branch is created.
--
-- 'fileContent', 'putFile_fileContent' - The content of the file, in binary object format.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'filePath', 'putFile_filePath' - The name of the file you want to add or update, including the relative
-- path to the file in the repository.
--
-- If the path does not currently exist in the repository, the path is
-- created as part of adding the file.
newPutFile ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'branchName'
  Core.Text ->
  -- | 'fileContent'
  Core.ByteString ->
  -- | 'filePath'
  Core.Text ->
  PutFile
newPutFile
  pRepositoryName_
  pBranchName_
  pFileContent_
  pFilePath_ =
    PutFile'
      { parentCommitId = Core.Nothing,
        commitMessage = Core.Nothing,
        name = Core.Nothing,
        email = Core.Nothing,
        fileMode = Core.Nothing,
        repositoryName = pRepositoryName_,
        branchName = pBranchName_,
        fileContent = Core._Base64 Lens.# pFileContent_,
        filePath = pFilePath_
      }

-- | The full commit ID of the head commit in the branch where you want to
-- add or update the file. If this is an empty repository, no commit ID is
-- required. If this is not an empty repository, a commit ID is required.
--
-- The commit ID must match the ID of the head commit at the time of the
-- operation. Otherwise, an error occurs, and the file is not added or
-- updated.
putFile_parentCommitId :: Lens.Lens' PutFile (Core.Maybe Core.Text)
putFile_parentCommitId = Lens.lens (\PutFile' {parentCommitId} -> parentCommitId) (\s@PutFile' {} a -> s {parentCommitId = a} :: PutFile)

-- | A message about why this file was added or updated. Although it is
-- optional, a message makes the commit history for your repository more
-- useful.
putFile_commitMessage :: Lens.Lens' PutFile (Core.Maybe Core.Text)
putFile_commitMessage = Lens.lens (\PutFile' {commitMessage} -> commitMessage) (\s@PutFile' {} a -> s {commitMessage = a} :: PutFile)

-- | The name of the person adding or updating the file. Although it is
-- optional, a name makes the commit history for your repository more
-- useful.
putFile_name :: Lens.Lens' PutFile (Core.Maybe Core.Text)
putFile_name = Lens.lens (\PutFile' {name} -> name) (\s@PutFile' {} a -> s {name = a} :: PutFile)

-- | An email address for the person adding or updating the file.
putFile_email :: Lens.Lens' PutFile (Core.Maybe Core.Text)
putFile_email = Lens.lens (\PutFile' {email} -> email) (\s@PutFile' {} a -> s {email = a} :: PutFile)

-- | The file mode permissions of the blob. Valid file mode permissions are
-- listed here.
putFile_fileMode :: Lens.Lens' PutFile (Core.Maybe FileModeTypeEnum)
putFile_fileMode = Lens.lens (\PutFile' {fileMode} -> fileMode) (\s@PutFile' {} a -> s {fileMode = a} :: PutFile)

-- | The name of the repository where you want to add or update the file.
putFile_repositoryName :: Lens.Lens' PutFile Core.Text
putFile_repositoryName = Lens.lens (\PutFile' {repositoryName} -> repositoryName) (\s@PutFile' {} a -> s {repositoryName = a} :: PutFile)

-- | The name of the branch where you want to add or update the file. If this
-- is an empty repository, this branch is created.
putFile_branchName :: Lens.Lens' PutFile Core.Text
putFile_branchName = Lens.lens (\PutFile' {branchName} -> branchName) (\s@PutFile' {} a -> s {branchName = a} :: PutFile)

-- | The content of the file, in binary object format.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
putFile_fileContent :: Lens.Lens' PutFile Core.ByteString
putFile_fileContent = Lens.lens (\PutFile' {fileContent} -> fileContent) (\s@PutFile' {} a -> s {fileContent = a} :: PutFile) Core.. Core._Base64

-- | The name of the file you want to add or update, including the relative
-- path to the file in the repository.
--
-- If the path does not currently exist in the repository, the path is
-- created as part of adding the file.
putFile_filePath :: Lens.Lens' PutFile Core.Text
putFile_filePath = Lens.lens (\PutFile' {filePath} -> filePath) (\s@PutFile' {} a -> s {filePath = a} :: PutFile)

instance Core.AWSRequest PutFile where
  type AWSResponse PutFile = PutFileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutFileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "commitId")
            Core.<*> (x Core..:> "blobId")
            Core.<*> (x Core..:> "treeId")
      )

instance Core.Hashable PutFile

instance Core.NFData PutFile

instance Core.ToHeaders PutFile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("CodeCommit_20150413.PutFile" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutFile where
  toJSON PutFile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("parentCommitId" Core..=) Core.<$> parentCommitId,
            ("commitMessage" Core..=) Core.<$> commitMessage,
            ("name" Core..=) Core.<$> name,
            ("email" Core..=) Core.<$> email,
            ("fileMode" Core..=) Core.<$> fileMode,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("branchName" Core..= branchName),
            Core.Just ("fileContent" Core..= fileContent),
            Core.Just ("filePath" Core..= filePath)
          ]
      )

instance Core.ToPath PutFile where
  toPath = Core.const "/"

instance Core.ToQuery PutFile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutFileResponse' smart constructor.
data PutFileResponse = PutFileResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The full SHA ID of the commit that contains this file change.
    commitId :: Core.Text,
    -- | The ID of the blob, which is its SHA-1 pointer.
    blobId :: Core.Text,
    -- | The full SHA-1 pointer of the tree information for the commit that
    -- contains this file change.
    treeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutFileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putFileResponse_httpStatus' - The response's http status code.
--
-- 'commitId', 'putFileResponse_commitId' - The full SHA ID of the commit that contains this file change.
--
-- 'blobId', 'putFileResponse_blobId' - The ID of the blob, which is its SHA-1 pointer.
--
-- 'treeId', 'putFileResponse_treeId' - The full SHA-1 pointer of the tree information for the commit that
-- contains this file change.
newPutFileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'commitId'
  Core.Text ->
  -- | 'blobId'
  Core.Text ->
  -- | 'treeId'
  Core.Text ->
  PutFileResponse
newPutFileResponse
  pHttpStatus_
  pCommitId_
  pBlobId_
  pTreeId_ =
    PutFileResponse'
      { httpStatus = pHttpStatus_,
        commitId = pCommitId_,
        blobId = pBlobId_,
        treeId = pTreeId_
      }

-- | The response's http status code.
putFileResponse_httpStatus :: Lens.Lens' PutFileResponse Core.Int
putFileResponse_httpStatus = Lens.lens (\PutFileResponse' {httpStatus} -> httpStatus) (\s@PutFileResponse' {} a -> s {httpStatus = a} :: PutFileResponse)

-- | The full SHA ID of the commit that contains this file change.
putFileResponse_commitId :: Lens.Lens' PutFileResponse Core.Text
putFileResponse_commitId = Lens.lens (\PutFileResponse' {commitId} -> commitId) (\s@PutFileResponse' {} a -> s {commitId = a} :: PutFileResponse)

-- | The ID of the blob, which is its SHA-1 pointer.
putFileResponse_blobId :: Lens.Lens' PutFileResponse Core.Text
putFileResponse_blobId = Lens.lens (\PutFileResponse' {blobId} -> blobId) (\s@PutFileResponse' {} a -> s {blobId = a} :: PutFileResponse)

-- | The full SHA-1 pointer of the tree information for the commit that
-- contains this file change.
putFileResponse_treeId :: Lens.Lens' PutFileResponse Core.Text
putFileResponse_treeId = Lens.lens (\PutFileResponse' {treeId} -> treeId) (\s@PutFileResponse' {} a -> s {treeId = a} :: PutFileResponse)

instance Core.NFData PutFileResponse
