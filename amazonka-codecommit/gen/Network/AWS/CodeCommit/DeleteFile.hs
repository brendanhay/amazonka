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
-- Module      : Network.AWS.CodeCommit.DeleteFile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified file from a specified branch. A commit is created on
-- the branch that contains the revision. The file still exists in the
-- commits earlier to the commit that contains the deletion.
module Network.AWS.CodeCommit.DeleteFile
  ( -- * Creating a Request
    DeleteFile (..),
    newDeleteFile,

    -- * Request Lenses
    deleteFile_commitMessage,
    deleteFile_name,
    deleteFile_email,
    deleteFile_keepEmptyFolders,
    deleteFile_repositoryName,
    deleteFile_branchName,
    deleteFile_filePath,
    deleteFile_parentCommitId,

    -- * Destructuring the Response
    DeleteFileResponse (..),
    newDeleteFileResponse,

    -- * Response Lenses
    deleteFileResponse_httpStatus,
    deleteFileResponse_commitId,
    deleteFileResponse_blobId,
    deleteFileResponse_treeId,
    deleteFileResponse_filePath,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFile' smart constructor.
data DeleteFile = DeleteFile'
  { -- | The commit message you want to include as part of deleting the file.
    -- Commit messages are limited to 256 KB. If no message is specified, a
    -- default message is used.
    commitMessage :: Core.Maybe Core.Text,
    -- | The name of the author of the commit that deletes the file. If no name
    -- is specified, the user\'s ARN is used as the author name and committer
    -- name.
    name :: Core.Maybe Core.Text,
    -- | The email address for the commit that deletes the file. If no email
    -- address is specified, the email address is left blank.
    email :: Core.Maybe Core.Text,
    -- | If a file is the only object in the folder or directory, specifies
    -- whether to delete the folder or directory that contains the file. By
    -- default, empty folders are deleted. This includes empty folders that are
    -- part of the directory structure. For example, if the path to a file is
    -- dir1\/dir2\/dir3\/dir4, and dir2 and dir3 are empty, deleting the last
    -- file in dir4 also deletes the empty folders dir4, dir3, and dir2.
    keepEmptyFolders :: Core.Maybe Core.Bool,
    -- | The name of the repository that contains the file to delete.
    repositoryName :: Core.Text,
    -- | The name of the branch where the commit that deletes the file is made.
    branchName :: Core.Text,
    -- | The fully qualified path to the file that to be deleted, including the
    -- full name and extension of that file. For example, \/examples\/file.md
    -- is a fully qualified path to a file named file.md in a folder named
    -- examples.
    filePath :: Core.Text,
    -- | The ID of the commit that is the tip of the branch where you want to
    -- create the commit that deletes the file. This must be the HEAD commit
    -- for the branch. The commit that deletes the file is created from this
    -- commit ID.
    parentCommitId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitMessage', 'deleteFile_commitMessage' - The commit message you want to include as part of deleting the file.
-- Commit messages are limited to 256 KB. If no message is specified, a
-- default message is used.
--
-- 'name', 'deleteFile_name' - The name of the author of the commit that deletes the file. If no name
-- is specified, the user\'s ARN is used as the author name and committer
-- name.
--
-- 'email', 'deleteFile_email' - The email address for the commit that deletes the file. If no email
-- address is specified, the email address is left blank.
--
-- 'keepEmptyFolders', 'deleteFile_keepEmptyFolders' - If a file is the only object in the folder or directory, specifies
-- whether to delete the folder or directory that contains the file. By
-- default, empty folders are deleted. This includes empty folders that are
-- part of the directory structure. For example, if the path to a file is
-- dir1\/dir2\/dir3\/dir4, and dir2 and dir3 are empty, deleting the last
-- file in dir4 also deletes the empty folders dir4, dir3, and dir2.
--
-- 'repositoryName', 'deleteFile_repositoryName' - The name of the repository that contains the file to delete.
--
-- 'branchName', 'deleteFile_branchName' - The name of the branch where the commit that deletes the file is made.
--
-- 'filePath', 'deleteFile_filePath' - The fully qualified path to the file that to be deleted, including the
-- full name and extension of that file. For example, \/examples\/file.md
-- is a fully qualified path to a file named file.md in a folder named
-- examples.
--
-- 'parentCommitId', 'deleteFile_parentCommitId' - The ID of the commit that is the tip of the branch where you want to
-- create the commit that deletes the file. This must be the HEAD commit
-- for the branch. The commit that deletes the file is created from this
-- commit ID.
newDeleteFile ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'branchName'
  Core.Text ->
  -- | 'filePath'
  Core.Text ->
  -- | 'parentCommitId'
  Core.Text ->
  DeleteFile
newDeleteFile
  pRepositoryName_
  pBranchName_
  pFilePath_
  pParentCommitId_ =
    DeleteFile'
      { commitMessage = Core.Nothing,
        name = Core.Nothing,
        email = Core.Nothing,
        keepEmptyFolders = Core.Nothing,
        repositoryName = pRepositoryName_,
        branchName = pBranchName_,
        filePath = pFilePath_,
        parentCommitId = pParentCommitId_
      }

-- | The commit message you want to include as part of deleting the file.
-- Commit messages are limited to 256 KB. If no message is specified, a
-- default message is used.
deleteFile_commitMessage :: Lens.Lens' DeleteFile (Core.Maybe Core.Text)
deleteFile_commitMessage = Lens.lens (\DeleteFile' {commitMessage} -> commitMessage) (\s@DeleteFile' {} a -> s {commitMessage = a} :: DeleteFile)

-- | The name of the author of the commit that deletes the file. If no name
-- is specified, the user\'s ARN is used as the author name and committer
-- name.
deleteFile_name :: Lens.Lens' DeleteFile (Core.Maybe Core.Text)
deleteFile_name = Lens.lens (\DeleteFile' {name} -> name) (\s@DeleteFile' {} a -> s {name = a} :: DeleteFile)

-- | The email address for the commit that deletes the file. If no email
-- address is specified, the email address is left blank.
deleteFile_email :: Lens.Lens' DeleteFile (Core.Maybe Core.Text)
deleteFile_email = Lens.lens (\DeleteFile' {email} -> email) (\s@DeleteFile' {} a -> s {email = a} :: DeleteFile)

-- | If a file is the only object in the folder or directory, specifies
-- whether to delete the folder or directory that contains the file. By
-- default, empty folders are deleted. This includes empty folders that are
-- part of the directory structure. For example, if the path to a file is
-- dir1\/dir2\/dir3\/dir4, and dir2 and dir3 are empty, deleting the last
-- file in dir4 also deletes the empty folders dir4, dir3, and dir2.
deleteFile_keepEmptyFolders :: Lens.Lens' DeleteFile (Core.Maybe Core.Bool)
deleteFile_keepEmptyFolders = Lens.lens (\DeleteFile' {keepEmptyFolders} -> keepEmptyFolders) (\s@DeleteFile' {} a -> s {keepEmptyFolders = a} :: DeleteFile)

-- | The name of the repository that contains the file to delete.
deleteFile_repositoryName :: Lens.Lens' DeleteFile Core.Text
deleteFile_repositoryName = Lens.lens (\DeleteFile' {repositoryName} -> repositoryName) (\s@DeleteFile' {} a -> s {repositoryName = a} :: DeleteFile)

-- | The name of the branch where the commit that deletes the file is made.
deleteFile_branchName :: Lens.Lens' DeleteFile Core.Text
deleteFile_branchName = Lens.lens (\DeleteFile' {branchName} -> branchName) (\s@DeleteFile' {} a -> s {branchName = a} :: DeleteFile)

-- | The fully qualified path to the file that to be deleted, including the
-- full name and extension of that file. For example, \/examples\/file.md
-- is a fully qualified path to a file named file.md in a folder named
-- examples.
deleteFile_filePath :: Lens.Lens' DeleteFile Core.Text
deleteFile_filePath = Lens.lens (\DeleteFile' {filePath} -> filePath) (\s@DeleteFile' {} a -> s {filePath = a} :: DeleteFile)

-- | The ID of the commit that is the tip of the branch where you want to
-- create the commit that deletes the file. This must be the HEAD commit
-- for the branch. The commit that deletes the file is created from this
-- commit ID.
deleteFile_parentCommitId :: Lens.Lens' DeleteFile Core.Text
deleteFile_parentCommitId = Lens.lens (\DeleteFile' {parentCommitId} -> parentCommitId) (\s@DeleteFile' {} a -> s {parentCommitId = a} :: DeleteFile)

instance Core.AWSRequest DeleteFile where
  type AWSResponse DeleteFile = DeleteFileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "commitId")
            Core.<*> (x Core..:> "blobId")
            Core.<*> (x Core..:> "treeId")
            Core.<*> (x Core..:> "filePath")
      )

instance Core.Hashable DeleteFile

instance Core.NFData DeleteFile

instance Core.ToHeaders DeleteFile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.DeleteFile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteFile where
  toJSON DeleteFile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("commitMessage" Core..=) Core.<$> commitMessage,
            ("name" Core..=) Core.<$> name,
            ("email" Core..=) Core.<$> email,
            ("keepEmptyFolders" Core..=)
              Core.<$> keepEmptyFolders,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("branchName" Core..= branchName),
            Core.Just ("filePath" Core..= filePath),
            Core.Just ("parentCommitId" Core..= parentCommitId)
          ]
      )

instance Core.ToPath DeleteFile where
  toPath = Core.const "/"

instance Core.ToQuery DeleteFile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteFileResponse' smart constructor.
data DeleteFileResponse = DeleteFileResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The full commit ID of the commit that contains the change that deletes
    -- the file.
    commitId :: Core.Text,
    -- | The blob ID removed from the tree as part of deleting the file.
    blobId :: Core.Text,
    -- | The full SHA-1 pointer of the tree information for the commit that
    -- contains the delete file change.
    treeId :: Core.Text,
    -- | The fully qualified path to the file to be deleted, including the full
    -- name and extension of that file.
    filePath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFileResponse_httpStatus' - The response's http status code.
--
-- 'commitId', 'deleteFileResponse_commitId' - The full commit ID of the commit that contains the change that deletes
-- the file.
--
-- 'blobId', 'deleteFileResponse_blobId' - The blob ID removed from the tree as part of deleting the file.
--
-- 'treeId', 'deleteFileResponse_treeId' - The full SHA-1 pointer of the tree information for the commit that
-- contains the delete file change.
--
-- 'filePath', 'deleteFileResponse_filePath' - The fully qualified path to the file to be deleted, including the full
-- name and extension of that file.
newDeleteFileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'commitId'
  Core.Text ->
  -- | 'blobId'
  Core.Text ->
  -- | 'treeId'
  Core.Text ->
  -- | 'filePath'
  Core.Text ->
  DeleteFileResponse
newDeleteFileResponse
  pHttpStatus_
  pCommitId_
  pBlobId_
  pTreeId_
  pFilePath_ =
    DeleteFileResponse'
      { httpStatus = pHttpStatus_,
        commitId = pCommitId_,
        blobId = pBlobId_,
        treeId = pTreeId_,
        filePath = pFilePath_
      }

-- | The response's http status code.
deleteFileResponse_httpStatus :: Lens.Lens' DeleteFileResponse Core.Int
deleteFileResponse_httpStatus = Lens.lens (\DeleteFileResponse' {httpStatus} -> httpStatus) (\s@DeleteFileResponse' {} a -> s {httpStatus = a} :: DeleteFileResponse)

-- | The full commit ID of the commit that contains the change that deletes
-- the file.
deleteFileResponse_commitId :: Lens.Lens' DeleteFileResponse Core.Text
deleteFileResponse_commitId = Lens.lens (\DeleteFileResponse' {commitId} -> commitId) (\s@DeleteFileResponse' {} a -> s {commitId = a} :: DeleteFileResponse)

-- | The blob ID removed from the tree as part of deleting the file.
deleteFileResponse_blobId :: Lens.Lens' DeleteFileResponse Core.Text
deleteFileResponse_blobId = Lens.lens (\DeleteFileResponse' {blobId} -> blobId) (\s@DeleteFileResponse' {} a -> s {blobId = a} :: DeleteFileResponse)

-- | The full SHA-1 pointer of the tree information for the commit that
-- contains the delete file change.
deleteFileResponse_treeId :: Lens.Lens' DeleteFileResponse Core.Text
deleteFileResponse_treeId = Lens.lens (\DeleteFileResponse' {treeId} -> treeId) (\s@DeleteFileResponse' {} a -> s {treeId = a} :: DeleteFileResponse)

-- | The fully qualified path to the file to be deleted, including the full
-- name and extension of that file.
deleteFileResponse_filePath :: Lens.Lens' DeleteFileResponse Core.Text
deleteFileResponse_filePath = Lens.lens (\DeleteFileResponse' {filePath} -> filePath) (\s@DeleteFileResponse' {} a -> s {filePath = a} :: DeleteFileResponse)

instance Core.NFData DeleteFileResponse
