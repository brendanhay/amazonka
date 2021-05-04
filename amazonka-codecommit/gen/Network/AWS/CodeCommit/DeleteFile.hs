{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFile' smart constructor.
data DeleteFile = DeleteFile'
  { -- | The commit message you want to include as part of deleting the file.
    -- Commit messages are limited to 256 KB. If no message is specified, a
    -- default message is used.
    commitMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the author of the commit that deletes the file. If no name
    -- is specified, the user\'s ARN is used as the author name and committer
    -- name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The email address for the commit that deletes the file. If no email
    -- address is specified, the email address is left blank.
    email :: Prelude.Maybe Prelude.Text,
    -- | If a file is the only object in the folder or directory, specifies
    -- whether to delete the folder or directory that contains the file. By
    -- default, empty folders are deleted. This includes empty folders that are
    -- part of the directory structure. For example, if the path to a file is
    -- dir1\/dir2\/dir3\/dir4, and dir2 and dir3 are empty, deleting the last
    -- file in dir4 also deletes the empty folders dir4, dir3, and dir2.
    keepEmptyFolders :: Prelude.Maybe Prelude.Bool,
    -- | The name of the repository that contains the file to delete.
    repositoryName :: Prelude.Text,
    -- | The name of the branch where the commit that deletes the file is made.
    branchName :: Prelude.Text,
    -- | The fully qualified path to the file that to be deleted, including the
    -- full name and extension of that file. For example, \/examples\/file.md
    -- is a fully qualified path to a file named file.md in a folder named
    -- examples.
    filePath :: Prelude.Text,
    -- | The ID of the commit that is the tip of the branch where you want to
    -- create the commit that deletes the file. This must be the HEAD commit
    -- for the branch. The commit that deletes the file is created from this
    -- commit ID.
    parentCommitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  -- | 'filePath'
  Prelude.Text ->
  -- | 'parentCommitId'
  Prelude.Text ->
  DeleteFile
newDeleteFile
  pRepositoryName_
  pBranchName_
  pFilePath_
  pParentCommitId_ =
    DeleteFile'
      { commitMessage = Prelude.Nothing,
        name = Prelude.Nothing,
        email = Prelude.Nothing,
        keepEmptyFolders = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        branchName = pBranchName_,
        filePath = pFilePath_,
        parentCommitId = pParentCommitId_
      }

-- | The commit message you want to include as part of deleting the file.
-- Commit messages are limited to 256 KB. If no message is specified, a
-- default message is used.
deleteFile_commitMessage :: Lens.Lens' DeleteFile (Prelude.Maybe Prelude.Text)
deleteFile_commitMessage = Lens.lens (\DeleteFile' {commitMessage} -> commitMessage) (\s@DeleteFile' {} a -> s {commitMessage = a} :: DeleteFile)

-- | The name of the author of the commit that deletes the file. If no name
-- is specified, the user\'s ARN is used as the author name and committer
-- name.
deleteFile_name :: Lens.Lens' DeleteFile (Prelude.Maybe Prelude.Text)
deleteFile_name = Lens.lens (\DeleteFile' {name} -> name) (\s@DeleteFile' {} a -> s {name = a} :: DeleteFile)

-- | The email address for the commit that deletes the file. If no email
-- address is specified, the email address is left blank.
deleteFile_email :: Lens.Lens' DeleteFile (Prelude.Maybe Prelude.Text)
deleteFile_email = Lens.lens (\DeleteFile' {email} -> email) (\s@DeleteFile' {} a -> s {email = a} :: DeleteFile)

-- | If a file is the only object in the folder or directory, specifies
-- whether to delete the folder or directory that contains the file. By
-- default, empty folders are deleted. This includes empty folders that are
-- part of the directory structure. For example, if the path to a file is
-- dir1\/dir2\/dir3\/dir4, and dir2 and dir3 are empty, deleting the last
-- file in dir4 also deletes the empty folders dir4, dir3, and dir2.
deleteFile_keepEmptyFolders :: Lens.Lens' DeleteFile (Prelude.Maybe Prelude.Bool)
deleteFile_keepEmptyFolders = Lens.lens (\DeleteFile' {keepEmptyFolders} -> keepEmptyFolders) (\s@DeleteFile' {} a -> s {keepEmptyFolders = a} :: DeleteFile)

-- | The name of the repository that contains the file to delete.
deleteFile_repositoryName :: Lens.Lens' DeleteFile Prelude.Text
deleteFile_repositoryName = Lens.lens (\DeleteFile' {repositoryName} -> repositoryName) (\s@DeleteFile' {} a -> s {repositoryName = a} :: DeleteFile)

-- | The name of the branch where the commit that deletes the file is made.
deleteFile_branchName :: Lens.Lens' DeleteFile Prelude.Text
deleteFile_branchName = Lens.lens (\DeleteFile' {branchName} -> branchName) (\s@DeleteFile' {} a -> s {branchName = a} :: DeleteFile)

-- | The fully qualified path to the file that to be deleted, including the
-- full name and extension of that file. For example, \/examples\/file.md
-- is a fully qualified path to a file named file.md in a folder named
-- examples.
deleteFile_filePath :: Lens.Lens' DeleteFile Prelude.Text
deleteFile_filePath = Lens.lens (\DeleteFile' {filePath} -> filePath) (\s@DeleteFile' {} a -> s {filePath = a} :: DeleteFile)

-- | The ID of the commit that is the tip of the branch where you want to
-- create the commit that deletes the file. This must be the HEAD commit
-- for the branch. The commit that deletes the file is created from this
-- commit ID.
deleteFile_parentCommitId :: Lens.Lens' DeleteFile Prelude.Text
deleteFile_parentCommitId = Lens.lens (\DeleteFile' {parentCommitId} -> parentCommitId) (\s@DeleteFile' {} a -> s {parentCommitId = a} :: DeleteFile)

instance Prelude.AWSRequest DeleteFile where
  type Rs DeleteFile = DeleteFileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "commitId")
            Prelude.<*> (x Prelude..:> "blobId")
            Prelude.<*> (x Prelude..:> "treeId")
            Prelude.<*> (x Prelude..:> "filePath")
      )

instance Prelude.Hashable DeleteFile

instance Prelude.NFData DeleteFile

instance Prelude.ToHeaders DeleteFile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.DeleteFile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteFile where
  toJSON DeleteFile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("commitMessage" Prelude..=)
              Prelude.<$> commitMessage,
            ("name" Prelude..=) Prelude.<$> name,
            ("email" Prelude..=) Prelude.<$> email,
            ("keepEmptyFolders" Prelude..=)
              Prelude.<$> keepEmptyFolders,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName),
            Prelude.Just ("branchName" Prelude..= branchName),
            Prelude.Just ("filePath" Prelude..= filePath),
            Prelude.Just
              ("parentCommitId" Prelude..= parentCommitId)
          ]
      )

instance Prelude.ToPath DeleteFile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteFile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFileResponse' smart constructor.
data DeleteFileResponse = DeleteFileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full commit ID of the commit that contains the change that deletes
    -- the file.
    commitId :: Prelude.Text,
    -- | The blob ID removed from the tree as part of deleting the file.
    blobId :: Prelude.Text,
    -- | The full SHA-1 pointer of the tree information for the commit that
    -- contains the delete file change.
    treeId :: Prelude.Text,
    -- | The fully qualified path to the file to be deleted, including the full
    -- name and extension of that file.
    filePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'commitId'
  Prelude.Text ->
  -- | 'blobId'
  Prelude.Text ->
  -- | 'treeId'
  Prelude.Text ->
  -- | 'filePath'
  Prelude.Text ->
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
deleteFileResponse_httpStatus :: Lens.Lens' DeleteFileResponse Prelude.Int
deleteFileResponse_httpStatus = Lens.lens (\DeleteFileResponse' {httpStatus} -> httpStatus) (\s@DeleteFileResponse' {} a -> s {httpStatus = a} :: DeleteFileResponse)

-- | The full commit ID of the commit that contains the change that deletes
-- the file.
deleteFileResponse_commitId :: Lens.Lens' DeleteFileResponse Prelude.Text
deleteFileResponse_commitId = Lens.lens (\DeleteFileResponse' {commitId} -> commitId) (\s@DeleteFileResponse' {} a -> s {commitId = a} :: DeleteFileResponse)

-- | The blob ID removed from the tree as part of deleting the file.
deleteFileResponse_blobId :: Lens.Lens' DeleteFileResponse Prelude.Text
deleteFileResponse_blobId = Lens.lens (\DeleteFileResponse' {blobId} -> blobId) (\s@DeleteFileResponse' {} a -> s {blobId = a} :: DeleteFileResponse)

-- | The full SHA-1 pointer of the tree information for the commit that
-- contains the delete file change.
deleteFileResponse_treeId :: Lens.Lens' DeleteFileResponse Prelude.Text
deleteFileResponse_treeId = Lens.lens (\DeleteFileResponse' {treeId} -> treeId) (\s@DeleteFileResponse' {} a -> s {treeId = a} :: DeleteFileResponse)

-- | The fully qualified path to the file to be deleted, including the full
-- name and extension of that file.
deleteFileResponse_filePath :: Lens.Lens' DeleteFileResponse Prelude.Text
deleteFileResponse_filePath = Lens.lens (\DeleteFileResponse' {filePath} -> filePath) (\s@DeleteFileResponse' {} a -> s {filePath = a} :: DeleteFileResponse)

instance Prelude.NFData DeleteFileResponse
