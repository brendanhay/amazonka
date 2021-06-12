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
-- Module      : Network.AWS.CodeCommit.CreateCommit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a commit for a repository on the tip of a specified branch.
module Network.AWS.CodeCommit.CreateCommit
  ( -- * Creating a Request
    CreateCommit (..),
    newCreateCommit,

    -- * Request Lenses
    createCommit_deleteFiles,
    createCommit_setFileModes,
    createCommit_parentCommitId,
    createCommit_commitMessage,
    createCommit_authorName,
    createCommit_putFiles,
    createCommit_email,
    createCommit_keepEmptyFolders,
    createCommit_repositoryName,
    createCommit_branchName,

    -- * Destructuring the Response
    CreateCommitResponse (..),
    newCreateCommitResponse,

    -- * Response Lenses
    createCommitResponse_commitId,
    createCommitResponse_treeId,
    createCommitResponse_filesAdded,
    createCommitResponse_filesUpdated,
    createCommitResponse_filesDeleted,
    createCommitResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCommit' smart constructor.
data CreateCommit = CreateCommit'
  { -- | The files to delete in this commit. These files still exist in earlier
    -- commits.
    deleteFiles :: Core.Maybe [DeleteFileEntry],
    -- | The file modes to update for files in this commit.
    setFileModes :: Core.Maybe [SetFileModeEntry],
    -- | The ID of the commit that is the parent of the commit you create. Not
    -- required if this is an empty repository.
    parentCommitId :: Core.Maybe Core.Text,
    -- | The commit message you want to include in the commit. Commit messages
    -- are limited to 256 KB. If no message is specified, a default message is
    -- used.
    commitMessage :: Core.Maybe Core.Text,
    -- | The name of the author who created the commit. This information is used
    -- as both the author and committer for the commit.
    authorName :: Core.Maybe Core.Text,
    -- | The files to add or update in this commit.
    putFiles :: Core.Maybe [PutFileEntry],
    -- | The email address of the person who created the commit.
    email :: Core.Maybe Core.Text,
    -- | If the commit contains deletions, whether to keep a folder or folder
    -- structure if the changes leave the folders empty. If true, a ..gitkeep
    -- file is created for empty folders. The default is false.
    keepEmptyFolders :: Core.Maybe Core.Bool,
    -- | The name of the repository where you create the commit.
    repositoryName :: Core.Text,
    -- | The name of the branch where you create the commit.
    branchName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCommit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteFiles', 'createCommit_deleteFiles' - The files to delete in this commit. These files still exist in earlier
-- commits.
--
-- 'setFileModes', 'createCommit_setFileModes' - The file modes to update for files in this commit.
--
-- 'parentCommitId', 'createCommit_parentCommitId' - The ID of the commit that is the parent of the commit you create. Not
-- required if this is an empty repository.
--
-- 'commitMessage', 'createCommit_commitMessage' - The commit message you want to include in the commit. Commit messages
-- are limited to 256 KB. If no message is specified, a default message is
-- used.
--
-- 'authorName', 'createCommit_authorName' - The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
--
-- 'putFiles', 'createCommit_putFiles' - The files to add or update in this commit.
--
-- 'email', 'createCommit_email' - The email address of the person who created the commit.
--
-- 'keepEmptyFolders', 'createCommit_keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a ..gitkeep
-- file is created for empty folders. The default is false.
--
-- 'repositoryName', 'createCommit_repositoryName' - The name of the repository where you create the commit.
--
-- 'branchName', 'createCommit_branchName' - The name of the branch where you create the commit.
newCreateCommit ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'branchName'
  Core.Text ->
  CreateCommit
newCreateCommit pRepositoryName_ pBranchName_ =
  CreateCommit'
    { deleteFiles = Core.Nothing,
      setFileModes = Core.Nothing,
      parentCommitId = Core.Nothing,
      commitMessage = Core.Nothing,
      authorName = Core.Nothing,
      putFiles = Core.Nothing,
      email = Core.Nothing,
      keepEmptyFolders = Core.Nothing,
      repositoryName = pRepositoryName_,
      branchName = pBranchName_
    }

-- | The files to delete in this commit. These files still exist in earlier
-- commits.
createCommit_deleteFiles :: Lens.Lens' CreateCommit (Core.Maybe [DeleteFileEntry])
createCommit_deleteFiles = Lens.lens (\CreateCommit' {deleteFiles} -> deleteFiles) (\s@CreateCommit' {} a -> s {deleteFiles = a} :: CreateCommit) Core.. Lens.mapping Lens._Coerce

-- | The file modes to update for files in this commit.
createCommit_setFileModes :: Lens.Lens' CreateCommit (Core.Maybe [SetFileModeEntry])
createCommit_setFileModes = Lens.lens (\CreateCommit' {setFileModes} -> setFileModes) (\s@CreateCommit' {} a -> s {setFileModes = a} :: CreateCommit) Core.. Lens.mapping Lens._Coerce

-- | The ID of the commit that is the parent of the commit you create. Not
-- required if this is an empty repository.
createCommit_parentCommitId :: Lens.Lens' CreateCommit (Core.Maybe Core.Text)
createCommit_parentCommitId = Lens.lens (\CreateCommit' {parentCommitId} -> parentCommitId) (\s@CreateCommit' {} a -> s {parentCommitId = a} :: CreateCommit)

-- | The commit message you want to include in the commit. Commit messages
-- are limited to 256 KB. If no message is specified, a default message is
-- used.
createCommit_commitMessage :: Lens.Lens' CreateCommit (Core.Maybe Core.Text)
createCommit_commitMessage = Lens.lens (\CreateCommit' {commitMessage} -> commitMessage) (\s@CreateCommit' {} a -> s {commitMessage = a} :: CreateCommit)

-- | The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
createCommit_authorName :: Lens.Lens' CreateCommit (Core.Maybe Core.Text)
createCommit_authorName = Lens.lens (\CreateCommit' {authorName} -> authorName) (\s@CreateCommit' {} a -> s {authorName = a} :: CreateCommit)

-- | The files to add or update in this commit.
createCommit_putFiles :: Lens.Lens' CreateCommit (Core.Maybe [PutFileEntry])
createCommit_putFiles = Lens.lens (\CreateCommit' {putFiles} -> putFiles) (\s@CreateCommit' {} a -> s {putFiles = a} :: CreateCommit) Core.. Lens.mapping Lens._Coerce

-- | The email address of the person who created the commit.
createCommit_email :: Lens.Lens' CreateCommit (Core.Maybe Core.Text)
createCommit_email = Lens.lens (\CreateCommit' {email} -> email) (\s@CreateCommit' {} a -> s {email = a} :: CreateCommit)

-- | If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a ..gitkeep
-- file is created for empty folders. The default is false.
createCommit_keepEmptyFolders :: Lens.Lens' CreateCommit (Core.Maybe Core.Bool)
createCommit_keepEmptyFolders = Lens.lens (\CreateCommit' {keepEmptyFolders} -> keepEmptyFolders) (\s@CreateCommit' {} a -> s {keepEmptyFolders = a} :: CreateCommit)

-- | The name of the repository where you create the commit.
createCommit_repositoryName :: Lens.Lens' CreateCommit Core.Text
createCommit_repositoryName = Lens.lens (\CreateCommit' {repositoryName} -> repositoryName) (\s@CreateCommit' {} a -> s {repositoryName = a} :: CreateCommit)

-- | The name of the branch where you create the commit.
createCommit_branchName :: Lens.Lens' CreateCommit Core.Text
createCommit_branchName = Lens.lens (\CreateCommit' {branchName} -> branchName) (\s@CreateCommit' {} a -> s {branchName = a} :: CreateCommit)

instance Core.AWSRequest CreateCommit where
  type AWSResponse CreateCommit = CreateCommitResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCommitResponse'
            Core.<$> (x Core..?> "commitId")
            Core.<*> (x Core..?> "treeId")
            Core.<*> (x Core..?> "filesAdded" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "filesUpdated" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "filesDeleted" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCommit

instance Core.NFData CreateCommit

instance Core.ToHeaders CreateCommit where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.CreateCommit" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateCommit where
  toJSON CreateCommit' {..} =
    Core.object
      ( Core.catMaybes
          [ ("deleteFiles" Core..=) Core.<$> deleteFiles,
            ("setFileModes" Core..=) Core.<$> setFileModes,
            ("parentCommitId" Core..=) Core.<$> parentCommitId,
            ("commitMessage" Core..=) Core.<$> commitMessage,
            ("authorName" Core..=) Core.<$> authorName,
            ("putFiles" Core..=) Core.<$> putFiles,
            ("email" Core..=) Core.<$> email,
            ("keepEmptyFolders" Core..=)
              Core.<$> keepEmptyFolders,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("branchName" Core..= branchName)
          ]
      )

instance Core.ToPath CreateCommit where
  toPath = Core.const "/"

instance Core.ToQuery CreateCommit where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateCommitResponse' smart constructor.
data CreateCommitResponse = CreateCommitResponse'
  { -- | The full commit ID of the commit that contains your committed file
    -- changes.
    commitId :: Core.Maybe Core.Text,
    -- | The full SHA-1 pointer of the tree information for the commit that
    -- contains the commited file changes.
    treeId :: Core.Maybe Core.Text,
    -- | The files added as part of the committed file changes.
    filesAdded :: Core.Maybe [FileMetadata],
    -- | The files updated as part of the commited file changes.
    filesUpdated :: Core.Maybe [FileMetadata],
    -- | The files deleted as part of the committed file changes.
    filesDeleted :: Core.Maybe [FileMetadata],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCommitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitId', 'createCommitResponse_commitId' - The full commit ID of the commit that contains your committed file
-- changes.
--
-- 'treeId', 'createCommitResponse_treeId' - The full SHA-1 pointer of the tree information for the commit that
-- contains the commited file changes.
--
-- 'filesAdded', 'createCommitResponse_filesAdded' - The files added as part of the committed file changes.
--
-- 'filesUpdated', 'createCommitResponse_filesUpdated' - The files updated as part of the commited file changes.
--
-- 'filesDeleted', 'createCommitResponse_filesDeleted' - The files deleted as part of the committed file changes.
--
-- 'httpStatus', 'createCommitResponse_httpStatus' - The response's http status code.
newCreateCommitResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCommitResponse
newCreateCommitResponse pHttpStatus_ =
  CreateCommitResponse'
    { commitId = Core.Nothing,
      treeId = Core.Nothing,
      filesAdded = Core.Nothing,
      filesUpdated = Core.Nothing,
      filesDeleted = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full commit ID of the commit that contains your committed file
-- changes.
createCommitResponse_commitId :: Lens.Lens' CreateCommitResponse (Core.Maybe Core.Text)
createCommitResponse_commitId = Lens.lens (\CreateCommitResponse' {commitId} -> commitId) (\s@CreateCommitResponse' {} a -> s {commitId = a} :: CreateCommitResponse)

-- | The full SHA-1 pointer of the tree information for the commit that
-- contains the commited file changes.
createCommitResponse_treeId :: Lens.Lens' CreateCommitResponse (Core.Maybe Core.Text)
createCommitResponse_treeId = Lens.lens (\CreateCommitResponse' {treeId} -> treeId) (\s@CreateCommitResponse' {} a -> s {treeId = a} :: CreateCommitResponse)

-- | The files added as part of the committed file changes.
createCommitResponse_filesAdded :: Lens.Lens' CreateCommitResponse (Core.Maybe [FileMetadata])
createCommitResponse_filesAdded = Lens.lens (\CreateCommitResponse' {filesAdded} -> filesAdded) (\s@CreateCommitResponse' {} a -> s {filesAdded = a} :: CreateCommitResponse) Core.. Lens.mapping Lens._Coerce

-- | The files updated as part of the commited file changes.
createCommitResponse_filesUpdated :: Lens.Lens' CreateCommitResponse (Core.Maybe [FileMetadata])
createCommitResponse_filesUpdated = Lens.lens (\CreateCommitResponse' {filesUpdated} -> filesUpdated) (\s@CreateCommitResponse' {} a -> s {filesUpdated = a} :: CreateCommitResponse) Core.. Lens.mapping Lens._Coerce

-- | The files deleted as part of the committed file changes.
createCommitResponse_filesDeleted :: Lens.Lens' CreateCommitResponse (Core.Maybe [FileMetadata])
createCommitResponse_filesDeleted = Lens.lens (\CreateCommitResponse' {filesDeleted} -> filesDeleted) (\s@CreateCommitResponse' {} a -> s {filesDeleted = a} :: CreateCommitResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createCommitResponse_httpStatus :: Lens.Lens' CreateCommitResponse Core.Int
createCommitResponse_httpStatus = Lens.lens (\CreateCommitResponse' {httpStatus} -> httpStatus) (\s@CreateCommitResponse' {} a -> s {httpStatus = a} :: CreateCommitResponse)

instance Core.NFData CreateCommitResponse
