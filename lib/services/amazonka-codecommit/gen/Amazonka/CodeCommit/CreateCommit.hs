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
-- Module      : Amazonka.CodeCommit.CreateCommit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a commit for a repository on the tip of a specified branch.
module Amazonka.CodeCommit.CreateCommit
  ( -- * Creating a Request
    CreateCommit (..),
    newCreateCommit,

    -- * Request Lenses
    createCommit_parentCommitId,
    createCommit_keepEmptyFolders,
    createCommit_email,
    createCommit_authorName,
    createCommit_setFileModes,
    createCommit_deleteFiles,
    createCommit_putFiles,
    createCommit_commitMessage,
    createCommit_repositoryName,
    createCommit_branchName,

    -- * Destructuring the Response
    CreateCommitResponse (..),
    newCreateCommitResponse,

    -- * Response Lenses
    createCommitResponse_commitId,
    createCommitResponse_filesAdded,
    createCommitResponse_treeId,
    createCommitResponse_filesUpdated,
    createCommitResponse_filesDeleted,
    createCommitResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCommit' smart constructor.
data CreateCommit = CreateCommit'
  { -- | The ID of the commit that is the parent of the commit you create. Not
    -- required if this is an empty repository.
    parentCommitId :: Prelude.Maybe Prelude.Text,
    -- | If the commit contains deletions, whether to keep a folder or folder
    -- structure if the changes leave the folders empty. If true, a ..gitkeep
    -- file is created for empty folders. The default is false.
    keepEmptyFolders :: Prelude.Maybe Prelude.Bool,
    -- | The email address of the person who created the commit.
    email :: Prelude.Maybe Prelude.Text,
    -- | The name of the author who created the commit. This information is used
    -- as both the author and committer for the commit.
    authorName :: Prelude.Maybe Prelude.Text,
    -- | The file modes to update for files in this commit.
    setFileModes :: Prelude.Maybe [SetFileModeEntry],
    -- | The files to delete in this commit. These files still exist in earlier
    -- commits.
    deleteFiles :: Prelude.Maybe [DeleteFileEntry],
    -- | The files to add or update in this commit.
    putFiles :: Prelude.Maybe [PutFileEntry],
    -- | The commit message you want to include in the commit. Commit messages
    -- are limited to 256 KB. If no message is specified, a default message is
    -- used.
    commitMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository where you create the commit.
    repositoryName :: Prelude.Text,
    -- | The name of the branch where you create the commit.
    branchName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCommit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentCommitId', 'createCommit_parentCommitId' - The ID of the commit that is the parent of the commit you create. Not
-- required if this is an empty repository.
--
-- 'keepEmptyFolders', 'createCommit_keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a ..gitkeep
-- file is created for empty folders. The default is false.
--
-- 'email', 'createCommit_email' - The email address of the person who created the commit.
--
-- 'authorName', 'createCommit_authorName' - The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
--
-- 'setFileModes', 'createCommit_setFileModes' - The file modes to update for files in this commit.
--
-- 'deleteFiles', 'createCommit_deleteFiles' - The files to delete in this commit. These files still exist in earlier
-- commits.
--
-- 'putFiles', 'createCommit_putFiles' - The files to add or update in this commit.
--
-- 'commitMessage', 'createCommit_commitMessage' - The commit message you want to include in the commit. Commit messages
-- are limited to 256 KB. If no message is specified, a default message is
-- used.
--
-- 'repositoryName', 'createCommit_repositoryName' - The name of the repository where you create the commit.
--
-- 'branchName', 'createCommit_branchName' - The name of the branch where you create the commit.
newCreateCommit ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  CreateCommit
newCreateCommit pRepositoryName_ pBranchName_ =
  CreateCommit'
    { parentCommitId = Prelude.Nothing,
      keepEmptyFolders = Prelude.Nothing,
      email = Prelude.Nothing,
      authorName = Prelude.Nothing,
      setFileModes = Prelude.Nothing,
      deleteFiles = Prelude.Nothing,
      putFiles = Prelude.Nothing,
      commitMessage = Prelude.Nothing,
      repositoryName = pRepositoryName_,
      branchName = pBranchName_
    }

-- | The ID of the commit that is the parent of the commit you create. Not
-- required if this is an empty repository.
createCommit_parentCommitId :: Lens.Lens' CreateCommit (Prelude.Maybe Prelude.Text)
createCommit_parentCommitId = Lens.lens (\CreateCommit' {parentCommitId} -> parentCommitId) (\s@CreateCommit' {} a -> s {parentCommitId = a} :: CreateCommit)

-- | If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If true, a ..gitkeep
-- file is created for empty folders. The default is false.
createCommit_keepEmptyFolders :: Lens.Lens' CreateCommit (Prelude.Maybe Prelude.Bool)
createCommit_keepEmptyFolders = Lens.lens (\CreateCommit' {keepEmptyFolders} -> keepEmptyFolders) (\s@CreateCommit' {} a -> s {keepEmptyFolders = a} :: CreateCommit)

-- | The email address of the person who created the commit.
createCommit_email :: Lens.Lens' CreateCommit (Prelude.Maybe Prelude.Text)
createCommit_email = Lens.lens (\CreateCommit' {email} -> email) (\s@CreateCommit' {} a -> s {email = a} :: CreateCommit)

-- | The name of the author who created the commit. This information is used
-- as both the author and committer for the commit.
createCommit_authorName :: Lens.Lens' CreateCommit (Prelude.Maybe Prelude.Text)
createCommit_authorName = Lens.lens (\CreateCommit' {authorName} -> authorName) (\s@CreateCommit' {} a -> s {authorName = a} :: CreateCommit)

-- | The file modes to update for files in this commit.
createCommit_setFileModes :: Lens.Lens' CreateCommit (Prelude.Maybe [SetFileModeEntry])
createCommit_setFileModes = Lens.lens (\CreateCommit' {setFileModes} -> setFileModes) (\s@CreateCommit' {} a -> s {setFileModes = a} :: CreateCommit) Prelude.. Lens.mapping Lens.coerced

-- | The files to delete in this commit. These files still exist in earlier
-- commits.
createCommit_deleteFiles :: Lens.Lens' CreateCommit (Prelude.Maybe [DeleteFileEntry])
createCommit_deleteFiles = Lens.lens (\CreateCommit' {deleteFiles} -> deleteFiles) (\s@CreateCommit' {} a -> s {deleteFiles = a} :: CreateCommit) Prelude.. Lens.mapping Lens.coerced

-- | The files to add or update in this commit.
createCommit_putFiles :: Lens.Lens' CreateCommit (Prelude.Maybe [PutFileEntry])
createCommit_putFiles = Lens.lens (\CreateCommit' {putFiles} -> putFiles) (\s@CreateCommit' {} a -> s {putFiles = a} :: CreateCommit) Prelude.. Lens.mapping Lens.coerced

-- | The commit message you want to include in the commit. Commit messages
-- are limited to 256 KB. If no message is specified, a default message is
-- used.
createCommit_commitMessage :: Lens.Lens' CreateCommit (Prelude.Maybe Prelude.Text)
createCommit_commitMessage = Lens.lens (\CreateCommit' {commitMessage} -> commitMessage) (\s@CreateCommit' {} a -> s {commitMessage = a} :: CreateCommit)

-- | The name of the repository where you create the commit.
createCommit_repositoryName :: Lens.Lens' CreateCommit Prelude.Text
createCommit_repositoryName = Lens.lens (\CreateCommit' {repositoryName} -> repositoryName) (\s@CreateCommit' {} a -> s {repositoryName = a} :: CreateCommit)

-- | The name of the branch where you create the commit.
createCommit_branchName :: Lens.Lens' CreateCommit Prelude.Text
createCommit_branchName = Lens.lens (\CreateCommit' {branchName} -> branchName) (\s@CreateCommit' {} a -> s {branchName = a} :: CreateCommit)

instance Core.AWSRequest CreateCommit where
  type AWSResponse CreateCommit = CreateCommitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCommitResponse'
            Prelude.<$> (x Core..?> "commitId")
            Prelude.<*> (x Core..?> "filesAdded" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "treeId")
            Prelude.<*> (x Core..?> "filesUpdated" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "filesDeleted" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCommit where
  hashWithSalt _salt CreateCommit' {..} =
    _salt `Prelude.hashWithSalt` parentCommitId
      `Prelude.hashWithSalt` keepEmptyFolders
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` authorName
      `Prelude.hashWithSalt` setFileModes
      `Prelude.hashWithSalt` deleteFiles
      `Prelude.hashWithSalt` putFiles
      `Prelude.hashWithSalt` commitMessage
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` branchName

instance Prelude.NFData CreateCommit where
  rnf CreateCommit' {..} =
    Prelude.rnf parentCommitId
      `Prelude.seq` Prelude.rnf keepEmptyFolders
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf authorName
      `Prelude.seq` Prelude.rnf setFileModes
      `Prelude.seq` Prelude.rnf deleteFiles
      `Prelude.seq` Prelude.rnf putFiles
      `Prelude.seq` Prelude.rnf commitMessage
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf branchName

instance Core.ToHeaders CreateCommit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.CreateCommit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCommit where
  toJSON CreateCommit' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("parentCommitId" Core..=)
              Prelude.<$> parentCommitId,
            ("keepEmptyFolders" Core..=)
              Prelude.<$> keepEmptyFolders,
            ("email" Core..=) Prelude.<$> email,
            ("authorName" Core..=) Prelude.<$> authorName,
            ("setFileModes" Core..=) Prelude.<$> setFileModes,
            ("deleteFiles" Core..=) Prelude.<$> deleteFiles,
            ("putFiles" Core..=) Prelude.<$> putFiles,
            ("commitMessage" Core..=) Prelude.<$> commitMessage,
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just ("branchName" Core..= branchName)
          ]
      )

instance Core.ToPath CreateCommit where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCommit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCommitResponse' smart constructor.
data CreateCommitResponse = CreateCommitResponse'
  { -- | The full commit ID of the commit that contains your committed file
    -- changes.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | The files added as part of the committed file changes.
    filesAdded :: Prelude.Maybe [FileMetadata],
    -- | The full SHA-1 pointer of the tree information for the commit that
    -- contains the commited file changes.
    treeId :: Prelude.Maybe Prelude.Text,
    -- | The files updated as part of the commited file changes.
    filesUpdated :: Prelude.Maybe [FileMetadata],
    -- | The files deleted as part of the committed file changes.
    filesDeleted :: Prelude.Maybe [FileMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'filesAdded', 'createCommitResponse_filesAdded' - The files added as part of the committed file changes.
--
-- 'treeId', 'createCommitResponse_treeId' - The full SHA-1 pointer of the tree information for the commit that
-- contains the commited file changes.
--
-- 'filesUpdated', 'createCommitResponse_filesUpdated' - The files updated as part of the commited file changes.
--
-- 'filesDeleted', 'createCommitResponse_filesDeleted' - The files deleted as part of the committed file changes.
--
-- 'httpStatus', 'createCommitResponse_httpStatus' - The response's http status code.
newCreateCommitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCommitResponse
newCreateCommitResponse pHttpStatus_ =
  CreateCommitResponse'
    { commitId = Prelude.Nothing,
      filesAdded = Prelude.Nothing,
      treeId = Prelude.Nothing,
      filesUpdated = Prelude.Nothing,
      filesDeleted = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full commit ID of the commit that contains your committed file
-- changes.
createCommitResponse_commitId :: Lens.Lens' CreateCommitResponse (Prelude.Maybe Prelude.Text)
createCommitResponse_commitId = Lens.lens (\CreateCommitResponse' {commitId} -> commitId) (\s@CreateCommitResponse' {} a -> s {commitId = a} :: CreateCommitResponse)

-- | The files added as part of the committed file changes.
createCommitResponse_filesAdded :: Lens.Lens' CreateCommitResponse (Prelude.Maybe [FileMetadata])
createCommitResponse_filesAdded = Lens.lens (\CreateCommitResponse' {filesAdded} -> filesAdded) (\s@CreateCommitResponse' {} a -> s {filesAdded = a} :: CreateCommitResponse) Prelude.. Lens.mapping Lens.coerced

-- | The full SHA-1 pointer of the tree information for the commit that
-- contains the commited file changes.
createCommitResponse_treeId :: Lens.Lens' CreateCommitResponse (Prelude.Maybe Prelude.Text)
createCommitResponse_treeId = Lens.lens (\CreateCommitResponse' {treeId} -> treeId) (\s@CreateCommitResponse' {} a -> s {treeId = a} :: CreateCommitResponse)

-- | The files updated as part of the commited file changes.
createCommitResponse_filesUpdated :: Lens.Lens' CreateCommitResponse (Prelude.Maybe [FileMetadata])
createCommitResponse_filesUpdated = Lens.lens (\CreateCommitResponse' {filesUpdated} -> filesUpdated) (\s@CreateCommitResponse' {} a -> s {filesUpdated = a} :: CreateCommitResponse) Prelude.. Lens.mapping Lens.coerced

-- | The files deleted as part of the committed file changes.
createCommitResponse_filesDeleted :: Lens.Lens' CreateCommitResponse (Prelude.Maybe [FileMetadata])
createCommitResponse_filesDeleted = Lens.lens (\CreateCommitResponse' {filesDeleted} -> filesDeleted) (\s@CreateCommitResponse' {} a -> s {filesDeleted = a} :: CreateCommitResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createCommitResponse_httpStatus :: Lens.Lens' CreateCommitResponse Prelude.Int
createCommitResponse_httpStatus = Lens.lens (\CreateCommitResponse' {httpStatus} -> httpStatus) (\s@CreateCommitResponse' {} a -> s {httpStatus = a} :: CreateCommitResponse)

instance Prelude.NFData CreateCommitResponse where
  rnf CreateCommitResponse' {..} =
    Prelude.rnf commitId
      `Prelude.seq` Prelude.rnf filesAdded
      `Prelude.seq` Prelude.rnf treeId
      `Prelude.seq` Prelude.rnf filesUpdated
      `Prelude.seq` Prelude.rnf filesDeleted
      `Prelude.seq` Prelude.rnf httpStatus
