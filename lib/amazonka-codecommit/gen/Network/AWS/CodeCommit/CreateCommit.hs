{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreateCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a commit for a repository on the tip of a specified branch.
module Network.AWS.CodeCommit.CreateCommit
  ( -- * Creating a request
    CreateCommit (..),
    mkCreateCommit,

    -- ** Request lenses
    ccSetFileModes,
    ccEmail,
    ccAuthorName,
    ccParentCommitId,
    ccBranchName,
    ccDeleteFiles,
    ccPutFiles,
    ccCommitMessage,
    ccRepositoryName,
    ccKeepEmptyFolders,

    -- * Destructuring the response
    CreateCommitResponse (..),
    mkCreateCommitResponse,

    -- ** Response lenses
    ccrsCommitId,
    ccrsTreeId,
    ccrsFilesAdded,
    ccrsFilesUpdated,
    ccrsFilesDeleted,
    ccrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCommit' smart constructor.
data CreateCommit = CreateCommit'
  { -- | The file modes to update for files in this commit.
    setFileModes :: Lude.Maybe [SetFileModeEntry],
    -- | The email address of the person who created the commit.
    email :: Lude.Maybe Lude.Text,
    -- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
    authorName :: Lude.Maybe Lude.Text,
    -- | The ID of the commit that is the parent of the commit you create. Not required if this is an empty repository.
    parentCommitId :: Lude.Maybe Lude.Text,
    -- | The name of the branch where you create the commit.
    branchName :: Lude.Text,
    -- | The files to delete in this commit. These files still exist in earlier commits.
    deleteFiles :: Lude.Maybe [DeleteFileEntry],
    -- | The files to add or update in this commit.
    putFiles :: Lude.Maybe [PutFileEntry],
    -- | The commit message you want to include in the commit. Commit messages are limited to 256 KB. If no message is specified, a default message is used.
    commitMessage :: Lude.Maybe Lude.Text,
    -- | The name of the repository where you create the commit.
    repositoryName :: Lude.Text,
    -- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a ..gitkeep file is created for empty folders. The default is false.
    keepEmptyFolders :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCommit' with the minimum fields required to make a request.
--
-- * 'setFileModes' - The file modes to update for files in this commit.
-- * 'email' - The email address of the person who created the commit.
-- * 'authorName' - The name of the author who created the commit. This information is used as both the author and committer for the commit.
-- * 'parentCommitId' - The ID of the commit that is the parent of the commit you create. Not required if this is an empty repository.
-- * 'branchName' - The name of the branch where you create the commit.
-- * 'deleteFiles' - The files to delete in this commit. These files still exist in earlier commits.
-- * 'putFiles' - The files to add or update in this commit.
-- * 'commitMessage' - The commit message you want to include in the commit. Commit messages are limited to 256 KB. If no message is specified, a default message is used.
-- * 'repositoryName' - The name of the repository where you create the commit.
-- * 'keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a ..gitkeep file is created for empty folders. The default is false.
mkCreateCommit ::
  -- | 'branchName'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  CreateCommit
mkCreateCommit pBranchName_ pRepositoryName_ =
  CreateCommit'
    { setFileModes = Lude.Nothing,
      email = Lude.Nothing,
      authorName = Lude.Nothing,
      parentCommitId = Lude.Nothing,
      branchName = pBranchName_,
      deleteFiles = Lude.Nothing,
      putFiles = Lude.Nothing,
      commitMessage = Lude.Nothing,
      repositoryName = pRepositoryName_,
      keepEmptyFolders = Lude.Nothing
    }

-- | The file modes to update for files in this commit.
--
-- /Note:/ Consider using 'setFileModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSetFileModes :: Lens.Lens' CreateCommit (Lude.Maybe [SetFileModeEntry])
ccSetFileModes = Lens.lens (setFileModes :: CreateCommit -> Lude.Maybe [SetFileModeEntry]) (\s a -> s {setFileModes = a} :: CreateCommit)
{-# DEPRECATED ccSetFileModes "Use generic-lens or generic-optics with 'setFileModes' instead." #-}

-- | The email address of the person who created the commit.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEmail :: Lens.Lens' CreateCommit (Lude.Maybe Lude.Text)
ccEmail = Lens.lens (email :: CreateCommit -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: CreateCommit)
{-# DEPRECATED ccEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- /Note:/ Consider using 'authorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAuthorName :: Lens.Lens' CreateCommit (Lude.Maybe Lude.Text)
ccAuthorName = Lens.lens (authorName :: CreateCommit -> Lude.Maybe Lude.Text) (\s a -> s {authorName = a} :: CreateCommit)
{-# DEPRECATED ccAuthorName "Use generic-lens or generic-optics with 'authorName' instead." #-}

-- | The ID of the commit that is the parent of the commit you create. Not required if this is an empty repository.
--
-- /Note:/ Consider using 'parentCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccParentCommitId :: Lens.Lens' CreateCommit (Lude.Maybe Lude.Text)
ccParentCommitId = Lens.lens (parentCommitId :: CreateCommit -> Lude.Maybe Lude.Text) (\s a -> s {parentCommitId = a} :: CreateCommit)
{-# DEPRECATED ccParentCommitId "Use generic-lens or generic-optics with 'parentCommitId' instead." #-}

-- | The name of the branch where you create the commit.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBranchName :: Lens.Lens' CreateCommit Lude.Text
ccBranchName = Lens.lens (branchName :: CreateCommit -> Lude.Text) (\s a -> s {branchName = a} :: CreateCommit)
{-# DEPRECATED ccBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

-- | The files to delete in this commit. These files still exist in earlier commits.
--
-- /Note:/ Consider using 'deleteFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDeleteFiles :: Lens.Lens' CreateCommit (Lude.Maybe [DeleteFileEntry])
ccDeleteFiles = Lens.lens (deleteFiles :: CreateCommit -> Lude.Maybe [DeleteFileEntry]) (\s a -> s {deleteFiles = a} :: CreateCommit)
{-# DEPRECATED ccDeleteFiles "Use generic-lens or generic-optics with 'deleteFiles' instead." #-}

-- | The files to add or update in this commit.
--
-- /Note:/ Consider using 'putFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPutFiles :: Lens.Lens' CreateCommit (Lude.Maybe [PutFileEntry])
ccPutFiles = Lens.lens (putFiles :: CreateCommit -> Lude.Maybe [PutFileEntry]) (\s a -> s {putFiles = a} :: CreateCommit)
{-# DEPRECATED ccPutFiles "Use generic-lens or generic-optics with 'putFiles' instead." #-}

-- | The commit message you want to include in the commit. Commit messages are limited to 256 KB. If no message is specified, a default message is used.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCommitMessage :: Lens.Lens' CreateCommit (Lude.Maybe Lude.Text)
ccCommitMessage = Lens.lens (commitMessage :: CreateCommit -> Lude.Maybe Lude.Text) (\s a -> s {commitMessage = a} :: CreateCommit)
{-# DEPRECATED ccCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | The name of the repository where you create the commit.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRepositoryName :: Lens.Lens' CreateCommit Lude.Text
ccRepositoryName = Lens.lens (repositoryName :: CreateCommit -> Lude.Text) (\s a -> s {repositoryName = a} :: CreateCommit)
{-# DEPRECATED ccRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a ..gitkeep file is created for empty folders. The default is false.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccKeepEmptyFolders :: Lens.Lens' CreateCommit (Lude.Maybe Lude.Bool)
ccKeepEmptyFolders = Lens.lens (keepEmptyFolders :: CreateCommit -> Lude.Maybe Lude.Bool) (\s a -> s {keepEmptyFolders = a} :: CreateCommit)
{-# DEPRECATED ccKeepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead." #-}

instance Lude.AWSRequest CreateCommit where
  type Rs CreateCommit = CreateCommitResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCommitResponse'
            Lude.<$> (x Lude..?> "commitId")
            Lude.<*> (x Lude..?> "treeId")
            Lude.<*> (x Lude..?> "filesAdded" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "filesUpdated" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "filesDeleted" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCommit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.CreateCommit" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCommit where
  toJSON CreateCommit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("setFileModes" Lude..=) Lude.<$> setFileModes,
            ("email" Lude..=) Lude.<$> email,
            ("authorName" Lude..=) Lude.<$> authorName,
            ("parentCommitId" Lude..=) Lude.<$> parentCommitId,
            Lude.Just ("branchName" Lude..= branchName),
            ("deleteFiles" Lude..=) Lude.<$> deleteFiles,
            ("putFiles" Lude..=) Lude.<$> putFiles,
            ("commitMessage" Lude..=) Lude.<$> commitMessage,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            ("keepEmptyFolders" Lude..=) Lude.<$> keepEmptyFolders
          ]
      )

instance Lude.ToPath CreateCommit where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCommit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCommitResponse' smart constructor.
data CreateCommitResponse = CreateCommitResponse'
  { -- | The full commit ID of the commit that contains your committed file changes.
    commitId :: Lude.Maybe Lude.Text,
    -- | The full SHA-1 pointer of the tree information for the commit that contains the commited file changes.
    treeId :: Lude.Maybe Lude.Text,
    -- | The files added as part of the committed file changes.
    filesAdded :: Lude.Maybe [FileMetadata],
    -- | The files updated as part of the commited file changes.
    filesUpdated :: Lude.Maybe [FileMetadata],
    -- | The files deleted as part of the committed file changes.
    filesDeleted :: Lude.Maybe [FileMetadata],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCommitResponse' with the minimum fields required to make a request.
--
-- * 'commitId' - The full commit ID of the commit that contains your committed file changes.
-- * 'treeId' - The full SHA-1 pointer of the tree information for the commit that contains the commited file changes.
-- * 'filesAdded' - The files added as part of the committed file changes.
-- * 'filesUpdated' - The files updated as part of the commited file changes.
-- * 'filesDeleted' - The files deleted as part of the committed file changes.
-- * 'responseStatus' - The response status code.
mkCreateCommitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCommitResponse
mkCreateCommitResponse pResponseStatus_ =
  CreateCommitResponse'
    { commitId = Lude.Nothing,
      treeId = Lude.Nothing,
      filesAdded = Lude.Nothing,
      filesUpdated = Lude.Nothing,
      filesDeleted = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The full commit ID of the commit that contains your committed file changes.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsCommitId :: Lens.Lens' CreateCommitResponse (Lude.Maybe Lude.Text)
ccrsCommitId = Lens.lens (commitId :: CreateCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {commitId = a} :: CreateCommitResponse)
{-# DEPRECATED ccrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The full SHA-1 pointer of the tree information for the commit that contains the commited file changes.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsTreeId :: Lens.Lens' CreateCommitResponse (Lude.Maybe Lude.Text)
ccrsTreeId = Lens.lens (treeId :: CreateCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {treeId = a} :: CreateCommitResponse)
{-# DEPRECATED ccrsTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | The files added as part of the committed file changes.
--
-- /Note:/ Consider using 'filesAdded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsFilesAdded :: Lens.Lens' CreateCommitResponse (Lude.Maybe [FileMetadata])
ccrsFilesAdded = Lens.lens (filesAdded :: CreateCommitResponse -> Lude.Maybe [FileMetadata]) (\s a -> s {filesAdded = a} :: CreateCommitResponse)
{-# DEPRECATED ccrsFilesAdded "Use generic-lens or generic-optics with 'filesAdded' instead." #-}

-- | The files updated as part of the commited file changes.
--
-- /Note:/ Consider using 'filesUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsFilesUpdated :: Lens.Lens' CreateCommitResponse (Lude.Maybe [FileMetadata])
ccrsFilesUpdated = Lens.lens (filesUpdated :: CreateCommitResponse -> Lude.Maybe [FileMetadata]) (\s a -> s {filesUpdated = a} :: CreateCommitResponse)
{-# DEPRECATED ccrsFilesUpdated "Use generic-lens or generic-optics with 'filesUpdated' instead." #-}

-- | The files deleted as part of the committed file changes.
--
-- /Note:/ Consider using 'filesDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsFilesDeleted :: Lens.Lens' CreateCommitResponse (Lude.Maybe [FileMetadata])
ccrsFilesDeleted = Lens.lens (filesDeleted :: CreateCommitResponse -> Lude.Maybe [FileMetadata]) (\s a -> s {filesDeleted = a} :: CreateCommitResponse)
{-# DEPRECATED ccrsFilesDeleted "Use generic-lens or generic-optics with 'filesDeleted' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateCommitResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateCommitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCommitResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
