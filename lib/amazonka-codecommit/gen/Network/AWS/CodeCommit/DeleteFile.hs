{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DeleteFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified file from a specified branch. A commit is created on the branch that contains the revision. The file still exists in the commits earlier to the commit that contains the deletion.
module Network.AWS.CodeCommit.DeleteFile
  ( -- * Creating a request
    DeleteFile (..),
    mkDeleteFile,

    -- ** Request lenses
    dfEmail,
    dfParentCommitId,
    dfFilePath,
    dfBranchName,
    dfName,
    dfCommitMessage,
    dfRepositoryName,
    dfKeepEmptyFolders,

    -- * Destructuring the response
    DeleteFileResponse (..),
    mkDeleteFileResponse,

    -- ** Response lenses
    dfrsCommitId,
    dfrsFilePath,
    dfrsTreeId,
    dfrsBlobId,
    dfrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFile' smart constructor.
data DeleteFile = DeleteFile'
  { -- | The email address for the commit that deletes the file. If no email address is specified, the email address is left blank.
    email :: Lude.Maybe Lude.Text,
    -- | The ID of the commit that is the tip of the branch where you want to create the commit that deletes the file. This must be the HEAD commit for the branch. The commit that deletes the file is created from this commit ID.
    parentCommitId :: Lude.Text,
    -- | The fully qualified path to the file that to be deleted, including the full name and extension of that file. For example, /examples/file.md is a fully qualified path to a file named file.md in a folder named examples.
    filePath :: Lude.Text,
    -- | The name of the branch where the commit that deletes the file is made.
    branchName :: Lude.Text,
    -- | The name of the author of the commit that deletes the file. If no name is specified, the user's ARN is used as the author name and committer name.
    name :: Lude.Maybe Lude.Text,
    -- | The commit message you want to include as part of deleting the file. Commit messages are limited to 256 KB. If no message is specified, a default message is used.
    commitMessage :: Lude.Maybe Lude.Text,
    -- | The name of the repository that contains the file to delete.
    repositoryName :: Lude.Text,
    -- | If a file is the only object in the folder or directory, specifies whether to delete the folder or directory that contains the file. By default, empty folders are deleted. This includes empty folders that are part of the directory structure. For example, if the path to a file is dir1/dir2/dir3/dir4, and dir2 and dir3 are empty, deleting the last file in dir4 also deletes the empty folders dir4, dir3, and dir2.
    keepEmptyFolders :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFile' with the minimum fields required to make a request.
--
-- * 'email' - The email address for the commit that deletes the file. If no email address is specified, the email address is left blank.
-- * 'parentCommitId' - The ID of the commit that is the tip of the branch where you want to create the commit that deletes the file. This must be the HEAD commit for the branch. The commit that deletes the file is created from this commit ID.
-- * 'filePath' - The fully qualified path to the file that to be deleted, including the full name and extension of that file. For example, /examples/file.md is a fully qualified path to a file named file.md in a folder named examples.
-- * 'branchName' - The name of the branch where the commit that deletes the file is made.
-- * 'name' - The name of the author of the commit that deletes the file. If no name is specified, the user's ARN is used as the author name and committer name.
-- * 'commitMessage' - The commit message you want to include as part of deleting the file. Commit messages are limited to 256 KB. If no message is specified, a default message is used.
-- * 'repositoryName' - The name of the repository that contains the file to delete.
-- * 'keepEmptyFolders' - If a file is the only object in the folder or directory, specifies whether to delete the folder or directory that contains the file. By default, empty folders are deleted. This includes empty folders that are part of the directory structure. For example, if the path to a file is dir1/dir2/dir3/dir4, and dir2 and dir3 are empty, deleting the last file in dir4 also deletes the empty folders dir4, dir3, and dir2.
mkDeleteFile ::
  -- | 'parentCommitId'
  Lude.Text ->
  -- | 'filePath'
  Lude.Text ->
  -- | 'branchName'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  DeleteFile
mkDeleteFile
  pParentCommitId_
  pFilePath_
  pBranchName_
  pRepositoryName_ =
    DeleteFile'
      { email = Lude.Nothing,
        parentCommitId = pParentCommitId_,
        filePath = pFilePath_,
        branchName = pBranchName_,
        name = Lude.Nothing,
        commitMessage = Lude.Nothing,
        repositoryName = pRepositoryName_,
        keepEmptyFolders = Lude.Nothing
      }

-- | The email address for the commit that deletes the file. If no email address is specified, the email address is left blank.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfEmail :: Lens.Lens' DeleteFile (Lude.Maybe Lude.Text)
dfEmail = Lens.lens (email :: DeleteFile -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: DeleteFile)
{-# DEPRECATED dfEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The ID of the commit that is the tip of the branch where you want to create the commit that deletes the file. This must be the HEAD commit for the branch. The commit that deletes the file is created from this commit ID.
--
-- /Note:/ Consider using 'parentCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfParentCommitId :: Lens.Lens' DeleteFile Lude.Text
dfParentCommitId = Lens.lens (parentCommitId :: DeleteFile -> Lude.Text) (\s a -> s {parentCommitId = a} :: DeleteFile)
{-# DEPRECATED dfParentCommitId "Use generic-lens or generic-optics with 'parentCommitId' instead." #-}

-- | The fully qualified path to the file that to be deleted, including the full name and extension of that file. For example, /examples/file.md is a fully qualified path to a file named file.md in a folder named examples.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFilePath :: Lens.Lens' DeleteFile Lude.Text
dfFilePath = Lens.lens (filePath :: DeleteFile -> Lude.Text) (\s a -> s {filePath = a} :: DeleteFile)
{-# DEPRECATED dfFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The name of the branch where the commit that deletes the file is made.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfBranchName :: Lens.Lens' DeleteFile Lude.Text
dfBranchName = Lens.lens (branchName :: DeleteFile -> Lude.Text) (\s a -> s {branchName = a} :: DeleteFile)
{-# DEPRECATED dfBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

-- | The name of the author of the commit that deletes the file. If no name is specified, the user's ARN is used as the author name and committer name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' DeleteFile (Lude.Maybe Lude.Text)
dfName = Lens.lens (name :: DeleteFile -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteFile)
{-# DEPRECATED dfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The commit message you want to include as part of deleting the file. Commit messages are limited to 256 KB. If no message is specified, a default message is used.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfCommitMessage :: Lens.Lens' DeleteFile (Lude.Maybe Lude.Text)
dfCommitMessage = Lens.lens (commitMessage :: DeleteFile -> Lude.Maybe Lude.Text) (\s a -> s {commitMessage = a} :: DeleteFile)
{-# DEPRECATED dfCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | The name of the repository that contains the file to delete.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfRepositoryName :: Lens.Lens' DeleteFile Lude.Text
dfRepositoryName = Lens.lens (repositoryName :: DeleteFile -> Lude.Text) (\s a -> s {repositoryName = a} :: DeleteFile)
{-# DEPRECATED dfRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | If a file is the only object in the folder or directory, specifies whether to delete the folder or directory that contains the file. By default, empty folders are deleted. This includes empty folders that are part of the directory structure. For example, if the path to a file is dir1/dir2/dir3/dir4, and dir2 and dir3 are empty, deleting the last file in dir4 also deletes the empty folders dir4, dir3, and dir2.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfKeepEmptyFolders :: Lens.Lens' DeleteFile (Lude.Maybe Lude.Bool)
dfKeepEmptyFolders = Lens.lens (keepEmptyFolders :: DeleteFile -> Lude.Maybe Lude.Bool) (\s a -> s {keepEmptyFolders = a} :: DeleteFile)
{-# DEPRECATED dfKeepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead." #-}

instance Lude.AWSRequest DeleteFile where
  type Rs DeleteFile = DeleteFileResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteFileResponse'
            Lude.<$> (x Lude..:> "commitId")
            Lude.<*> (x Lude..:> "filePath")
            Lude.<*> (x Lude..:> "treeId")
            Lude.<*> (x Lude..:> "blobId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.DeleteFile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteFile where
  toJSON DeleteFile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("email" Lude..=) Lude.<$> email,
            Lude.Just ("parentCommitId" Lude..= parentCommitId),
            Lude.Just ("filePath" Lude..= filePath),
            Lude.Just ("branchName" Lude..= branchName),
            ("name" Lude..=) Lude.<$> name,
            ("commitMessage" Lude..=) Lude.<$> commitMessage,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            ("keepEmptyFolders" Lude..=) Lude.<$> keepEmptyFolders
          ]
      )

instance Lude.ToPath DeleteFile where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteFile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFileResponse' smart constructor.
data DeleteFileResponse = DeleteFileResponse'
  { -- | The full commit ID of the commit that contains the change that deletes the file.
    commitId :: Lude.Text,
    -- | The fully qualified path to the file to be deleted, including the full name and extension of that file.
    filePath :: Lude.Text,
    -- | The full SHA-1 pointer of the tree information for the commit that contains the delete file change.
    treeId :: Lude.Text,
    -- | The blob ID removed from the tree as part of deleting the file.
    blobId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFileResponse' with the minimum fields required to make a request.
--
-- * 'commitId' - The full commit ID of the commit that contains the change that deletes the file.
-- * 'filePath' - The fully qualified path to the file to be deleted, including the full name and extension of that file.
-- * 'treeId' - The full SHA-1 pointer of the tree information for the commit that contains the delete file change.
-- * 'blobId' - The blob ID removed from the tree as part of deleting the file.
-- * 'responseStatus' - The response status code.
mkDeleteFileResponse ::
  -- | 'commitId'
  Lude.Text ->
  -- | 'filePath'
  Lude.Text ->
  -- | 'treeId'
  Lude.Text ->
  -- | 'blobId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFileResponse
mkDeleteFileResponse
  pCommitId_
  pFilePath_
  pTreeId_
  pBlobId_
  pResponseStatus_ =
    DeleteFileResponse'
      { commitId = pCommitId_,
        filePath = pFilePath_,
        treeId = pTreeId_,
        blobId = pBlobId_,
        responseStatus = pResponseStatus_
      }

-- | The full commit ID of the commit that contains the change that deletes the file.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsCommitId :: Lens.Lens' DeleteFileResponse Lude.Text
dfrsCommitId = Lens.lens (commitId :: DeleteFileResponse -> Lude.Text) (\s a -> s {commitId = a} :: DeleteFileResponse)
{-# DEPRECATED dfrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The fully qualified path to the file to be deleted, including the full name and extension of that file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsFilePath :: Lens.Lens' DeleteFileResponse Lude.Text
dfrsFilePath = Lens.lens (filePath :: DeleteFileResponse -> Lude.Text) (\s a -> s {filePath = a} :: DeleteFileResponse)
{-# DEPRECATED dfrsFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The full SHA-1 pointer of the tree information for the commit that contains the delete file change.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsTreeId :: Lens.Lens' DeleteFileResponse Lude.Text
dfrsTreeId = Lens.lens (treeId :: DeleteFileResponse -> Lude.Text) (\s a -> s {treeId = a} :: DeleteFileResponse)
{-# DEPRECATED dfrsTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | The blob ID removed from the tree as part of deleting the file.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsBlobId :: Lens.Lens' DeleteFileResponse Lude.Text
dfrsBlobId = Lens.lens (blobId :: DeleteFileResponse -> Lude.Text) (\s a -> s {blobId = a} :: DeleteFileResponse)
{-# DEPRECATED dfrsBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsResponseStatus :: Lens.Lens' DeleteFileResponse Lude.Int
dfrsResponseStatus = Lens.lens (responseStatus :: DeleteFileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFileResponse)
{-# DEPRECATED dfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
