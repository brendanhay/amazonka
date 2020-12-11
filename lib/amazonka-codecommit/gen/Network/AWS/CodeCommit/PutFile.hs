{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PutFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a file in a branch in an AWS CodeCommit repository, and generates a commit for the addition in the specified branch.
module Network.AWS.CodeCommit.PutFile
  ( -- * Creating a request
    PutFile (..),
    mkPutFile,

    -- ** Request lenses
    pfEmail,
    pfFileMode,
    pfParentCommitId,
    pfName,
    pfCommitMessage,
    pfRepositoryName,
    pfBranchName,
    pfFileContent,
    pfFilePath,

    -- * Destructuring the response
    PutFileResponse (..),
    mkPutFileResponse,

    -- ** Response lenses
    pfrsResponseStatus,
    pfrsCommitId,
    pfrsBlobId,
    pfrsTreeId,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutFile' smart constructor.
data PutFile = PutFile'
  { email :: Lude.Maybe Lude.Text,
    fileMode :: Lude.Maybe FileModeTypeEnum,
    parentCommitId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    commitMessage :: Lude.Maybe Lude.Text,
    repositoryName :: Lude.Text,
    branchName :: Lude.Text,
    fileContent :: Lude.Base64,
    filePath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutFile' with the minimum fields required to make a request.
--
-- * 'branchName' - The name of the branch where you want to add or update the file. If this is an empty repository, this branch is created.
-- * 'commitMessage' - A message about why this file was added or updated. Although it is optional, a message makes the commit history for your repository more useful.
-- * 'email' - An email address for the person adding or updating the file.
-- * 'fileContent' - The content of the file, in binary object format. --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'fileMode' - The file mode permissions of the blob. Valid file mode permissions are listed here.
-- * 'filePath' - The name of the file you want to add or update, including the relative path to the file in the repository.
-- * 'name' - The name of the person adding or updating the file. Although it is optional, a name makes the commit history for your repository more useful.
-- * 'parentCommitId' - The full commit ID of the head commit in the branch where you want to add or update the file. If this is an empty repository, no commit ID is required. If this is not an empty repository, a commit ID is required.
--
-- The commit ID must match the ID of the head commit at the time of the operation. Otherwise, an error occurs, and the file is not added or updated.
-- * 'repositoryName' - The name of the repository where you want to add or update the file.
mkPutFile ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'branchName'
  Lude.Text ->
  -- | 'fileContent'
  Lude.Base64 ->
  -- | 'filePath'
  Lude.Text ->
  PutFile
mkPutFile pRepositoryName_ pBranchName_ pFileContent_ pFilePath_ =
  PutFile'
    { email = Lude.Nothing,
      fileMode = Lude.Nothing,
      parentCommitId = Lude.Nothing,
      name = Lude.Nothing,
      commitMessage = Lude.Nothing,
      repositoryName = pRepositoryName_,
      branchName = pBranchName_,
      fileContent = pFileContent_,
      filePath = pFilePath_
    }

-- | An email address for the person adding or updating the file.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfEmail :: Lens.Lens' PutFile (Lude.Maybe Lude.Text)
pfEmail = Lens.lens (email :: PutFile -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: PutFile)
{-# DEPRECATED pfEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The file mode permissions of the blob. Valid file mode permissions are listed here.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFileMode :: Lens.Lens' PutFile (Lude.Maybe FileModeTypeEnum)
pfFileMode = Lens.lens (fileMode :: PutFile -> Lude.Maybe FileModeTypeEnum) (\s a -> s {fileMode = a} :: PutFile)
{-# DEPRECATED pfFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The full commit ID of the head commit in the branch where you want to add or update the file. If this is an empty repository, no commit ID is required. If this is not an empty repository, a commit ID is required.
--
-- The commit ID must match the ID of the head commit at the time of the operation. Otherwise, an error occurs, and the file is not added or updated.
--
-- /Note:/ Consider using 'parentCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfParentCommitId :: Lens.Lens' PutFile (Lude.Maybe Lude.Text)
pfParentCommitId = Lens.lens (parentCommitId :: PutFile -> Lude.Maybe Lude.Text) (\s a -> s {parentCommitId = a} :: PutFile)
{-# DEPRECATED pfParentCommitId "Use generic-lens or generic-optics with 'parentCommitId' instead." #-}

-- | The name of the person adding or updating the file. Although it is optional, a name makes the commit history for your repository more useful.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfName :: Lens.Lens' PutFile (Lude.Maybe Lude.Text)
pfName = Lens.lens (name :: PutFile -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PutFile)
{-# DEPRECATED pfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A message about why this file was added or updated. Although it is optional, a message makes the commit history for your repository more useful.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfCommitMessage :: Lens.Lens' PutFile (Lude.Maybe Lude.Text)
pfCommitMessage = Lens.lens (commitMessage :: PutFile -> Lude.Maybe Lude.Text) (\s a -> s {commitMessage = a} :: PutFile)
{-# DEPRECATED pfCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | The name of the repository where you want to add or update the file.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfRepositoryName :: Lens.Lens' PutFile Lude.Text
pfRepositoryName = Lens.lens (repositoryName :: PutFile -> Lude.Text) (\s a -> s {repositoryName = a} :: PutFile)
{-# DEPRECATED pfRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The name of the branch where you want to add or update the file. If this is an empty repository, this branch is created.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfBranchName :: Lens.Lens' PutFile Lude.Text
pfBranchName = Lens.lens (branchName :: PutFile -> Lude.Text) (\s a -> s {branchName = a} :: PutFile)
{-# DEPRECATED pfBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

-- | The content of the file, in binary object format. --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'fileContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFileContent :: Lens.Lens' PutFile Lude.Base64
pfFileContent = Lens.lens (fileContent :: PutFile -> Lude.Base64) (\s a -> s {fileContent = a} :: PutFile)
{-# DEPRECATED pfFileContent "Use generic-lens or generic-optics with 'fileContent' instead." #-}

-- | The name of the file you want to add or update, including the relative path to the file in the repository.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFilePath :: Lens.Lens' PutFile Lude.Text
pfFilePath = Lens.lens (filePath :: PutFile -> Lude.Text) (\s a -> s {filePath = a} :: PutFile)
{-# DEPRECATED pfFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

instance Lude.AWSRequest PutFile where
  type Rs PutFile = PutFileResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutFileResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "commitId")
            Lude.<*> (x Lude..:> "blobId")
            Lude.<*> (x Lude..:> "treeId")
      )

instance Lude.ToHeaders PutFile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.PutFile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutFile where
  toJSON PutFile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("email" Lude..=) Lude.<$> email,
            ("fileMode" Lude..=) Lude.<$> fileMode,
            ("parentCommitId" Lude..=) Lude.<$> parentCommitId,
            ("name" Lude..=) Lude.<$> name,
            ("commitMessage" Lude..=) Lude.<$> commitMessage,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("branchName" Lude..= branchName),
            Lude.Just ("fileContent" Lude..= fileContent),
            Lude.Just ("filePath" Lude..= filePath)
          ]
      )

instance Lude.ToPath PutFile where
  toPath = Lude.const "/"

instance Lude.ToQuery PutFile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutFileResponse' smart constructor.
data PutFileResponse = PutFileResponse'
  { responseStatus :: Lude.Int,
    commitId :: Lude.Text,
    blobId :: Lude.Text,
    treeId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutFileResponse' with the minimum fields required to make a request.
--
-- * 'blobId' - The ID of the blob, which is its SHA-1 pointer.
-- * 'commitId' - The full SHA ID of the commit that contains this file change.
-- * 'responseStatus' - The response status code.
-- * 'treeId' - The full SHA-1 pointer of the tree information for the commit that contains this file change.
mkPutFileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'commitId'
  Lude.Text ->
  -- | 'blobId'
  Lude.Text ->
  -- | 'treeId'
  Lude.Text ->
  PutFileResponse
mkPutFileResponse pResponseStatus_ pCommitId_ pBlobId_ pTreeId_ =
  PutFileResponse'
    { responseStatus = pResponseStatus_,
      commitId = pCommitId_,
      blobId = pBlobId_,
      treeId = pTreeId_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfrsResponseStatus :: Lens.Lens' PutFileResponse Lude.Int
pfrsResponseStatus = Lens.lens (responseStatus :: PutFileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutFileResponse)
{-# DEPRECATED pfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The full SHA ID of the commit that contains this file change.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfrsCommitId :: Lens.Lens' PutFileResponse Lude.Text
pfrsCommitId = Lens.lens (commitId :: PutFileResponse -> Lude.Text) (\s a -> s {commitId = a} :: PutFileResponse)
{-# DEPRECATED pfrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The ID of the blob, which is its SHA-1 pointer.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfrsBlobId :: Lens.Lens' PutFileResponse Lude.Text
pfrsBlobId = Lens.lens (blobId :: PutFileResponse -> Lude.Text) (\s a -> s {blobId = a} :: PutFileResponse)
{-# DEPRECATED pfrsBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

-- | The full SHA-1 pointer of the tree information for the commit that contains this file change.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfrsTreeId :: Lens.Lens' PutFileResponse Lude.Text
pfrsTreeId = Lens.lens (treeId :: PutFileResponse -> Lude.Text) (\s a -> s {treeId = a} :: PutFileResponse)
{-# DEPRECATED pfrsTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}
