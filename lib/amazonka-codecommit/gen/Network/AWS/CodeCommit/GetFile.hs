{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the base-64 encoded contents of a specified file and its metadata.
module Network.AWS.CodeCommit.GetFile
  ( -- * Creating a request
    GetFile (..),
    mkGetFile,

    -- ** Request lenses
    getCommitSpecifier,
    getRepositoryName,
    getFilePath,

    -- * Destructuring the response
    GetFileResponse (..),
    mkGetFileResponse,

    -- ** Response lenses
    getrsResponseStatus,
    getrsCommitId,
    getrsBlobId,
    getrsFilePath,
    getrsFileMode,
    getrsFileSize,
    getrsFileContent,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFile' smart constructor.
data GetFile = GetFile'
  { commitSpecifier :: Lude.Maybe Lude.Text,
    repositoryName :: Lude.Text,
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

-- | Creates a value of 'GetFile' with the minimum fields required to make a request.
--
-- * 'commitSpecifier' - The fully quaified reference that identifies the commit that contains the file. For example, you can specify a full commit ID, a tag, a branch name, or a reference such as refs/heads/master. If none is provided, the head commit is used.
-- * 'filePath' - The fully qualified path to the file, including the full name and extension of the file. For example, /examples/file.md is the fully qualified path to a file named file.md in a folder named examples.
-- * 'repositoryName' - The name of the repository that contains the file.
mkGetFile ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'filePath'
  Lude.Text ->
  GetFile
mkGetFile pRepositoryName_ pFilePath_ =
  GetFile'
    { commitSpecifier = Lude.Nothing,
      repositoryName = pRepositoryName_,
      filePath = pFilePath_
    }

-- | The fully quaified reference that identifies the commit that contains the file. For example, you can specify a full commit ID, a tag, a branch name, or a reference such as refs/heads/master. If none is provided, the head commit is used.
--
-- /Note:/ Consider using 'commitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getCommitSpecifier :: Lens.Lens' GetFile (Lude.Maybe Lude.Text)
getCommitSpecifier = Lens.lens (commitSpecifier :: GetFile -> Lude.Maybe Lude.Text) (\s a -> s {commitSpecifier = a} :: GetFile)
{-# DEPRECATED getCommitSpecifier "Use generic-lens or generic-optics with 'commitSpecifier' instead." #-}

-- | The name of the repository that contains the file.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getRepositoryName :: Lens.Lens' GetFile Lude.Text
getRepositoryName = Lens.lens (repositoryName :: GetFile -> Lude.Text) (\s a -> s {repositoryName = a} :: GetFile)
{-# DEPRECATED getRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The fully qualified path to the file, including the full name and extension of the file. For example, /examples/file.md is the fully qualified path to a file named file.md in a folder named examples.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getFilePath :: Lens.Lens' GetFile Lude.Text
getFilePath = Lens.lens (filePath :: GetFile -> Lude.Text) (\s a -> s {filePath = a} :: GetFile)
{-# DEPRECATED getFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

instance Lude.AWSRequest GetFile where
  type Rs GetFile = GetFileResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFileResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "commitId")
            Lude.<*> (x Lude..:> "blobId")
            Lude.<*> (x Lude..:> "filePath")
            Lude.<*> (x Lude..:> "fileMode")
            Lude.<*> (x Lude..:> "fileSize")
            Lude.<*> (x Lude..:> "fileContent")
      )

instance Lude.ToHeaders GetFile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetFile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetFile where
  toJSON GetFile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("commitSpecifier" Lude..=) Lude.<$> commitSpecifier,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("filePath" Lude..= filePath)
          ]
      )

instance Lude.ToPath GetFile where
  toPath = Lude.const "/"

instance Lude.ToQuery GetFile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFileResponse' smart constructor.
data GetFileResponse = GetFileResponse'
  { responseStatus :: Lude.Int,
    commitId :: Lude.Text,
    blobId :: Lude.Text,
    filePath :: Lude.Text,
    fileMode :: FileModeTypeEnum,
    fileSize :: Lude.Integer,
    fileContent :: Lude.Base64
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFileResponse' with the minimum fields required to make a request.
--
-- * 'blobId' - The blob ID of the object that represents the file content.
-- * 'commitId' - The full commit ID of the commit that contains the content returned by GetFile.
-- * 'fileContent' - The base-64 encoded binary data object that represents the content of the file.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'fileMode' - The extrapolated file mode permissions of the blob. Valid values include strings such as EXECUTABLE and not numeric values.
-- * 'filePath' - The fully qualified path to the specified file. Returns the name and extension of the file.
-- * 'fileSize' - The size of the contents of the file, in bytes.
-- * 'responseStatus' - The response status code.
mkGetFileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'commitId'
  Lude.Text ->
  -- | 'blobId'
  Lude.Text ->
  -- | 'filePath'
  Lude.Text ->
  -- | 'fileMode'
  FileModeTypeEnum ->
  -- | 'fileSize'
  Lude.Integer ->
  -- | 'fileContent'
  Lude.Base64 ->
  GetFileResponse
mkGetFileResponse
  pResponseStatus_
  pCommitId_
  pBlobId_
  pFilePath_
  pFileMode_
  pFileSize_
  pFileContent_ =
    GetFileResponse'
      { responseStatus = pResponseStatus_,
        commitId = pCommitId_,
        blobId = pBlobId_,
        filePath = pFilePath_,
        fileMode = pFileMode_,
        fileSize = pFileSize_,
        fileContent = pFileContent_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsResponseStatus :: Lens.Lens' GetFileResponse Lude.Int
getrsResponseStatus = Lens.lens (responseStatus :: GetFileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFileResponse)
{-# DEPRECATED getrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The full commit ID of the commit that contains the content returned by GetFile.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsCommitId :: Lens.Lens' GetFileResponse Lude.Text
getrsCommitId = Lens.lens (commitId :: GetFileResponse -> Lude.Text) (\s a -> s {commitId = a} :: GetFileResponse)
{-# DEPRECATED getrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The blob ID of the object that represents the file content.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsBlobId :: Lens.Lens' GetFileResponse Lude.Text
getrsBlobId = Lens.lens (blobId :: GetFileResponse -> Lude.Text) (\s a -> s {blobId = a} :: GetFileResponse)
{-# DEPRECATED getrsBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

-- | The fully qualified path to the specified file. Returns the name and extension of the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsFilePath :: Lens.Lens' GetFileResponse Lude.Text
getrsFilePath = Lens.lens (filePath :: GetFileResponse -> Lude.Text) (\s a -> s {filePath = a} :: GetFileResponse)
{-# DEPRECATED getrsFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The extrapolated file mode permissions of the blob. Valid values include strings such as EXECUTABLE and not numeric values.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsFileMode :: Lens.Lens' GetFileResponse FileModeTypeEnum
getrsFileMode = Lens.lens (fileMode :: GetFileResponse -> FileModeTypeEnum) (\s a -> s {fileMode = a} :: GetFileResponse)
{-# DEPRECATED getrsFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The size of the contents of the file, in bytes.
--
-- /Note:/ Consider using 'fileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsFileSize :: Lens.Lens' GetFileResponse Lude.Integer
getrsFileSize = Lens.lens (fileSize :: GetFileResponse -> Lude.Integer) (\s a -> s {fileSize = a} :: GetFileResponse)
{-# DEPRECATED getrsFileSize "Use generic-lens or generic-optics with 'fileSize' instead." #-}

-- | The base-64 encoded binary data object that represents the content of the file.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'fileContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsFileContent :: Lens.Lens' GetFileResponse Lude.Base64
getrsFileContent = Lens.lens (fileContent :: GetFileResponse -> Lude.Base64) (\s a -> s {fileContent = a} :: GetFileResponse)
{-# DEPRECATED getrsFileContent "Use generic-lens or generic-optics with 'fileContent' instead." #-}
