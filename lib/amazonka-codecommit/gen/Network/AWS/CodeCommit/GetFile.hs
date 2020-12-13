{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gffFilePath,
    gffCommitSpecifier,
    gffRepositoryName,

    -- * Destructuring the response
    GetFileResponse (..),
    mkGetFileResponse,

    -- ** Response lenses
    gfrsCommitId,
    gfrsFileContent,
    gfrsFileMode,
    gfrsFilePath,
    gfrsFileSize,
    gfrsBlobId,
    gfrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFile' smart constructor.
data GetFile = GetFile'
  { -- | The fully qualified path to the file, including the full name and extension of the file. For example, /examples/file.md is the fully qualified path to a file named file.md in a folder named examples.
    filePath :: Lude.Text,
    -- | The fully quaified reference that identifies the commit that contains the file. For example, you can specify a full commit ID, a tag, a branch name, or a reference such as refs/heads/master. If none is provided, the head commit is used.
    commitSpecifier :: Lude.Maybe Lude.Text,
    -- | The name of the repository that contains the file.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFile' with the minimum fields required to make a request.
--
-- * 'filePath' - The fully qualified path to the file, including the full name and extension of the file. For example, /examples/file.md is the fully qualified path to a file named file.md in a folder named examples.
-- * 'commitSpecifier' - The fully quaified reference that identifies the commit that contains the file. For example, you can specify a full commit ID, a tag, a branch name, or a reference such as refs/heads/master. If none is provided, the head commit is used.
-- * 'repositoryName' - The name of the repository that contains the file.
mkGetFile ::
  -- | 'filePath'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  GetFile
mkGetFile pFilePath_ pRepositoryName_ =
  GetFile'
    { filePath = pFilePath_,
      commitSpecifier = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The fully qualified path to the file, including the full name and extension of the file. For example, /examples/file.md is the fully qualified path to a file named file.md in a folder named examples.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffFilePath :: Lens.Lens' GetFile Lude.Text
gffFilePath = Lens.lens (filePath :: GetFile -> Lude.Text) (\s a -> s {filePath = a} :: GetFile)
{-# DEPRECATED gffFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The fully quaified reference that identifies the commit that contains the file. For example, you can specify a full commit ID, a tag, a branch name, or a reference such as refs/heads/master. If none is provided, the head commit is used.
--
-- /Note:/ Consider using 'commitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffCommitSpecifier :: Lens.Lens' GetFile (Lude.Maybe Lude.Text)
gffCommitSpecifier = Lens.lens (commitSpecifier :: GetFile -> Lude.Maybe Lude.Text) (\s a -> s {commitSpecifier = a} :: GetFile)
{-# DEPRECATED gffCommitSpecifier "Use generic-lens or generic-optics with 'commitSpecifier' instead." #-}

-- | The name of the repository that contains the file.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffRepositoryName :: Lens.Lens' GetFile Lude.Text
gffRepositoryName = Lens.lens (repositoryName :: GetFile -> Lude.Text) (\s a -> s {repositoryName = a} :: GetFile)
{-# DEPRECATED gffRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest GetFile where
  type Rs GetFile = GetFileResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFileResponse'
            Lude.<$> (x Lude..:> "commitId")
            Lude.<*> (x Lude..:> "fileContent")
            Lude.<*> (x Lude..:> "fileMode")
            Lude.<*> (x Lude..:> "filePath")
            Lude.<*> (x Lude..:> "fileSize")
            Lude.<*> (x Lude..:> "blobId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
          [ Lude.Just ("filePath" Lude..= filePath),
            ("commitSpecifier" Lude..=) Lude.<$> commitSpecifier,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath GetFile where
  toPath = Lude.const "/"

instance Lude.ToQuery GetFile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFileResponse' smart constructor.
data GetFileResponse = GetFileResponse'
  { -- | The full commit ID of the commit that contains the content returned by GetFile.
    commitId :: Lude.Text,
    -- | The base-64 encoded binary data object that represents the content of the file.
    fileContent :: Lude.Base64,
    -- | The extrapolated file mode permissions of the blob. Valid values include strings such as EXECUTABLE and not numeric values.
    fileMode :: FileModeTypeEnum,
    -- | The fully qualified path to the specified file. Returns the name and extension of the file.
    filePath :: Lude.Text,
    -- | The size of the contents of the file, in bytes.
    fileSize :: Lude.Integer,
    -- | The blob ID of the object that represents the file content.
    blobId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFileResponse' with the minimum fields required to make a request.
--
-- * 'commitId' - The full commit ID of the commit that contains the content returned by GetFile.
-- * 'fileContent' - The base-64 encoded binary data object that represents the content of the file.
-- * 'fileMode' - The extrapolated file mode permissions of the blob. Valid values include strings such as EXECUTABLE and not numeric values.
-- * 'filePath' - The fully qualified path to the specified file. Returns the name and extension of the file.
-- * 'fileSize' - The size of the contents of the file, in bytes.
-- * 'blobId' - The blob ID of the object that represents the file content.
-- * 'responseStatus' - The response status code.
mkGetFileResponse ::
  -- | 'commitId'
  Lude.Text ->
  -- | 'fileContent'
  Lude.Base64 ->
  -- | 'fileMode'
  FileModeTypeEnum ->
  -- | 'filePath'
  Lude.Text ->
  -- | 'fileSize'
  Lude.Integer ->
  -- | 'blobId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  GetFileResponse
mkGetFileResponse
  pCommitId_
  pFileContent_
  pFileMode_
  pFilePath_
  pFileSize_
  pBlobId_
  pResponseStatus_ =
    GetFileResponse'
      { commitId = pCommitId_,
        fileContent = pFileContent_,
        fileMode = pFileMode_,
        filePath = pFilePath_,
        fileSize = pFileSize_,
        blobId = pBlobId_,
        responseStatus = pResponseStatus_
      }

-- | The full commit ID of the commit that contains the content returned by GetFile.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsCommitId :: Lens.Lens' GetFileResponse Lude.Text
gfrsCommitId = Lens.lens (commitId :: GetFileResponse -> Lude.Text) (\s a -> s {commitId = a} :: GetFileResponse)
{-# DEPRECATED gfrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The base-64 encoded binary data object that represents the content of the file.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'fileContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsFileContent :: Lens.Lens' GetFileResponse Lude.Base64
gfrsFileContent = Lens.lens (fileContent :: GetFileResponse -> Lude.Base64) (\s a -> s {fileContent = a} :: GetFileResponse)
{-# DEPRECATED gfrsFileContent "Use generic-lens or generic-optics with 'fileContent' instead." #-}

-- | The extrapolated file mode permissions of the blob. Valid values include strings such as EXECUTABLE and not numeric values.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsFileMode :: Lens.Lens' GetFileResponse FileModeTypeEnum
gfrsFileMode = Lens.lens (fileMode :: GetFileResponse -> FileModeTypeEnum) (\s a -> s {fileMode = a} :: GetFileResponse)
{-# DEPRECATED gfrsFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The fully qualified path to the specified file. Returns the name and extension of the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsFilePath :: Lens.Lens' GetFileResponse Lude.Text
gfrsFilePath = Lens.lens (filePath :: GetFileResponse -> Lude.Text) (\s a -> s {filePath = a} :: GetFileResponse)
{-# DEPRECATED gfrsFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The size of the contents of the file, in bytes.
--
-- /Note:/ Consider using 'fileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsFileSize :: Lens.Lens' GetFileResponse Lude.Integer
gfrsFileSize = Lens.lens (fileSize :: GetFileResponse -> Lude.Integer) (\s a -> s {fileSize = a} :: GetFileResponse)
{-# DEPRECATED gfrsFileSize "Use generic-lens or generic-optics with 'fileSize' instead." #-}

-- | The blob ID of the object that represents the file content.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsBlobId :: Lens.Lens' GetFileResponse Lude.Text
gfrsBlobId = Lens.lens (blobId :: GetFileResponse -> Lude.Text) (\s a -> s {blobId = a} :: GetFileResponse)
{-# DEPRECATED gfrsBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsResponseStatus :: Lens.Lens' GetFileResponse Lude.Int
gfrsResponseStatus = Lens.lens (responseStatus :: GetFileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFileResponse)
{-# DEPRECATED gfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
