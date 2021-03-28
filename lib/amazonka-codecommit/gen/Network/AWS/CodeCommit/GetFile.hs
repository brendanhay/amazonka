{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetFile (..)
    , mkGetFile
    -- ** Request lenses
    , gffRepositoryName
    , gffFilePath
    , gffCommitSpecifier

    -- * Destructuring the response
    , GetFileResponse (..)
    , mkGetFileResponse
    -- ** Response lenses
    , gfrfrsCommitId
    , gfrfrsBlobId
    , gfrfrsFilePath
    , gfrfrsFileMode
    , gfrfrsFileSize
    , gfrfrsFileContent
    , gfrfrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFile' smart constructor.
data GetFile = GetFile'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository that contains the file.
  , filePath :: Types.Path
    -- ^ The fully qualified path to the file, including the full name and extension of the file. For example, /examples/file.md is the fully qualified path to a file named file.md in a folder named examples.
  , commitSpecifier :: Core.Maybe Types.CommitName
    -- ^ The fully quaified reference that identifies the commit that contains the file. For example, you can specify a full commit ID, a tag, a branch name, or a reference such as refs/heads/master. If none is provided, the head commit is used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFile' value with any optional fields omitted.
mkGetFile
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.Path -- ^ 'filePath'
    -> GetFile
mkGetFile repositoryName filePath
  = GetFile'{repositoryName, filePath,
             commitSpecifier = Core.Nothing}

-- | The name of the repository that contains the file.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffRepositoryName :: Lens.Lens' GetFile Types.RepositoryName
gffRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE gffRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The fully qualified path to the file, including the full name and extension of the file. For example, /examples/file.md is the fully qualified path to a file named file.md in a folder named examples.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffFilePath :: Lens.Lens' GetFile Types.Path
gffFilePath = Lens.field @"filePath"
{-# INLINEABLE gffFilePath #-}
{-# DEPRECATED filePath "Use generic-lens or generic-optics with 'filePath' instead"  #-}

-- | The fully quaified reference that identifies the commit that contains the file. For example, you can specify a full commit ID, a tag, a branch name, or a reference such as refs/heads/master. If none is provided, the head commit is used.
--
-- /Note:/ Consider using 'commitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffCommitSpecifier :: Lens.Lens' GetFile (Core.Maybe Types.CommitName)
gffCommitSpecifier = Lens.field @"commitSpecifier"
{-# INLINEABLE gffCommitSpecifier #-}
{-# DEPRECATED commitSpecifier "Use generic-lens or generic-optics with 'commitSpecifier' instead"  #-}

instance Core.ToQuery GetFile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFile where
        toHeaders GetFile{..}
          = Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetFile") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetFile where
        toJSON GetFile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("filePath" Core..= filePath),
                  ("commitSpecifier" Core..=) Core.<$> commitSpecifier])

instance Core.AWSRequest GetFile where
        type Rs GetFile = GetFileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetFileResponse' Core.<$>
                   (x Core..: "commitId") Core.<*> x Core..: "blobId" Core.<*>
                     x Core..: "filePath"
                     Core.<*> x Core..: "fileMode"
                     Core.<*> x Core..: "fileSize"
                     Core.<*> x Core..: "fileContent"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFileResponse' smart constructor.
data GetFileResponse = GetFileResponse'
  { commitId :: Types.CommitId
    -- ^ The full commit ID of the commit that contains the content returned by GetFile.
  , blobId :: Types.BlobId
    -- ^ The blob ID of the object that represents the file content.
  , filePath :: Types.FilePath
    -- ^ The fully qualified path to the specified file. Returns the name and extension of the file.
  , fileMode :: Types.FileModeTypeEnum
    -- ^ The extrapolated file mode permissions of the blob. Valid values include strings such as EXECUTABLE and not numeric values.
  , fileSize :: Core.Integer
    -- ^ The size of the contents of the file, in bytes.
  , fileContent :: Core.Base64
    -- ^ The base-64 encoded binary data object that represents the content of the file.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFileResponse' value with any optional fields omitted.
mkGetFileResponse
    :: Types.CommitId -- ^ 'commitId'
    -> Types.BlobId -- ^ 'blobId'
    -> Types.FilePath -- ^ 'filePath'
    -> Types.FileModeTypeEnum -- ^ 'fileMode'
    -> Core.Integer -- ^ 'fileSize'
    -> Core.Base64 -- ^ 'fileContent'
    -> Core.Int -- ^ 'responseStatus'
    -> GetFileResponse
mkGetFileResponse commitId blobId filePath fileMode fileSize
  fileContent responseStatus
  = GetFileResponse'{commitId, blobId, filePath, fileMode, fileSize,
                     fileContent, responseStatus}

-- | The full commit ID of the commit that contains the content returned by GetFile.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrfrsCommitId :: Lens.Lens' GetFileResponse Types.CommitId
gfrfrsCommitId = Lens.field @"commitId"
{-# INLINEABLE gfrfrsCommitId #-}
{-# DEPRECATED commitId "Use generic-lens or generic-optics with 'commitId' instead"  #-}

-- | The blob ID of the object that represents the file content.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrfrsBlobId :: Lens.Lens' GetFileResponse Types.BlobId
gfrfrsBlobId = Lens.field @"blobId"
{-# INLINEABLE gfrfrsBlobId #-}
{-# DEPRECATED blobId "Use generic-lens or generic-optics with 'blobId' instead"  #-}

-- | The fully qualified path to the specified file. Returns the name and extension of the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrfrsFilePath :: Lens.Lens' GetFileResponse Types.FilePath
gfrfrsFilePath = Lens.field @"filePath"
{-# INLINEABLE gfrfrsFilePath #-}
{-# DEPRECATED filePath "Use generic-lens or generic-optics with 'filePath' instead"  #-}

-- | The extrapolated file mode permissions of the blob. Valid values include strings such as EXECUTABLE and not numeric values.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrfrsFileMode :: Lens.Lens' GetFileResponse Types.FileModeTypeEnum
gfrfrsFileMode = Lens.field @"fileMode"
{-# INLINEABLE gfrfrsFileMode #-}
{-# DEPRECATED fileMode "Use generic-lens or generic-optics with 'fileMode' instead"  #-}

-- | The size of the contents of the file, in bytes.
--
-- /Note:/ Consider using 'fileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrfrsFileSize :: Lens.Lens' GetFileResponse Core.Integer
gfrfrsFileSize = Lens.field @"fileSize"
{-# INLINEABLE gfrfrsFileSize #-}
{-# DEPRECATED fileSize "Use generic-lens or generic-optics with 'fileSize' instead"  #-}

-- | The base-64 encoded binary data object that represents the content of the file.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'fileContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrfrsFileContent :: Lens.Lens' GetFileResponse Core.Base64
gfrfrsFileContent = Lens.field @"fileContent"
{-# INLINEABLE gfrfrsFileContent #-}
{-# DEPRECATED fileContent "Use generic-lens or generic-optics with 'fileContent' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrfrsResponseStatus :: Lens.Lens' GetFileResponse Core.Int
gfrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gfrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
