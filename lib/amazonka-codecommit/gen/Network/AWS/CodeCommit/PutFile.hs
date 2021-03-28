{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutFile (..)
    , mkPutFile
    -- ** Request lenses
    , pfRepositoryName
    , pfBranchName
    , pfFileContent
    , pfFilePath
    , pfCommitMessage
    , pfEmail
    , pfFileMode
    , pfName
    , pfParentCommitId

    -- * Destructuring the response
    , PutFileResponse (..)
    , mkPutFileResponse
    -- ** Response lenses
    , pfrrsCommitId
    , pfrrsBlobId
    , pfrrsTreeId
    , pfrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutFile' smart constructor.
data PutFile = PutFile'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository where you want to add or update the file.
  , branchName :: Types.BranchName
    -- ^ The name of the branch where you want to add or update the file. If this is an empty repository, this branch is created.
  , fileContent :: Core.Base64
    -- ^ The content of the file, in binary object format. 
  , filePath :: Types.Path
    -- ^ The name of the file you want to add or update, including the relative path to the file in the repository.
  , commitMessage :: Core.Maybe Types.Message
    -- ^ A message about why this file was added or updated. Although it is optional, a message makes the commit history for your repository more useful.
  , email :: Core.Maybe Types.Email
    -- ^ An email address for the person adding or updating the file.
  , fileMode :: Core.Maybe Types.FileModeTypeEnum
    -- ^ The file mode permissions of the blob. Valid file mode permissions are listed here.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the person adding or updating the file. Although it is optional, a name makes the commit history for your repository more useful.
  , parentCommitId :: Core.Maybe Types.CommitId
    -- ^ The full commit ID of the head commit in the branch where you want to add or update the file. If this is an empty repository, no commit ID is required. If this is not an empty repository, a commit ID is required. 
--
-- The commit ID must match the ID of the head commit at the time of the operation. Otherwise, an error occurs, and the file is not added or updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutFile' value with any optional fields omitted.
mkPutFile
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.BranchName -- ^ 'branchName'
    -> Core.Base64 -- ^ 'fileContent'
    -> Types.Path -- ^ 'filePath'
    -> PutFile
mkPutFile repositoryName branchName fileContent filePath
  = PutFile'{repositoryName, branchName, fileContent, filePath,
             commitMessage = Core.Nothing, email = Core.Nothing,
             fileMode = Core.Nothing, name = Core.Nothing,
             parentCommitId = Core.Nothing}

-- | The name of the repository where you want to add or update the file.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfRepositoryName :: Lens.Lens' PutFile Types.RepositoryName
pfRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE pfRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The name of the branch where you want to add or update the file. If this is an empty repository, this branch is created.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfBranchName :: Lens.Lens' PutFile Types.BranchName
pfBranchName = Lens.field @"branchName"
{-# INLINEABLE pfBranchName #-}
{-# DEPRECATED branchName "Use generic-lens or generic-optics with 'branchName' instead"  #-}

-- | The content of the file, in binary object format. --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'fileContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFileContent :: Lens.Lens' PutFile Core.Base64
pfFileContent = Lens.field @"fileContent"
{-# INLINEABLE pfFileContent #-}
{-# DEPRECATED fileContent "Use generic-lens or generic-optics with 'fileContent' instead"  #-}

-- | The name of the file you want to add or update, including the relative path to the file in the repository.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFilePath :: Lens.Lens' PutFile Types.Path
pfFilePath = Lens.field @"filePath"
{-# INLINEABLE pfFilePath #-}
{-# DEPRECATED filePath "Use generic-lens or generic-optics with 'filePath' instead"  #-}

-- | A message about why this file was added or updated. Although it is optional, a message makes the commit history for your repository more useful.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfCommitMessage :: Lens.Lens' PutFile (Core.Maybe Types.Message)
pfCommitMessage = Lens.field @"commitMessage"
{-# INLINEABLE pfCommitMessage #-}
{-# DEPRECATED commitMessage "Use generic-lens or generic-optics with 'commitMessage' instead"  #-}

-- | An email address for the person adding or updating the file.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfEmail :: Lens.Lens' PutFile (Core.Maybe Types.Email)
pfEmail = Lens.field @"email"
{-# INLINEABLE pfEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The file mode permissions of the blob. Valid file mode permissions are listed here.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFileMode :: Lens.Lens' PutFile (Core.Maybe Types.FileModeTypeEnum)
pfFileMode = Lens.field @"fileMode"
{-# INLINEABLE pfFileMode #-}
{-# DEPRECATED fileMode "Use generic-lens or generic-optics with 'fileMode' instead"  #-}

-- | The name of the person adding or updating the file. Although it is optional, a name makes the commit history for your repository more useful.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfName :: Lens.Lens' PutFile (Core.Maybe Types.Name)
pfName = Lens.field @"name"
{-# INLINEABLE pfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The full commit ID of the head commit in the branch where you want to add or update the file. If this is an empty repository, no commit ID is required. If this is not an empty repository, a commit ID is required. 
--
-- The commit ID must match the ID of the head commit at the time of the operation. Otherwise, an error occurs, and the file is not added or updated.
--
-- /Note:/ Consider using 'parentCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfParentCommitId :: Lens.Lens' PutFile (Core.Maybe Types.CommitId)
pfParentCommitId = Lens.field @"parentCommitId"
{-# INLINEABLE pfParentCommitId #-}
{-# DEPRECATED parentCommitId "Use generic-lens or generic-optics with 'parentCommitId' instead"  #-}

instance Core.ToQuery PutFile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutFile where
        toHeaders PutFile{..}
          = Core.pure ("X-Amz-Target", "CodeCommit_20150413.PutFile") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutFile where
        toJSON PutFile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("branchName" Core..= branchName),
                  Core.Just ("fileContent" Core..= fileContent),
                  Core.Just ("filePath" Core..= filePath),
                  ("commitMessage" Core..=) Core.<$> commitMessage,
                  ("email" Core..=) Core.<$> email,
                  ("fileMode" Core..=) Core.<$> fileMode,
                  ("name" Core..=) Core.<$> name,
                  ("parentCommitId" Core..=) Core.<$> parentCommitId])

instance Core.AWSRequest PutFile where
        type Rs PutFile = PutFileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutFileResponse' Core.<$>
                   (x Core..: "commitId") Core.<*> x Core..: "blobId" Core.<*>
                     x Core..: "treeId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutFileResponse' smart constructor.
data PutFileResponse = PutFileResponse'
  { commitId :: Types.CommitId
    -- ^ The full SHA ID of the commit that contains this file change.
  , blobId :: Types.BlobId
    -- ^ The ID of the blob, which is its SHA-1 pointer.
  , treeId :: Types.TreeId
    -- ^ The full SHA-1 pointer of the tree information for the commit that contains this file change.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutFileResponse' value with any optional fields omitted.
mkPutFileResponse
    :: Types.CommitId -- ^ 'commitId'
    -> Types.BlobId -- ^ 'blobId'
    -> Types.TreeId -- ^ 'treeId'
    -> Core.Int -- ^ 'responseStatus'
    -> PutFileResponse
mkPutFileResponse commitId blobId treeId responseStatus
  = PutFileResponse'{commitId, blobId, treeId, responseStatus}

-- | The full SHA ID of the commit that contains this file change.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfrrsCommitId :: Lens.Lens' PutFileResponse Types.CommitId
pfrrsCommitId = Lens.field @"commitId"
{-# INLINEABLE pfrrsCommitId #-}
{-# DEPRECATED commitId "Use generic-lens or generic-optics with 'commitId' instead"  #-}

-- | The ID of the blob, which is its SHA-1 pointer.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfrrsBlobId :: Lens.Lens' PutFileResponse Types.BlobId
pfrrsBlobId = Lens.field @"blobId"
{-# INLINEABLE pfrrsBlobId #-}
{-# DEPRECATED blobId "Use generic-lens or generic-optics with 'blobId' instead"  #-}

-- | The full SHA-1 pointer of the tree information for the commit that contains this file change.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfrrsTreeId :: Lens.Lens' PutFileResponse Types.TreeId
pfrrsTreeId = Lens.field @"treeId"
{-# INLINEABLE pfrrsTreeId #-}
{-# DEPRECATED treeId "Use generic-lens or generic-optics with 'treeId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfrrsResponseStatus :: Lens.Lens' PutFileResponse Core.Int
pfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
