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
    ccRepositoryName,
    ccBranchName,
    ccAuthorName,
    ccCommitMessage,
    ccDeleteFiles,
    ccEmail,
    ccKeepEmptyFolders,
    ccParentCommitId,
    ccPutFiles,
    ccSetFileModes,

    -- * Destructuring the response
    CreateCommitResponse (..),
    mkCreateCommitResponse,

    -- ** Response lenses
    ccrrsCommitId,
    ccrrsFilesAdded,
    ccrrsFilesDeleted,
    ccrrsFilesUpdated,
    ccrrsTreeId,
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCommit' smart constructor.
data CreateCommit = CreateCommit'
  { -- | The name of the repository where you create the commit.
    repositoryName :: Types.RepositoryName,
    -- | The name of the branch where you create the commit.
    branchName :: Types.BranchName,
    -- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
    authorName :: Core.Maybe Types.AuthorName,
    -- | The commit message you want to include in the commit. Commit messages are limited to 256 KB. If no message is specified, a default message is used.
    commitMessage :: Core.Maybe Types.CommitMessage,
    -- | The files to delete in this commit. These files still exist in earlier commits.
    deleteFiles :: Core.Maybe [Types.DeleteFileEntry],
    -- | The email address of the person who created the commit.
    email :: Core.Maybe Types.Email,
    -- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a ..gitkeep file is created for empty folders. The default is false.
    keepEmptyFolders :: Core.Maybe Core.Bool,
    -- | The ID of the commit that is the parent of the commit you create. Not required if this is an empty repository.
    parentCommitId :: Core.Maybe Types.CommitId,
    -- | The files to add or update in this commit.
    putFiles :: Core.Maybe [Types.PutFileEntry],
    -- | The file modes to update for files in this commit.
    setFileModes :: Core.Maybe [Types.SetFileModeEntry]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCommit' value with any optional fields omitted.
mkCreateCommit ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'branchName'
  Types.BranchName ->
  CreateCommit
mkCreateCommit repositoryName branchName =
  CreateCommit'
    { repositoryName,
      branchName,
      authorName = Core.Nothing,
      commitMessage = Core.Nothing,
      deleteFiles = Core.Nothing,
      email = Core.Nothing,
      keepEmptyFolders = Core.Nothing,
      parentCommitId = Core.Nothing,
      putFiles = Core.Nothing,
      setFileModes = Core.Nothing
    }

-- | The name of the repository where you create the commit.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRepositoryName :: Lens.Lens' CreateCommit Types.RepositoryName
ccRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED ccRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The name of the branch where you create the commit.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBranchName :: Lens.Lens' CreateCommit Types.BranchName
ccBranchName = Lens.field @"branchName"
{-# DEPRECATED ccBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

-- | The name of the author who created the commit. This information is used as both the author and committer for the commit.
--
-- /Note:/ Consider using 'authorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAuthorName :: Lens.Lens' CreateCommit (Core.Maybe Types.AuthorName)
ccAuthorName = Lens.field @"authorName"
{-# DEPRECATED ccAuthorName "Use generic-lens or generic-optics with 'authorName' instead." #-}

-- | The commit message you want to include in the commit. Commit messages are limited to 256 KB. If no message is specified, a default message is used.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCommitMessage :: Lens.Lens' CreateCommit (Core.Maybe Types.CommitMessage)
ccCommitMessage = Lens.field @"commitMessage"
{-# DEPRECATED ccCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | The files to delete in this commit. These files still exist in earlier commits.
--
-- /Note:/ Consider using 'deleteFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDeleteFiles :: Lens.Lens' CreateCommit (Core.Maybe [Types.DeleteFileEntry])
ccDeleteFiles = Lens.field @"deleteFiles"
{-# DEPRECATED ccDeleteFiles "Use generic-lens or generic-optics with 'deleteFiles' instead." #-}

-- | The email address of the person who created the commit.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEmail :: Lens.Lens' CreateCommit (Core.Maybe Types.Email)
ccEmail = Lens.field @"email"
{-# DEPRECATED ccEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If true, a ..gitkeep file is created for empty folders. The default is false.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccKeepEmptyFolders :: Lens.Lens' CreateCommit (Core.Maybe Core.Bool)
ccKeepEmptyFolders = Lens.field @"keepEmptyFolders"
{-# DEPRECATED ccKeepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead." #-}

-- | The ID of the commit that is the parent of the commit you create. Not required if this is an empty repository.
--
-- /Note:/ Consider using 'parentCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccParentCommitId :: Lens.Lens' CreateCommit (Core.Maybe Types.CommitId)
ccParentCommitId = Lens.field @"parentCommitId"
{-# DEPRECATED ccParentCommitId "Use generic-lens or generic-optics with 'parentCommitId' instead." #-}

-- | The files to add or update in this commit.
--
-- /Note:/ Consider using 'putFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPutFiles :: Lens.Lens' CreateCommit (Core.Maybe [Types.PutFileEntry])
ccPutFiles = Lens.field @"putFiles"
{-# DEPRECATED ccPutFiles "Use generic-lens or generic-optics with 'putFiles' instead." #-}

-- | The file modes to update for files in this commit.
--
-- /Note:/ Consider using 'setFileModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSetFileModes :: Lens.Lens' CreateCommit (Core.Maybe [Types.SetFileModeEntry])
ccSetFileModes = Lens.field @"setFileModes"
{-# DEPRECATED ccSetFileModes "Use generic-lens or generic-optics with 'setFileModes' instead." #-}

instance Core.FromJSON CreateCommit where
  toJSON CreateCommit {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("branchName" Core..= branchName),
            ("authorName" Core..=) Core.<$> authorName,
            ("commitMessage" Core..=) Core.<$> commitMessage,
            ("deleteFiles" Core..=) Core.<$> deleteFiles,
            ("email" Core..=) Core.<$> email,
            ("keepEmptyFolders" Core..=) Core.<$> keepEmptyFolders,
            ("parentCommitId" Core..=) Core.<$> parentCommitId,
            ("putFiles" Core..=) Core.<$> putFiles,
            ("setFileModes" Core..=) Core.<$> setFileModes
          ]
      )

instance Core.AWSRequest CreateCommit where
  type Rs CreateCommit = CreateCommitResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.CreateCommit")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCommitResponse'
            Core.<$> (x Core..:? "commitId")
            Core.<*> (x Core..:? "filesAdded")
            Core.<*> (x Core..:? "filesDeleted")
            Core.<*> (x Core..:? "filesUpdated")
            Core.<*> (x Core..:? "treeId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCommitResponse' smart constructor.
data CreateCommitResponse = CreateCommitResponse'
  { -- | The full commit ID of the commit that contains your committed file changes.
    commitId :: Core.Maybe Types.ObjectId,
    -- | The files added as part of the committed file changes.
    filesAdded :: Core.Maybe [Types.FileMetadata],
    -- | The files deleted as part of the committed file changes.
    filesDeleted :: Core.Maybe [Types.FileMetadata],
    -- | The files updated as part of the commited file changes.
    filesUpdated :: Core.Maybe [Types.FileMetadata],
    -- | The full SHA-1 pointer of the tree information for the commit that contains the commited file changes.
    treeId :: Core.Maybe Types.ObjectId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCommitResponse' value with any optional fields omitted.
mkCreateCommitResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCommitResponse
mkCreateCommitResponse responseStatus =
  CreateCommitResponse'
    { commitId = Core.Nothing,
      filesAdded = Core.Nothing,
      filesDeleted = Core.Nothing,
      filesUpdated = Core.Nothing,
      treeId = Core.Nothing,
      responseStatus
    }

-- | The full commit ID of the commit that contains your committed file changes.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCommitId :: Lens.Lens' CreateCommitResponse (Core.Maybe Types.ObjectId)
ccrrsCommitId = Lens.field @"commitId"
{-# DEPRECATED ccrrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The files added as part of the committed file changes.
--
-- /Note:/ Consider using 'filesAdded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsFilesAdded :: Lens.Lens' CreateCommitResponse (Core.Maybe [Types.FileMetadata])
ccrrsFilesAdded = Lens.field @"filesAdded"
{-# DEPRECATED ccrrsFilesAdded "Use generic-lens or generic-optics with 'filesAdded' instead." #-}

-- | The files deleted as part of the committed file changes.
--
-- /Note:/ Consider using 'filesDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsFilesDeleted :: Lens.Lens' CreateCommitResponse (Core.Maybe [Types.FileMetadata])
ccrrsFilesDeleted = Lens.field @"filesDeleted"
{-# DEPRECATED ccrrsFilesDeleted "Use generic-lens or generic-optics with 'filesDeleted' instead." #-}

-- | The files updated as part of the commited file changes.
--
-- /Note:/ Consider using 'filesUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsFilesUpdated :: Lens.Lens' CreateCommitResponse (Core.Maybe [Types.FileMetadata])
ccrrsFilesUpdated = Lens.field @"filesUpdated"
{-# DEPRECATED ccrrsFilesUpdated "Use generic-lens or generic-optics with 'filesUpdated' instead." #-}

-- | The full SHA-1 pointer of the tree information for the commit that contains the commited file changes.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsTreeId :: Lens.Lens' CreateCommitResponse (Core.Maybe Types.ObjectId)
ccrrsTreeId = Lens.field @"treeId"
{-# DEPRECATED ccrrsTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateCommitResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
