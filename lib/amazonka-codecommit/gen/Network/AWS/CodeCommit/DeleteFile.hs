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
    dfRepositoryName,
    dfBranchName,
    dfFilePath,
    dfParentCommitId,
    dfCommitMessage,
    dfEmail,
    dfKeepEmptyFolders,
    dfName,

    -- * Destructuring the response
    DeleteFileResponse (..),
    mkDeleteFileResponse,

    -- ** Response lenses
    dfrrsCommitId,
    dfrrsBlobId,
    dfrrsTreeId,
    dfrrsFilePath,
    dfrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFile' smart constructor.
data DeleteFile = DeleteFile'
  { -- | The name of the repository that contains the file to delete.
    repositoryName :: Types.RepositoryName,
    -- | The name of the branch where the commit that deletes the file is made.
    branchName :: Types.BranchName,
    -- | The fully qualified path to the file that to be deleted, including the full name and extension of that file. For example, /examples/file.md is a fully qualified path to a file named file.md in a folder named examples.
    filePath :: Types.Path,
    -- | The ID of the commit that is the tip of the branch where you want to create the commit that deletes the file. This must be the HEAD commit for the branch. The commit that deletes the file is created from this commit ID.
    parentCommitId :: Types.CommitId,
    -- | The commit message you want to include as part of deleting the file. Commit messages are limited to 256 KB. If no message is specified, a default message is used.
    commitMessage :: Core.Maybe Types.Message,
    -- | The email address for the commit that deletes the file. If no email address is specified, the email address is left blank.
    email :: Core.Maybe Types.Email,
    -- | If a file is the only object in the folder or directory, specifies whether to delete the folder or directory that contains the file. By default, empty folders are deleted. This includes empty folders that are part of the directory structure. For example, if the path to a file is dir1/dir2/dir3/dir4, and dir2 and dir3 are empty, deleting the last file in dir4 also deletes the empty folders dir4, dir3, and dir2.
    keepEmptyFolders :: Core.Maybe Core.Bool,
    -- | The name of the author of the commit that deletes the file. If no name is specified, the user's ARN is used as the author name and committer name.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFile' value with any optional fields omitted.
mkDeleteFile ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'branchName'
  Types.BranchName ->
  -- | 'filePath'
  Types.Path ->
  -- | 'parentCommitId'
  Types.CommitId ->
  DeleteFile
mkDeleteFile repositoryName branchName filePath parentCommitId =
  DeleteFile'
    { repositoryName,
      branchName,
      filePath,
      parentCommitId,
      commitMessage = Core.Nothing,
      email = Core.Nothing,
      keepEmptyFolders = Core.Nothing,
      name = Core.Nothing
    }

-- | The name of the repository that contains the file to delete.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfRepositoryName :: Lens.Lens' DeleteFile Types.RepositoryName
dfRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED dfRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The name of the branch where the commit that deletes the file is made.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfBranchName :: Lens.Lens' DeleteFile Types.BranchName
dfBranchName = Lens.field @"branchName"
{-# DEPRECATED dfBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

-- | The fully qualified path to the file that to be deleted, including the full name and extension of that file. For example, /examples/file.md is a fully qualified path to a file named file.md in a folder named examples.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFilePath :: Lens.Lens' DeleteFile Types.Path
dfFilePath = Lens.field @"filePath"
{-# DEPRECATED dfFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The ID of the commit that is the tip of the branch where you want to create the commit that deletes the file. This must be the HEAD commit for the branch. The commit that deletes the file is created from this commit ID.
--
-- /Note:/ Consider using 'parentCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfParentCommitId :: Lens.Lens' DeleteFile Types.CommitId
dfParentCommitId = Lens.field @"parentCommitId"
{-# DEPRECATED dfParentCommitId "Use generic-lens or generic-optics with 'parentCommitId' instead." #-}

-- | The commit message you want to include as part of deleting the file. Commit messages are limited to 256 KB. If no message is specified, a default message is used.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfCommitMessage :: Lens.Lens' DeleteFile (Core.Maybe Types.Message)
dfCommitMessage = Lens.field @"commitMessage"
{-# DEPRECATED dfCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | The email address for the commit that deletes the file. If no email address is specified, the email address is left blank.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfEmail :: Lens.Lens' DeleteFile (Core.Maybe Types.Email)
dfEmail = Lens.field @"email"
{-# DEPRECATED dfEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | If a file is the only object in the folder or directory, specifies whether to delete the folder or directory that contains the file. By default, empty folders are deleted. This includes empty folders that are part of the directory structure. For example, if the path to a file is dir1/dir2/dir3/dir4, and dir2 and dir3 are empty, deleting the last file in dir4 also deletes the empty folders dir4, dir3, and dir2.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfKeepEmptyFolders :: Lens.Lens' DeleteFile (Core.Maybe Core.Bool)
dfKeepEmptyFolders = Lens.field @"keepEmptyFolders"
{-# DEPRECATED dfKeepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead." #-}

-- | The name of the author of the commit that deletes the file. If no name is specified, the user's ARN is used as the author name and committer name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' DeleteFile (Core.Maybe Types.Name)
dfName = Lens.field @"name"
{-# DEPRECATED dfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteFile where
  toJSON DeleteFile {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("branchName" Core..= branchName),
            Core.Just ("filePath" Core..= filePath),
            Core.Just ("parentCommitId" Core..= parentCommitId),
            ("commitMessage" Core..=) Core.<$> commitMessage,
            ("email" Core..=) Core.<$> email,
            ("keepEmptyFolders" Core..=) Core.<$> keepEmptyFolders,
            ("name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest DeleteFile where
  type Rs DeleteFile = DeleteFileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.DeleteFile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFileResponse'
            Core.<$> (x Core..: "commitId")
            Core.<*> (x Core..: "blobId")
            Core.<*> (x Core..: "treeId")
            Core.<*> (x Core..: "filePath")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteFileResponse' smart constructor.
data DeleteFileResponse = DeleteFileResponse'
  { -- | The full commit ID of the commit that contains the change that deletes the file.
    commitId :: Types.CommitId,
    -- | The blob ID removed from the tree as part of deleting the file.
    blobId :: Types.BlobId,
    -- | The full SHA-1 pointer of the tree information for the commit that contains the delete file change.
    treeId :: Types.TreeId,
    -- | The fully qualified path to the file to be deleted, including the full name and extension of that file.
    filePath :: Types.Path,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFileResponse' value with any optional fields omitted.
mkDeleteFileResponse ::
  -- | 'commitId'
  Types.CommitId ->
  -- | 'blobId'
  Types.BlobId ->
  -- | 'treeId'
  Types.TreeId ->
  -- | 'filePath'
  Types.Path ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteFileResponse
mkDeleteFileResponse commitId blobId treeId filePath responseStatus =
  DeleteFileResponse'
    { commitId,
      blobId,
      treeId,
      filePath,
      responseStatus
    }

-- | The full commit ID of the commit that contains the change that deletes the file.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsCommitId :: Lens.Lens' DeleteFileResponse Types.CommitId
dfrrsCommitId = Lens.field @"commitId"
{-# DEPRECATED dfrrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The blob ID removed from the tree as part of deleting the file.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsBlobId :: Lens.Lens' DeleteFileResponse Types.BlobId
dfrrsBlobId = Lens.field @"blobId"
{-# DEPRECATED dfrrsBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

-- | The full SHA-1 pointer of the tree information for the commit that contains the delete file change.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsTreeId :: Lens.Lens' DeleteFileResponse Types.TreeId
dfrrsTreeId = Lens.field @"treeId"
{-# DEPRECATED dfrrsTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | The fully qualified path to the file to be deleted, including the full name and extension of that file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsFilePath :: Lens.Lens' DeleteFileResponse Types.Path
dfrrsFilePath = Lens.field @"filePath"
{-# DEPRECATED dfrrsFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsResponseStatus :: Lens.Lens' DeleteFileResponse Core.Int
dfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
