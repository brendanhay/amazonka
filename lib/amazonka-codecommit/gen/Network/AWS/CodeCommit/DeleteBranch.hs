{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DeleteBranch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a branch from a repository, unless that branch is the default branch for the repository.
module Network.AWS.CodeCommit.DeleteBranch
  ( -- * Creating a request
    DeleteBranch (..),
    mkDeleteBranch,

    -- ** Request lenses
    dbRepositoryName,
    dbBranchName,

    -- * Destructuring the response
    DeleteBranchResponse (..),
    mkDeleteBranchResponse,

    -- ** Response lenses
    dbrrsDeletedBranch,
    dbrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a delete branch operation.
--
-- /See:/ 'mkDeleteBranch' smart constructor.
data DeleteBranch = DeleteBranch'
  { -- | The name of the repository that contains the branch to be deleted.
    repositoryName :: Types.RepositoryName,
    -- | The name of the branch to delete.
    branchName :: Types.BranchName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBranch' value with any optional fields omitted.
mkDeleteBranch ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'branchName'
  Types.BranchName ->
  DeleteBranch
mkDeleteBranch repositoryName branchName =
  DeleteBranch' {repositoryName, branchName}

-- | The name of the repository that contains the branch to be deleted.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbRepositoryName :: Lens.Lens' DeleteBranch Types.RepositoryName
dbRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED dbRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The name of the branch to delete.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBranchName :: Lens.Lens' DeleteBranch Types.BranchName
dbBranchName = Lens.field @"branchName"
{-# DEPRECATED dbBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

instance Core.FromJSON DeleteBranch where
  toJSON DeleteBranch {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("branchName" Core..= branchName)
          ]
      )

instance Core.AWSRequest DeleteBranch where
  type Rs DeleteBranch = DeleteBranchResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.DeleteBranch")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBranchResponse'
            Core.<$> (x Core..:? "deletedBranch")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a delete branch operation.
--
-- /See:/ 'mkDeleteBranchResponse' smart constructor.
data DeleteBranchResponse = DeleteBranchResponse'
  { -- | Information about the branch deleted by the operation, including the branch name and the commit ID that was the tip of the branch.
    deletedBranch :: Core.Maybe Types.BranchInfo,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBranchResponse' value with any optional fields omitted.
mkDeleteBranchResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteBranchResponse
mkDeleteBranchResponse responseStatus =
  DeleteBranchResponse'
    { deletedBranch = Core.Nothing,
      responseStatus
    }

-- | Information about the branch deleted by the operation, including the branch name and the commit ID that was the tip of the branch.
--
-- /Note:/ Consider using 'deletedBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsDeletedBranch :: Lens.Lens' DeleteBranchResponse (Core.Maybe Types.BranchInfo)
dbrrsDeletedBranch = Lens.field @"deletedBranch"
{-# DEPRECATED dbrrsDeletedBranch "Use generic-lens or generic-optics with 'deletedBranch' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsResponseStatus :: Lens.Lens' DeleteBranchResponse Core.Int
dbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
