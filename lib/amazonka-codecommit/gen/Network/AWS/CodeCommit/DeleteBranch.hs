{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteBranch (..)
    , mkDeleteBranch
    -- ** Request lenses
    , dbRepositoryName
    , dbBranchName

    -- * Destructuring the response
    , DeleteBranchResponse (..)
    , mkDeleteBranchResponse
    -- ** Response lenses
    , dbrrsDeletedBranch
    , dbrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a delete branch operation.
--
-- /See:/ 'mkDeleteBranch' smart constructor.
data DeleteBranch = DeleteBranch'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository that contains the branch to be deleted.
  , branchName :: Types.BranchName
    -- ^ The name of the branch to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBranch' value with any optional fields omitted.
mkDeleteBranch
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.BranchName -- ^ 'branchName'
    -> DeleteBranch
mkDeleteBranch repositoryName branchName
  = DeleteBranch'{repositoryName, branchName}

-- | The name of the repository that contains the branch to be deleted.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbRepositoryName :: Lens.Lens' DeleteBranch Types.RepositoryName
dbRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE dbRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The name of the branch to delete.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBranchName :: Lens.Lens' DeleteBranch Types.BranchName
dbBranchName = Lens.field @"branchName"
{-# INLINEABLE dbBranchName #-}
{-# DEPRECATED branchName "Use generic-lens or generic-optics with 'branchName' instead"  #-}

instance Core.ToQuery DeleteBranch where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBranch where
        toHeaders DeleteBranch{..}
          = Core.pure ("X-Amz-Target", "CodeCommit_20150413.DeleteBranch")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteBranch where
        toJSON DeleteBranch{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("branchName" Core..= branchName)])

instance Core.AWSRequest DeleteBranch where
        type Rs DeleteBranch = DeleteBranchResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteBranchResponse' Core.<$>
                   (x Core..:? "deletedBranch") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a delete branch operation.
--
-- /See:/ 'mkDeleteBranchResponse' smart constructor.
data DeleteBranchResponse = DeleteBranchResponse'
  { deletedBranch :: Core.Maybe Types.BranchInfo
    -- ^ Information about the branch deleted by the operation, including the branch name and the commit ID that was the tip of the branch.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBranchResponse' value with any optional fields omitted.
mkDeleteBranchResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteBranchResponse
mkDeleteBranchResponse responseStatus
  = DeleteBranchResponse'{deletedBranch = Core.Nothing,
                          responseStatus}

-- | Information about the branch deleted by the operation, including the branch name and the commit ID that was the tip of the branch.
--
-- /Note:/ Consider using 'deletedBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsDeletedBranch :: Lens.Lens' DeleteBranchResponse (Core.Maybe Types.BranchInfo)
dbrrsDeletedBranch = Lens.field @"deletedBranch"
{-# INLINEABLE dbrrsDeletedBranch #-}
{-# DEPRECATED deletedBranch "Use generic-lens or generic-optics with 'deletedBranch' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsResponseStatus :: Lens.Lens' DeleteBranchResponse Core.Int
dbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
