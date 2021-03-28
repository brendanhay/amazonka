{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergeBranchesByFastForward
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two branches using the fast-forward merge strategy.
module Network.AWS.CodeCommit.MergeBranchesByFastForward
    (
    -- * Creating a request
      MergeBranchesByFastForward (..)
    , mkMergeBranchesByFastForward
    -- ** Request lenses
    , mbbffRepositoryName
    , mbbffSourceCommitSpecifier
    , mbbffDestinationCommitSpecifier
    , mbbffTargetBranch

    -- * Destructuring the response
    , MergeBranchesByFastForwardResponse (..)
    , mkMergeBranchesByFastForwardResponse
    -- ** Response lenses
    , mbbffrrsCommitId
    , mbbffrrsTreeId
    , mbbffrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkMergeBranchesByFastForward' smart constructor.
data MergeBranchesByFastForward = MergeBranchesByFastForward'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository where you want to merge two branches.
  , sourceCommitSpecifier :: Types.SourceCommitSpecifier
    -- ^ The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
  , destinationCommitSpecifier :: Types.DestinationCommitSpecifier
    -- ^ The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
  , targetBranch :: Core.Maybe Types.TargetBranch
    -- ^ The branch where the merge is applied.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeBranchesByFastForward' value with any optional fields omitted.
mkMergeBranchesByFastForward
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.SourceCommitSpecifier -- ^ 'sourceCommitSpecifier'
    -> Types.DestinationCommitSpecifier -- ^ 'destinationCommitSpecifier'
    -> MergeBranchesByFastForward
mkMergeBranchesByFastForward repositoryName sourceCommitSpecifier
  destinationCommitSpecifier
  = MergeBranchesByFastForward'{repositoryName,
                                sourceCommitSpecifier, destinationCommitSpecifier,
                                targetBranch = Core.Nothing}

-- | The name of the repository where you want to merge two branches.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffRepositoryName :: Lens.Lens' MergeBranchesByFastForward Types.RepositoryName
mbbffRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE mbbffRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffSourceCommitSpecifier :: Lens.Lens' MergeBranchesByFastForward Types.SourceCommitSpecifier
mbbffSourceCommitSpecifier = Lens.field @"sourceCommitSpecifier"
{-# INLINEABLE mbbffSourceCommitSpecifier #-}
{-# DEPRECATED sourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead"  #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffDestinationCommitSpecifier :: Lens.Lens' MergeBranchesByFastForward Types.DestinationCommitSpecifier
mbbffDestinationCommitSpecifier = Lens.field @"destinationCommitSpecifier"
{-# INLINEABLE mbbffDestinationCommitSpecifier #-}
{-# DEPRECATED destinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead"  #-}

-- | The branch where the merge is applied.
--
-- /Note:/ Consider using 'targetBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffTargetBranch :: Lens.Lens' MergeBranchesByFastForward (Core.Maybe Types.TargetBranch)
mbbffTargetBranch = Lens.field @"targetBranch"
{-# INLINEABLE mbbffTargetBranch #-}
{-# DEPRECATED targetBranch "Use generic-lens or generic-optics with 'targetBranch' instead"  #-}

instance Core.ToQuery MergeBranchesByFastForward where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders MergeBranchesByFastForward where
        toHeaders MergeBranchesByFastForward{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.MergeBranchesByFastForward")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON MergeBranchesByFastForward where
        toJSON MergeBranchesByFastForward{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("sourceCommitSpecifier" Core..= sourceCommitSpecifier),
                  Core.Just
                    ("destinationCommitSpecifier" Core..= destinationCommitSpecifier),
                  ("targetBranch" Core..=) Core.<$> targetBranch])

instance Core.AWSRequest MergeBranchesByFastForward where
        type Rs MergeBranchesByFastForward =
             MergeBranchesByFastForwardResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 MergeBranchesByFastForwardResponse' Core.<$>
                   (x Core..:? "commitId") Core.<*> x Core..:? "treeId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkMergeBranchesByFastForwardResponse' smart constructor.
data MergeBranchesByFastForwardResponse = MergeBranchesByFastForwardResponse'
  { commitId :: Core.Maybe Types.CommitId
    -- ^ The commit ID of the merge in the destination or target branch.
  , treeId :: Core.Maybe Types.TreeId
    -- ^ The tree ID of the merge in the destination or target branch.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeBranchesByFastForwardResponse' value with any optional fields omitted.
mkMergeBranchesByFastForwardResponse
    :: Core.Int -- ^ 'responseStatus'
    -> MergeBranchesByFastForwardResponse
mkMergeBranchesByFastForwardResponse responseStatus
  = MergeBranchesByFastForwardResponse'{commitId = Core.Nothing,
                                        treeId = Core.Nothing, responseStatus}

-- | The commit ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffrrsCommitId :: Lens.Lens' MergeBranchesByFastForwardResponse (Core.Maybe Types.CommitId)
mbbffrrsCommitId = Lens.field @"commitId"
{-# INLINEABLE mbbffrrsCommitId #-}
{-# DEPRECATED commitId "Use generic-lens or generic-optics with 'commitId' instead"  #-}

-- | The tree ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffrrsTreeId :: Lens.Lens' MergeBranchesByFastForwardResponse (Core.Maybe Types.TreeId)
mbbffrrsTreeId = Lens.field @"treeId"
{-# INLINEABLE mbbffrrsTreeId #-}
{-# DEPRECATED treeId "Use generic-lens or generic-optics with 'treeId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffrrsResponseStatus :: Lens.Lens' MergeBranchesByFastForwardResponse Core.Int
mbbffrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mbbffrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
