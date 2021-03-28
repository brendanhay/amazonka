{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.PullRequestTarget
  ( PullRequestTarget (..)
  -- * Smart constructor
  , mkPullRequestTarget
  -- * Lenses
  , prtDestinationCommit
  , prtDestinationReference
  , prtMergeBase
  , prtMergeMetadata
  , prtRepositoryName
  , prtSourceCommit
  , prtSourceReference
  ) where

import qualified Network.AWS.CodeCommit.Types.CommitId as Types
import qualified Network.AWS.CodeCommit.Types.DestinationReference as Types
import qualified Network.AWS.CodeCommit.Types.MergeMetadata as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryName as Types
import qualified Network.AWS.CodeCommit.Types.SourceReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a pull request target.
--
-- /See:/ 'mkPullRequestTarget' smart constructor.
data PullRequestTarget = PullRequestTarget'
  { destinationCommit :: Core.Maybe Types.CommitId
    -- ^ The full commit ID that is the tip of the destination branch. This is the commit where the pull request was or will be merged.
  , destinationReference :: Core.Maybe Types.DestinationReference
    -- ^ The branch of the repository where the pull request changes are merged. Also known as the destination branch. 
  , mergeBase :: Core.Maybe Types.CommitId
    -- ^ The commit ID of the most recent commit that the source branch and the destination branch have in common.
  , mergeMetadata :: Core.Maybe Types.MergeMetadata
    -- ^ Returns metadata about the state of the merge, including whether the merge has been made.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The name of the repository that contains the pull request source and destination branches.
  , sourceCommit :: Core.Maybe Types.CommitId
    -- ^ The full commit ID of the tip of the source branch used to create the pull request. If the pull request branch is updated by a push while the pull request is open, the commit ID changes to reflect the new tip of the branch.
  , sourceReference :: Core.Maybe Types.SourceReference
    -- ^ The branch of the repository that contains the changes for the pull request. Also known as the source branch.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PullRequestTarget' value with any optional fields omitted.
mkPullRequestTarget
    :: PullRequestTarget
mkPullRequestTarget
  = PullRequestTarget'{destinationCommit = Core.Nothing,
                       destinationReference = Core.Nothing, mergeBase = Core.Nothing,
                       mergeMetadata = Core.Nothing, repositoryName = Core.Nothing,
                       sourceCommit = Core.Nothing, sourceReference = Core.Nothing}

-- | The full commit ID that is the tip of the destination branch. This is the commit where the pull request was or will be merged.
--
-- /Note:/ Consider using 'destinationCommit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtDestinationCommit :: Lens.Lens' PullRequestTarget (Core.Maybe Types.CommitId)
prtDestinationCommit = Lens.field @"destinationCommit"
{-# INLINEABLE prtDestinationCommit #-}
{-# DEPRECATED destinationCommit "Use generic-lens or generic-optics with 'destinationCommit' instead"  #-}

-- | The branch of the repository where the pull request changes are merged. Also known as the destination branch. 
--
-- /Note:/ Consider using 'destinationReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtDestinationReference :: Lens.Lens' PullRequestTarget (Core.Maybe Types.DestinationReference)
prtDestinationReference = Lens.field @"destinationReference"
{-# INLINEABLE prtDestinationReference #-}
{-# DEPRECATED destinationReference "Use generic-lens or generic-optics with 'destinationReference' instead"  #-}

-- | The commit ID of the most recent commit that the source branch and the destination branch have in common.
--
-- /Note:/ Consider using 'mergeBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtMergeBase :: Lens.Lens' PullRequestTarget (Core.Maybe Types.CommitId)
prtMergeBase = Lens.field @"mergeBase"
{-# INLINEABLE prtMergeBase #-}
{-# DEPRECATED mergeBase "Use generic-lens or generic-optics with 'mergeBase' instead"  #-}

-- | Returns metadata about the state of the merge, including whether the merge has been made.
--
-- /Note:/ Consider using 'mergeMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtMergeMetadata :: Lens.Lens' PullRequestTarget (Core.Maybe Types.MergeMetadata)
prtMergeMetadata = Lens.field @"mergeMetadata"
{-# INLINEABLE prtMergeMetadata #-}
{-# DEPRECATED mergeMetadata "Use generic-lens or generic-optics with 'mergeMetadata' instead"  #-}

-- | The name of the repository that contains the pull request source and destination branches.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtRepositoryName :: Lens.Lens' PullRequestTarget (Core.Maybe Types.RepositoryName)
prtRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE prtRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The full commit ID of the tip of the source branch used to create the pull request. If the pull request branch is updated by a push while the pull request is open, the commit ID changes to reflect the new tip of the branch.
--
-- /Note:/ Consider using 'sourceCommit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtSourceCommit :: Lens.Lens' PullRequestTarget (Core.Maybe Types.CommitId)
prtSourceCommit = Lens.field @"sourceCommit"
{-# INLINEABLE prtSourceCommit #-}
{-# DEPRECATED sourceCommit "Use generic-lens or generic-optics with 'sourceCommit' instead"  #-}

-- | The branch of the repository that contains the changes for the pull request. Also known as the source branch.
--
-- /Note:/ Consider using 'sourceReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtSourceReference :: Lens.Lens' PullRequestTarget (Core.Maybe Types.SourceReference)
prtSourceReference = Lens.field @"sourceReference"
{-# INLINEABLE prtSourceReference #-}
{-# DEPRECATED sourceReference "Use generic-lens or generic-optics with 'sourceReference' instead"  #-}

instance Core.FromJSON PullRequestTarget where
        parseJSON
          = Core.withObject "PullRequestTarget" Core.$
              \ x ->
                PullRequestTarget' Core.<$>
                  (x Core..:? "destinationCommit") Core.<*>
                    x Core..:? "destinationReference"
                    Core.<*> x Core..:? "mergeBase"
                    Core.<*> x Core..:? "mergeMetadata"
                    Core.<*> x Core..:? "repositoryName"
                    Core.<*> x Core..:? "sourceCommit"
                    Core.<*> x Core..:? "sourceReference"
