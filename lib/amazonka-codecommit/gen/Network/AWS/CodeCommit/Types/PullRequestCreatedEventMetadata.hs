{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata
  ( PullRequestCreatedEventMetadata (..)
  -- * Smart constructor
  , mkPullRequestCreatedEventMetadata
  -- * Lenses
  , prcemDestinationCommitId
  , prcemMergeBase
  , prcemRepositoryName
  , prcemSourceCommitId
  ) where

import qualified Network.AWS.CodeCommit.Types.CommitId as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Metadata about the pull request that is used when comparing the pull request source with its destination.
--
-- /See:/ 'mkPullRequestCreatedEventMetadata' smart constructor.
data PullRequestCreatedEventMetadata = PullRequestCreatedEventMetadata'
  { destinationCommitId :: Core.Maybe Types.CommitId
    -- ^ The commit ID of the tip of the branch specified as the destination branch when the pull request was created.
  , mergeBase :: Core.Maybe Types.CommitId
    -- ^ The commit ID of the most recent commit that the source branch and the destination branch have in common.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The name of the repository where the pull request was created.
  , sourceCommitId :: Core.Maybe Types.CommitId
    -- ^ The commit ID on the source branch used when the pull request was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PullRequestCreatedEventMetadata' value with any optional fields omitted.
mkPullRequestCreatedEventMetadata
    :: PullRequestCreatedEventMetadata
mkPullRequestCreatedEventMetadata
  = PullRequestCreatedEventMetadata'{destinationCommitId =
                                       Core.Nothing,
                                     mergeBase = Core.Nothing, repositoryName = Core.Nothing,
                                     sourceCommitId = Core.Nothing}

-- | The commit ID of the tip of the branch specified as the destination branch when the pull request was created.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcemDestinationCommitId :: Lens.Lens' PullRequestCreatedEventMetadata (Core.Maybe Types.CommitId)
prcemDestinationCommitId = Lens.field @"destinationCommitId"
{-# INLINEABLE prcemDestinationCommitId #-}
{-# DEPRECATED destinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead"  #-}

-- | The commit ID of the most recent commit that the source branch and the destination branch have in common.
--
-- /Note:/ Consider using 'mergeBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcemMergeBase :: Lens.Lens' PullRequestCreatedEventMetadata (Core.Maybe Types.CommitId)
prcemMergeBase = Lens.field @"mergeBase"
{-# INLINEABLE prcemMergeBase #-}
{-# DEPRECATED mergeBase "Use generic-lens or generic-optics with 'mergeBase' instead"  #-}

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcemRepositoryName :: Lens.Lens' PullRequestCreatedEventMetadata (Core.Maybe Types.RepositoryName)
prcemRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE prcemRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The commit ID on the source branch used when the pull request was created.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcemSourceCommitId :: Lens.Lens' PullRequestCreatedEventMetadata (Core.Maybe Types.CommitId)
prcemSourceCommitId = Lens.field @"sourceCommitId"
{-# INLINEABLE prcemSourceCommitId #-}
{-# DEPRECATED sourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead"  #-}

instance Core.FromJSON PullRequestCreatedEventMetadata where
        parseJSON
          = Core.withObject "PullRequestCreatedEventMetadata" Core.$
              \ x ->
                PullRequestCreatedEventMetadata' Core.<$>
                  (x Core..:? "destinationCommitId") Core.<*> x Core..:? "mergeBase"
                    Core.<*> x Core..:? "repositoryName"
                    Core.<*> x Core..:? "sourceCommitId"
