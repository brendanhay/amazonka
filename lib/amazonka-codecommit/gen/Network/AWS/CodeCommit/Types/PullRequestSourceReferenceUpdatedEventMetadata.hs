{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
  ( PullRequestSourceReferenceUpdatedEventMetadata (..),

    -- * Smart constructor
    mkPullRequestSourceReferenceUpdatedEventMetadata,

    -- * Lenses
    prsruemAfterCommitId,
    prsruemBeforeCommitId,
    prsruemMergeBase,
    prsruemRepositoryName,
  )
where

import qualified Network.AWS.CodeCommit.Types.CommitId as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an update to the source branch of a pull request.
--
-- /See:/ 'mkPullRequestSourceReferenceUpdatedEventMetadata' smart constructor.
data PullRequestSourceReferenceUpdatedEventMetadata = PullRequestSourceReferenceUpdatedEventMetadata'
  { -- | The full commit ID of the commit in the source branch that was the tip of the branch at the time the pull request was updated.
    afterCommitId :: Core.Maybe Types.CommitId,
    -- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was updated.
    beforeCommitId :: Core.Maybe Types.CommitId,
    -- | The commit ID of the most recent commit that the source branch and the destination branch have in common.
    mergeBase :: Core.Maybe Types.CommitId,
    -- | The name of the repository where the pull request was updated.
    repositoryName :: Core.Maybe Types.RepositoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PullRequestSourceReferenceUpdatedEventMetadata' value with any optional fields omitted.
mkPullRequestSourceReferenceUpdatedEventMetadata ::
  PullRequestSourceReferenceUpdatedEventMetadata
mkPullRequestSourceReferenceUpdatedEventMetadata =
  PullRequestSourceReferenceUpdatedEventMetadata'
    { afterCommitId =
        Core.Nothing,
      beforeCommitId = Core.Nothing,
      mergeBase = Core.Nothing,
      repositoryName = Core.Nothing
    }

-- | The full commit ID of the commit in the source branch that was the tip of the branch at the time the pull request was updated.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsruemAfterCommitId :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Core.Maybe Types.CommitId)
prsruemAfterCommitId = Lens.field @"afterCommitId"
{-# DEPRECATED prsruemAfterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead." #-}

-- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was updated.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsruemBeforeCommitId :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Core.Maybe Types.CommitId)
prsruemBeforeCommitId = Lens.field @"beforeCommitId"
{-# DEPRECATED prsruemBeforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead." #-}

-- | The commit ID of the most recent commit that the source branch and the destination branch have in common.
--
-- /Note:/ Consider using 'mergeBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsruemMergeBase :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Core.Maybe Types.CommitId)
prsruemMergeBase = Lens.field @"mergeBase"
{-# DEPRECATED prsruemMergeBase "Use generic-lens or generic-optics with 'mergeBase' instead." #-}

-- | The name of the repository where the pull request was updated.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsruemRepositoryName :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Core.Maybe Types.RepositoryName)
prsruemRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED prsruemRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance
  Core.FromJSON
    PullRequestSourceReferenceUpdatedEventMetadata
  where
  parseJSON =
    Core.withObject "PullRequestSourceReferenceUpdatedEventMetadata" Core.$
      \x ->
        PullRequestSourceReferenceUpdatedEventMetadata'
          Core.<$> (x Core..:? "afterCommitId")
          Core.<*> (x Core..:? "beforeCommitId")
          Core.<*> (x Core..:? "mergeBase")
          Core.<*> (x Core..:? "repositoryName")
