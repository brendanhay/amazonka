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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an update to the source branch of a pull request.
--
-- /See:/ 'mkPullRequestSourceReferenceUpdatedEventMetadata' smart constructor.
data PullRequestSourceReferenceUpdatedEventMetadata = PullRequestSourceReferenceUpdatedEventMetadata'
  { -- | The full commit ID of the commit in the source branch that was the tip of the branch at the time the pull request was updated.
    afterCommitId :: Lude.Maybe Lude.Text,
    -- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was updated.
    beforeCommitId :: Lude.Maybe Lude.Text,
    -- | The commit ID of the most recent commit that the source branch and the destination branch have in common.
    mergeBase :: Lude.Maybe Lude.Text,
    -- | The name of the repository where the pull request was updated.
    repositoryName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PullRequestSourceReferenceUpdatedEventMetadata' with the minimum fields required to make a request.
--
-- * 'afterCommitId' - The full commit ID of the commit in the source branch that was the tip of the branch at the time the pull request was updated.
-- * 'beforeCommitId' - The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was updated.
-- * 'mergeBase' - The commit ID of the most recent commit that the source branch and the destination branch have in common.
-- * 'repositoryName' - The name of the repository where the pull request was updated.
mkPullRequestSourceReferenceUpdatedEventMetadata ::
  PullRequestSourceReferenceUpdatedEventMetadata
mkPullRequestSourceReferenceUpdatedEventMetadata =
  PullRequestSourceReferenceUpdatedEventMetadata'
    { afterCommitId =
        Lude.Nothing,
      beforeCommitId = Lude.Nothing,
      mergeBase = Lude.Nothing,
      repositoryName = Lude.Nothing
    }

-- | The full commit ID of the commit in the source branch that was the tip of the branch at the time the pull request was updated.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsruemAfterCommitId :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Lude.Maybe Lude.Text)
prsruemAfterCommitId = Lens.lens (afterCommitId :: PullRequestSourceReferenceUpdatedEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {afterCommitId = a} :: PullRequestSourceReferenceUpdatedEventMetadata)
{-# DEPRECATED prsruemAfterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead." #-}

-- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was updated.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsruemBeforeCommitId :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Lude.Maybe Lude.Text)
prsruemBeforeCommitId = Lens.lens (beforeCommitId :: PullRequestSourceReferenceUpdatedEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {beforeCommitId = a} :: PullRequestSourceReferenceUpdatedEventMetadata)
{-# DEPRECATED prsruemBeforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead." #-}

-- | The commit ID of the most recent commit that the source branch and the destination branch have in common.
--
-- /Note:/ Consider using 'mergeBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsruemMergeBase :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Lude.Maybe Lude.Text)
prsruemMergeBase = Lens.lens (mergeBase :: PullRequestSourceReferenceUpdatedEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {mergeBase = a} :: PullRequestSourceReferenceUpdatedEventMetadata)
{-# DEPRECATED prsruemMergeBase "Use generic-lens or generic-optics with 'mergeBase' instead." #-}

-- | The name of the repository where the pull request was updated.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsruemRepositoryName :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Lude.Maybe Lude.Text)
prsruemRepositoryName = Lens.lens (repositoryName :: PullRequestSourceReferenceUpdatedEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: PullRequestSourceReferenceUpdatedEventMetadata)
{-# DEPRECATED prsruemRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance
  Lude.FromJSON
    PullRequestSourceReferenceUpdatedEventMetadata
  where
  parseJSON =
    Lude.withObject
      "PullRequestSourceReferenceUpdatedEventMetadata"
      ( \x ->
          PullRequestSourceReferenceUpdatedEventMetadata'
            Lude.<$> (x Lude..:? "afterCommitId")
            Lude.<*> (x Lude..:? "beforeCommitId")
            Lude.<*> (x Lude..:? "mergeBase")
            Lude.<*> (x Lude..:? "repositoryName")
      )
