-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata
  ( PullRequestCreatedEventMetadata (..),

    -- * Smart constructor
    mkPullRequestCreatedEventMetadata,

    -- * Lenses
    prcemDestinationCommitId,
    prcemMergeBase,
    prcemRepositoryName,
    prcemSourceCommitId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Metadata about the pull request that is used when comparing the pull request source with its destination.
--
-- /See:/ 'mkPullRequestCreatedEventMetadata' smart constructor.
data PullRequestCreatedEventMetadata = PullRequestCreatedEventMetadata'
  { destinationCommitId ::
      Lude.Maybe Lude.Text,
    mergeBase ::
      Lude.Maybe Lude.Text,
    repositoryName ::
      Lude.Maybe Lude.Text,
    sourceCommitId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PullRequestCreatedEventMetadata' with the minimum fields required to make a request.
--
-- * 'destinationCommitId' - The commit ID of the tip of the branch specified as the destination branch when the pull request was created.
-- * 'mergeBase' - The commit ID of the most recent commit that the source branch and the destination branch have in common.
-- * 'repositoryName' - The name of the repository where the pull request was created.
-- * 'sourceCommitId' - The commit ID on the source branch used when the pull request was created.
mkPullRequestCreatedEventMetadata ::
  PullRequestCreatedEventMetadata
mkPullRequestCreatedEventMetadata =
  PullRequestCreatedEventMetadata'
    { destinationCommitId =
        Lude.Nothing,
      mergeBase = Lude.Nothing,
      repositoryName = Lude.Nothing,
      sourceCommitId = Lude.Nothing
    }

-- | The commit ID of the tip of the branch specified as the destination branch when the pull request was created.
--
-- /Note:/ Consider using 'destinationCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcemDestinationCommitId :: Lens.Lens' PullRequestCreatedEventMetadata (Lude.Maybe Lude.Text)
prcemDestinationCommitId = Lens.lens (destinationCommitId :: PullRequestCreatedEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {destinationCommitId = a} :: PullRequestCreatedEventMetadata)
{-# DEPRECATED prcemDestinationCommitId "Use generic-lens or generic-optics with 'destinationCommitId' instead." #-}

-- | The commit ID of the most recent commit that the source branch and the destination branch have in common.
--
-- /Note:/ Consider using 'mergeBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcemMergeBase :: Lens.Lens' PullRequestCreatedEventMetadata (Lude.Maybe Lude.Text)
prcemMergeBase = Lens.lens (mergeBase :: PullRequestCreatedEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {mergeBase = a} :: PullRequestCreatedEventMetadata)
{-# DEPRECATED prcemMergeBase "Use generic-lens or generic-optics with 'mergeBase' instead." #-}

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcemRepositoryName :: Lens.Lens' PullRequestCreatedEventMetadata (Lude.Maybe Lude.Text)
prcemRepositoryName = Lens.lens (repositoryName :: PullRequestCreatedEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: PullRequestCreatedEventMetadata)
{-# DEPRECATED prcemRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The commit ID on the source branch used when the pull request was created.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcemSourceCommitId :: Lens.Lens' PullRequestCreatedEventMetadata (Lude.Maybe Lude.Text)
prcemSourceCommitId = Lens.lens (sourceCommitId :: PullRequestCreatedEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {sourceCommitId = a} :: PullRequestCreatedEventMetadata)
{-# DEPRECATED prcemSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

instance Lude.FromJSON PullRequestCreatedEventMetadata where
  parseJSON =
    Lude.withObject
      "PullRequestCreatedEventMetadata"
      ( \x ->
          PullRequestCreatedEventMetadata'
            Lude.<$> (x Lude..:? "destinationCommitId")
            Lude.<*> (x Lude..:? "mergeBase")
            Lude.<*> (x Lude..:? "repositoryName")
            Lude.<*> (x Lude..:? "sourceCommitId")
      )
