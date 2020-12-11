-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestTarget
  ( PullRequestTarget (..),

    -- * Smart constructor
    mkPullRequestTarget,

    -- * Lenses
    prtSourceCommit,
    prtDestinationReference,
    prtMergeMetadata,
    prtMergeBase,
    prtDestinationCommit,
    prtRepositoryName,
    prtSourceReference,
  )
where

import Network.AWS.CodeCommit.Types.MergeMetadata
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a pull request target.
--
-- /See:/ 'mkPullRequestTarget' smart constructor.
data PullRequestTarget = PullRequestTarget'
  { sourceCommit ::
      Lude.Maybe Lude.Text,
    destinationReference :: Lude.Maybe Lude.Text,
    mergeMetadata :: Lude.Maybe MergeMetadata,
    mergeBase :: Lude.Maybe Lude.Text,
    destinationCommit :: Lude.Maybe Lude.Text,
    repositoryName :: Lude.Maybe Lude.Text,
    sourceReference :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PullRequestTarget' with the minimum fields required to make a request.
--
-- * 'destinationCommit' - The full commit ID that is the tip of the destination branch. This is the commit where the pull request was or will be merged.
-- * 'destinationReference' - The branch of the repository where the pull request changes are merged. Also known as the destination branch.
-- * 'mergeBase' - The commit ID of the most recent commit that the source branch and the destination branch have in common.
-- * 'mergeMetadata' - Returns metadata about the state of the merge, including whether the merge has been made.
-- * 'repositoryName' - The name of the repository that contains the pull request source and destination branches.
-- * 'sourceCommit' - The full commit ID of the tip of the source branch used to create the pull request. If the pull request branch is updated by a push while the pull request is open, the commit ID changes to reflect the new tip of the branch.
-- * 'sourceReference' - The branch of the repository that contains the changes for the pull request. Also known as the source branch.
mkPullRequestTarget ::
  PullRequestTarget
mkPullRequestTarget =
  PullRequestTarget'
    { sourceCommit = Lude.Nothing,
      destinationReference = Lude.Nothing,
      mergeMetadata = Lude.Nothing,
      mergeBase = Lude.Nothing,
      destinationCommit = Lude.Nothing,
      repositoryName = Lude.Nothing,
      sourceReference = Lude.Nothing
    }

-- | The full commit ID of the tip of the source branch used to create the pull request. If the pull request branch is updated by a push while the pull request is open, the commit ID changes to reflect the new tip of the branch.
--
-- /Note:/ Consider using 'sourceCommit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtSourceCommit :: Lens.Lens' PullRequestTarget (Lude.Maybe Lude.Text)
prtSourceCommit = Lens.lens (sourceCommit :: PullRequestTarget -> Lude.Maybe Lude.Text) (\s a -> s {sourceCommit = a} :: PullRequestTarget)
{-# DEPRECATED prtSourceCommit "Use generic-lens or generic-optics with 'sourceCommit' instead." #-}

-- | The branch of the repository where the pull request changes are merged. Also known as the destination branch.
--
-- /Note:/ Consider using 'destinationReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtDestinationReference :: Lens.Lens' PullRequestTarget (Lude.Maybe Lude.Text)
prtDestinationReference = Lens.lens (destinationReference :: PullRequestTarget -> Lude.Maybe Lude.Text) (\s a -> s {destinationReference = a} :: PullRequestTarget)
{-# DEPRECATED prtDestinationReference "Use generic-lens or generic-optics with 'destinationReference' instead." #-}

-- | Returns metadata about the state of the merge, including whether the merge has been made.
--
-- /Note:/ Consider using 'mergeMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtMergeMetadata :: Lens.Lens' PullRequestTarget (Lude.Maybe MergeMetadata)
prtMergeMetadata = Lens.lens (mergeMetadata :: PullRequestTarget -> Lude.Maybe MergeMetadata) (\s a -> s {mergeMetadata = a} :: PullRequestTarget)
{-# DEPRECATED prtMergeMetadata "Use generic-lens or generic-optics with 'mergeMetadata' instead." #-}

-- | The commit ID of the most recent commit that the source branch and the destination branch have in common.
--
-- /Note:/ Consider using 'mergeBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtMergeBase :: Lens.Lens' PullRequestTarget (Lude.Maybe Lude.Text)
prtMergeBase = Lens.lens (mergeBase :: PullRequestTarget -> Lude.Maybe Lude.Text) (\s a -> s {mergeBase = a} :: PullRequestTarget)
{-# DEPRECATED prtMergeBase "Use generic-lens or generic-optics with 'mergeBase' instead." #-}

-- | The full commit ID that is the tip of the destination branch. This is the commit where the pull request was or will be merged.
--
-- /Note:/ Consider using 'destinationCommit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtDestinationCommit :: Lens.Lens' PullRequestTarget (Lude.Maybe Lude.Text)
prtDestinationCommit = Lens.lens (destinationCommit :: PullRequestTarget -> Lude.Maybe Lude.Text) (\s a -> s {destinationCommit = a} :: PullRequestTarget)
{-# DEPRECATED prtDestinationCommit "Use generic-lens or generic-optics with 'destinationCommit' instead." #-}

-- | The name of the repository that contains the pull request source and destination branches.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtRepositoryName :: Lens.Lens' PullRequestTarget (Lude.Maybe Lude.Text)
prtRepositoryName = Lens.lens (repositoryName :: PullRequestTarget -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: PullRequestTarget)
{-# DEPRECATED prtRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch of the repository that contains the changes for the pull request. Also known as the source branch.
--
-- /Note:/ Consider using 'sourceReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtSourceReference :: Lens.Lens' PullRequestTarget (Lude.Maybe Lude.Text)
prtSourceReference = Lens.lens (sourceReference :: PullRequestTarget -> Lude.Maybe Lude.Text) (\s a -> s {sourceReference = a} :: PullRequestTarget)
{-# DEPRECATED prtSourceReference "Use generic-lens or generic-optics with 'sourceReference' instead." #-}

instance Lude.FromJSON PullRequestTarget where
  parseJSON =
    Lude.withObject
      "PullRequestTarget"
      ( \x ->
          PullRequestTarget'
            Lude.<$> (x Lude..:? "sourceCommit")
            Lude.<*> (x Lude..:? "destinationReference")
            Lude.<*> (x Lude..:? "mergeMetadata")
            Lude.<*> (x Lude..:? "mergeBase")
            Lude.<*> (x Lude..:? "destinationCommit")
            Lude.<*> (x Lude..:? "repositoryName")
            Lude.<*> (x Lude..:? "sourceReference")
      )
