{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
  ( PullRequestMergedStateChangedEventMetadata (..),

    -- * Smart constructor
    mkPullRequestMergedStateChangedEventMetadata,

    -- * Lenses
    prmscemDestinationReference,
    prmscemMergeMetadata,
    prmscemRepositoryName,
  )
where

import Network.AWS.CodeCommit.Types.MergeMetadata
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about the change in the merge state for a pull request event.
--
-- /See:/ 'mkPullRequestMergedStateChangedEventMetadata' smart constructor.
data PullRequestMergedStateChangedEventMetadata = PullRequestMergedStateChangedEventMetadata'
  { destinationReference ::
      Lude.Maybe
        Lude.Text,
    mergeMetadata ::
      Lude.Maybe
        MergeMetadata,
    repositoryName ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PullRequestMergedStateChangedEventMetadata' with the minimum fields required to make a request.
--
-- * 'destinationReference' - The name of the branch that the pull request is merged into.
-- * 'mergeMetadata' - Information about the merge state change event.
-- * 'repositoryName' - The name of the repository where the pull request was created.
mkPullRequestMergedStateChangedEventMetadata ::
  PullRequestMergedStateChangedEventMetadata
mkPullRequestMergedStateChangedEventMetadata =
  PullRequestMergedStateChangedEventMetadata'
    { destinationReference =
        Lude.Nothing,
      mergeMetadata = Lude.Nothing,
      repositoryName = Lude.Nothing
    }

-- | The name of the branch that the pull request is merged into.
--
-- /Note:/ Consider using 'destinationReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prmscemDestinationReference :: Lens.Lens' PullRequestMergedStateChangedEventMetadata (Lude.Maybe Lude.Text)
prmscemDestinationReference = Lens.lens (destinationReference :: PullRequestMergedStateChangedEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {destinationReference = a} :: PullRequestMergedStateChangedEventMetadata)
{-# DEPRECATED prmscemDestinationReference "Use generic-lens or generic-optics with 'destinationReference' instead." #-}

-- | Information about the merge state change event.
--
-- /Note:/ Consider using 'mergeMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prmscemMergeMetadata :: Lens.Lens' PullRequestMergedStateChangedEventMetadata (Lude.Maybe MergeMetadata)
prmscemMergeMetadata = Lens.lens (mergeMetadata :: PullRequestMergedStateChangedEventMetadata -> Lude.Maybe MergeMetadata) (\s a -> s {mergeMetadata = a} :: PullRequestMergedStateChangedEventMetadata)
{-# DEPRECATED prmscemMergeMetadata "Use generic-lens or generic-optics with 'mergeMetadata' instead." #-}

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prmscemRepositoryName :: Lens.Lens' PullRequestMergedStateChangedEventMetadata (Lude.Maybe Lude.Text)
prmscemRepositoryName = Lens.lens (repositoryName :: PullRequestMergedStateChangedEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: PullRequestMergedStateChangedEventMetadata)
{-# DEPRECATED prmscemRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.FromJSON PullRequestMergedStateChangedEventMetadata where
  parseJSON =
    Lude.withObject
      "PullRequestMergedStateChangedEventMetadata"
      ( \x ->
          PullRequestMergedStateChangedEventMetadata'
            Lude.<$> (x Lude..:? "destinationReference")
            Lude.<*> (x Lude..:? "mergeMetadata")
            Lude.<*> (x Lude..:? "repositoryName")
      )
