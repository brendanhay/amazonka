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

import qualified Network.AWS.CodeCommit.Types.DestinationReference as Types
import qualified Network.AWS.CodeCommit.Types.MergeMetadata as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about the change in the merge state for a pull request event.
--
-- /See:/ 'mkPullRequestMergedStateChangedEventMetadata' smart constructor.
data PullRequestMergedStateChangedEventMetadata = PullRequestMergedStateChangedEventMetadata'
  { -- | The name of the branch that the pull request is merged into.
    destinationReference :: Core.Maybe Types.DestinationReference,
    -- | Information about the merge state change event.
    mergeMetadata :: Core.Maybe Types.MergeMetadata,
    -- | The name of the repository where the pull request was created.
    repositoryName :: Core.Maybe Types.RepositoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PullRequestMergedStateChangedEventMetadata' value with any optional fields omitted.
mkPullRequestMergedStateChangedEventMetadata ::
  PullRequestMergedStateChangedEventMetadata
mkPullRequestMergedStateChangedEventMetadata =
  PullRequestMergedStateChangedEventMetadata'
    { destinationReference =
        Core.Nothing,
      mergeMetadata = Core.Nothing,
      repositoryName = Core.Nothing
    }

-- | The name of the branch that the pull request is merged into.
--
-- /Note:/ Consider using 'destinationReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prmscemDestinationReference :: Lens.Lens' PullRequestMergedStateChangedEventMetadata (Core.Maybe Types.DestinationReference)
prmscemDestinationReference = Lens.field @"destinationReference"
{-# DEPRECATED prmscemDestinationReference "Use generic-lens or generic-optics with 'destinationReference' instead." #-}

-- | Information about the merge state change event.
--
-- /Note:/ Consider using 'mergeMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prmscemMergeMetadata :: Lens.Lens' PullRequestMergedStateChangedEventMetadata (Core.Maybe Types.MergeMetadata)
prmscemMergeMetadata = Lens.field @"mergeMetadata"
{-# DEPRECATED prmscemMergeMetadata "Use generic-lens or generic-optics with 'mergeMetadata' instead." #-}

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prmscemRepositoryName :: Lens.Lens' PullRequestMergedStateChangedEventMetadata (Core.Maybe Types.RepositoryName)
prmscemRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED prmscemRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Core.FromJSON PullRequestMergedStateChangedEventMetadata where
  parseJSON =
    Core.withObject "PullRequestMergedStateChangedEventMetadata" Core.$
      \x ->
        PullRequestMergedStateChangedEventMetadata'
          Core.<$> (x Core..:? "destinationReference")
          Core.<*> (x Core..:? "mergeMetadata")
          Core.<*> (x Core..:? "repositoryName")
