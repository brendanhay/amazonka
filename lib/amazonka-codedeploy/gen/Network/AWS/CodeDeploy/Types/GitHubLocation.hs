{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.GitHubLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GitHubLocation
  ( GitHubLocation (..),

    -- * Smart constructor
    mkGitHubLocation,

    -- * Lenses
    ghlCommitId,
    ghlRepository,
  )
where

import qualified Network.AWS.CodeDeploy.Types.CommitId as Types
import qualified Network.AWS.CodeDeploy.Types.Repository as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the location of application artifacts stored in GitHub.
--
-- /See:/ 'mkGitHubLocation' smart constructor.
data GitHubLocation = GitHubLocation'
  { -- | The SHA1 commit ID of the GitHub commit that represents the bundled artifacts for the application revision.
    commitId :: Core.Maybe Types.CommitId,
    -- | The GitHub account and repository pair that stores a reference to the commit that represents the bundled artifacts for the application revision.
    --
    -- Specified as account/repository.
    repository :: Core.Maybe Types.Repository
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GitHubLocation' value with any optional fields omitted.
mkGitHubLocation ::
  GitHubLocation
mkGitHubLocation =
  GitHubLocation'
    { commitId = Core.Nothing,
      repository = Core.Nothing
    }

-- | The SHA1 commit ID of the GitHub commit that represents the bundled artifacts for the application revision.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlCommitId :: Lens.Lens' GitHubLocation (Core.Maybe Types.CommitId)
ghlCommitId = Lens.field @"commitId"
{-# DEPRECATED ghlCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The GitHub account and repository pair that stores a reference to the commit that represents the bundled artifacts for the application revision.
--
-- Specified as account/repository.
--
-- /Note:/ Consider using 'repository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlRepository :: Lens.Lens' GitHubLocation (Core.Maybe Types.Repository)
ghlRepository = Lens.field @"repository"
{-# DEPRECATED ghlRepository "Use generic-lens or generic-optics with 'repository' instead." #-}

instance Core.FromJSON GitHubLocation where
  toJSON GitHubLocation {..} =
    Core.object
      ( Core.catMaybes
          [ ("commitId" Core..=) Core.<$> commitId,
            ("repository" Core..=) Core.<$> repository
          ]
      )

instance Core.FromJSON GitHubLocation where
  parseJSON =
    Core.withObject "GitHubLocation" Core.$
      \x ->
        GitHubLocation'
          Core.<$> (x Core..:? "commitId") Core.<*> (x Core..:? "repository")
