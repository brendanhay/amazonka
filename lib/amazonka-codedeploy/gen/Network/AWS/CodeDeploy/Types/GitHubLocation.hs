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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the location of application artifacts stored in GitHub.
--
-- /See:/ 'mkGitHubLocation' smart constructor.
data GitHubLocation = GitHubLocation'
  { commitId ::
      Lude.Maybe Lude.Text,
    repository :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GitHubLocation' with the minimum fields required to make a request.
--
-- * 'commitId' - The SHA1 commit ID of the GitHub commit that represents the bundled artifacts for the application revision.
-- * 'repository' - The GitHub account and repository pair that stores a reference to the commit that represents the bundled artifacts for the application revision.
--
-- Specified as account/repository.
mkGitHubLocation ::
  GitHubLocation
mkGitHubLocation =
  GitHubLocation'
    { commitId = Lude.Nothing,
      repository = Lude.Nothing
    }

-- | The SHA1 commit ID of the GitHub commit that represents the bundled artifacts for the application revision.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlCommitId :: Lens.Lens' GitHubLocation (Lude.Maybe Lude.Text)
ghlCommitId = Lens.lens (commitId :: GitHubLocation -> Lude.Maybe Lude.Text) (\s a -> s {commitId = a} :: GitHubLocation)
{-# DEPRECATED ghlCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The GitHub account and repository pair that stores a reference to the commit that represents the bundled artifacts for the application revision.
--
-- Specified as account/repository.
--
-- /Note:/ Consider using 'repository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlRepository :: Lens.Lens' GitHubLocation (Lude.Maybe Lude.Text)
ghlRepository = Lens.lens (repository :: GitHubLocation -> Lude.Maybe Lude.Text) (\s a -> s {repository = a} :: GitHubLocation)
{-# DEPRECATED ghlRepository "Use generic-lens or generic-optics with 'repository' instead." #-}

instance Lude.FromJSON GitHubLocation where
  parseJSON =
    Lude.withObject
      "GitHubLocation"
      ( \x ->
          GitHubLocation'
            Lude.<$> (x Lude..:? "commitId") Lude.<*> (x Lude..:? "repository")
      )

instance Lude.ToJSON GitHubLocation where
  toJSON GitHubLocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("commitId" Lude..=) Lude.<$> commitId,
            ("repository" Lude..=) Lude.<$> repository
          ]
      )
