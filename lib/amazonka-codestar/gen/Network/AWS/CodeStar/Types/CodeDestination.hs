-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.CodeDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.CodeDestination
  ( CodeDestination (..),

    -- * Smart constructor
    mkCodeDestination,

    -- * Lenses
    cdCodeCommit,
    cdGitHub,
  )
where

import Network.AWS.CodeStar.Types.CodeCommitCodeDestination
import Network.AWS.CodeStar.Types.GitHubCodeDestination
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
--
-- /See:/ 'mkCodeDestination' smart constructor.
data CodeDestination = CodeDestination'
  { codeCommit ::
      Lude.Maybe CodeCommitCodeDestination,
    gitHub :: Lude.Maybe GitHubCodeDestination
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeDestination' with the minimum fields required to make a request.
--
-- * 'codeCommit' - Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
-- * 'gitHub' - Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
mkCodeDestination ::
  CodeDestination
mkCodeDestination =
  CodeDestination'
    { codeCommit = Lude.Nothing,
      gitHub = Lude.Nothing
    }

-- | Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
-- /Note:/ Consider using 'codeCommit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCodeCommit :: Lens.Lens' CodeDestination (Lude.Maybe CodeCommitCodeDestination)
cdCodeCommit = Lens.lens (codeCommit :: CodeDestination -> Lude.Maybe CodeCommitCodeDestination) (\s a -> s {codeCommit = a} :: CodeDestination)
{-# DEPRECATED cdCodeCommit "Use generic-lens or generic-optics with 'codeCommit' instead." #-}

-- | Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
-- /Note:/ Consider using 'gitHub' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdGitHub :: Lens.Lens' CodeDestination (Lude.Maybe GitHubCodeDestination)
cdGitHub = Lens.lens (gitHub :: CodeDestination -> Lude.Maybe GitHubCodeDestination) (\s a -> s {gitHub = a} :: CodeDestination)
{-# DEPRECATED cdGitHub "Use generic-lens or generic-optics with 'gitHub' instead." #-}

instance Lude.ToJSON CodeDestination where
  toJSON CodeDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("codeCommit" Lude..=) Lude.<$> codeCommit,
            ("gitHub" Lude..=) Lude.<$> gitHub
          ]
      )
