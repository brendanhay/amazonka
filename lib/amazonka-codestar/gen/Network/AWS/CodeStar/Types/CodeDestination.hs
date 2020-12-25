{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.CodeStar.Types.CodeCommitCodeDestination as Types
import qualified Network.AWS.CodeStar.Types.GitHubCodeDestination as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
--
-- /See:/ 'mkCodeDestination' smart constructor.
data CodeDestination = CodeDestination'
  { -- | Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
    codeCommit :: Core.Maybe Types.CodeCommitCodeDestination,
    -- | Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
    gitHub :: Core.Maybe Types.GitHubCodeDestination
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CodeDestination' value with any optional fields omitted.
mkCodeDestination ::
  CodeDestination
mkCodeDestination =
  CodeDestination'
    { codeCommit = Core.Nothing,
      gitHub = Core.Nothing
    }

-- | Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
-- /Note:/ Consider using 'codeCommit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCodeCommit :: Lens.Lens' CodeDestination (Core.Maybe Types.CodeCommitCodeDestination)
cdCodeCommit = Lens.field @"codeCommit"
{-# DEPRECATED cdCodeCommit "Use generic-lens or generic-optics with 'codeCommit' instead." #-}

-- | Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
-- /Note:/ Consider using 'gitHub' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdGitHub :: Lens.Lens' CodeDestination (Core.Maybe Types.GitHubCodeDestination)
cdGitHub = Lens.field @"gitHub"
{-# DEPRECATED cdGitHub "Use generic-lens or generic-optics with 'gitHub' instead." #-}

instance Core.FromJSON CodeDestination where
  toJSON CodeDestination {..} =
    Core.object
      ( Core.catMaybes
          [ ("codeCommit" Core..=) Core.<$> codeCommit,
            ("gitHub" Core..=) Core.<$> gitHub
          ]
      )
