{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.GitHubCodeDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.GitHubCodeDestination
  ( GitHubCodeDestination (..),

    -- * Smart constructor
    mkGitHubCodeDestination,

    -- * Lenses
    ghcdName,
    ghcdType,
    ghcdOwner,
    ghcdPrivateRepository,
    ghcdIssuesEnabled,
    ghcdToken,
    ghcdDescription,
  )
where

import qualified Network.AWS.CodeStar.Types.Description as Types
import qualified Network.AWS.CodeStar.Types.Name as Types
import qualified Network.AWS.CodeStar.Types.Owner as Types
import qualified Network.AWS.CodeStar.Types.Token as Types
import qualified Network.AWS.CodeStar.Types.Type as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
-- /See:/ 'mkGitHubCodeDestination' smart constructor.
data GitHubCodeDestination = GitHubCodeDestination'
  { -- | Name of the GitHub repository to be created in AWS CodeStar.
    name :: Types.Name,
    -- | The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
    type' :: Types.Type,
    -- | The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
    owner :: Types.Owner,
    -- | Whether the GitHub repository is to be a private repository.
    privateRepository :: Core.Bool,
    -- | Whether to enable issues for the GitHub repository.
    issuesEnabled :: Core.Bool,
    -- | The GitHub user's personal access token for the GitHub repository.
    token :: Types.Token,
    -- | Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GitHubCodeDestination' value with any optional fields omitted.
mkGitHubCodeDestination ::
  -- | 'name'
  Types.Name ->
  -- | 'type\''
  Types.Type ->
  -- | 'owner'
  Types.Owner ->
  -- | 'privateRepository'
  Core.Bool ->
  -- | 'issuesEnabled'
  Core.Bool ->
  -- | 'token'
  Types.Token ->
  GitHubCodeDestination
mkGitHubCodeDestination
  name
  type'
  owner
  privateRepository
  issuesEnabled
  token =
    GitHubCodeDestination'
      { name,
        type',
        owner,
        privateRepository,
        issuesEnabled,
        token,
        description = Core.Nothing
      }

-- | Name of the GitHub repository to be created in AWS CodeStar.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdName :: Lens.Lens' GitHubCodeDestination Types.Name
ghcdName = Lens.field @"name"
{-# DEPRECATED ghcdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdType :: Lens.Lens' GitHubCodeDestination Types.Type
ghcdType = Lens.field @"type'"
{-# DEPRECATED ghcdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdOwner :: Lens.Lens' GitHubCodeDestination Types.Owner
ghcdOwner = Lens.field @"owner"
{-# DEPRECATED ghcdOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | Whether the GitHub repository is to be a private repository.
--
-- /Note:/ Consider using 'privateRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdPrivateRepository :: Lens.Lens' GitHubCodeDestination Core.Bool
ghcdPrivateRepository = Lens.field @"privateRepository"
{-# DEPRECATED ghcdPrivateRepository "Use generic-lens or generic-optics with 'privateRepository' instead." #-}

-- | Whether to enable issues for the GitHub repository.
--
-- /Note:/ Consider using 'issuesEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdIssuesEnabled :: Lens.Lens' GitHubCodeDestination Core.Bool
ghcdIssuesEnabled = Lens.field @"issuesEnabled"
{-# DEPRECATED ghcdIssuesEnabled "Use generic-lens or generic-optics with 'issuesEnabled' instead." #-}

-- | The GitHub user's personal access token for the GitHub repository.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdToken :: Lens.Lens' GitHubCodeDestination Types.Token
ghcdToken = Lens.field @"token"
{-# DEPRECATED ghcdToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdDescription :: Lens.Lens' GitHubCodeDestination (Core.Maybe Types.Description)
ghcdDescription = Lens.field @"description"
{-# DEPRECATED ghcdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON GitHubCodeDestination where
  toJSON GitHubCodeDestination {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("type" Core..= type'),
            Core.Just ("owner" Core..= owner),
            Core.Just ("privateRepository" Core..= privateRepository),
            Core.Just ("issuesEnabled" Core..= issuesEnabled),
            Core.Just ("token" Core..= token),
            ("description" Core..=) Core.<$> description
          ]
      )
