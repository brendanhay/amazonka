{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.GitHubCodeDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeStar.Types.GitHubCodeDestination
  ( GitHubCodeDestination (..)
  -- * Smart constructor
  , mkGitHubCodeDestination
  -- * Lenses
  , ghcdName
  , ghcdType
  , ghcdOwner
  , ghcdPrivateRepository
  , ghcdIssuesEnabled
  , ghcdToken
  , ghcdDescription
  ) where

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
  { name :: Types.Name
    -- ^ Name of the GitHub repository to be created in AWS CodeStar.
  , type' :: Types.Type
    -- ^ The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
  , owner :: Types.Owner
    -- ^ The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
  , privateRepository :: Core.Bool
    -- ^ Whether the GitHub repository is to be a private repository.
  , issuesEnabled :: Core.Bool
    -- ^ Whether to enable issues for the GitHub repository.
  , token :: Types.Token
    -- ^ The GitHub user's personal access token for the GitHub repository.
  , description :: Core.Maybe Types.Description
    -- ^ Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GitHubCodeDestination' value with any optional fields omitted.
mkGitHubCodeDestination
    :: Types.Name -- ^ 'name'
    -> Types.Type -- ^ 'type\''
    -> Types.Owner -- ^ 'owner'
    -> Core.Bool -- ^ 'privateRepository'
    -> Core.Bool -- ^ 'issuesEnabled'
    -> Types.Token -- ^ 'token'
    -> GitHubCodeDestination
mkGitHubCodeDestination name type' owner privateRepository
  issuesEnabled token
  = GitHubCodeDestination'{name, type', owner, privateRepository,
                           issuesEnabled, token, description = Core.Nothing}

-- | Name of the GitHub repository to be created in AWS CodeStar.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdName :: Lens.Lens' GitHubCodeDestination Types.Name
ghcdName = Lens.field @"name"
{-# INLINEABLE ghcdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdType :: Lens.Lens' GitHubCodeDestination Types.Type
ghcdType = Lens.field @"type'"
{-# INLINEABLE ghcdType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdOwner :: Lens.Lens' GitHubCodeDestination Types.Owner
ghcdOwner = Lens.field @"owner"
{-# INLINEABLE ghcdOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | Whether the GitHub repository is to be a private repository.
--
-- /Note:/ Consider using 'privateRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdPrivateRepository :: Lens.Lens' GitHubCodeDestination Core.Bool
ghcdPrivateRepository = Lens.field @"privateRepository"
{-# INLINEABLE ghcdPrivateRepository #-}
{-# DEPRECATED privateRepository "Use generic-lens or generic-optics with 'privateRepository' instead"  #-}

-- | Whether to enable issues for the GitHub repository.
--
-- /Note:/ Consider using 'issuesEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdIssuesEnabled :: Lens.Lens' GitHubCodeDestination Core.Bool
ghcdIssuesEnabled = Lens.field @"issuesEnabled"
{-# INLINEABLE ghcdIssuesEnabled #-}
{-# DEPRECATED issuesEnabled "Use generic-lens or generic-optics with 'issuesEnabled' instead"  #-}

-- | The GitHub user's personal access token for the GitHub repository.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdToken :: Lens.Lens' GitHubCodeDestination Types.Token
ghcdToken = Lens.field @"token"
{-# INLINEABLE ghcdToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

-- | Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdDescription :: Lens.Lens' GitHubCodeDestination (Core.Maybe Types.Description)
ghcdDescription = Lens.field @"description"
{-# INLINEABLE ghcdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.FromJSON GitHubCodeDestination where
        toJSON GitHubCodeDestination{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name), Core.Just ("type" Core..= type'),
                  Core.Just ("owner" Core..= owner),
                  Core.Just ("privateRepository" Core..= privateRepository),
                  Core.Just ("issuesEnabled" Core..= issuesEnabled),
                  Core.Just ("token" Core..= token),
                  ("description" Core..=) Core.<$> description])
