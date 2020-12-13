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
    ghcdPrivateRepository,
    ghcdToken,
    ghcdOwner,
    ghcdName,
    ghcdIssuesEnabled,
    ghcdType,
    ghcdDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
-- /See:/ 'mkGitHubCodeDestination' smart constructor.
data GitHubCodeDestination = GitHubCodeDestination'
  { -- | Whether the GitHub repository is to be a private repository.
    privateRepository :: Lude.Bool,
    -- | The GitHub user's personal access token for the GitHub repository.
    token :: Lude.Sensitive Lude.Text,
    -- | The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
    owner :: Lude.Text,
    -- | Name of the GitHub repository to be created in AWS CodeStar.
    name :: Lude.Text,
    -- | Whether to enable issues for the GitHub repository.
    issuesEnabled :: Lude.Bool,
    -- | The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
    type' :: Lude.Text,
    -- | Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GitHubCodeDestination' with the minimum fields required to make a request.
--
-- * 'privateRepository' - Whether the GitHub repository is to be a private repository.
-- * 'token' - The GitHub user's personal access token for the GitHub repository.
-- * 'owner' - The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
-- * 'name' - Name of the GitHub repository to be created in AWS CodeStar.
-- * 'issuesEnabled' - Whether to enable issues for the GitHub repository.
-- * 'type'' - The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
-- * 'description' - Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
mkGitHubCodeDestination ::
  -- | 'privateRepository'
  Lude.Bool ->
  -- | 'token'
  Lude.Sensitive Lude.Text ->
  -- | 'owner'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'issuesEnabled'
  Lude.Bool ->
  -- | 'type''
  Lude.Text ->
  GitHubCodeDestination
mkGitHubCodeDestination
  pPrivateRepository_
  pToken_
  pOwner_
  pName_
  pIssuesEnabled_
  pType_ =
    GitHubCodeDestination'
      { privateRepository = pPrivateRepository_,
        token = pToken_,
        owner = pOwner_,
        name = pName_,
        issuesEnabled = pIssuesEnabled_,
        type' = pType_,
        description = Lude.Nothing
      }

-- | Whether the GitHub repository is to be a private repository.
--
-- /Note:/ Consider using 'privateRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdPrivateRepository :: Lens.Lens' GitHubCodeDestination Lude.Bool
ghcdPrivateRepository = Lens.lens (privateRepository :: GitHubCodeDestination -> Lude.Bool) (\s a -> s {privateRepository = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdPrivateRepository "Use generic-lens or generic-optics with 'privateRepository' instead." #-}

-- | The GitHub user's personal access token for the GitHub repository.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdToken :: Lens.Lens' GitHubCodeDestination (Lude.Sensitive Lude.Text)
ghcdToken = Lens.lens (token :: GitHubCodeDestination -> Lude.Sensitive Lude.Text) (\s a -> s {token = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdOwner :: Lens.Lens' GitHubCodeDestination Lude.Text
ghcdOwner = Lens.lens (owner :: GitHubCodeDestination -> Lude.Text) (\s a -> s {owner = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | Name of the GitHub repository to be created in AWS CodeStar.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdName :: Lens.Lens' GitHubCodeDestination Lude.Text
ghcdName = Lens.lens (name :: GitHubCodeDestination -> Lude.Text) (\s a -> s {name = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Whether to enable issues for the GitHub repository.
--
-- /Note:/ Consider using 'issuesEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdIssuesEnabled :: Lens.Lens' GitHubCodeDestination Lude.Bool
ghcdIssuesEnabled = Lens.lens (issuesEnabled :: GitHubCodeDestination -> Lude.Bool) (\s a -> s {issuesEnabled = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdIssuesEnabled "Use generic-lens or generic-optics with 'issuesEnabled' instead." #-}

-- | The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdType :: Lens.Lens' GitHubCodeDestination Lude.Text
ghcdType = Lens.lens (type' :: GitHubCodeDestination -> Lude.Text) (\s a -> s {type' = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdDescription :: Lens.Lens' GitHubCodeDestination (Lude.Maybe Lude.Text)
ghcdDescription = Lens.lens (description :: GitHubCodeDestination -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.ToJSON GitHubCodeDestination where
  toJSON GitHubCodeDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("privateRepository" Lude..= privateRepository),
            Lude.Just ("token" Lude..= token),
            Lude.Just ("owner" Lude..= owner),
            Lude.Just ("name" Lude..= name),
            Lude.Just ("issuesEnabled" Lude..= issuesEnabled),
            Lude.Just ("type" Lude..= type'),
            ("description" Lude..=) Lude.<$> description
          ]
      )
