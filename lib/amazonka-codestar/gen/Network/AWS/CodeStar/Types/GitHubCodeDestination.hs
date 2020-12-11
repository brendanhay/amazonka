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
    ghcdDescription,
    ghcdName,
    ghcdType,
    ghcdOwner,
    ghcdPrivateRepository,
    ghcdIssuesEnabled,
    ghcdToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
-- /See:/ 'mkGitHubCodeDestination' smart constructor.
data GitHubCodeDestination = GitHubCodeDestination'
  { description ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text,
    type' :: Lude.Text,
    owner :: Lude.Text,
    privateRepository :: Lude.Bool,
    issuesEnabled :: Lude.Bool,
    token :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GitHubCodeDestination' with the minimum fields required to make a request.
--
-- * 'description' - Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
-- * 'issuesEnabled' - Whether to enable issues for the GitHub repository.
-- * 'name' - Name of the GitHub repository to be created in AWS CodeStar.
-- * 'owner' - The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
-- * 'privateRepository' - Whether the GitHub repository is to be a private repository.
-- * 'token' - The GitHub user's personal access token for the GitHub repository.
-- * 'type'' - The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
mkGitHubCodeDestination ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  -- | 'owner'
  Lude.Text ->
  -- | 'privateRepository'
  Lude.Bool ->
  -- | 'issuesEnabled'
  Lude.Bool ->
  -- | 'token'
  Lude.Sensitive Lude.Text ->
  GitHubCodeDestination
mkGitHubCodeDestination
  pName_
  pType_
  pOwner_
  pPrivateRepository_
  pIssuesEnabled_
  pToken_ =
    GitHubCodeDestination'
      { description = Lude.Nothing,
        name = pName_,
        type' = pType_,
        owner = pOwner_,
        privateRepository = pPrivateRepository_,
        issuesEnabled = pIssuesEnabled_,
        token = pToken_
      }

-- | Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdDescription :: Lens.Lens' GitHubCodeDestination (Lude.Maybe Lude.Text)
ghcdDescription = Lens.lens (description :: GitHubCodeDestination -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Name of the GitHub repository to be created in AWS CodeStar.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdName :: Lens.Lens' GitHubCodeDestination Lude.Text
ghcdName = Lens.lens (name :: GitHubCodeDestination -> Lude.Text) (\s a -> s {name = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdType :: Lens.Lens' GitHubCodeDestination Lude.Text
ghcdType = Lens.lens (type' :: GitHubCodeDestination -> Lude.Text) (\s a -> s {type' = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdOwner :: Lens.Lens' GitHubCodeDestination Lude.Text
ghcdOwner = Lens.lens (owner :: GitHubCodeDestination -> Lude.Text) (\s a -> s {owner = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | Whether the GitHub repository is to be a private repository.
--
-- /Note:/ Consider using 'privateRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdPrivateRepository :: Lens.Lens' GitHubCodeDestination Lude.Bool
ghcdPrivateRepository = Lens.lens (privateRepository :: GitHubCodeDestination -> Lude.Bool) (\s a -> s {privateRepository = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdPrivateRepository "Use generic-lens or generic-optics with 'privateRepository' instead." #-}

-- | Whether to enable issues for the GitHub repository.
--
-- /Note:/ Consider using 'issuesEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdIssuesEnabled :: Lens.Lens' GitHubCodeDestination Lude.Bool
ghcdIssuesEnabled = Lens.lens (issuesEnabled :: GitHubCodeDestination -> Lude.Bool) (\s a -> s {issuesEnabled = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdIssuesEnabled "Use generic-lens or generic-optics with 'issuesEnabled' instead." #-}

-- | The GitHub user's personal access token for the GitHub repository.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcdToken :: Lens.Lens' GitHubCodeDestination (Lude.Sensitive Lude.Text)
ghcdToken = Lens.lens (token :: GitHubCodeDestination -> Lude.Sensitive Lude.Text) (\s a -> s {token = a} :: GitHubCodeDestination)
{-# DEPRECATED ghcdToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Lude.ToJSON GitHubCodeDestination where
  toJSON GitHubCodeDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("description" Lude..=) Lude.<$> description,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("type" Lude..= type'),
            Lude.Just ("owner" Lude..= owner),
            Lude.Just ("privateRepository" Lude..= privateRepository),
            Lude.Just ("issuesEnabled" Lude..= issuesEnabled),
            Lude.Just ("token" Lude..= token)
          ]
      )
