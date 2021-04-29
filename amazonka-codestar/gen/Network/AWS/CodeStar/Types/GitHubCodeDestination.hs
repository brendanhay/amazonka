{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.GitHubCodeDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.GitHubCodeDestination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the GitHub repository to be created in AWS CodeStar.
-- This is where the source code files provided with the project request
-- will be uploaded after project creation.
--
-- /See:/ 'newGitHubCodeDestination' smart constructor.
data GitHubCodeDestination = GitHubCodeDestination'
  { -- | Description for the GitHub repository to be created in AWS CodeStar.
    -- This description displays in GitHub after the repository is created.
    description :: Prelude.Maybe Prelude.Text,
    -- | Name of the GitHub repository to be created in AWS CodeStar.
    name :: Prelude.Text,
    -- | The type of GitHub repository to be created in AWS CodeStar. Valid
    -- values are User or Organization.
    type' :: Prelude.Text,
    -- | The GitHub username for the owner of the GitHub repository to be created
    -- in AWS CodeStar. If this repository should be owned by a GitHub
    -- organization, provide its name.
    owner :: Prelude.Text,
    -- | Whether the GitHub repository is to be a private repository.
    privateRepository :: Prelude.Bool,
    -- | Whether to enable issues for the GitHub repository.
    issuesEnabled :: Prelude.Bool,
    -- | The GitHub user\'s personal access token for the GitHub repository.
    token :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GitHubCodeDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'gitHubCodeDestination_description' - Description for the GitHub repository to be created in AWS CodeStar.
-- This description displays in GitHub after the repository is created.
--
-- 'name', 'gitHubCodeDestination_name' - Name of the GitHub repository to be created in AWS CodeStar.
--
-- 'type'', 'gitHubCodeDestination_type' - The type of GitHub repository to be created in AWS CodeStar. Valid
-- values are User or Organization.
--
-- 'owner', 'gitHubCodeDestination_owner' - The GitHub username for the owner of the GitHub repository to be created
-- in AWS CodeStar. If this repository should be owned by a GitHub
-- organization, provide its name.
--
-- 'privateRepository', 'gitHubCodeDestination_privateRepository' - Whether the GitHub repository is to be a private repository.
--
-- 'issuesEnabled', 'gitHubCodeDestination_issuesEnabled' - Whether to enable issues for the GitHub repository.
--
-- 'token', 'gitHubCodeDestination_token' - The GitHub user\'s personal access token for the GitHub repository.
newGitHubCodeDestination ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  -- | 'owner'
  Prelude.Text ->
  -- | 'privateRepository'
  Prelude.Bool ->
  -- | 'issuesEnabled'
  Prelude.Bool ->
  -- | 'token'
  Prelude.Text ->
  GitHubCodeDestination
newGitHubCodeDestination
  pName_
  pType_
  pOwner_
  pPrivateRepository_
  pIssuesEnabled_
  pToken_ =
    GitHubCodeDestination'
      { description =
          Prelude.Nothing,
        name = pName_,
        type' = pType_,
        owner = pOwner_,
        privateRepository = pPrivateRepository_,
        issuesEnabled = pIssuesEnabled_,
        token = Prelude._Sensitive Lens.# pToken_
      }

-- | Description for the GitHub repository to be created in AWS CodeStar.
-- This description displays in GitHub after the repository is created.
gitHubCodeDestination_description :: Lens.Lens' GitHubCodeDestination (Prelude.Maybe Prelude.Text)
gitHubCodeDestination_description = Lens.lens (\GitHubCodeDestination' {description} -> description) (\s@GitHubCodeDestination' {} a -> s {description = a} :: GitHubCodeDestination)

-- | Name of the GitHub repository to be created in AWS CodeStar.
gitHubCodeDestination_name :: Lens.Lens' GitHubCodeDestination Prelude.Text
gitHubCodeDestination_name = Lens.lens (\GitHubCodeDestination' {name} -> name) (\s@GitHubCodeDestination' {} a -> s {name = a} :: GitHubCodeDestination)

-- | The type of GitHub repository to be created in AWS CodeStar. Valid
-- values are User or Organization.
gitHubCodeDestination_type :: Lens.Lens' GitHubCodeDestination Prelude.Text
gitHubCodeDestination_type = Lens.lens (\GitHubCodeDestination' {type'} -> type') (\s@GitHubCodeDestination' {} a -> s {type' = a} :: GitHubCodeDestination)

-- | The GitHub username for the owner of the GitHub repository to be created
-- in AWS CodeStar. If this repository should be owned by a GitHub
-- organization, provide its name.
gitHubCodeDestination_owner :: Lens.Lens' GitHubCodeDestination Prelude.Text
gitHubCodeDestination_owner = Lens.lens (\GitHubCodeDestination' {owner} -> owner) (\s@GitHubCodeDestination' {} a -> s {owner = a} :: GitHubCodeDestination)

-- | Whether the GitHub repository is to be a private repository.
gitHubCodeDestination_privateRepository :: Lens.Lens' GitHubCodeDestination Prelude.Bool
gitHubCodeDestination_privateRepository = Lens.lens (\GitHubCodeDestination' {privateRepository} -> privateRepository) (\s@GitHubCodeDestination' {} a -> s {privateRepository = a} :: GitHubCodeDestination)

-- | Whether to enable issues for the GitHub repository.
gitHubCodeDestination_issuesEnabled :: Lens.Lens' GitHubCodeDestination Prelude.Bool
gitHubCodeDestination_issuesEnabled = Lens.lens (\GitHubCodeDestination' {issuesEnabled} -> issuesEnabled) (\s@GitHubCodeDestination' {} a -> s {issuesEnabled = a} :: GitHubCodeDestination)

-- | The GitHub user\'s personal access token for the GitHub repository.
gitHubCodeDestination_token :: Lens.Lens' GitHubCodeDestination Prelude.Text
gitHubCodeDestination_token = Lens.lens (\GitHubCodeDestination' {token} -> token) (\s@GitHubCodeDestination' {} a -> s {token = a} :: GitHubCodeDestination) Prelude.. Prelude._Sensitive

instance Prelude.Hashable GitHubCodeDestination

instance Prelude.NFData GitHubCodeDestination

instance Prelude.ToJSON GitHubCodeDestination where
  toJSON GitHubCodeDestination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("type" Prelude..= type'),
            Prelude.Just ("owner" Prelude..= owner),
            Prelude.Just
              ("privateRepository" Prelude..= privateRepository),
            Prelude.Just
              ("issuesEnabled" Prelude..= issuesEnabled),
            Prelude.Just ("token" Prelude..= token)
          ]
      )
