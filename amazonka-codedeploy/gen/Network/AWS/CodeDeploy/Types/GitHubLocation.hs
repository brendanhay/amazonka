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
-- Module      : Network.AWS.CodeDeploy.Types.GitHubLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GitHubLocation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the location of application artifacts stored in
-- GitHub.
--
-- /See:/ 'newGitHubLocation' smart constructor.
data GitHubLocation = GitHubLocation'
  { -- | The SHA1 commit ID of the GitHub commit that represents the bundled
    -- artifacts for the application revision.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | The GitHub account and repository pair that stores a reference to the
    -- commit that represents the bundled artifacts for the application
    -- revision.
    --
    -- Specified as account\/repository.
    repository :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GitHubLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitId', 'gitHubLocation_commitId' - The SHA1 commit ID of the GitHub commit that represents the bundled
-- artifacts for the application revision.
--
-- 'repository', 'gitHubLocation_repository' - The GitHub account and repository pair that stores a reference to the
-- commit that represents the bundled artifacts for the application
-- revision.
--
-- Specified as account\/repository.
newGitHubLocation ::
  GitHubLocation
newGitHubLocation =
  GitHubLocation'
    { commitId = Prelude.Nothing,
      repository = Prelude.Nothing
    }

-- | The SHA1 commit ID of the GitHub commit that represents the bundled
-- artifacts for the application revision.
gitHubLocation_commitId :: Lens.Lens' GitHubLocation (Prelude.Maybe Prelude.Text)
gitHubLocation_commitId = Lens.lens (\GitHubLocation' {commitId} -> commitId) (\s@GitHubLocation' {} a -> s {commitId = a} :: GitHubLocation)

-- | The GitHub account and repository pair that stores a reference to the
-- commit that represents the bundled artifacts for the application
-- revision.
--
-- Specified as account\/repository.
gitHubLocation_repository :: Lens.Lens' GitHubLocation (Prelude.Maybe Prelude.Text)
gitHubLocation_repository = Lens.lens (\GitHubLocation' {repository} -> repository) (\s@GitHubLocation' {} a -> s {repository = a} :: GitHubLocation)

instance Prelude.FromJSON GitHubLocation where
  parseJSON =
    Prelude.withObject
      "GitHubLocation"
      ( \x ->
          GitHubLocation'
            Prelude.<$> (x Prelude..:? "commitId")
            Prelude.<*> (x Prelude..:? "repository")
      )

instance Prelude.Hashable GitHubLocation

instance Prelude.NFData GitHubLocation

instance Prelude.ToJSON GitHubLocation where
  toJSON GitHubLocation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("commitId" Prelude..=) Prelude.<$> commitId,
            ("repository" Prelude..=) Prelude.<$> repository
          ]
      )
