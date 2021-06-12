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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the location of application artifacts stored in
-- GitHub.
--
-- /See:/ 'newGitHubLocation' smart constructor.
data GitHubLocation = GitHubLocation'
  { -- | The SHA1 commit ID of the GitHub commit that represents the bundled
    -- artifacts for the application revision.
    commitId :: Core.Maybe Core.Text,
    -- | The GitHub account and repository pair that stores a reference to the
    -- commit that represents the bundled artifacts for the application
    -- revision.
    --
    -- Specified as account\/repository.
    repository :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { commitId = Core.Nothing,
      repository = Core.Nothing
    }

-- | The SHA1 commit ID of the GitHub commit that represents the bundled
-- artifacts for the application revision.
gitHubLocation_commitId :: Lens.Lens' GitHubLocation (Core.Maybe Core.Text)
gitHubLocation_commitId = Lens.lens (\GitHubLocation' {commitId} -> commitId) (\s@GitHubLocation' {} a -> s {commitId = a} :: GitHubLocation)

-- | The GitHub account and repository pair that stores a reference to the
-- commit that represents the bundled artifacts for the application
-- revision.
--
-- Specified as account\/repository.
gitHubLocation_repository :: Lens.Lens' GitHubLocation (Core.Maybe Core.Text)
gitHubLocation_repository = Lens.lens (\GitHubLocation' {repository} -> repository) (\s@GitHubLocation' {} a -> s {repository = a} :: GitHubLocation)

instance Core.FromJSON GitHubLocation where
  parseJSON =
    Core.withObject
      "GitHubLocation"
      ( \x ->
          GitHubLocation'
            Core.<$> (x Core..:? "commitId")
            Core.<*> (x Core..:? "repository")
      )

instance Core.Hashable GitHubLocation

instance Core.NFData GitHubLocation

instance Core.ToJSON GitHubLocation where
  toJSON GitHubLocation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("commitId" Core..=) Core.<$> commitId,
            ("repository" Core..=) Core.<$> repository
          ]
      )
