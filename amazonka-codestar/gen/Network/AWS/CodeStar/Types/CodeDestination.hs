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
-- Module      : Network.AWS.CodeStar.Types.CodeDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.CodeDestination where

import Network.AWS.CodeStar.Types.CodeCommitCodeDestination
import Network.AWS.CodeStar.Types.GitHubCodeDestination
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The repository to be created in AWS CodeStar. Valid values are AWS
-- CodeCommit or GitHub. After AWS CodeStar provisions the new repository,
-- the source code files provided with the project request are placed in
-- the repository.
--
-- /See:/ 'newCodeDestination' smart constructor.
data CodeDestination = CodeDestination'
  { -- | Information about the AWS CodeCommit repository to be created in AWS
    -- CodeStar. This is where the source code files provided with the project
    -- request will be uploaded after project creation.
    codeCommit :: Core.Maybe CodeCommitCodeDestination,
    -- | Information about the GitHub repository to be created in AWS CodeStar.
    -- This is where the source code files provided with the project request
    -- will be uploaded after project creation.
    gitHub :: Core.Maybe GitHubCodeDestination
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CodeDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeCommit', 'codeDestination_codeCommit' - Information about the AWS CodeCommit repository to be created in AWS
-- CodeStar. This is where the source code files provided with the project
-- request will be uploaded after project creation.
--
-- 'gitHub', 'codeDestination_gitHub' - Information about the GitHub repository to be created in AWS CodeStar.
-- This is where the source code files provided with the project request
-- will be uploaded after project creation.
newCodeDestination ::
  CodeDestination
newCodeDestination =
  CodeDestination'
    { codeCommit = Core.Nothing,
      gitHub = Core.Nothing
    }

-- | Information about the AWS CodeCommit repository to be created in AWS
-- CodeStar. This is where the source code files provided with the project
-- request will be uploaded after project creation.
codeDestination_codeCommit :: Lens.Lens' CodeDestination (Core.Maybe CodeCommitCodeDestination)
codeDestination_codeCommit = Lens.lens (\CodeDestination' {codeCommit} -> codeCommit) (\s@CodeDestination' {} a -> s {codeCommit = a} :: CodeDestination)

-- | Information about the GitHub repository to be created in AWS CodeStar.
-- This is where the source code files provided with the project request
-- will be uploaded after project creation.
codeDestination_gitHub :: Lens.Lens' CodeDestination (Core.Maybe GitHubCodeDestination)
codeDestination_gitHub = Lens.lens (\CodeDestination' {gitHub} -> gitHub) (\s@CodeDestination' {} a -> s {gitHub = a} :: CodeDestination)

instance Core.Hashable CodeDestination

instance Core.NFData CodeDestination

instance Core.ToJSON CodeDestination where
  toJSON CodeDestination' {..} =
    Core.object
      ( Core.catMaybes
          [ ("codeCommit" Core..=) Core.<$> codeCommit,
            ("gitHub" Core..=) Core.<$> gitHub
          ]
      )
