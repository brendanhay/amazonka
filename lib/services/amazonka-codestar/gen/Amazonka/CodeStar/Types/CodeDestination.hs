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
-- Module      : Amazonka.CodeStar.Types.CodeDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Types.CodeDestination where

import Amazonka.CodeStar.Types.CodeCommitCodeDestination
import Amazonka.CodeStar.Types.GitHubCodeDestination
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The repository to be created in AWS CodeStar. Valid values are AWS
-- CodeCommit or GitHub. After AWS CodeStar provisions the new repository,
-- the source code files provided with the project request are placed in
-- the repository.
--
-- /See:/ 'newCodeDestination' smart constructor.
data CodeDestination = CodeDestination'
  { -- | Information about the GitHub repository to be created in AWS CodeStar.
    -- This is where the source code files provided with the project request
    -- will be uploaded after project creation.
    gitHub :: Prelude.Maybe GitHubCodeDestination,
    -- | Information about the AWS CodeCommit repository to be created in AWS
    -- CodeStar. This is where the source code files provided with the project
    -- request will be uploaded after project creation.
    codeCommit :: Prelude.Maybe CodeCommitCodeDestination
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gitHub', 'codeDestination_gitHub' - Information about the GitHub repository to be created in AWS CodeStar.
-- This is where the source code files provided with the project request
-- will be uploaded after project creation.
--
-- 'codeCommit', 'codeDestination_codeCommit' - Information about the AWS CodeCommit repository to be created in AWS
-- CodeStar. This is where the source code files provided with the project
-- request will be uploaded after project creation.
newCodeDestination ::
  CodeDestination
newCodeDestination =
  CodeDestination'
    { gitHub = Prelude.Nothing,
      codeCommit = Prelude.Nothing
    }

-- | Information about the GitHub repository to be created in AWS CodeStar.
-- This is where the source code files provided with the project request
-- will be uploaded after project creation.
codeDestination_gitHub :: Lens.Lens' CodeDestination (Prelude.Maybe GitHubCodeDestination)
codeDestination_gitHub = Lens.lens (\CodeDestination' {gitHub} -> gitHub) (\s@CodeDestination' {} a -> s {gitHub = a} :: CodeDestination)

-- | Information about the AWS CodeCommit repository to be created in AWS
-- CodeStar. This is where the source code files provided with the project
-- request will be uploaded after project creation.
codeDestination_codeCommit :: Lens.Lens' CodeDestination (Prelude.Maybe CodeCommitCodeDestination)
codeDestination_codeCommit = Lens.lens (\CodeDestination' {codeCommit} -> codeCommit) (\s@CodeDestination' {} a -> s {codeCommit = a} :: CodeDestination)

instance Prelude.Hashable CodeDestination where
  hashWithSalt _salt CodeDestination' {..} =
    _salt `Prelude.hashWithSalt` gitHub
      `Prelude.hashWithSalt` codeCommit

instance Prelude.NFData CodeDestination where
  rnf CodeDestination' {..} =
    Prelude.rnf gitHub
      `Prelude.seq` Prelude.rnf codeCommit

instance Data.ToJSON CodeDestination where
  toJSON CodeDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("gitHub" Data..=) Prelude.<$> gitHub,
            ("codeCommit" Data..=) Prelude.<$> codeCommit
          ]
      )
