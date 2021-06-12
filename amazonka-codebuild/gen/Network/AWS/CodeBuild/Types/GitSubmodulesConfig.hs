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
-- Module      : Network.AWS.CodeBuild.Types.GitSubmodulesConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.GitSubmodulesConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the Git submodules configuration for an AWS CodeBuild
-- build project.
--
-- /See:/ 'newGitSubmodulesConfig' smart constructor.
data GitSubmodulesConfig = GitSubmodulesConfig'
  { -- | Set to true to fetch Git submodules for your AWS CodeBuild build
    -- project.
    fetchSubmodules :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GitSubmodulesConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fetchSubmodules', 'gitSubmodulesConfig_fetchSubmodules' - Set to true to fetch Git submodules for your AWS CodeBuild build
-- project.
newGitSubmodulesConfig ::
  -- | 'fetchSubmodules'
  Core.Bool ->
  GitSubmodulesConfig
newGitSubmodulesConfig pFetchSubmodules_ =
  GitSubmodulesConfig'
    { fetchSubmodules =
        pFetchSubmodules_
    }

-- | Set to true to fetch Git submodules for your AWS CodeBuild build
-- project.
gitSubmodulesConfig_fetchSubmodules :: Lens.Lens' GitSubmodulesConfig Core.Bool
gitSubmodulesConfig_fetchSubmodules = Lens.lens (\GitSubmodulesConfig' {fetchSubmodules} -> fetchSubmodules) (\s@GitSubmodulesConfig' {} a -> s {fetchSubmodules = a} :: GitSubmodulesConfig)

instance Core.FromJSON GitSubmodulesConfig where
  parseJSON =
    Core.withObject
      "GitSubmodulesConfig"
      ( \x ->
          GitSubmodulesConfig'
            Core.<$> (x Core..: "fetchSubmodules")
      )

instance Core.Hashable GitSubmodulesConfig

instance Core.NFData GitSubmodulesConfig

instance Core.ToJSON GitSubmodulesConfig where
  toJSON GitSubmodulesConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("fetchSubmodules" Core..= fetchSubmodules)
          ]
      )
