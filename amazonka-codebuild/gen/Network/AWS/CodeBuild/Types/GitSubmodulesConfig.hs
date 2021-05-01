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
-- Module      : Network.AWS.CodeBuild.Types.GitSubmodulesConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.GitSubmodulesConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the Git submodules configuration for an AWS CodeBuild
-- build project.
--
-- /See:/ 'newGitSubmodulesConfig' smart constructor.
data GitSubmodulesConfig = GitSubmodulesConfig'
  { -- | Set to true to fetch Git submodules for your AWS CodeBuild build
    -- project.
    fetchSubmodules :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Bool ->
  GitSubmodulesConfig
newGitSubmodulesConfig pFetchSubmodules_ =
  GitSubmodulesConfig'
    { fetchSubmodules =
        pFetchSubmodules_
    }

-- | Set to true to fetch Git submodules for your AWS CodeBuild build
-- project.
gitSubmodulesConfig_fetchSubmodules :: Lens.Lens' GitSubmodulesConfig Prelude.Bool
gitSubmodulesConfig_fetchSubmodules = Lens.lens (\GitSubmodulesConfig' {fetchSubmodules} -> fetchSubmodules) (\s@GitSubmodulesConfig' {} a -> s {fetchSubmodules = a} :: GitSubmodulesConfig)

instance Prelude.FromJSON GitSubmodulesConfig where
  parseJSON =
    Prelude.withObject
      "GitSubmodulesConfig"
      ( \x ->
          GitSubmodulesConfig'
            Prelude.<$> (x Prelude..: "fetchSubmodules")
      )

instance Prelude.Hashable GitSubmodulesConfig

instance Prelude.NFData GitSubmodulesConfig

instance Prelude.ToJSON GitSubmodulesConfig where
  toJSON GitSubmodulesConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("fetchSubmodules" Prelude..= fetchSubmodules)
          ]
      )
