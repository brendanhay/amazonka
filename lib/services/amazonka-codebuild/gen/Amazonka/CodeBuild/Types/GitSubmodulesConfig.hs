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
-- Module      : Amazonka.CodeBuild.Types.GitSubmodulesConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.GitSubmodulesConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the Git submodules configuration for an CodeBuild
-- build project.
--
-- /See:/ 'newGitSubmodulesConfig' smart constructor.
data GitSubmodulesConfig = GitSubmodulesConfig'
  { -- | Set to true to fetch Git submodules for your CodeBuild build project.
    fetchSubmodules :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GitSubmodulesConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fetchSubmodules', 'gitSubmodulesConfig_fetchSubmodules' - Set to true to fetch Git submodules for your CodeBuild build project.
newGitSubmodulesConfig ::
  -- | 'fetchSubmodules'
  Prelude.Bool ->
  GitSubmodulesConfig
newGitSubmodulesConfig pFetchSubmodules_ =
  GitSubmodulesConfig'
    { fetchSubmodules =
        pFetchSubmodules_
    }

-- | Set to true to fetch Git submodules for your CodeBuild build project.
gitSubmodulesConfig_fetchSubmodules :: Lens.Lens' GitSubmodulesConfig Prelude.Bool
gitSubmodulesConfig_fetchSubmodules = Lens.lens (\GitSubmodulesConfig' {fetchSubmodules} -> fetchSubmodules) (\s@GitSubmodulesConfig' {} a -> s {fetchSubmodules = a} :: GitSubmodulesConfig)

instance Data.FromJSON GitSubmodulesConfig where
  parseJSON =
    Data.withObject
      "GitSubmodulesConfig"
      ( \x ->
          GitSubmodulesConfig'
            Prelude.<$> (x Data..: "fetchSubmodules")
      )

instance Prelude.Hashable GitSubmodulesConfig where
  hashWithSalt _salt GitSubmodulesConfig' {..} =
    _salt `Prelude.hashWithSalt` fetchSubmodules

instance Prelude.NFData GitSubmodulesConfig where
  rnf GitSubmodulesConfig' {..} =
    Prelude.rnf fetchSubmodules

instance Data.ToJSON GitSubmodulesConfig where
  toJSON GitSubmodulesConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("fetchSubmodules" Data..= fetchSubmodules)
          ]
      )
