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
-- Module      : Amazonka.MigrationHubStrategy.Types.SourceCodeRepository
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.SourceCodeRepository where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Object containing source code information that is linked to an
-- application component.
--
-- /See:/ 'newSourceCodeRepository' smart constructor.
data SourceCodeRepository = SourceCodeRepository'
  { -- | The branch of the source code.
    branch :: Prelude.Maybe Prelude.Text,
    -- | The type of repository to use for the source code.
    versionControlType :: Prelude.Maybe Prelude.Text,
    -- | The repository name for the source code.
    repository :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceCodeRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branch', 'sourceCodeRepository_branch' - The branch of the source code.
--
-- 'versionControlType', 'sourceCodeRepository_versionControlType' - The type of repository to use for the source code.
--
-- 'repository', 'sourceCodeRepository_repository' - The repository name for the source code.
newSourceCodeRepository ::
  SourceCodeRepository
newSourceCodeRepository =
  SourceCodeRepository'
    { branch = Prelude.Nothing,
      versionControlType = Prelude.Nothing,
      repository = Prelude.Nothing
    }

-- | The branch of the source code.
sourceCodeRepository_branch :: Lens.Lens' SourceCodeRepository (Prelude.Maybe Prelude.Text)
sourceCodeRepository_branch = Lens.lens (\SourceCodeRepository' {branch} -> branch) (\s@SourceCodeRepository' {} a -> s {branch = a} :: SourceCodeRepository)

-- | The type of repository to use for the source code.
sourceCodeRepository_versionControlType :: Lens.Lens' SourceCodeRepository (Prelude.Maybe Prelude.Text)
sourceCodeRepository_versionControlType = Lens.lens (\SourceCodeRepository' {versionControlType} -> versionControlType) (\s@SourceCodeRepository' {} a -> s {versionControlType = a} :: SourceCodeRepository)

-- | The repository name for the source code.
sourceCodeRepository_repository :: Lens.Lens' SourceCodeRepository (Prelude.Maybe Prelude.Text)
sourceCodeRepository_repository = Lens.lens (\SourceCodeRepository' {repository} -> repository) (\s@SourceCodeRepository' {} a -> s {repository = a} :: SourceCodeRepository)

instance Core.FromJSON SourceCodeRepository where
  parseJSON =
    Core.withObject
      "SourceCodeRepository"
      ( \x ->
          SourceCodeRepository'
            Prelude.<$> (x Core..:? "branch")
            Prelude.<*> (x Core..:? "versionControlType")
            Prelude.<*> (x Core..:? "repository")
      )

instance Prelude.Hashable SourceCodeRepository where
  hashWithSalt _salt SourceCodeRepository' {..} =
    _salt `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` versionControlType
      `Prelude.hashWithSalt` repository

instance Prelude.NFData SourceCodeRepository where
  rnf SourceCodeRepository' {..} =
    Prelude.rnf branch
      `Prelude.seq` Prelude.rnf versionControlType
      `Prelude.seq` Prelude.rnf repository
