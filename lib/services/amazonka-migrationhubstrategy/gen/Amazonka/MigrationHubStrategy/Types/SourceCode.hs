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
-- Module      : Amazonka.MigrationHubStrategy.Types.SourceCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.SourceCode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.VersionControl
import qualified Amazonka.Prelude as Prelude

-- | Object containing source code information that is linked to an
-- application component.
--
-- /See:/ 'newSourceCode' smart constructor.
data SourceCode = SourceCode'
  { -- | The repository name for the source code.
    location :: Prelude.Maybe Prelude.Text,
    -- | The name of the project.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The branch of the source code.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of repository to use for the source code.
    versionControl :: Prelude.Maybe VersionControl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'sourceCode_location' - The repository name for the source code.
--
-- 'projectName', 'sourceCode_projectName' - The name of the project.
--
-- 'sourceVersion', 'sourceCode_sourceVersion' - The branch of the source code.
--
-- 'versionControl', 'sourceCode_versionControl' - The type of repository to use for the source code.
newSourceCode ::
  SourceCode
newSourceCode =
  SourceCode'
    { location = Prelude.Nothing,
      projectName = Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      versionControl = Prelude.Nothing
    }

-- | The repository name for the source code.
sourceCode_location :: Lens.Lens' SourceCode (Prelude.Maybe Prelude.Text)
sourceCode_location = Lens.lens (\SourceCode' {location} -> location) (\s@SourceCode' {} a -> s {location = a} :: SourceCode)

-- | The name of the project.
sourceCode_projectName :: Lens.Lens' SourceCode (Prelude.Maybe Prelude.Text)
sourceCode_projectName = Lens.lens (\SourceCode' {projectName} -> projectName) (\s@SourceCode' {} a -> s {projectName = a} :: SourceCode)

-- | The branch of the source code.
sourceCode_sourceVersion :: Lens.Lens' SourceCode (Prelude.Maybe Prelude.Text)
sourceCode_sourceVersion = Lens.lens (\SourceCode' {sourceVersion} -> sourceVersion) (\s@SourceCode' {} a -> s {sourceVersion = a} :: SourceCode)

-- | The type of repository to use for the source code.
sourceCode_versionControl :: Lens.Lens' SourceCode (Prelude.Maybe VersionControl)
sourceCode_versionControl = Lens.lens (\SourceCode' {versionControl} -> versionControl) (\s@SourceCode' {} a -> s {versionControl = a} :: SourceCode)

instance Prelude.Hashable SourceCode where
  hashWithSalt _salt SourceCode' {..} =
    _salt `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` versionControl

instance Prelude.NFData SourceCode where
  rnf SourceCode' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf versionControl

instance Data.ToJSON SourceCode where
  toJSON SourceCode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("location" Data..=) Prelude.<$> location,
            ("projectName" Data..=) Prelude.<$> projectName,
            ("sourceVersion" Data..=) Prelude.<$> sourceVersion,
            ("versionControl" Data..=)
              Prelude.<$> versionControl
          ]
      )
