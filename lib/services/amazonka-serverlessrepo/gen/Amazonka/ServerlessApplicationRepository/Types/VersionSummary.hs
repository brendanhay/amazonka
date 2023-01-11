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
-- Module      : Amazonka.ServerlessApplicationRepository.Types.VersionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServerlessApplicationRepository.Types.VersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An application version summary.
--
-- /See:/ 'newVersionSummary' smart constructor.
data VersionSummary = VersionSummary'
  { -- | A link to a public repository for the source code of your application,
    -- for example the URL of a specific GitHub commit.
    sourceCodeUrl :: Prelude.Maybe Prelude.Text,
    -- | The date and time this resource was created.
    creationTime :: Prelude.Text,
    -- | The application Amazon Resource Name (ARN).
    applicationId :: Prelude.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceCodeUrl', 'versionSummary_sourceCodeUrl' - A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
--
-- 'creationTime', 'versionSummary_creationTime' - The date and time this resource was created.
--
-- 'applicationId', 'versionSummary_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'semanticVersion', 'versionSummary_semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/>
newVersionSummary ::
  -- | 'creationTime'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'semanticVersion'
  Prelude.Text ->
  VersionSummary
newVersionSummary
  pCreationTime_
  pApplicationId_
  pSemanticVersion_ =
    VersionSummary'
      { sourceCodeUrl = Prelude.Nothing,
        creationTime = pCreationTime_,
        applicationId = pApplicationId_,
        semanticVersion = pSemanticVersion_
      }

-- | A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
versionSummary_sourceCodeUrl :: Lens.Lens' VersionSummary (Prelude.Maybe Prelude.Text)
versionSummary_sourceCodeUrl = Lens.lens (\VersionSummary' {sourceCodeUrl} -> sourceCodeUrl) (\s@VersionSummary' {} a -> s {sourceCodeUrl = a} :: VersionSummary)

-- | The date and time this resource was created.
versionSummary_creationTime :: Lens.Lens' VersionSummary Prelude.Text
versionSummary_creationTime = Lens.lens (\VersionSummary' {creationTime} -> creationTime) (\s@VersionSummary' {} a -> s {creationTime = a} :: VersionSummary)

-- | The application Amazon Resource Name (ARN).
versionSummary_applicationId :: Lens.Lens' VersionSummary Prelude.Text
versionSummary_applicationId = Lens.lens (\VersionSummary' {applicationId} -> applicationId) (\s@VersionSummary' {} a -> s {applicationId = a} :: VersionSummary)

-- | The semantic version of the application:
--
-- <https://semver.org/>
versionSummary_semanticVersion :: Lens.Lens' VersionSummary Prelude.Text
versionSummary_semanticVersion = Lens.lens (\VersionSummary' {semanticVersion} -> semanticVersion) (\s@VersionSummary' {} a -> s {semanticVersion = a} :: VersionSummary)

instance Data.FromJSON VersionSummary where
  parseJSON =
    Data.withObject
      "VersionSummary"
      ( \x ->
          VersionSummary'
            Prelude.<$> (x Data..:? "sourceCodeUrl")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "applicationId")
            Prelude.<*> (x Data..: "semanticVersion")
      )

instance Prelude.Hashable VersionSummary where
  hashWithSalt _salt VersionSummary' {..} =
    _salt `Prelude.hashWithSalt` sourceCodeUrl
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` semanticVersion

instance Prelude.NFData VersionSummary where
  rnf VersionSummary' {..} =
    Prelude.rnf sourceCodeUrl
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf semanticVersion
