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
-- Module      : Amazonka.CodeBuild.Types.BuildGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BuildGroup where

import Amazonka.CodeBuild.Types.BuildSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a batch build build group. Build groups are
-- used to combine builds that can run in parallel, while still being able
-- to set dependencies on other build groups.
--
-- /See:/ 'newBuildGroup' smart constructor.
data BuildGroup = BuildGroup'
  { -- | An array of strings that contain the identifiers of the build groups
    -- that this build group depends on.
    dependsOn :: Prelude.Maybe [Prelude.Text],
    -- | An array of @BuildSummary@ objects that contain summaries of previous
    -- build groups.
    priorBuildSummaryList :: Prelude.Maybe [BuildSummary],
    -- | Specifies if failures in this build group can be ignored.
    ignoreFailure :: Prelude.Maybe Prelude.Bool,
    -- | Contains the identifier of the build group.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | A @BuildSummary@ object that contains a summary of the current build
    -- group.
    currentBuildSummary :: Prelude.Maybe BuildSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuildGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dependsOn', 'buildGroup_dependsOn' - An array of strings that contain the identifiers of the build groups
-- that this build group depends on.
--
-- 'priorBuildSummaryList', 'buildGroup_priorBuildSummaryList' - An array of @BuildSummary@ objects that contain summaries of previous
-- build groups.
--
-- 'ignoreFailure', 'buildGroup_ignoreFailure' - Specifies if failures in this build group can be ignored.
--
-- 'identifier', 'buildGroup_identifier' - Contains the identifier of the build group.
--
-- 'currentBuildSummary', 'buildGroup_currentBuildSummary' - A @BuildSummary@ object that contains a summary of the current build
-- group.
newBuildGroup ::
  BuildGroup
newBuildGroup =
  BuildGroup'
    { dependsOn = Prelude.Nothing,
      priorBuildSummaryList = Prelude.Nothing,
      ignoreFailure = Prelude.Nothing,
      identifier = Prelude.Nothing,
      currentBuildSummary = Prelude.Nothing
    }

-- | An array of strings that contain the identifiers of the build groups
-- that this build group depends on.
buildGroup_dependsOn :: Lens.Lens' BuildGroup (Prelude.Maybe [Prelude.Text])
buildGroup_dependsOn = Lens.lens (\BuildGroup' {dependsOn} -> dependsOn) (\s@BuildGroup' {} a -> s {dependsOn = a} :: BuildGroup) Prelude.. Lens.mapping Lens.coerced

-- | An array of @BuildSummary@ objects that contain summaries of previous
-- build groups.
buildGroup_priorBuildSummaryList :: Lens.Lens' BuildGroup (Prelude.Maybe [BuildSummary])
buildGroup_priorBuildSummaryList = Lens.lens (\BuildGroup' {priorBuildSummaryList} -> priorBuildSummaryList) (\s@BuildGroup' {} a -> s {priorBuildSummaryList = a} :: BuildGroup) Prelude.. Lens.mapping Lens.coerced

-- | Specifies if failures in this build group can be ignored.
buildGroup_ignoreFailure :: Lens.Lens' BuildGroup (Prelude.Maybe Prelude.Bool)
buildGroup_ignoreFailure = Lens.lens (\BuildGroup' {ignoreFailure} -> ignoreFailure) (\s@BuildGroup' {} a -> s {ignoreFailure = a} :: BuildGroup)

-- | Contains the identifier of the build group.
buildGroup_identifier :: Lens.Lens' BuildGroup (Prelude.Maybe Prelude.Text)
buildGroup_identifier = Lens.lens (\BuildGroup' {identifier} -> identifier) (\s@BuildGroup' {} a -> s {identifier = a} :: BuildGroup)

-- | A @BuildSummary@ object that contains a summary of the current build
-- group.
buildGroup_currentBuildSummary :: Lens.Lens' BuildGroup (Prelude.Maybe BuildSummary)
buildGroup_currentBuildSummary = Lens.lens (\BuildGroup' {currentBuildSummary} -> currentBuildSummary) (\s@BuildGroup' {} a -> s {currentBuildSummary = a} :: BuildGroup)

instance Data.FromJSON BuildGroup where
  parseJSON =
    Data.withObject
      "BuildGroup"
      ( \x ->
          BuildGroup'
            Prelude.<$> (x Data..:? "dependsOn" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "priorBuildSummaryList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ignoreFailure")
            Prelude.<*> (x Data..:? "identifier")
            Prelude.<*> (x Data..:? "currentBuildSummary")
      )

instance Prelude.Hashable BuildGroup where
  hashWithSalt _salt BuildGroup' {..} =
    _salt `Prelude.hashWithSalt` dependsOn
      `Prelude.hashWithSalt` priorBuildSummaryList
      `Prelude.hashWithSalt` ignoreFailure
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` currentBuildSummary

instance Prelude.NFData BuildGroup where
  rnf BuildGroup' {..} =
    Prelude.rnf dependsOn
      `Prelude.seq` Prelude.rnf priorBuildSummaryList
      `Prelude.seq` Prelude.rnf ignoreFailure
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf currentBuildSummary
