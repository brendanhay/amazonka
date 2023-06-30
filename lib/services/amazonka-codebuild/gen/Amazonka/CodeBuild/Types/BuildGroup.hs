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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | A @BuildSummary@ object that contains a summary of the current build
    -- group.
    currentBuildSummary :: Prelude.Maybe BuildSummary,
    -- | An array of strings that contain the identifiers of the build groups
    -- that this build group depends on.
    dependsOn :: Prelude.Maybe [Prelude.Text],
    -- | Contains the identifier of the build group.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies if failures in this build group can be ignored.
    ignoreFailure :: Prelude.Maybe Prelude.Bool,
    -- | An array of @BuildSummary@ objects that contain summaries of previous
    -- build groups.
    priorBuildSummaryList :: Prelude.Maybe [BuildSummary]
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
-- 'currentBuildSummary', 'buildGroup_currentBuildSummary' - A @BuildSummary@ object that contains a summary of the current build
-- group.
--
-- 'dependsOn', 'buildGroup_dependsOn' - An array of strings that contain the identifiers of the build groups
-- that this build group depends on.
--
-- 'identifier', 'buildGroup_identifier' - Contains the identifier of the build group.
--
-- 'ignoreFailure', 'buildGroup_ignoreFailure' - Specifies if failures in this build group can be ignored.
--
-- 'priorBuildSummaryList', 'buildGroup_priorBuildSummaryList' - An array of @BuildSummary@ objects that contain summaries of previous
-- build groups.
newBuildGroup ::
  BuildGroup
newBuildGroup =
  BuildGroup'
    { currentBuildSummary = Prelude.Nothing,
      dependsOn = Prelude.Nothing,
      identifier = Prelude.Nothing,
      ignoreFailure = Prelude.Nothing,
      priorBuildSummaryList = Prelude.Nothing
    }

-- | A @BuildSummary@ object that contains a summary of the current build
-- group.
buildGroup_currentBuildSummary :: Lens.Lens' BuildGroup (Prelude.Maybe BuildSummary)
buildGroup_currentBuildSummary = Lens.lens (\BuildGroup' {currentBuildSummary} -> currentBuildSummary) (\s@BuildGroup' {} a -> s {currentBuildSummary = a} :: BuildGroup)

-- | An array of strings that contain the identifiers of the build groups
-- that this build group depends on.
buildGroup_dependsOn :: Lens.Lens' BuildGroup (Prelude.Maybe [Prelude.Text])
buildGroup_dependsOn = Lens.lens (\BuildGroup' {dependsOn} -> dependsOn) (\s@BuildGroup' {} a -> s {dependsOn = a} :: BuildGroup) Prelude.. Lens.mapping Lens.coerced

-- | Contains the identifier of the build group.
buildGroup_identifier :: Lens.Lens' BuildGroup (Prelude.Maybe Prelude.Text)
buildGroup_identifier = Lens.lens (\BuildGroup' {identifier} -> identifier) (\s@BuildGroup' {} a -> s {identifier = a} :: BuildGroup)

-- | Specifies if failures in this build group can be ignored.
buildGroup_ignoreFailure :: Lens.Lens' BuildGroup (Prelude.Maybe Prelude.Bool)
buildGroup_ignoreFailure = Lens.lens (\BuildGroup' {ignoreFailure} -> ignoreFailure) (\s@BuildGroup' {} a -> s {ignoreFailure = a} :: BuildGroup)

-- | An array of @BuildSummary@ objects that contain summaries of previous
-- build groups.
buildGroup_priorBuildSummaryList :: Lens.Lens' BuildGroup (Prelude.Maybe [BuildSummary])
buildGroup_priorBuildSummaryList = Lens.lens (\BuildGroup' {priorBuildSummaryList} -> priorBuildSummaryList) (\s@BuildGroup' {} a -> s {priorBuildSummaryList = a} :: BuildGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BuildGroup where
  parseJSON =
    Data.withObject
      "BuildGroup"
      ( \x ->
          BuildGroup'
            Prelude.<$> (x Data..:? "currentBuildSummary")
            Prelude.<*> (x Data..:? "dependsOn" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "identifier")
            Prelude.<*> (x Data..:? "ignoreFailure")
            Prelude.<*> ( x
                            Data..:? "priorBuildSummaryList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BuildGroup where
  hashWithSalt _salt BuildGroup' {..} =
    _salt
      `Prelude.hashWithSalt` currentBuildSummary
      `Prelude.hashWithSalt` dependsOn
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` ignoreFailure
      `Prelude.hashWithSalt` priorBuildSummaryList

instance Prelude.NFData BuildGroup where
  rnf BuildGroup' {..} =
    Prelude.rnf currentBuildSummary
      `Prelude.seq` Prelude.rnf dependsOn
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf ignoreFailure
      `Prelude.seq` Prelude.rnf priorBuildSummaryList
