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
-- Module      : Network.AWS.CodeBuild.Types.BuildGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildGroup where

import Network.AWS.CodeBuild.Types.BuildSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a batch build build group. Build groups are
-- used to combine builds that can run in parallel, while still being able
-- to set dependencies on other build groups.
--
-- /See:/ 'newBuildGroup' smart constructor.
data BuildGroup = BuildGroup'
  { -- | An array of strings that contain the identifiers of the build groups
    -- that this build group depends on.
    dependsOn :: Prelude.Maybe [Prelude.Text],
    -- | A @BuildSummary@ object that contains a summary of the current build
    -- group.
    currentBuildSummary :: Prelude.Maybe BuildSummary,
    -- | Contains the identifier of the build group.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies if failures in this build group can be ignored.
    ignoreFailure :: Prelude.Maybe Prelude.Bool,
    -- | An array of @BuildSummary@ objects that contain summaries of previous
    -- build groups.
    priorBuildSummaryList :: Prelude.Maybe [BuildSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'currentBuildSummary', 'buildGroup_currentBuildSummary' - A @BuildSummary@ object that contains a summary of the current build
-- group.
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
    { dependsOn = Prelude.Nothing,
      currentBuildSummary = Prelude.Nothing,
      identifier = Prelude.Nothing,
      ignoreFailure = Prelude.Nothing,
      priorBuildSummaryList = Prelude.Nothing
    }

-- | An array of strings that contain the identifiers of the build groups
-- that this build group depends on.
buildGroup_dependsOn :: Lens.Lens' BuildGroup (Prelude.Maybe [Prelude.Text])
buildGroup_dependsOn = Lens.lens (\BuildGroup' {dependsOn} -> dependsOn) (\s@BuildGroup' {} a -> s {dependsOn = a} :: BuildGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | A @BuildSummary@ object that contains a summary of the current build
-- group.
buildGroup_currentBuildSummary :: Lens.Lens' BuildGroup (Prelude.Maybe BuildSummary)
buildGroup_currentBuildSummary = Lens.lens (\BuildGroup' {currentBuildSummary} -> currentBuildSummary) (\s@BuildGroup' {} a -> s {currentBuildSummary = a} :: BuildGroup)

-- | Contains the identifier of the build group.
buildGroup_identifier :: Lens.Lens' BuildGroup (Prelude.Maybe Prelude.Text)
buildGroup_identifier = Lens.lens (\BuildGroup' {identifier} -> identifier) (\s@BuildGroup' {} a -> s {identifier = a} :: BuildGroup)

-- | Specifies if failures in this build group can be ignored.
buildGroup_ignoreFailure :: Lens.Lens' BuildGroup (Prelude.Maybe Prelude.Bool)
buildGroup_ignoreFailure = Lens.lens (\BuildGroup' {ignoreFailure} -> ignoreFailure) (\s@BuildGroup' {} a -> s {ignoreFailure = a} :: BuildGroup)

-- | An array of @BuildSummary@ objects that contain summaries of previous
-- build groups.
buildGroup_priorBuildSummaryList :: Lens.Lens' BuildGroup (Prelude.Maybe [BuildSummary])
buildGroup_priorBuildSummaryList = Lens.lens (\BuildGroup' {priorBuildSummaryList} -> priorBuildSummaryList) (\s@BuildGroup' {} a -> s {priorBuildSummaryList = a} :: BuildGroup) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON BuildGroup where
  parseJSON =
    Prelude.withObject
      "BuildGroup"
      ( \x ->
          BuildGroup'
            Prelude.<$> ( x Prelude..:? "dependsOn"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "currentBuildSummary")
            Prelude.<*> (x Prelude..:? "identifier")
            Prelude.<*> (x Prelude..:? "ignoreFailure")
            Prelude.<*> ( x Prelude..:? "priorBuildSummaryList"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BuildGroup

instance Prelude.NFData BuildGroup
