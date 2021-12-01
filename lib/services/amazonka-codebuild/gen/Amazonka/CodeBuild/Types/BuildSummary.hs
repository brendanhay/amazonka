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
-- Module      : Amazonka.CodeBuild.Types.BuildSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BuildSummary where

import Amazonka.CodeBuild.Types.ResolvedArtifact
import Amazonka.CodeBuild.Types.StatusType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a batch build group.
--
-- /See:/ 'newBuildSummary' smart constructor.
data BuildSummary = BuildSummary'
  { -- | An array of @ResolvedArtifact@ objects that represents the secondary
    -- build artifacts for the build group.
    secondaryArtifacts :: Prelude.Maybe [ResolvedArtifact],
    -- | A @ResolvedArtifact@ object that represents the primary build artifacts
    -- for the build group.
    primaryArtifact :: Prelude.Maybe ResolvedArtifact,
    -- | The batch build ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the build group.
    --
    -- [FAILED]
    --     The build group failed.
    --
    -- [FAULT]
    --     The build group faulted.
    --
    -- [IN_PROGRESS]
    --     The build group is still in progress.
    --
    -- [STOPPED]
    --     The build group stopped.
    --
    -- [SUCCEEDED]
    --     The build group succeeded.
    --
    -- [TIMED_OUT]
    --     The build group timed out.
    buildStatus :: Prelude.Maybe StatusType,
    -- | When the build was started, expressed in Unix time format.
    requestedOn :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuildSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secondaryArtifacts', 'buildSummary_secondaryArtifacts' - An array of @ResolvedArtifact@ objects that represents the secondary
-- build artifacts for the build group.
--
-- 'primaryArtifact', 'buildSummary_primaryArtifact' - A @ResolvedArtifact@ object that represents the primary build artifacts
-- for the build group.
--
-- 'arn', 'buildSummary_arn' - The batch build ARN.
--
-- 'buildStatus', 'buildSummary_buildStatus' - The status of the build group.
--
-- [FAILED]
--     The build group failed.
--
-- [FAULT]
--     The build group faulted.
--
-- [IN_PROGRESS]
--     The build group is still in progress.
--
-- [STOPPED]
--     The build group stopped.
--
-- [SUCCEEDED]
--     The build group succeeded.
--
-- [TIMED_OUT]
--     The build group timed out.
--
-- 'requestedOn', 'buildSummary_requestedOn' - When the build was started, expressed in Unix time format.
newBuildSummary ::
  BuildSummary
newBuildSummary =
  BuildSummary'
    { secondaryArtifacts = Prelude.Nothing,
      primaryArtifact = Prelude.Nothing,
      arn = Prelude.Nothing,
      buildStatus = Prelude.Nothing,
      requestedOn = Prelude.Nothing
    }

-- | An array of @ResolvedArtifact@ objects that represents the secondary
-- build artifacts for the build group.
buildSummary_secondaryArtifacts :: Lens.Lens' BuildSummary (Prelude.Maybe [ResolvedArtifact])
buildSummary_secondaryArtifacts = Lens.lens (\BuildSummary' {secondaryArtifacts} -> secondaryArtifacts) (\s@BuildSummary' {} a -> s {secondaryArtifacts = a} :: BuildSummary) Prelude.. Lens.mapping Lens.coerced

-- | A @ResolvedArtifact@ object that represents the primary build artifacts
-- for the build group.
buildSummary_primaryArtifact :: Lens.Lens' BuildSummary (Prelude.Maybe ResolvedArtifact)
buildSummary_primaryArtifact = Lens.lens (\BuildSummary' {primaryArtifact} -> primaryArtifact) (\s@BuildSummary' {} a -> s {primaryArtifact = a} :: BuildSummary)

-- | The batch build ARN.
buildSummary_arn :: Lens.Lens' BuildSummary (Prelude.Maybe Prelude.Text)
buildSummary_arn = Lens.lens (\BuildSummary' {arn} -> arn) (\s@BuildSummary' {} a -> s {arn = a} :: BuildSummary)

-- | The status of the build group.
--
-- [FAILED]
--     The build group failed.
--
-- [FAULT]
--     The build group faulted.
--
-- [IN_PROGRESS]
--     The build group is still in progress.
--
-- [STOPPED]
--     The build group stopped.
--
-- [SUCCEEDED]
--     The build group succeeded.
--
-- [TIMED_OUT]
--     The build group timed out.
buildSummary_buildStatus :: Lens.Lens' BuildSummary (Prelude.Maybe StatusType)
buildSummary_buildStatus = Lens.lens (\BuildSummary' {buildStatus} -> buildStatus) (\s@BuildSummary' {} a -> s {buildStatus = a} :: BuildSummary)

-- | When the build was started, expressed in Unix time format.
buildSummary_requestedOn :: Lens.Lens' BuildSummary (Prelude.Maybe Prelude.UTCTime)
buildSummary_requestedOn = Lens.lens (\BuildSummary' {requestedOn} -> requestedOn) (\s@BuildSummary' {} a -> s {requestedOn = a} :: BuildSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON BuildSummary where
  parseJSON =
    Core.withObject
      "BuildSummary"
      ( \x ->
          BuildSummary'
            Prelude.<$> ( x Core..:? "secondaryArtifacts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "primaryArtifact")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "buildStatus")
            Prelude.<*> (x Core..:? "requestedOn")
      )

instance Prelude.Hashable BuildSummary where
  hashWithSalt salt' BuildSummary' {..} =
    salt' `Prelude.hashWithSalt` requestedOn
      `Prelude.hashWithSalt` buildStatus
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` primaryArtifact
      `Prelude.hashWithSalt` secondaryArtifacts

instance Prelude.NFData BuildSummary where
  rnf BuildSummary' {..} =
    Prelude.rnf secondaryArtifacts
      `Prelude.seq` Prelude.rnf requestedOn
      `Prelude.seq` Prelude.rnf buildStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf primaryArtifact
