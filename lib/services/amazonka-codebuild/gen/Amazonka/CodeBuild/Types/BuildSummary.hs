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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BuildSummary where

import Amazonka.CodeBuild.Types.ResolvedArtifact
import Amazonka.CodeBuild.Types.StatusType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a batch build group.
--
-- /See:/ 'newBuildSummary' smart constructor.
data BuildSummary = BuildSummary'
  { -- | The batch build ARN.
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
    -- | A @ResolvedArtifact@ object that represents the primary build artifacts
    -- for the build group.
    primaryArtifact :: Prelude.Maybe ResolvedArtifact,
    -- | When the build was started, expressed in Unix time format.
    requestedOn :: Prelude.Maybe Data.POSIX,
    -- | An array of @ResolvedArtifact@ objects that represents the secondary
    -- build artifacts for the build group.
    secondaryArtifacts :: Prelude.Maybe [ResolvedArtifact]
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
-- 'primaryArtifact', 'buildSummary_primaryArtifact' - A @ResolvedArtifact@ object that represents the primary build artifacts
-- for the build group.
--
-- 'requestedOn', 'buildSummary_requestedOn' - When the build was started, expressed in Unix time format.
--
-- 'secondaryArtifacts', 'buildSummary_secondaryArtifacts' - An array of @ResolvedArtifact@ objects that represents the secondary
-- build artifacts for the build group.
newBuildSummary ::
  BuildSummary
newBuildSummary =
  BuildSummary'
    { arn = Prelude.Nothing,
      buildStatus = Prelude.Nothing,
      primaryArtifact = Prelude.Nothing,
      requestedOn = Prelude.Nothing,
      secondaryArtifacts = Prelude.Nothing
    }

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

-- | A @ResolvedArtifact@ object that represents the primary build artifacts
-- for the build group.
buildSummary_primaryArtifact :: Lens.Lens' BuildSummary (Prelude.Maybe ResolvedArtifact)
buildSummary_primaryArtifact = Lens.lens (\BuildSummary' {primaryArtifact} -> primaryArtifact) (\s@BuildSummary' {} a -> s {primaryArtifact = a} :: BuildSummary)

-- | When the build was started, expressed in Unix time format.
buildSummary_requestedOn :: Lens.Lens' BuildSummary (Prelude.Maybe Prelude.UTCTime)
buildSummary_requestedOn = Lens.lens (\BuildSummary' {requestedOn} -> requestedOn) (\s@BuildSummary' {} a -> s {requestedOn = a} :: BuildSummary) Prelude.. Lens.mapping Data._Time

-- | An array of @ResolvedArtifact@ objects that represents the secondary
-- build artifacts for the build group.
buildSummary_secondaryArtifacts :: Lens.Lens' BuildSummary (Prelude.Maybe [ResolvedArtifact])
buildSummary_secondaryArtifacts = Lens.lens (\BuildSummary' {secondaryArtifacts} -> secondaryArtifacts) (\s@BuildSummary' {} a -> s {secondaryArtifacts = a} :: BuildSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BuildSummary where
  parseJSON =
    Data.withObject
      "BuildSummary"
      ( \x ->
          BuildSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "buildStatus")
            Prelude.<*> (x Data..:? "primaryArtifact")
            Prelude.<*> (x Data..:? "requestedOn")
            Prelude.<*> ( x
                            Data..:? "secondaryArtifacts"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BuildSummary where
  hashWithSalt _salt BuildSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` buildStatus
      `Prelude.hashWithSalt` primaryArtifact
      `Prelude.hashWithSalt` requestedOn
      `Prelude.hashWithSalt` secondaryArtifacts

instance Prelude.NFData BuildSummary where
  rnf BuildSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf buildStatus
      `Prelude.seq` Prelude.rnf primaryArtifact
      `Prelude.seq` Prelude.rnf requestedOn
      `Prelude.seq` Prelude.rnf secondaryArtifacts
