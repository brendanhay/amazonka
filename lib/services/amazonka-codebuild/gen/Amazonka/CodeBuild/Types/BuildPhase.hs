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
-- Module      : Amazonka.CodeBuild.Types.BuildPhase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BuildPhase where

import Amazonka.CodeBuild.Types.BuildPhaseType
import Amazonka.CodeBuild.Types.PhaseContext
import Amazonka.CodeBuild.Types.StatusType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a stage for a build.
--
-- /See:/ 'newBuildPhase' smart constructor.
data BuildPhase = BuildPhase'
  { -- | Additional information about a build phase, especially to help
    -- troubleshoot a failed build.
    contexts :: Prelude.Maybe [PhaseContext],
    -- | The current status of the build phase. Valid values include:
    --
    -- [FAILED]
    --     The build phase failed.
    --
    -- [FAULT]
    --     The build phase faulted.
    --
    -- [IN_PROGRESS]
    --     The build phase is still in progress.
    --
    -- [STOPPED]
    --     The build phase stopped.
    --
    -- [SUCCEEDED]
    --     The build phase succeeded.
    --
    -- [TIMED_OUT]
    --     The build phase timed out.
    phaseStatus :: Prelude.Maybe StatusType,
    -- | When the build phase ended, expressed in Unix time format.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the build phase. Valid values include:
    --
    -- [BUILD]
    --     Core build activities typically occur in this build phase.
    --
    -- [COMPLETED]
    --     The build has been completed.
    --
    -- [DOWNLOAD_SOURCE]
    --     Source code is being downloaded in this build phase.
    --
    -- [FINALIZING]
    --     The build process is completing in this build phase.
    --
    -- [INSTALL]
    --     Installation activities typically occur in this build phase.
    --
    -- [POST_BUILD]
    --     Post-build activities typically occur in this build phase.
    --
    -- [PRE_BUILD]
    --     Pre-build activities typically occur in this build phase.
    --
    -- [PROVISIONING]
    --     The build environment is being set up.
    --
    -- [QUEUED]
    --     The build has been submitted and is queued behind other submitted
    --     builds.
    --
    -- [SUBMITTED]
    --     The build has been submitted.
    --
    -- [UPLOAD_ARTIFACTS]
    --     Build output artifacts are being uploaded to the output location.
    phaseType :: Prelude.Maybe BuildPhaseType,
    -- | How long, in seconds, between the starting and ending times of the
    -- build\'s phase.
    durationInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | When the build phase started, expressed in Unix time format.
    startTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuildPhase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contexts', 'buildPhase_contexts' - Additional information about a build phase, especially to help
-- troubleshoot a failed build.
--
-- 'phaseStatus', 'buildPhase_phaseStatus' - The current status of the build phase. Valid values include:
--
-- [FAILED]
--     The build phase failed.
--
-- [FAULT]
--     The build phase faulted.
--
-- [IN_PROGRESS]
--     The build phase is still in progress.
--
-- [STOPPED]
--     The build phase stopped.
--
-- [SUCCEEDED]
--     The build phase succeeded.
--
-- [TIMED_OUT]
--     The build phase timed out.
--
-- 'endTime', 'buildPhase_endTime' - When the build phase ended, expressed in Unix time format.
--
-- 'phaseType', 'buildPhase_phaseType' - The name of the build phase. Valid values include:
--
-- [BUILD]
--     Core build activities typically occur in this build phase.
--
-- [COMPLETED]
--     The build has been completed.
--
-- [DOWNLOAD_SOURCE]
--     Source code is being downloaded in this build phase.
--
-- [FINALIZING]
--     The build process is completing in this build phase.
--
-- [INSTALL]
--     Installation activities typically occur in this build phase.
--
-- [POST_BUILD]
--     Post-build activities typically occur in this build phase.
--
-- [PRE_BUILD]
--     Pre-build activities typically occur in this build phase.
--
-- [PROVISIONING]
--     The build environment is being set up.
--
-- [QUEUED]
--     The build has been submitted and is queued behind other submitted
--     builds.
--
-- [SUBMITTED]
--     The build has been submitted.
--
-- [UPLOAD_ARTIFACTS]
--     Build output artifacts are being uploaded to the output location.
--
-- 'durationInSeconds', 'buildPhase_durationInSeconds' - How long, in seconds, between the starting and ending times of the
-- build\'s phase.
--
-- 'startTime', 'buildPhase_startTime' - When the build phase started, expressed in Unix time format.
newBuildPhase ::
  BuildPhase
newBuildPhase =
  BuildPhase'
    { contexts = Prelude.Nothing,
      phaseStatus = Prelude.Nothing,
      endTime = Prelude.Nothing,
      phaseType = Prelude.Nothing,
      durationInSeconds = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | Additional information about a build phase, especially to help
-- troubleshoot a failed build.
buildPhase_contexts :: Lens.Lens' BuildPhase (Prelude.Maybe [PhaseContext])
buildPhase_contexts = Lens.lens (\BuildPhase' {contexts} -> contexts) (\s@BuildPhase' {} a -> s {contexts = a} :: BuildPhase) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the build phase. Valid values include:
--
-- [FAILED]
--     The build phase failed.
--
-- [FAULT]
--     The build phase faulted.
--
-- [IN_PROGRESS]
--     The build phase is still in progress.
--
-- [STOPPED]
--     The build phase stopped.
--
-- [SUCCEEDED]
--     The build phase succeeded.
--
-- [TIMED_OUT]
--     The build phase timed out.
buildPhase_phaseStatus :: Lens.Lens' BuildPhase (Prelude.Maybe StatusType)
buildPhase_phaseStatus = Lens.lens (\BuildPhase' {phaseStatus} -> phaseStatus) (\s@BuildPhase' {} a -> s {phaseStatus = a} :: BuildPhase)

-- | When the build phase ended, expressed in Unix time format.
buildPhase_endTime :: Lens.Lens' BuildPhase (Prelude.Maybe Prelude.UTCTime)
buildPhase_endTime = Lens.lens (\BuildPhase' {endTime} -> endTime) (\s@BuildPhase' {} a -> s {endTime = a} :: BuildPhase) Prelude.. Lens.mapping Core._Time

-- | The name of the build phase. Valid values include:
--
-- [BUILD]
--     Core build activities typically occur in this build phase.
--
-- [COMPLETED]
--     The build has been completed.
--
-- [DOWNLOAD_SOURCE]
--     Source code is being downloaded in this build phase.
--
-- [FINALIZING]
--     The build process is completing in this build phase.
--
-- [INSTALL]
--     Installation activities typically occur in this build phase.
--
-- [POST_BUILD]
--     Post-build activities typically occur in this build phase.
--
-- [PRE_BUILD]
--     Pre-build activities typically occur in this build phase.
--
-- [PROVISIONING]
--     The build environment is being set up.
--
-- [QUEUED]
--     The build has been submitted and is queued behind other submitted
--     builds.
--
-- [SUBMITTED]
--     The build has been submitted.
--
-- [UPLOAD_ARTIFACTS]
--     Build output artifacts are being uploaded to the output location.
buildPhase_phaseType :: Lens.Lens' BuildPhase (Prelude.Maybe BuildPhaseType)
buildPhase_phaseType = Lens.lens (\BuildPhase' {phaseType} -> phaseType) (\s@BuildPhase' {} a -> s {phaseType = a} :: BuildPhase)

-- | How long, in seconds, between the starting and ending times of the
-- build\'s phase.
buildPhase_durationInSeconds :: Lens.Lens' BuildPhase (Prelude.Maybe Prelude.Integer)
buildPhase_durationInSeconds = Lens.lens (\BuildPhase' {durationInSeconds} -> durationInSeconds) (\s@BuildPhase' {} a -> s {durationInSeconds = a} :: BuildPhase)

-- | When the build phase started, expressed in Unix time format.
buildPhase_startTime :: Lens.Lens' BuildPhase (Prelude.Maybe Prelude.UTCTime)
buildPhase_startTime = Lens.lens (\BuildPhase' {startTime} -> startTime) (\s@BuildPhase' {} a -> s {startTime = a} :: BuildPhase) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON BuildPhase where
  parseJSON =
    Core.withObject
      "BuildPhase"
      ( \x ->
          BuildPhase'
            Prelude.<$> (x Core..:? "contexts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "phaseStatus")
            Prelude.<*> (x Core..:? "endTime")
            Prelude.<*> (x Core..:? "phaseType")
            Prelude.<*> (x Core..:? "durationInSeconds")
            Prelude.<*> (x Core..:? "startTime")
      )

instance Prelude.Hashable BuildPhase where
  hashWithSalt _salt BuildPhase' {..} =
    _salt `Prelude.hashWithSalt` contexts
      `Prelude.hashWithSalt` phaseStatus
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` phaseType
      `Prelude.hashWithSalt` durationInSeconds
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData BuildPhase where
  rnf BuildPhase' {..} =
    Prelude.rnf contexts
      `Prelude.seq` Prelude.rnf phaseStatus
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf phaseType
      `Prelude.seq` Prelude.rnf durationInSeconds
      `Prelude.seq` Prelude.rnf startTime
