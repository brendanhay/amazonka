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
-- Module      : Network.AWS.CodeBuild.Types.BuildPhase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildPhase where

import Network.AWS.CodeBuild.Types.BuildPhaseType
import Network.AWS.CodeBuild.Types.PhaseContext
import Network.AWS.CodeBuild.Types.StatusType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a stage for a build.
--
-- /See:/ 'newBuildPhase' smart constructor.
data BuildPhase = BuildPhase'
  { -- | The name of the build phase. Valid values include:
    --
    -- -   @BUILD@: Core build activities typically occur in this build phase.
    --
    -- -   @COMPLETED@: The build has been completed.
    --
    -- -   @DOWNLOAD_SOURCE@: Source code is being downloaded in this build
    --     phase.
    --
    -- -   @FINALIZING@: The build process is completing in this build phase.
    --
    -- -   @INSTALL@: Installation activities typically occur in this build
    --     phase.
    --
    -- -   @POST_BUILD@: Post-build activities typically occur in this build
    --     phase.
    --
    -- -   @PRE_BUILD@: Pre-build activities typically occur in this build
    --     phase.
    --
    -- -   @PROVISIONING@: The build environment is being set up.
    --
    -- -   @QUEUED@: The build has been submitted and is queued behind other
    --     submitted builds.
    --
    -- -   @SUBMITTED@: The build has been submitted.
    --
    -- -   @UPLOAD_ARTIFACTS@: Build output artifacts are being uploaded to the
    --     output location.
    phaseType :: Core.Maybe BuildPhaseType,
    -- | Additional information about a build phase, especially to help
    -- troubleshoot a failed build.
    contexts :: Core.Maybe [PhaseContext],
    -- | When the build phase started, expressed in Unix time format.
    startTime :: Core.Maybe Core.POSIX,
    -- | When the build phase ended, expressed in Unix time format.
    endTime :: Core.Maybe Core.POSIX,
    -- | How long, in seconds, between the starting and ending times of the
    -- build\'s phase.
    durationInSeconds :: Core.Maybe Core.Integer,
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
    -- [QUEUED]
    --     The build has been submitted and is queued behind other submitted
    --     builds.
    --
    -- [STOPPED]
    --     The build phase stopped.
    --
    -- [SUCCEEDED]
    --     The build phase succeeded.
    --
    -- [TIMED_OUT]
    --     The build phase timed out.
    phaseStatus :: Core.Maybe StatusType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BuildPhase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phaseType', 'buildPhase_phaseType' - The name of the build phase. Valid values include:
--
-- -   @BUILD@: Core build activities typically occur in this build phase.
--
-- -   @COMPLETED@: The build has been completed.
--
-- -   @DOWNLOAD_SOURCE@: Source code is being downloaded in this build
--     phase.
--
-- -   @FINALIZING@: The build process is completing in this build phase.
--
-- -   @INSTALL@: Installation activities typically occur in this build
--     phase.
--
-- -   @POST_BUILD@: Post-build activities typically occur in this build
--     phase.
--
-- -   @PRE_BUILD@: Pre-build activities typically occur in this build
--     phase.
--
-- -   @PROVISIONING@: The build environment is being set up.
--
-- -   @QUEUED@: The build has been submitted and is queued behind other
--     submitted builds.
--
-- -   @SUBMITTED@: The build has been submitted.
--
-- -   @UPLOAD_ARTIFACTS@: Build output artifacts are being uploaded to the
--     output location.
--
-- 'contexts', 'buildPhase_contexts' - Additional information about a build phase, especially to help
-- troubleshoot a failed build.
--
-- 'startTime', 'buildPhase_startTime' - When the build phase started, expressed in Unix time format.
--
-- 'endTime', 'buildPhase_endTime' - When the build phase ended, expressed in Unix time format.
--
-- 'durationInSeconds', 'buildPhase_durationInSeconds' - How long, in seconds, between the starting and ending times of the
-- build\'s phase.
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
-- [QUEUED]
--     The build has been submitted and is queued behind other submitted
--     builds.
--
-- [STOPPED]
--     The build phase stopped.
--
-- [SUCCEEDED]
--     The build phase succeeded.
--
-- [TIMED_OUT]
--     The build phase timed out.
newBuildPhase ::
  BuildPhase
newBuildPhase =
  BuildPhase'
    { phaseType = Core.Nothing,
      contexts = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      durationInSeconds = Core.Nothing,
      phaseStatus = Core.Nothing
    }

-- | The name of the build phase. Valid values include:
--
-- -   @BUILD@: Core build activities typically occur in this build phase.
--
-- -   @COMPLETED@: The build has been completed.
--
-- -   @DOWNLOAD_SOURCE@: Source code is being downloaded in this build
--     phase.
--
-- -   @FINALIZING@: The build process is completing in this build phase.
--
-- -   @INSTALL@: Installation activities typically occur in this build
--     phase.
--
-- -   @POST_BUILD@: Post-build activities typically occur in this build
--     phase.
--
-- -   @PRE_BUILD@: Pre-build activities typically occur in this build
--     phase.
--
-- -   @PROVISIONING@: The build environment is being set up.
--
-- -   @QUEUED@: The build has been submitted and is queued behind other
--     submitted builds.
--
-- -   @SUBMITTED@: The build has been submitted.
--
-- -   @UPLOAD_ARTIFACTS@: Build output artifacts are being uploaded to the
--     output location.
buildPhase_phaseType :: Lens.Lens' BuildPhase (Core.Maybe BuildPhaseType)
buildPhase_phaseType = Lens.lens (\BuildPhase' {phaseType} -> phaseType) (\s@BuildPhase' {} a -> s {phaseType = a} :: BuildPhase)

-- | Additional information about a build phase, especially to help
-- troubleshoot a failed build.
buildPhase_contexts :: Lens.Lens' BuildPhase (Core.Maybe [PhaseContext])
buildPhase_contexts = Lens.lens (\BuildPhase' {contexts} -> contexts) (\s@BuildPhase' {} a -> s {contexts = a} :: BuildPhase) Core.. Lens.mapping Lens._Coerce

-- | When the build phase started, expressed in Unix time format.
buildPhase_startTime :: Lens.Lens' BuildPhase (Core.Maybe Core.UTCTime)
buildPhase_startTime = Lens.lens (\BuildPhase' {startTime} -> startTime) (\s@BuildPhase' {} a -> s {startTime = a} :: BuildPhase) Core.. Lens.mapping Core._Time

-- | When the build phase ended, expressed in Unix time format.
buildPhase_endTime :: Lens.Lens' BuildPhase (Core.Maybe Core.UTCTime)
buildPhase_endTime = Lens.lens (\BuildPhase' {endTime} -> endTime) (\s@BuildPhase' {} a -> s {endTime = a} :: BuildPhase) Core.. Lens.mapping Core._Time

-- | How long, in seconds, between the starting and ending times of the
-- build\'s phase.
buildPhase_durationInSeconds :: Lens.Lens' BuildPhase (Core.Maybe Core.Integer)
buildPhase_durationInSeconds = Lens.lens (\BuildPhase' {durationInSeconds} -> durationInSeconds) (\s@BuildPhase' {} a -> s {durationInSeconds = a} :: BuildPhase)

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
-- [QUEUED]
--     The build has been submitted and is queued behind other submitted
--     builds.
--
-- [STOPPED]
--     The build phase stopped.
--
-- [SUCCEEDED]
--     The build phase succeeded.
--
-- [TIMED_OUT]
--     The build phase timed out.
buildPhase_phaseStatus :: Lens.Lens' BuildPhase (Core.Maybe StatusType)
buildPhase_phaseStatus = Lens.lens (\BuildPhase' {phaseStatus} -> phaseStatus) (\s@BuildPhase' {} a -> s {phaseStatus = a} :: BuildPhase)

instance Core.FromJSON BuildPhase where
  parseJSON =
    Core.withObject
      "BuildPhase"
      ( \x ->
          BuildPhase'
            Core.<$> (x Core..:? "phaseType")
            Core.<*> (x Core..:? "contexts" Core..!= Core.mempty)
            Core.<*> (x Core..:? "startTime")
            Core.<*> (x Core..:? "endTime")
            Core.<*> (x Core..:? "durationInSeconds")
            Core.<*> (x Core..:? "phaseStatus")
      )

instance Core.Hashable BuildPhase

instance Core.NFData BuildPhase
