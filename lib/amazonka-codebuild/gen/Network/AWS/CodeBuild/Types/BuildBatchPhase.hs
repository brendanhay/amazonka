{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildBatchPhase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatchPhase
  ( BuildBatchPhase (..),

    -- * Smart constructor
    mkBuildBatchPhase,

    -- * Lenses
    bbpContexts,
    bbpDurationInSeconds,
    bbpEndTime,
    bbpPhaseStatus,
    bbpPhaseType,
    bbpStartTime,
  )
where

import qualified Network.AWS.CodeBuild.Types.BuildBatchPhaseType as Types
import qualified Network.AWS.CodeBuild.Types.PhaseContext as Types
import qualified Network.AWS.CodeBuild.Types.StatusType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a stage for a batch build.
--
-- /See:/ 'mkBuildBatchPhase' smart constructor.
data BuildBatchPhase = BuildBatchPhase'
  { -- | Additional information about the batch build phase. Especially to help troubleshoot a failed btach build.
    contexts :: Core.Maybe [Types.PhaseContext],
    -- | How long, in seconds, between the starting and ending times of the batch build's phase.
    durationInSeconds :: Core.Maybe Core.Integer,
    -- | When the batch build phase ended, expressed in Unix time format.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The current status of the batch build phase. Valid values include:
    --
    --
    --     * FAILED
    --
    --     * The build phase failed.
    --
    --
    --     * FAULT
    --
    --     * The build phase faulted.
    --
    --
    --     * IN_PROGRESS
    --
    --     * The build phase is still in progress.
    --
    --
    --     * QUEUED
    --
    --     * The build has been submitted and is queued behind other submitted builds.
    --
    --
    --     * STOPPED
    --
    --     * The build phase stopped.
    --
    --
    --     * SUCCEEDED
    --
    --     * The build phase succeeded.
    --
    --
    --     * TIMED_OUT
    --
    --     * The build phase timed out.
    phaseStatus :: Core.Maybe Types.StatusType,
    -- | The name of the batch build phase. Valid values include:
    --
    --
    --     * COMBINE_ARTIFACTS
    --
    --     * Build output artifacts are being combined and uploaded to the output location.
    --
    --
    --     * DOWNLOAD_BATCHSPEC
    --
    --     * The batch build specification is being downloaded.
    --
    --
    --     * FAILED
    --
    --     * One or more of the builds failed.
    --
    --
    --     * IN_PROGRESS
    --
    --     * The batch build is in progress.
    --
    --
    --     * STOPPED
    --
    --     * The batch build was stopped.
    --
    --
    --     * SUBMITTED
    --
    --     * The btach build has been submitted.
    --
    --
    --     * SUCCEEDED
    --
    --     * The batch build succeeded.
    phaseType :: Core.Maybe Types.BuildBatchPhaseType,
    -- | When the batch build phase started, expressed in Unix time format.
    startTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BuildBatchPhase' value with any optional fields omitted.
mkBuildBatchPhase ::
  BuildBatchPhase
mkBuildBatchPhase =
  BuildBatchPhase'
    { contexts = Core.Nothing,
      durationInSeconds = Core.Nothing,
      endTime = Core.Nothing,
      phaseStatus = Core.Nothing,
      phaseType = Core.Nothing,
      startTime = Core.Nothing
    }

-- | Additional information about the batch build phase. Especially to help troubleshoot a failed btach build.
--
-- /Note:/ Consider using 'contexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbpContexts :: Lens.Lens' BuildBatchPhase (Core.Maybe [Types.PhaseContext])
bbpContexts = Lens.field @"contexts"
{-# DEPRECATED bbpContexts "Use generic-lens or generic-optics with 'contexts' instead." #-}

-- | How long, in seconds, between the starting and ending times of the batch build's phase.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbpDurationInSeconds :: Lens.Lens' BuildBatchPhase (Core.Maybe Core.Integer)
bbpDurationInSeconds = Lens.field @"durationInSeconds"
{-# DEPRECATED bbpDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

-- | When the batch build phase ended, expressed in Unix time format.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbpEndTime :: Lens.Lens' BuildBatchPhase (Core.Maybe Core.NominalDiffTime)
bbpEndTime = Lens.field @"endTime"
{-# DEPRECATED bbpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The current status of the batch build phase. Valid values include:
--
--
--     * FAILED
--
--     * The build phase failed.
--
--
--     * FAULT
--
--     * The build phase faulted.
--
--
--     * IN_PROGRESS
--
--     * The build phase is still in progress.
--
--
--     * QUEUED
--
--     * The build has been submitted and is queued behind other submitted builds.
--
--
--     * STOPPED
--
--     * The build phase stopped.
--
--
--     * SUCCEEDED
--
--     * The build phase succeeded.
--
--
--     * TIMED_OUT
--
--     * The build phase timed out.
--
--
--
-- /Note:/ Consider using 'phaseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbpPhaseStatus :: Lens.Lens' BuildBatchPhase (Core.Maybe Types.StatusType)
bbpPhaseStatus = Lens.field @"phaseStatus"
{-# DEPRECATED bbpPhaseStatus "Use generic-lens or generic-optics with 'phaseStatus' instead." #-}

-- | The name of the batch build phase. Valid values include:
--
--
--     * COMBINE_ARTIFACTS
--
--     * Build output artifacts are being combined and uploaded to the output location.
--
--
--     * DOWNLOAD_BATCHSPEC
--
--     * The batch build specification is being downloaded.
--
--
--     * FAILED
--
--     * One or more of the builds failed.
--
--
--     * IN_PROGRESS
--
--     * The batch build is in progress.
--
--
--     * STOPPED
--
--     * The batch build was stopped.
--
--
--     * SUBMITTED
--
--     * The btach build has been submitted.
--
--
--     * SUCCEEDED
--
--     * The batch build succeeded.
--
--
--
-- /Note:/ Consider using 'phaseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbpPhaseType :: Lens.Lens' BuildBatchPhase (Core.Maybe Types.BuildBatchPhaseType)
bbpPhaseType = Lens.field @"phaseType"
{-# DEPRECATED bbpPhaseType "Use generic-lens or generic-optics with 'phaseType' instead." #-}

-- | When the batch build phase started, expressed in Unix time format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbpStartTime :: Lens.Lens' BuildBatchPhase (Core.Maybe Core.NominalDiffTime)
bbpStartTime = Lens.field @"startTime"
{-# DEPRECATED bbpStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON BuildBatchPhase where
  parseJSON =
    Core.withObject "BuildBatchPhase" Core.$
      \x ->
        BuildBatchPhase'
          Core.<$> (x Core..:? "contexts")
          Core.<*> (x Core..:? "durationInSeconds")
          Core.<*> (x Core..:? "endTime")
          Core.<*> (x Core..:? "phaseStatus")
          Core.<*> (x Core..:? "phaseType")
          Core.<*> (x Core..:? "startTime")
