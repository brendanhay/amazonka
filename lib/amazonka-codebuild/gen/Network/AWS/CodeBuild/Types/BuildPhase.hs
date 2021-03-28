{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildPhase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.BuildPhase
  ( BuildPhase (..)
  -- * Smart constructor
  , mkBuildPhase
  -- * Lenses
  , bpContexts
  , bpDurationInSeconds
  , bpEndTime
  , bpPhaseStatus
  , bpPhaseType
  , bpStartTime
  ) where

import qualified Network.AWS.CodeBuild.Types.BuildPhaseType as Types
import qualified Network.AWS.CodeBuild.Types.PhaseContext as Types
import qualified Network.AWS.CodeBuild.Types.StatusType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a stage for a build.
--
-- /See:/ 'mkBuildPhase' smart constructor.
data BuildPhase = BuildPhase'
  { contexts :: Core.Maybe [Types.PhaseContext]
    -- ^ Additional information about a build phase, especially to help troubleshoot a failed build.
  , durationInSeconds :: Core.Maybe Core.Integer
    -- ^ How long, in seconds, between the starting and ending times of the build's phase.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the build phase ended, expressed in Unix time format.
  , phaseStatus :: Core.Maybe Types.StatusType
    -- ^ The current status of the build phase. Valid values include:
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
  , phaseType :: Core.Maybe Types.BuildPhaseType
    -- ^ The name of the build phase. Valid values include:
--
--
--     * @BUILD@ : Core build activities typically occur in this build phase.
--
--
--     * @COMPLETED@ : The build has been completed.
--
--
--     * @DOWNLOAD_SOURCE@ : Source code is being downloaded in this build phase.
--
--
--     * @FINALIZING@ : The build process is completing in this build phase.
--
--
--     * @INSTALL@ : Installation activities typically occur in this build phase.
--
--
--     * @POST_BUILD@ : Post-build activities typically occur in this build phase.
--
--
--     * @PRE_BUILD@ : Pre-build activities typically occur in this build phase.
--
--
--     * @PROVISIONING@ : The build environment is being set up.
--
--
--     * @QUEUED@ : The build has been submitted and is queued behind other submitted builds.
--
--
--     * @SUBMITTED@ : The build has been submitted.
--
--
--     * @UPLOAD_ARTIFACTS@ : Build output artifacts are being uploaded to the output location.
--
--
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the build phase started, expressed in Unix time format.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BuildPhase' value with any optional fields omitted.
mkBuildPhase
    :: BuildPhase
mkBuildPhase
  = BuildPhase'{contexts = Core.Nothing,
                durationInSeconds = Core.Nothing, endTime = Core.Nothing,
                phaseStatus = Core.Nothing, phaseType = Core.Nothing,
                startTime = Core.Nothing}

-- | Additional information about a build phase, especially to help troubleshoot a failed build.
--
-- /Note:/ Consider using 'contexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpContexts :: Lens.Lens' BuildPhase (Core.Maybe [Types.PhaseContext])
bpContexts = Lens.field @"contexts"
{-# INLINEABLE bpContexts #-}
{-# DEPRECATED contexts "Use generic-lens or generic-optics with 'contexts' instead"  #-}

-- | How long, in seconds, between the starting and ending times of the build's phase.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpDurationInSeconds :: Lens.Lens' BuildPhase (Core.Maybe Core.Integer)
bpDurationInSeconds = Lens.field @"durationInSeconds"
{-# INLINEABLE bpDurationInSeconds #-}
{-# DEPRECATED durationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead"  #-}

-- | When the build phase ended, expressed in Unix time format.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpEndTime :: Lens.Lens' BuildPhase (Core.Maybe Core.NominalDiffTime)
bpEndTime = Lens.field @"endTime"
{-# INLINEABLE bpEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The current status of the build phase. Valid values include:
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
bpPhaseStatus :: Lens.Lens' BuildPhase (Core.Maybe Types.StatusType)
bpPhaseStatus = Lens.field @"phaseStatus"
{-# INLINEABLE bpPhaseStatus #-}
{-# DEPRECATED phaseStatus "Use generic-lens or generic-optics with 'phaseStatus' instead"  #-}

-- | The name of the build phase. Valid values include:
--
--
--     * @BUILD@ : Core build activities typically occur in this build phase.
--
--
--     * @COMPLETED@ : The build has been completed.
--
--
--     * @DOWNLOAD_SOURCE@ : Source code is being downloaded in this build phase.
--
--
--     * @FINALIZING@ : The build process is completing in this build phase.
--
--
--     * @INSTALL@ : Installation activities typically occur in this build phase.
--
--
--     * @POST_BUILD@ : Post-build activities typically occur in this build phase.
--
--
--     * @PRE_BUILD@ : Pre-build activities typically occur in this build phase.
--
--
--     * @PROVISIONING@ : The build environment is being set up.
--
--
--     * @QUEUED@ : The build has been submitted and is queued behind other submitted builds.
--
--
--     * @SUBMITTED@ : The build has been submitted.
--
--
--     * @UPLOAD_ARTIFACTS@ : Build output artifacts are being uploaded to the output location.
--
--
--
-- /Note:/ Consider using 'phaseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpPhaseType :: Lens.Lens' BuildPhase (Core.Maybe Types.BuildPhaseType)
bpPhaseType = Lens.field @"phaseType"
{-# INLINEABLE bpPhaseType #-}
{-# DEPRECATED phaseType "Use generic-lens or generic-optics with 'phaseType' instead"  #-}

-- | When the build phase started, expressed in Unix time format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpStartTime :: Lens.Lens' BuildPhase (Core.Maybe Core.NominalDiffTime)
bpStartTime = Lens.field @"startTime"
{-# INLINEABLE bpStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.FromJSON BuildPhase where
        parseJSON
          = Core.withObject "BuildPhase" Core.$
              \ x ->
                BuildPhase' Core.<$>
                  (x Core..:? "contexts") Core.<*> x Core..:? "durationInSeconds"
                    Core.<*> x Core..:? "endTime"
                    Core.<*> x Core..:? "phaseStatus"
                    Core.<*> x Core..:? "phaseType"
                    Core.<*> x Core..:? "startTime"
