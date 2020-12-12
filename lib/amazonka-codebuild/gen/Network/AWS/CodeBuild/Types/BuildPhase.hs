{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildPhase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildPhase
  ( BuildPhase (..),

    -- * Smart constructor
    mkBuildPhase,

    -- * Lenses
    bpContexts,
    bpStartTime,
    bpPhaseStatus,
    bpPhaseType,
    bpEndTime,
    bpDurationInSeconds,
  )
where

import Network.AWS.CodeBuild.Types.BuildPhaseType
import Network.AWS.CodeBuild.Types.PhaseContext
import Network.AWS.CodeBuild.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a stage for a build.
--
-- /See:/ 'mkBuildPhase' smart constructor.
data BuildPhase = BuildPhase'
  { contexts ::
      Lude.Maybe [PhaseContext],
    startTime :: Lude.Maybe Lude.Timestamp,
    phaseStatus :: Lude.Maybe StatusType,
    phaseType :: Lude.Maybe BuildPhaseType,
    endTime :: Lude.Maybe Lude.Timestamp,
    durationInSeconds :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuildPhase' with the minimum fields required to make a request.
--
-- * 'contexts' - Additional information about a build phase, especially to help troubleshoot a failed build.
-- * 'durationInSeconds' - How long, in seconds, between the starting and ending times of the build's phase.
-- * 'endTime' - When the build phase ended, expressed in Unix time format.
-- * 'phaseStatus' - The current status of the build phase. Valid values include:
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
-- * 'phaseType' - The name of the build phase. Valid values include:
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
-- * 'startTime' - When the build phase started, expressed in Unix time format.
mkBuildPhase ::
  BuildPhase
mkBuildPhase =
  BuildPhase'
    { contexts = Lude.Nothing,
      startTime = Lude.Nothing,
      phaseStatus = Lude.Nothing,
      phaseType = Lude.Nothing,
      endTime = Lude.Nothing,
      durationInSeconds = Lude.Nothing
    }

-- | Additional information about a build phase, especially to help troubleshoot a failed build.
--
-- /Note:/ Consider using 'contexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpContexts :: Lens.Lens' BuildPhase (Lude.Maybe [PhaseContext])
bpContexts = Lens.lens (contexts :: BuildPhase -> Lude.Maybe [PhaseContext]) (\s a -> s {contexts = a} :: BuildPhase)
{-# DEPRECATED bpContexts "Use generic-lens or generic-optics with 'contexts' instead." #-}

-- | When the build phase started, expressed in Unix time format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpStartTime :: Lens.Lens' BuildPhase (Lude.Maybe Lude.Timestamp)
bpStartTime = Lens.lens (startTime :: BuildPhase -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: BuildPhase)
{-# DEPRECATED bpStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

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
bpPhaseStatus :: Lens.Lens' BuildPhase (Lude.Maybe StatusType)
bpPhaseStatus = Lens.lens (phaseStatus :: BuildPhase -> Lude.Maybe StatusType) (\s a -> s {phaseStatus = a} :: BuildPhase)
{-# DEPRECATED bpPhaseStatus "Use generic-lens or generic-optics with 'phaseStatus' instead." #-}

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
bpPhaseType :: Lens.Lens' BuildPhase (Lude.Maybe BuildPhaseType)
bpPhaseType = Lens.lens (phaseType :: BuildPhase -> Lude.Maybe BuildPhaseType) (\s a -> s {phaseType = a} :: BuildPhase)
{-# DEPRECATED bpPhaseType "Use generic-lens or generic-optics with 'phaseType' instead." #-}

-- | When the build phase ended, expressed in Unix time format.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpEndTime :: Lens.Lens' BuildPhase (Lude.Maybe Lude.Timestamp)
bpEndTime = Lens.lens (endTime :: BuildPhase -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: BuildPhase)
{-# DEPRECATED bpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | How long, in seconds, between the starting and ending times of the build's phase.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpDurationInSeconds :: Lens.Lens' BuildPhase (Lude.Maybe Lude.Integer)
bpDurationInSeconds = Lens.lens (durationInSeconds :: BuildPhase -> Lude.Maybe Lude.Integer) (\s a -> s {durationInSeconds = a} :: BuildPhase)
{-# DEPRECATED bpDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

instance Lude.FromJSON BuildPhase where
  parseJSON =
    Lude.withObject
      "BuildPhase"
      ( \x ->
          BuildPhase'
            Lude.<$> (x Lude..:? "contexts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "phaseStatus")
            Lude.<*> (x Lude..:? "phaseType")
            Lude.<*> (x Lude..:? "endTime")
            Lude.<*> (x Lude..:? "durationInSeconds")
      )
