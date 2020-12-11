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
    bbpStartTime,
    bbpPhaseStatus,
    bbpPhaseType,
    bbpEndTime,
    bbpDurationInSeconds,
  )
where

import Network.AWS.CodeBuild.Types.BuildBatchPhaseType
import Network.AWS.CodeBuild.Types.PhaseContext
import Network.AWS.CodeBuild.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a stage for a batch build.
--
-- /See:/ 'mkBuildBatchPhase' smart constructor.
data BuildBatchPhase = BuildBatchPhase'
  { contexts ::
      Lude.Maybe [PhaseContext],
    startTime :: Lude.Maybe Lude.Timestamp,
    phaseStatus :: Lude.Maybe StatusType,
    phaseType :: Lude.Maybe BuildBatchPhaseType,
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

-- | Creates a value of 'BuildBatchPhase' with the minimum fields required to make a request.
--
-- * 'contexts' - Additional information about the batch build phase. Especially to help troubleshoot a failed btach build.
-- * 'durationInSeconds' - How long, in seconds, between the starting and ending times of the batch build's phase.
-- * 'endTime' - When the batch build phase ended, expressed in Unix time format.
-- * 'phaseStatus' - The current status of the batch build phase. Valid values include:
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
-- * 'phaseType' - The name of the batch build phase. Valid values include:
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
-- * 'startTime' - When the batch build phase started, expressed in Unix time format.
mkBuildBatchPhase ::
  BuildBatchPhase
mkBuildBatchPhase =
  BuildBatchPhase'
    { contexts = Lude.Nothing,
      startTime = Lude.Nothing,
      phaseStatus = Lude.Nothing,
      phaseType = Lude.Nothing,
      endTime = Lude.Nothing,
      durationInSeconds = Lude.Nothing
    }

-- | Additional information about the batch build phase. Especially to help troubleshoot a failed btach build.
--
-- /Note:/ Consider using 'contexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbpContexts :: Lens.Lens' BuildBatchPhase (Lude.Maybe [PhaseContext])
bbpContexts = Lens.lens (contexts :: BuildBatchPhase -> Lude.Maybe [PhaseContext]) (\s a -> s {contexts = a} :: BuildBatchPhase)
{-# DEPRECATED bbpContexts "Use generic-lens or generic-optics with 'contexts' instead." #-}

-- | When the batch build phase started, expressed in Unix time format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbpStartTime :: Lens.Lens' BuildBatchPhase (Lude.Maybe Lude.Timestamp)
bbpStartTime = Lens.lens (startTime :: BuildBatchPhase -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: BuildBatchPhase)
{-# DEPRECATED bbpStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

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
bbpPhaseStatus :: Lens.Lens' BuildBatchPhase (Lude.Maybe StatusType)
bbpPhaseStatus = Lens.lens (phaseStatus :: BuildBatchPhase -> Lude.Maybe StatusType) (\s a -> s {phaseStatus = a} :: BuildBatchPhase)
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
bbpPhaseType :: Lens.Lens' BuildBatchPhase (Lude.Maybe BuildBatchPhaseType)
bbpPhaseType = Lens.lens (phaseType :: BuildBatchPhase -> Lude.Maybe BuildBatchPhaseType) (\s a -> s {phaseType = a} :: BuildBatchPhase)
{-# DEPRECATED bbpPhaseType "Use generic-lens or generic-optics with 'phaseType' instead." #-}

-- | When the batch build phase ended, expressed in Unix time format.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbpEndTime :: Lens.Lens' BuildBatchPhase (Lude.Maybe Lude.Timestamp)
bbpEndTime = Lens.lens (endTime :: BuildBatchPhase -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: BuildBatchPhase)
{-# DEPRECATED bbpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | How long, in seconds, between the starting and ending times of the batch build's phase.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbpDurationInSeconds :: Lens.Lens' BuildBatchPhase (Lude.Maybe Lude.Integer)
bbpDurationInSeconds = Lens.lens (durationInSeconds :: BuildBatchPhase -> Lude.Maybe Lude.Integer) (\s a -> s {durationInSeconds = a} :: BuildBatchPhase)
{-# DEPRECATED bbpDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

instance Lude.FromJSON BuildBatchPhase where
  parseJSON =
    Lude.withObject
      "BuildBatchPhase"
      ( \x ->
          BuildBatchPhase'
            Lude.<$> (x Lude..:? "contexts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "phaseStatus")
            Lude.<*> (x Lude..:? "phaseType")
            Lude.<*> (x Lude..:? "endTime")
            Lude.<*> (x Lude..:? "durationInSeconds")
      )
