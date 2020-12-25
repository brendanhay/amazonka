{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types.JobExecutionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTJobsData.Types.JobExecutionState
  ( JobExecutionState (..),

    -- * Smart constructor
    mkJobExecutionState,

    -- * Lenses
    jesStatus,
    jesStatusDetails,
    jesVersionNumber,
  )
where

import qualified Network.AWS.IoTJobsData.Types.DetailsKey as Types
import qualified Network.AWS.IoTJobsData.Types.DetailsValue as Types
import qualified Network.AWS.IoTJobsData.Types.JobExecutionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains data about the state of a job execution.
--
-- /See:/ 'mkJobExecutionState' smart constructor.
data JobExecutionState = JobExecutionState'
  { -- | The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
    status :: Core.Maybe Types.JobExecutionStatus,
    -- | A collection of name/value pairs that describe the status of the job execution.
    statusDetails :: Core.Maybe (Core.HashMap Types.DetailsKey Types.DetailsValue),
    -- | The version of the job execution. Job execution versions are incremented each time they are updated by a device.
    versionNumber :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobExecutionState' value with any optional fields omitted.
mkJobExecutionState ::
  JobExecutionState
mkJobExecutionState =
  JobExecutionState'
    { status = Core.Nothing,
      statusDetails = Core.Nothing,
      versionNumber = Core.Nothing
    }

-- | The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesStatus :: Lens.Lens' JobExecutionState (Core.Maybe Types.JobExecutionStatus)
jesStatus = Lens.field @"status"
{-# DEPRECATED jesStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A collection of name/value pairs that describe the status of the job execution.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesStatusDetails :: Lens.Lens' JobExecutionState (Core.Maybe (Core.HashMap Types.DetailsKey Types.DetailsValue))
jesStatusDetails = Lens.field @"statusDetails"
{-# DEPRECATED jesStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The version of the job execution. Job execution versions are incremented each time they are updated by a device.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesVersionNumber :: Lens.Lens' JobExecutionState (Core.Maybe Core.Integer)
jesVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED jesVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Core.FromJSON JobExecutionState where
  parseJSON =
    Core.withObject "JobExecutionState" Core.$
      \x ->
        JobExecutionState'
          Core.<$> (x Core..:? "status")
          Core.<*> (x Core..:? "statusDetails")
          Core.<*> (x Core..:? "versionNumber")
