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

import Network.AWS.IoTJobsData.Types.JobExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains data about the state of a job execution.
--
-- /See:/ 'mkJobExecutionState' smart constructor.
data JobExecutionState = JobExecutionState'
  { -- | The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
    status :: Lude.Maybe JobExecutionStatus,
    -- | A collection of name/value pairs that describe the status of the job execution.
    statusDetails :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The version of the job execution. Job execution versions are incremented each time they are updated by a device.
    versionNumber :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobExecutionState' with the minimum fields required to make a request.
--
-- * 'status' - The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
-- * 'statusDetails' - A collection of name/value pairs that describe the status of the job execution.
-- * 'versionNumber' - The version of the job execution. Job execution versions are incremented each time they are updated by a device.
mkJobExecutionState ::
  JobExecutionState
mkJobExecutionState =
  JobExecutionState'
    { status = Lude.Nothing,
      statusDetails = Lude.Nothing,
      versionNumber = Lude.Nothing
    }

-- | The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesStatus :: Lens.Lens' JobExecutionState (Lude.Maybe JobExecutionStatus)
jesStatus = Lens.lens (status :: JobExecutionState -> Lude.Maybe JobExecutionStatus) (\s a -> s {status = a} :: JobExecutionState)
{-# DEPRECATED jesStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A collection of name/value pairs that describe the status of the job execution.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesStatusDetails :: Lens.Lens' JobExecutionState (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jesStatusDetails = Lens.lens (statusDetails :: JobExecutionState -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {statusDetails = a} :: JobExecutionState)
{-# DEPRECATED jesStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The version of the job execution. Job execution versions are incremented each time they are updated by a device.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesVersionNumber :: Lens.Lens' JobExecutionState (Lude.Maybe Lude.Integer)
jesVersionNumber = Lens.lens (versionNumber :: JobExecutionState -> Lude.Maybe Lude.Integer) (\s a -> s {versionNumber = a} :: JobExecutionState)
{-# DEPRECATED jesVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Lude.FromJSON JobExecutionState where
  parseJSON =
    Lude.withObject
      "JobExecutionState"
      ( \x ->
          JobExecutionState'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "statusDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "versionNumber")
      )
