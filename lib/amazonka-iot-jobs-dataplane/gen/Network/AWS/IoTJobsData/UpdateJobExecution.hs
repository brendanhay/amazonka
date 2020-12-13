{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.UpdateJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a job execution.
module Network.AWS.IoTJobsData.UpdateJobExecution
  ( -- * Creating a request
    UpdateJobExecution (..),
    mkUpdateJobExecution,

    -- ** Request lenses
    ujeStatus,
    ujeIncludeJobDocument,
    ujeJobId,
    ujeStepTimeoutInMinutes,
    ujeStatusDetails,
    ujeExecutionNumber,
    ujeExpectedVersion,
    ujeThingName,
    ujeIncludeJobExecutionState,

    -- * Destructuring the response
    UpdateJobExecutionResponse (..),
    mkUpdateJobExecutionResponse,

    -- ** Response lenses
    ujersJobDocument,
    ujersExecutionState,
    ujersResponseStatus,
  )
where

import Network.AWS.IoTJobsData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateJobExecution' smart constructor.
data UpdateJobExecution = UpdateJobExecution'
  { -- | The new status for the job execution (IN_PROGRESS, FAILED, SUCCESS, or REJECTED). This must be specified on every update.
    status :: JobExecutionStatus,
    -- | Optional. When set to true, the response contains the job document. The default is false.
    includeJobDocument :: Lude.Maybe Lude.Bool,
    -- | The unique identifier assigned to this job when it was created.
    jobId :: Lude.Text,
    -- | Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by again calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in this field) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting or resetting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
    stepTimeoutInMinutes :: Lude.Maybe Lude.Integer,
    -- | Optional. A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
    statusDetails :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Optional. A number that identifies a particular job execution on a particular device.
    executionNumber :: Lude.Maybe Lude.Integer,
    -- | Optional. The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
    expectedVersion :: Lude.Maybe Lude.Integer,
    -- | The name of the thing associated with the device.
    thingName :: Lude.Text,
    -- | Optional. When included and set to true, the response contains the JobExecutionState data. The default is false.
    includeJobExecutionState :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJobExecution' with the minimum fields required to make a request.
--
-- * 'status' - The new status for the job execution (IN_PROGRESS, FAILED, SUCCESS, or REJECTED). This must be specified on every update.
-- * 'includeJobDocument' - Optional. When set to true, the response contains the job document. The default is false.
-- * 'jobId' - The unique identifier assigned to this job when it was created.
-- * 'stepTimeoutInMinutes' - Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by again calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in this field) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting or resetting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
-- * 'statusDetails' - Optional. A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
-- * 'executionNumber' - Optional. A number that identifies a particular job execution on a particular device.
-- * 'expectedVersion' - Optional. The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
-- * 'thingName' - The name of the thing associated with the device.
-- * 'includeJobExecutionState' - Optional. When included and set to true, the response contains the JobExecutionState data. The default is false.
mkUpdateJobExecution ::
  -- | 'status'
  JobExecutionStatus ->
  -- | 'jobId'
  Lude.Text ->
  -- | 'thingName'
  Lude.Text ->
  UpdateJobExecution
mkUpdateJobExecution pStatus_ pJobId_ pThingName_ =
  UpdateJobExecution'
    { status = pStatus_,
      includeJobDocument = Lude.Nothing,
      jobId = pJobId_,
      stepTimeoutInMinutes = Lude.Nothing,
      statusDetails = Lude.Nothing,
      executionNumber = Lude.Nothing,
      expectedVersion = Lude.Nothing,
      thingName = pThingName_,
      includeJobExecutionState = Lude.Nothing
    }

-- | The new status for the job execution (IN_PROGRESS, FAILED, SUCCESS, or REJECTED). This must be specified on every update.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeStatus :: Lens.Lens' UpdateJobExecution JobExecutionStatus
ujeStatus = Lens.lens (status :: UpdateJobExecution -> JobExecutionStatus) (\s a -> s {status = a} :: UpdateJobExecution)
{-# DEPRECATED ujeStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Optional. When set to true, the response contains the job document. The default is false.
--
-- /Note:/ Consider using 'includeJobDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeIncludeJobDocument :: Lens.Lens' UpdateJobExecution (Lude.Maybe Lude.Bool)
ujeIncludeJobDocument = Lens.lens (includeJobDocument :: UpdateJobExecution -> Lude.Maybe Lude.Bool) (\s a -> s {includeJobDocument = a} :: UpdateJobExecution)
{-# DEPRECATED ujeIncludeJobDocument "Use generic-lens or generic-optics with 'includeJobDocument' instead." #-}

-- | The unique identifier assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeJobId :: Lens.Lens' UpdateJobExecution Lude.Text
ujeJobId = Lens.lens (jobId :: UpdateJobExecution -> Lude.Text) (\s a -> s {jobId = a} :: UpdateJobExecution)
{-# DEPRECATED ujeJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by again calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in this field) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting or resetting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
--
-- /Note:/ Consider using 'stepTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeStepTimeoutInMinutes :: Lens.Lens' UpdateJobExecution (Lude.Maybe Lude.Integer)
ujeStepTimeoutInMinutes = Lens.lens (stepTimeoutInMinutes :: UpdateJobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {stepTimeoutInMinutes = a} :: UpdateJobExecution)
{-# DEPRECATED ujeStepTimeoutInMinutes "Use generic-lens or generic-optics with 'stepTimeoutInMinutes' instead." #-}

-- | Optional. A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeStatusDetails :: Lens.Lens' UpdateJobExecution (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ujeStatusDetails = Lens.lens (statusDetails :: UpdateJobExecution -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {statusDetails = a} :: UpdateJobExecution)
{-# DEPRECATED ujeStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | Optional. A number that identifies a particular job execution on a particular device.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeExecutionNumber :: Lens.Lens' UpdateJobExecution (Lude.Maybe Lude.Integer)
ujeExecutionNumber = Lens.lens (executionNumber :: UpdateJobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {executionNumber = a} :: UpdateJobExecution)
{-# DEPRECATED ujeExecutionNumber "Use generic-lens or generic-optics with 'executionNumber' instead." #-}

-- | Optional. The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeExpectedVersion :: Lens.Lens' UpdateJobExecution (Lude.Maybe Lude.Integer)
ujeExpectedVersion = Lens.lens (expectedVersion :: UpdateJobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: UpdateJobExecution)
{-# DEPRECATED ujeExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The name of the thing associated with the device.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeThingName :: Lens.Lens' UpdateJobExecution Lude.Text
ujeThingName = Lens.lens (thingName :: UpdateJobExecution -> Lude.Text) (\s a -> s {thingName = a} :: UpdateJobExecution)
{-# DEPRECATED ujeThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | Optional. When included and set to true, the response contains the JobExecutionState data. The default is false.
--
-- /Note:/ Consider using 'includeJobExecutionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujeIncludeJobExecutionState :: Lens.Lens' UpdateJobExecution (Lude.Maybe Lude.Bool)
ujeIncludeJobExecutionState = Lens.lens (includeJobExecutionState :: UpdateJobExecution -> Lude.Maybe Lude.Bool) (\s a -> s {includeJobExecutionState = a} :: UpdateJobExecution)
{-# DEPRECATED ujeIncludeJobExecutionState "Use generic-lens or generic-optics with 'includeJobExecutionState' instead." #-}

instance Lude.AWSRequest UpdateJobExecution where
  type Rs UpdateJobExecution = UpdateJobExecutionResponse
  request = Req.postJSON ioTJobsDataService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateJobExecutionResponse'
            Lude.<$> (x Lude..?> "jobDocument")
            Lude.<*> (x Lude..?> "executionState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateJobExecution where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateJobExecution where
  toJSON UpdateJobExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("status" Lude..= status),
            ("includeJobDocument" Lude..=) Lude.<$> includeJobDocument,
            ("stepTimeoutInMinutes" Lude..=) Lude.<$> stepTimeoutInMinutes,
            ("statusDetails" Lude..=) Lude.<$> statusDetails,
            ("executionNumber" Lude..=) Lude.<$> executionNumber,
            ("expectedVersion" Lude..=) Lude.<$> expectedVersion,
            ("includeJobExecutionState" Lude..=)
              Lude.<$> includeJobExecutionState
          ]
      )

instance Lude.ToPath UpdateJobExecution where
  toPath UpdateJobExecution' {..} =
    Lude.mconcat
      ["/things/", Lude.toBS thingName, "/jobs/", Lude.toBS jobId]

instance Lude.ToQuery UpdateJobExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateJobExecutionResponse' smart constructor.
data UpdateJobExecutionResponse = UpdateJobExecutionResponse'
  { -- | The contents of the Job Documents.
    jobDocument :: Lude.Maybe Lude.Text,
    -- | A JobExecutionState object.
    executionState :: Lude.Maybe JobExecutionState,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJobExecutionResponse' with the minimum fields required to make a request.
--
-- * 'jobDocument' - The contents of the Job Documents.
-- * 'executionState' - A JobExecutionState object.
-- * 'responseStatus' - The response status code.
mkUpdateJobExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateJobExecutionResponse
mkUpdateJobExecutionResponse pResponseStatus_ =
  UpdateJobExecutionResponse'
    { jobDocument = Lude.Nothing,
      executionState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The contents of the Job Documents.
--
-- /Note:/ Consider using 'jobDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujersJobDocument :: Lens.Lens' UpdateJobExecutionResponse (Lude.Maybe Lude.Text)
ujersJobDocument = Lens.lens (jobDocument :: UpdateJobExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobDocument = a} :: UpdateJobExecutionResponse)
{-# DEPRECATED ujersJobDocument "Use generic-lens or generic-optics with 'jobDocument' instead." #-}

-- | A JobExecutionState object.
--
-- /Note:/ Consider using 'executionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujersExecutionState :: Lens.Lens' UpdateJobExecutionResponse (Lude.Maybe JobExecutionState)
ujersExecutionState = Lens.lens (executionState :: UpdateJobExecutionResponse -> Lude.Maybe JobExecutionState) (\s a -> s {executionState = a} :: UpdateJobExecutionResponse)
{-# DEPRECATED ujersExecutionState "Use generic-lens or generic-optics with 'executionState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujersResponseStatus :: Lens.Lens' UpdateJobExecutionResponse Lude.Int
ujersResponseStatus = Lens.lens (responseStatus :: UpdateJobExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateJobExecutionResponse)
{-# DEPRECATED ujersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
