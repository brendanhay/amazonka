{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.StartNextPendingJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets and starts the next pending (status IN_PROGRESS or QUEUED) job execution for a thing.
module Network.AWS.IoTJobsData.StartNextPendingJobExecution
  ( -- * Creating a request
    StartNextPendingJobExecution (..),
    mkStartNextPendingJobExecution,

    -- ** Request lenses
    snpjeStepTimeoutInMinutes,
    snpjeStatusDetails,
    snpjeThingName,

    -- * Destructuring the response
    StartNextPendingJobExecutionResponse (..),
    mkStartNextPendingJobExecutionResponse,

    -- ** Response lenses
    snpjersExecution,
    snpjersResponseStatus,
  )
where

import Network.AWS.IoTJobsData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartNextPendingJobExecution' smart constructor.
data StartNextPendingJobExecution = StartNextPendingJobExecution'
  { -- | Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in field @stepTimeoutInMinutes@ ) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
    stepTimeoutInMinutes :: Lude.Maybe Lude.Integer,
    -- | A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
    statusDetails :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The name of the thing associated with the device.
    thingName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartNextPendingJobExecution' with the minimum fields required to make a request.
--
-- * 'stepTimeoutInMinutes' - Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in field @stepTimeoutInMinutes@ ) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
-- * 'statusDetails' - A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
-- * 'thingName' - The name of the thing associated with the device.
mkStartNextPendingJobExecution ::
  -- | 'thingName'
  Lude.Text ->
  StartNextPendingJobExecution
mkStartNextPendingJobExecution pThingName_ =
  StartNextPendingJobExecution'
    { stepTimeoutInMinutes =
        Lude.Nothing,
      statusDetails = Lude.Nothing,
      thingName = pThingName_
    }

-- | Specifies the amount of time this device has to finish execution of this job. If the job execution status is not set to a terminal state before this timer expires, or before the timer is reset (by calling @UpdateJobExecution@ , setting the status to @IN_PROGRESS@ and specifying a new timeout value in field @stepTimeoutInMinutes@ ) the job execution status will be automatically set to @TIMED_OUT@ . Note that setting this timeout has no effect on that job execution timeout which may have been specified when the job was created (@CreateJob@ using field @timeoutConfig@ ).
--
-- /Note:/ Consider using 'stepTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpjeStepTimeoutInMinutes :: Lens.Lens' StartNextPendingJobExecution (Lude.Maybe Lude.Integer)
snpjeStepTimeoutInMinutes = Lens.lens (stepTimeoutInMinutes :: StartNextPendingJobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {stepTimeoutInMinutes = a} :: StartNextPendingJobExecution)
{-# DEPRECATED snpjeStepTimeoutInMinutes "Use generic-lens or generic-optics with 'stepTimeoutInMinutes' instead." #-}

-- | A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpjeStatusDetails :: Lens.Lens' StartNextPendingJobExecution (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
snpjeStatusDetails = Lens.lens (statusDetails :: StartNextPendingJobExecution -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {statusDetails = a} :: StartNextPendingJobExecution)
{-# DEPRECATED snpjeStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The name of the thing associated with the device.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpjeThingName :: Lens.Lens' StartNextPendingJobExecution Lude.Text
snpjeThingName = Lens.lens (thingName :: StartNextPendingJobExecution -> Lude.Text) (\s a -> s {thingName = a} :: StartNextPendingJobExecution)
{-# DEPRECATED snpjeThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest StartNextPendingJobExecution where
  type
    Rs StartNextPendingJobExecution =
      StartNextPendingJobExecutionResponse
  request = Req.putJSON ioTJobsDataService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartNextPendingJobExecutionResponse'
            Lude.<$> (x Lude..?> "execution") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartNextPendingJobExecution where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON StartNextPendingJobExecution where
  toJSON StartNextPendingJobExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stepTimeoutInMinutes" Lude..=) Lude.<$> stepTimeoutInMinutes,
            ("statusDetails" Lude..=) Lude.<$> statusDetails
          ]
      )

instance Lude.ToPath StartNextPendingJobExecution where
  toPath StartNextPendingJobExecution' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName, "/jobs/$next"]

instance Lude.ToQuery StartNextPendingJobExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartNextPendingJobExecutionResponse' smart constructor.
data StartNextPendingJobExecutionResponse = StartNextPendingJobExecutionResponse'
  { -- | A JobExecution object.
    execution :: Lude.Maybe JobExecution,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartNextPendingJobExecutionResponse' with the minimum fields required to make a request.
--
-- * 'execution' - A JobExecution object.
-- * 'responseStatus' - The response status code.
mkStartNextPendingJobExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartNextPendingJobExecutionResponse
mkStartNextPendingJobExecutionResponse pResponseStatus_ =
  StartNextPendingJobExecutionResponse'
    { execution = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A JobExecution object.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpjersExecution :: Lens.Lens' StartNextPendingJobExecutionResponse (Lude.Maybe JobExecution)
snpjersExecution = Lens.lens (execution :: StartNextPendingJobExecutionResponse -> Lude.Maybe JobExecution) (\s a -> s {execution = a} :: StartNextPendingJobExecutionResponse)
{-# DEPRECATED snpjersExecution "Use generic-lens or generic-optics with 'execution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpjersResponseStatus :: Lens.Lens' StartNextPendingJobExecutionResponse Lude.Int
snpjersResponseStatus = Lens.lens (responseStatus :: StartNextPendingJobExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartNextPendingJobExecutionResponse)
{-# DEPRECATED snpjersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
