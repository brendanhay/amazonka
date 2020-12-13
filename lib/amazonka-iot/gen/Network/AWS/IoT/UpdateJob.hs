{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates supported fields of the specified job.
module Network.AWS.IoT.UpdateJob
  ( -- * Creating a request
    UpdateJob (..),
    mkUpdateJob,

    -- ** Request lenses
    ujJobExecutionsRolloutConfig,
    ujJobId,
    ujAbortConfig,
    ujNamespaceId,
    ujPresignedURLConfig,
    ujDescription,
    ujTimeoutConfig,

    -- * Destructuring the response
    UpdateJobResponse (..),
    mkUpdateJobResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { -- | Allows you to create a staged rollout of the job.
    jobExecutionsRolloutConfig :: Lude.Maybe JobExecutionsRolloutConfig,
    -- | The ID of the job to be updated.
    jobId :: Lude.Text,
    -- | Allows you to create criteria to abort a job.
    abortConfig :: Lude.Maybe AbortConfig,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
    -- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
    namespaceId :: Lude.Maybe Lude.Text,
    -- | Configuration information for pre-signed S3 URLs.
    presignedURLConfig :: Lude.Maybe PresignedURLConfig,
    -- | A short text description of the job.
    description :: Lude.Maybe Lude.Text,
    -- | Specifies the amount of time each device has to finish its execution of the job. The timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the time expires, it will be automatically set to @TIMED_OUT@ .
    timeoutConfig :: Lude.Maybe TimeoutConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJob' with the minimum fields required to make a request.
--
-- * 'jobExecutionsRolloutConfig' - Allows you to create a staged rollout of the job.
-- * 'jobId' - The ID of the job to be updated.
-- * 'abortConfig' - Allows you to create criteria to abort a job.
-- * 'namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
-- * 'presignedURLConfig' - Configuration information for pre-signed S3 URLs.
-- * 'description' - A short text description of the job.
-- * 'timeoutConfig' - Specifies the amount of time each device has to finish its execution of the job. The timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the time expires, it will be automatically set to @TIMED_OUT@ .
mkUpdateJob ::
  -- | 'jobId'
  Lude.Text ->
  UpdateJob
mkUpdateJob pJobId_ =
  UpdateJob'
    { jobExecutionsRolloutConfig = Lude.Nothing,
      jobId = pJobId_,
      abortConfig = Lude.Nothing,
      namespaceId = Lude.Nothing,
      presignedURLConfig = Lude.Nothing,
      description = Lude.Nothing,
      timeoutConfig = Lude.Nothing
    }

-- | Allows you to create a staged rollout of the job.
--
-- /Note:/ Consider using 'jobExecutionsRolloutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobExecutionsRolloutConfig :: Lens.Lens' UpdateJob (Lude.Maybe JobExecutionsRolloutConfig)
ujJobExecutionsRolloutConfig = Lens.lens (jobExecutionsRolloutConfig :: UpdateJob -> Lude.Maybe JobExecutionsRolloutConfig) (\s a -> s {jobExecutionsRolloutConfig = a} :: UpdateJob)
{-# DEPRECATED ujJobExecutionsRolloutConfig "Use generic-lens or generic-optics with 'jobExecutionsRolloutConfig' instead." #-}

-- | The ID of the job to be updated.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobId :: Lens.Lens' UpdateJob Lude.Text
ujJobId = Lens.lens (jobId :: UpdateJob -> Lude.Text) (\s a -> s {jobId = a} :: UpdateJob)
{-# DEPRECATED ujJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Allows you to create criteria to abort a job.
--
-- /Note:/ Consider using 'abortConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujAbortConfig :: Lens.Lens' UpdateJob (Lude.Maybe AbortConfig)
ujAbortConfig = Lens.lens (abortConfig :: UpdateJob -> Lude.Maybe AbortConfig) (\s a -> s {abortConfig = a} :: UpdateJob)
{-# DEPRECATED ujAbortConfig "Use generic-lens or generic-optics with 'abortConfig' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujNamespaceId :: Lens.Lens' UpdateJob (Lude.Maybe Lude.Text)
ujNamespaceId = Lens.lens (namespaceId :: UpdateJob -> Lude.Maybe Lude.Text) (\s a -> s {namespaceId = a} :: UpdateJob)
{-# DEPRECATED ujNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | Configuration information for pre-signed S3 URLs.
--
-- /Note:/ Consider using 'presignedURLConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujPresignedURLConfig :: Lens.Lens' UpdateJob (Lude.Maybe PresignedURLConfig)
ujPresignedURLConfig = Lens.lens (presignedURLConfig :: UpdateJob -> Lude.Maybe PresignedURLConfig) (\s a -> s {presignedURLConfig = a} :: UpdateJob)
{-# DEPRECATED ujPresignedURLConfig "Use generic-lens or generic-optics with 'presignedURLConfig' instead." #-}

-- | A short text description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujDescription :: Lens.Lens' UpdateJob (Lude.Maybe Lude.Text)
ujDescription = Lens.lens (description :: UpdateJob -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateJob)
{-# DEPRECATED ujDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the amount of time each device has to finish its execution of the job. The timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the time expires, it will be automatically set to @TIMED_OUT@ .
--
-- /Note:/ Consider using 'timeoutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujTimeoutConfig :: Lens.Lens' UpdateJob (Lude.Maybe TimeoutConfig)
ujTimeoutConfig = Lens.lens (timeoutConfig :: UpdateJob -> Lude.Maybe TimeoutConfig) (\s a -> s {timeoutConfig = a} :: UpdateJob)
{-# DEPRECATED ujTimeoutConfig "Use generic-lens or generic-optics with 'timeoutConfig' instead." #-}

instance Lude.AWSRequest UpdateJob where
  type Rs UpdateJob = UpdateJobResponse
  request = Req.patchJSON ioTService
  response = Res.receiveNull UpdateJobResponse'

instance Lude.ToHeaders UpdateJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateJob where
  toJSON UpdateJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("jobExecutionsRolloutConfig" Lude..=)
              Lude.<$> jobExecutionsRolloutConfig,
            ("abortConfig" Lude..=) Lude.<$> abortConfig,
            ("presignedUrlConfig" Lude..=) Lude.<$> presignedURLConfig,
            ("description" Lude..=) Lude.<$> description,
            ("timeoutConfig" Lude..=) Lude.<$> timeoutConfig
          ]
      )

instance Lude.ToPath UpdateJob where
  toPath UpdateJob' {..} = Lude.mconcat ["/jobs/", Lude.toBS jobId]

instance Lude.ToQuery UpdateJob where
  toQuery UpdateJob' {..} =
    Lude.mconcat ["namespaceId" Lude.=: namespaceId]

-- | /See:/ 'mkUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJobResponse' with the minimum fields required to make a request.
mkUpdateJobResponse ::
  UpdateJobResponse
mkUpdateJobResponse = UpdateJobResponse'
