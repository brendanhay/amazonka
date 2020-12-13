{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Job
  ( Job (..),

    -- * Smart constructor
    mkJob,

    -- * Lenses
    jStatus,
    jJobExecutionsRolloutConfig,
    jJobId,
    jLastUpdatedAt,
    jJobARN,
    jCreatedAt,
    jAbortConfig,
    jJobProcessDetails,
    jNamespaceId,
    jReasonCode,
    jPresignedURLConfig,
    jForceCanceled,
    jTargets,
    jCompletedAt,
    jComment,
    jDescription,
    jTargetSelection,
    jTimeoutConfig,
  )
where

import Network.AWS.IoT.Types.AbortConfig
import Network.AWS.IoT.Types.JobExecutionsRolloutConfig
import Network.AWS.IoT.Types.JobProcessDetails
import Network.AWS.IoT.Types.JobStatus
import Network.AWS.IoT.Types.PresignedURLConfig
import Network.AWS.IoT.Types.TargetSelection
import Network.AWS.IoT.Types.TimeoutConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @Job@ object contains details about a job.
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { -- | The status of the job, one of @IN_PROGRESS@ , @CANCELED@ , @DELETION_IN_PROGRESS@ or @COMPLETED@ .
    status :: Lude.Maybe JobStatus,
    -- | Allows you to create a staged rollout of a job.
    jobExecutionsRolloutConfig :: Lude.Maybe JobExecutionsRolloutConfig,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Lude.Maybe Lude.Text,
    -- | The time, in seconds since the epoch, when the job was last updated.
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    -- | An ARN identifying the job with format "arn:aws:iot:region:account:job/jobId".
    jobARN :: Lude.Maybe Lude.Text,
    -- | The time, in seconds since the epoch, when the job was created.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | Configuration for criteria to abort the job.
    abortConfig :: Lude.Maybe AbortConfig,
    -- | Details about the job process.
    jobProcessDetails :: Lude.Maybe JobProcessDetails,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
    -- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
    namespaceId :: Lude.Maybe Lude.Text,
    -- | If the job was updated, provides the reason code for the update.
    reasonCode :: Lude.Maybe Lude.Text,
    -- | Configuration for pre-signed S3 URLs.
    presignedURLConfig :: Lude.Maybe PresignedURLConfig,
    -- | Will be @true@ if the job was canceled with the optional @force@ parameter set to @true@ .
    forceCanceled :: Lude.Maybe Lude.Bool,
    -- | A list of IoT things and thing groups to which the job should be sent.
    targets :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The time, in seconds since the epoch, when the job was completed.
    completedAt :: Lude.Maybe Lude.Timestamp,
    -- | If the job was updated, describes the reason for the update.
    comment :: Lude.Maybe Lude.Text,
    -- | A short text description of the job.
    description :: Lude.Maybe Lude.Text,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a device when the thing representing the device is added to a target group, even after the job was completed by all things originally in the group.
    targetSelection :: Lude.Maybe TargetSelection,
    -- | Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
    timeoutConfig :: Lude.Maybe TimeoutConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- * 'status' - The status of the job, one of @IN_PROGRESS@ , @CANCELED@ , @DELETION_IN_PROGRESS@ or @COMPLETED@ .
-- * 'jobExecutionsRolloutConfig' - Allows you to create a staged rollout of a job.
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
-- * 'lastUpdatedAt' - The time, in seconds since the epoch, when the job was last updated.
-- * 'jobARN' - An ARN identifying the job with format "arn:aws:iot:region:account:job/jobId".
-- * 'createdAt' - The time, in seconds since the epoch, when the job was created.
-- * 'abortConfig' - Configuration for criteria to abort the job.
-- * 'jobProcessDetails' - Details about the job process.
-- * 'namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
-- * 'reasonCode' - If the job was updated, provides the reason code for the update.
-- * 'presignedURLConfig' - Configuration for pre-signed S3 URLs.
-- * 'forceCanceled' - Will be @true@ if the job was canceled with the optional @force@ parameter set to @true@ .
-- * 'targets' - A list of IoT things and thing groups to which the job should be sent.
-- * 'completedAt' - The time, in seconds since the epoch, when the job was completed.
-- * 'comment' - If the job was updated, describes the reason for the update.
-- * 'description' - A short text description of the job.
-- * 'targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a device when the thing representing the device is added to a target group, even after the job was completed by all things originally in the group.
-- * 'timeoutConfig' - Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
mkJob ::
  Job
mkJob =
  Job'
    { status = Lude.Nothing,
      jobExecutionsRolloutConfig = Lude.Nothing,
      jobId = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      jobARN = Lude.Nothing,
      createdAt = Lude.Nothing,
      abortConfig = Lude.Nothing,
      jobProcessDetails = Lude.Nothing,
      namespaceId = Lude.Nothing,
      reasonCode = Lude.Nothing,
      presignedURLConfig = Lude.Nothing,
      forceCanceled = Lude.Nothing,
      targets = Lude.Nothing,
      completedAt = Lude.Nothing,
      comment = Lude.Nothing,
      description = Lude.Nothing,
      targetSelection = Lude.Nothing,
      timeoutConfig = Lude.Nothing
    }

-- | The status of the job, one of @IN_PROGRESS@ , @CANCELED@ , @DELETION_IN_PROGRESS@ or @COMPLETED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStatus :: Lens.Lens' Job (Lude.Maybe JobStatus)
jStatus = Lens.lens (status :: Job -> Lude.Maybe JobStatus) (\s a -> s {status = a} :: Job)
{-# DEPRECATED jStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Allows you to create a staged rollout of a job.
--
-- /Note:/ Consider using 'jobExecutionsRolloutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobExecutionsRolloutConfig :: Lens.Lens' Job (Lude.Maybe JobExecutionsRolloutConfig)
jJobExecutionsRolloutConfig = Lens.lens (jobExecutionsRolloutConfig :: Job -> Lude.Maybe JobExecutionsRolloutConfig) (\s a -> s {jobExecutionsRolloutConfig = a} :: Job)
{-# DEPRECATED jJobExecutionsRolloutConfig "Use generic-lens or generic-optics with 'jobExecutionsRolloutConfig' instead." #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobId :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jJobId = Lens.lens (jobId :: Job -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: Job)
{-# DEPRECATED jJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The time, in seconds since the epoch, when the job was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jLastUpdatedAt :: Lens.Lens' Job (Lude.Maybe Lude.Timestamp)
jLastUpdatedAt = Lens.lens (lastUpdatedAt :: Job -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: Job)
{-# DEPRECATED jLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | An ARN identifying the job with format "arn:aws:iot:region:account:job/jobId".
--
-- /Note:/ Consider using 'jobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobARN :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jJobARN = Lens.lens (jobARN :: Job -> Lude.Maybe Lude.Text) (\s a -> s {jobARN = a} :: Job)
{-# DEPRECATED jJobARN "Use generic-lens or generic-optics with 'jobARN' instead." #-}

-- | The time, in seconds since the epoch, when the job was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCreatedAt :: Lens.Lens' Job (Lude.Maybe Lude.Timestamp)
jCreatedAt = Lens.lens (createdAt :: Job -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Job)
{-# DEPRECATED jCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Configuration for criteria to abort the job.
--
-- /Note:/ Consider using 'abortConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jAbortConfig :: Lens.Lens' Job (Lude.Maybe AbortConfig)
jAbortConfig = Lens.lens (abortConfig :: Job -> Lude.Maybe AbortConfig) (\s a -> s {abortConfig = a} :: Job)
{-# DEPRECATED jAbortConfig "Use generic-lens or generic-optics with 'abortConfig' instead." #-}

-- | Details about the job process.
--
-- /Note:/ Consider using 'jobProcessDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobProcessDetails :: Lens.Lens' Job (Lude.Maybe JobProcessDetails)
jJobProcessDetails = Lens.lens (jobProcessDetails :: Job -> Lude.Maybe JobProcessDetails) (\s a -> s {jobProcessDetails = a} :: Job)
{-# DEPRECATED jJobProcessDetails "Use generic-lens or generic-optics with 'jobProcessDetails' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jNamespaceId :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jNamespaceId = Lens.lens (namespaceId :: Job -> Lude.Maybe Lude.Text) (\s a -> s {namespaceId = a} :: Job)
{-# DEPRECATED jNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | If the job was updated, provides the reason code for the update.
--
-- /Note:/ Consider using 'reasonCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jReasonCode :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jReasonCode = Lens.lens (reasonCode :: Job -> Lude.Maybe Lude.Text) (\s a -> s {reasonCode = a} :: Job)
{-# DEPRECATED jReasonCode "Use generic-lens or generic-optics with 'reasonCode' instead." #-}

-- | Configuration for pre-signed S3 URLs.
--
-- /Note:/ Consider using 'presignedURLConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jPresignedURLConfig :: Lens.Lens' Job (Lude.Maybe PresignedURLConfig)
jPresignedURLConfig = Lens.lens (presignedURLConfig :: Job -> Lude.Maybe PresignedURLConfig) (\s a -> s {presignedURLConfig = a} :: Job)
{-# DEPRECATED jPresignedURLConfig "Use generic-lens or generic-optics with 'presignedURLConfig' instead." #-}

-- | Will be @true@ if the job was canceled with the optional @force@ parameter set to @true@ .
--
-- /Note:/ Consider using 'forceCanceled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jForceCanceled :: Lens.Lens' Job (Lude.Maybe Lude.Bool)
jForceCanceled = Lens.lens (forceCanceled :: Job -> Lude.Maybe Lude.Bool) (\s a -> s {forceCanceled = a} :: Job)
{-# DEPRECATED jForceCanceled "Use generic-lens or generic-optics with 'forceCanceled' instead." #-}

-- | A list of IoT things and thing groups to which the job should be sent.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTargets :: Lens.Lens' Job (Lude.Maybe (Lude.NonEmpty Lude.Text))
jTargets = Lens.lens (targets :: Job -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {targets = a} :: Job)
{-# DEPRECATED jTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The time, in seconds since the epoch, when the job was completed.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCompletedAt :: Lens.Lens' Job (Lude.Maybe Lude.Timestamp)
jCompletedAt = Lens.lens (completedAt :: Job -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedAt = a} :: Job)
{-# DEPRECATED jCompletedAt "Use generic-lens or generic-optics with 'completedAt' instead." #-}

-- | If the job was updated, describes the reason for the update.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jComment :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jComment = Lens.lens (comment :: Job -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: Job)
{-# DEPRECATED jComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A short text description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jDescription :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jDescription = Lens.lens (description :: Job -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Job)
{-# DEPRECATED jDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a device when the thing representing the device is added to a target group, even after the job was completed by all things originally in the group.
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTargetSelection :: Lens.Lens' Job (Lude.Maybe TargetSelection)
jTargetSelection = Lens.lens (targetSelection :: Job -> Lude.Maybe TargetSelection) (\s a -> s {targetSelection = a} :: Job)
{-# DEPRECATED jTargetSelection "Use generic-lens or generic-optics with 'targetSelection' instead." #-}

-- | Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
--
-- /Note:/ Consider using 'timeoutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTimeoutConfig :: Lens.Lens' Job (Lude.Maybe TimeoutConfig)
jTimeoutConfig = Lens.lens (timeoutConfig :: Job -> Lude.Maybe TimeoutConfig) (\s a -> s {timeoutConfig = a} :: Job)
{-# DEPRECATED jTimeoutConfig "Use generic-lens or generic-optics with 'timeoutConfig' instead." #-}

instance Lude.FromJSON Job where
  parseJSON =
    Lude.withObject
      "Job"
      ( \x ->
          Job'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "jobExecutionsRolloutConfig")
            Lude.<*> (x Lude..:? "jobId")
            Lude.<*> (x Lude..:? "lastUpdatedAt")
            Lude.<*> (x Lude..:? "jobArn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "abortConfig")
            Lude.<*> (x Lude..:? "jobProcessDetails")
            Lude.<*> (x Lude..:? "namespaceId")
            Lude.<*> (x Lude..:? "reasonCode")
            Lude.<*> (x Lude..:? "presignedUrlConfig")
            Lude.<*> (x Lude..:? "forceCanceled")
            Lude.<*> (x Lude..:? "targets")
            Lude.<*> (x Lude..:? "completedAt")
            Lude.<*> (x Lude..:? "comment")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "targetSelection")
            Lude.<*> (x Lude..:? "timeoutConfig")
      )
