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
    jAbortConfig,
    jComment,
    jCompletedAt,
    jCreatedAt,
    jDescription,
    jForceCanceled,
    jJobArn,
    jJobExecutionsRolloutConfig,
    jJobId,
    jJobProcessDetails,
    jLastUpdatedAt,
    jNamespaceId,
    jPresignedUrlConfig,
    jReasonCode,
    jStatus,
    jTargetSelection,
    jTargets,
    jTimeoutConfig,
  )
where

import qualified Network.AWS.IoT.Types.AbortConfig as Types
import qualified Network.AWS.IoT.Types.Comment as Types
import qualified Network.AWS.IoT.Types.JobArn as Types
import qualified Network.AWS.IoT.Types.JobDescription as Types
import qualified Network.AWS.IoT.Types.JobExecutionsRolloutConfig as Types
import qualified Network.AWS.IoT.Types.JobId as Types
import qualified Network.AWS.IoT.Types.JobProcessDetails as Types
import qualified Network.AWS.IoT.Types.JobStatus as Types
import qualified Network.AWS.IoT.Types.NamespaceId as Types
import qualified Network.AWS.IoT.Types.PresignedUrlConfig as Types
import qualified Network.AWS.IoT.Types.ReasonCode as Types
import qualified Network.AWS.IoT.Types.TargetArn as Types
import qualified Network.AWS.IoT.Types.TargetSelection as Types
import qualified Network.AWS.IoT.Types.TimeoutConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @Job@ object contains details about a job.
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { -- | Configuration for criteria to abort the job.
    abortConfig :: Core.Maybe Types.AbortConfig,
    -- | If the job was updated, describes the reason for the update.
    comment :: Core.Maybe Types.Comment,
    -- | The time, in seconds since the epoch, when the job was completed.
    completedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The time, in seconds since the epoch, when the job was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | A short text description of the job.
    description :: Core.Maybe Types.JobDescription,
    -- | Will be @true@ if the job was canceled with the optional @force@ parameter set to @true@ .
    forceCanceled :: Core.Maybe Core.Bool,
    -- | An ARN identifying the job with format "arn:aws:iot:region:account:job/jobId".
    jobArn :: Core.Maybe Types.JobArn,
    -- | Allows you to create a staged rollout of a job.
    jobExecutionsRolloutConfig :: Core.Maybe Types.JobExecutionsRolloutConfig,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Maybe Types.JobId,
    -- | Details about the job process.
    jobProcessDetails :: Core.Maybe Types.JobProcessDetails,
    -- | The time, in seconds since the epoch, when the job was last updated.
    lastUpdatedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
    -- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
    namespaceId :: Core.Maybe Types.NamespaceId,
    -- | Configuration for pre-signed S3 URLs.
    presignedUrlConfig :: Core.Maybe Types.PresignedUrlConfig,
    -- | If the job was updated, provides the reason code for the update.
    reasonCode :: Core.Maybe Types.ReasonCode,
    -- | The status of the job, one of @IN_PROGRESS@ , @CANCELED@ , @DELETION_IN_PROGRESS@ or @COMPLETED@ .
    status :: Core.Maybe Types.JobStatus,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a device when the thing representing the device is added to a target group, even after the job was completed by all things originally in the group.
    targetSelection :: Core.Maybe Types.TargetSelection,
    -- | A list of IoT things and thing groups to which the job should be sent.
    targets :: Core.Maybe (Core.NonEmpty Types.TargetArn),
    -- | Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
    timeoutConfig :: Core.Maybe Types.TimeoutConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Job' value with any optional fields omitted.
mkJob ::
  Job
mkJob =
  Job'
    { abortConfig = Core.Nothing,
      comment = Core.Nothing,
      completedAt = Core.Nothing,
      createdAt = Core.Nothing,
      description = Core.Nothing,
      forceCanceled = Core.Nothing,
      jobArn = Core.Nothing,
      jobExecutionsRolloutConfig = Core.Nothing,
      jobId = Core.Nothing,
      jobProcessDetails = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      namespaceId = Core.Nothing,
      presignedUrlConfig = Core.Nothing,
      reasonCode = Core.Nothing,
      status = Core.Nothing,
      targetSelection = Core.Nothing,
      targets = Core.Nothing,
      timeoutConfig = Core.Nothing
    }

-- | Configuration for criteria to abort the job.
--
-- /Note:/ Consider using 'abortConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jAbortConfig :: Lens.Lens' Job (Core.Maybe Types.AbortConfig)
jAbortConfig = Lens.field @"abortConfig"
{-# DEPRECATED jAbortConfig "Use generic-lens or generic-optics with 'abortConfig' instead." #-}

-- | If the job was updated, describes the reason for the update.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jComment :: Lens.Lens' Job (Core.Maybe Types.Comment)
jComment = Lens.field @"comment"
{-# DEPRECATED jComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The time, in seconds since the epoch, when the job was completed.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCompletedAt :: Lens.Lens' Job (Core.Maybe Core.NominalDiffTime)
jCompletedAt = Lens.field @"completedAt"
{-# DEPRECATED jCompletedAt "Use generic-lens or generic-optics with 'completedAt' instead." #-}

-- | The time, in seconds since the epoch, when the job was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCreatedAt :: Lens.Lens' Job (Core.Maybe Core.NominalDiffTime)
jCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED jCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | A short text description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jDescription :: Lens.Lens' Job (Core.Maybe Types.JobDescription)
jDescription = Lens.field @"description"
{-# DEPRECATED jDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Will be @true@ if the job was canceled with the optional @force@ parameter set to @true@ .
--
-- /Note:/ Consider using 'forceCanceled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jForceCanceled :: Lens.Lens' Job (Core.Maybe Core.Bool)
jForceCanceled = Lens.field @"forceCanceled"
{-# DEPRECATED jForceCanceled "Use generic-lens or generic-optics with 'forceCanceled' instead." #-}

-- | An ARN identifying the job with format "arn:aws:iot:region:account:job/jobId".
--
-- /Note:/ Consider using 'jobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobArn :: Lens.Lens' Job (Core.Maybe Types.JobArn)
jJobArn = Lens.field @"jobArn"
{-# DEPRECATED jJobArn "Use generic-lens or generic-optics with 'jobArn' instead." #-}

-- | Allows you to create a staged rollout of a job.
--
-- /Note:/ Consider using 'jobExecutionsRolloutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobExecutionsRolloutConfig :: Lens.Lens' Job (Core.Maybe Types.JobExecutionsRolloutConfig)
jJobExecutionsRolloutConfig = Lens.field @"jobExecutionsRolloutConfig"
{-# DEPRECATED jJobExecutionsRolloutConfig "Use generic-lens or generic-optics with 'jobExecutionsRolloutConfig' instead." #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobId :: Lens.Lens' Job (Core.Maybe Types.JobId)
jJobId = Lens.field @"jobId"
{-# DEPRECATED jJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Details about the job process.
--
-- /Note:/ Consider using 'jobProcessDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobProcessDetails :: Lens.Lens' Job (Core.Maybe Types.JobProcessDetails)
jJobProcessDetails = Lens.field @"jobProcessDetails"
{-# DEPRECATED jJobProcessDetails "Use generic-lens or generic-optics with 'jobProcessDetails' instead." #-}

-- | The time, in seconds since the epoch, when the job was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jLastUpdatedAt :: Lens.Lens' Job (Core.Maybe Core.NominalDiffTime)
jLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# DEPRECATED jLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jNamespaceId :: Lens.Lens' Job (Core.Maybe Types.NamespaceId)
jNamespaceId = Lens.field @"namespaceId"
{-# DEPRECATED jNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | Configuration for pre-signed S3 URLs.
--
-- /Note:/ Consider using 'presignedUrlConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jPresignedUrlConfig :: Lens.Lens' Job (Core.Maybe Types.PresignedUrlConfig)
jPresignedUrlConfig = Lens.field @"presignedUrlConfig"
{-# DEPRECATED jPresignedUrlConfig "Use generic-lens or generic-optics with 'presignedUrlConfig' instead." #-}

-- | If the job was updated, provides the reason code for the update.
--
-- /Note:/ Consider using 'reasonCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jReasonCode :: Lens.Lens' Job (Core.Maybe Types.ReasonCode)
jReasonCode = Lens.field @"reasonCode"
{-# DEPRECATED jReasonCode "Use generic-lens or generic-optics with 'reasonCode' instead." #-}

-- | The status of the job, one of @IN_PROGRESS@ , @CANCELED@ , @DELETION_IN_PROGRESS@ or @COMPLETED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStatus :: Lens.Lens' Job (Core.Maybe Types.JobStatus)
jStatus = Lens.field @"status"
{-# DEPRECATED jStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a device when the thing representing the device is added to a target group, even after the job was completed by all things originally in the group.
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTargetSelection :: Lens.Lens' Job (Core.Maybe Types.TargetSelection)
jTargetSelection = Lens.field @"targetSelection"
{-# DEPRECATED jTargetSelection "Use generic-lens or generic-optics with 'targetSelection' instead." #-}

-- | A list of IoT things and thing groups to which the job should be sent.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTargets :: Lens.Lens' Job (Core.Maybe (Core.NonEmpty Types.TargetArn))
jTargets = Lens.field @"targets"
{-# DEPRECATED jTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
--
-- /Note:/ Consider using 'timeoutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTimeoutConfig :: Lens.Lens' Job (Core.Maybe Types.TimeoutConfig)
jTimeoutConfig = Lens.field @"timeoutConfig"
{-# DEPRECATED jTimeoutConfig "Use generic-lens or generic-optics with 'timeoutConfig' instead." #-}

instance Core.FromJSON Job where
  parseJSON =
    Core.withObject "Job" Core.$
      \x ->
        Job'
          Core.<$> (x Core..:? "abortConfig")
          Core.<*> (x Core..:? "comment")
          Core.<*> (x Core..:? "completedAt")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "forceCanceled")
          Core.<*> (x Core..:? "jobArn")
          Core.<*> (x Core..:? "jobExecutionsRolloutConfig")
          Core.<*> (x Core..:? "jobId")
          Core.<*> (x Core..:? "jobProcessDetails")
          Core.<*> (x Core..:? "lastUpdatedAt")
          Core.<*> (x Core..:? "namespaceId")
          Core.<*> (x Core..:? "presignedUrlConfig")
          Core.<*> (x Core..:? "reasonCode")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "targetSelection")
          Core.<*> (x Core..:? "targets")
          Core.<*> (x Core..:? "timeoutConfig")
