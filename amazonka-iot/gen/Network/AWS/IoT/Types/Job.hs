{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Job
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Job where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AbortConfig
import Network.AWS.IoT.Types.JobExecutionsRolloutConfig
import Network.AWS.IoT.Types.JobProcessDetails
import Network.AWS.IoT.Types.JobStatus
import Network.AWS.IoT.Types.PresignedUrlConfig
import Network.AWS.IoT.Types.TargetSelection
import Network.AWS.IoT.Types.TimeoutConfig
import qualified Network.AWS.Lens as Lens

-- | The @Job@ object contains details about a job.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | Allows you to create a staged rollout of a job.
    jobExecutionsRolloutConfig :: Core.Maybe JobExecutionsRolloutConfig,
    -- | The status of the job, one of @IN_PROGRESS@, @CANCELED@,
    -- @DELETION_IN_PROGRESS@ or @COMPLETED@.
    status :: Core.Maybe JobStatus,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a device
    -- when the thing representing the device is added to a target group, even
    -- after the job was completed by all things originally in the group.
    targetSelection :: Core.Maybe TargetSelection,
    -- | Specifies the amount of time each device has to finish its execution of
    -- the job. A timer is started when the job execution status is set to
    -- @IN_PROGRESS@. If the job execution status is not set to another
    -- terminal state before the timer expires, it will be automatically set to
    -- @TIMED_OUT@.
    timeoutConfig :: Core.Maybe TimeoutConfig,
    -- | If the job was updated, provides the reason code for the update.
    reasonCode :: Core.Maybe Core.Text,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs
    -- notifications to MQTT topics that contain the value in the following
    -- format.
    --
    -- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Core.Maybe Core.Text,
    -- | Details about the job process.
    jobProcessDetails :: Core.Maybe JobProcessDetails,
    -- | If the job was updated, describes the reason for the update.
    comment :: Core.Maybe Core.Text,
    -- | The time, in seconds since the epoch, when the job was completed.
    completedAt :: Core.Maybe Core.POSIX,
    -- | The time, in seconds since the epoch, when the job was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | An ARN identifying the job with format
    -- \"arn:aws:iot:region:account:job\/jobId\".
    jobArn :: Core.Maybe Core.Text,
    -- | A list of IoT things and thing groups to which the job should be sent.
    targets :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | Will be @true@ if the job was canceled with the optional @force@
    -- parameter set to @true@.
    forceCanceled :: Core.Maybe Core.Bool,
    -- | Configuration for pre-signed S3 URLs.
    presignedUrlConfig :: Core.Maybe PresignedUrlConfig,
    -- | A short text description of the job.
    description :: Core.Maybe Core.Text,
    -- | Configuration for criteria to abort the job.
    abortConfig :: Core.Maybe AbortConfig,
    -- | The time, in seconds since the epoch, when the job was last updated.
    lastUpdatedAt :: Core.Maybe Core.POSIX,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobExecutionsRolloutConfig', 'job_jobExecutionsRolloutConfig' - Allows you to create a staged rollout of a job.
--
-- 'status', 'job_status' - The status of the job, one of @IN_PROGRESS@, @CANCELED@,
-- @DELETION_IN_PROGRESS@ or @COMPLETED@.
--
-- 'targetSelection', 'job_targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a device
-- when the thing representing the device is added to a target group, even
-- after the job was completed by all things originally in the group.
--
-- 'timeoutConfig', 'job_timeoutConfig' - Specifies the amount of time each device has to finish its execution of
-- the job. A timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the timer expires, it will be automatically set to
-- @TIMED_OUT@.
--
-- 'reasonCode', 'job_reasonCode' - If the job was updated, provides the reason code for the update.
--
-- 'namespaceId', 'job_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'jobProcessDetails', 'job_jobProcessDetails' - Details about the job process.
--
-- 'comment', 'job_comment' - If the job was updated, describes the reason for the update.
--
-- 'completedAt', 'job_completedAt' - The time, in seconds since the epoch, when the job was completed.
--
-- 'createdAt', 'job_createdAt' - The time, in seconds since the epoch, when the job was created.
--
-- 'jobArn', 'job_jobArn' - An ARN identifying the job with format
-- \"arn:aws:iot:region:account:job\/jobId\".
--
-- 'targets', 'job_targets' - A list of IoT things and thing groups to which the job should be sent.
--
-- 'forceCanceled', 'job_forceCanceled' - Will be @true@ if the job was canceled with the optional @force@
-- parameter set to @true@.
--
-- 'presignedUrlConfig', 'job_presignedUrlConfig' - Configuration for pre-signed S3 URLs.
--
-- 'description', 'job_description' - A short text description of the job.
--
-- 'abortConfig', 'job_abortConfig' - Configuration for criteria to abort the job.
--
-- 'lastUpdatedAt', 'job_lastUpdatedAt' - The time, in seconds since the epoch, when the job was last updated.
--
-- 'jobId', 'job_jobId' - The unique identifier you assigned to this job when it was created.
newJob ::
  Job
newJob =
  Job'
    { jobExecutionsRolloutConfig = Core.Nothing,
      status = Core.Nothing,
      targetSelection = Core.Nothing,
      timeoutConfig = Core.Nothing,
      reasonCode = Core.Nothing,
      namespaceId = Core.Nothing,
      jobProcessDetails = Core.Nothing,
      comment = Core.Nothing,
      completedAt = Core.Nothing,
      createdAt = Core.Nothing,
      jobArn = Core.Nothing,
      targets = Core.Nothing,
      forceCanceled = Core.Nothing,
      presignedUrlConfig = Core.Nothing,
      description = Core.Nothing,
      abortConfig = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      jobId = Core.Nothing
    }

-- | Allows you to create a staged rollout of a job.
job_jobExecutionsRolloutConfig :: Lens.Lens' Job (Core.Maybe JobExecutionsRolloutConfig)
job_jobExecutionsRolloutConfig = Lens.lens (\Job' {jobExecutionsRolloutConfig} -> jobExecutionsRolloutConfig) (\s@Job' {} a -> s {jobExecutionsRolloutConfig = a} :: Job)

-- | The status of the job, one of @IN_PROGRESS@, @CANCELED@,
-- @DELETION_IN_PROGRESS@ or @COMPLETED@.
job_status :: Lens.Lens' Job (Core.Maybe JobStatus)
job_status = Lens.lens (\Job' {status} -> status) (\s@Job' {} a -> s {status = a} :: Job)

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a device
-- when the thing representing the device is added to a target group, even
-- after the job was completed by all things originally in the group.
job_targetSelection :: Lens.Lens' Job (Core.Maybe TargetSelection)
job_targetSelection = Lens.lens (\Job' {targetSelection} -> targetSelection) (\s@Job' {} a -> s {targetSelection = a} :: Job)

-- | Specifies the amount of time each device has to finish its execution of
-- the job. A timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the timer expires, it will be automatically set to
-- @TIMED_OUT@.
job_timeoutConfig :: Lens.Lens' Job (Core.Maybe TimeoutConfig)
job_timeoutConfig = Lens.lens (\Job' {timeoutConfig} -> timeoutConfig) (\s@Job' {} a -> s {timeoutConfig = a} :: Job)

-- | If the job was updated, provides the reason code for the update.
job_reasonCode :: Lens.Lens' Job (Core.Maybe Core.Text)
job_reasonCode = Lens.lens (\Job' {reasonCode} -> reasonCode) (\s@Job' {} a -> s {reasonCode = a} :: Job)

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
job_namespaceId :: Lens.Lens' Job (Core.Maybe Core.Text)
job_namespaceId = Lens.lens (\Job' {namespaceId} -> namespaceId) (\s@Job' {} a -> s {namespaceId = a} :: Job)

-- | Details about the job process.
job_jobProcessDetails :: Lens.Lens' Job (Core.Maybe JobProcessDetails)
job_jobProcessDetails = Lens.lens (\Job' {jobProcessDetails} -> jobProcessDetails) (\s@Job' {} a -> s {jobProcessDetails = a} :: Job)

-- | If the job was updated, describes the reason for the update.
job_comment :: Lens.Lens' Job (Core.Maybe Core.Text)
job_comment = Lens.lens (\Job' {comment} -> comment) (\s@Job' {} a -> s {comment = a} :: Job)

-- | The time, in seconds since the epoch, when the job was completed.
job_completedAt :: Lens.Lens' Job (Core.Maybe Core.UTCTime)
job_completedAt = Lens.lens (\Job' {completedAt} -> completedAt) (\s@Job' {} a -> s {completedAt = a} :: Job) Core.. Lens.mapping Core._Time

-- | The time, in seconds since the epoch, when the job was created.
job_createdAt :: Lens.Lens' Job (Core.Maybe Core.UTCTime)
job_createdAt = Lens.lens (\Job' {createdAt} -> createdAt) (\s@Job' {} a -> s {createdAt = a} :: Job) Core.. Lens.mapping Core._Time

-- | An ARN identifying the job with format
-- \"arn:aws:iot:region:account:job\/jobId\".
job_jobArn :: Lens.Lens' Job (Core.Maybe Core.Text)
job_jobArn = Lens.lens (\Job' {jobArn} -> jobArn) (\s@Job' {} a -> s {jobArn = a} :: Job)

-- | A list of IoT things and thing groups to which the job should be sent.
job_targets :: Lens.Lens' Job (Core.Maybe (Core.NonEmpty Core.Text))
job_targets = Lens.lens (\Job' {targets} -> targets) (\s@Job' {} a -> s {targets = a} :: Job) Core.. Lens.mapping Lens._Coerce

-- | Will be @true@ if the job was canceled with the optional @force@
-- parameter set to @true@.
job_forceCanceled :: Lens.Lens' Job (Core.Maybe Core.Bool)
job_forceCanceled = Lens.lens (\Job' {forceCanceled} -> forceCanceled) (\s@Job' {} a -> s {forceCanceled = a} :: Job)

-- | Configuration for pre-signed S3 URLs.
job_presignedUrlConfig :: Lens.Lens' Job (Core.Maybe PresignedUrlConfig)
job_presignedUrlConfig = Lens.lens (\Job' {presignedUrlConfig} -> presignedUrlConfig) (\s@Job' {} a -> s {presignedUrlConfig = a} :: Job)

-- | A short text description of the job.
job_description :: Lens.Lens' Job (Core.Maybe Core.Text)
job_description = Lens.lens (\Job' {description} -> description) (\s@Job' {} a -> s {description = a} :: Job)

-- | Configuration for criteria to abort the job.
job_abortConfig :: Lens.Lens' Job (Core.Maybe AbortConfig)
job_abortConfig = Lens.lens (\Job' {abortConfig} -> abortConfig) (\s@Job' {} a -> s {abortConfig = a} :: Job)

-- | The time, in seconds since the epoch, when the job was last updated.
job_lastUpdatedAt :: Lens.Lens' Job (Core.Maybe Core.UTCTime)
job_lastUpdatedAt = Lens.lens (\Job' {lastUpdatedAt} -> lastUpdatedAt) (\s@Job' {} a -> s {lastUpdatedAt = a} :: Job) Core.. Lens.mapping Core._Time

-- | The unique identifier you assigned to this job when it was created.
job_jobId :: Lens.Lens' Job (Core.Maybe Core.Text)
job_jobId = Lens.lens (\Job' {jobId} -> jobId) (\s@Job' {} a -> s {jobId = a} :: Job)

instance Core.FromJSON Job where
  parseJSON =
    Core.withObject
      "Job"
      ( \x ->
          Job'
            Core.<$> (x Core..:? "jobExecutionsRolloutConfig")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "targetSelection")
            Core.<*> (x Core..:? "timeoutConfig")
            Core.<*> (x Core..:? "reasonCode")
            Core.<*> (x Core..:? "namespaceId")
            Core.<*> (x Core..:? "jobProcessDetails")
            Core.<*> (x Core..:? "comment")
            Core.<*> (x Core..:? "completedAt")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "jobArn")
            Core.<*> (x Core..:? "targets")
            Core.<*> (x Core..:? "forceCanceled")
            Core.<*> (x Core..:? "presignedUrlConfig")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "abortConfig")
            Core.<*> (x Core..:? "lastUpdatedAt")
            Core.<*> (x Core..:? "jobId")
      )

instance Core.Hashable Job

instance Core.NFData Job
