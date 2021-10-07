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
import qualified Network.AWS.Prelude as Prelude

-- | The @Job@ object contains details about a job.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | Allows you to create a staged rollout of a job.
    jobExecutionsRolloutConfig :: Prelude.Maybe JobExecutionsRolloutConfig,
    -- | The status of the job, one of @IN_PROGRESS@, @CANCELED@,
    -- @DELETION_IN_PROGRESS@ or @COMPLETED@.
    status :: Prelude.Maybe JobStatus,
    -- | If the job was updated, provides the reason code for the update.
    reasonCode :: Prelude.Maybe Prelude.Text,
    -- | Specifies the amount of time each device has to finish its execution of
    -- the job. A timer is started when the job execution status is set to
    -- @IN_PROGRESS@. If the job execution status is not set to another
    -- terminal state before the timer expires, it will be automatically set to
    -- @TIMED_OUT@.
    timeoutConfig :: Prelude.Maybe TimeoutConfig,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a device
    -- when the thing representing the device is added to a target group, even
    -- after the job was completed by all things originally in the group.
    targetSelection :: Prelude.Maybe TargetSelection,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, Amazon Web Services IoT
    -- Core sends jobs notifications to MQTT topics that contain the value in
    -- the following format.
    --
    -- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | Details about the job process.
    jobProcessDetails :: Prelude.Maybe JobProcessDetails,
    -- | If the job was updated, describes the reason for the update.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job was completed.
    completedAt :: Prelude.Maybe Core.POSIX,
    -- | The time, in seconds since the epoch, when the job was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Will be @true@ if the job was canceled with the optional @force@
    -- parameter set to @true@.
    forceCanceled :: Prelude.Maybe Prelude.Bool,
    -- | An ARN identifying the job with format
    -- \"arn:aws:iot:region:account:job\/jobId\".
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | A list of IoT things and thing groups to which the job should be sent.
    targets :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Configuration for pre-signed S3 URLs.
    presignedUrlConfig :: Prelude.Maybe PresignedUrlConfig,
    -- | A short text description of the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | Configuration for criteria to abort the job.
    abortConfig :: Prelude.Maybe AbortConfig,
    -- | The ARN of the job template used to create the job.
    jobTemplateArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'reasonCode', 'job_reasonCode' - If the job was updated, provides the reason code for the update.
--
-- 'timeoutConfig', 'job_timeoutConfig' - Specifies the amount of time each device has to finish its execution of
-- the job. A timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the timer expires, it will be automatically set to
-- @TIMED_OUT@.
--
-- 'targetSelection', 'job_targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a device
-- when the thing representing the device is added to a target group, even
-- after the job was completed by all things originally in the group.
--
-- 'namespaceId', 'job_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
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
-- 'forceCanceled', 'job_forceCanceled' - Will be @true@ if the job was canceled with the optional @force@
-- parameter set to @true@.
--
-- 'jobArn', 'job_jobArn' - An ARN identifying the job with format
-- \"arn:aws:iot:region:account:job\/jobId\".
--
-- 'targets', 'job_targets' - A list of IoT things and thing groups to which the job should be sent.
--
-- 'presignedUrlConfig', 'job_presignedUrlConfig' - Configuration for pre-signed S3 URLs.
--
-- 'description', 'job_description' - A short text description of the job.
--
-- 'abortConfig', 'job_abortConfig' - Configuration for criteria to abort the job.
--
-- 'jobTemplateArn', 'job_jobTemplateArn' - The ARN of the job template used to create the job.
--
-- 'jobId', 'job_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'lastUpdatedAt', 'job_lastUpdatedAt' - The time, in seconds since the epoch, when the job was last updated.
newJob ::
  Job
newJob =
  Job'
    { jobExecutionsRolloutConfig = Prelude.Nothing,
      status = Prelude.Nothing,
      reasonCode = Prelude.Nothing,
      timeoutConfig = Prelude.Nothing,
      targetSelection = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      jobProcessDetails = Prelude.Nothing,
      comment = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      forceCanceled = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      targets = Prelude.Nothing,
      presignedUrlConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      abortConfig = Prelude.Nothing,
      jobTemplateArn = Prelude.Nothing,
      jobId = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing
    }

-- | Allows you to create a staged rollout of a job.
job_jobExecutionsRolloutConfig :: Lens.Lens' Job (Prelude.Maybe JobExecutionsRolloutConfig)
job_jobExecutionsRolloutConfig = Lens.lens (\Job' {jobExecutionsRolloutConfig} -> jobExecutionsRolloutConfig) (\s@Job' {} a -> s {jobExecutionsRolloutConfig = a} :: Job)

-- | The status of the job, one of @IN_PROGRESS@, @CANCELED@,
-- @DELETION_IN_PROGRESS@ or @COMPLETED@.
job_status :: Lens.Lens' Job (Prelude.Maybe JobStatus)
job_status = Lens.lens (\Job' {status} -> status) (\s@Job' {} a -> s {status = a} :: Job)

-- | If the job was updated, provides the reason code for the update.
job_reasonCode :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_reasonCode = Lens.lens (\Job' {reasonCode} -> reasonCode) (\s@Job' {} a -> s {reasonCode = a} :: Job)

-- | Specifies the amount of time each device has to finish its execution of
-- the job. A timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the timer expires, it will be automatically set to
-- @TIMED_OUT@.
job_timeoutConfig :: Lens.Lens' Job (Prelude.Maybe TimeoutConfig)
job_timeoutConfig = Lens.lens (\Job' {timeoutConfig} -> timeoutConfig) (\s@Job' {} a -> s {timeoutConfig = a} :: Job)

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a device
-- when the thing representing the device is added to a target group, even
-- after the job was completed by all things originally in the group.
job_targetSelection :: Lens.Lens' Job (Prelude.Maybe TargetSelection)
job_targetSelection = Lens.lens (\Job' {targetSelection} -> targetSelection) (\s@Job' {} a -> s {targetSelection = a} :: Job)

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
job_namespaceId :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_namespaceId = Lens.lens (\Job' {namespaceId} -> namespaceId) (\s@Job' {} a -> s {namespaceId = a} :: Job)

-- | Details about the job process.
job_jobProcessDetails :: Lens.Lens' Job (Prelude.Maybe JobProcessDetails)
job_jobProcessDetails = Lens.lens (\Job' {jobProcessDetails} -> jobProcessDetails) (\s@Job' {} a -> s {jobProcessDetails = a} :: Job)

-- | If the job was updated, describes the reason for the update.
job_comment :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_comment = Lens.lens (\Job' {comment} -> comment) (\s@Job' {} a -> s {comment = a} :: Job)

-- | The time, in seconds since the epoch, when the job was completed.
job_completedAt :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_completedAt = Lens.lens (\Job' {completedAt} -> completedAt) (\s@Job' {} a -> s {completedAt = a} :: Job) Prelude.. Lens.mapping Core._Time

-- | The time, in seconds since the epoch, when the job was created.
job_createdAt :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_createdAt = Lens.lens (\Job' {createdAt} -> createdAt) (\s@Job' {} a -> s {createdAt = a} :: Job) Prelude.. Lens.mapping Core._Time

-- | Will be @true@ if the job was canceled with the optional @force@
-- parameter set to @true@.
job_forceCanceled :: Lens.Lens' Job (Prelude.Maybe Prelude.Bool)
job_forceCanceled = Lens.lens (\Job' {forceCanceled} -> forceCanceled) (\s@Job' {} a -> s {forceCanceled = a} :: Job)

-- | An ARN identifying the job with format
-- \"arn:aws:iot:region:account:job\/jobId\".
job_jobArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_jobArn = Lens.lens (\Job' {jobArn} -> jobArn) (\s@Job' {} a -> s {jobArn = a} :: Job)

-- | A list of IoT things and thing groups to which the job should be sent.
job_targets :: Lens.Lens' Job (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
job_targets = Lens.lens (\Job' {targets} -> targets) (\s@Job' {} a -> s {targets = a} :: Job) Prelude.. Lens.mapping Lens._Coerce

-- | Configuration for pre-signed S3 URLs.
job_presignedUrlConfig :: Lens.Lens' Job (Prelude.Maybe PresignedUrlConfig)
job_presignedUrlConfig = Lens.lens (\Job' {presignedUrlConfig} -> presignedUrlConfig) (\s@Job' {} a -> s {presignedUrlConfig = a} :: Job)

-- | A short text description of the job.
job_description :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_description = Lens.lens (\Job' {description} -> description) (\s@Job' {} a -> s {description = a} :: Job)

-- | Configuration for criteria to abort the job.
job_abortConfig :: Lens.Lens' Job (Prelude.Maybe AbortConfig)
job_abortConfig = Lens.lens (\Job' {abortConfig} -> abortConfig) (\s@Job' {} a -> s {abortConfig = a} :: Job)

-- | The ARN of the job template used to create the job.
job_jobTemplateArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_jobTemplateArn = Lens.lens (\Job' {jobTemplateArn} -> jobTemplateArn) (\s@Job' {} a -> s {jobTemplateArn = a} :: Job)

-- | The unique identifier you assigned to this job when it was created.
job_jobId :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_jobId = Lens.lens (\Job' {jobId} -> jobId) (\s@Job' {} a -> s {jobId = a} :: Job)

-- | The time, in seconds since the epoch, when the job was last updated.
job_lastUpdatedAt :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_lastUpdatedAt = Lens.lens (\Job' {lastUpdatedAt} -> lastUpdatedAt) (\s@Job' {} a -> s {lastUpdatedAt = a} :: Job) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Job where
  parseJSON =
    Core.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Core..:? "jobExecutionsRolloutConfig")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "reasonCode")
            Prelude.<*> (x Core..:? "timeoutConfig")
            Prelude.<*> (x Core..:? "targetSelection")
            Prelude.<*> (x Core..:? "namespaceId")
            Prelude.<*> (x Core..:? "jobProcessDetails")
            Prelude.<*> (x Core..:? "comment")
            Prelude.<*> (x Core..:? "completedAt")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "forceCanceled")
            Prelude.<*> (x Core..:? "jobArn")
            Prelude.<*> (x Core..:? "targets")
            Prelude.<*> (x Core..:? "presignedUrlConfig")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "abortConfig")
            Prelude.<*> (x Core..:? "jobTemplateArn")
            Prelude.<*> (x Core..:? "jobId")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
      )

instance Prelude.Hashable Job

instance Prelude.NFData Job
