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
-- Module      : Amazonka.IoT.Types.Job
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Job where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.AbortConfig
import Amazonka.IoT.Types.JobExecutionsRolloutConfig
import Amazonka.IoT.Types.JobProcessDetails
import Amazonka.IoT.Types.JobStatus
import Amazonka.IoT.Types.PresignedUrlConfig
import Amazonka.IoT.Types.TargetSelection
import Amazonka.IoT.Types.TimeoutConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The @Job@ object contains details about a job.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | The status of the job, one of @IN_PROGRESS@, @CANCELED@,
    -- @DELETION_IN_PROGRESS@ or @COMPLETED@.
    status :: Prelude.Maybe JobStatus,
    -- | Allows you to create a staged rollout of a job.
    jobExecutionsRolloutConfig :: Prelude.Maybe JobExecutionsRolloutConfig,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | An ARN identifying the job with format
    -- \"arn:aws:iot:region:account:job\/jobId\".
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Configuration for criteria to abort the job.
    abortConfig :: Prelude.Maybe AbortConfig,
    -- | Details about the job process.
    jobProcessDetails :: Prelude.Maybe JobProcessDetails,
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
    -- | If the job was updated, provides the reason code for the update.
    reasonCode :: Prelude.Maybe Prelude.Text,
    -- | Configuration for pre-signed S3 URLs.
    presignedUrlConfig :: Prelude.Maybe PresignedUrlConfig,
    -- | Will be @true@ if the job was canceled with the optional @force@
    -- parameter set to @true@.
    forceCanceled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the job template used to create the job.
    jobTemplateArn :: Prelude.Maybe Prelude.Text,
    -- | A list of IoT things and thing groups to which the job should be sent.
    targets :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The time, in seconds since the epoch, when the job was completed.
    completedAt :: Prelude.Maybe Core.POSIX,
    -- | If the job was updated, describes the reason for the update.
    comment :: Prelude.Maybe Prelude.Text,
    -- | A short text description of the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a device
    -- when the thing representing the device is added to a target group, even
    -- after the job was completed by all things originally in the group.
    targetSelection :: Prelude.Maybe TargetSelection,
    -- | Specifies the amount of time each device has to finish its execution of
    -- the job. A timer is started when the job execution status is set to
    -- @IN_PROGRESS@. If the job execution status is not set to another
    -- terminal state before the timer expires, it will be automatically set to
    -- @TIMED_OUT@.
    timeoutConfig :: Prelude.Maybe TimeoutConfig
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
-- 'status', 'job_status' - The status of the job, one of @IN_PROGRESS@, @CANCELED@,
-- @DELETION_IN_PROGRESS@ or @COMPLETED@.
--
-- 'jobExecutionsRolloutConfig', 'job_jobExecutionsRolloutConfig' - Allows you to create a staged rollout of a job.
--
-- 'jobId', 'job_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'lastUpdatedAt', 'job_lastUpdatedAt' - The time, in seconds since the epoch, when the job was last updated.
--
-- 'jobArn', 'job_jobArn' - An ARN identifying the job with format
-- \"arn:aws:iot:region:account:job\/jobId\".
--
-- 'createdAt', 'job_createdAt' - The time, in seconds since the epoch, when the job was created.
--
-- 'abortConfig', 'job_abortConfig' - Configuration for criteria to abort the job.
--
-- 'jobProcessDetails', 'job_jobProcessDetails' - Details about the job process.
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
-- 'reasonCode', 'job_reasonCode' - If the job was updated, provides the reason code for the update.
--
-- 'presignedUrlConfig', 'job_presignedUrlConfig' - Configuration for pre-signed S3 URLs.
--
-- 'forceCanceled', 'job_forceCanceled' - Will be @true@ if the job was canceled with the optional @force@
-- parameter set to @true@.
--
-- 'jobTemplateArn', 'job_jobTemplateArn' - The ARN of the job template used to create the job.
--
-- 'targets', 'job_targets' - A list of IoT things and thing groups to which the job should be sent.
--
-- 'completedAt', 'job_completedAt' - The time, in seconds since the epoch, when the job was completed.
--
-- 'comment', 'job_comment' - If the job was updated, describes the reason for the update.
--
-- 'description', 'job_description' - A short text description of the job.
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
newJob ::
  Job
newJob =
  Job'
    { status = Prelude.Nothing,
      jobExecutionsRolloutConfig = Prelude.Nothing,
      jobId = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      abortConfig = Prelude.Nothing,
      jobProcessDetails = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      reasonCode = Prelude.Nothing,
      presignedUrlConfig = Prelude.Nothing,
      forceCanceled = Prelude.Nothing,
      jobTemplateArn = Prelude.Nothing,
      targets = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      comment = Prelude.Nothing,
      description = Prelude.Nothing,
      targetSelection = Prelude.Nothing,
      timeoutConfig = Prelude.Nothing
    }

-- | The status of the job, one of @IN_PROGRESS@, @CANCELED@,
-- @DELETION_IN_PROGRESS@ or @COMPLETED@.
job_status :: Lens.Lens' Job (Prelude.Maybe JobStatus)
job_status = Lens.lens (\Job' {status} -> status) (\s@Job' {} a -> s {status = a} :: Job)

-- | Allows you to create a staged rollout of a job.
job_jobExecutionsRolloutConfig :: Lens.Lens' Job (Prelude.Maybe JobExecutionsRolloutConfig)
job_jobExecutionsRolloutConfig = Lens.lens (\Job' {jobExecutionsRolloutConfig} -> jobExecutionsRolloutConfig) (\s@Job' {} a -> s {jobExecutionsRolloutConfig = a} :: Job)

-- | The unique identifier you assigned to this job when it was created.
job_jobId :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_jobId = Lens.lens (\Job' {jobId} -> jobId) (\s@Job' {} a -> s {jobId = a} :: Job)

-- | The time, in seconds since the epoch, when the job was last updated.
job_lastUpdatedAt :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_lastUpdatedAt = Lens.lens (\Job' {lastUpdatedAt} -> lastUpdatedAt) (\s@Job' {} a -> s {lastUpdatedAt = a} :: Job) Prelude.. Lens.mapping Core._Time

-- | An ARN identifying the job with format
-- \"arn:aws:iot:region:account:job\/jobId\".
job_jobArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_jobArn = Lens.lens (\Job' {jobArn} -> jobArn) (\s@Job' {} a -> s {jobArn = a} :: Job)

-- | The time, in seconds since the epoch, when the job was created.
job_createdAt :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_createdAt = Lens.lens (\Job' {createdAt} -> createdAt) (\s@Job' {} a -> s {createdAt = a} :: Job) Prelude.. Lens.mapping Core._Time

-- | Configuration for criteria to abort the job.
job_abortConfig :: Lens.Lens' Job (Prelude.Maybe AbortConfig)
job_abortConfig = Lens.lens (\Job' {abortConfig} -> abortConfig) (\s@Job' {} a -> s {abortConfig = a} :: Job)

-- | Details about the job process.
job_jobProcessDetails :: Lens.Lens' Job (Prelude.Maybe JobProcessDetails)
job_jobProcessDetails = Lens.lens (\Job' {jobProcessDetails} -> jobProcessDetails) (\s@Job' {} a -> s {jobProcessDetails = a} :: Job)

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

-- | If the job was updated, provides the reason code for the update.
job_reasonCode :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_reasonCode = Lens.lens (\Job' {reasonCode} -> reasonCode) (\s@Job' {} a -> s {reasonCode = a} :: Job)

-- | Configuration for pre-signed S3 URLs.
job_presignedUrlConfig :: Lens.Lens' Job (Prelude.Maybe PresignedUrlConfig)
job_presignedUrlConfig = Lens.lens (\Job' {presignedUrlConfig} -> presignedUrlConfig) (\s@Job' {} a -> s {presignedUrlConfig = a} :: Job)

-- | Will be @true@ if the job was canceled with the optional @force@
-- parameter set to @true@.
job_forceCanceled :: Lens.Lens' Job (Prelude.Maybe Prelude.Bool)
job_forceCanceled = Lens.lens (\Job' {forceCanceled} -> forceCanceled) (\s@Job' {} a -> s {forceCanceled = a} :: Job)

-- | The ARN of the job template used to create the job.
job_jobTemplateArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_jobTemplateArn = Lens.lens (\Job' {jobTemplateArn} -> jobTemplateArn) (\s@Job' {} a -> s {jobTemplateArn = a} :: Job)

-- | A list of IoT things and thing groups to which the job should be sent.
job_targets :: Lens.Lens' Job (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
job_targets = Lens.lens (\Job' {targets} -> targets) (\s@Job' {} a -> s {targets = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The time, in seconds since the epoch, when the job was completed.
job_completedAt :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_completedAt = Lens.lens (\Job' {completedAt} -> completedAt) (\s@Job' {} a -> s {completedAt = a} :: Job) Prelude.. Lens.mapping Core._Time

-- | If the job was updated, describes the reason for the update.
job_comment :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_comment = Lens.lens (\Job' {comment} -> comment) (\s@Job' {} a -> s {comment = a} :: Job)

-- | A short text description of the job.
job_description :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_description = Lens.lens (\Job' {description} -> description) (\s@Job' {} a -> s {description = a} :: Job)

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a device
-- when the thing representing the device is added to a target group, even
-- after the job was completed by all things originally in the group.
job_targetSelection :: Lens.Lens' Job (Prelude.Maybe TargetSelection)
job_targetSelection = Lens.lens (\Job' {targetSelection} -> targetSelection) (\s@Job' {} a -> s {targetSelection = a} :: Job)

-- | Specifies the amount of time each device has to finish its execution of
-- the job. A timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the timer expires, it will be automatically set to
-- @TIMED_OUT@.
job_timeoutConfig :: Lens.Lens' Job (Prelude.Maybe TimeoutConfig)
job_timeoutConfig = Lens.lens (\Job' {timeoutConfig} -> timeoutConfig) (\s@Job' {} a -> s {timeoutConfig = a} :: Job)

instance Core.FromJSON Job where
  parseJSON =
    Core.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "jobExecutionsRolloutConfig")
            Prelude.<*> (x Core..:? "jobId")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "jobArn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "abortConfig")
            Prelude.<*> (x Core..:? "jobProcessDetails")
            Prelude.<*> (x Core..:? "namespaceId")
            Prelude.<*> (x Core..:? "reasonCode")
            Prelude.<*> (x Core..:? "presignedUrlConfig")
            Prelude.<*> (x Core..:? "forceCanceled")
            Prelude.<*> (x Core..:? "jobTemplateArn")
            Prelude.<*> (x Core..:? "targets")
            Prelude.<*> (x Core..:? "completedAt")
            Prelude.<*> (x Core..:? "comment")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "targetSelection")
            Prelude.<*> (x Core..:? "timeoutConfig")
      )

instance Prelude.Hashable Job where
  hashWithSalt salt' Job' {..} =
    salt' `Prelude.hashWithSalt` timeoutConfig
      `Prelude.hashWithSalt` targetSelection
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` completedAt
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` jobTemplateArn
      `Prelude.hashWithSalt` forceCanceled
      `Prelude.hashWithSalt` presignedUrlConfig
      `Prelude.hashWithSalt` reasonCode
      `Prelude.hashWithSalt` namespaceId
      `Prelude.hashWithSalt` jobProcessDetails
      `Prelude.hashWithSalt` abortConfig
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` jobArn
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobExecutionsRolloutConfig
      `Prelude.hashWithSalt` status

instance Prelude.NFData Job where
  rnf Job' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf timeoutConfig
      `Prelude.seq` Prelude.rnf targetSelection
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf completedAt
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf jobTemplateArn
      `Prelude.seq` Prelude.rnf forceCanceled
      `Prelude.seq` Prelude.rnf presignedUrlConfig
      `Prelude.seq` Prelude.rnf reasonCode
      `Prelude.seq` Prelude.rnf namespaceId
      `Prelude.seq` Prelude.rnf jobProcessDetails
      `Prelude.seq` Prelude.rnf abortConfig
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobExecutionsRolloutConfig
