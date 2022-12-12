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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Job where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AbortConfig
import Amazonka.IoT.Types.JobExecutionsRetryConfig
import Amazonka.IoT.Types.JobExecutionsRolloutConfig
import Amazonka.IoT.Types.JobProcessDetails
import Amazonka.IoT.Types.JobStatus
import Amazonka.IoT.Types.PresignedUrlConfig
import Amazonka.IoT.Types.SchedulingConfig
import Amazonka.IoT.Types.TargetSelection
import Amazonka.IoT.Types.TimeoutConfig
import qualified Amazonka.Prelude as Prelude

-- | The @Job@ object contains details about a job.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | Configuration for criteria to abort the job.
    abortConfig :: Prelude.Maybe AbortConfig,
    -- | If the job was updated, describes the reason for the update.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job was completed.
    completedAt :: Prelude.Maybe Data.POSIX,
    -- | The time, in seconds since the epoch, when the job was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | A short text description of the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | A key-value map that pairs the patterns that need to be replaced in a
    -- managed template job document schema. You can use the description of
    -- each key as a guidance to specify the inputs during runtime when
    -- creating a job.
    --
    -- @documentParameters@ can only be used when creating jobs from Amazon Web
    -- Services managed templates. This parameter can\'t be used with custom
    -- job templates or to create jobs from them.
    documentParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Will be @true@ if the job was canceled with the optional @force@
    -- parameter set to @true@.
    forceCanceled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a job is concurrent. Will be true when a job is
    -- rolling out new job executions or canceling previously created
    -- executions, otherwise false.
    isConcurrent :: Prelude.Maybe Prelude.Bool,
    -- | An ARN identifying the job with format
    -- \"arn:aws:iot:region:account:job\/jobId\".
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration for the criteria to retry the job.
    jobExecutionsRetryConfig :: Prelude.Maybe JobExecutionsRetryConfig,
    -- | Allows you to create a staged rollout of a job.
    jobExecutionsRolloutConfig :: Prelude.Maybe JobExecutionsRolloutConfig,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | Details about the job process.
    jobProcessDetails :: Prelude.Maybe JobProcessDetails,
    -- | The ARN of the job template used to create the job.
    jobTemplateArn :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
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
    -- | Configuration for pre-signed S3 URLs.
    presignedUrlConfig :: Prelude.Maybe PresignedUrlConfig,
    -- | If the job was updated, provides the reason code for the update.
    reasonCode :: Prelude.Maybe Prelude.Text,
    -- | The configuration that allows you to schedule a job for a future date
    -- and time in addition to specifying the end behavior for each job
    -- execution.
    schedulingConfig :: Prelude.Maybe SchedulingConfig,
    -- | The status of the job, one of @IN_PROGRESS@, @CANCELED@,
    -- @DELETION_IN_PROGRESS@ or @COMPLETED@.
    status :: Prelude.Maybe JobStatus,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a device
    -- when the thing representing the device is added to a target group, even
    -- after the job was completed by all things originally in the group.
    --
    -- We recommend that you use continuous jobs instead of snapshot jobs for
    -- dynamic thing group targets. By using continuous jobs, devices that join
    -- the group receive the job execution even after the job has been created.
    targetSelection :: Prelude.Maybe TargetSelection,
    -- | A list of IoT things and thing groups to which the job should be sent.
    targets :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
-- 'abortConfig', 'job_abortConfig' - Configuration for criteria to abort the job.
--
-- 'comment', 'job_comment' - If the job was updated, describes the reason for the update.
--
-- 'completedAt', 'job_completedAt' - The time, in seconds since the epoch, when the job was completed.
--
-- 'createdAt', 'job_createdAt' - The time, in seconds since the epoch, when the job was created.
--
-- 'description', 'job_description' - A short text description of the job.
--
-- 'documentParameters', 'job_documentParameters' - A key-value map that pairs the patterns that need to be replaced in a
-- managed template job document schema. You can use the description of
-- each key as a guidance to specify the inputs during runtime when
-- creating a job.
--
-- @documentParameters@ can only be used when creating jobs from Amazon Web
-- Services managed templates. This parameter can\'t be used with custom
-- job templates or to create jobs from them.
--
-- 'forceCanceled', 'job_forceCanceled' - Will be @true@ if the job was canceled with the optional @force@
-- parameter set to @true@.
--
-- 'isConcurrent', 'job_isConcurrent' - Indicates whether a job is concurrent. Will be true when a job is
-- rolling out new job executions or canceling previously created
-- executions, otherwise false.
--
-- 'jobArn', 'job_jobArn' - An ARN identifying the job with format
-- \"arn:aws:iot:region:account:job\/jobId\".
--
-- 'jobExecutionsRetryConfig', 'job_jobExecutionsRetryConfig' - The configuration for the criteria to retry the job.
--
-- 'jobExecutionsRolloutConfig', 'job_jobExecutionsRolloutConfig' - Allows you to create a staged rollout of a job.
--
-- 'jobId', 'job_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'jobProcessDetails', 'job_jobProcessDetails' - Details about the job process.
--
-- 'jobTemplateArn', 'job_jobTemplateArn' - The ARN of the job template used to create the job.
--
-- 'lastUpdatedAt', 'job_lastUpdatedAt' - The time, in seconds since the epoch, when the job was last updated.
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
-- 'presignedUrlConfig', 'job_presignedUrlConfig' - Configuration for pre-signed S3 URLs.
--
-- 'reasonCode', 'job_reasonCode' - If the job was updated, provides the reason code for the update.
--
-- 'schedulingConfig', 'job_schedulingConfig' - The configuration that allows you to schedule a job for a future date
-- and time in addition to specifying the end behavior for each job
-- execution.
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
-- We recommend that you use continuous jobs instead of snapshot jobs for
-- dynamic thing group targets. By using continuous jobs, devices that join
-- the group receive the job execution even after the job has been created.
--
-- 'targets', 'job_targets' - A list of IoT things and thing groups to which the job should be sent.
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
    { abortConfig = Prelude.Nothing,
      comment = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      documentParameters = Prelude.Nothing,
      forceCanceled = Prelude.Nothing,
      isConcurrent = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobExecutionsRetryConfig = Prelude.Nothing,
      jobExecutionsRolloutConfig = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobProcessDetails = Prelude.Nothing,
      jobTemplateArn = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      presignedUrlConfig = Prelude.Nothing,
      reasonCode = Prelude.Nothing,
      schedulingConfig = Prelude.Nothing,
      status = Prelude.Nothing,
      targetSelection = Prelude.Nothing,
      targets = Prelude.Nothing,
      timeoutConfig = Prelude.Nothing
    }

-- | Configuration for criteria to abort the job.
job_abortConfig :: Lens.Lens' Job (Prelude.Maybe AbortConfig)
job_abortConfig = Lens.lens (\Job' {abortConfig} -> abortConfig) (\s@Job' {} a -> s {abortConfig = a} :: Job)

-- | If the job was updated, describes the reason for the update.
job_comment :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_comment = Lens.lens (\Job' {comment} -> comment) (\s@Job' {} a -> s {comment = a} :: Job)

-- | The time, in seconds since the epoch, when the job was completed.
job_completedAt :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_completedAt = Lens.lens (\Job' {completedAt} -> completedAt) (\s@Job' {} a -> s {completedAt = a} :: Job) Prelude.. Lens.mapping Data._Time

-- | The time, in seconds since the epoch, when the job was created.
job_createdAt :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_createdAt = Lens.lens (\Job' {createdAt} -> createdAt) (\s@Job' {} a -> s {createdAt = a} :: Job) Prelude.. Lens.mapping Data._Time

-- | A short text description of the job.
job_description :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_description = Lens.lens (\Job' {description} -> description) (\s@Job' {} a -> s {description = a} :: Job)

-- | A key-value map that pairs the patterns that need to be replaced in a
-- managed template job document schema. You can use the description of
-- each key as a guidance to specify the inputs during runtime when
-- creating a job.
--
-- @documentParameters@ can only be used when creating jobs from Amazon Web
-- Services managed templates. This parameter can\'t be used with custom
-- job templates or to create jobs from them.
job_documentParameters :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_documentParameters = Lens.lens (\Job' {documentParameters} -> documentParameters) (\s@Job' {} a -> s {documentParameters = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | Will be @true@ if the job was canceled with the optional @force@
-- parameter set to @true@.
job_forceCanceled :: Lens.Lens' Job (Prelude.Maybe Prelude.Bool)
job_forceCanceled = Lens.lens (\Job' {forceCanceled} -> forceCanceled) (\s@Job' {} a -> s {forceCanceled = a} :: Job)

-- | Indicates whether a job is concurrent. Will be true when a job is
-- rolling out new job executions or canceling previously created
-- executions, otherwise false.
job_isConcurrent :: Lens.Lens' Job (Prelude.Maybe Prelude.Bool)
job_isConcurrent = Lens.lens (\Job' {isConcurrent} -> isConcurrent) (\s@Job' {} a -> s {isConcurrent = a} :: Job)

-- | An ARN identifying the job with format
-- \"arn:aws:iot:region:account:job\/jobId\".
job_jobArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_jobArn = Lens.lens (\Job' {jobArn} -> jobArn) (\s@Job' {} a -> s {jobArn = a} :: Job)

-- | The configuration for the criteria to retry the job.
job_jobExecutionsRetryConfig :: Lens.Lens' Job (Prelude.Maybe JobExecutionsRetryConfig)
job_jobExecutionsRetryConfig = Lens.lens (\Job' {jobExecutionsRetryConfig} -> jobExecutionsRetryConfig) (\s@Job' {} a -> s {jobExecutionsRetryConfig = a} :: Job)

-- | Allows you to create a staged rollout of a job.
job_jobExecutionsRolloutConfig :: Lens.Lens' Job (Prelude.Maybe JobExecutionsRolloutConfig)
job_jobExecutionsRolloutConfig = Lens.lens (\Job' {jobExecutionsRolloutConfig} -> jobExecutionsRolloutConfig) (\s@Job' {} a -> s {jobExecutionsRolloutConfig = a} :: Job)

-- | The unique identifier you assigned to this job when it was created.
job_jobId :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_jobId = Lens.lens (\Job' {jobId} -> jobId) (\s@Job' {} a -> s {jobId = a} :: Job)

-- | Details about the job process.
job_jobProcessDetails :: Lens.Lens' Job (Prelude.Maybe JobProcessDetails)
job_jobProcessDetails = Lens.lens (\Job' {jobProcessDetails} -> jobProcessDetails) (\s@Job' {} a -> s {jobProcessDetails = a} :: Job)

-- | The ARN of the job template used to create the job.
job_jobTemplateArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_jobTemplateArn = Lens.lens (\Job' {jobTemplateArn} -> jobTemplateArn) (\s@Job' {} a -> s {jobTemplateArn = a} :: Job)

-- | The time, in seconds since the epoch, when the job was last updated.
job_lastUpdatedAt :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_lastUpdatedAt = Lens.lens (\Job' {lastUpdatedAt} -> lastUpdatedAt) (\s@Job' {} a -> s {lastUpdatedAt = a} :: Job) Prelude.. Lens.mapping Data._Time

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

-- | Configuration for pre-signed S3 URLs.
job_presignedUrlConfig :: Lens.Lens' Job (Prelude.Maybe PresignedUrlConfig)
job_presignedUrlConfig = Lens.lens (\Job' {presignedUrlConfig} -> presignedUrlConfig) (\s@Job' {} a -> s {presignedUrlConfig = a} :: Job)

-- | If the job was updated, provides the reason code for the update.
job_reasonCode :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_reasonCode = Lens.lens (\Job' {reasonCode} -> reasonCode) (\s@Job' {} a -> s {reasonCode = a} :: Job)

-- | The configuration that allows you to schedule a job for a future date
-- and time in addition to specifying the end behavior for each job
-- execution.
job_schedulingConfig :: Lens.Lens' Job (Prelude.Maybe SchedulingConfig)
job_schedulingConfig = Lens.lens (\Job' {schedulingConfig} -> schedulingConfig) (\s@Job' {} a -> s {schedulingConfig = a} :: Job)

-- | The status of the job, one of @IN_PROGRESS@, @CANCELED@,
-- @DELETION_IN_PROGRESS@ or @COMPLETED@.
job_status :: Lens.Lens' Job (Prelude.Maybe JobStatus)
job_status = Lens.lens (\Job' {status} -> status) (\s@Job' {} a -> s {status = a} :: Job)

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a device
-- when the thing representing the device is added to a target group, even
-- after the job was completed by all things originally in the group.
--
-- We recommend that you use continuous jobs instead of snapshot jobs for
-- dynamic thing group targets. By using continuous jobs, devices that join
-- the group receive the job execution even after the job has been created.
job_targetSelection :: Lens.Lens' Job (Prelude.Maybe TargetSelection)
job_targetSelection = Lens.lens (\Job' {targetSelection} -> targetSelection) (\s@Job' {} a -> s {targetSelection = a} :: Job)

-- | A list of IoT things and thing groups to which the job should be sent.
job_targets :: Lens.Lens' Job (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
job_targets = Lens.lens (\Job' {targets} -> targets) (\s@Job' {} a -> s {targets = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the amount of time each device has to finish its execution of
-- the job. A timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the timer expires, it will be automatically set to
-- @TIMED_OUT@.
job_timeoutConfig :: Lens.Lens' Job (Prelude.Maybe TimeoutConfig)
job_timeoutConfig = Lens.lens (\Job' {timeoutConfig} -> timeoutConfig) (\s@Job' {} a -> s {timeoutConfig = a} :: Job)

instance Data.FromJSON Job where
  parseJSON =
    Data.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Data..:? "abortConfig")
            Prelude.<*> (x Data..:? "comment")
            Prelude.<*> (x Data..:? "completedAt")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> ( x Data..:? "documentParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "forceCanceled")
            Prelude.<*> (x Data..:? "isConcurrent")
            Prelude.<*> (x Data..:? "jobArn")
            Prelude.<*> (x Data..:? "jobExecutionsRetryConfig")
            Prelude.<*> (x Data..:? "jobExecutionsRolloutConfig")
            Prelude.<*> (x Data..:? "jobId")
            Prelude.<*> (x Data..:? "jobProcessDetails")
            Prelude.<*> (x Data..:? "jobTemplateArn")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "namespaceId")
            Prelude.<*> (x Data..:? "presignedUrlConfig")
            Prelude.<*> (x Data..:? "reasonCode")
            Prelude.<*> (x Data..:? "schedulingConfig")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "targetSelection")
            Prelude.<*> (x Data..:? "targets")
            Prelude.<*> (x Data..:? "timeoutConfig")
      )

instance Prelude.Hashable Job where
  hashWithSalt _salt Job' {..} =
    _salt `Prelude.hashWithSalt` abortConfig
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` completedAt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` documentParameters
      `Prelude.hashWithSalt` forceCanceled
      `Prelude.hashWithSalt` isConcurrent
      `Prelude.hashWithSalt` jobArn
      `Prelude.hashWithSalt` jobExecutionsRetryConfig
      `Prelude.hashWithSalt` jobExecutionsRolloutConfig
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobProcessDetails
      `Prelude.hashWithSalt` jobTemplateArn
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` namespaceId
      `Prelude.hashWithSalt` presignedUrlConfig
      `Prelude.hashWithSalt` reasonCode
      `Prelude.hashWithSalt` schedulingConfig
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetSelection
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` timeoutConfig

instance Prelude.NFData Job where
  rnf Job' {..} =
    Prelude.rnf abortConfig
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf completedAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf documentParameters
      `Prelude.seq` Prelude.rnf forceCanceled
      `Prelude.seq` Prelude.rnf isConcurrent
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobExecutionsRetryConfig
      `Prelude.seq` Prelude.rnf jobExecutionsRolloutConfig
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobProcessDetails
      `Prelude.seq` Prelude.rnf jobTemplateArn
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf namespaceId
      `Prelude.seq` Prelude.rnf presignedUrlConfig
      `Prelude.seq` Prelude.rnf reasonCode
      `Prelude.seq` Prelude.rnf schedulingConfig
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetSelection
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf
        timeoutConfig
