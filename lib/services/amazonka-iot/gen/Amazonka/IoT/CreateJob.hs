{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoT.CreateJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateJob>
-- action.
module Amazonka.IoT.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_abortConfig,
    createJob_description,
    createJob_document,
    createJob_documentParameters,
    createJob_documentSource,
    createJob_jobExecutionsRetryConfig,
    createJob_jobExecutionsRolloutConfig,
    createJob_jobTemplateArn,
    createJob_namespaceId,
    createJob_presignedUrlConfig,
    createJob_schedulingConfig,
    createJob_tags,
    createJob_targetSelection,
    createJob_timeoutConfig,
    createJob_jobId,
    createJob_targets,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_description,
    createJobResponse_jobArn,
    createJobResponse_jobId,
    createJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | Allows you to create the criteria to abort a job.
    abortConfig :: Prelude.Maybe AbortConfig,
    -- | A short text description of the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | The job document. Required if you don\'t specify a value for
    -- @documentSource@.
    document :: Prelude.Maybe Prelude.Text,
    -- | Parameters of an Amazon Web Services managed template that you can
    -- specify to create the job document.
    --
    -- @documentParameters@ can only be used when creating jobs from Amazon Web
    -- Services managed templates. This parameter can\'t be used with custom
    -- job templates or to create jobs from them.
    documentParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An S3 link to the job document. Required if you don\'t specify a value
    -- for @document@.
    --
    -- If the job document resides in an S3 bucket, you must use a placeholder
    -- link when specifying the document.
    --
    -- The placeholder link is of the following form:
    --
    -- @${aws:iot:s3-presigned-url:https:\/\/s3.amazonaws.com\/bucket\/key}@
    --
    -- where /bucket/ is your bucket name and /key/ is the object in the bucket
    -- to which you are linking.
    documentSource :: Prelude.Maybe Prelude.Text,
    -- | Allows you to create the criteria to retry a job.
    jobExecutionsRetryConfig :: Prelude.Maybe JobExecutionsRetryConfig,
    -- | Allows you to create a staged rollout of the job.
    jobExecutionsRolloutConfig :: Prelude.Maybe JobExecutionsRolloutConfig,
    -- | The ARN of the job template used to create the job.
    jobTemplateArn :: Prelude.Maybe Prelude.Text,
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
    -- | Configuration information for pre-signed S3 URLs.
    presignedUrlConfig :: Prelude.Maybe PresignedUrlConfig,
    -- | The configuration that allows you to schedule a job for a future date
    -- and time in addition to specifying the end behavior for each job
    -- execution.
    schedulingConfig :: Prelude.Maybe SchedulingConfig,
    -- | Metadata which can be used to manage the job.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a thing
    -- when the thing is added to a target group, even after the job was
    -- completed by all things originally in the group.
    --
    -- We recommend that you use continuous jobs instead of snapshot jobs for
    -- dynamic thing group targets. By using continuous jobs, devices that join
    -- the group receive the job execution even after the job has been created.
    targetSelection :: Prelude.Maybe TargetSelection,
    -- | Specifies the amount of time each device has to finish its execution of
    -- the job. The timer is started when the job execution status is set to
    -- @IN_PROGRESS@. If the job execution status is not set to another
    -- terminal state before the time expires, it will be automatically set to
    -- @TIMED_OUT@.
    timeoutConfig :: Prelude.Maybe TimeoutConfig,
    -- | A job identifier which must be unique for your Amazon Web Services
    -- account. We recommend using a UUID. Alpha-numeric characters, \"-\" and
    -- \"_\" are valid for use here.
    jobId :: Prelude.Text,
    -- | A list of things and thing groups to which the job should be sent.
    targets :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'abortConfig', 'createJob_abortConfig' - Allows you to create the criteria to abort a job.
--
-- 'description', 'createJob_description' - A short text description of the job.
--
-- 'document', 'createJob_document' - The job document. Required if you don\'t specify a value for
-- @documentSource@.
--
-- 'documentParameters', 'createJob_documentParameters' - Parameters of an Amazon Web Services managed template that you can
-- specify to create the job document.
--
-- @documentParameters@ can only be used when creating jobs from Amazon Web
-- Services managed templates. This parameter can\'t be used with custom
-- job templates or to create jobs from them.
--
-- 'documentSource', 'createJob_documentSource' - An S3 link to the job document. Required if you don\'t specify a value
-- for @document@.
--
-- If the job document resides in an S3 bucket, you must use a placeholder
-- link when specifying the document.
--
-- The placeholder link is of the following form:
--
-- @${aws:iot:s3-presigned-url:https:\/\/s3.amazonaws.com\/bucket\/key}@
--
-- where /bucket/ is your bucket name and /key/ is the object in the bucket
-- to which you are linking.
--
-- 'jobExecutionsRetryConfig', 'createJob_jobExecutionsRetryConfig' - Allows you to create the criteria to retry a job.
--
-- 'jobExecutionsRolloutConfig', 'createJob_jobExecutionsRolloutConfig' - Allows you to create a staged rollout of the job.
--
-- 'jobTemplateArn', 'createJob_jobTemplateArn' - The ARN of the job template used to create the job.
--
-- 'namespaceId', 'createJob_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'presignedUrlConfig', 'createJob_presignedUrlConfig' - Configuration information for pre-signed S3 URLs.
--
-- 'schedulingConfig', 'createJob_schedulingConfig' - The configuration that allows you to schedule a job for a future date
-- and time in addition to specifying the end behavior for each job
-- execution.
--
-- 'tags', 'createJob_tags' - Metadata which can be used to manage the job.
--
-- 'targetSelection', 'createJob_targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
--
-- We recommend that you use continuous jobs instead of snapshot jobs for
-- dynamic thing group targets. By using continuous jobs, devices that join
-- the group receive the job execution even after the job has been created.
--
-- 'timeoutConfig', 'createJob_timeoutConfig' - Specifies the amount of time each device has to finish its execution of
-- the job. The timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the time expires, it will be automatically set to
-- @TIMED_OUT@.
--
-- 'jobId', 'createJob_jobId' - A job identifier which must be unique for your Amazon Web Services
-- account. We recommend using a UUID. Alpha-numeric characters, \"-\" and
-- \"_\" are valid for use here.
--
-- 'targets', 'createJob_targets' - A list of things and thing groups to which the job should be sent.
newCreateJob ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'targets'
  Prelude.NonEmpty Prelude.Text ->
  CreateJob
newCreateJob pJobId_ pTargets_ =
  CreateJob'
    { abortConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      document = Prelude.Nothing,
      documentParameters = Prelude.Nothing,
      documentSource = Prelude.Nothing,
      jobExecutionsRetryConfig = Prelude.Nothing,
      jobExecutionsRolloutConfig = Prelude.Nothing,
      jobTemplateArn = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      presignedUrlConfig = Prelude.Nothing,
      schedulingConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetSelection = Prelude.Nothing,
      timeoutConfig = Prelude.Nothing,
      jobId = pJobId_,
      targets = Lens.coerced Lens.# pTargets_
    }

-- | Allows you to create the criteria to abort a job.
createJob_abortConfig :: Lens.Lens' CreateJob (Prelude.Maybe AbortConfig)
createJob_abortConfig = Lens.lens (\CreateJob' {abortConfig} -> abortConfig) (\s@CreateJob' {} a -> s {abortConfig = a} :: CreateJob)

-- | A short text description of the job.
createJob_description :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_description = Lens.lens (\CreateJob' {description} -> description) (\s@CreateJob' {} a -> s {description = a} :: CreateJob)

-- | The job document. Required if you don\'t specify a value for
-- @documentSource@.
createJob_document :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_document = Lens.lens (\CreateJob' {document} -> document) (\s@CreateJob' {} a -> s {document = a} :: CreateJob)

-- | Parameters of an Amazon Web Services managed template that you can
-- specify to create the job document.
--
-- @documentParameters@ can only be used when creating jobs from Amazon Web
-- Services managed templates. This parameter can\'t be used with custom
-- job templates or to create jobs from them.
createJob_documentParameters :: Lens.Lens' CreateJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createJob_documentParameters = Lens.lens (\CreateJob' {documentParameters} -> documentParameters) (\s@CreateJob' {} a -> s {documentParameters = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | An S3 link to the job document. Required if you don\'t specify a value
-- for @document@.
--
-- If the job document resides in an S3 bucket, you must use a placeholder
-- link when specifying the document.
--
-- The placeholder link is of the following form:
--
-- @${aws:iot:s3-presigned-url:https:\/\/s3.amazonaws.com\/bucket\/key}@
--
-- where /bucket/ is your bucket name and /key/ is the object in the bucket
-- to which you are linking.
createJob_documentSource :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_documentSource = Lens.lens (\CreateJob' {documentSource} -> documentSource) (\s@CreateJob' {} a -> s {documentSource = a} :: CreateJob)

-- | Allows you to create the criteria to retry a job.
createJob_jobExecutionsRetryConfig :: Lens.Lens' CreateJob (Prelude.Maybe JobExecutionsRetryConfig)
createJob_jobExecutionsRetryConfig = Lens.lens (\CreateJob' {jobExecutionsRetryConfig} -> jobExecutionsRetryConfig) (\s@CreateJob' {} a -> s {jobExecutionsRetryConfig = a} :: CreateJob)

-- | Allows you to create a staged rollout of the job.
createJob_jobExecutionsRolloutConfig :: Lens.Lens' CreateJob (Prelude.Maybe JobExecutionsRolloutConfig)
createJob_jobExecutionsRolloutConfig = Lens.lens (\CreateJob' {jobExecutionsRolloutConfig} -> jobExecutionsRolloutConfig) (\s@CreateJob' {} a -> s {jobExecutionsRolloutConfig = a} :: CreateJob)

-- | The ARN of the job template used to create the job.
createJob_jobTemplateArn :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_jobTemplateArn = Lens.lens (\CreateJob' {jobTemplateArn} -> jobTemplateArn) (\s@CreateJob' {} a -> s {jobTemplateArn = a} :: CreateJob)

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
createJob_namespaceId :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_namespaceId = Lens.lens (\CreateJob' {namespaceId} -> namespaceId) (\s@CreateJob' {} a -> s {namespaceId = a} :: CreateJob)

-- | Configuration information for pre-signed S3 URLs.
createJob_presignedUrlConfig :: Lens.Lens' CreateJob (Prelude.Maybe PresignedUrlConfig)
createJob_presignedUrlConfig = Lens.lens (\CreateJob' {presignedUrlConfig} -> presignedUrlConfig) (\s@CreateJob' {} a -> s {presignedUrlConfig = a} :: CreateJob)

-- | The configuration that allows you to schedule a job for a future date
-- and time in addition to specifying the end behavior for each job
-- execution.
createJob_schedulingConfig :: Lens.Lens' CreateJob (Prelude.Maybe SchedulingConfig)
createJob_schedulingConfig = Lens.lens (\CreateJob' {schedulingConfig} -> schedulingConfig) (\s@CreateJob' {} a -> s {schedulingConfig = a} :: CreateJob)

-- | Metadata which can be used to manage the job.
createJob_tags :: Lens.Lens' CreateJob (Prelude.Maybe [Tag])
createJob_tags = Lens.lens (\CreateJob' {tags} -> tags) (\s@CreateJob' {} a -> s {tags = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
--
-- We recommend that you use continuous jobs instead of snapshot jobs for
-- dynamic thing group targets. By using continuous jobs, devices that join
-- the group receive the job execution even after the job has been created.
createJob_targetSelection :: Lens.Lens' CreateJob (Prelude.Maybe TargetSelection)
createJob_targetSelection = Lens.lens (\CreateJob' {targetSelection} -> targetSelection) (\s@CreateJob' {} a -> s {targetSelection = a} :: CreateJob)

-- | Specifies the amount of time each device has to finish its execution of
-- the job. The timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the time expires, it will be automatically set to
-- @TIMED_OUT@.
createJob_timeoutConfig :: Lens.Lens' CreateJob (Prelude.Maybe TimeoutConfig)
createJob_timeoutConfig = Lens.lens (\CreateJob' {timeoutConfig} -> timeoutConfig) (\s@CreateJob' {} a -> s {timeoutConfig = a} :: CreateJob)

-- | A job identifier which must be unique for your Amazon Web Services
-- account. We recommend using a UUID. Alpha-numeric characters, \"-\" and
-- \"_\" are valid for use here.
createJob_jobId :: Lens.Lens' CreateJob Prelude.Text
createJob_jobId = Lens.lens (\CreateJob' {jobId} -> jobId) (\s@CreateJob' {} a -> s {jobId = a} :: CreateJob)

-- | A list of things and thing groups to which the job should be sent.
createJob_targets :: Lens.Lens' CreateJob (Prelude.NonEmpty Prelude.Text)
createJob_targets = Lens.lens (\CreateJob' {targets} -> targets) (\s@CreateJob' {} a -> s {targets = a} :: CreateJob) Prelude.. Lens.coerced

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "jobArn")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateJob where
  hashWithSalt _salt CreateJob' {..} =
    _salt `Prelude.hashWithSalt` abortConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` document
      `Prelude.hashWithSalt` documentParameters
      `Prelude.hashWithSalt` documentSource
      `Prelude.hashWithSalt` jobExecutionsRetryConfig
      `Prelude.hashWithSalt` jobExecutionsRolloutConfig
      `Prelude.hashWithSalt` jobTemplateArn
      `Prelude.hashWithSalt` namespaceId
      `Prelude.hashWithSalt` presignedUrlConfig
      `Prelude.hashWithSalt` schedulingConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetSelection
      `Prelude.hashWithSalt` timeoutConfig
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` targets

instance Prelude.NFData CreateJob where
  rnf CreateJob' {..} =
    Prelude.rnf abortConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf document
      `Prelude.seq` Prelude.rnf documentParameters
      `Prelude.seq` Prelude.rnf documentSource
      `Prelude.seq` Prelude.rnf jobExecutionsRetryConfig
      `Prelude.seq` Prelude.rnf jobExecutionsRolloutConfig
      `Prelude.seq` Prelude.rnf jobTemplateArn
      `Prelude.seq` Prelude.rnf namespaceId
      `Prelude.seq` Prelude.rnf presignedUrlConfig
      `Prelude.seq` Prelude.rnf schedulingConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetSelection
      `Prelude.seq` Prelude.rnf timeoutConfig
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf targets

instance Data.ToHeaders CreateJob where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("abortConfig" Data..=) Prelude.<$> abortConfig,
            ("description" Data..=) Prelude.<$> description,
            ("document" Data..=) Prelude.<$> document,
            ("documentParameters" Data..=)
              Prelude.<$> documentParameters,
            ("documentSource" Data..=)
              Prelude.<$> documentSource,
            ("jobExecutionsRetryConfig" Data..=)
              Prelude.<$> jobExecutionsRetryConfig,
            ("jobExecutionsRolloutConfig" Data..=)
              Prelude.<$> jobExecutionsRolloutConfig,
            ("jobTemplateArn" Data..=)
              Prelude.<$> jobTemplateArn,
            ("namespaceId" Data..=) Prelude.<$> namespaceId,
            ("presignedUrlConfig" Data..=)
              Prelude.<$> presignedUrlConfig,
            ("schedulingConfig" Data..=)
              Prelude.<$> schedulingConfig,
            ("tags" Data..=) Prelude.<$> tags,
            ("targetSelection" Data..=)
              Prelude.<$> targetSelection,
            ("timeoutConfig" Data..=) Prelude.<$> timeoutConfig,
            Prelude.Just ("targets" Data..= targets)
          ]
      )

instance Data.ToPath CreateJob where
  toPath CreateJob' {..} =
    Prelude.mconcat ["/jobs/", Data.toBS jobId]

instance Data.ToQuery CreateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | The job description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The job ARN.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier you assigned to this job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createJobResponse_description' - The job description.
--
-- 'jobArn', 'createJobResponse_jobArn' - The job ARN.
--
-- 'jobId', 'createJobResponse_jobId' - The unique identifier you assigned to this job.
--
-- 'httpStatus', 'createJobResponse_httpStatus' - The response's http status code.
newCreateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ =
  CreateJobResponse'
    { description = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job description.
createJobResponse_description :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_description = Lens.lens (\CreateJobResponse' {description} -> description) (\s@CreateJobResponse' {} a -> s {description = a} :: CreateJobResponse)

-- | The job ARN.
createJobResponse_jobArn :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_jobArn = Lens.lens (\CreateJobResponse' {jobArn} -> jobArn) (\s@CreateJobResponse' {} a -> s {jobArn = a} :: CreateJobResponse)

-- | The unique identifier you assigned to this job.
createJobResponse_jobId :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_jobId = Lens.lens (\CreateJobResponse' {jobId} -> jobId) (\s@CreateJobResponse' {} a -> s {jobId = a} :: CreateJobResponse)

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Prelude.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Prelude.NFData CreateJobResponse where
  rnf CreateJobResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
