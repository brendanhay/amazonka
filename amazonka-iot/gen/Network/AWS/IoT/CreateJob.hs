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
-- Module      : Network.AWS.IoT.CreateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job.
module Network.AWS.IoT.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_jobExecutionsRolloutConfig,
    createJob_targetSelection,
    createJob_timeoutConfig,
    createJob_namespaceId,
    createJob_documentSource,
    createJob_document,
    createJob_presignedUrlConfig,
    createJob_tags,
    createJob_description,
    createJob_abortConfig,
    createJob_jobId,
    createJob_targets,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_jobArn,
    createJobResponse_description,
    createJobResponse_jobId,
    createJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | Allows you to create a staged rollout of the job.
    jobExecutionsRolloutConfig :: Prelude.Maybe JobExecutionsRolloutConfig,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a thing
    -- when the thing is added to a target group, even after the job was
    -- completed by all things originally in the group.
    targetSelection :: Prelude.Maybe TargetSelection,
    -- | Specifies the amount of time each device has to finish its execution of
    -- the job. The timer is started when the job execution status is set to
    -- @IN_PROGRESS@. If the job execution status is not set to another
    -- terminal state before the time expires, it will be automatically set to
    -- @TIMED_OUT@.
    timeoutConfig :: Prelude.Maybe TimeoutConfig,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs
    -- notifications to MQTT topics that contain the value in the following
    -- format.
    --
    -- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | An S3 link to the job document.
    documentSource :: Prelude.Maybe Prelude.Text,
    -- | The job document.
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
    document :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for pre-signed S3 URLs.
    presignedUrlConfig :: Prelude.Maybe PresignedUrlConfig,
    -- | Metadata which can be used to manage the job.
    tags :: Prelude.Maybe [Tag],
    -- | A short text description of the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | Allows you to create criteria to abort a job.
    abortConfig :: Prelude.Maybe AbortConfig,
    -- | A job identifier which must be unique for your AWS account. We recommend
    -- using a UUID. Alpha-numeric characters, \"-\" and \"_\" are valid for
    -- use here.
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
-- 'jobExecutionsRolloutConfig', 'createJob_jobExecutionsRolloutConfig' - Allows you to create a staged rollout of the job.
--
-- 'targetSelection', 'createJob_targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
--
-- 'timeoutConfig', 'createJob_timeoutConfig' - Specifies the amount of time each device has to finish its execution of
-- the job. The timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the time expires, it will be automatically set to
-- @TIMED_OUT@.
--
-- 'namespaceId', 'createJob_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'documentSource', 'createJob_documentSource' - An S3 link to the job document.
--
-- 'document', 'createJob_document' - The job document.
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
-- 'presignedUrlConfig', 'createJob_presignedUrlConfig' - Configuration information for pre-signed S3 URLs.
--
-- 'tags', 'createJob_tags' - Metadata which can be used to manage the job.
--
-- 'description', 'createJob_description' - A short text description of the job.
--
-- 'abortConfig', 'createJob_abortConfig' - Allows you to create criteria to abort a job.
--
-- 'jobId', 'createJob_jobId' - A job identifier which must be unique for your AWS account. We recommend
-- using a UUID. Alpha-numeric characters, \"-\" and \"_\" are valid for
-- use here.
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
    { jobExecutionsRolloutConfig =
        Prelude.Nothing,
      targetSelection = Prelude.Nothing,
      timeoutConfig = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      documentSource = Prelude.Nothing,
      document = Prelude.Nothing,
      presignedUrlConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      abortConfig = Prelude.Nothing,
      jobId = pJobId_,
      targets = Lens._Coerce Lens.# pTargets_
    }

-- | Allows you to create a staged rollout of the job.
createJob_jobExecutionsRolloutConfig :: Lens.Lens' CreateJob (Prelude.Maybe JobExecutionsRolloutConfig)
createJob_jobExecutionsRolloutConfig = Lens.lens (\CreateJob' {jobExecutionsRolloutConfig} -> jobExecutionsRolloutConfig) (\s@CreateJob' {} a -> s {jobExecutionsRolloutConfig = a} :: CreateJob)

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
createJob_targetSelection :: Lens.Lens' CreateJob (Prelude.Maybe TargetSelection)
createJob_targetSelection = Lens.lens (\CreateJob' {targetSelection} -> targetSelection) (\s@CreateJob' {} a -> s {targetSelection = a} :: CreateJob)

-- | Specifies the amount of time each device has to finish its execution of
-- the job. The timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the time expires, it will be automatically set to
-- @TIMED_OUT@.
createJob_timeoutConfig :: Lens.Lens' CreateJob (Prelude.Maybe TimeoutConfig)
createJob_timeoutConfig = Lens.lens (\CreateJob' {timeoutConfig} -> timeoutConfig) (\s@CreateJob' {} a -> s {timeoutConfig = a} :: CreateJob)

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
createJob_namespaceId :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_namespaceId = Lens.lens (\CreateJob' {namespaceId} -> namespaceId) (\s@CreateJob' {} a -> s {namespaceId = a} :: CreateJob)

-- | An S3 link to the job document.
createJob_documentSource :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_documentSource = Lens.lens (\CreateJob' {documentSource} -> documentSource) (\s@CreateJob' {} a -> s {documentSource = a} :: CreateJob)

-- | The job document.
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
createJob_document :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_document = Lens.lens (\CreateJob' {document} -> document) (\s@CreateJob' {} a -> s {document = a} :: CreateJob)

-- | Configuration information for pre-signed S3 URLs.
createJob_presignedUrlConfig :: Lens.Lens' CreateJob (Prelude.Maybe PresignedUrlConfig)
createJob_presignedUrlConfig = Lens.lens (\CreateJob' {presignedUrlConfig} -> presignedUrlConfig) (\s@CreateJob' {} a -> s {presignedUrlConfig = a} :: CreateJob)

-- | Metadata which can be used to manage the job.
createJob_tags :: Lens.Lens' CreateJob (Prelude.Maybe [Tag])
createJob_tags = Lens.lens (\CreateJob' {tags} -> tags) (\s@CreateJob' {} a -> s {tags = a} :: CreateJob) Prelude.. Lens.mapping Lens._Coerce

-- | A short text description of the job.
createJob_description :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_description = Lens.lens (\CreateJob' {description} -> description) (\s@CreateJob' {} a -> s {description = a} :: CreateJob)

-- | Allows you to create criteria to abort a job.
createJob_abortConfig :: Lens.Lens' CreateJob (Prelude.Maybe AbortConfig)
createJob_abortConfig = Lens.lens (\CreateJob' {abortConfig} -> abortConfig) (\s@CreateJob' {} a -> s {abortConfig = a} :: CreateJob)

-- | A job identifier which must be unique for your AWS account. We recommend
-- using a UUID. Alpha-numeric characters, \"-\" and \"_\" are valid for
-- use here.
createJob_jobId :: Lens.Lens' CreateJob Prelude.Text
createJob_jobId = Lens.lens (\CreateJob' {jobId} -> jobId) (\s@CreateJob' {} a -> s {jobId = a} :: CreateJob)

-- | A list of things and thing groups to which the job should be sent.
createJob_targets :: Lens.Lens' CreateJob (Prelude.NonEmpty Prelude.Text)
createJob_targets = Lens.lens (\CreateJob' {targets} -> targets) (\s@CreateJob' {} a -> s {targets = a} :: CreateJob) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Prelude.<$> (x Core..?> "jobArn")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "jobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateJob

instance Prelude.NFData CreateJob

instance Core.ToHeaders CreateJob where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("jobExecutionsRolloutConfig" Core..=)
              Prelude.<$> jobExecutionsRolloutConfig,
            ("targetSelection" Core..=)
              Prelude.<$> targetSelection,
            ("timeoutConfig" Core..=) Prelude.<$> timeoutConfig,
            ("namespaceId" Core..=) Prelude.<$> namespaceId,
            ("documentSource" Core..=)
              Prelude.<$> documentSource,
            ("document" Core..=) Prelude.<$> document,
            ("presignedUrlConfig" Core..=)
              Prelude.<$> presignedUrlConfig,
            ("tags" Core..=) Prelude.<$> tags,
            ("description" Core..=) Prelude.<$> description,
            ("abortConfig" Core..=) Prelude.<$> abortConfig,
            Prelude.Just ("targets" Core..= targets)
          ]
      )

instance Core.ToPath CreateJob where
  toPath CreateJob' {..} =
    Prelude.mconcat ["/jobs/", Core.toBS jobId]

instance Core.ToQuery CreateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | The job ARN.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The job description.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'jobArn', 'createJobResponse_jobArn' - The job ARN.
--
-- 'description', 'createJobResponse_description' - The job description.
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
    { jobArn = Prelude.Nothing,
      description = Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job ARN.
createJobResponse_jobArn :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_jobArn = Lens.lens (\CreateJobResponse' {jobArn} -> jobArn) (\s@CreateJobResponse' {} a -> s {jobArn = a} :: CreateJobResponse)

-- | The job description.
createJobResponse_description :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_description = Lens.lens (\CreateJobResponse' {description} -> description) (\s@CreateJobResponse' {} a -> s {description = a} :: CreateJobResponse)

-- | The unique identifier you assigned to this job.
createJobResponse_jobId :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_jobId = Lens.lens (\CreateJobResponse' {jobId} -> jobId) (\s@CreateJobResponse' {} a -> s {jobId = a} :: CreateJobResponse)

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Prelude.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Prelude.NFData CreateJobResponse
