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
-- Module      : Network.AWS.MediaConvert.CreateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new transcoding job. For information about jobs and job
-- settings, see the User Guide at
-- http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
module Network.AWS.MediaConvert.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_accelerationSettings,
    createJob_billingTagsSource,
    createJob_priority,
    createJob_statusUpdateInterval,
    createJob_jobTemplate,
    createJob_userMetadata,
    createJob_tags,
    createJob_queue,
    createJob_simulateReservedQueue,
    createJob_clientRequestToken,
    createJob_hopDestinations,
    createJob_role,
    createJob_settings,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_job,
    createJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | Optional. Accelerated transcoding can significantly speed up jobs with
    -- long, visually complex content. Outputs that use this feature incur
    -- pro-tier pricing. For information about feature limitations, see the AWS
    -- Elemental MediaConvert User Guide.
    accelerationSettings :: Prelude.Maybe AccelerationSettings,
    -- | Optional. Choose a tag type that AWS Billing and Cost Management will
    -- use to sort your AWS Elemental MediaConvert costs on any billing report
    -- that you set up. Any transcoding outputs that don\'t have an associated
    -- tag will appear in your billing report unsorted. If you don\'t choose a
    -- valid value for this field, your job outputs will appear on the billing
    -- report unsorted.
    billingTagsSource :: Prelude.Maybe BillingTagsSource,
    -- | Optional. Specify the relative priority for this job. In any given
    -- queue, the service begins processing the job with the highest value
    -- first. When more than one job has the same priority, the service begins
    -- processing the job that you submitted first. If you don\'t specify a
    -- priority, the service uses the default value 0.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Optional. Specify how often MediaConvert sends STATUS_UPDATE events to
    -- Amazon CloudWatch Events. Set the interval, in seconds, between status
    -- updates. MediaConvert sends an update at this interval from the time the
    -- service begins processing your job to the time it completes the
    -- transcode or encounters an error.
    statusUpdateInterval :: Prelude.Maybe StatusUpdateInterval,
    -- | Optional. When you create a job, you can either specify a job template
    -- or specify the transcoding settings individually.
    jobTemplate :: Prelude.Maybe Prelude.Text,
    -- | Optional. User-defined metadata that you want to associate with an
    -- MediaConvert job. You specify metadata in key\/value pairs. Use only for
    -- existing integrations or workflows that rely on job metadata tags.
    -- Otherwise, we recommend that you use standard AWS tags.
    userMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Optional. The tags that you want to add to the resource. You can tag
    -- resources with a key-value pair or with only a key. Use standard AWS
    -- tags on your job for automatic integration with AWS services and for
    -- custom integrations and workflows.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Optional. When you create a job, you can specify a queue to send it to.
    -- If you don\'t specify, the job will go to the default queue. For more
    -- about queues, see the User Guide topic at
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html.
    queue :: Prelude.Maybe Prelude.Text,
    -- | Optional. Enable this setting when you run a test job to estimate how
    -- many reserved transcoding slots (RTS) you need. When this is enabled,
    -- MediaConvert runs your job from an on-demand queue with similar
    -- performance to what you will see with one RTS in a reserved queue. This
    -- setting is disabled by default.
    simulateReservedQueue :: Prelude.Maybe SimulateReservedQueue,
    -- | Optional. Idempotency token for CreateJob operation.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Optional. Use queue hopping to avoid overly long waits in the backlog of
    -- the queue that you submit your job to. Specify an alternate queue and
    -- the maximum time that your job will wait in the initial queue before
    -- hopping. For more information about this feature, see the AWS Elemental
    -- MediaConvert User Guide.
    hopDestinations :: Prelude.Maybe [HopDestination],
    -- | Required. The IAM role you use for creating this job. For details about
    -- permissions, see the User Guide topic at the User Guide at
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html.
    role' :: Prelude.Text,
    -- | JobSettings contains all the transcode settings for a job.
    settings :: JobSettings
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
-- 'accelerationSettings', 'createJob_accelerationSettings' - Optional. Accelerated transcoding can significantly speed up jobs with
-- long, visually complex content. Outputs that use this feature incur
-- pro-tier pricing. For information about feature limitations, see the AWS
-- Elemental MediaConvert User Guide.
--
-- 'billingTagsSource', 'createJob_billingTagsSource' - Optional. Choose a tag type that AWS Billing and Cost Management will
-- use to sort your AWS Elemental MediaConvert costs on any billing report
-- that you set up. Any transcoding outputs that don\'t have an associated
-- tag will appear in your billing report unsorted. If you don\'t choose a
-- valid value for this field, your job outputs will appear on the billing
-- report unsorted.
--
-- 'priority', 'createJob_priority' - Optional. Specify the relative priority for this job. In any given
-- queue, the service begins processing the job with the highest value
-- first. When more than one job has the same priority, the service begins
-- processing the job that you submitted first. If you don\'t specify a
-- priority, the service uses the default value 0.
--
-- 'statusUpdateInterval', 'createJob_statusUpdateInterval' - Optional. Specify how often MediaConvert sends STATUS_UPDATE events to
-- Amazon CloudWatch Events. Set the interval, in seconds, between status
-- updates. MediaConvert sends an update at this interval from the time the
-- service begins processing your job to the time it completes the
-- transcode or encounters an error.
--
-- 'jobTemplate', 'createJob_jobTemplate' - Optional. When you create a job, you can either specify a job template
-- or specify the transcoding settings individually.
--
-- 'userMetadata', 'createJob_userMetadata' - Optional. User-defined metadata that you want to associate with an
-- MediaConvert job. You specify metadata in key\/value pairs. Use only for
-- existing integrations or workflows that rely on job metadata tags.
-- Otherwise, we recommend that you use standard AWS tags.
--
-- 'tags', 'createJob_tags' - Optional. The tags that you want to add to the resource. You can tag
-- resources with a key-value pair or with only a key. Use standard AWS
-- tags on your job for automatic integration with AWS services and for
-- custom integrations and workflows.
--
-- 'queue', 'createJob_queue' - Optional. When you create a job, you can specify a queue to send it to.
-- If you don\'t specify, the job will go to the default queue. For more
-- about queues, see the User Guide topic at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html.
--
-- 'simulateReservedQueue', 'createJob_simulateReservedQueue' - Optional. Enable this setting when you run a test job to estimate how
-- many reserved transcoding slots (RTS) you need. When this is enabled,
-- MediaConvert runs your job from an on-demand queue with similar
-- performance to what you will see with one RTS in a reserved queue. This
-- setting is disabled by default.
--
-- 'clientRequestToken', 'createJob_clientRequestToken' - Optional. Idempotency token for CreateJob operation.
--
-- 'hopDestinations', 'createJob_hopDestinations' - Optional. Use queue hopping to avoid overly long waits in the backlog of
-- the queue that you submit your job to. Specify an alternate queue and
-- the maximum time that your job will wait in the initial queue before
-- hopping. For more information about this feature, see the AWS Elemental
-- MediaConvert User Guide.
--
-- 'role'', 'createJob_role' - Required. The IAM role you use for creating this job. For details about
-- permissions, see the User Guide topic at the User Guide at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html.
--
-- 'settings', 'createJob_settings' - JobSettings contains all the transcode settings for a job.
newCreateJob ::
  -- | 'role''
  Prelude.Text ->
  -- | 'settings'
  JobSettings ->
  CreateJob
newCreateJob pRole_ pSettings_ =
  CreateJob'
    { accelerationSettings = Prelude.Nothing,
      billingTagsSource = Prelude.Nothing,
      priority = Prelude.Nothing,
      statusUpdateInterval = Prelude.Nothing,
      jobTemplate = Prelude.Nothing,
      userMetadata = Prelude.Nothing,
      tags = Prelude.Nothing,
      queue = Prelude.Nothing,
      simulateReservedQueue = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      hopDestinations = Prelude.Nothing,
      role' = pRole_,
      settings = pSettings_
    }

-- | Optional. Accelerated transcoding can significantly speed up jobs with
-- long, visually complex content. Outputs that use this feature incur
-- pro-tier pricing. For information about feature limitations, see the AWS
-- Elemental MediaConvert User Guide.
createJob_accelerationSettings :: Lens.Lens' CreateJob (Prelude.Maybe AccelerationSettings)
createJob_accelerationSettings = Lens.lens (\CreateJob' {accelerationSettings} -> accelerationSettings) (\s@CreateJob' {} a -> s {accelerationSettings = a} :: CreateJob)

-- | Optional. Choose a tag type that AWS Billing and Cost Management will
-- use to sort your AWS Elemental MediaConvert costs on any billing report
-- that you set up. Any transcoding outputs that don\'t have an associated
-- tag will appear in your billing report unsorted. If you don\'t choose a
-- valid value for this field, your job outputs will appear on the billing
-- report unsorted.
createJob_billingTagsSource :: Lens.Lens' CreateJob (Prelude.Maybe BillingTagsSource)
createJob_billingTagsSource = Lens.lens (\CreateJob' {billingTagsSource} -> billingTagsSource) (\s@CreateJob' {} a -> s {billingTagsSource = a} :: CreateJob)

-- | Optional. Specify the relative priority for this job. In any given
-- queue, the service begins processing the job with the highest value
-- first. When more than one job has the same priority, the service begins
-- processing the job that you submitted first. If you don\'t specify a
-- priority, the service uses the default value 0.
createJob_priority :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Int)
createJob_priority = Lens.lens (\CreateJob' {priority} -> priority) (\s@CreateJob' {} a -> s {priority = a} :: CreateJob)

-- | Optional. Specify how often MediaConvert sends STATUS_UPDATE events to
-- Amazon CloudWatch Events. Set the interval, in seconds, between status
-- updates. MediaConvert sends an update at this interval from the time the
-- service begins processing your job to the time it completes the
-- transcode or encounters an error.
createJob_statusUpdateInterval :: Lens.Lens' CreateJob (Prelude.Maybe StatusUpdateInterval)
createJob_statusUpdateInterval = Lens.lens (\CreateJob' {statusUpdateInterval} -> statusUpdateInterval) (\s@CreateJob' {} a -> s {statusUpdateInterval = a} :: CreateJob)

-- | Optional. When you create a job, you can either specify a job template
-- or specify the transcoding settings individually.
createJob_jobTemplate :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_jobTemplate = Lens.lens (\CreateJob' {jobTemplate} -> jobTemplate) (\s@CreateJob' {} a -> s {jobTemplate = a} :: CreateJob)

-- | Optional. User-defined metadata that you want to associate with an
-- MediaConvert job. You specify metadata in key\/value pairs. Use only for
-- existing integrations or workflows that rely on job metadata tags.
-- Otherwise, we recommend that you use standard AWS tags.
createJob_userMetadata :: Lens.Lens' CreateJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createJob_userMetadata = Lens.lens (\CreateJob' {userMetadata} -> userMetadata) (\s@CreateJob' {} a -> s {userMetadata = a} :: CreateJob) Prelude.. Lens.mapping Lens._Coerce

-- | Optional. The tags that you want to add to the resource. You can tag
-- resources with a key-value pair or with only a key. Use standard AWS
-- tags on your job for automatic integration with AWS services and for
-- custom integrations and workflows.
createJob_tags :: Lens.Lens' CreateJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createJob_tags = Lens.lens (\CreateJob' {tags} -> tags) (\s@CreateJob' {} a -> s {tags = a} :: CreateJob) Prelude.. Lens.mapping Lens._Coerce

-- | Optional. When you create a job, you can specify a queue to send it to.
-- If you don\'t specify, the job will go to the default queue. For more
-- about queues, see the User Guide topic at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html.
createJob_queue :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_queue = Lens.lens (\CreateJob' {queue} -> queue) (\s@CreateJob' {} a -> s {queue = a} :: CreateJob)

-- | Optional. Enable this setting when you run a test job to estimate how
-- many reserved transcoding slots (RTS) you need. When this is enabled,
-- MediaConvert runs your job from an on-demand queue with similar
-- performance to what you will see with one RTS in a reserved queue. This
-- setting is disabled by default.
createJob_simulateReservedQueue :: Lens.Lens' CreateJob (Prelude.Maybe SimulateReservedQueue)
createJob_simulateReservedQueue = Lens.lens (\CreateJob' {simulateReservedQueue} -> simulateReservedQueue) (\s@CreateJob' {} a -> s {simulateReservedQueue = a} :: CreateJob)

-- | Optional. Idempotency token for CreateJob operation.
createJob_clientRequestToken :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_clientRequestToken = Lens.lens (\CreateJob' {clientRequestToken} -> clientRequestToken) (\s@CreateJob' {} a -> s {clientRequestToken = a} :: CreateJob)

-- | Optional. Use queue hopping to avoid overly long waits in the backlog of
-- the queue that you submit your job to. Specify an alternate queue and
-- the maximum time that your job will wait in the initial queue before
-- hopping. For more information about this feature, see the AWS Elemental
-- MediaConvert User Guide.
createJob_hopDestinations :: Lens.Lens' CreateJob (Prelude.Maybe [HopDestination])
createJob_hopDestinations = Lens.lens (\CreateJob' {hopDestinations} -> hopDestinations) (\s@CreateJob' {} a -> s {hopDestinations = a} :: CreateJob) Prelude.. Lens.mapping Lens._Coerce

-- | Required. The IAM role you use for creating this job. For details about
-- permissions, see the User Guide topic at the User Guide at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html.
createJob_role :: Lens.Lens' CreateJob Prelude.Text
createJob_role = Lens.lens (\CreateJob' {role'} -> role') (\s@CreateJob' {} a -> s {role' = a} :: CreateJob)

-- | JobSettings contains all the transcode settings for a job.
createJob_settings :: Lens.Lens' CreateJob JobSettings
createJob_settings = Lens.lens (\CreateJob' {settings} -> settings) (\s@CreateJob' {} a -> s {settings = a} :: CreateJob)

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Prelude.<$> (x Core..?> "job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateJob

instance Prelude.NFData CreateJob

instance Core.ToHeaders CreateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("accelerationSettings" Core..=)
              Prelude.<$> accelerationSettings,
            ("billingTagsSource" Core..=)
              Prelude.<$> billingTagsSource,
            ("priority" Core..=) Prelude.<$> priority,
            ("statusUpdateInterval" Core..=)
              Prelude.<$> statusUpdateInterval,
            ("jobTemplate" Core..=) Prelude.<$> jobTemplate,
            ("userMetadata" Core..=) Prelude.<$> userMetadata,
            ("tags" Core..=) Prelude.<$> tags,
            ("queue" Core..=) Prelude.<$> queue,
            ("simulateReservedQueue" Core..=)
              Prelude.<$> simulateReservedQueue,
            ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("hopDestinations" Core..=)
              Prelude.<$> hopDestinations,
            Prelude.Just ("role" Core..= role'),
            Prelude.Just ("settings" Core..= settings)
          ]
      )

instance Core.ToPath CreateJob where
  toPath = Prelude.const "/2017-08-29/jobs"

instance Core.ToQuery CreateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | Each job converts an input file into an output file or files. For more
    -- information, see the User Guide at
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
    job :: Prelude.Maybe Job,
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
-- 'job', 'createJobResponse_job' - Each job converts an input file into an output file or files. For more
-- information, see the User Guide at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
--
-- 'httpStatus', 'createJobResponse_httpStatus' - The response's http status code.
newCreateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ =
  CreateJobResponse'
    { job = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Each job converts an input file into an output file or files. For more
-- information, see the User Guide at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
createJobResponse_job :: Lens.Lens' CreateJobResponse (Prelude.Maybe Job)
createJobResponse_job = Lens.lens (\CreateJobResponse' {job} -> job) (\s@CreateJobResponse' {} a -> s {job = a} :: CreateJobResponse)

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Prelude.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Prelude.NFData CreateJobResponse
