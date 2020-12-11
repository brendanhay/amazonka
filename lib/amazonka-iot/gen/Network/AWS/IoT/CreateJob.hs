{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job.
module Network.AWS.IoT.CreateJob
  ( -- * Creating a request
    CreateJob (..),
    mkCreateJob,

    -- ** Request lenses
    cjJobExecutionsRolloutConfig,
    cjDocumentSource,
    cjAbortConfig,
    cjNamespaceId,
    cjPresignedURLConfig,
    cjDocument,
    cjDescription,
    cjTargetSelection,
    cjTimeoutConfig,
    cjTags,
    cjJobId,
    cjTargets,

    -- * Destructuring the response
    CreateJobResponse (..),
    mkCreateJobResponse,

    -- ** Response lenses
    cjrsJobId,
    cjrsJobARN,
    cjrsDescription,
    cjrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { jobExecutionsRolloutConfig ::
      Lude.Maybe JobExecutionsRolloutConfig,
    documentSource :: Lude.Maybe Lude.Text,
    abortConfig :: Lude.Maybe AbortConfig,
    namespaceId :: Lude.Maybe Lude.Text,
    presignedURLConfig :: Lude.Maybe PresignedURLConfig,
    document :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    targetSelection :: Lude.Maybe TargetSelection,
    timeoutConfig :: Lude.Maybe TimeoutConfig,
    tags :: Lude.Maybe [Tag],
    jobId :: Lude.Text,
    targets :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- * 'abortConfig' - Allows you to create criteria to abort a job.
-- * 'description' - A short text description of the job.
-- * 'document' - The job document.
-- * 'documentSource' - An S3 link to the job document.
-- * 'jobExecutionsRolloutConfig' - Allows you to create a staged rollout of the job.
-- * 'jobId' - A job identifier which must be unique for your AWS account. We recommend using a UUID. Alpha-numeric characters, "-" and "_" are valid for use here.
-- * 'namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
-- * 'presignedURLConfig' - Configuration information for pre-signed S3 URLs.
-- * 'tags' - Metadata which can be used to manage the job.
-- * 'targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
-- * 'targets' - A list of things and thing groups to which the job should be sent.
-- * 'timeoutConfig' - Specifies the amount of time each device has to finish its execution of the job. The timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the time expires, it will be automatically set to @TIMED_OUT@ .
mkCreateJob ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'targets'
  Lude.NonEmpty Lude.Text ->
  CreateJob
mkCreateJob pJobId_ pTargets_ =
  CreateJob'
    { jobExecutionsRolloutConfig = Lude.Nothing,
      documentSource = Lude.Nothing,
      abortConfig = Lude.Nothing,
      namespaceId = Lude.Nothing,
      presignedURLConfig = Lude.Nothing,
      document = Lude.Nothing,
      description = Lude.Nothing,
      targetSelection = Lude.Nothing,
      timeoutConfig = Lude.Nothing,
      tags = Lude.Nothing,
      jobId = pJobId_,
      targets = pTargets_
    }

-- | Allows you to create a staged rollout of the job.
--
-- /Note:/ Consider using 'jobExecutionsRolloutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobExecutionsRolloutConfig :: Lens.Lens' CreateJob (Lude.Maybe JobExecutionsRolloutConfig)
cjJobExecutionsRolloutConfig = Lens.lens (jobExecutionsRolloutConfig :: CreateJob -> Lude.Maybe JobExecutionsRolloutConfig) (\s a -> s {jobExecutionsRolloutConfig = a} :: CreateJob)
{-# DEPRECATED cjJobExecutionsRolloutConfig "Use generic-lens or generic-optics with 'jobExecutionsRolloutConfig' instead." #-}

-- | An S3 link to the job document.
--
-- /Note:/ Consider using 'documentSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDocumentSource :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjDocumentSource = Lens.lens (documentSource :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {documentSource = a} :: CreateJob)
{-# DEPRECATED cjDocumentSource "Use generic-lens or generic-optics with 'documentSource' instead." #-}

-- | Allows you to create criteria to abort a job.
--
-- /Note:/ Consider using 'abortConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjAbortConfig :: Lens.Lens' CreateJob (Lude.Maybe AbortConfig)
cjAbortConfig = Lens.lens (abortConfig :: CreateJob -> Lude.Maybe AbortConfig) (\s a -> s {abortConfig = a} :: CreateJob)
{-# DEPRECATED cjAbortConfig "Use generic-lens or generic-optics with 'abortConfig' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjNamespaceId :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjNamespaceId = Lens.lens (namespaceId :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {namespaceId = a} :: CreateJob)
{-# DEPRECATED cjNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | Configuration information for pre-signed S3 URLs.
--
-- /Note:/ Consider using 'presignedURLConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjPresignedURLConfig :: Lens.Lens' CreateJob (Lude.Maybe PresignedURLConfig)
cjPresignedURLConfig = Lens.lens (presignedURLConfig :: CreateJob -> Lude.Maybe PresignedURLConfig) (\s a -> s {presignedURLConfig = a} :: CreateJob)
{-# DEPRECATED cjPresignedURLConfig "Use generic-lens or generic-optics with 'presignedURLConfig' instead." #-}

-- | The job document.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDocument :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjDocument = Lens.lens (document :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {document = a} :: CreateJob)
{-# DEPRECATED cjDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | A short text description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDescription :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjDescription = Lens.lens (description :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateJob)
{-# DEPRECATED cjDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTargetSelection :: Lens.Lens' CreateJob (Lude.Maybe TargetSelection)
cjTargetSelection = Lens.lens (targetSelection :: CreateJob -> Lude.Maybe TargetSelection) (\s a -> s {targetSelection = a} :: CreateJob)
{-# DEPRECATED cjTargetSelection "Use generic-lens or generic-optics with 'targetSelection' instead." #-}

-- | Specifies the amount of time each device has to finish its execution of the job. The timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the time expires, it will be automatically set to @TIMED_OUT@ .
--
-- /Note:/ Consider using 'timeoutConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTimeoutConfig :: Lens.Lens' CreateJob (Lude.Maybe TimeoutConfig)
cjTimeoutConfig = Lens.lens (timeoutConfig :: CreateJob -> Lude.Maybe TimeoutConfig) (\s a -> s {timeoutConfig = a} :: CreateJob)
{-# DEPRECATED cjTimeoutConfig "Use generic-lens or generic-optics with 'timeoutConfig' instead." #-}

-- | Metadata which can be used to manage the job.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTags :: Lens.Lens' CreateJob (Lude.Maybe [Tag])
cjTags = Lens.lens (tags :: CreateJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateJob)
{-# DEPRECATED cjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A job identifier which must be unique for your AWS account. We recommend using a UUID. Alpha-numeric characters, "-" and "_" are valid for use here.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobId :: Lens.Lens' CreateJob Lude.Text
cjJobId = Lens.lens (jobId :: CreateJob -> Lude.Text) (\s a -> s {jobId = a} :: CreateJob)
{-# DEPRECATED cjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | A list of things and thing groups to which the job should be sent.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTargets :: Lens.Lens' CreateJob (Lude.NonEmpty Lude.Text)
cjTargets = Lens.lens (targets :: CreateJob -> Lude.NonEmpty Lude.Text) (\s a -> s {targets = a} :: CreateJob)
{-# DEPRECATED cjTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Lude.AWSRequest CreateJob where
  type Rs CreateJob = CreateJobResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Lude.<$> (x Lude..?> "jobId")
            Lude.<*> (x Lude..?> "jobArn")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("jobExecutionsRolloutConfig" Lude..=)
              Lude.<$> jobExecutionsRolloutConfig,
            ("documentSource" Lude..=) Lude.<$> documentSource,
            ("abortConfig" Lude..=) Lude.<$> abortConfig,
            ("namespaceId" Lude..=) Lude.<$> namespaceId,
            ("presignedUrlConfig" Lude..=) Lude.<$> presignedURLConfig,
            ("document" Lude..=) Lude.<$> document,
            ("description" Lude..=) Lude.<$> description,
            ("targetSelection" Lude..=) Lude.<$> targetSelection,
            ("timeoutConfig" Lude..=) Lude.<$> timeoutConfig,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("targets" Lude..= targets)
          ]
      )

instance Lude.ToPath CreateJob where
  toPath CreateJob' {..} = Lude.mconcat ["/jobs/", Lude.toBS jobId]

instance Lude.ToQuery CreateJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { jobId ::
      Lude.Maybe Lude.Text,
    jobARN :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- * 'description' - The job description.
-- * 'jobARN' - The job ARN.
-- * 'jobId' - The unique identifier you assigned to this job.
-- * 'responseStatus' - The response status code.
mkCreateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateJobResponse
mkCreateJobResponse pResponseStatus_ =
  CreateJobResponse'
    { jobId = Lude.Nothing,
      jobARN = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier you assigned to this job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsJobId :: Lens.Lens' CreateJobResponse (Lude.Maybe Lude.Text)
cjrsJobId = Lens.lens (jobId :: CreateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: CreateJobResponse)
{-# DEPRECATED cjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The job ARN.
--
-- /Note:/ Consider using 'jobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsJobARN :: Lens.Lens' CreateJobResponse (Lude.Maybe Lude.Text)
cjrsJobARN = Lens.lens (jobARN :: CreateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobARN = a} :: CreateJobResponse)
{-# DEPRECATED cjrsJobARN "Use generic-lens or generic-optics with 'jobARN' instead." #-}

-- | The job description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsDescription :: Lens.Lens' CreateJobResponse (Lude.Maybe Lude.Text)
cjrsDescription = Lens.lens (description :: CreateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateJobResponse)
{-# DEPRECATED cjrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsResponseStatus :: Lens.Lens' CreateJobResponse Lude.Int
cjrsResponseStatus = Lens.lens (responseStatus :: CreateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateJobResponse)
{-# DEPRECATED cjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
