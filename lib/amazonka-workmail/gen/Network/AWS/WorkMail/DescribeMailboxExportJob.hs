{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DescribeMailboxExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current status of a mailbox export job.
module Network.AWS.WorkMail.DescribeMailboxExportJob
  ( -- * Creating a request
    DescribeMailboxExportJob (..),
    mkDescribeMailboxExportJob,

    -- ** Request lenses
    dmejJobId,
    dmejOrganizationId,

    -- * Destructuring the response
    DescribeMailboxExportJobResponse (..),
    mkDescribeMailboxExportJobResponse,

    -- ** Response lenses
    dmejrsState,
    dmejrsKMSKeyARN,
    dmejrsStartTime,
    dmejrsEstimatedProgress,
    dmejrsEndTime,
    dmejrsS3Path,
    dmejrsS3Prefix,
    dmejrsEntityId,
    dmejrsDescription,
    dmejrsErrorInfo,
    dmejrsS3BucketName,
    dmejrsRoleARN,
    dmejrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDescribeMailboxExportJob' smart constructor.
data DescribeMailboxExportJob = DescribeMailboxExportJob'
  { -- | The mailbox export job ID.
    jobId :: Lude.Text,
    -- | The organization ID.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMailboxExportJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The mailbox export job ID.
-- * 'organizationId' - The organization ID.
mkDescribeMailboxExportJob ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  DescribeMailboxExportJob
mkDescribeMailboxExportJob pJobId_ pOrganizationId_ =
  DescribeMailboxExportJob'
    { jobId = pJobId_,
      organizationId = pOrganizationId_
    }

-- | The mailbox export job ID.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejJobId :: Lens.Lens' DescribeMailboxExportJob Lude.Text
dmejJobId = Lens.lens (jobId :: DescribeMailboxExportJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeMailboxExportJob)
{-# DEPRECATED dmejJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejOrganizationId :: Lens.Lens' DescribeMailboxExportJob Lude.Text
dmejOrganizationId = Lens.lens (organizationId :: DescribeMailboxExportJob -> Lude.Text) (\s a -> s {organizationId = a} :: DescribeMailboxExportJob)
{-# DEPRECATED dmejOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DescribeMailboxExportJob where
  type Rs DescribeMailboxExportJob = DescribeMailboxExportJobResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMailboxExportJobResponse'
            Lude.<$> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "KmsKeyArn")
            Lude.<*> (x Lude..?> "StartTime")
            Lude.<*> (x Lude..?> "EstimatedProgress")
            Lude.<*> (x Lude..?> "EndTime")
            Lude.<*> (x Lude..?> "S3Path")
            Lude.<*> (x Lude..?> "S3Prefix")
            Lude.<*> (x Lude..?> "EntityId")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "ErrorInfo")
            Lude.<*> (x Lude..?> "S3BucketName")
            Lude.<*> (x Lude..?> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMailboxExportJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DescribeMailboxExportJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMailboxExportJob where
  toJSON DescribeMailboxExportJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobId" Lude..= jobId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath DescribeMailboxExportJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMailboxExportJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMailboxExportJobResponse' smart constructor.
data DescribeMailboxExportJobResponse = DescribeMailboxExportJobResponse'
  { -- | The state of the mailbox export job.
    state :: Lude.Maybe MailboxExportJobState,
    -- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
    kmsKeyARN :: Lude.Maybe Lude.Text,
    -- | The mailbox export job start timestamp.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The estimated progress of the mailbox export job, in percentage points.
    estimatedProgress :: Lude.Maybe Lude.Natural,
    -- | The mailbox export job end timestamp.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The path to the S3 bucket and file that the mailbox export job is exporting to.
    s3Path :: Lude.Maybe Lude.Text,
    -- | The S3 bucket prefix.
    s3Prefix :: Lude.Maybe Lude.Text,
    -- | The identifier of the user or resource associated with the mailbox.
    entityId :: Lude.Maybe Lude.Text,
    -- | The mailbox export job description.
    description :: Lude.Maybe Lude.Text,
    -- | Error information for failed mailbox export jobs.
    errorInfo :: Lude.Maybe Lude.Text,
    -- | The name of the S3 bucket.
    s3BucketName :: Lude.Maybe Lude.Text,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the Amazon Simple Storage Service (Amazon S3) bucket.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMailboxExportJobResponse' with the minimum fields required to make a request.
--
-- * 'state' - The state of the mailbox export job.
-- * 'kmsKeyARN' - The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
-- * 'startTime' - The mailbox export job start timestamp.
-- * 'estimatedProgress' - The estimated progress of the mailbox export job, in percentage points.
-- * 'endTime' - The mailbox export job end timestamp.
-- * 's3Path' - The path to the S3 bucket and file that the mailbox export job is exporting to.
-- * 's3Prefix' - The S3 bucket prefix.
-- * 'entityId' - The identifier of the user or resource associated with the mailbox.
-- * 'description' - The mailbox export job description.
-- * 'errorInfo' - Error information for failed mailbox export jobs.
-- * 's3BucketName' - The name of the S3 bucket.
-- * 'roleARN' - The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the Amazon Simple Storage Service (Amazon S3) bucket.
-- * 'responseStatus' - The response status code.
mkDescribeMailboxExportJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMailboxExportJobResponse
mkDescribeMailboxExportJobResponse pResponseStatus_ =
  DescribeMailboxExportJobResponse'
    { state = Lude.Nothing,
      kmsKeyARN = Lude.Nothing,
      startTime = Lude.Nothing,
      estimatedProgress = Lude.Nothing,
      endTime = Lude.Nothing,
      s3Path = Lude.Nothing,
      s3Prefix = Lude.Nothing,
      entityId = Lude.Nothing,
      description = Lude.Nothing,
      errorInfo = Lude.Nothing,
      s3BucketName = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state of the mailbox export job.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsState :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe MailboxExportJobState)
dmejrsState = Lens.lens (state :: DescribeMailboxExportJobResponse -> Lude.Maybe MailboxExportJobState) (\s a -> s {state = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsKMSKeyARN :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe Lude.Text)
dmejrsKMSKeyARN = Lens.lens (kmsKeyARN :: DescribeMailboxExportJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | The mailbox export job start timestamp.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsStartTime :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe Lude.Timestamp)
dmejrsStartTime = Lens.lens (startTime :: DescribeMailboxExportJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The estimated progress of the mailbox export job, in percentage points.
--
-- /Note:/ Consider using 'estimatedProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsEstimatedProgress :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe Lude.Natural)
dmejrsEstimatedProgress = Lens.lens (estimatedProgress :: DescribeMailboxExportJobResponse -> Lude.Maybe Lude.Natural) (\s a -> s {estimatedProgress = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsEstimatedProgress "Use generic-lens or generic-optics with 'estimatedProgress' instead." #-}

-- | The mailbox export job end timestamp.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsEndTime :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe Lude.Timestamp)
dmejrsEndTime = Lens.lens (endTime :: DescribeMailboxExportJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The path to the S3 bucket and file that the mailbox export job is exporting to.
--
-- /Note:/ Consider using 's3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsS3Path :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe Lude.Text)
dmejrsS3Path = Lens.lens (s3Path :: DescribeMailboxExportJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3Path = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsS3Path "Use generic-lens or generic-optics with 's3Path' instead." #-}

-- | The S3 bucket prefix.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsS3Prefix :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe Lude.Text)
dmejrsS3Prefix = Lens.lens (s3Prefix :: DescribeMailboxExportJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3Prefix = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | The identifier of the user or resource associated with the mailbox.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsEntityId :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe Lude.Text)
dmejrsEntityId = Lens.lens (entityId :: DescribeMailboxExportJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {entityId = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The mailbox export job description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsDescription :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe Lude.Text)
dmejrsDescription = Lens.lens (description :: DescribeMailboxExportJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Error information for failed mailbox export jobs.
--
-- /Note:/ Consider using 'errorInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsErrorInfo :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe Lude.Text)
dmejrsErrorInfo = Lens.lens (errorInfo :: DescribeMailboxExportJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {errorInfo = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsErrorInfo "Use generic-lens or generic-optics with 'errorInfo' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsS3BucketName :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe Lude.Text)
dmejrsS3BucketName = Lens.lens (s3BucketName :: DescribeMailboxExportJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the Amazon Simple Storage Service (Amazon S3) bucket.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsRoleARN :: Lens.Lens' DescribeMailboxExportJobResponse (Lude.Maybe Lude.Text)
dmejrsRoleARN = Lens.lens (roleARN :: DescribeMailboxExportJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrsResponseStatus :: Lens.Lens' DescribeMailboxExportJobResponse Lude.Int
dmejrsResponseStatus = Lens.lens (responseStatus :: DescribeMailboxExportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMailboxExportJobResponse)
{-# DEPRECATED dmejrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
