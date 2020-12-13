{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.StartMailboxExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a mailbox export job to export MIME-format email messages and calendar items from the specified mailbox to the specified Amazon Simple Storage Service (Amazon S3) bucket. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/mail-export.html Exporting mailbox content> in the /Amazon WorkMail Administrator Guide/ .
module Network.AWS.WorkMail.StartMailboxExportJob
  ( -- * Creating a request
    StartMailboxExportJob (..),
    mkStartMailboxExportJob,

    -- ** Request lenses
    smejKMSKeyARN,
    smejClientToken,
    smejS3Prefix,
    smejEntityId,
    smejDescription,
    smejOrganizationId,
    smejS3BucketName,
    smejRoleARN,

    -- * Destructuring the response
    StartMailboxExportJobResponse (..),
    mkStartMailboxExportJobResponse,

    -- ** Response lenses
    smejrsJobId,
    smejrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkStartMailboxExportJob' smart constructor.
data StartMailboxExportJob = StartMailboxExportJob'
  { -- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
    kmsKeyARN :: Lude.Text,
    -- | The idempotency token for the client request.
    clientToken :: Lude.Text,
    -- | The S3 bucket prefix.
    s3Prefix :: Lude.Text,
    -- | The identifier of the user or resource associated with the mailbox.
    entityId :: Lude.Text,
    -- | The mailbox export job description.
    description :: Lude.Maybe Lude.Text,
    -- | The identifier associated with the organization.
    organizationId :: Lude.Text,
    -- | The name of the S3 bucket.
    s3BucketName :: Lude.Text,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the S3 bucket.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMailboxExportJob' with the minimum fields required to make a request.
--
-- * 'kmsKeyARN' - The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
-- * 'clientToken' - The idempotency token for the client request.
-- * 's3Prefix' - The S3 bucket prefix.
-- * 'entityId' - The identifier of the user or resource associated with the mailbox.
-- * 'description' - The mailbox export job description.
-- * 'organizationId' - The identifier associated with the organization.
-- * 's3BucketName' - The name of the S3 bucket.
-- * 'roleARN' - The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the S3 bucket.
mkStartMailboxExportJob ::
  -- | 'kmsKeyARN'
  Lude.Text ->
  -- | 'clientToken'
  Lude.Text ->
  -- | 's3Prefix'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  -- | 's3BucketName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  StartMailboxExportJob
mkStartMailboxExportJob
  pKMSKeyARN_
  pClientToken_
  pS3Prefix_
  pEntityId_
  pOrganizationId_
  pS3BucketName_
  pRoleARN_ =
    StartMailboxExportJob'
      { kmsKeyARN = pKMSKeyARN_,
        clientToken = pClientToken_,
        s3Prefix = pS3Prefix_,
        entityId = pEntityId_,
        description = Lude.Nothing,
        organizationId = pOrganizationId_,
        s3BucketName = pS3BucketName_,
        roleARN = pRoleARN_
      }

-- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejKMSKeyARN :: Lens.Lens' StartMailboxExportJob Lude.Text
smejKMSKeyARN = Lens.lens (kmsKeyARN :: StartMailboxExportJob -> Lude.Text) (\s a -> s {kmsKeyARN = a} :: StartMailboxExportJob)
{-# DEPRECATED smejKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | The idempotency token for the client request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejClientToken :: Lens.Lens' StartMailboxExportJob Lude.Text
smejClientToken = Lens.lens (clientToken :: StartMailboxExportJob -> Lude.Text) (\s a -> s {clientToken = a} :: StartMailboxExportJob)
{-# DEPRECATED smejClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The S3 bucket prefix.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejS3Prefix :: Lens.Lens' StartMailboxExportJob Lude.Text
smejS3Prefix = Lens.lens (s3Prefix :: StartMailboxExportJob -> Lude.Text) (\s a -> s {s3Prefix = a} :: StartMailboxExportJob)
{-# DEPRECATED smejS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | The identifier of the user or resource associated with the mailbox.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejEntityId :: Lens.Lens' StartMailboxExportJob Lude.Text
smejEntityId = Lens.lens (entityId :: StartMailboxExportJob -> Lude.Text) (\s a -> s {entityId = a} :: StartMailboxExportJob)
{-# DEPRECATED smejEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The mailbox export job description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejDescription :: Lens.Lens' StartMailboxExportJob (Lude.Maybe Lude.Text)
smejDescription = Lens.lens (description :: StartMailboxExportJob -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StartMailboxExportJob)
{-# DEPRECATED smejDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The identifier associated with the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejOrganizationId :: Lens.Lens' StartMailboxExportJob Lude.Text
smejOrganizationId = Lens.lens (organizationId :: StartMailboxExportJob -> Lude.Text) (\s a -> s {organizationId = a} :: StartMailboxExportJob)
{-# DEPRECATED smejOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejS3BucketName :: Lens.Lens' StartMailboxExportJob Lude.Text
smejS3BucketName = Lens.lens (s3BucketName :: StartMailboxExportJob -> Lude.Text) (\s a -> s {s3BucketName = a} :: StartMailboxExportJob)
{-# DEPRECATED smejS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the S3 bucket.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejRoleARN :: Lens.Lens' StartMailboxExportJob Lude.Text
smejRoleARN = Lens.lens (roleARN :: StartMailboxExportJob -> Lude.Text) (\s a -> s {roleARN = a} :: StartMailboxExportJob)
{-# DEPRECATED smejRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest StartMailboxExportJob where
  type Rs StartMailboxExportJob = StartMailboxExportJobResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartMailboxExportJobResponse'
            Lude.<$> (x Lude..?> "JobId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartMailboxExportJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.StartMailboxExportJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartMailboxExportJob where
  toJSON StartMailboxExportJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KmsKeyArn" Lude..= kmsKeyARN),
            Lude.Just ("ClientToken" Lude..= clientToken),
            Lude.Just ("S3Prefix" Lude..= s3Prefix),
            Lude.Just ("EntityId" Lude..= entityId),
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("S3BucketName" Lude..= s3BucketName),
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath StartMailboxExportJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartMailboxExportJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartMailboxExportJobResponse' smart constructor.
data StartMailboxExportJobResponse = StartMailboxExportJobResponse'
  { -- | The job ID.
    jobId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMailboxExportJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The job ID.
-- * 'responseStatus' - The response status code.
mkStartMailboxExportJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartMailboxExportJobResponse
mkStartMailboxExportJobResponse pResponseStatus_ =
  StartMailboxExportJobResponse'
    { jobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The job ID.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejrsJobId :: Lens.Lens' StartMailboxExportJobResponse (Lude.Maybe Lude.Text)
smejrsJobId = Lens.lens (jobId :: StartMailboxExportJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartMailboxExportJobResponse)
{-# DEPRECATED smejrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejrsResponseStatus :: Lens.Lens' StartMailboxExportJobResponse Lude.Int
smejrsResponseStatus = Lens.lens (responseStatus :: StartMailboxExportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMailboxExportJobResponse)
{-# DEPRECATED smejrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
