{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    smejDescription,
    smejClientToken,
    smejOrganizationId,
    smejEntityId,
    smejRoleARN,
    smejKMSKeyARN,
    smejS3BucketName,
    smejS3Prefix,

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
  { description ::
      Lude.Maybe Lude.Text,
    clientToken :: Lude.Text,
    organizationId :: Lude.Text,
    entityId :: Lude.Text,
    roleARN :: Lude.Text,
    kmsKeyARN :: Lude.Text,
    s3BucketName :: Lude.Text,
    s3Prefix :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMailboxExportJob' with the minimum fields required to make a request.
--
-- * 'clientToken' - The idempotency token for the client request.
-- * 'description' - The mailbox export job description.
-- * 'entityId' - The identifier of the user or resource associated with the mailbox.
-- * 'kmsKeyARN' - The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
-- * 'organizationId' - The identifier associated with the organization.
-- * 'roleARN' - The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the S3 bucket.
-- * 's3BucketName' - The name of the S3 bucket.
-- * 's3Prefix' - The S3 bucket prefix.
mkStartMailboxExportJob ::
  -- | 'clientToken'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'kmsKeyARN'
  Lude.Text ->
  -- | 's3BucketName'
  Lude.Text ->
  -- | 's3Prefix'
  Lude.Text ->
  StartMailboxExportJob
mkStartMailboxExportJob
  pClientToken_
  pOrganizationId_
  pEntityId_
  pRoleARN_
  pKMSKeyARN_
  pS3BucketName_
  pS3Prefix_ =
    StartMailboxExportJob'
      { description = Lude.Nothing,
        clientToken = pClientToken_,
        organizationId = pOrganizationId_,
        entityId = pEntityId_,
        roleARN = pRoleARN_,
        kmsKeyARN = pKMSKeyARN_,
        s3BucketName = pS3BucketName_,
        s3Prefix = pS3Prefix_
      }

-- | The mailbox export job description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejDescription :: Lens.Lens' StartMailboxExportJob (Lude.Maybe Lude.Text)
smejDescription = Lens.lens (description :: StartMailboxExportJob -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StartMailboxExportJob)
{-# DEPRECATED smejDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The idempotency token for the client request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejClientToken :: Lens.Lens' StartMailboxExportJob Lude.Text
smejClientToken = Lens.lens (clientToken :: StartMailboxExportJob -> Lude.Text) (\s a -> s {clientToken = a} :: StartMailboxExportJob)
{-# DEPRECATED smejClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The identifier associated with the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejOrganizationId :: Lens.Lens' StartMailboxExportJob Lude.Text
smejOrganizationId = Lens.lens (organizationId :: StartMailboxExportJob -> Lude.Text) (\s a -> s {organizationId = a} :: StartMailboxExportJob)
{-# DEPRECATED smejOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier of the user or resource associated with the mailbox.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejEntityId :: Lens.Lens' StartMailboxExportJob Lude.Text
smejEntityId = Lens.lens (entityId :: StartMailboxExportJob -> Lude.Text) (\s a -> s {entityId = a} :: StartMailboxExportJob)
{-# DEPRECATED smejEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the S3 bucket.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejRoleARN :: Lens.Lens' StartMailboxExportJob Lude.Text
smejRoleARN = Lens.lens (roleARN :: StartMailboxExportJob -> Lude.Text) (\s a -> s {roleARN = a} :: StartMailboxExportJob)
{-# DEPRECATED smejRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejKMSKeyARN :: Lens.Lens' StartMailboxExportJob Lude.Text
smejKMSKeyARN = Lens.lens (kmsKeyARN :: StartMailboxExportJob -> Lude.Text) (\s a -> s {kmsKeyARN = a} :: StartMailboxExportJob)
{-# DEPRECATED smejKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejS3BucketName :: Lens.Lens' StartMailboxExportJob Lude.Text
smejS3BucketName = Lens.lens (s3BucketName :: StartMailboxExportJob -> Lude.Text) (\s a -> s {s3BucketName = a} :: StartMailboxExportJob)
{-# DEPRECATED smejS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The S3 bucket prefix.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejS3Prefix :: Lens.Lens' StartMailboxExportJob Lude.Text
smejS3Prefix = Lens.lens (s3Prefix :: StartMailboxExportJob -> Lude.Text) (\s a -> s {s3Prefix = a} :: StartMailboxExportJob)
{-# DEPRECATED smejS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

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
          [ ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("ClientToken" Lude..= clientToken),
            Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("EntityId" Lude..= entityId),
            Lude.Just ("RoleArn" Lude..= roleARN),
            Lude.Just ("KmsKeyArn" Lude..= kmsKeyARN),
            Lude.Just ("S3BucketName" Lude..= s3BucketName),
            Lude.Just ("S3Prefix" Lude..= s3Prefix)
          ]
      )

instance Lude.ToPath StartMailboxExportJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartMailboxExportJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartMailboxExportJobResponse' smart constructor.
data StartMailboxExportJobResponse = StartMailboxExportJobResponse'
  { jobId ::
      Lude.Maybe Lude.Text,
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
