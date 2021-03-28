{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartMailboxExportJob (..)
    , mkStartMailboxExportJob
    -- ** Request lenses
    , smejClientToken
    , smejOrganizationId
    , smejEntityId
    , smejRoleArn
    , smejKmsKeyArn
    , smejS3BucketName
    , smejS3Prefix
    , smejDescription

    -- * Destructuring the response
    , StartMailboxExportJobResponse (..)
    , mkStartMailboxExportJobResponse
    -- ** Response lenses
    , smejrrsJobId
    , smejrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkStartMailboxExportJob' smart constructor.
data StartMailboxExportJob = StartMailboxExportJob'
  { clientToken :: Types.IdempotencyClientToken
    -- ^ The idempotency token for the client request.
  , organizationId :: Types.OrganizationId
    -- ^ The identifier associated with the organization.
  , entityId :: Types.WorkMailIdentifier
    -- ^ The identifier of the user or resource associated with the mailbox.
  , roleArn :: Types.RoleArn
    -- ^ The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the S3 bucket.
  , kmsKeyArn :: Types.KmsKeyArn
    -- ^ The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
  , s3BucketName :: Types.S3BucketName
    -- ^ The name of the S3 bucket.
  , s3Prefix :: Types.S3ObjectKey
    -- ^ The S3 bucket prefix.
  , description :: Core.Maybe Types.Description
    -- ^ The mailbox export job description.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartMailboxExportJob' value with any optional fields omitted.
mkStartMailboxExportJob
    :: Types.IdempotencyClientToken -- ^ 'clientToken'
    -> Types.OrganizationId -- ^ 'organizationId'
    -> Types.WorkMailIdentifier -- ^ 'entityId'
    -> Types.RoleArn -- ^ 'roleArn'
    -> Types.KmsKeyArn -- ^ 'kmsKeyArn'
    -> Types.S3BucketName -- ^ 's3BucketName'
    -> Types.S3ObjectKey -- ^ 's3Prefix'
    -> StartMailboxExportJob
mkStartMailboxExportJob clientToken organizationId entityId roleArn
  kmsKeyArn s3BucketName s3Prefix
  = StartMailboxExportJob'{clientToken, organizationId, entityId,
                           roleArn, kmsKeyArn, s3BucketName, s3Prefix,
                           description = Core.Nothing}

-- | The idempotency token for the client request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejClientToken :: Lens.Lens' StartMailboxExportJob Types.IdempotencyClientToken
smejClientToken = Lens.field @"clientToken"
{-# INLINEABLE smejClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The identifier associated with the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejOrganizationId :: Lens.Lens' StartMailboxExportJob Types.OrganizationId
smejOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE smejOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The identifier of the user or resource associated with the mailbox.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejEntityId :: Lens.Lens' StartMailboxExportJob Types.WorkMailIdentifier
smejEntityId = Lens.field @"entityId"
{-# INLINEABLE smejEntityId #-}
{-# DEPRECATED entityId "Use generic-lens or generic-optics with 'entityId' instead"  #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the S3 bucket.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejRoleArn :: Lens.Lens' StartMailboxExportJob Types.RoleArn
smejRoleArn = Lens.field @"roleArn"
{-# INLINEABLE smejRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
--
-- /Note:/ Consider using 'kmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejKmsKeyArn :: Lens.Lens' StartMailboxExportJob Types.KmsKeyArn
smejKmsKeyArn = Lens.field @"kmsKeyArn"
{-# INLINEABLE smejKmsKeyArn #-}
{-# DEPRECATED kmsKeyArn "Use generic-lens or generic-optics with 'kmsKeyArn' instead"  #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejS3BucketName :: Lens.Lens' StartMailboxExportJob Types.S3BucketName
smejS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE smejS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | The S3 bucket prefix.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejS3Prefix :: Lens.Lens' StartMailboxExportJob Types.S3ObjectKey
smejS3Prefix = Lens.field @"s3Prefix"
{-# INLINEABLE smejS3Prefix #-}
{-# DEPRECATED s3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead"  #-}

-- | The mailbox export job description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejDescription :: Lens.Lens' StartMailboxExportJob (Core.Maybe Types.Description)
smejDescription = Lens.field @"description"
{-# INLINEABLE smejDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery StartMailboxExportJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartMailboxExportJob where
        toHeaders StartMailboxExportJob{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.StartMailboxExportJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartMailboxExportJob where
        toJSON StartMailboxExportJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClientToken" Core..= clientToken),
                  Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("EntityId" Core..= entityId),
                  Core.Just ("RoleArn" Core..= roleArn),
                  Core.Just ("KmsKeyArn" Core..= kmsKeyArn),
                  Core.Just ("S3BucketName" Core..= s3BucketName),
                  Core.Just ("S3Prefix" Core..= s3Prefix),
                  ("Description" Core..=) Core.<$> description])

instance Core.AWSRequest StartMailboxExportJob where
        type Rs StartMailboxExportJob = StartMailboxExportJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartMailboxExportJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartMailboxExportJobResponse' smart constructor.
data StartMailboxExportJobResponse = StartMailboxExportJobResponse'
  { jobId :: Core.Maybe Types.MailboxExportJobId
    -- ^ The job ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartMailboxExportJobResponse' value with any optional fields omitted.
mkStartMailboxExportJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartMailboxExportJobResponse
mkStartMailboxExportJobResponse responseStatus
  = StartMailboxExportJobResponse'{jobId = Core.Nothing,
                                   responseStatus}

-- | The job ID.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejrrsJobId :: Lens.Lens' StartMailboxExportJobResponse (Core.Maybe Types.MailboxExportJobId)
smejrrsJobId = Lens.field @"jobId"
{-# INLINEABLE smejrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smejrrsResponseStatus :: Lens.Lens' StartMailboxExportJobResponse Core.Int
smejrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE smejrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
