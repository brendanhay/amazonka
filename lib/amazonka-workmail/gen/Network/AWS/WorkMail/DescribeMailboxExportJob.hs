{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeMailboxExportJob (..)
    , mkDescribeMailboxExportJob
    -- ** Request lenses
    , dmejJobId
    , dmejOrganizationId

    -- * Destructuring the response
    , DescribeMailboxExportJobResponse (..)
    , mkDescribeMailboxExportJobResponse
    -- ** Response lenses
    , dmejrrsDescription
    , dmejrrsEndTime
    , dmejrrsEntityId
    , dmejrrsErrorInfo
    , dmejrrsEstimatedProgress
    , dmejrrsKmsKeyArn
    , dmejrrsRoleArn
    , dmejrrsS3BucketName
    , dmejrrsS3Path
    , dmejrrsS3Prefix
    , dmejrrsStartTime
    , dmejrrsState
    , dmejrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDescribeMailboxExportJob' smart constructor.
data DescribeMailboxExportJob = DescribeMailboxExportJob'
  { jobId :: Types.MailboxExportJobId
    -- ^ The mailbox export job ID.
  , organizationId :: Types.OrganizationId
    -- ^ The organization ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMailboxExportJob' value with any optional fields omitted.
mkDescribeMailboxExportJob
    :: Types.MailboxExportJobId -- ^ 'jobId'
    -> Types.OrganizationId -- ^ 'organizationId'
    -> DescribeMailboxExportJob
mkDescribeMailboxExportJob jobId organizationId
  = DescribeMailboxExportJob'{jobId, organizationId}

-- | The mailbox export job ID.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejJobId :: Lens.Lens' DescribeMailboxExportJob Types.MailboxExportJobId
dmejJobId = Lens.field @"jobId"
{-# INLINEABLE dmejJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejOrganizationId :: Lens.Lens' DescribeMailboxExportJob Types.OrganizationId
dmejOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE dmejOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

instance Core.ToQuery DescribeMailboxExportJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeMailboxExportJob where
        toHeaders DescribeMailboxExportJob{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.DescribeMailboxExportJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeMailboxExportJob where
        toJSON DescribeMailboxExportJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobId" Core..= jobId),
                  Core.Just ("OrganizationId" Core..= organizationId)])

instance Core.AWSRequest DescribeMailboxExportJob where
        type Rs DescribeMailboxExportJob = DescribeMailboxExportJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeMailboxExportJobResponse' Core.<$>
                   (x Core..:? "Description") Core.<*> x Core..:? "EndTime" Core.<*>
                     x Core..:? "EntityId"
                     Core.<*> x Core..:? "ErrorInfo"
                     Core.<*> x Core..:? "EstimatedProgress"
                     Core.<*> x Core..:? "KmsKeyArn"
                     Core.<*> x Core..:? "RoleArn"
                     Core.<*> x Core..:? "S3BucketName"
                     Core.<*> x Core..:? "S3Path"
                     Core.<*> x Core..:? "S3Prefix"
                     Core.<*> x Core..:? "StartTime"
                     Core.<*> x Core..:? "State"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeMailboxExportJobResponse' smart constructor.
data DescribeMailboxExportJobResponse = DescribeMailboxExportJobResponse'
  { description :: Core.Maybe Types.Description
    -- ^ The mailbox export job description.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The mailbox export job end timestamp.
  , entityId :: Core.Maybe Types.WorkMailIdentifier
    -- ^ The identifier of the user or resource associated with the mailbox.
  , errorInfo :: Core.Maybe Types.ErrorInfo
    -- ^ Error information for failed mailbox export jobs.
  , estimatedProgress :: Core.Maybe Core.Natural
    -- ^ The estimated progress of the mailbox export job, in percentage points.
  , kmsKeyArn :: Core.Maybe Types.KmsKeyArn
    -- ^ The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the Amazon Simple Storage Service (Amazon S3) bucket.
  , s3BucketName :: Core.Maybe Types.S3BucketName
    -- ^ The name of the S3 bucket.
  , s3Path :: Core.Maybe Types.S3ObjectKey
    -- ^ The path to the S3 bucket and file that the mailbox export job is exporting to.
  , s3Prefix :: Core.Maybe Types.S3ObjectKey
    -- ^ The S3 bucket prefix.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The mailbox export job start timestamp.
  , state :: Core.Maybe Types.MailboxExportJobState
    -- ^ The state of the mailbox export job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeMailboxExportJobResponse' value with any optional fields omitted.
mkDescribeMailboxExportJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeMailboxExportJobResponse
mkDescribeMailboxExportJobResponse responseStatus
  = DescribeMailboxExportJobResponse'{description = Core.Nothing,
                                      endTime = Core.Nothing, entityId = Core.Nothing,
                                      errorInfo = Core.Nothing, estimatedProgress = Core.Nothing,
                                      kmsKeyArn = Core.Nothing, roleArn = Core.Nothing,
                                      s3BucketName = Core.Nothing, s3Path = Core.Nothing,
                                      s3Prefix = Core.Nothing, startTime = Core.Nothing,
                                      state = Core.Nothing, responseStatus}

-- | The mailbox export job description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsDescription :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Types.Description)
dmejrrsDescription = Lens.field @"description"
{-# INLINEABLE dmejrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The mailbox export job end timestamp.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsEndTime :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.NominalDiffTime)
dmejrrsEndTime = Lens.field @"endTime"
{-# INLINEABLE dmejrrsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The identifier of the user or resource associated with the mailbox.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsEntityId :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Types.WorkMailIdentifier)
dmejrrsEntityId = Lens.field @"entityId"
{-# INLINEABLE dmejrrsEntityId #-}
{-# DEPRECATED entityId "Use generic-lens or generic-optics with 'entityId' instead"  #-}

-- | Error information for failed mailbox export jobs.
--
-- /Note:/ Consider using 'errorInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsErrorInfo :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Types.ErrorInfo)
dmejrrsErrorInfo = Lens.field @"errorInfo"
{-# INLINEABLE dmejrrsErrorInfo #-}
{-# DEPRECATED errorInfo "Use generic-lens or generic-optics with 'errorInfo' instead"  #-}

-- | The estimated progress of the mailbox export job, in percentage points.
--
-- /Note:/ Consider using 'estimatedProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsEstimatedProgress :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.Natural)
dmejrrsEstimatedProgress = Lens.field @"estimatedProgress"
{-# INLINEABLE dmejrrsEstimatedProgress #-}
{-# DEPRECATED estimatedProgress "Use generic-lens or generic-optics with 'estimatedProgress' instead"  #-}

-- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management Service (AWS KMS) key that encrypts the exported mailbox content.
--
-- /Note:/ Consider using 'kmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsKmsKeyArn :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Types.KmsKeyArn)
dmejrrsKmsKeyArn = Lens.field @"kmsKeyArn"
{-# INLINEABLE dmejrrsKmsKeyArn #-}
{-# DEPRECATED kmsKeyArn "Use generic-lens or generic-optics with 'kmsKeyArn' instead"  #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that grants write permission to the Amazon Simple Storage Service (Amazon S3) bucket.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsRoleArn :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Types.RoleArn)
dmejrrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dmejrrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsS3BucketName :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Types.S3BucketName)
dmejrrsS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE dmejrrsS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | The path to the S3 bucket and file that the mailbox export job is exporting to.
--
-- /Note:/ Consider using 's3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsS3Path :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Types.S3ObjectKey)
dmejrrsS3Path = Lens.field @"s3Path"
{-# INLINEABLE dmejrrsS3Path #-}
{-# DEPRECATED s3Path "Use generic-lens or generic-optics with 's3Path' instead"  #-}

-- | The S3 bucket prefix.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsS3Prefix :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Types.S3ObjectKey)
dmejrrsS3Prefix = Lens.field @"s3Prefix"
{-# INLINEABLE dmejrrsS3Prefix #-}
{-# DEPRECATED s3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead"  #-}

-- | The mailbox export job start timestamp.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsStartTime :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.NominalDiffTime)
dmejrrsStartTime = Lens.field @"startTime"
{-# INLINEABLE dmejrrsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The state of the mailbox export job.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsState :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Types.MailboxExportJobState)
dmejrrsState = Lens.field @"state"
{-# INLINEABLE dmejrrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmejrrsResponseStatus :: Lens.Lens' DescribeMailboxExportJobResponse Core.Int
dmejrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmejrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
