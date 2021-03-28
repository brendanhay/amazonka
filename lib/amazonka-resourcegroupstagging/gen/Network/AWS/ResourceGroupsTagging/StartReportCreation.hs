{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.StartReportCreation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a report that lists all tagged resources in accounts across your organization and tells whether each resource is compliant with the effective tag policy. Compliance data is refreshed daily. 
--
-- The generated report is saved to the following location:
-- @s3://example-bucket/AwsTagPolicies/o-exampleorgid/YYYY-MM-ddTHH:mm:ssZ/report.csv@ 
-- You can call this operation only from the organization's master account and from the us-east-1 Region.
module Network.AWS.ResourceGroupsTagging.StartReportCreation
    (
    -- * Creating a request
      StartReportCreation (..)
    , mkStartReportCreation
    -- ** Request lenses
    , srcS3Bucket

    -- * Destructuring the response
    , StartReportCreationResponse (..)
    , mkStartReportCreationResponse
    -- ** Response lenses
    , srcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroupsTagging.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartReportCreation' smart constructor.
newtype StartReportCreation = StartReportCreation'
  { s3Bucket :: Types.S3Bucket
    -- ^ The name of the Amazon S3 bucket where the report will be stored; for example:
--
-- @awsexamplebucket@ 
-- For more information on S3 bucket requirements, including an example bucket policy, see the example S3 bucket policy on this page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartReportCreation' value with any optional fields omitted.
mkStartReportCreation
    :: Types.S3Bucket -- ^ 's3Bucket'
    -> StartReportCreation
mkStartReportCreation s3Bucket = StartReportCreation'{s3Bucket}

-- | The name of the Amazon S3 bucket where the report will be stored; for example:
--
-- @awsexamplebucket@ 
-- For more information on S3 bucket requirements, including an example bucket policy, see the example S3 bucket policy on this page.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcS3Bucket :: Lens.Lens' StartReportCreation Types.S3Bucket
srcS3Bucket = Lens.field @"s3Bucket"
{-# INLINEABLE srcS3Bucket #-}
{-# DEPRECATED s3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead"  #-}

instance Core.ToQuery StartReportCreation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartReportCreation where
        toHeaders StartReportCreation{..}
          = Core.pure
              ("X-Amz-Target",
               "ResourceGroupsTaggingAPI_20170126.StartReportCreation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartReportCreation where
        toJSON StartReportCreation{..}
          = Core.object
              (Core.catMaybes [Core.Just ("S3Bucket" Core..= s3Bucket)])

instance Core.AWSRequest StartReportCreation where
        type Rs StartReportCreation = StartReportCreationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StartReportCreationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartReportCreationResponse' smart constructor.
newtype StartReportCreationResponse = StartReportCreationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartReportCreationResponse' value with any optional fields omitted.
mkStartReportCreationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartReportCreationResponse
mkStartReportCreationResponse responseStatus
  = StartReportCreationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrrsResponseStatus :: Lens.Lens' StartReportCreationResponse Core.Int
srcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
