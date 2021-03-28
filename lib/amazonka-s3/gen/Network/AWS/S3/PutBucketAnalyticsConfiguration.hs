{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets an analytics configuration for the bucket (specified by the analytics configuration ID). You can have up to 1,000 analytics configurations per bucket.
--
-- You can choose to have storage class analysis export analysis reports sent to a comma-separated values (CSV) flat file. See the @DataExport@ request element. Reports are updated daily and are based on the object filters that you configure. When selecting data export, you specify a destination bucket and an optional destination prefix where the file is written. You can export the data to a destination bucket in a different account. However, the destination bucket must be in the same Region as the bucket that you are making the PUT analytics configuration to. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis> . 
-- /Important:/ You must create a bucket policy on the destination bucket where the exported file is written to grant permissions to Amazon S3 to write objects to the bucket. For an example policy, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/example-bucket-policies.html#example-bucket-policies-use-case-9 Granting Permissions for Amazon S3 Inventory and Storage Class Analysis> .
-- To use this operation, you must have permissions to perform the @s3:PutAnalyticsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- __Special Errors__ 
--
--     * 
--     * /HTTP Error: HTTP 400 Bad Request/ 
--
--
--     * /Code: InvalidArgument/ 
--
--
--     * /Cause: Invalid argument./ 
--
--
--
--
--     * 
--     * /HTTP Error: HTTP 400 Bad Request/ 
--
--
--     * /Code: TooManyConfigurations/ 
--
--
--     * /Cause: You are attempting to create a new configuration but have already reached the 1,000-configuration limit./ 
--
--
--
--
--     * 
--     * /HTTP Error: HTTP 403 Forbidden/ 
--
--
--     * /Code: AccessDenied/ 
--
--
--     * /Cause: You are not the owner of the specified bucket, or you do not have the s3:PutAnalyticsConfiguration bucket permission to set the configuration on the bucket./ 
--
--
--
--
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAnalyticsConfiguration.html GetBucketAnalyticsConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketAnalyticsConfiguration.html DeleteBucketAnalyticsConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations> 
--
--
module Network.AWS.S3.PutBucketAnalyticsConfiguration
    (
    -- * Creating a request
      PutBucketAnalyticsConfiguration (..)
    , mkPutBucketAnalyticsConfiguration
    -- ** Request lenses
    , pBucket
    , pId
    , pAnalyticsConfiguration
    , pExpectedBucketOwner

    -- * Destructuring the response
    , PutBucketAnalyticsConfigurationResponse (..)
    , mkPutBucketAnalyticsConfigurationResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketAnalyticsConfiguration' smart constructor.
data PutBucketAnalyticsConfiguration = PutBucketAnalyticsConfiguration'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket to which an analytics configuration is stored.
  , id :: Types.Id
    -- ^ The ID that identifies the analytics configuration.
  , analyticsConfiguration :: Types.AnalyticsConfiguration
    -- ^ The configuration and any analyses for the analytics filter.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketAnalyticsConfiguration' value with any optional fields omitted.
mkPutBucketAnalyticsConfiguration
    :: Types.BucketName -- ^ 'bucket'
    -> Types.Id -- ^ 'id'
    -> Types.AnalyticsConfiguration -- ^ 'analyticsConfiguration'
    -> PutBucketAnalyticsConfiguration
mkPutBucketAnalyticsConfiguration bucket id analyticsConfiguration
  = PutBucketAnalyticsConfiguration'{bucket, id,
                                     analyticsConfiguration, expectedBucketOwner = Core.Nothing}

-- | The name of the bucket to which an analytics configuration is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBucket :: Lens.Lens' PutBucketAnalyticsConfiguration Types.BucketName
pBucket = Lens.field @"bucket"
{-# INLINEABLE pBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The ID that identifies the analytics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pId :: Lens.Lens' PutBucketAnalyticsConfiguration Types.Id
pId = Lens.field @"id"
{-# INLINEABLE pId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The configuration and any analyses for the analytics filter.
--
-- /Note:/ Consider using 'analyticsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAnalyticsConfiguration :: Lens.Lens' PutBucketAnalyticsConfiguration Types.AnalyticsConfiguration
pAnalyticsConfiguration = Lens.field @"analyticsConfiguration"
{-# INLINEABLE pAnalyticsConfiguration #-}
{-# DEPRECATED analyticsConfiguration "Use generic-lens or generic-optics with 'analyticsConfiguration' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pExpectedBucketOwner :: Lens.Lens' PutBucketAnalyticsConfiguration (Core.Maybe Types.ExpectedBucketOwner)
pExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE pExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery PutBucketAnalyticsConfiguration where
        toQuery PutBucketAnalyticsConfiguration{..}
          = Core.toQueryPair "id" id Core.<>
              Core.toQueryPair "analytics" ("" :: Core.Text)

instance Core.ToHeaders PutBucketAnalyticsConfiguration where
        toHeaders PutBucketAnalyticsConfiguration{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest PutBucketAnalyticsConfiguration where
        type Rs PutBucketAnalyticsConfiguration =
             PutBucketAnalyticsConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull PutBucketAnalyticsConfigurationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBucketAnalyticsConfigurationResponse' smart constructor.
data PutBucketAnalyticsConfigurationResponse = PutBucketAnalyticsConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketAnalyticsConfigurationResponse' value with any optional fields omitted.
mkPutBucketAnalyticsConfigurationResponse
    :: PutBucketAnalyticsConfigurationResponse
mkPutBucketAnalyticsConfigurationResponse
  = PutBucketAnalyticsConfigurationResponse'
