{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketLifecycleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new lifecycle configuration for the bucket or replaces an existing lifecycle configuration. For information about lifecycle configuration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- __Rules__ 
-- You specify the lifecycle configuration in your request body. The lifecycle configuration is specified as XML consisting of one or more rules. Each rule consists of the following:
--
--     * Filter identifying a subset of objects to which the rule applies. The filter can be based on a key name prefix, object tags, or a combination of both.
--
--
--     * Status whether the rule is in effect.
--
--
--     * One or more lifecycle transition and expiration actions that you want Amazon S3 to perform on the objects identified by the filter. If the state of your bucket is versioning-enabled or versioning-suspended, you can have many versions of the same object (one current version and zero or more noncurrent versions). Amazon S3 provides predefined actions that you can specify for current and noncurrent object versions.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html Lifecycle Configuration Elements> .
-- __Permissions__ 
-- By default, all Amazon S3 resources are private, including buckets, objects, and related subresources (for example, lifecycle configuration and website configuration). Only the resource owner (that is, the AWS account that created it) can access the resource. The resource owner can optionally grant access permissions to others by writing an access policy. For this operation, a user must get the s3:PutLifecycleConfiguration permission.
-- You can also explicitly deny permissions. Explicit deny also supersedes any other permissions. If you want to block users or accounts from removing or deleting objects from your bucket, you must deny them permissions for the following actions:
--
--     * s3:DeleteObject
--
--
--     * s3:DeleteObjectVersion
--
--
--     * s3:PutLifecycleConfiguration
--
--
-- For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- The following are related to @PutBucketLifecycleConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/lifecycle-configuration-examples.html Examples of Lifecycle Configuration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycleConfiguration.html GetBucketLifecycleConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketLifecycle.html DeleteBucketLifecycle> 
--
--
module Network.AWS.S3.PutBucketLifecycleConfiguration
    (
    -- * Creating a request
      PutBucketLifecycleConfiguration (..)
    , mkPutBucketLifecycleConfiguration
    -- ** Request lenses
    , pblcBucket
    , pblcExpectedBucketOwner
    , pblcLifecycleConfiguration

    -- * Destructuring the response
    , PutBucketLifecycleConfigurationResponse (..)
    , mkPutBucketLifecycleConfigurationResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketLifecycleConfiguration' smart constructor.
data PutBucketLifecycleConfiguration = PutBucketLifecycleConfiguration'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket for which to set the configuration.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , lifecycleConfiguration :: Core.Maybe Types.BucketLifecycleConfiguration
    -- ^ Container for lifecycle rules. You can add as many as 1,000 rules.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutBucketLifecycleConfiguration' value with any optional fields omitted.
mkPutBucketLifecycleConfiguration
    :: Types.BucketName -- ^ 'bucket'
    -> PutBucketLifecycleConfiguration
mkPutBucketLifecycleConfiguration bucket
  = PutBucketLifecycleConfiguration'{bucket,
                                     expectedBucketOwner = Core.Nothing,
                                     lifecycleConfiguration = Core.Nothing}

-- | The name of the bucket for which to set the configuration.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblcBucket :: Lens.Lens' PutBucketLifecycleConfiguration Types.BucketName
pblcBucket = Lens.field @"bucket"
{-# INLINEABLE pblcBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblcExpectedBucketOwner :: Lens.Lens' PutBucketLifecycleConfiguration (Core.Maybe Types.ExpectedBucketOwner)
pblcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE pblcExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | Container for lifecycle rules. You can add as many as 1,000 rules.
--
-- /Note:/ Consider using 'lifecycleConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblcLifecycleConfiguration :: Lens.Lens' PutBucketLifecycleConfiguration (Core.Maybe Types.BucketLifecycleConfiguration)
pblcLifecycleConfiguration = Lens.field @"lifecycleConfiguration"
{-# INLINEABLE pblcLifecycleConfiguration #-}
{-# DEPRECATED lifecycleConfiguration "Use generic-lens or generic-optics with 'lifecycleConfiguration' instead"  #-}

instance Core.ToQuery PutBucketLifecycleConfiguration where
        toQuery PutBucketLifecycleConfiguration{..}
          = Core.toQueryPair "lifecycle" ("" :: Core.Text)

instance Core.ToHeaders PutBucketLifecycleConfiguration where
        toHeaders PutBucketLifecycleConfiguration{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest PutBucketLifecycleConfiguration where
        type Rs PutBucketLifecycleConfiguration =
             PutBucketLifecycleConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull PutBucketLifecycleConfigurationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBucketLifecycleConfigurationResponse' smart constructor.
data PutBucketLifecycleConfigurationResponse = PutBucketLifecycleConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketLifecycleConfigurationResponse' value with any optional fields omitted.
mkPutBucketLifecycleConfigurationResponse
    :: PutBucketLifecycleConfigurationResponse
mkPutBucketLifecycleConfigurationResponse
  = PutBucketLifecycleConfigurationResponse'
