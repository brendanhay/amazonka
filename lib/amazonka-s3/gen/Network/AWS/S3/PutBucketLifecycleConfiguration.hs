{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.S3.PutBucketLifecycleConfiguration
  ( -- * Creating a request
    PutBucketLifecycleConfiguration (..),
    mkPutBucketLifecycleConfiguration,

    -- ** Request lenses
    pblcBucket,
    pblcLifecycleConfiguration,
    pblcExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketLifecycleConfigurationResponse (..),
    mkPutBucketLifecycleConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketLifecycleConfiguration' smart constructor.
data PutBucketLifecycleConfiguration = PutBucketLifecycleConfiguration'
  { -- | The name of the bucket for which to set the configuration.
    bucket :: BucketName,
    -- | Container for lifecycle rules. You can add as many as 1,000 rules.
    lifecycleConfiguration :: Lude.Maybe BucketLifecycleConfiguration,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketLifecycleConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket for which to set the configuration.
-- * 'lifecycleConfiguration' - Container for lifecycle rules. You can add as many as 1,000 rules.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutBucketLifecycleConfiguration ::
  -- | 'bucket'
  BucketName ->
  PutBucketLifecycleConfiguration
mkPutBucketLifecycleConfiguration pBucket_ =
  PutBucketLifecycleConfiguration'
    { bucket = pBucket_,
      lifecycleConfiguration = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the bucket for which to set the configuration.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblcBucket :: Lens.Lens' PutBucketLifecycleConfiguration BucketName
pblcBucket = Lens.lens (bucket :: PutBucketLifecycleConfiguration -> BucketName) (\s a -> s {bucket = a} :: PutBucketLifecycleConfiguration)
{-# DEPRECATED pblcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Container for lifecycle rules. You can add as many as 1,000 rules.
--
-- /Note:/ Consider using 'lifecycleConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblcLifecycleConfiguration :: Lens.Lens' PutBucketLifecycleConfiguration (Lude.Maybe BucketLifecycleConfiguration)
pblcLifecycleConfiguration = Lens.lens (lifecycleConfiguration :: PutBucketLifecycleConfiguration -> Lude.Maybe BucketLifecycleConfiguration) (\s a -> s {lifecycleConfiguration = a} :: PutBucketLifecycleConfiguration)
{-# DEPRECATED pblcLifecycleConfiguration "Use generic-lens or generic-optics with 'lifecycleConfiguration' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblcExpectedBucketOwner :: Lens.Lens' PutBucketLifecycleConfiguration (Lude.Maybe Lude.Text)
pblcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketLifecycleConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketLifecycleConfiguration)
{-# DEPRECATED pblcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutBucketLifecycleConfiguration where
  type
    Rs PutBucketLifecycleConfiguration =
      PutBucketLifecycleConfigurationResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutBucketLifecycleConfigurationResponse'

instance Lude.ToElement PutBucketLifecycleConfiguration where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}LifecycleConfiguration"
      Lude.. lifecycleConfiguration

instance Lude.ToHeaders PutBucketLifecycleConfiguration where
  toHeaders PutBucketLifecycleConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath PutBucketLifecycleConfiguration where
  toPath PutBucketLifecycleConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketLifecycleConfiguration where
  toQuery = Lude.const (Lude.mconcat ["lifecycle"])

-- | /See:/ 'mkPutBucketLifecycleConfigurationResponse' smart constructor.
data PutBucketLifecycleConfigurationResponse = PutBucketLifecycleConfigurationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketLifecycleConfigurationResponse' with the minimum fields required to make a request.
mkPutBucketLifecycleConfigurationResponse ::
  PutBucketLifecycleConfigurationResponse
mkPutBucketLifecycleConfigurationResponse =
  PutBucketLifecycleConfigurationResponse'
