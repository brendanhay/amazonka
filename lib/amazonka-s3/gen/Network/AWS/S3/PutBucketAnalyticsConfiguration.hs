{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.S3.PutBucketAnalyticsConfiguration
  ( -- * Creating a request
    PutBucketAnalyticsConfiguration (..),
    mkPutBucketAnalyticsConfiguration,

    -- ** Request lenses
    pExpectedBucketOwner,
    pBucket,
    pId,
    pAnalyticsConfiguration,

    -- * Destructuring the response
    PutBucketAnalyticsConfigurationResponse (..),
    mkPutBucketAnalyticsConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketAnalyticsConfiguration' smart constructor.
data PutBucketAnalyticsConfiguration = PutBucketAnalyticsConfiguration'
  { expectedBucketOwner ::
      Lude.Maybe Lude.Text,
    bucket :: BucketName,
    id :: Lude.Text,
    analyticsConfiguration ::
      AnalyticsConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketAnalyticsConfiguration' with the minimum fields required to make a request.
--
-- * 'analyticsConfiguration' - The configuration and any analyses for the analytics filter.
-- * 'bucket' - The name of the bucket to which an analytics configuration is stored.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'id' - The ID that identifies the analytics configuration.
mkPutBucketAnalyticsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Lude.Text ->
  -- | 'analyticsConfiguration'
  AnalyticsConfiguration ->
  PutBucketAnalyticsConfiguration
mkPutBucketAnalyticsConfiguration
  pBucket_
  pId_
  pAnalyticsConfiguration_ =
    PutBucketAnalyticsConfiguration'
      { expectedBucketOwner =
          Lude.Nothing,
        bucket = pBucket_,
        id = pId_,
        analyticsConfiguration = pAnalyticsConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pExpectedBucketOwner :: Lens.Lens' PutBucketAnalyticsConfiguration (Lude.Maybe Lude.Text)
pExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketAnalyticsConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketAnalyticsConfiguration)
{-# DEPRECATED pExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket to which an analytics configuration is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBucket :: Lens.Lens' PutBucketAnalyticsConfiguration BucketName
pBucket = Lens.lens (bucket :: PutBucketAnalyticsConfiguration -> BucketName) (\s a -> s {bucket = a} :: PutBucketAnalyticsConfiguration)
{-# DEPRECATED pBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID that identifies the analytics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pId :: Lens.Lens' PutBucketAnalyticsConfiguration Lude.Text
pId = Lens.lens (id :: PutBucketAnalyticsConfiguration -> Lude.Text) (\s a -> s {id = a} :: PutBucketAnalyticsConfiguration)
{-# DEPRECATED pId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The configuration and any analyses for the analytics filter.
--
-- /Note:/ Consider using 'analyticsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAnalyticsConfiguration :: Lens.Lens' PutBucketAnalyticsConfiguration AnalyticsConfiguration
pAnalyticsConfiguration = Lens.lens (analyticsConfiguration :: PutBucketAnalyticsConfiguration -> AnalyticsConfiguration) (\s a -> s {analyticsConfiguration = a} :: PutBucketAnalyticsConfiguration)
{-# DEPRECATED pAnalyticsConfiguration "Use generic-lens or generic-optics with 'analyticsConfiguration' instead." #-}

instance Lude.AWSRequest PutBucketAnalyticsConfiguration where
  type
    Rs PutBucketAnalyticsConfiguration =
      PutBucketAnalyticsConfigurationResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutBucketAnalyticsConfigurationResponse'

instance Lude.ToElement PutBucketAnalyticsConfiguration where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AnalyticsConfiguration"
      Lude.. analyticsConfiguration

instance Lude.ToHeaders PutBucketAnalyticsConfiguration where
  toHeaders PutBucketAnalyticsConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath PutBucketAnalyticsConfiguration where
  toPath PutBucketAnalyticsConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketAnalyticsConfiguration where
  toQuery PutBucketAnalyticsConfiguration' {..} =
    Lude.mconcat ["id" Lude.=: id, "analytics"]

-- | /See:/ 'mkPutBucketAnalyticsConfigurationResponse' smart constructor.
data PutBucketAnalyticsConfigurationResponse = PutBucketAnalyticsConfigurationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketAnalyticsConfigurationResponse' with the minimum fields required to make a request.
mkPutBucketAnalyticsConfigurationResponse ::
  PutBucketAnalyticsConfigurationResponse
mkPutBucketAnalyticsConfigurationResponse =
  PutBucketAnalyticsConfigurationResponse'
