{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketNotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the notification configuration of a bucket.
--
-- If notifications are not enabled on the bucket, the operation returns an empty @NotificationConfiguration@ element.
-- By default, you must be the bucket owner to read the notification configuration of a bucket. However, the bucket owner can use a bucket policy to grant permission to other users to read this configuration with the @s3:GetBucketNotification@ permission.
-- For more information about setting and reading the notification configuration on a bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Setting Up Notification of Bucket Events> . For more information about bucket policies, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies> .
-- The following operation is related to @GetBucketNotification@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketNotification.html PutBucketNotification>
module Network.AWS.S3.GetBucketNotificationConfiguration
  ( -- * Creating a request
    GetBucketNotificationConfiguration (..),
    mkGetBucketNotificationConfiguration,

    -- ** Request lenses
    gbncBucket,
    gbncExpectedBucketOwner,

    -- * Destructuring the response
    NotificationConfiguration (..),
    mkNotificationConfiguration,

    -- ** Response lenses
    ncQueueConfigurations,
    ncTopicConfigurations,
    ncLambdaFunctionConfigurations,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketNotificationConfiguration' smart constructor.
data GetBucketNotificationConfiguration = GetBucketNotificationConfiguration'
  { -- | The name of the bucket for which to get the notification configuration.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketNotificationConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket for which to get the notification configuration.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketNotificationConfiguration ::
  -- | 'bucket'
  BucketName ->
  GetBucketNotificationConfiguration
mkGetBucketNotificationConfiguration pBucket_ =
  GetBucketNotificationConfiguration'
    { bucket = pBucket_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the bucket for which to get the notification configuration.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbncBucket :: Lens.Lens' GetBucketNotificationConfiguration BucketName
gbncBucket = Lens.lens (bucket :: GetBucketNotificationConfiguration -> BucketName) (\s a -> s {bucket = a} :: GetBucketNotificationConfiguration)
{-# DEPRECATED gbncBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbncExpectedBucketOwner :: Lens.Lens' GetBucketNotificationConfiguration (Lude.Maybe Lude.Text)
gbncExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketNotificationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketNotificationConfiguration)
{-# DEPRECATED gbncExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest GetBucketNotificationConfiguration where
  type
    Rs GetBucketNotificationConfiguration =
      NotificationConfiguration
  request = Req.get s3Service
  response = Res.receiveXML (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders GetBucketNotificationConfiguration where
  toHeaders GetBucketNotificationConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketNotificationConfiguration where
  toPath GetBucketNotificationConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketNotificationConfiguration where
  toQuery = Lude.const (Lude.mconcat ["notification"])
