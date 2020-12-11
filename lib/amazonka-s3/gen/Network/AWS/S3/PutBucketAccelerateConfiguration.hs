{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketAccelerateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the accelerate configuration of an existing bucket. Amazon S3 Transfer Acceleration is a bucket-level feature that enables you to perform faster data transfers to Amazon S3.
--
-- To use this operation, you must have permission to perform the s3:PutAccelerateConfiguration action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- The Transfer Acceleration state of a bucket can be set to one of the following two values:
--
--     * Enabled – Enables accelerated data transfers to the bucket.
--
--
--     * Suspended – Disables accelerated data transfers to the bucket.
--
--
-- The <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAccelerateConfiguration.html GetBucketAccelerateConfiguration> operation returns the transfer acceleration state of a bucket.
-- After setting the Transfer Acceleration state of a bucket to Enabled, it might take up to thirty minutes before the data transfer rates to the bucket increase.
-- The name of the bucket used for Transfer Acceleration must be DNS-compliant and must not contain periods (".").
-- For more information about transfer acceleration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Transfer Acceleration> .
-- The following operations are related to @PutBucketAccelerateConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAccelerateConfiguration.html GetBucketAccelerateConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
module Network.AWS.S3.PutBucketAccelerateConfiguration
  ( -- * Creating a request
    PutBucketAccelerateConfiguration (..),
    mkPutBucketAccelerateConfiguration,

    -- ** Request lenses
    pbacExpectedBucketOwner,
    pbacBucket,
    pbacAccelerateConfiguration,

    -- * Destructuring the response
    PutBucketAccelerateConfigurationResponse (..),
    mkPutBucketAccelerateConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketAccelerateConfiguration' smart constructor.
data PutBucketAccelerateConfiguration = PutBucketAccelerateConfiguration'
  { expectedBucketOwner ::
      Lude.Maybe Lude.Text,
    bucket :: BucketName,
    accelerateConfiguration ::
      AccelerateConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketAccelerateConfiguration' with the minimum fields required to make a request.
--
-- * 'accelerateConfiguration' - Container for setting the transfer acceleration state.
-- * 'bucket' - The name of the bucket for which the accelerate configuration is set.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutBucketAccelerateConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'accelerateConfiguration'
  AccelerateConfiguration ->
  PutBucketAccelerateConfiguration
mkPutBucketAccelerateConfiguration
  pBucket_
  pAccelerateConfiguration_ =
    PutBucketAccelerateConfiguration'
      { expectedBucketOwner =
          Lude.Nothing,
        bucket = pBucket_,
        accelerateConfiguration = pAccelerateConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbacExpectedBucketOwner :: Lens.Lens' PutBucketAccelerateConfiguration (Lude.Maybe Lude.Text)
pbacExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketAccelerateConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketAccelerateConfiguration)
{-# DEPRECATED pbacExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket for which the accelerate configuration is set.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbacBucket :: Lens.Lens' PutBucketAccelerateConfiguration BucketName
pbacBucket = Lens.lens (bucket :: PutBucketAccelerateConfiguration -> BucketName) (\s a -> s {bucket = a} :: PutBucketAccelerateConfiguration)
{-# DEPRECATED pbacBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Container for setting the transfer acceleration state.
--
-- /Note:/ Consider using 'accelerateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbacAccelerateConfiguration :: Lens.Lens' PutBucketAccelerateConfiguration AccelerateConfiguration
pbacAccelerateConfiguration = Lens.lens (accelerateConfiguration :: PutBucketAccelerateConfiguration -> AccelerateConfiguration) (\s a -> s {accelerateConfiguration = a} :: PutBucketAccelerateConfiguration)
{-# DEPRECATED pbacAccelerateConfiguration "Use generic-lens or generic-optics with 'accelerateConfiguration' instead." #-}

instance Lude.AWSRequest PutBucketAccelerateConfiguration where
  type
    Rs PutBucketAccelerateConfiguration =
      PutBucketAccelerateConfigurationResponse
  request = Req.putXML s3Service
  response =
    Res.receiveNull PutBucketAccelerateConfigurationResponse'

instance Lude.ToElement PutBucketAccelerateConfiguration where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AccelerateConfiguration"
      Lude.. accelerateConfiguration

instance Lude.ToHeaders PutBucketAccelerateConfiguration where
  toHeaders PutBucketAccelerateConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath PutBucketAccelerateConfiguration where
  toPath PutBucketAccelerateConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketAccelerateConfiguration where
  toQuery = Lude.const (Lude.mconcat ["accelerate"])

-- | /See:/ 'mkPutBucketAccelerateConfigurationResponse' smart constructor.
data PutBucketAccelerateConfigurationResponse = PutBucketAccelerateConfigurationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketAccelerateConfigurationResponse' with the minimum fields required to make a request.
mkPutBucketAccelerateConfigurationResponse ::
  PutBucketAccelerateConfigurationResponse
mkPutBucketAccelerateConfigurationResponse =
  PutBucketAccelerateConfigurationResponse'
