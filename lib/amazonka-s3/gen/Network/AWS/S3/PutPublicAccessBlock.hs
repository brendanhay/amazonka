{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutPublicAccessBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or modifies the @PublicAccessBlock@ configuration for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketPublicAccessBlock@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
-- /Important:/ When Amazon S3 evaluates the @PublicAccessBlock@ configuration for a bucket or an object, it checks the @PublicAccessBlock@ configuration for both the bucket (or the bucket that contains the object) and the bucket owner's account. If the @PublicAccessBlock@ configurations are different between the bucket and the account, Amazon S3 uses the most restrictive combination of the bucket-level and account-level settings.
-- For more information about when Amazon S3 considers a bucket or an object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> .
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketPolicyStatus.html GetBucketPolicyStatus>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
module Network.AWS.S3.PutPublicAccessBlock
  ( -- * Creating a request
    PutPublicAccessBlock (..),
    mkPutPublicAccessBlock,

    -- ** Request lenses
    ppabBucket,
    ppabPublicAccessBlockConfiguration,
    ppabContentMD5,
    ppabExpectedBucketOwner,

    -- * Destructuring the response
    PutPublicAccessBlockResponse (..),
    mkPutPublicAccessBlockResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutPublicAccessBlock' smart constructor.
data PutPublicAccessBlock = PutPublicAccessBlock'
  { -- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to set.
    bucket :: BucketName,
    -- | The @PublicAccessBlock@ configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
    publicAccessBlockConfiguration :: PublicAccessBlockConfiguration,
    -- | The MD5 hash of the @PutPublicAccessBlock@ request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPublicAccessBlock' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to set.
-- * 'publicAccessBlockConfiguration' - The @PublicAccessBlock@ configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'contentMD5' - The MD5 hash of the @PutPublicAccessBlock@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutPublicAccessBlock ::
  -- | 'bucket'
  BucketName ->
  -- | 'publicAccessBlockConfiguration'
  PublicAccessBlockConfiguration ->
  PutPublicAccessBlock
mkPutPublicAccessBlock pBucket_ pPublicAccessBlockConfiguration_ =
  PutPublicAccessBlock'
    { bucket = pBucket_,
      publicAccessBlockConfiguration = pPublicAccessBlockConfiguration_,
      contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to set.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppabBucket :: Lens.Lens' PutPublicAccessBlock BucketName
ppabBucket = Lens.lens (bucket :: PutPublicAccessBlock -> BucketName) (\s a -> s {bucket = a} :: PutPublicAccessBlock)
{-# DEPRECATED ppabBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The @PublicAccessBlock@ configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'publicAccessBlockConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppabPublicAccessBlockConfiguration :: Lens.Lens' PutPublicAccessBlock PublicAccessBlockConfiguration
ppabPublicAccessBlockConfiguration = Lens.lens (publicAccessBlockConfiguration :: PutPublicAccessBlock -> PublicAccessBlockConfiguration) (\s a -> s {publicAccessBlockConfiguration = a} :: PutPublicAccessBlock)
{-# DEPRECATED ppabPublicAccessBlockConfiguration "Use generic-lens or generic-optics with 'publicAccessBlockConfiguration' instead." #-}

-- | The MD5 hash of the @PutPublicAccessBlock@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppabContentMD5 :: Lens.Lens' PutPublicAccessBlock (Lude.Maybe Lude.Text)
ppabContentMD5 = Lens.lens (contentMD5 :: PutPublicAccessBlock -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutPublicAccessBlock)
{-# DEPRECATED ppabContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppabExpectedBucketOwner :: Lens.Lens' PutPublicAccessBlock (Lude.Maybe Lude.Text)
ppabExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutPublicAccessBlock -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutPublicAccessBlock)
{-# DEPRECATED ppabExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutPublicAccessBlock where
  type Rs PutPublicAccessBlock = PutPublicAccessBlockResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutPublicAccessBlockResponse'

instance Lude.ToElement PutPublicAccessBlock where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}PublicAccessBlockConfiguration"
      Lude.. publicAccessBlockConfiguration

instance Lude.ToHeaders PutPublicAccessBlock where
  toHeaders PutPublicAccessBlock' {..} =
    Lude.mconcat
      [ "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutPublicAccessBlock where
  toPath PutPublicAccessBlock' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutPublicAccessBlock where
  toQuery = Lude.const (Lude.mconcat ["publicAccessBlock"])

-- | /See:/ 'mkPutPublicAccessBlockResponse' smart constructor.
data PutPublicAccessBlockResponse = PutPublicAccessBlockResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPublicAccessBlockResponse' with the minimum fields required to make a request.
mkPutPublicAccessBlockResponse ::
  PutPublicAccessBlockResponse
mkPutPublicAccessBlockResponse = PutPublicAccessBlockResponse'
