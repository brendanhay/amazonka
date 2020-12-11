{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @PUT@ operation uses the @encryption@ subresource to set the default encryption state of an existing bucket.
--
-- This implementation of the @PUT@ operation sets default encryption for a bucket using server-side encryption with Amazon S3-managed keys SSE-S3 or AWS KMS customer master keys (CMKs) (SSE-KMS). For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> .
-- /Important:/ This operation requires AWS Signature Version 4. For more information, see <sig-v4-authenticating-requests.html Authenticating Requests (AWS Signature Version 4)> .
-- To use this operation, you must have permissions to perform the @s3:PutEncryptionConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> in the Amazon Simple Storage Service Developer Guide.
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketEncryption.html GetBucketEncryption>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketEncryption.html DeleteBucketEncryption>
module Network.AWS.S3.PutBucketEncryption
  ( -- * Creating a request
    PutBucketEncryption (..),
    mkPutBucketEncryption,

    -- ** Request lenses
    pbeContentMD5,
    pbeExpectedBucketOwner,
    pbeBucket,
    pbeServerSideEncryptionConfiguration,

    -- * Destructuring the response
    PutBucketEncryptionResponse (..),
    mkPutBucketEncryptionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketEncryption' smart constructor.
data PutBucketEncryption = PutBucketEncryption'
  { contentMD5 ::
      Lude.Maybe Lude.Text,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    bucket :: BucketName,
    serverSideEncryptionConfiguration ::
      ServerSideEncryptionConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketEncryption' with the minimum fields required to make a request.
--
-- * 'bucket' - Specifies default encryption for a bucket using server-side encryption with Amazon S3-managed keys (SSE-S3) or customer master keys stored in AWS KMS (SSE-KMS). For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'contentMD5' - The base64-encoded 128-bit MD5 digest of the server-side encryption configuration.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'serverSideEncryptionConfiguration' - Undocumented field.
mkPutBucketEncryption ::
  -- | 'bucket'
  BucketName ->
  -- | 'serverSideEncryptionConfiguration'
  ServerSideEncryptionConfiguration ->
  PutBucketEncryption
mkPutBucketEncryption pBucket_ pServerSideEncryptionConfiguration_ =
  PutBucketEncryption'
    { contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_,
      serverSideEncryptionConfiguration =
        pServerSideEncryptionConfiguration_
    }

-- | The base64-encoded 128-bit MD5 digest of the server-side encryption configuration.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbeContentMD5 :: Lens.Lens' PutBucketEncryption (Lude.Maybe Lude.Text)
pbeContentMD5 = Lens.lens (contentMD5 :: PutBucketEncryption -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutBucketEncryption)
{-# DEPRECATED pbeContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbeExpectedBucketOwner :: Lens.Lens' PutBucketEncryption (Lude.Maybe Lude.Text)
pbeExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketEncryption -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketEncryption)
{-# DEPRECATED pbeExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Specifies default encryption for a bucket using server-side encryption with Amazon S3-managed keys (SSE-S3) or customer master keys stored in AWS KMS (SSE-KMS). For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbeBucket :: Lens.Lens' PutBucketEncryption BucketName
pbeBucket = Lens.lens (bucket :: PutBucketEncryption -> BucketName) (\s a -> s {bucket = a} :: PutBucketEncryption)
{-# DEPRECATED pbeBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'serverSideEncryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbeServerSideEncryptionConfiguration :: Lens.Lens' PutBucketEncryption ServerSideEncryptionConfiguration
pbeServerSideEncryptionConfiguration = Lens.lens (serverSideEncryptionConfiguration :: PutBucketEncryption -> ServerSideEncryptionConfiguration) (\s a -> s {serverSideEncryptionConfiguration = a} :: PutBucketEncryption)
{-# DEPRECATED pbeServerSideEncryptionConfiguration "Use generic-lens or generic-optics with 'serverSideEncryptionConfiguration' instead." #-}

instance Lude.AWSRequest PutBucketEncryption where
  type Rs PutBucketEncryption = PutBucketEncryptionResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutBucketEncryptionResponse'

instance Lude.ToElement PutBucketEncryption where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}ServerSideEncryptionConfiguration"
      Lude.. serverSideEncryptionConfiguration

instance Lude.ToHeaders PutBucketEncryption where
  toHeaders PutBucketEncryption' {..} =
    Lude.mconcat
      [ "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutBucketEncryption where
  toPath PutBucketEncryption' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketEncryption where
  toQuery = Lude.const (Lude.mconcat ["encryption"])

-- | /See:/ 'mkPutBucketEncryptionResponse' smart constructor.
data PutBucketEncryptionResponse = PutBucketEncryptionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketEncryptionResponse' with the minimum fields required to make a request.
mkPutBucketEncryptionResponse ::
  PutBucketEncryptionResponse
mkPutBucketEncryptionResponse = PutBucketEncryptionResponse'
