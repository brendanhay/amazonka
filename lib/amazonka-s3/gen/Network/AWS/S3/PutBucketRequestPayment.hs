{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketRequestPayment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the request payment configuration for a bucket. By default, the bucket owner pays for downloads from the bucket. This configuration parameter enables the bucket owner (only) to specify that the person requesting the download will be charged for the download. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> .
--
-- The following operations are related to @PutBucketRequestPayment@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketRequestPayment.html GetBucketRequestPayment>
module Network.AWS.S3.PutBucketRequestPayment
  ( -- * Creating a request
    PutBucketRequestPayment (..),
    mkPutBucketRequestPayment,

    -- ** Request lenses
    pbrpRequestPaymentConfiguration,
    pbrpBucket,
    pbrpContentMD5,
    pbrpExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketRequestPaymentResponse (..),
    mkPutBucketRequestPaymentResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketRequestPayment' smart constructor.
data PutBucketRequestPayment = PutBucketRequestPayment'
  { -- | Container for Payer.
    requestPaymentConfiguration :: RequestPaymentConfiguration,
    -- | The bucket name.
    bucket :: BucketName,
    -- | >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketRequestPayment' with the minimum fields required to make a request.
--
-- * 'requestPaymentConfiguration' - Container for Payer.
-- * 'bucket' - The bucket name.
-- * 'contentMD5' - >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutBucketRequestPayment ::
  -- | 'requestPaymentConfiguration'
  RequestPaymentConfiguration ->
  -- | 'bucket'
  BucketName ->
  PutBucketRequestPayment
mkPutBucketRequestPayment pRequestPaymentConfiguration_ pBucket_ =
  PutBucketRequestPayment'
    { requestPaymentConfiguration =
        pRequestPaymentConfiguration_,
      bucket = pBucket_,
      contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | Container for Payer.
--
-- /Note:/ Consider using 'requestPaymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrpRequestPaymentConfiguration :: Lens.Lens' PutBucketRequestPayment RequestPaymentConfiguration
pbrpRequestPaymentConfiguration = Lens.lens (requestPaymentConfiguration :: PutBucketRequestPayment -> RequestPaymentConfiguration) (\s a -> s {requestPaymentConfiguration = a} :: PutBucketRequestPayment)
{-# DEPRECATED pbrpRequestPaymentConfiguration "Use generic-lens or generic-optics with 'requestPaymentConfiguration' instead." #-}

-- | The bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrpBucket :: Lens.Lens' PutBucketRequestPayment BucketName
pbrpBucket = Lens.lens (bucket :: PutBucketRequestPayment -> BucketName) (\s a -> s {bucket = a} :: PutBucketRequestPayment)
{-# DEPRECATED pbrpBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrpContentMD5 :: Lens.Lens' PutBucketRequestPayment (Lude.Maybe Lude.Text)
pbrpContentMD5 = Lens.lens (contentMD5 :: PutBucketRequestPayment -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutBucketRequestPayment)
{-# DEPRECATED pbrpContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrpExpectedBucketOwner :: Lens.Lens' PutBucketRequestPayment (Lude.Maybe Lude.Text)
pbrpExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketRequestPayment -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketRequestPayment)
{-# DEPRECATED pbrpExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutBucketRequestPayment where
  type Rs PutBucketRequestPayment = PutBucketRequestPaymentResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutBucketRequestPaymentResponse'

instance Lude.ToElement PutBucketRequestPayment where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}RequestPaymentConfiguration"
      Lude.. requestPaymentConfiguration

instance Lude.ToHeaders PutBucketRequestPayment where
  toHeaders PutBucketRequestPayment' {..} =
    Lude.mconcat
      [ "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutBucketRequestPayment where
  toPath PutBucketRequestPayment' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketRequestPayment where
  toQuery = Lude.const (Lude.mconcat ["requestPayment"])

-- | /See:/ 'mkPutBucketRequestPaymentResponse' smart constructor.
data PutBucketRequestPaymentResponse = PutBucketRequestPaymentResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketRequestPaymentResponse' with the minimum fields required to make a request.
mkPutBucketRequestPaymentResponse ::
  PutBucketRequestPaymentResponse
mkPutBucketRequestPaymentResponse =
  PutBucketRequestPaymentResponse'
