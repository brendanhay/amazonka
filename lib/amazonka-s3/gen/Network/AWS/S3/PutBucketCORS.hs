{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketCORS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the @cors@ configuration for your bucket. If the configuration exists, Amazon S3 replaces it.
--
-- To use this operation, you must be allowed to perform the @s3:PutBucketCORS@ action. By default, the bucket owner has this permission and can grant it to others.
-- You set this configuration on a bucket so that the bucket can service cross-origin requests. For example, you might want to enable a request whose origin is @http://www.example.com@ to access your Amazon S3 bucket at @my.example.bucket.com@ by using the browser's @XMLHttpRequest@ capability.
-- To enable cross-origin resource sharing (CORS) on a bucket, you add the @cors@ subresource to the bucket. The @cors@ subresource is an XML document in which you configure rules that identify origins and the HTTP methods that can be executed on your bucket. The document is limited to 64 KB in size.
-- When Amazon S3 receives a cross-origin request (or a pre-flight OPTIONS request) against a bucket, it evaluates the @cors@ configuration on the bucket and uses the first @CORSRule@ rule that matches the incoming browser request to enable a cross-origin request. For a rule to match, the following conditions must be met:
--
--     * The request's @Origin@ header must match @AllowedOrigin@ elements.
--
--
--     * The request method (for example, GET, PUT, HEAD, and so on) or the @Access-Control-Request-Method@ header in case of a pre-flight @OPTIONS@ request must be one of the @AllowedMethod@ elements.
--
--
--     * Every header specified in the @Access-Control-Request-Headers@ request header of a pre-flight request must match an @AllowedHeader@ element.
--
--
-- For more information about CORS, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketCors.html GetBucketCors>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketCors.html DeleteBucketCors>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTOPTIONSobject.html RESTOPTIONSobject>
module Network.AWS.S3.PutBucketCORS
  ( -- * Creating a request
    PutBucketCORS (..),
    mkPutBucketCORS,

    -- ** Request lenses
    pbcBucket,
    pbcContentMD5,
    pbcCORSConfiguration,
    pbcExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketCORSResponse (..),
    mkPutBucketCORSResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketCORS' smart constructor.
data PutBucketCORS = PutBucketCORS'
  { -- | Specifies the bucket impacted by the @cors@ configuration.
    bucket :: BucketName,
    -- | The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | Describes the cross-origin access configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
    corsConfiguration :: CORSConfiguration,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketCORS' with the minimum fields required to make a request.
--
-- * 'bucket' - Specifies the bucket impacted by the @cors@ configuration.
-- * 'contentMD5' - The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'corsConfiguration' - Describes the cross-origin access configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutBucketCORS ::
  -- | 'bucket'
  BucketName ->
  -- | 'corsConfiguration'
  CORSConfiguration ->
  PutBucketCORS
mkPutBucketCORS pBucket_ pCORSConfiguration_ =
  PutBucketCORS'
    { bucket = pBucket_,
      contentMD5 = Lude.Nothing,
      corsConfiguration = pCORSConfiguration_,
      expectedBucketOwner = Lude.Nothing
    }

-- | Specifies the bucket impacted by the @cors@ configuration.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcBucket :: Lens.Lens' PutBucketCORS BucketName
pbcBucket = Lens.lens (bucket :: PutBucketCORS -> BucketName) (\s a -> s {bucket = a} :: PutBucketCORS)
{-# DEPRECATED pbcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcContentMD5 :: Lens.Lens' PutBucketCORS (Lude.Maybe Lude.Text)
pbcContentMD5 = Lens.lens (contentMD5 :: PutBucketCORS -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutBucketCORS)
{-# DEPRECATED pbcContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | Describes the cross-origin access configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'corsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcCORSConfiguration :: Lens.Lens' PutBucketCORS CORSConfiguration
pbcCORSConfiguration = Lens.lens (corsConfiguration :: PutBucketCORS -> CORSConfiguration) (\s a -> s {corsConfiguration = a} :: PutBucketCORS)
{-# DEPRECATED pbcCORSConfiguration "Use generic-lens or generic-optics with 'corsConfiguration' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcExpectedBucketOwner :: Lens.Lens' PutBucketCORS (Lude.Maybe Lude.Text)
pbcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketCORS -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketCORS)
{-# DEPRECATED pbcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutBucketCORS where
  type Rs PutBucketCORS = PutBucketCORSResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutBucketCORSResponse'

instance Lude.ToElement PutBucketCORS where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}CORSConfiguration"
      Lude.. corsConfiguration

instance Lude.ToHeaders PutBucketCORS where
  toHeaders PutBucketCORS' {..} =
    Lude.mconcat
      [ "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutBucketCORS where
  toPath PutBucketCORS' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketCORS where
  toQuery = Lude.const (Lude.mconcat ["cors"])

-- | /See:/ 'mkPutBucketCORSResponse' smart constructor.
data PutBucketCORSResponse = PutBucketCORSResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketCORSResponse' with the minimum fields required to make a request.
mkPutBucketCORSResponse ::
  PutBucketCORSResponse
mkPutBucketCORSResponse = PutBucketCORSResponse'
