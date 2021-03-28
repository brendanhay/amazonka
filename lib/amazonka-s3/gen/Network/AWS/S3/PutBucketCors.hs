{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketCors
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
--
--
module Network.AWS.S3.PutBucketCors
    (
    -- * Creating a request
      PutBucketCors (..)
    , mkPutBucketCors
    -- ** Request lenses
    , pbcBucket
    , pbcCORSConfiguration
    , pbcContentMD5
    , pbcExpectedBucketOwner

    -- * Destructuring the response
    , PutBucketCorsResponse (..)
    , mkPutBucketCorsResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketCors' smart constructor.
data PutBucketCors = PutBucketCors'
  { bucket :: Types.BucketName
    -- ^ Specifies the bucket impacted by the @cors@ configuration.
  , cORSConfiguration :: Types.CORSConfiguration
    -- ^ Describes the cross-origin access configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
  , contentMD5 :: Core.Maybe Types.ContentMD5
    -- ^ The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.> 
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketCors' value with any optional fields omitted.
mkPutBucketCors
    :: Types.BucketName -- ^ 'bucket'
    -> Types.CORSConfiguration -- ^ 'cORSConfiguration'
    -> PutBucketCors
mkPutBucketCors bucket cORSConfiguration
  = PutBucketCors'{bucket, cORSConfiguration,
                   contentMD5 = Core.Nothing, expectedBucketOwner = Core.Nothing}

-- | Specifies the bucket impacted by the @cors@ configuration.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcBucket :: Lens.Lens' PutBucketCors Types.BucketName
pbcBucket = Lens.field @"bucket"
{-# INLINEABLE pbcBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Describes the cross-origin access configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'cORSConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcCORSConfiguration :: Lens.Lens' PutBucketCors Types.CORSConfiguration
pbcCORSConfiguration = Lens.field @"cORSConfiguration"
{-# INLINEABLE pbcCORSConfiguration #-}
{-# DEPRECATED cORSConfiguration "Use generic-lens or generic-optics with 'cORSConfiguration' instead"  #-}

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.> 
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcContentMD5 :: Lens.Lens' PutBucketCors (Core.Maybe Types.ContentMD5)
pbcContentMD5 = Lens.field @"contentMD5"
{-# INLINEABLE pbcContentMD5 #-}
{-# DEPRECATED contentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcExpectedBucketOwner :: Lens.Lens' PutBucketCors (Core.Maybe Types.ExpectedBucketOwner)
pbcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE pbcExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery PutBucketCors where
        toQuery PutBucketCors{..}
          = Core.toQueryPair "cors" ("" :: Core.Text)

instance Core.ToHeaders PutBucketCors where
        toHeaders PutBucketCors{..}
          = Core.toHeaders "Content-MD5" contentMD5 Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest PutBucketCors where
        type Rs PutBucketCors = PutBucketCorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PutBucketCorsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBucketCorsResponse' smart constructor.
data PutBucketCorsResponse = PutBucketCorsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketCorsResponse' value with any optional fields omitted.
mkPutBucketCorsResponse
    :: PutBucketCorsResponse
mkPutBucketCorsResponse = PutBucketCorsResponse'
