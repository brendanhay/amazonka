{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketWebsite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the configuration of the website that is specified in the @website@ subresource. To configure a bucket as a website, you can add this subresource on the bucket with website configuration information such as the file name of the index document and any redirect rules. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> .
--
-- This PUT operation requires the @S3:PutBucketWebsite@ permission. By default, only the bucket owner can configure the website attached to a bucket; however, bucket owners can allow other users to set the website configuration by writing a bucket policy that grants them the @S3:PutBucketWebsite@ permission.
-- To redirect all website requests sent to the bucket's website endpoint, you add a website configuration with the following elements. Because all requests are sent to another website, you don't need to provide index document name for the bucket.
--
--     * @WebsiteConfiguration@
--
--
--     * @RedirectAllRequestsTo@
--
--
--     * @HostName@
--
--
--     * @Protocol@
--
--
-- If you want granular control over redirects, you can use the following elements to add routing rules that describe conditions for redirecting requests and information about the redirect destination. In this case, the website configuration must provide an index document for the bucket, because some requests might not be redirected.
--
--     * @WebsiteConfiguration@
--
--
--     * @IndexDocument@
--
--
--     * @Suffix@
--
--
--     * @ErrorDocument@
--
--
--     * @Key@
--
--
--     * @RoutingRules@
--
--
--     * @RoutingRule@
--
--
--     * @Condition@
--
--
--     * @HttpErrorCodeReturnedEquals@
--
--
--     * @KeyPrefixEquals@
--
--
--     * @Redirect@
--
--
--     * @Protocol@
--
--
--     * @HostName@
--
--
--     * @ReplaceKeyPrefixWith@
--
--
--     * @ReplaceKeyWith@
--
--
--     * @HttpRedirectCode@
--
--
-- Amazon S3 has a limitation of 50 routing rules per website configuration. If you require more than 50 routing rules, you can use object redirect. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html Configuring an Object Redirect> in the /Amazon Simple Storage Service Developer Guide/ .
module Network.AWS.S3.PutBucketWebsite
  ( -- * Creating a request
    PutBucketWebsite (..),
    mkPutBucketWebsite,

    -- ** Request lenses
    pbwBucket,
    pbwWebsiteConfiguration,
    pbwContentMD5,
    pbwExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketWebsiteResponse (..),
    mkPutBucketWebsiteResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketWebsite' smart constructor.
data PutBucketWebsite = PutBucketWebsite'
  { -- | The bucket name.
    bucket :: Types.BucketName,
    -- | Container for the request.
    websiteConfiguration :: Types.WebsiteConfiguration,
    -- | The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Core.Maybe Types.ContentMD5,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketWebsite' value with any optional fields omitted.
mkPutBucketWebsite ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'websiteConfiguration'
  Types.WebsiteConfiguration ->
  PutBucketWebsite
mkPutBucketWebsite bucket websiteConfiguration =
  PutBucketWebsite'
    { bucket,
      websiteConfiguration,
      contentMD5 = Core.Nothing,
      expectedBucketOwner = Core.Nothing
    }

-- | The bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbwBucket :: Lens.Lens' PutBucketWebsite Types.BucketName
pbwBucket = Lens.field @"bucket"
{-# DEPRECATED pbwBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Container for the request.
--
-- /Note:/ Consider using 'websiteConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbwWebsiteConfiguration :: Lens.Lens' PutBucketWebsite Types.WebsiteConfiguration
pbwWebsiteConfiguration = Lens.field @"websiteConfiguration"
{-# DEPRECATED pbwWebsiteConfiguration "Use generic-lens or generic-optics with 'websiteConfiguration' instead." #-}

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbwContentMD5 :: Lens.Lens' PutBucketWebsite (Core.Maybe Types.ContentMD5)
pbwContentMD5 = Lens.field @"contentMD5"
{-# DEPRECATED pbwContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbwExpectedBucketOwner :: Lens.Lens' PutBucketWebsite (Core.Maybe Types.ExpectedBucketOwner)
pbwExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED pbwExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest PutBucketWebsite where
  type Rs PutBucketWebsite = PutBucketWebsiteResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("website", ""),
        Core._rqHeaders =
          Core.toHeaders "Content-MD5" contentMD5
            Core.<> (Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner),
        Core._rqBody = Core.toXMLBody x
      }
  response = Response.receiveNull PutBucketWebsiteResponse'

-- | /See:/ 'mkPutBucketWebsiteResponse' smart constructor.
data PutBucketWebsiteResponse = PutBucketWebsiteResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketWebsiteResponse' value with any optional fields omitted.
mkPutBucketWebsiteResponse ::
  PutBucketWebsiteResponse
mkPutBucketWebsiteResponse = PutBucketWebsiteResponse'
