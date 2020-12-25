{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketWebsite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the website configuration for a bucket. To host website on Amazon S3, you can configure a bucket as website by adding a website configuration. For more information about hosting websites, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> .
--
-- This GET operation requires the @S3:GetBucketWebsite@ permission. By default, only the bucket owner can read the bucket website configuration. However, bucket owners can allow other users to read the website configuration by writing a bucket policy granting them the @S3:GetBucketWebsite@ permission.
-- The following operations are related to @DeleteBucketWebsite@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketWebsite.html DeleteBucketWebsite>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketWebsite.html PutBucketWebsite>
module Network.AWS.S3.GetBucketWebsite
  ( -- * Creating a request
    GetBucketWebsite (..),
    mkGetBucketWebsite,

    -- ** Request lenses
    gbwBucket,
    gbwExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketWebsiteResponse (..),
    mkGetBucketWebsiteResponse,

    -- ** Response lenses
    gbwrrsErrorDocument,
    gbwrrsIndexDocument,
    gbwrrsRedirectAllRequestsTo,
    gbwrrsRoutingRules,
    gbwrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketWebsite' smart constructor.
data GetBucketWebsite = GetBucketWebsite'
  { -- | The bucket name for which to get the website configuration.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketWebsite' value with any optional fields omitted.
mkGetBucketWebsite ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketWebsite
mkGetBucketWebsite bucket =
  GetBucketWebsite' {bucket, expectedBucketOwner = Core.Nothing}

-- | The bucket name for which to get the website configuration.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwBucket :: Lens.Lens' GetBucketWebsite Types.BucketName
gbwBucket = Lens.field @"bucket"
{-# DEPRECATED gbwBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwExpectedBucketOwner :: Lens.Lens' GetBucketWebsite (Core.Maybe Types.ExpectedBucketOwner)
gbwExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gbwExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketWebsite where
  type Rs GetBucketWebsite = GetBucketWebsiteResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("website", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketWebsiteResponse'
            Core.<$> (x Core..@? "ErrorDocument")
            Core.<*> (x Core..@? "IndexDocument")
            Core.<*> (x Core..@? "RedirectAllRequestsTo")
            Core.<*> ( x Core..@? "RoutingRules"
                         Core..<@> Core.parseXMLList "RoutingRule"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketWebsiteResponse' smart constructor.
data GetBucketWebsiteResponse = GetBucketWebsiteResponse'
  { -- | The object key name of the website error document to use for 4XX class errors.
    errorDocument :: Core.Maybe Types.ErrorDocument,
    -- | The name of the index document for the website (for example @index.html@ ).
    indexDocument :: Core.Maybe Types.IndexDocument,
    -- | Specifies the redirect behavior of all requests to a website endpoint of an Amazon S3 bucket.
    redirectAllRequestsTo :: Core.Maybe Types.RedirectAllRequestsTo,
    -- | Rules that define when a redirect is applied and the redirect behavior.
    routingRules :: Core.Maybe [Types.RoutingRule],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketWebsiteResponse' value with any optional fields omitted.
mkGetBucketWebsiteResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBucketWebsiteResponse
mkGetBucketWebsiteResponse responseStatus =
  GetBucketWebsiteResponse'
    { errorDocument = Core.Nothing,
      indexDocument = Core.Nothing,
      redirectAllRequestsTo = Core.Nothing,
      routingRules = Core.Nothing,
      responseStatus
    }

-- | The object key name of the website error document to use for 4XX class errors.
--
-- /Note:/ Consider using 'errorDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwrrsErrorDocument :: Lens.Lens' GetBucketWebsiteResponse (Core.Maybe Types.ErrorDocument)
gbwrrsErrorDocument = Lens.field @"errorDocument"
{-# DEPRECATED gbwrrsErrorDocument "Use generic-lens or generic-optics with 'errorDocument' instead." #-}

-- | The name of the index document for the website (for example @index.html@ ).
--
-- /Note:/ Consider using 'indexDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwrrsIndexDocument :: Lens.Lens' GetBucketWebsiteResponse (Core.Maybe Types.IndexDocument)
gbwrrsIndexDocument = Lens.field @"indexDocument"
{-# DEPRECATED gbwrrsIndexDocument "Use generic-lens or generic-optics with 'indexDocument' instead." #-}

-- | Specifies the redirect behavior of all requests to a website endpoint of an Amazon S3 bucket.
--
-- /Note:/ Consider using 'redirectAllRequestsTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwrrsRedirectAllRequestsTo :: Lens.Lens' GetBucketWebsiteResponse (Core.Maybe Types.RedirectAllRequestsTo)
gbwrrsRedirectAllRequestsTo = Lens.field @"redirectAllRequestsTo"
{-# DEPRECATED gbwrrsRedirectAllRequestsTo "Use generic-lens or generic-optics with 'redirectAllRequestsTo' instead." #-}

-- | Rules that define when a redirect is applied and the redirect behavior.
--
-- /Note:/ Consider using 'routingRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwrrsRoutingRules :: Lens.Lens' GetBucketWebsiteResponse (Core.Maybe [Types.RoutingRule])
gbwrrsRoutingRules = Lens.field @"routingRules"
{-# DEPRECATED gbwrrsRoutingRules "Use generic-lens or generic-optics with 'routingRules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwrrsResponseStatus :: Lens.Lens' GetBucketWebsiteResponse Core.Int
gbwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
