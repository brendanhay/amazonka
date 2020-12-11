{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gbwExpectedBucketOwner,
    gbwBucket,

    -- * Destructuring the response
    GetBucketWebsiteResponse (..),
    mkGetBucketWebsiteResponse,

    -- ** Response lenses
    gbwrsRedirectAllRequestsTo,
    gbwrsErrorDocument,
    gbwrsIndexDocument,
    gbwrsRoutingRules,
    gbwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketWebsite' smart constructor.
data GetBucketWebsite = GetBucketWebsite'
  { expectedBucketOwner ::
      Lude.Maybe Lude.Text,
    bucket :: BucketName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketWebsite' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name for which to get the website configuration.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketWebsite ::
  -- | 'bucket'
  BucketName ->
  GetBucketWebsite
mkGetBucketWebsite pBucket_ =
  GetBucketWebsite'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwExpectedBucketOwner :: Lens.Lens' GetBucketWebsite (Lude.Maybe Lude.Text)
gbwExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketWebsite -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketWebsite)
{-# DEPRECATED gbwExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name for which to get the website configuration.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwBucket :: Lens.Lens' GetBucketWebsite BucketName
gbwBucket = Lens.lens (bucket :: GetBucketWebsite -> BucketName) (\s a -> s {bucket = a} :: GetBucketWebsite)
{-# DEPRECATED gbwBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest GetBucketWebsite where
  type Rs GetBucketWebsite = GetBucketWebsiteResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketWebsiteResponse'
            Lude.<$> (x Lude..@? "RedirectAllRequestsTo")
            Lude.<*> (x Lude..@? "ErrorDocument")
            Lude.<*> (x Lude..@? "IndexDocument")
            Lude.<*> ( x Lude..@? "RoutingRules" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "RoutingRule")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketWebsite where
  toHeaders GetBucketWebsite' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketWebsite where
  toPath GetBucketWebsite' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketWebsite where
  toQuery = Lude.const (Lude.mconcat ["website"])

-- | /See:/ 'mkGetBucketWebsiteResponse' smart constructor.
data GetBucketWebsiteResponse = GetBucketWebsiteResponse'
  { redirectAllRequestsTo ::
      Lude.Maybe RedirectAllRequestsTo,
    errorDocument :: Lude.Maybe ErrorDocument,
    indexDocument :: Lude.Maybe IndexDocument,
    routingRules :: Lude.Maybe [RoutingRule],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketWebsiteResponse' with the minimum fields required to make a request.
--
-- * 'errorDocument' - The object key name of the website error document to use for 4XX class errors.
-- * 'indexDocument' - The name of the index document for the website (for example @index.html@ ).
-- * 'redirectAllRequestsTo' - Specifies the redirect behavior of all requests to a website endpoint of an Amazon S3 bucket.
-- * 'responseStatus' - The response status code.
-- * 'routingRules' - Rules that define when a redirect is applied and the redirect behavior.
mkGetBucketWebsiteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketWebsiteResponse
mkGetBucketWebsiteResponse pResponseStatus_ =
  GetBucketWebsiteResponse'
    { redirectAllRequestsTo = Lude.Nothing,
      errorDocument = Lude.Nothing,
      indexDocument = Lude.Nothing,
      routingRules = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Specifies the redirect behavior of all requests to a website endpoint of an Amazon S3 bucket.
--
-- /Note:/ Consider using 'redirectAllRequestsTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwrsRedirectAllRequestsTo :: Lens.Lens' GetBucketWebsiteResponse (Lude.Maybe RedirectAllRequestsTo)
gbwrsRedirectAllRequestsTo = Lens.lens (redirectAllRequestsTo :: GetBucketWebsiteResponse -> Lude.Maybe RedirectAllRequestsTo) (\s a -> s {redirectAllRequestsTo = a} :: GetBucketWebsiteResponse)
{-# DEPRECATED gbwrsRedirectAllRequestsTo "Use generic-lens or generic-optics with 'redirectAllRequestsTo' instead." #-}

-- | The object key name of the website error document to use for 4XX class errors.
--
-- /Note:/ Consider using 'errorDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwrsErrorDocument :: Lens.Lens' GetBucketWebsiteResponse (Lude.Maybe ErrorDocument)
gbwrsErrorDocument = Lens.lens (errorDocument :: GetBucketWebsiteResponse -> Lude.Maybe ErrorDocument) (\s a -> s {errorDocument = a} :: GetBucketWebsiteResponse)
{-# DEPRECATED gbwrsErrorDocument "Use generic-lens or generic-optics with 'errorDocument' instead." #-}

-- | The name of the index document for the website (for example @index.html@ ).
--
-- /Note:/ Consider using 'indexDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwrsIndexDocument :: Lens.Lens' GetBucketWebsiteResponse (Lude.Maybe IndexDocument)
gbwrsIndexDocument = Lens.lens (indexDocument :: GetBucketWebsiteResponse -> Lude.Maybe IndexDocument) (\s a -> s {indexDocument = a} :: GetBucketWebsiteResponse)
{-# DEPRECATED gbwrsIndexDocument "Use generic-lens or generic-optics with 'indexDocument' instead." #-}

-- | Rules that define when a redirect is applied and the redirect behavior.
--
-- /Note:/ Consider using 'routingRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwrsRoutingRules :: Lens.Lens' GetBucketWebsiteResponse (Lude.Maybe [RoutingRule])
gbwrsRoutingRules = Lens.lens (routingRules :: GetBucketWebsiteResponse -> Lude.Maybe [RoutingRule]) (\s a -> s {routingRules = a} :: GetBucketWebsiteResponse)
{-# DEPRECATED gbwrsRoutingRules "Use generic-lens or generic-optics with 'routingRules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbwrsResponseStatus :: Lens.Lens' GetBucketWebsiteResponse Lude.Int
gbwrsResponseStatus = Lens.lens (responseStatus :: GetBucketWebsiteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketWebsiteResponse)
{-# DEPRECATED gbwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
