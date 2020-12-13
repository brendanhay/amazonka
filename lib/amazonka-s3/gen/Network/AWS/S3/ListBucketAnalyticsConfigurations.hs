{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListBucketAnalyticsConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the analytics configurations for the bucket. You can have up to 1,000 analytics configurations per bucket.
--
-- This operation supports list pagination and does not return more than 100 configurations at a time. You should always check the @IsTruncated@ element in the response. If there are no more configurations to list, @IsTruncated@ is set to false. If there are more configurations to list, @IsTruncated@ is set to true, and there will be a value in @NextContinuationToken@ . You use the @NextContinuationToken@ value to continue the pagination of the list by passing the value in continuation-token in the request to @GET@ the next page.
-- To use this operation, you must have permissions to perform the @s3:GetAnalyticsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about Amazon S3 analytics feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis> .
-- The following operations are related to @ListBucketAnalyticsConfigurations@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAnalyticsConfiguration.html GetBucketAnalyticsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketAnalyticsConfiguration.html DeleteBucketAnalyticsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration>
module Network.AWS.S3.ListBucketAnalyticsConfigurations
  ( -- * Creating a request
    ListBucketAnalyticsConfigurations (..),
    mkListBucketAnalyticsConfigurations,

    -- ** Request lenses
    lbacContinuationToken,
    lbacBucket,
    lbacExpectedBucketOwner,

    -- * Destructuring the response
    ListBucketAnalyticsConfigurationsResponse (..),
    mkListBucketAnalyticsConfigurationsResponse,

    -- ** Response lenses
    lbacrsAnalyticsConfigurationList,
    lbacrsContinuationToken,
    lbacrsNextContinuationToken,
    lbacrsIsTruncated,
    lbacrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkListBucketAnalyticsConfigurations' smart constructor.
data ListBucketAnalyticsConfigurations = ListBucketAnalyticsConfigurations'
  { -- | The ContinuationToken that represents a placeholder from where this request should begin.
    continuationToken :: Lude.Maybe Lude.Text,
    -- | The name of the bucket from which analytics configurations are retrieved.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBucketAnalyticsConfigurations' with the minimum fields required to make a request.
--
-- * 'continuationToken' - The ContinuationToken that represents a placeholder from where this request should begin.
-- * 'bucket' - The name of the bucket from which analytics configurations are retrieved.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkListBucketAnalyticsConfigurations ::
  -- | 'bucket'
  BucketName ->
  ListBucketAnalyticsConfigurations
mkListBucketAnalyticsConfigurations pBucket_ =
  ListBucketAnalyticsConfigurations'
    { continuationToken =
        Lude.Nothing,
      bucket = pBucket_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The ContinuationToken that represents a placeholder from where this request should begin.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbacContinuationToken :: Lens.Lens' ListBucketAnalyticsConfigurations (Lude.Maybe Lude.Text)
lbacContinuationToken = Lens.lens (continuationToken :: ListBucketAnalyticsConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: ListBucketAnalyticsConfigurations)
{-# DEPRECATED lbacContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The name of the bucket from which analytics configurations are retrieved.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbacBucket :: Lens.Lens' ListBucketAnalyticsConfigurations BucketName
lbacBucket = Lens.lens (bucket :: ListBucketAnalyticsConfigurations -> BucketName) (\s a -> s {bucket = a} :: ListBucketAnalyticsConfigurations)
{-# DEPRECATED lbacBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbacExpectedBucketOwner :: Lens.Lens' ListBucketAnalyticsConfigurations (Lude.Maybe Lude.Text)
lbacExpectedBucketOwner = Lens.lens (expectedBucketOwner :: ListBucketAnalyticsConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: ListBucketAnalyticsConfigurations)
{-# DEPRECATED lbacExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest ListBucketAnalyticsConfigurations where
  type
    Rs ListBucketAnalyticsConfigurations =
      ListBucketAnalyticsConfigurationsResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListBucketAnalyticsConfigurationsResponse'
            Lude.<$> (Lude.may (Lude.parseXMLList "AnalyticsConfiguration") x)
            Lude.<*> (x Lude..@? "ContinuationToken")
            Lude.<*> (x Lude..@? "NextContinuationToken")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBucketAnalyticsConfigurations where
  toHeaders ListBucketAnalyticsConfigurations' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath ListBucketAnalyticsConfigurations where
  toPath ListBucketAnalyticsConfigurations' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery ListBucketAnalyticsConfigurations where
  toQuery ListBucketAnalyticsConfigurations' {..} =
    Lude.mconcat
      ["continuation-token" Lude.=: continuationToken, "analytics"]

-- | /See:/ 'mkListBucketAnalyticsConfigurationsResponse' smart constructor.
data ListBucketAnalyticsConfigurationsResponse = ListBucketAnalyticsConfigurationsResponse'
  { -- | The list of analytics configurations for a bucket.
    analyticsConfigurationList :: Lude.Maybe [AnalyticsConfiguration],
    -- | The marker that is used as a starting point for this analytics configuration list response. This value is present if it was sent in the request.
    continuationToken :: Lude.Maybe Lude.Text,
    -- | @NextContinuationToken@ is sent when @isTruncated@ is true, which indicates that there are more analytics configurations to list. The next request must include this @NextContinuationToken@ . The token is obfuscated and is not a usable value.
    nextContinuationToken :: Lude.Maybe Lude.Text,
    -- | Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBucketAnalyticsConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'analyticsConfigurationList' - The list of analytics configurations for a bucket.
-- * 'continuationToken' - The marker that is used as a starting point for this analytics configuration list response. This value is present if it was sent in the request.
-- * 'nextContinuationToken' - @NextContinuationToken@ is sent when @isTruncated@ is true, which indicates that there are more analytics configurations to list. The next request must include this @NextContinuationToken@ . The token is obfuscated and is not a usable value.
-- * 'isTruncated' - Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
-- * 'responseStatus' - The response status code.
mkListBucketAnalyticsConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBucketAnalyticsConfigurationsResponse
mkListBucketAnalyticsConfigurationsResponse pResponseStatus_ =
  ListBucketAnalyticsConfigurationsResponse'
    { analyticsConfigurationList =
        Lude.Nothing,
      continuationToken = Lude.Nothing,
      nextContinuationToken = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of analytics configurations for a bucket.
--
-- /Note:/ Consider using 'analyticsConfigurationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbacrsAnalyticsConfigurationList :: Lens.Lens' ListBucketAnalyticsConfigurationsResponse (Lude.Maybe [AnalyticsConfiguration])
lbacrsAnalyticsConfigurationList = Lens.lens (analyticsConfigurationList :: ListBucketAnalyticsConfigurationsResponse -> Lude.Maybe [AnalyticsConfiguration]) (\s a -> s {analyticsConfigurationList = a} :: ListBucketAnalyticsConfigurationsResponse)
{-# DEPRECATED lbacrsAnalyticsConfigurationList "Use generic-lens or generic-optics with 'analyticsConfigurationList' instead." #-}

-- | The marker that is used as a starting point for this analytics configuration list response. This value is present if it was sent in the request.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbacrsContinuationToken :: Lens.Lens' ListBucketAnalyticsConfigurationsResponse (Lude.Maybe Lude.Text)
lbacrsContinuationToken = Lens.lens (continuationToken :: ListBucketAnalyticsConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: ListBucketAnalyticsConfigurationsResponse)
{-# DEPRECATED lbacrsContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | @NextContinuationToken@ is sent when @isTruncated@ is true, which indicates that there are more analytics configurations to list. The next request must include this @NextContinuationToken@ . The token is obfuscated and is not a usable value.
--
-- /Note:/ Consider using 'nextContinuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbacrsNextContinuationToken :: Lens.Lens' ListBucketAnalyticsConfigurationsResponse (Lude.Maybe Lude.Text)
lbacrsNextContinuationToken = Lens.lens (nextContinuationToken :: ListBucketAnalyticsConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextContinuationToken = a} :: ListBucketAnalyticsConfigurationsResponse)
{-# DEPRECATED lbacrsNextContinuationToken "Use generic-lens or generic-optics with 'nextContinuationToken' instead." #-}

-- | Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbacrsIsTruncated :: Lens.Lens' ListBucketAnalyticsConfigurationsResponse (Lude.Maybe Lude.Bool)
lbacrsIsTruncated = Lens.lens (isTruncated :: ListBucketAnalyticsConfigurationsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListBucketAnalyticsConfigurationsResponse)
{-# DEPRECATED lbacrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbacrsResponseStatus :: Lens.Lens' ListBucketAnalyticsConfigurationsResponse Lude.Int
lbacrsResponseStatus = Lens.lens (responseStatus :: ListBucketAnalyticsConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBucketAnalyticsConfigurationsResponse)
{-# DEPRECATED lbacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
