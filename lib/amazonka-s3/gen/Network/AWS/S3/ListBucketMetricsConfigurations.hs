{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListBucketMetricsConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the metrics configurations for the bucket. The metrics configurations are only for the request metrics of the bucket and do not provide information on daily storage metrics. You can have up to 1,000 configurations per bucket.
--
-- This operation supports list pagination and does not return more than 100 configurations at a time. Always check the @IsTruncated@ element in the response. If there are no more configurations to list, @IsTruncated@ is set to false. If there are more configurations to list, @IsTruncated@ is set to true, and there is a value in @NextContinuationToken@ . You use the @NextContinuationToken@ value to continue the pagination of the list by passing the value in @continuation-token@ in the request to @GET@ the next page.
-- To use this operation, you must have permissions to perform the @s3:GetMetricsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For more information about metrics configurations and CloudWatch request metrics, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch> .
-- The following operations are related to @ListBucketMetricsConfigurations@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketMetricsConfiguration.html GetBucketMetricsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketMetricsConfiguration.html DeleteBucketMetricsConfiguration>
module Network.AWS.S3.ListBucketMetricsConfigurations
  ( -- * Creating a request
    ListBucketMetricsConfigurations (..),
    mkListBucketMetricsConfigurations,

    -- ** Request lenses
    lbmcContinuationToken,
    lbmcExpectedBucketOwner,
    lbmcBucket,

    -- * Destructuring the response
    ListBucketMetricsConfigurationsResponse (..),
    mkListBucketMetricsConfigurationsResponse,

    -- ** Response lenses
    lbmcrsContinuationToken,
    lbmcrsMetricsConfigurationList,
    lbmcrsNextContinuationToken,
    lbmcrsIsTruncated,
    lbmcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkListBucketMetricsConfigurations' smart constructor.
data ListBucketMetricsConfigurations = ListBucketMetricsConfigurations'
  { continuationToken ::
      Lude.Maybe Lude.Text,
    expectedBucketOwner ::
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

-- | Creates a value of 'ListBucketMetricsConfigurations' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket containing the metrics configurations to retrieve.
-- * 'continuationToken' - The marker that is used to continue a metrics configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkListBucketMetricsConfigurations ::
  -- | 'bucket'
  BucketName ->
  ListBucketMetricsConfigurations
mkListBucketMetricsConfigurations pBucket_ =
  ListBucketMetricsConfigurations'
    { continuationToken =
        Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The marker that is used to continue a metrics configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcContinuationToken :: Lens.Lens' ListBucketMetricsConfigurations (Lude.Maybe Lude.Text)
lbmcContinuationToken = Lens.lens (continuationToken :: ListBucketMetricsConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: ListBucketMetricsConfigurations)
{-# DEPRECATED lbmcContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcExpectedBucketOwner :: Lens.Lens' ListBucketMetricsConfigurations (Lude.Maybe Lude.Text)
lbmcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: ListBucketMetricsConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: ListBucketMetricsConfigurations)
{-# DEPRECATED lbmcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket containing the metrics configurations to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcBucket :: Lens.Lens' ListBucketMetricsConfigurations BucketName
lbmcBucket = Lens.lens (bucket :: ListBucketMetricsConfigurations -> BucketName) (\s a -> s {bucket = a} :: ListBucketMetricsConfigurations)
{-# DEPRECATED lbmcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest ListBucketMetricsConfigurations where
  type
    Rs ListBucketMetricsConfigurations =
      ListBucketMetricsConfigurationsResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListBucketMetricsConfigurationsResponse'
            Lude.<$> (x Lude..@? "ContinuationToken")
            Lude.<*> (Lude.may (Lude.parseXMLList "MetricsConfiguration") x)
            Lude.<*> (x Lude..@? "NextContinuationToken")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBucketMetricsConfigurations where
  toHeaders ListBucketMetricsConfigurations' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath ListBucketMetricsConfigurations where
  toPath ListBucketMetricsConfigurations' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery ListBucketMetricsConfigurations where
  toQuery ListBucketMetricsConfigurations' {..} =
    Lude.mconcat
      ["continuation-token" Lude.=: continuationToken, "metrics"]

-- | /See:/ 'mkListBucketMetricsConfigurationsResponse' smart constructor.
data ListBucketMetricsConfigurationsResponse = ListBucketMetricsConfigurationsResponse'
  { continuationToken ::
      Lude.Maybe
        Lude.Text,
    metricsConfigurationList ::
      Lude.Maybe
        [MetricsConfiguration],
    nextContinuationToken ::
      Lude.Maybe
        Lude.Text,
    isTruncated ::
      Lude.Maybe
        Lude.Bool,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBucketMetricsConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'continuationToken' - The marker that is used as a starting point for this metrics configuration list response. This value is present if it was sent in the request.
-- * 'isTruncated' - Indicates whether the returned list of metrics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
-- * 'metricsConfigurationList' - The list of metrics configurations for a bucket.
-- * 'nextContinuationToken' - The marker used to continue a metrics configuration listing that has been truncated. Use the @NextContinuationToken@ from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
-- * 'responseStatus' - The response status code.
mkListBucketMetricsConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBucketMetricsConfigurationsResponse
mkListBucketMetricsConfigurationsResponse pResponseStatus_ =
  ListBucketMetricsConfigurationsResponse'
    { continuationToken =
        Lude.Nothing,
      metricsConfigurationList = Lude.Nothing,
      nextContinuationToken = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The marker that is used as a starting point for this metrics configuration list response. This value is present if it was sent in the request.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcrsContinuationToken :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Lude.Maybe Lude.Text)
lbmcrsContinuationToken = Lens.lens (continuationToken :: ListBucketMetricsConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: ListBucketMetricsConfigurationsResponse)
{-# DEPRECATED lbmcrsContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The list of metrics configurations for a bucket.
--
-- /Note:/ Consider using 'metricsConfigurationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcrsMetricsConfigurationList :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Lude.Maybe [MetricsConfiguration])
lbmcrsMetricsConfigurationList = Lens.lens (metricsConfigurationList :: ListBucketMetricsConfigurationsResponse -> Lude.Maybe [MetricsConfiguration]) (\s a -> s {metricsConfigurationList = a} :: ListBucketMetricsConfigurationsResponse)
{-# DEPRECATED lbmcrsMetricsConfigurationList "Use generic-lens or generic-optics with 'metricsConfigurationList' instead." #-}

-- | The marker used to continue a metrics configuration listing that has been truncated. Use the @NextContinuationToken@ from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
--
-- /Note:/ Consider using 'nextContinuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcrsNextContinuationToken :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Lude.Maybe Lude.Text)
lbmcrsNextContinuationToken = Lens.lens (nextContinuationToken :: ListBucketMetricsConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextContinuationToken = a} :: ListBucketMetricsConfigurationsResponse)
{-# DEPRECATED lbmcrsNextContinuationToken "Use generic-lens or generic-optics with 'nextContinuationToken' instead." #-}

-- | Indicates whether the returned list of metrics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcrsIsTruncated :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Lude.Maybe Lude.Bool)
lbmcrsIsTruncated = Lens.lens (isTruncated :: ListBucketMetricsConfigurationsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListBucketMetricsConfigurationsResponse)
{-# DEPRECATED lbmcrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcrsResponseStatus :: Lens.Lens' ListBucketMetricsConfigurationsResponse Lude.Int
lbmcrsResponseStatus = Lens.lens (responseStatus :: ListBucketMetricsConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBucketMetricsConfigurationsResponse)
{-# DEPRECATED lbmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
