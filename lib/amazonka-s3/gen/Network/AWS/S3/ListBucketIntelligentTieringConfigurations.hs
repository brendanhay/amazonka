{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListBucketIntelligentTieringConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the S3 Intelligent-Tiering configuration from the specified bucket.
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead. S3 Intelligent-Tiering delivers automatic cost savings by moving data between access tiers, when access patterns change.
-- The S3 Intelligent-Tiering storage class is suitable for objects larger than 128 KB that you plan to store for at least 30 days. If the size of an object is less than 128 KB, it is not eligible for auto-tiering. Smaller objects can be stored, but they are always charged at the frequent access tier rates in the S3 Intelligent-Tiering storage class.
-- If you delete an object before the end of the 30-day minimum storage duration period, you are charged for 30 days. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
-- Operations related to @ListBucketIntelligentTieringConfigurations@ include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketIntelligentTieringConfiguration.html DeleteBucketIntelligentTieringConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketIntelligentTieringConfiguration.html PutBucketIntelligentTieringConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketIntelligentTieringConfiguration.html GetBucketIntelligentTieringConfiguration>
module Network.AWS.S3.ListBucketIntelligentTieringConfigurations
  ( -- * Creating a request
    ListBucketIntelligentTieringConfigurations (..),
    mkListBucketIntelligentTieringConfigurations,

    -- ** Request lenses
    lbitcContinuationToken,
    lbitcBucket,

    -- * Destructuring the response
    ListBucketIntelligentTieringConfigurationsResponse (..),
    mkListBucketIntelligentTieringConfigurationsResponse,

    -- ** Response lenses
    lbitcrsIntelligentTieringConfigurationList,
    lbitcrsContinuationToken,
    lbitcrsNextContinuationToken,
    lbitcrsIsTruncated,
    lbitcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkListBucketIntelligentTieringConfigurations' smart constructor.
data ListBucketIntelligentTieringConfigurations = ListBucketIntelligentTieringConfigurations'
  { continuationToken ::
      Lude.Maybe
        Lude.Text,
    bucket ::
      BucketName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBucketIntelligentTieringConfigurations' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
-- * 'continuationToken' - The ContinuationToken that represents a placeholder from where this request should begin.
mkListBucketIntelligentTieringConfigurations ::
  -- | 'bucket'
  BucketName ->
  ListBucketIntelligentTieringConfigurations
mkListBucketIntelligentTieringConfigurations pBucket_ =
  ListBucketIntelligentTieringConfigurations'
    { continuationToken =
        Lude.Nothing,
      bucket = pBucket_
    }

-- | The ContinuationToken that represents a placeholder from where this request should begin.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcContinuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurations (Lude.Maybe Lude.Text)
lbitcContinuationToken = Lens.lens (continuationToken :: ListBucketIntelligentTieringConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: ListBucketIntelligentTieringConfigurations)
{-# DEPRECATED lbitcContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcBucket :: Lens.Lens' ListBucketIntelligentTieringConfigurations BucketName
lbitcBucket = Lens.lens (bucket :: ListBucketIntelligentTieringConfigurations -> BucketName) (\s a -> s {bucket = a} :: ListBucketIntelligentTieringConfigurations)
{-# DEPRECATED lbitcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest ListBucketIntelligentTieringConfigurations where
  type
    Rs ListBucketIntelligentTieringConfigurations =
      ListBucketIntelligentTieringConfigurationsResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListBucketIntelligentTieringConfigurationsResponse'
            Lude.<$> (Lude.may (Lude.parseXMLList "IntelligentTieringConfiguration") x)
            Lude.<*> (x Lude..@? "ContinuationToken")
            Lude.<*> (x Lude..@? "NextContinuationToken")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBucketIntelligentTieringConfigurations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListBucketIntelligentTieringConfigurations where
  toPath ListBucketIntelligentTieringConfigurations' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery ListBucketIntelligentTieringConfigurations where
  toQuery ListBucketIntelligentTieringConfigurations' {..} =
    Lude.mconcat
      [ "continuation-token" Lude.=: continuationToken,
        "intelligent-tiering"
      ]

-- | /See:/ 'mkListBucketIntelligentTieringConfigurationsResponse' smart constructor.
data ListBucketIntelligentTieringConfigurationsResponse = ListBucketIntelligentTieringConfigurationsResponse'
  { intelligentTieringConfigurationList ::
      Lude.Maybe
        [IntelligentTieringConfiguration],
    continuationToken ::
      Lude.Maybe
        Lude.Text,
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ListBucketIntelligentTieringConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'continuationToken' - The ContinuationToken that represents a placeholder from where this request should begin.
-- * 'intelligentTieringConfigurationList' - The list of S3 Intelligent-Tiering configurations for a bucket.
-- * 'isTruncated' - Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
-- * 'nextContinuationToken' - The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
-- * 'responseStatus' - The response status code.
mkListBucketIntelligentTieringConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBucketIntelligentTieringConfigurationsResponse
mkListBucketIntelligentTieringConfigurationsResponse
  pResponseStatus_ =
    ListBucketIntelligentTieringConfigurationsResponse'
      { intelligentTieringConfigurationList =
          Lude.Nothing,
        continuationToken = Lude.Nothing,
        nextContinuationToken = Lude.Nothing,
        isTruncated = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The list of S3 Intelligent-Tiering configurations for a bucket.
--
-- /Note:/ Consider using 'intelligentTieringConfigurationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcrsIntelligentTieringConfigurationList :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Lude.Maybe [IntelligentTieringConfiguration])
lbitcrsIntelligentTieringConfigurationList = Lens.lens (intelligentTieringConfigurationList :: ListBucketIntelligentTieringConfigurationsResponse -> Lude.Maybe [IntelligentTieringConfiguration]) (\s a -> s {intelligentTieringConfigurationList = a} :: ListBucketIntelligentTieringConfigurationsResponse)
{-# DEPRECATED lbitcrsIntelligentTieringConfigurationList "Use generic-lens or generic-optics with 'intelligentTieringConfigurationList' instead." #-}

-- | The ContinuationToken that represents a placeholder from where this request should begin.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcrsContinuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Lude.Maybe Lude.Text)
lbitcrsContinuationToken = Lens.lens (continuationToken :: ListBucketIntelligentTieringConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: ListBucketIntelligentTieringConfigurationsResponse)
{-# DEPRECATED lbitcrsContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
--
-- /Note:/ Consider using 'nextContinuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcrsNextContinuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Lude.Maybe Lude.Text)
lbitcrsNextContinuationToken = Lens.lens (nextContinuationToken :: ListBucketIntelligentTieringConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextContinuationToken = a} :: ListBucketIntelligentTieringConfigurationsResponse)
{-# DEPRECATED lbitcrsNextContinuationToken "Use generic-lens or generic-optics with 'nextContinuationToken' instead." #-}

-- | Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcrsIsTruncated :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Lude.Maybe Lude.Bool)
lbitcrsIsTruncated = Lens.lens (isTruncated :: ListBucketIntelligentTieringConfigurationsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListBucketIntelligentTieringConfigurationsResponse)
{-# DEPRECATED lbitcrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcrsResponseStatus :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse Lude.Int
lbitcrsResponseStatus = Lens.lens (responseStatus :: ListBucketIntelligentTieringConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBucketIntelligentTieringConfigurationsResponse)
{-# DEPRECATED lbitcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
