{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    lbitcBucket,
    lbitcContinuationToken,

    -- * Destructuring the response
    ListBucketIntelligentTieringConfigurationsResponse (..),
    mkListBucketIntelligentTieringConfigurationsResponse,

    -- ** Response lenses
    lbitcrrsContinuationToken,
    lbitcrrsIntelligentTieringConfigurationList,
    lbitcrrsIsTruncated,
    lbitcrrsNextContinuationToken,
    lbitcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkListBucketIntelligentTieringConfigurations' smart constructor.
data ListBucketIntelligentTieringConfigurations = ListBucketIntelligentTieringConfigurations'
  { -- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
    bucket :: Types.BucketName,
    -- | The ContinuationToken that represents a placeholder from where this request should begin.
    continuationToken :: Core.Maybe Types.Token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBucketIntelligentTieringConfigurations' value with any optional fields omitted.
mkListBucketIntelligentTieringConfigurations ::
  -- | 'bucket'
  Types.BucketName ->
  ListBucketIntelligentTieringConfigurations
mkListBucketIntelligentTieringConfigurations bucket =
  ListBucketIntelligentTieringConfigurations'
    { bucket,
      continuationToken = Core.Nothing
    }

-- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcBucket :: Lens.Lens' ListBucketIntelligentTieringConfigurations Types.BucketName
lbitcBucket = Lens.field @"bucket"
{-# DEPRECATED lbitcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ContinuationToken that represents a placeholder from where this request should begin.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcContinuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurations (Core.Maybe Types.Token)
lbitcContinuationToken = Lens.field @"continuationToken"
{-# DEPRECATED lbitcContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

instance Core.AWSRequest ListBucketIntelligentTieringConfigurations where
  type
    Rs ListBucketIntelligentTieringConfigurations =
      ListBucketIntelligentTieringConfigurationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery =
          Core.toQueryValue "continuation-token" Core.<$> continuationToken
            Core.<> (Core.pure ("intelligent-tiering", "")),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListBucketIntelligentTieringConfigurationsResponse'
            Core.<$> (x Core..@? "ContinuationToken")
            Core.<*> (x Core..@? "IntelligentTieringConfiguration")
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "NextContinuationToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListBucketIntelligentTieringConfigurationsResponse' smart constructor.
data ListBucketIntelligentTieringConfigurationsResponse = ListBucketIntelligentTieringConfigurationsResponse'
  { -- | The ContinuationToken that represents a placeholder from where this request should begin.
    continuationToken :: Core.Maybe Types.ContinuationToken,
    -- | The list of S3 Intelligent-Tiering configurations for a bucket.
    intelligentTieringConfigurationList :: Core.Maybe [Types.IntelligentTieringConfiguration],
    -- | Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
    isTruncated :: Core.Maybe Core.Bool,
    -- | The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
    nextContinuationToken :: Core.Maybe Types.NextContinuationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBucketIntelligentTieringConfigurationsResponse' value with any optional fields omitted.
mkListBucketIntelligentTieringConfigurationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListBucketIntelligentTieringConfigurationsResponse
mkListBucketIntelligentTieringConfigurationsResponse responseStatus =
  ListBucketIntelligentTieringConfigurationsResponse'
    { continuationToken =
        Core.Nothing,
      intelligentTieringConfigurationList =
        Core.Nothing,
      isTruncated = Core.Nothing,
      nextContinuationToken = Core.Nothing,
      responseStatus
    }

-- | The ContinuationToken that represents a placeholder from where this request should begin.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcrrsContinuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Core.Maybe Types.ContinuationToken)
lbitcrrsContinuationToken = Lens.field @"continuationToken"
{-# DEPRECATED lbitcrrsContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The list of S3 Intelligent-Tiering configurations for a bucket.
--
-- /Note:/ Consider using 'intelligentTieringConfigurationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcrrsIntelligentTieringConfigurationList :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Core.Maybe [Types.IntelligentTieringConfiguration])
lbitcrrsIntelligentTieringConfigurationList = Lens.field @"intelligentTieringConfigurationList"
{-# DEPRECATED lbitcrrsIntelligentTieringConfigurationList "Use generic-lens or generic-optics with 'intelligentTieringConfigurationList' instead." #-}

-- | Indicates whether the returned list of analytics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcrrsIsTruncated :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Core.Maybe Core.Bool)
lbitcrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lbitcrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
--
-- /Note:/ Consider using 'nextContinuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcrrsNextContinuationToken :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse (Core.Maybe Types.NextContinuationToken)
lbitcrrsNextContinuationToken = Lens.field @"nextContinuationToken"
{-# DEPRECATED lbitcrrsNextContinuationToken "Use generic-lens or generic-optics with 'nextContinuationToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbitcrrsResponseStatus :: Lens.Lens' ListBucketIntelligentTieringConfigurationsResponse Core.Int
lbitcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lbitcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
