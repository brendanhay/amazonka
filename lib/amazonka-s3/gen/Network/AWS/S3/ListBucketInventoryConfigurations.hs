{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListBucketInventoryConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of inventory configurations for the bucket. You can have up to 1,000 analytics configurations per bucket.
--
-- This operation supports list pagination and does not return more than 100 configurations at a time. Always check the @IsTruncated@ element in the response. If there are no more configurations to list, @IsTruncated@ is set to false. If there are more configurations to list, @IsTruncated@ is set to true, and there is a value in @NextContinuationToken@ . You use the @NextContinuationToken@ value to continue the pagination of the list by passing the value in continuation-token in the request to @GET@ the next page.
-- To use this operation, you must have permissions to perform the @s3:GetInventoryConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about the Amazon S3 inventory feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory>
-- The following operations are related to @ListBucketInventoryConfigurations@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration>
module Network.AWS.S3.ListBucketInventoryConfigurations
  ( -- * Creating a request
    ListBucketInventoryConfigurations (..),
    mkListBucketInventoryConfigurations,

    -- ** Request lenses
    lbicContinuationToken,
    lbicExpectedBucketOwner,
    lbicBucket,

    -- * Destructuring the response
    ListBucketInventoryConfigurationsResponse (..),
    mkListBucketInventoryConfigurationsResponse,

    -- ** Response lenses
    lbicrsContinuationToken,
    lbicrsInventoryConfigurationList,
    lbicrsNextContinuationToken,
    lbicrsIsTruncated,
    lbicrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkListBucketInventoryConfigurations' smart constructor.
data ListBucketInventoryConfigurations = ListBucketInventoryConfigurations'
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

-- | Creates a value of 'ListBucketInventoryConfigurations' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket containing the inventory configurations to retrieve.
-- * 'continuationToken' - The marker used to continue an inventory configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkListBucketInventoryConfigurations ::
  -- | 'bucket'
  BucketName ->
  ListBucketInventoryConfigurations
mkListBucketInventoryConfigurations pBucket_ =
  ListBucketInventoryConfigurations'
    { continuationToken =
        Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The marker used to continue an inventory configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicContinuationToken :: Lens.Lens' ListBucketInventoryConfigurations (Lude.Maybe Lude.Text)
lbicContinuationToken = Lens.lens (continuationToken :: ListBucketInventoryConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: ListBucketInventoryConfigurations)
{-# DEPRECATED lbicContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicExpectedBucketOwner :: Lens.Lens' ListBucketInventoryConfigurations (Lude.Maybe Lude.Text)
lbicExpectedBucketOwner = Lens.lens (expectedBucketOwner :: ListBucketInventoryConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: ListBucketInventoryConfigurations)
{-# DEPRECATED lbicExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket containing the inventory configurations to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicBucket :: Lens.Lens' ListBucketInventoryConfigurations BucketName
lbicBucket = Lens.lens (bucket :: ListBucketInventoryConfigurations -> BucketName) (\s a -> s {bucket = a} :: ListBucketInventoryConfigurations)
{-# DEPRECATED lbicBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest ListBucketInventoryConfigurations where
  type
    Rs ListBucketInventoryConfigurations =
      ListBucketInventoryConfigurationsResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListBucketInventoryConfigurationsResponse'
            Lude.<$> (x Lude..@? "ContinuationToken")
            Lude.<*> (Lude.may (Lude.parseXMLList "InventoryConfiguration") x)
            Lude.<*> (x Lude..@? "NextContinuationToken")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBucketInventoryConfigurations where
  toHeaders ListBucketInventoryConfigurations' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath ListBucketInventoryConfigurations where
  toPath ListBucketInventoryConfigurations' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery ListBucketInventoryConfigurations where
  toQuery ListBucketInventoryConfigurations' {..} =
    Lude.mconcat
      ["continuation-token" Lude.=: continuationToken, "inventory"]

-- | /See:/ 'mkListBucketInventoryConfigurationsResponse' smart constructor.
data ListBucketInventoryConfigurationsResponse = ListBucketInventoryConfigurationsResponse'
  { continuationToken ::
      Lude.Maybe
        Lude.Text,
    inventoryConfigurationList ::
      Lude.Maybe
        [InventoryConfiguration],
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
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBucketInventoryConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'continuationToken' - If sent in the request, the marker that is used as a starting point for this inventory configuration list response.
-- * 'inventoryConfigurationList' - The list of inventory configurations for a bucket.
-- * 'isTruncated' - Tells whether the returned list of inventory configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken is provided for a subsequent request.
-- * 'nextContinuationToken' - The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
-- * 'responseStatus' - The response status code.
mkListBucketInventoryConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBucketInventoryConfigurationsResponse
mkListBucketInventoryConfigurationsResponse pResponseStatus_ =
  ListBucketInventoryConfigurationsResponse'
    { continuationToken =
        Lude.Nothing,
      inventoryConfigurationList = Lude.Nothing,
      nextContinuationToken = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If sent in the request, the marker that is used as a starting point for this inventory configuration list response.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicrsContinuationToken :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Lude.Maybe Lude.Text)
lbicrsContinuationToken = Lens.lens (continuationToken :: ListBucketInventoryConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: ListBucketInventoryConfigurationsResponse)
{-# DEPRECATED lbicrsContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The list of inventory configurations for a bucket.
--
-- /Note:/ Consider using 'inventoryConfigurationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicrsInventoryConfigurationList :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Lude.Maybe [InventoryConfiguration])
lbicrsInventoryConfigurationList = Lens.lens (inventoryConfigurationList :: ListBucketInventoryConfigurationsResponse -> Lude.Maybe [InventoryConfiguration]) (\s a -> s {inventoryConfigurationList = a} :: ListBucketInventoryConfigurationsResponse)
{-# DEPRECATED lbicrsInventoryConfigurationList "Use generic-lens or generic-optics with 'inventoryConfigurationList' instead." #-}

-- | The marker used to continue this inventory configuration listing. Use the @NextContinuationToken@ from this response to continue the listing in a subsequent request. The continuation token is an opaque value that Amazon S3 understands.
--
-- /Note:/ Consider using 'nextContinuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicrsNextContinuationToken :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Lude.Maybe Lude.Text)
lbicrsNextContinuationToken = Lens.lens (nextContinuationToken :: ListBucketInventoryConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextContinuationToken = a} :: ListBucketInventoryConfigurationsResponse)
{-# DEPRECATED lbicrsNextContinuationToken "Use generic-lens or generic-optics with 'nextContinuationToken' instead." #-}

-- | Tells whether the returned list of inventory configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken is provided for a subsequent request.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicrsIsTruncated :: Lens.Lens' ListBucketInventoryConfigurationsResponse (Lude.Maybe Lude.Bool)
lbicrsIsTruncated = Lens.lens (isTruncated :: ListBucketInventoryConfigurationsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListBucketInventoryConfigurationsResponse)
{-# DEPRECATED lbicrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbicrsResponseStatus :: Lens.Lens' ListBucketInventoryConfigurationsResponse Lude.Int
lbicrsResponseStatus = Lens.lens (responseStatus :: ListBucketInventoryConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBucketInventoryConfigurationsResponse)
{-# DEPRECATED lbicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
