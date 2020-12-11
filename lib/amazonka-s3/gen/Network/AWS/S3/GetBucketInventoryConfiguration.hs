{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketInventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an inventory configuration (identified by the inventory configuration ID) from the bucket.
--
-- To use this operation, you must have permissions to perform the @s3:GetInventoryConfiguration@ action. The bucket owner has this permission by default and can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about the Amazon S3 inventory feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory> .
-- The following operations are related to @GetBucketInventoryConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketInventoryConfiguration.html DeleteBucketInventoryConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration>
module Network.AWS.S3.GetBucketInventoryConfiguration
  ( -- * Creating a request
    GetBucketInventoryConfiguration (..),
    mkGetBucketInventoryConfiguration,

    -- ** Request lenses
    gbicExpectedBucketOwner,
    gbicBucket,
    gbicId,

    -- * Destructuring the response
    GetBucketInventoryConfigurationResponse (..),
    mkGetBucketInventoryConfigurationResponse,

    -- ** Response lenses
    gbicrsInventoryConfiguration,
    gbicrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketInventoryConfiguration' smart constructor.
data GetBucketInventoryConfiguration = GetBucketInventoryConfiguration'
  { expectedBucketOwner ::
      Lude.Maybe Lude.Text,
    bucket :: BucketName,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketInventoryConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket containing the inventory configuration to retrieve.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'id' - The ID used to identify the inventory configuration.
mkGetBucketInventoryConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Lude.Text ->
  GetBucketInventoryConfiguration
mkGetBucketInventoryConfiguration pBucket_ pId_ =
  GetBucketInventoryConfiguration'
    { expectedBucketOwner =
        Lude.Nothing,
      bucket = pBucket_,
      id = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbicExpectedBucketOwner :: Lens.Lens' GetBucketInventoryConfiguration (Lude.Maybe Lude.Text)
gbicExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketInventoryConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketInventoryConfiguration)
{-# DEPRECATED gbicExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket containing the inventory configuration to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbicBucket :: Lens.Lens' GetBucketInventoryConfiguration BucketName
gbicBucket = Lens.lens (bucket :: GetBucketInventoryConfiguration -> BucketName) (\s a -> s {bucket = a} :: GetBucketInventoryConfiguration)
{-# DEPRECATED gbicBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID used to identify the inventory configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbicId :: Lens.Lens' GetBucketInventoryConfiguration Lude.Text
gbicId = Lens.lens (id :: GetBucketInventoryConfiguration -> Lude.Text) (\s a -> s {id = a} :: GetBucketInventoryConfiguration)
{-# DEPRECATED gbicId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetBucketInventoryConfiguration where
  type
    Rs GetBucketInventoryConfiguration =
      GetBucketInventoryConfigurationResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketInventoryConfigurationResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketInventoryConfiguration where
  toHeaders GetBucketInventoryConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketInventoryConfiguration where
  toPath GetBucketInventoryConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketInventoryConfiguration where
  toQuery GetBucketInventoryConfiguration' {..} =
    Lude.mconcat ["id" Lude.=: id, "inventory"]

-- | /See:/ 'mkGetBucketInventoryConfigurationResponse' smart constructor.
data GetBucketInventoryConfigurationResponse = GetBucketInventoryConfigurationResponse'
  { inventoryConfiguration ::
      Lude.Maybe
        InventoryConfiguration,
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

-- | Creates a value of 'GetBucketInventoryConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'inventoryConfiguration' - Specifies the inventory configuration.
-- * 'responseStatus' - The response status code.
mkGetBucketInventoryConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketInventoryConfigurationResponse
mkGetBucketInventoryConfigurationResponse pResponseStatus_ =
  GetBucketInventoryConfigurationResponse'
    { inventoryConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Specifies the inventory configuration.
--
-- /Note:/ Consider using 'inventoryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbicrsInventoryConfiguration :: Lens.Lens' GetBucketInventoryConfigurationResponse (Lude.Maybe InventoryConfiguration)
gbicrsInventoryConfiguration = Lens.lens (inventoryConfiguration :: GetBucketInventoryConfigurationResponse -> Lude.Maybe InventoryConfiguration) (\s a -> s {inventoryConfiguration = a} :: GetBucketInventoryConfigurationResponse)
{-# DEPRECATED gbicrsInventoryConfiguration "Use generic-lens or generic-optics with 'inventoryConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbicrsResponseStatus :: Lens.Lens' GetBucketInventoryConfigurationResponse Lude.Int
gbicrsResponseStatus = Lens.lens (responseStatus :: GetBucketInventoryConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketInventoryConfigurationResponse)
{-# DEPRECATED gbicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
