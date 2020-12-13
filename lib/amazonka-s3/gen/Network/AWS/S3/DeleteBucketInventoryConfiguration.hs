{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketInventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an inventory configuration (identified by the inventory ID) from the bucket.
--
-- To use this operation, you must have permissions to perform the @s3:PutInventoryConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about the Amazon S3 inventory feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-inventory.html Amazon S3 Inventory> .
-- Operations related to @DeleteBucketInventoryConfiguration@ include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketInventoryConfiguration.html GetBucketInventoryConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketInventoryConfiguration.html PutBucketInventoryConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketInventoryConfigurations.html ListBucketInventoryConfigurations>
module Network.AWS.S3.DeleteBucketInventoryConfiguration
  ( -- * Creating a request
    DeleteBucketInventoryConfiguration (..),
    mkDeleteBucketInventoryConfiguration,

    -- ** Request lenses
    dbicBucket,
    dbicId,
    dbicExpectedBucketOwner,

    -- * Destructuring the response
    DeleteBucketInventoryConfigurationResponse (..),
    mkDeleteBucketInventoryConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketInventoryConfiguration' smart constructor.
data DeleteBucketInventoryConfiguration = DeleteBucketInventoryConfiguration'
  { -- | The name of the bucket containing the inventory configuration to delete.
    bucket :: BucketName,
    -- | The ID used to identify the inventory configuration.
    id :: Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketInventoryConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket containing the inventory configuration to delete.
-- * 'id' - The ID used to identify the inventory configuration.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteBucketInventoryConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Lude.Text ->
  DeleteBucketInventoryConfiguration
mkDeleteBucketInventoryConfiguration pBucket_ pId_ =
  DeleteBucketInventoryConfiguration'
    { bucket = pBucket_,
      id = pId_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the bucket containing the inventory configuration to delete.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbicBucket :: Lens.Lens' DeleteBucketInventoryConfiguration BucketName
dbicBucket = Lens.lens (bucket :: DeleteBucketInventoryConfiguration -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketInventoryConfiguration)
{-# DEPRECATED dbicBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID used to identify the inventory configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbicId :: Lens.Lens' DeleteBucketInventoryConfiguration Lude.Text
dbicId = Lens.lens (id :: DeleteBucketInventoryConfiguration -> Lude.Text) (\s a -> s {id = a} :: DeleteBucketInventoryConfiguration)
{-# DEPRECATED dbicId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbicExpectedBucketOwner :: Lens.Lens' DeleteBucketInventoryConfiguration (Lude.Maybe Lude.Text)
dbicExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucketInventoryConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucketInventoryConfiguration)
{-# DEPRECATED dbicExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest DeleteBucketInventoryConfiguration where
  type
    Rs DeleteBucketInventoryConfiguration =
      DeleteBucketInventoryConfigurationResponse
  request = Req.delete s3Service
  response =
    Res.receiveNull DeleteBucketInventoryConfigurationResponse'

instance Lude.ToHeaders DeleteBucketInventoryConfiguration where
  toHeaders DeleteBucketInventoryConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucketInventoryConfiguration where
  toPath DeleteBucketInventoryConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketInventoryConfiguration where
  toQuery DeleteBucketInventoryConfiguration' {..} =
    Lude.mconcat ["id" Lude.=: id, "inventory"]

-- | /See:/ 'mkDeleteBucketInventoryConfigurationResponse' smart constructor.
data DeleteBucketInventoryConfigurationResponse = DeleteBucketInventoryConfigurationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketInventoryConfigurationResponse' with the minimum fields required to make a request.
mkDeleteBucketInventoryConfigurationResponse ::
  DeleteBucketInventoryConfigurationResponse
mkDeleteBucketInventoryConfigurationResponse =
  DeleteBucketInventoryConfigurationResponse'
