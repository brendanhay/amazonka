{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an analytics configuration for the bucket (specified by the analytics configuration ID).
--
-- To use this operation, you must have permissions to perform the @s3:PutAnalyticsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about the Amazon S3 analytics feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis> .
-- The following operations are related to @DeleteBucketAnalyticsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAnalyticsConfiguration.html GetBucketAnalyticsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration>
module Network.AWS.S3.DeleteBucketAnalyticsConfiguration
  ( -- * Creating a request
    DeleteBucketAnalyticsConfiguration (..),
    mkDeleteBucketAnalyticsConfiguration,

    -- ** Request lenses
    dbacBucket,
    dbacId,
    dbacExpectedBucketOwner,

    -- * Destructuring the response
    DeleteBucketAnalyticsConfigurationResponse (..),
    mkDeleteBucketAnalyticsConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketAnalyticsConfiguration' smart constructor.
data DeleteBucketAnalyticsConfiguration = DeleteBucketAnalyticsConfiguration'
  { -- | The name of the bucket from which an analytics configuration is deleted.
    bucket :: BucketName,
    -- | The ID that identifies the analytics configuration.
    id :: Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketAnalyticsConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket from which an analytics configuration is deleted.
-- * 'id' - The ID that identifies the analytics configuration.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteBucketAnalyticsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Lude.Text ->
  DeleteBucketAnalyticsConfiguration
mkDeleteBucketAnalyticsConfiguration pBucket_ pId_ =
  DeleteBucketAnalyticsConfiguration'
    { bucket = pBucket_,
      id = pId_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the bucket from which an analytics configuration is deleted.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbacBucket :: Lens.Lens' DeleteBucketAnalyticsConfiguration BucketName
dbacBucket = Lens.lens (bucket :: DeleteBucketAnalyticsConfiguration -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketAnalyticsConfiguration)
{-# DEPRECATED dbacBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID that identifies the analytics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbacId :: Lens.Lens' DeleteBucketAnalyticsConfiguration Lude.Text
dbacId = Lens.lens (id :: DeleteBucketAnalyticsConfiguration -> Lude.Text) (\s a -> s {id = a} :: DeleteBucketAnalyticsConfiguration)
{-# DEPRECATED dbacId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbacExpectedBucketOwner :: Lens.Lens' DeleteBucketAnalyticsConfiguration (Lude.Maybe Lude.Text)
dbacExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucketAnalyticsConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucketAnalyticsConfiguration)
{-# DEPRECATED dbacExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest DeleteBucketAnalyticsConfiguration where
  type
    Rs DeleteBucketAnalyticsConfiguration =
      DeleteBucketAnalyticsConfigurationResponse
  request = Req.delete s3Service
  response =
    Res.receiveNull DeleteBucketAnalyticsConfigurationResponse'

instance Lude.ToHeaders DeleteBucketAnalyticsConfiguration where
  toHeaders DeleteBucketAnalyticsConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucketAnalyticsConfiguration where
  toPath DeleteBucketAnalyticsConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketAnalyticsConfiguration where
  toQuery DeleteBucketAnalyticsConfiguration' {..} =
    Lude.mconcat ["id" Lude.=: id, "analytics"]

-- | /See:/ 'mkDeleteBucketAnalyticsConfigurationResponse' smart constructor.
data DeleteBucketAnalyticsConfigurationResponse = DeleteBucketAnalyticsConfigurationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketAnalyticsConfigurationResponse' with the minimum fields required to make a request.
mkDeleteBucketAnalyticsConfigurationResponse ::
  DeleteBucketAnalyticsConfigurationResponse
mkDeleteBucketAnalyticsConfigurationResponse =
  DeleteBucketAnalyticsConfigurationResponse'
