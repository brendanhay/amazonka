{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketMetricsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a metrics configuration for the Amazon CloudWatch request metrics (specified by the metrics configuration ID) from the bucket. Note that this doesn't include the daily storage metrics.
--
-- To use this operation, you must have permissions to perform the @s3:PutMetricsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about CloudWatch request metrics for Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch> .
-- The following operations are related to @DeleteBucketMetricsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketMetricsConfiguration.html GetBucketMetricsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketMetricsConfigurations.html ListBucketMetricsConfigurations>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch>
module Network.AWS.S3.DeleteBucketMetricsConfiguration
  ( -- * Creating a request
    DeleteBucketMetricsConfiguration (..),
    mkDeleteBucketMetricsConfiguration,

    -- ** Request lenses
    dbmcExpectedBucketOwner,
    dbmcBucket,
    dbmcId,

    -- * Destructuring the response
    DeleteBucketMetricsConfigurationResponse (..),
    mkDeleteBucketMetricsConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketMetricsConfiguration' smart constructor.
data DeleteBucketMetricsConfiguration = DeleteBucketMetricsConfiguration'
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

-- | Creates a value of 'DeleteBucketMetricsConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket containing the metrics configuration to delete.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'id' - The ID used to identify the metrics configuration.
mkDeleteBucketMetricsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Lude.Text ->
  DeleteBucketMetricsConfiguration
mkDeleteBucketMetricsConfiguration pBucket_ pId_ =
  DeleteBucketMetricsConfiguration'
    { expectedBucketOwner =
        Lude.Nothing,
      bucket = pBucket_,
      id = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmcExpectedBucketOwner :: Lens.Lens' DeleteBucketMetricsConfiguration (Lude.Maybe Lude.Text)
dbmcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucketMetricsConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucketMetricsConfiguration)
{-# DEPRECATED dbmcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket containing the metrics configuration to delete.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmcBucket :: Lens.Lens' DeleteBucketMetricsConfiguration BucketName
dbmcBucket = Lens.lens (bucket :: DeleteBucketMetricsConfiguration -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketMetricsConfiguration)
{-# DEPRECATED dbmcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID used to identify the metrics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmcId :: Lens.Lens' DeleteBucketMetricsConfiguration Lude.Text
dbmcId = Lens.lens (id :: DeleteBucketMetricsConfiguration -> Lude.Text) (\s a -> s {id = a} :: DeleteBucketMetricsConfiguration)
{-# DEPRECATED dbmcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteBucketMetricsConfiguration where
  type
    Rs DeleteBucketMetricsConfiguration =
      DeleteBucketMetricsConfigurationResponse
  request = Req.delete s3Service
  response =
    Res.receiveNull DeleteBucketMetricsConfigurationResponse'

instance Lude.ToHeaders DeleteBucketMetricsConfiguration where
  toHeaders DeleteBucketMetricsConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucketMetricsConfiguration where
  toPath DeleteBucketMetricsConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketMetricsConfiguration where
  toQuery DeleteBucketMetricsConfiguration' {..} =
    Lude.mconcat ["id" Lude.=: id, "metrics"]

-- | /See:/ 'mkDeleteBucketMetricsConfigurationResponse' smart constructor.
data DeleteBucketMetricsConfigurationResponse = DeleteBucketMetricsConfigurationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketMetricsConfigurationResponse' with the minimum fields required to make a request.
mkDeleteBucketMetricsConfigurationResponse ::
  DeleteBucketMetricsConfigurationResponse
mkDeleteBucketMetricsConfigurationResponse =
  DeleteBucketMetricsConfigurationResponse'
