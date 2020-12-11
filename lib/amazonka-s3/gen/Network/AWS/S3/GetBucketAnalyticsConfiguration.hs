{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the GET operation returns an analytics configuration (identified by the analytics configuration ID) from the bucket.
--
-- To use this operation, you must have permissions to perform the @s3:GetAnalyticsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> in the /Amazon Simple Storage Service Developer Guide/ .
-- For information about Amazon S3 analytics feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis> in the /Amazon Simple Storage Service Developer Guide/ .
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketAnalyticsConfiguration.html DeleteBucketAnalyticsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration>
module Network.AWS.S3.GetBucketAnalyticsConfiguration
  ( -- * Creating a request
    GetBucketAnalyticsConfiguration (..),
    mkGetBucketAnalyticsConfiguration,

    -- ** Request lenses
    getExpectedBucketOwner,
    getBucket,
    getId,

    -- * Destructuring the response
    GetBucketAnalyticsConfigurationResponse (..),
    mkGetBucketAnalyticsConfigurationResponse,

    -- ** Response lenses
    gbacrsAnalyticsConfiguration,
    gbacrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketAnalyticsConfiguration' smart constructor.
data GetBucketAnalyticsConfiguration = GetBucketAnalyticsConfiguration'
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

-- | Creates a value of 'GetBucketAnalyticsConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket from which an analytics configuration is retrieved.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'id' - The ID that identifies the analytics configuration.
mkGetBucketAnalyticsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Lude.Text ->
  GetBucketAnalyticsConfiguration
mkGetBucketAnalyticsConfiguration pBucket_ pId_ =
  GetBucketAnalyticsConfiguration'
    { expectedBucketOwner =
        Lude.Nothing,
      bucket = pBucket_,
      id = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getExpectedBucketOwner :: Lens.Lens' GetBucketAnalyticsConfiguration (Lude.Maybe Lude.Text)
getExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketAnalyticsConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketAnalyticsConfiguration)
{-# DEPRECATED getExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket from which an analytics configuration is retrieved.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getBucket :: Lens.Lens' GetBucketAnalyticsConfiguration BucketName
getBucket = Lens.lens (bucket :: GetBucketAnalyticsConfiguration -> BucketName) (\s a -> s {bucket = a} :: GetBucketAnalyticsConfiguration)
{-# DEPRECATED getBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID that identifies the analytics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getId :: Lens.Lens' GetBucketAnalyticsConfiguration Lude.Text
getId = Lens.lens (id :: GetBucketAnalyticsConfiguration -> Lude.Text) (\s a -> s {id = a} :: GetBucketAnalyticsConfiguration)
{-# DEPRECATED getId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetBucketAnalyticsConfiguration where
  type
    Rs GetBucketAnalyticsConfiguration =
      GetBucketAnalyticsConfigurationResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketAnalyticsConfigurationResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketAnalyticsConfiguration where
  toHeaders GetBucketAnalyticsConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketAnalyticsConfiguration where
  toPath GetBucketAnalyticsConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketAnalyticsConfiguration where
  toQuery GetBucketAnalyticsConfiguration' {..} =
    Lude.mconcat ["id" Lude.=: id, "analytics"]

-- | /See:/ 'mkGetBucketAnalyticsConfigurationResponse' smart constructor.
data GetBucketAnalyticsConfigurationResponse = GetBucketAnalyticsConfigurationResponse'
  { analyticsConfiguration ::
      Lude.Maybe
        AnalyticsConfiguration,
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

-- | Creates a value of 'GetBucketAnalyticsConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'analyticsConfiguration' - The configuration and any analyses for the analytics filter.
-- * 'responseStatus' - The response status code.
mkGetBucketAnalyticsConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketAnalyticsConfigurationResponse
mkGetBucketAnalyticsConfigurationResponse pResponseStatus_ =
  GetBucketAnalyticsConfigurationResponse'
    { analyticsConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The configuration and any analyses for the analytics filter.
--
-- /Note:/ Consider using 'analyticsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacrsAnalyticsConfiguration :: Lens.Lens' GetBucketAnalyticsConfigurationResponse (Lude.Maybe AnalyticsConfiguration)
gbacrsAnalyticsConfiguration = Lens.lens (analyticsConfiguration :: GetBucketAnalyticsConfigurationResponse -> Lude.Maybe AnalyticsConfiguration) (\s a -> s {analyticsConfiguration = a} :: GetBucketAnalyticsConfigurationResponse)
{-# DEPRECATED gbacrsAnalyticsConfiguration "Use generic-lens or generic-optics with 'analyticsConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacrsResponseStatus :: Lens.Lens' GetBucketAnalyticsConfigurationResponse Lude.Int
gbacrsResponseStatus = Lens.lens (responseStatus :: GetBucketAnalyticsConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketAnalyticsConfigurationResponse)
{-# DEPRECATED gbacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
