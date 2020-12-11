{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectLockConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Object Lock configuration for a bucket. The rule specified in the Object Lock configuration will be applied by default to every new object placed in the specified bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects> .
module Network.AWS.S3.GetObjectLockConfiguration
  ( -- * Creating a request
    GetObjectLockConfiguration (..),
    mkGetObjectLockConfiguration,

    -- ** Request lenses
    golcExpectedBucketOwner,
    golcBucket,

    -- * Destructuring the response
    GetObjectLockConfigurationResponse (..),
    mkGetObjectLockConfigurationResponse,

    -- ** Response lenses
    golcrsObjectLockConfiguration,
    golcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetObjectLockConfiguration' smart constructor.
data GetObjectLockConfiguration = GetObjectLockConfiguration'
  { expectedBucketOwner ::
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

-- | Creates a value of 'GetObjectLockConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket whose Object Lock configuration you want to retrieve.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetObjectLockConfiguration ::
  -- | 'bucket'
  BucketName ->
  GetObjectLockConfiguration
mkGetObjectLockConfiguration pBucket_ =
  GetObjectLockConfiguration'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golcExpectedBucketOwner :: Lens.Lens' GetObjectLockConfiguration (Lude.Maybe Lude.Text)
golcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetObjectLockConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetObjectLockConfiguration)
{-# DEPRECATED golcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket whose Object Lock configuration you want to retrieve.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golcBucket :: Lens.Lens' GetObjectLockConfiguration BucketName
golcBucket = Lens.lens (bucket :: GetObjectLockConfiguration -> BucketName) (\s a -> s {bucket = a} :: GetObjectLockConfiguration)
{-# DEPRECATED golcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest GetObjectLockConfiguration where
  type
    Rs GetObjectLockConfiguration =
      GetObjectLockConfigurationResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetObjectLockConfigurationResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetObjectLockConfiguration where
  toHeaders GetObjectLockConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetObjectLockConfiguration where
  toPath GetObjectLockConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetObjectLockConfiguration where
  toQuery = Lude.const (Lude.mconcat ["object-lock"])

-- | /See:/ 'mkGetObjectLockConfigurationResponse' smart constructor.
data GetObjectLockConfigurationResponse = GetObjectLockConfigurationResponse'
  { objectLockConfiguration ::
      Lude.Maybe
        ObjectLockConfiguration,
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

-- | Creates a value of 'GetObjectLockConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'objectLockConfiguration' - The specified bucket's Object Lock configuration.
-- * 'responseStatus' - The response status code.
mkGetObjectLockConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetObjectLockConfigurationResponse
mkGetObjectLockConfigurationResponse pResponseStatus_ =
  GetObjectLockConfigurationResponse'
    { objectLockConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The specified bucket's Object Lock configuration.
--
-- /Note:/ Consider using 'objectLockConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golcrsObjectLockConfiguration :: Lens.Lens' GetObjectLockConfigurationResponse (Lude.Maybe ObjectLockConfiguration)
golcrsObjectLockConfiguration = Lens.lens (objectLockConfiguration :: GetObjectLockConfigurationResponse -> Lude.Maybe ObjectLockConfiguration) (\s a -> s {objectLockConfiguration = a} :: GetObjectLockConfigurationResponse)
{-# DEPRECATED golcrsObjectLockConfiguration "Use generic-lens or generic-optics with 'objectLockConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golcrsResponseStatus :: Lens.Lens' GetObjectLockConfigurationResponse Lude.Int
golcrsResponseStatus = Lens.lens (responseStatus :: GetObjectLockConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetObjectLockConfigurationResponse)
{-# DEPRECATED golcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
