{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketLifecycleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the lifecycle configuration information set on the bucket. For information about lifecycle configuration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management> .
--
-- To use this operation, you must have permission to perform the @s3:GetLifecycleConfiguration@ action. The bucket owner has this permission, by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- @GetBucketLifecycleConfiguration@ has the following special error:
--
--     * Error code: @NoSuchLifecycleConfiguration@
--
--     * Description: The lifecycle configuration does not exist.
--
--
--     * HTTP Status Code: 404 Not Found
--
--
--     * SOAP Fault Code Prefix: Client
--
--
--
--
-- The following operations are related to @GetBucketLifecycleConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycle.html GetBucketLifecycle>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycle.html PutBucketLifecycle>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketLifecycle.html DeleteBucketLifecycle>
module Network.AWS.S3.GetBucketLifecycleConfiguration
  ( -- * Creating a request
    GetBucketLifecycleConfiguration (..),
    mkGetBucketLifecycleConfiguration,

    -- ** Request lenses
    gblcBucket,
    gblcExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketLifecycleConfigurationResponse (..),
    mkGetBucketLifecycleConfigurationResponse,

    -- ** Response lenses
    gblcrsRules,
    gblcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketLifecycleConfiguration' smart constructor.
data GetBucketLifecycleConfiguration = GetBucketLifecycleConfiguration'
  { -- | The name of the bucket for which to get the lifecycle information.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketLifecycleConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket for which to get the lifecycle information.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketLifecycleConfiguration ::
  -- | 'bucket'
  BucketName ->
  GetBucketLifecycleConfiguration
mkGetBucketLifecycleConfiguration pBucket_ =
  GetBucketLifecycleConfiguration'
    { bucket = pBucket_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the bucket for which to get the lifecycle information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblcBucket :: Lens.Lens' GetBucketLifecycleConfiguration BucketName
gblcBucket = Lens.lens (bucket :: GetBucketLifecycleConfiguration -> BucketName) (\s a -> s {bucket = a} :: GetBucketLifecycleConfiguration)
{-# DEPRECATED gblcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblcExpectedBucketOwner :: Lens.Lens' GetBucketLifecycleConfiguration (Lude.Maybe Lude.Text)
gblcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketLifecycleConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketLifecycleConfiguration)
{-# DEPRECATED gblcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest GetBucketLifecycleConfiguration where
  type
    Rs GetBucketLifecycleConfiguration =
      GetBucketLifecycleConfigurationResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketLifecycleConfigurationResponse'
            Lude.<$> (Lude.may (Lude.parseXMLList "Rule") x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketLifecycleConfiguration where
  toHeaders GetBucketLifecycleConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketLifecycleConfiguration where
  toPath GetBucketLifecycleConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketLifecycleConfiguration where
  toQuery = Lude.const (Lude.mconcat ["lifecycle"])

-- | /See:/ 'mkGetBucketLifecycleConfigurationResponse' smart constructor.
data GetBucketLifecycleConfigurationResponse = GetBucketLifecycleConfigurationResponse'
  { -- | Container for a lifecycle rule.
    rules :: Lude.Maybe [LifecycleRule],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketLifecycleConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'rules' - Container for a lifecycle rule.
-- * 'responseStatus' - The response status code.
mkGetBucketLifecycleConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketLifecycleConfigurationResponse
mkGetBucketLifecycleConfigurationResponse pResponseStatus_ =
  GetBucketLifecycleConfigurationResponse'
    { rules = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Container for a lifecycle rule.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblcrsRules :: Lens.Lens' GetBucketLifecycleConfigurationResponse (Lude.Maybe [LifecycleRule])
gblcrsRules = Lens.lens (rules :: GetBucketLifecycleConfigurationResponse -> Lude.Maybe [LifecycleRule]) (\s a -> s {rules = a} :: GetBucketLifecycleConfigurationResponse)
{-# DEPRECATED gblcrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblcrsResponseStatus :: Lens.Lens' GetBucketLifecycleConfigurationResponse Lude.Int
gblcrsResponseStatus = Lens.lens (responseStatus :: GetBucketLifecycleConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketLifecycleConfigurationResponse)
{-# DEPRECATED gblcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
