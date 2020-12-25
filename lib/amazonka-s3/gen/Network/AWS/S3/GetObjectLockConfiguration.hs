{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    golcBucket,
    golcExpectedBucketOwner,

    -- * Destructuring the response
    GetObjectLockConfigurationResponse (..),
    mkGetObjectLockConfigurationResponse,

    -- ** Response lenses
    golcrrsObjectLockConfiguration,
    golcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetObjectLockConfiguration' smart constructor.
data GetObjectLockConfiguration = GetObjectLockConfiguration'
  { -- | The bucket whose Object Lock configuration you want to retrieve.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectLockConfiguration' value with any optional fields omitted.
mkGetObjectLockConfiguration ::
  -- | 'bucket'
  Types.BucketName ->
  GetObjectLockConfiguration
mkGetObjectLockConfiguration bucket =
  GetObjectLockConfiguration'
    { bucket,
      expectedBucketOwner = Core.Nothing
    }

-- | The bucket whose Object Lock configuration you want to retrieve.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golcBucket :: Lens.Lens' GetObjectLockConfiguration Types.BucketName
golcBucket = Lens.field @"bucket"
{-# DEPRECATED golcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golcExpectedBucketOwner :: Lens.Lens' GetObjectLockConfiguration (Core.Maybe Types.AccountId)
golcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED golcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetObjectLockConfiguration where
  type
    Rs GetObjectLockConfiguration =
      GetObjectLockConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("object-lock", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetObjectLockConfigurationResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetObjectLockConfigurationResponse' smart constructor.
data GetObjectLockConfigurationResponse = GetObjectLockConfigurationResponse'
  { -- | The specified bucket's Object Lock configuration.
    objectLockConfiguration :: Core.Maybe Types.ObjectLockConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectLockConfigurationResponse' value with any optional fields omitted.
mkGetObjectLockConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetObjectLockConfigurationResponse
mkGetObjectLockConfigurationResponse responseStatus =
  GetObjectLockConfigurationResponse'
    { objectLockConfiguration =
        Core.Nothing,
      responseStatus
    }

-- | The specified bucket's Object Lock configuration.
--
-- /Note:/ Consider using 'objectLockConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golcrrsObjectLockConfiguration :: Lens.Lens' GetObjectLockConfigurationResponse (Core.Maybe Types.ObjectLockConfiguration)
golcrrsObjectLockConfiguration = Lens.field @"objectLockConfiguration"
{-# DEPRECATED golcrrsObjectLockConfiguration "Use generic-lens or generic-optics with 'objectLockConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
golcrrsResponseStatus :: Lens.Lens' GetObjectLockConfigurationResponse Core.Int
golcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED golcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
