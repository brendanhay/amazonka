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
    gblcrrsRules,
    gblcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketLifecycleConfiguration' smart constructor.
data GetBucketLifecycleConfiguration = GetBucketLifecycleConfiguration'
  { -- | The name of the bucket for which to get the lifecycle information.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketLifecycleConfiguration' value with any optional fields omitted.
mkGetBucketLifecycleConfiguration ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketLifecycleConfiguration
mkGetBucketLifecycleConfiguration bucket =
  GetBucketLifecycleConfiguration'
    { bucket,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the bucket for which to get the lifecycle information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblcBucket :: Lens.Lens' GetBucketLifecycleConfiguration Types.BucketName
gblcBucket = Lens.field @"bucket"
{-# DEPRECATED gblcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblcExpectedBucketOwner :: Lens.Lens' GetBucketLifecycleConfiguration (Core.Maybe Types.AccountId)
gblcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gblcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketLifecycleConfiguration where
  type
    Rs GetBucketLifecycleConfiguration =
      GetBucketLifecycleConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("lifecycle", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketLifecycleConfigurationResponse'
            Core.<$> (x Core..@? "Rule") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketLifecycleConfigurationResponse' smart constructor.
data GetBucketLifecycleConfigurationResponse = GetBucketLifecycleConfigurationResponse'
  { -- | Container for a lifecycle rule.
    rules :: Core.Maybe [Types.LifecycleRule],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetBucketLifecycleConfigurationResponse' value with any optional fields omitted.
mkGetBucketLifecycleConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBucketLifecycleConfigurationResponse
mkGetBucketLifecycleConfigurationResponse responseStatus =
  GetBucketLifecycleConfigurationResponse'
    { rules = Core.Nothing,
      responseStatus
    }

-- | Container for a lifecycle rule.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblcrrsRules :: Lens.Lens' GetBucketLifecycleConfigurationResponse (Core.Maybe [Types.LifecycleRule])
gblcrrsRules = Lens.field @"rules"
{-# DEPRECATED gblcrrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblcrrsResponseStatus :: Lens.Lens' GetBucketLifecycleConfigurationResponse Core.Int
gblcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gblcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
