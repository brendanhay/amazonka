{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the logging status of a bucket and the permissions users have to view and modify that status. To use GET, you must be the bucket owner.
--
-- The following operations are related to @GetBucketLogging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLogging.html PutBucketLogging>
module Network.AWS.S3.GetBucketLogging
  ( -- * Creating a request
    GetBucketLogging (..),
    mkGetBucketLogging,

    -- ** Request lenses
    gBucket,
    gExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketLoggingResponse (..),
    mkGetBucketLoggingResponse,

    -- ** Response lenses
    grsLoggingEnabled,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketLogging' smart constructor.
data GetBucketLogging = GetBucketLogging'
  { -- | The bucket name for which to get the logging information.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketLogging' value with any optional fields omitted.
mkGetBucketLogging ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketLogging
mkGetBucketLogging bucket =
  GetBucketLogging' {bucket, expectedBucketOwner = Core.Nothing}

-- | The bucket name for which to get the logging information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gBucket :: Lens.Lens' GetBucketLogging Types.BucketName
gBucket = Lens.field @"bucket"
{-# DEPRECATED gBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gExpectedBucketOwner :: Lens.Lens' GetBucketLogging (Core.Maybe Types.AccountId)
gExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketLogging where
  type Rs GetBucketLogging = GetBucketLoggingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("logging", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketLoggingResponse'
            Core.<$> (x Core..@? "LoggingEnabled")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketLoggingResponse' smart constructor.
data GetBucketLoggingResponse = GetBucketLoggingResponse'
  { loggingEnabled :: Core.Maybe Types.LoggingEnabled,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketLoggingResponse' value with any optional fields omitted.
mkGetBucketLoggingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBucketLoggingResponse
mkGetBucketLoggingResponse responseStatus =
  GetBucketLoggingResponse'
    { loggingEnabled = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'loggingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsLoggingEnabled :: Lens.Lens' GetBucketLoggingResponse (Core.Maybe Types.LoggingEnabled)
grsLoggingEnabled = Lens.field @"loggingEnabled"
{-# DEPRECATED grsLoggingEnabled "Use generic-lens or generic-optics with 'loggingEnabled' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetBucketLoggingResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
