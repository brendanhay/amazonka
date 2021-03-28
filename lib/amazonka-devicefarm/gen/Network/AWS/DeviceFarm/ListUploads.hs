{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListUploads
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about uploads, given an AWS Device Farm project ARN.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListUploads
    (
    -- * Creating a request
      ListUploads (..)
    , mkListUploads
    -- ** Request lenses
    , luArn
    , luNextToken
    , luType

    -- * Destructuring the response
    , ListUploadsResponse (..)
    , mkListUploadsResponse
    -- ** Response lenses
    , lurrsNextToken
    , lurrsUploads
    , lurrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list uploads operation.
--
-- /See:/ 'mkListUploads' smart constructor.
data ListUploads = ListUploads'
  { arn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the project for which you want to list uploads.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  , type' :: Core.Maybe Types.UploadType
    -- ^ The type of upload.
--
-- Must be one of the following values:
--
--     * ANDROID_APP
--
--
--     * IOS_APP
--
--
--     * WEB_APP
--
--
--     * EXTERNAL_DATA
--
--
--     * APPIUM_JAVA_JUNIT_TEST_PACKAGE
--
--
--     * APPIUM_JAVA_TESTNG_TEST_PACKAGE
--
--
--     * APPIUM_PYTHON_TEST_PACKAGE
--
--
--     * APPIUM_NODE_TEST_PACKAGE
--
--
--     * APPIUM_RUBY_TEST_PACKAGE
--
--
--     * APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE
--
--
--     * APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE
--
--
--     * APPIUM_WEB_PYTHON_TEST_PACKAGE
--
--
--     * APPIUM_WEB_NODE_TEST_PACKAGE
--
--
--     * APPIUM_WEB_RUBY_TEST_PACKAGE
--
--
--     * CALABASH_TEST_PACKAGE
--
--
--     * INSTRUMENTATION_TEST_PACKAGE
--
--
--     * UIAUTOMATION_TEST_PACKAGE
--
--
--     * UIAUTOMATOR_TEST_PACKAGE
--
--
--     * XCTEST_TEST_PACKAGE
--
--
--     * XCTEST_UI_TEST_PACKAGE
--
--
--     * APPIUM_JAVA_JUNIT_TEST_SPEC
--
--
--     * APPIUM_JAVA_TESTNG_TEST_SPEC
--
--
--     * APPIUM_PYTHON_TEST_SPEC
--
--
--     * APPIUM_NODE_TEST_SPEC
--
--
--     * APPIUM_RUBY_TEST_SPEC
--
--
--     * APPIUM_WEB_JAVA_JUNIT_TEST_SPEC
--
--
--     * APPIUM_WEB_JAVA_TESTNG_TEST_SPEC
--
--
--     * APPIUM_WEB_PYTHON_TEST_SPEC
--
--
--     * APPIUM_WEB_NODE_TEST_SPEC
--
--
--     * APPIUM_WEB_RUBY_TEST_SPEC
--
--
--     * INSTRUMENTATION_TEST_SPEC
--
--
--     * XCTEST_UI_TEST_SPEC
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUploads' value with any optional fields omitted.
mkListUploads
    :: Types.Arn -- ^ 'arn'
    -> ListUploads
mkListUploads arn
  = ListUploads'{arn, nextToken = Core.Nothing, type' = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the project for which you want to list uploads.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luArn :: Lens.Lens' ListUploads Types.Arn
luArn = Lens.field @"arn"
{-# INLINEABLE luArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luNextToken :: Lens.Lens' ListUploads (Core.Maybe Types.PaginationToken)
luNextToken = Lens.field @"nextToken"
{-# INLINEABLE luNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The type of upload.
--
-- Must be one of the following values:
--
--     * ANDROID_APP
--
--
--     * IOS_APP
--
--
--     * WEB_APP
--
--
--     * EXTERNAL_DATA
--
--
--     * APPIUM_JAVA_JUNIT_TEST_PACKAGE
--
--
--     * APPIUM_JAVA_TESTNG_TEST_PACKAGE
--
--
--     * APPIUM_PYTHON_TEST_PACKAGE
--
--
--     * APPIUM_NODE_TEST_PACKAGE
--
--
--     * APPIUM_RUBY_TEST_PACKAGE
--
--
--     * APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE
--
--
--     * APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE
--
--
--     * APPIUM_WEB_PYTHON_TEST_PACKAGE
--
--
--     * APPIUM_WEB_NODE_TEST_PACKAGE
--
--
--     * APPIUM_WEB_RUBY_TEST_PACKAGE
--
--
--     * CALABASH_TEST_PACKAGE
--
--
--     * INSTRUMENTATION_TEST_PACKAGE
--
--
--     * UIAUTOMATION_TEST_PACKAGE
--
--
--     * UIAUTOMATOR_TEST_PACKAGE
--
--
--     * XCTEST_TEST_PACKAGE
--
--
--     * XCTEST_UI_TEST_PACKAGE
--
--
--     * APPIUM_JAVA_JUNIT_TEST_SPEC
--
--
--     * APPIUM_JAVA_TESTNG_TEST_SPEC
--
--
--     * APPIUM_PYTHON_TEST_SPEC
--
--
--     * APPIUM_NODE_TEST_SPEC
--
--
--     * APPIUM_RUBY_TEST_SPEC
--
--
--     * APPIUM_WEB_JAVA_JUNIT_TEST_SPEC
--
--
--     * APPIUM_WEB_JAVA_TESTNG_TEST_SPEC
--
--
--     * APPIUM_WEB_PYTHON_TEST_SPEC
--
--
--     * APPIUM_WEB_NODE_TEST_SPEC
--
--
--     * APPIUM_WEB_RUBY_TEST_SPEC
--
--
--     * INSTRUMENTATION_TEST_SPEC
--
--
--     * XCTEST_UI_TEST_SPEC
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luType :: Lens.Lens' ListUploads (Core.Maybe Types.UploadType)
luType = Lens.field @"type'"
{-# INLINEABLE luType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery ListUploads where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListUploads where
        toHeaders ListUploads{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.ListUploads")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListUploads where
        toJSON ListUploads{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("arn" Core..= arn),
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("type" Core..=) Core.<$> type'])

instance Core.AWSRequest ListUploads where
        type Rs ListUploads = ListUploadsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListUploadsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "uploads" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListUploads where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"uploads" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the result of a list uploads request.
--
-- /See:/ 'mkListUploadsResponse' smart constructor.
data ListUploadsResponse = ListUploadsResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
  , uploads :: Core.Maybe [Types.Upload]
    -- ^ Information about the uploads.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListUploadsResponse' value with any optional fields omitted.
mkListUploadsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListUploadsResponse
mkListUploadsResponse responseStatus
  = ListUploadsResponse'{nextToken = Core.Nothing,
                         uploads = Core.Nothing, responseStatus}

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsNextToken :: Lens.Lens' ListUploadsResponse (Core.Maybe Types.PaginationToken)
lurrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lurrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the uploads.
--
-- /Note:/ Consider using 'uploads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsUploads :: Lens.Lens' ListUploadsResponse (Core.Maybe [Types.Upload])
lurrsUploads = Lens.field @"uploads"
{-# INLINEABLE lurrsUploads #-}
{-# DEPRECATED uploads "Use generic-lens or generic-optics with 'uploads' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lurrsResponseStatus :: Lens.Lens' ListUploadsResponse Core.Int
lurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
