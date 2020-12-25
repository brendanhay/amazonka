{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an app or test scripts.
module Network.AWS.DeviceFarm.CreateUpload
  ( -- * Creating a request
    CreateUpload (..),
    mkCreateUpload,

    -- ** Request lenses
    cuProjectArn,
    cuName,
    cuType,
    cuContentType,

    -- * Destructuring the response
    CreateUploadResponse (..),
    mkCreateUploadResponse,

    -- ** Response lenses
    currsUpload,
    currsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the create upload operation.
--
-- /See:/ 'mkCreateUpload' smart constructor.
data CreateUpload = CreateUpload'
  { -- | The ARN of the project for the upload.
    projectArn :: Types.AmazonResourceName,
    -- | The upload's file name. The name should not contain any forward slashes (@/@ ). If you are uploading an iOS app, the file name must end with the @.ipa@ extension. If you are uploading an Android app, the file name must end with the @.apk@ extension. For all others, the file name must end with the @.zip@ file extension.
    name :: Types.Name,
    -- | The upload's upload type.
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
    -- If you call @CreateUpload@ with @WEB_APP@ specified, AWS Device Farm throws an @ArgumentException@ error.
    type' :: Types.UploadType,
    -- | The upload's content type (for example, @application/octet-stream@ ).
    contentType :: Core.Maybe Types.ContentType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUpload' value with any optional fields omitted.
mkCreateUpload ::
  -- | 'projectArn'
  Types.AmazonResourceName ->
  -- | 'name'
  Types.Name ->
  -- | 'type\''
  Types.UploadType ->
  CreateUpload
mkCreateUpload projectArn name type' =
  CreateUpload'
    { projectArn,
      name,
      type',
      contentType = Core.Nothing
    }

-- | The ARN of the project for the upload.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuProjectArn :: Lens.Lens' CreateUpload Types.AmazonResourceName
cuProjectArn = Lens.field @"projectArn"
{-# DEPRECATED cuProjectArn "Use generic-lens or generic-optics with 'projectArn' instead." #-}

-- | The upload's file name. The name should not contain any forward slashes (@/@ ). If you are uploading an iOS app, the file name must end with the @.ipa@ extension. If you are uploading an Android app, the file name must end with the @.apk@ extension. For all others, the file name must end with the @.zip@ file extension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuName :: Lens.Lens' CreateUpload Types.Name
cuName = Lens.field @"name"
{-# DEPRECATED cuName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The upload's upload type.
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
-- If you call @CreateUpload@ with @WEB_APP@ specified, AWS Device Farm throws an @ArgumentException@ error.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuType :: Lens.Lens' CreateUpload Types.UploadType
cuType = Lens.field @"type'"
{-# DEPRECATED cuType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The upload's content type (for example, @application/octet-stream@ ).
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuContentType :: Lens.Lens' CreateUpload (Core.Maybe Types.ContentType)
cuContentType = Lens.field @"contentType"
{-# DEPRECATED cuContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Core.FromJSON CreateUpload where
  toJSON CreateUpload {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectArn" Core..= projectArn),
            Core.Just ("name" Core..= name),
            Core.Just ("type" Core..= type'),
            ("contentType" Core..=) Core.<$> contentType
          ]
      )

instance Core.AWSRequest CreateUpload where
  type Rs CreateUpload = CreateUploadResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.CreateUpload")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUploadResponse'
            Core.<$> (x Core..:? "upload") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of a create upload request.
--
-- /See:/ 'mkCreateUploadResponse' smart constructor.
data CreateUploadResponse = CreateUploadResponse'
  { -- | The newly created upload.
    upload :: Core.Maybe Types.Upload,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateUploadResponse' value with any optional fields omitted.
mkCreateUploadResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateUploadResponse
mkCreateUploadResponse responseStatus =
  CreateUploadResponse' {upload = Core.Nothing, responseStatus}

-- | The newly created upload.
--
-- /Note:/ Consider using 'upload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsUpload :: Lens.Lens' CreateUploadResponse (Core.Maybe Types.Upload)
currsUpload = Lens.field @"upload"
{-# DEPRECATED currsUpload "Use generic-lens or generic-optics with 'upload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsResponseStatus :: Lens.Lens' CreateUploadResponse Core.Int
currsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED currsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
