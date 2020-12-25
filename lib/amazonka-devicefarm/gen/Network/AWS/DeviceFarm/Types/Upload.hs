{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Upload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Upload
  ( Upload (..),

    -- * Smart constructor
    mkUpload,

    -- * Lenses
    uArn,
    uCategory,
    uContentType,
    uCreated,
    uMessage,
    uMetadata,
    uName,
    uStatus,
    uType,
    uUrl,
  )
where

import qualified Network.AWS.DeviceFarm.Types.AmazonResourceName as Types
import qualified Network.AWS.DeviceFarm.Types.ContentType as Types
import qualified Network.AWS.DeviceFarm.Types.Message as Types
import qualified Network.AWS.DeviceFarm.Types.Metadata as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.DeviceFarm.Types.URL as Types
import qualified Network.AWS.DeviceFarm.Types.UploadCategory as Types
import qualified Network.AWS.DeviceFarm.Types.UploadStatus as Types
import qualified Network.AWS.DeviceFarm.Types.UploadType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An app or a set of one or more tests to upload or that have been uploaded.
--
-- /See:/ 'mkUpload' smart constructor.
data Upload = Upload'
  { -- | The upload's ARN.
    arn :: Core.Maybe Types.AmazonResourceName,
    -- | The upload's category. Allowed values include:
    --
    --
    --     * CURATED: An upload managed by AWS Device Farm.
    --
    --
    --     * PRIVATE: An upload managed by the AWS Device Farm customer.
    category :: Core.Maybe Types.UploadCategory,
    -- | The upload's content type (for example, @application/octet-stream@ ).
    contentType :: Core.Maybe Types.ContentType,
    -- | When the upload was created.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | A message about the upload's result.
    message :: Core.Maybe Types.Message,
    -- | The upload's metadata. For example, for Android, this contains information that is parsed from the manifest and is displayed in the AWS Device Farm console after the associated app is uploaded.
    metadata :: Core.Maybe Types.Metadata,
    -- | The upload's file name.
    name :: Core.Maybe Types.Name,
    -- | The upload's status.
    --
    -- Must be one of the following values:
    --
    --     * FAILED
    --
    --
    --     * INITIALIZED
    --
    --
    --     * PROCESSING
    --
    --
    --     * SUCCEEDED
    status :: Core.Maybe Types.UploadStatus,
    -- | The upload's type.
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
    type' :: Core.Maybe Types.UploadType,
    -- | The presigned Amazon S3 URL that was used to store a file using a PUT request.
    url :: Core.Maybe Types.URL
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Upload' value with any optional fields omitted.
mkUpload ::
  Upload
mkUpload =
  Upload'
    { arn = Core.Nothing,
      category = Core.Nothing,
      contentType = Core.Nothing,
      created = Core.Nothing,
      message = Core.Nothing,
      metadata = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing,
      type' = Core.Nothing,
      url = Core.Nothing
    }

-- | The upload's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uArn :: Lens.Lens' Upload (Core.Maybe Types.AmazonResourceName)
uArn = Lens.field @"arn"
{-# DEPRECATED uArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The upload's category. Allowed values include:
--
--
--     * CURATED: An upload managed by AWS Device Farm.
--
--
--     * PRIVATE: An upload managed by the AWS Device Farm customer.
--
--
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCategory :: Lens.Lens' Upload (Core.Maybe Types.UploadCategory)
uCategory = Lens.field @"category"
{-# DEPRECATED uCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The upload's content type (for example, @application/octet-stream@ ).
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uContentType :: Lens.Lens' Upload (Core.Maybe Types.ContentType)
uContentType = Lens.field @"contentType"
{-# DEPRECATED uContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | When the upload was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCreated :: Lens.Lens' Upload (Core.Maybe Core.NominalDiffTime)
uCreated = Lens.field @"created"
{-# DEPRECATED uCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | A message about the upload's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uMessage :: Lens.Lens' Upload (Core.Maybe Types.Message)
uMessage = Lens.field @"message"
{-# DEPRECATED uMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The upload's metadata. For example, for Android, this contains information that is parsed from the manifest and is displayed in the AWS Device Farm console after the associated app is uploaded.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uMetadata :: Lens.Lens' Upload (Core.Maybe Types.Metadata)
uMetadata = Lens.field @"metadata"
{-# DEPRECATED uMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The upload's file name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' Upload (Core.Maybe Types.Name)
uName = Lens.field @"name"
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The upload's status.
--
-- Must be one of the following values:
--
--     * FAILED
--
--
--     * INITIALIZED
--
--
--     * PROCESSING
--
--
--     * SUCCEEDED
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStatus :: Lens.Lens' Upload (Core.Maybe Types.UploadStatus)
uStatus = Lens.field @"status"
{-# DEPRECATED uStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The upload's type.
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
uType :: Lens.Lens' Upload (Core.Maybe Types.UploadType)
uType = Lens.field @"type'"
{-# DEPRECATED uType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The presigned Amazon S3 URL that was used to store a file using a PUT request.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUrl :: Lens.Lens' Upload (Core.Maybe Types.URL)
uUrl = Lens.field @"url"
{-# DEPRECATED uUrl "Use generic-lens or generic-optics with 'url' instead." #-}

instance Core.FromJSON Upload where
  parseJSON =
    Core.withObject "Upload" Core.$
      \x ->
        Upload'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "category")
          Core.<*> (x Core..:? "contentType")
          Core.<*> (x Core..:? "created")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "metadata")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "type")
          Core.<*> (x Core..:? "url")
