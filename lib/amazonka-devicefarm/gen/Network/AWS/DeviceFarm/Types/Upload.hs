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
    uStatus,
    uArn,
    uCreated,
    uCategory,
    uUrl,
    uName,
    uMetadata,
    uType,
    uMessage,
    uContentType,
  )
where

import Network.AWS.DeviceFarm.Types.UploadCategory
import Network.AWS.DeviceFarm.Types.UploadStatus
import Network.AWS.DeviceFarm.Types.UploadType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An app or a set of one or more tests to upload or that have been uploaded.
--
-- /See:/ 'mkUpload' smart constructor.
data Upload = Upload'
  { status :: Lude.Maybe UploadStatus,
    arn :: Lude.Maybe Lude.Text,
    created :: Lude.Maybe Lude.Timestamp,
    category :: Lude.Maybe UploadCategory,
    url :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    metadata :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe UploadType,
    message :: Lude.Maybe Lude.Text,
    contentType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Upload' with the minimum fields required to make a request.
--
-- * 'arn' - The upload's ARN.
-- * 'category' - The upload's category. Allowed values include:
--
--
--     * CURATED: An upload managed by AWS Device Farm.
--
--
--     * PRIVATE: An upload managed by the AWS Device Farm customer.
--
--
-- * 'contentType' - The upload's content type (for example, @application/octet-stream@ ).
-- * 'created' - When the upload was created.
-- * 'message' - A message about the upload's result.
-- * 'metadata' - The upload's metadata. For example, for Android, this contains information that is parsed from the manifest and is displayed in the AWS Device Farm console after the associated app is uploaded.
-- * 'name' - The upload's file name.
-- * 'status' - The upload's status.
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
-- * 'type'' - The upload's type.
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
-- * 'url' - The presigned Amazon S3 URL that was used to store a file using a PUT request.
mkUpload ::
  Upload
mkUpload =
  Upload'
    { status = Lude.Nothing,
      arn = Lude.Nothing,
      created = Lude.Nothing,
      category = Lude.Nothing,
      url = Lude.Nothing,
      name = Lude.Nothing,
      metadata = Lude.Nothing,
      type' = Lude.Nothing,
      message = Lude.Nothing,
      contentType = Lude.Nothing
    }

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
uStatus :: Lens.Lens' Upload (Lude.Maybe UploadStatus)
uStatus = Lens.lens (status :: Upload -> Lude.Maybe UploadStatus) (\s a -> s {status = a} :: Upload)
{-# DEPRECATED uStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The upload's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uArn :: Lens.Lens' Upload (Lude.Maybe Lude.Text)
uArn = Lens.lens (arn :: Upload -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Upload)
{-# DEPRECATED uArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the upload was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCreated :: Lens.Lens' Upload (Lude.Maybe Lude.Timestamp)
uCreated = Lens.lens (created :: Upload -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: Upload)
{-# DEPRECATED uCreated "Use generic-lens or generic-optics with 'created' instead." #-}

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
uCategory :: Lens.Lens' Upload (Lude.Maybe UploadCategory)
uCategory = Lens.lens (category :: Upload -> Lude.Maybe UploadCategory) (\s a -> s {category = a} :: Upload)
{-# DEPRECATED uCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The presigned Amazon S3 URL that was used to store a file using a PUT request.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUrl :: Lens.Lens' Upload (Lude.Maybe Lude.Text)
uUrl = Lens.lens (url :: Upload -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: Upload)
{-# DEPRECATED uUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The upload's file name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' Upload (Lude.Maybe Lude.Text)
uName = Lens.lens (name :: Upload -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Upload)
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The upload's metadata. For example, for Android, this contains information that is parsed from the manifest and is displayed in the AWS Device Farm console after the associated app is uploaded.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uMetadata :: Lens.Lens' Upload (Lude.Maybe Lude.Text)
uMetadata = Lens.lens (metadata :: Upload -> Lude.Maybe Lude.Text) (\s a -> s {metadata = a} :: Upload)
{-# DEPRECATED uMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

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
uType :: Lens.Lens' Upload (Lude.Maybe UploadType)
uType = Lens.lens (type' :: Upload -> Lude.Maybe UploadType) (\s a -> s {type' = a} :: Upload)
{-# DEPRECATED uType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A message about the upload's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uMessage :: Lens.Lens' Upload (Lude.Maybe Lude.Text)
uMessage = Lens.lens (message :: Upload -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Upload)
{-# DEPRECATED uMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The upload's content type (for example, @application/octet-stream@ ).
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uContentType :: Lens.Lens' Upload (Lude.Maybe Lude.Text)
uContentType = Lens.lens (contentType :: Upload -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: Upload)
{-# DEPRECATED uContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Lude.FromJSON Upload where
  parseJSON =
    Lude.withObject
      "Upload"
      ( \x ->
          Upload'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "category")
            Lude.<*> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "metadata")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..:? "contentType")
      )
