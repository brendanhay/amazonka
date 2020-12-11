{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cuContentType,
    cuProjectARN,
    cuName,
    cuType,

    -- * Destructuring the response
    CreateUploadResponse (..),
    mkCreateUploadResponse,

    -- ** Response lenses
    cursUpload,
    cursResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the create upload operation.
--
-- /See:/ 'mkCreateUpload' smart constructor.
data CreateUpload = CreateUpload'
  { contentType ::
      Lude.Maybe Lude.Text,
    projectARN :: Lude.Text,
    name :: Lude.Text,
    type' :: UploadType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUpload' with the minimum fields required to make a request.
--
-- * 'contentType' - The upload's content type (for example, @application/octet-stream@ ).
-- * 'name' - The upload's file name. The name should not contain any forward slashes (@/@ ). If you are uploading an iOS app, the file name must end with the @.ipa@ extension. If you are uploading an Android app, the file name must end with the @.apk@ extension. For all others, the file name must end with the @.zip@ file extension.
-- * 'projectARN' - The ARN of the project for the upload.
-- * 'type'' - The upload's upload type.
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
mkCreateUpload ::
  -- | 'projectARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  UploadType ->
  CreateUpload
mkCreateUpload pProjectARN_ pName_ pType_ =
  CreateUpload'
    { contentType = Lude.Nothing,
      projectARN = pProjectARN_,
      name = pName_,
      type' = pType_
    }

-- | The upload's content type (for example, @application/octet-stream@ ).
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuContentType :: Lens.Lens' CreateUpload (Lude.Maybe Lude.Text)
cuContentType = Lens.lens (contentType :: CreateUpload -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: CreateUpload)
{-# DEPRECATED cuContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The ARN of the project for the upload.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuProjectARN :: Lens.Lens' CreateUpload Lude.Text
cuProjectARN = Lens.lens (projectARN :: CreateUpload -> Lude.Text) (\s a -> s {projectARN = a} :: CreateUpload)
{-# DEPRECATED cuProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

-- | The upload's file name. The name should not contain any forward slashes (@/@ ). If you are uploading an iOS app, the file name must end with the @.ipa@ extension. If you are uploading an Android app, the file name must end with the @.apk@ extension. For all others, the file name must end with the @.zip@ file extension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuName :: Lens.Lens' CreateUpload Lude.Text
cuName = Lens.lens (name :: CreateUpload -> Lude.Text) (\s a -> s {name = a} :: CreateUpload)
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
cuType :: Lens.Lens' CreateUpload UploadType
cuType = Lens.lens (type' :: CreateUpload -> UploadType) (\s a -> s {type' = a} :: CreateUpload)
{-# DEPRECATED cuType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest CreateUpload where
  type Rs CreateUpload = CreateUploadResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUploadResponse'
            Lude.<$> (x Lude..?> "upload") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUpload where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.CreateUpload" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUpload where
  toJSON CreateUpload' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("contentType" Lude..=) Lude.<$> contentType,
            Lude.Just ("projectArn" Lude..= projectARN),
            Lude.Just ("name" Lude..= name),
            Lude.Just ("type" Lude..= type')
          ]
      )

instance Lude.ToPath CreateUpload where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUpload where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a create upload request.
--
-- /See:/ 'mkCreateUploadResponse' smart constructor.
data CreateUploadResponse = CreateUploadResponse'
  { upload ::
      Lude.Maybe Upload,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUploadResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'upload' - The newly created upload.
mkCreateUploadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUploadResponse
mkCreateUploadResponse pResponseStatus_ =
  CreateUploadResponse'
    { upload = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly created upload.
--
-- /Note:/ Consider using 'upload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursUpload :: Lens.Lens' CreateUploadResponse (Lude.Maybe Upload)
cursUpload = Lens.lens (upload :: CreateUploadResponse -> Lude.Maybe Upload) (\s a -> s {upload = a} :: CreateUploadResponse)
{-# DEPRECATED cursUpload "Use generic-lens or generic-optics with 'upload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursResponseStatus :: Lens.Lens' CreateUploadResponse Lude.Int
cursResponseStatus = Lens.lens (responseStatus :: CreateUploadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUploadResponse)
{-# DEPRECATED cursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
