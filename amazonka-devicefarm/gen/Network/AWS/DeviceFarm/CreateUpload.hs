{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an app or test scripts.
module Network.AWS.DeviceFarm.CreateUpload
  ( -- * Creating a Request
    CreateUpload (..),
    newCreateUpload,

    -- * Request Lenses
    createUpload_contentType,
    createUpload_projectArn,
    createUpload_name,
    createUpload_type,

    -- * Destructuring the Response
    CreateUploadResponse (..),
    newCreateUploadResponse,

    -- * Response Lenses
    createUploadResponse_upload,
    createUploadResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the create upload operation.
--
-- /See:/ 'newCreateUpload' smart constructor.
data CreateUpload = CreateUpload'
  { -- | The upload\'s content type (for example, @application\/octet-stream@).
    contentType :: Core.Maybe Core.Text,
    -- | The ARN of the project for the upload.
    projectArn :: Core.Text,
    -- | The upload\'s file name. The name should not contain any forward slashes
    -- (@\/@). If you are uploading an iOS app, the file name must end with the
    -- @.ipa@ extension. If you are uploading an Android app, the file name
    -- must end with the @.apk@ extension. For all others, the file name must
    -- end with the @.zip@ file extension.
    name :: Core.Text,
    -- | The upload\'s upload type.
    --
    -- Must be one of the following values:
    --
    -- -   ANDROID_APP
    --
    -- -   IOS_APP
    --
    -- -   WEB_APP
    --
    -- -   EXTERNAL_DATA
    --
    -- -   APPIUM_JAVA_JUNIT_TEST_PACKAGE
    --
    -- -   APPIUM_JAVA_TESTNG_TEST_PACKAGE
    --
    -- -   APPIUM_PYTHON_TEST_PACKAGE
    --
    -- -   APPIUM_NODE_TEST_PACKAGE
    --
    -- -   APPIUM_RUBY_TEST_PACKAGE
    --
    -- -   APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE
    --
    -- -   APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE
    --
    -- -   APPIUM_WEB_PYTHON_TEST_PACKAGE
    --
    -- -   APPIUM_WEB_NODE_TEST_PACKAGE
    --
    -- -   APPIUM_WEB_RUBY_TEST_PACKAGE
    --
    -- -   CALABASH_TEST_PACKAGE
    --
    -- -   INSTRUMENTATION_TEST_PACKAGE
    --
    -- -   UIAUTOMATION_TEST_PACKAGE
    --
    -- -   UIAUTOMATOR_TEST_PACKAGE
    --
    -- -   XCTEST_TEST_PACKAGE
    --
    -- -   XCTEST_UI_TEST_PACKAGE
    --
    -- -   APPIUM_JAVA_JUNIT_TEST_SPEC
    --
    -- -   APPIUM_JAVA_TESTNG_TEST_SPEC
    --
    -- -   APPIUM_PYTHON_TEST_SPEC
    --
    -- -   APPIUM_NODE_TEST_SPEC
    --
    -- -   APPIUM_RUBY_TEST_SPEC
    --
    -- -   APPIUM_WEB_JAVA_JUNIT_TEST_SPEC
    --
    -- -   APPIUM_WEB_JAVA_TESTNG_TEST_SPEC
    --
    -- -   APPIUM_WEB_PYTHON_TEST_SPEC
    --
    -- -   APPIUM_WEB_NODE_TEST_SPEC
    --
    -- -   APPIUM_WEB_RUBY_TEST_SPEC
    --
    -- -   INSTRUMENTATION_TEST_SPEC
    --
    -- -   XCTEST_UI_TEST_SPEC
    --
    -- If you call @CreateUpload@ with @WEB_APP@ specified, AWS Device Farm
    -- throws an @ArgumentException@ error.
    type' :: UploadType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'createUpload_contentType' - The upload\'s content type (for example, @application\/octet-stream@).
--
-- 'projectArn', 'createUpload_projectArn' - The ARN of the project for the upload.
--
-- 'name', 'createUpload_name' - The upload\'s file name. The name should not contain any forward slashes
-- (@\/@). If you are uploading an iOS app, the file name must end with the
-- @.ipa@ extension. If you are uploading an Android app, the file name
-- must end with the @.apk@ extension. For all others, the file name must
-- end with the @.zip@ file extension.
--
-- 'type'', 'createUpload_type' - The upload\'s upload type.
--
-- Must be one of the following values:
--
-- -   ANDROID_APP
--
-- -   IOS_APP
--
-- -   WEB_APP
--
-- -   EXTERNAL_DATA
--
-- -   APPIUM_JAVA_JUNIT_TEST_PACKAGE
--
-- -   APPIUM_JAVA_TESTNG_TEST_PACKAGE
--
-- -   APPIUM_PYTHON_TEST_PACKAGE
--
-- -   APPIUM_NODE_TEST_PACKAGE
--
-- -   APPIUM_RUBY_TEST_PACKAGE
--
-- -   APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE
--
-- -   APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE
--
-- -   APPIUM_WEB_PYTHON_TEST_PACKAGE
--
-- -   APPIUM_WEB_NODE_TEST_PACKAGE
--
-- -   APPIUM_WEB_RUBY_TEST_PACKAGE
--
-- -   CALABASH_TEST_PACKAGE
--
-- -   INSTRUMENTATION_TEST_PACKAGE
--
-- -   UIAUTOMATION_TEST_PACKAGE
--
-- -   UIAUTOMATOR_TEST_PACKAGE
--
-- -   XCTEST_TEST_PACKAGE
--
-- -   XCTEST_UI_TEST_PACKAGE
--
-- -   APPIUM_JAVA_JUNIT_TEST_SPEC
--
-- -   APPIUM_JAVA_TESTNG_TEST_SPEC
--
-- -   APPIUM_PYTHON_TEST_SPEC
--
-- -   APPIUM_NODE_TEST_SPEC
--
-- -   APPIUM_RUBY_TEST_SPEC
--
-- -   APPIUM_WEB_JAVA_JUNIT_TEST_SPEC
--
-- -   APPIUM_WEB_JAVA_TESTNG_TEST_SPEC
--
-- -   APPIUM_WEB_PYTHON_TEST_SPEC
--
-- -   APPIUM_WEB_NODE_TEST_SPEC
--
-- -   APPIUM_WEB_RUBY_TEST_SPEC
--
-- -   INSTRUMENTATION_TEST_SPEC
--
-- -   XCTEST_UI_TEST_SPEC
--
-- If you call @CreateUpload@ with @WEB_APP@ specified, AWS Device Farm
-- throws an @ArgumentException@ error.
newCreateUpload ::
  -- | 'projectArn'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'type''
  UploadType ->
  CreateUpload
newCreateUpload pProjectArn_ pName_ pType_ =
  CreateUpload'
    { contentType = Core.Nothing,
      projectArn = pProjectArn_,
      name = pName_,
      type' = pType_
    }

-- | The upload\'s content type (for example, @application\/octet-stream@).
createUpload_contentType :: Lens.Lens' CreateUpload (Core.Maybe Core.Text)
createUpload_contentType = Lens.lens (\CreateUpload' {contentType} -> contentType) (\s@CreateUpload' {} a -> s {contentType = a} :: CreateUpload)

-- | The ARN of the project for the upload.
createUpload_projectArn :: Lens.Lens' CreateUpload Core.Text
createUpload_projectArn = Lens.lens (\CreateUpload' {projectArn} -> projectArn) (\s@CreateUpload' {} a -> s {projectArn = a} :: CreateUpload)

-- | The upload\'s file name. The name should not contain any forward slashes
-- (@\/@). If you are uploading an iOS app, the file name must end with the
-- @.ipa@ extension. If you are uploading an Android app, the file name
-- must end with the @.apk@ extension. For all others, the file name must
-- end with the @.zip@ file extension.
createUpload_name :: Lens.Lens' CreateUpload Core.Text
createUpload_name = Lens.lens (\CreateUpload' {name} -> name) (\s@CreateUpload' {} a -> s {name = a} :: CreateUpload)

-- | The upload\'s upload type.
--
-- Must be one of the following values:
--
-- -   ANDROID_APP
--
-- -   IOS_APP
--
-- -   WEB_APP
--
-- -   EXTERNAL_DATA
--
-- -   APPIUM_JAVA_JUNIT_TEST_PACKAGE
--
-- -   APPIUM_JAVA_TESTNG_TEST_PACKAGE
--
-- -   APPIUM_PYTHON_TEST_PACKAGE
--
-- -   APPIUM_NODE_TEST_PACKAGE
--
-- -   APPIUM_RUBY_TEST_PACKAGE
--
-- -   APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE
--
-- -   APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE
--
-- -   APPIUM_WEB_PYTHON_TEST_PACKAGE
--
-- -   APPIUM_WEB_NODE_TEST_PACKAGE
--
-- -   APPIUM_WEB_RUBY_TEST_PACKAGE
--
-- -   CALABASH_TEST_PACKAGE
--
-- -   INSTRUMENTATION_TEST_PACKAGE
--
-- -   UIAUTOMATION_TEST_PACKAGE
--
-- -   UIAUTOMATOR_TEST_PACKAGE
--
-- -   XCTEST_TEST_PACKAGE
--
-- -   XCTEST_UI_TEST_PACKAGE
--
-- -   APPIUM_JAVA_JUNIT_TEST_SPEC
--
-- -   APPIUM_JAVA_TESTNG_TEST_SPEC
--
-- -   APPIUM_PYTHON_TEST_SPEC
--
-- -   APPIUM_NODE_TEST_SPEC
--
-- -   APPIUM_RUBY_TEST_SPEC
--
-- -   APPIUM_WEB_JAVA_JUNIT_TEST_SPEC
--
-- -   APPIUM_WEB_JAVA_TESTNG_TEST_SPEC
--
-- -   APPIUM_WEB_PYTHON_TEST_SPEC
--
-- -   APPIUM_WEB_NODE_TEST_SPEC
--
-- -   APPIUM_WEB_RUBY_TEST_SPEC
--
-- -   INSTRUMENTATION_TEST_SPEC
--
-- -   XCTEST_UI_TEST_SPEC
--
-- If you call @CreateUpload@ with @WEB_APP@ specified, AWS Device Farm
-- throws an @ArgumentException@ error.
createUpload_type :: Lens.Lens' CreateUpload UploadType
createUpload_type = Lens.lens (\CreateUpload' {type'} -> type') (\s@CreateUpload' {} a -> s {type' = a} :: CreateUpload)

instance Core.AWSRequest CreateUpload where
  type AWSResponse CreateUpload = CreateUploadResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUploadResponse'
            Core.<$> (x Core..?> "upload")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUpload

instance Core.NFData CreateUpload

instance Core.ToHeaders CreateUpload where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.CreateUpload" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUpload where
  toJSON CreateUpload' {..} =
    Core.object
      ( Core.catMaybes
          [ ("contentType" Core..=) Core.<$> contentType,
            Core.Just ("projectArn" Core..= projectArn),
            Core.Just ("name" Core..= name),
            Core.Just ("type" Core..= type')
          ]
      )

instance Core.ToPath CreateUpload where
  toPath = Core.const "/"

instance Core.ToQuery CreateUpload where
  toQuery = Core.const Core.mempty

-- | Represents the result of a create upload request.
--
-- /See:/ 'newCreateUploadResponse' smart constructor.
data CreateUploadResponse = CreateUploadResponse'
  { -- | The newly created upload.
    upload :: Core.Maybe Upload,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upload', 'createUploadResponse_upload' - The newly created upload.
--
-- 'httpStatus', 'createUploadResponse_httpStatus' - The response's http status code.
newCreateUploadResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUploadResponse
newCreateUploadResponse pHttpStatus_ =
  CreateUploadResponse'
    { upload = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created upload.
createUploadResponse_upload :: Lens.Lens' CreateUploadResponse (Core.Maybe Upload)
createUploadResponse_upload = Lens.lens (\CreateUploadResponse' {upload} -> upload) (\s@CreateUploadResponse' {} a -> s {upload = a} :: CreateUploadResponse)

-- | The response's http status code.
createUploadResponse_httpStatus :: Lens.Lens' CreateUploadResponse Core.Int
createUploadResponse_httpStatus = Lens.lens (\CreateUploadResponse' {httpStatus} -> httpStatus) (\s@CreateUploadResponse' {} a -> s {httpStatus = a} :: CreateUploadResponse)

instance Core.NFData CreateUploadResponse
