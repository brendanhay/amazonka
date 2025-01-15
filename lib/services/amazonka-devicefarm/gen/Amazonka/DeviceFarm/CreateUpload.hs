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
-- Module      : Amazonka.DeviceFarm.CreateUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an app or test scripts.
module Amazonka.DeviceFarm.CreateUpload
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the create upload operation.
--
-- /See:/ 'newCreateUpload' smart constructor.
data CreateUpload = CreateUpload'
  { -- | The upload\'s content type (for example, @application\/octet-stream@).
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the project for the upload.
    projectArn :: Prelude.Text,
    -- | The upload\'s file name. The name should not contain any forward slashes
    -- (@\/@). If you are uploading an iOS app, the file name must end with the
    -- @.ipa@ extension. If you are uploading an Android app, the file name
    -- must end with the @.apk@ extension. For all others, the file name must
    -- end with the @.zip@ file extension.
    name :: Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  UploadType ->
  CreateUpload
newCreateUpload pProjectArn_ pName_ pType_ =
  CreateUpload'
    { contentType = Prelude.Nothing,
      projectArn = pProjectArn_,
      name = pName_,
      type' = pType_
    }

-- | The upload\'s content type (for example, @application\/octet-stream@).
createUpload_contentType :: Lens.Lens' CreateUpload (Prelude.Maybe Prelude.Text)
createUpload_contentType = Lens.lens (\CreateUpload' {contentType} -> contentType) (\s@CreateUpload' {} a -> s {contentType = a} :: CreateUpload)

-- | The ARN of the project for the upload.
createUpload_projectArn :: Lens.Lens' CreateUpload Prelude.Text
createUpload_projectArn = Lens.lens (\CreateUpload' {projectArn} -> projectArn) (\s@CreateUpload' {} a -> s {projectArn = a} :: CreateUpload)

-- | The upload\'s file name. The name should not contain any forward slashes
-- (@\/@). If you are uploading an iOS app, the file name must end with the
-- @.ipa@ extension. If you are uploading an Android app, the file name
-- must end with the @.apk@ extension. For all others, the file name must
-- end with the @.zip@ file extension.
createUpload_name :: Lens.Lens' CreateUpload Prelude.Text
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUploadResponse'
            Prelude.<$> (x Data..?> "upload")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUpload where
  hashWithSalt _salt CreateUpload' {..} =
    _salt
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateUpload where
  rnf CreateUpload' {..} =
    Prelude.rnf contentType `Prelude.seq`
      Prelude.rnf projectArn `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf type'

instance Data.ToHeaders CreateUpload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.CreateUpload" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUpload where
  toJSON CreateUpload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("contentType" Data..=) Prelude.<$> contentType,
            Prelude.Just ("projectArn" Data..= projectArn),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath CreateUpload where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUpload where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a create upload request.
--
-- /See:/ 'newCreateUploadResponse' smart constructor.
data CreateUploadResponse = CreateUploadResponse'
  { -- | The newly created upload.
    upload :: Prelude.Maybe Upload,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateUploadResponse
newCreateUploadResponse pHttpStatus_ =
  CreateUploadResponse'
    { upload = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created upload.
createUploadResponse_upload :: Lens.Lens' CreateUploadResponse (Prelude.Maybe Upload)
createUploadResponse_upload = Lens.lens (\CreateUploadResponse' {upload} -> upload) (\s@CreateUploadResponse' {} a -> s {upload = a} :: CreateUploadResponse)

-- | The response's http status code.
createUploadResponse_httpStatus :: Lens.Lens' CreateUploadResponse Prelude.Int
createUploadResponse_httpStatus = Lens.lens (\CreateUploadResponse' {httpStatus} -> httpStatus) (\s@CreateUploadResponse' {} a -> s {httpStatus = a} :: CreateUploadResponse)

instance Prelude.NFData CreateUploadResponse where
  rnf CreateUploadResponse' {..} =
    Prelude.rnf upload `Prelude.seq`
      Prelude.rnf httpStatus
