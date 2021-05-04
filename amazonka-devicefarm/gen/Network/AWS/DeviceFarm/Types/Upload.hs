{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Upload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Upload where

import Network.AWS.DeviceFarm.Types.UploadCategory
import Network.AWS.DeviceFarm.Types.UploadStatus
import Network.AWS.DeviceFarm.Types.UploadType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An app or a set of one or more tests to upload or that have been
-- uploaded.
--
-- /See:/ 'newUpload' smart constructor.
data Upload = Upload'
  { -- | The upload\'s status.
    --
    -- Must be one of the following values:
    --
    -- -   FAILED
    --
    -- -   INITIALIZED
    --
    -- -   PROCESSING
    --
    -- -   SUCCEEDED
    status :: Prelude.Maybe UploadStatus,
    -- | The upload\'s content type (for example, @application\/octet-stream@).
    contentType :: Prelude.Maybe Prelude.Text,
    -- | A message about the upload\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | The upload\'s category. Allowed values include:
    --
    -- -   CURATED: An upload managed by AWS Device Farm.
    --
    -- -   PRIVATE: An upload managed by the AWS Device Farm customer.
    category :: Prelude.Maybe UploadCategory,
    -- | The upload\'s metadata. For example, for Android, this contains
    -- information that is parsed from the manifest and is displayed in the AWS
    -- Device Farm console after the associated app is uploaded.
    metadata :: Prelude.Maybe Prelude.Text,
    -- | The upload\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The upload\'s file name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The presigned Amazon S3 URL that was used to store a file using a PUT
    -- request.
    url :: Prelude.Maybe Prelude.Text,
    -- | When the upload was created.
    created :: Prelude.Maybe Prelude.POSIX,
    -- | The upload\'s type.
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
    type' :: Prelude.Maybe UploadType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Upload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'upload_status' - The upload\'s status.
--
-- Must be one of the following values:
--
-- -   FAILED
--
-- -   INITIALIZED
--
-- -   PROCESSING
--
-- -   SUCCEEDED
--
-- 'contentType', 'upload_contentType' - The upload\'s content type (for example, @application\/octet-stream@).
--
-- 'message', 'upload_message' - A message about the upload\'s result.
--
-- 'category', 'upload_category' - The upload\'s category. Allowed values include:
--
-- -   CURATED: An upload managed by AWS Device Farm.
--
-- -   PRIVATE: An upload managed by the AWS Device Farm customer.
--
-- 'metadata', 'upload_metadata' - The upload\'s metadata. For example, for Android, this contains
-- information that is parsed from the manifest and is displayed in the AWS
-- Device Farm console after the associated app is uploaded.
--
-- 'arn', 'upload_arn' - The upload\'s ARN.
--
-- 'name', 'upload_name' - The upload\'s file name.
--
-- 'url', 'upload_url' - The presigned Amazon S3 URL that was used to store a file using a PUT
-- request.
--
-- 'created', 'upload_created' - When the upload was created.
--
-- 'type'', 'upload_type' - The upload\'s type.
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
newUpload ::
  Upload
newUpload =
  Upload'
    { status = Prelude.Nothing,
      contentType = Prelude.Nothing,
      message = Prelude.Nothing,
      category = Prelude.Nothing,
      metadata = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      url = Prelude.Nothing,
      created = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The upload\'s status.
--
-- Must be one of the following values:
--
-- -   FAILED
--
-- -   INITIALIZED
--
-- -   PROCESSING
--
-- -   SUCCEEDED
upload_status :: Lens.Lens' Upload (Prelude.Maybe UploadStatus)
upload_status = Lens.lens (\Upload' {status} -> status) (\s@Upload' {} a -> s {status = a} :: Upload)

-- | The upload\'s content type (for example, @application\/octet-stream@).
upload_contentType :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_contentType = Lens.lens (\Upload' {contentType} -> contentType) (\s@Upload' {} a -> s {contentType = a} :: Upload)

-- | A message about the upload\'s result.
upload_message :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_message = Lens.lens (\Upload' {message} -> message) (\s@Upload' {} a -> s {message = a} :: Upload)

-- | The upload\'s category. Allowed values include:
--
-- -   CURATED: An upload managed by AWS Device Farm.
--
-- -   PRIVATE: An upload managed by the AWS Device Farm customer.
upload_category :: Lens.Lens' Upload (Prelude.Maybe UploadCategory)
upload_category = Lens.lens (\Upload' {category} -> category) (\s@Upload' {} a -> s {category = a} :: Upload)

-- | The upload\'s metadata. For example, for Android, this contains
-- information that is parsed from the manifest and is displayed in the AWS
-- Device Farm console after the associated app is uploaded.
upload_metadata :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_metadata = Lens.lens (\Upload' {metadata} -> metadata) (\s@Upload' {} a -> s {metadata = a} :: Upload)

-- | The upload\'s ARN.
upload_arn :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_arn = Lens.lens (\Upload' {arn} -> arn) (\s@Upload' {} a -> s {arn = a} :: Upload)

-- | The upload\'s file name.
upload_name :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_name = Lens.lens (\Upload' {name} -> name) (\s@Upload' {} a -> s {name = a} :: Upload)

-- | The presigned Amazon S3 URL that was used to store a file using a PUT
-- request.
upload_url :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_url = Lens.lens (\Upload' {url} -> url) (\s@Upload' {} a -> s {url = a} :: Upload)

-- | When the upload was created.
upload_created :: Lens.Lens' Upload (Prelude.Maybe Prelude.UTCTime)
upload_created = Lens.lens (\Upload' {created} -> created) (\s@Upload' {} a -> s {created = a} :: Upload) Prelude.. Lens.mapping Prelude._Time

-- | The upload\'s type.
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
upload_type :: Lens.Lens' Upload (Prelude.Maybe UploadType)
upload_type = Lens.lens (\Upload' {type'} -> type') (\s@Upload' {} a -> s {type' = a} :: Upload)

instance Prelude.FromJSON Upload where
  parseJSON =
    Prelude.withObject
      "Upload"
      ( \x ->
          Upload'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "contentType")
            Prelude.<*> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "category")
            Prelude.<*> (x Prelude..:? "metadata")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "url")
            Prelude.<*> (x Prelude..:? "created")
            Prelude.<*> (x Prelude..:? "type")
      )

instance Prelude.Hashable Upload

instance Prelude.NFData Upload
