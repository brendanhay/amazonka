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
-- Module      : Amazonka.DeviceFarm.Types.Upload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Upload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.UploadCategory
import Amazonka.DeviceFarm.Types.UploadStatus
import Amazonka.DeviceFarm.Types.UploadType
import qualified Amazonka.Prelude as Prelude

-- | An app or a set of one or more tests to upload or that have been
-- uploaded.
--
-- /See:/ 'newUpload' smart constructor.
data Upload = Upload'
  { -- | The upload\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The upload\'s category. Allowed values include:
    --
    -- -   CURATED: An upload managed by AWS Device Farm.
    --
    -- -   PRIVATE: An upload managed by the AWS Device Farm customer.
    category :: Prelude.Maybe UploadCategory,
    -- | The upload\'s content type (for example, @application\/octet-stream@).
    contentType :: Prelude.Maybe Prelude.Text,
    -- | When the upload was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | A message about the upload\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | The upload\'s metadata. For example, for Android, this contains
    -- information that is parsed from the manifest and is displayed in the AWS
    -- Device Farm console after the associated app is uploaded.
    metadata :: Prelude.Maybe Prelude.Text,
    -- | The upload\'s file name.
    name :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe UploadStatus,
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
    type' :: Prelude.Maybe UploadType,
    -- | The presigned Amazon S3 URL that was used to store a file using a PUT
    -- request.
    url :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Upload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'upload_arn' - The upload\'s ARN.
--
-- 'category', 'upload_category' - The upload\'s category. Allowed values include:
--
-- -   CURATED: An upload managed by AWS Device Farm.
--
-- -   PRIVATE: An upload managed by the AWS Device Farm customer.
--
-- 'contentType', 'upload_contentType' - The upload\'s content type (for example, @application\/octet-stream@).
--
-- 'created', 'upload_created' - When the upload was created.
--
-- 'message', 'upload_message' - A message about the upload\'s result.
--
-- 'metadata', 'upload_metadata' - The upload\'s metadata. For example, for Android, this contains
-- information that is parsed from the manifest and is displayed in the AWS
-- Device Farm console after the associated app is uploaded.
--
-- 'name', 'upload_name' - The upload\'s file name.
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
--
-- 'url', 'upload_url' - The presigned Amazon S3 URL that was used to store a file using a PUT
-- request.
newUpload ::
  Upload
newUpload =
  Upload'
    { arn = Prelude.Nothing,
      category = Prelude.Nothing,
      contentType = Prelude.Nothing,
      created = Prelude.Nothing,
      message = Prelude.Nothing,
      metadata = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The upload\'s ARN.
upload_arn :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_arn = Lens.lens (\Upload' {arn} -> arn) (\s@Upload' {} a -> s {arn = a} :: Upload)

-- | The upload\'s category. Allowed values include:
--
-- -   CURATED: An upload managed by AWS Device Farm.
--
-- -   PRIVATE: An upload managed by the AWS Device Farm customer.
upload_category :: Lens.Lens' Upload (Prelude.Maybe UploadCategory)
upload_category = Lens.lens (\Upload' {category} -> category) (\s@Upload' {} a -> s {category = a} :: Upload)

-- | The upload\'s content type (for example, @application\/octet-stream@).
upload_contentType :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_contentType = Lens.lens (\Upload' {contentType} -> contentType) (\s@Upload' {} a -> s {contentType = a} :: Upload)

-- | When the upload was created.
upload_created :: Lens.Lens' Upload (Prelude.Maybe Prelude.UTCTime)
upload_created = Lens.lens (\Upload' {created} -> created) (\s@Upload' {} a -> s {created = a} :: Upload) Prelude.. Lens.mapping Data._Time

-- | A message about the upload\'s result.
upload_message :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_message = Lens.lens (\Upload' {message} -> message) (\s@Upload' {} a -> s {message = a} :: Upload)

-- | The upload\'s metadata. For example, for Android, this contains
-- information that is parsed from the manifest and is displayed in the AWS
-- Device Farm console after the associated app is uploaded.
upload_metadata :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_metadata = Lens.lens (\Upload' {metadata} -> metadata) (\s@Upload' {} a -> s {metadata = a} :: Upload)

-- | The upload\'s file name.
upload_name :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_name = Lens.lens (\Upload' {name} -> name) (\s@Upload' {} a -> s {name = a} :: Upload)

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

-- | The presigned Amazon S3 URL that was used to store a file using a PUT
-- request.
upload_url :: Lens.Lens' Upload (Prelude.Maybe Prelude.Text)
upload_url = Lens.lens (\Upload' {url} -> url) (\s@Upload' {} a -> s {url = a} :: Upload) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON Upload where
  parseJSON =
    Data.withObject
      "Upload"
      ( \x ->
          Upload'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "category")
            Prelude.<*> (x Data..:? "contentType")
            Prelude.<*> (x Data..:? "created")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "metadata")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "url")
      )

instance Prelude.Hashable Upload where
  hashWithSalt _salt Upload' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` url

instance Prelude.NFData Upload where
  rnf Upload' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf url
