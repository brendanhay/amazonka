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
-- Module      : Network.AWS.DeviceFarm.ListUploads
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about uploads, given an AWS Device Farm project ARN.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListUploads
  ( -- * Creating a Request
    ListUploads (..),
    newListUploads,

    -- * Request Lenses
    listUploads_nextToken,
    listUploads_type,
    listUploads_arn,

    -- * Destructuring the Response
    ListUploadsResponse (..),
    newListUploadsResponse,

    -- * Response Lenses
    listUploadsResponse_nextToken,
    listUploadsResponse_uploads,
    listUploadsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list uploads operation.
--
-- /See:/ 'newListUploads' smart constructor.
data ListUploads = ListUploads'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The type of upload.
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
    type' :: Core.Maybe UploadType,
    -- | The Amazon Resource Name (ARN) of the project for which you want to list
    -- uploads.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUploads' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUploads_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'type'', 'listUploads_type' - The type of upload.
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
-- 'arn', 'listUploads_arn' - The Amazon Resource Name (ARN) of the project for which you want to list
-- uploads.
newListUploads ::
  -- | 'arn'
  Core.Text ->
  ListUploads
newListUploads pArn_ =
  ListUploads'
    { nextToken = Core.Nothing,
      type' = Core.Nothing,
      arn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUploads_nextToken :: Lens.Lens' ListUploads (Core.Maybe Core.Text)
listUploads_nextToken = Lens.lens (\ListUploads' {nextToken} -> nextToken) (\s@ListUploads' {} a -> s {nextToken = a} :: ListUploads)

-- | The type of upload.
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
listUploads_type :: Lens.Lens' ListUploads (Core.Maybe UploadType)
listUploads_type = Lens.lens (\ListUploads' {type'} -> type') (\s@ListUploads' {} a -> s {type' = a} :: ListUploads)

-- | The Amazon Resource Name (ARN) of the project for which you want to list
-- uploads.
listUploads_arn :: Lens.Lens' ListUploads Core.Text
listUploads_arn = Lens.lens (\ListUploads' {arn} -> arn) (\s@ListUploads' {} a -> s {arn = a} :: ListUploads)

instance Core.AWSPager ListUploads where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUploadsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listUploadsResponse_uploads Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listUploads_nextToken
          Lens..~ rs
          Lens.^? listUploadsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListUploads where
  type AWSResponse ListUploads = ListUploadsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUploadsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "uploads" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUploads

instance Core.NFData ListUploads

instance Core.ToHeaders ListUploads where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListUploads" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListUploads where
  toJSON ListUploads' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("type" Core..=) Core.<$> type',
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath ListUploads where
  toPath = Core.const "/"

instance Core.ToQuery ListUploads where
  toQuery = Core.const Core.mempty

-- | Represents the result of a list uploads request.
--
-- /See:/ 'newListUploadsResponse' smart constructor.
data ListUploadsResponse = ListUploadsResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the uploads.
    uploads :: Core.Maybe [Upload],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUploadsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUploadsResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'uploads', 'listUploadsResponse_uploads' - Information about the uploads.
--
-- 'httpStatus', 'listUploadsResponse_httpStatus' - The response's http status code.
newListUploadsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUploadsResponse
newListUploadsResponse pHttpStatus_ =
  ListUploadsResponse'
    { nextToken = Core.Nothing,
      uploads = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listUploadsResponse_nextToken :: Lens.Lens' ListUploadsResponse (Core.Maybe Core.Text)
listUploadsResponse_nextToken = Lens.lens (\ListUploadsResponse' {nextToken} -> nextToken) (\s@ListUploadsResponse' {} a -> s {nextToken = a} :: ListUploadsResponse)

-- | Information about the uploads.
listUploadsResponse_uploads :: Lens.Lens' ListUploadsResponse (Core.Maybe [Upload])
listUploadsResponse_uploads = Lens.lens (\ListUploadsResponse' {uploads} -> uploads) (\s@ListUploadsResponse' {} a -> s {uploads = a} :: ListUploadsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listUploadsResponse_httpStatus :: Lens.Lens' ListUploadsResponse Core.Int
listUploadsResponse_httpStatus = Lens.lens (\ListUploadsResponse' {httpStatus} -> httpStatus) (\s@ListUploadsResponse' {} a -> s {httpStatus = a} :: ListUploadsResponse)

instance Core.NFData ListUploadsResponse
