{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListUploads (..),
    mkListUploads,

    -- ** Request lenses
    luArn,
    luNextToken,
    luType,

    -- * Destructuring the response
    ListUploadsResponse (..),
    mkListUploadsResponse,

    -- ** Response lenses
    lursNextToken,
    lursUploads,
    lursResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the list uploads operation.
--
-- /See:/ 'mkListUploads' smart constructor.
data ListUploads = ListUploads'
  { -- | The Amazon Resource Name (ARN) of the project for which you want to list uploads.
    arn :: Lude.Text,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
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
    type' :: Lude.Maybe UploadType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUploads' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the project for which you want to list uploads.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'type'' - The type of upload.
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
mkListUploads ::
  -- | 'arn'
  Lude.Text ->
  ListUploads
mkListUploads pArn_ =
  ListUploads'
    { arn = pArn_,
      nextToken = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project for which you want to list uploads.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luArn :: Lens.Lens' ListUploads Lude.Text
luArn = Lens.lens (arn :: ListUploads -> Lude.Text) (\s a -> s {arn = a} :: ListUploads)
{-# DEPRECATED luArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luNextToken :: Lens.Lens' ListUploads (Lude.Maybe Lude.Text)
luNextToken = Lens.lens (nextToken :: ListUploads -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUploads)
{-# DEPRECATED luNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

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
luType :: Lens.Lens' ListUploads (Lude.Maybe UploadType)
luType = Lens.lens (type' :: ListUploads -> Lude.Maybe UploadType) (\s a -> s {type' = a} :: ListUploads)
{-# DEPRECATED luType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Page.AWSPager ListUploads where
  page rq rs
    | Page.stop (rs Lens.^. lursNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lursUploads) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& luNextToken Lens..~ rs Lens.^. lursNextToken

instance Lude.AWSRequest ListUploads where
  type Rs ListUploads = ListUploadsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUploadsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "uploads" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUploads where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListUploads" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListUploads where
  toJSON ListUploads' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("arn" Lude..= arn),
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("type" Lude..=) Lude.<$> type'
          ]
      )

instance Lude.ToPath ListUploads where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUploads where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a list uploads request.
--
-- /See:/ 'mkListUploadsResponse' smart constructor.
data ListUploadsResponse = ListUploadsResponse'
  { -- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the uploads.
    uploads :: Lude.Maybe [Upload],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUploadsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
-- * 'uploads' - Information about the uploads.
-- * 'responseStatus' - The response status code.
mkListUploadsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUploadsResponse
mkListUploadsResponse pResponseStatus_ =
  ListUploadsResponse'
    { nextToken = Lude.Nothing,
      uploads = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursNextToken :: Lens.Lens' ListUploadsResponse (Lude.Maybe Lude.Text)
lursNextToken = Lens.lens (nextToken :: ListUploadsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUploadsResponse)
{-# DEPRECATED lursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the uploads.
--
-- /Note:/ Consider using 'uploads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursUploads :: Lens.Lens' ListUploadsResponse (Lude.Maybe [Upload])
lursUploads = Lens.lens (uploads :: ListUploadsResponse -> Lude.Maybe [Upload]) (\s a -> s {uploads = a} :: ListUploadsResponse)
{-# DEPRECATED lursUploads "Use generic-lens or generic-optics with 'uploads' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lursResponseStatus :: Lens.Lens' ListUploadsResponse Lude.Int
lursResponseStatus = Lens.lens (responseStatus :: ListUploadsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUploadsResponse)
{-# DEPRECATED lursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
