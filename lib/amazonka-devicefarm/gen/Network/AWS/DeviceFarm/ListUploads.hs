{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListUploads
  ( -- * Creating a Request
    listUploads,
    ListUploads,

    -- * Request Lenses
    luNextToken,
    luType,
    luArn,

    -- * Destructuring the Response
    listUploadsResponse,
    ListUploadsResponse,

    -- * Response Lenses
    lursNextToken,
    lursUploads,
    lursResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the list uploads operation.
--
--
--
-- /See:/ 'listUploads' smart constructor.
data ListUploads = ListUploads'
  { _luNextToken :: !(Maybe Text),
    _luType :: !(Maybe UploadType),
    _luArn :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListUploads' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'luType' - The type of upload. Must be one of the following values:     * ANDROID_APP     * IOS_APP     * WEB_APP     * EXTERNAL_DATA     * APPIUM_JAVA_JUNIT_TEST_PACKAGE     * APPIUM_JAVA_TESTNG_TEST_PACKAGE     * APPIUM_PYTHON_TEST_PACKAGE     * APPIUM_NODE_TEST_PACKAGE     * APPIUM_RUBY_TEST_PACKAGE     * APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE     * APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE     * APPIUM_WEB_PYTHON_TEST_PACKAGE     * APPIUM_WEB_NODE_TEST_PACKAGE     * APPIUM_WEB_RUBY_TEST_PACKAGE     * CALABASH_TEST_PACKAGE     * INSTRUMENTATION_TEST_PACKAGE     * UIAUTOMATION_TEST_PACKAGE     * UIAUTOMATOR_TEST_PACKAGE     * XCTEST_TEST_PACKAGE     * XCTEST_UI_TEST_PACKAGE     * APPIUM_JAVA_JUNIT_TEST_SPEC     * APPIUM_JAVA_TESTNG_TEST_SPEC     * APPIUM_PYTHON_TEST_SPEC     * APPIUM_NODE_TEST_SPEC     * APPIUM_RUBY_TEST_SPEC     * APPIUM_WEB_JAVA_JUNIT_TEST_SPEC     * APPIUM_WEB_JAVA_TESTNG_TEST_SPEC     * APPIUM_WEB_PYTHON_TEST_SPEC     * APPIUM_WEB_NODE_TEST_SPEC     * APPIUM_WEB_RUBY_TEST_SPEC     * INSTRUMENTATION_TEST_SPEC     * XCTEST_UI_TEST_SPEC
--
-- * 'luArn' - The Amazon Resource Name (ARN) of the project for which you want to list uploads.
listUploads ::
  -- | 'luArn'
  Text ->
  ListUploads
listUploads pArn_ =
  ListUploads'
    { _luNextToken = Nothing,
      _luType = Nothing,
      _luArn = pArn_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
luNextToken :: Lens' ListUploads (Maybe Text)
luNextToken = lens _luNextToken (\s a -> s {_luNextToken = a})

-- | The type of upload. Must be one of the following values:     * ANDROID_APP     * IOS_APP     * WEB_APP     * EXTERNAL_DATA     * APPIUM_JAVA_JUNIT_TEST_PACKAGE     * APPIUM_JAVA_TESTNG_TEST_PACKAGE     * APPIUM_PYTHON_TEST_PACKAGE     * APPIUM_NODE_TEST_PACKAGE     * APPIUM_RUBY_TEST_PACKAGE     * APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE     * APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE     * APPIUM_WEB_PYTHON_TEST_PACKAGE     * APPIUM_WEB_NODE_TEST_PACKAGE     * APPIUM_WEB_RUBY_TEST_PACKAGE     * CALABASH_TEST_PACKAGE     * INSTRUMENTATION_TEST_PACKAGE     * UIAUTOMATION_TEST_PACKAGE     * UIAUTOMATOR_TEST_PACKAGE     * XCTEST_TEST_PACKAGE     * XCTEST_UI_TEST_PACKAGE     * APPIUM_JAVA_JUNIT_TEST_SPEC     * APPIUM_JAVA_TESTNG_TEST_SPEC     * APPIUM_PYTHON_TEST_SPEC     * APPIUM_NODE_TEST_SPEC     * APPIUM_RUBY_TEST_SPEC     * APPIUM_WEB_JAVA_JUNIT_TEST_SPEC     * APPIUM_WEB_JAVA_TESTNG_TEST_SPEC     * APPIUM_WEB_PYTHON_TEST_SPEC     * APPIUM_WEB_NODE_TEST_SPEC     * APPIUM_WEB_RUBY_TEST_SPEC     * INSTRUMENTATION_TEST_SPEC     * XCTEST_UI_TEST_SPEC
luType :: Lens' ListUploads (Maybe UploadType)
luType = lens _luType (\s a -> s {_luType = a})

-- | The Amazon Resource Name (ARN) of the project for which you want to list uploads.
luArn :: Lens' ListUploads Text
luArn = lens _luArn (\s a -> s {_luArn = a})

instance AWSPager ListUploads where
  page rq rs
    | stop (rs ^. lursNextToken) = Nothing
    | stop (rs ^. lursUploads) = Nothing
    | otherwise = Just $ rq & luNextToken .~ rs ^. lursNextToken

instance AWSRequest ListUploads where
  type Rs ListUploads = ListUploadsResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          ListUploadsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "uploads" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListUploads

instance NFData ListUploads

instance ToHeaders ListUploads where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.ListUploads" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListUploads where
  toJSON ListUploads' {..} =
    object
      ( catMaybes
          [ ("nextToken" .=) <$> _luNextToken,
            ("type" .=) <$> _luType,
            Just ("arn" .= _luArn)
          ]
      )

instance ToPath ListUploads where
  toPath = const "/"

instance ToQuery ListUploads where
  toQuery = const mempty

-- | Represents the result of a list uploads request.
--
--
--
-- /See:/ 'listUploadsResponse' smart constructor.
data ListUploadsResponse = ListUploadsResponse'
  { _lursNextToken ::
      !(Maybe Text),
    _lursUploads :: !(Maybe [Upload]),
    _lursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListUploadsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lursNextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- * 'lursUploads' - Information about the uploads.
--
-- * 'lursResponseStatus' - -- | The response status code.
listUploadsResponse ::
  -- | 'lursResponseStatus'
  Int ->
  ListUploadsResponse
listUploadsResponse pResponseStatus_ =
  ListUploadsResponse'
    { _lursNextToken = Nothing,
      _lursUploads = Nothing,
      _lursResponseStatus = pResponseStatus_
    }

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
lursNextToken :: Lens' ListUploadsResponse (Maybe Text)
lursNextToken = lens _lursNextToken (\s a -> s {_lursNextToken = a})

-- | Information about the uploads.
lursUploads :: Lens' ListUploadsResponse [Upload]
lursUploads = lens _lursUploads (\s a -> s {_lursUploads = a}) . _Default . _Coerce

-- | -- | The response status code.
lursResponseStatus :: Lens' ListUploadsResponse Int
lursResponseStatus = lens _lursResponseStatus (\s a -> s {_lursResponseStatus = a})

instance NFData ListUploadsResponse
