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
-- Module      : Network.AWS.DeviceFarm.CreateUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an app or test scripts.
module Network.AWS.DeviceFarm.CreateUpload
  ( -- * Creating a Request
    createUpload,
    CreateUpload,

    -- * Request Lenses
    cuContentType,
    cuProjectARN,
    cuName,
    cuType,

    -- * Destructuring the Response
    createUploadResponse,
    CreateUploadResponse,

    -- * Response Lenses
    cursUpload,
    cursResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the create upload operation.
--
--
--
-- /See:/ 'createUpload' smart constructor.
data CreateUpload = CreateUpload'
  { _cuContentType :: !(Maybe Text),
    _cuProjectARN :: !Text,
    _cuName :: !Text,
    _cuType :: !UploadType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuContentType' - The upload's content type (for example, @application/octet-stream@ ).
--
-- * 'cuProjectARN' - The ARN of the project for the upload.
--
-- * 'cuName' - The upload's file name. The name should not contain any forward slashes (@/@ ). If you are uploading an iOS app, the file name must end with the @.ipa@ extension. If you are uploading an Android app, the file name must end with the @.apk@ extension. For all others, the file name must end with the @.zip@ file extension.
--
-- * 'cuType' - The upload's upload type. Must be one of the following values:     * ANDROID_APP     * IOS_APP     * WEB_APP     * EXTERNAL_DATA     * APPIUM_JAVA_JUNIT_TEST_PACKAGE     * APPIUM_JAVA_TESTNG_TEST_PACKAGE     * APPIUM_PYTHON_TEST_PACKAGE     * APPIUM_NODE_TEST_PACKAGE     * APPIUM_RUBY_TEST_PACKAGE     * APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE     * APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE     * APPIUM_WEB_PYTHON_TEST_PACKAGE     * APPIUM_WEB_NODE_TEST_PACKAGE     * APPIUM_WEB_RUBY_TEST_PACKAGE     * CALABASH_TEST_PACKAGE     * INSTRUMENTATION_TEST_PACKAGE     * UIAUTOMATION_TEST_PACKAGE     * UIAUTOMATOR_TEST_PACKAGE     * XCTEST_TEST_PACKAGE     * XCTEST_UI_TEST_PACKAGE     * APPIUM_JAVA_JUNIT_TEST_SPEC     * APPIUM_JAVA_TESTNG_TEST_SPEC     * APPIUM_PYTHON_TEST_SPEC     * APPIUM_NODE_TEST_SPEC     * APPIUM_RUBY_TEST_SPEC     * APPIUM_WEB_JAVA_JUNIT_TEST_SPEC     * APPIUM_WEB_JAVA_TESTNG_TEST_SPEC     * APPIUM_WEB_PYTHON_TEST_SPEC     * APPIUM_WEB_NODE_TEST_SPEC     * APPIUM_WEB_RUBY_TEST_SPEC     * INSTRUMENTATION_TEST_SPEC     * XCTEST_UI_TEST_SPEC If you call @CreateUpload@ with @WEB_APP@ specified, AWS Device Farm throws an @ArgumentException@ error.
createUpload ::
  -- | 'cuProjectARN'
  Text ->
  -- | 'cuName'
  Text ->
  -- | 'cuType'
  UploadType ->
  CreateUpload
createUpload pProjectARN_ pName_ pType_ =
  CreateUpload'
    { _cuContentType = Nothing,
      _cuProjectARN = pProjectARN_,
      _cuName = pName_,
      _cuType = pType_
    }

-- | The upload's content type (for example, @application/octet-stream@ ).
cuContentType :: Lens' CreateUpload (Maybe Text)
cuContentType = lens _cuContentType (\s a -> s {_cuContentType = a})

-- | The ARN of the project for the upload.
cuProjectARN :: Lens' CreateUpload Text
cuProjectARN = lens _cuProjectARN (\s a -> s {_cuProjectARN = a})

-- | The upload's file name. The name should not contain any forward slashes (@/@ ). If you are uploading an iOS app, the file name must end with the @.ipa@ extension. If you are uploading an Android app, the file name must end with the @.apk@ extension. For all others, the file name must end with the @.zip@ file extension.
cuName :: Lens' CreateUpload Text
cuName = lens _cuName (\s a -> s {_cuName = a})

-- | The upload's upload type. Must be one of the following values:     * ANDROID_APP     * IOS_APP     * WEB_APP     * EXTERNAL_DATA     * APPIUM_JAVA_JUNIT_TEST_PACKAGE     * APPIUM_JAVA_TESTNG_TEST_PACKAGE     * APPIUM_PYTHON_TEST_PACKAGE     * APPIUM_NODE_TEST_PACKAGE     * APPIUM_RUBY_TEST_PACKAGE     * APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE     * APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE     * APPIUM_WEB_PYTHON_TEST_PACKAGE     * APPIUM_WEB_NODE_TEST_PACKAGE     * APPIUM_WEB_RUBY_TEST_PACKAGE     * CALABASH_TEST_PACKAGE     * INSTRUMENTATION_TEST_PACKAGE     * UIAUTOMATION_TEST_PACKAGE     * UIAUTOMATOR_TEST_PACKAGE     * XCTEST_TEST_PACKAGE     * XCTEST_UI_TEST_PACKAGE     * APPIUM_JAVA_JUNIT_TEST_SPEC     * APPIUM_JAVA_TESTNG_TEST_SPEC     * APPIUM_PYTHON_TEST_SPEC     * APPIUM_NODE_TEST_SPEC     * APPIUM_RUBY_TEST_SPEC     * APPIUM_WEB_JAVA_JUNIT_TEST_SPEC     * APPIUM_WEB_JAVA_TESTNG_TEST_SPEC     * APPIUM_WEB_PYTHON_TEST_SPEC     * APPIUM_WEB_NODE_TEST_SPEC     * APPIUM_WEB_RUBY_TEST_SPEC     * INSTRUMENTATION_TEST_SPEC     * XCTEST_UI_TEST_SPEC If you call @CreateUpload@ with @WEB_APP@ specified, AWS Device Farm throws an @ArgumentException@ error.
cuType :: Lens' CreateUpload UploadType
cuType = lens _cuType (\s a -> s {_cuType = a})

instance AWSRequest CreateUpload where
  type Rs CreateUpload = CreateUploadResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          CreateUploadResponse' <$> (x .?> "upload") <*> (pure (fromEnum s))
      )

instance Hashable CreateUpload

instance NFData CreateUpload

instance ToHeaders CreateUpload where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.CreateUpload" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateUpload where
  toJSON CreateUpload' {..} =
    object
      ( catMaybes
          [ ("contentType" .=) <$> _cuContentType,
            Just ("projectArn" .= _cuProjectARN),
            Just ("name" .= _cuName),
            Just ("type" .= _cuType)
          ]
      )

instance ToPath CreateUpload where
  toPath = const "/"

instance ToQuery CreateUpload where
  toQuery = const mempty

-- | Represents the result of a create upload request.
--
--
--
-- /See:/ 'createUploadResponse' smart constructor.
data CreateUploadResponse = CreateUploadResponse'
  { _cursUpload ::
      !(Maybe Upload),
    _cursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cursUpload' - The newly created upload.
--
-- * 'cursResponseStatus' - -- | The response status code.
createUploadResponse ::
  -- | 'cursResponseStatus'
  Int ->
  CreateUploadResponse
createUploadResponse pResponseStatus_ =
  CreateUploadResponse'
    { _cursUpload = Nothing,
      _cursResponseStatus = pResponseStatus_
    }

-- | The newly created upload.
cursUpload :: Lens' CreateUploadResponse (Maybe Upload)
cursUpload = lens _cursUpload (\s a -> s {_cursUpload = a})

-- | -- | The response status code.
cursResponseStatus :: Lens' CreateUploadResponse Int
cursResponseStatus = lens _cursResponseStatus (\s a -> s {_cursResponseStatus = a})

instance NFData CreateUploadResponse
