{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateUpload
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an app or test scripts.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_CreateUpload.html AWS API Reference> for CreateUpload.
module Network.AWS.DeviceFarm.CreateUpload
    (
    -- * Creating a Request
      createUpload
    , CreateUpload
    -- * Request Lenses
    , cuContentType
    , cuProjectARN
    , cuName
    , cuType

    -- * Destructuring the Response
    , createUploadResponse
    , CreateUploadResponse
    -- * Response Lenses
    , cursUpload
    , cursStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.DeviceFarm.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the create upload operation.
--
-- /See:/ 'createUpload' smart constructor.
data CreateUpload = CreateUpload'
    { _cuContentType :: !(Maybe Text)
    , _cuProjectARN  :: !Text
    , _cuName        :: !Text
    , _cuType        :: !UploadType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuContentType'
--
-- * 'cuProjectARN'
--
-- * 'cuName'
--
-- * 'cuType'
createUpload
    :: Text -- ^ 'cuProjectARN'
    -> Text -- ^ 'cuName'
    -> UploadType -- ^ 'cuType'
    -> CreateUpload
createUpload pProjectARN_ pName_ pType_ =
    CreateUpload'
    { _cuContentType = Nothing
    , _cuProjectARN = pProjectARN_
    , _cuName = pName_
    , _cuType = pType_
    }

-- | The upload\'s content type (for example, \"application\/octet-stream\").
cuContentType :: Lens' CreateUpload (Maybe Text)
cuContentType = lens _cuContentType (\ s a -> s{_cuContentType = a});

-- | The ARN of the project for the upload.
cuProjectARN :: Lens' CreateUpload Text
cuProjectARN = lens _cuProjectARN (\ s a -> s{_cuProjectARN = a});

-- | The upload\'s file name.
cuName :: Lens' CreateUpload Text
cuName = lens _cuName (\ s a -> s{_cuName = a});

-- | The upload\'s upload type.
--
-- Must be one of the following values:
--
-- -   ANDROID_APP: An Android upload.
--
-- -   IOS_APP: An iOS upload.
--
-- -   EXTERNAL_DATA: An external data upload.
--
-- -   APPIUM_JAVA_JUNIT_TEST_PACKAGE: An Appium Java JUnit test package
--     upload.
--
-- -   APPIUM_JAVA_TESTNG_TEST_PACKAGE: An Appium Java TestNG test package
--     upload.
--
-- -   CALABASH_TEST_PACKAGE: A Calabash test package upload.
--
-- -   INSTRUMENTATION_TEST_PACKAGE: An instrumentation upload.
--
-- -   UIAUTOMATOR_TEST_PACKAGE: A uiautomator test package upload.
--
-- -   XCTEST_TEST_PACKAGE: An XCode test package upload.
--
cuType :: Lens' CreateUpload UploadType
cuType = lens _cuType (\ s a -> s{_cuType = a});

instance AWSRequest CreateUpload where
        type Rs CreateUpload = CreateUploadResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 CreateUploadResponse' <$>
                   (x .?> "upload") <*> (pure (fromEnum s)))

instance ToHeaders CreateUpload where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.CreateUpload" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateUpload where
        toJSON CreateUpload'{..}
          = object
              (catMaybes
                 [("contentType" .=) <$> _cuContentType,
                  Just ("projectArn" .= _cuProjectARN),
                  Just ("name" .= _cuName), Just ("type" .= _cuType)])

instance ToPath CreateUpload where
        toPath = const "/"

instance ToQuery CreateUpload where
        toQuery = const mempty

-- | Represents the result of a create upload request.
--
-- /See:/ 'createUploadResponse' smart constructor.
data CreateUploadResponse = CreateUploadResponse'
    { _cursUpload :: !(Maybe Upload)
    , _cursStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cursUpload'
--
-- * 'cursStatus'
createUploadResponse
    :: Int -- ^ 'cursStatus'
    -> CreateUploadResponse
createUploadResponse pStatus_ =
    CreateUploadResponse'
    { _cursUpload = Nothing
    , _cursStatus = pStatus_
    }

-- | The newly created upload.
cursUpload :: Lens' CreateUploadResponse (Maybe Upload)
cursUpload = lens _cursUpload (\ s a -> s{_cursUpload = a});

-- | The response status code.
cursStatus :: Lens' CreateUploadResponse Int
cursStatus = lens _cursStatus (\ s a -> s{_cursStatus = a});
