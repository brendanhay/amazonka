{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateUpload
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Uploads an app or test scripts.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_CreateUpload.html>
module Network.AWS.DeviceFarm.CreateUpload
    (
    -- * Request
      CreateUpload
    -- ** Request constructor
    , createUpload
    -- ** Request lenses
    , cuContentType
    , cuProjectARN
    , cuName
    , cuType

    -- * Response
    , CreateUploadResponse
    -- ** Response constructor
    , createUploadResponse
    -- ** Response lenses
    , cursUpload
    , cursStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the create upload operation.
--
-- /See:/ 'createUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cuContentType'
--
-- * 'cuProjectARN'
--
-- * 'cuName'
--
-- * 'cuType'
data CreateUpload = CreateUpload'
    { _cuContentType :: !(Maybe Text)
    , _cuProjectARN  :: !Text
    , _cuName        :: !Text
    , _cuType        :: !UploadType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateUpload' smart constructor.
createUpload :: Text -> Text -> UploadType -> CreateUpload
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
-- -   APPIUM_JAVA_JUNIT_TEST_PACKAGE: An Appium Java JUnit test package
--     upload.
--
-- -   APPIUM_JAVA_TESTNG_TEST_PACKAGE: An Appium Java TestNG test package
--     upload.
--
-- -   CALABASH_TEST_PACKAGE: A Calabash test package upload.
--
-- -   EXTERNAL_DATA: An external data upload.
--
-- -   INSTRUMENTATION_TEST_PACKAGE: An instrumentation upload.
--
-- -   UIAUTOMATOR_TEST_PACKAGE: A uiautomator test package upload.
--
cuType :: Lens' CreateUpload UploadType
cuType = lens _cuType (\ s a -> s{_cuType = a});

instance AWSRequest CreateUpload where
        type Sv CreateUpload = DeviceFarm
        type Rs CreateUpload = CreateUploadResponse
        request = postJSON
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
              ["contentType" .= _cuContentType,
               "projectArn" .= _cuProjectARN, "name" .= _cuName,
               "type" .= _cuType]

instance ToPath CreateUpload where
        toPath = const "/"

instance ToQuery CreateUpload where
        toQuery = const mempty

-- | Represents the result of a create upload request.
--
-- /See:/ 'createUploadResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cursUpload'
--
-- * 'cursStatus'
data CreateUploadResponse = CreateUploadResponse'
    { _cursUpload :: !(Maybe Upload)
    , _cursStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateUploadResponse' smart constructor.
createUploadResponse :: Int -> CreateUploadResponse
createUploadResponse pStatus_ =
    CreateUploadResponse'
    { _cursUpload = Nothing
    , _cursStatus = pStatus_
    }

-- | The newly created upload.
cursUpload :: Lens' CreateUploadResponse (Maybe Upload)
cursUpload = lens _cursUpload (\ s a -> s{_cursUpload = a});

-- | FIXME: Undocumented member.
cursStatus :: Lens' CreateUploadResponse Int
cursStatus = lens _cursStatus (\ s a -> s{_cursStatus = a});
