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
-- Module      : Network.AWS.GameLift.RequestUploadCredentials
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a fresh set of upload credentials and the assigned Amazon S3
-- storage location for a specific build. Valid credentials are required to
-- upload your game build files to Amazon S3.
--
-- Call this action only if you need credentials for a build created with
-- < CreateBuild>. This is a rare situation; in most cases, builds are
-- created using the CLI command 'upload-build', which creates a build
-- record and also uploads build files.
--
-- Upload credentials are returned when you create the build, but they have
-- a limited lifespan. You can get fresh credentials and use them to
-- re-upload game files until the state of that build changes to READY.
-- Once this happens, you must create a brand new build.
module Network.AWS.GameLift.RequestUploadCredentials
    (
    -- * Creating a Request
      requestUploadCredentials
    , RequestUploadCredentials
    -- * Request Lenses
    , rucBuildId

    -- * Destructuring the Response
    , requestUploadCredentialsResponse
    , RequestUploadCredentialsResponse
    -- * Response Lenses
    , rucrsStorageLocation
    , rucrsUploadCredentials
    , rucrsResponseStatus
    ) where

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for a request action.
--
-- /See:/ 'requestUploadCredentials' smart constructor.
newtype RequestUploadCredentials = RequestUploadCredentials'
    { _rucBuildId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RequestUploadCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rucBuildId'
requestUploadCredentials
    :: Text -- ^ 'rucBuildId'
    -> RequestUploadCredentials
requestUploadCredentials pBuildId_ =
    RequestUploadCredentials'
    { _rucBuildId = pBuildId_
    }

-- | Unique identifier for the build you want to get credentials for.
rucBuildId :: Lens' RequestUploadCredentials Text
rucBuildId = lens _rucBuildId (\ s a -> s{_rucBuildId = a});

instance AWSRequest RequestUploadCredentials where
        type Rs RequestUploadCredentials =
             RequestUploadCredentialsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 RequestUploadCredentialsResponse' <$>
                   (x .?> "StorageLocation") <*>
                     (x .?> "UploadCredentials")
                     <*> (pure (fromEnum s)))

instance Hashable RequestUploadCredentials

instance ToHeaders RequestUploadCredentials where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.RequestUploadCredentials" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RequestUploadCredentials where
        toJSON RequestUploadCredentials'{..}
          = object
              (catMaybes [Just ("BuildId" .= _rucBuildId)])

instance ToPath RequestUploadCredentials where
        toPath = const "/"

instance ToQuery RequestUploadCredentials where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
-- /See:/ 'requestUploadCredentialsResponse' smart constructor.
data RequestUploadCredentialsResponse = RequestUploadCredentialsResponse'
    { _rucrsStorageLocation   :: !(Maybe S3Location)
    , _rucrsUploadCredentials :: !(Maybe (Sensitive AWSCredentials))
    , _rucrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RequestUploadCredentialsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rucrsStorageLocation'
--
-- * 'rucrsUploadCredentials'
--
-- * 'rucrsResponseStatus'
requestUploadCredentialsResponse
    :: Int -- ^ 'rucrsResponseStatus'
    -> RequestUploadCredentialsResponse
requestUploadCredentialsResponse pResponseStatus_ =
    RequestUploadCredentialsResponse'
    { _rucrsStorageLocation = Nothing
    , _rucrsUploadCredentials = Nothing
    , _rucrsResponseStatus = pResponseStatus_
    }

-- | Amazon S3 path and key, identifying where the game build files are
-- stored.
rucrsStorageLocation :: Lens' RequestUploadCredentialsResponse (Maybe S3Location)
rucrsStorageLocation = lens _rucrsStorageLocation (\ s a -> s{_rucrsStorageLocation = a});

-- | AWS credentials required when uploading a game build to the storage
-- location. These credentials have a limited lifespan and are valid only
-- for the build they were issued for.
rucrsUploadCredentials :: Lens' RequestUploadCredentialsResponse (Maybe AWSCredentials)
rucrsUploadCredentials = lens _rucrsUploadCredentials (\ s a -> s{_rucrsUploadCredentials = a}) . mapping _Sensitive;

-- | The response status code.
rucrsResponseStatus :: Lens' RequestUploadCredentialsResponse Int
rucrsResponseStatus = lens _rucrsResponseStatus (\ s a -> s{_rucrsResponseStatus = a});
