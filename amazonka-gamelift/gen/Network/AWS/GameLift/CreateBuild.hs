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
-- Module      : Network.AWS.GameLift.CreateBuild
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initializes a new build record and generates information required to upload a game build to Amazon GameLift. Once the build record has been created and its status is 'INITIALIZED', you can upload your game build.
--
-- Do not use this API action unless you are using your own Amazon Simple Storage Service (Amazon S3) client and need to manually upload your build files. Instead, to create a build, use the CLI command 'upload-build', which creates a new build record and uploads the build files in one step. (See the <http://docs.aws.amazon.com/gamelift/latest/developerguide/ Amazon GameLift Developer Guide> for more details on the CLI and the upload process.)
--
-- To create a new build, optionally specify a build name and version. This metadata is stored with other properties in the build record and is displayed in the GameLift console (it is not visible to players). If successful, this action returns the newly created build record along with the Amazon S3 storage location and AWS account credentials. Use the location and credentials to upload your game build.
module Network.AWS.GameLift.CreateBuild
    (
    -- * Creating a Request
      createBuild
    , CreateBuild
    -- * Request Lenses
    , cbStorageLocation
    , cbName
    , cbVersion

    -- * Destructuring the Response
    , createBuildResponse
    , CreateBuildResponse
    -- * Response Lenses
    , cbrsStorageLocation
    , cbrsUploadCredentials
    , cbrsBuild
    , cbrsResponseStatus
    ) where

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for a request action.
--
-- /See:/ 'createBuild' smart constructor.
data CreateBuild = CreateBuild'
    { _cbStorageLocation :: !(Maybe S3Location)
    , _cbName            :: !(Maybe Text)
    , _cbVersion         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateBuild' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbStorageLocation'
--
-- * 'cbName'
--
-- * 'cbVersion'
createBuild
    :: CreateBuild
createBuild =
    CreateBuild'
    { _cbStorageLocation = Nothing
    , _cbName = Nothing
    , _cbVersion = Nothing
    }

-- | Undocumented member.
cbStorageLocation :: Lens' CreateBuild (Maybe S3Location)
cbStorageLocation = lens _cbStorageLocation (\ s a -> s{_cbStorageLocation = a});

-- | Descriptive label associated with a build. Build names do not need to be unique. A build name can be changed later using 'UpdateBuild'.
cbName :: Lens' CreateBuild (Maybe Text)
cbName = lens _cbName (\ s a -> s{_cbName = a});

-- | Version associated with this build. Version strings do not need to be unique to a build. A build version can be changed later using 'UpdateBuild'.
cbVersion :: Lens' CreateBuild (Maybe Text)
cbVersion = lens _cbVersion (\ s a -> s{_cbVersion = a});

instance AWSRequest CreateBuild where
        type Rs CreateBuild = CreateBuildResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreateBuildResponse' <$>
                   (x .?> "StorageLocation") <*>
                     (x .?> "UploadCredentials")
                     <*> (x .?> "Build")
                     <*> (pure (fromEnum s)))

instance Hashable CreateBuild

instance NFData CreateBuild

instance ToHeaders CreateBuild where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.CreateBuild" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBuild where
        toJSON CreateBuild'{..}
          = object
              (catMaybes
                 [("StorageLocation" .=) <$> _cbStorageLocation,
                  ("Name" .=) <$> _cbName,
                  ("Version" .=) <$> _cbVersion])

instance ToPath CreateBuild where
        toPath = const "/"

instance ToQuery CreateBuild where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
-- /See:/ 'createBuildResponse' smart constructor.
data CreateBuildResponse = CreateBuildResponse'
    { _cbrsStorageLocation   :: !(Maybe S3Location)
    , _cbrsUploadCredentials :: !(Maybe (Sensitive AWSCredentials))
    , _cbrsBuild             :: !(Maybe Build)
    , _cbrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateBuildResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsStorageLocation'
--
-- * 'cbrsUploadCredentials'
--
-- * 'cbrsBuild'
--
-- * 'cbrsResponseStatus'
createBuildResponse
    :: Int -- ^ 'cbrsResponseStatus'
    -> CreateBuildResponse
createBuildResponse pResponseStatus_ =
    CreateBuildResponse'
    { _cbrsStorageLocation = Nothing
    , _cbrsUploadCredentials = Nothing
    , _cbrsBuild = Nothing
    , _cbrsResponseStatus = pResponseStatus_
    }

-- | Amazon S3 path and key, identifying where the game build files are stored.
cbrsStorageLocation :: Lens' CreateBuildResponse (Maybe S3Location)
cbrsStorageLocation = lens _cbrsStorageLocation (\ s a -> s{_cbrsStorageLocation = a});

-- | AWS credentials required when uploading a game build to the storage location. These credentials have a limited lifespan and are valid only for the build they were issued for. If you need to get fresh credentials, call 'RequestUploadCredentials'.
cbrsUploadCredentials :: Lens' CreateBuildResponse (Maybe AWSCredentials)
cbrsUploadCredentials = lens _cbrsUploadCredentials (\ s a -> s{_cbrsUploadCredentials = a}) . mapping _Sensitive;

-- | Set of properties for the newly created build.
cbrsBuild :: Lens' CreateBuildResponse (Maybe Build)
cbrsBuild = lens _cbrsBuild (\ s a -> s{_cbrsBuild = a});

-- | The response status code.
cbrsResponseStatus :: Lens' CreateBuildResponse Int
cbrsResponseStatus = lens _cbrsResponseStatus (\ s a -> s{_cbrsResponseStatus = a});

instance NFData CreateBuildResponse
