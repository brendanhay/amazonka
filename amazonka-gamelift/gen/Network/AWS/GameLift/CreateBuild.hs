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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon GameLift build from a set of game server binary files stored in an Amazon Simple Storage Service (Amazon S3) location. To use this API call, create a @.zip@ file containing all of the files for the build and store it in an Amazon S3 bucket under your AWS account. For help on packaging your build files and creating a build, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Uploading Your Game to Amazon GameLift> .
--
--
-- /Important:/ Use this API action ONLY if you are storing your game build files in an Amazon S3 bucket. To create a build using files stored locally, use the CLI command <http://docs.aws.amazon.com/cli/latest/reference/gamelift/upload-build.html @upload-build@ > , which uploads the build files from a file location you specify.
--
-- To create a new build using @CreateBuild@ , identify the storage location and operating system of your game build. You also have the option of specifying a build name and version. If successful, this action creates a new build record with an unique build ID and in @INITIALIZED@ status. Use the API call 'DescribeBuild' to check the status of your build. A build must be in @READY@ status before it can be used to create fleets to host your game.
--
-- Build-related operations include:
--
--     * 'CreateBuild'
--
--     * 'ListBuilds'
--
--     * 'DescribeBuild'
--
--     * 'UpdateBuild'
--
--     * 'DeleteBuild'
--
--
--
module Network.AWS.GameLift.CreateBuild
    (
    -- * Creating a Request
      createBuild
    , CreateBuild
    -- * Request Lenses
    , cbStorageLocation
    , cbOperatingSystem
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

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'createBuild' smart constructor.
data CreateBuild = CreateBuild'
  { _cbStorageLocation :: !(Maybe S3Location)
  , _cbOperatingSystem :: !(Maybe OperatingSystem)
  , _cbName            :: !(Maybe Text)
  , _cbVersion         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBuild' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbStorageLocation' - Amazon S3 location of the game build files to be uploaded. The S3 bucket must be owned by the same AWS account that you're using to manage Amazon GameLift. It also must in the same region that you want to create a new build in. Before calling @CreateBuild@ with this location, you must allow Amazon GameLift to access your Amazon S3 bucket (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build Create a Build with Files in Amazon S3> ).
--
-- * 'cbOperatingSystem' - Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build. If your game build contains multiple executables, they all must run on the same operating system.
--
-- * 'cbName' - Descriptive label that is associated with a build. Build names do not need to be unique. You can use 'UpdateBuild' to change this value later.
--
-- * 'cbVersion' - Version that is associated with this build. Version strings do not need to be unique. You can use 'UpdateBuild' to change this value later.
createBuild
    :: CreateBuild
createBuild =
  CreateBuild'
  { _cbStorageLocation = Nothing
  , _cbOperatingSystem = Nothing
  , _cbName = Nothing
  , _cbVersion = Nothing
  }


-- | Amazon S3 location of the game build files to be uploaded. The S3 bucket must be owned by the same AWS account that you're using to manage Amazon GameLift. It also must in the same region that you want to create a new build in. Before calling @CreateBuild@ with this location, you must allow Amazon GameLift to access your Amazon S3 bucket (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build Create a Build with Files in Amazon S3> ).
cbStorageLocation :: Lens' CreateBuild (Maybe S3Location)
cbStorageLocation = lens _cbStorageLocation (\ s a -> s{_cbStorageLocation = a});

-- | Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build. If your game build contains multiple executables, they all must run on the same operating system.
cbOperatingSystem :: Lens' CreateBuild (Maybe OperatingSystem)
cbOperatingSystem = lens _cbOperatingSystem (\ s a -> s{_cbOperatingSystem = a});

-- | Descriptive label that is associated with a build. Build names do not need to be unique. You can use 'UpdateBuild' to change this value later.
cbName :: Lens' CreateBuild (Maybe Text)
cbName = lens _cbName (\ s a -> s{_cbName = a});

-- | Version that is associated with this build. Version strings do not need to be unique. You can use 'UpdateBuild' to change this value later.
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

instance Hashable CreateBuild where

instance NFData CreateBuild where

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
                  ("OperatingSystem" .=) <$> _cbOperatingSystem,
                  ("Name" .=) <$> _cbName,
                  ("Version" .=) <$> _cbVersion])

instance ToPath CreateBuild where
        toPath = const "/"

instance ToQuery CreateBuild where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'createBuildResponse' smart constructor.
data CreateBuildResponse = CreateBuildResponse'
  { _cbrsStorageLocation   :: !(Maybe S3Location)
  , _cbrsUploadCredentials :: !(Maybe (Sensitive AWSCredentials))
  , _cbrsBuild             :: !(Maybe Build)
  , _cbrsResponseStatus    :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBuildResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsStorageLocation' - Amazon S3 location specified in the request.
--
-- * 'cbrsUploadCredentials' - This element is not currently in use.
--
-- * 'cbrsBuild' - The newly created build record, including a unique build ID and status.
--
-- * 'cbrsResponseStatus' - -- | The response status code.
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


-- | Amazon S3 location specified in the request.
cbrsStorageLocation :: Lens' CreateBuildResponse (Maybe S3Location)
cbrsStorageLocation = lens _cbrsStorageLocation (\ s a -> s{_cbrsStorageLocation = a});

-- | This element is not currently in use.
cbrsUploadCredentials :: Lens' CreateBuildResponse (Maybe AWSCredentials)
cbrsUploadCredentials = lens _cbrsUploadCredentials (\ s a -> s{_cbrsUploadCredentials = a}) . mapping _Sensitive;

-- | The newly created build record, including a unique build ID and status.
cbrsBuild :: Lens' CreateBuildResponse (Maybe Build)
cbrsBuild = lens _cbrsBuild (\ s a -> s{_cbrsBuild = a});

-- | -- | The response status code.
cbrsResponseStatus :: Lens' CreateBuildResponse Int
cbrsResponseStatus = lens _cbrsResponseStatus (\ s a -> s{_cbrsResponseStatus = a});

instance NFData CreateBuildResponse where
