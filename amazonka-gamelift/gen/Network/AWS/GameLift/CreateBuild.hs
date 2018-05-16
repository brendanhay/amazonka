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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon GameLift build record for your game server binary files and points to the location of your game server build files in an Amazon Simple Storage Service (Amazon S3) location.
--
--
-- Game server binaries must be combined into a @.zip@ file for use with Amazon GameLift. See <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Uploading Your Game> for more information.
--
-- /Important:/ To create new builds quickly and easily, use the AWS CLI command __<http://docs.aws.amazon.com/cli/latest/reference/gamelift/upload-build.html upload-build> __ . This helper command uploads your build and creates a new build record in one step, and automatically handles the necessary permissions. See <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html Upload Build Files to Amazon GameLift> for more help.
--
-- The @CreateBuild@ operation should be used only when you need to manually upload your build files, as in the following scenarios:
--
--     * Store a build file in an Amazon S3 bucket under your own AWS account. To use this option, you must first give Amazon GameLift access to that Amazon S3 bucket. See <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build Create a Build with Files in Amazon S3> for detailed help. To create a new build record using files in your Amazon S3 bucket, call @CreateBuild@ and specify a build name, operating system, and the storage location of your game build.
--
--     * Upload a build file directly to Amazon GameLift's Amazon S3 account. To use this option, you first call @CreateBuild@ with a build name and operating system. This action creates a new build record and returns an Amazon S3 storage location (bucket and key only) and temporary access credentials. Use the credentials to manually upload your build file to the storage location (see the Amazon S3 topic <http://docs.aws.amazon.com/AmazonS3/latest/dev/UploadingObjects.html Uploading Objects> ). You can upload files to a location only once.
--
--
--
-- If successful, this operation creates a new build record with a unique build ID and places it in @INITIALIZED@ status. You can use 'DescribeBuild' to check the status of your build. A build must be in @READY@ status before it can be used to create fleets.
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
-- * 'cbStorageLocation' - Information indicating where your game build files are stored. Use this parameter only when creating a build with files stored in an Amazon S3 bucket that you own. The storage location must specify an Amazon S3 bucket name and key, as well as a role ARN that you set up to allow Amazon GameLift to access your Amazon S3 bucket. The S3 bucket must be in the same region that you want to create a new build in.
--
-- * 'cbOperatingSystem' - Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build. If your game build contains multiple executables, they all must run on the same operating system. If an operating system is not specified when creating a build, Amazon GameLift uses the default value (WINDOWS_2012). This value cannot be changed later.
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


-- | Information indicating where your game build files are stored. Use this parameter only when creating a build with files stored in an Amazon S3 bucket that you own. The storage location must specify an Amazon S3 bucket name and key, as well as a role ARN that you set up to allow Amazon GameLift to access your Amazon S3 bucket. The S3 bucket must be in the same region that you want to create a new build in.
cbStorageLocation :: Lens' CreateBuild (Maybe S3Location)
cbStorageLocation = lens _cbStorageLocation (\ s a -> s{_cbStorageLocation = a})

-- | Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build. If your game build contains multiple executables, they all must run on the same operating system. If an operating system is not specified when creating a build, Amazon GameLift uses the default value (WINDOWS_2012). This value cannot be changed later.
cbOperatingSystem :: Lens' CreateBuild (Maybe OperatingSystem)
cbOperatingSystem = lens _cbOperatingSystem (\ s a -> s{_cbOperatingSystem = a})

-- | Descriptive label that is associated with a build. Build names do not need to be unique. You can use 'UpdateBuild' to change this value later.
cbName :: Lens' CreateBuild (Maybe Text)
cbName = lens _cbName (\ s a -> s{_cbName = a})

-- | Version that is associated with this build. Version strings do not need to be unique. You can use 'UpdateBuild' to change this value later.
cbVersion :: Lens' CreateBuild (Maybe Text)
cbVersion = lens _cbVersion (\ s a -> s{_cbVersion = a})

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
-- * 'cbrsStorageLocation' - Amazon S3 location for your game build file, including bucket name and key.
--
-- * 'cbrsUploadCredentials' - This element is returned only when the operation is called without a storage location. It contains credentials to use when you are uploading a build file to an Amazon S3 bucket that is owned by Amazon GameLift. Credentials have a limited life span. To refresh these credentials, call 'RequestUploadCredentials' .
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


-- | Amazon S3 location for your game build file, including bucket name and key.
cbrsStorageLocation :: Lens' CreateBuildResponse (Maybe S3Location)
cbrsStorageLocation = lens _cbrsStorageLocation (\ s a -> s{_cbrsStorageLocation = a})

-- | This element is returned only when the operation is called without a storage location. It contains credentials to use when you are uploading a build file to an Amazon S3 bucket that is owned by Amazon GameLift. Credentials have a limited life span. To refresh these credentials, call 'RequestUploadCredentials' .
cbrsUploadCredentials :: Lens' CreateBuildResponse (Maybe AWSCredentials)
cbrsUploadCredentials = lens _cbrsUploadCredentials (\ s a -> s{_cbrsUploadCredentials = a}) . mapping _Sensitive

-- | The newly created build record, including a unique build ID and status.
cbrsBuild :: Lens' CreateBuildResponse (Maybe Build)
cbrsBuild = lens _cbrsBuild (\ s a -> s{_cbrsBuild = a})

-- | -- | The response status code.
cbrsResponseStatus :: Lens' CreateBuildResponse Int
cbrsResponseStatus = lens _cbrsResponseStatus (\ s a -> s{_cbrsResponseStatus = a})

instance NFData CreateBuildResponse where
