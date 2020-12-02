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
-- Module      : Network.AWS.GameLift.CreateBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon GameLift build resource for your game server binary files. Game server binaries must be combined into a zip file for use with Amazon GameLift.
--
--
-- /Important:/ When setting up a new game build for GameLift, we recommend using the AWS CLI command __<https://docs.aws.amazon.com/cli/latest/reference/gamelift/upload-build.html upload-build> __ . This helper command combines two tasks: (1) it uploads your build files from a file directory to a GameLift Amazon S3 location, and (2) it creates a new build resource.
--
-- The @CreateBuild@ operation can used in the following scenarios:
--
--     * To create a new game build with build files that are in an S3 location under an AWS account that you control. To use this option, you must first give Amazon GameLift access to the S3 bucket. With permissions in place, call @CreateBuild@ and specify a build name, operating system, and the S3 storage location of your game build.
--
--     * To directly upload your build files to a GameLift S3 location. To use this option, first call @CreateBuild@ and specify a build name and operating system. This operation creates a new build resource and also returns an S3 location with temporary access credentials. Use the credentials to manually upload your build files to the specified S3 location. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UploadingObjects.html Uploading Objects> in the /Amazon S3 Developer Guide/ . Build files can be uploaded to the GameLift S3 location once only; that can't be updated.
--
--
--
-- If successful, this operation creates a new build resource with a unique build ID and places it in @INITIALIZED@ status. A build must be in @READY@ status before you can create fleets with it.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Uploading Your Game>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build Create a Build with Files in Amazon S3>
--
-- __Related operations__
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
module Network.AWS.GameLift.CreateBuild
  ( -- * Creating a Request
    createBuild,
    CreateBuild,

    -- * Request Lenses
    cbStorageLocation,
    cbOperatingSystem,
    cbName,
    cbVersion,
    cbTags,

    -- * Destructuring the Response
    createBuildResponse,
    CreateBuildResponse,

    -- * Response Lenses
    cbrsStorageLocation,
    cbrsUploadCredentials,
    cbrsBuild,
    cbrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'createBuild' smart constructor.
data CreateBuild = CreateBuild'
  { _cbStorageLocation ::
      !(Maybe S3Location),
    _cbOperatingSystem :: !(Maybe OperatingSystem),
    _cbName :: !(Maybe Text),
    _cbVersion :: !(Maybe Text),
    _cbTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateBuild' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbStorageLocation' - The location where your game build files are stored. Use this parameter only when creating a build using files that are stored in an S3 bucket that you own. Identify an S3 bucket name and key, which must in the same Region where you're creating a build. This parameter must also specify the ARN for an IAM role that you've set up to give Amazon GameLift access your S3 bucket. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
--
-- * 'cbOperatingSystem' - The operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build. If your game build contains multiple executables, they all must run on the same operating system. If an operating system is not specified when creating a build, Amazon GameLift uses the default value (WINDOWS_2012). This value cannot be changed later.
--
-- * 'cbName' - A descriptive label that is associated with a build. Build names do not need to be unique. You can use 'UpdateBuild' to change this value later.
--
-- * 'cbVersion' - Version information that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateBuild' to change this value later.
--
-- * 'cbTags' - A list of labels to assign to the new build resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
createBuild ::
  CreateBuild
createBuild =
  CreateBuild'
    { _cbStorageLocation = Nothing,
      _cbOperatingSystem = Nothing,
      _cbName = Nothing,
      _cbVersion = Nothing,
      _cbTags = Nothing
    }

-- | The location where your game build files are stored. Use this parameter only when creating a build using files that are stored in an S3 bucket that you own. Identify an S3 bucket name and key, which must in the same Region where you're creating a build. This parameter must also specify the ARN for an IAM role that you've set up to give Amazon GameLift access your S3 bucket. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
cbStorageLocation :: Lens' CreateBuild (Maybe S3Location)
cbStorageLocation = lens _cbStorageLocation (\s a -> s {_cbStorageLocation = a})

-- | The operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build. If your game build contains multiple executables, they all must run on the same operating system. If an operating system is not specified when creating a build, Amazon GameLift uses the default value (WINDOWS_2012). This value cannot be changed later.
cbOperatingSystem :: Lens' CreateBuild (Maybe OperatingSystem)
cbOperatingSystem = lens _cbOperatingSystem (\s a -> s {_cbOperatingSystem = a})

-- | A descriptive label that is associated with a build. Build names do not need to be unique. You can use 'UpdateBuild' to change this value later.
cbName :: Lens' CreateBuild (Maybe Text)
cbName = lens _cbName (\s a -> s {_cbName = a})

-- | Version information that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateBuild' to change this value later.
cbVersion :: Lens' CreateBuild (Maybe Text)
cbVersion = lens _cbVersion (\s a -> s {_cbVersion = a})

-- | A list of labels to assign to the new build resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
cbTags :: Lens' CreateBuild [Tag]
cbTags = lens _cbTags (\s a -> s {_cbTags = a}) . _Default . _Coerce

instance AWSRequest CreateBuild where
  type Rs CreateBuild = CreateBuildResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          CreateBuildResponse'
            <$> (x .?> "StorageLocation")
            <*> (x .?> "UploadCredentials")
            <*> (x .?> "Build")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateBuild

instance NFData CreateBuild

instance ToHeaders CreateBuild where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.CreateBuild" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateBuild where
  toJSON CreateBuild' {..} =
    object
      ( catMaybes
          [ ("StorageLocation" .=) <$> _cbStorageLocation,
            ("OperatingSystem" .=) <$> _cbOperatingSystem,
            ("Name" .=) <$> _cbName,
            ("Version" .=) <$> _cbVersion,
            ("Tags" .=) <$> _cbTags
          ]
      )

instance ToPath CreateBuild where
  toPath = const "/"

instance ToQuery CreateBuild where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'createBuildResponse' smart constructor.
data CreateBuildResponse = CreateBuildResponse'
  { _cbrsStorageLocation ::
      !(Maybe S3Location),
    _cbrsUploadCredentials ::
      !(Maybe (Sensitive AWSCredentials)),
    _cbrsBuild :: !(Maybe Build),
    _cbrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateBuildResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsStorageLocation' - Amazon S3 location for your game build file, including bucket name and key.
--
-- * 'cbrsUploadCredentials' - This element is returned only when the operation is called without a storage location. It contains credentials to use when you are uploading a build file to an S3 bucket that is owned by Amazon GameLift. Credentials have a limited life span. To refresh these credentials, call 'RequestUploadCredentials' .
--
-- * 'cbrsBuild' - The newly created build resource, including a unique build IDs and status.
--
-- * 'cbrsResponseStatus' - -- | The response status code.
createBuildResponse ::
  -- | 'cbrsResponseStatus'
  Int ->
  CreateBuildResponse
createBuildResponse pResponseStatus_ =
  CreateBuildResponse'
    { _cbrsStorageLocation = Nothing,
      _cbrsUploadCredentials = Nothing,
      _cbrsBuild = Nothing,
      _cbrsResponseStatus = pResponseStatus_
    }

-- | Amazon S3 location for your game build file, including bucket name and key.
cbrsStorageLocation :: Lens' CreateBuildResponse (Maybe S3Location)
cbrsStorageLocation = lens _cbrsStorageLocation (\s a -> s {_cbrsStorageLocation = a})

-- | This element is returned only when the operation is called without a storage location. It contains credentials to use when you are uploading a build file to an S3 bucket that is owned by Amazon GameLift. Credentials have a limited life span. To refresh these credentials, call 'RequestUploadCredentials' .
cbrsUploadCredentials :: Lens' CreateBuildResponse (Maybe AWSCredentials)
cbrsUploadCredentials = lens _cbrsUploadCredentials (\s a -> s {_cbrsUploadCredentials = a}) . mapping _Sensitive

-- | The newly created build resource, including a unique build IDs and status.
cbrsBuild :: Lens' CreateBuildResponse (Maybe Build)
cbrsBuild = lens _cbrsBuild (\s a -> s {_cbrsBuild = a})

-- | -- | The response status code.
cbrsResponseStatus :: Lens' CreateBuildResponse Int
cbrsResponseStatus = lens _cbrsResponseStatus (\s a -> s {_cbrsResponseStatus = a})

instance NFData CreateBuildResponse
