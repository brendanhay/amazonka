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
-- Module      : Network.AWS.GameLift.CreateScript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new script record for your Realtime Servers script. Realtime scripts are JavaScript that provide configuration settings and optional custom game logic for your game. The script is deployed when you create a Realtime Servers fleet to host your game sessions. Script logic is executed during an active game session.
--
--
-- To create a new script record, specify a script name and provide the script file(s). The script files and all dependencies must be zipped into a single file. You can pull the zip file from either of these locations:
--
--     * A locally available directory. Use the /ZipFile/ parameter for this option.
--
--     * An Amazon Simple Storage Service (Amazon S3) bucket under your AWS account. Use the /StorageLocation/ parameter for this option. You'll need to have an Identity Access Management (IAM) role that allows the Amazon GameLift service to access your S3 bucket.
--
--
--
-- If the call is successful, a new script record is created with a unique script ID. If the script file is provided as a local file, the file is uploaded to an Amazon GameLift-owned S3 bucket and the script record's storage location reflects this location. If the script file is provided as an S3 bucket, Amazon GameLift accesses the file at this storage location as needed for deployment.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set Up a Role for Amazon GameLift Access>
--
-- __Related operations__
--
--     * 'CreateScript'
--
--     * 'ListScripts'
--
--     * 'DescribeScript'
--
--     * 'UpdateScript'
--
--     * 'DeleteScript'
module Network.AWS.GameLift.CreateScript
  ( -- * Creating a Request
    createScript,
    CreateScript,

    -- * Request Lenses
    csStorageLocation,
    csZipFile,
    csName,
    csVersion,
    csTags,

    -- * Destructuring the Response
    createScriptResponse,
    CreateScriptResponse,

    -- * Response Lenses
    csrsScript,
    csrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createScript' smart constructor.
data CreateScript = CreateScript'
  { _csStorageLocation ::
      !(Maybe S3Location),
    _csZipFile :: !(Maybe Base64),
    _csName :: !(Maybe Text),
    _csVersion :: !(Maybe Text),
    _csTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateScript' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csStorageLocation' - The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region where you are creating a new script. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
--
-- * 'csZipFile' - A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB. When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'csName' - A descriptive label that is associated with a script. Script names do not need to be unique. You can use 'UpdateScript' to change this value later.
--
-- * 'csVersion' - The version that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateScript' to change this value later.
--
-- * 'csTags' - A list of labels to assign to the new script resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
createScript ::
  CreateScript
createScript =
  CreateScript'
    { _csStorageLocation = Nothing,
      _csZipFile = Nothing,
      _csName = Nothing,
      _csVersion = Nothing,
      _csTags = Nothing
    }

-- | The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region where you are creating a new script. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
csStorageLocation :: Lens' CreateScript (Maybe S3Location)
csStorageLocation = lens _csStorageLocation (\s a -> s {_csStorageLocation = a})

-- | A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB. When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
csZipFile :: Lens' CreateScript (Maybe ByteString)
csZipFile = lens _csZipFile (\s a -> s {_csZipFile = a}) . mapping _Base64

-- | A descriptive label that is associated with a script. Script names do not need to be unique. You can use 'UpdateScript' to change this value later.
csName :: Lens' CreateScript (Maybe Text)
csName = lens _csName (\s a -> s {_csName = a})

-- | The version that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateScript' to change this value later.
csVersion :: Lens' CreateScript (Maybe Text)
csVersion = lens _csVersion (\s a -> s {_csVersion = a})

-- | A list of labels to assign to the new script resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
csTags :: Lens' CreateScript [Tag]
csTags = lens _csTags (\s a -> s {_csTags = a}) . _Default . _Coerce

instance AWSRequest CreateScript where
  type Rs CreateScript = CreateScriptResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          CreateScriptResponse' <$> (x .?> "Script") <*> (pure (fromEnum s))
      )

instance Hashable CreateScript

instance NFData CreateScript

instance ToHeaders CreateScript where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.CreateScript" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateScript where
  toJSON CreateScript' {..} =
    object
      ( catMaybes
          [ ("StorageLocation" .=) <$> _csStorageLocation,
            ("ZipFile" .=) <$> _csZipFile,
            ("Name" .=) <$> _csName,
            ("Version" .=) <$> _csVersion,
            ("Tags" .=) <$> _csTags
          ]
      )

instance ToPath CreateScript where
  toPath = const "/"

instance ToQuery CreateScript where
  toQuery = const mempty

-- | /See:/ 'createScriptResponse' smart constructor.
data CreateScriptResponse = CreateScriptResponse'
  { _csrsScript ::
      !(Maybe Script),
    _csrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateScriptResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsScript' - The newly created script record with a unique script ID and ARN. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
--
-- * 'csrsResponseStatus' - -- | The response status code.
createScriptResponse ::
  -- | 'csrsResponseStatus'
  Int ->
  CreateScriptResponse
createScriptResponse pResponseStatus_ =
  CreateScriptResponse'
    { _csrsScript = Nothing,
      _csrsResponseStatus = pResponseStatus_
    }

-- | The newly created script record with a unique script ID and ARN. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
csrsScript :: Lens' CreateScriptResponse (Maybe Script)
csrsScript = lens _csrsScript (\s a -> s {_csrsScript = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateScriptResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\s a -> s {_csrsResponseStatus = a})

instance NFData CreateScriptResponse
