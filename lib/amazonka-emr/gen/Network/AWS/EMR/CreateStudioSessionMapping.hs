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
-- Module      : Network.AWS.EMR.CreateStudioSessionMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Maps a user or group to the Amazon EMR Studio specified by @StudioId@ , and applies a session policy to refine Studio permissions for that user or group.
module Network.AWS.EMR.CreateStudioSessionMapping
  ( -- * Creating a Request
    createStudioSessionMapping,
    CreateStudioSessionMapping,

    -- * Request Lenses
    cssmIdentityId,
    cssmIdentityName,
    cssmStudioId,
    cssmIdentityType,
    cssmSessionPolicyARN,

    -- * Destructuring the Response
    createStudioSessionMappingResponse,
    CreateStudioSessionMappingResponse,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStudioSessionMapping' smart constructor.
data CreateStudioSessionMapping = CreateStudioSessionMapping'
  { _cssmIdentityId ::
      !(Maybe Text),
    _cssmIdentityName :: !(Maybe Text),
    _cssmStudioId :: !Text,
    _cssmIdentityType :: !IdentityType,
    _cssmSessionPolicyARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateStudioSessionMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssmIdentityId' - The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- * 'cssmIdentityName' - The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- * 'cssmStudioId' - The ID of the Amazon EMR Studio to which the user or group will be mapped.
--
-- * 'cssmIdentityType' - Specifies whether the identity to map to the Studio is a user or a group.
--
-- * 'cssmSessionPolicyARN' - The Amazon Resource Name (ARN) for the session policy that will be applied to the user or group. Session policies refine Studio user permissions without the need to use multiple IAM user roles.
createStudioSessionMapping ::
  -- | 'cssmStudioId'
  Text ->
  -- | 'cssmIdentityType'
  IdentityType ->
  -- | 'cssmSessionPolicyARN'
  Text ->
  CreateStudioSessionMapping
createStudioSessionMapping
  pStudioId_
  pIdentityType_
  pSessionPolicyARN_ =
    CreateStudioSessionMapping'
      { _cssmIdentityId = Nothing,
        _cssmIdentityName = Nothing,
        _cssmStudioId = pStudioId_,
        _cssmIdentityType = pIdentityType_,
        _cssmSessionPolicyARN = pSessionPolicyARN_
      }

-- | The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
cssmIdentityId :: Lens' CreateStudioSessionMapping (Maybe Text)
cssmIdentityId = lens _cssmIdentityId (\s a -> s {_cssmIdentityId = a})

-- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
cssmIdentityName :: Lens' CreateStudioSessionMapping (Maybe Text)
cssmIdentityName = lens _cssmIdentityName (\s a -> s {_cssmIdentityName = a})

-- | The ID of the Amazon EMR Studio to which the user or group will be mapped.
cssmStudioId :: Lens' CreateStudioSessionMapping Text
cssmStudioId = lens _cssmStudioId (\s a -> s {_cssmStudioId = a})

-- | Specifies whether the identity to map to the Studio is a user or a group.
cssmIdentityType :: Lens' CreateStudioSessionMapping IdentityType
cssmIdentityType = lens _cssmIdentityType (\s a -> s {_cssmIdentityType = a})

-- | The Amazon Resource Name (ARN) for the session policy that will be applied to the user or group. Session policies refine Studio user permissions without the need to use multiple IAM user roles.
cssmSessionPolicyARN :: Lens' CreateStudioSessionMapping Text
cssmSessionPolicyARN = lens _cssmSessionPolicyARN (\s a -> s {_cssmSessionPolicyARN = a})

instance AWSRequest CreateStudioSessionMapping where
  type
    Rs CreateStudioSessionMapping =
      CreateStudioSessionMappingResponse
  request = postJSON emr
  response = receiveNull CreateStudioSessionMappingResponse'

instance Hashable CreateStudioSessionMapping

instance NFData CreateStudioSessionMapping

instance ToHeaders CreateStudioSessionMapping where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.CreateStudioSessionMapping" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateStudioSessionMapping where
  toJSON CreateStudioSessionMapping' {..} =
    object
      ( catMaybes
          [ ("IdentityId" .=) <$> _cssmIdentityId,
            ("IdentityName" .=) <$> _cssmIdentityName,
            Just ("StudioId" .= _cssmStudioId),
            Just ("IdentityType" .= _cssmIdentityType),
            Just ("SessionPolicyArn" .= _cssmSessionPolicyARN)
          ]
      )

instance ToPath CreateStudioSessionMapping where
  toPath = const "/"

instance ToQuery CreateStudioSessionMapping where
  toQuery = const mempty

-- | /See:/ 'createStudioSessionMappingResponse' smart constructor.
data CreateStudioSessionMappingResponse = CreateStudioSessionMappingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateStudioSessionMappingResponse' with the minimum fields required to make a request.
createStudioSessionMappingResponse ::
  CreateStudioSessionMappingResponse
createStudioSessionMappingResponse =
  CreateStudioSessionMappingResponse'

instance NFData CreateStudioSessionMappingResponse
