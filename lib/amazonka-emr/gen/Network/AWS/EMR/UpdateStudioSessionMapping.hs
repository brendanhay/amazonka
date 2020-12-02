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
-- Module      : Network.AWS.EMR.UpdateStudioSessionMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the session policy attached to the user or group for the specified Amazon EMR Studio.
module Network.AWS.EMR.UpdateStudioSessionMapping
  ( -- * Creating a Request
    updateStudioSessionMapping,
    UpdateStudioSessionMapping,

    -- * Request Lenses
    ussmIdentityId,
    ussmIdentityName,
    ussmStudioId,
    ussmIdentityType,
    ussmSessionPolicyARN,

    -- * Destructuring the Response
    updateStudioSessionMappingResponse,
    UpdateStudioSessionMappingResponse,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateStudioSessionMapping' smart constructor.
data UpdateStudioSessionMapping = UpdateStudioSessionMapping'
  { _ussmIdentityId ::
      !(Maybe Text),
    _ussmIdentityName :: !(Maybe Text),
    _ussmStudioId :: !Text,
    _ussmIdentityType :: !IdentityType,
    _ussmSessionPolicyARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateStudioSessionMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ussmIdentityId' - The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- * 'ussmIdentityName' - The name of the user or group to update. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- * 'ussmStudioId' - The ID of the EMR Studio.
--
-- * 'ussmIdentityType' - Specifies whether the identity to update is a user or a group.
--
-- * 'ussmSessionPolicyARN' - The Amazon Resource Name (ARN) of the session policy to associate with the specified user or group.
updateStudioSessionMapping ::
  -- | 'ussmStudioId'
  Text ->
  -- | 'ussmIdentityType'
  IdentityType ->
  -- | 'ussmSessionPolicyARN'
  Text ->
  UpdateStudioSessionMapping
updateStudioSessionMapping
  pStudioId_
  pIdentityType_
  pSessionPolicyARN_ =
    UpdateStudioSessionMapping'
      { _ussmIdentityId = Nothing,
        _ussmIdentityName = Nothing,
        _ussmStudioId = pStudioId_,
        _ussmIdentityType = pIdentityType_,
        _ussmSessionPolicyARN = pSessionPolicyARN_
      }

-- | The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
ussmIdentityId :: Lens' UpdateStudioSessionMapping (Maybe Text)
ussmIdentityId = lens _ussmIdentityId (\s a -> s {_ussmIdentityId = a})

-- | The name of the user or group to update. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
ussmIdentityName :: Lens' UpdateStudioSessionMapping (Maybe Text)
ussmIdentityName = lens _ussmIdentityName (\s a -> s {_ussmIdentityName = a})

-- | The ID of the EMR Studio.
ussmStudioId :: Lens' UpdateStudioSessionMapping Text
ussmStudioId = lens _ussmStudioId (\s a -> s {_ussmStudioId = a})

-- | Specifies whether the identity to update is a user or a group.
ussmIdentityType :: Lens' UpdateStudioSessionMapping IdentityType
ussmIdentityType = lens _ussmIdentityType (\s a -> s {_ussmIdentityType = a})

-- | The Amazon Resource Name (ARN) of the session policy to associate with the specified user or group.
ussmSessionPolicyARN :: Lens' UpdateStudioSessionMapping Text
ussmSessionPolicyARN = lens _ussmSessionPolicyARN (\s a -> s {_ussmSessionPolicyARN = a})

instance AWSRequest UpdateStudioSessionMapping where
  type
    Rs UpdateStudioSessionMapping =
      UpdateStudioSessionMappingResponse
  request = postJSON emr
  response = receiveNull UpdateStudioSessionMappingResponse'

instance Hashable UpdateStudioSessionMapping

instance NFData UpdateStudioSessionMapping

instance ToHeaders UpdateStudioSessionMapping where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.UpdateStudioSessionMapping" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateStudioSessionMapping where
  toJSON UpdateStudioSessionMapping' {..} =
    object
      ( catMaybes
          [ ("IdentityId" .=) <$> _ussmIdentityId,
            ("IdentityName" .=) <$> _ussmIdentityName,
            Just ("StudioId" .= _ussmStudioId),
            Just ("IdentityType" .= _ussmIdentityType),
            Just ("SessionPolicyArn" .= _ussmSessionPolicyARN)
          ]
      )

instance ToPath UpdateStudioSessionMapping where
  toPath = const "/"

instance ToQuery UpdateStudioSessionMapping where
  toQuery = const mempty

-- | /See:/ 'updateStudioSessionMappingResponse' smart constructor.
data UpdateStudioSessionMappingResponse = UpdateStudioSessionMappingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateStudioSessionMappingResponse' with the minimum fields required to make a request.
updateStudioSessionMappingResponse ::
  UpdateStudioSessionMappingResponse
updateStudioSessionMappingResponse =
  UpdateStudioSessionMappingResponse'

instance NFData UpdateStudioSessionMappingResponse
