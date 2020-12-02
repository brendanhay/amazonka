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
-- Module      : Network.AWS.EMR.DeleteStudioSessionMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a user or group from an Amazon EMR Studio.
module Network.AWS.EMR.DeleteStudioSessionMapping
  ( -- * Creating a Request
    deleteStudioSessionMapping,
    DeleteStudioSessionMapping,

    -- * Request Lenses
    dssmIdentityId,
    dssmIdentityName,
    dssmStudioId,
    dssmIdentityType,

    -- * Destructuring the Response
    deleteStudioSessionMappingResponse,
    DeleteStudioSessionMappingResponse,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteStudioSessionMapping' smart constructor.
data DeleteStudioSessionMapping = DeleteStudioSessionMapping'
  { _dssmIdentityId ::
      !(Maybe Text),
    _dssmIdentityName :: !(Maybe Text),
    _dssmStudioId :: !Text,
    _dssmIdentityType :: !IdentityType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteStudioSessionMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssmIdentityId' - The globally unique identifier (GUID) of the user or group to remove from the Amazon EMR Studio. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- * 'dssmIdentityName' - The name of the user name or group to remove from the Studio. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- * 'dssmStudioId' - The ID of the Amazon EMR Studio.
--
-- * 'dssmIdentityType' - Specifies whether the identity to delete from the Studio is a user or a group.
deleteStudioSessionMapping ::
  -- | 'dssmStudioId'
  Text ->
  -- | 'dssmIdentityType'
  IdentityType ->
  DeleteStudioSessionMapping
deleteStudioSessionMapping pStudioId_ pIdentityType_ =
  DeleteStudioSessionMapping'
    { _dssmIdentityId = Nothing,
      _dssmIdentityName = Nothing,
      _dssmStudioId = pStudioId_,
      _dssmIdentityType = pIdentityType_
    }

-- | The globally unique identifier (GUID) of the user or group to remove from the Amazon EMR Studio. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
dssmIdentityId :: Lens' DeleteStudioSessionMapping (Maybe Text)
dssmIdentityId = lens _dssmIdentityId (\s a -> s {_dssmIdentityId = a})

-- | The name of the user name or group to remove from the Studio. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
dssmIdentityName :: Lens' DeleteStudioSessionMapping (Maybe Text)
dssmIdentityName = lens _dssmIdentityName (\s a -> s {_dssmIdentityName = a})

-- | The ID of the Amazon EMR Studio.
dssmStudioId :: Lens' DeleteStudioSessionMapping Text
dssmStudioId = lens _dssmStudioId (\s a -> s {_dssmStudioId = a})

-- | Specifies whether the identity to delete from the Studio is a user or a group.
dssmIdentityType :: Lens' DeleteStudioSessionMapping IdentityType
dssmIdentityType = lens _dssmIdentityType (\s a -> s {_dssmIdentityType = a})

instance AWSRequest DeleteStudioSessionMapping where
  type
    Rs DeleteStudioSessionMapping =
      DeleteStudioSessionMappingResponse
  request = postJSON emr
  response = receiveNull DeleteStudioSessionMappingResponse'

instance Hashable DeleteStudioSessionMapping

instance NFData DeleteStudioSessionMapping

instance ToHeaders DeleteStudioSessionMapping where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.DeleteStudioSessionMapping" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteStudioSessionMapping where
  toJSON DeleteStudioSessionMapping' {..} =
    object
      ( catMaybes
          [ ("IdentityId" .=) <$> _dssmIdentityId,
            ("IdentityName" .=) <$> _dssmIdentityName,
            Just ("StudioId" .= _dssmStudioId),
            Just ("IdentityType" .= _dssmIdentityType)
          ]
      )

instance ToPath DeleteStudioSessionMapping where
  toPath = const "/"

instance ToQuery DeleteStudioSessionMapping where
  toQuery = const mempty

-- | /See:/ 'deleteStudioSessionMappingResponse' smart constructor.
data DeleteStudioSessionMappingResponse = DeleteStudioSessionMappingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteStudioSessionMappingResponse' with the minimum fields required to make a request.
deleteStudioSessionMappingResponse ::
  DeleteStudioSessionMappingResponse
deleteStudioSessionMappingResponse =
  DeleteStudioSessionMappingResponse'

instance NFData DeleteStudioSessionMappingResponse
