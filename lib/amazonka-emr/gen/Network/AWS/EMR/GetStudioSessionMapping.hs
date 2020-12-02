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
-- Module      : Network.AWS.EMR.GetStudioSessionMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches mapping details for the specified Amazon EMR Studio and identity (user or group).
module Network.AWS.EMR.GetStudioSessionMapping
  ( -- * Creating a Request
    getStudioSessionMapping,
    GetStudioSessionMapping,

    -- * Request Lenses
    gssmIdentityId,
    gssmIdentityName,
    gssmStudioId,
    gssmIdentityType,

    -- * Destructuring the Response
    getStudioSessionMappingResponse,
    GetStudioSessionMappingResponse,

    -- * Response Lenses
    gssmrsSessionMapping,
    gssmrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getStudioSessionMapping' smart constructor.
data GetStudioSessionMapping = GetStudioSessionMapping'
  { _gssmIdentityId ::
      !(Maybe Text),
    _gssmIdentityName :: !(Maybe Text),
    _gssmStudioId :: !Text,
    _gssmIdentityType :: !IdentityType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetStudioSessionMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssmIdentityId' - The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- * 'gssmIdentityName' - The name of the user or group to fetch. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- * 'gssmStudioId' - The ID of the Amazon EMR Studio.
--
-- * 'gssmIdentityType' - Specifies whether the identity to fetch is a user or a group.
getStudioSessionMapping ::
  -- | 'gssmStudioId'
  Text ->
  -- | 'gssmIdentityType'
  IdentityType ->
  GetStudioSessionMapping
getStudioSessionMapping pStudioId_ pIdentityType_ =
  GetStudioSessionMapping'
    { _gssmIdentityId = Nothing,
      _gssmIdentityName = Nothing,
      _gssmStudioId = pStudioId_,
      _gssmIdentityType = pIdentityType_
    }

-- | The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
gssmIdentityId :: Lens' GetStudioSessionMapping (Maybe Text)
gssmIdentityId = lens _gssmIdentityId (\s a -> s {_gssmIdentityId = a})

-- | The name of the user or group to fetch. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
gssmIdentityName :: Lens' GetStudioSessionMapping (Maybe Text)
gssmIdentityName = lens _gssmIdentityName (\s a -> s {_gssmIdentityName = a})

-- | The ID of the Amazon EMR Studio.
gssmStudioId :: Lens' GetStudioSessionMapping Text
gssmStudioId = lens _gssmStudioId (\s a -> s {_gssmStudioId = a})

-- | Specifies whether the identity to fetch is a user or a group.
gssmIdentityType :: Lens' GetStudioSessionMapping IdentityType
gssmIdentityType = lens _gssmIdentityType (\s a -> s {_gssmIdentityType = a})

instance AWSRequest GetStudioSessionMapping where
  type Rs GetStudioSessionMapping = GetStudioSessionMappingResponse
  request = postJSON emr
  response =
    receiveJSON
      ( \s h x ->
          GetStudioSessionMappingResponse'
            <$> (x .?> "SessionMapping") <*> (pure (fromEnum s))
      )

instance Hashable GetStudioSessionMapping

instance NFData GetStudioSessionMapping

instance ToHeaders GetStudioSessionMapping where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.GetStudioSessionMapping" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetStudioSessionMapping where
  toJSON GetStudioSessionMapping' {..} =
    object
      ( catMaybes
          [ ("IdentityId" .=) <$> _gssmIdentityId,
            ("IdentityName" .=) <$> _gssmIdentityName,
            Just ("StudioId" .= _gssmStudioId),
            Just ("IdentityType" .= _gssmIdentityType)
          ]
      )

instance ToPath GetStudioSessionMapping where
  toPath = const "/"

instance ToQuery GetStudioSessionMapping where
  toQuery = const mempty

-- | /See:/ 'getStudioSessionMappingResponse' smart constructor.
data GetStudioSessionMappingResponse = GetStudioSessionMappingResponse'
  { _gssmrsSessionMapping ::
      !( Maybe
           SessionMappingDetail
       ),
    _gssmrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetStudioSessionMappingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssmrsSessionMapping' - The session mapping details for the specified Amazon EMR Studio and identity, including session policy ARN and creation time.
--
-- * 'gssmrsResponseStatus' - -- | The response status code.
getStudioSessionMappingResponse ::
  -- | 'gssmrsResponseStatus'
  Int ->
  GetStudioSessionMappingResponse
getStudioSessionMappingResponse pResponseStatus_ =
  GetStudioSessionMappingResponse'
    { _gssmrsSessionMapping = Nothing,
      _gssmrsResponseStatus = pResponseStatus_
    }

-- | The session mapping details for the specified Amazon EMR Studio and identity, including session policy ARN and creation time.
gssmrsSessionMapping :: Lens' GetStudioSessionMappingResponse (Maybe SessionMappingDetail)
gssmrsSessionMapping = lens _gssmrsSessionMapping (\s a -> s {_gssmrsSessionMapping = a})

-- | -- | The response status code.
gssmrsResponseStatus :: Lens' GetStudioSessionMappingResponse Int
gssmrsResponseStatus = lens _gssmrsResponseStatus (\s a -> s {_gssmrsResponseStatus = a})

instance NFData GetStudioSessionMappingResponse
