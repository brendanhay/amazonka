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
-- Module      : Network.AWS.APIGatewayManagementAPI.GetConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about the connection with the provided id.
module Network.AWS.APIGatewayManagementAPI.GetConnection
  ( -- * Creating a Request
    getConnection,
    GetConnection,

    -- * Request Lenses
    gcConnectionId,

    -- * Destructuring the Response
    getConnectionResponse,
    GetConnectionResponse,

    -- * Response Lenses
    gcrsConnectedAt,
    gcrsLastActiveAt,
    gcrsIdentity,
    gcrsResponseStatus,
  )
where

import Network.AWS.APIGatewayManagementAPI.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConnection' smart constructor.
newtype GetConnection = GetConnection' {_gcConnectionId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcConnectionId' - Undocumented member.
getConnection ::
  -- | 'gcConnectionId'
  Text ->
  GetConnection
getConnection pConnectionId_ =
  GetConnection' {_gcConnectionId = pConnectionId_}

-- | Undocumented member.
gcConnectionId :: Lens' GetConnection Text
gcConnectionId = lens _gcConnectionId (\s a -> s {_gcConnectionId = a})

instance AWSRequest GetConnection where
  type Rs GetConnection = GetConnectionResponse
  request = get apiGatewayManagementAPI
  response =
    receiveJSON
      ( \s h x ->
          GetConnectionResponse'
            <$> (x .?> "connectedAt")
            <*> (x .?> "lastActiveAt")
            <*> (x .?> "identity")
            <*> (pure (fromEnum s))
      )

instance Hashable GetConnection

instance NFData GetConnection

instance ToHeaders GetConnection where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetConnection where
  toPath GetConnection' {..} =
    mconcat ["/@connections/", toBS _gcConnectionId]

instance ToQuery GetConnection where
  toQuery = const mempty

-- | /See:/ 'getConnectionResponse' smart constructor.
data GetConnectionResponse = GetConnectionResponse'
  { _gcrsConnectedAt ::
      !(Maybe POSIX),
    _gcrsLastActiveAt :: !(Maybe POSIX),
    _gcrsIdentity :: !(Maybe Identity),
    _gcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsConnectedAt' - The time in ISO 8601 format for when the connection was established.
--
-- * 'gcrsLastActiveAt' - The time in ISO 8601 format for when the connection was last active.
--
-- * 'gcrsIdentity' - Undocumented member.
--
-- * 'gcrsResponseStatus' - -- | The response status code.
getConnectionResponse ::
  -- | 'gcrsResponseStatus'
  Int ->
  GetConnectionResponse
getConnectionResponse pResponseStatus_ =
  GetConnectionResponse'
    { _gcrsConnectedAt = Nothing,
      _gcrsLastActiveAt = Nothing,
      _gcrsIdentity = Nothing,
      _gcrsResponseStatus = pResponseStatus_
    }

-- | The time in ISO 8601 format for when the connection was established.
gcrsConnectedAt :: Lens' GetConnectionResponse (Maybe UTCTime)
gcrsConnectedAt = lens _gcrsConnectedAt (\s a -> s {_gcrsConnectedAt = a}) . mapping _Time

-- | The time in ISO 8601 format for when the connection was last active.
gcrsLastActiveAt :: Lens' GetConnectionResponse (Maybe UTCTime)
gcrsLastActiveAt = lens _gcrsLastActiveAt (\s a -> s {_gcrsLastActiveAt = a}) . mapping _Time

-- | Undocumented member.
gcrsIdentity :: Lens' GetConnectionResponse (Maybe Identity)
gcrsIdentity = lens _gcrsIdentity (\s a -> s {_gcrsIdentity = a})

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetConnectionResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\s a -> s {_gcrsResponseStatus = a})

instance NFData GetConnectionResponse
