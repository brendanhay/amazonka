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
-- Module      : Network.AWS.APIGatewayManagementAPI.PostToConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends the provided data to the specified connection.
module Network.AWS.APIGatewayManagementAPI.PostToConnection
  ( -- * Creating a Request
    postToConnection,
    PostToConnection,

    -- * Request Lenses
    ptcConnectionId,
    ptcData,

    -- * Destructuring the Response
    postToConnectionResponse,
    PostToConnectionResponse,
  )
where

import Network.AWS.APIGatewayManagementAPI.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'postToConnection' smart constructor.
data PostToConnection = PostToConnection'
  { _ptcConnectionId ::
      !Text,
    _ptcData :: !ByteString
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PostToConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptcConnectionId' - The identifier of the connection that a specific client is using.
--
-- * 'ptcData' - The data to be sent to the client specified by its connection id.
postToConnection ::
  -- | 'ptcConnectionId'
  Text ->
  -- | 'ptcData'
  ByteString ->
  PostToConnection
postToConnection pConnectionId_ pData_ =
  PostToConnection'
    { _ptcConnectionId = pConnectionId_,
      _ptcData = pData_
    }

-- | The identifier of the connection that a specific client is using.
ptcConnectionId :: Lens' PostToConnection Text
ptcConnectionId = lens _ptcConnectionId (\s a -> s {_ptcConnectionId = a})

-- | The data to be sent to the client specified by its connection id.
ptcData :: Lens' PostToConnection ByteString
ptcData = lens _ptcData (\s a -> s {_ptcData = a})

instance AWSRequest PostToConnection where
  type Rs PostToConnection = PostToConnectionResponse
  request = postBody apiGatewayManagementAPI
  response = receiveNull PostToConnectionResponse'

instance Hashable PostToConnection

instance NFData PostToConnection

instance ToBody PostToConnection where
  toBody = toBody . _ptcData

instance ToHeaders PostToConnection where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath PostToConnection where
  toPath PostToConnection' {..} =
    mconcat ["/@connections/", toBS _ptcConnectionId]

instance ToQuery PostToConnection where
  toQuery = const mempty

-- | /See:/ 'postToConnectionResponse' smart constructor.
data PostToConnectionResponse = PostToConnectionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PostToConnectionResponse' with the minimum fields required to make a request.
postToConnectionResponse ::
  PostToConnectionResponse
postToConnectionResponse = PostToConnectionResponse'

instance NFData PostToConnectionResponse
