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
-- Module      : Network.AWS.DMS.DeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the connection between a replication instance and an endpoint.
module Network.AWS.DMS.DeleteConnection
  ( -- * Creating a Request
    deleteConnection,
    DeleteConnection,

    -- * Request Lenses
    dcEndpointARN,
    dcReplicationInstanceARN,

    -- * Destructuring the Response
    deleteConnectionResponse,
    DeleteConnectionResponse,

    -- * Response Lenses
    dcrsConnection,
    dcrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { _dcEndpointARN :: !Text,
    _dcReplicationInstanceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'dcReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
deleteConnection ::
  -- | 'dcEndpointARN'
  Text ->
  -- | 'dcReplicationInstanceARN'
  Text ->
  DeleteConnection
deleteConnection pEndpointARN_ pReplicationInstanceARN_ =
  DeleteConnection'
    { _dcEndpointARN = pEndpointARN_,
      _dcReplicationInstanceARN = pReplicationInstanceARN_
    }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
dcEndpointARN :: Lens' DeleteConnection Text
dcEndpointARN = lens _dcEndpointARN (\s a -> s {_dcEndpointARN = a})

-- | The Amazon Resource Name (ARN) of the replication instance.
dcReplicationInstanceARN :: Lens' DeleteConnection Text
dcReplicationInstanceARN = lens _dcReplicationInstanceARN (\s a -> s {_dcReplicationInstanceARN = a})

instance AWSRequest DeleteConnection where
  type Rs DeleteConnection = DeleteConnectionResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          DeleteConnectionResponse'
            <$> (x .?> "Connection") <*> (pure (fromEnum s))
      )

instance Hashable DeleteConnection

instance NFData DeleteConnection

instance ToHeaders DeleteConnection where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonDMSv20160101.DeleteConnection" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    object
      ( catMaybes
          [ Just ("EndpointArn" .= _dcEndpointARN),
            Just ("ReplicationInstanceArn" .= _dcReplicationInstanceARN)
          ]
      )

instance ToPath DeleteConnection where
  toPath = const "/"

instance ToQuery DeleteConnection where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'deleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { _dcrsConnection ::
      !(Maybe Connection),
    _dcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsConnection' - The connection that is being deleted.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
deleteConnectionResponse ::
  -- | 'dcrsResponseStatus'
  Int ->
  DeleteConnectionResponse
deleteConnectionResponse pResponseStatus_ =
  DeleteConnectionResponse'
    { _dcrsConnection = Nothing,
      _dcrsResponseStatus = pResponseStatus_
    }

-- | The connection that is being deleted.
dcrsConnection :: Lens' DeleteConnectionResponse (Maybe Connection)
dcrsConnection = lens _dcrsConnection (\s a -> s {_dcrsConnection = a})

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DeleteConnectionResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\s a -> s {_dcrsResponseStatus = a})

instance NFData DeleteConnectionResponse
