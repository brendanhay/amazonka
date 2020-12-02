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
-- Module      : Network.AWS.APIGatewayManagementAPI.DeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the connection with the provided id.
module Network.AWS.APIGatewayManagementAPI.DeleteConnection
  ( -- * Creating a Request
    deleteConnection,
    DeleteConnection,

    -- * Request Lenses
    dcConnectionId,

    -- * Destructuring the Response
    deleteConnectionResponse,
    DeleteConnectionResponse,
  )
where

import Network.AWS.APIGatewayManagementAPI.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteConnection' smart constructor.
newtype DeleteConnection = DeleteConnection'
  { _dcConnectionId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcConnectionId' - Undocumented member.
deleteConnection ::
  -- | 'dcConnectionId'
  Text ->
  DeleteConnection
deleteConnection pConnectionId_ =
  DeleteConnection' {_dcConnectionId = pConnectionId_}

-- | Undocumented member.
dcConnectionId :: Lens' DeleteConnection Text
dcConnectionId = lens _dcConnectionId (\s a -> s {_dcConnectionId = a})

instance AWSRequest DeleteConnection where
  type Rs DeleteConnection = DeleteConnectionResponse
  request = delete apiGatewayManagementAPI
  response = receiveNull DeleteConnectionResponse'

instance Hashable DeleteConnection

instance NFData DeleteConnection

instance ToHeaders DeleteConnection where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteConnection where
  toPath DeleteConnection' {..} =
    mconcat ["/@connections/", toBS _dcConnectionId]

instance ToQuery DeleteConnection where
  toQuery = const mempty

-- | /See:/ 'deleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteConnectionResponse' with the minimum fields required to make a request.
deleteConnectionResponse ::
  DeleteConnectionResponse
deleteConnectionResponse = DeleteConnectionResponse'

instance NFData DeleteConnectionResponse
