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
-- Module      : Network.AWS.Pinpoint.DeleteUserEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all the endpoints that are associated with a specific user ID.
module Network.AWS.Pinpoint.DeleteUserEndpoints
  ( -- * Creating a Request
    deleteUserEndpoints,
    DeleteUserEndpoints,

    -- * Request Lenses
    dueApplicationId,
    dueUserId,

    -- * Destructuring the Response
    deleteUserEndpointsResponse,
    DeleteUserEndpointsResponse,

    -- * Response Lenses
    duersResponseStatus,
    duersEndpointsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUserEndpoints' smart constructor.
data DeleteUserEndpoints = DeleteUserEndpoints'
  { _dueApplicationId ::
      !Text,
    _dueUserId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dueApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'dueUserId' - The unique identifier for the user.
deleteUserEndpoints ::
  -- | 'dueApplicationId'
  Text ->
  -- | 'dueUserId'
  Text ->
  DeleteUserEndpoints
deleteUserEndpoints pApplicationId_ pUserId_ =
  DeleteUserEndpoints'
    { _dueApplicationId = pApplicationId_,
      _dueUserId = pUserId_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
dueApplicationId :: Lens' DeleteUserEndpoints Text
dueApplicationId = lens _dueApplicationId (\s a -> s {_dueApplicationId = a})

-- | The unique identifier for the user.
dueUserId :: Lens' DeleteUserEndpoints Text
dueUserId = lens _dueUserId (\s a -> s {_dueUserId = a})

instance AWSRequest DeleteUserEndpoints where
  type Rs DeleteUserEndpoints = DeleteUserEndpointsResponse
  request = delete pinpoint
  response =
    receiveJSON
      ( \s h x ->
          DeleteUserEndpointsResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable DeleteUserEndpoints

instance NFData DeleteUserEndpoints

instance ToHeaders DeleteUserEndpoints where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteUserEndpoints where
  toPath DeleteUserEndpoints' {..} =
    mconcat
      ["/v1/apps/", toBS _dueApplicationId, "/users/", toBS _dueUserId]

instance ToQuery DeleteUserEndpoints where
  toQuery = const mempty

-- | /See:/ 'deleteUserEndpointsResponse' smart constructor.
data DeleteUserEndpointsResponse = DeleteUserEndpointsResponse'
  { _duersResponseStatus ::
      !Int,
    _duersEndpointsResponse ::
      !EndpointsResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duersResponseStatus' - -- | The response status code.
--
-- * 'duersEndpointsResponse' - Undocumented member.
deleteUserEndpointsResponse ::
  -- | 'duersResponseStatus'
  Int ->
  -- | 'duersEndpointsResponse'
  EndpointsResponse ->
  DeleteUserEndpointsResponse
deleteUserEndpointsResponse pResponseStatus_ pEndpointsResponse_ =
  DeleteUserEndpointsResponse'
    { _duersResponseStatus =
        pResponseStatus_,
      _duersEndpointsResponse = pEndpointsResponse_
    }

-- | -- | The response status code.
duersResponseStatus :: Lens' DeleteUserEndpointsResponse Int
duersResponseStatus = lens _duersResponseStatus (\s a -> s {_duersResponseStatus = a})

-- | Undocumented member.
duersEndpointsResponse :: Lens' DeleteUserEndpointsResponse EndpointsResponse
duersEndpointsResponse = lens _duersEndpointsResponse (\s a -> s {_duersEndpointsResponse = a})

instance NFData DeleteUserEndpointsResponse
