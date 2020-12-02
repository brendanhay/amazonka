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
-- Module      : Network.AWS.AlexaBusiness.DeleteGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a gateway group.
module Network.AWS.AlexaBusiness.DeleteGatewayGroup
  ( -- * Creating a Request
    deleteGatewayGroup,
    DeleteGatewayGroup,

    -- * Request Lenses
    dggGatewayGroupARN,

    -- * Destructuring the Response
    deleteGatewayGroupResponse,
    DeleteGatewayGroupResponse,

    -- * Response Lenses
    dggrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteGatewayGroup' smart constructor.
newtype DeleteGatewayGroup = DeleteGatewayGroup'
  { _dggGatewayGroupARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteGatewayGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dggGatewayGroupARN' - The ARN of the gateway group to delete.
deleteGatewayGroup ::
  -- | 'dggGatewayGroupARN'
  Text ->
  DeleteGatewayGroup
deleteGatewayGroup pGatewayGroupARN_ =
  DeleteGatewayGroup' {_dggGatewayGroupARN = pGatewayGroupARN_}

-- | The ARN of the gateway group to delete.
dggGatewayGroupARN :: Lens' DeleteGatewayGroup Text
dggGatewayGroupARN = lens _dggGatewayGroupARN (\s a -> s {_dggGatewayGroupARN = a})

instance AWSRequest DeleteGatewayGroup where
  type Rs DeleteGatewayGroup = DeleteGatewayGroupResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      (\s h x -> DeleteGatewayGroupResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteGatewayGroup

instance NFData DeleteGatewayGroup

instance ToHeaders DeleteGatewayGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.DeleteGatewayGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteGatewayGroup where
  toJSON DeleteGatewayGroup' {..} =
    object
      (catMaybes [Just ("GatewayGroupArn" .= _dggGatewayGroupARN)])

instance ToPath DeleteGatewayGroup where
  toPath = const "/"

instance ToQuery DeleteGatewayGroup where
  toQuery = const mempty

-- | /See:/ 'deleteGatewayGroupResponse' smart constructor.
newtype DeleteGatewayGroupResponse = DeleteGatewayGroupResponse'
  { _dggrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteGatewayGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dggrsResponseStatus' - -- | The response status code.
deleteGatewayGroupResponse ::
  -- | 'dggrsResponseStatus'
  Int ->
  DeleteGatewayGroupResponse
deleteGatewayGroupResponse pResponseStatus_ =
  DeleteGatewayGroupResponse'
    { _dggrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dggrsResponseStatus :: Lens' DeleteGatewayGroupResponse Int
dggrsResponseStatus = lens _dggrsResponseStatus (\s a -> s {_dggrsResponseStatus = a})

instance NFData DeleteGatewayGroupResponse
