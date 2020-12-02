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
-- Module      : Network.AWS.EC2.DeleteLocalGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified local gateway route table.
module Network.AWS.EC2.DeleteLocalGatewayRoute
  ( -- * Creating a Request
    deleteLocalGatewayRoute,
    DeleteLocalGatewayRoute,

    -- * Request Lenses
    dlgrDryRun,
    dlgrDestinationCidrBlock,
    dlgrLocalGatewayRouteTableId,

    -- * Destructuring the Response
    deleteLocalGatewayRouteResponse,
    DeleteLocalGatewayRouteResponse,

    -- * Response Lenses
    dlgrrsRoute,
    dlgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLocalGatewayRoute' smart constructor.
data DeleteLocalGatewayRoute = DeleteLocalGatewayRoute'
  { _dlgrDryRun ::
      !(Maybe Bool),
    _dlgrDestinationCidrBlock :: !Text,
    _dlgrLocalGatewayRouteTableId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteLocalGatewayRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dlgrDestinationCidrBlock' - The CIDR range for the route. This must match the CIDR for the route exactly.
--
-- * 'dlgrLocalGatewayRouteTableId' - The ID of the local gateway route table.
deleteLocalGatewayRoute ::
  -- | 'dlgrDestinationCidrBlock'
  Text ->
  -- | 'dlgrLocalGatewayRouteTableId'
  Text ->
  DeleteLocalGatewayRoute
deleteLocalGatewayRoute
  pDestinationCidrBlock_
  pLocalGatewayRouteTableId_ =
    DeleteLocalGatewayRoute'
      { _dlgrDryRun = Nothing,
        _dlgrDestinationCidrBlock = pDestinationCidrBlock_,
        _dlgrLocalGatewayRouteTableId = pLocalGatewayRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dlgrDryRun :: Lens' DeleteLocalGatewayRoute (Maybe Bool)
dlgrDryRun = lens _dlgrDryRun (\s a -> s {_dlgrDryRun = a})

-- | The CIDR range for the route. This must match the CIDR for the route exactly.
dlgrDestinationCidrBlock :: Lens' DeleteLocalGatewayRoute Text
dlgrDestinationCidrBlock = lens _dlgrDestinationCidrBlock (\s a -> s {_dlgrDestinationCidrBlock = a})

-- | The ID of the local gateway route table.
dlgrLocalGatewayRouteTableId :: Lens' DeleteLocalGatewayRoute Text
dlgrLocalGatewayRouteTableId = lens _dlgrLocalGatewayRouteTableId (\s a -> s {_dlgrLocalGatewayRouteTableId = a})

instance AWSRequest DeleteLocalGatewayRoute where
  type Rs DeleteLocalGatewayRoute = DeleteLocalGatewayRouteResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteLocalGatewayRouteResponse'
            <$> (x .@? "route") <*> (pure (fromEnum s))
      )

instance Hashable DeleteLocalGatewayRoute

instance NFData DeleteLocalGatewayRoute

instance ToHeaders DeleteLocalGatewayRoute where
  toHeaders = const mempty

instance ToPath DeleteLocalGatewayRoute where
  toPath = const "/"

instance ToQuery DeleteLocalGatewayRoute where
  toQuery DeleteLocalGatewayRoute' {..} =
    mconcat
      [ "Action" =: ("DeleteLocalGatewayRoute" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dlgrDryRun,
        "DestinationCidrBlock" =: _dlgrDestinationCidrBlock,
        "LocalGatewayRouteTableId" =: _dlgrLocalGatewayRouteTableId
      ]

-- | /See:/ 'deleteLocalGatewayRouteResponse' smart constructor.
data DeleteLocalGatewayRouteResponse = DeleteLocalGatewayRouteResponse'
  { _dlgrrsRoute ::
      !(Maybe LocalGatewayRoute),
    _dlgrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteLocalGatewayRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgrrsRoute' - Information about the route.
--
-- * 'dlgrrsResponseStatus' - -- | The response status code.
deleteLocalGatewayRouteResponse ::
  -- | 'dlgrrsResponseStatus'
  Int ->
  DeleteLocalGatewayRouteResponse
deleteLocalGatewayRouteResponse pResponseStatus_ =
  DeleteLocalGatewayRouteResponse'
    { _dlgrrsRoute = Nothing,
      _dlgrrsResponseStatus = pResponseStatus_
    }

-- | Information about the route.
dlgrrsRoute :: Lens' DeleteLocalGatewayRouteResponse (Maybe LocalGatewayRoute)
dlgrrsRoute = lens _dlgrrsRoute (\s a -> s {_dlgrrsRoute = a})

-- | -- | The response status code.
dlgrrsResponseStatus :: Lens' DeleteLocalGatewayRouteResponse Int
dlgrrsResponseStatus = lens _dlgrrsResponseStatus (\s a -> s {_dlgrrsResponseStatus = a})

instance NFData DeleteLocalGatewayRouteResponse
