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
-- Module      : Network.AWS.EC2.CreateLocalGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route for the specified local gateway route table.
module Network.AWS.EC2.CreateLocalGatewayRoute
  ( -- * Creating a Request
    createLocalGatewayRoute,
    CreateLocalGatewayRoute,

    -- * Request Lenses
    clgrDryRun,
    clgrDestinationCidrBlock,
    clgrLocalGatewayRouteTableId,
    clgrLocalGatewayVirtualInterfaceGroupId,

    -- * Destructuring the Response
    createLocalGatewayRouteResponse,
    CreateLocalGatewayRouteResponse,

    -- * Response Lenses
    clgrrsRoute,
    clgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLocalGatewayRoute' smart constructor.
data CreateLocalGatewayRoute = CreateLocalGatewayRoute'
  { _clgrDryRun ::
      !(Maybe Bool),
    _clgrDestinationCidrBlock :: !Text,
    _clgrLocalGatewayRouteTableId :: !Text,
    _clgrLocalGatewayVirtualInterfaceGroupId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateLocalGatewayRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clgrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'clgrDestinationCidrBlock' - The CIDR range used for destination matches. Routing decisions are based on the most specific match.
--
-- * 'clgrLocalGatewayRouteTableId' - The ID of the local gateway route table.
--
-- * 'clgrLocalGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
createLocalGatewayRoute ::
  -- | 'clgrDestinationCidrBlock'
  Text ->
  -- | 'clgrLocalGatewayRouteTableId'
  Text ->
  -- | 'clgrLocalGatewayVirtualInterfaceGroupId'
  Text ->
  CreateLocalGatewayRoute
createLocalGatewayRoute
  pDestinationCidrBlock_
  pLocalGatewayRouteTableId_
  pLocalGatewayVirtualInterfaceGroupId_ =
    CreateLocalGatewayRoute'
      { _clgrDryRun = Nothing,
        _clgrDestinationCidrBlock = pDestinationCidrBlock_,
        _clgrLocalGatewayRouteTableId = pLocalGatewayRouteTableId_,
        _clgrLocalGatewayVirtualInterfaceGroupId =
          pLocalGatewayVirtualInterfaceGroupId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
clgrDryRun :: Lens' CreateLocalGatewayRoute (Maybe Bool)
clgrDryRun = lens _clgrDryRun (\s a -> s {_clgrDryRun = a})

-- | The CIDR range used for destination matches. Routing decisions are based on the most specific match.
clgrDestinationCidrBlock :: Lens' CreateLocalGatewayRoute Text
clgrDestinationCidrBlock = lens _clgrDestinationCidrBlock (\s a -> s {_clgrDestinationCidrBlock = a})

-- | The ID of the local gateway route table.
clgrLocalGatewayRouteTableId :: Lens' CreateLocalGatewayRoute Text
clgrLocalGatewayRouteTableId = lens _clgrLocalGatewayRouteTableId (\s a -> s {_clgrLocalGatewayRouteTableId = a})

-- | The ID of the virtual interface group.
clgrLocalGatewayVirtualInterfaceGroupId :: Lens' CreateLocalGatewayRoute Text
clgrLocalGatewayVirtualInterfaceGroupId = lens _clgrLocalGatewayVirtualInterfaceGroupId (\s a -> s {_clgrLocalGatewayVirtualInterfaceGroupId = a})

instance AWSRequest CreateLocalGatewayRoute where
  type Rs CreateLocalGatewayRoute = CreateLocalGatewayRouteResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateLocalGatewayRouteResponse'
            <$> (x .@? "route") <*> (pure (fromEnum s))
      )

instance Hashable CreateLocalGatewayRoute

instance NFData CreateLocalGatewayRoute

instance ToHeaders CreateLocalGatewayRoute where
  toHeaders = const mempty

instance ToPath CreateLocalGatewayRoute where
  toPath = const "/"

instance ToQuery CreateLocalGatewayRoute where
  toQuery CreateLocalGatewayRoute' {..} =
    mconcat
      [ "Action" =: ("CreateLocalGatewayRoute" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _clgrDryRun,
        "DestinationCidrBlock" =: _clgrDestinationCidrBlock,
        "LocalGatewayRouteTableId" =: _clgrLocalGatewayRouteTableId,
        "LocalGatewayVirtualInterfaceGroupId"
          =: _clgrLocalGatewayVirtualInterfaceGroupId
      ]

-- | /See:/ 'createLocalGatewayRouteResponse' smart constructor.
data CreateLocalGatewayRouteResponse = CreateLocalGatewayRouteResponse'
  { _clgrrsRoute ::
      !(Maybe LocalGatewayRoute),
    _clgrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateLocalGatewayRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clgrrsRoute' - Information about the route.
--
-- * 'clgrrsResponseStatus' - -- | The response status code.
createLocalGatewayRouteResponse ::
  -- | 'clgrrsResponseStatus'
  Int ->
  CreateLocalGatewayRouteResponse
createLocalGatewayRouteResponse pResponseStatus_ =
  CreateLocalGatewayRouteResponse'
    { _clgrrsRoute = Nothing,
      _clgrrsResponseStatus = pResponseStatus_
    }

-- | Information about the route.
clgrrsRoute :: Lens' CreateLocalGatewayRouteResponse (Maybe LocalGatewayRoute)
clgrrsRoute = lens _clgrrsRoute (\s a -> s {_clgrrsRoute = a})

-- | -- | The response status code.
clgrrsResponseStatus :: Lens' CreateLocalGatewayRouteResponse Int
clgrrsResponseStatus = lens _clgrrsResponseStatus (\s a -> s {_clgrrsResponseStatus = a})

instance NFData CreateLocalGatewayRouteResponse
