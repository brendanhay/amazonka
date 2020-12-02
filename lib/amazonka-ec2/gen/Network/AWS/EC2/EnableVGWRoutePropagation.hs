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
-- Module      : Network.AWS.EC2.EnableVGWRoutePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a virtual private gateway (VGW) to propagate routes to the specified route table of a VPC.
module Network.AWS.EC2.EnableVGWRoutePropagation
  ( -- * Creating a Request
    enableVGWRoutePropagation,
    EnableVGWRoutePropagation,

    -- * Request Lenses
    evrpDryRun,
    evrpGatewayId,
    evrpRouteTableId,

    -- * Destructuring the Response
    enableVGWRoutePropagationResponse,
    EnableVGWRoutePropagationResponse,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for EnableVgwRoutePropagation.
--
--
--
-- /See:/ 'enableVGWRoutePropagation' smart constructor.
data EnableVGWRoutePropagation = EnableVGWRoutePropagation'
  { _evrpDryRun ::
      !(Maybe Bool),
    _evrpGatewayId :: !Text,
    _evrpRouteTableId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableVGWRoutePropagation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evrpDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'evrpGatewayId' - The ID of the virtual private gateway that is attached to a VPC. The virtual private gateway must be attached to the same VPC that the routing tables are associated with.
--
-- * 'evrpRouteTableId' - The ID of the route table. The routing table must be associated with the same VPC that the virtual private gateway is attached to.
enableVGWRoutePropagation ::
  -- | 'evrpGatewayId'
  Text ->
  -- | 'evrpRouteTableId'
  Text ->
  EnableVGWRoutePropagation
enableVGWRoutePropagation pGatewayId_ pRouteTableId_ =
  EnableVGWRoutePropagation'
    { _evrpDryRun = Nothing,
      _evrpGatewayId = pGatewayId_,
      _evrpRouteTableId = pRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
evrpDryRun :: Lens' EnableVGWRoutePropagation (Maybe Bool)
evrpDryRun = lens _evrpDryRun (\s a -> s {_evrpDryRun = a})

-- | The ID of the virtual private gateway that is attached to a VPC. The virtual private gateway must be attached to the same VPC that the routing tables are associated with.
evrpGatewayId :: Lens' EnableVGWRoutePropagation Text
evrpGatewayId = lens _evrpGatewayId (\s a -> s {_evrpGatewayId = a})

-- | The ID of the route table. The routing table must be associated with the same VPC that the virtual private gateway is attached to.
evrpRouteTableId :: Lens' EnableVGWRoutePropagation Text
evrpRouteTableId = lens _evrpRouteTableId (\s a -> s {_evrpRouteTableId = a})

instance AWSRequest EnableVGWRoutePropagation where
  type
    Rs EnableVGWRoutePropagation =
      EnableVGWRoutePropagationResponse
  request = postQuery ec2
  response = receiveNull EnableVGWRoutePropagationResponse'

instance Hashable EnableVGWRoutePropagation

instance NFData EnableVGWRoutePropagation

instance ToHeaders EnableVGWRoutePropagation where
  toHeaders = const mempty

instance ToPath EnableVGWRoutePropagation where
  toPath = const "/"

instance ToQuery EnableVGWRoutePropagation where
  toQuery EnableVGWRoutePropagation' {..} =
    mconcat
      [ "Action" =: ("EnableVgwRoutePropagation" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _evrpDryRun,
        "GatewayId" =: _evrpGatewayId,
        "RouteTableId" =: _evrpRouteTableId
      ]

-- | /See:/ 'enableVGWRoutePropagationResponse' smart constructor.
data EnableVGWRoutePropagationResponse = EnableVGWRoutePropagationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableVGWRoutePropagationResponse' with the minimum fields required to make a request.
enableVGWRoutePropagationResponse ::
  EnableVGWRoutePropagationResponse
enableVGWRoutePropagationResponse =
  EnableVGWRoutePropagationResponse'

instance NFData EnableVGWRoutePropagationResponse
