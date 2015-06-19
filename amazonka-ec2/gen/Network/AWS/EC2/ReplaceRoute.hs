{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.ReplaceRoute
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Replaces an existing route within a route table in a VPC. You must
-- provide only one of the following: Internet gateway or virtual private
-- gateway, NAT instance, VPC peering connection, or network interface.
--
-- For more information about route tables, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceRoute.html>
module Network.AWS.EC2.ReplaceRoute
    (
    -- * Request
      ReplaceRoute
    -- ** Request constructor
    , replaceRoute
    -- ** Request lenses
    , rrInstanceId
    , rrVPCPeeringConnectionId
    , rrNetworkInterfaceId
    , rrGatewayId
    , rrDryRun
    , rrRouteTableId
    , rrDestinationCIDRBlock

    -- * Response
    , ReplaceRouteResponse
    -- ** Response constructor
    , replaceRouteResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'replaceRoute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrInstanceId'
--
-- * 'rrVPCPeeringConnectionId'
--
-- * 'rrNetworkInterfaceId'
--
-- * 'rrGatewayId'
--
-- * 'rrDryRun'
--
-- * 'rrRouteTableId'
--
-- * 'rrDestinationCIDRBlock'
data ReplaceRoute = ReplaceRoute'{_rrInstanceId :: Maybe Text, _rrVPCPeeringConnectionId :: Maybe Text, _rrNetworkInterfaceId :: Maybe Text, _rrGatewayId :: Maybe Text, _rrDryRun :: Maybe Bool, _rrRouteTableId :: Text, _rrDestinationCIDRBlock :: Text} deriving (Eq, Read, Show)

-- | 'ReplaceRoute' smart constructor.
replaceRoute :: Text -> Text -> ReplaceRoute
replaceRoute pRouteTableId pDestinationCIDRBlock = ReplaceRoute'{_rrInstanceId = Nothing, _rrVPCPeeringConnectionId = Nothing, _rrNetworkInterfaceId = Nothing, _rrGatewayId = Nothing, _rrDryRun = Nothing, _rrRouteTableId = pRouteTableId, _rrDestinationCIDRBlock = pDestinationCIDRBlock};

-- | The ID of a NAT instance in your VPC.
rrInstanceId :: Lens' ReplaceRoute (Maybe Text)
rrInstanceId = lens _rrInstanceId (\ s a -> s{_rrInstanceId = a});

-- | The ID of a VPC peering connection.
rrVPCPeeringConnectionId :: Lens' ReplaceRoute (Maybe Text)
rrVPCPeeringConnectionId = lens _rrVPCPeeringConnectionId (\ s a -> s{_rrVPCPeeringConnectionId = a});

-- | The ID of a network interface.
rrNetworkInterfaceId :: Lens' ReplaceRoute (Maybe Text)
rrNetworkInterfaceId = lens _rrNetworkInterfaceId (\ s a -> s{_rrNetworkInterfaceId = a});

-- | The ID of an Internet gateway or virtual private gateway.
rrGatewayId :: Lens' ReplaceRoute (Maybe Text)
rrGatewayId = lens _rrGatewayId (\ s a -> s{_rrGatewayId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rrDryRun :: Lens' ReplaceRoute (Maybe Bool)
rrDryRun = lens _rrDryRun (\ s a -> s{_rrDryRun = a});

-- | The ID of the route table.
rrRouteTableId :: Lens' ReplaceRoute Text
rrRouteTableId = lens _rrRouteTableId (\ s a -> s{_rrRouteTableId = a});

-- | The CIDR address block used for the destination match. The value you
-- provide must match the CIDR of an existing route in the table.
rrDestinationCIDRBlock :: Lens' ReplaceRoute Text
rrDestinationCIDRBlock = lens _rrDestinationCIDRBlock (\ s a -> s{_rrDestinationCIDRBlock = a});

instance AWSRequest ReplaceRoute where
        type Sv ReplaceRoute = EC2
        type Rs ReplaceRoute = ReplaceRouteResponse
        request = post
        response = receiveNull ReplaceRouteResponse'

instance ToHeaders ReplaceRoute where
        toHeaders = const mempty

instance ToPath ReplaceRoute where
        toPath = const "/"

instance ToQuery ReplaceRoute where
        toQuery ReplaceRoute'{..}
          = mconcat
              ["Action" =: ("ReplaceRoute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "InstanceId" =: _rrInstanceId,
               "VpcPeeringConnectionId" =:
                 _rrVPCPeeringConnectionId,
               "NetworkInterfaceId" =: _rrNetworkInterfaceId,
               "GatewayId" =: _rrGatewayId, "DryRun" =: _rrDryRun,
               "RouteTableId" =: _rrRouteTableId,
               "DestinationCidrBlock" =: _rrDestinationCIDRBlock]

-- | /See:/ 'replaceRouteResponse' smart constructor.
data ReplaceRouteResponse = ReplaceRouteResponse' deriving (Eq, Read, Show)

-- | 'ReplaceRouteResponse' smart constructor.
replaceRouteResponse :: ReplaceRouteResponse
replaceRouteResponse = ReplaceRouteResponse';
