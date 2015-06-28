{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.CreateRoute
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

-- | Creates a route in a route table within a VPC.
--
-- You must specify one of the following targets: Internet gateway or
-- virtual private gateway, NAT instance, VPC peering connection, or
-- network interface.
--
-- When determining how to route traffic, we use the route with the most
-- specific match. For example, let\'s say the traffic is destined for
-- @192.0.2.3@, and the route table includes the following two routes:
--
-- -   @192.0.2.0\/24@ (goes to some target A)
--
-- -   @192.0.2.0\/28@ (goes to some target B)
--
-- Both routes apply to the traffic destined for @192.0.2.3@. However, the
-- second route in the list covers a smaller number of IP addresses and is
-- therefore more specific, so we use that route to determine where to
-- target the traffic.
--
-- For more information about route tables, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateRoute.html>
module Network.AWS.EC2.CreateRoute
    (
    -- * Request
      CreateRoute
    -- ** Request constructor
    , createRoute
    -- ** Request lenses
    , crInstanceId
    , crVPCPeeringConnectionId
    , crClientToken
    , crNetworkInterfaceId
    , crGatewayId
    , crDryRun
    , crRouteTableId
    , crDestinationCIDRBlock

    -- * Response
    , CreateRouteResponse
    -- ** Response constructor
    , createRouteResponse
    -- ** Response lenses
    , crrReturn
    , crrClientToken
    , crrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createRoute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crInstanceId'
--
-- * 'crVPCPeeringConnectionId'
--
-- * 'crClientToken'
--
-- * 'crNetworkInterfaceId'
--
-- * 'crGatewayId'
--
-- * 'crDryRun'
--
-- * 'crRouteTableId'
--
-- * 'crDestinationCIDRBlock'
data CreateRoute = CreateRoute'
    { _crInstanceId             :: !(Maybe Text)
    , _crVPCPeeringConnectionId :: !(Maybe Text)
    , _crClientToken            :: !(Maybe Text)
    , _crNetworkInterfaceId     :: !(Maybe Text)
    , _crGatewayId              :: !(Maybe Text)
    , _crDryRun                 :: !(Maybe Bool)
    , _crRouteTableId           :: !Text
    , _crDestinationCIDRBlock   :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateRoute' smart constructor.
createRoute :: Text -> Text -> CreateRoute
createRoute pRouteTableId pDestinationCIDRBlock =
    CreateRoute'
    { _crInstanceId = Nothing
    , _crVPCPeeringConnectionId = Nothing
    , _crClientToken = Nothing
    , _crNetworkInterfaceId = Nothing
    , _crGatewayId = Nothing
    , _crDryRun = Nothing
    , _crRouteTableId = pRouteTableId
    , _crDestinationCIDRBlock = pDestinationCIDRBlock
    }

-- | The ID of a NAT instance in your VPC. The operation fails if you specify
-- an instance ID unless exactly one network interface is attached.
crInstanceId :: Lens' CreateRoute (Maybe Text)
crInstanceId = lens _crInstanceId (\ s a -> s{_crInstanceId = a});

-- | The ID of a VPC peering connection.
crVPCPeeringConnectionId :: Lens' CreateRoute (Maybe Text)
crVPCPeeringConnectionId = lens _crVPCPeeringConnectionId (\ s a -> s{_crVPCPeeringConnectionId = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
crClientToken :: Lens' CreateRoute (Maybe Text)
crClientToken = lens _crClientToken (\ s a -> s{_crClientToken = a});

-- | The ID of a network interface.
crNetworkInterfaceId :: Lens' CreateRoute (Maybe Text)
crNetworkInterfaceId = lens _crNetworkInterfaceId (\ s a -> s{_crNetworkInterfaceId = a});

-- | The ID of an Internet gateway or virtual private gateway attached to
-- your VPC.
crGatewayId :: Lens' CreateRoute (Maybe Text)
crGatewayId = lens _crGatewayId (\ s a -> s{_crGatewayId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
crDryRun :: Lens' CreateRoute (Maybe Bool)
crDryRun = lens _crDryRun (\ s a -> s{_crDryRun = a});

-- | The ID of the route table for the route.
crRouteTableId :: Lens' CreateRoute Text
crRouteTableId = lens _crRouteTableId (\ s a -> s{_crRouteTableId = a});

-- | The CIDR address block used for the destination match. Routing decisions
-- are based on the most specific match.
crDestinationCIDRBlock :: Lens' CreateRoute Text
crDestinationCIDRBlock = lens _crDestinationCIDRBlock (\ s a -> s{_crDestinationCIDRBlock = a});

instance AWSRequest CreateRoute where
        type Sv CreateRoute = EC2
        type Rs CreateRoute = CreateRouteResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateRouteResponse' <$>
                   (x .@? "return") <*> (x .@? "clientToken") <*>
                     (pure s))

instance ToHeaders CreateRoute where
        toHeaders = const mempty

instance ToPath CreateRoute where
        toPath = const "/"

instance ToQuery CreateRoute where
        toQuery CreateRoute'{..}
          = mconcat
              ["Action" =: ("CreateRoute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "InstanceId" =: _crInstanceId,
               "VpcPeeringConnectionId" =:
                 _crVPCPeeringConnectionId,
               "ClientToken" =: _crClientToken,
               "NetworkInterfaceId" =: _crNetworkInterfaceId,
               "GatewayId" =: _crGatewayId, "DryRun" =: _crDryRun,
               "RouteTableId" =: _crRouteTableId,
               "DestinationCidrBlock" =: _crDestinationCIDRBlock]

-- | /See:/ 'createRouteResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrReturn'
--
-- * 'crrClientToken'
--
-- * 'crrStatus'
data CreateRouteResponse = CreateRouteResponse'
    { _crrReturn      :: !(Maybe Bool)
    , _crrClientToken :: !(Maybe Text)
    , _crrStatus      :: !Status
    } deriving (Eq,Read,Show)

-- | 'CreateRouteResponse' smart constructor.
createRouteResponse :: Status -> CreateRouteResponse
createRouteResponse pStatus =
    CreateRouteResponse'
    { _crrReturn = Nothing
    , _crrClientToken = Nothing
    , _crrStatus = pStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
crrReturn :: Lens' CreateRouteResponse (Maybe Bool)
crrReturn = lens _crrReturn (\ s a -> s{_crrReturn = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request.
crrClientToken :: Lens' CreateRouteResponse (Maybe Text)
crrClientToken = lens _crrClientToken (\ s a -> s{_crrClientToken = a});

-- | FIXME: Undocumented member.
crrStatus :: Lens' CreateRouteResponse Status
crrStatus = lens _crrStatus (\ s a -> s{_crrStatus = a});
