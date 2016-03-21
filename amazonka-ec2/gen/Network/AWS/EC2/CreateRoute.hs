{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateRoute
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route in a route table within a VPC.
--
-- You must specify one of the following targets: Internet gateway or
-- virtual private gateway, NAT instance, NAT gateway, VPC peering
-- connection, or network interface.
--
-- When determining how to route traffic, we use the route with the most
-- specific match. For example, let\'s say the traffic is destined for
-- '192.0.2.3', and the route table includes the following two routes:
--
-- -   '192.0.2.0\/24' (goes to some target A)
--
-- -   '192.0.2.0\/28' (goes to some target B)
--
-- Both routes apply to the traffic destined for '192.0.2.3'. However, the
-- second route in the list covers a smaller number of IP addresses and is
-- therefore more specific, so we use that route to determine where to
-- target the traffic.
--
-- For more information about route tables, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.CreateRoute
    (
    -- * Creating a Request
      createRoute
    , CreateRoute
    -- * Request Lenses
    , crVPCPeeringConnectionId
    , crInstanceId
    , crNatGatewayId
    , crNetworkInterfaceId
    , crGatewayId
    , crDryRun
    , crRouteTableId
    , crDestinationCIdRBlock

    -- * Destructuring the Response
    , createRouteResponse
    , CreateRouteResponse
    -- * Response Lenses
    , crrsReturn
    , crrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createRoute' smart constructor.
data CreateRoute = CreateRoute'
    { _crVPCPeeringConnectionId :: !(Maybe Text)
    , _crInstanceId             :: !(Maybe Text)
    , _crNatGatewayId           :: !(Maybe Text)
    , _crNetworkInterfaceId     :: !(Maybe Text)
    , _crGatewayId              :: !(Maybe Text)
    , _crDryRun                 :: !(Maybe Bool)
    , _crRouteTableId           :: !Text
    , _crDestinationCIdRBlock   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crVPCPeeringConnectionId'
--
-- * 'crInstanceId'
--
-- * 'crNatGatewayId'
--
-- * 'crNetworkInterfaceId'
--
-- * 'crGatewayId'
--
-- * 'crDryRun'
--
-- * 'crRouteTableId'
--
-- * 'crDestinationCIdRBlock'
createRoute
    :: Text -- ^ 'crRouteTableId'
    -> Text -- ^ 'crDestinationCIdRBlock'
    -> CreateRoute
createRoute pRouteTableId_ pDestinationCIdRBlock_ =
    CreateRoute'
    { _crVPCPeeringConnectionId = Nothing
    , _crInstanceId = Nothing
    , _crNatGatewayId = Nothing
    , _crNetworkInterfaceId = Nothing
    , _crGatewayId = Nothing
    , _crDryRun = Nothing
    , _crRouteTableId = pRouteTableId_
    , _crDestinationCIdRBlock = pDestinationCIdRBlock_
    }

-- | The ID of a VPC peering connection.
crVPCPeeringConnectionId :: Lens' CreateRoute (Maybe Text)
crVPCPeeringConnectionId = lens _crVPCPeeringConnectionId (\ s a -> s{_crVPCPeeringConnectionId = a});

-- | The ID of a NAT instance in your VPC. The operation fails if you specify
-- an instance ID unless exactly one network interface is attached.
crInstanceId :: Lens' CreateRoute (Maybe Text)
crInstanceId = lens _crInstanceId (\ s a -> s{_crInstanceId = a});

-- | The ID of a NAT gateway.
crNatGatewayId :: Lens' CreateRoute (Maybe Text)
crNatGatewayId = lens _crNatGatewayId (\ s a -> s{_crNatGatewayId = a});

-- | The ID of a network interface.
crNetworkInterfaceId :: Lens' CreateRoute (Maybe Text)
crNetworkInterfaceId = lens _crNetworkInterfaceId (\ s a -> s{_crNetworkInterfaceId = a});

-- | The ID of an Internet gateway or virtual private gateway attached to
-- your VPC.
crGatewayId :: Lens' CreateRoute (Maybe Text)
crGatewayId = lens _crGatewayId (\ s a -> s{_crGatewayId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
crDryRun :: Lens' CreateRoute (Maybe Bool)
crDryRun = lens _crDryRun (\ s a -> s{_crDryRun = a});

-- | The ID of the route table for the route.
crRouteTableId :: Lens' CreateRoute Text
crRouteTableId = lens _crRouteTableId (\ s a -> s{_crRouteTableId = a});

-- | The CIDR address block used for the destination match. Routing decisions
-- are based on the most specific match.
crDestinationCIdRBlock :: Lens' CreateRoute Text
crDestinationCIdRBlock = lens _crDestinationCIdRBlock (\ s a -> s{_crDestinationCIdRBlock = a});

instance AWSRequest CreateRoute where
        type Rs CreateRoute = CreateRouteResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateRouteResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable CreateRoute

instance ToHeaders CreateRoute where
        toHeaders = const mempty

instance ToPath CreateRoute where
        toPath = const "/"

instance ToQuery CreateRoute where
        toQuery CreateRoute'{..}
          = mconcat
              ["Action" =: ("CreateRoute" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "VpcPeeringConnectionId" =:
                 _crVPCPeeringConnectionId,
               "InstanceId" =: _crInstanceId,
               "NatGatewayId" =: _crNatGatewayId,
               "NetworkInterfaceId" =: _crNetworkInterfaceId,
               "GatewayId" =: _crGatewayId, "DryRun" =: _crDryRun,
               "RouteTableId" =: _crRouteTableId,
               "DestinationCidrBlock" =: _crDestinationCIdRBlock]

-- | /See:/ 'createRouteResponse' smart constructor.
data CreateRouteResponse = CreateRouteResponse'
    { _crrsReturn         :: !(Maybe Bool)
    , _crrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsReturn'
--
-- * 'crrsResponseStatus'
createRouteResponse
    :: Int -- ^ 'crrsResponseStatus'
    -> CreateRouteResponse
createRouteResponse pResponseStatus_ =
    CreateRouteResponse'
    { _crrsReturn = Nothing
    , _crrsResponseStatus = pResponseStatus_
    }

-- | Returns 'true' if the request succeeds; otherwise, it returns an error.
crrsReturn :: Lens' CreateRouteResponse (Maybe Bool)
crrsReturn = lens _crrsReturn (\ s a -> s{_crrsReturn = a});

-- | The response status code.
crrsResponseStatus :: Lens' CreateRouteResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\ s a -> s{_crrsResponseStatus = a});
