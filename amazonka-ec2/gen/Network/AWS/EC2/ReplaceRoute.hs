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
-- Module      : Network.AWS.EC2.ReplaceRoute
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an existing route within a route table in a VPC. You must provide only one of the following: Internet gateway or virtual private gateway, NAT instance, NAT gateway, VPC peering connection, or network interface.
--
-- For more information about route tables, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.ReplaceRoute
    (
    -- * Creating a Request
      replaceRoute
    , ReplaceRoute
    -- * Request Lenses
    , rrVPCPeeringConnectionId
    , rrInstanceId
    , rrNatGatewayId
    , rrNetworkInterfaceId
    , rrGatewayId
    , rrDryRun
    , rrRouteTableId
    , rrDestinationCIdRBlock

    -- * Destructuring the Response
    , replaceRouteResponse
    , ReplaceRouteResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for ReplaceRoute.
--
-- /See:/ 'replaceRoute' smart constructor.
data ReplaceRoute = ReplaceRoute'
    { _rrVPCPeeringConnectionId :: !(Maybe Text)
    , _rrInstanceId             :: !(Maybe Text)
    , _rrNatGatewayId           :: !(Maybe Text)
    , _rrNetworkInterfaceId     :: !(Maybe Text)
    , _rrGatewayId              :: !(Maybe Text)
    , _rrDryRun                 :: !(Maybe Bool)
    , _rrRouteTableId           :: !Text
    , _rrDestinationCIdRBlock   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReplaceRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrVPCPeeringConnectionId'
--
-- * 'rrInstanceId'
--
-- * 'rrNatGatewayId'
--
-- * 'rrNetworkInterfaceId'
--
-- * 'rrGatewayId'
--
-- * 'rrDryRun'
--
-- * 'rrRouteTableId'
--
-- * 'rrDestinationCIdRBlock'
replaceRoute
    :: Text -- ^ 'rrRouteTableId'
    -> Text -- ^ 'rrDestinationCIdRBlock'
    -> ReplaceRoute
replaceRoute pRouteTableId_ pDestinationCIdRBlock_ =
    ReplaceRoute'
    { _rrVPCPeeringConnectionId = Nothing
    , _rrInstanceId = Nothing
    , _rrNatGatewayId = Nothing
    , _rrNetworkInterfaceId = Nothing
    , _rrGatewayId = Nothing
    , _rrDryRun = Nothing
    , _rrRouteTableId = pRouteTableId_
    , _rrDestinationCIdRBlock = pDestinationCIdRBlock_
    }

-- | The ID of a VPC peering connection.
rrVPCPeeringConnectionId :: Lens' ReplaceRoute (Maybe Text)
rrVPCPeeringConnectionId = lens _rrVPCPeeringConnectionId (\ s a -> s{_rrVPCPeeringConnectionId = a});

-- | The ID of a NAT instance in your VPC.
rrInstanceId :: Lens' ReplaceRoute (Maybe Text)
rrInstanceId = lens _rrInstanceId (\ s a -> s{_rrInstanceId = a});

-- | The ID of a NAT gateway.
rrNatGatewayId :: Lens' ReplaceRoute (Maybe Text)
rrNatGatewayId = lens _rrNatGatewayId (\ s a -> s{_rrNatGatewayId = a});

-- | The ID of a network interface.
rrNetworkInterfaceId :: Lens' ReplaceRoute (Maybe Text)
rrNetworkInterfaceId = lens _rrNetworkInterfaceId (\ s a -> s{_rrNetworkInterfaceId = a});

-- | The ID of an Internet gateway or virtual private gateway.
rrGatewayId :: Lens' ReplaceRoute (Maybe Text)
rrGatewayId = lens _rrGatewayId (\ s a -> s{_rrGatewayId = a});

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
rrDryRun :: Lens' ReplaceRoute (Maybe Bool)
rrDryRun = lens _rrDryRun (\ s a -> s{_rrDryRun = a});

-- | The ID of the route table.
rrRouteTableId :: Lens' ReplaceRoute Text
rrRouteTableId = lens _rrRouteTableId (\ s a -> s{_rrRouteTableId = a});

-- | The CIDR address block used for the destination match. The value you provide must match the CIDR of an existing route in the table.
rrDestinationCIdRBlock :: Lens' ReplaceRoute Text
rrDestinationCIdRBlock = lens _rrDestinationCIdRBlock (\ s a -> s{_rrDestinationCIdRBlock = a});

instance AWSRequest ReplaceRoute where
        type Rs ReplaceRoute = ReplaceRouteResponse
        request = postQuery ec2
        response = receiveNull ReplaceRouteResponse'

instance Hashable ReplaceRoute

instance NFData ReplaceRoute

instance ToHeaders ReplaceRoute where
        toHeaders = const mempty

instance ToPath ReplaceRoute where
        toPath = const "/"

instance ToQuery ReplaceRoute where
        toQuery ReplaceRoute'{..}
          = mconcat
              ["Action" =: ("ReplaceRoute" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "VpcPeeringConnectionId" =:
                 _rrVPCPeeringConnectionId,
               "InstanceId" =: _rrInstanceId,
               "NatGatewayId" =: _rrNatGatewayId,
               "NetworkInterfaceId" =: _rrNetworkInterfaceId,
               "GatewayId" =: _rrGatewayId, "DryRun" =: _rrDryRun,
               "RouteTableId" =: _rrRouteTableId,
               "DestinationCidrBlock" =: _rrDestinationCIdRBlock]

-- | /See:/ 'replaceRouteResponse' smart constructor.
data ReplaceRouteResponse =
    ReplaceRouteResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReplaceRouteResponse' with the minimum fields required to make a request.
--
replaceRouteResponse
    :: ReplaceRouteResponse
replaceRouteResponse = ReplaceRouteResponse'

instance NFData ReplaceRouteResponse
