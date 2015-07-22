{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateRoute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a route in a route table within a VPC.
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
    , crrqInstanceId
    , crrqVPCPeeringConnectionId
    , crrqClientToken
    , crrqNetworkInterfaceId
    , crrqGatewayId
    , crrqDryRun
    , crrqRouteTableId
    , crrqDestinationCIdRBlock

    -- * Response
    , CreateRouteResponse
    -- ** Response constructor
    , createRouteResponse
    -- ** Response lenses
    , crrsReturn
    , crrsClientToken
    , crrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createRoute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrqInstanceId'
--
-- * 'crrqVPCPeeringConnectionId'
--
-- * 'crrqClientToken'
--
-- * 'crrqNetworkInterfaceId'
--
-- * 'crrqGatewayId'
--
-- * 'crrqDryRun'
--
-- * 'crrqRouteTableId'
--
-- * 'crrqDestinationCIdRBlock'
data CreateRoute = CreateRoute'
    { _crrqInstanceId             :: !(Maybe Text)
    , _crrqVPCPeeringConnectionId :: !(Maybe Text)
    , _crrqClientToken            :: !(Maybe Text)
    , _crrqNetworkInterfaceId     :: !(Maybe Text)
    , _crrqGatewayId              :: !(Maybe Text)
    , _crrqDryRun                 :: !(Maybe Bool)
    , _crrqRouteTableId           :: !Text
    , _crrqDestinationCIdRBlock   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateRoute' smart constructor.
createRoute :: Text -> Text -> CreateRoute
createRoute pRouteTableId pDestinationCIdRBlock =
    CreateRoute'
    { _crrqInstanceId = Nothing
    , _crrqVPCPeeringConnectionId = Nothing
    , _crrqClientToken = Nothing
    , _crrqNetworkInterfaceId = Nothing
    , _crrqGatewayId = Nothing
    , _crrqDryRun = Nothing
    , _crrqRouteTableId = pRouteTableId
    , _crrqDestinationCIdRBlock = pDestinationCIdRBlock
    }

-- | The ID of a NAT instance in your VPC. The operation fails if you specify
-- an instance ID unless exactly one network interface is attached.
crrqInstanceId :: Lens' CreateRoute (Maybe Text)
crrqInstanceId = lens _crrqInstanceId (\ s a -> s{_crrqInstanceId = a});

-- | The ID of a VPC peering connection.
crrqVPCPeeringConnectionId :: Lens' CreateRoute (Maybe Text)
crrqVPCPeeringConnectionId = lens _crrqVPCPeeringConnectionId (\ s a -> s{_crrqVPCPeeringConnectionId = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
crrqClientToken :: Lens' CreateRoute (Maybe Text)
crrqClientToken = lens _crrqClientToken (\ s a -> s{_crrqClientToken = a});

-- | The ID of a network interface.
crrqNetworkInterfaceId :: Lens' CreateRoute (Maybe Text)
crrqNetworkInterfaceId = lens _crrqNetworkInterfaceId (\ s a -> s{_crrqNetworkInterfaceId = a});

-- | The ID of an Internet gateway or virtual private gateway attached to
-- your VPC.
crrqGatewayId :: Lens' CreateRoute (Maybe Text)
crrqGatewayId = lens _crrqGatewayId (\ s a -> s{_crrqGatewayId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
crrqDryRun :: Lens' CreateRoute (Maybe Bool)
crrqDryRun = lens _crrqDryRun (\ s a -> s{_crrqDryRun = a});

-- | The ID of the route table for the route.
crrqRouteTableId :: Lens' CreateRoute Text
crrqRouteTableId = lens _crrqRouteTableId (\ s a -> s{_crrqRouteTableId = a});

-- | The CIDR address block used for the destination match. Routing decisions
-- are based on the most specific match.
crrqDestinationCIdRBlock :: Lens' CreateRoute Text
crrqDestinationCIdRBlock = lens _crrqDestinationCIdRBlock (\ s a -> s{_crrqDestinationCIdRBlock = a});

instance AWSRequest CreateRoute where
        type Sv CreateRoute = EC2
        type Rs CreateRoute = CreateRouteResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateRouteResponse' <$>
                   (x .@? "return") <*> (x .@? "clientToken") <*>
                     (pure (fromEnum s)))

instance ToHeaders CreateRoute where
        toHeaders = const mempty

instance ToPath CreateRoute where
        toPath = const "/"

instance ToQuery CreateRoute where
        toQuery CreateRoute'{..}
          = mconcat
              ["Action" =: ("CreateRoute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "InstanceId" =: _crrqInstanceId,
               "VpcPeeringConnectionId" =:
                 _crrqVPCPeeringConnectionId,
               "ClientToken" =: _crrqClientToken,
               "NetworkInterfaceId" =: _crrqNetworkInterfaceId,
               "GatewayId" =: _crrqGatewayId,
               "DryRun" =: _crrqDryRun,
               "RouteTableId" =: _crrqRouteTableId,
               "DestinationCidrBlock" =: _crrqDestinationCIdRBlock]

-- | /See:/ 'createRouteResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrsReturn'
--
-- * 'crrsClientToken'
--
-- * 'crrsStatus'
data CreateRouteResponse = CreateRouteResponse'
    { _crrsReturn      :: !(Maybe Bool)
    , _crrsClientToken :: !(Maybe Text)
    , _crrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateRouteResponse' smart constructor.
createRouteResponse :: Int -> CreateRouteResponse
createRouteResponse pStatus =
    CreateRouteResponse'
    { _crrsReturn = Nothing
    , _crrsClientToken = Nothing
    , _crrsStatus = pStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
crrsReturn :: Lens' CreateRouteResponse (Maybe Bool)
crrsReturn = lens _crrsReturn (\ s a -> s{_crrsReturn = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request.
crrsClientToken :: Lens' CreateRouteResponse (Maybe Text)
crrsClientToken = lens _crrsClientToken (\ s a -> s{_crrsClientToken = a});

-- | FIXME: Undocumented member.
crrsStatus :: Lens' CreateRouteResponse Int
crrsStatus = lens _crrsStatus (\ s a -> s{_crrsStatus = a});
