{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceRoute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Replaces an existing route within a route table in a VPC. You must
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
    , rrrqInstanceId
    , rrrqVPCPeeringConnectionId
    , rrrqNetworkInterfaceId
    , rrrqGatewayId
    , rrrqDryRun
    , rrrqRouteTableId
    , rrrqDestinationCIdRBlock

    -- * Response
    , ReplaceRouteResponse
    -- ** Response constructor
    , replaceRouteResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'replaceRoute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrrqInstanceId'
--
-- * 'rrrqVPCPeeringConnectionId'
--
-- * 'rrrqNetworkInterfaceId'
--
-- * 'rrrqGatewayId'
--
-- * 'rrrqDryRun'
--
-- * 'rrrqRouteTableId'
--
-- * 'rrrqDestinationCIdRBlock'
data ReplaceRoute = ReplaceRoute'
    { _rrrqInstanceId             :: !(Maybe Text)
    , _rrrqVPCPeeringConnectionId :: !(Maybe Text)
    , _rrrqNetworkInterfaceId     :: !(Maybe Text)
    , _rrrqGatewayId              :: !(Maybe Text)
    , _rrrqDryRun                 :: !(Maybe Bool)
    , _rrrqRouteTableId           :: !Text
    , _rrrqDestinationCIdRBlock   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceRoute' smart constructor.
replaceRoute :: Text -> Text -> ReplaceRoute
replaceRoute pRouteTableId_ pDestinationCIdRBlock_ =
    ReplaceRoute'
    { _rrrqInstanceId = Nothing
    , _rrrqVPCPeeringConnectionId = Nothing
    , _rrrqNetworkInterfaceId = Nothing
    , _rrrqGatewayId = Nothing
    , _rrrqDryRun = Nothing
    , _rrrqRouteTableId = pRouteTableId_
    , _rrrqDestinationCIdRBlock = pDestinationCIdRBlock_
    }

-- | The ID of a NAT instance in your VPC.
rrrqInstanceId :: Lens' ReplaceRoute (Maybe Text)
rrrqInstanceId = lens _rrrqInstanceId (\ s a -> s{_rrrqInstanceId = a});

-- | The ID of a VPC peering connection.
rrrqVPCPeeringConnectionId :: Lens' ReplaceRoute (Maybe Text)
rrrqVPCPeeringConnectionId = lens _rrrqVPCPeeringConnectionId (\ s a -> s{_rrrqVPCPeeringConnectionId = a});

-- | The ID of a network interface.
rrrqNetworkInterfaceId :: Lens' ReplaceRoute (Maybe Text)
rrrqNetworkInterfaceId = lens _rrrqNetworkInterfaceId (\ s a -> s{_rrrqNetworkInterfaceId = a});

-- | The ID of an Internet gateway or virtual private gateway.
rrrqGatewayId :: Lens' ReplaceRoute (Maybe Text)
rrrqGatewayId = lens _rrrqGatewayId (\ s a -> s{_rrrqGatewayId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rrrqDryRun :: Lens' ReplaceRoute (Maybe Bool)
rrrqDryRun = lens _rrrqDryRun (\ s a -> s{_rrrqDryRun = a});

-- | The ID of the route table.
rrrqRouteTableId :: Lens' ReplaceRoute Text
rrrqRouteTableId = lens _rrrqRouteTableId (\ s a -> s{_rrrqRouteTableId = a});

-- | The CIDR address block used for the destination match. The value you
-- provide must match the CIDR of an existing route in the table.
rrrqDestinationCIdRBlock :: Lens' ReplaceRoute Text
rrrqDestinationCIdRBlock = lens _rrrqDestinationCIdRBlock (\ s a -> s{_rrrqDestinationCIdRBlock = a});

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
               "InstanceId" =: _rrrqInstanceId,
               "VpcPeeringConnectionId" =:
                 _rrrqVPCPeeringConnectionId,
               "NetworkInterfaceId" =: _rrrqNetworkInterfaceId,
               "GatewayId" =: _rrrqGatewayId,
               "DryRun" =: _rrrqDryRun,
               "RouteTableId" =: _rrrqRouteTableId,
               "DestinationCidrBlock" =: _rrrqDestinationCIdRBlock]

-- | /See:/ 'replaceRouteResponse' smart constructor.
data ReplaceRouteResponse =
    ReplaceRouteResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceRouteResponse' smart constructor.
replaceRouteResponse :: ReplaceRouteResponse
replaceRouteResponse = ReplaceRouteResponse'
