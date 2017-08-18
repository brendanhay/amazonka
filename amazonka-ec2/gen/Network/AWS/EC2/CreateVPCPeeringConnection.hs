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
-- Module      : Network.AWS.EC2.CreateVPCPeeringConnection
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a VPC peering connection between two VPCs: a requester VPC that you own and a peer VPC with which to create the connection. The peer VPC can belong to another AWS account. The requester VPC and peer VPC cannot have overlapping CIDR blocks.
--
--
-- The owner of the peer VPC must accept the peering request to activate the peering connection. The VPC peering connection request expires after 7 days, after which it cannot be accepted or rejected.
--
-- If you try to create a VPC peering connection between VPCs that have overlapping CIDR blocks, the VPC peering connection status goes to @failed@ .
--
module Network.AWS.EC2.CreateVPCPeeringConnection
    (
    -- * Creating a Request
      createVPCPeeringConnection
    , CreateVPCPeeringConnection
    -- * Request Lenses
    , cvpcPeerVPCId
    , cvpcVPCId
    , cvpcPeerOwnerId
    , cvpcDryRun

    -- * Destructuring the Response
    , createVPCPeeringConnectionResponse
    , CreateVPCPeeringConnectionResponse
    -- * Response Lenses
    , cvpcrsVPCPeeringConnection
    , cvpcrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CreateVpcPeeringConnection.
--
--
--
-- /See:/ 'createVPCPeeringConnection' smart constructor.
data CreateVPCPeeringConnection = CreateVPCPeeringConnection'
    { _cvpcPeerVPCId   :: !(Maybe Text)
    , _cvpcVPCId       :: !(Maybe Text)
    , _cvpcPeerOwnerId :: !(Maybe Text)
    , _cvpcDryRun      :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateVPCPeeringConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvpcPeerVPCId' - The ID of the VPC with which you are creating the VPC peering connection.
--
-- * 'cvpcVPCId' - The ID of the requester VPC.
--
-- * 'cvpcPeerOwnerId' - The AWS account ID of the owner of the peer VPC. Default: Your AWS account ID
--
-- * 'cvpcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
createVPCPeeringConnection
    :: CreateVPCPeeringConnection
createVPCPeeringConnection =
    CreateVPCPeeringConnection'
    { _cvpcPeerVPCId = Nothing
    , _cvpcVPCId = Nothing
    , _cvpcPeerOwnerId = Nothing
    , _cvpcDryRun = Nothing
    }

-- | The ID of the VPC with which you are creating the VPC peering connection.
cvpcPeerVPCId :: Lens' CreateVPCPeeringConnection (Maybe Text)
cvpcPeerVPCId = lens _cvpcPeerVPCId (\ s a -> s{_cvpcPeerVPCId = a});

-- | The ID of the requester VPC.
cvpcVPCId :: Lens' CreateVPCPeeringConnection (Maybe Text)
cvpcVPCId = lens _cvpcVPCId (\ s a -> s{_cvpcVPCId = a});

-- | The AWS account ID of the owner of the peer VPC. Default: Your AWS account ID
cvpcPeerOwnerId :: Lens' CreateVPCPeeringConnection (Maybe Text)
cvpcPeerOwnerId = lens _cvpcPeerOwnerId (\ s a -> s{_cvpcPeerOwnerId = a});

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cvpcDryRun :: Lens' CreateVPCPeeringConnection (Maybe Bool)
cvpcDryRun = lens _cvpcDryRun (\ s a -> s{_cvpcDryRun = a});

instance AWSRequest CreateVPCPeeringConnection where
        type Rs CreateVPCPeeringConnection =
             CreateVPCPeeringConnectionResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateVPCPeeringConnectionResponse' <$>
                   (x .@? "vpcPeeringConnection") <*>
                     (pure (fromEnum s)))

instance Hashable CreateVPCPeeringConnection

instance NFData CreateVPCPeeringConnection

instance ToHeaders CreateVPCPeeringConnection where
        toHeaders = const mempty

instance ToPath CreateVPCPeeringConnection where
        toPath = const "/"

instance ToQuery CreateVPCPeeringConnection where
        toQuery CreateVPCPeeringConnection'{..}
          = mconcat
              ["Action" =:
                 ("CreateVpcPeeringConnection" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "PeerVpcId" =: _cvpcPeerVPCId, "VpcId" =: _cvpcVPCId,
               "PeerOwnerId" =: _cvpcPeerOwnerId,
               "DryRun" =: _cvpcDryRun]

-- | Contains the output of CreateVpcPeeringConnection.
--
--
--
-- /See:/ 'createVPCPeeringConnectionResponse' smart constructor.
data CreateVPCPeeringConnectionResponse = CreateVPCPeeringConnectionResponse'
    { _cvpcrsVPCPeeringConnection :: !(Maybe VPCPeeringConnection)
    , _cvpcrsResponseStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateVPCPeeringConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvpcrsVPCPeeringConnection' - Information about the VPC peering connection.
--
-- * 'cvpcrsResponseStatus' - -- | The response status code.
createVPCPeeringConnectionResponse
    :: Int -- ^ 'cvpcrsResponseStatus'
    -> CreateVPCPeeringConnectionResponse
createVPCPeeringConnectionResponse pResponseStatus_ =
    CreateVPCPeeringConnectionResponse'
    { _cvpcrsVPCPeeringConnection = Nothing
    , _cvpcrsResponseStatus = pResponseStatus_
    }

-- | Information about the VPC peering connection.
cvpcrsVPCPeeringConnection :: Lens' CreateVPCPeeringConnectionResponse (Maybe VPCPeeringConnection)
cvpcrsVPCPeeringConnection = lens _cvpcrsVPCPeeringConnection (\ s a -> s{_cvpcrsVPCPeeringConnection = a});

-- | -- | The response status code.
cvpcrsResponseStatus :: Lens' CreateVPCPeeringConnectionResponse Int
cvpcrsResponseStatus = lens _cvpcrsResponseStatus (\ s a -> s{_cvpcrsResponseStatus = a});

instance NFData CreateVPCPeeringConnectionResponse
