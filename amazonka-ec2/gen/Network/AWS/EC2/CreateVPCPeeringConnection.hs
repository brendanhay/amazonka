{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.CreateVPCPeeringConnection
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

-- | Requests a VPC peering connection between two VPCs: a requester VPC that
-- you own and a peer VPC with which to create the connection. The peer VPC
-- can belong to another AWS account. The requester VPC and peer VPC cannot
-- have overlapping CIDR blocks.
--
-- The owner of the peer VPC must accept the peering request to activate
-- the peering connection. The VPC peering connection request expires after
-- 7 days, after which it cannot be accepted or rejected.
--
-- A @CreateVpcPeeringConnection@ request between VPCs with overlapping
-- CIDR blocks results in the VPC peering connection having a status of
-- @failed@.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPCPeeringConnection.html>
module Network.AWS.EC2.CreateVPCPeeringConnection
    (
    -- * Request
      CreateVPCPeeringConnection
    -- ** Request constructor
    , createVPCPeeringConnection
    -- ** Request lenses
    , cvpcPeerVPCId
    , cvpcVPCId
    , cvpcPeerOwnerId
    , cvpcDryRun

    -- * Response
    , CreateVPCPeeringConnectionResponse
    -- ** Response constructor
    , createVPCPeeringConnectionResponse
    -- ** Response lenses
    , cvpcrVPCPeeringConnection
    , cvpcrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createVPCPeeringConnection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpcPeerVPCId'
--
-- * 'cvpcVPCId'
--
-- * 'cvpcPeerOwnerId'
--
-- * 'cvpcDryRun'
data CreateVPCPeeringConnection = CreateVPCPeeringConnection'
    { _cvpcPeerVPCId   :: !(Maybe Text)
    , _cvpcVPCId       :: !(Maybe Text)
    , _cvpcPeerOwnerId :: !(Maybe Text)
    , _cvpcDryRun      :: !(Maybe Bool)
    } deriving (Eq,Read,Show)

-- | 'CreateVPCPeeringConnection' smart constructor.
createVPCPeeringConnection :: CreateVPCPeeringConnection
createVPCPeeringConnection =
    CreateVPCPeeringConnection'
    { _cvpcPeerVPCId = Nothing
    , _cvpcVPCId = Nothing
    , _cvpcPeerOwnerId = Nothing
    , _cvpcDryRun = Nothing
    }

-- | The ID of the VPC with which you are creating the VPC peering
-- connection.
cvpcPeerVPCId :: Lens' CreateVPCPeeringConnection (Maybe Text)
cvpcPeerVPCId = lens _cvpcPeerVPCId (\ s a -> s{_cvpcPeerVPCId = a});

-- | The ID of the requester VPC.
cvpcVPCId :: Lens' CreateVPCPeeringConnection (Maybe Text)
cvpcVPCId = lens _cvpcVPCId (\ s a -> s{_cvpcVPCId = a});

-- | The AWS account ID of the owner of the peer VPC.
--
-- Default: Your AWS account ID
cvpcPeerOwnerId :: Lens' CreateVPCPeeringConnection (Maybe Text)
cvpcPeerOwnerId = lens _cvpcPeerOwnerId (\ s a -> s{_cvpcPeerOwnerId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cvpcDryRun :: Lens' CreateVPCPeeringConnection (Maybe Bool)
cvpcDryRun = lens _cvpcDryRun (\ s a -> s{_cvpcDryRun = a});

instance AWSRequest CreateVPCPeeringConnection where
        type Sv CreateVPCPeeringConnection = EC2
        type Rs CreateVPCPeeringConnection =
             CreateVPCPeeringConnectionResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateVPCPeeringConnectionResponse' <$>
                   (x .@? "vpcPeeringConnection") <*> (pure s))

instance ToHeaders CreateVPCPeeringConnection where
        toHeaders = const mempty

instance ToPath CreateVPCPeeringConnection where
        toPath = const "/"

instance ToQuery CreateVPCPeeringConnection where
        toQuery CreateVPCPeeringConnection'{..}
          = mconcat
              ["Action" =:
                 ("CreateVPCPeeringConnection" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "PeerVpcId" =: _cvpcPeerVPCId, "VpcId" =: _cvpcVPCId,
               "PeerOwnerId" =: _cvpcPeerOwnerId,
               "DryRun" =: _cvpcDryRun]

-- | /See:/ 'createVPCPeeringConnectionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpcrVPCPeeringConnection'
--
-- * 'cvpcrStatus'
data CreateVPCPeeringConnectionResponse = CreateVPCPeeringConnectionResponse'
    { _cvpcrVPCPeeringConnection :: !(Maybe VPCPeeringConnection)
    , _cvpcrStatus               :: !Status
    } deriving (Eq,Show)

-- | 'CreateVPCPeeringConnectionResponse' smart constructor.
createVPCPeeringConnectionResponse :: Status -> CreateVPCPeeringConnectionResponse
createVPCPeeringConnectionResponse pStatus =
    CreateVPCPeeringConnectionResponse'
    { _cvpcrVPCPeeringConnection = Nothing
    , _cvpcrStatus = pStatus
    }

-- | Information about the VPC peering connection.
cvpcrVPCPeeringConnection :: Lens' CreateVPCPeeringConnectionResponse (Maybe VPCPeeringConnection)
cvpcrVPCPeeringConnection = lens _cvpcrVPCPeeringConnection (\ s a -> s{_cvpcrVPCPeeringConnection = a});

-- | FIXME: Undocumented member.
cvpcrStatus :: Lens' CreateVPCPeeringConnectionResponse Status
cvpcrStatus = lens _cvpcrStatus (\ s a -> s{_cvpcrStatus = a});
