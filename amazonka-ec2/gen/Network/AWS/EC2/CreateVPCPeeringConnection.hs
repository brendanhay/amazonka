{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPCPeeringConnection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Requests a VPC peering connection between two VPCs: a requester VPC that
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
    , cvpcrqPeerVPCId
    , cvpcrqVPCId
    , cvpcrqPeerOwnerId
    , cvpcrqDryRun

    -- * Response
    , CreateVPCPeeringConnectionResponse
    -- ** Response constructor
    , createVPCPeeringConnectionResponse
    -- ** Response lenses
    , cvpcrsVPCPeeringConnection
    , cvpcrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createVPCPeeringConnection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpcrqPeerVPCId'
--
-- * 'cvpcrqVPCId'
--
-- * 'cvpcrqPeerOwnerId'
--
-- * 'cvpcrqDryRun'
data CreateVPCPeeringConnection = CreateVPCPeeringConnection'
    { _cvpcrqPeerVPCId   :: !(Maybe Text)
    , _cvpcrqVPCId       :: !(Maybe Text)
    , _cvpcrqPeerOwnerId :: !(Maybe Text)
    , _cvpcrqDryRun      :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPCPeeringConnection' smart constructor.
createVPCPeeringConnection :: CreateVPCPeeringConnection
createVPCPeeringConnection =
    CreateVPCPeeringConnection'
    { _cvpcrqPeerVPCId = Nothing
    , _cvpcrqVPCId = Nothing
    , _cvpcrqPeerOwnerId = Nothing
    , _cvpcrqDryRun = Nothing
    }

-- | The ID of the VPC with which you are creating the VPC peering
-- connection.
cvpcrqPeerVPCId :: Lens' CreateVPCPeeringConnection (Maybe Text)
cvpcrqPeerVPCId = lens _cvpcrqPeerVPCId (\ s a -> s{_cvpcrqPeerVPCId = a});

-- | The ID of the requester VPC.
cvpcrqVPCId :: Lens' CreateVPCPeeringConnection (Maybe Text)
cvpcrqVPCId = lens _cvpcrqVPCId (\ s a -> s{_cvpcrqVPCId = a});

-- | The AWS account ID of the owner of the peer VPC.
--
-- Default: Your AWS account ID
cvpcrqPeerOwnerId :: Lens' CreateVPCPeeringConnection (Maybe Text)
cvpcrqPeerOwnerId = lens _cvpcrqPeerOwnerId (\ s a -> s{_cvpcrqPeerOwnerId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cvpcrqDryRun :: Lens' CreateVPCPeeringConnection (Maybe Bool)
cvpcrqDryRun = lens _cvpcrqDryRun (\ s a -> s{_cvpcrqDryRun = a});

instance AWSRequest CreateVPCPeeringConnection where
        type Sv CreateVPCPeeringConnection = EC2
        type Rs CreateVPCPeeringConnection =
             CreateVPCPeeringConnectionResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateVPCPeeringConnectionResponse' <$>
                   (x .@? "vpcPeeringConnection") <*>
                     (pure (fromEnum s)))

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
               "PeerVpcId" =: _cvpcrqPeerVPCId,
               "VpcId" =: _cvpcrqVPCId,
               "PeerOwnerId" =: _cvpcrqPeerOwnerId,
               "DryRun" =: _cvpcrqDryRun]

-- | /See:/ 'createVPCPeeringConnectionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpcrsVPCPeeringConnection'
--
-- * 'cvpcrsStatus'
data CreateVPCPeeringConnectionResponse = CreateVPCPeeringConnectionResponse'
    { _cvpcrsVPCPeeringConnection :: !(Maybe VPCPeeringConnection)
    , _cvpcrsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPCPeeringConnectionResponse' smart constructor.
createVPCPeeringConnectionResponse :: Int -> CreateVPCPeeringConnectionResponse
createVPCPeeringConnectionResponse pStatus =
    CreateVPCPeeringConnectionResponse'
    { _cvpcrsVPCPeeringConnection = Nothing
    , _cvpcrsStatus = pStatus
    }

-- | Information about the VPC peering connection.
cvpcrsVPCPeeringConnection :: Lens' CreateVPCPeeringConnectionResponse (Maybe VPCPeeringConnection)
cvpcrsVPCPeeringConnection = lens _cvpcrsVPCPeeringConnection (\ s a -> s{_cvpcrsVPCPeeringConnection = a});

-- | FIXME: Undocumented member.
cvpcrsStatus :: Lens' CreateVPCPeeringConnectionResponse Int
cvpcrsStatus = lens _cvpcrsStatus (\ s a -> s{_cvpcrsStatus = a});
