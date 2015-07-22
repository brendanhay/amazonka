{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AcceptVPCPeeringConnection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Accept a VPC peering connection request. To accept a request, the VPC
-- peering connection must be in the @pending-acceptance@ state, and you
-- must be the owner of the peer VPC. Use the
-- @DescribeVpcPeeringConnections@ request to view your outstanding VPC
-- peering connection requests.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AcceptVPCPeeringConnection.html>
module Network.AWS.EC2.AcceptVPCPeeringConnection
    (
    -- * Request
      AcceptVPCPeeringConnection
    -- ** Request constructor
    , acceptVPCPeeringConnection
    -- ** Request lenses
    , avpcrqVPCPeeringConnectionId
    , avpcrqDryRun

    -- * Response
    , AcceptVPCPeeringConnectionResponse
    -- ** Response constructor
    , acceptVPCPeeringConnectionResponse
    -- ** Response lenses
    , avpcrsVPCPeeringConnection
    , avpcrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'acceptVPCPeeringConnection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avpcrqVPCPeeringConnectionId'
--
-- * 'avpcrqDryRun'
data AcceptVPCPeeringConnection = AcceptVPCPeeringConnection'
    { _avpcrqVPCPeeringConnectionId :: !(Maybe Text)
    , _avpcrqDryRun                 :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AcceptVPCPeeringConnection' smart constructor.
acceptVPCPeeringConnection :: AcceptVPCPeeringConnection
acceptVPCPeeringConnection =
    AcceptVPCPeeringConnection'
    { _avpcrqVPCPeeringConnectionId = Nothing
    , _avpcrqDryRun = Nothing
    }

-- | The ID of the VPC peering connection.
avpcrqVPCPeeringConnectionId :: Lens' AcceptVPCPeeringConnection (Maybe Text)
avpcrqVPCPeeringConnectionId = lens _avpcrqVPCPeeringConnectionId (\ s a -> s{_avpcrqVPCPeeringConnectionId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
avpcrqDryRun :: Lens' AcceptVPCPeeringConnection (Maybe Bool)
avpcrqDryRun = lens _avpcrqDryRun (\ s a -> s{_avpcrqDryRun = a});

instance AWSRequest AcceptVPCPeeringConnection where
        type Sv AcceptVPCPeeringConnection = EC2
        type Rs AcceptVPCPeeringConnection =
             AcceptVPCPeeringConnectionResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 AcceptVPCPeeringConnectionResponse' <$>
                   (x .@? "vpcPeeringConnection") <*>
                     (pure (fromEnum s)))

instance ToHeaders AcceptVPCPeeringConnection where
        toHeaders = const mempty

instance ToPath AcceptVPCPeeringConnection where
        toPath = const "/"

instance ToQuery AcceptVPCPeeringConnection where
        toQuery AcceptVPCPeeringConnection'{..}
          = mconcat
              ["Action" =:
                 ("AcceptVPCPeeringConnection" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "VpcPeeringConnectionId" =:
                 _avpcrqVPCPeeringConnectionId,
               "DryRun" =: _avpcrqDryRun]

-- | /See:/ 'acceptVPCPeeringConnectionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avpcrsVPCPeeringConnection'
--
-- * 'avpcrsStatus'
data AcceptVPCPeeringConnectionResponse = AcceptVPCPeeringConnectionResponse'
    { _avpcrsVPCPeeringConnection :: !(Maybe VPCPeeringConnection)
    , _avpcrsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AcceptVPCPeeringConnectionResponse' smart constructor.
acceptVPCPeeringConnectionResponse :: Int -> AcceptVPCPeeringConnectionResponse
acceptVPCPeeringConnectionResponse pStatus =
    AcceptVPCPeeringConnectionResponse'
    { _avpcrsVPCPeeringConnection = Nothing
    , _avpcrsStatus = pStatus
    }

-- | Information about the VPC peering connection.
avpcrsVPCPeeringConnection :: Lens' AcceptVPCPeeringConnectionResponse (Maybe VPCPeeringConnection)
avpcrsVPCPeeringConnection = lens _avpcrsVPCPeeringConnection (\ s a -> s{_avpcrsVPCPeeringConnection = a});

-- | FIXME: Undocumented member.
avpcrsStatus :: Lens' AcceptVPCPeeringConnectionResponse Int
avpcrsStatus = lens _avpcrsStatus (\ s a -> s{_avpcrsStatus = a});
