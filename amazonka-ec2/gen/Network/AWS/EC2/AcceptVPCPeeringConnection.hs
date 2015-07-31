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
    , avpcVPCPeeringConnectionId
    , avpcDryRun

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
-- * 'avpcVPCPeeringConnectionId'
--
-- * 'avpcDryRun'
data AcceptVPCPeeringConnection = AcceptVPCPeeringConnection'
    { _avpcVPCPeeringConnectionId :: !(Maybe Text)
    , _avpcDryRun                 :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AcceptVPCPeeringConnection' smart constructor.
acceptVPCPeeringConnection :: AcceptVPCPeeringConnection
acceptVPCPeeringConnection =
    AcceptVPCPeeringConnection'
    { _avpcVPCPeeringConnectionId = Nothing
    , _avpcDryRun = Nothing
    }

-- | The ID of the VPC peering connection.
avpcVPCPeeringConnectionId :: Lens' AcceptVPCPeeringConnection (Maybe Text)
avpcVPCPeeringConnectionId = lens _avpcVPCPeeringConnectionId (\ s a -> s{_avpcVPCPeeringConnectionId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
avpcDryRun :: Lens' AcceptVPCPeeringConnection (Maybe Bool)
avpcDryRun = lens _avpcDryRun (\ s a -> s{_avpcDryRun = a});

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
                 ("AcceptVpcPeeringConnection" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "VpcPeeringConnectionId" =:
                 _avpcVPCPeeringConnectionId,
               "DryRun" =: _avpcDryRun]

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
acceptVPCPeeringConnectionResponse pStatus_ =
    AcceptVPCPeeringConnectionResponse'
    { _avpcrsVPCPeeringConnection = Nothing
    , _avpcrsStatus = pStatus_
    }

-- | Information about the VPC peering connection.
avpcrsVPCPeeringConnection :: Lens' AcceptVPCPeeringConnectionResponse (Maybe VPCPeeringConnection)
avpcrsVPCPeeringConnection = lens _avpcrsVPCPeeringConnection (\ s a -> s{_avpcrsVPCPeeringConnection = a});

-- | FIXME: Undocumented member.
avpcrsStatus :: Lens' AcceptVPCPeeringConnectionResponse Int
avpcrsStatus = lens _avpcrsStatus (\ s a -> s{_avpcrsStatus = a});
