{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RejectVPCPeeringConnection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Rejects a VPC peering connection request. The VPC peering connection
-- must be in the @pending-acceptance@ state. Use the
-- DescribeVpcPeeringConnections request to view your outstanding VPC
-- peering connection requests. To delete an active VPC peering connection,
-- or to delete a VPC peering connection request that you initiated, use
-- DeleteVpcPeeringConnection.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RejectVPCPeeringConnection.html>
module Network.AWS.EC2.RejectVPCPeeringConnection
    (
    -- * Request
      RejectVPCPeeringConnection
    -- ** Request constructor
    , rejectVPCPeeringConnection
    -- ** Request lenses
    , rvpcDryRun
    , rvpcVPCPeeringConnectionId

    -- * Response
    , RejectVPCPeeringConnectionResponse
    -- ** Response constructor
    , rejectVPCPeeringConnectionResponse
    -- ** Response lenses
    , rvpcrsReturn
    , rvpcrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'rejectVPCPeeringConnection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rvpcDryRun'
--
-- * 'rvpcVPCPeeringConnectionId'
data RejectVPCPeeringConnection = RejectVPCPeeringConnection'
    { _rvpcDryRun                 :: !(Maybe Bool)
    , _rvpcVPCPeeringConnectionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RejectVPCPeeringConnection' smart constructor.
rejectVPCPeeringConnection :: Text -> RejectVPCPeeringConnection
rejectVPCPeeringConnection pVPCPeeringConnectionId_ =
    RejectVPCPeeringConnection'
    { _rvpcDryRun = Nothing
    , _rvpcVPCPeeringConnectionId = pVPCPeeringConnectionId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rvpcDryRun :: Lens' RejectVPCPeeringConnection (Maybe Bool)
rvpcDryRun = lens _rvpcDryRun (\ s a -> s{_rvpcDryRun = a});

-- | The ID of the VPC peering connection.
rvpcVPCPeeringConnectionId :: Lens' RejectVPCPeeringConnection Text
rvpcVPCPeeringConnectionId = lens _rvpcVPCPeeringConnectionId (\ s a -> s{_rvpcVPCPeeringConnectionId = a});

instance AWSRequest RejectVPCPeeringConnection where
        type Sv RejectVPCPeeringConnection = EC2
        type Rs RejectVPCPeeringConnection =
             RejectVPCPeeringConnectionResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 RejectVPCPeeringConnectionResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance ToHeaders RejectVPCPeeringConnection where
        toHeaders = const mempty

instance ToPath RejectVPCPeeringConnection where
        toPath = const "/"

instance ToQuery RejectVPCPeeringConnection where
        toQuery RejectVPCPeeringConnection'{..}
          = mconcat
              ["Action" =:
                 ("RejectVPCPeeringConnection" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _rvpcDryRun,
               "VpcPeeringConnectionId" =:
                 _rvpcVPCPeeringConnectionId]

-- | /See:/ 'rejectVPCPeeringConnectionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rvpcrsReturn'
--
-- * 'rvpcrsStatus'
data RejectVPCPeeringConnectionResponse = RejectVPCPeeringConnectionResponse'
    { _rvpcrsReturn :: !(Maybe Bool)
    , _rvpcrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RejectVPCPeeringConnectionResponse' smart constructor.
rejectVPCPeeringConnectionResponse :: Int -> RejectVPCPeeringConnectionResponse
rejectVPCPeeringConnectionResponse pStatus_ =
    RejectVPCPeeringConnectionResponse'
    { _rvpcrsReturn = Nothing
    , _rvpcrsStatus = pStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
rvpcrsReturn :: Lens' RejectVPCPeeringConnectionResponse (Maybe Bool)
rvpcrsReturn = lens _rvpcrsReturn (\ s a -> s{_rvpcrsReturn = a});

-- | FIXME: Undocumented member.
rvpcrsStatus :: Lens' RejectVPCPeeringConnectionResponse Int
rvpcrsStatus = lens _rvpcrsStatus (\ s a -> s{_rvpcrsStatus = a});
