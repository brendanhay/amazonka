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
-- Module      : Network.AWS.EC2.RejectVPCPeeringConnection
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a VPC peering connection request. The VPC peering connection must be in the 'pending-acceptance' state. Use the < DescribeVpcPeeringConnections> request to view your outstanding VPC peering connection requests. To delete an active VPC peering connection, or to delete a VPC peering connection request that you initiated, use < DeleteVpcPeeringConnection>.
module Network.AWS.EC2.RejectVPCPeeringConnection
    (
    -- * Creating a Request
      rejectVPCPeeringConnection
    , RejectVPCPeeringConnection
    -- * Request Lenses
    , rvpcDryRun
    , rvpcVPCPeeringConnectionId

    -- * Destructuring the Response
    , rejectVPCPeeringConnectionResponse
    , RejectVPCPeeringConnectionResponse
    -- * Response Lenses
    , rvpcrsReturn
    , rvpcrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for RejectVpcPeeringConnection.
--
-- /See:/ 'rejectVPCPeeringConnection' smart constructor.
data RejectVPCPeeringConnection = RejectVPCPeeringConnection'
    { _rvpcDryRun                 :: !(Maybe Bool)
    , _rvpcVPCPeeringConnectionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RejectVPCPeeringConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rvpcDryRun'
--
-- * 'rvpcVPCPeeringConnectionId'
rejectVPCPeeringConnection
    :: Text -- ^ 'rvpcVPCPeeringConnectionId'
    -> RejectVPCPeeringConnection
rejectVPCPeeringConnection pVPCPeeringConnectionId_ =
    RejectVPCPeeringConnection'
    { _rvpcDryRun = Nothing
    , _rvpcVPCPeeringConnectionId = pVPCPeeringConnectionId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
rvpcDryRun :: Lens' RejectVPCPeeringConnection (Maybe Bool)
rvpcDryRun = lens _rvpcDryRun (\ s a -> s{_rvpcDryRun = a});

-- | The ID of the VPC peering connection.
rvpcVPCPeeringConnectionId :: Lens' RejectVPCPeeringConnection Text
rvpcVPCPeeringConnectionId = lens _rvpcVPCPeeringConnectionId (\ s a -> s{_rvpcVPCPeeringConnectionId = a});

instance AWSRequest RejectVPCPeeringConnection where
        type Rs RejectVPCPeeringConnection =
             RejectVPCPeeringConnectionResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 RejectVPCPeeringConnectionResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable RejectVPCPeeringConnection

instance NFData RejectVPCPeeringConnection

instance ToHeaders RejectVPCPeeringConnection where
        toHeaders = const mempty

instance ToPath RejectVPCPeeringConnection where
        toPath = const "/"

instance ToQuery RejectVPCPeeringConnection where
        toQuery RejectVPCPeeringConnection'{..}
          = mconcat
              ["Action" =:
                 ("RejectVpcPeeringConnection" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "DryRun" =: _rvpcDryRun,
               "VpcPeeringConnectionId" =:
                 _rvpcVPCPeeringConnectionId]

-- | Contains the output of RejectVpcPeeringConnection.
--
-- /See:/ 'rejectVPCPeeringConnectionResponse' smart constructor.
data RejectVPCPeeringConnectionResponse = RejectVPCPeeringConnectionResponse'
    { _rvpcrsReturn         :: !(Maybe Bool)
    , _rvpcrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RejectVPCPeeringConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rvpcrsReturn'
--
-- * 'rvpcrsResponseStatus'
rejectVPCPeeringConnectionResponse
    :: Int -- ^ 'rvpcrsResponseStatus'
    -> RejectVPCPeeringConnectionResponse
rejectVPCPeeringConnectionResponse pResponseStatus_ =
    RejectVPCPeeringConnectionResponse'
    { _rvpcrsReturn = Nothing
    , _rvpcrsResponseStatus = pResponseStatus_
    }

-- | Returns 'true' if the request succeeds; otherwise, it returns an error.
rvpcrsReturn :: Lens' RejectVPCPeeringConnectionResponse (Maybe Bool)
rvpcrsReturn = lens _rvpcrsReturn (\ s a -> s{_rvpcrsReturn = a});

-- | The response status code.
rvpcrsResponseStatus :: Lens' RejectVPCPeeringConnectionResponse Int
rvpcrsResponseStatus = lens _rvpcrsResponseStatus (\ s a -> s{_rvpcrsResponseStatus = a});

instance NFData RejectVPCPeeringConnectionResponse
