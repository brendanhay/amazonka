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
-- Module      : Network.AWS.EC2.AcceptVPCPeeringConnection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accept a VPC peering connection request. To accept a request, the VPC peering connection must be in the @pending-acceptance@ state, and you must be the owner of the peer VPC. Use 'DescribeVpcPeeringConnections' to view your outstanding VPC peering connection requests.
--
--
-- For an inter-region VPC peering connection request, you must accept the VPC peering connection in the region of the accepter VPC.
--
module Network.AWS.EC2.AcceptVPCPeeringConnection
    (
    -- * Creating a Request
      acceptVPCPeeringConnection
    , AcceptVPCPeeringConnection
    -- * Request Lenses
    , avpcVPCPeeringConnectionId
    , avpcDryRun

    -- * Destructuring the Response
    , acceptVPCPeeringConnectionResponse
    , AcceptVPCPeeringConnectionResponse
    -- * Response Lenses
    , avpcrsVPCPeeringConnection
    , avpcrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'acceptVPCPeeringConnection' smart constructor.
data AcceptVPCPeeringConnection = AcceptVPCPeeringConnection'
  { _avpcVPCPeeringConnectionId :: !(Maybe Text)
  , _avpcDryRun                 :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptVPCPeeringConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avpcVPCPeeringConnectionId' - The ID of the VPC peering connection. You must specify this parameter in the request.
--
-- * 'avpcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
acceptVPCPeeringConnection
    :: AcceptVPCPeeringConnection
acceptVPCPeeringConnection =
  AcceptVPCPeeringConnection'
    {_avpcVPCPeeringConnectionId = Nothing, _avpcDryRun = Nothing}


-- | The ID of the VPC peering connection. You must specify this parameter in the request.
avpcVPCPeeringConnectionId :: Lens' AcceptVPCPeeringConnection (Maybe Text)
avpcVPCPeeringConnectionId = lens _avpcVPCPeeringConnectionId (\ s a -> s{_avpcVPCPeeringConnectionId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
avpcDryRun :: Lens' AcceptVPCPeeringConnection (Maybe Bool)
avpcDryRun = lens _avpcDryRun (\ s a -> s{_avpcDryRun = a})

instance AWSRequest AcceptVPCPeeringConnection where
        type Rs AcceptVPCPeeringConnection =
             AcceptVPCPeeringConnectionResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AcceptVPCPeeringConnectionResponse' <$>
                   (x .@? "vpcPeeringConnection") <*>
                     (pure (fromEnum s)))

instance Hashable AcceptVPCPeeringConnection where

instance NFData AcceptVPCPeeringConnection where

instance ToHeaders AcceptVPCPeeringConnection where
        toHeaders = const mempty

instance ToPath AcceptVPCPeeringConnection where
        toPath = const "/"

instance ToQuery AcceptVPCPeeringConnection where
        toQuery AcceptVPCPeeringConnection'{..}
          = mconcat
              ["Action" =:
                 ("AcceptVpcPeeringConnection" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "VpcPeeringConnectionId" =:
                 _avpcVPCPeeringConnectionId,
               "DryRun" =: _avpcDryRun]

-- | /See:/ 'acceptVPCPeeringConnectionResponse' smart constructor.
data AcceptVPCPeeringConnectionResponse = AcceptVPCPeeringConnectionResponse'
  { _avpcrsVPCPeeringConnection :: !(Maybe VPCPeeringConnection)
  , _avpcrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptVPCPeeringConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avpcrsVPCPeeringConnection' - Information about the VPC peering connection.
--
-- * 'avpcrsResponseStatus' - -- | The response status code.
acceptVPCPeeringConnectionResponse
    :: Int -- ^ 'avpcrsResponseStatus'
    -> AcceptVPCPeeringConnectionResponse
acceptVPCPeeringConnectionResponse pResponseStatus_ =
  AcceptVPCPeeringConnectionResponse'
    { _avpcrsVPCPeeringConnection = Nothing
    , _avpcrsResponseStatus = pResponseStatus_
    }


-- | Information about the VPC peering connection.
avpcrsVPCPeeringConnection :: Lens' AcceptVPCPeeringConnectionResponse (Maybe VPCPeeringConnection)
avpcrsVPCPeeringConnection = lens _avpcrsVPCPeeringConnection (\ s a -> s{_avpcrsVPCPeeringConnection = a})

-- | -- | The response status code.
avpcrsResponseStatus :: Lens' AcceptVPCPeeringConnectionResponse Int
avpcrsResponseStatus = lens _avpcrsResponseStatus (\ s a -> s{_avpcrsResponseStatus = a})

instance NFData AcceptVPCPeeringConnectionResponse
         where
