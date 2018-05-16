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
-- Module      : Network.AWS.GameLift.DescribeVPCPeeringConnections
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information on VPC peering connections. Use this operation to get peering information for all fleets or for one specific fleet ID.
--
--
-- To retrieve connection information, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Specify a fleet ID or leave the parameter empty to retrieve all connection records. If successful, the retrieved information includes both active and pending connections. Active connections identify the IpV4 CIDR block that the VPC uses to connect.
--
-- VPC peering connection operations include:
--
--     * 'CreateVpcPeeringAuthorization'
--
--     * 'DescribeVpcPeeringAuthorizations'
--
--     * 'DeleteVpcPeeringAuthorization'
--
--     * 'CreateVpcPeeringConnection'
--
--     * 'DescribeVpcPeeringConnections'
--
--     * 'DeleteVpcPeeringConnection'
--
--
--
module Network.AWS.GameLift.DescribeVPCPeeringConnections
    (
    -- * Creating a Request
      describeVPCPeeringConnections
    , DescribeVPCPeeringConnections
    -- * Request Lenses
    , dvpcpcFleetId

    -- * Destructuring the Response
    , describeVPCPeeringConnectionsResponse
    , DescribeVPCPeeringConnectionsResponse
    -- * Response Lenses
    , dvpcpcrsVPCPeeringConnections
    , dvpcpcrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'describeVPCPeeringConnections' smart constructor.
newtype DescribeVPCPeeringConnections = DescribeVPCPeeringConnections'
  { _dvpcpcFleetId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCPeeringConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcpcFleetId' - Unique identifier for a fleet.
describeVPCPeeringConnections
    :: DescribeVPCPeeringConnections
describeVPCPeeringConnections =
  DescribeVPCPeeringConnections' {_dvpcpcFleetId = Nothing}


-- | Unique identifier for a fleet.
dvpcpcFleetId :: Lens' DescribeVPCPeeringConnections (Maybe Text)
dvpcpcFleetId = lens _dvpcpcFleetId (\ s a -> s{_dvpcpcFleetId = a})

instance AWSRequest DescribeVPCPeeringConnections
         where
        type Rs DescribeVPCPeeringConnections =
             DescribeVPCPeeringConnectionsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVPCPeeringConnectionsResponse' <$>
                   (x .?> "VpcPeeringConnections" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeVPCPeeringConnections where

instance NFData DescribeVPCPeeringConnections where

instance ToHeaders DescribeVPCPeeringConnections
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeVpcPeeringConnections" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeVPCPeeringConnections where
        toJSON DescribeVPCPeeringConnections'{..}
          = object
              (catMaybes [("FleetId" .=) <$> _dvpcpcFleetId])

instance ToPath DescribeVPCPeeringConnections where
        toPath = const "/"

instance ToQuery DescribeVPCPeeringConnections where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeVPCPeeringConnectionsResponse' smart constructor.
data DescribeVPCPeeringConnectionsResponse = DescribeVPCPeeringConnectionsResponse'
  { _dvpcpcrsVPCPeeringConnections :: !(Maybe [VPCPeeringConnection])
  , _dvpcpcrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCPeeringConnectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcpcrsVPCPeeringConnections' - Collection of VPC peering connection records that match the request.
--
-- * 'dvpcpcrsResponseStatus' - -- | The response status code.
describeVPCPeeringConnectionsResponse
    :: Int -- ^ 'dvpcpcrsResponseStatus'
    -> DescribeVPCPeeringConnectionsResponse
describeVPCPeeringConnectionsResponse pResponseStatus_ =
  DescribeVPCPeeringConnectionsResponse'
    { _dvpcpcrsVPCPeeringConnections = Nothing
    , _dvpcpcrsResponseStatus = pResponseStatus_
    }


-- | Collection of VPC peering connection records that match the request.
dvpcpcrsVPCPeeringConnections :: Lens' DescribeVPCPeeringConnectionsResponse [VPCPeeringConnection]
dvpcpcrsVPCPeeringConnections = lens _dvpcpcrsVPCPeeringConnections (\ s a -> s{_dvpcpcrsVPCPeeringConnections = a}) . _Default . _Coerce

-- | -- | The response status code.
dvpcpcrsResponseStatus :: Lens' DescribeVPCPeeringConnectionsResponse Int
dvpcpcrsResponseStatus = lens _dvpcpcrsResponseStatus (\ s a -> s{_dvpcpcrsResponseStatus = a})

instance NFData DescribeVPCPeeringConnectionsResponse
         where
