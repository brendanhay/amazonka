{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateVPCPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Establishes a VPC peering connection between a virtual private cloud (VPC) in an AWS account with the VPC for your Amazon GameLift fleet. VPC peering enables the game servers on your fleet to communicate directly with other AWS resources. You can peer with VPCs in any AWS account that you have access to, including the account that you use to manage your Amazon GameLift fleets. You cannot peer with VPCs that are in different Regions. For more information, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
--
-- Before calling this operation to establish the peering connection, you first need to call 'CreateVpcPeeringAuthorization' and identify the VPC you want to peer with. Once the authorization for the specified VPC is issued, you have 24 hours to establish the connection. These two operations handle all tasks necessary to peer the two VPCs, including acceptance, updating routing tables, etc.
--
-- To establish the connection, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Identify the following values: (1) The ID of the fleet you want to be enable a VPC peering connection for; (2) The AWS account with the VPC that you want to peer with; and (3) The ID of the VPC you want to peer with. This operation is asynchronous. If successful, a 'VpcPeeringConnection' request is created. You can use continuous polling to track the request's status using 'DescribeVpcPeeringConnections' , or by monitoring fleet events for success or failure using 'DescribeFleetEvents' .
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
module Network.AWS.GameLift.CreateVPCPeeringConnection
  ( -- * Creating a Request
    createVPCPeeringConnection,
    CreateVPCPeeringConnection,

    -- * Request Lenses
    cvpcFleetId,
    cvpcPeerVPCAWSAccountId,
    cvpcPeerVPCId,

    -- * Destructuring the Response
    createVPCPeeringConnectionResponse,
    CreateVPCPeeringConnectionResponse,

    -- * Response Lenses
    cvpcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'createVPCPeeringConnection' smart constructor.
data CreateVPCPeeringConnection = CreateVPCPeeringConnection'
  { _cvpcFleetId ::
      !Text,
    _cvpcPeerVPCAWSAccountId :: !Text,
    _cvpcPeerVPCId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVPCPeeringConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvpcFleetId' - A unique identifier for a fleet. You can use either the fleet ID or ARN value. This tells Amazon GameLift which GameLift VPC to peer with.
--
-- * 'cvpcPeerVPCAWSAccountId' - A unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your Account ID in the AWS Management Console under account settings.
--
-- * 'cvpcPeerVPCId' - A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
createVPCPeeringConnection ::
  -- | 'cvpcFleetId'
  Text ->
  -- | 'cvpcPeerVPCAWSAccountId'
  Text ->
  -- | 'cvpcPeerVPCId'
  Text ->
  CreateVPCPeeringConnection
createVPCPeeringConnection
  pFleetId_
  pPeerVPCAWSAccountId_
  pPeerVPCId_ =
    CreateVPCPeeringConnection'
      { _cvpcFleetId = pFleetId_,
        _cvpcPeerVPCAWSAccountId = pPeerVPCAWSAccountId_,
        _cvpcPeerVPCId = pPeerVPCId_
      }

-- | A unique identifier for a fleet. You can use either the fleet ID or ARN value. This tells Amazon GameLift which GameLift VPC to peer with.
cvpcFleetId :: Lens' CreateVPCPeeringConnection Text
cvpcFleetId = lens _cvpcFleetId (\s a -> s {_cvpcFleetId = a})

-- | A unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your Account ID in the AWS Management Console under account settings.
cvpcPeerVPCAWSAccountId :: Lens' CreateVPCPeeringConnection Text
cvpcPeerVPCAWSAccountId = lens _cvpcPeerVPCAWSAccountId (\s a -> s {_cvpcPeerVPCAWSAccountId = a})

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
cvpcPeerVPCId :: Lens' CreateVPCPeeringConnection Text
cvpcPeerVPCId = lens _cvpcPeerVPCId (\s a -> s {_cvpcPeerVPCId = a})

instance AWSRequest CreateVPCPeeringConnection where
  type
    Rs CreateVPCPeeringConnection =
      CreateVPCPeeringConnectionResponse
  request = postJSON gameLift
  response =
    receiveEmpty
      ( \s h x ->
          CreateVPCPeeringConnectionResponse' <$> (pure (fromEnum s))
      )

instance Hashable CreateVPCPeeringConnection

instance NFData CreateVPCPeeringConnection

instance ToHeaders CreateVPCPeeringConnection where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.CreateVpcPeeringConnection" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateVPCPeeringConnection where
  toJSON CreateVPCPeeringConnection' {..} =
    object
      ( catMaybes
          [ Just ("FleetId" .= _cvpcFleetId),
            Just ("PeerVpcAwsAccountId" .= _cvpcPeerVPCAWSAccountId),
            Just ("PeerVpcId" .= _cvpcPeerVPCId)
          ]
      )

instance ToPath CreateVPCPeeringConnection where
  toPath = const "/"

instance ToQuery CreateVPCPeeringConnection where
  toQuery = const mempty

-- | /See:/ 'createVPCPeeringConnectionResponse' smart constructor.
newtype CreateVPCPeeringConnectionResponse = CreateVPCPeeringConnectionResponse'
  { _cvpcrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVPCPeeringConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvpcrsResponseStatus' - -- | The response status code.
createVPCPeeringConnectionResponse ::
  -- | 'cvpcrsResponseStatus'
  Int ->
  CreateVPCPeeringConnectionResponse
createVPCPeeringConnectionResponse pResponseStatus_ =
  CreateVPCPeeringConnectionResponse'
    { _cvpcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
cvpcrsResponseStatus :: Lens' CreateVPCPeeringConnectionResponse Int
cvpcrsResponseStatus = lens _cvpcrsResponseStatus (\s a -> s {_cvpcrsResponseStatus = a})

instance NFData CreateVPCPeeringConnectionResponse
