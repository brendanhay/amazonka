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
-- Module      : Network.AWS.GameLift.CreateVPCPeeringAuthorization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests authorization to create or delete a peer connection between the VPC for your Amazon GameLift fleet and a virtual private cloud (VPC) in your AWS account. VPC peering enables the game servers on your fleet to communicate directly with other AWS resources. Once you've received authorization, call 'CreateVpcPeeringConnection' to establish the peering connection. For more information, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
--
-- You can peer with VPCs that are owned by any AWS account you have access to, including the account that you use to manage your Amazon GameLift fleets. You cannot peer with VPCs that are in different regions.
--
-- To request authorization to create a connection, call this operation from the AWS account with the VPC that you want to peer to your Amazon GameLift fleet. For example, to enable your game servers to retrieve data from a DynamoDB table, use the account that manages that DynamoDB resource. Identify the following values: (1) The ID of the VPC that you want to peer with, and (2) the ID of the AWS account that you use to manage Amazon GameLift. If successful, VPC peering is authorized for the specified VPC.
--
-- To request authorization to delete a connection, call this operation from the AWS account with the VPC that is peered with your Amazon GameLift fleet. Identify the following values: (1) VPC ID that you want to delete the peering connection for, and (2) ID of the AWS account that you use to manage Amazon GameLift.
--
-- The authorization remains valid for 24 hours unless it is canceled by a call to 'DeleteVpcPeeringAuthorization' . You must create or delete the peering connection while the authorization is valid.
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
module Network.AWS.GameLift.CreateVPCPeeringAuthorization
    (
    -- * Creating a Request
      createVPCPeeringAuthorization
    , CreateVPCPeeringAuthorization
    -- * Request Lenses
    , cvpaGameLiftAWSAccountId
    , cvpaPeerVPCId

    -- * Destructuring the Response
    , createVPCPeeringAuthorizationResponse
    , CreateVPCPeeringAuthorizationResponse
    -- * Response Lenses
    , cvparsVPCPeeringAuthorization
    , cvparsResponseStatus
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
-- /See:/ 'createVPCPeeringAuthorization' smart constructor.
data CreateVPCPeeringAuthorization = CreateVPCPeeringAuthorization'
  { _cvpaGameLiftAWSAccountId :: !Text
  , _cvpaPeerVPCId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCPeeringAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvpaGameLiftAWSAccountId' - Unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
--
-- * 'cvpaPeerVPCId' - Unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same region where your fleet is deployed. To get VPC information, including IDs, use the Virtual Private Cloud service tools, including the VPC Dashboard in the AWS Management Console.
createVPCPeeringAuthorization
    :: Text -- ^ 'cvpaGameLiftAWSAccountId'
    -> Text -- ^ 'cvpaPeerVPCId'
    -> CreateVPCPeeringAuthorization
createVPCPeeringAuthorization pGameLiftAWSAccountId_ pPeerVPCId_ =
  CreateVPCPeeringAuthorization'
    { _cvpaGameLiftAWSAccountId = pGameLiftAWSAccountId_
    , _cvpaPeerVPCId = pPeerVPCId_
    }


-- | Unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
cvpaGameLiftAWSAccountId :: Lens' CreateVPCPeeringAuthorization Text
cvpaGameLiftAWSAccountId = lens _cvpaGameLiftAWSAccountId (\ s a -> s{_cvpaGameLiftAWSAccountId = a})

-- | Unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same region where your fleet is deployed. To get VPC information, including IDs, use the Virtual Private Cloud service tools, including the VPC Dashboard in the AWS Management Console.
cvpaPeerVPCId :: Lens' CreateVPCPeeringAuthorization Text
cvpaPeerVPCId = lens _cvpaPeerVPCId (\ s a -> s{_cvpaPeerVPCId = a})

instance AWSRequest CreateVPCPeeringAuthorization
         where
        type Rs CreateVPCPeeringAuthorization =
             CreateVPCPeeringAuthorizationResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreateVPCPeeringAuthorizationResponse' <$>
                   (x .?> "VpcPeeringAuthorization") <*>
                     (pure (fromEnum s)))

instance Hashable CreateVPCPeeringAuthorization where

instance NFData CreateVPCPeeringAuthorization where

instance ToHeaders CreateVPCPeeringAuthorization
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.CreateVpcPeeringAuthorization" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateVPCPeeringAuthorization where
        toJSON CreateVPCPeeringAuthorization'{..}
          = object
              (catMaybes
                 [Just
                    ("GameLiftAwsAccountId" .=
                       _cvpaGameLiftAWSAccountId),
                  Just ("PeerVpcId" .= _cvpaPeerVPCId)])

instance ToPath CreateVPCPeeringAuthorization where
        toPath = const "/"

instance ToQuery CreateVPCPeeringAuthorization where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'createVPCPeeringAuthorizationResponse' smart constructor.
data CreateVPCPeeringAuthorizationResponse = CreateVPCPeeringAuthorizationResponse'
  { _cvparsVPCPeeringAuthorization :: !(Maybe VPCPeeringAuthorization)
  , _cvparsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCPeeringAuthorizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvparsVPCPeeringAuthorization' - Details on the requested VPC peering authorization, including expiration.
--
-- * 'cvparsResponseStatus' - -- | The response status code.
createVPCPeeringAuthorizationResponse
    :: Int -- ^ 'cvparsResponseStatus'
    -> CreateVPCPeeringAuthorizationResponse
createVPCPeeringAuthorizationResponse pResponseStatus_ =
  CreateVPCPeeringAuthorizationResponse'
    { _cvparsVPCPeeringAuthorization = Nothing
    , _cvparsResponseStatus = pResponseStatus_
    }


-- | Details on the requested VPC peering authorization, including expiration.
cvparsVPCPeeringAuthorization :: Lens' CreateVPCPeeringAuthorizationResponse (Maybe VPCPeeringAuthorization)
cvparsVPCPeeringAuthorization = lens _cvparsVPCPeeringAuthorization (\ s a -> s{_cvparsVPCPeeringAuthorization = a})

-- | -- | The response status code.
cvparsResponseStatus :: Lens' CreateVPCPeeringAuthorizationResponse Int
cvparsResponseStatus = lens _cvparsResponseStatus (\ s a -> s{_cvparsResponseStatus = a})

instance NFData CreateVPCPeeringAuthorizationResponse
         where
