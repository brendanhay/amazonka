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
-- Module      : Network.AWS.GameLift.DeleteVPCPeeringAuthorization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending VPC peering authorization for the specified VPC. If the authorization has already been used to create a peering connection, call 'DeleteVpcPeeringConnection' to remove the connection.
--
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
module Network.AWS.GameLift.DeleteVPCPeeringAuthorization
    (
    -- * Creating a Request
      deleteVPCPeeringAuthorization
    , DeleteVPCPeeringAuthorization
    -- * Request Lenses
    , dvpaGameLiftAWSAccountId
    , dvpaPeerVPCId

    -- * Destructuring the Response
    , deleteVPCPeeringAuthorizationResponse
    , DeleteVPCPeeringAuthorizationResponse
    -- * Response Lenses
    , dvparsResponseStatus
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
-- /See:/ 'deleteVPCPeeringAuthorization' smart constructor.
data DeleteVPCPeeringAuthorization = DeleteVPCPeeringAuthorization'
  { _dvpaGameLiftAWSAccountId :: !Text
  , _dvpaPeerVPCId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCPeeringAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpaGameLiftAWSAccountId' - Unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
--
-- * 'dvpaPeerVPCId' - Unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same region where your fleet is deployed. To get VPC information, including IDs, use the Virtual Private Cloud service tools, including the VPC Dashboard in the AWS Management Console.
deleteVPCPeeringAuthorization
    :: Text -- ^ 'dvpaGameLiftAWSAccountId'
    -> Text -- ^ 'dvpaPeerVPCId'
    -> DeleteVPCPeeringAuthorization
deleteVPCPeeringAuthorization pGameLiftAWSAccountId_ pPeerVPCId_ =
  DeleteVPCPeeringAuthorization'
    { _dvpaGameLiftAWSAccountId = pGameLiftAWSAccountId_
    , _dvpaPeerVPCId = pPeerVPCId_
    }


-- | Unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
dvpaGameLiftAWSAccountId :: Lens' DeleteVPCPeeringAuthorization Text
dvpaGameLiftAWSAccountId = lens _dvpaGameLiftAWSAccountId (\ s a -> s{_dvpaGameLiftAWSAccountId = a})

-- | Unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same region where your fleet is deployed. To get VPC information, including IDs, use the Virtual Private Cloud service tools, including the VPC Dashboard in the AWS Management Console.
dvpaPeerVPCId :: Lens' DeleteVPCPeeringAuthorization Text
dvpaPeerVPCId = lens _dvpaPeerVPCId (\ s a -> s{_dvpaPeerVPCId = a})

instance AWSRequest DeleteVPCPeeringAuthorization
         where
        type Rs DeleteVPCPeeringAuthorization =
             DeleteVPCPeeringAuthorizationResponse
        request = postJSON gameLift
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteVPCPeeringAuthorizationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteVPCPeeringAuthorization where

instance NFData DeleteVPCPeeringAuthorization where

instance ToHeaders DeleteVPCPeeringAuthorization
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DeleteVpcPeeringAuthorization" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteVPCPeeringAuthorization where
        toJSON DeleteVPCPeeringAuthorization'{..}
          = object
              (catMaybes
                 [Just
                    ("GameLiftAwsAccountId" .=
                       _dvpaGameLiftAWSAccountId),
                  Just ("PeerVpcId" .= _dvpaPeerVPCId)])

instance ToPath DeleteVPCPeeringAuthorization where
        toPath = const "/"

instance ToQuery DeleteVPCPeeringAuthorization where
        toQuery = const mempty

-- | /See:/ 'deleteVPCPeeringAuthorizationResponse' smart constructor.
newtype DeleteVPCPeeringAuthorizationResponse = DeleteVPCPeeringAuthorizationResponse'
  { _dvparsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCPeeringAuthorizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvparsResponseStatus' - -- | The response status code.
deleteVPCPeeringAuthorizationResponse
    :: Int -- ^ 'dvparsResponseStatus'
    -> DeleteVPCPeeringAuthorizationResponse
deleteVPCPeeringAuthorizationResponse pResponseStatus_ =
  DeleteVPCPeeringAuthorizationResponse'
    {_dvparsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dvparsResponseStatus :: Lens' DeleteVPCPeeringAuthorizationResponse Int
dvparsResponseStatus = lens _dvparsResponseStatus (\ s a -> s{_dvparsResponseStatus = a})

instance NFData DeleteVPCPeeringAuthorizationResponse
         where
