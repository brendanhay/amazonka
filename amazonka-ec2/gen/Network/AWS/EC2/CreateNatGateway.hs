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
-- Module      : Network.AWS.EC2.CreateNatGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a NAT gateway in the specified public subnet. This action creates a network interface in the specified subnet with a private IP address from the IP address range of the subnet. Internet-bound traffic from a private subnet can be routed to the NAT gateway, therefore enabling instances in the private subnet to connect to the internet. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html NAT Gateways> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
module Network.AWS.EC2.CreateNatGateway
    (
    -- * Creating a Request
      createNatGateway
    , CreateNatGateway
    -- * Request Lenses
    , cngClientToken
    , cngAllocationId
    , cngSubnetId

    -- * Destructuring the Response
    , createNatGatewayResponse
    , CreateNatGatewayResponse
    -- * Response Lenses
    , cngrsClientToken
    , cngrsNatGateway
    , cngrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateNatGateway.
--
--
--
-- /See:/ 'createNatGateway' smart constructor.
data CreateNatGateway = CreateNatGateway'
  { _cngClientToken  :: !(Maybe Text)
  , _cngAllocationId :: !Text
  , _cngSubnetId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNatGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cngClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> . Constraint: Maximum 64 ASCII characters.
--
-- * 'cngAllocationId' - The allocation ID of an Elastic IP address to associate with the NAT gateway. If the Elastic IP address is associated with another resource, you must first disassociate it.
--
-- * 'cngSubnetId' - The subnet in which to create the NAT gateway.
createNatGateway
    :: Text -- ^ 'cngAllocationId'
    -> Text -- ^ 'cngSubnetId'
    -> CreateNatGateway
createNatGateway pAllocationId_ pSubnetId_ =
  CreateNatGateway'
    { _cngClientToken = Nothing
    , _cngAllocationId = pAllocationId_
    , _cngSubnetId = pSubnetId_
    }


-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> . Constraint: Maximum 64 ASCII characters.
cngClientToken :: Lens' CreateNatGateway (Maybe Text)
cngClientToken = lens _cngClientToken (\ s a -> s{_cngClientToken = a})

-- | The allocation ID of an Elastic IP address to associate with the NAT gateway. If the Elastic IP address is associated with another resource, you must first disassociate it.
cngAllocationId :: Lens' CreateNatGateway Text
cngAllocationId = lens _cngAllocationId (\ s a -> s{_cngAllocationId = a})

-- | The subnet in which to create the NAT gateway.
cngSubnetId :: Lens' CreateNatGateway Text
cngSubnetId = lens _cngSubnetId (\ s a -> s{_cngSubnetId = a})

instance AWSRequest CreateNatGateway where
        type Rs CreateNatGateway = CreateNatGatewayResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateNatGatewayResponse' <$>
                   (x .@? "clientToken") <*> (x .@? "natGateway") <*>
                     (pure (fromEnum s)))

instance Hashable CreateNatGateway where

instance NFData CreateNatGateway where

instance ToHeaders CreateNatGateway where
        toHeaders = const mempty

instance ToPath CreateNatGateway where
        toPath = const "/"

instance ToQuery CreateNatGateway where
        toQuery CreateNatGateway'{..}
          = mconcat
              ["Action" =: ("CreateNatGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _cngClientToken,
               "AllocationId" =: _cngAllocationId,
               "SubnetId" =: _cngSubnetId]

-- | Contains the output of CreateNatGateway.
--
--
--
-- /See:/ 'createNatGatewayResponse' smart constructor.
data CreateNatGatewayResponse = CreateNatGatewayResponse'
  { _cngrsClientToken    :: !(Maybe Text)
  , _cngrsNatGateway     :: !(Maybe NatGateway)
  , _cngrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNatGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cngrsClientToken' - Unique, case-sensitive identifier to ensure the idempotency of the request. Only returned if a client token was provided in the request.
--
-- * 'cngrsNatGateway' - Information about the NAT gateway.
--
-- * 'cngrsResponseStatus' - -- | The response status code.
createNatGatewayResponse
    :: Int -- ^ 'cngrsResponseStatus'
    -> CreateNatGatewayResponse
createNatGatewayResponse pResponseStatus_ =
  CreateNatGatewayResponse'
    { _cngrsClientToken = Nothing
    , _cngrsNatGateway = Nothing
    , _cngrsResponseStatus = pResponseStatus_
    }


-- | Unique, case-sensitive identifier to ensure the idempotency of the request. Only returned if a client token was provided in the request.
cngrsClientToken :: Lens' CreateNatGatewayResponse (Maybe Text)
cngrsClientToken = lens _cngrsClientToken (\ s a -> s{_cngrsClientToken = a})

-- | Information about the NAT gateway.
cngrsNatGateway :: Lens' CreateNatGatewayResponse (Maybe NatGateway)
cngrsNatGateway = lens _cngrsNatGateway (\ s a -> s{_cngrsNatGateway = a})

-- | -- | The response status code.
cngrsResponseStatus :: Lens' CreateNatGatewayResponse Int
cngrsResponseStatus = lens _cngrsResponseStatus (\ s a -> s{_cngrsResponseStatus = a})

instance NFData CreateNatGatewayResponse where
