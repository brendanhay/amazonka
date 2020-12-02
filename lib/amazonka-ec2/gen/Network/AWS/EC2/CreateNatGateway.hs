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
-- Module      : Network.AWS.EC2.CreateNatGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a NAT gateway in the specified public subnet. This action creates a network interface in the specified subnet with a private IP address from the IP address range of the subnet. Internet-bound traffic from a private subnet can be routed to the NAT gateway, therefore enabling instances in the private subnet to connect to the internet. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html NAT Gateways> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateNatGateway
  ( -- * Creating a Request
    createNatGateway,
    CreateNatGateway,

    -- * Request Lenses
    cngClientToken,
    cngTagSpecifications,
    cngDryRun,
    cngAllocationId,
    cngSubnetId,

    -- * Destructuring the Response
    createNatGatewayResponse,
    CreateNatGatewayResponse,

    -- * Response Lenses
    cngrsClientToken,
    cngrsNatGateway,
    cngrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createNatGateway' smart constructor.
data CreateNatGateway = CreateNatGateway'
  { _cngClientToken ::
      !(Maybe Text),
    _cngTagSpecifications :: !(Maybe [TagSpecification]),
    _cngDryRun :: !(Maybe Bool),
    _cngAllocationId :: !Text,
    _cngSubnetId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateNatGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cngClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> . Constraint: Maximum 64 ASCII characters.
--
-- * 'cngTagSpecifications' - The tags to assign to the NAT gateway.
--
-- * 'cngDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cngAllocationId' - The allocation ID of an Elastic IP address to associate with the NAT gateway. If the Elastic IP address is associated with another resource, you must first disassociate it.
--
-- * 'cngSubnetId' - The subnet in which to create the NAT gateway.
createNatGateway ::
  -- | 'cngAllocationId'
  Text ->
  -- | 'cngSubnetId'
  Text ->
  CreateNatGateway
createNatGateway pAllocationId_ pSubnetId_ =
  CreateNatGateway'
    { _cngClientToken = Nothing,
      _cngTagSpecifications = Nothing,
      _cngDryRun = Nothing,
      _cngAllocationId = pAllocationId_,
      _cngSubnetId = pSubnetId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> . Constraint: Maximum 64 ASCII characters.
cngClientToken :: Lens' CreateNatGateway (Maybe Text)
cngClientToken = lens _cngClientToken (\s a -> s {_cngClientToken = a})

-- | The tags to assign to the NAT gateway.
cngTagSpecifications :: Lens' CreateNatGateway [TagSpecification]
cngTagSpecifications = lens _cngTagSpecifications (\s a -> s {_cngTagSpecifications = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cngDryRun :: Lens' CreateNatGateway (Maybe Bool)
cngDryRun = lens _cngDryRun (\s a -> s {_cngDryRun = a})

-- | The allocation ID of an Elastic IP address to associate with the NAT gateway. If the Elastic IP address is associated with another resource, you must first disassociate it.
cngAllocationId :: Lens' CreateNatGateway Text
cngAllocationId = lens _cngAllocationId (\s a -> s {_cngAllocationId = a})

-- | The subnet in which to create the NAT gateway.
cngSubnetId :: Lens' CreateNatGateway Text
cngSubnetId = lens _cngSubnetId (\s a -> s {_cngSubnetId = a})

instance AWSRequest CreateNatGateway where
  type Rs CreateNatGateway = CreateNatGatewayResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateNatGatewayResponse'
            <$> (x .@? "clientToken")
            <*> (x .@? "natGateway")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateNatGateway

instance NFData CreateNatGateway

instance ToHeaders CreateNatGateway where
  toHeaders = const mempty

instance ToPath CreateNatGateway where
  toPath = const "/"

instance ToQuery CreateNatGateway where
  toQuery CreateNatGateway' {..} =
    mconcat
      [ "Action" =: ("CreateNatGateway" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "ClientToken" =: _cngClientToken,
        toQuery (toQueryList "TagSpecification" <$> _cngTagSpecifications),
        "DryRun" =: _cngDryRun,
        "AllocationId" =: _cngAllocationId,
        "SubnetId" =: _cngSubnetId
      ]

-- | /See:/ 'createNatGatewayResponse' smart constructor.
data CreateNatGatewayResponse = CreateNatGatewayResponse'
  { _cngrsClientToken ::
      !(Maybe Text),
    _cngrsNatGateway :: !(Maybe NatGateway),
    _cngrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateNatGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cngrsClientToken' - Unique, case-sensitive identifier to ensure the idempotency of the request. Only returned if a client token was provided in the request.
--
-- * 'cngrsNatGateway' - Information about the NAT gateway.
--
-- * 'cngrsResponseStatus' - -- | The response status code.
createNatGatewayResponse ::
  -- | 'cngrsResponseStatus'
  Int ->
  CreateNatGatewayResponse
createNatGatewayResponse pResponseStatus_ =
  CreateNatGatewayResponse'
    { _cngrsClientToken = Nothing,
      _cngrsNatGateway = Nothing,
      _cngrsResponseStatus = pResponseStatus_
    }

-- | Unique, case-sensitive identifier to ensure the idempotency of the request. Only returned if a client token was provided in the request.
cngrsClientToken :: Lens' CreateNatGatewayResponse (Maybe Text)
cngrsClientToken = lens _cngrsClientToken (\s a -> s {_cngrsClientToken = a})

-- | Information about the NAT gateway.
cngrsNatGateway :: Lens' CreateNatGatewayResponse (Maybe NatGateway)
cngrsNatGateway = lens _cngrsNatGateway (\s a -> s {_cngrsNatGateway = a})

-- | -- | The response status code.
cngrsResponseStatus :: Lens' CreateNatGatewayResponse Int
cngrsResponseStatus = lens _cngrsResponseStatus (\s a -> s {_cngrsResponseStatus = a})

instance NFData CreateNatGatewayResponse
