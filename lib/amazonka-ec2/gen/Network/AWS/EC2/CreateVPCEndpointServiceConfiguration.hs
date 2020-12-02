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
-- Module      : Network.AWS.EC2.CreateVPCEndpointServiceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC endpoint service configuration to which service consumers (AWS accounts, IAM users, and IAM roles) can connect.
--
--
-- To create an endpoint service configuration, you must first create one of the following for your service:
--
--     * A <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/introduction.html Network Load Balancer> . Service consumers connect to your service using an interface endpoint.
--
--     * A <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/introduction.html Gateway Load Balancer> . Service consumers connect to your service using a Gateway Load Balancer endpoint.
--
--
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html VPC Endpoint Services> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- If you set the private DNS name, you must prove that you own the private DNS domain name. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-services-dns-validation.html VPC Endpoint Service Private DNS Name Verification> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateVPCEndpointServiceConfiguration
  ( -- * Creating a Request
    createVPCEndpointServiceConfiguration,
    CreateVPCEndpointServiceConfiguration,

    -- * Request Lenses
    cvescNetworkLoadBalancerARNs,
    cvescClientToken,
    cvescTagSpecifications,
    cvescGatewayLoadBalancerARNs,
    cvescAcceptanceRequired,
    cvescPrivateDNSName,
    cvescDryRun,

    -- * Destructuring the Response
    createVPCEndpointServiceConfigurationResponse,
    CreateVPCEndpointServiceConfigurationResponse,

    -- * Response Lenses
    cvescrsClientToken,
    cvescrsServiceConfiguration,
    cvescrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createVPCEndpointServiceConfiguration' smart constructor.
data CreateVPCEndpointServiceConfiguration = CreateVPCEndpointServiceConfiguration'
  { _cvescNetworkLoadBalancerARNs ::
      !(Maybe [Text]),
    _cvescClientToken ::
      !(Maybe Text),
    _cvescTagSpecifications ::
      !( Maybe
           [TagSpecification]
       ),
    _cvescGatewayLoadBalancerARNs ::
      !(Maybe [Text]),
    _cvescAcceptanceRequired ::
      !(Maybe Bool),
    _cvescPrivateDNSName ::
      !(Maybe Text),
    _cvescDryRun ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVPCEndpointServiceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvescNetworkLoadBalancerARNs' - The Amazon Resource Names (ARNs) of one or more Network Load Balancers for your service.
--
-- * 'cvescClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'cvescTagSpecifications' - The tags to associate with the service.
--
-- * 'cvescGatewayLoadBalancerARNs' - The Amazon Resource Names (ARNs) of one or more Gateway Load Balancers.
--
-- * 'cvescAcceptanceRequired' - Indicates whether requests from service consumers to create an endpoint to your service must be accepted. To accept a request, use 'AcceptVpcEndpointConnections' .
--
-- * 'cvescPrivateDNSName' - (Interface endpoint configuration) The private DNS name to assign to the VPC endpoint service.
--
-- * 'cvescDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
createVPCEndpointServiceConfiguration ::
  CreateVPCEndpointServiceConfiguration
createVPCEndpointServiceConfiguration =
  CreateVPCEndpointServiceConfiguration'
    { _cvescNetworkLoadBalancerARNs =
        Nothing,
      _cvescClientToken = Nothing,
      _cvescTagSpecifications = Nothing,
      _cvescGatewayLoadBalancerARNs = Nothing,
      _cvescAcceptanceRequired = Nothing,
      _cvescPrivateDNSName = Nothing,
      _cvescDryRun = Nothing
    }

-- | The Amazon Resource Names (ARNs) of one or more Network Load Balancers for your service.
cvescNetworkLoadBalancerARNs :: Lens' CreateVPCEndpointServiceConfiguration [Text]
cvescNetworkLoadBalancerARNs = lens _cvescNetworkLoadBalancerARNs (\s a -> s {_cvescNetworkLoadBalancerARNs = a}) . _Default . _Coerce

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
cvescClientToken :: Lens' CreateVPCEndpointServiceConfiguration (Maybe Text)
cvescClientToken = lens _cvescClientToken (\s a -> s {_cvescClientToken = a})

-- | The tags to associate with the service.
cvescTagSpecifications :: Lens' CreateVPCEndpointServiceConfiguration [TagSpecification]
cvescTagSpecifications = lens _cvescTagSpecifications (\s a -> s {_cvescTagSpecifications = a}) . _Default . _Coerce

-- | The Amazon Resource Names (ARNs) of one or more Gateway Load Balancers.
cvescGatewayLoadBalancerARNs :: Lens' CreateVPCEndpointServiceConfiguration [Text]
cvescGatewayLoadBalancerARNs = lens _cvescGatewayLoadBalancerARNs (\s a -> s {_cvescGatewayLoadBalancerARNs = a}) . _Default . _Coerce

-- | Indicates whether requests from service consumers to create an endpoint to your service must be accepted. To accept a request, use 'AcceptVpcEndpointConnections' .
cvescAcceptanceRequired :: Lens' CreateVPCEndpointServiceConfiguration (Maybe Bool)
cvescAcceptanceRequired = lens _cvescAcceptanceRequired (\s a -> s {_cvescAcceptanceRequired = a})

-- | (Interface endpoint configuration) The private DNS name to assign to the VPC endpoint service.
cvescPrivateDNSName :: Lens' CreateVPCEndpointServiceConfiguration (Maybe Text)
cvescPrivateDNSName = lens _cvescPrivateDNSName (\s a -> s {_cvescPrivateDNSName = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cvescDryRun :: Lens' CreateVPCEndpointServiceConfiguration (Maybe Bool)
cvescDryRun = lens _cvescDryRun (\s a -> s {_cvescDryRun = a})

instance AWSRequest CreateVPCEndpointServiceConfiguration where
  type
    Rs CreateVPCEndpointServiceConfiguration =
      CreateVPCEndpointServiceConfigurationResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateVPCEndpointServiceConfigurationResponse'
            <$> (x .@? "clientToken")
            <*> (x .@? "serviceConfiguration")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateVPCEndpointServiceConfiguration

instance NFData CreateVPCEndpointServiceConfiguration

instance ToHeaders CreateVPCEndpointServiceConfiguration where
  toHeaders = const mempty

instance ToPath CreateVPCEndpointServiceConfiguration where
  toPath = const "/"

instance ToQuery CreateVPCEndpointServiceConfiguration where
  toQuery CreateVPCEndpointServiceConfiguration' {..} =
    mconcat
      [ "Action"
          =: ("CreateVpcEndpointServiceConfiguration" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          ( toQueryList "NetworkLoadBalancerArn"
              <$> _cvescNetworkLoadBalancerARNs
          ),
        "ClientToken" =: _cvescClientToken,
        toQuery
          (toQueryList "TagSpecification" <$> _cvescTagSpecifications),
        toQuery
          ( toQueryList "GatewayLoadBalancerArn"
              <$> _cvescGatewayLoadBalancerARNs
          ),
        "AcceptanceRequired" =: _cvescAcceptanceRequired,
        "PrivateDnsName" =: _cvescPrivateDNSName,
        "DryRun" =: _cvescDryRun
      ]

-- | /See:/ 'createVPCEndpointServiceConfigurationResponse' smart constructor.
data CreateVPCEndpointServiceConfigurationResponse = CreateVPCEndpointServiceConfigurationResponse'
  { _cvescrsClientToken ::
      !( Maybe
           Text
       ),
    _cvescrsServiceConfiguration ::
      !( Maybe
           ServiceConfiguration
       ),
    _cvescrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'CreateVPCEndpointServiceConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvescrsClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- * 'cvescrsServiceConfiguration' - Information about the service configuration.
--
-- * 'cvescrsResponseStatus' - -- | The response status code.
createVPCEndpointServiceConfigurationResponse ::
  -- | 'cvescrsResponseStatus'
  Int ->
  CreateVPCEndpointServiceConfigurationResponse
createVPCEndpointServiceConfigurationResponse pResponseStatus_ =
  CreateVPCEndpointServiceConfigurationResponse'
    { _cvescrsClientToken =
        Nothing,
      _cvescrsServiceConfiguration = Nothing,
      _cvescrsResponseStatus = pResponseStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
cvescrsClientToken :: Lens' CreateVPCEndpointServiceConfigurationResponse (Maybe Text)
cvescrsClientToken = lens _cvescrsClientToken (\s a -> s {_cvescrsClientToken = a})

-- | Information about the service configuration.
cvescrsServiceConfiguration :: Lens' CreateVPCEndpointServiceConfigurationResponse (Maybe ServiceConfiguration)
cvescrsServiceConfiguration = lens _cvescrsServiceConfiguration (\s a -> s {_cvescrsServiceConfiguration = a})

-- | -- | The response status code.
cvescrsResponseStatus :: Lens' CreateVPCEndpointServiceConfigurationResponse Int
cvescrsResponseStatus = lens _cvescrsResponseStatus (\s a -> s {_cvescrsResponseStatus = a})

instance NFData CreateVPCEndpointServiceConfigurationResponse
