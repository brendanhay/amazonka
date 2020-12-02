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
-- Module      : Network.AWS.EC2.ModifyVPCEndpointServiceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the attributes of your VPC endpoint service configuration. You can change the Network Load Balancers or Gateway Load Balancers for your service, and you can specify whether acceptance is required for requests to connect to your endpoint service through an interface VPC endpoint.
--
--
-- If you set or modify the private DNS name, you must prove that you own the private DNS domain name. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-services-dns-validation.html VPC Endpoint Service Private DNS Name Verification> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.ModifyVPCEndpointServiceConfiguration
  ( -- * Creating a Request
    modifyVPCEndpointServiceConfiguration,
    ModifyVPCEndpointServiceConfiguration,

    -- * Request Lenses
    mvescRemoveGatewayLoadBalancerARNs,
    mvescRemovePrivateDNSName,
    mvescAddGatewayLoadBalancerARNs,
    mvescRemoveNetworkLoadBalancerARNs,
    mvescAcceptanceRequired,
    mvescAddNetworkLoadBalancerARNs,
    mvescPrivateDNSName,
    mvescDryRun,
    mvescServiceId,

    -- * Destructuring the Response
    modifyVPCEndpointServiceConfigurationResponse,
    ModifyVPCEndpointServiceConfigurationResponse,

    -- * Response Lenses
    mvescrsReturn,
    mvescrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyVPCEndpointServiceConfiguration' smart constructor.
data ModifyVPCEndpointServiceConfiguration = ModifyVPCEndpointServiceConfiguration'
  { _mvescRemoveGatewayLoadBalancerARNs ::
      !(Maybe [Text]),
    _mvescRemovePrivateDNSName ::
      !(Maybe Bool),
    _mvescAddGatewayLoadBalancerARNs ::
      !(Maybe [Text]),
    _mvescRemoveNetworkLoadBalancerARNs ::
      !(Maybe [Text]),
    _mvescAcceptanceRequired ::
      !(Maybe Bool),
    _mvescAddNetworkLoadBalancerARNs ::
      !(Maybe [Text]),
    _mvescPrivateDNSName ::
      !(Maybe Text),
    _mvescDryRun ::
      !(Maybe Bool),
    _mvescServiceId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyVPCEndpointServiceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvescRemoveGatewayLoadBalancerARNs' - The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove from your service configuration.
--
-- * 'mvescRemovePrivateDNSName' - (Interface endpoint configuration) Removes the private DNS name of the endpoint service.
--
-- * 'mvescAddGatewayLoadBalancerARNs' - The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to your service configuration.
--
-- * 'mvescRemoveNetworkLoadBalancerARNs' - The Amazon Resource Names (ARNs) of Network Load Balancers to remove from your service configuration.
--
-- * 'mvescAcceptanceRequired' - Indicates whether requests to create an endpoint to your service must be accepted.
--
-- * 'mvescAddNetworkLoadBalancerARNs' - The Amazon Resource Names (ARNs) of Network Load Balancers to add to your service configuration.
--
-- * 'mvescPrivateDNSName' - (Interface endpoint configuration) The private DNS name to assign to the endpoint service.
--
-- * 'mvescDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mvescServiceId' - The ID of the service.
modifyVPCEndpointServiceConfiguration ::
  -- | 'mvescServiceId'
  Text ->
  ModifyVPCEndpointServiceConfiguration
modifyVPCEndpointServiceConfiguration pServiceId_ =
  ModifyVPCEndpointServiceConfiguration'
    { _mvescRemoveGatewayLoadBalancerARNs =
        Nothing,
      _mvescRemovePrivateDNSName = Nothing,
      _mvescAddGatewayLoadBalancerARNs = Nothing,
      _mvescRemoveNetworkLoadBalancerARNs = Nothing,
      _mvescAcceptanceRequired = Nothing,
      _mvescAddNetworkLoadBalancerARNs = Nothing,
      _mvescPrivateDNSName = Nothing,
      _mvescDryRun = Nothing,
      _mvescServiceId = pServiceId_
    }

-- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove from your service configuration.
mvescRemoveGatewayLoadBalancerARNs :: Lens' ModifyVPCEndpointServiceConfiguration [Text]
mvescRemoveGatewayLoadBalancerARNs = lens _mvescRemoveGatewayLoadBalancerARNs (\s a -> s {_mvescRemoveGatewayLoadBalancerARNs = a}) . _Default . _Coerce

-- | (Interface endpoint configuration) Removes the private DNS name of the endpoint service.
mvescRemovePrivateDNSName :: Lens' ModifyVPCEndpointServiceConfiguration (Maybe Bool)
mvescRemovePrivateDNSName = lens _mvescRemovePrivateDNSName (\s a -> s {_mvescRemovePrivateDNSName = a})

-- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to your service configuration.
mvescAddGatewayLoadBalancerARNs :: Lens' ModifyVPCEndpointServiceConfiguration [Text]
mvescAddGatewayLoadBalancerARNs = lens _mvescAddGatewayLoadBalancerARNs (\s a -> s {_mvescAddGatewayLoadBalancerARNs = a}) . _Default . _Coerce

-- | The Amazon Resource Names (ARNs) of Network Load Balancers to remove from your service configuration.
mvescRemoveNetworkLoadBalancerARNs :: Lens' ModifyVPCEndpointServiceConfiguration [Text]
mvescRemoveNetworkLoadBalancerARNs = lens _mvescRemoveNetworkLoadBalancerARNs (\s a -> s {_mvescRemoveNetworkLoadBalancerARNs = a}) . _Default . _Coerce

-- | Indicates whether requests to create an endpoint to your service must be accepted.
mvescAcceptanceRequired :: Lens' ModifyVPCEndpointServiceConfiguration (Maybe Bool)
mvescAcceptanceRequired = lens _mvescAcceptanceRequired (\s a -> s {_mvescAcceptanceRequired = a})

-- | The Amazon Resource Names (ARNs) of Network Load Balancers to add to your service configuration.
mvescAddNetworkLoadBalancerARNs :: Lens' ModifyVPCEndpointServiceConfiguration [Text]
mvescAddNetworkLoadBalancerARNs = lens _mvescAddNetworkLoadBalancerARNs (\s a -> s {_mvescAddNetworkLoadBalancerARNs = a}) . _Default . _Coerce

-- | (Interface endpoint configuration) The private DNS name to assign to the endpoint service.
mvescPrivateDNSName :: Lens' ModifyVPCEndpointServiceConfiguration (Maybe Text)
mvescPrivateDNSName = lens _mvescPrivateDNSName (\s a -> s {_mvescPrivateDNSName = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mvescDryRun :: Lens' ModifyVPCEndpointServiceConfiguration (Maybe Bool)
mvescDryRun = lens _mvescDryRun (\s a -> s {_mvescDryRun = a})

-- | The ID of the service.
mvescServiceId :: Lens' ModifyVPCEndpointServiceConfiguration Text
mvescServiceId = lens _mvescServiceId (\s a -> s {_mvescServiceId = a})

instance AWSRequest ModifyVPCEndpointServiceConfiguration where
  type
    Rs ModifyVPCEndpointServiceConfiguration =
      ModifyVPCEndpointServiceConfigurationResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyVPCEndpointServiceConfigurationResponse'
            <$> (x .@? "return") <*> (pure (fromEnum s))
      )

instance Hashable ModifyVPCEndpointServiceConfiguration

instance NFData ModifyVPCEndpointServiceConfiguration

instance ToHeaders ModifyVPCEndpointServiceConfiguration where
  toHeaders = const mempty

instance ToPath ModifyVPCEndpointServiceConfiguration where
  toPath = const "/"

instance ToQuery ModifyVPCEndpointServiceConfiguration where
  toQuery ModifyVPCEndpointServiceConfiguration' {..} =
    mconcat
      [ "Action"
          =: ("ModifyVpcEndpointServiceConfiguration" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          ( toQueryList "RemoveGatewayLoadBalancerArn"
              <$> _mvescRemoveGatewayLoadBalancerARNs
          ),
        "RemovePrivateDnsName" =: _mvescRemovePrivateDNSName,
        toQuery
          ( toQueryList "AddGatewayLoadBalancerArn"
              <$> _mvescAddGatewayLoadBalancerARNs
          ),
        toQuery
          ( toQueryList "RemoveNetworkLoadBalancerArn"
              <$> _mvescRemoveNetworkLoadBalancerARNs
          ),
        "AcceptanceRequired" =: _mvescAcceptanceRequired,
        toQuery
          ( toQueryList "AddNetworkLoadBalancerArn"
              <$> _mvescAddNetworkLoadBalancerARNs
          ),
        "PrivateDnsName" =: _mvescPrivateDNSName,
        "DryRun" =: _mvescDryRun,
        "ServiceId" =: _mvescServiceId
      ]

-- | /See:/ 'modifyVPCEndpointServiceConfigurationResponse' smart constructor.
data ModifyVPCEndpointServiceConfigurationResponse = ModifyVPCEndpointServiceConfigurationResponse'
  { _mvescrsReturn ::
      !( Maybe
           Bool
       ),
    _mvescrsResponseStatus ::
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

-- | Creates a value of 'ModifyVPCEndpointServiceConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvescrsReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'mvescrsResponseStatus' - -- | The response status code.
modifyVPCEndpointServiceConfigurationResponse ::
  -- | 'mvescrsResponseStatus'
  Int ->
  ModifyVPCEndpointServiceConfigurationResponse
modifyVPCEndpointServiceConfigurationResponse pResponseStatus_ =
  ModifyVPCEndpointServiceConfigurationResponse'
    { _mvescrsReturn =
        Nothing,
      _mvescrsResponseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
mvescrsReturn :: Lens' ModifyVPCEndpointServiceConfigurationResponse (Maybe Bool)
mvescrsReturn = lens _mvescrsReturn (\s a -> s {_mvescrsReturn = a})

-- | -- | The response status code.
mvescrsResponseStatus :: Lens' ModifyVPCEndpointServiceConfigurationResponse Int
mvescrsResponseStatus = lens _mvescrsResponseStatus (\s a -> s {_mvescrsResponseStatus = a})

instance NFData ModifyVPCEndpointServiceConfigurationResponse
