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
-- Module      : Network.AWS.EC2.CreateVPCEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC endpoint for a specified service. An endpoint enables you to create a private connection between your VPC and the service. The service may be provided by AWS, an AWS Marketplace Partner, or another AWS account. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-endpoints.html VPC Endpoints> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
-- A @gateway@ endpoint serves as a target for a route in your route table for traffic destined for the AWS service. You can specify an endpoint policy to attach to the endpoint, which will control access to the service from your VPC. You can also specify the VPC route tables that use the endpoint.
--
-- An @interface@ endpoint is a network interface in your subnet that serves as an endpoint for communicating with the specified service. You can specify the subnets in which to create an endpoint, and the security groups to associate with the endpoint network interface.
--
-- A @GatewayLoadBalancer@ endpoint is a network interface in your subnet that serves an endpoint for communicating with a Gateway Load Balancer that you've configured as a VPC endpoint service.
--
-- Use 'DescribeVpcEndpointServices' to get a list of supported services.
module Network.AWS.EC2.CreateVPCEndpoint
  ( -- * Creating a Request
    createVPCEndpoint,
    CreateVPCEndpoint,

    -- * Request Lenses
    cvpcePolicyDocument,
    cvpceSecurityGroupIds,
    cvpceClientToken,
    cvpceSubnetIds,
    cvpceVPCEndpointType,
    cvpcePrivateDNSEnabled,
    cvpceTagSpecifications,
    cvpceDryRun,
    cvpceRouteTableIds,
    cvpceVPCId,
    cvpceServiceName,

    -- * Destructuring the Response
    createVPCEndpointResponse,
    CreateVPCEndpointResponse,

    -- * Response Lenses
    cversClientToken,
    cversVPCEndpoint,
    cversResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateVpcEndpoint.
--
--
--
-- /See:/ 'createVPCEndpoint' smart constructor.
data CreateVPCEndpoint = CreateVPCEndpoint'
  { _cvpcePolicyDocument ::
      !(Maybe Text),
    _cvpceSecurityGroupIds :: !(Maybe [Text]),
    _cvpceClientToken :: !(Maybe Text),
    _cvpceSubnetIds :: !(Maybe [Text]),
    _cvpceVPCEndpointType :: !(Maybe VPCEndpointType),
    _cvpcePrivateDNSEnabled :: !(Maybe Bool),
    _cvpceTagSpecifications :: !(Maybe [TagSpecification]),
    _cvpceDryRun :: !(Maybe Bool),
    _cvpceRouteTableIds :: !(Maybe [Text]),
    _cvpceVPCId :: !Text,
    _cvpceServiceName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVPCEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvpcePolicyDocument' - (Interface and gateway endpoints) A policy to attach to the endpoint that controls access to the service. The policy must be in valid JSON format. If this parameter is not specified, we attach a default policy that allows full access to the service.
--
-- * 'cvpceSecurityGroupIds' - (Interface endpoint) The ID of one or more security groups to associate with the endpoint network interface.
--
-- * 'cvpceClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'cvpceSubnetIds' - (Interface and Gateway Load Balancer endpoints) The ID of one or more subnets in which to create an endpoint network interface. For a Gateway Load Balancer endpoint, you can specify one subnet only.
--
-- * 'cvpceVPCEndpointType' - The type of endpoint. Default: Gateway
--
-- * 'cvpcePrivateDNSEnabled' - (Interface endpoint) Indicates whether to associate a private hosted zone with the specified VPC. The private hosted zone contains a record set for the default public DNS name for the service for the Region (for example, @kinesis.us-east-1.amazonaws.com@ ), which resolves to the private IP addresses of the endpoint network interfaces in the VPC. This enables you to make requests to the default public DNS name for the service instead of the public DNS names that are automatically generated by the VPC endpoint service. To use a private hosted zone, you must set the following VPC attributes to @true@ : @enableDnsHostnames@ and @enableDnsSupport@ . Use 'ModifyVpcAttribute' to set the VPC attributes. Default: @true@
--
-- * 'cvpceTagSpecifications' - The tags to associate with the endpoint.
--
-- * 'cvpceDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cvpceRouteTableIds' - (Gateway endpoint) One or more route table IDs.
--
-- * 'cvpceVPCId' - The ID of the VPC in which the endpoint will be used.
--
-- * 'cvpceServiceName' - The service name. To get a list of available services, use the 'DescribeVpcEndpointServices' request, or get the name from the service provider.
createVPCEndpoint ::
  -- | 'cvpceVPCId'
  Text ->
  -- | 'cvpceServiceName'
  Text ->
  CreateVPCEndpoint
createVPCEndpoint pVPCId_ pServiceName_ =
  CreateVPCEndpoint'
    { _cvpcePolicyDocument = Nothing,
      _cvpceSecurityGroupIds = Nothing,
      _cvpceClientToken = Nothing,
      _cvpceSubnetIds = Nothing,
      _cvpceVPCEndpointType = Nothing,
      _cvpcePrivateDNSEnabled = Nothing,
      _cvpceTagSpecifications = Nothing,
      _cvpceDryRun = Nothing,
      _cvpceRouteTableIds = Nothing,
      _cvpceVPCId = pVPCId_,
      _cvpceServiceName = pServiceName_
    }

-- | (Interface and gateway endpoints) A policy to attach to the endpoint that controls access to the service. The policy must be in valid JSON format. If this parameter is not specified, we attach a default policy that allows full access to the service.
cvpcePolicyDocument :: Lens' CreateVPCEndpoint (Maybe Text)
cvpcePolicyDocument = lens _cvpcePolicyDocument (\s a -> s {_cvpcePolicyDocument = a})

-- | (Interface endpoint) The ID of one or more security groups to associate with the endpoint network interface.
cvpceSecurityGroupIds :: Lens' CreateVPCEndpoint [Text]
cvpceSecurityGroupIds = lens _cvpceSecurityGroupIds (\s a -> s {_cvpceSecurityGroupIds = a}) . _Default . _Coerce

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
cvpceClientToken :: Lens' CreateVPCEndpoint (Maybe Text)
cvpceClientToken = lens _cvpceClientToken (\s a -> s {_cvpceClientToken = a})

-- | (Interface and Gateway Load Balancer endpoints) The ID of one or more subnets in which to create an endpoint network interface. For a Gateway Load Balancer endpoint, you can specify one subnet only.
cvpceSubnetIds :: Lens' CreateVPCEndpoint [Text]
cvpceSubnetIds = lens _cvpceSubnetIds (\s a -> s {_cvpceSubnetIds = a}) . _Default . _Coerce

-- | The type of endpoint. Default: Gateway
cvpceVPCEndpointType :: Lens' CreateVPCEndpoint (Maybe VPCEndpointType)
cvpceVPCEndpointType = lens _cvpceVPCEndpointType (\s a -> s {_cvpceVPCEndpointType = a})

-- | (Interface endpoint) Indicates whether to associate a private hosted zone with the specified VPC. The private hosted zone contains a record set for the default public DNS name for the service for the Region (for example, @kinesis.us-east-1.amazonaws.com@ ), which resolves to the private IP addresses of the endpoint network interfaces in the VPC. This enables you to make requests to the default public DNS name for the service instead of the public DNS names that are automatically generated by the VPC endpoint service. To use a private hosted zone, you must set the following VPC attributes to @true@ : @enableDnsHostnames@ and @enableDnsSupport@ . Use 'ModifyVpcAttribute' to set the VPC attributes. Default: @true@
cvpcePrivateDNSEnabled :: Lens' CreateVPCEndpoint (Maybe Bool)
cvpcePrivateDNSEnabled = lens _cvpcePrivateDNSEnabled (\s a -> s {_cvpcePrivateDNSEnabled = a})

-- | The tags to associate with the endpoint.
cvpceTagSpecifications :: Lens' CreateVPCEndpoint [TagSpecification]
cvpceTagSpecifications = lens _cvpceTagSpecifications (\s a -> s {_cvpceTagSpecifications = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cvpceDryRun :: Lens' CreateVPCEndpoint (Maybe Bool)
cvpceDryRun = lens _cvpceDryRun (\s a -> s {_cvpceDryRun = a})

-- | (Gateway endpoint) One or more route table IDs.
cvpceRouteTableIds :: Lens' CreateVPCEndpoint [Text]
cvpceRouteTableIds = lens _cvpceRouteTableIds (\s a -> s {_cvpceRouteTableIds = a}) . _Default . _Coerce

-- | The ID of the VPC in which the endpoint will be used.
cvpceVPCId :: Lens' CreateVPCEndpoint Text
cvpceVPCId = lens _cvpceVPCId (\s a -> s {_cvpceVPCId = a})

-- | The service name. To get a list of available services, use the 'DescribeVpcEndpointServices' request, or get the name from the service provider.
cvpceServiceName :: Lens' CreateVPCEndpoint Text
cvpceServiceName = lens _cvpceServiceName (\s a -> s {_cvpceServiceName = a})

instance AWSRequest CreateVPCEndpoint where
  type Rs CreateVPCEndpoint = CreateVPCEndpointResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateVPCEndpointResponse'
            <$> (x .@? "clientToken")
            <*> (x .@? "vpcEndpoint")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateVPCEndpoint

instance NFData CreateVPCEndpoint

instance ToHeaders CreateVPCEndpoint where
  toHeaders = const mempty

instance ToPath CreateVPCEndpoint where
  toPath = const "/"

instance ToQuery CreateVPCEndpoint where
  toQuery CreateVPCEndpoint' {..} =
    mconcat
      [ "Action" =: ("CreateVpcEndpoint" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "PolicyDocument" =: _cvpcePolicyDocument,
        toQuery (toQueryList "SecurityGroupId" <$> _cvpceSecurityGroupIds),
        "ClientToken" =: _cvpceClientToken,
        toQuery (toQueryList "SubnetId" <$> _cvpceSubnetIds),
        "VpcEndpointType" =: _cvpceVPCEndpointType,
        "PrivateDnsEnabled" =: _cvpcePrivateDNSEnabled,
        toQuery
          (toQueryList "TagSpecification" <$> _cvpceTagSpecifications),
        "DryRun" =: _cvpceDryRun,
        toQuery (toQueryList "RouteTableId" <$> _cvpceRouteTableIds),
        "VpcId" =: _cvpceVPCId,
        "ServiceName" =: _cvpceServiceName
      ]

-- | Contains the output of CreateVpcEndpoint.
--
--
--
-- /See:/ 'createVPCEndpointResponse' smart constructor.
data CreateVPCEndpointResponse = CreateVPCEndpointResponse'
  { _cversClientToken ::
      !(Maybe Text),
    _cversVPCEndpoint ::
      !(Maybe VPCEndpoint),
    _cversResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVPCEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cversClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- * 'cversVPCEndpoint' - Information about the endpoint.
--
-- * 'cversResponseStatus' - -- | The response status code.
createVPCEndpointResponse ::
  -- | 'cversResponseStatus'
  Int ->
  CreateVPCEndpointResponse
createVPCEndpointResponse pResponseStatus_ =
  CreateVPCEndpointResponse'
    { _cversClientToken = Nothing,
      _cversVPCEndpoint = Nothing,
      _cversResponseStatus = pResponseStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
cversClientToken :: Lens' CreateVPCEndpointResponse (Maybe Text)
cversClientToken = lens _cversClientToken (\s a -> s {_cversClientToken = a})

-- | Information about the endpoint.
cversVPCEndpoint :: Lens' CreateVPCEndpointResponse (Maybe VPCEndpoint)
cversVPCEndpoint = lens _cversVPCEndpoint (\s a -> s {_cversVPCEndpoint = a})

-- | -- | The response status code.
cversResponseStatus :: Lens' CreateVPCEndpointResponse Int
cversResponseStatus = lens _cversResponseStatus (\s a -> s {_cversResponseStatus = a})

instance NFData CreateVPCEndpointResponse
