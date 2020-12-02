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
-- Module      : Network.AWS.ELBv2.CreateLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Application Load Balancer, Network Load Balancer, or Gateway Load Balancer.
--
--
-- For more information, see the following:
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/application-load-balancers.html Application Load Balancers>
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/network-load-balancers.html Network Load Balancers>
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/gateway-load-balancers.html Gateway Load Balancers>
--
--
--
-- This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple load balancers with the same settings, each call succeeds.
module Network.AWS.ELBv2.CreateLoadBalancer
  ( -- * Creating a Request
    createLoadBalancer,
    CreateLoadBalancer,

    -- * Request Lenses
    clbSubnetMappings,
    clbSecurityGroups,
    clbSubnets,
    clbCustomerOwnedIPv4Pool,
    clbIPAddressType,
    clbScheme,
    clbType,
    clbTags,
    clbName,

    -- * Destructuring the Response
    createLoadBalancerResponse,
    CreateLoadBalancerResponse,

    -- * Response Lenses
    clbrsLoadBalancers,
    clbrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { _clbSubnetMappings ::
      !(Maybe [SubnetMapping]),
    _clbSecurityGroups :: !(Maybe [Text]),
    _clbSubnets :: !(Maybe [Text]),
    _clbCustomerOwnedIPv4Pool :: !(Maybe Text),
    _clbIPAddressType :: !(Maybe IPAddressType),
    _clbScheme :: !(Maybe LoadBalancerSchemeEnum),
    _clbType :: !(Maybe LoadBalancerTypeEnum),
    _clbTags :: !(Maybe (List1 Tag)),
    _clbName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbSubnetMappings' - The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings. [Application Load Balancers] You must specify subnets from at least two Availability Zones. You cannot specify Elastic IP addresses for your subnets. [Application Load Balancers on Outposts] You must specify one Outpost subnet. [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones. [Network Load Balancers] You can specify subnets from one or more Availability Zones. You can specify one Elastic IP address per subnet if you need static IP addresses for your internet-facing load balancer. For internal load balancers, you can specify one private IP address per subnet from the IPv4 range of the subnet. For internet-facing load balancer, you can specify one IPv6 address per subnet. [Gateway Load Balancers] You can specify subnets from one or more Availability Zones. You cannot specify Elastic IP addresses for your subnets.
--
-- * 'clbSecurityGroups' - [Application Load Balancers] The IDs of the security groups for the load balancer.
--
-- * 'clbSubnets' - The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings. [Application Load Balancers] You must specify subnets from at least two Availability Zones. [Application Load Balancers on Outposts] You must specify one Outpost subnet. [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones. [Network Load Balancers] You can specify subnets from one or more Availability Zones. [Gateway Load Balancers] You can specify subnets from one or more Availability Zones.
--
-- * 'clbCustomerOwnedIPv4Pool' - [Application Load Balancers on Outposts] The ID of the customer-owned address pool (CoIP pool).
--
-- * 'clbIPAddressType' - The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ .
--
-- * 'clbScheme' - The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet. The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can route requests only from clients with access to the VPC for the load balancer. The default is an Internet-facing load balancer. You cannot specify a scheme for a Gateway Load Balancer.
--
-- * 'clbType' - The type of load balancer. The default is @application@ .
--
-- * 'clbTags' - The tags to assign to the load balancer.
--
-- * 'clbName' - The name of the load balancer. This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, must not begin or end with a hyphen, and must not begin with "internal-".
createLoadBalancer ::
  -- | 'clbName'
  Text ->
  CreateLoadBalancer
createLoadBalancer pName_ =
  CreateLoadBalancer'
    { _clbSubnetMappings = Nothing,
      _clbSecurityGroups = Nothing,
      _clbSubnets = Nothing,
      _clbCustomerOwnedIPv4Pool = Nothing,
      _clbIPAddressType = Nothing,
      _clbScheme = Nothing,
      _clbType = Nothing,
      _clbTags = Nothing,
      _clbName = pName_
    }

-- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings. [Application Load Balancers] You must specify subnets from at least two Availability Zones. You cannot specify Elastic IP addresses for your subnets. [Application Load Balancers on Outposts] You must specify one Outpost subnet. [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones. [Network Load Balancers] You can specify subnets from one or more Availability Zones. You can specify one Elastic IP address per subnet if you need static IP addresses for your internet-facing load balancer. For internal load balancers, you can specify one private IP address per subnet from the IPv4 range of the subnet. For internet-facing load balancer, you can specify one IPv6 address per subnet. [Gateway Load Balancers] You can specify subnets from one or more Availability Zones. You cannot specify Elastic IP addresses for your subnets.
clbSubnetMappings :: Lens' CreateLoadBalancer [SubnetMapping]
clbSubnetMappings = lens _clbSubnetMappings (\s a -> s {_clbSubnetMappings = a}) . _Default . _Coerce

-- | [Application Load Balancers] The IDs of the security groups for the load balancer.
clbSecurityGroups :: Lens' CreateLoadBalancer [Text]
clbSecurityGroups = lens _clbSecurityGroups (\s a -> s {_clbSecurityGroups = a}) . _Default . _Coerce

-- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings. [Application Load Balancers] You must specify subnets from at least two Availability Zones. [Application Load Balancers on Outposts] You must specify one Outpost subnet. [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones. [Network Load Balancers] You can specify subnets from one or more Availability Zones. [Gateway Load Balancers] You can specify subnets from one or more Availability Zones.
clbSubnets :: Lens' CreateLoadBalancer [Text]
clbSubnets = lens _clbSubnets (\s a -> s {_clbSubnets = a}) . _Default . _Coerce

-- | [Application Load Balancers on Outposts] The ID of the customer-owned address pool (CoIP pool).
clbCustomerOwnedIPv4Pool :: Lens' CreateLoadBalancer (Maybe Text)
clbCustomerOwnedIPv4Pool = lens _clbCustomerOwnedIPv4Pool (\s a -> s {_clbCustomerOwnedIPv4Pool = a})

-- | The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ .
clbIPAddressType :: Lens' CreateLoadBalancer (Maybe IPAddressType)
clbIPAddressType = lens _clbIPAddressType (\s a -> s {_clbIPAddressType = a})

-- | The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet. The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can route requests only from clients with access to the VPC for the load balancer. The default is an Internet-facing load balancer. You cannot specify a scheme for a Gateway Load Balancer.
clbScheme :: Lens' CreateLoadBalancer (Maybe LoadBalancerSchemeEnum)
clbScheme = lens _clbScheme (\s a -> s {_clbScheme = a})

-- | The type of load balancer. The default is @application@ .
clbType :: Lens' CreateLoadBalancer (Maybe LoadBalancerTypeEnum)
clbType = lens _clbType (\s a -> s {_clbType = a})

-- | The tags to assign to the load balancer.
clbTags :: Lens' CreateLoadBalancer (Maybe (NonEmpty Tag))
clbTags = lens _clbTags (\s a -> s {_clbTags = a}) . mapping _List1

-- | The name of the load balancer. This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, must not begin or end with a hyphen, and must not begin with "internal-".
clbName :: Lens' CreateLoadBalancer Text
clbName = lens _clbName (\s a -> s {_clbName = a})

instance AWSRequest CreateLoadBalancer where
  type Rs CreateLoadBalancer = CreateLoadBalancerResponse
  request = postQuery eLBv2
  response =
    receiveXMLWrapper
      "CreateLoadBalancerResult"
      ( \s h x ->
          CreateLoadBalancerResponse'
            <$> (x .@? "LoadBalancers" .!@ mempty >>= may (parseXMLList "member"))
            <*> (pure (fromEnum s))
      )

instance Hashable CreateLoadBalancer

instance NFData CreateLoadBalancer

instance ToHeaders CreateLoadBalancer where
  toHeaders = const mempty

instance ToPath CreateLoadBalancer where
  toPath = const "/"

instance ToQuery CreateLoadBalancer where
  toQuery CreateLoadBalancer' {..} =
    mconcat
      [ "Action" =: ("CreateLoadBalancer" :: ByteString),
        "Version" =: ("2015-12-01" :: ByteString),
        "SubnetMappings"
          =: toQuery (toQueryList "member" <$> _clbSubnetMappings),
        "SecurityGroups"
          =: toQuery (toQueryList "member" <$> _clbSecurityGroups),
        "Subnets" =: toQuery (toQueryList "member" <$> _clbSubnets),
        "CustomerOwnedIpv4Pool" =: _clbCustomerOwnedIPv4Pool,
        "IpAddressType" =: _clbIPAddressType,
        "Scheme" =: _clbScheme,
        "Type" =: _clbType,
        "Tags" =: toQuery (toQueryList "member" <$> _clbTags),
        "Name" =: _clbName
      ]

-- | /See:/ 'createLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
  { _clbrsLoadBalancers ::
      !(Maybe [LoadBalancer]),
    _clbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbrsLoadBalancers' - Information about the load balancer.
--
-- * 'clbrsResponseStatus' - -- | The response status code.
createLoadBalancerResponse ::
  -- | 'clbrsResponseStatus'
  Int ->
  CreateLoadBalancerResponse
createLoadBalancerResponse pResponseStatus_ =
  CreateLoadBalancerResponse'
    { _clbrsLoadBalancers = Nothing,
      _clbrsResponseStatus = pResponseStatus_
    }

-- | Information about the load balancer.
clbrsLoadBalancers :: Lens' CreateLoadBalancerResponse [LoadBalancer]
clbrsLoadBalancers = lens _clbrsLoadBalancers (\s a -> s {_clbrsLoadBalancers = a}) . _Default . _Coerce

-- | -- | The response status code.
clbrsResponseStatus :: Lens' CreateLoadBalancerResponse Int
clbrsResponseStatus = lens _clbrsResponseStatus (\s a -> s {_clbrsResponseStatus = a})

instance NFData CreateLoadBalancerResponse
