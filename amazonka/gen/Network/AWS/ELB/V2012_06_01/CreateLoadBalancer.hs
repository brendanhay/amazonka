{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ELB.V2012_06_01.CreateLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new load balancer. After the call has completed successfully, a
-- new load balancer is created with a unique Domain Name Service (DNS) name.
-- The DNS name includes the name of the AWS region in which the load balance
-- was created. For example, if your load balancer was created in the United
-- States, the DNS name might end with either of the following:
-- us-east-1.elb.amazonaws.com (for the Northern Virginia Region)
-- us-west-1.elb.amazonaws.com (for the Northern California Region) For
-- information about the AWS regions supported by Elastic Load Balancing, see
-- Regions and Endpoints. You can create up to 20 load balancers per region
-- per account. Elastic Load Balancing supports load balancing your Amazon EC2
-- instances launched within any one of the following platforms: EC2-Classic
-- For information on creating and managing your load balancers in
-- EC2-Classic, see Deploy Elastic Load Balancing in Amazon EC2-Classic.
-- EC2-VPC For information on creating and managing your load balancers in
-- EC2-VPC, see Deploy Elastic Load Balancing in Amazon VPC. Create a TCP load
-- balancer in EC2-Classic
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyLoadBalancer
-- &AvailabilityZones.member.1=us-east-1c
-- &Listeners.member.1.LoadBalancerPort=80 &Listeners.member.1.InstancePort=80
-- &Listeners.member.1.Protocol=http &Listeners.member.1.InstanceProtocol=http
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- MyLoadBalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create HTTPS load balancer in
-- EC2-Classic
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyHTTPSLoadBalancer
-- &AvailabilityZones.member.1=us-east-1c
-- &Listeners.member.1.LoadBalancerPort=443
-- &Listeners.member.1.InstancePort=443 &Listeners.member.1.Protocol=https
-- &Listeners.member.1.InstanceProtocol=https
-- &Listeners.member.1.SSLCertificateId=arn:aws:iam::123456789012:server-certificate/servercert
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- MyHTTPSLoadBalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create a TCP load balancer in EC2-VPC
-- https://elasticloadbalancing.amazonaws.com/?SecurityGroups.member.1=sg-6801da07
-- &LoadBalancerName=MyVPCLoadBalancer &Listeners.member.1.LoadBalancerPort=80
-- &Listeners.member.1.InstancePort=80 &Listeners.member.1.Protocol=http
-- &Listeners.member.1.InstanceProtocol=http &Subnets.member.1=subnet-6dec9f03
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- MyVPCLoadBalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create an internal TCP load balancer
-- in EC2-VPC https://elasticloadbalancing.amazonaws.com/?Scheme=internal
-- &SecurityGroups.member.1=sg-706cb61f
-- &LoadBalancerName=MyInternalLoadBalancer
-- &Listeners.member.1.LoadBalancerPort=80 &Listeners.member.1.InstancePort=80
-- &Listeners.member.1.Protocol=http &Listeners.member.1.InstanceProtocol=http
-- &Subnets.member.1=subnet-9edc97f0 &Version=2012-06-01
-- &Action=CreateLoadBalancer &AUTHPARAMS
-- internal-MyInternalLoadBalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create a TCP load balancer in default
-- VPC
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyDefaultVPCLoadBalancer
-- &AvailabilityZones.member.1=sa-east-1b
-- &Listeners.member.1.LoadBalancerPort=80 &Listeners.member.1.InstancePort=80
-- &Listeners.member.1.Protocol=http &Listeners.member.1.InstanceProtocol=http
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- MyDefaultVPCLoadBalancer-1234567890.sa.east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
module Network.AWS.ELB.V2012_06_01.CreateLoadBalancer where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateLoadBalancer' request.
createLoadBalancer :: Text -- ^ '_capiLoadBalancerName'
                   -> [Listener] -- ^ '_capiListeners'
                   -> CreateLoadBalancer
createLoadBalancer p1 p2 = CreateLoadBalancer
    { _capiLoadBalancerName = p1
    , _capiListeners = p2
    , _capiAvailabilityZones = mempty
    , _capiScheme = Nothing
    , _capiSecurityGroups = mempty
    , _capiSubnets = mempty
    }

data CreateLoadBalancer = CreateLoadBalancer
    { _capiLoadBalancerName :: Text
      -- ^ The name associated with the load balancer. The name must be
      -- unique within your set of load balancers.
    , _capiListeners :: [Listener]
      -- ^ A list of the following tuples: Protocol, LoadBalancerPort,
      -- InstanceProtocol, InstancePort, and SSLCertificateId.
    , _capiAvailabilityZones :: [Text]
      -- ^ A list of Availability Zones. At least one Availability Zone must
      -- be specified. Specified Availability Zones must be in the same
      -- EC2 Region as the load balancer. Traffic will be equally
      -- distributed across all zones. You can later add more Availability
      -- Zones after the creation of the load balancer by calling
      -- EnableAvailabilityZonesForLoadBalancer action.
    , _capiScheme :: Maybe Text
      -- ^ The type of a load balancer. By default, Elastic Load Balancing
      -- creates an Internet-facing load balancer with a publicly
      -- resolvable DNS name, which resolves to public IP addresses. For
      -- more informationabout Internet-facing and Internal load
      -- balancers, see Internet-facing and Internal Load Balancers.
      -- Specify the value internal for this option to create an internal
      -- load balancer with a DNS name that resolves to private IP
      -- addresses. This option is only available for load balancers
      -- created within EC2-VPC.
    , _capiSecurityGroups :: [Text]
      -- ^ The security groups to assign to your load balancer within your
      -- VPC.
    , _capiSubnets :: [Text]
      -- ^ A list of subnet IDs in your VPC to attach to your load balancer.
      -- Specify one subnet per Availability Zone.
    } deriving (Show, Generic)

makeLenses ''CreateLoadBalancer

instance ToQuery CreateLoadBalancer where
    toQuery = genericToQuery def

data CreateLoadBalancerResponse = CreateLoadBalancerResponse
    { _capoDNSName :: Maybe Text
      -- ^ The DNS name for the load balancer.
    } deriving (Show, Generic)

makeLenses ''CreateLoadBalancerResponse

instance AWSRequest CreateLoadBalancer where
    type Sv CreateLoadBalancer = ELB
    type Rs CreateLoadBalancer = CreateLoadBalancerResponse

    request = post "CreateLoadBalancer"
    response _ = cursorResponse $ \hs xml ->
        pure CreateLoadBalancerResponse
            <*> xml %|? "DNSName"
