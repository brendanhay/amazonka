{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.CreateLoadBalancer
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
-- us-east-1.elb.amazonaws.com (for the Northern Virginia region)
-- us-west-1.elb.amazonaws.com (for the Northern California region) For
-- information about the AWS regions supported by Elastic Load Balancing, see
-- Regions and Endpoints. You can create up to 20 load balancers per region
-- per account. Elastic Load Balancing supports load balancing your Amazon EC2
-- instances launched within any one of the following platforms: EC2-Classic
-- For information on creating and managing your load balancers in
-- EC2-Classic, see Deploy Elastic Load Balancing in Amazon EC2-Classic.
-- EC2-VPC For information on creating and managing your load balancers in
-- EC2-VPC, see Deploy Elastic Load Balancing in Amazon VPC. Create a TCP Load
-- Balancer in EC2-Classic
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &AvailabilityZones.member.1=us-east-1c
-- &Listeners.member.1.LoadBalancerPort=80 &Listeners.member.1.InstancePort=80
-- &Listeners.member.1.Protocol=http &Listeners.member.1.InstanceProtocol=http
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- my-test-loadbalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create an HTTPS Load Balancer in
-- EC2-Classic
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyHTTPSLoadBalancer
-- &AvailabilityZones.member.1=us-east-1c
-- &Listeners.member.1.LoadBalancerPort=443
-- &Listeners.member.1.InstancePort=443 &Listeners.member.1.Protocol=https
-- &Listeners.member.1.InstanceProtocol=https
-- &Listeners.member.1.SSLCertificateId=arn:aws:iam::123456789012:server-certificate/servercert
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- MyHTTPSLoadBalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create a TCP Load Balancer in EC2-VPC
-- https://elasticloadbalancing.amazonaws.com/?SecurityGroups.member.1=sg-6801da07
-- &LoadBalancerName=MyVPCLoadBalancer &Listeners.member.1.LoadBalancerPort=80
-- &Listeners.member.1.InstancePort=80 &Listeners.member.1.Protocol=http
-- &Listeners.member.1.InstanceProtocol=http &Subnets.member.1=subnet-6dec9f03
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- MyVPCLoadBalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create an Internal TCP Load Balancer
-- in EC2-VPC https://elasticloadbalancing.amazonaws.com/?Scheme=internal
-- &SecurityGroups.member.1=sg-706cb61f
-- &LoadBalancerName=MyInternalLoadBalancer
-- &Listeners.member.1.LoadBalancerPort=80 &Listeners.member.1.InstancePort=80
-- &Listeners.member.1.Protocol=http &Listeners.member.1.InstanceProtocol=http
-- &Subnets.member.1=subnet-9edc97f0 &Version=2012-06-01
-- &Action=CreateLoadBalancer &AUTHPARAMS
-- internal-MyInternalLoadBalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create a TCP Load Balancer in a
-- Default VPC
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyDefaultVPCLoadBalancer
-- &AvailabilityZones.member.1=sa-east-1b
-- &Listeners.member.1.LoadBalancerPort=80 &Listeners.member.1.InstancePort=80
-- &Listeners.member.1.Protocol=http &Listeners.member.1.InstanceProtocol=http
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- MyDefaultVPCLoadBalancer-1234567890.sa.east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Create a TCP Load Balancer in
-- EC2-Classic and Assign a Tag
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &AvailabilityZones.member.1=us-east-1c
-- &Listeners.member.1.LoadBalancerPort=80 &Listeners.member.1.InstancePort=80
-- &Listeners.member.1.Protocol=http &Listeners.member.1.InstanceProtocol=http
-- &Tags.member.1.Value=test &Tags.member.1.Key=environment
-- &Version=2012-06-01 &Action=CreateLoadBalancer &AUTHPARAMS
-- my-test-loadbalancer-1234567890.us-east-1.elb.amazonaws.com
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
module Network.AWS.ELB.CreateLoadBalancer
    (
    -- * Request
      CreateLoadBalancer
    -- ** Request constructor
    , mkCreateLoadBalancer
    -- ** Request lenses
    , clbLoadBalancerName
    , clbListeners
    , clbAvailabilityZones
    , clbSubnets
    , clbSecurityGroups
    , clbScheme
    , clbTags

    -- * Response
    , CreateLoadBalancerResponse
    -- ** Response constructor
    , mkCreateLoadBalancerResponse
    -- ** Response lenses
    , clbrDNSName
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the CreateLoadBalancer action.
data CreateLoadBalancer = CreateLoadBalancer
    { _clbLoadBalancerName :: Text
    , _clbListeners :: [Listener]
    , _clbAvailabilityZones :: [Text]
    , _clbSubnets :: [Text]
    , _clbSecurityGroups :: [Text]
    , _clbScheme :: Maybe Text
    , _clbTags :: Maybe (List1 Tag)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLoadBalancer' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @Listeners ::@ @[Listener]@
--
-- * @AvailabilityZones ::@ @[Text]@
--
-- * @Subnets ::@ @[Text]@
--
-- * @SecurityGroups ::@ @[Text]@
--
-- * @Scheme ::@ @Maybe Text@
--
-- * @Tags ::@ @Maybe (List1 Tag)@
--
mkCreateLoadBalancer :: Text -- ^ 'clbLoadBalancerName'
                     -> [Listener] -- ^ 'clbListeners'
                     -> CreateLoadBalancer
mkCreateLoadBalancer p1 p2 = CreateLoadBalancer
    { _clbLoadBalancerName = p1
    , _clbListeners = p2
    , _clbAvailabilityZones = mempty
    , _clbSubnets = mempty
    , _clbSecurityGroups = mempty
    , _clbScheme = Nothing
    , _clbTags = Nothing
    }

-- | The name associated with the load balancer. The name must be unique within
-- your set of load balancers, must have a maximum of 32 characters, and must
-- only contain alphanumeric characters or hyphens.
clbLoadBalancerName :: Lens' CreateLoadBalancer Text
clbLoadBalancerName =
    lens _clbLoadBalancerName (\s a -> s { _clbLoadBalancerName = a })

-- | A list of the following tuples: Protocol, LoadBalancerPort,
-- InstanceProtocol, InstancePort, and SSLCertificateId.
clbListeners :: Lens' CreateLoadBalancer [Listener]
clbListeners = lens _clbListeners (\s a -> s { _clbListeners = a })

-- | A list of Availability Zones. At least one Availability Zone must be
-- specified. Specified Availability Zones must be in the same EC2 Region as
-- the load balancer. Traffic will be equally distributed across all zones.
-- You can later add more Availability Zones after the creation of the load
-- balancer by calling EnableAvailabilityZonesForLoadBalancer action.
clbAvailabilityZones :: Lens' CreateLoadBalancer [Text]
clbAvailabilityZones =
    lens _clbAvailabilityZones (\s a -> s { _clbAvailabilityZones = a })

-- | A list of subnet IDs in your VPC to attach to your load balancer. Specify
-- one subnet per Availability Zone.
clbSubnets :: Lens' CreateLoadBalancer [Text]
clbSubnets = lens _clbSubnets (\s a -> s { _clbSubnets = a })

-- | The security groups to assign to your load balancer within your VPC.
clbSecurityGroups :: Lens' CreateLoadBalancer [Text]
clbSecurityGroups =
    lens _clbSecurityGroups (\s a -> s { _clbSecurityGroups = a })

-- | The type of a load balancer. By default, Elastic Load Balancing creates an
-- Internet-facing load balancer with a publicly resolvable DNS name, which
-- resolves to public IP addresses. For more information about Internet-facing
-- and Internal load balancers, see Internet-facing and Internal Load
-- Balancers. Specify the value internal for this option to create an internal
-- load balancer with a DNS name that resolves to private IP addresses. This
-- option is only available for load balancers created within EC2-VPC.
clbScheme :: Lens' CreateLoadBalancer (Maybe Text)
clbScheme = lens _clbScheme (\s a -> s { _clbScheme = a })

-- | A list of tags to assign to the load balancer. For more information about
-- setting tags for your load balancer, see Tagging.
clbTags :: Lens' CreateLoadBalancer (Maybe (List1 Tag))
clbTags = lens _clbTags (\s a -> s { _clbTags = a })

instance ToQuery CreateLoadBalancer where
    toQuery = genericQuery def

-- | The output for the CreateLoadBalancer action.
newtype CreateLoadBalancerResponse = CreateLoadBalancerResponse
    { _clbrDNSName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLoadBalancerResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DNSName ::@ @Maybe Text@
--
mkCreateLoadBalancerResponse :: CreateLoadBalancerResponse
mkCreateLoadBalancerResponse = CreateLoadBalancerResponse
    { _clbrDNSName = Nothing
    }

-- | The DNS name for the load balancer.
clbrDNSName :: Lens' CreateLoadBalancerResponse (Maybe Text)
clbrDNSName = lens _clbrDNSName (\s a -> s { _clbrDNSName = a })

instance FromXML CreateLoadBalancerResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateLoadBalancer where
    type Sv CreateLoadBalancer = ELB
    type Rs CreateLoadBalancer = CreateLoadBalancerResponse

    request = post "CreateLoadBalancer"
    response _ = xmlResponse
