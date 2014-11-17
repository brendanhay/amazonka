{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
-- EC2-VPC, see Deploy Elastic Load Balancing in Amazon VPC.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancer.html>
module Network.AWS.ELB.CreateLoadBalancer
    (
    -- * Request
      CreateLoadBalancer
    -- ** Request constructor
    , createLoadBalancer
    -- ** Request lenses
    , clbAvailabilityZones
    , clbListeners
    , clbLoadBalancerName
    , clbScheme
    , clbSecurityGroups
    , clbSubnets
    , clbTags

    -- * Response
    , CreateLoadBalancerResponse
    -- ** Response constructor
    , createLoadBalancerResponse
    -- ** Response lenses
    , clbrDNSName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data CreateLoadBalancer = CreateLoadBalancer
    { _clbAvailabilityZones :: [Text]
    , _clbListeners         :: [Listener]
    , _clbLoadBalancerName  :: Text
    , _clbScheme            :: Maybe Text
    , _clbSecurityGroups    :: [Text]
    , _clbSubnets           :: [Text]
    , _clbTags              :: List1 Tag
    } deriving (Eq, Show, Generic)

-- | 'CreateLoadBalancer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbAvailabilityZones' @::@ ['Text']
--
-- * 'clbListeners' @::@ ['Listener']
--
-- * 'clbLoadBalancerName' @::@ 'Text'
--
-- * 'clbScheme' @::@ 'Maybe' 'Text'
--
-- * 'clbSecurityGroups' @::@ ['Text']
--
-- * 'clbSubnets' @::@ ['Text']
--
-- * 'clbTags' @::@ 'NonEmpty' 'Tag'
--
createLoadBalancer :: Text -- ^ 'clbLoadBalancerName'
                   -> NonEmpty Tag -- ^ 'clbTags'
                   -> CreateLoadBalancer
createLoadBalancer p1 p2 = CreateLoadBalancer
    { _clbLoadBalancerName  = p1
    , _clbTags              = withIso _List1 (const id) p2
    , _clbListeners         = mempty
    , _clbAvailabilityZones = mempty
    , _clbSubnets           = mempty
    , _clbSecurityGroups    = mempty
    , _clbScheme            = Nothing
    }

-- | A list of Availability Zones. At least one Availability Zone must be
-- specified. Specified Availability Zones must be in the same EC2 Region as
-- the load balancer. Traffic will be equally distributed across all zones.
-- You can later add more Availability Zones after the creation of the load
-- balancer by calling EnableAvailabilityZonesForLoadBalancer action.
clbAvailabilityZones :: Lens' CreateLoadBalancer [Text]
clbAvailabilityZones =
    lens _clbAvailabilityZones (\s a -> s { _clbAvailabilityZones = a })

-- | A list of the following tuples: Protocol, LoadBalancerPort,
-- InstanceProtocol, InstancePort, and SSLCertificateId.
clbListeners :: Lens' CreateLoadBalancer [Listener]
clbListeners = lens _clbListeners (\s a -> s { _clbListeners = a })

-- | The name associated with the load balancer. The name must be unique
-- within your set of load balancers, must have a maximum of 32 characters,
-- and must only contain alphanumeric characters or hyphens.
clbLoadBalancerName :: Lens' CreateLoadBalancer Text
clbLoadBalancerName =
    lens _clbLoadBalancerName (\s a -> s { _clbLoadBalancerName = a })

-- | The type of a load balancer. By default, Elastic Load Balancing creates
-- an Internet-facing load balancer with a publicly resolvable DNS name,
-- which resolves to public IP addresses. For more information about
-- Internet-facing and Internal load balancers, see Internet-facing and
-- Internal Load Balancers. Specify the value internal for this option to
-- create an internal load balancer with a DNS name that resolves to private
-- IP addresses. This option is only available for load balancers created
-- within EC2-VPC.
clbScheme :: Lens' CreateLoadBalancer (Maybe Text)
clbScheme = lens _clbScheme (\s a -> s { _clbScheme = a })

-- | The security groups to assign to your load balancer within your VPC.
clbSecurityGroups :: Lens' CreateLoadBalancer [Text]
clbSecurityGroups =
    lens _clbSecurityGroups (\s a -> s { _clbSecurityGroups = a })

-- | A list of subnet IDs in your VPC to attach to your load balancer. Specify
-- one subnet per Availability Zone.
clbSubnets :: Lens' CreateLoadBalancer [Text]
clbSubnets = lens _clbSubnets (\s a -> s { _clbSubnets = a })

-- | A list of tags to assign to the load balancer. For more information about
-- setting tags for your load balancer, see Tagging.
clbTags :: Lens' CreateLoadBalancer (NonEmpty Tag)
clbTags = lens _clbTags (\s a -> s { _clbTags = a })
    . _List1

newtype CreateLoadBalancerResponse = CreateLoadBalancerResponse
    { _clbrDNSName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateLoadBalancerResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clbrDNSName' @::@ 'Maybe' 'Text'
--
createLoadBalancerResponse :: CreateLoadBalancerResponse
createLoadBalancerResponse = CreateLoadBalancerResponse
    { _clbrDNSName = Nothing
    }

-- | The DNS name for the load balancer.
clbrDNSName :: Lens' CreateLoadBalancerResponse (Maybe Text)
clbrDNSName = lens _clbrDNSName (\s a -> s { _clbrDNSName = a })

instance ToPath CreateLoadBalancer where
    toPath = const "/"

instance ToQuery CreateLoadBalancer

instance ToHeaders CreateLoadBalancer

instance AWSRequest CreateLoadBalancer where
    type Sv CreateLoadBalancer = ELB
    type Rs CreateLoadBalancer = CreateLoadBalancerResponse

    request  = post "CreateLoadBalancer"
    response = xmlResponse

instance FromXML CreateLoadBalancerResponse where
    parseXML c = CreateLoadBalancerResponse
        <$> c .: "DNSName"
