{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a load balancer.
--
-- If the call completes successfully, a new load balancer is created with a
-- unique Domain Name Service (DNS) name. The DNS name includes the name of the
-- AWS region in which the load balancer was created. For example, the DNS name
-- might end with either of the following:
--
-- 'us-east-1.elb.amazonaws.com'   'us-west-2.elb.amazonaws.com'   For
-- information about the AWS regions supported by Elastic Load Balancing, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#elb_region Regions and Endpoints> in the /Amazon Web Services General Reference/.
--
-- You can create up to 20 load balancers per region per account. You can
-- request an increase for the number of load balancers for your account. For
-- more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elb-limits.html Elastic Load Balancing Limits> in the /Elastic LoadBalancing Developer Guide/.
--
-- Elastic Load Balancing supports load balancing your EC2 instances launched
-- in either the EC2-Classic or EC2-VPC platform. For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/UserScenariosForEC2.html Elastic Load Balancing in EC2-Classic> or <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/UserScenariosForVPC.html Elastic Load Balancing in a VPC> in the /Elastic Load Balancing DeveloperGuide/.
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
    { _clbAvailabilityZones :: List "member" Text
    , _clbListeners         :: List "member" Listener
    , _clbLoadBalancerName  :: Text
    , _clbScheme            :: Maybe Text
    , _clbSecurityGroups    :: List "member" Text
    , _clbSubnets           :: List "member" Text
    , _clbTags              :: List1 "member" Tag
    } deriving (Eq, Read, Show)

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

-- | One or more Availability Zones from the same region as the load balancer.
-- Traffic is equally distributed across all specified Availability Zones.
--
-- You must specify at least one Availability Zone.
--
-- You can add more Availability Zones after you create the load balancer using 'EnableAvailabilityZonesForLoadBalancer'.
clbAvailabilityZones :: Lens' CreateLoadBalancer [Text]
clbAvailabilityZones =
    lens _clbAvailabilityZones (\s a -> s { _clbAvailabilityZones = a })
        . _List

-- | The listeners.
--
-- For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elb-listener-config.html Listener Configurations for Elastic Load Balancing>
-- in the /Elastic Load Balancing Developer Guide/.
clbListeners :: Lens' CreateLoadBalancer [Listener]
clbListeners = lens _clbListeners (\s a -> s { _clbListeners = a }) . _List

-- | The name of the load balancer.
--
-- This name must be unique within your AWS account, must have a maximum of 32
-- characters, must contain only alphanumeric characters or hyphens, and cannot
-- begin or end with a hyphen.
clbLoadBalancerName :: Lens' CreateLoadBalancer Text
clbLoadBalancerName =
    lens _clbLoadBalancerName (\s a -> s { _clbLoadBalancerName = a })

-- | The type of a load balancer. Valid only for load balancers in a VPC.
--
-- By default, Elastic Load Balancing creates an Internet-facing load balancer
-- with a publicly resolvable DNS name, which resolves to public IP addresses.
-- For more information about Internet-facing and Internal load balancers, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/vpc-loadbalancer-types.html Internet-facing and Internal Load Balancers> in the /Elastic Load Balancing Developer Guide/.
--
-- Specify 'internal' to create an internal load balancer with a DNS name that
-- resolves to private IP addresses.
clbScheme :: Lens' CreateLoadBalancer (Maybe Text)
clbScheme = lens _clbScheme (\s a -> s { _clbScheme = a })

-- | The IDs of the security groups to assign to the load balancer.
clbSecurityGroups :: Lens' CreateLoadBalancer [Text]
clbSecurityGroups =
    lens _clbSecurityGroups (\s a -> s { _clbSecurityGroups = a })
        . _List

-- | The IDs of the subnets in your VPC to attach to the load balancer. Specify
-- one subnet per Availability Zone specified in 'AvailabilityZones'.
clbSubnets :: Lens' CreateLoadBalancer [Text]
clbSubnets = lens _clbSubnets (\s a -> s { _clbSubnets = a }) . _List

-- | A list of tags to assign to the load balancer.
--
-- For more information about tagging your load balancer, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#tagging-elb Tagging> in the /Elastic Load Balancing Developer Guide/.
clbTags :: Lens' CreateLoadBalancer (NonEmpty Tag)
clbTags = lens _clbTags (\s a -> s { _clbTags = a }) . _List1

newtype CreateLoadBalancerResponse = CreateLoadBalancerResponse
    { _clbrDNSName :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

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

-- | The DNS name of the load balancer.
clbrDNSName :: Lens' CreateLoadBalancerResponse (Maybe Text)
clbrDNSName = lens _clbrDNSName (\s a -> s { _clbrDNSName = a })

instance ToPath CreateLoadBalancer where
    toPath = const "/"

instance ToQuery CreateLoadBalancer where
    toQuery CreateLoadBalancer{..} = mconcat
        [ "AvailabilityZones" =? _clbAvailabilityZones
        , "Listeners"         =? _clbListeners
        , "LoadBalancerName"  =? _clbLoadBalancerName
        , "Scheme"            =? _clbScheme
        , "SecurityGroups"    =? _clbSecurityGroups
        , "Subnets"           =? _clbSubnets
        , "Tags"              =? _clbTags
        ]

instance ToHeaders CreateLoadBalancer

instance AWSRequest CreateLoadBalancer where
    type Sv CreateLoadBalancer = ELB
    type Rs CreateLoadBalancer = CreateLoadBalancerResponse

    request  = post "CreateLoadBalancer"
    response = xmlResponse

instance FromXML CreateLoadBalancerResponse where
    parseXML = withElement "CreateLoadBalancerResult" $ \x -> CreateLoadBalancerResponse
        <$> x .@? "DNSName"
