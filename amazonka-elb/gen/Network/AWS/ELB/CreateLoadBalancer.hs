{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.ELB.CreateLoadBalancer
    (
    -- * Request
      CreateAccessPointInput
    -- ** Request constructor
    , createAccessPointInput
    -- ** Request lenses
    , capiAvailabilityZones
    , capiListeners
    , capiLoadBalancerName
    , capiScheme
    , capiSecurityGroups
    , capiSubnets
    , capiTags

    -- * Response
    , CreateAccessPointOutput
    -- ** Response constructor
    , createAccessPointOutput
    -- ** Response lenses
    , capoDNSName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data CreateAccessPointInput = CreateAccessPointInput
    { _capiAvailabilityZones :: [Text]
    , _capiListeners         :: [Listener]
    , _capiLoadBalancerName  :: Text
    , _capiScheme            :: Maybe Text
    , _capiSecurityGroups    :: [Text]
    , _capiSubnets           :: [Text]
    , _capiTags              :: List1 Tag
    } deriving (Eq, Show, Generic)

-- | 'CreateAccessPointInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'capiAvailabilityZones' @::@ ['Text']
--
-- * 'capiListeners' @::@ ['Listener']
--
-- * 'capiLoadBalancerName' @::@ 'Text'
--
-- * 'capiScheme' @::@ 'Maybe' 'Text'
--
-- * 'capiSecurityGroups' @::@ ['Text']
--
-- * 'capiSubnets' @::@ ['Text']
--
-- * 'capiTags' @::@ 'NonEmpty' 'Tag'
--
createAccessPointInput :: Text -- ^ 'capiLoadBalancerName'
                       -> NonEmpty Tag -- ^ 'capiTags'
                       -> CreateAccessPointInput
createAccessPointInput p1 p2 = CreateAccessPointInput
    { _capiLoadBalancerName  = p1
    , _capiTags              = withIso _List1 (const id) p2
    , _capiListeners         = mempty
    , _capiAvailabilityZones = mempty
    , _capiSubnets           = mempty
    , _capiSecurityGroups    = mempty
    , _capiScheme            = Nothing
    }

-- | A list of Availability Zones. At least one Availability Zone must be
-- specified. Specified Availability Zones must be in the same EC2 Region as
-- the load balancer. Traffic will be equally distributed across all zones.
-- You can later add more Availability Zones after the creation of the load
-- balancer by calling EnableAvailabilityZonesForLoadBalancer action.
capiAvailabilityZones :: Lens' CreateAccessPointInput [Text]
capiAvailabilityZones =
    lens _capiAvailabilityZones (\s a -> s { _capiAvailabilityZones = a })

-- | A list of the following tuples: Protocol, LoadBalancerPort,
-- InstanceProtocol, InstancePort, and SSLCertificateId.
capiListeners :: Lens' CreateAccessPointInput [Listener]
capiListeners = lens _capiListeners (\s a -> s { _capiListeners = a })

-- | The name associated with the load balancer. The name must be unique
-- within your set of load balancers, must have a maximum of 32 characters,
-- and must only contain alphanumeric characters or hyphens.
capiLoadBalancerName :: Lens' CreateAccessPointInput Text
capiLoadBalancerName =
    lens _capiLoadBalancerName (\s a -> s { _capiLoadBalancerName = a })

-- | The type of a load balancer. By default, Elastic Load Balancing creates
-- an Internet-facing load balancer with a publicly resolvable DNS name,
-- which resolves to public IP addresses. For more information about
-- Internet-facing and Internal load balancers, see Internet-facing and
-- Internal Load Balancers. Specify the value internal for this option to
-- create an internal load balancer with a DNS name that resolves to private
-- IP addresses. This option is only available for load balancers created
-- within EC2-VPC.
capiScheme :: Lens' CreateAccessPointInput (Maybe Text)
capiScheme = lens _capiScheme (\s a -> s { _capiScheme = a })

-- | The security groups to assign to your load balancer within your VPC.
capiSecurityGroups :: Lens' CreateAccessPointInput [Text]
capiSecurityGroups =
    lens _capiSecurityGroups (\s a -> s { _capiSecurityGroups = a })

-- | A list of subnet IDs in your VPC to attach to your load balancer. Specify
-- one subnet per Availability Zone.
capiSubnets :: Lens' CreateAccessPointInput [Text]
capiSubnets = lens _capiSubnets (\s a -> s { _capiSubnets = a })

-- | A list of tags to assign to the load balancer. For more information about
-- setting tags for your load balancer, see Tagging.
capiTags :: Lens' CreateAccessPointInput (NonEmpty Tag)
capiTags = lens _capiTags (\s a -> s { _capiTags = a })
    . _List1

instance ToQuery CreateAccessPointInput

instance ToPath CreateAccessPointInput where
    toPath = const "/"

newtype CreateAccessPointOutput = CreateAccessPointOutput
    { _capoDNSName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateAccessPointOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'capoDNSName' @::@ 'Maybe' 'Text'
--
createAccessPointOutput :: CreateAccessPointOutput
createAccessPointOutput = CreateAccessPointOutput
    { _capoDNSName = Nothing
    }

-- | The DNS name for the load balancer.
capoDNSName :: Lens' CreateAccessPointOutput (Maybe Text)
capoDNSName = lens _capoDNSName (\s a -> s { _capoDNSName = a })

instance FromXML CreateAccessPointOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateAccessPointOutput"

instance AWSRequest CreateAccessPointInput where
    type Sv CreateAccessPointInput = ELB
    type Rs CreateAccessPointInput = CreateAccessPointOutput

    request  = post "CreateLoadBalancer"
    response = xmlResponse $ \h x -> CreateAccessPointOutput
        <$> x %| "DNSName"
