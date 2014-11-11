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

-- Module      : Network.AWS.AutoScaling.CreateAutoScalingGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new Auto Scaling group with the specified name and other
-- attributes. When the creation request is completed, the Auto Scaling group
-- is ready to be used in other calls.
module Network.AWS.AutoScaling.CreateAutoScalingGroup
    (
    -- * Request
      CreateAutoScalingGroupType
    -- ** Request constructor
    , createAutoScalingGroupType
    -- ** Request lenses
    , casgtAutoScalingGroupName
    , casgtAvailabilityZones
    , casgtDefaultCooldown
    , casgtDesiredCapacity
    , casgtHealthCheckGracePeriod
    , casgtHealthCheckType
    , casgtInstanceId
    , casgtLaunchConfigurationName
    , casgtLoadBalancerNames
    , casgtMaxSize
    , casgtMinSize
    , casgtPlacementGroup
    , casgtTags
    , casgtTerminationPolicies
    , casgtVPCZoneIdentifier

    -- * Response
    , CreateAutoScalingGroupResponse
    -- ** Response constructor
    , createAutoScalingGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data CreateAutoScalingGroupType = CreateAutoScalingGroupType
    { _casgtAutoScalingGroupName    :: Text
    , _casgtAvailabilityZones       :: List1 Text
    , _casgtDefaultCooldown         :: Maybe Int
    , _casgtDesiredCapacity         :: Maybe Int
    , _casgtHealthCheckGracePeriod  :: Maybe Int
    , _casgtHealthCheckType         :: Maybe Text
    , _casgtInstanceId              :: Maybe Text
    , _casgtLaunchConfigurationName :: Maybe Text
    , _casgtLoadBalancerNames       :: [Text]
    , _casgtMaxSize                 :: Int
    , _casgtMinSize                 :: Int
    , _casgtPlacementGroup          :: Maybe Text
    , _casgtTags                    :: [Tag]
    , _casgtTerminationPolicies     :: [Text]
    , _casgtVPCZoneIdentifier       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CreateAutoScalingGroupType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'casgtAutoScalingGroupName' @::@ 'Text'
--
-- * 'casgtAvailabilityZones' @::@ 'NonEmpty' 'Text'
--
-- * 'casgtDefaultCooldown' @::@ 'Maybe' 'Int'
--
-- * 'casgtDesiredCapacity' @::@ 'Maybe' 'Int'
--
-- * 'casgtHealthCheckGracePeriod' @::@ 'Maybe' 'Int'
--
-- * 'casgtHealthCheckType' @::@ 'Maybe' 'Text'
--
-- * 'casgtInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'casgtLaunchConfigurationName' @::@ 'Maybe' 'Text'
--
-- * 'casgtLoadBalancerNames' @::@ ['Text']
--
-- * 'casgtMaxSize' @::@ 'Int'
--
-- * 'casgtMinSize' @::@ 'Int'
--
-- * 'casgtPlacementGroup' @::@ 'Maybe' 'Text'
--
-- * 'casgtTags' @::@ ['Tag']
--
-- * 'casgtTerminationPolicies' @::@ ['Text']
--
-- * 'casgtVPCZoneIdentifier' @::@ 'Maybe' 'Text'
--
createAutoScalingGroupType :: Text -- ^ 'casgtAutoScalingGroupName'
                           -> Int -- ^ 'casgtMinSize'
                           -> Int -- ^ 'casgtMaxSize'
                           -> NonEmpty Text -- ^ 'casgtAvailabilityZones'
                           -> CreateAutoScalingGroupType
createAutoScalingGroupType p1 p2 p3 p4 = CreateAutoScalingGroupType
    { _casgtAutoScalingGroupName    = p1
    , _casgtMinSize                 = p2
    , _casgtMaxSize                 = p3
    , _casgtAvailabilityZones       = withIso _List1 (const id) p4
    , _casgtLaunchConfigurationName = Nothing
    , _casgtInstanceId              = Nothing
    , _casgtDesiredCapacity         = Nothing
    , _casgtDefaultCooldown         = Nothing
    , _casgtLoadBalancerNames       = mempty
    , _casgtHealthCheckType         = Nothing
    , _casgtHealthCheckGracePeriod  = Nothing
    , _casgtPlacementGroup          = Nothing
    , _casgtVPCZoneIdentifier       = Nothing
    , _casgtTerminationPolicies     = mempty
    , _casgtTags                    = mempty
    }

-- | The name of the Auto Scaling group.
casgtAutoScalingGroupName :: Lens' CreateAutoScalingGroupType Text
casgtAutoScalingGroupName =
    lens _casgtAutoScalingGroupName
        (\s a -> s { _casgtAutoScalingGroupName = a })

-- | A list of Availability Zones for the Auto Scaling group. This is required
-- unless you have specified subnets.
casgtAvailabilityZones :: Lens' CreateAutoScalingGroupType (NonEmpty Text)
casgtAvailabilityZones =
    lens _casgtAvailabilityZones (\s a -> s { _casgtAvailabilityZones = a })
        . _List1

-- | The amount of time, in seconds, between a successful scaling activity and
-- the succeeding scaling activity. If a DefaultCooldown period is not
-- specified, Auto Scaling uses the default value of 300 as the default cool
-- down period for the Auto Scaling group. For more information, see
-- Cooldown Period.
casgtDefaultCooldown :: Lens' CreateAutoScalingGroupType (Maybe Int)
casgtDefaultCooldown =
    lens _casgtDefaultCooldown (\s a -> s { _casgtDefaultCooldown = a })

-- | The number of Amazon EC2 instances that should be running in the group.
-- The desired capacity must be greater than or equal to the minimum size
-- and less than or equal to the maximum size specified for the Auto Scaling
-- group.
casgtDesiredCapacity :: Lens' CreateAutoScalingGroupType (Maybe Int)
casgtDesiredCapacity =
    lens _casgtDesiredCapacity (\s a -> s { _casgtDesiredCapacity = a })

-- | Length of time in seconds after a new Amazon EC2 instance comes into
-- service that Auto Scaling starts checking its health. During this time
-- any health check failure for the that instance is ignored. This is
-- required if you are adding ELB health check. Frequently, new instances
-- need to warm up, briefly, before they can pass a health check. To provide
-- ample warm-up time, set the health check grace period of the group to
-- match the expected startup period of your application. For more
-- information, see Add an Elastic Load Balancing Health Check.
casgtHealthCheckGracePeriod :: Lens' CreateAutoScalingGroupType (Maybe Int)
casgtHealthCheckGracePeriod =
    lens _casgtHealthCheckGracePeriod
        (\s a -> s { _casgtHealthCheckGracePeriod = a })

-- | The service you want the health checks from, Amazon EC2 or Elastic Load
-- Balancer. Valid values are EC2 or ELB. By default, the Auto Scaling
-- health check uses the results of Amazon EC2 instance status checks to
-- determine the health of an instance. For more information, see Health
-- Check.
casgtHealthCheckType :: Lens' CreateAutoScalingGroupType (Maybe Text)
casgtHealthCheckType =
    lens _casgtHealthCheckType (\s a -> s { _casgtHealthCheckType = a })

-- | The ID of the Amazon EC2 instance you want to use to create the Auto
-- Scaling group. Use this attribute if you want to create an Auto Scaling
-- group using an EC2 instance instead of a launch configuration. When you
-- use an instance to create an Auto Scaling group, a new launch
-- configuration is first created and then associated with the Auto Scaling
-- group. The new launch configuration derives all its attributes from the
-- instance that is used to create the Auto Scaling group, with the
-- exception of BlockDeviceMapping. For more information, see Create an Auto
-- Scaling Group Using EC2 Instance in the Auto Scaling Developer Guide.
casgtInstanceId :: Lens' CreateAutoScalingGroupType (Maybe Text)
casgtInstanceId = lens _casgtInstanceId (\s a -> s { _casgtInstanceId = a })

-- | The name of an existing launch configuration to use to launch new
-- instances. Use this attribute if you want to create an Auto Scaling group
-- using an existing launch configuration instead of an EC2 instance.
casgtLaunchConfigurationName :: Lens' CreateAutoScalingGroupType (Maybe Text)
casgtLaunchConfigurationName =
    lens _casgtLaunchConfigurationName
        (\s a -> s { _casgtLaunchConfigurationName = a })

-- | A list of existing Elastic Load Balancing load balancers to use. The load
-- balancers must be associated with the AWS account. For information on
-- using load balancers, see Load Balance Your Auto Scaling Group in the
-- Auto Scaling Developer Guide.
casgtLoadBalancerNames :: Lens' CreateAutoScalingGroupType [Text]
casgtLoadBalancerNames =
    lens _casgtLoadBalancerNames (\s a -> s { _casgtLoadBalancerNames = a })

-- | The maximum size of the Auto Scaling group.
casgtMaxSize :: Lens' CreateAutoScalingGroupType Int
casgtMaxSize = lens _casgtMaxSize (\s a -> s { _casgtMaxSize = a })

-- | The minimum size of the Auto Scaling group.
casgtMinSize :: Lens' CreateAutoScalingGroupType Int
casgtMinSize = lens _casgtMinSize (\s a -> s { _casgtMinSize = a })

-- | Physical location of an existing cluster placement group into which you
-- want to launch your instances. For information about cluster placement
-- group, see Using Cluster Instances.
casgtPlacementGroup :: Lens' CreateAutoScalingGroupType (Maybe Text)
casgtPlacementGroup =
    lens _casgtPlacementGroup (\s a -> s { _casgtPlacementGroup = a })

-- | The tag to be created or updated. Each tag should be defined by its
-- resource type, resource ID, key, value, and a propagate flag. Valid
-- values: key=value, value=value, propagate=true or false. Value and
-- propagate are optional parameters. For information about using tags, see
-- Tag Your Auto Scaling Groups and Amazon EC2 Instances in the Auto Scaling
-- Developer Guide.
casgtTags :: Lens' CreateAutoScalingGroupType [Tag]
casgtTags = lens _casgtTags (\s a -> s { _casgtTags = a })

-- | A standalone termination policy or a list of termination policies used to
-- select the instance to terminate. The policies are executed in the order
-- that they are listed. For more information on configuring a termination
-- policy for your Auto Scaling group, see Instance Termination Policy for
-- Your Auto Scaling Group in the Auto Scaling Developer Guide.
casgtTerminationPolicies :: Lens' CreateAutoScalingGroupType [Text]
casgtTerminationPolicies =
    lens _casgtTerminationPolicies
        (\s a -> s { _casgtTerminationPolicies = a })

-- | A comma-separated list of subnet identifiers of Amazon Virtual Private
-- Clouds (Amazon VPCs). If you specify subnets and Availability Zones with
-- this call, ensure that the subnets' Availability Zones match the
-- Availability Zones specified. For information on launching your Auto
-- Scaling group into Amazon VPC subnets, see Auto Scaling in Amazon Virtual
-- Private Cloud in the Auto Scaling Developer Guide .
casgtVPCZoneIdentifier :: Lens' CreateAutoScalingGroupType (Maybe Text)
casgtVPCZoneIdentifier =
    lens _casgtVPCZoneIdentifier (\s a -> s { _casgtVPCZoneIdentifier = a })
instance ToQuery CreateAutoScalingGroupType

instance ToPath CreateAutoScalingGroupType where
    toPath = const "/"

data CreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateAutoScalingGroupResponse' constructor.
createAutoScalingGroupResponse :: CreateAutoScalingGroupResponse
createAutoScalingGroupResponse = CreateAutoScalingGroupResponse
instance FromXML CreateAutoScalingGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateAutoScalingGroupResponse"

instance AWSRequest CreateAutoScalingGroupType where
    type Sv CreateAutoScalingGroupType = AutoScaling
    type Rs CreateAutoScalingGroupType = CreateAutoScalingGroupResponse

    request  = post "CreateAutoScalingGroup"
    response = nullaryResponse CreateAutoScalingGroupResponse
