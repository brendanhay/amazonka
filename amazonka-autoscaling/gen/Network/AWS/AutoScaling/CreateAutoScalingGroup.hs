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
      CreateAutoScalingGroup
    -- ** Request constructor
    , createAutoScalingGroup
    -- ** Request lenses
    , casgAutoScalingGroupName
    , casgAvailabilityZones
    , casgDefaultCooldown
    , casgDesiredCapacity
    , casgHealthCheckGracePeriod
    , casgHealthCheckType
    , casgInstanceId
    , casgLaunchConfigurationName
    , casgLoadBalancerNames
    , casgMaxSize
    , casgMinSize
    , casgPlacementGroup
    , casgTags
    , casgTerminationPolicies
    , casgVPCZoneIdentifier

    -- * Response
    , CreateAutoScalingGroupResponse
    -- ** Response constructor
    , createAutoScalingGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data CreateAutoScalingGroup = CreateAutoScalingGroup
    { _casgAutoScalingGroupName    :: Text
    , _casgAvailabilityZones       :: List1 Text
    , _casgDefaultCooldown         :: Maybe Int
    , _casgDesiredCapacity         :: Maybe Int
    , _casgHealthCheckGracePeriod  :: Maybe Int
    , _casgHealthCheckType         :: Maybe Text
    , _casgInstanceId              :: Maybe Text
    , _casgLaunchConfigurationName :: Maybe Text
    , _casgLoadBalancerNames       :: [Text]
    , _casgMaxSize                 :: Int
    , _casgMinSize                 :: Int
    , _casgPlacementGroup          :: Maybe Text
    , _casgTags                    :: [Tag]
    , _casgTerminationPolicies     :: [Text]
    , _casgVPCZoneIdentifier       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CreateAutoScalingGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'casgAutoScalingGroupName' @::@ 'Text'
--
-- * 'casgAvailabilityZones' @::@ 'NonEmpty' 'Text'
--
-- * 'casgDefaultCooldown' @::@ 'Maybe' 'Int'
--
-- * 'casgDesiredCapacity' @::@ 'Maybe' 'Int'
--
-- * 'casgHealthCheckGracePeriod' @::@ 'Maybe' 'Int'
--
-- * 'casgHealthCheckType' @::@ 'Maybe' 'Text'
--
-- * 'casgInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'casgLaunchConfigurationName' @::@ 'Maybe' 'Text'
--
-- * 'casgLoadBalancerNames' @::@ ['Text']
--
-- * 'casgMaxSize' @::@ 'Int'
--
-- * 'casgMinSize' @::@ 'Int'
--
-- * 'casgPlacementGroup' @::@ 'Maybe' 'Text'
--
-- * 'casgTags' @::@ ['Tag']
--
-- * 'casgTerminationPolicies' @::@ ['Text']
--
-- * 'casgVPCZoneIdentifier' @::@ 'Maybe' 'Text'
--
createAutoScalingGroup :: Text -- ^ 'casgAutoScalingGroupName'
                       -> Int -- ^ 'casgMinSize'
                       -> Int -- ^ 'casgMaxSize'
                       -> NonEmpty Text -- ^ 'casgAvailabilityZones'
                       -> CreateAutoScalingGroup
createAutoScalingGroup p1 p2 p3 p4 = CreateAutoScalingGroup
    { _casgAutoScalingGroupName    = p1
    , _casgMinSize                 = p2
    , _casgMaxSize                 = p3
    , _casgAvailabilityZones       = withIso _List1 (const id) p4
    , _casgLaunchConfigurationName = Nothing
    , _casgInstanceId              = Nothing
    , _casgDesiredCapacity         = Nothing
    , _casgDefaultCooldown         = Nothing
    , _casgLoadBalancerNames       = mempty
    , _casgHealthCheckType         = Nothing
    , _casgHealthCheckGracePeriod  = Nothing
    , _casgPlacementGroup          = Nothing
    , _casgVPCZoneIdentifier       = Nothing
    , _casgTerminationPolicies     = mempty
    , _casgTags                    = mempty
    }

-- | The name of the Auto Scaling group.
casgAutoScalingGroupName :: Lens' CreateAutoScalingGroup Text
casgAutoScalingGroupName =
    lens _casgAutoScalingGroupName
        (\s a -> s { _casgAutoScalingGroupName = a })

-- | A list of Availability Zones for the Auto Scaling group. This is required
-- unless you have specified subnets.
casgAvailabilityZones :: Lens' CreateAutoScalingGroup (NonEmpty Text)
casgAvailabilityZones =
    lens _casgAvailabilityZones (\s a -> s { _casgAvailabilityZones = a })
        . _List1

-- | The amount of time, in seconds, between a successful scaling activity and
-- the succeeding scaling activity. If a DefaultCooldown period is not
-- specified, Auto Scaling uses the default value of 300 as the default cool
-- down period for the Auto Scaling group. For more information, see
-- Cooldown Period.
casgDefaultCooldown :: Lens' CreateAutoScalingGroup (Maybe Int)
casgDefaultCooldown =
    lens _casgDefaultCooldown (\s a -> s { _casgDefaultCooldown = a })

-- | The number of Amazon EC2 instances that should be running in the group.
-- The desired capacity must be greater than or equal to the minimum size
-- and less than or equal to the maximum size specified for the Auto Scaling
-- group.
casgDesiredCapacity :: Lens' CreateAutoScalingGroup (Maybe Int)
casgDesiredCapacity =
    lens _casgDesiredCapacity (\s a -> s { _casgDesiredCapacity = a })

-- | Length of time in seconds after a new Amazon EC2 instance comes into
-- service that Auto Scaling starts checking its health. During this time
-- any health check failure for the that instance is ignored. This is
-- required if you are adding ELB health check. Frequently, new instances
-- need to warm up, briefly, before they can pass a health check. To provide
-- ample warm-up time, set the health check grace period of the group to
-- match the expected startup period of your application. For more
-- information, see Add an Elastic Load Balancing Health Check.
casgHealthCheckGracePeriod :: Lens' CreateAutoScalingGroup (Maybe Int)
casgHealthCheckGracePeriod =
    lens _casgHealthCheckGracePeriod
        (\s a -> s { _casgHealthCheckGracePeriod = a })

-- | The service you want the health checks from, Amazon EC2 or Elastic Load
-- Balancer. Valid values are EC2 or ELB. By default, the Auto Scaling
-- health check uses the results of Amazon EC2 instance status checks to
-- determine the health of an instance. For more information, see Health
-- Check.
casgHealthCheckType :: Lens' CreateAutoScalingGroup (Maybe Text)
casgHealthCheckType =
    lens _casgHealthCheckType (\s a -> s { _casgHealthCheckType = a })

-- | The ID of the Amazon EC2 instance you want to use to create the Auto
-- Scaling group. Use this attribute if you want to create an Auto Scaling
-- group using an EC2 instance instead of a launch configuration. When you
-- use an instance to create an Auto Scaling group, a new launch
-- configuration is first created and then associated with the Auto Scaling
-- group. The new launch configuration derives all its attributes from the
-- instance that is used to create the Auto Scaling group, with the
-- exception of BlockDeviceMapping. For more information, see Create an Auto
-- Scaling Group Using EC2 Instance in the Auto Scaling Developer Guide.
casgInstanceId :: Lens' CreateAutoScalingGroup (Maybe Text)
casgInstanceId = lens _casgInstanceId (\s a -> s { _casgInstanceId = a })

-- | The name of an existing launch configuration to use to launch new
-- instances. Use this attribute if you want to create an Auto Scaling group
-- using an existing launch configuration instead of an EC2 instance.
casgLaunchConfigurationName :: Lens' CreateAutoScalingGroup (Maybe Text)
casgLaunchConfigurationName =
    lens _casgLaunchConfigurationName
        (\s a -> s { _casgLaunchConfigurationName = a })

-- | A list of existing Elastic Load Balancing load balancers to use. The load
-- balancers must be associated with the AWS account. For information on
-- using load balancers, see Load Balance Your Auto Scaling Group in the
-- Auto Scaling Developer Guide.
casgLoadBalancerNames :: Lens' CreateAutoScalingGroup [Text]
casgLoadBalancerNames =
    lens _casgLoadBalancerNames (\s a -> s { _casgLoadBalancerNames = a })

-- | The maximum size of the Auto Scaling group.
casgMaxSize :: Lens' CreateAutoScalingGroup Int
casgMaxSize = lens _casgMaxSize (\s a -> s { _casgMaxSize = a })

-- | The minimum size of the Auto Scaling group.
casgMinSize :: Lens' CreateAutoScalingGroup Int
casgMinSize = lens _casgMinSize (\s a -> s { _casgMinSize = a })

-- | Physical location of an existing cluster placement group into which you
-- want to launch your instances. For information about cluster placement
-- group, see Using Cluster Instances.
casgPlacementGroup :: Lens' CreateAutoScalingGroup (Maybe Text)
casgPlacementGroup =
    lens _casgPlacementGroup (\s a -> s { _casgPlacementGroup = a })

-- | The tag to be created or updated. Each tag should be defined by its
-- resource type, resource ID, key, value, and a propagate flag. Valid
-- values: key=value, value=value, propagate=true or false. Value and
-- propagate are optional parameters. For information about using tags, see
-- Tag Your Auto Scaling Groups and Amazon EC2 Instances in the Auto Scaling
-- Developer Guide.
casgTags :: Lens' CreateAutoScalingGroup [Tag]
casgTags = lens _casgTags (\s a -> s { _casgTags = a })

-- | A standalone termination policy or a list of termination policies used to
-- select the instance to terminate. The policies are executed in the order
-- that they are listed. For more information on configuring a termination
-- policy for your Auto Scaling group, see Instance Termination Policy for
-- Your Auto Scaling Group in the Auto Scaling Developer Guide.
casgTerminationPolicies :: Lens' CreateAutoScalingGroup [Text]
casgTerminationPolicies =
    lens _casgTerminationPolicies (\s a -> s { _casgTerminationPolicies = a })

-- | A comma-separated list of subnet identifiers of Amazon Virtual Private
-- Clouds (Amazon VPCs). If you specify subnets and Availability Zones with
-- this call, ensure that the subnets' Availability Zones match the
-- Availability Zones specified. For information on launching your Auto
-- Scaling group into Amazon VPC subnets, see Auto Scaling in Amazon Virtual
-- Private Cloud in the Auto Scaling Developer Guide .
casgVPCZoneIdentifier :: Lens' CreateAutoScalingGroup (Maybe Text)
casgVPCZoneIdentifier =
    lens _casgVPCZoneIdentifier (\s a -> s { _casgVPCZoneIdentifier = a })

instance ToQuery CreateAutoScalingGroup

instance ToPath CreateAutoScalingGroup where
    toPath = const "/"

data CreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateAutoScalingGroupResponse' constructor.
createAutoScalingGroupResponse :: CreateAutoScalingGroupResponse
createAutoScalingGroupResponse = CreateAutoScalingGroupResponse

instance AWSRequest CreateAutoScalingGroup where
    type Sv CreateAutoScalingGroup = AutoScaling
    type Rs CreateAutoScalingGroup = CreateAutoScalingGroupResponse

    request  = post "CreateAutoScalingGroup"
    response = nullaryResponse CreateAutoScalingGroupResponse
