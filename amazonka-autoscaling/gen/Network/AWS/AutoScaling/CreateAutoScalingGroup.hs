{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- is ready to be used in other calls. The Auto Scaling group name must be
-- unique within the scope of your AWS account.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &AvailabilityZones.member.1=us-east-1a
-- &AvailabilityZones.member.2=us-east-1b &MinSize=2 &MaxSize=10
-- &DesiredCapacity=2 &LoadBalancerNames.member.1=my-test-asg-loadbalancer
-- &HealthCheckType=ELB &HealthCheckGracePeriod=120
-- &LaunchConfigurationName=my-test-lc &Version=2011-01-01
-- &Action=CreateAutoScalingGroup &AUTHPARAMS
-- 8d798a29-f083-11e1-bdfb-cb223EXAMPLE.
module Network.AWS.AutoScaling.CreateAutoScalingGroup
    (
    -- * Request
      CreateAutoScalingGroup
    -- ** Request constructor
    , createAutoScalingGroup
    -- ** Request lenses
    , casgAutoScalingGroupName
    , casgLaunchConfigurationName
    , casgInstanceId
    , casgMinSize
    , casgMaxSize
    , casgDesiredCapacity
    , casgDefaultCooldown
    , casgAvailabilityZones
    , casgLoadBalancerNames
    , casgHealthCheckType
    , casgHealthCheckGracePeriod
    , casgPlacementGroup
    , casgVPCZoneIdentifier
    , casgTerminationPolicies
    , casgTags

    -- * Response
    , CreateAutoScalingGroupResponse
    -- ** Response constructor
    , createAutoScalingGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

-- | 
data CreateAutoScalingGroup = CreateAutoScalingGroup
    { _casgAutoScalingGroupName :: Text
    , _casgLaunchConfigurationName :: Maybe Text
    , _casgInstanceId :: Maybe Text
    , _casgMinSize :: !Integer
    , _casgMaxSize :: !Integer
    , _casgDesiredCapacity :: Maybe Integer
    , _casgDefaultCooldown :: Maybe Integer
    , _casgAvailabilityZones :: Maybe (List1 Text)
    , _casgLoadBalancerNames :: [Text]
    , _casgHealthCheckType :: Maybe Text
    , _casgHealthCheckGracePeriod :: Maybe Integer
    , _casgPlacementGroup :: Maybe Text
    , _casgVPCZoneIdentifier :: Maybe Text
    , _casgTerminationPolicies :: [Text]
    , _casgTags :: [Tag]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateAutoScalingGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @LaunchConfigurationName ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @MinSize ::@ @Integer@
--
-- * @MaxSize ::@ @Integer@
--
-- * @DesiredCapacity ::@ @Maybe Integer@
--
-- * @DefaultCooldown ::@ @Maybe Integer@
--
-- * @AvailabilityZones ::@ @Maybe (List1 Text)@
--
-- * @LoadBalancerNames ::@ @[Text]@
--
-- * @HealthCheckType ::@ @Maybe Text@
--
-- * @HealthCheckGracePeriod ::@ @Maybe Integer@
--
-- * @PlacementGroup ::@ @Maybe Text@
--
-- * @VPCZoneIdentifier ::@ @Maybe Text@
--
-- * @TerminationPolicies ::@ @[Text]@
--
-- * @Tags ::@ @[Tag]@
--
createAutoScalingGroup :: Text -- ^ 'casgAutoScalingGroupName'
                       -> Integer -- ^ 'casgMinSize'
                       -> Integer -- ^ 'casgMaxSize'
                       -> CreateAutoScalingGroup
createAutoScalingGroup p1 p4 p5 = CreateAutoScalingGroup
    { _casgAutoScalingGroupName = p1
    , _casgLaunchConfigurationName = Nothing
    , _casgInstanceId = Nothing
    , _casgMinSize = p4
    , _casgMaxSize = p5
    , _casgDesiredCapacity = Nothing
    , _casgDefaultCooldown = Nothing
    , _casgAvailabilityZones = Nothing
    , _casgLoadBalancerNames = mempty
    , _casgHealthCheckType = Nothing
    , _casgHealthCheckGracePeriod = Nothing
    , _casgPlacementGroup = Nothing
    , _casgVPCZoneIdentifier = Nothing
    , _casgTerminationPolicies = mempty
    , _casgTags = mempty
    }

-- | The name of the Auto Scaling group.
casgAutoScalingGroupName :: Lens' CreateAutoScalingGroup Text
casgAutoScalingGroupName =
    lens _casgAutoScalingGroupName
         (\s a -> s { _casgAutoScalingGroupName = a })

-- | The name of an existing launch configuration to use to launch new
-- instances. Use this attribute if you want to create an Auto Scaling group
-- using an existing launch configuration instead of an EC2 instance.
casgLaunchConfigurationName :: Lens' CreateAutoScalingGroup (Maybe Text)
casgLaunchConfigurationName =
    lens _casgLaunchConfigurationName
         (\s a -> s { _casgLaunchConfigurationName = a })

-- | The ID of the Amazon EC2 instance you want to use to create the Auto
-- Scaling group. Use this attribute if you want to create an Auto Scaling
-- group using an EC2 instance instead of a launch configuration. When you use
-- an instance to create an Auto Scaling group, a new launch configuration is
-- first created and then associated with the Auto Scaling group. The new
-- launch configuration derives all its attributes from the instance that is
-- used to create the Auto Scaling group, with the exception of
-- BlockDeviceMapping. For more information, see Create an Auto Scaling Group
-- Using EC2 Instance in the Auto Scaling Developer Guide.
casgInstanceId :: Lens' CreateAutoScalingGroup (Maybe Text)
casgInstanceId = lens _casgInstanceId (\s a -> s { _casgInstanceId = a })

-- | The minimum size of the Auto Scaling group.
casgMinSize :: Lens' CreateAutoScalingGroup Integer
casgMinSize = lens _casgMinSize (\s a -> s { _casgMinSize = a })

-- | The maximum size of the Auto Scaling group.
casgMaxSize :: Lens' CreateAutoScalingGroup Integer
casgMaxSize = lens _casgMaxSize (\s a -> s { _casgMaxSize = a })

-- | The number of Amazon EC2 instances that should be running in the group. The
-- desired capacity must be greater than or equal to the minimum size and less
-- than or equal to the maximum size specified for the Auto Scaling group.
casgDesiredCapacity :: Lens' CreateAutoScalingGroup (Maybe Integer)
casgDesiredCapacity =
    lens _casgDesiredCapacity (\s a -> s { _casgDesiredCapacity = a })

-- | The amount of time, in seconds, between a successful scaling activity and
-- the succeeding scaling activity. If a DefaultCooldown period is not
-- specified, Auto Scaling uses the default value of 300 as the default cool
-- down period for the Auto Scaling group. For more information, see Cooldown
-- Period.
casgDefaultCooldown :: Lens' CreateAutoScalingGroup (Maybe Integer)
casgDefaultCooldown =
    lens _casgDefaultCooldown (\s a -> s { _casgDefaultCooldown = a })

-- | A list of Availability Zones for the Auto Scaling group. This is required
-- unless you have specified subnets.
casgAvailabilityZones :: Lens' CreateAutoScalingGroup (Maybe (List1 Text))
casgAvailabilityZones =
    lens _casgAvailabilityZones (\s a -> s { _casgAvailabilityZones = a })

-- | A list of existing Elastic Load Balancing load balancers to use. The load
-- balancers must be associated with the AWS account. For information on using
-- load balancers, see Load Balance Your Auto Scaling Group in the Auto
-- Scaling Developer Guide.
casgLoadBalancerNames :: Lens' CreateAutoScalingGroup [Text]
casgLoadBalancerNames =
    lens _casgLoadBalancerNames (\s a -> s { _casgLoadBalancerNames = a })

-- | The service you want the health checks from, Amazon EC2 or Elastic Load
-- Balancer. Valid values are EC2 or ELB. By default, the Auto Scaling health
-- check uses the results of Amazon EC2 instance status checks to determine
-- the health of an instance. For more information, see Health Check.
casgHealthCheckType :: Lens' CreateAutoScalingGroup (Maybe Text)
casgHealthCheckType =
    lens _casgHealthCheckType (\s a -> s { _casgHealthCheckType = a })

-- | Length of time in seconds after a new Amazon EC2 instance comes into
-- service that Auto Scaling starts checking its health. During this time any
-- health check failure for the that instance is ignored. This is required if
-- you are adding ELB health check. Frequently, new instances need to warm up,
-- briefly, before they can pass a health check. To provide ample warm-up
-- time, set the health check grace period of the group to match the expected
-- startup period of your application. For more information, see Add an
-- Elastic Load Balancing Health Check.
casgHealthCheckGracePeriod :: Lens' CreateAutoScalingGroup (Maybe Integer)
casgHealthCheckGracePeriod =
    lens _casgHealthCheckGracePeriod
         (\s a -> s { _casgHealthCheckGracePeriod = a })

-- | Physical location of an existing cluster placement group into which you
-- want to launch your instances. For information about cluster placement
-- group, see Using Cluster Instances.
casgPlacementGroup :: Lens' CreateAutoScalingGroup (Maybe Text)
casgPlacementGroup =
    lens _casgPlacementGroup (\s a -> s { _casgPlacementGroup = a })

-- | A comma-separated list of subnet identifiers of Amazon Virtual Private
-- Clouds (Amazon VPCs). If you specify subnets and Availability Zones with
-- this call, ensure that the subnets' Availability Zones match the
-- Availability Zones specified. For information on launching your Auto
-- Scaling group into Amazon VPC subnets, see Auto Scaling in Amazon Virtual
-- Private Cloud in the Auto Scaling Developer Guide .
casgVPCZoneIdentifier :: Lens' CreateAutoScalingGroup (Maybe Text)
casgVPCZoneIdentifier =
    lens _casgVPCZoneIdentifier (\s a -> s { _casgVPCZoneIdentifier = a })

-- | A standalone termination policy or a list of termination policies used to
-- select the instance to terminate. The policies are executed in the order
-- that they are listed. For more information on configuring a termination
-- policy for your Auto Scaling group, see Instance Termination Policy for
-- Your Auto Scaling Group in the Auto Scaling Developer Guide.
casgTerminationPolicies :: Lens' CreateAutoScalingGroup [Text]
casgTerminationPolicies =
    lens _casgTerminationPolicies
         (\s a -> s { _casgTerminationPolicies = a })

-- | The tag to be created or updated. Each tag should be defined by its
-- resource type, resource ID, key, value, and a propagate flag. Valid values:
-- key=value, value=value, propagate=true or false. Value and propagate are
-- optional parameters. For information about using tags, see Tag Your Auto
-- Scaling Groups and Amazon EC2 Instances in the Auto Scaling Developer
-- Guide.
casgTags :: Lens' CreateAutoScalingGroup [Tag]
casgTags = lens _casgTags (\s a -> s { _casgTags = a })

instance ToQuery CreateAutoScalingGroup where
    toQuery = genericQuery def

data CreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateAutoScalingGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
createAutoScalingGroupResponse :: CreateAutoScalingGroupResponse
createAutoScalingGroupResponse = CreateAutoScalingGroupResponse

instance AWSRequest CreateAutoScalingGroup where
    type Sv CreateAutoScalingGroup = AutoScaling
    type Rs CreateAutoScalingGroup = CreateAutoScalingGroupResponse

    request = post "CreateAutoScalingGroup"
    response _ = nullaryResponse CreateAutoScalingGroupResponse
