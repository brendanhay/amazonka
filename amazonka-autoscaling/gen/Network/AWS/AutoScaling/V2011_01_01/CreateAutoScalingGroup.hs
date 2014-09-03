{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.CreateAutoScalingGroup
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
module Network.AWS.AutoScaling.V2011_01_01.CreateAutoScalingGroup
    (
    -- * Request
      CreateAutoScalingGroup
    -- ** Request constructor
    , createAutoScalingGroup
    -- ** Request lenses
    , casgtMaxSize
    , casgtMinSize
    , casgtAutoScalingGroupName
    , casgtDesiredCapacity
    , casgtAvailabilityZones
    , casgtDefaultCooldown
    , casgtHealthCheckGracePeriod
    , casgtLoadBalancerNames
    , casgtLaunchConfigurationName
    , casgtTags
    , casgtTerminationPolicies
    , casgtInstanceId
    , casgtPlacementGroup
    , casgtVPCZoneIdentifier
    , casgtHealthCheckType

    -- * Response
    , CreateAutoScalingGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateAutoScalingGroup' request.
createAutoScalingGroup :: Integer -- ^ 'casgtMaxSize'
                       -> Integer -- ^ 'casgtMinSize'
                       -> Text -- ^ 'casgtAutoScalingGroupName'
                       -> CreateAutoScalingGroup
createAutoScalingGroup p1 p2 p3 = CreateAutoScalingGroup
    { _casgtMaxSize = p1
    , _casgtMinSize = p2
    , _casgtAutoScalingGroupName = p3
    , _casgtDesiredCapacity = Nothing
    , _casgtAvailabilityZones = Nothing
    , _casgtDefaultCooldown = Nothing
    , _casgtHealthCheckGracePeriod = Nothing
    , _casgtLoadBalancerNames = mempty
    , _casgtLaunchConfigurationName = Nothing
    , _casgtTags = mempty
    , _casgtTerminationPolicies = mempty
    , _casgtInstanceId = Nothing
    , _casgtPlacementGroup = Nothing
    , _casgtVPCZoneIdentifier = Nothing
    , _casgtHealthCheckType = Nothing
    }

data CreateAutoScalingGroup = CreateAutoScalingGroup
    { _casgtMaxSize :: Integer
      -- ^ The maximum size of the Auto Scaling group.
    , _casgtMinSize :: Integer
      -- ^ The minimum size of the Auto Scaling group.
    , _casgtAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group.
    , _casgtDesiredCapacity :: Maybe Integer
      -- ^ The number of Amazon EC2 instances that should be running in the
      -- group. The desired capacity must be greater than or equal to the
      -- minimum size and less than or equal to the maximum size specified
      -- for the Auto Scaling group.
    , _casgtAvailabilityZones :: Maybe [Text]
      -- ^ A list of Availability Zones for the Auto Scaling group. This is
      -- required unless you have specified subnets.
    , _casgtDefaultCooldown :: Maybe Integer
      -- ^ The amount of time, in seconds, between a successful scaling
      -- activity and the succeeding scaling activity. If a
      -- DefaultCooldown period is not specified, Auto Scaling uses the
      -- default value of 300 as the default cool down period for the Auto
      -- Scaling group. For more information, see Cooldown Period.
    , _casgtHealthCheckGracePeriod :: Maybe Integer
      -- ^ Length of time in seconds after a new Amazon EC2 instance comes
      -- into service that Auto Scaling starts checking its health. During
      -- this time any health check failure for the that instance is
      -- ignored. This is required if you are adding ELB health check.
      -- Frequently, new instances need to warm up, briefly, before they
      -- can pass a health check. To provide ample warm-up time, set the
      -- health check grace period of the group to match the expected
      -- startup period of your application. For more information, see Add
      -- an Elastic Load Balancing Health Check.
    , _casgtLoadBalancerNames :: [Text]
      -- ^ A list of existing Elastic Load Balancing load balancers to use.
      -- The load balancers must be associated with the AWS account. For
      -- information on using load balancers, see Load Balance Your Auto
      -- Scaling Group in the Auto Scaling Developer Guide.
    , _casgtLaunchConfigurationName :: Maybe Text
      -- ^ The name of an existing launch configuration to use to launch new
      -- instances. Use this attribute if you want to create an Auto
      -- Scaling group using an existing launch configuration instead of
      -- an EC2 instance.
    , _casgtTags :: [Tag]
      -- ^ The tag to be created or updated. Each tag should be defined by
      -- its resource type, resource ID, key, value, and a propagate flag.
      -- Valid values: key=value, value=value, propagate=true or false.
      -- Value and propagate are optional parameters. For information
      -- about using tags, see Tag Your Auto Scaling Groups and Amazon EC2
      -- Instances in the Auto Scaling Developer Guide.
    , _casgtTerminationPolicies :: [Text]
      -- ^ A standalone termination policy or a list of termination policies
      -- used to select the instance to terminate. The policies are
      -- executed in the order that they are listed. For more information
      -- on configuring a termination policy for your Auto Scaling group,
      -- see Instance Termination Policy for Your Auto Scaling Group in
      -- the Auto Scaling Developer Guide.
    , _casgtInstanceId :: Maybe Text
      -- ^ The ID of the Amazon EC2 instance you want to use to create the
      -- Auto Scaling group. Use this attribute if you want to create an
      -- Auto Scaling group using an EC2 instance instead of a launch
      -- configuration. When you use an instance to create an Auto Scaling
      -- group, a new launch configuration is first created and then
      -- associated with the Auto Scaling group. The new launch
      -- configuration derives all its attributes from the instance that
      -- is used to create the Auto Scaling group, with the exception of
      -- BlockDeviceMapping. For more information, see Create an Auto
      -- Scaling Group Using EC2 Instance in the Auto Scaling Developer
      -- Guide.
    , _casgtPlacementGroup :: Maybe Text
      -- ^ Physical location of an existing cluster placement group into
      -- which you want to launch your instances. For information about
      -- cluster placement group, see Using Cluster Instances.
    , _casgtVPCZoneIdentifier :: Maybe Text
      -- ^ A comma-separated list of subnet identifiers of Amazon Virtual
      -- Private Clouds (Amazon VPCs). If you specify subnets and
      -- Availability Zones with this call, ensure that the subnets'
      -- Availability Zones match the Availability Zones specified. For
      -- information on launching your Auto Scaling group into Amazon VPC
      -- subnets, see Auto Scaling in Amazon Virtual Private Cloud in the
      -- Auto Scaling Developer Guide .
    , _casgtHealthCheckType :: Maybe Text
      -- ^ The service you want the health checks from, Amazon EC2 or
      -- Elastic Load Balancer. Valid values are EC2 or ELB. By default,
      -- the Auto Scaling health check uses the results of Amazon EC2
      -- instance status checks to determine the health of an instance.
      -- For more information, see Health Check.
    } deriving (Show, Generic)

-- | The maximum size of the Auto Scaling group.
casgtMaxSize
    :: Functor f
    => (Integer
    -> f (Integer))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtMaxSize f x =
    (\y -> x { _casgtMaxSize = y })
       <$> f (_casgtMaxSize x)
{-# INLINE casgtMaxSize #-}

-- | The minimum size of the Auto Scaling group.
casgtMinSize
    :: Functor f
    => (Integer
    -> f (Integer))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtMinSize f x =
    (\y -> x { _casgtMinSize = y })
       <$> f (_casgtMinSize x)
{-# INLINE casgtMinSize #-}

-- | The name of the Auto Scaling group.
casgtAutoScalingGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtAutoScalingGroupName f x =
    (\y -> x { _casgtAutoScalingGroupName = y })
       <$> f (_casgtAutoScalingGroupName x)
{-# INLINE casgtAutoScalingGroupName #-}

-- | The number of Amazon EC2 instances that should be running in the group. The
-- desired capacity must be greater than or equal to the minimum size and less
-- than or equal to the maximum size specified for the Auto Scaling group.
casgtDesiredCapacity
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtDesiredCapacity f x =
    (\y -> x { _casgtDesiredCapacity = y })
       <$> f (_casgtDesiredCapacity x)
{-# INLINE casgtDesiredCapacity #-}

-- | A list of Availability Zones for the Auto Scaling group. This is required
-- unless you have specified subnets.
casgtAvailabilityZones
    :: Functor f
    => (Maybe [Text]
    -> f (Maybe [Text]))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtAvailabilityZones f x =
    (\y -> x { _casgtAvailabilityZones = y })
       <$> f (_casgtAvailabilityZones x)
{-# INLINE casgtAvailabilityZones #-}

-- | The amount of time, in seconds, between a successful scaling activity and
-- the succeeding scaling activity. If a DefaultCooldown period is not
-- specified, Auto Scaling uses the default value of 300 as the default cool
-- down period for the Auto Scaling group. For more information, see Cooldown
-- Period.
casgtDefaultCooldown
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtDefaultCooldown f x =
    (\y -> x { _casgtDefaultCooldown = y })
       <$> f (_casgtDefaultCooldown x)
{-# INLINE casgtDefaultCooldown #-}

-- | Length of time in seconds after a new Amazon EC2 instance comes into
-- service that Auto Scaling starts checking its health. During this time any
-- health check failure for the that instance is ignored. This is required if
-- you are adding ELB health check. Frequently, new instances need to warm up,
-- briefly, before they can pass a health check. To provide ample warm-up
-- time, set the health check grace period of the group to match the expected
-- startup period of your application. For more information, see Add an
-- Elastic Load Balancing Health Check.
casgtHealthCheckGracePeriod
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtHealthCheckGracePeriod f x =
    (\y -> x { _casgtHealthCheckGracePeriod = y })
       <$> f (_casgtHealthCheckGracePeriod x)
{-# INLINE casgtHealthCheckGracePeriod #-}

-- | A list of existing Elastic Load Balancing load balancers to use. The load
-- balancers must be associated with the AWS account. For information on using
-- load balancers, see Load Balance Your Auto Scaling Group in the Auto
-- Scaling Developer Guide.
casgtLoadBalancerNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtLoadBalancerNames f x =
    (\y -> x { _casgtLoadBalancerNames = y })
       <$> f (_casgtLoadBalancerNames x)
{-# INLINE casgtLoadBalancerNames #-}

-- | The name of an existing launch configuration to use to launch new
-- instances. Use this attribute if you want to create an Auto Scaling group
-- using an existing launch configuration instead of an EC2 instance.
casgtLaunchConfigurationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtLaunchConfigurationName f x =
    (\y -> x { _casgtLaunchConfigurationName = y })
       <$> f (_casgtLaunchConfigurationName x)
{-# INLINE casgtLaunchConfigurationName #-}

-- | The tag to be created or updated. Each tag should be defined by its
-- resource type, resource ID, key, value, and a propagate flag. Valid values:
-- key=value, value=value, propagate=true or false. Value and propagate are
-- optional parameters. For information about using tags, see Tag Your Auto
-- Scaling Groups and Amazon EC2 Instances in the Auto Scaling Developer
-- Guide.
casgtTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtTags f x =
    (\y -> x { _casgtTags = y })
       <$> f (_casgtTags x)
{-# INLINE casgtTags #-}

-- | A standalone termination policy or a list of termination policies used to
-- select the instance to terminate. The policies are executed in the order
-- that they are listed. For more information on configuring a termination
-- policy for your Auto Scaling group, see Instance Termination Policy for
-- Your Auto Scaling Group in the Auto Scaling Developer Guide.
casgtTerminationPolicies
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtTerminationPolicies f x =
    (\y -> x { _casgtTerminationPolicies = y })
       <$> f (_casgtTerminationPolicies x)
{-# INLINE casgtTerminationPolicies #-}

-- | The ID of the Amazon EC2 instance you want to use to create the Auto
-- Scaling group. Use this attribute if you want to create an Auto Scaling
-- group using an EC2 instance instead of a launch configuration. When you use
-- an instance to create an Auto Scaling group, a new launch configuration is
-- first created and then associated with the Auto Scaling group. The new
-- launch configuration derives all its attributes from the instance that is
-- used to create the Auto Scaling group, with the exception of
-- BlockDeviceMapping. For more information, see Create an Auto Scaling Group
-- Using EC2 Instance in the Auto Scaling Developer Guide.
casgtInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtInstanceId f x =
    (\y -> x { _casgtInstanceId = y })
       <$> f (_casgtInstanceId x)
{-# INLINE casgtInstanceId #-}

-- | Physical location of an existing cluster placement group into which you
-- want to launch your instances. For information about cluster placement
-- group, see Using Cluster Instances.
casgtPlacementGroup
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtPlacementGroup f x =
    (\y -> x { _casgtPlacementGroup = y })
       <$> f (_casgtPlacementGroup x)
{-# INLINE casgtPlacementGroup #-}

-- | A comma-separated list of subnet identifiers of Amazon Virtual Private
-- Clouds (Amazon VPCs). If you specify subnets and Availability Zones with
-- this call, ensure that the subnets' Availability Zones match the
-- Availability Zones specified. For information on launching your Auto
-- Scaling group into Amazon VPC subnets, see Auto Scaling in Amazon Virtual
-- Private Cloud in the Auto Scaling Developer Guide .
casgtVPCZoneIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtVPCZoneIdentifier f x =
    (\y -> x { _casgtVPCZoneIdentifier = y })
       <$> f (_casgtVPCZoneIdentifier x)
{-# INLINE casgtVPCZoneIdentifier #-}

-- | The service you want the health checks from, Amazon EC2 or Elastic Load
-- Balancer. Valid values are EC2 or ELB. By default, the Auto Scaling health
-- check uses the results of Amazon EC2 instance status checks to determine
-- the health of an instance. For more information, see Health Check.
casgtHealthCheckType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateAutoScalingGroup
    -> f CreateAutoScalingGroup
casgtHealthCheckType f x =
    (\y -> x { _casgtHealthCheckType = y })
       <$> f (_casgtHealthCheckType x)
{-# INLINE casgtHealthCheckType #-}

instance ToQuery CreateAutoScalingGroup where
    toQuery = genericQuery def

data CreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest CreateAutoScalingGroup where
    type Sv CreateAutoScalingGroup = AutoScaling
    type Rs CreateAutoScalingGroup = CreateAutoScalingGroupResponse

    request = post "CreateAutoScalingGroup"
    response _ = nullaryResponse CreateAutoScalingGroupResponse
