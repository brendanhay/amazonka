{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.UpdateAutoScalingGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the configuration for the specified AutoScalingGroup. To update an
-- Auto Scaling group with a launch configuration that has the
-- InstanceMonitoring flag set to False, you must first ensure that collection
-- of group metrics is disabled. Otherwise, calls to UpdateAutoScalingGroup
-- will fail. If you have previously enabled group metrics collection, you can
-- disable collection of all group metrics by calling
-- DisableMetricsCollection. The new settings are registered upon the
-- completion of this call. Any launch configuration settings take effect on
-- any triggers after this call returns. Scaling activities that are currently
-- in progress aren't affected. If a new value is specified for MinSize
-- without specifying the value for DesiredCapacity, and if the new MinSize is
-- larger than the current size of the Auto Scaling Group, there will be an
-- implicit call to SetDesiredCapacity to set the group to the new MinSize. If
-- a new value is specified for MaxSize without specifying the value for
-- DesiredCapacity, and the new MaxSize is smaller than the current size of
-- the Auto Scaling Group, there will be an implicit call to
-- SetDesiredCapacity to set the group to the new MaxSize. All other optional
-- parameters are left unchanged if not passed in the request. Update existing
-- Auto Scaling group with ELB health check
-- https://autoscaling.amazonaws.com/?HealthCheckType=ELB
-- &HealthCheckGracePeriod=300 &AutoScalingGroupName=my-test-asg-lbs
-- &Version=2011-01-01 &Action=UpdateAutoScalingGroup &AUTHPARAMS
-- adafead0-ab8a-11e2-ba13-ab0ccEXAMPLE Update existing Auto Scaling group
-- with a new Availability Zone
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg-lbs
-- &AvailabilityZones.member.1=us-east-1a
-- &AvailabilityZones.member.2=us-east-1b
-- &AvailabilityZones.member.3=us-east-1c &MinSize=3 &Version=2011-01-01
-- &Action=UpdateAutoScalingGroup &AUTHPARAMS
-- adafead0-ab8a-11e2-ba13-ab0ccEXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.UpdateAutoScalingGroup
    (
    -- * Request
      UpdateAutoScalingGroup
    -- ** Request constructor
    , updateAutoScalingGroup
    -- ** Request lenses
    , uasgtAutoScalingGroupName
    , uasgtDesiredCapacity
    , uasgtMaxSize
    , uasgtMinSize
    , uasgtAvailabilityZones
    , uasgtDefaultCooldown
    , uasgtHealthCheckGracePeriod
    , uasgtLaunchConfigurationName
    , uasgtTerminationPolicies
    , uasgtPlacementGroup
    , uasgtVPCZoneIdentifier
    , uasgtHealthCheckType

    -- * Response
    , UpdateAutoScalingGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateAutoScalingGroup' request.
updateAutoScalingGroup :: Text -- ^ 'uasgtAutoScalingGroupName'
                       -> UpdateAutoScalingGroup
updateAutoScalingGroup p1 = UpdateAutoScalingGroup
    { _uasgtAutoScalingGroupName = p1
    , _uasgtDesiredCapacity = Nothing
    , _uasgtMaxSize = Nothing
    , _uasgtMinSize = Nothing
    , _uasgtAvailabilityZones = Nothing
    , _uasgtDefaultCooldown = Nothing
    , _uasgtHealthCheckGracePeriod = Nothing
    , _uasgtLaunchConfigurationName = Nothing
    , _uasgtTerminationPolicies = mempty
    , _uasgtPlacementGroup = Nothing
    , _uasgtVPCZoneIdentifier = Nothing
    , _uasgtHealthCheckType = Nothing
    }

data UpdateAutoScalingGroup = UpdateAutoScalingGroup
    { _uasgtAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group.
    , _uasgtDesiredCapacity :: Maybe Integer
      -- ^ The desired capacity for the Auto Scaling group.
    , _uasgtMaxSize :: Maybe Integer
      -- ^ The maximum size of the Auto Scaling group.
    , _uasgtMinSize :: Maybe Integer
      -- ^ The minimum size of the Auto Scaling group.
    , _uasgtAvailabilityZones :: Maybe [Text]
      -- ^ Availability Zones for the group.
    , _uasgtDefaultCooldown :: Maybe Integer
      -- ^ The amount of time, in seconds, after a scaling activity
      -- completes before any further scaling activities can start. For
      -- more information, see Cooldown Period.
    , _uasgtHealthCheckGracePeriod :: Maybe Integer
      -- ^ The length of time that Auto Scaling waits before checking an
      -- instance's health status. The grace period begins when the
      -- instance passes System Status and the Instance Status checks from
      -- Amazon EC2. For more information, see DescribeInstanceStatus.
    , _uasgtLaunchConfigurationName :: Maybe Text
      -- ^ The name of the launch configuration.
    , _uasgtTerminationPolicies :: [Text]
      -- ^ A standalone termination policy or a list of termination policies
      -- used to select the instance to terminate. The policies are
      -- executed in the order that they are listed. For more information
      -- on creating a termination policy for your Auto Scaling group, go
      -- to Instance Termination Policy for Your Auto Scaling Group in the
      -- the Auto Scaling Developer Guide.
    , _uasgtPlacementGroup :: Maybe Text
      -- ^ The name of the cluster placement group, if applicable. For more
      -- information, go to Using Cluster Instances in the Amazon EC2 User
      -- Guide.
    , _uasgtVPCZoneIdentifier :: Maybe Text
      -- ^ The subnet identifier for the Amazon VPC connection, if
      -- applicable. You can specify several subnets in a comma-separated
      -- list. When you specify VPCZoneIdentifier with AvailabilityZones,
      -- ensure that the subnets' Availability Zones match the values you
      -- specify for AvailabilityZones. For more information on creating
      -- your Auto Scaling group in Amazon VPC by specifying subnets, see
      -- Launch Auto Scaling Instances into Amazon VPC in the the Auto
      -- Scaling Developer Guide.
    , _uasgtHealthCheckType :: Maybe Text
      -- ^ The type of health check for the instances in the Auto Scaling
      -- group. The health check type can either be EC2 for Amazon EC2 or
      -- ELB for Elastic Load Balancing.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
uasgtAutoScalingGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtAutoScalingGroupName f x =
    (\y -> x { _uasgtAutoScalingGroupName = y })
       <$> f (_uasgtAutoScalingGroupName x)
{-# INLINE uasgtAutoScalingGroupName #-}

-- | The desired capacity for the Auto Scaling group.
uasgtDesiredCapacity
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtDesiredCapacity f x =
    (\y -> x { _uasgtDesiredCapacity = y })
       <$> f (_uasgtDesiredCapacity x)
{-# INLINE uasgtDesiredCapacity #-}

-- | The maximum size of the Auto Scaling group.
uasgtMaxSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtMaxSize f x =
    (\y -> x { _uasgtMaxSize = y })
       <$> f (_uasgtMaxSize x)
{-# INLINE uasgtMaxSize #-}

-- | The minimum size of the Auto Scaling group.
uasgtMinSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtMinSize f x =
    (\y -> x { _uasgtMinSize = y })
       <$> f (_uasgtMinSize x)
{-# INLINE uasgtMinSize #-}

-- | Availability Zones for the group.
uasgtAvailabilityZones
    :: Functor f
    => (Maybe [Text]
    -> f (Maybe [Text]))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtAvailabilityZones f x =
    (\y -> x { _uasgtAvailabilityZones = y })
       <$> f (_uasgtAvailabilityZones x)
{-# INLINE uasgtAvailabilityZones #-}

-- | The amount of time, in seconds, after a scaling activity completes before
-- any further scaling activities can start. For more information, see
-- Cooldown Period.
uasgtDefaultCooldown
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtDefaultCooldown f x =
    (\y -> x { _uasgtDefaultCooldown = y })
       <$> f (_uasgtDefaultCooldown x)
{-# INLINE uasgtDefaultCooldown #-}

-- | The length of time that Auto Scaling waits before checking an instance's
-- health status. The grace period begins when the instance passes System
-- Status and the Instance Status checks from Amazon EC2. For more
-- information, see DescribeInstanceStatus.
uasgtHealthCheckGracePeriod
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtHealthCheckGracePeriod f x =
    (\y -> x { _uasgtHealthCheckGracePeriod = y })
       <$> f (_uasgtHealthCheckGracePeriod x)
{-# INLINE uasgtHealthCheckGracePeriod #-}

-- | The name of the launch configuration.
uasgtLaunchConfigurationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtLaunchConfigurationName f x =
    (\y -> x { _uasgtLaunchConfigurationName = y })
       <$> f (_uasgtLaunchConfigurationName x)
{-# INLINE uasgtLaunchConfigurationName #-}

-- | A standalone termination policy or a list of termination policies used to
-- select the instance to terminate. The policies are executed in the order
-- that they are listed. For more information on creating a termination policy
-- for your Auto Scaling group, go to Instance Termination Policy for Your
-- Auto Scaling Group in the the Auto Scaling Developer Guide.
uasgtTerminationPolicies
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtTerminationPolicies f x =
    (\y -> x { _uasgtTerminationPolicies = y })
       <$> f (_uasgtTerminationPolicies x)
{-# INLINE uasgtTerminationPolicies #-}

-- | The name of the cluster placement group, if applicable. For more
-- information, go to Using Cluster Instances in the Amazon EC2 User Guide.
uasgtPlacementGroup
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtPlacementGroup f x =
    (\y -> x { _uasgtPlacementGroup = y })
       <$> f (_uasgtPlacementGroup x)
{-# INLINE uasgtPlacementGroup #-}

-- | The subnet identifier for the Amazon VPC connection, if applicable. You can
-- specify several subnets in a comma-separated list. When you specify
-- VPCZoneIdentifier with AvailabilityZones, ensure that the subnets'
-- Availability Zones match the values you specify for AvailabilityZones. For
-- more information on creating your Auto Scaling group in Amazon VPC by
-- specifying subnets, see Launch Auto Scaling Instances into Amazon VPC in
-- the the Auto Scaling Developer Guide.
uasgtVPCZoneIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtVPCZoneIdentifier f x =
    (\y -> x { _uasgtVPCZoneIdentifier = y })
       <$> f (_uasgtVPCZoneIdentifier x)
{-# INLINE uasgtVPCZoneIdentifier #-}

-- | The type of health check for the instances in the Auto Scaling group. The
-- health check type can either be EC2 for Amazon EC2 or ELB for Elastic Load
-- Balancing.
uasgtHealthCheckType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateAutoScalingGroup
    -> f UpdateAutoScalingGroup
uasgtHealthCheckType f x =
    (\y -> x { _uasgtHealthCheckType = y })
       <$> f (_uasgtHealthCheckType x)
{-# INLINE uasgtHealthCheckType #-}

instance ToQuery UpdateAutoScalingGroup where
    toQuery = genericQuery def

data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateAutoScalingGroup where
    type Sv UpdateAutoScalingGroup = AutoScaling
    type Rs UpdateAutoScalingGroup = UpdateAutoScalingGroupResponse

    request = post "UpdateAutoScalingGroup"
    response _ = nullaryResponse UpdateAutoScalingGroupResponse
