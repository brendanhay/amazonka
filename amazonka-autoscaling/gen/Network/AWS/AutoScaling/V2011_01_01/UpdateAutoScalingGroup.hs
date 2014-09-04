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
    , mkUpdateAutoScalingGroupType
    -- ** Request lenses
    , uasgtAutoScalingGroupName
    , uasgtLaunchConfigurationName
    , uasgtMinSize
    , uasgtMaxSize
    , uasgtDesiredCapacity
    , uasgtDefaultCooldown
    , uasgtAvailabilityZones
    , uasgtHealthCheckType
    , uasgtHealthCheckGracePeriod
    , uasgtPlacementGroup
    , uasgtVPCZoneIdentifier
    , uasgtTerminationPolicies

    -- * Response
    , UpdateAutoScalingGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAutoScalingGroup' request.
mkUpdateAutoScalingGroupType :: Text -- ^ 'uasgtAutoScalingGroupName'
                             -> UpdateAutoScalingGroup
mkUpdateAutoScalingGroupType p1 = UpdateAutoScalingGroup
    { _uasgtAutoScalingGroupName = p1
    , _uasgtLaunchConfigurationName = Nothing
    , _uasgtMinSize = Nothing
    , _uasgtMaxSize = Nothing
    , _uasgtDesiredCapacity = Nothing
    , _uasgtDefaultCooldown = Nothing
    , _uasgtAvailabilityZones = Nothing
    , _uasgtHealthCheckType = Nothing
    , _uasgtHealthCheckGracePeriod = Nothing
    , _uasgtPlacementGroup = Nothing
    , _uasgtVPCZoneIdentifier = Nothing
    , _uasgtTerminationPolicies = mempty
    }
{-# INLINE mkUpdateAutoScalingGroupType #-}

data UpdateAutoScalingGroup = UpdateAutoScalingGroup
    { _uasgtAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group.
    , _uasgtLaunchConfigurationName :: Maybe Text
      -- ^ The name of the launch configuration.
    , _uasgtMinSize :: Maybe Integer
      -- ^ The minimum size of the Auto Scaling group.
    , _uasgtMaxSize :: Maybe Integer
      -- ^ The maximum size of the Auto Scaling group.
    , _uasgtDesiredCapacity :: Maybe Integer
      -- ^ The desired capacity for the Auto Scaling group.
    , _uasgtDefaultCooldown :: Maybe Integer
      -- ^ The amount of time, in seconds, after a scaling activity
      -- completes before any further scaling activities can start. For
      -- more information, see Cooldown Period.
    , _uasgtAvailabilityZones :: Maybe [Text]
      -- ^ Availability Zones for the group.
    , _uasgtHealthCheckType :: Maybe Text
      -- ^ The type of health check for the instances in the Auto Scaling
      -- group. The health check type can either be EC2 for Amazon EC2 or
      -- ELB for Elastic Load Balancing.
    , _uasgtHealthCheckGracePeriod :: Maybe Integer
      -- ^ The length of time that Auto Scaling waits before checking an
      -- instance's health status. The grace period begins when the
      -- instance passes System Status and the Instance Status checks from
      -- Amazon EC2. For more information, see DescribeInstanceStatus.
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
    , _uasgtTerminationPolicies :: [Text]
      -- ^ A standalone termination policy or a list of termination policies
      -- used to select the instance to terminate. The policies are
      -- executed in the order that they are listed. For more information
      -- on creating a termination policy for your Auto Scaling group, go
      -- to Instance Termination Policy for Your Auto Scaling Group in the
      -- the Auto Scaling Developer Guide.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
uasgtAutoScalingGroupName :: Lens' UpdateAutoScalingGroup (Text)
uasgtAutoScalingGroupName = lens _uasgtAutoScalingGroupName (\s a -> s { _uasgtAutoScalingGroupName = a })
{-# INLINE uasgtAutoScalingGroupName #-}

-- | The name of the launch configuration.
uasgtLaunchConfigurationName :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgtLaunchConfigurationName = lens _uasgtLaunchConfigurationName (\s a -> s { _uasgtLaunchConfigurationName = a })
{-# INLINE uasgtLaunchConfigurationName #-}

-- | The minimum size of the Auto Scaling group.
uasgtMinSize :: Lens' UpdateAutoScalingGroup (Maybe Integer)
uasgtMinSize = lens _uasgtMinSize (\s a -> s { _uasgtMinSize = a })
{-# INLINE uasgtMinSize #-}

-- | The maximum size of the Auto Scaling group.
uasgtMaxSize :: Lens' UpdateAutoScalingGroup (Maybe Integer)
uasgtMaxSize = lens _uasgtMaxSize (\s a -> s { _uasgtMaxSize = a })
{-# INLINE uasgtMaxSize #-}

-- | The desired capacity for the Auto Scaling group.
uasgtDesiredCapacity :: Lens' UpdateAutoScalingGroup (Maybe Integer)
uasgtDesiredCapacity = lens _uasgtDesiredCapacity (\s a -> s { _uasgtDesiredCapacity = a })
{-# INLINE uasgtDesiredCapacity #-}

-- | The amount of time, in seconds, after a scaling activity completes before
-- any further scaling activities can start. For more information, see
-- Cooldown Period.
uasgtDefaultCooldown :: Lens' UpdateAutoScalingGroup (Maybe Integer)
uasgtDefaultCooldown = lens _uasgtDefaultCooldown (\s a -> s { _uasgtDefaultCooldown = a })
{-# INLINE uasgtDefaultCooldown #-}

-- | Availability Zones for the group.
uasgtAvailabilityZones :: Lens' UpdateAutoScalingGroup (Maybe [Text])
uasgtAvailabilityZones = lens _uasgtAvailabilityZones (\s a -> s { _uasgtAvailabilityZones = a })
{-# INLINE uasgtAvailabilityZones #-}

-- | The type of health check for the instances in the Auto Scaling group. The
-- health check type can either be EC2 for Amazon EC2 or ELB for Elastic Load
-- Balancing.
uasgtHealthCheckType :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgtHealthCheckType = lens _uasgtHealthCheckType (\s a -> s { _uasgtHealthCheckType = a })
{-# INLINE uasgtHealthCheckType #-}

-- | The length of time that Auto Scaling waits before checking an instance's
-- health status. The grace period begins when the instance passes System
-- Status and the Instance Status checks from Amazon EC2. For more
-- information, see DescribeInstanceStatus.
uasgtHealthCheckGracePeriod :: Lens' UpdateAutoScalingGroup (Maybe Integer)
uasgtHealthCheckGracePeriod = lens _uasgtHealthCheckGracePeriod (\s a -> s { _uasgtHealthCheckGracePeriod = a })
{-# INLINE uasgtHealthCheckGracePeriod #-}

-- | The name of the cluster placement group, if applicable. For more
-- information, go to Using Cluster Instances in the Amazon EC2 User Guide.
uasgtPlacementGroup :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgtPlacementGroup = lens _uasgtPlacementGroup (\s a -> s { _uasgtPlacementGroup = a })
{-# INLINE uasgtPlacementGroup #-}

-- | The subnet identifier for the Amazon VPC connection, if applicable. You can
-- specify several subnets in a comma-separated list. When you specify
-- VPCZoneIdentifier with AvailabilityZones, ensure that the subnets'
-- Availability Zones match the values you specify for AvailabilityZones. For
-- more information on creating your Auto Scaling group in Amazon VPC by
-- specifying subnets, see Launch Auto Scaling Instances into Amazon VPC in
-- the the Auto Scaling Developer Guide.
uasgtVPCZoneIdentifier :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgtVPCZoneIdentifier = lens _uasgtVPCZoneIdentifier (\s a -> s { _uasgtVPCZoneIdentifier = a })
{-# INLINE uasgtVPCZoneIdentifier #-}

-- | A standalone termination policy or a list of termination policies used to
-- select the instance to terminate. The policies are executed in the order
-- that they are listed. For more information on creating a termination policy
-- for your Auto Scaling group, go to Instance Termination Policy for Your
-- Auto Scaling Group in the the Auto Scaling Developer Guide.
uasgtTerminationPolicies :: Lens' UpdateAutoScalingGroup ([Text])
uasgtTerminationPolicies = lens _uasgtTerminationPolicies (\s a -> s { _uasgtTerminationPolicies = a })
{-# INLINE uasgtTerminationPolicies #-}

instance ToQuery UpdateAutoScalingGroup where
    toQuery = genericQuery def

data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateAutoScalingGroup where
    type Sv UpdateAutoScalingGroup = AutoScaling
    type Rs UpdateAutoScalingGroup = UpdateAutoScalingGroupResponse

    request = post "UpdateAutoScalingGroup"
    response _ = nullaryResponse UpdateAutoScalingGroupResponse
