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

-- Module      : Network.AWS.AutoScaling.UpdateAutoScalingGroup
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

-- | Updates the configuration for the specified 'AutoScalingGroup'.
--
-- To update an Auto Scaling group with a launch configuration that has the 'InstanceMonitoring' flag set to 'False', you must first ensure that collection of group metrics is
-- disabled. Otherwise, calls to 'UpdateAutoScalingGroup' will fail. If you have
-- previously enabled group metrics collection, you can disable collection of
-- all group metrics by calling 'DisableMetricsCollection'.
--
-- The new settings are registered upon the completion of this call. Any
-- launch configuration settings take effect on any triggers after this call
-- returns. Scaling activities that are currently in progress aren't affected.
--
-- If a new value is specified for /MinSize/ without specifying the value for /DesiredCapacity/, and if the new /MinSize/ is larger than the current size of the Auto Scaling
-- group, there will be an implicit call to 'SetDesiredCapacity' to set the group
-- to the new /MinSize/.
--
-- If a new value is specified for /MaxSize/ without specifying the value for /DesiredCapacity/, and the new /MaxSize/ is smaller than the current size of the Auto Scaling
-- group, there will be an implicit call to 'SetDesiredCapacity' to set the group
-- to the new /MaxSize/.
--
-- All other optional parameters are left unchanged if not passed in the
-- request.
--
--
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_UpdateAutoScalingGroup.html>
module Network.AWS.AutoScaling.UpdateAutoScalingGroup
    (
    -- * Request
      UpdateAutoScalingGroup
    -- ** Request constructor
    , updateAutoScalingGroup
    -- ** Request lenses
    , uasgAutoScalingGroupName
    , uasgAvailabilityZones
    , uasgDefaultCooldown
    , uasgDesiredCapacity
    , uasgHealthCheckGracePeriod
    , uasgHealthCheckType
    , uasgLaunchConfigurationName
    , uasgMaxSize
    , uasgMinSize
    , uasgPlacementGroup
    , uasgTerminationPolicies
    , uasgVPCZoneIdentifier

    -- * Response
    , UpdateAutoScalingGroupResponse
    -- ** Response constructor
    , updateAutoScalingGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data UpdateAutoScalingGroup = UpdateAutoScalingGroup
    { _uasgAutoScalingGroupName    :: Text
    , _uasgAvailabilityZones       :: List1 "member" Text
    , _uasgDefaultCooldown         :: Maybe Int
    , _uasgDesiredCapacity         :: Maybe Int
    , _uasgHealthCheckGracePeriod  :: Maybe Int
    , _uasgHealthCheckType         :: Maybe Text
    , _uasgLaunchConfigurationName :: Maybe Text
    , _uasgMaxSize                 :: Maybe Int
    , _uasgMinSize                 :: Maybe Int
    , _uasgPlacementGroup          :: Maybe Text
    , _uasgTerminationPolicies     :: List "member" Text
    , _uasgVPCZoneIdentifier       :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UpdateAutoScalingGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uasgAutoScalingGroupName' @::@ 'Text'
--
-- * 'uasgAvailabilityZones' @::@ 'NonEmpty' 'Text'
--
-- * 'uasgDefaultCooldown' @::@ 'Maybe' 'Int'
--
-- * 'uasgDesiredCapacity' @::@ 'Maybe' 'Int'
--
-- * 'uasgHealthCheckGracePeriod' @::@ 'Maybe' 'Int'
--
-- * 'uasgHealthCheckType' @::@ 'Maybe' 'Text'
--
-- * 'uasgLaunchConfigurationName' @::@ 'Maybe' 'Text'
--
-- * 'uasgMaxSize' @::@ 'Maybe' 'Int'
--
-- * 'uasgMinSize' @::@ 'Maybe' 'Int'
--
-- * 'uasgPlacementGroup' @::@ 'Maybe' 'Text'
--
-- * 'uasgTerminationPolicies' @::@ ['Text']
--
-- * 'uasgVPCZoneIdentifier' @::@ 'Maybe' 'Text'
--
updateAutoScalingGroup :: Text -- ^ 'uasgAutoScalingGroupName'
                       -> NonEmpty Text -- ^ 'uasgAvailabilityZones'
                       -> UpdateAutoScalingGroup
updateAutoScalingGroup p1 p2 = UpdateAutoScalingGroup
    { _uasgAutoScalingGroupName    = p1
    , _uasgAvailabilityZones       = withIso _List1 (const id) p2
    , _uasgLaunchConfigurationName = Nothing
    , _uasgMinSize                 = Nothing
    , _uasgMaxSize                 = Nothing
    , _uasgDesiredCapacity         = Nothing
    , _uasgDefaultCooldown         = Nothing
    , _uasgHealthCheckType         = Nothing
    , _uasgHealthCheckGracePeriod  = Nothing
    , _uasgPlacementGroup          = Nothing
    , _uasgVPCZoneIdentifier       = Nothing
    , _uasgTerminationPolicies     = mempty
    }

-- | The name of the Auto Scaling group.
uasgAutoScalingGroupName :: Lens' UpdateAutoScalingGroup Text
uasgAutoScalingGroupName =
    lens _uasgAutoScalingGroupName
        (\s a -> s { _uasgAutoScalingGroupName = a })

-- | One or more Availability Zones for the group.
uasgAvailabilityZones :: Lens' UpdateAutoScalingGroup (NonEmpty Text)
uasgAvailabilityZones =
    lens _uasgAvailabilityZones (\s a -> s { _uasgAvailabilityZones = a })
        . _List1

-- | The amount of time, in seconds, after a scaling activity completes before
-- another scaling activity can start. For more information, see <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/Cooldown.html UnderstandingAuto Scaling Cooldowns>.
uasgDefaultCooldown :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgDefaultCooldown =
    lens _uasgDefaultCooldown (\s a -> s { _uasgDefaultCooldown = a })

-- | The number of EC2 instances that should be running in the Auto Scaling group.
-- This value must be greater than or equal to the minimum size of the group and
-- less than or equal to the maximum size of the group.
uasgDesiredCapacity :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgDesiredCapacity =
    lens _uasgDesiredCapacity (\s a -> s { _uasgDesiredCapacity = a })

-- | The amount of time, in second, that Auto Scaling waits before checking the
-- health status of an instance. The grace period begins when the instance
-- passes System Status and the Instance Status checks from Amazon EC2. For more
-- information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstanceStatus.html DescribeInstanceStatus>.
uasgHealthCheckGracePeriod :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgHealthCheckGracePeriod =
    lens _uasgHealthCheckGracePeriod
        (\s a -> s { _uasgHealthCheckGracePeriod = a })

-- | The type of health check for the instances in the Auto Scaling group. The
-- health check type can either be 'EC2' for Amazon EC2 or 'ELB' for Elastic Load
-- Balancing.
uasgHealthCheckType :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgHealthCheckType =
    lens _uasgHealthCheckType (\s a -> s { _uasgHealthCheckType = a })

-- | The name of the launch configuration.
uasgLaunchConfigurationName :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgLaunchConfigurationName =
    lens _uasgLaunchConfigurationName
        (\s a -> s { _uasgLaunchConfigurationName = a })

-- | The maximum size of the Auto Scaling group.
uasgMaxSize :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgMaxSize = lens _uasgMaxSize (\s a -> s { _uasgMaxSize = a })

-- | The minimum size of the Auto Scaling group.
uasgMinSize :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgMinSize = lens _uasgMinSize (\s a -> s { _uasgMinSize = a })

-- | The name of the placement group into which you'll launch your instances, if
-- any. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>.
uasgPlacementGroup :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgPlacementGroup =
    lens _uasgPlacementGroup (\s a -> s { _uasgPlacementGroup = a })

-- | A standalone termination policy or a list of termination policies used to
-- select the instance to terminate. The policies are executed in the order that
-- they are listed.
--
-- For more information, see <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/us-termination-policy.html Choosing a Termination Policy for Your AutoScaling Group> in the /Auto Scaling Developer Guide/.
uasgTerminationPolicies :: Lens' UpdateAutoScalingGroup [Text]
uasgTerminationPolicies =
    lens _uasgTerminationPolicies (\s a -> s { _uasgTerminationPolicies = a })
        . _List

-- | The subnet identifier for the Amazon VPC connection, if applicable. You can
-- specify several subnets in a comma-separated list.
--
-- When you specify 'VPCZoneIdentifier' with 'AvailabilityZones', ensure that the
-- subnets' Availability Zones match the values you specify for 'AvailabilityZones'
-- .
--
-- For more information, see <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/autoscalingsubnets.html Auto Scaling and Amazon VPC> in the /Auto ScalingDeveloper Guide/.
uasgVPCZoneIdentifier :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgVPCZoneIdentifier =
    lens _uasgVPCZoneIdentifier (\s a -> s { _uasgVPCZoneIdentifier = a })

data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'UpdateAutoScalingGroupResponse' constructor.
updateAutoScalingGroupResponse :: UpdateAutoScalingGroupResponse
updateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse

instance ToPath UpdateAutoScalingGroup where
    toPath = const "/"

instance ToQuery UpdateAutoScalingGroup where
    toQuery UpdateAutoScalingGroup{..} = mconcat
        [ "AutoScalingGroupName"    =? _uasgAutoScalingGroupName
        , "AvailabilityZones"       =? _uasgAvailabilityZones
        , "DefaultCooldown"         =? _uasgDefaultCooldown
        , "DesiredCapacity"         =? _uasgDesiredCapacity
        , "HealthCheckGracePeriod"  =? _uasgHealthCheckGracePeriod
        , "HealthCheckType"         =? _uasgHealthCheckType
        , "LaunchConfigurationName" =? _uasgLaunchConfigurationName
        , "MaxSize"                 =? _uasgMaxSize
        , "MinSize"                 =? _uasgMinSize
        , "PlacementGroup"          =? _uasgPlacementGroup
        , "TerminationPolicies"     =? _uasgTerminationPolicies
        , "VPCZoneIdentifier"       =? _uasgVPCZoneIdentifier
        ]

instance ToHeaders UpdateAutoScalingGroup

instance AWSRequest UpdateAutoScalingGroup where
    type Sv UpdateAutoScalingGroup = AutoScaling
    type Rs UpdateAutoScalingGroup = UpdateAutoScalingGroupResponse

    request  = post "UpdateAutoScalingGroup"
    response = nullResponse UpdateAutoScalingGroupResponse
