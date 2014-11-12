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

-- Module      : Network.AWS.AutoScaling.UpdateAutoScalingGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the configuration for the specified AutoScalingGroup. The new
-- settings are registered upon the completion of this call. Any launch
-- configuration settings take effect on any triggers after this call returns.
-- Scaling activities that are currently in progress aren't affected.
module Network.AWS.AutoScaling.UpdateAutoScalingGroup
    (
    -- * Request
      UpdateAutoScalingGroupType
    -- ** Request constructor
    , updateAutoScalingGroup
    -- ** Request lenses
    , uasgtAutoScalingGroupName
    , uasgtAvailabilityZones
    , uasgtDefaultCooldown
    , uasgtDesiredCapacity
    , uasgtHealthCheckGracePeriod
    , uasgtHealthCheckType
    , uasgtLaunchConfigurationName
    , uasgtMaxSize
    , uasgtMinSize
    , uasgtPlacementGroup
    , uasgtTerminationPolicies
    , uasgtVPCZoneIdentifier

    -- * Response
    , UpdateAutoScalingGroupResponse
    -- ** Response constructor
    , updateAutoScalingGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data UpdateAutoScalingGroupType = UpdateAutoScalingGroupType
    { _uasgtAutoScalingGroupName    :: Text
    , _uasgtAvailabilityZones       :: List1 Text
    , _uasgtDefaultCooldown         :: Maybe Int
    , _uasgtDesiredCapacity         :: Maybe Int
    , _uasgtHealthCheckGracePeriod  :: Maybe Int
    , _uasgtHealthCheckType         :: Maybe Text
    , _uasgtLaunchConfigurationName :: Maybe Text
    , _uasgtMaxSize                 :: Maybe Int
    , _uasgtMinSize                 :: Maybe Int
    , _uasgtPlacementGroup          :: Maybe Text
    , _uasgtTerminationPolicies     :: [Text]
    , _uasgtVPCZoneIdentifier       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateAutoScalingGroupType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uasgtAutoScalingGroupName' @::@ 'Text'
--
-- * 'uasgtAvailabilityZones' @::@ 'NonEmpty' 'Text'
--
-- * 'uasgtDefaultCooldown' @::@ 'Maybe' 'Int'
--
-- * 'uasgtDesiredCapacity' @::@ 'Maybe' 'Int'
--
-- * 'uasgtHealthCheckGracePeriod' @::@ 'Maybe' 'Int'
--
-- * 'uasgtHealthCheckType' @::@ 'Maybe' 'Text'
--
-- * 'uasgtLaunchConfigurationName' @::@ 'Maybe' 'Text'
--
-- * 'uasgtMaxSize' @::@ 'Maybe' 'Int'
--
-- * 'uasgtMinSize' @::@ 'Maybe' 'Int'
--
-- * 'uasgtPlacementGroup' @::@ 'Maybe' 'Text'
--
-- * 'uasgtTerminationPolicies' @::@ ['Text']
--
-- * 'uasgtVPCZoneIdentifier' @::@ 'Maybe' 'Text'
--
updateAutoScalingGroup :: Text -- ^ 'uasgtAutoScalingGroupName'
                       -> NonEmpty Text -- ^ 'uasgtAvailabilityZones'
                       -> UpdateAutoScalingGroupType
updateAutoScalingGroup p1 p2 = UpdateAutoScalingGroupType
    { _uasgtAutoScalingGroupName    = p1
    , _uasgtAvailabilityZones       = withIso _List1 (const id) p2
    , _uasgtLaunchConfigurationName = Nothing
    , _uasgtMinSize                 = Nothing
    , _uasgtMaxSize                 = Nothing
    , _uasgtDesiredCapacity         = Nothing
    , _uasgtDefaultCooldown         = Nothing
    , _uasgtHealthCheckType         = Nothing
    , _uasgtHealthCheckGracePeriod  = Nothing
    , _uasgtPlacementGroup          = Nothing
    , _uasgtVPCZoneIdentifier       = Nothing
    , _uasgtTerminationPolicies     = mempty
    }

-- | The name of the Auto Scaling group.
uasgtAutoScalingGroupName :: Lens' UpdateAutoScalingGroupType Text
uasgtAutoScalingGroupName =
    lens _uasgtAutoScalingGroupName
        (\s a -> s { _uasgtAutoScalingGroupName = a })

-- | Availability Zones for the group.
uasgtAvailabilityZones :: Lens' UpdateAutoScalingGroupType (NonEmpty Text)
uasgtAvailabilityZones =
    lens _uasgtAvailabilityZones (\s a -> s { _uasgtAvailabilityZones = a })
        . _List1

-- | The amount of time, in seconds, after a scaling activity completes before
-- any further scaling activities can start. For more information, see
-- Cooldown Period.
uasgtDefaultCooldown :: Lens' UpdateAutoScalingGroupType (Maybe Int)
uasgtDefaultCooldown =
    lens _uasgtDefaultCooldown (\s a -> s { _uasgtDefaultCooldown = a })

-- | The desired capacity for the Auto Scaling group.
uasgtDesiredCapacity :: Lens' UpdateAutoScalingGroupType (Maybe Int)
uasgtDesiredCapacity =
    lens _uasgtDesiredCapacity (\s a -> s { _uasgtDesiredCapacity = a })

-- | The length of time that Auto Scaling waits before checking an instance's
-- health status. The grace period begins when the instance passes System
-- Status and the Instance Status checks from Amazon EC2. For more
-- information, see DescribeInstanceStatus.
uasgtHealthCheckGracePeriod :: Lens' UpdateAutoScalingGroupType (Maybe Int)
uasgtHealthCheckGracePeriod =
    lens _uasgtHealthCheckGracePeriod
        (\s a -> s { _uasgtHealthCheckGracePeriod = a })

-- | The type of health check for the instances in the Auto Scaling group. The
-- health check type can either be EC2 for Amazon EC2 or ELB for Elastic
-- Load Balancing.
uasgtHealthCheckType :: Lens' UpdateAutoScalingGroupType (Maybe Text)
uasgtHealthCheckType =
    lens _uasgtHealthCheckType (\s a -> s { _uasgtHealthCheckType = a })

-- | The name of the launch configuration.
uasgtLaunchConfigurationName :: Lens' UpdateAutoScalingGroupType (Maybe Text)
uasgtLaunchConfigurationName =
    lens _uasgtLaunchConfigurationName
        (\s a -> s { _uasgtLaunchConfigurationName = a })

-- | The maximum size of the Auto Scaling group.
uasgtMaxSize :: Lens' UpdateAutoScalingGroupType (Maybe Int)
uasgtMaxSize = lens _uasgtMaxSize (\s a -> s { _uasgtMaxSize = a })

-- | The minimum size of the Auto Scaling group.
uasgtMinSize :: Lens' UpdateAutoScalingGroupType (Maybe Int)
uasgtMinSize = lens _uasgtMinSize (\s a -> s { _uasgtMinSize = a })

-- | The name of the cluster placement group, if applicable. For more
-- information, go to Using Cluster Instances in the Amazon EC2 User Guide.
uasgtPlacementGroup :: Lens' UpdateAutoScalingGroupType (Maybe Text)
uasgtPlacementGroup =
    lens _uasgtPlacementGroup (\s a -> s { _uasgtPlacementGroup = a })

-- | A standalone termination policy or a list of termination policies used to
-- select the instance to terminate. The policies are executed in the order
-- that they are listed. For more information on creating a termination
-- policy for your Auto Scaling group, go to Instance Termination Policy for
-- Your Auto Scaling Group in the the Auto Scaling Developer Guide.
uasgtTerminationPolicies :: Lens' UpdateAutoScalingGroupType [Text]
uasgtTerminationPolicies =
    lens _uasgtTerminationPolicies
        (\s a -> s { _uasgtTerminationPolicies = a })

-- | The subnet identifier for the Amazon VPC connection, if applicable. You
-- can specify several subnets in a comma-separated list. When you specify
-- VPCZoneIdentifier with AvailabilityZones, ensure that the subnets'
-- Availability Zones match the values you specify for AvailabilityZones.
-- For more information on creating your Auto Scaling group in Amazon VPC by
-- specifying subnets, see Launch Auto Scaling Instances into Amazon VPC in
-- the the Auto Scaling Developer Guide.
uasgtVPCZoneIdentifier :: Lens' UpdateAutoScalingGroupType (Maybe Text)
uasgtVPCZoneIdentifier =
    lens _uasgtVPCZoneIdentifier (\s a -> s { _uasgtVPCZoneIdentifier = a })

instance ToQuery UpdateAutoScalingGroupType

instance ToPath UpdateAutoScalingGroupType where
    toPath = const "/"

data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateAutoScalingGroupResponse' constructor.
updateAutoScalingGroupResponse :: UpdateAutoScalingGroupResponse
updateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse

instance FromXML UpdateAutoScalingGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UpdateAutoScalingGroupResponse"

instance AWSRequest UpdateAutoScalingGroupType where
    type Sv UpdateAutoScalingGroupType = AutoScaling
    type Rs UpdateAutoScalingGroupType = UpdateAutoScalingGroupResponse

    request  = post "UpdateAutoScalingGroup"
    response = nullaryResponse UpdateAutoScalingGroupResponse
