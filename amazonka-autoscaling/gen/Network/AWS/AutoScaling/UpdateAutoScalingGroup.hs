{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.UpdateAutoScalingGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for the specified Auto Scaling group.
--
-- To update an Auto Scaling group with a launch configuration with
-- @InstanceMonitoring@ set to @False@, you must first disable the
-- collection of group metrics. Otherwise, you will get an error. If you
-- have previously enabled the collection of group metrics, you can disable
-- it using DisableMetricsCollection.
--
-- The new settings are registered upon the completion of this call. Any
-- launch configuration settings take effect on any triggers after this
-- call returns. Scaling activities that are currently in progress aren\'t
-- affected.
--
-- Note the following:
--
-- -   If you specify a new value for @MinSize@ without specifying a value
--     for @DesiredCapacity@, and the new @MinSize@ is larger than the
--     current size of the group, we implicitly call SetDesiredCapacity to
--     set the size of the group to the new value of @MinSize@.
--
-- -   If you specify a new value for @MaxSize@ without specifying a value
--     for @DesiredCapacity@, and the new @MaxSize@ is smaller than the
--     current size of the group, we implicitly call SetDesiredCapacity to
--     set the size of the group to the new value of @MaxSize@.
--
-- -   All other optional parameters are left unchanged if not specified.
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
    , uasgrqTerminationPolicies
    , uasgrqHealthCheckGracePeriod
    , uasgrqVPCZoneIdentifier
    , uasgrqDefaultCooldown
    , uasgrqMaxSize
    , uasgrqDesiredCapacity
    , uasgrqAvailabilityZones
    , uasgrqMinSize
    , uasgrqHealthCheckType
    , uasgrqLaunchConfigurationName
    , uasgrqPlacementGroup
    , uasgrqAutoScalingGroupName

    -- * Response
    , UpdateAutoScalingGroupResponse
    -- ** Response constructor
    , updateAutoScalingGroupResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateAutoScalingGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uasgrqTerminationPolicies'
--
-- * 'uasgrqHealthCheckGracePeriod'
--
-- * 'uasgrqVPCZoneIdentifier'
--
-- * 'uasgrqDefaultCooldown'
--
-- * 'uasgrqMaxSize'
--
-- * 'uasgrqDesiredCapacity'
--
-- * 'uasgrqAvailabilityZones'
--
-- * 'uasgrqMinSize'
--
-- * 'uasgrqHealthCheckType'
--
-- * 'uasgrqLaunchConfigurationName'
--
-- * 'uasgrqPlacementGroup'
--
-- * 'uasgrqAutoScalingGroupName'
data UpdateAutoScalingGroup = UpdateAutoScalingGroup'
    { _uasgrqTerminationPolicies     :: !(Maybe [Text])
    , _uasgrqHealthCheckGracePeriod  :: !(Maybe Int)
    , _uasgrqVPCZoneIdentifier       :: !(Maybe Text)
    , _uasgrqDefaultCooldown         :: !(Maybe Int)
    , _uasgrqMaxSize                 :: !(Maybe Int)
    , _uasgrqDesiredCapacity         :: !(Maybe Int)
    , _uasgrqAvailabilityZones       :: !(Maybe (List1 Text))
    , _uasgrqMinSize                 :: !(Maybe Int)
    , _uasgrqHealthCheckType         :: !(Maybe Text)
    , _uasgrqLaunchConfigurationName :: !(Maybe Text)
    , _uasgrqPlacementGroup          :: !(Maybe Text)
    , _uasgrqAutoScalingGroupName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAutoScalingGroup' smart constructor.
updateAutoScalingGroup :: Text -> UpdateAutoScalingGroup
updateAutoScalingGroup pAutoScalingGroupName =
    UpdateAutoScalingGroup'
    { _uasgrqTerminationPolicies = Nothing
    , _uasgrqHealthCheckGracePeriod = Nothing
    , _uasgrqVPCZoneIdentifier = Nothing
    , _uasgrqDefaultCooldown = Nothing
    , _uasgrqMaxSize = Nothing
    , _uasgrqDesiredCapacity = Nothing
    , _uasgrqAvailabilityZones = Nothing
    , _uasgrqMinSize = Nothing
    , _uasgrqHealthCheckType = Nothing
    , _uasgrqLaunchConfigurationName = Nothing
    , _uasgrqPlacementGroup = Nothing
    , _uasgrqAutoScalingGroupName = pAutoScalingGroupName
    }

-- | A standalone termination policy or a list of termination policies used
-- to select the instance to terminate. The policies are executed in the
-- order that they are listed.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/us-termination-policy.html Choosing a Termination Policy for Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
uasgrqTerminationPolicies :: Lens' UpdateAutoScalingGroup [Text]
uasgrqTerminationPolicies = lens _uasgrqTerminationPolicies (\ s a -> s{_uasgrqTerminationPolicies = a}) . _Default;

-- | The amount of time, in seconds, that Auto Scaling waits before checking
-- the health status of an instance. The grace period begins when the
-- instance passes the system status and instance status checks from Amazon
-- EC2. For more information, see < >.
uasgrqHealthCheckGracePeriod :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgrqHealthCheckGracePeriod = lens _uasgrqHealthCheckGracePeriod (\ s a -> s{_uasgrqHealthCheckGracePeriod = a});

-- | The ID of the subnet, if you are launching into a VPC. You can specify
-- several subnets in a comma-separated list.
--
-- When you specify @VPCZoneIdentifier@ with @AvailabilityZones@, ensure
-- that the subnets\' Availability Zones match the values you specify for
-- @AvailabilityZones@.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/autoscalingsubnets.html Auto Scaling and Amazon Virtual Private Cloud>
-- in the /Auto Scaling Developer Guide/.
uasgrqVPCZoneIdentifier :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgrqVPCZoneIdentifier = lens _uasgrqVPCZoneIdentifier (\ s a -> s{_uasgrqVPCZoneIdentifier = a});

-- | The amount of time, in seconds, after a scaling activity completes
-- before another scaling activity can start. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/Cooldown.html Understanding Auto Scaling Cooldowns>.
uasgrqDefaultCooldown :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgrqDefaultCooldown = lens _uasgrqDefaultCooldown (\ s a -> s{_uasgrqDefaultCooldown = a});

-- | The maximum size of the Auto Scaling group.
uasgrqMaxSize :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgrqMaxSize = lens _uasgrqMaxSize (\ s a -> s{_uasgrqMaxSize = a});

-- | The number of EC2 instances that should be running in the Auto Scaling
-- group. This number must be greater than or equal to the minimum size of
-- the group and less than or equal to the maximum size of the group.
uasgrqDesiredCapacity :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgrqDesiredCapacity = lens _uasgrqDesiredCapacity (\ s a -> s{_uasgrqDesiredCapacity = a});

-- | One or more Availability Zones for the group.
uasgrqAvailabilityZones :: Lens' UpdateAutoScalingGroup (Maybe (NonEmpty Text))
uasgrqAvailabilityZones = lens _uasgrqAvailabilityZones (\ s a -> s{_uasgrqAvailabilityZones = a}) . mapping _List1;

-- | The minimum size of the Auto Scaling group.
uasgrqMinSize :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgrqMinSize = lens _uasgrqMinSize (\ s a -> s{_uasgrqMinSize = a});

-- | The type of health check for the instances in the Auto Scaling group.
-- The health check type can either be @EC2@ for Amazon EC2 or @ELB@ for
-- Elastic Load Balancing.
uasgrqHealthCheckType :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgrqHealthCheckType = lens _uasgrqHealthCheckType (\ s a -> s{_uasgrqHealthCheckType = a});

-- | The name of the launch configuration.
uasgrqLaunchConfigurationName :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgrqLaunchConfigurationName = lens _uasgrqLaunchConfigurationName (\ s a -> s{_uasgrqLaunchConfigurationName = a});

-- | The name of the placement group into which you\'ll launch your
-- instances, if any. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>.
uasgrqPlacementGroup :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgrqPlacementGroup = lens _uasgrqPlacementGroup (\ s a -> s{_uasgrqPlacementGroup = a});

-- | The name of the Auto Scaling group.
uasgrqAutoScalingGroupName :: Lens' UpdateAutoScalingGroup Text
uasgrqAutoScalingGroupName = lens _uasgrqAutoScalingGroupName (\ s a -> s{_uasgrqAutoScalingGroupName = a});

instance AWSRequest UpdateAutoScalingGroup where
        type Sv UpdateAutoScalingGroup = AutoScaling
        type Rs UpdateAutoScalingGroup =
             UpdateAutoScalingGroupResponse
        request = post
        response
          = receiveNull UpdateAutoScalingGroupResponse'

instance ToHeaders UpdateAutoScalingGroup where
        toHeaders = const mempty

instance ToPath UpdateAutoScalingGroup where
        toPath = const "/"

instance ToQuery UpdateAutoScalingGroup where
        toQuery UpdateAutoScalingGroup'{..}
          = mconcat
              ["Action" =:
                 ("UpdateAutoScalingGroup" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "TerminationPolicies" =:
                 toQuery
                   (toQueryList "member" <$>
                      _uasgrqTerminationPolicies),
               "HealthCheckGracePeriod" =:
                 _uasgrqHealthCheckGracePeriod,
               "VPCZoneIdentifier" =: _uasgrqVPCZoneIdentifier,
               "DefaultCooldown" =: _uasgrqDefaultCooldown,
               "MaxSize" =: _uasgrqMaxSize,
               "DesiredCapacity" =: _uasgrqDesiredCapacity,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "member" <$> _uasgrqAvailabilityZones),
               "MinSize" =: _uasgrqMinSize,
               "HealthCheckType" =: _uasgrqHealthCheckType,
               "LaunchConfigurationName" =:
                 _uasgrqLaunchConfigurationName,
               "PlacementGroup" =: _uasgrqPlacementGroup,
               "AutoScalingGroupName" =:
                 _uasgrqAutoScalingGroupName]

-- | /See:/ 'updateAutoScalingGroupResponse' smart constructor.
data UpdateAutoScalingGroupResponse =
    UpdateAutoScalingGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAutoScalingGroupResponse' smart constructor.
updateAutoScalingGroupResponse :: UpdateAutoScalingGroupResponse
updateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse'
