{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CreateAutoScalingGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an Auto Scaling group with the specified name and attributes.
--
-- If you exceed your maximum limit of Auto Scaling groups, which by
-- default is 20 per region, the call fails. For information about viewing
-- and updating this limit, see DescribeAccountLimits.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingGroup.html Auto Scaling Groups>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_CreateAutoScalingGroup.html>
module Network.AWS.AutoScaling.CreateAutoScalingGroup
    (
    -- * Request
      CreateAutoScalingGroup
    -- ** Request constructor
    , createAutoScalingGroup
    -- ** Request lenses
    , casgInstanceId
    , casgTerminationPolicies
    , casgHealthCheckGracePeriod
    , casgVPCZoneIdentifier
    , casgDefaultCooldown
    , casgDesiredCapacity
    , casgAvailabilityZones
    , casgHealthCheckType
    , casgLaunchConfigurationName
    , casgPlacementGroup
    , casgLoadBalancerNames
    , casgTags
    , casgAutoScalingGroupName
    , casgMinSize
    , casgMaxSize

    -- * Response
    , CreateAutoScalingGroupResponse
    -- ** Response constructor
    , createAutoScalingGroupResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createAutoScalingGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'casgInstanceId'
--
-- * 'casgTerminationPolicies'
--
-- * 'casgHealthCheckGracePeriod'
--
-- * 'casgVPCZoneIdentifier'
--
-- * 'casgDefaultCooldown'
--
-- * 'casgDesiredCapacity'
--
-- * 'casgAvailabilityZones'
--
-- * 'casgHealthCheckType'
--
-- * 'casgLaunchConfigurationName'
--
-- * 'casgPlacementGroup'
--
-- * 'casgLoadBalancerNames'
--
-- * 'casgTags'
--
-- * 'casgAutoScalingGroupName'
--
-- * 'casgMinSize'
--
-- * 'casgMaxSize'
data CreateAutoScalingGroup = CreateAutoScalingGroup'
    { _casgInstanceId              :: !(Maybe Text)
    , _casgTerminationPolicies     :: !(Maybe [Text])
    , _casgHealthCheckGracePeriod  :: !(Maybe Int)
    , _casgVPCZoneIdentifier       :: !(Maybe Text)
    , _casgDefaultCooldown         :: !(Maybe Int)
    , _casgDesiredCapacity         :: !(Maybe Int)
    , _casgAvailabilityZones       :: !(Maybe (List1 Text))
    , _casgHealthCheckType         :: !(Maybe Text)
    , _casgLaunchConfigurationName :: !(Maybe Text)
    , _casgPlacementGroup          :: !(Maybe Text)
    , _casgLoadBalancerNames       :: !(Maybe [Text])
    , _casgTags                    :: !(Maybe [Tag])
    , _casgAutoScalingGroupName    :: !Text
    , _casgMinSize                 :: !Int
    , _casgMaxSize                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAutoScalingGroup' smart constructor.
createAutoScalingGroup :: Text -> Int -> Int -> CreateAutoScalingGroup
createAutoScalingGroup pAutoScalingGroupName_ pMinSize_ pMaxSize_ =
    CreateAutoScalingGroup'
    { _casgInstanceId = Nothing
    , _casgTerminationPolicies = Nothing
    , _casgHealthCheckGracePeriod = Nothing
    , _casgVPCZoneIdentifier = Nothing
    , _casgDefaultCooldown = Nothing
    , _casgDesiredCapacity = Nothing
    , _casgAvailabilityZones = Nothing
    , _casgHealthCheckType = Nothing
    , _casgLaunchConfigurationName = Nothing
    , _casgPlacementGroup = Nothing
    , _casgLoadBalancerNames = Nothing
    , _casgTags = Nothing
    , _casgAutoScalingGroupName = pAutoScalingGroupName_
    , _casgMinSize = pMinSize_
    , _casgMaxSize = pMaxSize_
    }

-- | The ID of the EC2 instance used to create a launch configuration for the
-- group. Alternatively, use the @LaunchConfigurationName@ parameter to
-- specify a launch configuration instead of an EC2 instance.
--
-- When you specify an ID of an instance, Auto Scaling creates a new launch
-- configuration and associates it with the group. This launch
-- configuration derives its attributes from the specified instance, with
-- the exception of the block device mapping.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/create-asg-from-instance.html Create an Auto Scaling Group from an EC2 Instance>
-- in the /Auto Scaling Developer Guide/.
casgInstanceId :: Lens' CreateAutoScalingGroup (Maybe Text)
casgInstanceId = lens _casgInstanceId (\ s a -> s{_casgInstanceId = a});

-- | One or more termination policies used to select the instance to
-- terminate. These policies are executed in the order that they are
-- listed.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/us-termination-policy.html Choosing a Termination Policy for Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
casgTerminationPolicies :: Lens' CreateAutoScalingGroup [Text]
casgTerminationPolicies = lens _casgTerminationPolicies (\ s a -> s{_casgTerminationPolicies = a}) . _Default . _Coerce;

-- | The amount of time, in seconds, after an EC2 instance comes into service
-- that Auto Scaling starts checking its health. During this time, any
-- health check failures for the instance are ignored.
--
-- This parameter is required if you are adding an @ELB@ health check.
-- Frequently, new instances need to warm up, briefly, before they can pass
-- a health check. To provide ample warm-up time, set the health check
-- grace period of the group to match the expected startup period of your
-- application.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/as-add-elb-healthcheck.html Add an Elastic Load Balancing Health Check to Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
casgHealthCheckGracePeriod :: Lens' CreateAutoScalingGroup (Maybe Int)
casgHealthCheckGracePeriod = lens _casgHealthCheckGracePeriod (\ s a -> s{_casgHealthCheckGracePeriod = a});

-- | A comma-separated list of subnet identifiers for your virtual private
-- cloud (VPC).
--
-- If you specify subnets and Availability Zones with this call, ensure
-- that the subnets\' Availability Zones match the Availability Zones
-- specified.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/autoscalingsubnets.html Auto Scaling and Amazon Virtual Private Cloud>
-- in the /Auto Scaling Developer Guide/.
casgVPCZoneIdentifier :: Lens' CreateAutoScalingGroup (Maybe Text)
casgVPCZoneIdentifier = lens _casgVPCZoneIdentifier (\ s a -> s{_casgVPCZoneIdentifier = a});

-- | The amount of time, in seconds, after a scaling activity completes
-- before another scaling activity can start.
--
-- If this parameter is not specified, the default value is 300. For more
-- information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/Cooldown.html Understanding Auto Scaling Cooldowns>
-- in the /Auto Scaling Developer Guide/.
casgDefaultCooldown :: Lens' CreateAutoScalingGroup (Maybe Int)
casgDefaultCooldown = lens _casgDefaultCooldown (\ s a -> s{_casgDefaultCooldown = a});

-- | The number of EC2 instances that should be running in the group. This
-- number must be greater than or equal to the minimum size of the group
-- and less than or equal to the maximum size of the group.
casgDesiredCapacity :: Lens' CreateAutoScalingGroup (Maybe Int)
casgDesiredCapacity = lens _casgDesiredCapacity (\ s a -> s{_casgDesiredCapacity = a});

-- | One or more Availability Zones for the group. This parameter is optional
-- if you specify subnets using the @VPCZoneIdentifier@ parameter.
casgAvailabilityZones :: Lens' CreateAutoScalingGroup (Maybe (NonEmpty Text))
casgAvailabilityZones = lens _casgAvailabilityZones (\ s a -> s{_casgAvailabilityZones = a}) . mapping _List1;

-- | The service to use for the health checks. The valid values are @EC2@ and
-- @ELB@.
--
-- By default, health checks use Amazon EC2 instance status checks to
-- determine the health of an instance. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/healthcheck.html Health Checks>.
casgHealthCheckType :: Lens' CreateAutoScalingGroup (Maybe Text)
casgHealthCheckType = lens _casgHealthCheckType (\ s a -> s{_casgHealthCheckType = a});

-- | The name of the launch configuration. Alternatively, use the
-- @InstanceId@ parameter to specify an EC2 instance instead of a launch
-- configuration.
casgLaunchConfigurationName :: Lens' CreateAutoScalingGroup (Maybe Text)
casgLaunchConfigurationName = lens _casgLaunchConfigurationName (\ s a -> s{_casgLaunchConfigurationName = a});

-- | The name of the placement group into which you\'ll launch your
-- instances, if any. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>
-- in the /Amazon Elastic Compute Cloud User Guide/.
casgPlacementGroup :: Lens' CreateAutoScalingGroup (Maybe Text)
casgPlacementGroup = lens _casgPlacementGroup (\ s a -> s{_casgPlacementGroup = a});

-- | One or more load balancers.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/US_SetUpASLBApp.html Load Balance Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
casgLoadBalancerNames :: Lens' CreateAutoScalingGroup [Text]
casgLoadBalancerNames = lens _casgLoadBalancerNames (\ s a -> s{_casgLoadBalancerNames = a}) . _Default . _Coerce;

-- | The tag to be created or updated. Each tag should be defined by its
-- resource type, resource ID, key, value, and a propagate flag. Valid
-- values: key=/value/, value=/value/, propagate=/true/ or /false/. Value
-- and propagate are optional parameters.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/ASTagging.html Tagging Auto Scaling Groups and Instances>
-- in the /Auto Scaling Developer Guide/.
casgTags :: Lens' CreateAutoScalingGroup [Tag]
casgTags = lens _casgTags (\ s a -> s{_casgTags = a}) . _Default . _Coerce;

-- | The name of the group. This name must be unique within the scope of your
-- AWS account.
casgAutoScalingGroupName :: Lens' CreateAutoScalingGroup Text
casgAutoScalingGroupName = lens _casgAutoScalingGroupName (\ s a -> s{_casgAutoScalingGroupName = a});

-- | The minimum size of the group.
casgMinSize :: Lens' CreateAutoScalingGroup Int
casgMinSize = lens _casgMinSize (\ s a -> s{_casgMinSize = a});

-- | The maximum size of the group.
casgMaxSize :: Lens' CreateAutoScalingGroup Int
casgMaxSize = lens _casgMaxSize (\ s a -> s{_casgMaxSize = a});

instance AWSRequest CreateAutoScalingGroup where
        type Sv CreateAutoScalingGroup = AutoScaling
        type Rs CreateAutoScalingGroup =
             CreateAutoScalingGroupResponse
        request = post
        response
          = receiveNull CreateAutoScalingGroupResponse'

instance ToHeaders CreateAutoScalingGroup where
        toHeaders = const mempty

instance ToPath CreateAutoScalingGroup where
        toPath = const "/"

instance ToQuery CreateAutoScalingGroup where
        toQuery CreateAutoScalingGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateAutoScalingGroup" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceId" =: _casgInstanceId,
               "TerminationPolicies" =:
                 toQuery
                   (toQueryList "member" <$> _casgTerminationPolicies),
               "HealthCheckGracePeriod" =:
                 _casgHealthCheckGracePeriod,
               "VPCZoneIdentifier" =: _casgVPCZoneIdentifier,
               "DefaultCooldown" =: _casgDefaultCooldown,
               "DesiredCapacity" =: _casgDesiredCapacity,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "member" <$> _casgAvailabilityZones),
               "HealthCheckType" =: _casgHealthCheckType,
               "LaunchConfigurationName" =:
                 _casgLaunchConfigurationName,
               "PlacementGroup" =: _casgPlacementGroup,
               "LoadBalancerNames" =:
                 toQuery
                   (toQueryList "member" <$> _casgLoadBalancerNames),
               "Tags" =:
                 toQuery (toQueryList "member" <$> _casgTags),
               "AutoScalingGroupName" =: _casgAutoScalingGroupName,
               "MinSize" =: _casgMinSize, "MaxSize" =: _casgMaxSize]

-- | /See:/ 'createAutoScalingGroupResponse' smart constructor.
data CreateAutoScalingGroupResponse =
    CreateAutoScalingGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAutoScalingGroupResponse' smart constructor.
createAutoScalingGroupResponse :: CreateAutoScalingGroupResponse
createAutoScalingGroupResponse = CreateAutoScalingGroupResponse'
