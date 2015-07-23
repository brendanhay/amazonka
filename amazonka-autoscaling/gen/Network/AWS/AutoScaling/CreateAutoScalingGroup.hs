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
    , casgrqInstanceId
    , casgrqTerminationPolicies
    , casgrqHealthCheckGracePeriod
    , casgrqVPCZoneIdentifier
    , casgrqDefaultCooldown
    , casgrqDesiredCapacity
    , casgrqAvailabilityZones
    , casgrqHealthCheckType
    , casgrqLaunchConfigurationName
    , casgrqPlacementGroup
    , casgrqLoadBalancerNames
    , casgrqTags
    , casgrqAutoScalingGroupName
    , casgrqMinSize
    , casgrqMaxSize

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
-- * 'casgrqInstanceId'
--
-- * 'casgrqTerminationPolicies'
--
-- * 'casgrqHealthCheckGracePeriod'
--
-- * 'casgrqVPCZoneIdentifier'
--
-- * 'casgrqDefaultCooldown'
--
-- * 'casgrqDesiredCapacity'
--
-- * 'casgrqAvailabilityZones'
--
-- * 'casgrqHealthCheckType'
--
-- * 'casgrqLaunchConfigurationName'
--
-- * 'casgrqPlacementGroup'
--
-- * 'casgrqLoadBalancerNames'
--
-- * 'casgrqTags'
--
-- * 'casgrqAutoScalingGroupName'
--
-- * 'casgrqMinSize'
--
-- * 'casgrqMaxSize'
data CreateAutoScalingGroup = CreateAutoScalingGroup'
    { _casgrqInstanceId              :: !(Maybe Text)
    , _casgrqTerminationPolicies     :: !(Maybe [Text])
    , _casgrqHealthCheckGracePeriod  :: !(Maybe Int)
    , _casgrqVPCZoneIdentifier       :: !(Maybe Text)
    , _casgrqDefaultCooldown         :: !(Maybe Int)
    , _casgrqDesiredCapacity         :: !(Maybe Int)
    , _casgrqAvailabilityZones       :: !(Maybe (List1 Text))
    , _casgrqHealthCheckType         :: !(Maybe Text)
    , _casgrqLaunchConfigurationName :: !(Maybe Text)
    , _casgrqPlacementGroup          :: !(Maybe Text)
    , _casgrqLoadBalancerNames       :: !(Maybe [Text])
    , _casgrqTags                    :: !(Maybe [Tag])
    , _casgrqAutoScalingGroupName    :: !Text
    , _casgrqMinSize                 :: !Int
    , _casgrqMaxSize                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAutoScalingGroup' smart constructor.
createAutoScalingGroup :: Text -> Int -> Int -> CreateAutoScalingGroup
createAutoScalingGroup pAutoScalingGroupName_ pMinSize_ pMaxSize_ =
    CreateAutoScalingGroup'
    { _casgrqInstanceId = Nothing
    , _casgrqTerminationPolicies = Nothing
    , _casgrqHealthCheckGracePeriod = Nothing
    , _casgrqVPCZoneIdentifier = Nothing
    , _casgrqDefaultCooldown = Nothing
    , _casgrqDesiredCapacity = Nothing
    , _casgrqAvailabilityZones = Nothing
    , _casgrqHealthCheckType = Nothing
    , _casgrqLaunchConfigurationName = Nothing
    , _casgrqPlacementGroup = Nothing
    , _casgrqLoadBalancerNames = Nothing
    , _casgrqTags = Nothing
    , _casgrqAutoScalingGroupName = pAutoScalingGroupName_
    , _casgrqMinSize = pMinSize_
    , _casgrqMaxSize = pMaxSize_
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
casgrqInstanceId :: Lens' CreateAutoScalingGroup (Maybe Text)
casgrqInstanceId = lens _casgrqInstanceId (\ s a -> s{_casgrqInstanceId = a});

-- | One or more termination policies used to select the instance to
-- terminate. These policies are executed in the order that they are
-- listed.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/us-termination-policy.html Choosing a Termination Policy for Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
casgrqTerminationPolicies :: Lens' CreateAutoScalingGroup [Text]
casgrqTerminationPolicies = lens _casgrqTerminationPolicies (\ s a -> s{_casgrqTerminationPolicies = a}) . _Default;

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
casgrqHealthCheckGracePeriod :: Lens' CreateAutoScalingGroup (Maybe Int)
casgrqHealthCheckGracePeriod = lens _casgrqHealthCheckGracePeriod (\ s a -> s{_casgrqHealthCheckGracePeriod = a});

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
casgrqVPCZoneIdentifier :: Lens' CreateAutoScalingGroup (Maybe Text)
casgrqVPCZoneIdentifier = lens _casgrqVPCZoneIdentifier (\ s a -> s{_casgrqVPCZoneIdentifier = a});

-- | The amount of time, in seconds, after a scaling activity completes
-- before another scaling activity can start.
--
-- If this parameter is not specified, the default value is 300. For more
-- information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/Cooldown.html Understanding Auto Scaling Cooldowns>
-- in the /Auto Scaling Developer Guide/.
casgrqDefaultCooldown :: Lens' CreateAutoScalingGroup (Maybe Int)
casgrqDefaultCooldown = lens _casgrqDefaultCooldown (\ s a -> s{_casgrqDefaultCooldown = a});

-- | The number of EC2 instances that should be running in the group. This
-- number must be greater than or equal to the minimum size of the group
-- and less than or equal to the maximum size of the group.
casgrqDesiredCapacity :: Lens' CreateAutoScalingGroup (Maybe Int)
casgrqDesiredCapacity = lens _casgrqDesiredCapacity (\ s a -> s{_casgrqDesiredCapacity = a});

-- | One or more Availability Zones for the group. This parameter is optional
-- if you specify subnets using the @VPCZoneIdentifier@ parameter.
casgrqAvailabilityZones :: Lens' CreateAutoScalingGroup (Maybe (NonEmpty Text))
casgrqAvailabilityZones = lens _casgrqAvailabilityZones (\ s a -> s{_casgrqAvailabilityZones = a}) . mapping _List1;

-- | The service to use for the health checks. The valid values are @EC2@ and
-- @ELB@.
--
-- By default, health checks use Amazon EC2 instance status checks to
-- determine the health of an instance. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/healthcheck.html Health Checks>.
casgrqHealthCheckType :: Lens' CreateAutoScalingGroup (Maybe Text)
casgrqHealthCheckType = lens _casgrqHealthCheckType (\ s a -> s{_casgrqHealthCheckType = a});

-- | The name of the launch configuration. Alternatively, use the
-- @InstanceId@ parameter to specify an EC2 instance instead of a launch
-- configuration.
casgrqLaunchConfigurationName :: Lens' CreateAutoScalingGroup (Maybe Text)
casgrqLaunchConfigurationName = lens _casgrqLaunchConfigurationName (\ s a -> s{_casgrqLaunchConfigurationName = a});

-- | The name of the placement group into which you\'ll launch your
-- instances, if any. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>
-- in the /Amazon Elastic Compute Cloud User Guide/.
casgrqPlacementGroup :: Lens' CreateAutoScalingGroup (Maybe Text)
casgrqPlacementGroup = lens _casgrqPlacementGroup (\ s a -> s{_casgrqPlacementGroup = a});

-- | One or more load balancers.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/US_SetUpASLBApp.html Load Balance Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
casgrqLoadBalancerNames :: Lens' CreateAutoScalingGroup [Text]
casgrqLoadBalancerNames = lens _casgrqLoadBalancerNames (\ s a -> s{_casgrqLoadBalancerNames = a}) . _Default;

-- | The tag to be created or updated. Each tag should be defined by its
-- resource type, resource ID, key, value, and a propagate flag. Valid
-- values: key=/value/, value=/value/, propagate=/true/ or /false/. Value
-- and propagate are optional parameters.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/ASTagging.html Tagging Auto Scaling Groups and Instances>
-- in the /Auto Scaling Developer Guide/.
casgrqTags :: Lens' CreateAutoScalingGroup [Tag]
casgrqTags = lens _casgrqTags (\ s a -> s{_casgrqTags = a}) . _Default;

-- | The name of the group. This name must be unique within the scope of your
-- AWS account.
casgrqAutoScalingGroupName :: Lens' CreateAutoScalingGroup Text
casgrqAutoScalingGroupName = lens _casgrqAutoScalingGroupName (\ s a -> s{_casgrqAutoScalingGroupName = a});

-- | The minimum size of the group.
casgrqMinSize :: Lens' CreateAutoScalingGroup Int
casgrqMinSize = lens _casgrqMinSize (\ s a -> s{_casgrqMinSize = a});

-- | The maximum size of the group.
casgrqMaxSize :: Lens' CreateAutoScalingGroup Int
casgrqMaxSize = lens _casgrqMaxSize (\ s a -> s{_casgrqMaxSize = a});

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
               "InstanceId" =: _casgrqInstanceId,
               "TerminationPolicies" =:
                 toQuery
                   (toQueryList "member" <$>
                      _casgrqTerminationPolicies),
               "HealthCheckGracePeriod" =:
                 _casgrqHealthCheckGracePeriod,
               "VPCZoneIdentifier" =: _casgrqVPCZoneIdentifier,
               "DefaultCooldown" =: _casgrqDefaultCooldown,
               "DesiredCapacity" =: _casgrqDesiredCapacity,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "member" <$> _casgrqAvailabilityZones),
               "HealthCheckType" =: _casgrqHealthCheckType,
               "LaunchConfigurationName" =:
                 _casgrqLaunchConfigurationName,
               "PlacementGroup" =: _casgrqPlacementGroup,
               "LoadBalancerNames" =:
                 toQuery
                   (toQueryList "member" <$> _casgrqLoadBalancerNames),
               "Tags" =:
                 toQuery (toQueryList "member" <$> _casgrqTags),
               "AutoScalingGroupName" =:
                 _casgrqAutoScalingGroupName,
               "MinSize" =: _casgrqMinSize,
               "MaxSize" =: _casgrqMaxSize]

-- | /See:/ 'createAutoScalingGroupResponse' smart constructor.
data CreateAutoScalingGroupResponse =
    CreateAutoScalingGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAutoScalingGroupResponse' smart constructor.
createAutoScalingGroupResponse :: CreateAutoScalingGroupResponse
createAutoScalingGroupResponse = CreateAutoScalingGroupResponse'
