{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CreateAutoScalingGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Auto Scaling group with the specified name and attributes.
--
--
-- If you exceed your maximum limit of Auto Scaling groups, the call fails. For information about viewing this limit, see 'DescribeAccountLimits' . For information about updating this limit, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/as-account-limits.html Auto Scaling Limits> in the /Auto Scaling User Guide/ .
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/AutoScalingGroup.html Auto Scaling Groups> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.CreateAutoScalingGroup
    (
    -- * Creating a Request
      createAutoScalingGroup
    , CreateAutoScalingGroup
    -- * Request Lenses
    , casgInstanceId
    , casgTerminationPolicies
    , casgHealthCheckGracePeriod
    , casgServiceLinkedRoleARN
    , casgNewInstancesProtectedFromScaleIn
    , casgVPCZoneIdentifier
    , casgTargetGroupARNs
    , casgDefaultCooldown
    , casgAvailabilityZones
    , casgDesiredCapacity
    , casgLaunchConfigurationName
    , casgLifecycleHookSpecificationList
    , casgHealthCheckType
    , casgLaunchTemplate
    , casgPlacementGroup
    , casgLoadBalancerNames
    , casgTags
    , casgAutoScalingGroupName
    , casgMinSize
    , casgMaxSize

    -- * Destructuring the Response
    , createAutoScalingGroupResponse
    , CreateAutoScalingGroupResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAutoScalingGroup' smart constructor.
data CreateAutoScalingGroup = CreateAutoScalingGroup'
  { _casgInstanceId :: !(Maybe Text)
  , _casgTerminationPolicies :: !(Maybe [Text])
  , _casgHealthCheckGracePeriod :: !(Maybe Int)
  , _casgServiceLinkedRoleARN :: !(Maybe Text)
  , _casgNewInstancesProtectedFromScaleIn :: !(Maybe Bool)
  , _casgVPCZoneIdentifier :: !(Maybe Text)
  , _casgTargetGroupARNs :: !(Maybe [Text])
  , _casgDefaultCooldown :: !(Maybe Int)
  , _casgAvailabilityZones :: !(Maybe (List1 Text))
  , _casgDesiredCapacity :: !(Maybe Int)
  , _casgLaunchConfigurationName :: !(Maybe Text)
  , _casgLifecycleHookSpecificationList :: !(Maybe [LifecycleHookSpecification])
  , _casgHealthCheckType :: !(Maybe Text)
  , _casgLaunchTemplate :: !(Maybe LaunchTemplateSpecification)
  , _casgPlacementGroup :: !(Maybe Text)
  , _casgLoadBalancerNames :: !(Maybe [Text])
  , _casgTags :: !(Maybe [Tag])
  , _casgAutoScalingGroupName :: !Text
  , _casgMinSize :: !Int
  , _casgMaxSize :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'casgInstanceId' - The ID of the instance used to create a launch configuration for the group. You must specify one of the following: an EC2 instance, a launch configuration, or a launch template. When you specify an ID of an instance, Auto Scaling creates a new launch configuration and associates it with the group. This launch configuration derives its attributes from the specified instance, with the exception of the block device mapping. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/create-asg-from-instance.html Create an Auto Scaling Group Using an EC2 Instance> in the /Auto Scaling User Guide/ .
--
-- * 'casgTerminationPolicies' - One or more termination policies used to select the instance to terminate. These policies are executed in the order that they are listed. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/as-instance-termination.html Controlling Which Instances Auto Scaling Terminates During Scale In> in the /Auto Scaling User Guide/ .
--
-- * 'casgHealthCheckGracePeriod' - The amount of time, in seconds, that Auto Scaling waits before checking the health status of an EC2 instance that has come into service. During this time, any health check failures for the instance are ignored. The default is 0. This parameter is required if you are adding an @ELB@ health check. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/healthcheck.html Health Checks> in the /Auto Scaling User Guide/ .
--
-- * 'casgServiceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. By default, Auto Scaling uses a service-linked role named AWSServiceRoleForAutoScaling, which it creates if it does not exist.
--
-- * 'casgNewInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from termination by Auto Scaling when scaling in.
--
-- * 'casgVPCZoneIdentifier' - A comma-separated list of subnet identifiers for your virtual private cloud (VPC). If you specify subnets and Availability Zones with this call, ensure that the subnets' Availability Zones match the Availability Zones specified. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/asg-in-vpc.html Launching Auto Scaling Instances in a VPC> in the /Auto Scaling User Guide/ .
--
-- * 'casgTargetGroupARNs' - The Amazon Resource Names (ARN) of the target groups.
--
-- * 'casgDefaultCooldown' - The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default is 300. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/Cooldown.html Auto Scaling Cooldowns> in the /Auto Scaling User Guide/ .
--
-- * 'casgAvailabilityZones' - One or more Availability Zones for the group. This parameter is optional if you specify one or more subnets.
--
-- * 'casgDesiredCapacity' - The number of EC2 instances that should be running in the group. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group. If you do not specify a desired capacity, the default is the minimum size of the group.
--
-- * 'casgLaunchConfigurationName' - The name of the launch configuration. You must specify one of the following: a launch configuration, a launch template, or an EC2 instance.
--
-- * 'casgLifecycleHookSpecificationList' - One or more lifecycle hooks.
--
-- * 'casgHealthCheckType' - The service to use for the health checks. The valid values are @EC2@ and @ELB@ . By default, health checks use Amazon EC2 instance status checks to determine the health of an instance. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/healthcheck.html Health Checks> in the /Auto Scaling User Guide/ .
--
-- * 'casgLaunchTemplate' - The launch template to use to launch instances. You must specify one of the following: a launch template, a launch configuration, or an EC2 instance.
--
-- * 'casgPlacementGroup' - The name of the placement group into which you'll launch your instances, if any. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'casgLoadBalancerNames' - One or more Classic Load Balancers. To specify an Application Load Balancer, use @TargetGroupARNs@ instead. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/create-asg-from-instance.html Using a Load Balancer With an Auto Scaling Group> in the /Auto Scaling User Guide/ .
--
-- * 'casgTags' - One or more tags. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/autoscaling-tagging.html Tagging Auto Scaling Groups and Instances> in the /Auto Scaling User Guide/ .
--
-- * 'casgAutoScalingGroupName' - The name of the Auto Scaling group. This name must be unique within the scope of your AWS account.
--
-- * 'casgMinSize' - The minimum size of the group.
--
-- * 'casgMaxSize' - The maximum size of the group.
createAutoScalingGroup
    :: Text -- ^ 'casgAutoScalingGroupName'
    -> Int -- ^ 'casgMinSize'
    -> Int -- ^ 'casgMaxSize'
    -> CreateAutoScalingGroup
createAutoScalingGroup pAutoScalingGroupName_ pMinSize_ pMaxSize_ =
  CreateAutoScalingGroup'
    { _casgInstanceId = Nothing
    , _casgTerminationPolicies = Nothing
    , _casgHealthCheckGracePeriod = Nothing
    , _casgServiceLinkedRoleARN = Nothing
    , _casgNewInstancesProtectedFromScaleIn = Nothing
    , _casgVPCZoneIdentifier = Nothing
    , _casgTargetGroupARNs = Nothing
    , _casgDefaultCooldown = Nothing
    , _casgAvailabilityZones = Nothing
    , _casgDesiredCapacity = Nothing
    , _casgLaunchConfigurationName = Nothing
    , _casgLifecycleHookSpecificationList = Nothing
    , _casgHealthCheckType = Nothing
    , _casgLaunchTemplate = Nothing
    , _casgPlacementGroup = Nothing
    , _casgLoadBalancerNames = Nothing
    , _casgTags = Nothing
    , _casgAutoScalingGroupName = pAutoScalingGroupName_
    , _casgMinSize = pMinSize_
    , _casgMaxSize = pMaxSize_
    }


-- | The ID of the instance used to create a launch configuration for the group. You must specify one of the following: an EC2 instance, a launch configuration, or a launch template. When you specify an ID of an instance, Auto Scaling creates a new launch configuration and associates it with the group. This launch configuration derives its attributes from the specified instance, with the exception of the block device mapping. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/create-asg-from-instance.html Create an Auto Scaling Group Using an EC2 Instance> in the /Auto Scaling User Guide/ .
casgInstanceId :: Lens' CreateAutoScalingGroup (Maybe Text)
casgInstanceId = lens _casgInstanceId (\ s a -> s{_casgInstanceId = a})

-- | One or more termination policies used to select the instance to terminate. These policies are executed in the order that they are listed. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/as-instance-termination.html Controlling Which Instances Auto Scaling Terminates During Scale In> in the /Auto Scaling User Guide/ .
casgTerminationPolicies :: Lens' CreateAutoScalingGroup [Text]
casgTerminationPolicies = lens _casgTerminationPolicies (\ s a -> s{_casgTerminationPolicies = a}) . _Default . _Coerce

-- | The amount of time, in seconds, that Auto Scaling waits before checking the health status of an EC2 instance that has come into service. During this time, any health check failures for the instance are ignored. The default is 0. This parameter is required if you are adding an @ELB@ health check. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/healthcheck.html Health Checks> in the /Auto Scaling User Guide/ .
casgHealthCheckGracePeriod :: Lens' CreateAutoScalingGroup (Maybe Int)
casgHealthCheckGracePeriod = lens _casgHealthCheckGracePeriod (\ s a -> s{_casgHealthCheckGracePeriod = a})

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. By default, Auto Scaling uses a service-linked role named AWSServiceRoleForAutoScaling, which it creates if it does not exist.
casgServiceLinkedRoleARN :: Lens' CreateAutoScalingGroup (Maybe Text)
casgServiceLinkedRoleARN = lens _casgServiceLinkedRoleARN (\ s a -> s{_casgServiceLinkedRoleARN = a})

-- | Indicates whether newly launched instances are protected from termination by Auto Scaling when scaling in.
casgNewInstancesProtectedFromScaleIn :: Lens' CreateAutoScalingGroup (Maybe Bool)
casgNewInstancesProtectedFromScaleIn = lens _casgNewInstancesProtectedFromScaleIn (\ s a -> s{_casgNewInstancesProtectedFromScaleIn = a})

-- | A comma-separated list of subnet identifiers for your virtual private cloud (VPC). If you specify subnets and Availability Zones with this call, ensure that the subnets' Availability Zones match the Availability Zones specified. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/asg-in-vpc.html Launching Auto Scaling Instances in a VPC> in the /Auto Scaling User Guide/ .
casgVPCZoneIdentifier :: Lens' CreateAutoScalingGroup (Maybe Text)
casgVPCZoneIdentifier = lens _casgVPCZoneIdentifier (\ s a -> s{_casgVPCZoneIdentifier = a})

-- | The Amazon Resource Names (ARN) of the target groups.
casgTargetGroupARNs :: Lens' CreateAutoScalingGroup [Text]
casgTargetGroupARNs = lens _casgTargetGroupARNs (\ s a -> s{_casgTargetGroupARNs = a}) . _Default . _Coerce

-- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default is 300. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/Cooldown.html Auto Scaling Cooldowns> in the /Auto Scaling User Guide/ .
casgDefaultCooldown :: Lens' CreateAutoScalingGroup (Maybe Int)
casgDefaultCooldown = lens _casgDefaultCooldown (\ s a -> s{_casgDefaultCooldown = a})

-- | One or more Availability Zones for the group. This parameter is optional if you specify one or more subnets.
casgAvailabilityZones :: Lens' CreateAutoScalingGroup (Maybe (NonEmpty Text))
casgAvailabilityZones = lens _casgAvailabilityZones (\ s a -> s{_casgAvailabilityZones = a}) . mapping _List1

-- | The number of EC2 instances that should be running in the group. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group. If you do not specify a desired capacity, the default is the minimum size of the group.
casgDesiredCapacity :: Lens' CreateAutoScalingGroup (Maybe Int)
casgDesiredCapacity = lens _casgDesiredCapacity (\ s a -> s{_casgDesiredCapacity = a})

-- | The name of the launch configuration. You must specify one of the following: a launch configuration, a launch template, or an EC2 instance.
casgLaunchConfigurationName :: Lens' CreateAutoScalingGroup (Maybe Text)
casgLaunchConfigurationName = lens _casgLaunchConfigurationName (\ s a -> s{_casgLaunchConfigurationName = a})

-- | One or more lifecycle hooks.
casgLifecycleHookSpecificationList :: Lens' CreateAutoScalingGroup [LifecycleHookSpecification]
casgLifecycleHookSpecificationList = lens _casgLifecycleHookSpecificationList (\ s a -> s{_casgLifecycleHookSpecificationList = a}) . _Default . _Coerce

-- | The service to use for the health checks. The valid values are @EC2@ and @ELB@ . By default, health checks use Amazon EC2 instance status checks to determine the health of an instance. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/healthcheck.html Health Checks> in the /Auto Scaling User Guide/ .
casgHealthCheckType :: Lens' CreateAutoScalingGroup (Maybe Text)
casgHealthCheckType = lens _casgHealthCheckType (\ s a -> s{_casgHealthCheckType = a})

-- | The launch template to use to launch instances. You must specify one of the following: a launch template, a launch configuration, or an EC2 instance.
casgLaunchTemplate :: Lens' CreateAutoScalingGroup (Maybe LaunchTemplateSpecification)
casgLaunchTemplate = lens _casgLaunchTemplate (\ s a -> s{_casgLaunchTemplate = a})

-- | The name of the placement group into which you'll launch your instances, if any. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon Elastic Compute Cloud User Guide/ .
casgPlacementGroup :: Lens' CreateAutoScalingGroup (Maybe Text)
casgPlacementGroup = lens _casgPlacementGroup (\ s a -> s{_casgPlacementGroup = a})

-- | One or more Classic Load Balancers. To specify an Application Load Balancer, use @TargetGroupARNs@ instead. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/create-asg-from-instance.html Using a Load Balancer With an Auto Scaling Group> in the /Auto Scaling User Guide/ .
casgLoadBalancerNames :: Lens' CreateAutoScalingGroup [Text]
casgLoadBalancerNames = lens _casgLoadBalancerNames (\ s a -> s{_casgLoadBalancerNames = a}) . _Default . _Coerce

-- | One or more tags. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/autoscaling-tagging.html Tagging Auto Scaling Groups and Instances> in the /Auto Scaling User Guide/ .
casgTags :: Lens' CreateAutoScalingGroup [Tag]
casgTags = lens _casgTags (\ s a -> s{_casgTags = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group. This name must be unique within the scope of your AWS account.
casgAutoScalingGroupName :: Lens' CreateAutoScalingGroup Text
casgAutoScalingGroupName = lens _casgAutoScalingGroupName (\ s a -> s{_casgAutoScalingGroupName = a})

-- | The minimum size of the group.
casgMinSize :: Lens' CreateAutoScalingGroup Int
casgMinSize = lens _casgMinSize (\ s a -> s{_casgMinSize = a})

-- | The maximum size of the group.
casgMaxSize :: Lens' CreateAutoScalingGroup Int
casgMaxSize = lens _casgMaxSize (\ s a -> s{_casgMaxSize = a})

instance AWSRequest CreateAutoScalingGroup where
        type Rs CreateAutoScalingGroup =
             CreateAutoScalingGroupResponse
        request = postQuery autoScaling
        response
          = receiveNull CreateAutoScalingGroupResponse'

instance Hashable CreateAutoScalingGroup where

instance NFData CreateAutoScalingGroup where

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
               "ServiceLinkedRoleARN" =: _casgServiceLinkedRoleARN,
               "NewInstancesProtectedFromScaleIn" =:
                 _casgNewInstancesProtectedFromScaleIn,
               "VPCZoneIdentifier" =: _casgVPCZoneIdentifier,
               "TargetGroupARNs" =:
                 toQuery
                   (toQueryList "member" <$> _casgTargetGroupARNs),
               "DefaultCooldown" =: _casgDefaultCooldown,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "member" <$> _casgAvailabilityZones),
               "DesiredCapacity" =: _casgDesiredCapacity,
               "LaunchConfigurationName" =:
                 _casgLaunchConfigurationName,
               "LifecycleHookSpecificationList" =:
                 toQuery
                   (toQueryList "member" <$>
                      _casgLifecycleHookSpecificationList),
               "HealthCheckType" =: _casgHealthCheckType,
               "LaunchTemplate" =: _casgLaunchTemplate,
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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAutoScalingGroupResponse' with the minimum fields required to make a request.
--
createAutoScalingGroupResponse
    :: CreateAutoScalingGroupResponse
createAutoScalingGroupResponse = CreateAutoScalingGroupResponse'


instance NFData CreateAutoScalingGroupResponse where
