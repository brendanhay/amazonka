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
-- Module      : Network.AWS.AutoScaling.UpdateAutoScalingGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for the specified Auto Scaling group.
--
--
-- The new settings take effect on any scaling activities after this call returns. Scaling activities that are currently in progress aren't affected.
--
-- To update an Auto Scaling group with a launch configuration with @InstanceMonitoring@ set to @false@ , you must first disable the collection of group metrics. Otherwise, you will get an error. If you have previously enabled the collection of group metrics, you can disable it using 'DisableMetricsCollection' .
--
-- Note the following:
--
--     * If you specify a new value for @MinSize@ without specifying a value for @DesiredCapacity@ , and the new @MinSize@ is larger than the current size of the group, we implicitly call 'SetDesiredCapacity' to set the size of the group to the new value of @MinSize@ .
--
--     * If you specify a new value for @MaxSize@ without specifying a value for @DesiredCapacity@ , and the new @MaxSize@ is smaller than the current size of the group, we implicitly call 'SetDesiredCapacity' to set the size of the group to the new value of @MaxSize@ .
--
--     * All other optional parameters are left unchanged if not specified.
--
--
--
module Network.AWS.AutoScaling.UpdateAutoScalingGroup
    (
    -- * Creating a Request
      updateAutoScalingGroup
    , UpdateAutoScalingGroup
    -- * Request Lenses
    , uasgTerminationPolicies
    , uasgHealthCheckGracePeriod
    , uasgServiceLinkedRoleARN
    , uasgNewInstancesProtectedFromScaleIn
    , uasgVPCZoneIdentifier
    , uasgDefaultCooldown
    , uasgMaxSize
    , uasgAvailabilityZones
    , uasgDesiredCapacity
    , uasgMinSize
    , uasgLaunchConfigurationName
    , uasgHealthCheckType
    , uasgLaunchTemplate
    , uasgPlacementGroup
    , uasgAutoScalingGroupName

    -- * Destructuring the Response
    , updateAutoScalingGroupResponse
    , UpdateAutoScalingGroupResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAutoScalingGroup' smart constructor.
data UpdateAutoScalingGroup = UpdateAutoScalingGroup'
  { _uasgTerminationPolicies :: !(Maybe [Text])
  , _uasgHealthCheckGracePeriod :: !(Maybe Int)
  , _uasgServiceLinkedRoleARN :: !(Maybe Text)
  , _uasgNewInstancesProtectedFromScaleIn :: !(Maybe Bool)
  , _uasgVPCZoneIdentifier :: !(Maybe Text)
  , _uasgDefaultCooldown :: !(Maybe Int)
  , _uasgMaxSize :: !(Maybe Int)
  , _uasgAvailabilityZones :: !(Maybe (List1 Text))
  , _uasgDesiredCapacity :: !(Maybe Int)
  , _uasgMinSize :: !(Maybe Int)
  , _uasgLaunchConfigurationName :: !(Maybe Text)
  , _uasgHealthCheckType :: !(Maybe Text)
  , _uasgLaunchTemplate :: !(Maybe LaunchTemplateSpecification)
  , _uasgPlacementGroup :: !(Maybe Text)
  , _uasgAutoScalingGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasgTerminationPolicies' - A standalone termination policy or a list of termination policies used to select the instance to terminate. The policies are executed in the order that they are listed. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/as-instance-termination.html Controlling Which Instances Auto Scaling Terminates During Scale In> in the /Auto Scaling User Guide/ .
--
-- * 'uasgHealthCheckGracePeriod' - The amount of time, in seconds, that Auto Scaling waits before checking the health status of an EC2 instance that has come into service. The default is 0. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/healthcheck.html Health Checks> in the /Auto Scaling User Guide/ .
--
-- * 'uasgServiceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf.
--
-- * 'uasgNewInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from termination by Auto Scaling when scaling in.
--
-- * 'uasgVPCZoneIdentifier' - The ID of the subnet, if you are launching into a VPC. You can specify several subnets in a comma-separated list. When you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , ensure that the subnets' Availability Zones match the values you specify for @AvailabilityZones@ . For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/asg-in-vpc.html Launching Auto Scaling Instances in a VPC> in the /Auto Scaling User Guide/ .
--
-- * 'uasgDefaultCooldown' - The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default is 300. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/Cooldown.html Auto Scaling Cooldowns> in the /Auto Scaling User Guide/ .
--
-- * 'uasgMaxSize' - The maximum size of the Auto Scaling group.
--
-- * 'uasgAvailabilityZones' - One or more Availability Zones for the group.
--
-- * 'uasgDesiredCapacity' - The number of EC2 instances that should be running in the Auto Scaling group. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group.
--
-- * 'uasgMinSize' - The minimum size of the Auto Scaling group.
--
-- * 'uasgLaunchConfigurationName' - The name of the launch configuration. If you specify a launch configuration, you can't specify a launch template.
--
-- * 'uasgHealthCheckType' - The service to use for the health checks. The valid values are @EC2@ and @ELB@ .
--
-- * 'uasgLaunchTemplate' - The launch template to use to specify the updates. If you specify a launch template, you can't specify a launch configuration.
--
-- * 'uasgPlacementGroup' - The name of the placement group into which you'll launch your instances, if any. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'uasgAutoScalingGroupName' - The name of the Auto Scaling group.
updateAutoScalingGroup
    :: Text -- ^ 'uasgAutoScalingGroupName'
    -> UpdateAutoScalingGroup
updateAutoScalingGroup pAutoScalingGroupName_ =
  UpdateAutoScalingGroup'
    { _uasgTerminationPolicies = Nothing
    , _uasgHealthCheckGracePeriod = Nothing
    , _uasgServiceLinkedRoleARN = Nothing
    , _uasgNewInstancesProtectedFromScaleIn = Nothing
    , _uasgVPCZoneIdentifier = Nothing
    , _uasgDefaultCooldown = Nothing
    , _uasgMaxSize = Nothing
    , _uasgAvailabilityZones = Nothing
    , _uasgDesiredCapacity = Nothing
    , _uasgMinSize = Nothing
    , _uasgLaunchConfigurationName = Nothing
    , _uasgHealthCheckType = Nothing
    , _uasgLaunchTemplate = Nothing
    , _uasgPlacementGroup = Nothing
    , _uasgAutoScalingGroupName = pAutoScalingGroupName_
    }


-- | A standalone termination policy or a list of termination policies used to select the instance to terminate. The policies are executed in the order that they are listed. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/as-instance-termination.html Controlling Which Instances Auto Scaling Terminates During Scale In> in the /Auto Scaling User Guide/ .
uasgTerminationPolicies :: Lens' UpdateAutoScalingGroup [Text]
uasgTerminationPolicies = lens _uasgTerminationPolicies (\ s a -> s{_uasgTerminationPolicies = a}) . _Default . _Coerce

-- | The amount of time, in seconds, that Auto Scaling waits before checking the health status of an EC2 instance that has come into service. The default is 0. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/healthcheck.html Health Checks> in the /Auto Scaling User Guide/ .
uasgHealthCheckGracePeriod :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgHealthCheckGracePeriod = lens _uasgHealthCheckGracePeriod (\ s a -> s{_uasgHealthCheckGracePeriod = a})

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf.
uasgServiceLinkedRoleARN :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgServiceLinkedRoleARN = lens _uasgServiceLinkedRoleARN (\ s a -> s{_uasgServiceLinkedRoleARN = a})

-- | Indicates whether newly launched instances are protected from termination by Auto Scaling when scaling in.
uasgNewInstancesProtectedFromScaleIn :: Lens' UpdateAutoScalingGroup (Maybe Bool)
uasgNewInstancesProtectedFromScaleIn = lens _uasgNewInstancesProtectedFromScaleIn (\ s a -> s{_uasgNewInstancesProtectedFromScaleIn = a})

-- | The ID of the subnet, if you are launching into a VPC. You can specify several subnets in a comma-separated list. When you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , ensure that the subnets' Availability Zones match the values you specify for @AvailabilityZones@ . For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/asg-in-vpc.html Launching Auto Scaling Instances in a VPC> in the /Auto Scaling User Guide/ .
uasgVPCZoneIdentifier :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgVPCZoneIdentifier = lens _uasgVPCZoneIdentifier (\ s a -> s{_uasgVPCZoneIdentifier = a})

-- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default is 300. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/Cooldown.html Auto Scaling Cooldowns> in the /Auto Scaling User Guide/ .
uasgDefaultCooldown :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgDefaultCooldown = lens _uasgDefaultCooldown (\ s a -> s{_uasgDefaultCooldown = a})

-- | The maximum size of the Auto Scaling group.
uasgMaxSize :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgMaxSize = lens _uasgMaxSize (\ s a -> s{_uasgMaxSize = a})

-- | One or more Availability Zones for the group.
uasgAvailabilityZones :: Lens' UpdateAutoScalingGroup (Maybe (NonEmpty Text))
uasgAvailabilityZones = lens _uasgAvailabilityZones (\ s a -> s{_uasgAvailabilityZones = a}) . mapping _List1

-- | The number of EC2 instances that should be running in the Auto Scaling group. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group.
uasgDesiredCapacity :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgDesiredCapacity = lens _uasgDesiredCapacity (\ s a -> s{_uasgDesiredCapacity = a})

-- | The minimum size of the Auto Scaling group.
uasgMinSize :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgMinSize = lens _uasgMinSize (\ s a -> s{_uasgMinSize = a})

-- | The name of the launch configuration. If you specify a launch configuration, you can't specify a launch template.
uasgLaunchConfigurationName :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgLaunchConfigurationName = lens _uasgLaunchConfigurationName (\ s a -> s{_uasgLaunchConfigurationName = a})

-- | The service to use for the health checks. The valid values are @EC2@ and @ELB@ .
uasgHealthCheckType :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgHealthCheckType = lens _uasgHealthCheckType (\ s a -> s{_uasgHealthCheckType = a})

-- | The launch template to use to specify the updates. If you specify a launch template, you can't specify a launch configuration.
uasgLaunchTemplate :: Lens' UpdateAutoScalingGroup (Maybe LaunchTemplateSpecification)
uasgLaunchTemplate = lens _uasgLaunchTemplate (\ s a -> s{_uasgLaunchTemplate = a})

-- | The name of the placement group into which you'll launch your instances, if any. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon Elastic Compute Cloud User Guide/ .
uasgPlacementGroup :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgPlacementGroup = lens _uasgPlacementGroup (\ s a -> s{_uasgPlacementGroup = a})

-- | The name of the Auto Scaling group.
uasgAutoScalingGroupName :: Lens' UpdateAutoScalingGroup Text
uasgAutoScalingGroupName = lens _uasgAutoScalingGroupName (\ s a -> s{_uasgAutoScalingGroupName = a})

instance AWSRequest UpdateAutoScalingGroup where
        type Rs UpdateAutoScalingGroup =
             UpdateAutoScalingGroupResponse
        request = postQuery autoScaling
        response
          = receiveNull UpdateAutoScalingGroupResponse'

instance Hashable UpdateAutoScalingGroup where

instance NFData UpdateAutoScalingGroup where

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
                   (toQueryList "member" <$> _uasgTerminationPolicies),
               "HealthCheckGracePeriod" =:
                 _uasgHealthCheckGracePeriod,
               "ServiceLinkedRoleARN" =: _uasgServiceLinkedRoleARN,
               "NewInstancesProtectedFromScaleIn" =:
                 _uasgNewInstancesProtectedFromScaleIn,
               "VPCZoneIdentifier" =: _uasgVPCZoneIdentifier,
               "DefaultCooldown" =: _uasgDefaultCooldown,
               "MaxSize" =: _uasgMaxSize,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "member" <$> _uasgAvailabilityZones),
               "DesiredCapacity" =: _uasgDesiredCapacity,
               "MinSize" =: _uasgMinSize,
               "LaunchConfigurationName" =:
                 _uasgLaunchConfigurationName,
               "HealthCheckType" =: _uasgHealthCheckType,
               "LaunchTemplate" =: _uasgLaunchTemplate,
               "PlacementGroup" =: _uasgPlacementGroup,
               "AutoScalingGroupName" =: _uasgAutoScalingGroupName]

-- | /See:/ 'updateAutoScalingGroupResponse' smart constructor.
data UpdateAutoScalingGroupResponse =
  UpdateAutoScalingGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAutoScalingGroupResponse' with the minimum fields required to make a request.
--
updateAutoScalingGroupResponse
    :: UpdateAutoScalingGroupResponse
updateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse'


instance NFData UpdateAutoScalingGroupResponse where
