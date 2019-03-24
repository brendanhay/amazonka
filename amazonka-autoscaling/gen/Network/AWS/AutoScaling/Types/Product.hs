{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.Product where

import Network.AWS.AutoScaling.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes scaling activity, which is a long-running process that represents a change to your Auto Scaling group, such as changing its size or replacing an instance.
--
--
--
-- /See:/ 'activity' smart constructor.
data Activity = Activity'
  { _aProgress             :: !(Maybe Int)
  , _aStatusMessage        :: !(Maybe Text)
  , _aEndTime              :: !(Maybe ISO8601)
  , _aDetails              :: !(Maybe Text)
  , _aDescription          :: !(Maybe Text)
  , _aActivityId           :: !Text
  , _aAutoScalingGroupName :: !Text
  , _aCause                :: !Text
  , _aStartTime            :: !ISO8601
  , _aStatusCode           :: !ScalingActivityStatusCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Activity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aProgress' - A value between 0 and 100 that indicates the progress of the activity.
--
-- * 'aStatusMessage' - A friendly, more verbose description of the activity status.
--
-- * 'aEndTime' - The end time of the activity.
--
-- * 'aDetails' - The details about the activity.
--
-- * 'aDescription' - A friendly, more verbose description of the activity.
--
-- * 'aActivityId' - The ID of the activity.
--
-- * 'aAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'aCause' - The reason the activity began.
--
-- * 'aStartTime' - The start time of the activity.
--
-- * 'aStatusCode' - The current status of the activity.
activity
    :: Text -- ^ 'aActivityId'
    -> Text -- ^ 'aAutoScalingGroupName'
    -> Text -- ^ 'aCause'
    -> UTCTime -- ^ 'aStartTime'
    -> ScalingActivityStatusCode -- ^ 'aStatusCode'
    -> Activity
activity pActivityId_ pAutoScalingGroupName_ pCause_ pStartTime_ pStatusCode_ =
  Activity'
    { _aProgress = Nothing
    , _aStatusMessage = Nothing
    , _aEndTime = Nothing
    , _aDetails = Nothing
    , _aDescription = Nothing
    , _aActivityId = pActivityId_
    , _aAutoScalingGroupName = pAutoScalingGroupName_
    , _aCause = pCause_
    , _aStartTime = _Time # pStartTime_
    , _aStatusCode = pStatusCode_
    }


-- | A value between 0 and 100 that indicates the progress of the activity.
aProgress :: Lens' Activity (Maybe Int)
aProgress = lens _aProgress (\ s a -> s{_aProgress = a})

-- | A friendly, more verbose description of the activity status.
aStatusMessage :: Lens' Activity (Maybe Text)
aStatusMessage = lens _aStatusMessage (\ s a -> s{_aStatusMessage = a})

-- | The end time of the activity.
aEndTime :: Lens' Activity (Maybe UTCTime)
aEndTime = lens _aEndTime (\ s a -> s{_aEndTime = a}) . mapping _Time

-- | The details about the activity.
aDetails :: Lens' Activity (Maybe Text)
aDetails = lens _aDetails (\ s a -> s{_aDetails = a})

-- | A friendly, more verbose description of the activity.
aDescription :: Lens' Activity (Maybe Text)
aDescription = lens _aDescription (\ s a -> s{_aDescription = a})

-- | The ID of the activity.
aActivityId :: Lens' Activity Text
aActivityId = lens _aActivityId (\ s a -> s{_aActivityId = a})

-- | The name of the Auto Scaling group.
aAutoScalingGroupName :: Lens' Activity Text
aAutoScalingGroupName = lens _aAutoScalingGroupName (\ s a -> s{_aAutoScalingGroupName = a})

-- | The reason the activity began.
aCause :: Lens' Activity Text
aCause = lens _aCause (\ s a -> s{_aCause = a})

-- | The start time of the activity.
aStartTime :: Lens' Activity UTCTime
aStartTime = lens _aStartTime (\ s a -> s{_aStartTime = a}) . _Time

-- | The current status of the activity.
aStatusCode :: Lens' Activity ScalingActivityStatusCode
aStatusCode = lens _aStatusCode (\ s a -> s{_aStatusCode = a})

instance FromXML Activity where
        parseXML x
          = Activity' <$>
              (x .@? "Progress") <*> (x .@? "StatusMessage") <*>
                (x .@? "EndTime")
                <*> (x .@? "Details")
                <*> (x .@? "Description")
                <*> (x .@ "ActivityId")
                <*> (x .@ "AutoScalingGroupName")
                <*> (x .@ "Cause")
                <*> (x .@ "StartTime")
                <*> (x .@ "StatusCode")

instance Hashable Activity where

instance NFData Activity where

-- | Describes a policy adjustment type.
--
--
--
-- /See:/ 'adjustmentType' smart constructor.
newtype AdjustmentType = AdjustmentType'
  { _atAdjustmentType :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdjustmentType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atAdjustmentType' - The policy adjustment type. The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
adjustmentType
    :: AdjustmentType
adjustmentType = AdjustmentType' {_atAdjustmentType = Nothing}


-- | The policy adjustment type. The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
atAdjustmentType :: Lens' AdjustmentType (Maybe Text)
atAdjustmentType = lens _atAdjustmentType (\ s a -> s{_atAdjustmentType = a})

instance FromXML AdjustmentType where
        parseXML x
          = AdjustmentType' <$> (x .@? "AdjustmentType")

instance Hashable AdjustmentType where

instance NFData AdjustmentType where

-- | Describes an alarm.
--
--
--
-- /See:/ 'alarm' smart constructor.
data Alarm = Alarm'
  { _aAlarmName :: !(Maybe Text)
  , _aAlarmARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Alarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAlarmName' - The name of the alarm.
--
-- * 'aAlarmARN' - The Amazon Resource Name (ARN) of the alarm.
alarm
    :: Alarm
alarm = Alarm' {_aAlarmName = Nothing, _aAlarmARN = Nothing}


-- | The name of the alarm.
aAlarmName :: Lens' Alarm (Maybe Text)
aAlarmName = lens _aAlarmName (\ s a -> s{_aAlarmName = a})

-- | The Amazon Resource Name (ARN) of the alarm.
aAlarmARN :: Lens' Alarm (Maybe Text)
aAlarmARN = lens _aAlarmARN (\ s a -> s{_aAlarmARN = a})

instance FromXML Alarm where
        parseXML x
          = Alarm' <$>
              (x .@? "AlarmName") <*> (x .@? "AlarmARN")

instance Hashable Alarm where

instance NFData Alarm where

-- | Describes an Auto Scaling group.
--
--
--
-- /See:/ 'autoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { _asgStatus                           :: !(Maybe Text)
  , _asgTerminationPolicies              :: !(Maybe [Text])
  , _asgHealthCheckGracePeriod           :: !(Maybe Int)
  , _asgServiceLinkedRoleARN             :: !(Maybe Text)
  , _asgNewInstancesProtectedFromScaleIn :: !(Maybe Bool)
  , _asgVPCZoneIdentifier                :: !(Maybe Text)
  , _asgTargetGroupARNs                  :: !(Maybe [Text])
  , _asgMixedInstancesPolicy             :: !(Maybe MixedInstancesPolicy)
  , _asgEnabledMetrics                   :: !(Maybe [EnabledMetric])
  , _asgLaunchConfigurationName          :: !(Maybe Text)
  , _asgInstances                        :: !(Maybe [Instance])
  , _asgLaunchTemplate                   :: !(Maybe LaunchTemplateSpecification)
  , _asgAutoScalingGroupARN              :: !(Maybe Text)
  , _asgPlacementGroup                   :: !(Maybe Text)
  , _asgSuspendedProcesses               :: !(Maybe [SuspendedProcess])
  , _asgLoadBalancerNames                :: !(Maybe [Text])
  , _asgTags                             :: !(Maybe [TagDescription])
  , _asgAutoScalingGroupName             :: !Text
  , _asgMinSize                          :: !Int
  , _asgMaxSize                          :: !Int
  , _asgDesiredCapacity                  :: !Int
  , _asgDefaultCooldown                  :: !Int
  , _asgAvailabilityZones                :: !(List1 Text)
  , _asgHealthCheckType                  :: !Text
  , _asgCreatedTime                      :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgStatus' - The current state of the group when 'DeleteAutoScalingGroup' is in progress.
--
-- * 'asgTerminationPolicies' - The termination policies for the group.
--
-- * 'asgHealthCheckGracePeriod' - The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service.
--
-- * 'asgServiceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf.
--
-- * 'asgNewInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in.
--
-- * 'asgVPCZoneIdentifier' - One or more subnet IDs, if applicable, separated by commas.
--
-- * 'asgTargetGroupARNs' - The Amazon Resource Names (ARN) of the target groups for your load balancer.
--
-- * 'asgMixedInstancesPolicy' - The mixed instances policy for the group.
--
-- * 'asgEnabledMetrics' - The metrics enabled for the group.
--
-- * 'asgLaunchConfigurationName' - The name of the associated launch configuration.
--
-- * 'asgInstances' - The EC2 instances associated with the group.
--
-- * 'asgLaunchTemplate' - The launch template for the group.
--
-- * 'asgAutoScalingGroupARN' - The Amazon Resource Name (ARN) of the Auto Scaling group.
--
-- * 'asgPlacementGroup' - The name of the placement group into which to launch your instances, if any.
--
-- * 'asgSuspendedProcesses' - The suspended processes associated with the group.
--
-- * 'asgLoadBalancerNames' - One or more load balancers associated with the group.
--
-- * 'asgTags' - The tags for the group.
--
-- * 'asgAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'asgMinSize' - The minimum size of the group.
--
-- * 'asgMaxSize' - The maximum size of the group.
--
-- * 'asgDesiredCapacity' - The desired size of the group.
--
-- * 'asgDefaultCooldown' - The amount of time, in seconds, after a scaling activity completes before another scaling activity can start.
--
-- * 'asgAvailabilityZones' - One or more Availability Zones for the group.
--
-- * 'asgHealthCheckType' - The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
--
-- * 'asgCreatedTime' - The date and time the group was created.
autoScalingGroup
    :: Text -- ^ 'asgAutoScalingGroupName'
    -> Int -- ^ 'asgMinSize'
    -> Int -- ^ 'asgMaxSize'
    -> Int -- ^ 'asgDesiredCapacity'
    -> Int -- ^ 'asgDefaultCooldown'
    -> NonEmpty Text -- ^ 'asgAvailabilityZones'
    -> Text -- ^ 'asgHealthCheckType'
    -> UTCTime -- ^ 'asgCreatedTime'
    -> AutoScalingGroup
autoScalingGroup pAutoScalingGroupName_ pMinSize_ pMaxSize_ pDesiredCapacity_ pDefaultCooldown_ pAvailabilityZones_ pHealthCheckType_ pCreatedTime_ =
  AutoScalingGroup'
    { _asgStatus = Nothing
    , _asgTerminationPolicies = Nothing
    , _asgHealthCheckGracePeriod = Nothing
    , _asgServiceLinkedRoleARN = Nothing
    , _asgNewInstancesProtectedFromScaleIn = Nothing
    , _asgVPCZoneIdentifier = Nothing
    , _asgTargetGroupARNs = Nothing
    , _asgMixedInstancesPolicy = Nothing
    , _asgEnabledMetrics = Nothing
    , _asgLaunchConfigurationName = Nothing
    , _asgInstances = Nothing
    , _asgLaunchTemplate = Nothing
    , _asgAutoScalingGroupARN = Nothing
    , _asgPlacementGroup = Nothing
    , _asgSuspendedProcesses = Nothing
    , _asgLoadBalancerNames = Nothing
    , _asgTags = Nothing
    , _asgAutoScalingGroupName = pAutoScalingGroupName_
    , _asgMinSize = pMinSize_
    , _asgMaxSize = pMaxSize_
    , _asgDesiredCapacity = pDesiredCapacity_
    , _asgDefaultCooldown = pDefaultCooldown_
    , _asgAvailabilityZones = _List1 # pAvailabilityZones_
    , _asgHealthCheckType = pHealthCheckType_
    , _asgCreatedTime = _Time # pCreatedTime_
    }


-- | The current state of the group when 'DeleteAutoScalingGroup' is in progress.
asgStatus :: Lens' AutoScalingGroup (Maybe Text)
asgStatus = lens _asgStatus (\ s a -> s{_asgStatus = a})

-- | The termination policies for the group.
asgTerminationPolicies :: Lens' AutoScalingGroup [Text]
asgTerminationPolicies = lens _asgTerminationPolicies (\ s a -> s{_asgTerminationPolicies = a}) . _Default . _Coerce

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service.
asgHealthCheckGracePeriod :: Lens' AutoScalingGroup (Maybe Int)
asgHealthCheckGracePeriod = lens _asgHealthCheckGracePeriod (\ s a -> s{_asgHealthCheckGracePeriod = a})

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf.
asgServiceLinkedRoleARN :: Lens' AutoScalingGroup (Maybe Text)
asgServiceLinkedRoleARN = lens _asgServiceLinkedRoleARN (\ s a -> s{_asgServiceLinkedRoleARN = a})

-- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in.
asgNewInstancesProtectedFromScaleIn :: Lens' AutoScalingGroup (Maybe Bool)
asgNewInstancesProtectedFromScaleIn = lens _asgNewInstancesProtectedFromScaleIn (\ s a -> s{_asgNewInstancesProtectedFromScaleIn = a})

-- | One or more subnet IDs, if applicable, separated by commas.
asgVPCZoneIdentifier :: Lens' AutoScalingGroup (Maybe Text)
asgVPCZoneIdentifier = lens _asgVPCZoneIdentifier (\ s a -> s{_asgVPCZoneIdentifier = a})

-- | The Amazon Resource Names (ARN) of the target groups for your load balancer.
asgTargetGroupARNs :: Lens' AutoScalingGroup [Text]
asgTargetGroupARNs = lens _asgTargetGroupARNs (\ s a -> s{_asgTargetGroupARNs = a}) . _Default . _Coerce

-- | The mixed instances policy for the group.
asgMixedInstancesPolicy :: Lens' AutoScalingGroup (Maybe MixedInstancesPolicy)
asgMixedInstancesPolicy = lens _asgMixedInstancesPolicy (\ s a -> s{_asgMixedInstancesPolicy = a})

-- | The metrics enabled for the group.
asgEnabledMetrics :: Lens' AutoScalingGroup [EnabledMetric]
asgEnabledMetrics = lens _asgEnabledMetrics (\ s a -> s{_asgEnabledMetrics = a}) . _Default . _Coerce

-- | The name of the associated launch configuration.
asgLaunchConfigurationName :: Lens' AutoScalingGroup (Maybe Text)
asgLaunchConfigurationName = lens _asgLaunchConfigurationName (\ s a -> s{_asgLaunchConfigurationName = a})

-- | The EC2 instances associated with the group.
asgInstances :: Lens' AutoScalingGroup [Instance]
asgInstances = lens _asgInstances (\ s a -> s{_asgInstances = a}) . _Default . _Coerce

-- | The launch template for the group.
asgLaunchTemplate :: Lens' AutoScalingGroup (Maybe LaunchTemplateSpecification)
asgLaunchTemplate = lens _asgLaunchTemplate (\ s a -> s{_asgLaunchTemplate = a})

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
asgAutoScalingGroupARN :: Lens' AutoScalingGroup (Maybe Text)
asgAutoScalingGroupARN = lens _asgAutoScalingGroupARN (\ s a -> s{_asgAutoScalingGroupARN = a})

-- | The name of the placement group into which to launch your instances, if any.
asgPlacementGroup :: Lens' AutoScalingGroup (Maybe Text)
asgPlacementGroup = lens _asgPlacementGroup (\ s a -> s{_asgPlacementGroup = a})

-- | The suspended processes associated with the group.
asgSuspendedProcesses :: Lens' AutoScalingGroup [SuspendedProcess]
asgSuspendedProcesses = lens _asgSuspendedProcesses (\ s a -> s{_asgSuspendedProcesses = a}) . _Default . _Coerce

-- | One or more load balancers associated with the group.
asgLoadBalancerNames :: Lens' AutoScalingGroup [Text]
asgLoadBalancerNames = lens _asgLoadBalancerNames (\ s a -> s{_asgLoadBalancerNames = a}) . _Default . _Coerce

-- | The tags for the group.
asgTags :: Lens' AutoScalingGroup [TagDescription]
asgTags = lens _asgTags (\ s a -> s{_asgTags = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group.
asgAutoScalingGroupName :: Lens' AutoScalingGroup Text
asgAutoScalingGroupName = lens _asgAutoScalingGroupName (\ s a -> s{_asgAutoScalingGroupName = a})

-- | The minimum size of the group.
asgMinSize :: Lens' AutoScalingGroup Int
asgMinSize = lens _asgMinSize (\ s a -> s{_asgMinSize = a})

-- | The maximum size of the group.
asgMaxSize :: Lens' AutoScalingGroup Int
asgMaxSize = lens _asgMaxSize (\ s a -> s{_asgMaxSize = a})

-- | The desired size of the group.
asgDesiredCapacity :: Lens' AutoScalingGroup Int
asgDesiredCapacity = lens _asgDesiredCapacity (\ s a -> s{_asgDesiredCapacity = a})

-- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start.
asgDefaultCooldown :: Lens' AutoScalingGroup Int
asgDefaultCooldown = lens _asgDefaultCooldown (\ s a -> s{_asgDefaultCooldown = a})

-- | One or more Availability Zones for the group.
asgAvailabilityZones :: Lens' AutoScalingGroup (NonEmpty Text)
asgAvailabilityZones = lens _asgAvailabilityZones (\ s a -> s{_asgAvailabilityZones = a}) . _List1

-- | The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
asgHealthCheckType :: Lens' AutoScalingGroup Text
asgHealthCheckType = lens _asgHealthCheckType (\ s a -> s{_asgHealthCheckType = a})

-- | The date and time the group was created.
asgCreatedTime :: Lens' AutoScalingGroup UTCTime
asgCreatedTime = lens _asgCreatedTime (\ s a -> s{_asgCreatedTime = a}) . _Time

instance FromXML AutoScalingGroup where
        parseXML x
          = AutoScalingGroup' <$>
              (x .@? "Status") <*>
                (x .@? "TerminationPolicies" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "HealthCheckGracePeriod")
                <*> (x .@? "ServiceLinkedRoleARN")
                <*> (x .@? "NewInstancesProtectedFromScaleIn")
                <*> (x .@? "VPCZoneIdentifier")
                <*>
                (x .@? "TargetGroupARNs" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "MixedInstancesPolicy")
                <*>
                (x .@? "EnabledMetrics" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LaunchConfigurationName")
                <*>
                (x .@? "Instances" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LaunchTemplate")
                <*> (x .@? "AutoScalingGroupARN")
                <*> (x .@? "PlacementGroup")
                <*>
                (x .@? "SuspendedProcesses" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "LoadBalancerNames" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@ "AutoScalingGroupName")
                <*> (x .@ "MinSize")
                <*> (x .@ "MaxSize")
                <*> (x .@ "DesiredCapacity")
                <*> (x .@ "DefaultCooldown")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   parseXMLList1 "member")
                <*> (x .@ "HealthCheckType")
                <*> (x .@ "CreatedTime")

instance Hashable AutoScalingGroup where

instance NFData AutoScalingGroup where

-- | Describes an EC2 instance associated with an Auto Scaling group.
--
--
--
-- /See:/ 'autoScalingInstanceDetails' smart constructor.
data AutoScalingInstanceDetails = AutoScalingInstanceDetails'
  { _asidLaunchConfigurationName :: !(Maybe Text)
  , _asidLaunchTemplate          :: !(Maybe LaunchTemplateSpecification)
  , _asidInstanceId              :: !Text
  , _asidAutoScalingGroupName    :: !Text
  , _asidAvailabilityZone        :: !Text
  , _asidLifecycleState          :: !Text
  , _asidHealthStatus            :: !Text
  , _asidProtectedFromScaleIn    :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingInstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asidLaunchConfigurationName' - The launch configuration used to launch the instance. This value is not available if you attached the instance to the Auto Scaling group.
--
-- * 'asidLaunchTemplate' - The launch template for the instance.
--
-- * 'asidInstanceId' - The ID of the instance.
--
-- * 'asidAutoScalingGroupName' - The name of the Auto Scaling group for the instance.
--
-- * 'asidAvailabilityZone' - The Availability Zone for the instance.
--
-- * 'asidLifecycleState' - The lifecycle state for the instance.
--
-- * 'asidHealthStatus' - The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Amazon EC2 Auto Scaling should terminate and replace it.
--
-- * 'asidProtectedFromScaleIn' - Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
autoScalingInstanceDetails
    :: Text -- ^ 'asidInstanceId'
    -> Text -- ^ 'asidAutoScalingGroupName'
    -> Text -- ^ 'asidAvailabilityZone'
    -> Text -- ^ 'asidLifecycleState'
    -> Text -- ^ 'asidHealthStatus'
    -> Bool -- ^ 'asidProtectedFromScaleIn'
    -> AutoScalingInstanceDetails
autoScalingInstanceDetails pInstanceId_ pAutoScalingGroupName_ pAvailabilityZone_ pLifecycleState_ pHealthStatus_ pProtectedFromScaleIn_ =
  AutoScalingInstanceDetails'
    { _asidLaunchConfigurationName = Nothing
    , _asidLaunchTemplate = Nothing
    , _asidInstanceId = pInstanceId_
    , _asidAutoScalingGroupName = pAutoScalingGroupName_
    , _asidAvailabilityZone = pAvailabilityZone_
    , _asidLifecycleState = pLifecycleState_
    , _asidHealthStatus = pHealthStatus_
    , _asidProtectedFromScaleIn = pProtectedFromScaleIn_
    }


-- | The launch configuration used to launch the instance. This value is not available if you attached the instance to the Auto Scaling group.
asidLaunchConfigurationName :: Lens' AutoScalingInstanceDetails (Maybe Text)
asidLaunchConfigurationName = lens _asidLaunchConfigurationName (\ s a -> s{_asidLaunchConfigurationName = a})

-- | The launch template for the instance.
asidLaunchTemplate :: Lens' AutoScalingInstanceDetails (Maybe LaunchTemplateSpecification)
asidLaunchTemplate = lens _asidLaunchTemplate (\ s a -> s{_asidLaunchTemplate = a})

-- | The ID of the instance.
asidInstanceId :: Lens' AutoScalingInstanceDetails Text
asidInstanceId = lens _asidInstanceId (\ s a -> s{_asidInstanceId = a})

-- | The name of the Auto Scaling group for the instance.
asidAutoScalingGroupName :: Lens' AutoScalingInstanceDetails Text
asidAutoScalingGroupName = lens _asidAutoScalingGroupName (\ s a -> s{_asidAutoScalingGroupName = a})

-- | The Availability Zone for the instance.
asidAvailabilityZone :: Lens' AutoScalingInstanceDetails Text
asidAvailabilityZone = lens _asidAvailabilityZone (\ s a -> s{_asidAvailabilityZone = a})

-- | The lifecycle state for the instance.
asidLifecycleState :: Lens' AutoScalingInstanceDetails Text
asidLifecycleState = lens _asidLifecycleState (\ s a -> s{_asidLifecycleState = a})

-- | The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Amazon EC2 Auto Scaling should terminate and replace it.
asidHealthStatus :: Lens' AutoScalingInstanceDetails Text
asidHealthStatus = lens _asidHealthStatus (\ s a -> s{_asidHealthStatus = a})

-- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
asidProtectedFromScaleIn :: Lens' AutoScalingInstanceDetails Bool
asidProtectedFromScaleIn = lens _asidProtectedFromScaleIn (\ s a -> s{_asidProtectedFromScaleIn = a})

instance FromXML AutoScalingInstanceDetails where
        parseXML x
          = AutoScalingInstanceDetails' <$>
              (x .@? "LaunchConfigurationName") <*>
                (x .@? "LaunchTemplate")
                <*> (x .@ "InstanceId")
                <*> (x .@ "AutoScalingGroupName")
                <*> (x .@ "AvailabilityZone")
                <*> (x .@ "LifecycleState")
                <*> (x .@ "HealthStatus")
                <*> (x .@ "ProtectedFromScaleIn")

instance Hashable AutoScalingInstanceDetails where

instance NFData AutoScalingInstanceDetails where

-- | Describes a block device mapping.
--
--
--
-- /See:/ 'blockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { _bdmVirtualName :: !(Maybe Text)
  , _bdmNoDevice    :: !(Maybe Bool)
  , _bdmEBS         :: !(Maybe EBS)
  , _bdmDeviceName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BlockDeviceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdmVirtualName' - The name of the virtual device (for example, @ephemeral0@ ).
--
-- * 'bdmNoDevice' - Suppresses a device mapping. If this parameter is true for the root device, the instance might fail the EC2 health check. In that case, Amazon EC2 Auto Scaling launches a replacement instance.
--
-- * 'bdmEBS' - The information about the Amazon EBS volume.
--
-- * 'bdmDeviceName' - The device name exposed to the EC2 instance (for example, @/dev/sdh@ or @xvdh@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
blockDeviceMapping
    :: Text -- ^ 'bdmDeviceName'
    -> BlockDeviceMapping
blockDeviceMapping pDeviceName_ =
  BlockDeviceMapping'
    { _bdmVirtualName = Nothing
    , _bdmNoDevice = Nothing
    , _bdmEBS = Nothing
    , _bdmDeviceName = pDeviceName_
    }


-- | The name of the virtual device (for example, @ephemeral0@ ).
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\ s a -> s{_bdmVirtualName = a})

-- | Suppresses a device mapping. If this parameter is true for the root device, the instance might fail the EC2 health check. In that case, Amazon EC2 Auto Scaling launches a replacement instance.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Bool)
bdmNoDevice = lens _bdmNoDevice (\ s a -> s{_bdmNoDevice = a})

-- | The information about the Amazon EBS volume.
bdmEBS :: Lens' BlockDeviceMapping (Maybe EBS)
bdmEBS = lens _bdmEBS (\ s a -> s{_bdmEBS = a})

-- | The device name exposed to the EC2 instance (for example, @/dev/sdh@ or @xvdh@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\ s a -> s{_bdmDeviceName = a})

instance FromXML BlockDeviceMapping where
        parseXML x
          = BlockDeviceMapping' <$>
              (x .@? "VirtualName") <*> (x .@? "NoDevice") <*>
                (x .@? "Ebs")
                <*> (x .@ "DeviceName")

instance Hashable BlockDeviceMapping where

instance NFData BlockDeviceMapping where

instance ToQuery BlockDeviceMapping where
        toQuery BlockDeviceMapping'{..}
          = mconcat
              ["VirtualName" =: _bdmVirtualName,
               "NoDevice" =: _bdmNoDevice, "Ebs" =: _bdmEBS,
               "DeviceName" =: _bdmDeviceName]

-- | Represents a CloudWatch metric of your choosing for a target tracking scaling policy to use with Amazon EC2 Auto Scaling.
--
--
-- To create your customized metric specification:
--
--     * Add values for each required parameter from CloudWatch. You can use an existing metric, or a new metric that you create. To use your own metric, you must first publish the metric to CloudWatch. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publish Custom Metrics> in the /Amazon CloudWatch User Guide/ .
--
--     * Choose a metric that changes proportionally with capacity. The value of the metric should increase or decrease in inverse proportion to the number of capacity units. That is, the value of the metric should decrease when capacity increases.
--
--
--
-- For more information about CloudWatch, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts> .
--
--
-- /See:/ 'customizedMetricSpecification' smart constructor.
data CustomizedMetricSpecification = CustomizedMetricSpecification'
  { _cmsDimensions :: !(Maybe [MetricDimension])
  , _cmsUnit       :: !(Maybe Text)
  , _cmsMetricName :: !Text
  , _cmsNamespace  :: !Text
  , _cmsStatistic  :: !MetricStatistic
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomizedMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmsDimensions' - The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
--
-- * 'cmsUnit' - The unit of the metric.
--
-- * 'cmsMetricName' - The name of the metric.
--
-- * 'cmsNamespace' - The namespace of the metric.
--
-- * 'cmsStatistic' - The statistic of the metric.
customizedMetricSpecification
    :: Text -- ^ 'cmsMetricName'
    -> Text -- ^ 'cmsNamespace'
    -> MetricStatistic -- ^ 'cmsStatistic'
    -> CustomizedMetricSpecification
customizedMetricSpecification pMetricName_ pNamespace_ pStatistic_ =
  CustomizedMetricSpecification'
    { _cmsDimensions = Nothing
    , _cmsUnit = Nothing
    , _cmsMetricName = pMetricName_
    , _cmsNamespace = pNamespace_
    , _cmsStatistic = pStatistic_
    }


-- | The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
cmsDimensions :: Lens' CustomizedMetricSpecification [MetricDimension]
cmsDimensions = lens _cmsDimensions (\ s a -> s{_cmsDimensions = a}) . _Default . _Coerce

-- | The unit of the metric.
cmsUnit :: Lens' CustomizedMetricSpecification (Maybe Text)
cmsUnit = lens _cmsUnit (\ s a -> s{_cmsUnit = a})

-- | The name of the metric.
cmsMetricName :: Lens' CustomizedMetricSpecification Text
cmsMetricName = lens _cmsMetricName (\ s a -> s{_cmsMetricName = a})

-- | The namespace of the metric.
cmsNamespace :: Lens' CustomizedMetricSpecification Text
cmsNamespace = lens _cmsNamespace (\ s a -> s{_cmsNamespace = a})

-- | The statistic of the metric.
cmsStatistic :: Lens' CustomizedMetricSpecification MetricStatistic
cmsStatistic = lens _cmsStatistic (\ s a -> s{_cmsStatistic = a})

instance FromXML CustomizedMetricSpecification where
        parseXML x
          = CustomizedMetricSpecification' <$>
              (x .@? "Dimensions" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "Unit")
                <*> (x .@ "MetricName")
                <*> (x .@ "Namespace")
                <*> (x .@ "Statistic")

instance Hashable CustomizedMetricSpecification where

instance NFData CustomizedMetricSpecification where

instance ToQuery CustomizedMetricSpecification where
        toQuery CustomizedMetricSpecification'{..}
          = mconcat
              ["Dimensions" =:
                 toQuery (toQueryList "member" <$> _cmsDimensions),
               "Unit" =: _cmsUnit, "MetricName" =: _cmsMetricName,
               "Namespace" =: _cmsNamespace,
               "Statistic" =: _cmsStatistic]

-- | Describes an Amazon EBS volume. Used in combination with 'BlockDeviceMapping' .
--
--
--
-- /See:/ 'ebs' smart constructor.
data EBS = EBS'
  { _ebsDeleteOnTermination :: !(Maybe Bool)
  , _ebsVolumeSize          :: !(Maybe Nat)
  , _ebsIOPS                :: !(Maybe Nat)
  , _ebsEncrypted           :: !(Maybe Bool)
  , _ebsVolumeType          :: !(Maybe Text)
  , _ebsSnapshotId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EBS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebsDeleteOnTermination' - Indicates whether the volume is deleted on instance termination. The default value is @true@ .
--
-- * 'ebsVolumeSize' - The volume size, in GiB.  Constraints: 1-1,024 for @standard@ , 4-16,384 for @io1@ , 1-16,384 for @gp2@ , and 500-16,384 for @st1@ and @sc1@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you create a volume from a snapshot and you don't specify a volume size, the default is the snapshot size.
--
-- * 'ebsIOPS' - The number of I/O operations per second (IOPS) to provision for the volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ . Conditional: This parameter is required when the volume type is @io1@ . (Not used with @standard@ , @gp2@ , @st1@ , or @sc1@ volumes.)
--
-- * 'ebsEncrypted' - Specifies whether the volume should be encrypted. Encrypted EBS volumes must be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are automatically encrypted. There is no way to create an encrypted volume from an unencrypted snapshot or an unencrypted volume from an encrypted snapshot. If your AMI uses encrypted volumes, you can only launch it on supported instance types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'ebsVolumeType' - The volume type, which can be @standard@ for Magnetic, @io1@ for Provisioned IOPS SSD, @gp2@ for General Purpose SSD, @st1@ for Throughput Optimized HDD, or @sc1@ for Cold HDD. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ . Valid values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@
--
-- * 'ebsSnapshotId' - The ID of the snapshot. This parameter is optional if you specify a volume size.
ebs
    :: EBS
ebs =
  EBS'
    { _ebsDeleteOnTermination = Nothing
    , _ebsVolumeSize = Nothing
    , _ebsIOPS = Nothing
    , _ebsEncrypted = Nothing
    , _ebsVolumeType = Nothing
    , _ebsSnapshotId = Nothing
    }


-- | Indicates whether the volume is deleted on instance termination. The default value is @true@ .
ebsDeleteOnTermination :: Lens' EBS (Maybe Bool)
ebsDeleteOnTermination = lens _ebsDeleteOnTermination (\ s a -> s{_ebsDeleteOnTermination = a})

-- | The volume size, in GiB.  Constraints: 1-1,024 for @standard@ , 4-16,384 for @io1@ , 1-16,384 for @gp2@ , and 500-16,384 for @st1@ and @sc1@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you create a volume from a snapshot and you don't specify a volume size, the default is the snapshot size.
ebsVolumeSize :: Lens' EBS (Maybe Natural)
ebsVolumeSize = lens _ebsVolumeSize (\ s a -> s{_ebsVolumeSize = a}) . mapping _Nat

-- | The number of I/O operations per second (IOPS) to provision for the volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ . Conditional: This parameter is required when the volume type is @io1@ . (Not used with @standard@ , @gp2@ , @st1@ , or @sc1@ volumes.)
ebsIOPS :: Lens' EBS (Maybe Natural)
ebsIOPS = lens _ebsIOPS (\ s a -> s{_ebsIOPS = a}) . mapping _Nat

-- | Specifies whether the volume should be encrypted. Encrypted EBS volumes must be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are automatically encrypted. There is no way to create an encrypted volume from an unencrypted snapshot or an unencrypted volume from an encrypted snapshot. If your AMI uses encrypted volumes, you can only launch it on supported instance types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon EC2 User Guide for Linux Instances/ .
ebsEncrypted :: Lens' EBS (Maybe Bool)
ebsEncrypted = lens _ebsEncrypted (\ s a -> s{_ebsEncrypted = a})

-- | The volume type, which can be @standard@ for Magnetic, @io1@ for Provisioned IOPS SSD, @gp2@ for General Purpose SSD, @st1@ for Throughput Optimized HDD, or @sc1@ for Cold HDD. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ . Valid values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@
ebsVolumeType :: Lens' EBS (Maybe Text)
ebsVolumeType = lens _ebsVolumeType (\ s a -> s{_ebsVolumeType = a})

-- | The ID of the snapshot. This parameter is optional if you specify a volume size.
ebsSnapshotId :: Lens' EBS (Maybe Text)
ebsSnapshotId = lens _ebsSnapshotId (\ s a -> s{_ebsSnapshotId = a})

instance FromXML EBS where
        parseXML x
          = EBS' <$>
              (x .@? "DeleteOnTermination") <*>
                (x .@? "VolumeSize")
                <*> (x .@? "Iops")
                <*> (x .@? "Encrypted")
                <*> (x .@? "VolumeType")
                <*> (x .@? "SnapshotId")

instance Hashable EBS where

instance NFData EBS where

instance ToQuery EBS where
        toQuery EBS'{..}
          = mconcat
              ["DeleteOnTermination" =: _ebsDeleteOnTermination,
               "VolumeSize" =: _ebsVolumeSize, "Iops" =: _ebsIOPS,
               "Encrypted" =: _ebsEncrypted,
               "VolumeType" =: _ebsVolumeType,
               "SnapshotId" =: _ebsSnapshotId]

-- | Describes an enabled metric.
--
--
--
-- /See:/ 'enabledMetric' smart constructor.
data EnabledMetric = EnabledMetric'
  { _emGranularity :: !(Maybe Text)
  , _emMetric      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnabledMetric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emGranularity' - The granularity of the metric. The only valid value is @1Minute@ .
--
-- * 'emMetric' - One of the following metrics:     * @GroupMinSize@      * @GroupMaxSize@      * @GroupDesiredCapacity@      * @GroupInServiceInstances@      * @GroupPendingInstances@      * @GroupStandbyInstances@      * @GroupTerminatingInstances@      * @GroupTotalInstances@
enabledMetric
    :: EnabledMetric
enabledMetric = EnabledMetric' {_emGranularity = Nothing, _emMetric = Nothing}


-- | The granularity of the metric. The only valid value is @1Minute@ .
emGranularity :: Lens' EnabledMetric (Maybe Text)
emGranularity = lens _emGranularity (\ s a -> s{_emGranularity = a})

-- | One of the following metrics:     * @GroupMinSize@      * @GroupMaxSize@      * @GroupDesiredCapacity@      * @GroupInServiceInstances@      * @GroupPendingInstances@      * @GroupStandbyInstances@      * @GroupTerminatingInstances@      * @GroupTotalInstances@
emMetric :: Lens' EnabledMetric (Maybe Text)
emMetric = lens _emMetric (\ s a -> s{_emMetric = a})

instance FromXML EnabledMetric where
        parseXML x
          = EnabledMetric' <$>
              (x .@? "Granularity") <*> (x .@? "Metric")

instance Hashable EnabledMetric where

instance NFData EnabledMetric where

-- | Describes a scheduled action that could not be created, updated, or deleted.
--
--
--
-- /See:/ 'failedScheduledUpdateGroupActionRequest' smart constructor.
data FailedScheduledUpdateGroupActionRequest = FailedScheduledUpdateGroupActionRequest'
  { _fsugarErrorCode           :: !(Maybe Text)
  , _fsugarErrorMessage        :: !(Maybe Text)
  , _fsugarScheduledActionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FailedScheduledUpdateGroupActionRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsugarErrorCode' - The error code.
--
-- * 'fsugarErrorMessage' - The error message accompanying the error code.
--
-- * 'fsugarScheduledActionName' - The name of the scheduled action.
failedScheduledUpdateGroupActionRequest
    :: Text -- ^ 'fsugarScheduledActionName'
    -> FailedScheduledUpdateGroupActionRequest
failedScheduledUpdateGroupActionRequest pScheduledActionName_ =
  FailedScheduledUpdateGroupActionRequest'
    { _fsugarErrorCode = Nothing
    , _fsugarErrorMessage = Nothing
    , _fsugarScheduledActionName = pScheduledActionName_
    }


-- | The error code.
fsugarErrorCode :: Lens' FailedScheduledUpdateGroupActionRequest (Maybe Text)
fsugarErrorCode = lens _fsugarErrorCode (\ s a -> s{_fsugarErrorCode = a})

-- | The error message accompanying the error code.
fsugarErrorMessage :: Lens' FailedScheduledUpdateGroupActionRequest (Maybe Text)
fsugarErrorMessage = lens _fsugarErrorMessage (\ s a -> s{_fsugarErrorMessage = a})

-- | The name of the scheduled action.
fsugarScheduledActionName :: Lens' FailedScheduledUpdateGroupActionRequest Text
fsugarScheduledActionName = lens _fsugarScheduledActionName (\ s a -> s{_fsugarScheduledActionName = a})

instance FromXML
           FailedScheduledUpdateGroupActionRequest
         where
        parseXML x
          = FailedScheduledUpdateGroupActionRequest' <$>
              (x .@? "ErrorCode") <*> (x .@? "ErrorMessage") <*>
                (x .@ "ScheduledActionName")

instance Hashable
           FailedScheduledUpdateGroupActionRequest
         where

instance NFData
           FailedScheduledUpdateGroupActionRequest
         where

-- | Describes a filter.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fValues :: !(Maybe [Text])
  , _fName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fValues' - The value of the filter.
--
-- * 'fName' - The name of the filter. The valid values are: @"auto-scaling-group"@ , @"key"@ , @"value"@ , and @"propagate-at-launch"@ .
filter'
    :: Text -- ^ 'fName'
    -> Filter
filter' pName_ = Filter' {_fValues = Nothing, _fName = pName_}


-- | The value of the filter.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Default . _Coerce

-- | The name of the filter. The valid values are: @"auto-scaling-group"@ , @"key"@ , @"value"@ , and @"propagate-at-launch"@ .
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a})

instance Hashable Filter where

instance NFData Filter where

instance ToQuery Filter where
        toQuery Filter'{..}
          = mconcat
              ["Values" =:
                 toQuery (toQueryList "member" <$> _fValues),
               "Name" =: _fName]

-- | Describes an EC2 instance.
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
  { _iLaunchConfigurationName :: !(Maybe Text)
  , _iLaunchTemplate          :: !(Maybe LaunchTemplateSpecification)
  , _iInstanceId              :: !Text
  , _iAvailabilityZone        :: !Text
  , _iLifecycleState          :: !LifecycleState
  , _iHealthStatus            :: !Text
  , _iProtectedFromScaleIn    :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iLaunchConfigurationName' - The launch configuration associated with the instance.
--
-- * 'iLaunchTemplate' - The launch template for the instance.
--
-- * 'iInstanceId' - The ID of the instance.
--
-- * 'iAvailabilityZone' - The Availability Zone in which the instance is running.
--
-- * 'iLifecycleState' - A description of the current lifecycle state. The @Quarantined@ state is not used.
--
-- * 'iHealthStatus' - The last reported health status of the instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and that Amazon EC2 Auto Scaling should terminate and replace it.
--
-- * 'iProtectedFromScaleIn' - Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
instance'
    :: Text -- ^ 'iInstanceId'
    -> Text -- ^ 'iAvailabilityZone'
    -> LifecycleState -- ^ 'iLifecycleState'
    -> Text -- ^ 'iHealthStatus'
    -> Bool -- ^ 'iProtectedFromScaleIn'
    -> Instance
instance' pInstanceId_ pAvailabilityZone_ pLifecycleState_ pHealthStatus_ pProtectedFromScaleIn_ =
  Instance'
    { _iLaunchConfigurationName = Nothing
    , _iLaunchTemplate = Nothing
    , _iInstanceId = pInstanceId_
    , _iAvailabilityZone = pAvailabilityZone_
    , _iLifecycleState = pLifecycleState_
    , _iHealthStatus = pHealthStatus_
    , _iProtectedFromScaleIn = pProtectedFromScaleIn_
    }


-- | The launch configuration associated with the instance.
iLaunchConfigurationName :: Lens' Instance (Maybe Text)
iLaunchConfigurationName = lens _iLaunchConfigurationName (\ s a -> s{_iLaunchConfigurationName = a})

-- | The launch template for the instance.
iLaunchTemplate :: Lens' Instance (Maybe LaunchTemplateSpecification)
iLaunchTemplate = lens _iLaunchTemplate (\ s a -> s{_iLaunchTemplate = a})

-- | The ID of the instance.
iInstanceId :: Lens' Instance Text
iInstanceId = lens _iInstanceId (\ s a -> s{_iInstanceId = a})

-- | The Availability Zone in which the instance is running.
iAvailabilityZone :: Lens' Instance Text
iAvailabilityZone = lens _iAvailabilityZone (\ s a -> s{_iAvailabilityZone = a})

-- | A description of the current lifecycle state. The @Quarantined@ state is not used.
iLifecycleState :: Lens' Instance LifecycleState
iLifecycleState = lens _iLifecycleState (\ s a -> s{_iLifecycleState = a})

-- | The last reported health status of the instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and that Amazon EC2 Auto Scaling should terminate and replace it.
iHealthStatus :: Lens' Instance Text
iHealthStatus = lens _iHealthStatus (\ s a -> s{_iHealthStatus = a})

-- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
iProtectedFromScaleIn :: Lens' Instance Bool
iProtectedFromScaleIn = lens _iProtectedFromScaleIn (\ s a -> s{_iProtectedFromScaleIn = a})

instance FromXML Instance where
        parseXML x
          = Instance' <$>
              (x .@? "LaunchConfigurationName") <*>
                (x .@? "LaunchTemplate")
                <*> (x .@ "InstanceId")
                <*> (x .@ "AvailabilityZone")
                <*> (x .@ "LifecycleState")
                <*> (x .@ "HealthStatus")
                <*> (x .@ "ProtectedFromScaleIn")

instance Hashable Instance where

instance NFData Instance where

-- | Describes whether detailed monitoring is enabled for the Auto Scaling instances.
--
--
--
-- /See:/ 'instanceMonitoring' smart constructor.
newtype InstanceMonitoring = InstanceMonitoring'
  { _imEnabled :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imEnabled' - If @true@ , detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
instanceMonitoring
    :: InstanceMonitoring
instanceMonitoring = InstanceMonitoring' {_imEnabled = Nothing}


-- | If @true@ , detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
imEnabled :: Lens' InstanceMonitoring (Maybe Bool)
imEnabled = lens _imEnabled (\ s a -> s{_imEnabled = a})

instance FromXML InstanceMonitoring where
        parseXML x
          = InstanceMonitoring' <$> (x .@? "Enabled")

instance Hashable InstanceMonitoring where

instance NFData InstanceMonitoring where

instance ToQuery InstanceMonitoring where
        toQuery InstanceMonitoring'{..}
          = mconcat ["Enabled" =: _imEnabled]

-- | Describes an instances distribution for an Auto Scaling group with 'MixedInstancesPolicy' .
--
--
-- The instances distribution specifies the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types.
--
--
-- /See:/ 'instancesDistribution' smart constructor.
data InstancesDistribution = InstancesDistribution'
  { _idSpotAllocationStrategy              :: !(Maybe Text)
  , _idSpotInstancePools                   :: !(Maybe Int)
  , _idSpotMaxPrice                        :: !(Maybe Text)
  , _idOnDemandBaseCapacity                :: !(Maybe Int)
  , _idOnDemandAllocationStrategy          :: !(Maybe Text)
  , _idOnDemandPercentageAboveBaseCapacity :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstancesDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idSpotAllocationStrategy' - Indicates how to allocate Spot capacity across Spot pools. The only valid value is @lowest-price@ , which is also the default value. The Auto Scaling group selects the cheapest Spot pools and evenly allocates your Spot capacity across the number of Spot pools that you specify.
--
-- * 'idSpotInstancePools' - The number of Spot pools to use to allocate your Spot capacity. The Spot pools are determined from the different instance types in the Overrides array of 'LaunchTemplate' .  The range is 1
