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
-- For more information, see <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/as-scale-based-on-demand.html Dynamic Scaling> in the /Auto Scaling User Guide/ .
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
-- * 'asgHealthCheckGracePeriod' - The amount of time, in seconds, that Auto Scaling waits before checking the health status of an EC2 instance that has come into service.
--
-- * 'asgServiceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf.
--
-- * 'asgNewInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from termination by Auto Scaling when scaling in.
--
-- * 'asgVPCZoneIdentifier' - One or more subnet IDs, if applicable, separated by commas. If you specify @VPCZoneIdentifier@ and @AvailabilityZones@ , ensure that the Availability Zones of the subnets match the values for @AvailabilityZones@ .
--
-- * 'asgTargetGroupARNs' - The Amazon Resource Names (ARN) of the target groups for your load balancer.
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
-- * 'asgPlacementGroup' - The name of the placement group into which you'll launch your instances, if any. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon Elastic Compute Cloud User Guide/ .
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
-- * 'asgHealthCheckType' - The service to use for the health checks. The valid values are @EC2@ and @ELB@ .
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

-- | The amount of time, in seconds, that Auto Scaling waits before checking the health status of an EC2 instance that has come into service.
asgHealthCheckGracePeriod :: Lens' AutoScalingGroup (Maybe Int)
asgHealthCheckGracePeriod = lens _asgHealthCheckGracePeriod (\ s a -> s{_asgHealthCheckGracePeriod = a})

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf.
asgServiceLinkedRoleARN :: Lens' AutoScalingGroup (Maybe Text)
asgServiceLinkedRoleARN = lens _asgServiceLinkedRoleARN (\ s a -> s{_asgServiceLinkedRoleARN = a})

-- | Indicates whether newly launched instances are protected from termination by Auto Scaling when scaling in.
asgNewInstancesProtectedFromScaleIn :: Lens' AutoScalingGroup (Maybe Bool)
asgNewInstancesProtectedFromScaleIn = lens _asgNewInstancesProtectedFromScaleIn (\ s a -> s{_asgNewInstancesProtectedFromScaleIn = a})

-- | One or more subnet IDs, if applicable, separated by commas. If you specify @VPCZoneIdentifier@ and @AvailabilityZones@ , ensure that the Availability Zones of the subnets match the values for @AvailabilityZones@ .
asgVPCZoneIdentifier :: Lens' AutoScalingGroup (Maybe Text)
asgVPCZoneIdentifier = lens _asgVPCZoneIdentifier (\ s a -> s{_asgVPCZoneIdentifier = a})

-- | The Amazon Resource Names (ARN) of the target groups for your load balancer.
asgTargetGroupARNs :: Lens' AutoScalingGroup [Text]
asgTargetGroupARNs = lens _asgTargetGroupARNs (\ s a -> s{_asgTargetGroupARNs = a}) . _Default . _Coerce

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

-- | The name of the placement group into which you'll launch your instances, if any. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon Elastic Compute Cloud User Guide/ .
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

-- | The service to use for the health checks. The valid values are @EC2@ and @ELB@ .
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
-- * 'asidLifecycleState' - The lifecycle state for the instance. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/AutoScalingGroupLifecycle.html Auto Scaling Lifecycle> in the /Auto Scaling User Guide/ .
--
-- * 'asidHealthStatus' - The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Auto Scaling should terminate and replace it.
--
-- * 'asidProtectedFromScaleIn' - Indicates whether the instance is protected from termination by Auto Scaling when scaling in.
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

-- | The lifecycle state for the instance. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/AutoScalingGroupLifecycle.html Auto Scaling Lifecycle> in the /Auto Scaling User Guide/ .
asidLifecycleState :: Lens' AutoScalingInstanceDetails Text
asidLifecycleState = lens _asidLifecycleState (\ s a -> s{_asidLifecycleState = a})

-- | The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Auto Scaling should terminate and replace it.
asidHealthStatus :: Lens' AutoScalingInstanceDetails Text
asidHealthStatus = lens _asidHealthStatus (\ s a -> s{_asidHealthStatus = a})

-- | Indicates whether the instance is protected from termination by Auto Scaling when scaling in.
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
-- * 'bdmNoDevice' - Suppresses a device mapping. If this parameter is true for the root device, the instance might fail the EC2 health check. Auto Scaling launches a replacement instance if the instance fails the health check.
--
-- * 'bdmEBS' - The information about the Amazon EBS volume.
--
-- * 'bdmDeviceName' - The device name exposed to the EC2 instance (for example, @/dev/sdh@ or @xvdh@ ).
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

-- | Suppresses a device mapping. If this parameter is true for the root device, the instance might fail the EC2 health check. Auto Scaling launches a replacement instance if the instance fails the health check.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Bool)
bdmNoDevice = lens _bdmNoDevice (\ s a -> s{_bdmNoDevice = a})

-- | The information about the Amazon EBS volume.
bdmEBS :: Lens' BlockDeviceMapping (Maybe EBS)
bdmEBS = lens _bdmEBS (\ s a -> s{_bdmEBS = a})

-- | The device name exposed to the EC2 instance (for example, @/dev/sdh@ or @xvdh@ ).
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

-- | Configures a customized metric for a target tracking policy.
--
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
-- * 'cmsDimensions' - The dimensions of the metric.
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


-- | The dimensions of the metric.
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

-- | Describes an Amazon EBS volume.
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
-- * 'ebsDeleteOnTermination' - Indicates whether the volume is deleted on instance termination. The default is @true@ .
--
-- * 'ebsVolumeSize' - The volume size, in GiB. For @standard@ volumes, specify a value from 1 to 1,024. For @io1@ volumes, specify a value from 4 to 16,384. For @gp2@ volumes, specify a value from 1 to 16,384. If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you create a volume from a snapshot and you don't specify a volume size, the default is the snapshot size.
--
-- * 'ebsIOPS' - The number of I/O operations per second (IOPS) to provision for the volume. Constraint: Required when the volume type is @io1@ .
--
-- * 'ebsEncrypted' - Indicates whether the volume should be encrypted. Encrypted EBS volumes must be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are automatically encrypted. There is no way to create an encrypted volume from an unencrypted snapshot or an unencrypted volume from an encrypted snapshot. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'ebsVolumeType' - The volume type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Valid values: @standard@ | @io1@ | @gp2@  Default: @standard@
--
-- * 'ebsSnapshotId' - The ID of the snapshot.
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


-- | Indicates whether the volume is deleted on instance termination. The default is @true@ .
ebsDeleteOnTermination :: Lens' EBS (Maybe Bool)
ebsDeleteOnTermination = lens _ebsDeleteOnTermination (\ s a -> s{_ebsDeleteOnTermination = a})

-- | The volume size, in GiB. For @standard@ volumes, specify a value from 1 to 1,024. For @io1@ volumes, specify a value from 4 to 16,384. For @gp2@ volumes, specify a value from 1 to 16,384. If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you create a volume from a snapshot and you don't specify a volume size, the default is the snapshot size.
ebsVolumeSize :: Lens' EBS (Maybe Natural)
ebsVolumeSize = lens _ebsVolumeSize (\ s a -> s{_ebsVolumeSize = a}) . mapping _Nat

-- | The number of I/O operations per second (IOPS) to provision for the volume. Constraint: Required when the volume type is @io1@ .
ebsIOPS :: Lens' EBS (Maybe Natural)
ebsIOPS = lens _ebsIOPS (\ s a -> s{_ebsIOPS = a}) . mapping _Nat

-- | Indicates whether the volume should be encrypted. Encrypted EBS volumes must be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are automatically encrypted. There is no way to create an encrypted volume from an unencrypted snapshot or an unencrypted volume from an encrypted snapshot. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
ebsEncrypted :: Lens' EBS (Maybe Bool)
ebsEncrypted = lens _ebsEncrypted (\ s a -> s{_ebsEncrypted = a})

-- | The volume type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Valid values: @standard@ | @io1@ | @gp2@  Default: @standard@
ebsVolumeType :: Lens' EBS (Maybe Text)
ebsVolumeType = lens _ebsVolumeType (\ s a -> s{_ebsVolumeType = a})

-- | The ID of the snapshot.
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
-- * 'iLifecycleState' - A description of the current lifecycle state. Note that the @Quarantined@ state is not used.
--
-- * 'iHealthStatus' - The last reported health status of the instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Auto Scaling should terminate and replace it.
--
-- * 'iProtectedFromScaleIn' - Indicates whether the instance is protected from termination by Auto Scaling when scaling in.
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

-- | A description of the current lifecycle state. Note that the @Quarantined@ state is not used.
iLifecycleState :: Lens' Instance LifecycleState
iLifecycleState = lens _iLifecycleState (\ s a -> s{_iLifecycleState = a})

-- | The last reported health status of the instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Auto Scaling should terminate and replace it.
iHealthStatus :: Lens' Instance Text
iHealthStatus = lens _iHealthStatus (\ s a -> s{_iHealthStatus = a})

-- | Indicates whether the instance is protected from termination by Auto Scaling when scaling in.
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

-- | Describes a launch configuration.
--
--
--
-- /See:/ 'launchConfiguration' smart constructor.
data LaunchConfiguration = LaunchConfiguration'
  { _lcAssociatePublicIPAddress     :: !(Maybe Bool)
  , _lcSecurityGroups               :: !(Maybe [Text])
  , _lcSpotPrice                    :: !(Maybe Text)
  , _lcInstanceMonitoring           :: !(Maybe InstanceMonitoring)
  , _lcKeyName                      :: !(Maybe Text)
  , _lcClassicLinkVPCSecurityGroups :: !(Maybe [Text])
  , _lcRAMDiskId                    :: !(Maybe Text)
  , _lcKernelId                     :: !(Maybe Text)
  , _lcEBSOptimized                 :: !(Maybe Bool)
  , _lcUserData                     :: !(Maybe Text)
  , _lcClassicLinkVPCId             :: !(Maybe Text)
  , _lcIAMInstanceProfile           :: !(Maybe Text)
  , _lcLaunchConfigurationARN       :: !(Maybe Text)
  , _lcPlacementTenancy             :: !(Maybe Text)
  , _lcBlockDeviceMappings          :: !(Maybe [BlockDeviceMapping])
  , _lcLaunchConfigurationName      :: !Text
  , _lcImageId                      :: !Text
  , _lcInstanceType                 :: !Text
  , _lcCreatedTime                  :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcAssociatePublicIPAddress' - [EC2-VPC] Indicates whether to assign a public IP address to each instance.
--
-- * 'lcSecurityGroups' - The security groups to associate with the instances.
--
-- * 'lcSpotPrice' - The price to bid when launching Spot Instances.
--
-- * 'lcInstanceMonitoring' - Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
--
-- * 'lcKeyName' - The name of the key pair.
--
-- * 'lcClassicLinkVPCSecurityGroups' - The IDs of one or more security groups for the VPC specified in @ClassicLinkVPCId@ . This parameter is required if you specify a ClassicLink-enabled VPC, and cannot be used otherwise. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'lcRAMDiskId' - The ID of the RAM disk associated with the AMI.
--
-- * 'lcKernelId' - The ID of the kernel associated with the AMI.
--
-- * 'lcEBSOptimized' - Controls whether the instance is optimized for EBS I/O (@true@ ) or not (@false@ ).
--
-- * 'lcUserData' - The user data available to the instances.
--
-- * 'lcClassicLinkVPCId' - The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. This parameter can only be used if you are launching EC2-Classic instances. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'lcIAMInstanceProfile' - The name or Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance.
--
-- * 'lcLaunchConfigurationARN' - The Amazon Resource Name (ARN) of the launch configuration.
--
-- * 'lcPlacementTenancy' - The tenancy of the instance, either @default@ or @dedicated@ . An instance with @dedicated@ tenancy runs in an isolated, single-tenant hardware and can only be launched into a VPC.
--
-- * 'lcBlockDeviceMappings' - A block device mapping, which specifies the block devices for the instance.
--
-- * 'lcLaunchConfigurationName' - The name of the launch configuration.
--
-- * 'lcImageId' - The ID of the Amazon Machine Image (AMI).
--
-- * 'lcInstanceType' - The instance type for the instances.
--
-- * 'lcCreatedTime' - The creation date and time for the launch configuration.
launchConfiguration
    :: Text -- ^ 'lcLaunchConfigurationName'
    -> Text -- ^ 'lcImageId'
    -> Text -- ^ 'lcInstanceType'
    -> UTCTime -- ^ 'lcCreatedTime'
    -> LaunchConfiguration
launchConfiguration pLaunchConfigurationName_ pImageId_ pInstanceType_ pCreatedTime_ =
  LaunchConfiguration'
    { _lcAssociatePublicIPAddress = Nothing
    , _lcSecurityGroups = Nothing
    , _lcSpotPrice = Nothing
    , _lcInstanceMonitoring = Nothing
    , _lcKeyName = Nothing
    , _lcClassicLinkVPCSecurityGroups = Nothing
    , _lcRAMDiskId = Nothing
    , _lcKernelId = Nothing
    , _lcEBSOptimized = Nothing
    , _lcUserData = Nothing
    , _lcClassicLinkVPCId = Nothing
    , _lcIAMInstanceProfile = Nothing
    , _lcLaunchConfigurationARN = Nothing
    , _lcPlacementTenancy = Nothing
    , _lcBlockDeviceMappings = Nothing
    , _lcLaunchConfigurationName = pLaunchConfigurationName_
    , _lcImageId = pImageId_
    , _lcInstanceType = pInstanceType_
    , _lcCreatedTime = _Time # pCreatedTime_
    }


-- | [EC2-VPC] Indicates whether to assign a public IP address to each instance.
lcAssociatePublicIPAddress :: Lens' LaunchConfiguration (Maybe Bool)
lcAssociatePublicIPAddress = lens _lcAssociatePublicIPAddress (\ s a -> s{_lcAssociatePublicIPAddress = a})

-- | The security groups to associate with the instances.
lcSecurityGroups :: Lens' LaunchConfiguration [Text]
lcSecurityGroups = lens _lcSecurityGroups (\ s a -> s{_lcSecurityGroups = a}) . _Default . _Coerce

-- | The price to bid when launching Spot Instances.
lcSpotPrice :: Lens' LaunchConfiguration (Maybe Text)
lcSpotPrice = lens _lcSpotPrice (\ s a -> s{_lcSpotPrice = a})

-- | Controls whether instances in this group are launched with detailed (@true@ ) or basic (@false@ ) monitoring.
lcInstanceMonitoring :: Lens' LaunchConfiguration (Maybe InstanceMonitoring)
lcInstanceMonitoring = lens _lcInstanceMonitoring (\ s a -> s{_lcInstanceMonitoring = a})

-- | The name of the key pair.
lcKeyName :: Lens' LaunchConfiguration (Maybe Text)
lcKeyName = lens _lcKeyName (\ s a -> s{_lcKeyName = a})

-- | The IDs of one or more security groups for the VPC specified in @ClassicLinkVPCId@ . This parameter is required if you specify a ClassicLink-enabled VPC, and cannot be used otherwise. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
lcClassicLinkVPCSecurityGroups :: Lens' LaunchConfiguration [Text]
lcClassicLinkVPCSecurityGroups = lens _lcClassicLinkVPCSecurityGroups (\ s a -> s{_lcClassicLinkVPCSecurityGroups = a}) . _Default . _Coerce

-- | The ID of the RAM disk associated with the AMI.
lcRAMDiskId :: Lens' LaunchConfiguration (Maybe Text)
lcRAMDiskId = lens _lcRAMDiskId (\ s a -> s{_lcRAMDiskId = a})

-- | The ID of the kernel associated with the AMI.
lcKernelId :: Lens' LaunchConfiguration (Maybe Text)
lcKernelId = lens _lcKernelId (\ s a -> s{_lcKernelId = a})

-- | Controls whether the instance is optimized for EBS I/O (@true@ ) or not (@false@ ).
lcEBSOptimized :: Lens' LaunchConfiguration (Maybe Bool)
lcEBSOptimized = lens _lcEBSOptimized (\ s a -> s{_lcEBSOptimized = a})

-- | The user data available to the instances.
lcUserData :: Lens' LaunchConfiguration (Maybe Text)
lcUserData = lens _lcUserData (\ s a -> s{_lcUserData = a})

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances to. This parameter can only be used if you are launching EC2-Classic instances. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
lcClassicLinkVPCId :: Lens' LaunchConfiguration (Maybe Text)
lcClassicLinkVPCId = lens _lcClassicLinkVPCId (\ s a -> s{_lcClassicLinkVPCId = a})

-- | The name or Amazon Resource Name (ARN) of the instance profile associated with the IAM role for the instance.
lcIAMInstanceProfile :: Lens' LaunchConfiguration (Maybe Text)
lcIAMInstanceProfile = lens _lcIAMInstanceProfile (\ s a -> s{_lcIAMInstanceProfile = a})

-- | The Amazon Resource Name (ARN) of the launch configuration.
lcLaunchConfigurationARN :: Lens' LaunchConfiguration (Maybe Text)
lcLaunchConfigurationARN = lens _lcLaunchConfigurationARN (\ s a -> s{_lcLaunchConfigurationARN = a})

-- | The tenancy of the instance, either @default@ or @dedicated@ . An instance with @dedicated@ tenancy runs in an isolated, single-tenant hardware and can only be launched into a VPC.
lcPlacementTenancy :: Lens' LaunchConfiguration (Maybe Text)
lcPlacementTenancy = lens _lcPlacementTenancy (\ s a -> s{_lcPlacementTenancy = a})

-- | A block device mapping, which specifies the block devices for the instance.
lcBlockDeviceMappings :: Lens' LaunchConfiguration [BlockDeviceMapping]
lcBlockDeviceMappings = lens _lcBlockDeviceMappings (\ s a -> s{_lcBlockDeviceMappings = a}) . _Default . _Coerce

-- | The name of the launch configuration.
lcLaunchConfigurationName :: Lens' LaunchConfiguration Text
lcLaunchConfigurationName = lens _lcLaunchConfigurationName (\ s a -> s{_lcLaunchConfigurationName = a})

-- | The ID of the Amazon Machine Image (AMI).
lcImageId :: Lens' LaunchConfiguration Text
lcImageId = lens _lcImageId (\ s a -> s{_lcImageId = a})

-- | The instance type for the instances.
lcInstanceType :: Lens' LaunchConfiguration Text
lcInstanceType = lens _lcInstanceType (\ s a -> s{_lcInstanceType = a})

-- | The creation date and time for the launch configuration.
lcCreatedTime :: Lens' LaunchConfiguration UTCTime
lcCreatedTime = lens _lcCreatedTime (\ s a -> s{_lcCreatedTime = a}) . _Time

instance FromXML LaunchConfiguration where
        parseXML x
          = LaunchConfiguration' <$>
              (x .@? "AssociatePublicIpAddress") <*>
                (x .@? "SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "SpotPrice")
                <*> (x .@? "InstanceMonitoring")
                <*> (x .@? "KeyName")
                <*>
                (x .@? "ClassicLinkVPCSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "RamdiskId")
                <*> (x .@? "KernelId")
                <*> (x .@? "EbsOptimized")
                <*> (x .@? "UserData")
                <*> (x .@? "ClassicLinkVPCId")
                <*> (x .@? "IamInstanceProfile")
                <*> (x .@? "LaunchConfigurationARN")
                <*> (x .@? "PlacementTenancy")
                <*>
                (x .@? "BlockDeviceMappings" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@ "LaunchConfigurationName")
                <*> (x .@ "ImageId")
                <*> (x .@ "InstanceType")
                <*> (x .@ "CreatedTime")

instance Hashable LaunchConfiguration where

instance NFData LaunchConfiguration where

-- | Describes a launch template.
--
--
--
-- /See:/ 'launchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { _ltsLaunchTemplateName :: !(Maybe Text)
  , _ltsLaunchTemplateId   :: !(Maybe Text)
  , _ltsVersion            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsLaunchTemplateName' - The name of the launch template. You must specify either a template name or a template ID.
--
-- * 'ltsLaunchTemplateId' - The ID of the launch template. You must specify either a template ID or a template name.
--
-- * 'ltsVersion' - The version number, @> Latest@ , or @> Default@ . If the value is @> Latest@ , Auto Scaling selects the latest version of the launch template when launching instances. If the value is @> Default@ , Auto Scaling selects the default version of the launch template when launching instances. The default value is @> Default@ .
launchTemplateSpecification
    :: LaunchTemplateSpecification
launchTemplateSpecification =
  LaunchTemplateSpecification'
    { _ltsLaunchTemplateName = Nothing
    , _ltsLaunchTemplateId = Nothing
    , _ltsVersion = Nothing
    }


-- | The name of the launch template. You must specify either a template name or a template ID.
ltsLaunchTemplateName :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateName = lens _ltsLaunchTemplateName (\ s a -> s{_ltsLaunchTemplateName = a})

-- | The ID of the launch template. You must specify either a template ID or a template name.
ltsLaunchTemplateId :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateId = lens _ltsLaunchTemplateId (\ s a -> s{_ltsLaunchTemplateId = a})

-- | The version number, @> Latest@ , or @> Default@ . If the value is @> Latest@ , Auto Scaling selects the latest version of the launch template when launching instances. If the value is @> Default@ , Auto Scaling selects the default version of the launch template when launching instances. The default value is @> Default@ .
ltsVersion :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsVersion = lens _ltsVersion (\ s a -> s{_ltsVersion = a})

instance FromXML LaunchTemplateSpecification where
        parseXML x
          = LaunchTemplateSpecification' <$>
              (x .@? "LaunchTemplateName") <*>
                (x .@? "LaunchTemplateId")
                <*> (x .@? "Version")

instance Hashable LaunchTemplateSpecification where

instance NFData LaunchTemplateSpecification where

instance ToQuery LaunchTemplateSpecification where
        toQuery LaunchTemplateSpecification'{..}
          = mconcat
              ["LaunchTemplateName" =: _ltsLaunchTemplateName,
               "LaunchTemplateId" =: _ltsLaunchTemplateId,
               "Version" =: _ltsVersion]

-- | Describes a lifecycle hook, which tells Auto Scaling that you want to perform an action whenever it launches instances or whenever it terminates instances.
--
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/lifecycle-hooks.html Auto Scaling Lifecycle Hooks> in the /Auto Scaling User Guide/ .
--
--
-- /See:/ 'lifecycleHook' smart constructor.
data LifecycleHook = LifecycleHook'
  { _lhDefaultResult         :: !(Maybe Text)
  , _lhLifecycleHookName     :: !(Maybe Text)
  , _lhHeartbeatTimeout      :: !(Maybe Int)
  , _lhAutoScalingGroupName  :: !(Maybe Text)
  , _lhNotificationMetadata  :: !(Maybe Text)
  , _lhGlobalTimeout         :: !(Maybe Int)
  , _lhNotificationTargetARN :: !(Maybe Text)
  , _lhLifecycleTransition   :: !(Maybe Text)
  , _lhRoleARN               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhDefaultResult' - Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The valid values are @CONTINUE@ and @ABANDON@ . The default value is @CONTINUE@ .
--
-- * 'lhLifecycleHookName' - The name of the lifecycle hook.
--
-- * 'lhHeartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Auto Scaling performs the default action. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
--
-- * 'lhAutoScalingGroupName' - The name of the Auto Scaling group for the lifecycle hook.
--
-- * 'lhNotificationMetadata' - Additional information that you want to include any time Auto Scaling sends a message to the notification target.
--
-- * 'lhGlobalTimeout' - The maximum time, in seconds, that an instance can remain in a @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800 seconds (48 hours) or 100 times @HeartbeatTimeout@ , whichever is smaller.
--
-- * 'lhNotificationTargetARN' - The ARN of the target that Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
--
-- * 'lhLifecycleTransition' - The state of the EC2 instance to which you want to attach the lifecycle hook. For a list of lifecycle hook types, see 'DescribeLifecycleHookTypes' .
--
-- * 'lhRoleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target.
lifecycleHook
    :: LifecycleHook
lifecycleHook =
  LifecycleHook'
    { _lhDefaultResult = Nothing
    , _lhLifecycleHookName = Nothing
    , _lhHeartbeatTimeout = Nothing
    , _lhAutoScalingGroupName = Nothing
    , _lhNotificationMetadata = Nothing
    , _lhGlobalTimeout = Nothing
    , _lhNotificationTargetARN = Nothing
    , _lhLifecycleTransition = Nothing
    , _lhRoleARN = Nothing
    }


-- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The valid values are @CONTINUE@ and @ABANDON@ . The default value is @CONTINUE@ .
lhDefaultResult :: Lens' LifecycleHook (Maybe Text)
lhDefaultResult = lens _lhDefaultResult (\ s a -> s{_lhDefaultResult = a})

-- | The name of the lifecycle hook.
lhLifecycleHookName :: Lens' LifecycleHook (Maybe Text)
lhLifecycleHookName = lens _lhLifecycleHookName (\ s a -> s{_lhLifecycleHookName = a})

-- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Auto Scaling performs the default action. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
lhHeartbeatTimeout :: Lens' LifecycleHook (Maybe Int)
lhHeartbeatTimeout = lens _lhHeartbeatTimeout (\ s a -> s{_lhHeartbeatTimeout = a})

-- | The name of the Auto Scaling group for the lifecycle hook.
lhAutoScalingGroupName :: Lens' LifecycleHook (Maybe Text)
lhAutoScalingGroupName = lens _lhAutoScalingGroupName (\ s a -> s{_lhAutoScalingGroupName = a})

-- | Additional information that you want to include any time Auto Scaling sends a message to the notification target.
lhNotificationMetadata :: Lens' LifecycleHook (Maybe Text)
lhNotificationMetadata = lens _lhNotificationMetadata (\ s a -> s{_lhNotificationMetadata = a})

-- | The maximum time, in seconds, that an instance can remain in a @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800 seconds (48 hours) or 100 times @HeartbeatTimeout@ , whichever is smaller.
lhGlobalTimeout :: Lens' LifecycleHook (Maybe Int)
lhGlobalTimeout = lens _lhGlobalTimeout (\ s a -> s{_lhGlobalTimeout = a})

-- | The ARN of the target that Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
lhNotificationTargetARN :: Lens' LifecycleHook (Maybe Text)
lhNotificationTargetARN = lens _lhNotificationTargetARN (\ s a -> s{_lhNotificationTargetARN = a})

-- | The state of the EC2 instance to which you want to attach the lifecycle hook. For a list of lifecycle hook types, see 'DescribeLifecycleHookTypes' .
lhLifecycleTransition :: Lens' LifecycleHook (Maybe Text)
lhLifecycleTransition = lens _lhLifecycleTransition (\ s a -> s{_lhLifecycleTransition = a})

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target.
lhRoleARN :: Lens' LifecycleHook (Maybe Text)
lhRoleARN = lens _lhRoleARN (\ s a -> s{_lhRoleARN = a})

instance FromXML LifecycleHook where
        parseXML x
          = LifecycleHook' <$>
              (x .@? "DefaultResult") <*>
                (x .@? "LifecycleHookName")
                <*> (x .@? "HeartbeatTimeout")
                <*> (x .@? "AutoScalingGroupName")
                <*> (x .@? "NotificationMetadata")
                <*> (x .@? "GlobalTimeout")
                <*> (x .@? "NotificationTargetARN")
                <*> (x .@? "LifecycleTransition")
                <*> (x .@? "RoleARN")

instance Hashable LifecycleHook where

instance NFData LifecycleHook where

-- | Describes a lifecycle hook, which tells Auto Scaling that you want to perform an action whenever it launches instances or whenever it terminates instances.
--
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/lifecycle-hooks.html Auto Scaling Lifecycle Hooks> in the /Auto Scaling User Guide/ .
--
--
-- /See:/ 'lifecycleHookSpecification' smart constructor.
data LifecycleHookSpecification = LifecycleHookSpecification'
  { _lhsDefaultResult         :: !(Maybe Text)
  , _lhsHeartbeatTimeout      :: !(Maybe Int)
  , _lhsNotificationMetadata  :: !(Maybe Text)
  , _lhsNotificationTargetARN :: !(Maybe Text)
  , _lhsRoleARN               :: !(Maybe Text)
  , _lhsLifecycleHookName     :: !Text
  , _lhsLifecycleTransition   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleHookSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhsDefaultResult' - Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The valid values are @CONTINUE@ and @ABANDON@ .
--
-- * 'lhsHeartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Auto Scaling performs the default action. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
--
-- * 'lhsNotificationMetadata' - Additional information that you want to include any time Auto Scaling sends a message to the notification target.
--
-- * 'lhsNotificationTargetARN' - The ARN of the target that Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
--
-- * 'lhsRoleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target.
--
-- * 'lhsLifecycleHookName' - The name of the lifecycle hook.
--
-- * 'lhsLifecycleTransition' - The state of the EC2 instance to which you want to attach the lifecycle hook. For a list of lifecycle hook types, see 'DescribeLifecycleHookTypes' .
lifecycleHookSpecification
    :: Text -- ^ 'lhsLifecycleHookName'
    -> Text -- ^ 'lhsLifecycleTransition'
    -> LifecycleHookSpecification
lifecycleHookSpecification pLifecycleHookName_ pLifecycleTransition_ =
  LifecycleHookSpecification'
    { _lhsDefaultResult = Nothing
    , _lhsHeartbeatTimeout = Nothing
    , _lhsNotificationMetadata = Nothing
    , _lhsNotificationTargetARN = Nothing
    , _lhsRoleARN = Nothing
    , _lhsLifecycleHookName = pLifecycleHookName_
    , _lhsLifecycleTransition = pLifecycleTransition_
    }


-- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The valid values are @CONTINUE@ and @ABANDON@ .
lhsDefaultResult :: Lens' LifecycleHookSpecification (Maybe Text)
lhsDefaultResult = lens _lhsDefaultResult (\ s a -> s{_lhsDefaultResult = a})

-- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Auto Scaling performs the default action. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
lhsHeartbeatTimeout :: Lens' LifecycleHookSpecification (Maybe Int)
lhsHeartbeatTimeout = lens _lhsHeartbeatTimeout (\ s a -> s{_lhsHeartbeatTimeout = a})

-- | Additional information that you want to include any time Auto Scaling sends a message to the notification target.
lhsNotificationMetadata :: Lens' LifecycleHookSpecification (Maybe Text)
lhsNotificationMetadata = lens _lhsNotificationMetadata (\ s a -> s{_lhsNotificationMetadata = a})

-- | The ARN of the target that Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
lhsNotificationTargetARN :: Lens' LifecycleHookSpecification (Maybe Text)
lhsNotificationTargetARN = lens _lhsNotificationTargetARN (\ s a -> s{_lhsNotificationTargetARN = a})

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target.
lhsRoleARN :: Lens' LifecycleHookSpecification (Maybe Text)
lhsRoleARN = lens _lhsRoleARN (\ s a -> s{_lhsRoleARN = a})

-- | The name of the lifecycle hook.
lhsLifecycleHookName :: Lens' LifecycleHookSpecification Text
lhsLifecycleHookName = lens _lhsLifecycleHookName (\ s a -> s{_lhsLifecycleHookName = a})

-- | The state of the EC2 instance to which you want to attach the lifecycle hook. For a list of lifecycle hook types, see 'DescribeLifecycleHookTypes' .
lhsLifecycleTransition :: Lens' LifecycleHookSpecification Text
lhsLifecycleTransition = lens _lhsLifecycleTransition (\ s a -> s{_lhsLifecycleTransition = a})

instance Hashable LifecycleHookSpecification where

instance NFData LifecycleHookSpecification where

instance ToQuery LifecycleHookSpecification where
        toQuery LifecycleHookSpecification'{..}
          = mconcat
              ["DefaultResult" =: _lhsDefaultResult,
               "HeartbeatTimeout" =: _lhsHeartbeatTimeout,
               "NotificationMetadata" =: _lhsNotificationMetadata,
               "NotificationTargetARN" =: _lhsNotificationTargetARN,
               "RoleARN" =: _lhsRoleARN,
               "LifecycleHookName" =: _lhsLifecycleHookName,
               "LifecycleTransition" =: _lhsLifecycleTransition]

-- | Describes the state of a Classic Load Balancer.
--
--
-- If you specify a load balancer when creating the Auto Scaling group, the state of the load balancer is @InService@ .
--
-- If you attach a load balancer to an existing Auto Scaling group, the initial state is @Adding@ . The state transitions to @Added@ after all instances in the group are registered with the load balancer. If ELB health checks are enabled for the load balancer, the state transitions to @InService@ after at least one instance in the group passes the health check. If EC2 health checks are enabled instead, the load balancer remains in the @Added@ state.
--
--
-- /See:/ 'loadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { _lbsState            :: !(Maybe Text)
  , _lbsLoadBalancerName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbsState' - One of the following load balancer states:     * @Adding@ - The instances in the group are being registered with the load balancer.     * @Added@ - All instances in the group are registered with the load balancer.     * @InService@ - At least one instance in the group passed an ELB health check.     * @Removing@ - The instances in the group are being deregistered from the load balancer. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.     * @Removed@ - All instances in the group are deregistered from the load balancer.
--
-- * 'lbsLoadBalancerName' - The name of the load balancer.
loadBalancerState
    :: LoadBalancerState
loadBalancerState =
  LoadBalancerState' {_lbsState = Nothing, _lbsLoadBalancerName = Nothing}


-- | One of the following load balancer states:     * @Adding@ - The instances in the group are being registered with the load balancer.     * @Added@ - All instances in the group are registered with the load balancer.     * @InService@ - At least one instance in the group passed an ELB health check.     * @Removing@ - The instances in the group are being deregistered from the load balancer. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.     * @Removed@ - All instances in the group are deregistered from the load balancer.
lbsState :: Lens' LoadBalancerState (Maybe Text)
lbsState = lens _lbsState (\ s a -> s{_lbsState = a})

-- | The name of the load balancer.
lbsLoadBalancerName :: Lens' LoadBalancerState (Maybe Text)
lbsLoadBalancerName = lens _lbsLoadBalancerName (\ s a -> s{_lbsLoadBalancerName = a})

instance FromXML LoadBalancerState where
        parseXML x
          = LoadBalancerState' <$>
              (x .@? "State") <*> (x .@? "LoadBalancerName")

instance Hashable LoadBalancerState where

instance NFData LoadBalancerState where

-- | Describes the state of a target group.
--
--
-- If you attach a target group to an existing Auto Scaling group, the initial state is @Adding@ . The state transitions to @Added@ after all Auto Scaling instances are registered with the target group. If ELB health checks are enabled, the state transitions to @InService@ after at least one Auto Scaling instance passes the health check. If EC2 health checks are enabled instead, the target group remains in the @Added@ state.
--
--
-- /See:/ 'loadBalancerTargetGroupState' smart constructor.
data LoadBalancerTargetGroupState = LoadBalancerTargetGroupState'
  { _lbtgsState                      :: !(Maybe Text)
  , _lbtgsLoadBalancerTargetGroupARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerTargetGroupState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbtgsState' - The state of the target group.     * @Adding@ - The Auto Scaling instances are being registered with the target group.     * @Added@ - All Auto Scaling instances are registered with the target group.     * @InService@ - At least one Auto Scaling instance passed an ELB health check.     * @Removing@ - The Auto Scaling instances are being deregistered from the target group. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.     * @Removed@ - All Auto Scaling instances are deregistered from the target group.
--
-- * 'lbtgsLoadBalancerTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
loadBalancerTargetGroupState
    :: LoadBalancerTargetGroupState
loadBalancerTargetGroupState =
  LoadBalancerTargetGroupState'
    {_lbtgsState = Nothing, _lbtgsLoadBalancerTargetGroupARN = Nothing}


-- | The state of the target group.     * @Adding@ - The Auto Scaling instances are being registered with the target group.     * @Added@ - All Auto Scaling instances are registered with the target group.     * @InService@ - At least one Auto Scaling instance passed an ELB health check.     * @Removing@ - The Auto Scaling instances are being deregistered from the target group. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.     * @Removed@ - All Auto Scaling instances are deregistered from the target group.
lbtgsState :: Lens' LoadBalancerTargetGroupState (Maybe Text)
lbtgsState = lens _lbtgsState (\ s a -> s{_lbtgsState = a})

-- | The Amazon Resource Name (ARN) of the target group.
lbtgsLoadBalancerTargetGroupARN :: Lens' LoadBalancerTargetGroupState (Maybe Text)
lbtgsLoadBalancerTargetGroupARN = lens _lbtgsLoadBalancerTargetGroupARN (\ s a -> s{_lbtgsLoadBalancerTargetGroupARN = a})

instance FromXML LoadBalancerTargetGroupState where
        parseXML x
          = LoadBalancerTargetGroupState' <$>
              (x .@? "State") <*>
                (x .@? "LoadBalancerTargetGroupARN")

instance Hashable LoadBalancerTargetGroupState where

instance NFData LoadBalancerTargetGroupState where

-- | Describes a metric.
--
--
--
-- /See:/ 'metricCollectionType' smart constructor.
newtype MetricCollectionType = MetricCollectionType'
  { _mctMetric :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricCollectionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mctMetric' - One of the following metrics:     * @GroupMinSize@      * @GroupMaxSize@      * @GroupDesiredCapacity@      * @GroupInServiceInstances@      * @GroupPendingInstances@      * @GroupStandbyInstances@      * @GroupTerminatingInstances@      * @GroupTotalInstances@
metricCollectionType
    :: MetricCollectionType
metricCollectionType = MetricCollectionType' {_mctMetric = Nothing}


-- | One of the following metrics:     * @GroupMinSize@      * @GroupMaxSize@      * @GroupDesiredCapacity@      * @GroupInServiceInstances@      * @GroupPendingInstances@      * @GroupStandbyInstances@      * @GroupTerminatingInstances@      * @GroupTotalInstances@
mctMetric :: Lens' MetricCollectionType (Maybe Text)
mctMetric = lens _mctMetric (\ s a -> s{_mctMetric = a})

instance FromXML MetricCollectionType where
        parseXML x
          = MetricCollectionType' <$> (x .@? "Metric")

instance Hashable MetricCollectionType where

instance NFData MetricCollectionType where

-- | Describes the dimension of a metric.
--
--
--
-- /See:/ 'metricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { _mdName  :: !Text
  , _mdValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdName' - The name of the dimension.
--
-- * 'mdValue' - The value of the dimension.
metricDimension
    :: Text -- ^ 'mdName'
    -> Text -- ^ 'mdValue'
    -> MetricDimension
metricDimension pName_ pValue_ =
  MetricDimension' {_mdName = pName_, _mdValue = pValue_}


-- | The name of the dimension.
mdName :: Lens' MetricDimension Text
mdName = lens _mdName (\ s a -> s{_mdName = a})

-- | The value of the dimension.
mdValue :: Lens' MetricDimension Text
mdValue = lens _mdValue (\ s a -> s{_mdValue = a})

instance FromXML MetricDimension where
        parseXML x
          = MetricDimension' <$>
              (x .@ "Name") <*> (x .@ "Value")

instance Hashable MetricDimension where

instance NFData MetricDimension where

instance ToQuery MetricDimension where
        toQuery MetricDimension'{..}
          = mconcat ["Name" =: _mdName, "Value" =: _mdValue]

-- | Describes a granularity of a metric.
--
--
--
-- /See:/ 'metricGranularityType' smart constructor.
newtype MetricGranularityType = MetricGranularityType'
  { _mgtGranularity :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricGranularityType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mgtGranularity' - The granularity. The only valid value is @1Minute@ .
metricGranularityType
    :: MetricGranularityType
metricGranularityType = MetricGranularityType' {_mgtGranularity = Nothing}


-- | The granularity. The only valid value is @1Minute@ .
mgtGranularity :: Lens' MetricGranularityType (Maybe Text)
mgtGranularity = lens _mgtGranularity (\ s a -> s{_mgtGranularity = a})

instance FromXML MetricGranularityType where
        parseXML x
          = MetricGranularityType' <$> (x .@? "Granularity")

instance Hashable MetricGranularityType where

instance NFData MetricGranularityType where

-- | Describes a notification.
--
--
--
-- /See:/ 'notificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { _ncTopicARN             :: !(Maybe Text)
  , _ncAutoScalingGroupName :: !(Maybe Text)
  , _ncNotificationType     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncTopicARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic.
--
-- * 'ncAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'ncNotificationType' - One of the following event notification types:     * @autoscaling:EC2_INSTANCE_LAUNCH@      * @autoscaling:EC2_INSTANCE_LAUNCH_ERROR@      * @autoscaling:EC2_INSTANCE_TERMINATE@      * @autoscaling:EC2_INSTANCE_TERMINATE_ERROR@      * @autoscaling:TEST_NOTIFICATION@
notificationConfiguration
    :: NotificationConfiguration
notificationConfiguration =
  NotificationConfiguration'
    { _ncTopicARN = Nothing
    , _ncAutoScalingGroupName = Nothing
    , _ncNotificationType = Nothing
    }


-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic.
ncTopicARN :: Lens' NotificationConfiguration (Maybe Text)
ncTopicARN = lens _ncTopicARN (\ s a -> s{_ncTopicARN = a})

-- | The name of the Auto Scaling group.
ncAutoScalingGroupName :: Lens' NotificationConfiguration (Maybe Text)
ncAutoScalingGroupName = lens _ncAutoScalingGroupName (\ s a -> s{_ncAutoScalingGroupName = a})

-- | One of the following event notification types:     * @autoscaling:EC2_INSTANCE_LAUNCH@      * @autoscaling:EC2_INSTANCE_LAUNCH_ERROR@      * @autoscaling:EC2_INSTANCE_TERMINATE@      * @autoscaling:EC2_INSTANCE_TERMINATE_ERROR@      * @autoscaling:TEST_NOTIFICATION@
ncNotificationType :: Lens' NotificationConfiguration (Maybe Text)
ncNotificationType = lens _ncNotificationType (\ s a -> s{_ncNotificationType = a})

instance FromXML NotificationConfiguration where
        parseXML x
          = NotificationConfiguration' <$>
              (x .@? "TopicARN") <*> (x .@? "AutoScalingGroupName")
                <*> (x .@? "NotificationType")

instance Hashable NotificationConfiguration where

instance NFData NotificationConfiguration where

-- | Configures a predefined metric for a target tracking policy.
--
--
--
-- /See:/ 'predefinedMetricSpecification' smart constructor.
data PredefinedMetricSpecification = PredefinedMetricSpecification'
  { _pmsResourceLabel        :: !(Maybe Text)
  , _pmsPredefinedMetricType :: !MetricType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PredefinedMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmsResourceLabel' - Identifies the resource associated with the metric type. The following predefined metrics are available:     * @ASGAverageCPUUtilization@ - average CPU utilization of the Auto Scaling group     * @ASGAverageNetworkIn@ - average number of bytes received on all network interfaces by the Auto Scaling group     * @ASGAverageNetworkOut@ - average number of bytes sent out on all network interfaces by the Auto Scaling group     * @ALBRequestCountPerTarget@ - number of requests completed per target in an Application Load Balancer target group For predefined metric types @ASGAverageCPUUtilization@ , @ASGAverageNetworkIn@ , and @ASGAverageNetworkOut@ , the parameter must not be specified as the resource associated with the metric type is the Auto Scaling group. For predefined metric type @ALBRequestCountPerTarget@ , the parameter must be specified in the format: @app//load-balancer-name/ //load-balancer-id/ /targetgroup//target-group-name/ //target-group-id/ @ , where @app//load-balancer-name/ //load-balancer-id/ @ is the final portion of the load balancer ARN, and @targetgroup//target-group-name/ //target-group-id/ @ is the final portion of the target group ARN. The target group must be attached to the Auto Scaling group.
--
-- * 'pmsPredefinedMetricType' - The metric type.
predefinedMetricSpecification
    :: MetricType -- ^ 'pmsPredefinedMetricType'
    -> PredefinedMetricSpecification
predefinedMetricSpecification pPredefinedMetricType_ =
  PredefinedMetricSpecification'
    { _pmsResourceLabel = Nothing
    , _pmsPredefinedMetricType = pPredefinedMetricType_
    }


-- | Identifies the resource associated with the metric type. The following predefined metrics are available:     * @ASGAverageCPUUtilization@ - average CPU utilization of the Auto Scaling group     * @ASGAverageNetworkIn@ - average number of bytes received on all network interfaces by the Auto Scaling group     * @ASGAverageNetworkOut@ - average number of bytes sent out on all network interfaces by the Auto Scaling group     * @ALBRequestCountPerTarget@ - number of requests completed per target in an Application Load Balancer target group For predefined metric types @ASGAverageCPUUtilization@ , @ASGAverageNetworkIn@ , and @ASGAverageNetworkOut@ , the parameter must not be specified as the resource associated with the metric type is the Auto Scaling group. For predefined metric type @ALBRequestCountPerTarget@ , the parameter must be specified in the format: @app//load-balancer-name/ //load-balancer-id/ /targetgroup//target-group-name/ //target-group-id/ @ , where @app//load-balancer-name/ //load-balancer-id/ @ is the final portion of the load balancer ARN, and @targetgroup//target-group-name/ //target-group-id/ @ is the final portion of the target group ARN. The target group must be attached to the Auto Scaling group.
pmsResourceLabel :: Lens' PredefinedMetricSpecification (Maybe Text)
pmsResourceLabel = lens _pmsResourceLabel (\ s a -> s{_pmsResourceLabel = a})

-- | The metric type.
pmsPredefinedMetricType :: Lens' PredefinedMetricSpecification MetricType
pmsPredefinedMetricType = lens _pmsPredefinedMetricType (\ s a -> s{_pmsPredefinedMetricType = a})

instance FromXML PredefinedMetricSpecification where
        parseXML x
          = PredefinedMetricSpecification' <$>
              (x .@? "ResourceLabel") <*>
                (x .@ "PredefinedMetricType")

instance Hashable PredefinedMetricSpecification where

instance NFData PredefinedMetricSpecification where

instance ToQuery PredefinedMetricSpecification where
        toQuery PredefinedMetricSpecification'{..}
          = mconcat
              ["ResourceLabel" =: _pmsResourceLabel,
               "PredefinedMetricType" =: _pmsPredefinedMetricType]

-- | Describes a process type.
--
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/as-suspend-resume-processes.html#process-types Auto Scaling Processes> in the /Auto Scaling User Guide/ .
--
--
-- /See:/ 'processType' smart constructor.
newtype ProcessType = ProcessType'
  { _ptProcessName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProcessType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptProcessName' - One of the following processes:     * @Launch@      * @Terminate@      * @AddToLoadBalancer@      * @AlarmNotification@      * @AZRebalance@      * @HealthCheck@      * @ReplaceUnhealthy@      * @ScheduledActions@
processType
    :: Text -- ^ 'ptProcessName'
    -> ProcessType
processType pProcessName_ = ProcessType' {_ptProcessName = pProcessName_}


-- | One of the following processes:     * @Launch@      * @Terminate@      * @AddToLoadBalancer@      * @AlarmNotification@      * @AZRebalance@      * @HealthCheck@      * @ReplaceUnhealthy@      * @ScheduledActions@
ptProcessName :: Lens' ProcessType Text
ptProcessName = lens _ptProcessName (\ s a -> s{_ptProcessName = a})

instance FromXML ProcessType where
        parseXML x = ProcessType' <$> (x .@ "ProcessName")

instance Hashable ProcessType where

instance NFData ProcessType where

-- | Describes a scaling policy.
--
--
--
-- /See:/ 'scalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { _sMinAdjustmentStep           :: !(Maybe Int)
  , _sEstimatedInstanceWarmup     :: !(Maybe Int)
  , _sPolicyName                  :: !(Maybe Text)
  , _sPolicyType                  :: !(Maybe Text)
  , _sStepAdjustments             :: !(Maybe [StepAdjustment])
  , _sTargetTrackingConfiguration :: !(Maybe TargetTrackingConfiguration)
  , _sAdjustmentType              :: !(Maybe Text)
  , _sAutoScalingGroupName        :: !(Maybe Text)
  , _sScalingAdjustment           :: !(Maybe Int)
  , _sCooldown                    :: !(Maybe Int)
  , _sPolicyARN                   :: !(Maybe Text)
  , _sAlarms                      :: !(Maybe [Alarm])
  , _sMetricAggregationType       :: !(Maybe Text)
  , _sMinAdjustmentMagnitude      :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sMinAdjustmentStep' - Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
--
-- * 'sEstimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics.
--
-- * 'sPolicyName' - The name of the scaling policy.
--
-- * 'sPolicyType' - The policy type. Valid values are @SimpleScaling@ and @StepScaling@ .
--
-- * 'sStepAdjustments' - A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- * 'sTargetTrackingConfiguration' - A target tracking policy.
--
-- * 'sAdjustmentType' - The adjustment type, which specifies how @ScalingAdjustment@ is interpreted. Valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
--
-- * 'sAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'sScalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
--
-- * 'sCooldown' - The amount of time, in seconds, after a scaling activity completes before any further dynamic scaling activities can start.
--
-- * 'sPolicyARN' - The Amazon Resource Name (ARN) of the policy.
--
-- * 'sAlarms' - The CloudWatch alarms related to the policy.
--
-- * 'sMetricAggregationType' - The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ .
--
-- * 'sMinAdjustmentMagnitude' - The minimum number of instances to scale. If the value of @AdjustmentType@ is @PercentChangeInCapacity@ , the scaling policy changes the @DesiredCapacity@ of the Auto Scaling group by at least this many instances. Otherwise, the error is @ValidationError@ .
scalingPolicy
    :: ScalingPolicy
scalingPolicy =
  ScalingPolicy'
    { _sMinAdjustmentStep = Nothing
    , _sEstimatedInstanceWarmup = Nothing
    , _sPolicyName = Nothing
    , _sPolicyType = Nothing
    , _sStepAdjustments = Nothing
    , _sTargetTrackingConfiguration = Nothing
    , _sAdjustmentType = Nothing
    , _sAutoScalingGroupName = Nothing
    , _sScalingAdjustment = Nothing
    , _sCooldown = Nothing
    , _sPolicyARN = Nothing
    , _sAlarms = Nothing
    , _sMetricAggregationType = Nothing
    , _sMinAdjustmentMagnitude = Nothing
    }


-- | Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
sMinAdjustmentStep :: Lens' ScalingPolicy (Maybe Int)
sMinAdjustmentStep = lens _sMinAdjustmentStep (\ s a -> s{_sMinAdjustmentStep = a})

-- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics.
sEstimatedInstanceWarmup :: Lens' ScalingPolicy (Maybe Int)
sEstimatedInstanceWarmup = lens _sEstimatedInstanceWarmup (\ s a -> s{_sEstimatedInstanceWarmup = a})

-- | The name of the scaling policy.
sPolicyName :: Lens' ScalingPolicy (Maybe Text)
sPolicyName = lens _sPolicyName (\ s a -> s{_sPolicyName = a})

-- | The policy type. Valid values are @SimpleScaling@ and @StepScaling@ .
sPolicyType :: Lens' ScalingPolicy (Maybe Text)
sPolicyType = lens _sPolicyType (\ s a -> s{_sPolicyType = a})

-- | A set of adjustments that enable you to scale based on the size of the alarm breach.
sStepAdjustments :: Lens' ScalingPolicy [StepAdjustment]
sStepAdjustments = lens _sStepAdjustments (\ s a -> s{_sStepAdjustments = a}) . _Default . _Coerce

-- | A target tracking policy.
sTargetTrackingConfiguration :: Lens' ScalingPolicy (Maybe TargetTrackingConfiguration)
sTargetTrackingConfiguration = lens _sTargetTrackingConfiguration (\ s a -> s{_sTargetTrackingConfiguration = a})

-- | The adjustment type, which specifies how @ScalingAdjustment@ is interpreted. Valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
sAdjustmentType :: Lens' ScalingPolicy (Maybe Text)
sAdjustmentType = lens _sAdjustmentType (\ s a -> s{_sAdjustmentType = a})

-- | The name of the Auto Scaling group.
sAutoScalingGroupName :: Lens' ScalingPolicy (Maybe Text)
sAutoScalingGroupName = lens _sAutoScalingGroupName (\ s a -> s{_sAutoScalingGroupName = a})

-- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
sScalingAdjustment :: Lens' ScalingPolicy (Maybe Int)
sScalingAdjustment = lens _sScalingAdjustment (\ s a -> s{_sScalingAdjustment = a})

-- | The amount of time, in seconds, after a scaling activity completes before any further dynamic scaling activities can start.
sCooldown :: Lens' ScalingPolicy (Maybe Int)
sCooldown = lens _sCooldown (\ s a -> s{_sCooldown = a})

-- | The Amazon Resource Name (ARN) of the policy.
sPolicyARN :: Lens' ScalingPolicy (Maybe Text)
sPolicyARN = lens _sPolicyARN (\ s a -> s{_sPolicyARN = a})

-- | The CloudWatch alarms related to the policy.
sAlarms :: Lens' ScalingPolicy [Alarm]
sAlarms = lens _sAlarms (\ s a -> s{_sAlarms = a}) . _Default . _Coerce

-- | The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ .
sMetricAggregationType :: Lens' ScalingPolicy (Maybe Text)
sMetricAggregationType = lens _sMetricAggregationType (\ s a -> s{_sMetricAggregationType = a})

-- | The minimum number of instances to scale. If the value of @AdjustmentType@ is @PercentChangeInCapacity@ , the scaling policy changes the @DesiredCapacity@ of the Auto Scaling group by at least this many instances. Otherwise, the error is @ValidationError@ .
sMinAdjustmentMagnitude :: Lens' ScalingPolicy (Maybe Int)
sMinAdjustmentMagnitude = lens _sMinAdjustmentMagnitude (\ s a -> s{_sMinAdjustmentMagnitude = a})

instance FromXML ScalingPolicy where
        parseXML x
          = ScalingPolicy' <$>
              (x .@? "MinAdjustmentStep") <*>
                (x .@? "EstimatedInstanceWarmup")
                <*> (x .@? "PolicyName")
                <*> (x .@? "PolicyType")
                <*>
                (x .@? "StepAdjustments" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "TargetTrackingConfiguration")
                <*> (x .@? "AdjustmentType")
                <*> (x .@? "AutoScalingGroupName")
                <*> (x .@? "ScalingAdjustment")
                <*> (x .@? "Cooldown")
                <*> (x .@? "PolicyARN")
                <*>
                (x .@? "Alarms" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "MetricAggregationType")
                <*> (x .@? "MinAdjustmentMagnitude")

instance Hashable ScalingPolicy where

instance NFData ScalingPolicy where

-- | /See:/ 'scalingProcessQuery' smart constructor.
data ScalingProcessQuery = ScalingProcessQuery'
  { _spqScalingProcesses     :: !(Maybe [Text])
  , _spqAutoScalingGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalingProcessQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spqScalingProcesses' - One or more of the following processes. If you omit this parameter, all processes are specified.     * @Launch@      * @Terminate@      * @HealthCheck@      * @ReplaceUnhealthy@      * @AZRebalance@      * @AlarmNotification@      * @ScheduledActions@      * @AddToLoadBalancer@
--
-- * 'spqAutoScalingGroupName' - The name of the Auto Scaling group.
scalingProcessQuery
    :: Text -- ^ 'spqAutoScalingGroupName'
    -> ScalingProcessQuery
scalingProcessQuery pAutoScalingGroupName_ =
  ScalingProcessQuery'
    { _spqScalingProcesses = Nothing
    , _spqAutoScalingGroupName = pAutoScalingGroupName_
    }


-- | One or more of the following processes. If you omit this parameter, all processes are specified.     * @Launch@      * @Terminate@      * @HealthCheck@      * @ReplaceUnhealthy@      * @AZRebalance@      * @AlarmNotification@      * @ScheduledActions@      * @AddToLoadBalancer@
spqScalingProcesses :: Lens' ScalingProcessQuery [Text]
spqScalingProcesses = lens _spqScalingProcesses (\ s a -> s{_spqScalingProcesses = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group.
spqAutoScalingGroupName :: Lens' ScalingProcessQuery Text
spqAutoScalingGroupName = lens _spqAutoScalingGroupName (\ s a -> s{_spqAutoScalingGroupName = a})

instance Hashable ScalingProcessQuery where

instance NFData ScalingProcessQuery where

instance ToQuery ScalingProcessQuery where
        toQuery ScalingProcessQuery'{..}
          = mconcat
              ["ScalingProcesses" =:
                 toQuery
                   (toQueryList "member" <$> _spqScalingProcesses),
               "AutoScalingGroupName" =: _spqAutoScalingGroupName]

-- | Describes a scheduled update to an Auto Scaling group.
--
--
--
-- /See:/ 'scheduledUpdateGroupAction' smart constructor.
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction'
  { _sugaScheduledActionARN   :: !(Maybe Text)
  , _sugaStartTime            :: !(Maybe ISO8601)
  , _sugaTime                 :: !(Maybe ISO8601)
  , _sugaScheduledActionName  :: !(Maybe Text)
  , _sugaMaxSize              :: !(Maybe Int)
  , _sugaRecurrence           :: !(Maybe Text)
  , _sugaDesiredCapacity      :: !(Maybe Int)
  , _sugaMinSize              :: !(Maybe Int)
  , _sugaAutoScalingGroupName :: !(Maybe Text)
  , _sugaEndTime              :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledUpdateGroupAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sugaScheduledActionARN' - The Amazon Resource Name (ARN) of the scheduled action.
--
-- * 'sugaStartTime' - The date and time that the action is scheduled to begin. This date and time can be up to one month in the future. When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action will start and stop.
--
-- * 'sugaTime' - This parameter is deprecated.
--
-- * 'sugaScheduledActionName' - The name of the scheduled action.
--
-- * 'sugaMaxSize' - The maximum size of the group.
--
-- * 'sugaRecurrence' - The recurring schedule for the action.
--
-- * 'sugaDesiredCapacity' - The number of instances you prefer to maintain in the group.
--
-- * 'sugaMinSize' - The minimum size of the group.
--
-- * 'sugaAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'sugaEndTime' - The date and time that the action is scheduled to end. This date and time can be up to one month in the future.
scheduledUpdateGroupAction
    :: ScheduledUpdateGroupAction
scheduledUpdateGroupAction =
  ScheduledUpdateGroupAction'
    { _sugaScheduledActionARN = Nothing
    , _sugaStartTime = Nothing
    , _sugaTime = Nothing
    , _sugaScheduledActionName = Nothing
    , _sugaMaxSize = Nothing
    , _sugaRecurrence = Nothing
    , _sugaDesiredCapacity = Nothing
    , _sugaMinSize = Nothing
    , _sugaAutoScalingGroupName = Nothing
    , _sugaEndTime = Nothing
    }


-- | The Amazon Resource Name (ARN) of the scheduled action.
sugaScheduledActionARN :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionARN = lens _sugaScheduledActionARN (\ s a -> s{_sugaScheduledActionARN = a})

-- | The date and time that the action is scheduled to begin. This date and time can be up to one month in the future. When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action will start and stop.
sugaStartTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaStartTime = lens _sugaStartTime (\ s a -> s{_sugaStartTime = a}) . mapping _Time

-- | This parameter is deprecated.
sugaTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaTime = lens _sugaTime (\ s a -> s{_sugaTime = a}) . mapping _Time

-- | The name of the scheduled action.
sugaScheduledActionName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionName = lens _sugaScheduledActionName (\ s a -> s{_sugaScheduledActionName = a})

-- | The maximum size of the group.
sugaMaxSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMaxSize = lens _sugaMaxSize (\ s a -> s{_sugaMaxSize = a})

-- | The recurring schedule for the action.
sugaRecurrence :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaRecurrence = lens _sugaRecurrence (\ s a -> s{_sugaRecurrence = a})

-- | The number of instances you prefer to maintain in the group.
sugaDesiredCapacity :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaDesiredCapacity = lens _sugaDesiredCapacity (\ s a -> s{_sugaDesiredCapacity = a})

-- | The minimum size of the group.
sugaMinSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMinSize = lens _sugaMinSize (\ s a -> s{_sugaMinSize = a})

-- | The name of the Auto Scaling group.
sugaAutoScalingGroupName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaAutoScalingGroupName = lens _sugaAutoScalingGroupName (\ s a -> s{_sugaAutoScalingGroupName = a})

-- | The date and time that the action is scheduled to end. This date and time can be up to one month in the future.
sugaEndTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaEndTime = lens _sugaEndTime (\ s a -> s{_sugaEndTime = a}) . mapping _Time

instance FromXML ScheduledUpdateGroupAction where
        parseXML x
          = ScheduledUpdateGroupAction' <$>
              (x .@? "ScheduledActionARN") <*> (x .@? "StartTime")
                <*> (x .@? "Time")
                <*> (x .@? "ScheduledActionName")
                <*> (x .@? "MaxSize")
                <*> (x .@? "Recurrence")
                <*> (x .@? "DesiredCapacity")
                <*> (x .@? "MinSize")
                <*> (x .@? "AutoScalingGroupName")
                <*> (x .@? "EndTime")

instance Hashable ScheduledUpdateGroupAction where

instance NFData ScheduledUpdateGroupAction where

-- | Describes an adjustment based on the difference between the value of the aggregated CloudWatch metric and the breach threshold that you've defined for the alarm.
--
--
-- For the following examples, suppose that you have an alarm with a breach threshold of 50:
--
--     * If you want the adjustment to be triggered when the metric is greater than or equal to 50 and less than 60, specify a lower bound of 0 and an upper bound of 10.
--
--     * If you want the adjustment to be triggered when the metric is greater than 40 and less than or equal to 50, specify a lower bound of -10 and an upper bound of 0.
--
--
--
-- There are a few rules for the step adjustments for your step policy:
--
--     * The ranges of your step adjustments can't overlap or have a gap.
--
--     * At most one step adjustment can have a null lower bound. If one step adjustment has a negative lower bound, then there must be a step adjustment with a null lower bound.
--
--     * At most one step adjustment can have a null upper bound. If one step adjustment has a positive upper bound, then there must be a step adjustment with a null upper bound.
--
--     * The upper and lower bound can't be null in the same step adjustment.
--
--
--
--
-- /See:/ 'stepAdjustment' smart constructor.
data StepAdjustment = StepAdjustment'
  { _saMetricIntervalLowerBound :: !(Maybe Double)
  , _saMetricIntervalUpperBound :: !(Maybe Double)
  , _saScalingAdjustment        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StepAdjustment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saMetricIntervalLowerBound' - The lower bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the lower bound is inclusive (the metric must be greater than or equal to the threshold plus the lower bound). Otherwise, it is exclusive (the metric must be greater than the threshold plus the lower bound). A null value indicates negative infinity.
--
-- * 'saMetricIntervalUpperBound' - The upper bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the upper bound is exclusive (the metric must be less than the threshold plus the upper bound). Otherwise, it is inclusive (the metric must be less than or equal to the threshold plus the upper bound). A null value indicates positive infinity. The upper bound must be greater than the lower bound.
--
-- * 'saScalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
stepAdjustment
    :: Int -- ^ 'saScalingAdjustment'
    -> StepAdjustment
stepAdjustment pScalingAdjustment_ =
  StepAdjustment'
    { _saMetricIntervalLowerBound = Nothing
    , _saMetricIntervalUpperBound = Nothing
    , _saScalingAdjustment = pScalingAdjustment_
    }


-- | The lower bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the lower bound is inclusive (the metric must be greater than or equal to the threshold plus the lower bound). Otherwise, it is exclusive (the metric must be greater than the threshold plus the lower bound). A null value indicates negative infinity.
saMetricIntervalLowerBound :: Lens' StepAdjustment (Maybe Double)
saMetricIntervalLowerBound = lens _saMetricIntervalLowerBound (\ s a -> s{_saMetricIntervalLowerBound = a})

-- | The upper bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the upper bound is exclusive (the metric must be less than the threshold plus the upper bound). Otherwise, it is inclusive (the metric must be less than or equal to the threshold plus the upper bound). A null value indicates positive infinity. The upper bound must be greater than the lower bound.
saMetricIntervalUpperBound :: Lens' StepAdjustment (Maybe Double)
saMetricIntervalUpperBound = lens _saMetricIntervalUpperBound (\ s a -> s{_saMetricIntervalUpperBound = a})

-- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
saScalingAdjustment :: Lens' StepAdjustment Int
saScalingAdjustment = lens _saScalingAdjustment (\ s a -> s{_saScalingAdjustment = a})

instance FromXML StepAdjustment where
        parseXML x
          = StepAdjustment' <$>
              (x .@? "MetricIntervalLowerBound") <*>
                (x .@? "MetricIntervalUpperBound")
                <*> (x .@ "ScalingAdjustment")

instance Hashable StepAdjustment where

instance NFData StepAdjustment where

instance ToQuery StepAdjustment where
        toQuery StepAdjustment'{..}
          = mconcat
              ["MetricIntervalLowerBound" =:
                 _saMetricIntervalLowerBound,
               "MetricIntervalUpperBound" =:
                 _saMetricIntervalUpperBound,
               "ScalingAdjustment" =: _saScalingAdjustment]

-- | Describes an Auto Scaling process that has been suspended. For more information, see 'ProcessType' .
--
--
--
-- /See:/ 'suspendedProcess' smart constructor.
data SuspendedProcess = SuspendedProcess'
  { _spProcessName      :: !(Maybe Text)
  , _spSuspensionReason :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SuspendedProcess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spProcessName' - The name of the suspended process.
--
-- * 'spSuspensionReason' - The reason that the process was suspended.
suspendedProcess
    :: SuspendedProcess
suspendedProcess =
  SuspendedProcess' {_spProcessName = Nothing, _spSuspensionReason = Nothing}


-- | The name of the suspended process.
spProcessName :: Lens' SuspendedProcess (Maybe Text)
spProcessName = lens _spProcessName (\ s a -> s{_spProcessName = a})

-- | The reason that the process was suspended.
spSuspensionReason :: Lens' SuspendedProcess (Maybe Text)
spSuspensionReason = lens _spSuspensionReason (\ s a -> s{_spSuspensionReason = a})

instance FromXML SuspendedProcess where
        parseXML x
          = SuspendedProcess' <$>
              (x .@? "ProcessName") <*> (x .@? "SuspensionReason")

instance Hashable SuspendedProcess where

instance NFData SuspendedProcess where

-- | Describes a tag for an Auto Scaling group.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey               :: !Text
  , _tagResourceId        :: !Text
  , _tagResourceType      :: !Text
  , _tagPropagateAtLaunch :: !Bool
  , _tagValue             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The tag key.
--
-- * 'tagResourceId' - The name of the group.
--
-- * 'tagResourceType' - The type of resource. The only supported value is @auto-scaling-group@ .
--
-- * 'tagPropagateAtLaunch' - Determines whether the tag is added to new instances as they are launched in the group.
--
-- * 'tagValue' - The tag value.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagResourceId'
    -> Text -- ^ 'tagResourceType'
    -> Bool -- ^ 'tagPropagateAtLaunch'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pResourceId_ pResourceType_ pPropagateAtLaunch_ pValue_ =
  Tag'
    { _tagKey = pKey_
    , _tagResourceId = pResourceId_
    , _tagResourceType = pResourceType_
    , _tagPropagateAtLaunch = pPropagateAtLaunch_
    , _tagValue = pValue_
    }


-- | The tag key.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The name of the group.
tagResourceId :: Lens' Tag Text
tagResourceId = lens _tagResourceId (\ s a -> s{_tagResourceId = a})

-- | The type of resource. The only supported value is @auto-scaling-group@ .
tagResourceType :: Lens' Tag Text
tagResourceType = lens _tagResourceType (\ s a -> s{_tagResourceType = a})

-- | Determines whether the tag is added to new instances as they are launched in the group.
tagPropagateAtLaunch :: Lens' Tag Bool
tagPropagateAtLaunch = lens _tagPropagateAtLaunch (\ s a -> s{_tagPropagateAtLaunch = a})

-- | The tag value.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance Hashable Tag where

instance NFData Tag where

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat
              ["Key" =: _tagKey, "ResourceId" =: _tagResourceId,
               "ResourceType" =: _tagResourceType,
               "PropagateAtLaunch" =: _tagPropagateAtLaunch,
               "Value" =: _tagValue]

-- | Describes a tag for an Auto Scaling group.
--
--
--
-- /See:/ 'tagDescription' smart constructor.
data TagDescription = TagDescription'
  { _tdResourceId        :: !Text
  , _tdResourceType      :: !Text
  , _tdKey               :: !Text
  , _tdPropagateAtLaunch :: !Bool
  , _tdValue             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdResourceId' - The name of the group.
--
-- * 'tdResourceType' - The type of resource. The only supported value is @auto-scaling-group@ .
--
-- * 'tdKey' - The tag key.
--
-- * 'tdPropagateAtLaunch' - Determines whether the tag is added to new instances as they are launched in the group.
--
-- * 'tdValue' - The tag value.
tagDescription
    :: Text -- ^ 'tdResourceId'
    -> Text -- ^ 'tdResourceType'
    -> Text -- ^ 'tdKey'
    -> Bool -- ^ 'tdPropagateAtLaunch'
    -> Text -- ^ 'tdValue'
    -> TagDescription
tagDescription pResourceId_ pResourceType_ pKey_ pPropagateAtLaunch_ pValue_ =
  TagDescription'
    { _tdResourceId = pResourceId_
    , _tdResourceType = pResourceType_
    , _tdKey = pKey_
    , _tdPropagateAtLaunch = pPropagateAtLaunch_
    , _tdValue = pValue_
    }


-- | The name of the group.
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\ s a -> s{_tdResourceId = a})

-- | The type of resource. The only supported value is @auto-scaling-group@ .
tdResourceType :: Lens' TagDescription Text
tdResourceType = lens _tdResourceType (\ s a -> s{_tdResourceType = a})

-- | The tag key.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\ s a -> s{_tdKey = a})

-- | Determines whether the tag is added to new instances as they are launched in the group.
tdPropagateAtLaunch :: Lens' TagDescription Bool
tdPropagateAtLaunch = lens _tdPropagateAtLaunch (\ s a -> s{_tdPropagateAtLaunch = a})

-- | The tag value.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\ s a -> s{_tdValue = a})

instance FromXML TagDescription where
        parseXML x
          = TagDescription' <$>
              (x .@ "ResourceId") <*> (x .@ "ResourceType") <*>
                (x .@ "Key")
                <*> (x .@ "PropagateAtLaunch")
                <*> (x .@ "Value")

instance Hashable TagDescription where

instance NFData TagDescription where

-- | Represents a target tracking policy configuration.
--
--
--
-- /See:/ 'targetTrackingConfiguration' smart constructor.
data TargetTrackingConfiguration = TargetTrackingConfiguration'
  { _ttcPredefinedMetricSpecification :: !(Maybe PredefinedMetricSpecification)
  , _ttcCustomizedMetricSpecification :: !(Maybe CustomizedMetricSpecification)
  , _ttcDisableScaleIn                :: !(Maybe Bool)
  , _ttcTargetValue                   :: !Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetTrackingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttcPredefinedMetricSpecification' - A predefined metric. You can specify either a predefined metric or a customized metric.
--
-- * 'ttcCustomizedMetricSpecification' - A customized metric.
--
-- * 'ttcDisableScaleIn' - Indicates whether scale in by the target tracking policy is disabled. If scale in is disabled, the target tracking policy won't remove instances from the Auto Scaling group. Otherwise, the target tracking policy can remove instances from the Auto Scaling group. The default is disabled.
--
-- * 'ttcTargetValue' - The target value for the metric.
targetTrackingConfiguration
    :: Double -- ^ 'ttcTargetValue'
    -> TargetTrackingConfiguration
targetTrackingConfiguration pTargetValue_ =
  TargetTrackingConfiguration'
    { _ttcPredefinedMetricSpecification = Nothing
    , _ttcCustomizedMetricSpecification = Nothing
    , _ttcDisableScaleIn = Nothing
    , _ttcTargetValue = pTargetValue_
    }


-- | A predefined metric. You can specify either a predefined metric or a customized metric.
ttcPredefinedMetricSpecification :: Lens' TargetTrackingConfiguration (Maybe PredefinedMetricSpecification)
ttcPredefinedMetricSpecification = lens _ttcPredefinedMetricSpecification (\ s a -> s{_ttcPredefinedMetricSpecification = a})

-- | A customized metric.
ttcCustomizedMetricSpecification :: Lens' TargetTrackingConfiguration (Maybe CustomizedMetricSpecification)
ttcCustomizedMetricSpecification = lens _ttcCustomizedMetricSpecification (\ s a -> s{_ttcCustomizedMetricSpecification = a})

-- | Indicates whether scale in by the target tracking policy is disabled. If scale in is disabled, the target tracking policy won't remove instances from the Auto Scaling group. Otherwise, the target tracking policy can remove instances from the Auto Scaling group. The default is disabled.
ttcDisableScaleIn :: Lens' TargetTrackingConfiguration (Maybe Bool)
ttcDisableScaleIn = lens _ttcDisableScaleIn (\ s a -> s{_ttcDisableScaleIn = a})

-- | The target value for the metric.
ttcTargetValue :: Lens' TargetTrackingConfiguration Double
ttcTargetValue = lens _ttcTargetValue (\ s a -> s{_ttcTargetValue = a})

instance FromXML TargetTrackingConfiguration where
        parseXML x
          = TargetTrackingConfiguration' <$>
              (x .@? "PredefinedMetricSpecification") <*>
                (x .@? "CustomizedMetricSpecification")
                <*> (x .@? "DisableScaleIn")
                <*> (x .@ "TargetValue")

instance Hashable TargetTrackingConfiguration where

instance NFData TargetTrackingConfiguration where

instance ToQuery TargetTrackingConfiguration where
        toQuery TargetTrackingConfiguration'{..}
          = mconcat
              ["PredefinedMetricSpecification" =:
                 _ttcPredefinedMetricSpecification,
               "CustomizedMetricSpecification" =:
                 _ttcCustomizedMetricSpecification,
               "DisableScaleIn" =: _ttcDisableScaleIn,
               "TargetValue" =: _ttcTargetValue]
