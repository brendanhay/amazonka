{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.Product where

import           Network.AWS.AutoScaling.Types.Sum
import           Network.AWS.Prelude

-- | Describes scaling activity, which is a long-running process that
-- represents a change to your Auto Scaling group, such as changing its
-- size or replacing an instance.
--
-- /See:/ 'activity' smart constructor.
data Activity = Activity'
    { _aProgress             :: !(Maybe Int)
    , _aStatusMessage        :: !(Maybe Text)
    , _aDetails              :: !(Maybe Text)
    , _aEndTime              :: !(Maybe ISO8601)
    , _aDescription          :: !(Maybe Text)
    , _aActivityId           :: !Text
    , _aAutoScalingGroupName :: !Text
    , _aCause                :: !Text
    , _aStartTime            :: !ISO8601
    , _aStatusCode           :: !ScalingActivityStatusCode
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Activity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aProgress'
--
-- * 'aStatusMessage'
--
-- * 'aDetails'
--
-- * 'aEndTime'
--
-- * 'aDescription'
--
-- * 'aActivityId'
--
-- * 'aAutoScalingGroupName'
--
-- * 'aCause'
--
-- * 'aStartTime'
--
-- * 'aStatusCode'
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
    , _aDetails = Nothing
    , _aEndTime = Nothing
    , _aDescription = Nothing
    , _aActivityId = pActivityId_
    , _aAutoScalingGroupName = pAutoScalingGroupName_
    , _aCause = pCause_
    , _aStartTime = _Time # pStartTime_
    , _aStatusCode = pStatusCode_
    }

-- | A value between 0 and 100 that indicates the progress of the activity.
aProgress :: Lens' Activity (Maybe Int)
aProgress = lens _aProgress (\ s a -> s{_aProgress = a});

-- | A friendly, more verbose description of the activity status.
aStatusMessage :: Lens' Activity (Maybe Text)
aStatusMessage = lens _aStatusMessage (\ s a -> s{_aStatusMessage = a});

-- | The details about the activity.
aDetails :: Lens' Activity (Maybe Text)
aDetails = lens _aDetails (\ s a -> s{_aDetails = a});

-- | The end time of the activity.
aEndTime :: Lens' Activity (Maybe UTCTime)
aEndTime = lens _aEndTime (\ s a -> s{_aEndTime = a}) . mapping _Time;

-- | A friendly, more verbose description of the activity.
aDescription :: Lens' Activity (Maybe Text)
aDescription = lens _aDescription (\ s a -> s{_aDescription = a});

-- | The ID of the activity.
aActivityId :: Lens' Activity Text
aActivityId = lens _aActivityId (\ s a -> s{_aActivityId = a});

-- | The name of the Auto Scaling group.
aAutoScalingGroupName :: Lens' Activity Text
aAutoScalingGroupName = lens _aAutoScalingGroupName (\ s a -> s{_aAutoScalingGroupName = a});

-- | The reason the activity began.
aCause :: Lens' Activity Text
aCause = lens _aCause (\ s a -> s{_aCause = a});

-- | The start time of the activity.
aStartTime :: Lens' Activity UTCTime
aStartTime = lens _aStartTime (\ s a -> s{_aStartTime = a}) . _Time;

-- | The current status of the activity.
aStatusCode :: Lens' Activity ScalingActivityStatusCode
aStatusCode = lens _aStatusCode (\ s a -> s{_aStatusCode = a});

instance FromXML Activity where
        parseXML x
          = Activity' <$>
              (x .@? "Progress") <*> (x .@? "StatusMessage") <*>
                (x .@? "Details")
                <*> (x .@? "EndTime")
                <*> (x .@? "Description")
                <*> (x .@ "ActivityId")
                <*> (x .@ "AutoScalingGroupName")
                <*> (x .@ "Cause")
                <*> (x .@ "StartTime")
                <*> (x .@ "StatusCode")

-- | Describes a policy adjustment type.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/as-scale-based-on-demand.html Dynamic Scaling>
-- in the /Auto Scaling Developer Guide/.
--
-- /See:/ 'adjustmentType' smart constructor.
newtype AdjustmentType = AdjustmentType'
    { _atAdjustmentType :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AdjustmentType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atAdjustmentType'
adjustmentType
    :: AdjustmentType
adjustmentType =
    AdjustmentType'
    { _atAdjustmentType = Nothing
    }

-- | The policy adjustment type. The valid values are 'ChangeInCapacity',
-- 'ExactCapacity', and 'PercentChangeInCapacity'.
atAdjustmentType :: Lens' AdjustmentType (Maybe Text)
atAdjustmentType = lens _atAdjustmentType (\ s a -> s{_atAdjustmentType = a});

instance FromXML AdjustmentType where
        parseXML x
          = AdjustmentType' <$> (x .@? "AdjustmentType")

-- | Describes an alarm.
--
-- /See:/ 'alarm' smart constructor.
data Alarm = Alarm'
    { _aAlarmName :: !(Maybe Text)
    , _aAlarmARN  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Alarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAlarmName'
--
-- * 'aAlarmARN'
alarm
    :: Alarm
alarm =
    Alarm'
    { _aAlarmName = Nothing
    , _aAlarmARN = Nothing
    }

-- | The name of the alarm.
aAlarmName :: Lens' Alarm (Maybe Text)
aAlarmName = lens _aAlarmName (\ s a -> s{_aAlarmName = a});

-- | The Amazon Resource Name (ARN) of the alarm.
aAlarmARN :: Lens' Alarm (Maybe Text)
aAlarmARN = lens _aAlarmARN (\ s a -> s{_aAlarmARN = a});

instance FromXML Alarm where
        parseXML x
          = Alarm' <$>
              (x .@? "AlarmName") <*> (x .@? "AlarmARN")

-- | Describes an Auto Scaling group.
--
-- /See:/ 'autoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
    { _asgStatus                  :: !(Maybe Text)
    , _asgTerminationPolicies     :: !(Maybe [Text])
    , _asgHealthCheckGracePeriod  :: !(Maybe Int)
    , _asgVPCZoneIdentifier       :: !(Maybe Text)
    , _asgEnabledMetrics          :: !(Maybe [EnabledMetric])
    , _asgInstances               :: !(Maybe [Instance])
    , _asgLaunchConfigurationName :: !(Maybe Text)
    , _asgAutoScalingGroupARN     :: !(Maybe Text)
    , _asgSuspendedProcesses      :: !(Maybe [SuspendedProcess])
    , _asgPlacementGroup          :: !(Maybe Text)
    , _asgLoadBalancerNames       :: !(Maybe [Text])
    , _asgTags                    :: !(Maybe [TagDescription])
    , _asgAutoScalingGroupName    :: !Text
    , _asgMinSize                 :: !Int
    , _asgMaxSize                 :: !Int
    , _asgDesiredCapacity         :: !Int
    , _asgDefaultCooldown         :: !Int
    , _asgAvailabilityZones       :: !(List1 Text)
    , _asgHealthCheckType         :: !Text
    , _asgCreatedTime             :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgStatus'
--
-- * 'asgTerminationPolicies'
--
-- * 'asgHealthCheckGracePeriod'
--
-- * 'asgVPCZoneIdentifier'
--
-- * 'asgEnabledMetrics'
--
-- * 'asgInstances'
--
-- * 'asgLaunchConfigurationName'
--
-- * 'asgAutoScalingGroupARN'
--
-- * 'asgSuspendedProcesses'
--
-- * 'asgPlacementGroup'
--
-- * 'asgLoadBalancerNames'
--
-- * 'asgTags'
--
-- * 'asgAutoScalingGroupName'
--
-- * 'asgMinSize'
--
-- * 'asgMaxSize'
--
-- * 'asgDesiredCapacity'
--
-- * 'asgDefaultCooldown'
--
-- * 'asgAvailabilityZones'
--
-- * 'asgHealthCheckType'
--
-- * 'asgCreatedTime'
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
    , _asgVPCZoneIdentifier = Nothing
    , _asgEnabledMetrics = Nothing
    , _asgInstances = Nothing
    , _asgLaunchConfigurationName = Nothing
    , _asgAutoScalingGroupARN = Nothing
    , _asgSuspendedProcesses = Nothing
    , _asgPlacementGroup = Nothing
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

-- | The current state of the group when DeleteAutoScalingGroup is in
-- progress.
asgStatus :: Lens' AutoScalingGroup (Maybe Text)
asgStatus = lens _asgStatus (\ s a -> s{_asgStatus = a});

-- | The termination policies for the group.
asgTerminationPolicies :: Lens' AutoScalingGroup [Text]
asgTerminationPolicies = lens _asgTerminationPolicies (\ s a -> s{_asgTerminationPolicies = a}) . _Default . _Coerce;

-- | The amount of time that Auto Scaling waits before checking an
-- instance\'s health status. The grace period begins when an instance
-- comes into service.
asgHealthCheckGracePeriod :: Lens' AutoScalingGroup (Maybe Int)
asgHealthCheckGracePeriod = lens _asgHealthCheckGracePeriod (\ s a -> s{_asgHealthCheckGracePeriod = a});

-- | One or more subnet IDs, if applicable, separated by commas.
--
-- If you specify 'VPCZoneIdentifier' and 'AvailabilityZones', ensure that
-- the Availability Zones of the subnets match the values for
-- 'AvailabilityZones'.
asgVPCZoneIdentifier :: Lens' AutoScalingGroup (Maybe Text)
asgVPCZoneIdentifier = lens _asgVPCZoneIdentifier (\ s a -> s{_asgVPCZoneIdentifier = a});

-- | The metrics enabled for the group.
asgEnabledMetrics :: Lens' AutoScalingGroup [EnabledMetric]
asgEnabledMetrics = lens _asgEnabledMetrics (\ s a -> s{_asgEnabledMetrics = a}) . _Default . _Coerce;

-- | The EC2 instances associated with the group.
asgInstances :: Lens' AutoScalingGroup [Instance]
asgInstances = lens _asgInstances (\ s a -> s{_asgInstances = a}) . _Default . _Coerce;

-- | The name of the associated launch configuration.
asgLaunchConfigurationName :: Lens' AutoScalingGroup (Maybe Text)
asgLaunchConfigurationName = lens _asgLaunchConfigurationName (\ s a -> s{_asgLaunchConfigurationName = a});

-- | The Amazon Resource Name (ARN) of the group.
asgAutoScalingGroupARN :: Lens' AutoScalingGroup (Maybe Text)
asgAutoScalingGroupARN = lens _asgAutoScalingGroupARN (\ s a -> s{_asgAutoScalingGroupARN = a});

-- | The suspended processes associated with the group.
asgSuspendedProcesses :: Lens' AutoScalingGroup [SuspendedProcess]
asgSuspendedProcesses = lens _asgSuspendedProcesses (\ s a -> s{_asgSuspendedProcesses = a}) . _Default . _Coerce;

-- | The name of the placement group into which you\'ll launch your
-- instances, if any. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>.
asgPlacementGroup :: Lens' AutoScalingGroup (Maybe Text)
asgPlacementGroup = lens _asgPlacementGroup (\ s a -> s{_asgPlacementGroup = a});

-- | One or more load balancers associated with the group.
asgLoadBalancerNames :: Lens' AutoScalingGroup [Text]
asgLoadBalancerNames = lens _asgLoadBalancerNames (\ s a -> s{_asgLoadBalancerNames = a}) . _Default . _Coerce;

-- | The tags for the group.
asgTags :: Lens' AutoScalingGroup [TagDescription]
asgTags = lens _asgTags (\ s a -> s{_asgTags = a}) . _Default . _Coerce;

-- | The name of the group.
asgAutoScalingGroupName :: Lens' AutoScalingGroup Text
asgAutoScalingGroupName = lens _asgAutoScalingGroupName (\ s a -> s{_asgAutoScalingGroupName = a});

-- | The minimum size of the group.
asgMinSize :: Lens' AutoScalingGroup Int
asgMinSize = lens _asgMinSize (\ s a -> s{_asgMinSize = a});

-- | The maximum size of the group.
asgMaxSize :: Lens' AutoScalingGroup Int
asgMaxSize = lens _asgMaxSize (\ s a -> s{_asgMaxSize = a});

-- | The desired size of the group.
asgDesiredCapacity :: Lens' AutoScalingGroup Int
asgDesiredCapacity = lens _asgDesiredCapacity (\ s a -> s{_asgDesiredCapacity = a});

-- | The number of seconds after a scaling activity completes before any
-- further scaling activities can start.
asgDefaultCooldown :: Lens' AutoScalingGroup Int
asgDefaultCooldown = lens _asgDefaultCooldown (\ s a -> s{_asgDefaultCooldown = a});

-- | One or more Availability Zones for the group.
asgAvailabilityZones :: Lens' AutoScalingGroup (NonEmpty Text)
asgAvailabilityZones = lens _asgAvailabilityZones (\ s a -> s{_asgAvailabilityZones = a}) . _List1;

-- | The service of interest for the health status check, which can be either
-- 'EC2' for Amazon EC2 or 'ELB' for Elastic Load Balancing.
asgHealthCheckType :: Lens' AutoScalingGroup Text
asgHealthCheckType = lens _asgHealthCheckType (\ s a -> s{_asgHealthCheckType = a});

-- | The date and time the group was created.
asgCreatedTime :: Lens' AutoScalingGroup UTCTime
asgCreatedTime = lens _asgCreatedTime (\ s a -> s{_asgCreatedTime = a}) . _Time;

instance FromXML AutoScalingGroup where
        parseXML x
          = AutoScalingGroup' <$>
              (x .@? "Status") <*>
                (x .@? "TerminationPolicies" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "HealthCheckGracePeriod")
                <*> (x .@? "VPCZoneIdentifier")
                <*>
                (x .@? "EnabledMetrics" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Instances" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LaunchConfigurationName")
                <*> (x .@? "AutoScalingGroupARN")
                <*>
                (x .@? "SuspendedProcesses" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "PlacementGroup")
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

-- | Describes an EC2 instance associated with an Auto Scaling group.
--
-- /See:/ 'autoScalingInstanceDetails' smart constructor.
data AutoScalingInstanceDetails = AutoScalingInstanceDetails'
    { _asidInstanceId              :: !Text
    , _asidAutoScalingGroupName    :: !Text
    , _asidAvailabilityZone        :: !Text
    , _asidLifecycleState          :: !Text
    , _asidHealthStatus            :: !Text
    , _asidLaunchConfigurationName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AutoScalingInstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asidInstanceId'
--
-- * 'asidAutoScalingGroupName'
--
-- * 'asidAvailabilityZone'
--
-- * 'asidLifecycleState'
--
-- * 'asidHealthStatus'
--
-- * 'asidLaunchConfigurationName'
autoScalingInstanceDetails
    :: Text -- ^ 'asidInstanceId'
    -> Text -- ^ 'asidAutoScalingGroupName'
    -> Text -- ^ 'asidAvailabilityZone'
    -> Text -- ^ 'asidLifecycleState'
    -> Text -- ^ 'asidHealthStatus'
    -> Text -- ^ 'asidLaunchConfigurationName'
    -> AutoScalingInstanceDetails
autoScalingInstanceDetails pInstanceId_ pAutoScalingGroupName_ pAvailabilityZone_ pLifecycleState_ pHealthStatus_ pLaunchConfigurationName_ =
    AutoScalingInstanceDetails'
    { _asidInstanceId = pInstanceId_
    , _asidAutoScalingGroupName = pAutoScalingGroupName_
    , _asidAvailabilityZone = pAvailabilityZone_
    , _asidLifecycleState = pLifecycleState_
    , _asidHealthStatus = pHealthStatus_
    , _asidLaunchConfigurationName = pLaunchConfigurationName_
    }

-- | The ID of the instance.
asidInstanceId :: Lens' AutoScalingInstanceDetails Text
asidInstanceId = lens _asidInstanceId (\ s a -> s{_asidInstanceId = a});

-- | The name of the Auto Scaling group associated with the instance.
asidAutoScalingGroupName :: Lens' AutoScalingInstanceDetails Text
asidAutoScalingGroupName = lens _asidAutoScalingGroupName (\ s a -> s{_asidAutoScalingGroupName = a});

-- | The Availability Zone for the instance.
asidAvailabilityZone :: Lens' AutoScalingInstanceDetails Text
asidAvailabilityZone = lens _asidAvailabilityZone (\ s a -> s{_asidAvailabilityZone = a});

-- | The lifecycle state for the instance. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingGroupLifecycle.html#AutoScalingStates Auto Scaling Instance States>
-- in the /Auto Scaling Developer Guide/.
asidLifecycleState :: Lens' AutoScalingInstanceDetails Text
asidLifecycleState = lens _asidLifecycleState (\ s a -> s{_asidLifecycleState = a});

-- | The health status of this instance. \"Healthy\" means that the instance
-- is healthy and should remain in service. \"Unhealthy\" means that the
-- instance is unhealthy and Auto Scaling should terminate and replace it.
asidHealthStatus :: Lens' AutoScalingInstanceDetails Text
asidHealthStatus = lens _asidHealthStatus (\ s a -> s{_asidHealthStatus = a});

-- | The launch configuration associated with the instance.
asidLaunchConfigurationName :: Lens' AutoScalingInstanceDetails Text
asidLaunchConfigurationName = lens _asidLaunchConfigurationName (\ s a -> s{_asidLaunchConfigurationName = a});

instance FromXML AutoScalingInstanceDetails where
        parseXML x
          = AutoScalingInstanceDetails' <$>
              (x .@ "InstanceId") <*> (x .@ "AutoScalingGroupName")
                <*> (x .@ "AvailabilityZone")
                <*> (x .@ "LifecycleState")
                <*> (x .@ "HealthStatus")
                <*> (x .@ "LaunchConfigurationName")

-- | Describes a block device mapping.
--
-- /See:/ 'blockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
    { _bdmVirtualName :: !(Maybe Text)
    , _bdmNoDevice    :: !(Maybe Bool)
    , _bdmEBS         :: !(Maybe EBS)
    , _bdmDeviceName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BlockDeviceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdmVirtualName'
--
-- * 'bdmNoDevice'
--
-- * 'bdmEBS'
--
-- * 'bdmDeviceName'
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

-- | The name of the virtual device, 'ephemeral0' to 'ephemeral3'.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\ s a -> s{_bdmVirtualName = a});

-- | Suppresses a device mapping.
--
-- If this parameter is true for the root device, the instance might fail
-- the EC2 health check. Auto Scaling launches a replacement instance if
-- the instance fails the health check.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Bool)
bdmNoDevice = lens _bdmNoDevice (\ s a -> s{_bdmNoDevice = a});

-- | The information about the Amazon EBS volume.
bdmEBS :: Lens' BlockDeviceMapping (Maybe EBS)
bdmEBS = lens _bdmEBS (\ s a -> s{_bdmEBS = a});

-- | The device name exposed to the EC2 instance (for example, '\/dev\/sdh'
-- or 'xvdh').
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\ s a -> s{_bdmDeviceName = a});

instance FromXML BlockDeviceMapping where
        parseXML x
          = BlockDeviceMapping' <$>
              (x .@? "VirtualName") <*> (x .@? "NoDevice") <*>
                (x .@? "Ebs")
                <*> (x .@ "DeviceName")

instance ToQuery BlockDeviceMapping where
        toQuery BlockDeviceMapping'{..}
          = mconcat
              ["VirtualName" =: _bdmVirtualName,
               "NoDevice" =: _bdmNoDevice, "Ebs" =: _bdmEBS,
               "DeviceName" =: _bdmDeviceName]

-- | Describes an Amazon EBS volume.
--
-- /See:/ 'ebs' smart constructor.
data EBS = EBS'
    { _ebsDeleteOnTermination :: !(Maybe Bool)
    , _ebsVolumeSize          :: !(Maybe Nat)
    , _ebsIOPS                :: !(Maybe Nat)
    , _ebsVolumeType          :: !(Maybe Text)
    , _ebsSnapshotId          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EBS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebsDeleteOnTermination'
--
-- * 'ebsVolumeSize'
--
-- * 'ebsIOPS'
--
-- * 'ebsVolumeType'
--
-- * 'ebsSnapshotId'
ebs
    :: EBS
ebs =
    EBS'
    { _ebsDeleteOnTermination = Nothing
    , _ebsVolumeSize = Nothing
    , _ebsIOPS = Nothing
    , _ebsVolumeType = Nothing
    , _ebsSnapshotId = Nothing
    }

-- | Indicates whether to delete the volume on instance termination.
--
-- Default: 'true'
ebsDeleteOnTermination :: Lens' EBS (Maybe Bool)
ebsDeleteOnTermination = lens _ebsDeleteOnTermination (\ s a -> s{_ebsDeleteOnTermination = a});

-- | The volume size, in gigabytes.
--
-- Valid values: If the volume type is 'io1', the minimum size of the
-- volume is 10 GiB. If you specify 'SnapshotId' and 'VolumeSize',
-- 'VolumeSize' must be equal to or larger than the size of the snapshot.
--
-- Default: If you create a volume from a snapshot and you don\'t specify a
-- volume size, the default is the size of the snapshot.
--
-- Required: Required when the volume type is 'io1'.
ebsVolumeSize :: Lens' EBS (Maybe Natural)
ebsVolumeSize = lens _ebsVolumeSize (\ s a -> s{_ebsVolumeSize = a}) . mapping _Nat;

-- | For Provisioned IOPS (SSD) volumes only. The number of I\/O operations
-- per second (IOPS) to provision for the volume.
--
-- Valid values: Range is 100 to 4000.
--
-- Default: None
ebsIOPS :: Lens' EBS (Maybe Natural)
ebsIOPS = lens _ebsIOPS (\ s a -> s{_ebsIOPS = a}) . mapping _Nat;

-- | The volume type.
--
-- Valid values: 'standard | io1 | gp2'
--
-- Default: 'standard'
ebsVolumeType :: Lens' EBS (Maybe Text)
ebsVolumeType = lens _ebsVolumeType (\ s a -> s{_ebsVolumeType = a});

-- | The ID of the snapshot.
ebsSnapshotId :: Lens' EBS (Maybe Text)
ebsSnapshotId = lens _ebsSnapshotId (\ s a -> s{_ebsSnapshotId = a});

instance FromXML EBS where
        parseXML x
          = EBS' <$>
              (x .@? "DeleteOnTermination") <*>
                (x .@? "VolumeSize")
                <*> (x .@? "Iops")
                <*> (x .@? "VolumeType")
                <*> (x .@? "SnapshotId")

instance ToQuery EBS where
        toQuery EBS'{..}
          = mconcat
              ["DeleteOnTermination" =: _ebsDeleteOnTermination,
               "VolumeSize" =: _ebsVolumeSize, "Iops" =: _ebsIOPS,
               "VolumeType" =: _ebsVolumeType,
               "SnapshotId" =: _ebsSnapshotId]

-- | Describes an enabled metric.
--
-- /See:/ 'enabledMetric' smart constructor.
data EnabledMetric = EnabledMetric'
    { _emGranularity :: !(Maybe Text)
    , _emMetric      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnabledMetric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emGranularity'
--
-- * 'emMetric'
enabledMetric
    :: EnabledMetric
enabledMetric =
    EnabledMetric'
    { _emGranularity = Nothing
    , _emMetric = Nothing
    }

-- | The granularity of the metric. The only valid value is '1Minute'.
emGranularity :: Lens' EnabledMetric (Maybe Text)
emGranularity = lens _emGranularity (\ s a -> s{_emGranularity = a});

-- | The name of the metric.
--
-- -   'GroupMinSize'
--
-- -   'GroupMaxSize'
--
-- -   'GroupDesiredCapacity'
--
-- -   'GroupInServiceInstances'
--
-- -   'GroupPendingInstances'
--
-- -   'GroupStandbyInstances'
--
-- -   'GroupTerminatingInstances'
--
-- -   'GroupTotalInstances'
--
emMetric :: Lens' EnabledMetric (Maybe Text)
emMetric = lens _emMetric (\ s a -> s{_emMetric = a});

instance FromXML EnabledMetric where
        parseXML x
          = EnabledMetric' <$>
              (x .@? "Granularity") <*> (x .@? "Metric")

-- | Describes a filter.
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
    { _fValues :: !(Maybe [Text])
    , _fName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fValues'
--
-- * 'fName'
filter'
    :: Text -- ^ 'fName'
    -> Filter
filter' pName_ =
    Filter'
    { _fValues = Nothing
    , _fName = pName_
    }

-- | The value of the filter.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Default . _Coerce;

-- | The name of the filter. The valid values are: '\"auto-scaling-group\"',
-- '\"key\"', '\"value\"', and '\"propagate-at-launch\"'.
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a});

instance ToQuery Filter where
        toQuery Filter'{..}
          = mconcat
              ["Values" =:
                 toQuery (toQueryList "member" <$> _fValues),
               "Name" =: _fName]

-- | Describes an EC2 instance.
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
    { _iLaunchConfigurationName :: !(Maybe Text)
    , _iInstanceId              :: !Text
    , _iAvailabilityZone        :: !Text
    , _iLifecycleState          :: !LifecycleState
    , _iHealthStatus            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iLaunchConfigurationName'
--
-- * 'iInstanceId'
--
-- * 'iAvailabilityZone'
--
-- * 'iLifecycleState'
--
-- * 'iHealthStatus'
instance'
    :: Text -- ^ 'iInstanceId'
    -> Text -- ^ 'iAvailabilityZone'
    -> LifecycleState -- ^ 'iLifecycleState'
    -> Text -- ^ 'iHealthStatus'
    -> Instance
instance' pInstanceId_ pAvailabilityZone_ pLifecycleState_ pHealthStatus_ =
    Instance'
    { _iLaunchConfigurationName = Nothing
    , _iInstanceId = pInstanceId_
    , _iAvailabilityZone = pAvailabilityZone_
    , _iLifecycleState = pLifecycleState_
    , _iHealthStatus = pHealthStatus_
    }

-- | The launch configuration associated with the instance.
iLaunchConfigurationName :: Lens' Instance (Maybe Text)
iLaunchConfigurationName = lens _iLaunchConfigurationName (\ s a -> s{_iLaunchConfigurationName = a});

-- | The ID of the instance.
iInstanceId :: Lens' Instance Text
iInstanceId = lens _iInstanceId (\ s a -> s{_iInstanceId = a});

-- | The Availability Zone in which the instance is running.
iAvailabilityZone :: Lens' Instance Text
iAvailabilityZone = lens _iAvailabilityZone (\ s a -> s{_iAvailabilityZone = a});

-- | A description of the current lifecycle state. Note that the
-- 'Quarantined' state is not used.
iLifecycleState :: Lens' Instance LifecycleState
iLifecycleState = lens _iLifecycleState (\ s a -> s{_iLifecycleState = a});

-- | The health status of the instance.
iHealthStatus :: Lens' Instance Text
iHealthStatus = lens _iHealthStatus (\ s a -> s{_iHealthStatus = a});

instance FromXML Instance where
        parseXML x
          = Instance' <$>
              (x .@? "LaunchConfigurationName") <*>
                (x .@ "InstanceId")
                <*> (x .@ "AvailabilityZone")
                <*> (x .@ "LifecycleState")
                <*> (x .@ "HealthStatus")

-- | Describes whether instance monitoring is enabled.
--
-- /See:/ 'instanceMonitoring' smart constructor.
newtype InstanceMonitoring = InstanceMonitoring'
    { _imEnabled :: Maybe Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imEnabled'
instanceMonitoring
    :: InstanceMonitoring
instanceMonitoring =
    InstanceMonitoring'
    { _imEnabled = Nothing
    }

-- | If 'True', instance monitoring is enabled.
imEnabled :: Lens' InstanceMonitoring (Maybe Bool)
imEnabled = lens _imEnabled (\ s a -> s{_imEnabled = a});

instance FromXML InstanceMonitoring where
        parseXML x
          = InstanceMonitoring' <$> (x .@? "Enabled")

instance ToQuery InstanceMonitoring where
        toQuery InstanceMonitoring'{..}
          = mconcat ["Enabled" =: _imEnabled]

-- | Describes a launch configuration.
--
-- /See:/ 'launchConfiguration' smart constructor.
data LaunchConfiguration = LaunchConfiguration'
    { _lcSecurityGroups               :: !(Maybe [Text])
    , _lcAssociatePublicIPAddress     :: !(Maybe Bool)
    , _lcInstanceMonitoring           :: !(Maybe InstanceMonitoring)
    , _lcSpotPrice                    :: !(Maybe Text)
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcSecurityGroups'
--
-- * 'lcAssociatePublicIPAddress'
--
-- * 'lcInstanceMonitoring'
--
-- * 'lcSpotPrice'
--
-- * 'lcKeyName'
--
-- * 'lcClassicLinkVPCSecurityGroups'
--
-- * 'lcRAMDiskId'
--
-- * 'lcKernelId'
--
-- * 'lcEBSOptimized'
--
-- * 'lcUserData'
--
-- * 'lcClassicLinkVPCId'
--
-- * 'lcIAMInstanceProfile'
--
-- * 'lcLaunchConfigurationARN'
--
-- * 'lcPlacementTenancy'
--
-- * 'lcBlockDeviceMappings'
--
-- * 'lcLaunchConfigurationName'
--
-- * 'lcImageId'
--
-- * 'lcInstanceType'
--
-- * 'lcCreatedTime'
launchConfiguration
    :: Text -- ^ 'lcLaunchConfigurationName'
    -> Text -- ^ 'lcImageId'
    -> Text -- ^ 'lcInstanceType'
    -> UTCTime -- ^ 'lcCreatedTime'
    -> LaunchConfiguration
launchConfiguration pLaunchConfigurationName_ pImageId_ pInstanceType_ pCreatedTime_ =
    LaunchConfiguration'
    { _lcSecurityGroups = Nothing
    , _lcAssociatePublicIPAddress = Nothing
    , _lcInstanceMonitoring = Nothing
    , _lcSpotPrice = Nothing
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

-- | The security groups to associate with the instances.
lcSecurityGroups :: Lens' LaunchConfiguration [Text]
lcSecurityGroups = lens _lcSecurityGroups (\ s a -> s{_lcSecurityGroups = a}) . _Default . _Coerce;

-- | Specifies whether the instances are associated with a public IP address
-- ('true') or not ('false').
lcAssociatePublicIPAddress :: Lens' LaunchConfiguration (Maybe Bool)
lcAssociatePublicIPAddress = lens _lcAssociatePublicIPAddress (\ s a -> s{_lcAssociatePublicIPAddress = a});

-- | Controls whether instances in this group are launched with detailed
-- monitoring.
lcInstanceMonitoring :: Lens' LaunchConfiguration (Maybe InstanceMonitoring)
lcInstanceMonitoring = lens _lcInstanceMonitoring (\ s a -> s{_lcInstanceMonitoring = a});

-- | The price to bid when launching Spot Instances.
lcSpotPrice :: Lens' LaunchConfiguration (Maybe Text)
lcSpotPrice = lens _lcSpotPrice (\ s a -> s{_lcSpotPrice = a});

-- | The name of the key pair.
lcKeyName :: Lens' LaunchConfiguration (Maybe Text)
lcKeyName = lens _lcKeyName (\ s a -> s{_lcKeyName = a});

-- | The IDs of one or more security groups for the VPC specified in
-- 'ClassicLinkVPCId'. This parameter is required if 'ClassicLinkVPCId' is
-- specified, and cannot be used otherwise. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
lcClassicLinkVPCSecurityGroups :: Lens' LaunchConfiguration [Text]
lcClassicLinkVPCSecurityGroups = lens _lcClassicLinkVPCSecurityGroups (\ s a -> s{_lcClassicLinkVPCSecurityGroups = a}) . _Default . _Coerce;

-- | The ID of the RAM disk associated with the AMI.
lcRAMDiskId :: Lens' LaunchConfiguration (Maybe Text)
lcRAMDiskId = lens _lcRAMDiskId (\ s a -> s{_lcRAMDiskId = a});

-- | The ID of the kernel associated with the AMI.
lcKernelId :: Lens' LaunchConfiguration (Maybe Text)
lcKernelId = lens _lcKernelId (\ s a -> s{_lcKernelId = a});

-- | Controls whether the instance is optimized for EBS I\/O ('true') or not
-- ('false').
lcEBSOptimized :: Lens' LaunchConfiguration (Maybe Bool)
lcEBSOptimized = lens _lcEBSOptimized (\ s a -> s{_lcEBSOptimized = a});

-- | The user data available to the instances.
lcUserData :: Lens' LaunchConfiguration (Maybe Text)
lcUserData = lens _lcUserData (\ s a -> s{_lcUserData = a});

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances
-- to. This parameter can only be used if you are launching EC2-Classic
-- instances. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
lcClassicLinkVPCId :: Lens' LaunchConfiguration (Maybe Text)
lcClassicLinkVPCId = lens _lcClassicLinkVPCId (\ s a -> s{_lcClassicLinkVPCId = a});

-- | The name or Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance.
lcIAMInstanceProfile :: Lens' LaunchConfiguration (Maybe Text)
lcIAMInstanceProfile = lens _lcIAMInstanceProfile (\ s a -> s{_lcIAMInstanceProfile = a});

-- | The Amazon Resource Name (ARN) of the launch configuration.
lcLaunchConfigurationARN :: Lens' LaunchConfiguration (Maybe Text)
lcLaunchConfigurationARN = lens _lcLaunchConfigurationARN (\ s a -> s{_lcLaunchConfigurationARN = a});

-- | The tenancy of the instance, either 'default' or 'dedicated'. An
-- instance with 'dedicated' tenancy runs in an isolated, single-tenant
-- hardware and can only be launched into a VPC.
lcPlacementTenancy :: Lens' LaunchConfiguration (Maybe Text)
lcPlacementTenancy = lens _lcPlacementTenancy (\ s a -> s{_lcPlacementTenancy = a});

-- | A block device mapping, which specifies the block devices for the
-- instance.
lcBlockDeviceMappings :: Lens' LaunchConfiguration [BlockDeviceMapping]
lcBlockDeviceMappings = lens _lcBlockDeviceMappings (\ s a -> s{_lcBlockDeviceMappings = a}) . _Default . _Coerce;

-- | The name of the launch configuration.
lcLaunchConfigurationName :: Lens' LaunchConfiguration Text
lcLaunchConfigurationName = lens _lcLaunchConfigurationName (\ s a -> s{_lcLaunchConfigurationName = a});

-- | The ID of the Amazon Machine Image (AMI).
lcImageId :: Lens' LaunchConfiguration Text
lcImageId = lens _lcImageId (\ s a -> s{_lcImageId = a});

-- | The instance type for the instances.
lcInstanceType :: Lens' LaunchConfiguration Text
lcInstanceType = lens _lcInstanceType (\ s a -> s{_lcInstanceType = a});

-- | The creation date and time for the launch configuration.
lcCreatedTime :: Lens' LaunchConfiguration UTCTime
lcCreatedTime = lens _lcCreatedTime (\ s a -> s{_lcCreatedTime = a}) . _Time;

instance FromXML LaunchConfiguration where
        parseXML x
          = LaunchConfiguration' <$>
              (x .@? "SecurityGroups" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "AssociatePublicIpAddress")
                <*> (x .@? "InstanceMonitoring")
                <*> (x .@? "SpotPrice")
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

-- | Describes a lifecycle hook, which tells Auto Scaling that you want to
-- perform an action when an instance launches or terminates. When you have
-- a lifecycle hook in place, the Auto Scaling group will either:
--
-- -   Pause the instance after it launches, but before it is put into
--     service
-- -   Pause the instance as it terminates, but before it is fully
--     terminated
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingPendingState.html Auto Scaling Pending State>
-- and
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingTerminatingState.html Auto Scaling Terminating State>
-- in the /Auto Scaling Developer Guide/.
--
-- /See:/ 'lifecycleHook' smart constructor.
data LifecycleHook = LifecycleHook'
    { _lhDefaultResult         :: !(Maybe Text)
    , _lhLifecycleHookName     :: !(Maybe Text)
    , _lhHeartbeatTimeout      :: !(Maybe Int)
    , _lhAutoScalingGroupName  :: !(Maybe Text)
    , _lhNotificationMetadata  :: !(Maybe Text)
    , _lhGlobalTimeout         :: !(Maybe Int)
    , _lhRoleARN               :: !(Maybe Text)
    , _lhLifecycleTransition   :: !(Maybe Text)
    , _lhNotificationTargetARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LifecycleHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhDefaultResult'
--
-- * 'lhLifecycleHookName'
--
-- * 'lhHeartbeatTimeout'
--
-- * 'lhAutoScalingGroupName'
--
-- * 'lhNotificationMetadata'
--
-- * 'lhGlobalTimeout'
--
-- * 'lhRoleARN'
--
-- * 'lhLifecycleTransition'
--
-- * 'lhNotificationTargetARN'
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
    , _lhRoleARN = Nothing
    , _lhLifecycleTransition = Nothing
    , _lhNotificationTargetARN = Nothing
    }

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The valid
-- values are 'CONTINUE' and 'ABANDON'. The default value is 'CONTINUE'.
lhDefaultResult :: Lens' LifecycleHook (Maybe Text)
lhDefaultResult = lens _lhDefaultResult (\ s a -> s{_lhDefaultResult = a});

-- | The name of the lifecycle hook.
lhLifecycleHookName :: Lens' LifecycleHook (Maybe Text)
lhLifecycleHookName = lens _lhLifecycleHookName (\ s a -> s{_lhLifecycleHookName = a});

-- | The amount of time that can elapse before the lifecycle hook times out.
-- When the lifecycle hook times out, Auto Scaling performs the action
-- defined in the 'DefaultResult' parameter. You can prevent the lifecycle
-- hook from timing out by calling RecordLifecycleActionHeartbeat.
lhHeartbeatTimeout :: Lens' LifecycleHook (Maybe Int)
lhHeartbeatTimeout = lens _lhHeartbeatTimeout (\ s a -> s{_lhHeartbeatTimeout = a});

-- | The name of the Auto Scaling group for the lifecycle hook.
lhAutoScalingGroupName :: Lens' LifecycleHook (Maybe Text)
lhAutoScalingGroupName = lens _lhAutoScalingGroupName (\ s a -> s{_lhAutoScalingGroupName = a});

-- | Additional information that you want to include any time Auto Scaling
-- sends a message to the notification target.
lhNotificationMetadata :: Lens' LifecycleHook (Maybe Text)
lhNotificationMetadata = lens _lhNotificationMetadata (\ s a -> s{_lhNotificationMetadata = a});

-- | The maximum length of time an instance can remain in a 'Pending:Wait' or
-- 'Terminating:Wait' state. Currently, the maximum is set to 48 hours.
lhGlobalTimeout :: Lens' LifecycleHook (Maybe Int)
lhGlobalTimeout = lens _lhGlobalTimeout (\ s a -> s{_lhGlobalTimeout = a});

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target.
lhRoleARN :: Lens' LifecycleHook (Maybe Text)
lhRoleARN = lens _lhRoleARN (\ s a -> s{_lhRoleARN = a});

-- | The state of the EC2 instance to which you want to attach the lifecycle
-- hook. For a list of lifecycle hook types, see
-- DescribeLifecycleHookTypes.
lhLifecycleTransition :: Lens' LifecycleHook (Maybe Text)
lhLifecycleTransition = lens _lhLifecycleTransition (\ s a -> s{_lhLifecycleTransition = a});

-- | The ARN of the notification target that Auto Scaling uses to notify you
-- when an instance is in the transition state for the lifecycle hook. This
-- ARN target can be either an SQS queue or an SNS topic. The notification
-- message sent to the target includes the following:
--
-- -   Lifecycle action token
-- -   User account ID
-- -   Name of the Auto Scaling group
-- -   Lifecycle hook name
-- -   EC2 instance ID
-- -   Lifecycle transition
-- -   Notification metadata
lhNotificationTargetARN :: Lens' LifecycleHook (Maybe Text)
lhNotificationTargetARN = lens _lhNotificationTargetARN (\ s a -> s{_lhNotificationTargetARN = a});

instance FromXML LifecycleHook where
        parseXML x
          = LifecycleHook' <$>
              (x .@? "DefaultResult") <*>
                (x .@? "LifecycleHookName")
                <*> (x .@? "HeartbeatTimeout")
                <*> (x .@? "AutoScalingGroupName")
                <*> (x .@? "NotificationMetadata")
                <*> (x .@? "GlobalTimeout")
                <*> (x .@? "RoleARN")
                <*> (x .@? "LifecycleTransition")
                <*> (x .@? "NotificationTargetARN")

-- | Describes the state of a load balancer.
--
-- /See:/ 'loadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
    { _lbsState            :: !(Maybe Text)
    , _lbsLoadBalancerName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LoadBalancerState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbsState'
--
-- * 'lbsLoadBalancerName'
loadBalancerState
    :: LoadBalancerState
loadBalancerState =
    LoadBalancerState'
    { _lbsState = Nothing
    , _lbsLoadBalancerName = Nothing
    }

-- | The state of the load balancer.
--
-- -   'Adding' - The instances in the group are being registered with the
--     load balancer.
--
-- -   'Added' - All instances in the group are registered with the load
--     balancer.
--
-- -   'InService' - At least one instance in the group passed an ELB
--     health check.
--
-- -   'Removing' - The instances are being deregistered from the load
--     balancer. If connection draining is enabled, Elastic Load Balancing
--     waits for in-flight requests to complete before deregistering the
--     instances.
--
lbsState :: Lens' LoadBalancerState (Maybe Text)
lbsState = lens _lbsState (\ s a -> s{_lbsState = a});

-- | The name of the load balancer.
lbsLoadBalancerName :: Lens' LoadBalancerState (Maybe Text)
lbsLoadBalancerName = lens _lbsLoadBalancerName (\ s a -> s{_lbsLoadBalancerName = a});

instance FromXML LoadBalancerState where
        parseXML x
          = LoadBalancerState' <$>
              (x .@? "State") <*> (x .@? "LoadBalancerName")

-- | Describes a metric.
--
-- /See:/ 'metricCollectionType' smart constructor.
newtype MetricCollectionType = MetricCollectionType'
    { _mctMetric :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricCollectionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mctMetric'
metricCollectionType
    :: MetricCollectionType
metricCollectionType =
    MetricCollectionType'
    { _mctMetric = Nothing
    }

-- | The metric.
--
-- -   'GroupMinSize'
--
-- -   'GroupMaxSize'
--
-- -   'GroupDesiredCapacity'
--
-- -   'GroupInServiceInstances'
--
-- -   'GroupPendingInstances'
--
-- -   'GroupStandbyInstances'
--
-- -   'GroupTerminatingInstances'
--
-- -   'GroupTotalInstances'
--
mctMetric :: Lens' MetricCollectionType (Maybe Text)
mctMetric = lens _mctMetric (\ s a -> s{_mctMetric = a});

instance FromXML MetricCollectionType where
        parseXML x
          = MetricCollectionType' <$> (x .@? "Metric")

-- | Describes a granularity of a metric.
--
-- /See:/ 'metricGranularityType' smart constructor.
newtype MetricGranularityType = MetricGranularityType'
    { _mgtGranularity :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricGranularityType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mgtGranularity'
metricGranularityType
    :: MetricGranularityType
metricGranularityType =
    MetricGranularityType'
    { _mgtGranularity = Nothing
    }

-- | The granularity. The only valid value is '1Minute'.
mgtGranularity :: Lens' MetricGranularityType (Maybe Text)
mgtGranularity = lens _mgtGranularity (\ s a -> s{_mgtGranularity = a});

instance FromXML MetricGranularityType where
        parseXML x
          = MetricGranularityType' <$> (x .@? "Granularity")

-- | Describes a notification.
--
-- /See:/ 'notificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
    { _ncTopicARN             :: !(Maybe Text)
    , _ncAutoScalingGroupName :: !(Maybe Text)
    , _ncNotificationType     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncTopicARN'
--
-- * 'ncAutoScalingGroupName'
--
-- * 'ncNotificationType'
notificationConfiguration
    :: NotificationConfiguration
notificationConfiguration =
    NotificationConfiguration'
    { _ncTopicARN = Nothing
    , _ncAutoScalingGroupName = Nothing
    , _ncNotificationType = Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
ncTopicARN :: Lens' NotificationConfiguration (Maybe Text)
ncTopicARN = lens _ncTopicARN (\ s a -> s{_ncTopicARN = a});

-- | The name of the group.
ncAutoScalingGroupName :: Lens' NotificationConfiguration (Maybe Text)
ncAutoScalingGroupName = lens _ncAutoScalingGroupName (\ s a -> s{_ncAutoScalingGroupName = a});

-- | The types of events for an action to start.
--
-- -   'autoscaling:EC2_INSTANCE_LAUNCH'
--
-- -   'autoscaling:EC2_INSTANCE_LAUNCH_ERROR'
--
-- -   'autoscaling:EC2_INSTANCE_TERMINATE'
--
-- -   'autoscaling:EC2_INSTANCE_TERMINATE_ERROR'
--
-- -   'autoscaling:TEST_NOTIFICATION'
--
ncNotificationType :: Lens' NotificationConfiguration (Maybe Text)
ncNotificationType = lens _ncNotificationType (\ s a -> s{_ncNotificationType = a});

instance FromXML NotificationConfiguration where
        parseXML x
          = NotificationConfiguration' <$>
              (x .@? "TopicARN") <*> (x .@? "AutoScalingGroupName")
                <*> (x .@? "NotificationType")

-- | Describes a process type.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/US_SuspendResume.html#process-types Auto Scaling Processes>
-- in the /Auto Scaling Developer Guide/.
--
-- /See:/ 'processType' smart constructor.
newtype ProcessType = ProcessType'
    { _ptProcessName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProcessType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptProcessName'
processType
    :: Text -- ^ 'ptProcessName'
    -> ProcessType
processType pProcessName_ =
    ProcessType'
    { _ptProcessName = pProcessName_
    }

-- | The name of the process.
--
-- -   'Launch'
--
-- -   'Terminate'
--
-- -   'AddToLoadBalancer'
--
-- -   'AlarmNotification'
--
-- -   'AZRebalance'
--
-- -   'HealthCheck'
--
-- -   'ReplaceUnhealthy'
--
-- -   'ScheduledActions'
--
ptProcessName :: Lens' ProcessType Text
ptProcessName = lens _ptProcessName (\ s a -> s{_ptProcessName = a});

instance FromXML ProcessType where
        parseXML x = ProcessType' <$> (x .@ "ProcessName")

-- | Describes a scaling policy.
--
-- /See:/ 'scalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
    { _sEstimatedInstanceWarmup :: !(Maybe Int)
    , _sMinAdjustmentStep       :: !(Maybe Int)
    , _sPolicyName              :: !(Maybe Text)
    , _sPolicyType              :: !(Maybe Text)
    , _sStepAdjustments         :: !(Maybe [StepAdjustment])
    , _sAdjustmentType          :: !(Maybe Text)
    , _sScalingAdjustment       :: !(Maybe Int)
    , _sAutoScalingGroupName    :: !(Maybe Text)
    , _sCooldown                :: !(Maybe Int)
    , _sPolicyARN               :: !(Maybe Text)
    , _sAlarms                  :: !(Maybe [Alarm])
    , _sMetricAggregationType   :: !(Maybe Text)
    , _sMinAdjustmentMagnitude  :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sEstimatedInstanceWarmup'
--
-- * 'sMinAdjustmentStep'
--
-- * 'sPolicyName'
--
-- * 'sPolicyType'
--
-- * 'sStepAdjustments'
--
-- * 'sAdjustmentType'
--
-- * 'sScalingAdjustment'
--
-- * 'sAutoScalingGroupName'
--
-- * 'sCooldown'
--
-- * 'sPolicyARN'
--
-- * 'sAlarms'
--
-- * 'sMetricAggregationType'
--
-- * 'sMinAdjustmentMagnitude'
scalingPolicy
    :: ScalingPolicy
scalingPolicy =
    ScalingPolicy'
    { _sEstimatedInstanceWarmup = Nothing
    , _sMinAdjustmentStep = Nothing
    , _sPolicyName = Nothing
    , _sPolicyType = Nothing
    , _sStepAdjustments = Nothing
    , _sAdjustmentType = Nothing
    , _sScalingAdjustment = Nothing
    , _sAutoScalingGroupName = Nothing
    , _sCooldown = Nothing
    , _sPolicyARN = Nothing
    , _sAlarms = Nothing
    , _sMetricAggregationType = Nothing
    , _sMinAdjustmentMagnitude = Nothing
    }

-- | The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics.
sEstimatedInstanceWarmup :: Lens' ScalingPolicy (Maybe Int)
sEstimatedInstanceWarmup = lens _sEstimatedInstanceWarmup (\ s a -> s{_sEstimatedInstanceWarmup = a});

-- | Available for backward compatibility. Use 'MinAdjustmentMagnitude'
-- instead.
sMinAdjustmentStep :: Lens' ScalingPolicy (Maybe Int)
sMinAdjustmentStep = lens _sMinAdjustmentStep (\ s a -> s{_sMinAdjustmentStep = a});

-- | The name of the scaling policy.
sPolicyName :: Lens' ScalingPolicy (Maybe Text)
sPolicyName = lens _sPolicyName (\ s a -> s{_sPolicyName = a});

-- | The policy type. Valid values are 'SimpleScaling' and 'StepScaling'.
sPolicyType :: Lens' ScalingPolicy (Maybe Text)
sPolicyType = lens _sPolicyType (\ s a -> s{_sPolicyType = a});

-- | A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
sStepAdjustments :: Lens' ScalingPolicy [StepAdjustment]
sStepAdjustments = lens _sStepAdjustments (\ s a -> s{_sStepAdjustments = a}) . _Default . _Coerce;

-- | The adjustment type, which specifies how 'ScalingAdjustment' is
-- interpreted. Valid values are 'ChangeInCapacity', 'ExactCapacity', and
-- 'PercentChangeInCapacity'.
sAdjustmentType :: Lens' ScalingPolicy (Maybe Text)
sAdjustmentType = lens _sAdjustmentType (\ s a -> s{_sAdjustmentType = a});

-- | The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity.
sScalingAdjustment :: Lens' ScalingPolicy (Maybe Int)
sScalingAdjustment = lens _sScalingAdjustment (\ s a -> s{_sScalingAdjustment = a});

-- | The name of the Auto Scaling group associated with this scaling policy.
sAutoScalingGroupName :: Lens' ScalingPolicy (Maybe Text)
sAutoScalingGroupName = lens _sAutoScalingGroupName (\ s a -> s{_sAutoScalingGroupName = a});

-- | The amount of time, in seconds, after a scaling activity completes
-- before any further trigger-related scaling activities can start.
sCooldown :: Lens' ScalingPolicy (Maybe Int)
sCooldown = lens _sCooldown (\ s a -> s{_sCooldown = a});

-- | The Amazon Resource Name (ARN) of the policy.
sPolicyARN :: Lens' ScalingPolicy (Maybe Text)
sPolicyARN = lens _sPolicyARN (\ s a -> s{_sPolicyARN = a});

-- | The CloudWatch alarms related to the policy.
sAlarms :: Lens' ScalingPolicy [Alarm]
sAlarms = lens _sAlarms (\ s a -> s{_sAlarms = a}) . _Default . _Coerce;

-- | The aggregation type for the CloudWatch metrics. Valid values are
-- 'Minimum', 'Maximum', and 'Average'.
sMetricAggregationType :: Lens' ScalingPolicy (Maybe Text)
sMetricAggregationType = lens _sMetricAggregationType (\ s a -> s{_sMetricAggregationType = a});

-- | The minimum number of instances to scale. If the value of
-- 'AdjustmentType' is 'PercentChangeInCapacity', the scaling policy
-- changes the 'DesiredCapacity' of the Auto Scaling group by at least this
-- many instances. Otherwise, the error is 'ValidationError'.
sMinAdjustmentMagnitude :: Lens' ScalingPolicy (Maybe Int)
sMinAdjustmentMagnitude = lens _sMinAdjustmentMagnitude (\ s a -> s{_sMinAdjustmentMagnitude = a});

instance FromXML ScalingPolicy where
        parseXML x
          = ScalingPolicy' <$>
              (x .@? "EstimatedInstanceWarmup") <*>
                (x .@? "MinAdjustmentStep")
                <*> (x .@? "PolicyName")
                <*> (x .@? "PolicyType")
                <*>
                (x .@? "StepAdjustments" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "AdjustmentType")
                <*> (x .@? "ScalingAdjustment")
                <*> (x .@? "AutoScalingGroupName")
                <*> (x .@? "Cooldown")
                <*> (x .@? "PolicyARN")
                <*>
                (x .@? "Alarms" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "MetricAggregationType")
                <*> (x .@? "MinAdjustmentMagnitude")

-- | /See:/ 'scalingProcessQuery' smart constructor.
data ScalingProcessQuery = ScalingProcessQuery'
    { _spqScalingProcesses     :: !(Maybe [Text])
    , _spqAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScalingProcessQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spqScalingProcesses'
--
-- * 'spqAutoScalingGroupName'
scalingProcessQuery
    :: Text -- ^ 'spqAutoScalingGroupName'
    -> ScalingProcessQuery
scalingProcessQuery pAutoScalingGroupName_ =
    ScalingProcessQuery'
    { _spqScalingProcesses = Nothing
    , _spqAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more of the following processes:
--
-- -   'Launch'
--
-- -   'Terminate'
--
-- -   'HealthCheck'
--
-- -   'ReplaceUnhealthy'
--
-- -   'AZRebalance'
--
-- -   'AlarmNotification'
--
-- -   'ScheduledActions'
--
-- -   'AddToLoadBalancer'
--
spqScalingProcesses :: Lens' ScalingProcessQuery [Text]
spqScalingProcesses = lens _spqScalingProcesses (\ s a -> s{_spqScalingProcesses = a}) . _Default . _Coerce;

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
spqAutoScalingGroupName :: Lens' ScalingProcessQuery Text
spqAutoScalingGroupName = lens _spqAutoScalingGroupName (\ s a -> s{_spqAutoScalingGroupName = a});

instance ToQuery ScalingProcessQuery where
        toQuery ScalingProcessQuery'{..}
          = mconcat
              ["ScalingProcesses" =:
                 toQuery
                   (toQueryList "member" <$> _spqScalingProcesses),
               "AutoScalingGroupName" =: _spqAutoScalingGroupName]

-- | Describes a scheduled update to an Auto Scaling group.
--
-- /See:/ 'scheduledUpdateGroupAction' smart constructor.
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction'
    { _sugaScheduledActionARN   :: !(Maybe Text)
    , _sugaTime                 :: !(Maybe ISO8601)
    , _sugaStartTime            :: !(Maybe ISO8601)
    , _sugaScheduledActionName  :: !(Maybe Text)
    , _sugaMaxSize              :: !(Maybe Int)
    , _sugaDesiredCapacity      :: !(Maybe Int)
    , _sugaRecurrence           :: !(Maybe Text)
    , _sugaMinSize              :: !(Maybe Int)
    , _sugaEndTime              :: !(Maybe ISO8601)
    , _sugaAutoScalingGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScheduledUpdateGroupAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sugaScheduledActionARN'
--
-- * 'sugaTime'
--
-- * 'sugaStartTime'
--
-- * 'sugaScheduledActionName'
--
-- * 'sugaMaxSize'
--
-- * 'sugaDesiredCapacity'
--
-- * 'sugaRecurrence'
--
-- * 'sugaMinSize'
--
-- * 'sugaEndTime'
--
-- * 'sugaAutoScalingGroupName'
scheduledUpdateGroupAction
    :: ScheduledUpdateGroupAction
scheduledUpdateGroupAction =
    ScheduledUpdateGroupAction'
    { _sugaScheduledActionARN = Nothing
    , _sugaTime = Nothing
    , _sugaStartTime = Nothing
    , _sugaScheduledActionName = Nothing
    , _sugaMaxSize = Nothing
    , _sugaDesiredCapacity = Nothing
    , _sugaRecurrence = Nothing
    , _sugaMinSize = Nothing
    , _sugaEndTime = Nothing
    , _sugaAutoScalingGroupName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the scheduled action.
sugaScheduledActionARN :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionARN = lens _sugaScheduledActionARN (\ s a -> s{_sugaScheduledActionARN = a});

-- | This parameter is deprecated; use 'StartTime' instead.
sugaTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaTime = lens _sugaTime (\ s a -> s{_sugaTime = a}) . mapping _Time;

-- | The date and time that the action is scheduled to begin. This date and
-- time can be up to one month in the future.
--
-- When 'StartTime' and 'EndTime' are specified with 'Recurrence', they
-- form the boundaries of when the recurring action will start and stop.
sugaStartTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaStartTime = lens _sugaStartTime (\ s a -> s{_sugaStartTime = a}) . mapping _Time;

-- | The name of the scheduled action.
sugaScheduledActionName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionName = lens _sugaScheduledActionName (\ s a -> s{_sugaScheduledActionName = a});

-- | The maximum size of the group.
sugaMaxSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMaxSize = lens _sugaMaxSize (\ s a -> s{_sugaMaxSize = a});

-- | The number of instances you prefer to maintain in the group.
sugaDesiredCapacity :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaDesiredCapacity = lens _sugaDesiredCapacity (\ s a -> s{_sugaDesiredCapacity = a});

-- | The recurring schedule for the action.
sugaRecurrence :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaRecurrence = lens _sugaRecurrence (\ s a -> s{_sugaRecurrence = a});

-- | The minimum size of the group.
sugaMinSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMinSize = lens _sugaMinSize (\ s a -> s{_sugaMinSize = a});

-- | The date and time that the action is scheduled to end. This date and
-- time can be up to one month in the future.
sugaEndTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaEndTime = lens _sugaEndTime (\ s a -> s{_sugaEndTime = a}) . mapping _Time;

-- | The name of the group.
sugaAutoScalingGroupName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaAutoScalingGroupName = lens _sugaAutoScalingGroupName (\ s a -> s{_sugaAutoScalingGroupName = a});

instance FromXML ScheduledUpdateGroupAction where
        parseXML x
          = ScheduledUpdateGroupAction' <$>
              (x .@? "ScheduledActionARN") <*> (x .@? "Time") <*>
                (x .@? "StartTime")
                <*> (x .@? "ScheduledActionName")
                <*> (x .@? "MaxSize")
                <*> (x .@? "DesiredCapacity")
                <*> (x .@? "Recurrence")
                <*> (x .@? "MinSize")
                <*> (x .@? "EndTime")
                <*> (x .@? "AutoScalingGroupName")

-- | Describes an adjustment based on the difference between the value of the
-- aggregated CloudWatch metric and the breach threshold that you\'ve
-- defined for the alarm.
--
-- For the following examples, suppose that you have an alarm with a breach
-- threshold of 50:
--
-- -   If you want the adjustment to be triggered when the metric is
--     greater than or equal to 50 and less than 60, specify a lower bound
--     of 0 and an upper bound of 10.
--
-- -   If you want the adjustment to be triggered when the metric is
--     greater than 40 and less than or equal to 50, specify a lower bound
--     of -10 and an upper bound of 0.
--
-- There are a few rules for the step adjustments for your step policy:
--
-- -   The ranges of your step adjustments can\'t overlap or have a gap.
--
-- -   At most one step adjustment can have a null lower bound. If one step
--     adjustment has a negative lower bound, then there must be a step
--     adjustment with a null lower bound.
--
-- -   At most one step adjustment can have a null upper bound. If one step
--     adjustment has a positive upper bound, then there must be a step
--     adjustment with a null upper bound.
--
-- -   The upper and lower bound can\'t be null in the same step
--     adjustment.
--
--
-- /See:/ 'stepAdjustment' smart constructor.
data StepAdjustment = StepAdjustment'
    { _saMetricIntervalLowerBound :: !(Maybe Double)
    , _saMetricIntervalUpperBound :: !(Maybe Double)
    , _saScalingAdjustment        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StepAdjustment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saMetricIntervalLowerBound'
--
-- * 'saMetricIntervalUpperBound'
--
-- * 'saScalingAdjustment'
stepAdjustment
    :: Int -- ^ 'saScalingAdjustment'
    -> StepAdjustment
stepAdjustment pScalingAdjustment_ =
    StepAdjustment'
    { _saMetricIntervalLowerBound = Nothing
    , _saMetricIntervalUpperBound = Nothing
    , _saScalingAdjustment = pScalingAdjustment_
    }

-- | The lower bound for the difference between the alarm threshold and the
-- CloudWatch metric. If the metric value is above the breach threshold,
-- the lower bound is inclusive (the metric must be greater than or equal
-- to the threshold plus the lower bound). Otherwise, it is exclusive (the
-- metric must be greater than the threshold plus the lower bound). A null
-- value indicates negative infinity.
saMetricIntervalLowerBound :: Lens' StepAdjustment (Maybe Double)
saMetricIntervalLowerBound = lens _saMetricIntervalLowerBound (\ s a -> s{_saMetricIntervalLowerBound = a});

-- | The upper bound for the difference between the alarm threshold and the
-- CloudWatch metric. If the metric value is above the breach threshold,
-- the upper bound is exclusive (the metric must be less than the threshold
-- plus the upper bound). Otherwise, it is inclusive (the metric must be
-- less than or equal to the threshold plus the upper bound). A null value
-- indicates positive infinity.
--
-- The upper bound must be greater than the lower bound.
saMetricIntervalUpperBound :: Lens' StepAdjustment (Maybe Double)
saMetricIntervalUpperBound = lens _saMetricIntervalUpperBound (\ s a -> s{_saMetricIntervalUpperBound = a});

-- | The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity.
saScalingAdjustment :: Lens' StepAdjustment Int
saScalingAdjustment = lens _saScalingAdjustment (\ s a -> s{_saScalingAdjustment = a});

instance FromXML StepAdjustment where
        parseXML x
          = StepAdjustment' <$>
              (x .@? "MetricIntervalLowerBound") <*>
                (x .@? "MetricIntervalUpperBound")
                <*> (x .@ "ScalingAdjustment")

instance ToQuery StepAdjustment where
        toQuery StepAdjustment'{..}
          = mconcat
              ["MetricIntervalLowerBound" =:
                 _saMetricIntervalLowerBound,
               "MetricIntervalUpperBound" =:
                 _saMetricIntervalUpperBound,
               "ScalingAdjustment" =: _saScalingAdjustment]

-- | Describes an Auto Scaling process that has been suspended. For more
-- information, see ProcessType.
--
-- /See:/ 'suspendedProcess' smart constructor.
data SuspendedProcess = SuspendedProcess'
    { _spProcessName      :: !(Maybe Text)
    , _spSuspensionReason :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SuspendedProcess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spProcessName'
--
-- * 'spSuspensionReason'
suspendedProcess
    :: SuspendedProcess
suspendedProcess =
    SuspendedProcess'
    { _spProcessName = Nothing
    , _spSuspensionReason = Nothing
    }

-- | The name of the suspended process.
spProcessName :: Lens' SuspendedProcess (Maybe Text)
spProcessName = lens _spProcessName (\ s a -> s{_spProcessName = a});

-- | The reason that the process was suspended.
spSuspensionReason :: Lens' SuspendedProcess (Maybe Text)
spSuspensionReason = lens _spSuspensionReason (\ s a -> s{_spSuspensionReason = a});

instance FromXML SuspendedProcess where
        parseXML x
          = SuspendedProcess' <$>
              (x .@? "ProcessName") <*> (x .@? "SuspensionReason")

-- | Describes a tag for an Auto Scaling group.
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagKey               :: !Text
    , _tagResourceId        :: !Text
    , _tagResourceType      :: !Text
    , _tagPropagateAtLaunch :: !Bool
    , _tagValue             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey'
--
-- * 'tagResourceId'
--
-- * 'tagResourceType'
--
-- * 'tagPropagateAtLaunch'
--
-- * 'tagValue'
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
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

-- | The name of the group.
tagResourceId :: Lens' Tag Text
tagResourceId = lens _tagResourceId (\ s a -> s{_tagResourceId = a});

-- | The type of resource. The only supported value is 'auto-scaling-group'.
tagResourceType :: Lens' Tag Text
tagResourceType = lens _tagResourceType (\ s a -> s{_tagResourceType = a});

-- | Determines whether the tag is added to new instances as they are
-- launched in the group.
tagPropagateAtLaunch :: Lens' Tag Bool
tagPropagateAtLaunch = lens _tagPropagateAtLaunch (\ s a -> s{_tagPropagateAtLaunch = a});

-- | The tag value.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat
              ["Key" =: _tagKey, "ResourceId" =: _tagResourceId,
               "ResourceType" =: _tagResourceType,
               "PropagateAtLaunch" =: _tagPropagateAtLaunch,
               "Value" =: _tagValue]

-- | Describes a tag for an Auto Scaling group.
--
-- /See:/ 'tagDescription' smart constructor.
data TagDescription = TagDescription'
    { _tdResourceId        :: !Text
    , _tdResourceType      :: !Text
    , _tdKey               :: !Text
    , _tdPropagateAtLaunch :: !Bool
    , _tdValue             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdResourceId'
--
-- * 'tdResourceType'
--
-- * 'tdKey'
--
-- * 'tdPropagateAtLaunch'
--
-- * 'tdValue'
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
tdResourceId = lens _tdResourceId (\ s a -> s{_tdResourceId = a});

-- | The type of resource. The only supported value is 'auto-scaling-group'.
tdResourceType :: Lens' TagDescription Text
tdResourceType = lens _tdResourceType (\ s a -> s{_tdResourceType = a});

-- | The tag key.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\ s a -> s{_tdKey = a});

-- | Determines whether the tag is added to new instances as they are
-- launched in the group.
tdPropagateAtLaunch :: Lens' TagDescription Bool
tdPropagateAtLaunch = lens _tdPropagateAtLaunch (\ s a -> s{_tdPropagateAtLaunch = a});

-- | The tag value.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\ s a -> s{_tdValue = a});

instance FromXML TagDescription where
        parseXML x
          = TagDescription' <$>
              (x .@ "ResourceId") <*> (x .@ "ResourceType") <*>
                (x .@ "Key")
                <*> (x .@ "PropagateAtLaunch")
                <*> (x .@ "Value")
