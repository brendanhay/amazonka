{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      : Network.AWS.AutoScaling.V20110101.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.AutoScaling.V20110101.Types where

import Data.Aeson.TH
import Data.Text
import Data.Time
import Network.AWS.Internal

--
-- Common
--

data ResponseMetadata = ResponseMetadata
    { rmRequestId :: !Text
    } deriving (Show)

$(deriveJSON fieldOptions ''ResponseMetadata)

data ErrorType = Receiver | Sender
    deriving (Show)

$(deriveJSON fieldOptions ''ErrorType)

data Error = Error
    { eType :: !ErrorType
    , eCode :: !Text
    , eMessage :: !Text
    , eDetail :: !Text
    } deriving (Show)

$(deriveJSON fieldOptions ''Error)

newtype ResourceName = ResourceName Text
    deriving (Show, IsText)

$(deriveQS ''ResourceName)
$(deriveJSON fieldOptions ''ResourceName)

--
-- Types
--

-- Shouldnt need a FromXML since i've just aliased the MetadataResponse?
-- get as object then lookup ResponseMetadata as a key

data MetricGranularityType = MetricGranularityType
    { mgtGranularity :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''MetricGranularityType)

newtype MetricGranularityTypes = MetricGranularityTypes [MetricGranularityType]
   deriving (Show)

$(deriveJSON fieldOptions ''MetricGranularityTypes)

newtype LoadBalancerNames = LoadBalancerNames [Text]
    deriving (Show)

$(deriveQS' (++ ".member") ''LoadBalancerNames)
$(deriveJSON fieldOptions ''LoadBalancerNames)

data SuspendedProcess = SuspendedProcess
    { spProcessName :: !(Maybe Text)
    , spSuspensionReason :: !(Maybe Text)
    } deriving (Show)

$(deriveQS ''SuspendedProcess)
$(deriveJSON fieldOptions ''SuspendedProcess)

newtype SuspendedProcesses = SuspendedProcesses [SuspendedProcess]
    deriving (Show)

$(deriveJSON fieldOptions ''SuspendedProcesses)

data Tag = Tag
    { tResourceId :: !(Maybe Text)
    , tResourceType :: !(Maybe Text)
    , tKey :: !Text
    , tValue :: !(Maybe Text)
    , tPropagateAtLaunch :: !(Maybe Bool)
    } deriving (Show)

$(deriveQS ''Tag)

newtype Tags = Tags [Tag]
    deriving (Show)

$(deriveQS' (++ ".member") ''Tags)

-- data Tags = Tags
--     { tMember :: ![Tag]
--     } deriving (Show)

newtype PolicyNames = PolicyNames [ResourceName]
   deriving (Show)

$(deriveQS' (++ ".member") ''PolicyNames)

data NotificationConfiguration = NotificationConfiguration
    { ncAutoScalingGroupName :: !(Maybe ResourceName)
    , ncTopicARN :: !(Maybe ResourceName)
    , ncNotificationType :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''NotificationConfiguration)

newtype NotificationConfigurations = NotificationConfigurations [NotificationConfiguration]
   deriving (Show)

$(deriveJSON fieldOptions ''NotificationConfigurations)

newtype ScheduledActionNames = ScheduledActionNames [ResourceName]
   deriving (Show)

$(deriveQS' (++ ".member") ''ScheduledActionNames)

data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { sugaAutoScalingGroupName :: !(Maybe Text)
    , sugaScheduledActionName :: !(Maybe Text)
    , sugaScheduledActionARN :: !(Maybe ResourceName)
    , sugaTime :: !(Maybe UTCTime)
    , sugaStartTime :: !(Maybe UTCTime)
    , sugaEndTime :: !(Maybe UTCTime)
    , sugaRecurrence :: !(Maybe Text)
    , sugaMinSize :: !(Maybe Integer)
    , sugaMaxSize :: !(Maybe Integer)
    , sugaDesiredCapacity :: !(Maybe Integer)
    } deriving (Show)

$(deriveJSON fieldOptions ''ScheduledUpdateGroupAction)

newtype ScheduledUpdateGroupActions = ScheduledUpdateGroupActions [ScheduledUpdateGroupAction]
   deriving (Show)

$(deriveJSON fieldOptions ''ScheduledUpdateGroupActions)

newtype SecurityGroups = SecurityGroups [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''SecurityGroups)
$(deriveJSON fieldOptions ''SecurityGroups)

data Ebs = Ebs
    { eSnapshotId :: !(Maybe Text)
    , eVolumeSize :: !(Maybe Integer)
    } deriving (Show)

$(deriveQS ''Ebs)
$(deriveJSON fieldOptions ''Ebs)

data BlockDeviceMapping = BlockDeviceMapping
    { bdmVirtualName :: !(Maybe Text)
    , bdmDeviceName :: !Text
    , bdmEbs :: !(Maybe Ebs)
    } deriving (Show)

$(deriveQS ''BlockDeviceMapping)
$(deriveJSON fieldOptions ''BlockDeviceMapping)

newtype BlockDeviceMappings = BlockDeviceMappings [BlockDeviceMapping]
   deriving (Show)

$(deriveQS' (++ ".member") ''BlockDeviceMappings)
$(deriveJSON fieldOptions ''BlockDeviceMappings)

data InstanceMonitoring = InstanceMonitoring
    { imEnabled :: !(Maybe Bool)
    } deriving (Show)

$(deriveQS ''InstanceMonitoring)
$(deriveJSON fieldOptions ''InstanceMonitoring)

newtype LaunchConfigurationNames = LaunchConfigurationNames [ResourceName]
   deriving (Show)

$(deriveQS' (++ ".member") ''LaunchConfigurationNames)
$(deriveJSON fieldOptions ''LaunchConfigurationNames)

data LaunchConfiguration = LaunchConfiguration
    { lcLaunchConfigurationName :: !Text
    , lcLaunchConfigurationARN :: !(Maybe ResourceName)
    , lcImageId :: !Text
    , lcKeyName :: !(Maybe Text)
    , lcSecurityGroups :: !(Maybe SecurityGroups)
    , lcUserData :: !(Maybe Text)
    , lcInstanceType :: !Text
    , lcKernelId :: !(Maybe Text)
    , lcRamdiskId :: !(Maybe Text)
    , lcBlockDeviceMappings :: !(Maybe BlockDeviceMappings)
    , lcInstanceMonitoring :: !(Maybe InstanceMonitoring)
    , lcSpotPrice :: !(Maybe Text)
    , lcIamInstanceProfile :: !(Maybe Text)
    , lcCreatedTime :: !UTCTime
    , lcEbsOptimized :: !(Maybe Bool)
    } deriving (Show)

$(deriveQS ''LaunchConfiguration)
$(deriveJSON fieldOptions ''LaunchConfiguration)

newtype LaunchConfigurations = LaunchConfigurations [LaunchConfiguration]
   deriving (Show)

$(deriveQS' (++ ".member") ''LaunchConfigurations)
$(deriveJSON fieldOptions ''LaunchConfigurations)

newtype AvailabilityZones = AvailabilityZones [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''AvailabilityZones)
$(deriveJSON fieldOptions ''AvailabilityZones)

data EnabledMetric = EnabledMetric
    { emMetric :: !(Maybe Text)
    , emGranularity :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''EnabledMetric)

newtype EnabledMetrics = EnabledMetrics [EnabledMetric]
   deriving (Show)

$(deriveJSON fieldOptions ''EnabledMetrics)

data TagDescription = TagDescription
    { tdResourceId :: !(Maybe Text)
    , tdResourceType :: !(Maybe Text)
    , tdKey :: !(Maybe Text)
    , tdValue :: !(Maybe Text)
    , tdPropagateAtLaunch :: !(Maybe Bool)
    } deriving (Show)

$(deriveQS ''TagDescription)
$(deriveJSON fieldOptions ''TagDescription)

newtype TagDescriptionList = TagDescriptionList [TagDescription]
    deriving (Show)

$(deriveQS' (++ ".member") ''TagDescriptionList)
$(deriveJSON fieldOptions ''TagDescriptionList)

newtype TerminationPolicies = TerminationPolicies [Text]
    deriving (Show)

$(deriveQS' (++ ".member") ''TerminationPolicies)
$(deriveJSON fieldOptions ''TerminationPolicies)

data LifecycleState
    = Pending
    | Quarantined
    | InService
    | Terminating
    | Terminated
      deriving (Show)

$(deriveJSON defaultOptions ''LifecycleState)

data Instance = Instance
    { iInstanceId :: !Text
    , iAvailabilityZone :: !Text
    , iLifecycleState :: !LifecycleState
    , iHealthStatus :: !Text
    , iLaunchConfigurationName :: !Text
    } deriving (Show)

$(deriveJSON fieldOptions ''Instance)

newtype Instances = Instances [Instance]
   deriving (Show)

$(deriveJSON fieldOptions ''Instances)

data AutoScalingGroup = AutoScalingGroup
    { asgAutoScalingGroupName :: !Text
    , asgAutoScalingGroupARN :: !(Maybe ResourceName)
    , asgLaunchConfigurationName :: !Text
    , asgMinSize :: !Integer
    , asgMaxSize :: !Integer
    , asgDesiredCapacity :: !Integer
    , asgDefaultCooldown :: !Integer
    , asgAvailabilityZones :: !AvailabilityZones
    , asgLoadBalancerNames :: !(Maybe LoadBalancerNames)
    , asgHealthCheckType :: !Text
    , asgHealthCheckGracePeriod :: !(Maybe Integer)
    , asgInstances :: !(Maybe Instances)
    , asgCreatedTime :: !UTCTime
    , asgSuspendedProcesses :: !SuspendedProcesses
    , asgPlacementGroup :: !(Maybe Text)
    , asgVPCZoneIdentifier :: !(Maybe Text)
    , asgEnabledMetrics :: !(Maybe EnabledMetrics)
    , asgStatus :: !(Maybe Text)
    , asgTags :: !(Maybe TagDescriptionList)
    , asgTerminationPolicies :: !TerminationPolicies
    } deriving (Show)

$(deriveJSON fieldOptions ''AutoScalingGroup)

data MetricCollectionType = MetricCollectionType
    { mctMetric :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''MetricCollectionType)

newtype MetricCollectionTypes = MetricCollectionTypes [MetricCollectionType]
   deriving (Show)

$(deriveJSON fieldOptions ''MetricCollectionTypes)

newtype AutoScalingNotificationTypes = AutoScalingNotificationTypes [Text]
    deriving (Show)

$(deriveQS' (++ ".member") ''AutoScalingNotificationTypes)
$(deriveJSON fieldOptions ''AutoScalingNotificationTypes)

data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { asidInstanceId :: !Text
    , asidAutoScalingGroupName :: !Text
    , asidAvailabilityZone :: !Text
    , asidLifecycleState :: !Text
    , asidHealthStatus :: !Text
    , asidLaunchConfigurationName :: !Text
    } deriving (Show)

$(deriveJSON fieldOptions ''AutoScalingInstanceDetails)

newtype AutoScalingInstances = AutoScalingInstances [AutoScalingInstanceDetails]
   deriving (Show)

$(deriveJSON fieldOptions ''AutoScalingInstances)

newtype Metrics = Metrics [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''Metrics)

data ScalingActivityStatusCode
    = WaitingForSpotInstanceRequestId
    | WaitingForSpotInstanceId
    | WaitingForInstanceId
    | PreInService
    | InProgress
    | Successful
    | Failed
    | Cancelled
      deriving (Show)

$(deriveJSON fieldOptions ''ScalingActivityStatusCode)

data Activity = Activity
    { aActivityId :: !Text
    , aAutoScalingGroupName :: !Text
    , aDescription :: !(Maybe Text)
    , aCause :: !Text
    , aStartTime :: !UTCTime
    , aEndTime :: !(Maybe UTCTime)
    , aStatusCode :: !ScalingActivityStatusCode
    , aStatusMessage :: !(Maybe Text)
    , aProgress :: !(Maybe Integer)
    , aDetails :: !(Maybe Text)
    } deriving (Show)

$(deriveJSON fieldOptions ''Activity)

newtype Activities = Activities [Activity]
   deriving (Show)

$(deriveJSON fieldOptions ''Activities)

newtype ActivityIds = ActivityIds [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''ActivityIds)

data Alarm = Alarm
    { aAlarmName :: !(Maybe Text)
    , aAlarmARN :: !(Maybe ResourceName)
    } deriving (Show)

$(deriveJSON fieldOptions ''Alarm)

newtype Alarms = Alarms [Alarm]
   deriving (Show)

$(deriveJSON fieldOptions ''Alarms)

data ScalingPolicy = ScalingPolicy
    { spAutoScalingGroupName :: !(Maybe Text)
    , spPolicyName :: !(Maybe Text)
    , spScalingAdjustment :: !(Maybe Integer)
    , spAdjustmentType :: !(Maybe Text)
    , spCooldown :: !(Maybe Integer)
    , spPolicyARN :: !(Maybe ResourceName)
    , spAlarms :: ![Alarms]
    , spMinAdjustmentStep :: !(Maybe Integer)
    } deriving (Show)

$(deriveJSON fieldOptions ''ScalingPolicy)

newtype ScalingPolicies = ScalingPolicies [ScalingPolicy]
   deriving (Show)

$(deriveJSON fieldOptions ''ScalingPolicies)

newtype Values = Values [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''Values)

data Filter = Filter
    { fName :: !(Maybe Text)
    , fValues :: !(Maybe Values)
    } deriving (Show)

$(deriveQS ''Filter)

newtype Filters = Filters [Filter]
   deriving (Show)

$(deriveQS' (++ ".member") ''Filters)

data ProcessType = ProcessType
    { ptProcessName :: !Text
    } deriving (Show)

$(deriveJSON fieldOptions ''ProcessType)

newtype Processes = Processes [ProcessType]
   deriving (Show)

$(deriveJSON fieldOptions ''Processes)

newtype InstanceIds = InstanceIds [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''InstanceIds)

newtype AutoScalingGroupNames = AutoScalingGroupNames [ResourceName]
   deriving (Show)

$(deriveQS' (++ ".member") ''AutoScalingGroupNames)

newtype AutoScalingGroups = AutoScalingGroups [AutoScalingGroup]
   deriving (Show)

$(deriveJSON fieldOptions ''AutoScalingGroups)

newtype ProcessNames = ProcessNames [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''ProcessNames)

newtype AdjustmentTypes = AdjustmentTypes [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''AdjustmentTypes)
$(deriveJSON fieldOptions ''AdjustmentTypes)
