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

--
-- Types
--

-- Shouldnt need a FromXML since i've just aliased the MetadataResponse?
-- get as object then lookup ResponseMetadata as a key

newtype AutoScalingNotificationTypes = AutoScalingNotificationTypes [Text]
    deriving (Show)

$(deriveQS' (++ ".member") ''AutoScalingNotificationTypes)

data MetricGranularityType = MetricGranularityType
    { mgtGranularity :: !(Maybe Text)
    } deriving (Show)

newtype LoadBalancerNames = LoadBalancerNames [Text]
    deriving (Show)

$(deriveQS' (++ ".member") ''LoadBalancerNames)

data SuspendedProcess = SuspendedProcess
    { spProcessName :: !(Maybe Text)
    , spSuspensionReason :: !(Maybe Text)
    } deriving (Show)

$(deriveQS ''SuspendedProcess)

newtype SuspendedProcesses = SuspendedProcesses [SuspendedProcess]
    deriving (Show)

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

newtype ScheduledUpdateGroupActions = ScheduledUpdateGroupActions [ScheduledUpdateGroupAction]
   deriving (Show)

newtype NotificationConfigurations = NotificationConfigurations [NotificationConfiguration]
   deriving (Show)

newtype SecurityGroups = SecurityGroups [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''SecurityGroups)

data Ebs = Ebs
    { eSnapshotId :: !(Maybe Text)
    , eVolumeSize :: !(Maybe Integer)
    } deriving (Show)

$(deriveQS ''Ebs)

data BlockDeviceMapping = BlockDeviceMapping
    { bdmVirtualName :: !(Maybe Text)
    , bdmDeviceName :: !Text
    , bdmEbs :: !(Maybe Ebs)
    } deriving (Show)

$(deriveQS ''BlockDeviceMapping)

newtype BlockDeviceMappings = BlockDeviceMappings [BlockDeviceMapping]
   deriving (Show)

$(deriveQS' (++ ".member") ''BlockDeviceMappings)

data InstanceMonitoring = InstanceMonitoring
    { imEnabled :: !(Maybe Bool)
    } deriving (Show)

$(deriveQS ''InstanceMonitoring)

newtype LaunchConfigurationNames = LaunchConfigurationNames [ResourceName]
   deriving (Show)

$(deriveQS' (++ ".member") ''LaunchConfigurationNames)

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

newtype LaunchConfigurations = LaunchConfigurations [LaunchConfiguration]
   deriving (Show)

$(deriveQS' (++ ".member") ''LaunchConfigurations)

newtype AvailabilityZones = AvailabilityZones [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''AvailabilityZones)

data EnabledMetric = EnabledMetric
    { emMetric :: !(Maybe Text)
    , emGranularity :: !(Maybe Text)
    } deriving (Show)

newtype EnabledMetrics = EnabledMetrics [EnabledMetric]
   deriving (Show)

data TagDescription = TagDescription
    { tdResourceId :: !(Maybe Text)
    , tdResourceType :: !(Maybe Text)
    , tdKey :: !(Maybe Text)
    , tdValue :: !(Maybe Text)
    , tdPropagateAtLaunch :: !(Maybe Bool)
    } deriving (Show)

$(deriveQS ''TagDescription)

newtype TagDescriptionList = TagDescriptionList [TagDescription]
    deriving (Show)

$(deriveQS' (++ ".member") ''TagDescriptionList)

newtype TerminationPolicies = TerminationPolicies [Text]
    deriving (Show)

$(deriveQS' (++ ".member") ''TerminationPolicies)

data Instance = Instance
    { iInstanceId :: !Text
    , iAvailabilityZone :: !Text
    , iLifecycleState :: !LifecycleState
    , iHealthStatus :: !Text
    , iLaunchConfigurationName :: !Text
    } deriving (Show)

newtype Instances = Instances [Instance]
   deriving (Show)

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

data LifecycleState
    = Pending
    | Quarantined
    | InService
    | Terminating
    | Terminated
      deriving (Show)

data MetricCollectionType = MetricCollectionType
    { mctMetric :: !(Maybe Text)
    } deriving (Show)

newtype MetricCollectionTypes = MetricCollectionTypes [MetricCollectionType]
   deriving (Show)

data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { asidInstanceId :: !Text
    , asidAutoScalingGroupName :: !Text
    , asidAvailabilityZone :: !Text
    , asidLifecycleState :: !Text
    , asidHealthStatus :: !Text
    , asidLaunchConfigurationName :: !Text
    } deriving (Show)

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

$(deriveXML ''ScalingActivityStatusCode)

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

$(deriveXML ''Activity)

data Alarm = Alarm
    { aAlarmName :: !(Maybe Text)
    , aAlarmARN :: !(Maybe ResourceName)
    } deriving (Show)

newtype Alarms = Alarms [Alarm]
   deriving (Show)

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

newtype AutoScalingInstances = AutoScalingInstances [AutoScalingInstanceDetails]
   deriving (Show)

data ProcessType = ProcessType
    { ptProcessName :: !Text
    } deriving (Show)

newtype Processes = Processes [ProcessType]
   deriving (Show)

newtype InstanceIds = InstanceIds [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''InstanceIds)

newtype MetricGranularityTypes = MetricGranularityTypes [MetricGranularityType]
   deriving (Show)

newtype AutoScalingGroupNames = AutoScalingGroupNames [ResourceName]
   deriving (Show)

$(deriveQS' (++ ".member") ''AutoScalingGroupNames)

newtype AutoScalingGroups = AutoScalingGroups [AutoScalingGroup]
   deriving (Show)

newtype ScalingPolicies = ScalingPolicies [ScalingPolicy]
   deriving (Show)

newtype Activities = Activities [Activity]
   deriving (Show)

newtype ActivityIds = ActivityIds [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''ActivityIds)

newtype ProcessNames = ProcessNames [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''ProcessNames)

newtype AdjustmentTypes = AdjustmentTypes [Text]
   deriving (Show)

$(deriveQS' (++ ".member") ''AdjustmentTypes)
