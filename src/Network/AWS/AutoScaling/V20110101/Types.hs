{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

--
-- Types
--

data AutoScalingNotificationTypes = AutoScalingNotificationTypes
    { asntMember :: ![Text]
    } deriving (Show)

data MetricGranularityType = MetricGranularityType
    { mgtGranularity :: !(Maybe Text)
    } deriving (Show)

data SuspendedProcess = SuspendedProcess
    { spProcessName :: !(Maybe Text)
    , spSuspensionReason :: !(Maybe Text)
    } deriving (Show)

data SuspendedProcesses = SuspendedProcesses
    { spsMember :: ![SuspendedProcess]
    } deriving (Show)

data LoadBalancerNames = LoadBalancerNames
    { lbnMember :: ![Text]
    } deriving (Show)

data Tag = Tag
    { tResourceId :: !(Maybe Text)
    , tResourceType :: !(Maybe Text)
    , tKey :: !Text
    , tValue :: !(Maybe Text)
    , tPropagateAtLaunch :: !(Maybe Bool)
    } deriving (Show)

data Tags = Tags
    { tMember :: ![Tag]
    } deriving (Show)

data PolicyNames = PolicyNames
    { pnMember :: ![ResourceName]
    } deriving (Show)

data NotificationConfiguration = NotificationConfiguration
    { ncAutoScalingGroupName :: !(Maybe ResourceName)
    , ncTopicARN :: !(Maybe ResourceName)
    , ncNotificationType :: !(Maybe Text)
    } deriving (Show)

data AdjustmentType = AdjustmentType
    { atAdjustmentType :: !(Maybe Text)
    } deriving (Show)

data AdjustmentTypes = AdjustmentTypes
    { atMember :: ![AdjustmentType]
    } deriving (Show)

data ScheduledActionNames = ScheduledActionNames
    { sanMember :: ![ResourceName]
    } deriving (Show)

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

data ScheduledUpdateGroupActions = ScheduledUpdateGroupActions
    { sugaMember :: ![ScheduledUpdateGroupAction]
    } deriving (Show)

data NotificationConfigurations = NotificationConfigurations
    { ncMember :: ![NotificationConfiguration]
    } deriving (Show)

data SecurityGroups = SecurityGroups
    { sgMember :: ![Text]
    } deriving (Show)

data BlockDeviceMapping = BlockDeviceMapping
    { bdmVirtualName :: !(Maybe Text)
    , bdmDeviceName :: !Text
    , bdmEbs :: !(Maybe Ebs)
    } deriving (Show)

data BlockDeviceMappings = BlockDeviceMappings
    { bdmMember :: ![BlockDeviceMapping]
    } deriving (Show)

data InstanceMonitoring = InstanceMonitoring
    { imEnabled :: !(Maybe Bool)
    } deriving (Show)

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

data LaunchConfigurations = LaunchConfigurations
    { lcMember :: ![LaunchConfiguration]
    } deriving (Show)

data AvailabilityZones = AvailabilityZones
    { azMember :: ![Text]
    } deriving (Show)

data EnabledMetric = EnabledMetric
    { emMetric :: !(Maybe Text)
    , emGranularity :: !(Maybe Text)
    } deriving (Show)

data EnabledMetrics = EnabledMetrics
    { emMember :: ![EnabledMetric]
    } deriving (Show)

data TagDescription = TagDescription
    { tdResourceId :: !(Maybe Text)
    , tdResourceType :: !(Maybe Text)
    , tdKey :: !(Maybe Text)
    , tdValue :: !(Maybe Text)
    , tdPropagateAtLaunch :: !(Maybe Bool)
    } deriving (Show)

data TagDescriptionList = TagDescriptionList
    { tdlMember :: ![TagDescription]
    } deriving (Show)

data TerminationPolicies = TerminationPolicies
    { tpMember :: ![Text]
    } deriving (Show)

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
    , asgSuspendedProcesses :: !(Maybe SuspendedProcesses)
    , asgPlacementGroup :: !(Maybe Text)
    , asgVPCZoneIdentifier :: !(Maybe Text)
    , asgEnabledMetrics :: !(Maybe EnabledMetrics)
    , asgStatus :: !(Maybe Text)
    , asgTags :: !(Maybe TagDescriptionList)
    , asgTerminationPolicies :: !(Maybe TerminationPolicies)
    } deriving (Show)

data Ebs = Ebs
    { eSnapshotId :: !(Maybe Text)
    , eVolumeSize :: !(Maybe Integer)
    } deriving (Show)

data LifecycleState
    = Pending
    | Quarantined
    | InService
    | Terminating
    | Terminated
      deriving (Show)

data Instance = Instance
    { iInstanceId :: !Text
    , iAvailabilityZone :: !Text
    , iLifecycleState :: !LifecycleState
    , iHealthStatus :: !Text
    , iLaunchConfigurationName :: !Text
    } deriving (Show)

data Instances = Instances
    { iMember :: ![Instance]
    } deriving (Show)

data MetricCollectionType = MetricCollectionType
    { mctMetric :: !(Maybe Text)
    } deriving (Show)

data MetricCollectionTypes = MetricCollectionTypes
    { mctMember :: ![MetricCollectionType]
    } deriving (Show)

data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { asidInstanceId :: !Text
    , asidAutoScalingGroupName :: !Text
    , asidAvailabilityZone :: !Text
    , asidLifecycleState :: !Text
    , asidHealthStatus :: !Text
    , asidLaunchConfigurationName :: !Text
    } deriving (Show)

data Metrics = Metrics
    { mMember :: ![Text]
    } deriving (Show)

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

data ScalingPolicy = ScalingPolicy
    { spAutoScalingGroupName :: !(Maybe Text)
    , spPolicyName :: !(Maybe Text)
    , spScalingAdjustment :: !(Maybe Integer)
    , spAdjustmentType :: !(Maybe Text)
    , spCooldown :: !(Maybe Integer)
    , spPolicyARN :: !(Maybe ResourceName)
    , spAlarms :: !(Maybe Alarms)
    , spMinAdjustmentStep :: !(Maybe Integer)
    } deriving (Show)

data Values = Values
    { vMember :: ![Text]
    } deriving (Show)

data AutoScalingInstances = AutoScalingInstances
    { asiMember :: ![AutoScalingInstanceDetails]
    } deriving (Show)

data ProcessType = ProcessType
    { ptProcessName :: !Text
    } deriving (Show)

data Processes = Processes
    { pMember :: ![ProcessType]
    } deriving (Show)

data Alarm = Alarm
    { aAlarmName :: !(Maybe Text)
    , aAlarmARN :: !(Maybe ResourceName)
    } deriving (Show)

data InstanceIds = InstanceIds
    { iiMember :: ![Text]
    } deriving (Show)

data LaunchConfigurationNames = LaunchConfigurationNames
    { lcnMember :: ![ResourceName]
    } deriving (Show)

data MetricGranularityTypes = MetricGranularityTypes
    { mgtMember :: ![MetricGranularityType]
    } deriving (Show)

data AutoScalingGroupNames = AutoScalingGroupNames
    { asgnMember :: ![ResourceName]
    } deriving (Show)

data Filters = Filters
    { fMember :: ![Filter]
    } deriving (Show)

data Filter = Filter
    { fName :: !(Maybe Text)
    , fValues :: !(Maybe Values)
    } deriving (Show)

data AutoScalingGroups = AutoScalingGroups
    { asgMember :: ![AutoScalingGroup]
    } deriving (Show)

data Alarms = Alarms
    { asMember :: ![Alarm]
    } deriving (Show)

data ScalingPolicies = ScalingPolicies
    { spMember :: ![ScalingPolicy]
    } deriving (Show)

data Activities = Activities
    { aMember :: ![Activity]
    } deriving (Show)

data ActivityIds = ActivityIds
    { aiMember :: ![Text]
    } deriving (Show)

data ProcessNames = ProcessNames
    { pnsMember :: ![Text]
    } deriving (Show)

--
-- Responses
--
