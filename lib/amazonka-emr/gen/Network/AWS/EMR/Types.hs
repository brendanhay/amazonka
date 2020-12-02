{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types
    (
    -- * Service Configuration
      emr

    -- * Errors
    , _InvalidRequestException
    , _InternalServerError
    , _InternalServerException

    -- * ActionOnFailure
    , ActionOnFailure (..)

    -- * AdjustmentType
    , AdjustmentType (..)

    -- * AutoScalingPolicyState
    , AutoScalingPolicyState (..)

    -- * AutoScalingPolicyStateChangeReasonCode
    , AutoScalingPolicyStateChangeReasonCode (..)

    -- * CancelStepsRequestStatus
    , CancelStepsRequestStatus (..)

    -- * ClusterState
    , ClusterState (..)

    -- * ClusterStateChangeReasonCode
    , ClusterStateChangeReasonCode (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * InstanceCollectionType
    , InstanceCollectionType (..)

    -- * InstanceFleetState
    , InstanceFleetState (..)

    -- * InstanceFleetStateChangeReasonCode
    , InstanceFleetStateChangeReasonCode (..)

    -- * InstanceFleetType
    , InstanceFleetType (..)

    -- * InstanceGroupState
    , InstanceGroupState (..)

    -- * InstanceGroupStateChangeReasonCode
    , InstanceGroupStateChangeReasonCode (..)

    -- * InstanceGroupType
    , InstanceGroupType (..)

    -- * InstanceRoleType
    , InstanceRoleType (..)

    -- * InstanceState
    , InstanceState (..)

    -- * InstanceStateChangeReasonCode
    , InstanceStateChangeReasonCode (..)

    -- * MarketType
    , MarketType (..)

    -- * RepoUpgradeOnBoot
    , RepoUpgradeOnBoot (..)

    -- * ScaleDownBehavior
    , ScaleDownBehavior (..)

    -- * SpotProvisioningTimeoutAction
    , SpotProvisioningTimeoutAction (..)

    -- * Statistic
    , Statistic (..)

    -- * StepState
    , StepState (..)

    -- * StepStateChangeReasonCode
    , StepStateChangeReasonCode (..)

    -- * Unit
    , Unit (..)

    -- * Application
    , Application
    , application
    , aArgs
    , aAdditionalInfo
    , aName
    , aVersion

    -- * AutoScalingPolicy
    , AutoScalingPolicy
    , autoScalingPolicy
    , aspConstraints
    , aspRules

    -- * AutoScalingPolicyDescription
    , AutoScalingPolicyDescription
    , autoScalingPolicyDescription
    , aspdStatus
    , aspdRules
    , aspdConstraints

    -- * AutoScalingPolicyStateChangeReason
    , AutoScalingPolicyStateChangeReason
    , autoScalingPolicyStateChangeReason
    , aspscrCode
    , aspscrMessage

    -- * AutoScalingPolicyStatus
    , AutoScalingPolicyStatus
    , autoScalingPolicyStatus
    , aspsState
    , aspsStateChangeReason

    -- * BootstrapActionConfig
    , BootstrapActionConfig
    , bootstrapActionConfig
    , bacName
    , bacScriptBootstrapAction

    -- * CancelStepsInfo
    , CancelStepsInfo
    , cancelStepsInfo
    , csiStatus
    , csiStepId
    , csiReason

    -- * CloudWatchAlarmDefinition
    , CloudWatchAlarmDefinition
    , cloudWatchAlarmDefinition
    , cwadEvaluationPeriods
    , cwadNamespace
    , cwadDimensions
    , cwadUnit
    , cwadStatistic
    , cwadComparisonOperator
    , cwadMetricName
    , cwadPeriod
    , cwadThreshold

    -- * Cluster
    , Cluster
    , cluster
    , cluRequestedAMIVersion
    , cluEBSRootVolumeSize
    , cluEC2InstanceAttributes
    , cluNormalizedInstanceHours
    , cluConfigurations
    , cluCustomAMIId
    , cluAutoScalingRole
    , cluSecurityConfiguration
    , cluScaleDownBehavior
    , cluInstanceCollectionType
    , cluReleaseLabel
    , cluRepoUpgradeOnBoot
    , cluLogURI
    , cluKerberosAttributes
    , cluRunningAMIVersion
    , cluMasterPublicDNSName
    , cluTerminationProtected
    , cluVisibleToAllUsers
    , cluAutoTerminate
    , cluApplications
    , cluTags
    , cluServiceRole
    , cluId
    , cluName
    , cluStatus

    -- * ClusterStateChangeReason
    , ClusterStateChangeReason
    , clusterStateChangeReason
    , cscrCode
    , cscrMessage

    -- * ClusterStatus
    , ClusterStatus
    , clusterStatus
    , csState
    , csStateChangeReason
    , csTimeline

    -- * ClusterSummary
    , ClusterSummary
    , clusterSummary
    , csStatus
    , csNormalizedInstanceHours
    , csName
    , csId

    -- * ClusterTimeline
    , ClusterTimeline
    , clusterTimeline
    , ctReadyDateTime
    , ctCreationDateTime
    , ctEndDateTime

    -- * Command
    , Command
    , command
    , cArgs
    , cScriptPath
    , cName

    -- * Configuration
    , Configuration
    , configuration
    , cConfigurations
    , cClassification
    , cProperties

    -- * EBSBlockDevice
    , EBSBlockDevice
    , ebsBlockDevice
    , ebdDevice
    , ebdVolumeSpecification

    -- * EBSBlockDeviceConfig
    , EBSBlockDeviceConfig
    , ebsBlockDeviceConfig
    , ebdcVolumesPerInstance
    , ebdcVolumeSpecification

    -- * EBSConfiguration
    , EBSConfiguration
    , ebsConfiguration
    , ecEBSOptimized
    , ecEBSBlockDeviceConfigs

    -- * EBSVolume
    , EBSVolume
    , ebsVolume
    , evDevice
    , evVolumeId

    -- * EC2InstanceAttributes
    , EC2InstanceAttributes
    , ec2InstanceAttributes
    , eiaEC2KeyName
    , eiaEmrManagedSlaveSecurityGroup
    , eiaAdditionalSlaveSecurityGroups
    , eiaRequestedEC2SubnetIds
    , eiaAdditionalMasterSecurityGroups
    , eiaIAMInstanceProfile
    , eiaEmrManagedMasterSecurityGroup
    , eiaEC2SubnetId
    , eiaRequestedEC2AvailabilityZones
    , eiaServiceAccessSecurityGroup
    , eiaEC2AvailabilityZone

    -- * FailureDetails
    , FailureDetails
    , failureDetails
    , fdLogFile
    , fdReason
    , fdMessage

    -- * HadoopJARStepConfig
    , HadoopJARStepConfig
    , hadoopJARStepConfig
    , hjscArgs
    , hjscMainClass
    , hjscProperties
    , hjscJAR

    -- * HadoopStepConfig
    , HadoopStepConfig
    , hadoopStepConfig
    , hscArgs
    , hscJAR
    , hscMainClass
    , hscProperties

    -- * Instance
    , Instance
    , instance'
    , iStatus
    , iPublicDNSName
    , iEBSVolumes
    , iEC2InstanceId
    , iInstanceType
    , iMarket
    , iPrivateIPAddress
    , iInstanceFleetId
    , iId
    , iInstanceGroupId
    , iPrivateDNSName
    , iPublicIPAddress

    -- * InstanceFleet
    , InstanceFleet
    , instanceFleet
    , ifProvisionedSpotCapacity
    , ifStatus
    , ifTargetOnDemandCapacity
    , ifInstanceFleetType
    , ifInstanceTypeSpecifications
    , ifName
    , ifProvisionedOnDemandCapacity
    , ifTargetSpotCapacity
    , ifId
    , ifLaunchSpecifications

    -- * InstanceFleetConfig
    , InstanceFleetConfig
    , instanceFleetConfig
    , ifcInstanceTypeConfigs
    , ifcTargetOnDemandCapacity
    , ifcName
    , ifcTargetSpotCapacity
    , ifcLaunchSpecifications
    , ifcInstanceFleetType

    -- * InstanceFleetModifyConfig
    , InstanceFleetModifyConfig
    , instanceFleetModifyConfig
    , ifmcTargetOnDemandCapacity
    , ifmcTargetSpotCapacity
    , ifmcInstanceFleetId

    -- * InstanceFleetProvisioningSpecifications
    , InstanceFleetProvisioningSpecifications
    , instanceFleetProvisioningSpecifications
    , ifpsSpotSpecification

    -- * InstanceFleetStateChangeReason
    , InstanceFleetStateChangeReason
    , instanceFleetStateChangeReason
    , ifscrCode
    , ifscrMessage

    -- * InstanceFleetStatus
    , InstanceFleetStatus
    , instanceFleetStatus
    , ifsState
    , ifsStateChangeReason
    , ifsTimeline

    -- * InstanceFleetTimeline
    , InstanceFleetTimeline
    , instanceFleetTimeline
    , iftReadyDateTime
    , iftCreationDateTime
    , iftEndDateTime

    -- * InstanceGroup
    , InstanceGroup
    , instanceGroup
    , igStatus
    , igBidPrice
    , igRequestedInstanceCount
    , igRunningInstanceCount
    , igConfigurations
    , igInstanceGroupType
    , igEBSBlockDevices
    , igInstanceType
    , igEBSOptimized
    , igMarket
    , igName
    , igAutoScalingPolicy
    , igShrinkPolicy
    , igId

    -- * InstanceGroupConfig
    , InstanceGroupConfig
    , instanceGroupConfig
    , igcEBSConfiguration
    , igcBidPrice
    , igcConfigurations
    , igcMarket
    , igcName
    , igcAutoScalingPolicy
    , igcInstanceRole
    , igcInstanceType
    , igcInstanceCount

    -- * InstanceGroupModifyConfig
    , InstanceGroupModifyConfig
    , instanceGroupModifyConfig
    , igmcInstanceCount
    , igmcEC2InstanceIdsToTerminate
    , igmcShrinkPolicy
    , igmcInstanceGroupId

    -- * InstanceGroupStateChangeReason
    , InstanceGroupStateChangeReason
    , instanceGroupStateChangeReason
    , igscrCode
    , igscrMessage

    -- * InstanceGroupStatus
    , InstanceGroupStatus
    , instanceGroupStatus
    , igsState
    , igsStateChangeReason
    , igsTimeline

    -- * InstanceGroupTimeline
    , InstanceGroupTimeline
    , instanceGroupTimeline
    , igtReadyDateTime
    , igtCreationDateTime
    , igtEndDateTime

    -- * InstanceResizePolicy
    , InstanceResizePolicy
    , instanceResizePolicy
    , irpInstancesToProtect
    , irpInstancesToTerminate
    , irpInstanceTerminationTimeout

    -- * InstanceStateChangeReason
    , InstanceStateChangeReason
    , instanceStateChangeReason
    , iscrCode
    , iscrMessage

    -- * InstanceStatus
    , InstanceStatus
    , instanceStatus
    , isState
    , isStateChangeReason
    , isTimeline

    -- * InstanceTimeline
    , InstanceTimeline
    , instanceTimeline
    , itReadyDateTime
    , itCreationDateTime
    , itEndDateTime

    -- * InstanceTypeConfig
    , InstanceTypeConfig
    , instanceTypeConfig
    , itcEBSConfiguration
    , itcBidPrice
    , itcWeightedCapacity
    , itcConfigurations
    , itcBidPriceAsPercentageOfOnDemandPrice
    , itcInstanceType

    -- * InstanceTypeSpecification
    , InstanceTypeSpecification
    , instanceTypeSpecification
    , itsBidPrice
    , itsWeightedCapacity
    , itsConfigurations
    , itsEBSBlockDevices
    , itsInstanceType
    , itsEBSOptimized
    , itsBidPriceAsPercentageOfOnDemandPrice

    -- * JobFlowInstancesConfig
    , JobFlowInstancesConfig
    , jobFlowInstancesConfig
    , jficInstanceFleets
    , jficEC2KeyName
    , jficSlaveInstanceType
    , jficInstanceCount
    , jficEmrManagedSlaveSecurityGroup
    , jficAdditionalSlaveSecurityGroups
    , jficEC2SubnetIds
    , jficHadoopVersion
    , jficAdditionalMasterSecurityGroups
    , jficEmrManagedMasterSecurityGroup
    , jficEC2SubnetId
    , jficMasterInstanceType
    , jficInstanceGroups
    , jficKeepJobFlowAliveWhenNoSteps
    , jficServiceAccessSecurityGroup
    , jficTerminationProtected
    , jficPlacement

    -- * KerberosAttributes
    , KerberosAttributes
    , kerberosAttributes
    , kaADDomainJoinPassword
    , kaCrossRealmTrustPrincipalPassword
    , kaADDomainJoinUser
    , kaRealm
    , kaKdcAdminPassword

    -- * KeyValue
    , KeyValue
    , keyValue
    , kvValue
    , kvKey

    -- * MetricDimension
    , MetricDimension
    , metricDimension
    , mdValue
    , mdKey

    -- * PlacementType
    , PlacementType
    , placementType
    , ptAvailabilityZones
    , ptAvailabilityZone

    -- * ScalingAction
    , ScalingAction
    , scalingAction
    , saMarket
    , saSimpleScalingPolicyConfiguration

    -- * ScalingConstraints
    , ScalingConstraints
    , scalingConstraints
    , scMinCapacity
    , scMaxCapacity

    -- * ScalingRule
    , ScalingRule
    , scalingRule
    , srDescription
    , srName
    , srAction
    , srTrigger

    -- * ScalingTrigger
    , ScalingTrigger
    , scalingTrigger
    , stCloudWatchAlarmDefinition

    -- * ScriptBootstrapActionConfig
    , ScriptBootstrapActionConfig
    , scriptBootstrapActionConfig
    , sbacArgs
    , sbacPath

    -- * SecurityConfigurationSummary
    , SecurityConfigurationSummary
    , securityConfigurationSummary
    , scsName
    , scsCreationDateTime

    -- * ShrinkPolicy
    , ShrinkPolicy
    , shrinkPolicy
    , spDecommissionTimeout
    , spInstanceResizePolicy

    -- * SimpleScalingPolicyConfiguration
    , SimpleScalingPolicyConfiguration
    , simpleScalingPolicyConfiguration
    , sspcAdjustmentType
    , sspcCoolDown
    , sspcScalingAdjustment

    -- * SpotProvisioningSpecification
    , SpotProvisioningSpecification
    , spotProvisioningSpecification
    , spsBlockDurationMinutes
    , spsTimeoutDurationMinutes
    , spsTimeoutAction

    -- * Step
    , Step
    , step
    , sStatus
    , sActionOnFailure
    , sConfig
    , sName
    , sId

    -- * StepConfig
    , StepConfig
    , stepConfig
    , scActionOnFailure
    , scName
    , scHadoopJARStep

    -- * StepStateChangeReason
    , StepStateChangeReason
    , stepStateChangeReason
    , sscrCode
    , sscrMessage

    -- * StepStatus
    , StepStatus
    , stepStatus
    , ssState
    , ssFailureDetails
    , ssStateChangeReason
    , ssTimeline

    -- * StepSummary
    , StepSummary
    , stepSummary
    , ssStatus
    , ssActionOnFailure
    , ssConfig
    , ssName
    , ssId

    -- * StepTimeline
    , StepTimeline
    , stepTimeline
    , stCreationDateTime
    , stEndDateTime
    , stStartDateTime

    -- * SupportedProductConfig
    , SupportedProductConfig
    , supportedProductConfig
    , spcArgs
    , spcName

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * VolumeSpecification
    , VolumeSpecification
    , volumeSpecification
    , vsIOPS
    , vsVolumeType
    , vsSizeInGB
    ) where

import Network.AWS.EMR.Types.Product
import Network.AWS.EMR.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2009-03-31@ of the Amazon Elastic MapReduce SDK configuration.
emr :: Service
emr =
  Service
    { _svcAbbrev = "EMR"
    , _svcSigner = v4
    , _svcPrefix = "elasticmapreduce"
    , _svcVersion = "2009-03-31"
    , _svcEndpoint = defaultEndpoint emr
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "EMR"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | This exception occurs when there is something wrong with user input.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException = _MatchServiceError emr "InvalidRequestException"


-- | Indicates that an error occurred while processing the request and that the request was not completed.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _MatchServiceError emr "InternalServerError"


-- | This exception occurs when there is an internal failure in the EMR service.
--
--
_InternalServerException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerException = _MatchServiceError emr "InternalServerException"

