{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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

    -- * ClusterState
    , ClusterState (..)

    -- * ClusterStateChangeReasonCode
    , ClusterStateChangeReasonCode (..)

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

    -- * StepState
    , StepState (..)

    -- * StepStateChangeReasonCode
    , StepStateChangeReasonCode (..)

    -- * Application
    , Application
    , application
    , aArgs
    , aAdditionalInfo
    , aName
    , aVersion

    -- * BootstrapActionConfig
    , BootstrapActionConfig
    , bootstrapActionConfig
    , bacName
    , bacScriptBootstrapAction

    -- * Cluster
    , Cluster
    , cluster
    , cluRequestedAMIVersion
    , cluEC2InstanceAttributes
    , cluNormalizedInstanceHours
    , cluConfigurations
    , cluReleaseLabel
    , cluLogURI
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
    , eiaAdditionalMasterSecurityGroups
    , eiaIAMInstanceProfile
    , eiaEmrManagedMasterSecurityGroup
    , eiaEC2SubnetId
    , eiaServiceAccessSecurityGroup
    , eiaEC2AvailabilityZone

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
    , iPrivateIPAddress
    , iId
    , iInstanceGroupId
    , iPrivateDNSName
    , iPublicIPAddress

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
    , igId

    -- * InstanceGroupConfig
    , InstanceGroupConfig
    , instanceGroupConfig
    , igcEBSConfiguration
    , igcBidPrice
    , igcConfigurations
    , igcMarket
    , igcName
    , igcInstanceRole
    , igcInstanceType
    , igcInstanceCount

    -- * InstanceGroupModifyConfig
    , InstanceGroupModifyConfig
    , instanceGroupModifyConfig
    , igmcInstanceCount
    , igmcEC2InstanceIdsToTerminate
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

    -- * JobFlowInstancesConfig
    , JobFlowInstancesConfig
    , jobFlowInstancesConfig
    , jficEC2KeyName
    , jficSlaveInstanceType
    , jficInstanceCount
    , jficEmrManagedSlaveSecurityGroup
    , jficAdditionalSlaveSecurityGroups
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

    -- * KeyValue
    , KeyValue
    , keyValue
    , kvValue
    , kvKey

    -- * PlacementType
    , PlacementType
    , placementType
    , ptAvailabilityZone

    -- * ScriptBootstrapActionConfig
    , ScriptBootstrapActionConfig
    , scriptBootstrapActionConfig
    , sbacArgs
    , sbacPath

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

import           Network.AWS.EMR.Types.Product
import           Network.AWS.EMR.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2009-03-31' of the Amazon Elastic MapReduce SDK configuration.
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
    , _svcError = parseJSONError
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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | This exception occurs when there is something wrong with user input.
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException = _ServiceError . hasCode "InvalidRequestException"

-- | Indicates that an error occurred while processing the request and that
-- the request was not completed.
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _ServiceError . hasCode "InternalServerError"

-- | This exception occurs when there is an internal failure in the EMR
-- service.
_InternalServerException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerException = _ServiceError . hasCode "InternalServerException"
