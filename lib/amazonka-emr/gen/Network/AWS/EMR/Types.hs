-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidRequestException
    , _InternalServerError
    , _InternalServerException

    -- * BlockPublicAccessConfiguration
    , BlockPublicAccessConfiguration (..)
    , mkBlockPublicAccessConfiguration
    , bpacBlockPublicSecurityGroupRules
    , bpacPermittedPublicSecurityGroupRuleRanges

    -- * XmlString
    , XmlString (..)

    -- * MarketType
    , MarketType (..)

    -- * InstanceGroupConfig
    , InstanceGroupConfig (..)
    , mkInstanceGroupConfig
    , igcInstanceRole
    , igcInstanceType
    , igcInstanceCount
    , igcAutoScalingPolicy
    , igcBidPrice
    , igcConfigurations
    , igcEbsConfiguration
    , igcMarket
    , igcName

    -- * InstanceId
    , InstanceId (..)

    -- * SpotProvisioningTimeoutAction
    , SpotProvisioningTimeoutAction (..)

    -- * EbsConfiguration
    , EbsConfiguration (..)
    , mkEbsConfiguration
    , ecEbsBlockDeviceConfigs
    , ecEbsOptimized

    -- * InstanceStateChangeReason
    , InstanceStateChangeReason (..)
    , mkInstanceStateChangeReason
    , iscrCode
    , iscrMessage

    -- * PlacementGroupConfig
    , PlacementGroupConfig (..)
    , mkPlacementGroupConfig
    , pgcInstanceRole
    , pgcPlacementStrategy

    -- * KeyValue
    , KeyValue (..)
    , mkKeyValue
    , kvKey
    , kvValue

    -- * InstanceTypeSpecification
    , InstanceTypeSpecification (..)
    , mkInstanceTypeSpecification
    , itsBidPrice
    , itsBidPriceAsPercentageOfOnDemandPrice
    , itsConfigurations
    , itsEbsBlockDevices
    , itsEbsOptimized
    , itsInstanceType
    , itsWeightedCapacity

    -- * NotebookExecutionStatus
    , NotebookExecutionStatus (..)

    -- * SupportedProductConfig
    , SupportedProductConfig (..)
    , mkSupportedProductConfig
    , spcArgs
    , spcName

    -- * Command
    , Command (..)
    , mkCommand
    , cArgs
    , cName
    , cScriptPath

    -- * StepId
    , StepId (..)

    -- * ActionOnFailure
    , ActionOnFailure (..)

    -- * InstanceFleet
    , InstanceFleet (..)
    , mkInstanceFleet
    , ifId
    , ifInstanceFleetType
    , ifInstanceTypeSpecifications
    , ifLaunchSpecifications
    , ifName
    , ifProvisionedOnDemandCapacity
    , ifProvisionedSpotCapacity
    , ifStatus
    , ifTargetOnDemandCapacity
    , ifTargetSpotCapacity

    -- * ClusterStateChangeReason
    , ClusterStateChangeReason (..)
    , mkClusterStateChangeReason
    , cscrCode
    , cscrMessage

    -- * InstanceFleetStateChangeReasonCode
    , InstanceFleetStateChangeReasonCode (..)

    -- * ResourceId
    , ResourceId (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * Application
    , Application (..)
    , mkApplication
    , aAdditionalInfo
    , aArgs
    , aName
    , aVersion

    -- * AutoScalingPolicyStateChangeReasonCode
    , AutoScalingPolicyStateChangeReasonCode (..)

    -- * SpotProvisioningAllocationStrategy
    , SpotProvisioningAllocationStrategy (..)

    -- * InstanceGroupStatus
    , InstanceGroupStatus (..)
    , mkInstanceGroupStatus
    , igsState
    , igsStateChangeReason
    , igsTimeline

    -- * Cluster
    , Cluster (..)
    , mkCluster
    , cfApplications
    , cfAutoScalingRole
    , cfAutoTerminate
    , cfClusterArn
    , cfConfigurations
    , cfCustomAmiId
    , cfEbsRootVolumeSize
    , cfEc2InstanceAttributes
    , cfId
    , cfInstanceCollectionType
    , cfKerberosAttributes
    , cfLogEncryptionKmsKeyId
    , cfLogUri
    , cfMasterPublicDnsName
    , cfName
    , cfNormalizedInstanceHours
    , cfOutpostArn
    , cfPlacementGroups
    , cfReleaseLabel
    , cfRepoUpgradeOnBoot
    , cfRequestedAmiVersion
    , cfRunningAmiVersion
    , cfScaleDownBehavior
    , cfSecurityConfiguration
    , cfServiceRole
    , cfStatus
    , cfStepConcurrencyLevel
    , cfTags
    , cfTerminationProtected
    , cfVisibleToAllUsers

    -- * CancelStepsRequestStatus
    , CancelStepsRequestStatus (..)

    -- * InstanceTimeline
    , InstanceTimeline (..)
    , mkInstanceTimeline
    , itCreationDateTime
    , itEndDateTime
    , itReadyDateTime

    -- * Ec2InstanceAttributes
    , Ec2InstanceAttributes (..)
    , mkEc2InstanceAttributes
    , eiaAdditionalMasterSecurityGroups
    , eiaAdditionalSlaveSecurityGroups
    , eiaEc2AvailabilityZone
    , eiaEc2KeyName
    , eiaEc2SubnetId
    , eiaEmrManagedMasterSecurityGroup
    , eiaEmrManagedSlaveSecurityGroup
    , eiaIamInstanceProfile
    , eiaRequestedEc2AvailabilityZones
    , eiaRequestedEc2SubnetIds
    , eiaServiceAccessSecurityGroup

    -- * OnDemandProvisioningSpecification
    , OnDemandProvisioningSpecification (..)
    , mkOnDemandProvisioningSpecification
    , odpsAllocationStrategy

    -- * StepStateChangeReasonCode
    , StepStateChangeReasonCode (..)

    -- * FailureDetails
    , FailureDetails (..)
    , mkFailureDetails
    , fdLogFile
    , fdMessage
    , fdReason

    -- * ClusterState
    , ClusterState (..)

    -- * InstanceFleetConfig
    , InstanceFleetConfig (..)
    , mkInstanceFleetConfig
    , ifcInstanceFleetType
    , ifcInstanceTypeConfigs
    , ifcLaunchSpecifications
    , ifcName
    , ifcTargetOnDemandCapacity
    , ifcTargetSpotCapacity

    -- * ComputeLimitsUnitType
    , ComputeLimitsUnitType (..)

    -- * PlacementGroupStrategy
    , PlacementGroupStrategy (..)

    -- * HadoopStepConfig
    , HadoopStepConfig (..)
    , mkHadoopStepConfig
    , hscArgs
    , hscJar
    , hscMainClass
    , hscProperties

    -- * SessionMappingDetail
    , SessionMappingDetail (..)
    , mkSessionMappingDetail
    , smdCreationTime
    , smdIdentityId
    , smdIdentityName
    , smdIdentityType
    , smdLastModifiedTime
    , smdSessionPolicyArn
    , smdStudioId

    -- * ScalingRule
    , ScalingRule (..)
    , mkScalingRule
    , srName
    , srAction
    , srTrigger
    , srDescription

    -- * ScalingAction
    , ScalingAction (..)
    , mkScalingAction
    , saSimpleScalingPolicyConfiguration
    , saMarket

    -- * InstanceTypeConfig
    , InstanceTypeConfig (..)
    , mkInstanceTypeConfig
    , itcInstanceType
    , itcBidPrice
    , itcBidPriceAsPercentageOfOnDemandPrice
    , itcConfigurations
    , itcEbsConfiguration
    , itcWeightedCapacity

    -- * NotebookExecution
    , NotebookExecution (..)
    , mkNotebookExecution
    , neArn
    , neEditorId
    , neEndTime
    , neExecutionEngine
    , neLastStateChangeReason
    , neNotebookExecutionId
    , neNotebookExecutionName
    , neNotebookInstanceSecurityGroupId
    , neNotebookParams
    , neOutputNotebookURI
    , neStartTime
    , neStatus
    , neTags

    -- * AuthMode
    , AuthMode (..)

    -- * InstanceResizePolicy
    , InstanceResizePolicy (..)
    , mkInstanceResizePolicy
    , irpInstanceTerminationTimeout
    , irpInstancesToProtect
    , irpInstancesToTerminate

    -- * InstanceFleetStatus
    , InstanceFleetStatus (..)
    , mkInstanceFleetStatus
    , ifsState
    , ifsStateChangeReason
    , ifsTimeline

    -- * InstanceGroupStateChangeReason
    , InstanceGroupStateChangeReason (..)
    , mkInstanceGroupStateChangeReason
    , igscrCode
    , igscrMessage

    -- * SessionMappingSummary
    , SessionMappingSummary (..)
    , mkSessionMappingSummary
    , smsCreationTime
    , smsIdentityId
    , smsIdentityName
    , smsIdentityType
    , smsSessionPolicyArn
    , smsStudioId

    -- * InstanceGroupType
    , InstanceGroupType (..)

    -- * AutoScalingPolicyStatus
    , AutoScalingPolicyStatus (..)
    , mkAutoScalingPolicyStatus
    , aspsState
    , aspsStateChangeReason

    -- * XmlStringMaxLen256
    , XmlStringMaxLen256 (..)

    -- * StepCancellationOption
    , StepCancellationOption (..)

    -- * IdentityType
    , IdentityType (..)

    -- * ScaleDownBehavior
    , ScaleDownBehavior (..)

    -- * PortRange
    , PortRange (..)
    , mkPortRange
    , prMinRange
    , prMaxRange

    -- * InstanceGroupStateChangeReasonCode
    , InstanceGroupStateChangeReasonCode (..)

    -- * StepStatus
    , StepStatus (..)
    , mkStepStatus
    , ssFailureDetails
    , ssState
    , ssStateChangeReason
    , ssTimeline

    -- * StepSummary
    , StepSummary (..)
    , mkStepSummary
    , sActionOnFailure
    , sConfig
    , sId
    , sName
    , sStatus

    -- * InstanceGroupState
    , InstanceGroupState (..)

    -- * StepTimeline
    , StepTimeline (..)
    , mkStepTimeline
    , stCreationDateTime
    , stEndDateTime
    , stStartDateTime

    -- * PlacementType
    , PlacementType (..)
    , mkPlacementType
    , ptAvailabilityZone
    , ptAvailabilityZones

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * CancelStepsInfo
    , CancelStepsInfo (..)
    , mkCancelStepsInfo
    , csiReason
    , csiStatus
    , csiStepId

    -- * InstanceType
    , InstanceType (..)

    -- * HadoopJarStepConfig
    , HadoopJarStepConfig (..)
    , mkHadoopJarStepConfig
    , hjscJar
    , hjscArgs
    , hjscMainClass
    , hjscProperties

    -- * MetricDimension
    , MetricDimension (..)
    , mkMetricDimension
    , mdKey
    , mdValue

    -- * ExecutionEngineType
    , ExecutionEngineType (..)

    -- * InstanceGroupModifyConfig
    , InstanceGroupModifyConfig (..)
    , mkInstanceGroupModifyConfig
    , igmcInstanceGroupId
    , igmcConfigurations
    , igmcEC2InstanceIdsToTerminate
    , igmcInstanceCount
    , igmcShrinkPolicy

    -- * StudioSummary
    , StudioSummary (..)
    , mkStudioSummary
    , ssCreationTime
    , ssDescription
    , ssName
    , ssStudioId
    , ssUrl
    , ssVpcId

    -- * SpotProvisioningSpecification
    , SpotProvisioningSpecification (..)
    , mkSpotProvisioningSpecification
    , spsTimeoutDurationMinutes
    , spsTimeoutAction
    , spsAllocationStrategy
    , spsBlockDurationMinutes

    -- * AutoScalingPolicyDescription
    , AutoScalingPolicyDescription (..)
    , mkAutoScalingPolicyDescription
    , aspdConstraints
    , aspdRules
    , aspdStatus

    -- * OnDemandProvisioningAllocationStrategy
    , OnDemandProvisioningAllocationStrategy (..)

    -- * InstanceFleetTimeline
    , InstanceFleetTimeline (..)
    , mkInstanceFleetTimeline
    , iftCreationDateTime
    , iftEndDateTime
    , iftReadyDateTime

    -- * EbsVolume
    , EbsVolume (..)
    , mkEbsVolume
    , evDevice
    , evVolumeId

    -- * EbsBlockDeviceConfig
    , EbsBlockDeviceConfig (..)
    , mkEbsBlockDeviceConfig
    , ebdcVolumeSpecification
    , ebdcVolumesPerInstance

    -- * InstanceCollectionType
    , InstanceCollectionType (..)

    -- * InstanceFleetType
    , InstanceFleetType (..)

    -- * AutoScalingPolicyStateChangeReason
    , AutoScalingPolicyStateChangeReason (..)
    , mkAutoScalingPolicyStateChangeReason
    , aspscrCode
    , aspscrMessage

    -- * AdjustmentType
    , AdjustmentType (..)

    -- * NotebookExecutionSummary
    , NotebookExecutionSummary (..)
    , mkNotebookExecutionSummary
    , nesEditorId
    , nesEndTime
    , nesNotebookExecutionId
    , nesNotebookExecutionName
    , nesStartTime
    , nesStatus

    -- * StepStateChangeReason
    , StepStateChangeReason (..)
    , mkStepStateChangeReason
    , sscrCode
    , sscrMessage

    -- * RepoUpgradeOnBoot
    , RepoUpgradeOnBoot (..)

    -- * ClusterId
    , ClusterId (..)

    -- * SecurityConfigurationSummary
    , SecurityConfigurationSummary (..)
    , mkSecurityConfigurationSummary
    , scsCreationDateTime
    , scsName

    -- * KerberosAttributes
    , KerberosAttributes (..)
    , mkKerberosAttributes
    , kaRealm
    , kaKdcAdminPassword
    , kaADDomainJoinPassword
    , kaADDomainJoinUser
    , kaCrossRealmTrustPrincipalPassword

    -- * Marker
    , Marker (..)

    -- * InstanceFleetStateChangeReason
    , InstanceFleetStateChangeReason (..)
    , mkInstanceFleetStateChangeReason
    , ifscrCode
    , ifscrMessage

    -- * ClusterStateChangeReasonCode
    , ClusterStateChangeReasonCode (..)

    -- * AutoScalingPolicy
    , AutoScalingPolicy (..)
    , mkAutoScalingPolicy
    , aspConstraints
    , aspRules

    -- * InstanceFleetId
    , InstanceFleetId (..)

    -- * Step
    , Step (..)
    , mkStep
    , sfActionOnFailure
    , sfConfig
    , sfId
    , sfName
    , sfStatus

    -- * StepState
    , StepState (..)

    -- * ShrinkPolicy
    , ShrinkPolicy (..)
    , mkShrinkPolicy
    , spDecommissionTimeout
    , spInstanceResizePolicy

    -- * VolumeSpecification
    , VolumeSpecification (..)
    , mkVolumeSpecification
    , vsVolumeType
    , vsSizeInGB
    , vsIops

    -- * ScalingTrigger
    , ScalingTrigger (..)
    , mkScalingTrigger
    , stCloudWatchAlarmDefinition

    -- * InstanceGroupTimeline
    , InstanceGroupTimeline (..)
    , mkInstanceGroupTimeline
    , igtCreationDateTime
    , igtEndDateTime
    , igtReadyDateTime

    -- * InstanceStatus
    , InstanceStatus (..)
    , mkInstanceStatus
    , isState
    , isStateChangeReason
    , isTimeline

    -- * AutoScalingPolicyState
    , AutoScalingPolicyState (..)

    -- * InstanceRoleType
    , InstanceRoleType (..)

    -- * EbsBlockDevice
    , EbsBlockDevice (..)
    , mkEbsBlockDevice
    , ebdDevice
    , ebdVolumeSpecification

    -- * JobFlowInstancesConfig
    , JobFlowInstancesConfig (..)
    , mkJobFlowInstancesConfig
    , jficAdditionalMasterSecurityGroups
    , jficAdditionalSlaveSecurityGroups
    , jficEc2KeyName
    , jficEc2SubnetId
    , jficEc2SubnetIds
    , jficEmrManagedMasterSecurityGroup
    , jficEmrManagedSlaveSecurityGroup
    , jficHadoopVersion
    , jficInstanceCount
    , jficInstanceFleets
    , jficInstanceGroups
    , jficKeepJobFlowAliveWhenNoSteps
    , jficMasterInstanceType
    , jficPlacement
    , jficServiceAccessSecurityGroup
    , jficSlaveInstanceType
    , jficTerminationProtected

    -- * InstanceFleetModifyConfig
    , InstanceFleetModifyConfig (..)
    , mkInstanceFleetModifyConfig
    , ifmcInstanceFleetId
    , ifmcTargetOnDemandCapacity
    , ifmcTargetSpotCapacity

    -- * ArnType
    , ArnType (..)

    -- * InstanceFleetState
    , InstanceFleetState (..)

    -- * Configuration
    , Configuration (..)
    , mkConfiguration
    , cClassification
    , cConfigurations
    , cProperties

    -- * InstanceFleetProvisioningSpecifications
    , InstanceFleetProvisioningSpecifications (..)
    , mkInstanceFleetProvisioningSpecifications
    , ifpsOnDemandSpecification
    , ifpsSpotSpecification

    -- * Studio
    , Studio (..)
    , mkStudio
    , sgAuthMode
    , sgCreationTime
    , sgDefaultS3Location
    , sgDescription
    , sgEngineSecurityGroupId
    , sgName
    , sgServiceRole
    , sgStudioArn
    , sgStudioId
    , sgSubnetIds
    , sgTags
    , sgUrl
    , sgUserRole
    , sgVpcId
    , sgWorkspaceSecurityGroupId

    -- * ManagedScalingPolicy
    , ManagedScalingPolicy (..)
    , mkManagedScalingPolicy
    , mspComputeLimits

    -- * StepConfig
    , StepConfig (..)
    , mkStepConfig
    , scName
    , scHadoopJarStep
    , scActionOnFailure

    -- * InstanceGroupId
    , InstanceGroupId (..)

    -- * OptionalArnType
    , OptionalArnType (..)

    -- * ScalingConstraints
    , ScalingConstraints (..)
    , mkScalingConstraints
    , scMinCapacity
    , scMaxCapacity

    -- * SimpleScalingPolicyConfiguration
    , SimpleScalingPolicyConfiguration (..)
    , mkSimpleScalingPolicyConfiguration
    , sspcScalingAdjustment
    , sspcAdjustmentType
    , sspcCoolDown

    -- * ComputeLimits
    , ComputeLimits (..)
    , mkComputeLimits
    , clUnitType
    , clMinimumCapacityUnits
    , clMaximumCapacityUnits
    , clMaximumCoreCapacityUnits
    , clMaximumOnDemandCapacityUnits

    -- * InstanceGroup
    , InstanceGroup (..)
    , mkInstanceGroup
    , igAutoScalingPolicy
    , igBidPrice
    , igConfigurations
    , igConfigurationsVersion
    , igEbsBlockDevices
    , igEbsOptimized
    , igId
    , igInstanceGroupType
    , igInstanceType
    , igLastSuccessfullyAppliedConfigurations
    , igLastSuccessfullyAppliedConfigurationsVersion
    , igMarket
    , igName
    , igRequestedInstanceCount
    , igRunningInstanceCount
    , igShrinkPolicy
    , igStatus

    -- * BootstrapActionConfig
    , BootstrapActionConfig (..)
    , mkBootstrapActionConfig
    , bacName
    , bacScriptBootstrapAction

    -- * ClusterSummary
    , ClusterSummary (..)
    , mkClusterSummary
    , csClusterArn
    , csId
    , csName
    , csNormalizedInstanceHours
    , csOutpostArn
    , csStatus

    -- * ExecutionEngineConfig
    , ExecutionEngineConfig (..)
    , mkExecutionEngineConfig
    , eecId
    , eecMasterInstanceSecurityGroupId
    , eecType

    -- * Unit
    , Unit (..)

    -- * CloudWatchAlarmDefinition
    , CloudWatchAlarmDefinition (..)
    , mkCloudWatchAlarmDefinition
    , cwadComparisonOperator
    , cwadMetricName
    , cwadPeriod
    , cwadThreshold
    , cwadDimensions
    , cwadEvaluationPeriods
    , cwadNamespace
    , cwadStatistic
    , cwadUnit

    -- * ClusterStatus
    , ClusterStatus (..)
    , mkClusterStatus
    , csState
    , csStateChangeReason
    , csTimeline

    -- * InstanceState
    , InstanceState (..)

    -- * Statistic
    , Statistic (..)

    -- * BlockPublicAccessConfigurationMetadata
    , BlockPublicAccessConfigurationMetadata (..)
    , mkBlockPublicAccessConfigurationMetadata
    , bpacmCreationDateTime
    , bpacmCreatedByArn

    -- * ClusterTimeline
    , ClusterTimeline (..)
    , mkClusterTimeline
    , ctCreationDateTime
    , ctEndDateTime
    , ctReadyDateTime

    -- * InstanceStateChangeReasonCode
    , InstanceStateChangeReasonCode (..)

    -- * Instance
    , Instance (..)
    , mkInstance
    , iEbsVolumes
    , iEc2InstanceId
    , iId
    , iInstanceFleetId
    , iInstanceGroupId
    , iInstanceType
    , iMarket
    , iPrivateDnsName
    , iPrivateIpAddress
    , iPublicDnsName
    , iPublicIpAddress
    , iStatus

    -- * ScriptBootstrapActionConfig
    , ScriptBootstrapActionConfig (..)
    , mkScriptBootstrapActionConfig
    , sbacPath
    , sbacArgs

    -- * BidPrice
    , BidPrice (..)

    -- * Name
    , Name (..)

    -- * JobFlowId
    , JobFlowId (..)

    -- * ClusterArn
    , ClusterArn (..)

    -- * Id
    , Id (..)

    -- * StudioId
    , StudioId (..)

    -- * CustomAmiId
    , CustomAmiId (..)

    -- * OutpostArn
    , OutpostArn (..)

    -- * IdentityId
    , IdentityId (..)

    -- * IdentityName
    , IdentityName (..)

    -- * EditorId
    , EditorId (..)

    -- * NotebookExecutionName
    , NotebookExecutionName (..)

    -- * NotebookInstanceSecurityGroupId
    , NotebookInstanceSecurityGroupId (..)

    -- * SessionPolicyArn
    , SessionPolicyArn (..)

    -- * Arn
    , Arn (..)

    -- * NotebookExecutionId
    , NotebookExecutionId (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.EMR.Types.BlockPublicAccessConfiguration
  
import Network.AWS.EMR.Types.XmlString
  
import Network.AWS.EMR.Types.MarketType
  
import Network.AWS.EMR.Types.InstanceGroupConfig
  
import Network.AWS.EMR.Types.InstanceId
  
import Network.AWS.EMR.Types.SpotProvisioningTimeoutAction
  
import Network.AWS.EMR.Types.EbsConfiguration
  
import Network.AWS.EMR.Types.InstanceStateChangeReason
  
import Network.AWS.EMR.Types.PlacementGroupConfig
  
import Network.AWS.EMR.Types.KeyValue
  
import Network.AWS.EMR.Types.InstanceTypeSpecification
  
import Network.AWS.EMR.Types.NotebookExecutionStatus
  
import Network.AWS.EMR.Types.SupportedProductConfig
  
import Network.AWS.EMR.Types.Command
  
import Network.AWS.EMR.Types.StepId
  
import Network.AWS.EMR.Types.ActionOnFailure
  
import Network.AWS.EMR.Types.InstanceFleet
  
import Network.AWS.EMR.Types.ClusterStateChangeReason
  
  
import Network.AWS.EMR.Types.InstanceFleetStateChangeReasonCode
  
import Network.AWS.EMR.Types.ResourceId
  
import Network.AWS.EMR.Types.Tag
  
import Network.AWS.EMR.Types.Application
  
import Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode
  
import Network.AWS.EMR.Types.SpotProvisioningAllocationStrategy
  
import Network.AWS.EMR.Types.InstanceGroupStatus
  
import Network.AWS.EMR.Types.Cluster
  
import Network.AWS.EMR.Types.CancelStepsRequestStatus
  
import Network.AWS.EMR.Types.InstanceTimeline
  
import Network.AWS.EMR.Types.Ec2InstanceAttributes
  
import Network.AWS.EMR.Types.OnDemandProvisioningSpecification
  
import Network.AWS.EMR.Types.StepStateChangeReasonCode
  
import Network.AWS.EMR.Types.FailureDetails
  
import Network.AWS.EMR.Types.ClusterState
  
import Network.AWS.EMR.Types.InstanceFleetConfig
  
import Network.AWS.EMR.Types.ComputeLimitsUnitType
  
import Network.AWS.EMR.Types.PlacementGroupStrategy
  
import Network.AWS.EMR.Types.HadoopStepConfig
  
import Network.AWS.EMR.Types.SessionMappingDetail
  
import Network.AWS.EMR.Types.ScalingRule
  
import Network.AWS.EMR.Types.ScalingAction
  
import Network.AWS.EMR.Types.InstanceTypeConfig
  
import Network.AWS.EMR.Types.NotebookExecution
  
import Network.AWS.EMR.Types.AuthMode
  
import Network.AWS.EMR.Types.InstanceResizePolicy
  
import Network.AWS.EMR.Types.InstanceFleetStatus
  
import Network.AWS.EMR.Types.InstanceGroupStateChangeReason
  
import Network.AWS.EMR.Types.SessionMappingSummary
  
import Network.AWS.EMR.Types.InstanceGroupType
  
import Network.AWS.EMR.Types.AutoScalingPolicyStatus
  
import Network.AWS.EMR.Types.XmlStringMaxLen256
  
import Network.AWS.EMR.Types.StepCancellationOption
  
import Network.AWS.EMR.Types.IdentityType
  
import Network.AWS.EMR.Types.ScaleDownBehavior
  
import Network.AWS.EMR.Types.PortRange
  
import Network.AWS.EMR.Types.InstanceGroupStateChangeReasonCode
  
import Network.AWS.EMR.Types.StepStatus
  
import Network.AWS.EMR.Types.StepSummary
  
import Network.AWS.EMR.Types.InstanceGroupState
  
import Network.AWS.EMR.Types.StepTimeline
  
import Network.AWS.EMR.Types.PlacementType
  
import Network.AWS.EMR.Types.ComparisonOperator
  
import Network.AWS.EMR.Types.CancelStepsInfo
  
import Network.AWS.EMR.Types.InstanceType
  
import Network.AWS.EMR.Types.HadoopJarStepConfig
  
import Network.AWS.EMR.Types.MetricDimension
  
import Network.AWS.EMR.Types.ExecutionEngineType
  
import Network.AWS.EMR.Types.InstanceGroupModifyConfig
  
import Network.AWS.EMR.Types.StudioSummary
  
import Network.AWS.EMR.Types.SpotProvisioningSpecification
  
import Network.AWS.EMR.Types.AutoScalingPolicyDescription
  
import Network.AWS.EMR.Types.OnDemandProvisioningAllocationStrategy
  
import Network.AWS.EMR.Types.InstanceFleetTimeline
  
import Network.AWS.EMR.Types.EbsVolume
  
import Network.AWS.EMR.Types.EbsBlockDeviceConfig
  
import Network.AWS.EMR.Types.InstanceCollectionType
  
import Network.AWS.EMR.Types.InstanceFleetType
  
import Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
  
import Network.AWS.EMR.Types.AdjustmentType
  
import Network.AWS.EMR.Types.NotebookExecutionSummary
  
import Network.AWS.EMR.Types.StepStateChangeReason
  
import Network.AWS.EMR.Types.RepoUpgradeOnBoot
  
import Network.AWS.EMR.Types.ClusterId
  
import Network.AWS.EMR.Types.SecurityConfigurationSummary
  
import Network.AWS.EMR.Types.KerberosAttributes
  
import Network.AWS.EMR.Types.Marker
  
import Network.AWS.EMR.Types.InstanceFleetStateChangeReason
  
  
import Network.AWS.EMR.Types.ClusterStateChangeReasonCode
  
import Network.AWS.EMR.Types.AutoScalingPolicy
  
import Network.AWS.EMR.Types.InstanceFleetId
  
import Network.AWS.EMR.Types.Step
  
import Network.AWS.EMR.Types.StepState
  
import Network.AWS.EMR.Types.ShrinkPolicy
  
import Network.AWS.EMR.Types.VolumeSpecification
  
import Network.AWS.EMR.Types.ScalingTrigger
  
import Network.AWS.EMR.Types.InstanceGroupTimeline
  
import Network.AWS.EMR.Types.InstanceStatus
  
import Network.AWS.EMR.Types.AutoScalingPolicyState
  
import Network.AWS.EMR.Types.InstanceRoleType
  
import Network.AWS.EMR.Types.EbsBlockDevice
  
  
import Network.AWS.EMR.Types.JobFlowInstancesConfig
  
import Network.AWS.EMR.Types.InstanceFleetModifyConfig
  
import Network.AWS.EMR.Types.ArnType
  
import Network.AWS.EMR.Types.InstanceFleetState
  
import Network.AWS.EMR.Types.Configuration
  
import Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
  
import Network.AWS.EMR.Types.Studio
  
import Network.AWS.EMR.Types.ManagedScalingPolicy
  
import Network.AWS.EMR.Types.StepConfig
  
import Network.AWS.EMR.Types.InstanceGroupId
  
import Network.AWS.EMR.Types.OptionalArnType
  
import Network.AWS.EMR.Types.ScalingConstraints
  
import Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
  
import Network.AWS.EMR.Types.ComputeLimits
  
import Network.AWS.EMR.Types.InstanceGroup
  
import Network.AWS.EMR.Types.BootstrapActionConfig
  
import Network.AWS.EMR.Types.ClusterSummary
  
import Network.AWS.EMR.Types.ExecutionEngineConfig
  
import Network.AWS.EMR.Types.Unit
  
import Network.AWS.EMR.Types.CloudWatchAlarmDefinition
  
import Network.AWS.EMR.Types.ClusterStatus
  
import Network.AWS.EMR.Types.InstanceState
  
import Network.AWS.EMR.Types.Statistic
  
import Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata
  
import Network.AWS.EMR.Types.ClusterTimeline
  
import Network.AWS.EMR.Types.InstanceStateChangeReasonCode
  
import Network.AWS.EMR.Types.Instance
  
import Network.AWS.EMR.Types.ScriptBootstrapActionConfig
  
import Network.AWS.EMR.Types.BidPrice
  
import Network.AWS.EMR.Types.Name
  
import Network.AWS.EMR.Types.JobFlowId
  
import Network.AWS.EMR.Types.ClusterArn
  
import Network.AWS.EMR.Types.Id
  
import Network.AWS.EMR.Types.StudioId
  
import Network.AWS.EMR.Types.CustomAmiId
  
import Network.AWS.EMR.Types.OutpostArn
  
import Network.AWS.EMR.Types.IdentityId
  
import Network.AWS.EMR.Types.IdentityName
  
import Network.AWS.EMR.Types.EditorId
  
import Network.AWS.EMR.Types.NotebookExecutionName
  
import Network.AWS.EMR.Types.NotebookInstanceSecurityGroupId
  
import Network.AWS.EMR.Types.SessionPolicyArn
  
import Network.AWS.EMR.Types.Arn
  
import Network.AWS.EMR.Types.NotebookExecutionId
  

-- | API version @2009-03-31@ of the Amazon Elastic MapReduce SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "EMR", Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "elasticmapreduce",
                 Core._svcVersion = "2009-03-31", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "EMR",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | This exception occurs when there is something wrong with user input.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException
  = Core._MatchServiceError mkServiceConfig "InvalidRequestException"
{-# INLINEABLE _InvalidRequestException #-}
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that an error occurred while processing the request and that the request was not completed.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError
  = Core._MatchServiceError mkServiceConfig "InternalServerError"
{-# INLINEABLE _InternalServerError #-}
{-# DEPRECATED _InternalServerError "Use generic-lens or generic-optics instead"  #-}

-- | This exception occurs when there is an internal failure in the EMR service.
_InternalServerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerException
  = Core._MatchServiceError mkServiceConfig "InternalServerException"
{-# INLINEABLE _InternalServerException #-}
{-# DEPRECATED _InternalServerException "Use generic-lens or generic-optics instead"  #-}
