{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon EMR is a web service that makes it easy to process large amounts of data efficiently. Amazon EMR uses Hadoop processing combined with several AWS products to do tasks such as web indexing, data mining, log file analysis, machine learning, scientific simulation, and data warehousing.
--
--
module Network.AWS.EMR
    (
    -- * Service Configuration
      emr

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** InternalServerError
    , _InternalServerError

    -- ** InternalServerException
    , _InternalServerException

    -- * Waiters
    -- $waiters

    -- ** StepComplete
    , stepComplete

    -- ** ClusterTerminated
    , clusterTerminated

    -- ** ClusterRunning
    , clusterRunning

    -- * Operations
    -- $operations

    -- ** RunJobFlow
    , module Network.AWS.EMR.RunJobFlow

    -- ** RemoveAutoScalingPolicy
    , module Network.AWS.EMR.RemoveAutoScalingPolicy

    -- ** SetVisibleToAllUsers
    , module Network.AWS.EMR.SetVisibleToAllUsers

    -- ** TerminateJobFlows
    , module Network.AWS.EMR.TerminateJobFlows

    -- ** DescribeStep
    , module Network.AWS.EMR.DescribeStep

    -- ** RemoveTags
    , module Network.AWS.EMR.RemoveTags

    -- ** DescribeCluster
    , module Network.AWS.EMR.DescribeCluster

    -- ** ListSecurityConfigurations
    , module Network.AWS.EMR.ListSecurityConfigurations

    -- ** CancelSteps
    , module Network.AWS.EMR.CancelSteps

    -- ** CreateSecurityConfiguration
    , module Network.AWS.EMR.CreateSecurityConfiguration

    -- ** SetTerminationProtection
    , module Network.AWS.EMR.SetTerminationProtection

    -- ** AddJobFlowSteps
    , module Network.AWS.EMR.AddJobFlowSteps

    -- ** ModifyInstanceGroups
    , module Network.AWS.EMR.ModifyInstanceGroups

    -- ** ListSteps (Paginated)
    , module Network.AWS.EMR.ListSteps

    -- ** AddInstanceFleet
    , module Network.AWS.EMR.AddInstanceFleet

    -- ** AddInstanceGroups
    , module Network.AWS.EMR.AddInstanceGroups

    -- ** DeleteSecurityConfiguration
    , module Network.AWS.EMR.DeleteSecurityConfiguration

    -- ** ModifyInstanceFleet
    , module Network.AWS.EMR.ModifyInstanceFleet

    -- ** ListInstanceGroups (Paginated)
    , module Network.AWS.EMR.ListInstanceGroups

    -- ** ListBootstrapActions (Paginated)
    , module Network.AWS.EMR.ListBootstrapActions

    -- ** AddTags
    , module Network.AWS.EMR.AddTags

    -- ** ListInstances (Paginated)
    , module Network.AWS.EMR.ListInstances

    -- ** PutAutoScalingPolicy
    , module Network.AWS.EMR.PutAutoScalingPolicy

    -- ** ListClusters (Paginated)
    , module Network.AWS.EMR.ListClusters

    -- ** DescribeSecurityConfiguration
    , module Network.AWS.EMR.DescribeSecurityConfiguration

    -- ** ListInstanceFleets (Paginated)
    , module Network.AWS.EMR.ListInstanceFleets

    -- * Types

    -- ** ActionOnFailure
    , ActionOnFailure (..)

    -- ** AdjustmentType
    , AdjustmentType (..)

    -- ** AutoScalingPolicyState
    , AutoScalingPolicyState (..)

    -- ** AutoScalingPolicyStateChangeReasonCode
    , AutoScalingPolicyStateChangeReasonCode (..)

    -- ** CancelStepsRequestStatus
    , CancelStepsRequestStatus (..)

    -- ** ClusterState
    , ClusterState (..)

    -- ** ClusterStateChangeReasonCode
    , ClusterStateChangeReasonCode (..)

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** InstanceCollectionType
    , InstanceCollectionType (..)

    -- ** InstanceFleetState
    , InstanceFleetState (..)

    -- ** InstanceFleetStateChangeReasonCode
    , InstanceFleetStateChangeReasonCode (..)

    -- ** InstanceFleetType
    , InstanceFleetType (..)

    -- ** InstanceGroupState
    , InstanceGroupState (..)

    -- ** InstanceGroupStateChangeReasonCode
    , InstanceGroupStateChangeReasonCode (..)

    -- ** InstanceGroupType
    , InstanceGroupType (..)

    -- ** InstanceRoleType
    , InstanceRoleType (..)

    -- ** InstanceState
    , InstanceState (..)

    -- ** InstanceStateChangeReasonCode
    , InstanceStateChangeReasonCode (..)

    -- ** MarketType
    , MarketType (..)

    -- ** RepoUpgradeOnBoot
    , RepoUpgradeOnBoot (..)

    -- ** ScaleDownBehavior
    , ScaleDownBehavior (..)

    -- ** SpotProvisioningTimeoutAction
    , SpotProvisioningTimeoutAction (..)

    -- ** Statistic
    , Statistic (..)

    -- ** StepState
    , StepState (..)

    -- ** StepStateChangeReasonCode
    , StepStateChangeReasonCode (..)

    -- ** Unit
    , Unit (..)

    -- ** Application
    , Application
    , application
    , aArgs
    , aAdditionalInfo
    , aName
    , aVersion

    -- ** AutoScalingPolicy
    , AutoScalingPolicy
    , autoScalingPolicy
    , aspConstraints
    , aspRules

    -- ** AutoScalingPolicyDescription
    , AutoScalingPolicyDescription
    , autoScalingPolicyDescription
    , aspdStatus
    , aspdRules
    , aspdConstraints

    -- ** AutoScalingPolicyStateChangeReason
    , AutoScalingPolicyStateChangeReason
    , autoScalingPolicyStateChangeReason
    , aspscrCode
    , aspscrMessage

    -- ** AutoScalingPolicyStatus
    , AutoScalingPolicyStatus
    , autoScalingPolicyStatus
    , aspsState
    , aspsStateChangeReason

    -- ** BootstrapActionConfig
    , BootstrapActionConfig
    , bootstrapActionConfig
    , bacName
    , bacScriptBootstrapAction

    -- ** CancelStepsInfo
    , CancelStepsInfo
    , cancelStepsInfo
    , csiStatus
    , csiStepId
    , csiReason

    -- ** CloudWatchAlarmDefinition
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

    -- ** Cluster
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

    -- ** ClusterStateChangeReason
    , ClusterStateChangeReason
    , clusterStateChangeReason
    , cscrCode
    , cscrMessage

    -- ** ClusterStatus
    , ClusterStatus
    , clusterStatus
    , csState
    , csStateChangeReason
    , csTimeline

    -- ** ClusterSummary
    , ClusterSummary
    , clusterSummary
    , csStatus
    , csNormalizedInstanceHours
    , csName
    , csId

    -- ** ClusterTimeline
    , ClusterTimeline
    , clusterTimeline
    , ctReadyDateTime
    , ctCreationDateTime
    , ctEndDateTime

    -- ** Command
    , Command
    , command
    , cArgs
    , cScriptPath
    , cName

    -- ** Configuration
    , Configuration
    , configuration
    , cConfigurations
    , cClassification
    , cProperties

    -- ** EBSBlockDevice
    , EBSBlockDevice
    , ebsBlockDevice
    , ebdDevice
    , ebdVolumeSpecification

    -- ** EBSBlockDeviceConfig
    , EBSBlockDeviceConfig
    , ebsBlockDeviceConfig
    , ebdcVolumesPerInstance
    , ebdcVolumeSpecification

    -- ** EBSConfiguration
    , EBSConfiguration
    , ebsConfiguration
    , ecEBSOptimized
    , ecEBSBlockDeviceConfigs

    -- ** EBSVolume
    , EBSVolume
    , ebsVolume
    , evDevice
    , evVolumeId

    -- ** EC2InstanceAttributes
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

    -- ** FailureDetails
    , FailureDetails
    , failureDetails
    , fdLogFile
    , fdReason
    , fdMessage

    -- ** HadoopJARStepConfig
    , HadoopJARStepConfig
    , hadoopJARStepConfig
    , hjscArgs
    , hjscMainClass
    , hjscProperties
    , hjscJAR

    -- ** HadoopStepConfig
    , HadoopStepConfig
    , hadoopStepConfig
    , hscArgs
    , hscJAR
    , hscMainClass
    , hscProperties

    -- ** Instance
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

    -- ** InstanceFleet
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

    -- ** InstanceFleetConfig
    , InstanceFleetConfig
    , instanceFleetConfig
    , ifcInstanceTypeConfigs
    , ifcTargetOnDemandCapacity
    , ifcName
    , ifcTargetSpotCapacity
    , ifcLaunchSpecifications
    , ifcInstanceFleetType

    -- ** InstanceFleetModifyConfig
    , InstanceFleetModifyConfig
    , instanceFleetModifyConfig
    , ifmcTargetOnDemandCapacity
    , ifmcTargetSpotCapacity
    , ifmcInstanceFleetId

    -- ** InstanceFleetProvisioningSpecifications
    , InstanceFleetProvisioningSpecifications
    , instanceFleetProvisioningSpecifications
    , ifpsSpotSpecification

    -- ** InstanceFleetStateChangeReason
    , InstanceFleetStateChangeReason
    , instanceFleetStateChangeReason
    , ifscrCode
    , ifscrMessage

    -- ** InstanceFleetStatus
    , InstanceFleetStatus
    , instanceFleetStatus
    , ifsState
    , ifsStateChangeReason
    , ifsTimeline

    -- ** InstanceFleetTimeline
    , InstanceFleetTimeline
    , instanceFleetTimeline
    , iftReadyDateTime
    , iftCreationDateTime
    , iftEndDateTime

    -- ** InstanceGroup
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

    -- ** InstanceGroupConfig
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

    -- ** InstanceGroupModifyConfig
    , InstanceGroupModifyConfig
    , instanceGroupModifyConfig
    , igmcInstanceCount
    , igmcEC2InstanceIdsToTerminate
    , igmcShrinkPolicy
    , igmcInstanceGroupId

    -- ** InstanceGroupStateChangeReason
    , InstanceGroupStateChangeReason
    , instanceGroupStateChangeReason
    , igscrCode
    , igscrMessage

    -- ** InstanceGroupStatus
    , InstanceGroupStatus
    , instanceGroupStatus
    , igsState
    , igsStateChangeReason
    , igsTimeline

    -- ** InstanceGroupTimeline
    , InstanceGroupTimeline
    , instanceGroupTimeline
    , igtReadyDateTime
    , igtCreationDateTime
    , igtEndDateTime

    -- ** InstanceResizePolicy
    , InstanceResizePolicy
    , instanceResizePolicy
    , irpInstancesToProtect
    , irpInstancesToTerminate
    , irpInstanceTerminationTimeout

    -- ** InstanceStateChangeReason
    , InstanceStateChangeReason
    , instanceStateChangeReason
    , iscrCode
    , iscrMessage

    -- ** InstanceStatus
    , InstanceStatus
    , instanceStatus
    , isState
    , isStateChangeReason
    , isTimeline

    -- ** InstanceTimeline
    , InstanceTimeline
    , instanceTimeline
    , itReadyDateTime
    , itCreationDateTime
    , itEndDateTime

    -- ** InstanceTypeConfig
    , InstanceTypeConfig
    , instanceTypeConfig
    , itcEBSConfiguration
    , itcBidPrice
    , itcWeightedCapacity
    , itcConfigurations
    , itcBidPriceAsPercentageOfOnDemandPrice
    , itcInstanceType

    -- ** InstanceTypeSpecification
    , InstanceTypeSpecification
    , instanceTypeSpecification
    , itsBidPrice
    , itsWeightedCapacity
    , itsConfigurations
    , itsEBSBlockDevices
    , itsInstanceType
    , itsEBSOptimized
    , itsBidPriceAsPercentageOfOnDemandPrice

    -- ** JobFlowInstancesConfig
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

    -- ** KerberosAttributes
    , KerberosAttributes
    , kerberosAttributes
    , kaADDomainJoinPassword
    , kaCrossRealmTrustPrincipalPassword
    , kaADDomainJoinUser
    , kaRealm
    , kaKdcAdminPassword

    -- ** KeyValue
    , KeyValue
    , keyValue
    , kvValue
    , kvKey

    -- ** MetricDimension
    , MetricDimension
    , metricDimension
    , mdValue
    , mdKey

    -- ** PlacementType
    , PlacementType
    , placementType
    , ptAvailabilityZones
    , ptAvailabilityZone

    -- ** ScalingAction
    , ScalingAction
    , scalingAction
    , saMarket
    , saSimpleScalingPolicyConfiguration

    -- ** ScalingConstraints
    , ScalingConstraints
    , scalingConstraints
    , scMinCapacity
    , scMaxCapacity

    -- ** ScalingRule
    , ScalingRule
    , scalingRule
    , srDescription
    , srName
    , srAction
    , srTrigger

    -- ** ScalingTrigger
    , ScalingTrigger
    , scalingTrigger
    , stCloudWatchAlarmDefinition

    -- ** ScriptBootstrapActionConfig
    , ScriptBootstrapActionConfig
    , scriptBootstrapActionConfig
    , sbacArgs
    , sbacPath

    -- ** SecurityConfigurationSummary
    , SecurityConfigurationSummary
    , securityConfigurationSummary
    , scsName
    , scsCreationDateTime

    -- ** ShrinkPolicy
    , ShrinkPolicy
    , shrinkPolicy
    , spDecommissionTimeout
    , spInstanceResizePolicy

    -- ** SimpleScalingPolicyConfiguration
    , SimpleScalingPolicyConfiguration
    , simpleScalingPolicyConfiguration
    , sspcAdjustmentType
    , sspcCoolDown
    , sspcScalingAdjustment

    -- ** SpotProvisioningSpecification
    , SpotProvisioningSpecification
    , spotProvisioningSpecification
    , spsBlockDurationMinutes
    , spsTimeoutDurationMinutes
    , spsTimeoutAction

    -- ** Step
    , Step
    , step
    , sStatus
    , sActionOnFailure
    , sConfig
    , sName
    , sId

    -- ** StepConfig
    , StepConfig
    , stepConfig
    , scActionOnFailure
    , scName
    , scHadoopJARStep

    -- ** StepStateChangeReason
    , StepStateChangeReason
    , stepStateChangeReason
    , sscrCode
    , sscrMessage

    -- ** StepStatus
    , StepStatus
    , stepStatus
    , ssState
    , ssFailureDetails
    , ssStateChangeReason
    , ssTimeline

    -- ** StepSummary
    , StepSummary
    , stepSummary
    , ssStatus
    , ssActionOnFailure
    , ssConfig
    , ssName
    , ssId

    -- ** StepTimeline
    , StepTimeline
    , stepTimeline
    , stCreationDateTime
    , stEndDateTime
    , stStartDateTime

    -- ** SupportedProductConfig
    , SupportedProductConfig
    , supportedProductConfig
    , spcArgs
    , spcName

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** VolumeSpecification
    , VolumeSpecification
    , volumeSpecification
    , vsIOPS
    , vsVolumeType
    , vsSizeInGB
    ) where

import Network.AWS.EMR.AddInstanceFleet
import Network.AWS.EMR.AddInstanceGroups
import Network.AWS.EMR.AddJobFlowSteps
import Network.AWS.EMR.AddTags
import Network.AWS.EMR.CancelSteps
import Network.AWS.EMR.CreateSecurityConfiguration
import Network.AWS.EMR.DeleteSecurityConfiguration
import Network.AWS.EMR.DescribeCluster
import Network.AWS.EMR.DescribeSecurityConfiguration
import Network.AWS.EMR.DescribeStep
import Network.AWS.EMR.ListBootstrapActions
import Network.AWS.EMR.ListClusters
import Network.AWS.EMR.ListInstanceFleets
import Network.AWS.EMR.ListInstanceGroups
import Network.AWS.EMR.ListInstances
import Network.AWS.EMR.ListSecurityConfigurations
import Network.AWS.EMR.ListSteps
import Network.AWS.EMR.ModifyInstanceFleet
import Network.AWS.EMR.ModifyInstanceGroups
import Network.AWS.EMR.PutAutoScalingPolicy
import Network.AWS.EMR.RemoveAutoScalingPolicy
import Network.AWS.EMR.RemoveTags
import Network.AWS.EMR.RunJobFlow
import Network.AWS.EMR.SetTerminationProtection
import Network.AWS.EMR.SetVisibleToAllUsers
import Network.AWS.EMR.TerminateJobFlows
import Network.AWS.EMR.Types
import Network.AWS.EMR.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'EMR'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
