{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Elastic MapReduce (Amazon EMR) is a web service that makes it
-- easy to process large amounts of data efficiently. Amazon EMR uses
-- Hadoop processing combined with several AWS products to do tasks such as
-- web indexing, data mining, log file analysis, machine learning,
-- scientific simulation, and data warehousing.
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

    -- ** ClusterRunning
    , clusterRunning

    -- * Operations
    -- $operations

    -- ** RunJobFlow
    , module Network.AWS.EMR.RunJobFlow

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

    -- ** SetTerminationProtection
    , module Network.AWS.EMR.SetTerminationProtection

    -- ** AddJobFlowSteps
    , module Network.AWS.EMR.AddJobFlowSteps

    -- ** ModifyInstanceGroups
    , module Network.AWS.EMR.ModifyInstanceGroups

    -- ** ListSteps (Paginated)
    , module Network.AWS.EMR.ListSteps

    -- ** AddInstanceGroups
    , module Network.AWS.EMR.AddInstanceGroups

    -- ** ListInstanceGroups (Paginated)
    , module Network.AWS.EMR.ListInstanceGroups

    -- ** ListBootstrapActions (Paginated)
    , module Network.AWS.EMR.ListBootstrapActions

    -- ** AddTags
    , module Network.AWS.EMR.AddTags

    -- ** ListInstances (Paginated)
    , module Network.AWS.EMR.ListInstances

    -- ** ListClusters (Paginated)
    , module Network.AWS.EMR.ListClusters

    -- * Types

    -- ** ActionOnFailure
    , ActionOnFailure (..)

    -- ** ClusterState
    , ClusterState (..)

    -- ** ClusterStateChangeReasonCode
    , ClusterStateChangeReasonCode (..)

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

    -- ** StepState
    , StepState (..)

    -- ** StepStateChangeReasonCode
    , StepStateChangeReasonCode (..)

    -- ** Application
    , Application
    , application
    , aArgs
    , aAdditionalInfo
    , aName
    , aVersion

    -- ** BootstrapActionConfig
    , BootstrapActionConfig
    , bootstrapActionConfig
    , bacName
    , bacScriptBootstrapAction

    -- ** Cluster
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
    , eiaAdditionalMasterSecurityGroups
    , eiaIAMInstanceProfile
    , eiaEmrManagedMasterSecurityGroup
    , eiaEC2SubnetId
    , eiaServiceAccessSecurityGroup
    , eiaEC2AvailabilityZone

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
    , iPrivateIPAddress
    , iId
    , iInstanceGroupId
    , iPrivateDNSName
    , iPublicIPAddress

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
    , igId

    -- ** InstanceGroupConfig
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

    -- ** InstanceGroupModifyConfig
    , InstanceGroupModifyConfig
    , instanceGroupModifyConfig
    , igmcInstanceCount
    , igmcEC2InstanceIdsToTerminate
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

    -- ** JobFlowInstancesConfig
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

    -- ** KeyValue
    , KeyValue
    , keyValue
    , kvValue
    , kvKey

    -- ** PlacementType
    , PlacementType
    , placementType
    , ptAvailabilityZone

    -- ** ScriptBootstrapActionConfig
    , ScriptBootstrapActionConfig
    , scriptBootstrapActionConfig
    , sbacArgs
    , sbacPath

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

import           Network.AWS.EMR.AddInstanceGroups
import           Network.AWS.EMR.AddJobFlowSteps
import           Network.AWS.EMR.AddTags
import           Network.AWS.EMR.DescribeCluster
import           Network.AWS.EMR.DescribeStep
import           Network.AWS.EMR.ListBootstrapActions
import           Network.AWS.EMR.ListClusters
import           Network.AWS.EMR.ListInstanceGroups
import           Network.AWS.EMR.ListInstances
import           Network.AWS.EMR.ListSteps
import           Network.AWS.EMR.ModifyInstanceGroups
import           Network.AWS.EMR.RemoveTags
import           Network.AWS.EMR.RunJobFlow
import           Network.AWS.EMR.SetTerminationProtection
import           Network.AWS.EMR.SetVisibleToAllUsers
import           Network.AWS.EMR.TerminateJobFlows
import           Network.AWS.EMR.Types
import           Network.AWS.EMR.Waiters

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
