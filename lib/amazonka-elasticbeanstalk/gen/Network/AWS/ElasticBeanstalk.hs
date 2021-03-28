{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Elastic Beanstalk__ 
--
-- AWS Elastic Beanstalk makes it easy for you to create, deploy, and manage scalable, fault-tolerant applications running on the Amazon Web Services cloud.
-- For more information about this product, go to the <http://aws.amazon.com/elasticbeanstalk/ AWS Elastic Beanstalk> details page. The location of the latest AWS Elastic Beanstalk WSDL is <https://elasticbeanstalk.s3.amazonaws.com/doc/2010-12-01/AWSElasticBeanstalk.wsdl https://elasticbeanstalk.s3.amazonaws.com/doc/2010-12-01/AWSElasticBeanstalk.wsdl> . To install the Software Development Kits (SDKs), Integrated Development Environment (IDE) Toolkits, and command line tools that enable you to access the API, go to <http://aws.amazon.com/tools/ Tools for Amazon Web Services> .
-- __Endpoints__ 
-- For a list of region-specific endpoints that AWS Elastic Beanstalk supports, go to <https://docs.aws.amazon.com/general/latest/gr/rande.html#elasticbeanstalk_region Regions and Endpoints> in the /Amazon Web Services Glossary/ .
module Network.AWS.ElasticBeanstalk
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** TooManyBucketsException
    , _TooManyBucketsException

    -- ** S3SubscriptionRequiredException
    , _S3SubscriptionRequiredException

    -- ** OperationInProgressException
    , _OperationInProgressException

    -- ** PlatformVersionStillReferencedException
    , _PlatformVersionStillReferencedException

    -- ** TooManyApplicationVersionsException
    , _TooManyApplicationVersionsException

    -- ** TooManyConfigurationTemplatesException
    , _TooManyConfigurationTemplatesException

    -- ** ResourceTypeNotSupportedException
    , _ResourceTypeNotSupportedException

    -- ** InsufficientPrivilegesException
    , _InsufficientPrivilegesException

    -- ** ElasticBeanstalkServiceException
    , _ElasticBeanstalkServiceException

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** TooManyApplicationsException
    , _TooManyApplicationsException

    -- ** TooManyPlatformsException
    , _TooManyPlatformsException

    -- ** ManagedActionInvalidStateException
    , _ManagedActionInvalidStateException

    -- ** SourceBundleDeletionException
    , _SourceBundleDeletionException

    -- ** S3LocationNotInServiceRegionException
    , _S3LocationNotInServiceRegionException

    -- ** CodeBuildNotInServiceRegionException
    , _CodeBuildNotInServiceRegionException

    -- ** TooManyEnvironmentsException
    , _TooManyEnvironmentsException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- ** EnvironmentExists
    , mkEnvironmentExists

    -- ** EnvironmentUpdated
    , mkEnvironmentUpdated

    -- ** EnvironmentTerminated
    , mkEnvironmentTerminated

    -- * Operations
    -- $operations

    -- ** DescribeApplications 
    , module Network.AWS.ElasticBeanstalk.DescribeApplications

    -- ** UpdateEnvironment 
    , module Network.AWS.ElasticBeanstalk.UpdateEnvironment

    -- ** TerminateEnvironment 
    , module Network.AWS.ElasticBeanstalk.TerminateEnvironment

    -- ** ListPlatformVersions (Paginated)
    , module Network.AWS.ElasticBeanstalk.ListPlatformVersions

    -- ** DeletePlatformVersion 
    , module Network.AWS.ElasticBeanstalk.DeletePlatformVersion

    -- ** CreateApplicationVersion 
    , module Network.AWS.ElasticBeanstalk.CreateApplicationVersion

    -- ** ListPlatformBranches 
    , module Network.AWS.ElasticBeanstalk.ListPlatformBranches

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.ElasticBeanstalk.DescribeEvents

    -- ** RequestEnvironmentInfo 
    , module Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo

    -- ** ListTagsForResource 
    , module Network.AWS.ElasticBeanstalk.ListTagsForResource

    -- ** RetrieveEnvironmentInfo 
    , module Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo

    -- ** DescribePlatformVersion 
    , module Network.AWS.ElasticBeanstalk.DescribePlatformVersion

    -- ** DeleteApplication 
    , module Network.AWS.ElasticBeanstalk.DeleteApplication

    -- ** UpdateApplication 
    , module Network.AWS.ElasticBeanstalk.UpdateApplication

    -- ** DescribeInstancesHealth 
    , module Network.AWS.ElasticBeanstalk.DescribeInstancesHealth

    -- ** CreateApplication 
    , module Network.AWS.ElasticBeanstalk.CreateApplication

    -- ** ComposeEnvironments 
    , module Network.AWS.ElasticBeanstalk.ComposeEnvironments

    -- ** AbortEnvironmentUpdate 
    , module Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate

    -- ** DeleteConfigurationTemplate 
    , module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate

    -- ** UpdateConfigurationTemplate 
    , module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate

    -- ** UpdateTagsForResource 
    , module Network.AWS.ElasticBeanstalk.UpdateTagsForResource

    -- ** DescribeEnvironmentResources 
    , module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources

    -- ** DescribeEnvironmentManagedActionHistory (Paginated)
    , module Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory

    -- ** DeleteApplicationVersion 
    , module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion

    -- ** UpdateApplicationVersion 
    , module Network.AWS.ElasticBeanstalk.UpdateApplicationVersion

    -- ** CreateConfigurationTemplate 
    , module Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate

    -- ** DescribeEnvironmentHealth 
    , module Network.AWS.ElasticBeanstalk.DescribeEnvironmentHealth

    -- ** RebuildEnvironment 
    , module Network.AWS.ElasticBeanstalk.RebuildEnvironment

    -- ** DeleteEnvironmentConfiguration 
    , module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration

    -- ** UpdateApplicationResourceLifecycle 
    , module Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle

    -- ** SwapEnvironmentCNAMEs 
    , module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs

    -- ** ListAvailableSolutionStacks 
    , module Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks

    -- ** ApplyEnvironmentManagedAction 
    , module Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction

    -- ** DescribeConfigurationOptions 
    , module Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions

    -- ** DisassociateEnvironmentOperationsRole 
    , module Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole

    -- ** CreateStorageLocation 
    , module Network.AWS.ElasticBeanstalk.CreateStorageLocation

    -- ** DescribeEnvironmentManagedActions 
    , module Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActions

    -- ** DescribeConfigurationSettings 
    , module Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings

    -- ** ValidateConfigurationSettings 
    , module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings

    -- ** DescribeAccountAttributes 
    , module Network.AWS.ElasticBeanstalk.DescribeAccountAttributes

    -- ** AssociateEnvironmentOperationsRole 
    , module Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole

    -- ** RestartAppServer 
    , module Network.AWS.ElasticBeanstalk.RestartAppServer

    -- ** DescribeEnvironments (Paginated)
    , module Network.AWS.ElasticBeanstalk.DescribeEnvironments

    -- ** CheckDNSAvailability 
    , module Network.AWS.ElasticBeanstalk.CheckDNSAvailability

    -- ** DescribeApplicationVersions (Paginated)
    , module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions

    -- ** CreateEnvironment 
    , module Network.AWS.ElasticBeanstalk.CreateEnvironment

    -- ** CreatePlatformVersion 
    , module Network.AWS.ElasticBeanstalk.CreatePlatformVersion

    -- * Types

    -- ** RequestId
    , RequestId (..)

    -- ** InstanceId
    , InstanceId (..)

    -- ** ApplicationResourceLifecycleConfig
    , ApplicationResourceLifecycleConfig (..)
    , mkApplicationResourceLifecycleConfig
    , arlcServiceRole
    , arlcVersionLifecycleConfig

    -- ** ApplicationVersionLifecycleConfig
    , ApplicationVersionLifecycleConfig (..)
    , mkApplicationVersionLifecycleConfig
    , avlcMaxAgeRule
    , avlcMaxCountRule

    -- ** PlatformProgrammingLanguage
    , PlatformProgrammingLanguage (..)
    , mkPlatformProgrammingLanguage
    , pplName
    , pplVersion

    -- ** PlatformFilterValue
    , PlatformFilterValue (..)

    -- ** ApplicationDescription
    , ApplicationDescription (..)
    , mkApplicationDescription
    , adApplicationArn
    , adApplicationName
    , adConfigurationTemplates
    , adDateCreated
    , adDateUpdated
    , adDescription
    , adResourceLifecycleConfig
    , adVersions

    -- ** SupportedTier
    , SupportedTier (..)

    -- ** PlatformFilterType
    , PlatformFilterType (..)

    -- ** ResourceQuotas
    , ResourceQuotas (..)
    , mkResourceQuotas
    , rqApplicationQuota
    , rqApplicationVersionQuota
    , rqConfigurationTemplateQuota
    , rqCustomPlatformQuota
    , rqEnvironmentQuota

    -- ** PlatformCategory
    , PlatformCategory (..)

    -- ** VirtualizationType
    , VirtualizationType (..)

    -- ** EventSeverity
    , EventSeverity (..)

    -- ** PlatformBranchSummary
    , PlatformBranchSummary (..)
    , mkPlatformBranchSummary
    , pbsBranchName
    , pbsBranchOrder
    , pbsLifecycleState
    , pbsPlatformName
    , pbsSupportedTierList

    -- ** ApplicationArn
    , ApplicationArn (..)

    -- ** ResourceId
    , ResourceId (..)

    -- ** S3Key
    , S3Key (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** InstancesHealthAttribute
    , InstancesHealthAttribute (..)

    -- ** PlatformBranchLifecycleState
    , PlatformBranchLifecycleState (..)

    -- ** EventDescription
    , EventDescription (..)
    , mkEventDescription
    , edApplicationName
    , edEnvironmentName
    , edEventDate
    , edMessage
    , edPlatformArn
    , edRequestId
    , edSeverity
    , edTemplateName
    , edVersionLabel

    -- ** DNSCname
    , DNSCname (..)

    -- ** LaunchConfiguration
    , LaunchConfiguration (..)
    , mkLaunchConfiguration
    , lcName

    -- ** ApplicationVersionDescriptionMessage
    , ApplicationVersionDescriptionMessage (..)
    , mkApplicationVersionDescriptionMessage
    , avdmApplicationVersion

    -- ** AutoScalingGroup
    , AutoScalingGroup (..)
    , mkAutoScalingGroup
    , asgName

    -- ** EnvironmentHealthAttribute
    , EnvironmentHealthAttribute (..)

    -- ** ARN
    , ARN (..)

    -- ** ResourceName
    , ResourceName (..)

    -- ** DNSCnamePrefix
    , DNSCnamePrefix (..)

    -- ** SourceType
    , SourceType (..)

    -- ** ConfigurationDeploymentStatus
    , ConfigurationDeploymentStatus (..)

    -- ** ApplicationMetrics
    , ApplicationMetrics (..)
    , mkApplicationMetrics
    , amDuration
    , amLatency
    , amRequestCount
    , amStatusCodes

    -- ** EnvironmentLink
    , EnvironmentLink (..)
    , mkEnvironmentLink
    , elEnvironmentName
    , elLinkName

    -- ** ConfigurationOptionPossibleValue
    , ConfigurationOptionPossibleValue (..)

    -- ** ConfigurationOptionSetting
    , ConfigurationOptionSetting (..)
    , mkConfigurationOptionSetting
    , cosNamespace
    , cosOptionName
    , cosResourceName
    , cosValue

    -- ** PlatformVersion
    , PlatformVersion (..)

    -- ** ConfigurationOptionSeverity
    , ConfigurationOptionSeverity (..)

    -- ** ConfigurationOptionValueType
    , ConfigurationOptionValueType (..)

    -- ** EndpointURL
    , EndpointURL (..)

    -- ** OptionNamespace
    , OptionNamespace (..)

    -- ** Ec2InstanceId
    , Ec2InstanceId (..)

    -- ** FailureType
    , FailureType (..)

    -- ** Latency
    , Latency (..)
    , mkLatency
    , lP10
    , lP50
    , lP75
    , lP85
    , lP90
    , lP95
    , lP99
    , lP999

    -- ** Token
    , Token (..)

    -- ** PlatformStatus
    , PlatformStatus (..)

    -- ** EnvironmentDescriptionsMessage
    , EnvironmentDescriptionsMessage (..)
    , mkEnvironmentDescriptionsMessage
    , edmEnvironments
    , edmNextToken

    -- ** BranchName
    , BranchName (..)

    -- ** ConfigurationSettingsDescription
    , ConfigurationSettingsDescription (..)
    , mkConfigurationSettingsDescription
    , csdApplicationName
    , csdDateCreated
    , csdDateUpdated
    , csdDeploymentStatus
    , csdDescription
    , csdEnvironmentName
    , csdOptionSettings
    , csdPlatformArn
    , csdSolutionStackName
    , csdTemplateName

    -- ** ApplicationVersionDescription
    , ApplicationVersionDescription (..)
    , mkApplicationVersionDescription
    , avdApplicationName
    , avdApplicationVersionArn
    , avdBuildArn
    , avdDateCreated
    , avdDateUpdated
    , avdDescription
    , avdSourceBuildInformation
    , avdSourceBundle
    , avdStatus
    , avdVersionLabel

    -- ** CustomAmi
    , CustomAmi (..)
    , mkCustomAmi
    , caImageId
    , caVirtualizationType

    -- ** OptionSpecification
    , OptionSpecification (..)
    , mkOptionSpecification
    , osNamespace
    , osOptionName
    , osResourceName

    -- ** SearchFilter
    , SearchFilter (..)
    , mkSearchFilter
    , sfAttribute
    , sfOperator
    , sfValues

    -- ** Maintainer
    , Maintainer (..)

    -- ** PlatformLifecycleState
    , PlatformLifecycleState (..)

    -- ** PlatformOwner
    , PlatformOwner (..)

    -- ** SystemStatus
    , SystemStatus (..)
    , mkSystemStatus
    , ssCPUUtilization
    , ssLoadAverage

    -- ** EnvironmentResourceDescription
    , EnvironmentResourceDescription (..)
    , mkEnvironmentResourceDescription
    , erdAutoScalingGroups
    , erdEnvironmentName
    , erdInstances
    , erdLaunchConfigurations
    , erdLaunchTemplates
    , erdLoadBalancers
    , erdQueues
    , erdTriggers

    -- ** Builder
    , Builder (..)
    , mkBuilder
    , bARN

    -- ** Queue
    , Queue (..)
    , mkQueue
    , qName
    , qURL

    -- ** Cause
    , Cause (..)

    -- ** EnvironmentStatus
    , EnvironmentStatus (..)

    -- ** ManagedActionHistoryItem
    , ManagedActionHistoryItem (..)
    , mkManagedActionHistoryItem
    , mahiActionDescription
    , mahiActionId
    , mahiActionType
    , mahiExecutedTime
    , mahiFailureDescription
    , mahiFailureType
    , mahiFinishedTime
    , mahiStatus

    -- ** CPUUtilization
    , CPUUtilization (..)
    , mkCPUUtilization
    , cpuuIOWait
    , cpuuIRQ
    , cpuuIdle
    , cpuuNice
    , cpuuPrivileged
    , cpuuSoftIRQ
    , cpuuSystem
    , cpuuUser

    -- ** NextToken
    , NextToken (..)

    -- ** ApplicationVersionStatus
    , ApplicationVersionStatus (..)

    -- ** LoadBalancerDescription
    , LoadBalancerDescription (..)
    , mkLoadBalancerDescription
    , lbdDomain
    , lbdListeners
    , lbdLoadBalancerName

    -- ** ConfigurationOptionName
    , ConfigurationOptionName (..)

    -- ** VersionLabel
    , VersionLabel (..)

    -- ** ResourceQuota
    , ResourceQuota (..)
    , mkResourceQuota
    , rqMaximum

    -- ** NonEmptyString
    , NonEmptyString (..)

    -- ** ConfigurationTemplateName
    , ConfigurationTemplateName (..)

    -- ** SourceBuildInformation
    , SourceBuildInformation (..)
    , mkSourceBuildInformation
    , sbiSourceType
    , sbiSourceRepository
    , sbiSourceLocation

    -- ** ResourceArn
    , ResourceArn (..)

    -- ** SingleInstanceHealth
    , SingleInstanceHealth (..)
    , mkSingleInstanceHealth
    , sihApplicationMetrics
    , sihAvailabilityZone
    , sihCauses
    , sihColor
    , sihDeployment
    , sihHealthStatus
    , sihInstanceId
    , sihInstanceType
    , sihLaunchedAt
    , sihSystem

    -- ** SourceRepository
    , SourceRepository (..)

    -- ** PlatformFilter
    , PlatformFilter (..)
    , mkPlatformFilter
    , pfOperator
    , pfType
    , pfValues

    -- ** ApplicationDescriptionMessage
    , ApplicationDescriptionMessage (..)
    , mkApplicationDescriptionMessage
    , admApplication

    -- ** FileTypeExtension
    , FileTypeExtension (..)

    -- ** OperatingSystemName
    , OperatingSystemName (..)

    -- ** EnvironmentTier
    , EnvironmentTier (..)
    , mkEnvironmentTier
    , etName
    , etType
    , etVersion

    -- ** LoadBalancer
    , LoadBalancer (..)
    , mkLoadBalancer
    , lbName

    -- ** ImageId
    , ImageId (..)

    -- ** OperationsRole
    , OperationsRole (..)

    -- ** StatusCodes
    , StatusCodes (..)
    , mkStatusCodes
    , scStatus2xx
    , scStatus3xx
    , scStatus4xx
    , scStatus5xx

    -- ** PlatformArn
    , PlatformArn (..)

    -- ** OperatingSystemVersion
    , OperatingSystemVersion (..)

    -- ** EnvironmentResourcesDescription
    , EnvironmentResourcesDescription (..)
    , mkEnvironmentResourcesDescription
    , erdLoadBalancer

    -- ** OptionRestrictionRegex
    , OptionRestrictionRegex (..)
    , mkOptionRestrictionRegex
    , orrLabel
    , orrPattern

    -- ** EnvironmentName
    , EnvironmentName (..)

    -- ** ActionStatus
    , ActionStatus (..)

    -- ** ApplicationName
    , ApplicationName (..)

    -- ** LaunchTemplate
    , LaunchTemplate (..)
    , mkLaunchTemplate
    , ltId

    -- ** ConfigurationOptionDescription
    , ConfigurationOptionDescription (..)
    , mkConfigurationOptionDescription
    , codChangeSeverity
    , codDefaultValue
    , codMaxLength
    , codMaxValue
    , codMinValue
    , codName
    , codNamespace
    , codRegex
    , codUserDefined
    , codValueOptions
    , codValueType

    -- ** SearchFilterValue
    , SearchFilterValue (..)

    -- ** ApplicationVersionArn
    , ApplicationVersionArn (..)

    -- ** ComputeType
    , ComputeType (..)

    -- ** SourceConfiguration
    , SourceConfiguration (..)
    , mkSourceConfiguration
    , scApplicationName
    , scTemplateName

    -- ** ValidationMessageString
    , ValidationMessageString (..)

    -- ** EnvironmentInfoDescription
    , EnvironmentInfoDescription (..)
    , mkEnvironmentInfoDescription
    , eidEc2InstanceId
    , eidInfoType
    , eidMessage
    , eidSampleTimestamp

    -- ** EnvironmentHealthStatus
    , EnvironmentHealthStatus (..)

    -- ** TagKey
    , TagKey (..)

    -- ** S3Location
    , S3Location (..)
    , mkS3Location
    , slS3Bucket
    , slS3Key

    -- ** EnvironmentArn
    , EnvironmentArn (..)

    -- ** SolutionStackName
    , SolutionStackName (..)

    -- ** ValidationMessage
    , ValidationMessage (..)
    , mkValidationMessage
    , vmMessage
    , vmNamespace
    , vmOptionName
    , vmSeverity

    -- ** EnvironmentId
    , EnvironmentId (..)

    -- ** PlatformName
    , PlatformName (..)

    -- ** ValidationSeverity
    , ValidationSeverity (..)

    -- ** Trigger
    , Trigger (..)
    , mkTrigger
    , tName

    -- ** ActionHistoryStatus
    , ActionHistoryStatus (..)

    -- ** SourceLocation
    , SourceLocation (..)

    -- ** GroupName
    , GroupName (..)

    -- ** ManagedAction
    , ManagedAction (..)
    , mkManagedAction
    , maActionDescription
    , maActionId
    , maActionType
    , maStatus
    , maWindowStartTime

    -- ** EnvironmentInfoType
    , EnvironmentInfoType (..)

    -- ** PlatformFramework
    , PlatformFramework (..)
    , mkPlatformFramework
    , pfName
    , pfVersion

    -- ** Message
    , Message (..)

    -- ** PlatformSummary
    , PlatformSummary (..)
    , mkPlatformSummary
    , psOperatingSystemName
    , psOperatingSystemVersion
    , psPlatformArn
    , psPlatformBranchLifecycleState
    , psPlatformBranchName
    , psPlatformCategory
    , psPlatformLifecycleState
    , psPlatformOwner
    , psPlatformStatus
    , psPlatformVersion
    , psSupportedAddonList
    , psSupportedTierList

    -- ** EnvironmentDescription
    , EnvironmentDescription (..)
    , mkEnvironmentDescription
    , eAbortableOperationInProgress
    , eApplicationName
    , eCNAME
    , eDateCreated
    , eDateUpdated
    , eDescription
    , eEndpointURL
    , eEnvironmentArn
    , eEnvironmentId
    , eEnvironmentLinks
    , eEnvironmentName
    , eHealth
    , eHealthStatus
    , eOperationsRole
    , ePlatformArn
    , eResources
    , eSolutionStackName
    , eStatus
    , eTemplateName
    , eTier
    , eVersionLabel

    -- ** SupportedAddon
    , SupportedAddon (..)

    -- ** Description
    , Description (..)

    -- ** BuildConfiguration
    , BuildConfiguration (..)
    , mkBuildConfiguration
    , bcCodeBuildServiceRole
    , bcImage
    , bcArtifactName
    , bcComputeType
    , bcTimeoutInMinutes

    -- ** ActionType
    , ActionType (..)

    -- ** Listener
    , Listener (..)
    , mkListener
    , lPort
    , lProtocol

    -- ** S3Bucket
    , S3Bucket (..)

    -- ** EnvironmentHealth
    , EnvironmentHealth (..)

    -- ** Instance
    , Instance (..)
    , mkInstance
    , iId

    -- ** Deployment
    , Deployment (..)
    , mkDeployment
    , dDeploymentId
    , dDeploymentTime
    , dStatus
    , dVersionLabel

    -- ** MaxAgeRule
    , MaxAgeRule (..)
    , mkMaxAgeRule
    , marEnabled
    , marDeleteSourceFromS3
    , marMaxAgeInDays

    -- ** PlatformDescription
    , PlatformDescription (..)
    , mkPlatformDescription
    , pdCustomAmiList
    , pdDateCreated
    , pdDateUpdated
    , pdDescription
    , pdFrameworks
    , pdMaintainer
    , pdOperatingSystemName
    , pdOperatingSystemVersion
    , pdPlatformArn
    , pdPlatformBranchLifecycleState
    , pdPlatformBranchName
    , pdPlatformCategory
    , pdPlatformLifecycleState
    , pdPlatformName
    , pdPlatformOwner
    , pdPlatformStatus
    , pdPlatformVersion
    , pdProgrammingLanguages
    , pdSolutionStackName
    , pdSupportedAddonList
    , pdSupportedTierList

    -- ** MaxCountRule
    , MaxCountRule (..)
    , mkMaxCountRule
    , mcrEnabled
    , mcrDeleteSourceFromS3
    , mcrMaxCount

    -- ** InstanceHealthSummary
    , InstanceHealthSummary (..)
    , mkInstanceHealthSummary
    , ihsDegraded
    , ihsInfo
    , ihsNoData
    , ihsOk
    , ihsPending
    , ihsSevere
    , ihsUnknown
    , ihsWarning

    -- ** SolutionStackDescription
    , SolutionStackDescription (..)
    , mkSolutionStackDescription
    , ssdPermittedFileTypes
    , ssdSolutionStackName

    -- ** FullyQualifiedCNAME
    , FullyQualifiedCNAME (..)

    -- ** TemplateName
    , TemplateName (..)

    -- ** LifecycleState
    , LifecycleState (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** Namespace
    , Namespace (..)

    -- ** OptionName
    , OptionName (..)

    -- ** Attribute
    , Attribute (..)

    -- ** Operator
    , Operator (..)

    -- ** DestinationEnvironmentId
    , DestinationEnvironmentId (..)

    -- ** DestinationEnvironmentName
    , DestinationEnvironmentName (..)

    -- ** SourceEnvironmentId
    , SourceEnvironmentId (..)

    -- ** SourceEnvironmentName
    , SourceEnvironmentName (..)

    -- ** Label
    , Label (..)

    -- ** Pattern
    , Pattern (..)

    -- ** DefaultValue
    , DefaultValue (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Waiters
import Network.AWS.ElasticBeanstalk.DescribeApplications
import Network.AWS.ElasticBeanstalk.UpdateEnvironment
import Network.AWS.ElasticBeanstalk.TerminateEnvironment
import Network.AWS.ElasticBeanstalk.ListPlatformVersions
import Network.AWS.ElasticBeanstalk.DeletePlatformVersion
import Network.AWS.ElasticBeanstalk.CreateApplicationVersion
import Network.AWS.ElasticBeanstalk.ListPlatformBranches
import Network.AWS.ElasticBeanstalk.DescribeEvents
import Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
import Network.AWS.ElasticBeanstalk.ListTagsForResource
import Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
import Network.AWS.ElasticBeanstalk.DescribePlatformVersion
import Network.AWS.ElasticBeanstalk.DeleteApplication
import Network.AWS.ElasticBeanstalk.UpdateApplication
import Network.AWS.ElasticBeanstalk.DescribeInstancesHealth
import Network.AWS.ElasticBeanstalk.CreateApplication
import Network.AWS.ElasticBeanstalk.ComposeEnvironments
import Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
import Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
import Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.UpdateTagsForResource
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
import Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
import Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
import Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentHealth
import Network.AWS.ElasticBeanstalk.RebuildEnvironment
import Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
import Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle
import Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
import Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
import Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction
import Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
import Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
import Network.AWS.ElasticBeanstalk.CreateStorageLocation
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActions
import Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
import Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
import Network.AWS.ElasticBeanstalk.DescribeAccountAttributes
import Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole
import Network.AWS.ElasticBeanstalk.RestartAppServer
import Network.AWS.ElasticBeanstalk.DescribeEnvironments
import Network.AWS.ElasticBeanstalk.CheckDNSAvailability
import Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
import Network.AWS.ElasticBeanstalk.CreateEnvironment
import Network.AWS.ElasticBeanstalk.CreatePlatformVersion
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ElasticBeanstalk'.
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
