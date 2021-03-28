-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidRequestException
    , _TooManyBucketsException
    , _S3SubscriptionRequiredException
    , _OperationInProgressException
    , _PlatformVersionStillReferencedException
    , _TooManyApplicationVersionsException
    , _TooManyConfigurationTemplatesException
    , _ResourceTypeNotSupportedException
    , _InsufficientPrivilegesException
    , _ElasticBeanstalkServiceException
    , _TooManyTagsException
    , _TooManyApplicationsException
    , _TooManyPlatformsException
    , _ManagedActionInvalidStateException
    , _SourceBundleDeletionException
    , _S3LocationNotInServiceRegionException
    , _CodeBuildNotInServiceRegionException
    , _TooManyEnvironmentsException
    , _ResourceNotFoundException

    -- * RequestId
    , RequestId (..)

    -- * InstanceId
    , InstanceId (..)

    -- * ApplicationResourceLifecycleConfig
    , ApplicationResourceLifecycleConfig (..)
    , mkApplicationResourceLifecycleConfig
    , arlcServiceRole
    , arlcVersionLifecycleConfig

    -- * ApplicationVersionLifecycleConfig
    , ApplicationVersionLifecycleConfig (..)
    , mkApplicationVersionLifecycleConfig
    , avlcMaxAgeRule
    , avlcMaxCountRule

    -- * PlatformProgrammingLanguage
    , PlatformProgrammingLanguage (..)
    , mkPlatformProgrammingLanguage
    , pplName
    , pplVersion

    -- * PlatformFilterValue
    , PlatformFilterValue (..)

    -- * ApplicationDescription
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

    -- * SupportedTier
    , SupportedTier (..)

    -- * PlatformFilterType
    , PlatformFilterType (..)

    -- * ResourceQuotas
    , ResourceQuotas (..)
    , mkResourceQuotas
    , rqApplicationQuota
    , rqApplicationVersionQuota
    , rqConfigurationTemplateQuota
    , rqCustomPlatformQuota
    , rqEnvironmentQuota

    -- * PlatformCategory
    , PlatformCategory (..)

    -- * VirtualizationType
    , VirtualizationType (..)

    -- * EventSeverity
    , EventSeverity (..)

    -- * PlatformBranchSummary
    , PlatformBranchSummary (..)
    , mkPlatformBranchSummary
    , pbsBranchName
    , pbsBranchOrder
    , pbsLifecycleState
    , pbsPlatformName
    , pbsSupportedTierList

    -- * ApplicationArn
    , ApplicationArn (..)

    -- * ResourceId
    , ResourceId (..)

    -- * S3Key
    , S3Key (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * InstancesHealthAttribute
    , InstancesHealthAttribute (..)

    -- * PlatformBranchLifecycleState
    , PlatformBranchLifecycleState (..)

    -- * EventDescription
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

    -- * DNSCname
    , DNSCname (..)

    -- * LaunchConfiguration
    , LaunchConfiguration (..)
    , mkLaunchConfiguration
    , lcName

    -- * ApplicationVersionDescriptionMessage
    , ApplicationVersionDescriptionMessage (..)
    , mkApplicationVersionDescriptionMessage
    , avdmApplicationVersion

    -- * AutoScalingGroup
    , AutoScalingGroup (..)
    , mkAutoScalingGroup
    , asgName

    -- * EnvironmentHealthAttribute
    , EnvironmentHealthAttribute (..)

    -- * ARN
    , ARN (..)

    -- * ResourceName
    , ResourceName (..)

    -- * DNSCnamePrefix
    , DNSCnamePrefix (..)

    -- * SourceType
    , SourceType (..)

    -- * ConfigurationDeploymentStatus
    , ConfigurationDeploymentStatus (..)

    -- * ApplicationMetrics
    , ApplicationMetrics (..)
    , mkApplicationMetrics
    , amDuration
    , amLatency
    , amRequestCount
    , amStatusCodes

    -- * EnvironmentLink
    , EnvironmentLink (..)
    , mkEnvironmentLink
    , elEnvironmentName
    , elLinkName

    -- * ConfigurationOptionPossibleValue
    , ConfigurationOptionPossibleValue (..)

    -- * ConfigurationOptionSetting
    , ConfigurationOptionSetting (..)
    , mkConfigurationOptionSetting
    , cosNamespace
    , cosOptionName
    , cosResourceName
    , cosValue

    -- * PlatformVersion
    , PlatformVersion (..)

    -- * ConfigurationOptionSeverity
    , ConfigurationOptionSeverity (..)

    -- * ConfigurationOptionValueType
    , ConfigurationOptionValueType (..)

    -- * EndpointURL
    , EndpointURL (..)

    -- * OptionNamespace
    , OptionNamespace (..)

    -- * Ec2InstanceId
    , Ec2InstanceId (..)

    -- * FailureType
    , FailureType (..)

    -- * Latency
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

    -- * Token
    , Token (..)

    -- * PlatformStatus
    , PlatformStatus (..)

    -- * EnvironmentDescriptionsMessage
    , EnvironmentDescriptionsMessage (..)
    , mkEnvironmentDescriptionsMessage
    , edmEnvironments
    , edmNextToken

    -- * BranchName
    , BranchName (..)

    -- * ConfigurationSettingsDescription
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

    -- * ApplicationVersionDescription
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

    -- * CustomAmi
    , CustomAmi (..)
    , mkCustomAmi
    , caImageId
    , caVirtualizationType

    -- * OptionSpecification
    , OptionSpecification (..)
    , mkOptionSpecification
    , osNamespace
    , osOptionName
    , osResourceName

    -- * SearchFilter
    , SearchFilter (..)
    , mkSearchFilter
    , sfAttribute
    , sfOperator
    , sfValues

    -- * Maintainer
    , Maintainer (..)

    -- * PlatformLifecycleState
    , PlatformLifecycleState (..)

    -- * PlatformOwner
    , PlatformOwner (..)

    -- * SystemStatus
    , SystemStatus (..)
    , mkSystemStatus
    , ssCPUUtilization
    , ssLoadAverage

    -- * EnvironmentResourceDescription
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

    -- * Builder
    , Builder (..)
    , mkBuilder
    , bARN

    -- * Queue
    , Queue (..)
    , mkQueue
    , qName
    , qURL

    -- * Cause
    , Cause (..)

    -- * EnvironmentStatus
    , EnvironmentStatus (..)

    -- * ManagedActionHistoryItem
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

    -- * CPUUtilization
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

    -- * NextToken
    , NextToken (..)

    -- * ApplicationVersionStatus
    , ApplicationVersionStatus (..)

    -- * LoadBalancerDescription
    , LoadBalancerDescription (..)
    , mkLoadBalancerDescription
    , lbdDomain
    , lbdListeners
    , lbdLoadBalancerName

    -- * ConfigurationOptionName
    , ConfigurationOptionName (..)

    -- * VersionLabel
    , VersionLabel (..)

    -- * ResourceQuota
    , ResourceQuota (..)
    , mkResourceQuota
    , rqMaximum

    -- * NonEmptyString
    , NonEmptyString (..)

    -- * ConfigurationTemplateName
    , ConfigurationTemplateName (..)

    -- * SourceBuildInformation
    , SourceBuildInformation (..)
    , mkSourceBuildInformation
    , sbiSourceType
    , sbiSourceRepository
    , sbiSourceLocation

    -- * ResourceArn
    , ResourceArn (..)

    -- * SingleInstanceHealth
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

    -- * SourceRepository
    , SourceRepository (..)

    -- * PlatformFilter
    , PlatformFilter (..)
    , mkPlatformFilter
    , pfOperator
    , pfType
    , pfValues

    -- * ApplicationDescriptionMessage
    , ApplicationDescriptionMessage (..)
    , mkApplicationDescriptionMessage
    , admApplication

    -- * FileTypeExtension
    , FileTypeExtension (..)

    -- * OperatingSystemName
    , OperatingSystemName (..)

    -- * EnvironmentTier
    , EnvironmentTier (..)
    , mkEnvironmentTier
    , etName
    , etType
    , etVersion

    -- * LoadBalancer
    , LoadBalancer (..)
    , mkLoadBalancer
    , lbName

    -- * ImageId
    , ImageId (..)

    -- * OperationsRole
    , OperationsRole (..)

    -- * StatusCodes
    , StatusCodes (..)
    , mkStatusCodes
    , scStatus2xx
    , scStatus3xx
    , scStatus4xx
    , scStatus5xx

    -- * PlatformArn
    , PlatformArn (..)

    -- * OperatingSystemVersion
    , OperatingSystemVersion (..)

    -- * EnvironmentResourcesDescription
    , EnvironmentResourcesDescription (..)
    , mkEnvironmentResourcesDescription
    , erdLoadBalancer

    -- * OptionRestrictionRegex
    , OptionRestrictionRegex (..)
    , mkOptionRestrictionRegex
    , orrLabel
    , orrPattern

    -- * EnvironmentName
    , EnvironmentName (..)

    -- * ActionStatus
    , ActionStatus (..)

    -- * ApplicationName
    , ApplicationName (..)

    -- * LaunchTemplate
    , LaunchTemplate (..)
    , mkLaunchTemplate
    , ltId

    -- * ConfigurationOptionDescription
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

    -- * SearchFilterValue
    , SearchFilterValue (..)

    -- * ApplicationVersionArn
    , ApplicationVersionArn (..)

    -- * ComputeType
    , ComputeType (..)

    -- * SourceConfiguration
    , SourceConfiguration (..)
    , mkSourceConfiguration
    , scApplicationName
    , scTemplateName

    -- * ValidationMessageString
    , ValidationMessageString (..)

    -- * EnvironmentInfoDescription
    , EnvironmentInfoDescription (..)
    , mkEnvironmentInfoDescription
    , eidEc2InstanceId
    , eidInfoType
    , eidMessage
    , eidSampleTimestamp

    -- * EnvironmentHealthStatus
    , EnvironmentHealthStatus (..)

    -- * TagKey
    , TagKey (..)

    -- * S3Location
    , S3Location (..)
    , mkS3Location
    , slS3Bucket
    , slS3Key

    -- * EnvironmentArn
    , EnvironmentArn (..)

    -- * SolutionStackName
    , SolutionStackName (..)

    -- * ValidationMessage
    , ValidationMessage (..)
    , mkValidationMessage
    , vmMessage
    , vmNamespace
    , vmOptionName
    , vmSeverity

    -- * EnvironmentId
    , EnvironmentId (..)

    -- * PlatformName
    , PlatformName (..)

    -- * ValidationSeverity
    , ValidationSeverity (..)

    -- * Trigger
    , Trigger (..)
    , mkTrigger
    , tName

    -- * ActionHistoryStatus
    , ActionHistoryStatus (..)

    -- * SourceLocation
    , SourceLocation (..)

    -- * GroupName
    , GroupName (..)

    -- * ManagedAction
    , ManagedAction (..)
    , mkManagedAction
    , maActionDescription
    , maActionId
    , maActionType
    , maStatus
    , maWindowStartTime

    -- * EnvironmentInfoType
    , EnvironmentInfoType (..)

    -- * PlatformFramework
    , PlatformFramework (..)
    , mkPlatformFramework
    , pfName
    , pfVersion

    -- * Message
    , Message (..)

    -- * PlatformSummary
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

    -- * EnvironmentDescription
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

    -- * SupportedAddon
    , SupportedAddon (..)

    -- * Description
    , Description (..)

    -- * BuildConfiguration
    , BuildConfiguration (..)
    , mkBuildConfiguration
    , bcCodeBuildServiceRole
    , bcImage
    , bcArtifactName
    , bcComputeType
    , bcTimeoutInMinutes

    -- * ActionType
    , ActionType (..)

    -- * Listener
    , Listener (..)
    , mkListener
    , lPort
    , lProtocol

    -- * S3Bucket
    , S3Bucket (..)

    -- * EnvironmentHealth
    , EnvironmentHealth (..)

    -- * Instance
    , Instance (..)
    , mkInstance
    , iId

    -- * Deployment
    , Deployment (..)
    , mkDeployment
    , dDeploymentId
    , dDeploymentTime
    , dStatus
    , dVersionLabel

    -- * MaxAgeRule
    , MaxAgeRule (..)
    , mkMaxAgeRule
    , marEnabled
    , marDeleteSourceFromS3
    , marMaxAgeInDays

    -- * PlatformDescription
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

    -- * MaxCountRule
    , MaxCountRule (..)
    , mkMaxCountRule
    , mcrEnabled
    , mcrDeleteSourceFromS3
    , mcrMaxCount

    -- * InstanceHealthSummary
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

    -- * SolutionStackDescription
    , SolutionStackDescription (..)
    , mkSolutionStackDescription
    , ssdPermittedFileTypes
    , ssdSolutionStackName

    -- * FullyQualifiedCNAME
    , FullyQualifiedCNAME (..)

    -- * TemplateName
    , TemplateName (..)

    -- * LifecycleState
    , LifecycleState (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * Namespace
    , Namespace (..)

    -- * OptionName
    , OptionName (..)

    -- * Attribute
    , Attribute (..)

    -- * Operator
    , Operator (..)

    -- * DestinationEnvironmentId
    , DestinationEnvironmentId (..)

    -- * DestinationEnvironmentName
    , DestinationEnvironmentName (..)

    -- * SourceEnvironmentId
    , SourceEnvironmentId (..)

    -- * SourceEnvironmentName
    , SourceEnvironmentName (..)

    -- * Label
    , Label (..)

    -- * Pattern
    , Pattern (..)

    -- * DefaultValue
    , DefaultValue (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.ElasticBeanstalk.Types.RequestId
  
import Network.AWS.ElasticBeanstalk.Types.InstanceId
  
import Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
  
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
  
import Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
  
import Network.AWS.ElasticBeanstalk.Types.PlatformFilterValue
  
import Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
  
import Network.AWS.ElasticBeanstalk.Types.SupportedTier
  
import Network.AWS.ElasticBeanstalk.Types.PlatformFilterType
  
import Network.AWS.ElasticBeanstalk.Types.ResourceQuotas
  
import Network.AWS.ElasticBeanstalk.Types.PlatformCategory
  
import Network.AWS.ElasticBeanstalk.Types.VirtualizationType
  
import Network.AWS.ElasticBeanstalk.Types.EventSeverity
  
import Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary
  
  
import Network.AWS.ElasticBeanstalk.Types.ApplicationArn
  
import Network.AWS.ElasticBeanstalk.Types.ResourceId
  
import Network.AWS.ElasticBeanstalk.Types.S3Key
  
import Network.AWS.ElasticBeanstalk.Types.Tag
  
import Network.AWS.ElasticBeanstalk.Types.InstancesHealthAttribute
  
import Network.AWS.ElasticBeanstalk.Types.PlatformBranchLifecycleState
  
import Network.AWS.ElasticBeanstalk.Types.EventDescription
  
import Network.AWS.ElasticBeanstalk.Types.DNSCname
  
import Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
  
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage
  
import Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthAttribute
  
import Network.AWS.ElasticBeanstalk.Types.ARN
  
import Network.AWS.ElasticBeanstalk.Types.ResourceName
  
import Network.AWS.ElasticBeanstalk.Types.DNSCnamePrefix
  
  
  
import Network.AWS.ElasticBeanstalk.Types.SourceType
  
import Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
  
import Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
  
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionPossibleValue
  
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting
  
import Network.AWS.ElasticBeanstalk.Types.PlatformVersion
  
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSeverity
  
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionValueType
  
import Network.AWS.ElasticBeanstalk.Types.EndpointURL
  
import Network.AWS.ElasticBeanstalk.Types.OptionNamespace
  
import Network.AWS.ElasticBeanstalk.Types.Ec2InstanceId
  
import Network.AWS.ElasticBeanstalk.Types.FailureType
  
  
import Network.AWS.ElasticBeanstalk.Types.Latency
  
import Network.AWS.ElasticBeanstalk.Types.Token
  
import Network.AWS.ElasticBeanstalk.Types.PlatformStatus
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
  
import Network.AWS.ElasticBeanstalk.Types.BranchName
  
  
  
import Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription
  
  
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription
  
import Network.AWS.ElasticBeanstalk.Types.CustomAmi
  
  
  
import Network.AWS.ElasticBeanstalk.Types.OptionSpecification
  
import Network.AWS.ElasticBeanstalk.Types.SearchFilter
  
import Network.AWS.ElasticBeanstalk.Types.Maintainer
  
import Network.AWS.ElasticBeanstalk.Types.PlatformLifecycleState
  
import Network.AWS.ElasticBeanstalk.Types.PlatformOwner
  
  
import Network.AWS.ElasticBeanstalk.Types.SystemStatus
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription
  
import Network.AWS.ElasticBeanstalk.Types.Builder
  
  
import Network.AWS.ElasticBeanstalk.Types.Queue
  
import Network.AWS.ElasticBeanstalk.Types.Cause
  
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentStatus
  
import Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem
  
import Network.AWS.ElasticBeanstalk.Types.CPUUtilization
  
import Network.AWS.ElasticBeanstalk.Types.NextToken
  
  
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus
  
import Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
  
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionName
  
import Network.AWS.ElasticBeanstalk.Types.VersionLabel
  
import Network.AWS.ElasticBeanstalk.Types.ResourceQuota
  
import Network.AWS.ElasticBeanstalk.Types.NonEmptyString
  
import Network.AWS.ElasticBeanstalk.Types.ConfigurationTemplateName
  
import Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
  
import Network.AWS.ElasticBeanstalk.Types.ResourceArn
  
import Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth
  
import Network.AWS.ElasticBeanstalk.Types.SourceRepository
  
import Network.AWS.ElasticBeanstalk.Types.PlatformFilter
  
import Network.AWS.ElasticBeanstalk.Types.ApplicationDescriptionMessage
  
import Network.AWS.ElasticBeanstalk.Types.FileTypeExtension
  
import Network.AWS.ElasticBeanstalk.Types.OperatingSystemName
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentTier
  
import Network.AWS.ElasticBeanstalk.Types.LoadBalancer
  
  
  
import Network.AWS.ElasticBeanstalk.Types.ImageId
  
import Network.AWS.ElasticBeanstalk.Types.OperationsRole
  
import Network.AWS.ElasticBeanstalk.Types.StatusCodes
  
import Network.AWS.ElasticBeanstalk.Types.PlatformArn
  
import Network.AWS.ElasticBeanstalk.Types.OperatingSystemVersion
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription
  
import Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentName
  
import Network.AWS.ElasticBeanstalk.Types.ActionStatus
  
  
import Network.AWS.ElasticBeanstalk.Types.ApplicationName
  
import Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
  
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionDescription
  
import Network.AWS.ElasticBeanstalk.Types.SearchFilterValue
  
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionArn
  
import Network.AWS.ElasticBeanstalk.Types.ComputeType
  
import Network.AWS.ElasticBeanstalk.Types.SourceConfiguration
  
import Network.AWS.ElasticBeanstalk.Types.ValidationMessageString
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoDescription
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus
  
import Network.AWS.ElasticBeanstalk.Types.TagKey
  
import Network.AWS.ElasticBeanstalk.Types.S3Location
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentArn
  
import Network.AWS.ElasticBeanstalk.Types.SolutionStackName
  
import Network.AWS.ElasticBeanstalk.Types.ValidationMessage
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentId
  
import Network.AWS.ElasticBeanstalk.Types.PlatformName
  
import Network.AWS.ElasticBeanstalk.Types.ValidationSeverity
  
import Network.AWS.ElasticBeanstalk.Types.Trigger
  
import Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus
  
import Network.AWS.ElasticBeanstalk.Types.SourceLocation
  
import Network.AWS.ElasticBeanstalk.Types.GroupName
  
  
import Network.AWS.ElasticBeanstalk.Types.ManagedAction
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoType
  
import Network.AWS.ElasticBeanstalk.Types.PlatformFramework
  
import Network.AWS.ElasticBeanstalk.Types.Message
  
import Network.AWS.ElasticBeanstalk.Types.PlatformSummary
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription
  
import Network.AWS.ElasticBeanstalk.Types.SupportedAddon
  
import Network.AWS.ElasticBeanstalk.Types.Description
  
  
import Network.AWS.ElasticBeanstalk.Types.BuildConfiguration
  
import Network.AWS.ElasticBeanstalk.Types.ActionType
  
import Network.AWS.ElasticBeanstalk.Types.Listener
  
import Network.AWS.ElasticBeanstalk.Types.S3Bucket
  
  
import Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth
  
import Network.AWS.ElasticBeanstalk.Types.Instance
  
import Network.AWS.ElasticBeanstalk.Types.Deployment
  
import Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
  
import Network.AWS.ElasticBeanstalk.Types.PlatformDescription
  
import Network.AWS.ElasticBeanstalk.Types.MaxCountRule
  
import Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary
  
import Network.AWS.ElasticBeanstalk.Types.SolutionStackDescription
  
import Network.AWS.ElasticBeanstalk.Types.FullyQualifiedCNAME
  
import Network.AWS.ElasticBeanstalk.Types.TemplateName
  
import Network.AWS.ElasticBeanstalk.Types.LifecycleState
  
import Network.AWS.ElasticBeanstalk.Types.Key
  
import Network.AWS.ElasticBeanstalk.Types.Value
  
import Network.AWS.ElasticBeanstalk.Types.Namespace
  
import Network.AWS.ElasticBeanstalk.Types.OptionName
  
import Network.AWS.ElasticBeanstalk.Types.Attribute
  
import Network.AWS.ElasticBeanstalk.Types.Operator
  
import Network.AWS.ElasticBeanstalk.Types.DestinationEnvironmentId
  
import Network.AWS.ElasticBeanstalk.Types.DestinationEnvironmentName
  
import Network.AWS.ElasticBeanstalk.Types.SourceEnvironmentId
  
import Network.AWS.ElasticBeanstalk.Types.SourceEnvironmentName
  
import Network.AWS.ElasticBeanstalk.Types.Label
  
import Network.AWS.ElasticBeanstalk.Types.Pattern
  
import Network.AWS.ElasticBeanstalk.Types.DefaultValue
  

-- | API version @2010-12-01@ of the Amazon Elastic Beanstalk SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ElasticBeanstalk",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "elasticbeanstalk",
                 Core._svcVersion = "2010-12-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "ElasticBeanstalk",
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

-- | One or more input parameters is not valid. Please correct the input parameters and try the operation again.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException
  = Core._MatchServiceError mkServiceConfig "InvalidRequestException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidRequestException #-}
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead"  #-}

-- | The specified account has reached its limit of Amazon S3 buckets.
_TooManyBucketsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyBucketsException
  = Core._MatchServiceError mkServiceConfig "TooManyBucketsException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyBucketsException #-}
{-# DEPRECATED _TooManyBucketsException "Use generic-lens or generic-optics instead"  #-}

-- | The specified account does not have a subscription to Amazon S3.
_S3SubscriptionRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_S3SubscriptionRequiredException
  = Core._MatchServiceError mkServiceConfig
      "S3SubscriptionRequiredException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _S3SubscriptionRequiredException #-}
{-# DEPRECATED _S3SubscriptionRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | Unable to perform the specified operation because another operation that effects an element in this activity is already in progress.
_OperationInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationInProgressException
  = Core._MatchServiceError mkServiceConfig
      "OperationInProgressFailure"
      Core.. Core.hasStatues 400
{-# INLINEABLE _OperationInProgressException #-}
{-# DEPRECATED _OperationInProgressException "Use generic-lens or generic-optics instead"  #-}

-- | You cannot delete the platform version because there are still environments running on it.
_PlatformVersionStillReferencedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PlatformVersionStillReferencedException
  = Core._MatchServiceError mkServiceConfig
      "PlatformVersionStillReferencedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _PlatformVersionStillReferencedException #-}
{-# DEPRECATED _PlatformVersionStillReferencedException "Use generic-lens or generic-optics instead"  #-}

-- | The specified account has reached its limit of application versions.
_TooManyApplicationVersionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyApplicationVersionsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyApplicationVersionsException"
{-# INLINEABLE _TooManyApplicationVersionsException #-}
{-# DEPRECATED _TooManyApplicationVersionsException "Use generic-lens or generic-optics instead"  #-}

-- | The specified account has reached its limit of configuration templates.
_TooManyConfigurationTemplatesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyConfigurationTemplatesException
  = Core._MatchServiceError mkServiceConfig
      "TooManyConfigurationTemplatesException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyConfigurationTemplatesException #-}
{-# DEPRECATED _TooManyConfigurationTemplatesException "Use generic-lens or generic-optics instead"  #-}

-- | The type of the specified Amazon Resource Name (ARN) isn't supported for this operation.
_ResourceTypeNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceTypeNotSupportedException
  = Core._MatchServiceError mkServiceConfig
      "ResourceTypeNotSupportedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ResourceTypeNotSupportedException #-}
{-# DEPRECATED _ResourceTypeNotSupportedException "Use generic-lens or generic-optics instead"  #-}

-- | The specified account does not have sufficient privileges for one or more AWS services.
_InsufficientPrivilegesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientPrivilegesException
  = Core._MatchServiceError mkServiceConfig
      "InsufficientPrivilegesException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _InsufficientPrivilegesException #-}
{-# DEPRECATED _InsufficientPrivilegesException "Use generic-lens or generic-optics instead"  #-}

-- | A generic service exception has occurred.
_ElasticBeanstalkServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ElasticBeanstalkServiceException
  = Core._MatchServiceError mkServiceConfig
      "ElasticBeanstalkServiceException"
{-# INLINEABLE _ElasticBeanstalkServiceException #-}
{-# DEPRECATED _ElasticBeanstalkServiceException "Use generic-lens or generic-optics instead"  #-}

-- | The number of tags in the resource would exceed the number of tags that each resource can have.
--
-- To calculate this, the operation considers both the number of tags the resource already has and the tags this operation would add if it succeeded.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException
  = Core._MatchServiceError mkServiceConfig "TooManyTagsException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyTagsException #-}
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead"  #-}

-- | The specified account has reached its limit of applications.
_TooManyApplicationsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyApplicationsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyApplicationsException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyApplicationsException #-}
{-# DEPRECATED _TooManyApplicationsException "Use generic-lens or generic-optics instead"  #-}

-- | You have exceeded the maximum number of allowed platforms associated with the account.
_TooManyPlatformsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyPlatformsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyPlatformsException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyPlatformsException #-}
{-# DEPRECATED _TooManyPlatformsException "Use generic-lens or generic-optics instead"  #-}

-- | Cannot modify the managed action in its current state.
_ManagedActionInvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ManagedActionInvalidStateException
  = Core._MatchServiceError mkServiceConfig
      "ManagedActionInvalidStateException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ManagedActionInvalidStateException #-}
{-# DEPRECATED _ManagedActionInvalidStateException "Use generic-lens or generic-optics instead"  #-}

-- | Unable to delete the Amazon S3 source bundle associated with the application version. The application version was deleted successfully.
_SourceBundleDeletionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SourceBundleDeletionException
  = Core._MatchServiceError mkServiceConfig
      "SourceBundleDeletionFailure"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SourceBundleDeletionException #-}
{-# DEPRECATED _SourceBundleDeletionException "Use generic-lens or generic-optics instead"  #-}

-- | The specified S3 bucket does not belong to the S3 region in which the service is running. The following regions are supported:
--
--
--     * IAD/us-east-1
--
--
--     * PDX/us-west-2
--
--
--     * DUB/eu-west-1
--
--
_S3LocationNotInServiceRegionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_S3LocationNotInServiceRegionException
  = Core._MatchServiceError mkServiceConfig
      "S3LocationNotInServiceRegionException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _S3LocationNotInServiceRegionException #-}
{-# DEPRECATED _S3LocationNotInServiceRegionException "Use generic-lens or generic-optics instead"  #-}

-- | AWS CodeBuild is not available in the specified region.
_CodeBuildNotInServiceRegionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CodeBuildNotInServiceRegionException
  = Core._MatchServiceError mkServiceConfig
      "CodeBuildNotInServiceRegionException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CodeBuildNotInServiceRegionException #-}
{-# DEPRECATED _CodeBuildNotInServiceRegionException "Use generic-lens or generic-optics instead"  #-}

-- | The specified account has reached its limit of environments.
_TooManyEnvironmentsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyEnvironmentsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyEnvironmentsException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyEnvironmentsException #-}
{-# DEPRECATED _TooManyEnvironmentsException "Use generic-lens or generic-optics instead"  #-}

-- | A resource doesn't exist for the specified Amazon Resource Name (ARN).
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}
