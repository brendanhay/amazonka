{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
  ( -- * Service configuration
    elasticBeanstalkService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** EnvironmentExists
    mkEnvironmentExists,

    -- ** EnvironmentUpdated
    mkEnvironmentUpdated,

    -- ** EnvironmentTerminated
    mkEnvironmentTerminated,

    -- * Operations
    -- $operations

    -- ** DescribeApplications
    module Network.AWS.ElasticBeanstalk.DescribeApplications,

    -- ** UpdateEnvironment
    module Network.AWS.ElasticBeanstalk.UpdateEnvironment,

    -- ** TerminateEnvironment
    module Network.AWS.ElasticBeanstalk.TerminateEnvironment,

    -- ** ListPlatformVersions (Paginated)
    module Network.AWS.ElasticBeanstalk.ListPlatformVersions,

    -- ** DeletePlatformVersion
    module Network.AWS.ElasticBeanstalk.DeletePlatformVersion,

    -- ** CreateApplicationVersion
    module Network.AWS.ElasticBeanstalk.CreateApplicationVersion,

    -- ** ListPlatformBranches
    module Network.AWS.ElasticBeanstalk.ListPlatformBranches,

    -- ** DescribeEvents (Paginated)
    module Network.AWS.ElasticBeanstalk.DescribeEvents,

    -- ** RequestEnvironmentInfo
    module Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo,

    -- ** ListTagsForResource
    module Network.AWS.ElasticBeanstalk.ListTagsForResource,

    -- ** RetrieveEnvironmentInfo
    module Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo,

    -- ** DescribePlatformVersion
    module Network.AWS.ElasticBeanstalk.DescribePlatformVersion,

    -- ** DeleteApplication
    module Network.AWS.ElasticBeanstalk.DeleteApplication,

    -- ** UpdateApplication
    module Network.AWS.ElasticBeanstalk.UpdateApplication,

    -- ** DescribeInstancesHealth
    module Network.AWS.ElasticBeanstalk.DescribeInstancesHealth,

    -- ** CreateApplication
    module Network.AWS.ElasticBeanstalk.CreateApplication,

    -- ** ComposeEnvironments
    module Network.AWS.ElasticBeanstalk.ComposeEnvironments,

    -- ** AbortEnvironmentUpdate
    module Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate,

    -- ** DeleteConfigurationTemplate
    module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate,

    -- ** UpdateConfigurationTemplate
    module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate,

    -- ** UpdateTagsForResource
    module Network.AWS.ElasticBeanstalk.UpdateTagsForResource,

    -- ** DescribeEnvironmentResources
    module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources,

    -- ** DescribeEnvironmentManagedActionHistory (Paginated)
    module Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory,

    -- ** DeleteApplicationVersion
    module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion,

    -- ** UpdateApplicationVersion
    module Network.AWS.ElasticBeanstalk.UpdateApplicationVersion,

    -- ** CreateConfigurationTemplate
    module Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate,

    -- ** DescribeEnvironmentHealth
    module Network.AWS.ElasticBeanstalk.DescribeEnvironmentHealth,

    -- ** RebuildEnvironment
    module Network.AWS.ElasticBeanstalk.RebuildEnvironment,

    -- ** DeleteEnvironmentConfiguration
    module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration,

    -- ** UpdateApplicationResourceLifecycle
    module Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle,

    -- ** SwapEnvironmentCNAMEs
    module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs,

    -- ** ListAvailableSolutionStacks
    module Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks,

    -- ** ApplyEnvironmentManagedAction
    module Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction,

    -- ** DescribeConfigurationOptions
    module Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions,

    -- ** DisassociateEnvironmentOperationsRole
    module Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole,

    -- ** CreateStorageLocation
    module Network.AWS.ElasticBeanstalk.CreateStorageLocation,

    -- ** DescribeEnvironmentManagedActions
    module Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActions,

    -- ** DescribeConfigurationSettings
    module Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings,

    -- ** ValidateConfigurationSettings
    module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings,

    -- ** DescribeAccountAttributes
    module Network.AWS.ElasticBeanstalk.DescribeAccountAttributes,

    -- ** AssociateEnvironmentOperationsRole
    module Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole,

    -- ** RestartAppServer
    module Network.AWS.ElasticBeanstalk.RestartAppServer,

    -- ** DescribeEnvironments (Paginated)
    module Network.AWS.ElasticBeanstalk.DescribeEnvironments,

    -- ** CheckDNSAvailability
    module Network.AWS.ElasticBeanstalk.CheckDNSAvailability,

    -- ** DescribeApplicationVersions (Paginated)
    module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions,

    -- ** CreateEnvironment
    module Network.AWS.ElasticBeanstalk.CreateEnvironment,

    -- ** CreatePlatformVersion
    module Network.AWS.ElasticBeanstalk.CreatePlatformVersion,

    -- * Types

    -- ** ActionHistoryStatus
    ActionHistoryStatus (..),

    -- ** ActionStatus
    ActionStatus (..),

    -- ** ActionType
    ActionType (..),

    -- ** ApplicationVersionStatus
    ApplicationVersionStatus (..),

    -- ** ComputeType
    ComputeType (..),

    -- ** ConfigurationDeploymentStatus
    ConfigurationDeploymentStatus (..),

    -- ** ConfigurationOptionValueType
    ConfigurationOptionValueType (..),

    -- ** EnvironmentHealth
    EnvironmentHealth (..),

    -- ** EnvironmentHealthAttribute
    EnvironmentHealthAttribute (..),

    -- ** EnvironmentHealthStatus
    EnvironmentHealthStatus (..),

    -- ** EnvironmentInfoType
    EnvironmentInfoType (..),

    -- ** EnvironmentStatus
    EnvironmentStatus (..),

    -- ** EventSeverity
    EventSeverity (..),

    -- ** FailureType
    FailureType (..),

    -- ** InstancesHealthAttribute
    InstancesHealthAttribute (..),

    -- ** PlatformStatus
    PlatformStatus (..),

    -- ** SourceRepository
    SourceRepository (..),

    -- ** SourceType
    SourceType (..),

    -- ** ValidationSeverity
    ValidationSeverity (..),

    -- ** ApplicationDescription
    ApplicationDescription (..),
    mkApplicationDescription,
    adApplicationARN,
    adVersions,
    adDateUpdated,
    adDateCreated,
    adApplicationName,
    adConfigurationTemplates,
    adResourceLifecycleConfig,
    adDescription,

    -- ** ApplicationDescriptionMessage
    ApplicationDescriptionMessage (..),
    mkApplicationDescriptionMessage,
    admApplication,

    -- ** ApplicationMetrics
    ApplicationMetrics (..),
    mkApplicationMetrics,
    amRequestCount,
    amLatency,
    amStatusCodes,
    amDuration,

    -- ** ApplicationResourceLifecycleConfig
    ApplicationResourceLifecycleConfig (..),
    mkApplicationResourceLifecycleConfig,
    arlcVersionLifecycleConfig,
    arlcServiceRole,

    -- ** ApplicationVersionDescription
    ApplicationVersionDescription (..),
    mkApplicationVersionDescription,
    avdStatus,
    avdSourceBundle,
    avdDateUpdated,
    avdDateCreated,
    avdVersionLabel,
    avdSourceBuildInformation,
    avdApplicationName,
    avdApplicationVersionARN,
    avdBuildARN,
    avdDescription,

    -- ** ApplicationVersionDescriptionMessage
    ApplicationVersionDescriptionMessage (..),
    mkApplicationVersionDescriptionMessage,
    avdmApplicationVersion,

    -- ** ApplicationVersionLifecycleConfig
    ApplicationVersionLifecycleConfig (..),
    mkApplicationVersionLifecycleConfig,
    avlcMaxAgeRule,
    avlcMaxCountRule,

    -- ** AutoScalingGroup
    AutoScalingGroup (..),
    mkAutoScalingGroup,
    asgName,

    -- ** BuildConfiguration
    BuildConfiguration (..),
    mkBuildConfiguration,
    bcArtifactName,
    bcComputeType,
    bcTimeoutInMinutes,
    bcCodeBuildServiceRole,
    bcImage,

    -- ** Builder
    Builder (..),
    mkBuilder,
    bARN,

    -- ** CPUUtilization
    CPUUtilization (..),
    mkCPUUtilization,
    cuSoftIRQ,
    cuIdle,
    cuIRQ,
    cuSystem,
    cuPrivileged,
    cuUser,
    cuIOWait,
    cuNice,

    -- ** ConfigurationOptionDescription
    ConfigurationOptionDescription (..),
    mkConfigurationOptionDescription,
    codMaxValue,
    codRegex,
    codMaxLength,
    codUserDefined,
    codNamespace,
    codValueOptions,
    codName,
    codChangeSeverity,
    codDefaultValue,
    codValueType,
    codMinValue,

    -- ** ConfigurationOptionSetting
    ConfigurationOptionSetting (..),
    mkConfigurationOptionSetting,
    cosOptionName,
    cosResourceName,
    cosNamespace,
    cosValue,

    -- ** ConfigurationSettingsDescription
    ConfigurationSettingsDescription (..),
    mkConfigurationSettingsDescription,
    csdTemplateName,
    csdOptionSettings,
    csdDateUpdated,
    csdDateCreated,
    csdPlatformARN,
    csdEnvironmentName,
    csdApplicationName,
    csdDeploymentStatus,
    csdSolutionStackName,
    csdDescription,

    -- ** CustomAMI
    CustomAMI (..),
    mkCustomAMI,
    caVirtualizationType,
    caImageId,

    -- ** Deployment
    Deployment (..),
    mkDeployment,
    dDeploymentId,
    dStatus,
    dDeploymentTime,
    dVersionLabel,

    -- ** EnvironmentDescription
    EnvironmentDescription (..),
    mkEnvironmentDescription,
    eStatus,
    eCNAME,
    eTemplateName,
    eAbortableOperationInProgress,
    eEndpointURL,
    eResources,
    eDateUpdated,
    eDateCreated,
    eHealth,
    eVersionLabel,
    eOperationsRole,
    ePlatformARN,
    eTier,
    eEnvironmentName,
    eApplicationName,
    eEnvironmentARN,
    eSolutionStackName,
    eEnvironmentId,
    eHealthStatus,
    eEnvironmentLinks,
    eDescription,

    -- ** EnvironmentDescriptionsMessage
    EnvironmentDescriptionsMessage (..),
    mkEnvironmentDescriptionsMessage,
    edmNextToken,
    edmEnvironments,

    -- ** EnvironmentInfoDescription
    EnvironmentInfoDescription (..),
    mkEnvironmentInfoDescription,
    eidSampleTimestamp,
    eidEC2InstanceId,
    eidInfoType,
    eidMessage,

    -- ** EnvironmentLink
    EnvironmentLink (..),
    mkEnvironmentLink,
    elLinkName,
    elEnvironmentName,

    -- ** EnvironmentResourceDescription
    EnvironmentResourceDescription (..),
    mkEnvironmentResourceDescription,
    erdQueues,
    erdTriggers,
    erdLaunchTemplates,
    erdLoadBalancers,
    erdEnvironmentName,
    erdInstances,
    erdLaunchConfigurations,
    erdAutoScalingGroups,

    -- ** EnvironmentResourcesDescription
    EnvironmentResourcesDescription (..),
    mkEnvironmentResourcesDescription,
    erdLoadBalancer,

    -- ** EnvironmentTier
    EnvironmentTier (..),
    mkEnvironmentTier,
    etName,
    etVersion,
    etType,

    -- ** EventDescription
    EventDescription (..),
    mkEventDescription,
    edRequestId,
    edTemplateName,
    edSeverity,
    edVersionLabel,
    edPlatformARN,
    edEnvironmentName,
    edApplicationName,
    edEventDate,
    edMessage,

    -- ** Instance
    Instance (..),
    mkInstance,
    iId,

    -- ** InstanceHealthSummary
    InstanceHealthSummary (..),
    mkInstanceHealthSummary,
    ihsOK,
    ihsPending,
    ihsSevere,
    ihsUnknown,
    ihsNoData,
    ihsWarning,
    ihsDegraded,
    ihsInfo,

    -- ** Latency
    Latency (..),
    mkLatency,
    lP75,
    lP50,
    lP85,
    lP999,
    lP90,
    lP95,
    lP99,
    lP10,

    -- ** LaunchConfiguration
    LaunchConfiguration (..),
    mkLaunchConfiguration,
    lcName,

    -- ** LaunchTemplate
    LaunchTemplate (..),
    mkLaunchTemplate,
    ltId,

    -- ** Listener
    Listener (..),
    mkListener,
    lProtocol,
    lPort,

    -- ** LoadBalancer
    LoadBalancer (..),
    mkLoadBalancer,
    lbName,

    -- ** LoadBalancerDescription
    LoadBalancerDescription (..),
    mkLoadBalancerDescription,
    lbdLoadBalancerName,
    lbdDomain,
    lbdListeners,

    -- ** ManagedAction
    ManagedAction (..),
    mkManagedAction,
    maStatus,
    maActionId,
    maWindowStartTime,
    maActionDescription,
    maActionType,

    -- ** ManagedActionHistoryItem
    ManagedActionHistoryItem (..),
    mkManagedActionHistoryItem,
    mahiStatus,
    mahiFailureType,
    mahiActionId,
    mahiFailureDescription,
    mahiFinishedTime,
    mahiActionDescription,
    mahiExecutedTime,
    mahiActionType,

    -- ** MaxAgeRule
    MaxAgeRule (..),
    mkMaxAgeRule,
    marDeleteSourceFromS3,
    marMaxAgeInDays,
    marEnabled,

    -- ** MaxCountRule
    MaxCountRule (..),
    mkMaxCountRule,
    mcrMaxCount,
    mcrDeleteSourceFromS3,
    mcrEnabled,

    -- ** OptionRestrictionRegex
    OptionRestrictionRegex (..),
    mkOptionRestrictionRegex,
    orrPattern,
    orrLabel,

    -- ** OptionSpecification
    OptionSpecification (..),
    mkOptionSpecification,
    osOptionName,
    osResourceName,
    osNamespace,

    -- ** PlatformBranchSummary
    PlatformBranchSummary (..),
    mkPlatformBranchSummary,
    pbsBranchName,
    pbsBranchOrder,
    pbsPlatformName,
    pbsSupportedTierList,
    pbsLifecycleState,

    -- ** PlatformDescription
    PlatformDescription (..),
    mkPlatformDescription,
    pdPlatformBranchName,
    pdSupportedAddonList,
    pdPlatformCategory,
    pdPlatformBranchLifecycleState,
    pdPlatformVersion,
    pdPlatformStatus,
    pdMaintainer,
    pdPlatformLifecycleState,
    pdPlatformOwner,
    pdDateUpdated,
    pdCustomAMIList,
    pdDateCreated,
    pdOperatingSystemName,
    pdFrameworks,
    pdPlatformARN,
    pdOperatingSystemVersion,
    pdProgrammingLanguages,
    pdSolutionStackName,
    pdPlatformName,
    pdDescription,
    pdSupportedTierList,

    -- ** PlatformFilter
    PlatformFilter (..),
    mkPlatformFilter,
    pfValues,
    pfOperator,
    pfType,

    -- ** PlatformFramework
    PlatformFramework (..),
    mkPlatformFramework,
    pfName,
    pfVersion,

    -- ** PlatformProgrammingLanguage
    PlatformProgrammingLanguage (..),
    mkPlatformProgrammingLanguage,
    pplName,
    pplVersion,

    -- ** PlatformSummary
    PlatformSummary (..),
    mkPlatformSummary,
    psPlatformBranchName,
    psSupportedAddonList,
    psPlatformCategory,
    psPlatformBranchLifecycleState,
    psPlatformVersion,
    psPlatformStatus,
    psPlatformLifecycleState,
    psPlatformOwner,
    psOperatingSystemName,
    psPlatformARN,
    psOperatingSystemVersion,
    psSupportedTierList,

    -- ** Queue
    Queue (..),
    mkQueue,
    qURL,
    qName,

    -- ** ResourceQuota
    ResourceQuota (..),
    mkResourceQuota,
    rqMaximum,

    -- ** ResourceQuotas
    ResourceQuotas (..),
    mkResourceQuotas,
    rqApplicationQuota,
    rqCustomPlatformQuota,
    rqApplicationVersionQuota,
    rqEnvironmentQuota,
    rqConfigurationTemplateQuota,

    -- ** S3Location
    S3Location (..),
    mkS3Location,
    slS3Key,
    slS3Bucket,

    -- ** SearchFilter
    SearchFilter (..),
    mkSearchFilter,
    sfAttribute,
    sfValues,
    sfOperator,

    -- ** SingleInstanceHealth
    SingleInstanceHealth (..),
    mkSingleInstanceHealth,
    sihInstanceId,
    sihCauses,
    sihSystem,
    sihApplicationMetrics,
    sihColor,
    sihInstanceType,
    sihAvailabilityZone,
    sihHealthStatus,
    sihDeployment,
    sihLaunchedAt,

    -- ** SolutionStackDescription
    SolutionStackDescription (..),
    mkSolutionStackDescription,
    ssdPermittedFileTypes,
    ssdSolutionStackName,

    -- ** SourceBuildInformation
    SourceBuildInformation (..),
    mkSourceBuildInformation,
    sbiSourceType,
    sbiSourceRepository,
    sbiSourceLocation,

    -- ** SourceConfiguration
    SourceConfiguration (..),
    mkSourceConfiguration,
    scTemplateName,
    scApplicationName,

    -- ** StatusCodes
    StatusCodes (..),
    mkStatusCodes,
    scStatus2xx,
    scStatus3xx,
    scStatus4xx,
    scStatus5xx,

    -- ** SystemStatus
    SystemStatus (..),
    mkSystemStatus,
    ssCPUUtilization,
    ssLoadAverage,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** Trigger
    Trigger (..),
    mkTrigger,
    tName,

    -- ** ValidationMessage
    ValidationMessage (..),
    mkValidationMessage,
    vmOptionName,
    vmSeverity,
    vmNamespace,
    vmMessage,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
import Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction
import Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole
import Network.AWS.ElasticBeanstalk.CheckDNSAvailability
import Network.AWS.ElasticBeanstalk.ComposeEnvironments
import Network.AWS.ElasticBeanstalk.CreateApplication
import Network.AWS.ElasticBeanstalk.CreateApplicationVersion
import Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.CreateEnvironment
import Network.AWS.ElasticBeanstalk.CreatePlatformVersion
import Network.AWS.ElasticBeanstalk.CreateStorageLocation
import Network.AWS.ElasticBeanstalk.DeleteApplication
import Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
import Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
import Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
import Network.AWS.ElasticBeanstalk.DeletePlatformVersion
import Network.AWS.ElasticBeanstalk.DescribeAccountAttributes
import Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
import Network.AWS.ElasticBeanstalk.DescribeApplications
import Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
import Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentHealth
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActions
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
import Network.AWS.ElasticBeanstalk.DescribeEnvironments
import Network.AWS.ElasticBeanstalk.DescribeEvents
import Network.AWS.ElasticBeanstalk.DescribeInstancesHealth
import Network.AWS.ElasticBeanstalk.DescribePlatformVersion
import Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
import Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
import Network.AWS.ElasticBeanstalk.ListPlatformBranches
import Network.AWS.ElasticBeanstalk.ListPlatformVersions
import Network.AWS.ElasticBeanstalk.ListTagsForResource
import Network.AWS.ElasticBeanstalk.RebuildEnvironment
import Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
import Network.AWS.ElasticBeanstalk.RestartAppServer
import Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
import Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
import Network.AWS.ElasticBeanstalk.TerminateEnvironment
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.UpdateApplication
import Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle
import Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
import Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.UpdateEnvironment
import Network.AWS.ElasticBeanstalk.UpdateTagsForResource
import Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
import Network.AWS.ElasticBeanstalk.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ElasticBeanstalk'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
