{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Elastic Beanstalk
--
-- AWS Elastic Beanstalk makes it easy for you to create, deploy, and
-- manage scalable, fault-tolerant applications running on the Amazon Web
-- Services cloud.
--
-- For more information about this product, go to the
-- <http://aws.amazon.com/elasticbeanstalk/ AWS Elastic Beanstalk> details
-- page. The location of the latest AWS Elastic Beanstalk WSDL is
-- <https://elasticbeanstalk.s3.amazonaws.com/doc/2010-12-01/AWSElasticBeanstalk.wsdl>.
-- To install the Software Development Kits (SDKs), Integrated Development
-- Environment (IDE) Toolkits, and command line tools that enable you to
-- access the API, go to
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
--
-- __Endpoints__
--
-- For a list of region-specific endpoints that AWS Elastic Beanstalk
-- supports, go to
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#elasticbeanstalk_region Regions and Endpoints>
-- in the /Amazon Web Services Glossary/.
module Network.AWS.ElasticBeanstalk
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** TooManyPlatformsException
    _TooManyPlatformsException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** TooManyApplicationsException
    _TooManyApplicationsException,

    -- ** ResourceTypeNotSupportedException
    _ResourceTypeNotSupportedException,

    -- ** TooManyEnvironmentsException
    _TooManyEnvironmentsException,

    -- ** InsufficientPrivilegesException
    _InsufficientPrivilegesException,

    -- ** TooManyConfigurationTemplatesException
    _TooManyConfigurationTemplatesException,

    -- ** OperationInProgressException
    _OperationInProgressException,

    -- ** TooManyBucketsException
    _TooManyBucketsException,

    -- ** S3SubscriptionRequiredException
    _S3SubscriptionRequiredException,

    -- ** SourceBundleDeletionException
    _SourceBundleDeletionException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ElasticBeanstalkServiceException
    _ElasticBeanstalkServiceException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** CodeBuildNotInServiceRegionException
    _CodeBuildNotInServiceRegionException,

    -- ** PlatformVersionStillReferencedException
    _PlatformVersionStillReferencedException,

    -- ** TooManyApplicationVersionsException
    _TooManyApplicationVersionsException,

    -- ** S3LocationNotInServiceRegionException
    _S3LocationNotInServiceRegionException,

    -- ** ManagedActionInvalidStateException
    _ManagedActionInvalidStateException,

    -- * Waiters
    -- $waiters

    -- ** EnvironmentTerminated
    newEnvironmentTerminated,

    -- ** EnvironmentUpdated
    newEnvironmentUpdated,

    -- ** EnvironmentExists
    newEnvironmentExists,

    -- * Operations
    -- $operations

    -- ** SwapEnvironmentCNAMEs
    SwapEnvironmentCNAMEs (SwapEnvironmentCNAMEs'),
    newSwapEnvironmentCNAMEs,
    SwapEnvironmentCNAMEsResponse (SwapEnvironmentCNAMEsResponse'),
    newSwapEnvironmentCNAMEsResponse,

    -- ** ListPlatformBranches
    ListPlatformBranches (ListPlatformBranches'),
    newListPlatformBranches,
    ListPlatformBranchesResponse (ListPlatformBranchesResponse'),
    newListPlatformBranchesResponse,

    -- ** ListAvailableSolutionStacks
    ListAvailableSolutionStacks (ListAvailableSolutionStacks'),
    newListAvailableSolutionStacks,
    ListAvailableSolutionStacksResponse (ListAvailableSolutionStacksResponse'),
    newListAvailableSolutionStacksResponse,

    -- ** DescribeEnvironmentHealth
    DescribeEnvironmentHealth (DescribeEnvironmentHealth'),
    newDescribeEnvironmentHealth,
    DescribeEnvironmentHealthResponse (DescribeEnvironmentHealthResponse'),
    newDescribeEnvironmentHealthResponse,

    -- ** CreateConfigurationTemplate
    CreateConfigurationTemplate (CreateConfigurationTemplate'),
    newCreateConfigurationTemplate,
    ConfigurationSettingsDescription (ConfigurationSettingsDescription'),
    newConfigurationSettingsDescription,

    -- ** DescribeApplications
    DescribeApplications (DescribeApplications'),
    newDescribeApplications,
    DescribeApplicationsResponse (DescribeApplicationsResponse'),
    newDescribeApplicationsResponse,

    -- ** ListPlatformVersions (Paginated)
    ListPlatformVersions (ListPlatformVersions'),
    newListPlatformVersions,
    ListPlatformVersionsResponse (ListPlatformVersionsResponse'),
    newListPlatformVersionsResponse,

    -- ** CreateApplicationVersion
    CreateApplicationVersion (CreateApplicationVersion'),
    newCreateApplicationVersion,
    ApplicationVersionDescriptionMessage (ApplicationVersionDescriptionMessage'),
    newApplicationVersionDescriptionMessage,

    -- ** TerminateEnvironment
    TerminateEnvironment (TerminateEnvironment'),
    newTerminateEnvironment,
    EnvironmentDescription (EnvironmentDescription'),
    newEnvironmentDescription,

    -- ** DescribeEnvironmentResources
    DescribeEnvironmentResources (DescribeEnvironmentResources'),
    newDescribeEnvironmentResources,
    DescribeEnvironmentResourcesResponse (DescribeEnvironmentResourcesResponse'),
    newDescribeEnvironmentResourcesResponse,

    -- ** UpdateApplicationVersion
    UpdateApplicationVersion (UpdateApplicationVersion'),
    newUpdateApplicationVersion,
    ApplicationVersionDescriptionMessage (ApplicationVersionDescriptionMessage'),
    newApplicationVersionDescriptionMessage,

    -- ** CreatePlatformVersion
    CreatePlatformVersion (CreatePlatformVersion'),
    newCreatePlatformVersion,
    CreatePlatformVersionResponse (CreatePlatformVersionResponse'),
    newCreatePlatformVersionResponse,

    -- ** DeleteApplicationVersion
    DeleteApplicationVersion (DeleteApplicationVersion'),
    newDeleteApplicationVersion,
    DeleteApplicationVersionResponse (DeleteApplicationVersionResponse'),
    newDeleteApplicationVersionResponse,

    -- ** CreateEnvironment
    CreateEnvironment (CreateEnvironment'),
    newCreateEnvironment,
    EnvironmentDescription (EnvironmentDescription'),
    newEnvironmentDescription,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    ApplicationDescriptionMessage (ApplicationDescriptionMessage'),
    newApplicationDescriptionMessage,

    -- ** ComposeEnvironments
    ComposeEnvironments (ComposeEnvironments'),
    newComposeEnvironments,
    EnvironmentDescriptionsMessage (EnvironmentDescriptionsMessage'),
    newEnvironmentDescriptionsMessage,

    -- ** CheckDNSAvailability
    CheckDNSAvailability (CheckDNSAvailability'),
    newCheckDNSAvailability,
    CheckDNSAvailabilityResponse (CheckDNSAvailabilityResponse'),
    newCheckDNSAvailabilityResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** ValidateConfigurationSettings
    ValidateConfigurationSettings (ValidateConfigurationSettings'),
    newValidateConfigurationSettings,
    ValidateConfigurationSettingsResponse (ValidateConfigurationSettingsResponse'),
    newValidateConfigurationSettingsResponse,

    -- ** CreateStorageLocation
    CreateStorageLocation (CreateStorageLocation'),
    newCreateStorageLocation,
    CreateStorageLocationResponse (CreateStorageLocationResponse'),
    newCreateStorageLocationResponse,

    -- ** DescribeEnvironmentManagedActions
    DescribeEnvironmentManagedActions (DescribeEnvironmentManagedActions'),
    newDescribeEnvironmentManagedActions,
    DescribeEnvironmentManagedActionsResponse (DescribeEnvironmentManagedActionsResponse'),
    newDescribeEnvironmentManagedActionsResponse,

    -- ** DescribeConfigurationSettings
    DescribeConfigurationSettings (DescribeConfigurationSettings'),
    newDescribeConfigurationSettings,
    DescribeConfigurationSettingsResponse (DescribeConfigurationSettingsResponse'),
    newDescribeConfigurationSettingsResponse,

    -- ** DescribeConfigurationOptions
    DescribeConfigurationOptions (DescribeConfigurationOptions'),
    newDescribeConfigurationOptions,
    DescribeConfigurationOptionsResponse (DescribeConfigurationOptionsResponse'),
    newDescribeConfigurationOptionsResponse,

    -- ** RetrieveEnvironmentInfo
    RetrieveEnvironmentInfo (RetrieveEnvironmentInfo'),
    newRetrieveEnvironmentInfo,
    RetrieveEnvironmentInfoResponse (RetrieveEnvironmentInfoResponse'),
    newRetrieveEnvironmentInfoResponse,

    -- ** RequestEnvironmentInfo
    RequestEnvironmentInfo (RequestEnvironmentInfo'),
    newRequestEnvironmentInfo,
    RequestEnvironmentInfoResponse (RequestEnvironmentInfoResponse'),
    newRequestEnvironmentInfoResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** ApplyEnvironmentManagedAction
    ApplyEnvironmentManagedAction (ApplyEnvironmentManagedAction'),
    newApplyEnvironmentManagedAction,
    ApplyEnvironmentManagedActionResponse (ApplyEnvironmentManagedActionResponse'),
    newApplyEnvironmentManagedActionResponse,

    -- ** UpdateApplicationResourceLifecycle
    UpdateApplicationResourceLifecycle (UpdateApplicationResourceLifecycle'),
    newUpdateApplicationResourceLifecycle,
    UpdateApplicationResourceLifecycleResponse (UpdateApplicationResourceLifecycleResponse'),
    newUpdateApplicationResourceLifecycleResponse,

    -- ** RebuildEnvironment
    RebuildEnvironment (RebuildEnvironment'),
    newRebuildEnvironment,
    RebuildEnvironmentResponse (RebuildEnvironmentResponse'),
    newRebuildEnvironmentResponse,

    -- ** UpdateEnvironment
    UpdateEnvironment (UpdateEnvironment'),
    newUpdateEnvironment,
    EnvironmentDescription (EnvironmentDescription'),
    newEnvironmentDescription,

    -- ** DeletePlatformVersion
    DeletePlatformVersion (DeletePlatformVersion'),
    newDeletePlatformVersion,
    DeletePlatformVersionResponse (DeletePlatformVersionResponse'),
    newDeletePlatformVersionResponse,

    -- ** DeleteEnvironmentConfiguration
    DeleteEnvironmentConfiguration (DeleteEnvironmentConfiguration'),
    newDeleteEnvironmentConfiguration,
    DeleteEnvironmentConfigurationResponse (DeleteEnvironmentConfigurationResponse'),
    newDeleteEnvironmentConfigurationResponse,

    -- ** DescribeEnvironmentManagedActionHistory (Paginated)
    DescribeEnvironmentManagedActionHistory (DescribeEnvironmentManagedActionHistory'),
    newDescribeEnvironmentManagedActionHistory,
    DescribeEnvironmentManagedActionHistoryResponse (DescribeEnvironmentManagedActionHistoryResponse'),
    newDescribeEnvironmentManagedActionHistoryResponse,

    -- ** UpdateConfigurationTemplate
    UpdateConfigurationTemplate (UpdateConfigurationTemplate'),
    newUpdateConfigurationTemplate,
    ConfigurationSettingsDescription (ConfigurationSettingsDescription'),
    newConfigurationSettingsDescription,

    -- ** DeleteConfigurationTemplate
    DeleteConfigurationTemplate (DeleteConfigurationTemplate'),
    newDeleteConfigurationTemplate,
    DeleteConfigurationTemplateResponse (DeleteConfigurationTemplateResponse'),
    newDeleteConfigurationTemplateResponse,

    -- ** UpdateTagsForResource
    UpdateTagsForResource (UpdateTagsForResource'),
    newUpdateTagsForResource,
    UpdateTagsForResourceResponse (UpdateTagsForResourceResponse'),
    newUpdateTagsForResourceResponse,

    -- ** DescribeApplicationVersions (Paginated)
    DescribeApplicationVersions (DescribeApplicationVersions'),
    newDescribeApplicationVersions,
    DescribeApplicationVersionsResponse (DescribeApplicationVersionsResponse'),
    newDescribeApplicationVersionsResponse,

    -- ** AbortEnvironmentUpdate
    AbortEnvironmentUpdate (AbortEnvironmentUpdate'),
    newAbortEnvironmentUpdate,
    AbortEnvironmentUpdateResponse (AbortEnvironmentUpdateResponse'),
    newAbortEnvironmentUpdateResponse,

    -- ** DescribeEnvironments (Paginated)
    DescribeEnvironments (DescribeEnvironments'),
    newDescribeEnvironments,
    EnvironmentDescriptionsMessage (EnvironmentDescriptionsMessage'),
    newEnvironmentDescriptionsMessage,

    -- ** RestartAppServer
    RestartAppServer (RestartAppServer'),
    newRestartAppServer,
    RestartAppServerResponse (RestartAppServerResponse'),
    newRestartAppServerResponse,

    -- ** AssociateEnvironmentOperationsRole
    AssociateEnvironmentOperationsRole (AssociateEnvironmentOperationsRole'),
    newAssociateEnvironmentOperationsRole,
    AssociateEnvironmentOperationsRoleResponse (AssociateEnvironmentOperationsRoleResponse'),
    newAssociateEnvironmentOperationsRoleResponse,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** DescribeInstancesHealth
    DescribeInstancesHealth (DescribeInstancesHealth'),
    newDescribeInstancesHealth,
    DescribeInstancesHealthResponse (DescribeInstancesHealthResponse'),
    newDescribeInstancesHealthResponse,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    ApplicationDescriptionMessage (ApplicationDescriptionMessage'),
    newApplicationDescriptionMessage,

    -- ** DescribePlatformVersion
    DescribePlatformVersion (DescribePlatformVersion'),
    newDescribePlatformVersion,
    DescribePlatformVersionResponse (DescribePlatformVersionResponse'),
    newDescribePlatformVersionResponse,

    -- ** DisassociateEnvironmentOperationsRole
    DisassociateEnvironmentOperationsRole (DisassociateEnvironmentOperationsRole'),
    newDisassociateEnvironmentOperationsRole,
    DisassociateEnvironmentOperationsRoleResponse (DisassociateEnvironmentOperationsRoleResponse'),
    newDisassociateEnvironmentOperationsRoleResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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
    ApplicationDescription (ApplicationDescription'),
    newApplicationDescription,

    -- ** ApplicationDescriptionMessage
    ApplicationDescriptionMessage (ApplicationDescriptionMessage'),
    newApplicationDescriptionMessage,

    -- ** ApplicationMetrics
    ApplicationMetrics (ApplicationMetrics'),
    newApplicationMetrics,

    -- ** ApplicationResourceLifecycleConfig
    ApplicationResourceLifecycleConfig (ApplicationResourceLifecycleConfig'),
    newApplicationResourceLifecycleConfig,

    -- ** ApplicationVersionDescription
    ApplicationVersionDescription (ApplicationVersionDescription'),
    newApplicationVersionDescription,

    -- ** ApplicationVersionDescriptionMessage
    ApplicationVersionDescriptionMessage (ApplicationVersionDescriptionMessage'),
    newApplicationVersionDescriptionMessage,

    -- ** ApplicationVersionLifecycleConfig
    ApplicationVersionLifecycleConfig (ApplicationVersionLifecycleConfig'),
    newApplicationVersionLifecycleConfig,

    -- ** AutoScalingGroup
    AutoScalingGroup (AutoScalingGroup'),
    newAutoScalingGroup,

    -- ** BuildConfiguration
    BuildConfiguration (BuildConfiguration'),
    newBuildConfiguration,

    -- ** Builder
    Builder (Builder'),
    newBuilder,

    -- ** CPUUtilization
    CPUUtilization (CPUUtilization'),
    newCPUUtilization,

    -- ** ConfigurationOptionDescription
    ConfigurationOptionDescription (ConfigurationOptionDescription'),
    newConfigurationOptionDescription,

    -- ** ConfigurationOptionSetting
    ConfigurationOptionSetting (ConfigurationOptionSetting'),
    newConfigurationOptionSetting,

    -- ** ConfigurationSettingsDescription
    ConfigurationSettingsDescription (ConfigurationSettingsDescription'),
    newConfigurationSettingsDescription,

    -- ** CustomAmi
    CustomAmi (CustomAmi'),
    newCustomAmi,

    -- ** Deployment
    Deployment (Deployment'),
    newDeployment,

    -- ** EnvironmentDescription
    EnvironmentDescription (EnvironmentDescription'),
    newEnvironmentDescription,

    -- ** EnvironmentDescriptionsMessage
    EnvironmentDescriptionsMessage (EnvironmentDescriptionsMessage'),
    newEnvironmentDescriptionsMessage,

    -- ** EnvironmentInfoDescription
    EnvironmentInfoDescription (EnvironmentInfoDescription'),
    newEnvironmentInfoDescription,

    -- ** EnvironmentLink
    EnvironmentLink (EnvironmentLink'),
    newEnvironmentLink,

    -- ** EnvironmentResourceDescription
    EnvironmentResourceDescription (EnvironmentResourceDescription'),
    newEnvironmentResourceDescription,

    -- ** EnvironmentResourcesDescription
    EnvironmentResourcesDescription (EnvironmentResourcesDescription'),
    newEnvironmentResourcesDescription,

    -- ** EnvironmentTier
    EnvironmentTier (EnvironmentTier'),
    newEnvironmentTier,

    -- ** EventDescription
    EventDescription (EventDescription'),
    newEventDescription,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceHealthSummary
    InstanceHealthSummary (InstanceHealthSummary'),
    newInstanceHealthSummary,

    -- ** Latency
    Latency (Latency'),
    newLatency,

    -- ** LaunchConfiguration
    LaunchConfiguration (LaunchConfiguration'),
    newLaunchConfiguration,

    -- ** LaunchTemplate
    LaunchTemplate (LaunchTemplate'),
    newLaunchTemplate,

    -- ** Listener
    Listener (Listener'),
    newListener,

    -- ** LoadBalancer
    LoadBalancer (LoadBalancer'),
    newLoadBalancer,

    -- ** LoadBalancerDescription
    LoadBalancerDescription (LoadBalancerDescription'),
    newLoadBalancerDescription,

    -- ** ManagedAction
    ManagedAction (ManagedAction'),
    newManagedAction,

    -- ** ManagedActionHistoryItem
    ManagedActionHistoryItem (ManagedActionHistoryItem'),
    newManagedActionHistoryItem,

    -- ** MaxAgeRule
    MaxAgeRule (MaxAgeRule'),
    newMaxAgeRule,

    -- ** MaxCountRule
    MaxCountRule (MaxCountRule'),
    newMaxCountRule,

    -- ** OptionRestrictionRegex
    OptionRestrictionRegex (OptionRestrictionRegex'),
    newOptionRestrictionRegex,

    -- ** OptionSpecification
    OptionSpecification (OptionSpecification'),
    newOptionSpecification,

    -- ** PlatformBranchSummary
    PlatformBranchSummary (PlatformBranchSummary'),
    newPlatformBranchSummary,

    -- ** PlatformDescription
    PlatformDescription (PlatformDescription'),
    newPlatformDescription,

    -- ** PlatformFilter
    PlatformFilter (PlatformFilter'),
    newPlatformFilter,

    -- ** PlatformFramework
    PlatformFramework (PlatformFramework'),
    newPlatformFramework,

    -- ** PlatformProgrammingLanguage
    PlatformProgrammingLanguage (PlatformProgrammingLanguage'),
    newPlatformProgrammingLanguage,

    -- ** PlatformSummary
    PlatformSummary (PlatformSummary'),
    newPlatformSummary,

    -- ** Queue
    Queue (Queue'),
    newQueue,

    -- ** ResourceQuota
    ResourceQuota (ResourceQuota'),
    newResourceQuota,

    -- ** ResourceQuotas
    ResourceQuotas (ResourceQuotas'),
    newResourceQuotas,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** SearchFilter
    SearchFilter (SearchFilter'),
    newSearchFilter,

    -- ** SingleInstanceHealth
    SingleInstanceHealth (SingleInstanceHealth'),
    newSingleInstanceHealth,

    -- ** SolutionStackDescription
    SolutionStackDescription (SolutionStackDescription'),
    newSolutionStackDescription,

    -- ** SourceBuildInformation
    SourceBuildInformation (SourceBuildInformation'),
    newSourceBuildInformation,

    -- ** SourceConfiguration
    SourceConfiguration (SourceConfiguration'),
    newSourceConfiguration,

    -- ** StatusCodes
    StatusCodes (StatusCodes'),
    newStatusCodes,

    -- ** SystemStatus
    SystemStatus (SystemStatus'),
    newSystemStatus,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Trigger
    Trigger (Trigger'),
    newTrigger,

    -- ** ValidationMessage
    ValidationMessage (ValidationMessage'),
    newValidationMessage,
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
import Network.AWS.ElasticBeanstalk.Lens
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
