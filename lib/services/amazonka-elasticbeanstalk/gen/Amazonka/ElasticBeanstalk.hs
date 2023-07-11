{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ElasticBeanstalk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2010-12-01@ of the AWS service descriptions, licensed under Apache 2.0.
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
module Amazonka.ElasticBeanstalk
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CodeBuildNotInServiceRegionException
    _CodeBuildNotInServiceRegionException,

    -- ** ElasticBeanstalkServiceException
    _ElasticBeanstalkServiceException,

    -- ** InsufficientPrivilegesException
    _InsufficientPrivilegesException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ManagedActionInvalidStateException
    _ManagedActionInvalidStateException,

    -- ** OperationInProgressException
    _OperationInProgressException,

    -- ** PlatformVersionStillReferencedException
    _PlatformVersionStillReferencedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceTypeNotSupportedException
    _ResourceTypeNotSupportedException,

    -- ** S3LocationNotInServiceRegionException
    _S3LocationNotInServiceRegionException,

    -- ** S3SubscriptionRequiredException
    _S3SubscriptionRequiredException,

    -- ** SourceBundleDeletionException
    _SourceBundleDeletionException,

    -- ** TooManyApplicationVersionsException
    _TooManyApplicationVersionsException,

    -- ** TooManyApplicationsException
    _TooManyApplicationsException,

    -- ** TooManyBucketsException
    _TooManyBucketsException,

    -- ** TooManyConfigurationTemplatesException
    _TooManyConfigurationTemplatesException,

    -- ** TooManyEnvironmentsException
    _TooManyEnvironmentsException,

    -- ** TooManyPlatformsException
    _TooManyPlatformsException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- * Waiters
    -- $waiters

    -- ** EnvironmentExists
    newEnvironmentExists,

    -- ** EnvironmentTerminated
    newEnvironmentTerminated,

    -- ** EnvironmentUpdated
    newEnvironmentUpdated,

    -- * Operations
    -- $operations

    -- ** AbortEnvironmentUpdate
    AbortEnvironmentUpdate (AbortEnvironmentUpdate'),
    newAbortEnvironmentUpdate,
    AbortEnvironmentUpdateResponse (AbortEnvironmentUpdateResponse'),
    newAbortEnvironmentUpdateResponse,

    -- ** ApplyEnvironmentManagedAction
    ApplyEnvironmentManagedAction (ApplyEnvironmentManagedAction'),
    newApplyEnvironmentManagedAction,
    ApplyEnvironmentManagedActionResponse (ApplyEnvironmentManagedActionResponse'),
    newApplyEnvironmentManagedActionResponse,

    -- ** AssociateEnvironmentOperationsRole
    AssociateEnvironmentOperationsRole (AssociateEnvironmentOperationsRole'),
    newAssociateEnvironmentOperationsRole,
    AssociateEnvironmentOperationsRoleResponse (AssociateEnvironmentOperationsRoleResponse'),
    newAssociateEnvironmentOperationsRoleResponse,

    -- ** CheckDNSAvailability
    CheckDNSAvailability (CheckDNSAvailability'),
    newCheckDNSAvailability,
    CheckDNSAvailabilityResponse (CheckDNSAvailabilityResponse'),
    newCheckDNSAvailabilityResponse,

    -- ** ComposeEnvironments
    ComposeEnvironments (ComposeEnvironments'),
    newComposeEnvironments,
    EnvironmentDescriptionsMessage (EnvironmentDescriptionsMessage'),
    newEnvironmentDescriptionsMessage,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    ApplicationDescriptionMessage (ApplicationDescriptionMessage'),
    newApplicationDescriptionMessage,

    -- ** CreateApplicationVersion
    CreateApplicationVersion (CreateApplicationVersion'),
    newCreateApplicationVersion,
    ApplicationVersionDescriptionMessage (ApplicationVersionDescriptionMessage'),
    newApplicationVersionDescriptionMessage,

    -- ** CreateConfigurationTemplate
    CreateConfigurationTemplate (CreateConfigurationTemplate'),
    newCreateConfigurationTemplate,
    ConfigurationSettingsDescription (ConfigurationSettingsDescription'),
    newConfigurationSettingsDescription,

    -- ** CreateEnvironment
    CreateEnvironment (CreateEnvironment'),
    newCreateEnvironment,
    EnvironmentDescription (EnvironmentDescription'),
    newEnvironmentDescription,

    -- ** CreatePlatformVersion
    CreatePlatformVersion (CreatePlatformVersion'),
    newCreatePlatformVersion,
    CreatePlatformVersionResponse (CreatePlatformVersionResponse'),
    newCreatePlatformVersionResponse,

    -- ** CreateStorageLocation
    CreateStorageLocation (CreateStorageLocation'),
    newCreateStorageLocation,
    CreateStorageLocationResponse (CreateStorageLocationResponse'),
    newCreateStorageLocationResponse,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** DeleteApplicationVersion
    DeleteApplicationVersion (DeleteApplicationVersion'),
    newDeleteApplicationVersion,
    DeleteApplicationVersionResponse (DeleteApplicationVersionResponse'),
    newDeleteApplicationVersionResponse,

    -- ** DeleteConfigurationTemplate
    DeleteConfigurationTemplate (DeleteConfigurationTemplate'),
    newDeleteConfigurationTemplate,
    DeleteConfigurationTemplateResponse (DeleteConfigurationTemplateResponse'),
    newDeleteConfigurationTemplateResponse,

    -- ** DeleteEnvironmentConfiguration
    DeleteEnvironmentConfiguration (DeleteEnvironmentConfiguration'),
    newDeleteEnvironmentConfiguration,
    DeleteEnvironmentConfigurationResponse (DeleteEnvironmentConfigurationResponse'),
    newDeleteEnvironmentConfigurationResponse,

    -- ** DeletePlatformVersion
    DeletePlatformVersion (DeletePlatformVersion'),
    newDeletePlatformVersion,
    DeletePlatformVersionResponse (DeletePlatformVersionResponse'),
    newDeletePlatformVersionResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** DescribeApplicationVersions (Paginated)
    DescribeApplicationVersions (DescribeApplicationVersions'),
    newDescribeApplicationVersions,
    DescribeApplicationVersionsResponse (DescribeApplicationVersionsResponse'),
    newDescribeApplicationVersionsResponse,

    -- ** DescribeApplications
    DescribeApplications (DescribeApplications'),
    newDescribeApplications,
    DescribeApplicationsResponse (DescribeApplicationsResponse'),
    newDescribeApplicationsResponse,

    -- ** DescribeConfigurationOptions
    DescribeConfigurationOptions (DescribeConfigurationOptions'),
    newDescribeConfigurationOptions,
    DescribeConfigurationOptionsResponse (DescribeConfigurationOptionsResponse'),
    newDescribeConfigurationOptionsResponse,

    -- ** DescribeConfigurationSettings
    DescribeConfigurationSettings (DescribeConfigurationSettings'),
    newDescribeConfigurationSettings,
    DescribeConfigurationSettingsResponse (DescribeConfigurationSettingsResponse'),
    newDescribeConfigurationSettingsResponse,

    -- ** DescribeEnvironmentHealth
    DescribeEnvironmentHealth (DescribeEnvironmentHealth'),
    newDescribeEnvironmentHealth,
    DescribeEnvironmentHealthResponse (DescribeEnvironmentHealthResponse'),
    newDescribeEnvironmentHealthResponse,

    -- ** DescribeEnvironmentManagedActionHistory (Paginated)
    DescribeEnvironmentManagedActionHistory (DescribeEnvironmentManagedActionHistory'),
    newDescribeEnvironmentManagedActionHistory,
    DescribeEnvironmentManagedActionHistoryResponse (DescribeEnvironmentManagedActionHistoryResponse'),
    newDescribeEnvironmentManagedActionHistoryResponse,

    -- ** DescribeEnvironmentManagedActions
    DescribeEnvironmentManagedActions (DescribeEnvironmentManagedActions'),
    newDescribeEnvironmentManagedActions,
    DescribeEnvironmentManagedActionsResponse (DescribeEnvironmentManagedActionsResponse'),
    newDescribeEnvironmentManagedActionsResponse,

    -- ** DescribeEnvironmentResources
    DescribeEnvironmentResources (DescribeEnvironmentResources'),
    newDescribeEnvironmentResources,
    DescribeEnvironmentResourcesResponse (DescribeEnvironmentResourcesResponse'),
    newDescribeEnvironmentResourcesResponse,

    -- ** DescribeEnvironments (Paginated)
    DescribeEnvironments (DescribeEnvironments'),
    newDescribeEnvironments,
    EnvironmentDescriptionsMessage (EnvironmentDescriptionsMessage'),
    newEnvironmentDescriptionsMessage,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeInstancesHealth
    DescribeInstancesHealth (DescribeInstancesHealth'),
    newDescribeInstancesHealth,
    DescribeInstancesHealthResponse (DescribeInstancesHealthResponse'),
    newDescribeInstancesHealthResponse,

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

    -- ** ListAvailableSolutionStacks
    ListAvailableSolutionStacks (ListAvailableSolutionStacks'),
    newListAvailableSolutionStacks,
    ListAvailableSolutionStacksResponse (ListAvailableSolutionStacksResponse'),
    newListAvailableSolutionStacksResponse,

    -- ** ListPlatformBranches
    ListPlatformBranches (ListPlatformBranches'),
    newListPlatformBranches,
    ListPlatformBranchesResponse (ListPlatformBranchesResponse'),
    newListPlatformBranchesResponse,

    -- ** ListPlatformVersions (Paginated)
    ListPlatformVersions (ListPlatformVersions'),
    newListPlatformVersions,
    ListPlatformVersionsResponse (ListPlatformVersionsResponse'),
    newListPlatformVersionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RebuildEnvironment
    RebuildEnvironment (RebuildEnvironment'),
    newRebuildEnvironment,
    RebuildEnvironmentResponse (RebuildEnvironmentResponse'),
    newRebuildEnvironmentResponse,

    -- ** RequestEnvironmentInfo
    RequestEnvironmentInfo (RequestEnvironmentInfo'),
    newRequestEnvironmentInfo,
    RequestEnvironmentInfoResponse (RequestEnvironmentInfoResponse'),
    newRequestEnvironmentInfoResponse,

    -- ** RestartAppServer
    RestartAppServer (RestartAppServer'),
    newRestartAppServer,
    RestartAppServerResponse (RestartAppServerResponse'),
    newRestartAppServerResponse,

    -- ** RetrieveEnvironmentInfo
    RetrieveEnvironmentInfo (RetrieveEnvironmentInfo'),
    newRetrieveEnvironmentInfo,
    RetrieveEnvironmentInfoResponse (RetrieveEnvironmentInfoResponse'),
    newRetrieveEnvironmentInfoResponse,

    -- ** SwapEnvironmentCNAMEs
    SwapEnvironmentCNAMEs (SwapEnvironmentCNAMEs'),
    newSwapEnvironmentCNAMEs,
    SwapEnvironmentCNAMEsResponse (SwapEnvironmentCNAMEsResponse'),
    newSwapEnvironmentCNAMEsResponse,

    -- ** TerminateEnvironment
    TerminateEnvironment (TerminateEnvironment'),
    newTerminateEnvironment,
    EnvironmentDescription (EnvironmentDescription'),
    newEnvironmentDescription,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    ApplicationDescriptionMessage (ApplicationDescriptionMessage'),
    newApplicationDescriptionMessage,

    -- ** UpdateApplicationResourceLifecycle
    UpdateApplicationResourceLifecycle (UpdateApplicationResourceLifecycle'),
    newUpdateApplicationResourceLifecycle,
    UpdateApplicationResourceLifecycleResponse (UpdateApplicationResourceLifecycleResponse'),
    newUpdateApplicationResourceLifecycleResponse,

    -- ** UpdateApplicationVersion
    UpdateApplicationVersion (UpdateApplicationVersion'),
    newUpdateApplicationVersion,
    ApplicationVersionDescriptionMessage (ApplicationVersionDescriptionMessage'),
    newApplicationVersionDescriptionMessage,

    -- ** UpdateConfigurationTemplate
    UpdateConfigurationTemplate (UpdateConfigurationTemplate'),
    newUpdateConfigurationTemplate,
    ConfigurationSettingsDescription (ConfigurationSettingsDescription'),
    newConfigurationSettingsDescription,

    -- ** UpdateEnvironment
    UpdateEnvironment (UpdateEnvironment'),
    newUpdateEnvironment,
    EnvironmentDescription (EnvironmentDescription'),
    newEnvironmentDescription,

    -- ** UpdateTagsForResource
    UpdateTagsForResource (UpdateTagsForResource'),
    newUpdateTagsForResource,
    UpdateTagsForResourceResponse (UpdateTagsForResourceResponse'),
    newUpdateTagsForResourceResponse,

    -- ** ValidateConfigurationSettings
    ValidateConfigurationSettings (ValidateConfigurationSettings'),
    newValidateConfigurationSettings,
    ValidateConfigurationSettingsResponse (ValidateConfigurationSettingsResponse'),
    newValidateConfigurationSettingsResponse,

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

import Amazonka.ElasticBeanstalk.AbortEnvironmentUpdate
import Amazonka.ElasticBeanstalk.ApplyEnvironmentManagedAction
import Amazonka.ElasticBeanstalk.AssociateEnvironmentOperationsRole
import Amazonka.ElasticBeanstalk.CheckDNSAvailability
import Amazonka.ElasticBeanstalk.ComposeEnvironments
import Amazonka.ElasticBeanstalk.CreateApplication
import Amazonka.ElasticBeanstalk.CreateApplicationVersion
import Amazonka.ElasticBeanstalk.CreateConfigurationTemplate
import Amazonka.ElasticBeanstalk.CreateEnvironment
import Amazonka.ElasticBeanstalk.CreatePlatformVersion
import Amazonka.ElasticBeanstalk.CreateStorageLocation
import Amazonka.ElasticBeanstalk.DeleteApplication
import Amazonka.ElasticBeanstalk.DeleteApplicationVersion
import Amazonka.ElasticBeanstalk.DeleteConfigurationTemplate
import Amazonka.ElasticBeanstalk.DeleteEnvironmentConfiguration
import Amazonka.ElasticBeanstalk.DeletePlatformVersion
import Amazonka.ElasticBeanstalk.DescribeAccountAttributes
import Amazonka.ElasticBeanstalk.DescribeApplicationVersions
import Amazonka.ElasticBeanstalk.DescribeApplications
import Amazonka.ElasticBeanstalk.DescribeConfigurationOptions
import Amazonka.ElasticBeanstalk.DescribeConfigurationSettings
import Amazonka.ElasticBeanstalk.DescribeEnvironmentHealth
import Amazonka.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
import Amazonka.ElasticBeanstalk.DescribeEnvironmentManagedActions
import Amazonka.ElasticBeanstalk.DescribeEnvironmentResources
import Amazonka.ElasticBeanstalk.DescribeEnvironments
import Amazonka.ElasticBeanstalk.DescribeEvents
import Amazonka.ElasticBeanstalk.DescribeInstancesHealth
import Amazonka.ElasticBeanstalk.DescribePlatformVersion
import Amazonka.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
import Amazonka.ElasticBeanstalk.Lens
import Amazonka.ElasticBeanstalk.ListAvailableSolutionStacks
import Amazonka.ElasticBeanstalk.ListPlatformBranches
import Amazonka.ElasticBeanstalk.ListPlatformVersions
import Amazonka.ElasticBeanstalk.ListTagsForResource
import Amazonka.ElasticBeanstalk.RebuildEnvironment
import Amazonka.ElasticBeanstalk.RequestEnvironmentInfo
import Amazonka.ElasticBeanstalk.RestartAppServer
import Amazonka.ElasticBeanstalk.RetrieveEnvironmentInfo
import Amazonka.ElasticBeanstalk.SwapEnvironmentCNAMEs
import Amazonka.ElasticBeanstalk.TerminateEnvironment
import Amazonka.ElasticBeanstalk.Types
import Amazonka.ElasticBeanstalk.UpdateApplication
import Amazonka.ElasticBeanstalk.UpdateApplicationResourceLifecycle
import Amazonka.ElasticBeanstalk.UpdateApplicationVersion
import Amazonka.ElasticBeanstalk.UpdateConfigurationTemplate
import Amazonka.ElasticBeanstalk.UpdateEnvironment
import Amazonka.ElasticBeanstalk.UpdateTagsForResource
import Amazonka.ElasticBeanstalk.ValidateConfigurationSettings
import Amazonka.ElasticBeanstalk.Waiters

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
