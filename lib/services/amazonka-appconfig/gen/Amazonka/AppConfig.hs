{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AppConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-10-09@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Use AppConfig, a capability of Amazon Web Services Systems Manager, to
-- create, manage, and quickly deploy application configurations. AppConfig
-- supports controlled deployments to applications of any size and includes
-- built-in validation checks and monitoring. You can use AppConfig with
-- applications hosted on Amazon EC2 instances, Lambda, containers, mobile
-- applications, or IoT devices.
--
-- To prevent errors when deploying application configurations, especially
-- for production systems where a simple typo could cause an unexpected
-- outage, AppConfig includes validators. A validator provides a syntactic
-- or semantic check to ensure that the configuration you want to deploy
-- works as intended. To validate your application configuration data, you
-- provide a schema or an Amazon Web Services Lambda function that runs
-- against the configuration. The configuration deployment or update can
-- only proceed when the configuration data is valid.
--
-- During a configuration deployment, AppConfig monitors the application to
-- ensure that the deployment is successful. If the system encounters an
-- error, AppConfig rolls back the change to minimize impact for your
-- application users. You can configure a deployment strategy for each
-- application or environment that includes deployment criteria, including
-- velocity, bake time, and alarms to monitor. Similar to error monitoring,
-- if a deployment triggers an alarm, AppConfig automatically rolls back to
-- the previous version.
--
-- AppConfig supports multiple use cases. Here are some examples:
--
-- -   __Feature flags__: Use AppConfig to turn on new features that
--     require a timely deployment, such as a product launch or
--     announcement.
--
-- -   __Application tuning__: Use AppConfig to carefully introduce changes
--     to your application that can only be tested with production traffic.
--
-- -   __Allow list__: Use AppConfig to allow premium subscribers to access
--     paid content.
--
-- -   __Operational issues__: Use AppConfig to reduce stress on your
--     application when a dependency or other external factor impacts the
--     system.
--
-- This reference is intended to be used with the
-- <http://docs.aws.amazon.com/appconfig/latest/userguide/what-is-appconfig.html AppConfig User Guide>.
module Amazonka.AppConfig
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** PayloadTooLargeException
    _PayloadTooLargeException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    Application (Application'),
    newApplication,

    -- ** CreateConfigurationProfile
    CreateConfigurationProfile (CreateConfigurationProfile'),
    newCreateConfigurationProfile,
    ConfigurationProfile (ConfigurationProfile'),
    newConfigurationProfile,

    -- ** CreateDeploymentStrategy
    CreateDeploymentStrategy (CreateDeploymentStrategy'),
    newCreateDeploymentStrategy,
    DeploymentStrategy (DeploymentStrategy'),
    newDeploymentStrategy,

    -- ** CreateEnvironment
    CreateEnvironment (CreateEnvironment'),
    newCreateEnvironment,
    Environment (Environment'),
    newEnvironment,

    -- ** CreateExtension
    CreateExtension (CreateExtension'),
    newCreateExtension,
    Extension (Extension'),
    newExtension,

    -- ** CreateExtensionAssociation
    CreateExtensionAssociation (CreateExtensionAssociation'),
    newCreateExtensionAssociation,
    ExtensionAssociation (ExtensionAssociation'),
    newExtensionAssociation,

    -- ** CreateHostedConfigurationVersion
    CreateHostedConfigurationVersion (CreateHostedConfigurationVersion'),
    newCreateHostedConfigurationVersion,
    HostedConfigurationVersion (HostedConfigurationVersion'),
    newHostedConfigurationVersion,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** DeleteConfigurationProfile
    DeleteConfigurationProfile (DeleteConfigurationProfile'),
    newDeleteConfigurationProfile,
    DeleteConfigurationProfileResponse (DeleteConfigurationProfileResponse'),
    newDeleteConfigurationProfileResponse,

    -- ** DeleteDeploymentStrategy
    DeleteDeploymentStrategy (DeleteDeploymentStrategy'),
    newDeleteDeploymentStrategy,
    DeleteDeploymentStrategyResponse (DeleteDeploymentStrategyResponse'),
    newDeleteDeploymentStrategyResponse,

    -- ** DeleteEnvironment
    DeleteEnvironment (DeleteEnvironment'),
    newDeleteEnvironment,
    DeleteEnvironmentResponse (DeleteEnvironmentResponse'),
    newDeleteEnvironmentResponse,

    -- ** DeleteExtension
    DeleteExtension (DeleteExtension'),
    newDeleteExtension,
    DeleteExtensionResponse (DeleteExtensionResponse'),
    newDeleteExtensionResponse,

    -- ** DeleteExtensionAssociation
    DeleteExtensionAssociation (DeleteExtensionAssociation'),
    newDeleteExtensionAssociation,
    DeleteExtensionAssociationResponse (DeleteExtensionAssociationResponse'),
    newDeleteExtensionAssociationResponse,

    -- ** DeleteHostedConfigurationVersion
    DeleteHostedConfigurationVersion (DeleteHostedConfigurationVersion'),
    newDeleteHostedConfigurationVersion,
    DeleteHostedConfigurationVersionResponse (DeleteHostedConfigurationVersionResponse'),
    newDeleteHostedConfigurationVersionResponse,

    -- ** GetApplication
    GetApplication (GetApplication'),
    newGetApplication,
    Application (Application'),
    newApplication,

    -- ** GetConfigurationProfile
    GetConfigurationProfile (GetConfigurationProfile'),
    newGetConfigurationProfile,
    ConfigurationProfile (ConfigurationProfile'),
    newConfigurationProfile,

    -- ** GetDeployment
    GetDeployment (GetDeployment'),
    newGetDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** GetDeploymentStrategy
    GetDeploymentStrategy (GetDeploymentStrategy'),
    newGetDeploymentStrategy,
    DeploymentStrategy (DeploymentStrategy'),
    newDeploymentStrategy,

    -- ** GetEnvironment
    GetEnvironment (GetEnvironment'),
    newGetEnvironment,
    Environment (Environment'),
    newEnvironment,

    -- ** GetExtension
    GetExtension (GetExtension'),
    newGetExtension,
    Extension (Extension'),
    newExtension,

    -- ** GetExtensionAssociation
    GetExtensionAssociation (GetExtensionAssociation'),
    newGetExtensionAssociation,
    ExtensionAssociation (ExtensionAssociation'),
    newExtensionAssociation,

    -- ** GetHostedConfigurationVersion
    GetHostedConfigurationVersion (GetHostedConfigurationVersion'),
    newGetHostedConfigurationVersion,
    HostedConfigurationVersion (HostedConfigurationVersion'),
    newHostedConfigurationVersion,

    -- ** ListApplications
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** ListConfigurationProfiles
    ListConfigurationProfiles (ListConfigurationProfiles'),
    newListConfigurationProfiles,
    ListConfigurationProfilesResponse (ListConfigurationProfilesResponse'),
    newListConfigurationProfilesResponse,

    -- ** ListDeploymentStrategies
    ListDeploymentStrategies (ListDeploymentStrategies'),
    newListDeploymentStrategies,
    ListDeploymentStrategiesResponse (ListDeploymentStrategiesResponse'),
    newListDeploymentStrategiesResponse,

    -- ** ListDeployments
    ListDeployments (ListDeployments'),
    newListDeployments,
    ListDeploymentsResponse (ListDeploymentsResponse'),
    newListDeploymentsResponse,

    -- ** ListEnvironments
    ListEnvironments (ListEnvironments'),
    newListEnvironments,
    ListEnvironmentsResponse (ListEnvironmentsResponse'),
    newListEnvironmentsResponse,

    -- ** ListExtensionAssociations
    ListExtensionAssociations (ListExtensionAssociations'),
    newListExtensionAssociations,
    ListExtensionAssociationsResponse (ListExtensionAssociationsResponse'),
    newListExtensionAssociationsResponse,

    -- ** ListExtensions
    ListExtensions (ListExtensions'),
    newListExtensions,
    ListExtensionsResponse (ListExtensionsResponse'),
    newListExtensionsResponse,

    -- ** ListHostedConfigurationVersions
    ListHostedConfigurationVersions (ListHostedConfigurationVersions'),
    newListHostedConfigurationVersions,
    ListHostedConfigurationVersionsResponse (ListHostedConfigurationVersionsResponse'),
    newListHostedConfigurationVersionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartDeployment
    StartDeployment (StartDeployment'),
    newStartDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** StopDeployment
    StopDeployment (StopDeployment'),
    newStopDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    Application (Application'),
    newApplication,

    -- ** UpdateConfigurationProfile
    UpdateConfigurationProfile (UpdateConfigurationProfile'),
    newUpdateConfigurationProfile,
    ConfigurationProfile (ConfigurationProfile'),
    newConfigurationProfile,

    -- ** UpdateDeploymentStrategy
    UpdateDeploymentStrategy (UpdateDeploymentStrategy'),
    newUpdateDeploymentStrategy,
    DeploymentStrategy (DeploymentStrategy'),
    newDeploymentStrategy,

    -- ** UpdateEnvironment
    UpdateEnvironment (UpdateEnvironment'),
    newUpdateEnvironment,
    Environment (Environment'),
    newEnvironment,

    -- ** UpdateExtension
    UpdateExtension (UpdateExtension'),
    newUpdateExtension,
    Extension (Extension'),
    newExtension,

    -- ** UpdateExtensionAssociation
    UpdateExtensionAssociation (UpdateExtensionAssociation'),
    newUpdateExtensionAssociation,
    ExtensionAssociation (ExtensionAssociation'),
    newExtensionAssociation,

    -- ** ValidateConfiguration
    ValidateConfiguration (ValidateConfiguration'),
    newValidateConfiguration,
    ValidateConfigurationResponse (ValidateConfigurationResponse'),
    newValidateConfigurationResponse,

    -- * Types

    -- ** ActionPoint
    ActionPoint (..),

    -- ** DeploymentEventType
    DeploymentEventType (..),

    -- ** DeploymentState
    DeploymentState (..),

    -- ** EnvironmentState
    EnvironmentState (..),

    -- ** GrowthType
    GrowthType (..),

    -- ** ReplicateTo
    ReplicateTo (..),

    -- ** TriggeredBy
    TriggeredBy (..),

    -- ** ValidatorType
    ValidatorType (..),

    -- ** Action
    Action (Action'),
    newAction,

    -- ** ActionInvocation
    ActionInvocation (ActionInvocation'),
    newActionInvocation,

    -- ** Application
    Application (Application'),
    newApplication,

    -- ** AppliedExtension
    AppliedExtension (AppliedExtension'),
    newAppliedExtension,

    -- ** ConfigurationProfile
    ConfigurationProfile (ConfigurationProfile'),
    newConfigurationProfile,

    -- ** ConfigurationProfileSummary
    ConfigurationProfileSummary (ConfigurationProfileSummary'),
    newConfigurationProfileSummary,

    -- ** Deployment
    Deployment (Deployment'),
    newDeployment,

    -- ** DeploymentEvent
    DeploymentEvent (DeploymentEvent'),
    newDeploymentEvent,

    -- ** DeploymentStrategy
    DeploymentStrategy (DeploymentStrategy'),
    newDeploymentStrategy,

    -- ** DeploymentSummary
    DeploymentSummary (DeploymentSummary'),
    newDeploymentSummary,

    -- ** Environment
    Environment (Environment'),
    newEnvironment,

    -- ** Extension
    Extension (Extension'),
    newExtension,

    -- ** ExtensionAssociation
    ExtensionAssociation (ExtensionAssociation'),
    newExtensionAssociation,

    -- ** ExtensionAssociationSummary
    ExtensionAssociationSummary (ExtensionAssociationSummary'),
    newExtensionAssociationSummary,

    -- ** ExtensionSummary
    ExtensionSummary (ExtensionSummary'),
    newExtensionSummary,

    -- ** HostedConfigurationVersion
    HostedConfigurationVersion (HostedConfigurationVersion'),
    newHostedConfigurationVersion,

    -- ** HostedConfigurationVersionSummary
    HostedConfigurationVersionSummary (HostedConfigurationVersionSummary'),
    newHostedConfigurationVersionSummary,

    -- ** Monitor
    Monitor (Monitor'),
    newMonitor,

    -- ** Parameter
    Parameter (Parameter'),
    newParameter,

    -- ** Validator
    Validator (Validator'),
    newValidator,
  )
where

import Amazonka.AppConfig.CreateApplication
import Amazonka.AppConfig.CreateConfigurationProfile
import Amazonka.AppConfig.CreateDeploymentStrategy
import Amazonka.AppConfig.CreateEnvironment
import Amazonka.AppConfig.CreateExtension
import Amazonka.AppConfig.CreateExtensionAssociation
import Amazonka.AppConfig.CreateHostedConfigurationVersion
import Amazonka.AppConfig.DeleteApplication
import Amazonka.AppConfig.DeleteConfigurationProfile
import Amazonka.AppConfig.DeleteDeploymentStrategy
import Amazonka.AppConfig.DeleteEnvironment
import Amazonka.AppConfig.DeleteExtension
import Amazonka.AppConfig.DeleteExtensionAssociation
import Amazonka.AppConfig.DeleteHostedConfigurationVersion
import Amazonka.AppConfig.GetApplication
import Amazonka.AppConfig.GetConfigurationProfile
import Amazonka.AppConfig.GetDeployment
import Amazonka.AppConfig.GetDeploymentStrategy
import Amazonka.AppConfig.GetEnvironment
import Amazonka.AppConfig.GetExtension
import Amazonka.AppConfig.GetExtensionAssociation
import Amazonka.AppConfig.GetHostedConfigurationVersion
import Amazonka.AppConfig.Lens
import Amazonka.AppConfig.ListApplications
import Amazonka.AppConfig.ListConfigurationProfiles
import Amazonka.AppConfig.ListDeploymentStrategies
import Amazonka.AppConfig.ListDeployments
import Amazonka.AppConfig.ListEnvironments
import Amazonka.AppConfig.ListExtensionAssociations
import Amazonka.AppConfig.ListExtensions
import Amazonka.AppConfig.ListHostedConfigurationVersions
import Amazonka.AppConfig.ListTagsForResource
import Amazonka.AppConfig.StartDeployment
import Amazonka.AppConfig.StopDeployment
import Amazonka.AppConfig.TagResource
import Amazonka.AppConfig.Types
import Amazonka.AppConfig.UntagResource
import Amazonka.AppConfig.UpdateApplication
import Amazonka.AppConfig.UpdateConfigurationProfile
import Amazonka.AppConfig.UpdateDeploymentStrategy
import Amazonka.AppConfig.UpdateEnvironment
import Amazonka.AppConfig.UpdateExtension
import Amazonka.AppConfig.UpdateExtensionAssociation
import Amazonka.AppConfig.ValidateConfiguration
import Amazonka.AppConfig.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AppConfig'.

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
