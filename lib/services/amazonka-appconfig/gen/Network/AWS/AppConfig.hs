{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.AppConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-10-09@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS AppConfig
--
-- Use AWS AppConfig, a capability of AWS Systems Manager, to create,
-- manage, and quickly deploy application configurations. AppConfig
-- supports controlled deployments to applications of any size and includes
-- built-in validation checks and monitoring. You can use AppConfig with
-- applications hosted on Amazon EC2 instances, AWS Lambda, containers,
-- mobile applications, or IoT devices.
--
-- To prevent errors when deploying application configurations, especially
-- for production systems where a simple typo could cause an unexpected
-- outage, AppConfig includes validators. A validator provides a syntactic
-- or semantic check to ensure that the configuration you want to deploy
-- works as intended. To validate your application configuration data, you
-- provide a schema or a Lambda function that runs against the
-- configuration. The configuration deployment or update can only proceed
-- when the configuration data is valid.
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
-- AppConfig supports multiple use cases. Here are some examples.
--
-- -   __Application tuning__: Use AppConfig to carefully introduce changes
--     to your application that can only be tested with production traffic.
--
-- -   __Feature toggle__: Use AppConfig to turn on new features that
--     require a timely deployment, such as a product launch or
--     announcement.
--
-- -   __Allow list__: Use AppConfig to allow premium subscribers to access
--     paid content.
--
-- -   __Operational issues__: Use AppConfig to reduce stress on your
--     application when a dependency or other external factor impacts the
--     system.
--
-- This reference is intended to be used with the
-- <http://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig.html AWS AppConfig User Guide>.
module Network.AWS.AppConfig
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** PayloadTooLargeException
    _PayloadTooLargeException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListEnvironments
    ListEnvironments (ListEnvironments'),
    newListEnvironments,
    ListEnvironmentsResponse (ListEnvironmentsResponse'),
    newListEnvironmentsResponse,

    -- ** UpdateEnvironment
    UpdateEnvironment (UpdateEnvironment'),
    newUpdateEnvironment,
    Environment (Environment'),
    newEnvironment,

    -- ** DeleteEnvironment
    DeleteEnvironment (DeleteEnvironment'),
    newDeleteEnvironment,
    DeleteEnvironmentResponse (DeleteEnvironmentResponse'),
    newDeleteEnvironmentResponse,

    -- ** GetDeploymentStrategy
    GetDeploymentStrategy (GetDeploymentStrategy'),
    newGetDeploymentStrategy,
    DeploymentStrategy (DeploymentStrategy'),
    newDeploymentStrategy,

    -- ** CreateConfigurationProfile
    CreateConfigurationProfile (CreateConfigurationProfile'),
    newCreateConfigurationProfile,
    ConfigurationProfile (ConfigurationProfile'),
    newConfigurationProfile,

    -- ** GetDeployment
    GetDeployment (GetDeployment'),
    newGetDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** UpdateConfigurationProfile
    UpdateConfigurationProfile (UpdateConfigurationProfile'),
    newUpdateConfigurationProfile,
    ConfigurationProfile (ConfigurationProfile'),
    newConfigurationProfile,

    -- ** DeleteConfigurationProfile
    DeleteConfigurationProfile (DeleteConfigurationProfile'),
    newDeleteConfigurationProfile,
    DeleteConfigurationProfileResponse (DeleteConfigurationProfileResponse'),
    newDeleteConfigurationProfileResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListHostedConfigurationVersions
    ListHostedConfigurationVersions (ListHostedConfigurationVersions'),
    newListHostedConfigurationVersions,
    ListHostedConfigurationVersionsResponse (ListHostedConfigurationVersionsResponse'),
    newListHostedConfigurationVersionsResponse,

    -- ** GetConfigurationProfile
    GetConfigurationProfile (GetConfigurationProfile'),
    newGetConfigurationProfile,
    ConfigurationProfile (ConfigurationProfile'),
    newConfigurationProfile,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    Application (Application'),
    newApplication,

    -- ** UpdateDeploymentStrategy
    UpdateDeploymentStrategy (UpdateDeploymentStrategy'),
    newUpdateDeploymentStrategy,
    DeploymentStrategy (DeploymentStrategy'),
    newDeploymentStrategy,

    -- ** DeleteDeploymentStrategy
    DeleteDeploymentStrategy (DeleteDeploymentStrategy'),
    newDeleteDeploymentStrategy,
    DeleteDeploymentStrategyResponse (DeleteDeploymentStrategyResponse'),
    newDeleteDeploymentStrategyResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    Application (Application'),
    newApplication,

    -- ** ValidateConfiguration
    ValidateConfiguration (ValidateConfiguration'),
    newValidateConfiguration,
    ValidateConfigurationResponse (ValidateConfigurationResponse'),
    newValidateConfigurationResponse,

    -- ** StopDeployment
    StopDeployment (StopDeployment'),
    newStopDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** GetApplication
    GetApplication (GetApplication'),
    newGetApplication,
    Application (Application'),
    newApplication,

    -- ** CreateHostedConfigurationVersion
    CreateHostedConfigurationVersion (CreateHostedConfigurationVersion'),
    newCreateHostedConfigurationVersion,
    HostedConfigurationVersion (HostedConfigurationVersion'),
    newHostedConfigurationVersion,

    -- ** ListConfigurationProfiles
    ListConfigurationProfiles (ListConfigurationProfiles'),
    newListConfigurationProfiles,
    ListConfigurationProfilesResponse (ListConfigurationProfilesResponse'),
    newListConfigurationProfilesResponse,

    -- ** DeleteHostedConfigurationVersion
    DeleteHostedConfigurationVersion (DeleteHostedConfigurationVersion'),
    newDeleteHostedConfigurationVersion,
    DeleteHostedConfigurationVersionResponse (DeleteHostedConfigurationVersionResponse'),
    newDeleteHostedConfigurationVersionResponse,

    -- ** GetHostedConfigurationVersion
    GetHostedConfigurationVersion (GetHostedConfigurationVersion'),
    newGetHostedConfigurationVersion,
    HostedConfigurationVersion (HostedConfigurationVersion'),
    newHostedConfigurationVersion,

    -- ** ListDeployments
    ListDeployments (ListDeployments'),
    newListDeployments,
    ListDeploymentsResponse (ListDeploymentsResponse'),
    newListDeploymentsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetEnvironment
    GetEnvironment (GetEnvironment'),
    newGetEnvironment,
    Environment (Environment'),
    newEnvironment,

    -- ** ListApplications
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListDeploymentStrategies
    ListDeploymentStrategies (ListDeploymentStrategies'),
    newListDeploymentStrategies,
    ListDeploymentStrategiesResponse (ListDeploymentStrategiesResponse'),
    newListDeploymentStrategiesResponse,

    -- ** GetConfiguration
    GetConfiguration (GetConfiguration'),
    newGetConfiguration,
    GetConfigurationResponse (GetConfigurationResponse'),
    newGetConfigurationResponse,

    -- ** CreateDeploymentStrategy
    CreateDeploymentStrategy (CreateDeploymentStrategy'),
    newCreateDeploymentStrategy,
    DeploymentStrategy (DeploymentStrategy'),
    newDeploymentStrategy,

    -- ** StartDeployment
    StartDeployment (StartDeployment'),
    newStartDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** CreateEnvironment
    CreateEnvironment (CreateEnvironment'),
    newCreateEnvironment,
    Environment (Environment'),
    newEnvironment,

    -- * Types

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

    -- ** Application
    Application (Application'),
    newApplication,

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

    -- ** HostedConfigurationVersion
    HostedConfigurationVersion (HostedConfigurationVersion'),
    newHostedConfigurationVersion,

    -- ** HostedConfigurationVersionSummary
    HostedConfigurationVersionSummary (HostedConfigurationVersionSummary'),
    newHostedConfigurationVersionSummary,

    -- ** Monitor
    Monitor (Monitor'),
    newMonitor,

    -- ** Validator
    Validator (Validator'),
    newValidator,
  )
where

import Network.AWS.AppConfig.CreateApplication
import Network.AWS.AppConfig.CreateConfigurationProfile
import Network.AWS.AppConfig.CreateDeploymentStrategy
import Network.AWS.AppConfig.CreateEnvironment
import Network.AWS.AppConfig.CreateHostedConfigurationVersion
import Network.AWS.AppConfig.DeleteApplication
import Network.AWS.AppConfig.DeleteConfigurationProfile
import Network.AWS.AppConfig.DeleteDeploymentStrategy
import Network.AWS.AppConfig.DeleteEnvironment
import Network.AWS.AppConfig.DeleteHostedConfigurationVersion
import Network.AWS.AppConfig.GetApplication
import Network.AWS.AppConfig.GetConfiguration
import Network.AWS.AppConfig.GetConfigurationProfile
import Network.AWS.AppConfig.GetDeployment
import Network.AWS.AppConfig.GetDeploymentStrategy
import Network.AWS.AppConfig.GetEnvironment
import Network.AWS.AppConfig.GetHostedConfigurationVersion
import Network.AWS.AppConfig.Lens
import Network.AWS.AppConfig.ListApplications
import Network.AWS.AppConfig.ListConfigurationProfiles
import Network.AWS.AppConfig.ListDeploymentStrategies
import Network.AWS.AppConfig.ListDeployments
import Network.AWS.AppConfig.ListEnvironments
import Network.AWS.AppConfig.ListHostedConfigurationVersions
import Network.AWS.AppConfig.ListTagsForResource
import Network.AWS.AppConfig.StartDeployment
import Network.AWS.AppConfig.StopDeployment
import Network.AWS.AppConfig.TagResource
import Network.AWS.AppConfig.Types
import Network.AWS.AppConfig.UntagResource
import Network.AWS.AppConfig.UpdateApplication
import Network.AWS.AppConfig.UpdateConfigurationProfile
import Network.AWS.AppConfig.UpdateDeploymentStrategy
import Network.AWS.AppConfig.UpdateEnvironment
import Network.AWS.AppConfig.ValidateConfiguration
import Network.AWS.AppConfig.Waiters

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
