{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SMS
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-10-24@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- __Product update__
--
-- We recommend
-- <http://aws.amazon.com/application-migration-service Amazon Web Services Application Migration Service>
-- (Amazon Web Services MGN) as the primary migration service for
-- lift-and-shift migrations. If Amazon Web Services MGN is unavailable in
-- a specific Amazon Web Services Region, you can use the Server Migration
-- Service APIs through March 2023.
--
-- Server Migration Service (Server Migration Service) makes it easier and
-- faster for you to migrate your on-premises workloads to Amazon Web
-- Services. To learn more about Server Migration Service, see the
-- following resources:
--
-- -   <http://aws.amazon.com/server-migration-service/ Server Migration Service product page>
--
-- -   <https://docs.aws.amazon.com/server-migration-service/latest/userguide/ Server Migration Service User Guide>
module Amazonka.SMS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DryRunOperationException
    _DryRunOperationException,

    -- ** InternalError
    _InternalError,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** MissingRequiredParameterException
    _MissingRequiredParameterException,

    -- ** NoConnectorsAvailableException
    _NoConnectorsAvailableException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** ReplicationJobAlreadyExistsException
    _ReplicationJobAlreadyExistsException,

    -- ** ReplicationJobNotFoundException
    _ReplicationJobNotFoundException,

    -- ** ReplicationRunLimitExceededException
    _ReplicationRunLimitExceededException,

    -- ** ServerCannotBeReplicatedException
    _ServerCannotBeReplicatedException,

    -- ** TemporarilyUnavailableException
    _TemporarilyUnavailableException,

    -- ** UnauthorizedOperationException
    _UnauthorizedOperationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** CreateReplicationJob
    CreateReplicationJob (CreateReplicationJob'),
    newCreateReplicationJob,
    CreateReplicationJobResponse (CreateReplicationJobResponse'),
    newCreateReplicationJobResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** DeleteAppLaunchConfiguration
    DeleteAppLaunchConfiguration (DeleteAppLaunchConfiguration'),
    newDeleteAppLaunchConfiguration,
    DeleteAppLaunchConfigurationResponse (DeleteAppLaunchConfigurationResponse'),
    newDeleteAppLaunchConfigurationResponse,

    -- ** DeleteAppReplicationConfiguration
    DeleteAppReplicationConfiguration (DeleteAppReplicationConfiguration'),
    newDeleteAppReplicationConfiguration,
    DeleteAppReplicationConfigurationResponse (DeleteAppReplicationConfigurationResponse'),
    newDeleteAppReplicationConfigurationResponse,

    -- ** DeleteAppValidationConfiguration
    DeleteAppValidationConfiguration (DeleteAppValidationConfiguration'),
    newDeleteAppValidationConfiguration,
    DeleteAppValidationConfigurationResponse (DeleteAppValidationConfigurationResponse'),
    newDeleteAppValidationConfigurationResponse,

    -- ** DeleteReplicationJob
    DeleteReplicationJob (DeleteReplicationJob'),
    newDeleteReplicationJob,
    DeleteReplicationJobResponse (DeleteReplicationJobResponse'),
    newDeleteReplicationJobResponse,

    -- ** DeleteServerCatalog
    DeleteServerCatalog (DeleteServerCatalog'),
    newDeleteServerCatalog,
    DeleteServerCatalogResponse (DeleteServerCatalogResponse'),
    newDeleteServerCatalogResponse,

    -- ** DisassociateConnector
    DisassociateConnector (DisassociateConnector'),
    newDisassociateConnector,
    DisassociateConnectorResponse (DisassociateConnectorResponse'),
    newDisassociateConnectorResponse,

    -- ** GenerateChangeSet
    GenerateChangeSet (GenerateChangeSet'),
    newGenerateChangeSet,
    GenerateChangeSetResponse (GenerateChangeSetResponse'),
    newGenerateChangeSetResponse,

    -- ** GenerateTemplate
    GenerateTemplate (GenerateTemplate'),
    newGenerateTemplate,
    GenerateTemplateResponse (GenerateTemplateResponse'),
    newGenerateTemplateResponse,

    -- ** GetApp
    GetApp (GetApp'),
    newGetApp,
    GetAppResponse (GetAppResponse'),
    newGetAppResponse,

    -- ** GetAppLaunchConfiguration
    GetAppLaunchConfiguration (GetAppLaunchConfiguration'),
    newGetAppLaunchConfiguration,
    GetAppLaunchConfigurationResponse (GetAppLaunchConfigurationResponse'),
    newGetAppLaunchConfigurationResponse,

    -- ** GetAppReplicationConfiguration
    GetAppReplicationConfiguration (GetAppReplicationConfiguration'),
    newGetAppReplicationConfiguration,
    GetAppReplicationConfigurationResponse (GetAppReplicationConfigurationResponse'),
    newGetAppReplicationConfigurationResponse,

    -- ** GetAppValidationConfiguration
    GetAppValidationConfiguration (GetAppValidationConfiguration'),
    newGetAppValidationConfiguration,
    GetAppValidationConfigurationResponse (GetAppValidationConfigurationResponse'),
    newGetAppValidationConfigurationResponse,

    -- ** GetAppValidationOutput
    GetAppValidationOutput (GetAppValidationOutput'),
    newGetAppValidationOutput,
    GetAppValidationOutputResponse (GetAppValidationOutputResponse'),
    newGetAppValidationOutputResponse,

    -- ** GetConnectors (Paginated)
    GetConnectors (GetConnectors'),
    newGetConnectors,
    GetConnectorsResponse (GetConnectorsResponse'),
    newGetConnectorsResponse,

    -- ** GetReplicationJobs (Paginated)
    GetReplicationJobs (GetReplicationJobs'),
    newGetReplicationJobs,
    GetReplicationJobsResponse (GetReplicationJobsResponse'),
    newGetReplicationJobsResponse,

    -- ** GetReplicationRuns (Paginated)
    GetReplicationRuns (GetReplicationRuns'),
    newGetReplicationRuns,
    GetReplicationRunsResponse (GetReplicationRunsResponse'),
    newGetReplicationRunsResponse,

    -- ** GetServers (Paginated)
    GetServers (GetServers'),
    newGetServers,
    GetServersResponse (GetServersResponse'),
    newGetServersResponse,

    -- ** ImportAppCatalog
    ImportAppCatalog (ImportAppCatalog'),
    newImportAppCatalog,
    ImportAppCatalogResponse (ImportAppCatalogResponse'),
    newImportAppCatalogResponse,

    -- ** ImportServerCatalog
    ImportServerCatalog (ImportServerCatalog'),
    newImportServerCatalog,
    ImportServerCatalogResponse (ImportServerCatalogResponse'),
    newImportServerCatalogResponse,

    -- ** LaunchApp
    LaunchApp (LaunchApp'),
    newLaunchApp,
    LaunchAppResponse (LaunchAppResponse'),
    newLaunchAppResponse,

    -- ** ListApps (Paginated)
    ListApps (ListApps'),
    newListApps,
    ListAppsResponse (ListAppsResponse'),
    newListAppsResponse,

    -- ** NotifyAppValidationOutput
    NotifyAppValidationOutput (NotifyAppValidationOutput'),
    newNotifyAppValidationOutput,
    NotifyAppValidationOutputResponse (NotifyAppValidationOutputResponse'),
    newNotifyAppValidationOutputResponse,

    -- ** PutAppLaunchConfiguration
    PutAppLaunchConfiguration (PutAppLaunchConfiguration'),
    newPutAppLaunchConfiguration,
    PutAppLaunchConfigurationResponse (PutAppLaunchConfigurationResponse'),
    newPutAppLaunchConfigurationResponse,

    -- ** PutAppReplicationConfiguration
    PutAppReplicationConfiguration (PutAppReplicationConfiguration'),
    newPutAppReplicationConfiguration,
    PutAppReplicationConfigurationResponse (PutAppReplicationConfigurationResponse'),
    newPutAppReplicationConfigurationResponse,

    -- ** PutAppValidationConfiguration
    PutAppValidationConfiguration (PutAppValidationConfiguration'),
    newPutAppValidationConfiguration,
    PutAppValidationConfigurationResponse (PutAppValidationConfigurationResponse'),
    newPutAppValidationConfigurationResponse,

    -- ** StartAppReplication
    StartAppReplication (StartAppReplication'),
    newStartAppReplication,
    StartAppReplicationResponse (StartAppReplicationResponse'),
    newStartAppReplicationResponse,

    -- ** StartOnDemandAppReplication
    StartOnDemandAppReplication (StartOnDemandAppReplication'),
    newStartOnDemandAppReplication,
    StartOnDemandAppReplicationResponse (StartOnDemandAppReplicationResponse'),
    newStartOnDemandAppReplicationResponse,

    -- ** StartOnDemandReplicationRun
    StartOnDemandReplicationRun (StartOnDemandReplicationRun'),
    newStartOnDemandReplicationRun,
    StartOnDemandReplicationRunResponse (StartOnDemandReplicationRunResponse'),
    newStartOnDemandReplicationRunResponse,

    -- ** StopAppReplication
    StopAppReplication (StopAppReplication'),
    newStopAppReplication,
    StopAppReplicationResponse (StopAppReplicationResponse'),
    newStopAppReplicationResponse,

    -- ** TerminateApp
    TerminateApp (TerminateApp'),
    newTerminateApp,
    TerminateAppResponse (TerminateAppResponse'),
    newTerminateAppResponse,

    -- ** UpdateApp
    UpdateApp (UpdateApp'),
    newUpdateApp,
    UpdateAppResponse (UpdateAppResponse'),
    newUpdateAppResponse,

    -- ** UpdateReplicationJob
    UpdateReplicationJob (UpdateReplicationJob'),
    newUpdateReplicationJob,
    UpdateReplicationJobResponse (UpdateReplicationJobResponse'),
    newUpdateReplicationJobResponse,

    -- * Types

    -- ** AppLaunchConfigurationStatus
    AppLaunchConfigurationStatus (..),

    -- ** AppLaunchStatus
    AppLaunchStatus (..),

    -- ** AppReplicationConfigurationStatus
    AppReplicationConfigurationStatus (..),

    -- ** AppReplicationStatus
    AppReplicationStatus (..),

    -- ** AppStatus
    AppStatus (..),

    -- ** AppValidationStrategy
    AppValidationStrategy (..),

    -- ** ConnectorCapability
    ConnectorCapability (..),

    -- ** ConnectorStatus
    ConnectorStatus (..),

    -- ** LicenseType
    LicenseType (..),

    -- ** OutputFormat
    OutputFormat (..),

    -- ** ReplicationJobState
    ReplicationJobState (..),

    -- ** ReplicationRunState
    ReplicationRunState (..),

    -- ** ReplicationRunType
    ReplicationRunType (..),

    -- ** ScriptType
    ScriptType (..),

    -- ** ServerCatalogStatus
    ServerCatalogStatus (..),

    -- ** ServerType
    ServerType (..),

    -- ** ServerValidationStrategy
    ServerValidationStrategy (..),

    -- ** ValidationStatus
    ValidationStatus (..),

    -- ** VmManagerType
    VmManagerType (..),

    -- ** AppSummary
    AppSummary (AppSummary'),
    newAppSummary,

    -- ** AppValidationConfiguration
    AppValidationConfiguration (AppValidationConfiguration'),
    newAppValidationConfiguration,

    -- ** AppValidationOutput
    AppValidationOutput (AppValidationOutput'),
    newAppValidationOutput,

    -- ** Connector
    Connector (Connector'),
    newConnector,

    -- ** LaunchDetails
    LaunchDetails (LaunchDetails'),
    newLaunchDetails,

    -- ** NotificationContext
    NotificationContext (NotificationContext'),
    newNotificationContext,

    -- ** ReplicationJob
    ReplicationJob (ReplicationJob'),
    newReplicationJob,

    -- ** ReplicationRun
    ReplicationRun (ReplicationRun'),
    newReplicationRun,

    -- ** ReplicationRunStageDetails
    ReplicationRunStageDetails (ReplicationRunStageDetails'),
    newReplicationRunStageDetails,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** SSMOutput
    SSMOutput (SSMOutput'),
    newSSMOutput,

    -- ** SSMValidationParameters
    SSMValidationParameters (SSMValidationParameters'),
    newSSMValidationParameters,

    -- ** Server
    Server (Server'),
    newServer,

    -- ** ServerGroup
    ServerGroup (ServerGroup'),
    newServerGroup,

    -- ** ServerGroupLaunchConfiguration
    ServerGroupLaunchConfiguration (ServerGroupLaunchConfiguration'),
    newServerGroupLaunchConfiguration,

    -- ** ServerGroupReplicationConfiguration
    ServerGroupReplicationConfiguration (ServerGroupReplicationConfiguration'),
    newServerGroupReplicationConfiguration,

    -- ** ServerGroupValidationConfiguration
    ServerGroupValidationConfiguration (ServerGroupValidationConfiguration'),
    newServerGroupValidationConfiguration,

    -- ** ServerLaunchConfiguration
    ServerLaunchConfiguration (ServerLaunchConfiguration'),
    newServerLaunchConfiguration,

    -- ** ServerReplicationConfiguration
    ServerReplicationConfiguration (ServerReplicationConfiguration'),
    newServerReplicationConfiguration,

    -- ** ServerReplicationParameters
    ServerReplicationParameters (ServerReplicationParameters'),
    newServerReplicationParameters,

    -- ** ServerValidationConfiguration
    ServerValidationConfiguration (ServerValidationConfiguration'),
    newServerValidationConfiguration,

    -- ** ServerValidationOutput
    ServerValidationOutput (ServerValidationOutput'),
    newServerValidationOutput,

    -- ** Source
    Source (Source'),
    newSource,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UserData
    UserData (UserData'),
    newUserData,

    -- ** UserDataValidationParameters
    UserDataValidationParameters (UserDataValidationParameters'),
    newUserDataValidationParameters,

    -- ** ValidationOutput
    ValidationOutput (ValidationOutput'),
    newValidationOutput,

    -- ** VmServer
    VmServer (VmServer'),
    newVmServer,

    -- ** VmServerAddress
    VmServerAddress (VmServerAddress'),
    newVmServerAddress,
  )
where

import Amazonka.SMS.CreateApp
import Amazonka.SMS.CreateReplicationJob
import Amazonka.SMS.DeleteApp
import Amazonka.SMS.DeleteAppLaunchConfiguration
import Amazonka.SMS.DeleteAppReplicationConfiguration
import Amazonka.SMS.DeleteAppValidationConfiguration
import Amazonka.SMS.DeleteReplicationJob
import Amazonka.SMS.DeleteServerCatalog
import Amazonka.SMS.DisassociateConnector
import Amazonka.SMS.GenerateChangeSet
import Amazonka.SMS.GenerateTemplate
import Amazonka.SMS.GetApp
import Amazonka.SMS.GetAppLaunchConfiguration
import Amazonka.SMS.GetAppReplicationConfiguration
import Amazonka.SMS.GetAppValidationConfiguration
import Amazonka.SMS.GetAppValidationOutput
import Amazonka.SMS.GetConnectors
import Amazonka.SMS.GetReplicationJobs
import Amazonka.SMS.GetReplicationRuns
import Amazonka.SMS.GetServers
import Amazonka.SMS.ImportAppCatalog
import Amazonka.SMS.ImportServerCatalog
import Amazonka.SMS.LaunchApp
import Amazonka.SMS.Lens
import Amazonka.SMS.ListApps
import Amazonka.SMS.NotifyAppValidationOutput
import Amazonka.SMS.PutAppLaunchConfiguration
import Amazonka.SMS.PutAppReplicationConfiguration
import Amazonka.SMS.PutAppValidationConfiguration
import Amazonka.SMS.StartAppReplication
import Amazonka.SMS.StartOnDemandAppReplication
import Amazonka.SMS.StartOnDemandReplicationRun
import Amazonka.SMS.StopAppReplication
import Amazonka.SMS.TerminateApp
import Amazonka.SMS.Types
import Amazonka.SMS.UpdateApp
import Amazonka.SMS.UpdateReplicationJob
import Amazonka.SMS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SMS'.

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
