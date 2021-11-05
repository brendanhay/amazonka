{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SMS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-10-24@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Server Migration Service
--
-- AWS Server Migration Service (AWS SMS) makes it easier and faster for
-- you to migrate your on-premises workloads to AWS. To learn more about
-- AWS SMS, see the following resources:
--
-- -   <http://aws.amazon.com/server-migration-service/ AWS Server Migration Service product page>
--
-- -   <https://docs.aws.amazon.com/server-migration-service/latest/userguide/ AWS Server Migration Service User Guide>
module Amazonka.SMS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ReplicationRunLimitExceededException
    _ReplicationRunLimitExceededException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** NoConnectorsAvailableException
    _NoConnectorsAvailableException,

    -- ** ReplicationJobNotFoundException
    _ReplicationJobNotFoundException,

    -- ** ServerCannotBeReplicatedException
    _ServerCannotBeReplicatedException,

    -- ** DryRunOperationException
    _DryRunOperationException,

    -- ** InternalError
    _InternalError,

    -- ** ReplicationJobAlreadyExistsException
    _ReplicationJobAlreadyExistsException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** TemporarilyUnavailableException
    _TemporarilyUnavailableException,

    -- ** MissingRequiredParameterException
    _MissingRequiredParameterException,

    -- ** UnauthorizedOperationException
    _UnauthorizedOperationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteAppReplicationConfiguration
    DeleteAppReplicationConfiguration (DeleteAppReplicationConfiguration'),
    newDeleteAppReplicationConfiguration,
    DeleteAppReplicationConfigurationResponse (DeleteAppReplicationConfigurationResponse'),
    newDeleteAppReplicationConfigurationResponse,

    -- ** PutAppReplicationConfiguration
    PutAppReplicationConfiguration (PutAppReplicationConfiguration'),
    newPutAppReplicationConfiguration,
    PutAppReplicationConfigurationResponse (PutAppReplicationConfigurationResponse'),
    newPutAppReplicationConfigurationResponse,

    -- ** DeleteServerCatalog
    DeleteServerCatalog (DeleteServerCatalog'),
    newDeleteServerCatalog,
    DeleteServerCatalogResponse (DeleteServerCatalogResponse'),
    newDeleteServerCatalogResponse,

    -- ** ImportAppCatalog
    ImportAppCatalog (ImportAppCatalog'),
    newImportAppCatalog,
    ImportAppCatalogResponse (ImportAppCatalogResponse'),
    newImportAppCatalogResponse,

    -- ** GetAppLaunchConfiguration
    GetAppLaunchConfiguration (GetAppLaunchConfiguration'),
    newGetAppLaunchConfiguration,
    GetAppLaunchConfigurationResponse (GetAppLaunchConfigurationResponse'),
    newGetAppLaunchConfigurationResponse,

    -- ** DeleteAppLaunchConfiguration
    DeleteAppLaunchConfiguration (DeleteAppLaunchConfiguration'),
    newDeleteAppLaunchConfiguration,
    DeleteAppLaunchConfigurationResponse (DeleteAppLaunchConfigurationResponse'),
    newDeleteAppLaunchConfigurationResponse,

    -- ** StartAppReplication
    StartAppReplication (StartAppReplication'),
    newStartAppReplication,
    StartAppReplicationResponse (StartAppReplicationResponse'),
    newStartAppReplicationResponse,

    -- ** PutAppLaunchConfiguration
    PutAppLaunchConfiguration (PutAppLaunchConfiguration'),
    newPutAppLaunchConfiguration,
    PutAppLaunchConfigurationResponse (PutAppLaunchConfigurationResponse'),
    newPutAppLaunchConfigurationResponse,

    -- ** GetReplicationRuns (Paginated)
    GetReplicationRuns (GetReplicationRuns'),
    newGetReplicationRuns,
    GetReplicationRunsResponse (GetReplicationRunsResponse'),
    newGetReplicationRunsResponse,

    -- ** TerminateApp
    TerminateApp (TerminateApp'),
    newTerminateApp,
    TerminateAppResponse (TerminateAppResponse'),
    newTerminateAppResponse,

    -- ** ListApps (Paginated)
    ListApps (ListApps'),
    newListApps,
    ListAppsResponse (ListAppsResponse'),
    newListAppsResponse,

    -- ** GetServers (Paginated)
    GetServers (GetServers'),
    newGetServers,
    GetServersResponse (GetServersResponse'),
    newGetServersResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** UpdateApp
    UpdateApp (UpdateApp'),
    newUpdateApp,
    UpdateAppResponse (UpdateAppResponse'),
    newUpdateAppResponse,

    -- ** StartOnDemandAppReplication
    StartOnDemandAppReplication (StartOnDemandAppReplication'),
    newStartOnDemandAppReplication,
    StartOnDemandAppReplicationResponse (StartOnDemandAppReplicationResponse'),
    newStartOnDemandAppReplicationResponse,

    -- ** ImportServerCatalog
    ImportServerCatalog (ImportServerCatalog'),
    newImportServerCatalog,
    ImportServerCatalogResponse (ImportServerCatalogResponse'),
    newImportServerCatalogResponse,

    -- ** GenerateTemplate
    GenerateTemplate (GenerateTemplate'),
    newGenerateTemplate,
    GenerateTemplateResponse (GenerateTemplateResponse'),
    newGenerateTemplateResponse,

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

    -- ** DisassociateConnector
    DisassociateConnector (DisassociateConnector'),
    newDisassociateConnector,
    DisassociateConnectorResponse (DisassociateConnectorResponse'),
    newDisassociateConnectorResponse,

    -- ** LaunchApp
    LaunchApp (LaunchApp'),
    newLaunchApp,
    LaunchAppResponse (LaunchAppResponse'),
    newLaunchAppResponse,

    -- ** GetAppValidationConfiguration
    GetAppValidationConfiguration (GetAppValidationConfiguration'),
    newGetAppValidationConfiguration,
    GetAppValidationConfigurationResponse (GetAppValidationConfigurationResponse'),
    newGetAppValidationConfigurationResponse,

    -- ** CreateReplicationJob
    CreateReplicationJob (CreateReplicationJob'),
    newCreateReplicationJob,
    CreateReplicationJobResponse (CreateReplicationJobResponse'),
    newCreateReplicationJobResponse,

    -- ** GenerateChangeSet
    GenerateChangeSet (GenerateChangeSet'),
    newGenerateChangeSet,
    GenerateChangeSetResponse (GenerateChangeSetResponse'),
    newGenerateChangeSetResponse,

    -- ** GetApp
    GetApp (GetApp'),
    newGetApp,
    GetAppResponse (GetAppResponse'),
    newGetAppResponse,

    -- ** UpdateReplicationJob
    UpdateReplicationJob (UpdateReplicationJob'),
    newUpdateReplicationJob,
    UpdateReplicationJobResponse (UpdateReplicationJobResponse'),
    newUpdateReplicationJobResponse,

    -- ** DeleteReplicationJob
    DeleteReplicationJob (DeleteReplicationJob'),
    newDeleteReplicationJob,
    DeleteReplicationJobResponse (DeleteReplicationJobResponse'),
    newDeleteReplicationJobResponse,

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** StopAppReplication
    StopAppReplication (StopAppReplication'),
    newStopAppReplication,
    StopAppReplicationResponse (StopAppReplicationResponse'),
    newStopAppReplicationResponse,

    -- ** DeleteAppValidationConfiguration
    DeleteAppValidationConfiguration (DeleteAppValidationConfiguration'),
    newDeleteAppValidationConfiguration,
    DeleteAppValidationConfigurationResponse (DeleteAppValidationConfigurationResponse'),
    newDeleteAppValidationConfigurationResponse,

    -- ** PutAppValidationConfiguration
    PutAppValidationConfiguration (PutAppValidationConfiguration'),
    newPutAppValidationConfiguration,
    PutAppValidationConfigurationResponse (PutAppValidationConfigurationResponse'),
    newPutAppValidationConfigurationResponse,

    -- ** GetAppValidationOutput
    GetAppValidationOutput (GetAppValidationOutput'),
    newGetAppValidationOutput,
    GetAppValidationOutputResponse (GetAppValidationOutputResponse'),
    newGetAppValidationOutputResponse,

    -- ** GetAppReplicationConfiguration
    GetAppReplicationConfiguration (GetAppReplicationConfiguration'),
    newGetAppReplicationConfiguration,
    GetAppReplicationConfigurationResponse (GetAppReplicationConfigurationResponse'),
    newGetAppReplicationConfigurationResponse,

    -- ** StartOnDemandReplicationRun
    StartOnDemandReplicationRun (StartOnDemandReplicationRun'),
    newStartOnDemandReplicationRun,
    StartOnDemandReplicationRunResponse (StartOnDemandReplicationRunResponse'),
    newStartOnDemandReplicationRunResponse,

    -- ** NotifyAppValidationOutput
    NotifyAppValidationOutput (NotifyAppValidationOutput'),
    newNotifyAppValidationOutput,
    NotifyAppValidationOutputResponse (NotifyAppValidationOutputResponse'),
    newNotifyAppValidationOutputResponse,

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
