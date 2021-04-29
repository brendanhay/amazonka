{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
module Network.AWS.SMS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalError
    _InternalError,

    -- ** NoConnectorsAvailableException
    _NoConnectorsAvailableException,

    -- ** ReplicationRunLimitExceededException
    _ReplicationRunLimitExceededException,

    -- ** UnauthorizedOperationException
    _UnauthorizedOperationException,

    -- ** MissingRequiredParameterException
    _MissingRequiredParameterException,

    -- ** TemporarilyUnavailableException
    _TemporarilyUnavailableException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** ReplicationJobNotFoundException
    _ReplicationJobNotFoundException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** DryRunOperationException
    _DryRunOperationException,

    -- ** ServerCannotBeReplicatedException
    _ServerCannotBeReplicatedException,

    -- ** ReplicationJobAlreadyExistsException
    _ReplicationJobAlreadyExistsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GenerateChangeSet
    GenerateChangeSet (GenerateChangeSet'),
    newGenerateChangeSet,
    GenerateChangeSetResponse (GenerateChangeSetResponse'),
    newGenerateChangeSetResponse,

    -- ** ImportAppCatalog
    ImportAppCatalog (ImportAppCatalog'),
    newImportAppCatalog,
    ImportAppCatalogResponse (ImportAppCatalogResponse'),
    newImportAppCatalogResponse,

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

    -- ** PutAppReplicationConfiguration
    PutAppReplicationConfiguration (PutAppReplicationConfiguration'),
    newPutAppReplicationConfiguration,
    PutAppReplicationConfigurationResponse (PutAppReplicationConfigurationResponse'),
    newPutAppReplicationConfigurationResponse,

    -- ** GetConnectors (Paginated)
    GetConnectors (GetConnectors'),
    newGetConnectors,
    GetConnectorsResponse (GetConnectorsResponse'),
    newGetConnectorsResponse,

    -- ** GenerateTemplate
    GenerateTemplate (GenerateTemplate'),
    newGenerateTemplate,
    GenerateTemplateResponse (GenerateTemplateResponse'),
    newGenerateTemplateResponse,

    -- ** PutAppValidationConfiguration
    PutAppValidationConfiguration (PutAppValidationConfiguration'),
    newPutAppValidationConfiguration,
    PutAppValidationConfigurationResponse (PutAppValidationConfigurationResponse'),
    newPutAppValidationConfigurationResponse,

    -- ** StartOnDemandReplicationRun
    StartOnDemandReplicationRun (StartOnDemandReplicationRun'),
    newStartOnDemandReplicationRun,
    StartOnDemandReplicationRunResponse (StartOnDemandReplicationRunResponse'),
    newStartOnDemandReplicationRunResponse,

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

    -- ** StopAppReplication
    StopAppReplication (StopAppReplication'),
    newStopAppReplication,
    StopAppReplicationResponse (StopAppReplicationResponse'),
    newStopAppReplicationResponse,

    -- ** CreateReplicationJob
    CreateReplicationJob (CreateReplicationJob'),
    newCreateReplicationJob,
    CreateReplicationJobResponse (CreateReplicationJobResponse'),
    newCreateReplicationJobResponse,

    -- ** DeleteServerCatalog
    DeleteServerCatalog (DeleteServerCatalog'),
    newDeleteServerCatalog,
    DeleteServerCatalogResponse (DeleteServerCatalogResponse'),
    newDeleteServerCatalogResponse,

    -- ** GetApp
    GetApp (GetApp'),
    newGetApp,
    GetAppResponse (GetAppResponse'),
    newGetAppResponse,

    -- ** DeleteAppReplicationConfiguration
    DeleteAppReplicationConfiguration (DeleteAppReplicationConfiguration'),
    newDeleteAppReplicationConfiguration,
    DeleteAppReplicationConfigurationResponse (DeleteAppReplicationConfigurationResponse'),
    newDeleteAppReplicationConfigurationResponse,

    -- ** DisassociateConnector
    DisassociateConnector (DisassociateConnector'),
    newDisassociateConnector,
    DisassociateConnectorResponse (DisassociateConnectorResponse'),
    newDisassociateConnectorResponse,

    -- ** NotifyAppValidationOutput
    NotifyAppValidationOutput (NotifyAppValidationOutput'),
    newNotifyAppValidationOutput,
    NotifyAppValidationOutputResponse (NotifyAppValidationOutputResponse'),
    newNotifyAppValidationOutputResponse,

    -- ** GetReplicationJobs (Paginated)
    GetReplicationJobs (GetReplicationJobs'),
    newGetReplicationJobs,
    GetReplicationJobsResponse (GetReplicationJobsResponse'),
    newGetReplicationJobsResponse,

    -- ** StartOnDemandAppReplication
    StartOnDemandAppReplication (StartOnDemandAppReplication'),
    newStartOnDemandAppReplication,
    StartOnDemandAppReplicationResponse (StartOnDemandAppReplicationResponse'),
    newStartOnDemandAppReplicationResponse,

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

    -- ** DeleteAppValidationConfiguration
    DeleteAppValidationConfiguration (DeleteAppValidationConfiguration'),
    newDeleteAppValidationConfiguration,
    DeleteAppValidationConfigurationResponse (DeleteAppValidationConfigurationResponse'),
    newDeleteAppValidationConfigurationResponse,

    -- ** ImportServerCatalog
    ImportServerCatalog (ImportServerCatalog'),
    newImportServerCatalog,
    ImportServerCatalogResponse (ImportServerCatalogResponse'),
    newImportServerCatalogResponse,

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

    -- ** DeleteAppLaunchConfiguration
    DeleteAppLaunchConfiguration (DeleteAppLaunchConfiguration'),
    newDeleteAppLaunchConfiguration,
    DeleteAppLaunchConfigurationResponse (DeleteAppLaunchConfigurationResponse'),
    newDeleteAppLaunchConfigurationResponse,

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** GetAppLaunchConfiguration
    GetAppLaunchConfiguration (GetAppLaunchConfiguration'),
    newGetAppLaunchConfiguration,
    GetAppLaunchConfigurationResponse (GetAppLaunchConfigurationResponse'),
    newGetAppLaunchConfigurationResponse,

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

import Network.AWS.SMS.CreateApp
import Network.AWS.SMS.CreateReplicationJob
import Network.AWS.SMS.DeleteApp
import Network.AWS.SMS.DeleteAppLaunchConfiguration
import Network.AWS.SMS.DeleteAppReplicationConfiguration
import Network.AWS.SMS.DeleteAppValidationConfiguration
import Network.AWS.SMS.DeleteReplicationJob
import Network.AWS.SMS.DeleteServerCatalog
import Network.AWS.SMS.DisassociateConnector
import Network.AWS.SMS.GenerateChangeSet
import Network.AWS.SMS.GenerateTemplate
import Network.AWS.SMS.GetApp
import Network.AWS.SMS.GetAppLaunchConfiguration
import Network.AWS.SMS.GetAppReplicationConfiguration
import Network.AWS.SMS.GetAppValidationConfiguration
import Network.AWS.SMS.GetAppValidationOutput
import Network.AWS.SMS.GetConnectors
import Network.AWS.SMS.GetReplicationJobs
import Network.AWS.SMS.GetReplicationRuns
import Network.AWS.SMS.GetServers
import Network.AWS.SMS.ImportAppCatalog
import Network.AWS.SMS.ImportServerCatalog
import Network.AWS.SMS.LaunchApp
import Network.AWS.SMS.Lens
import Network.AWS.SMS.ListApps
import Network.AWS.SMS.NotifyAppValidationOutput
import Network.AWS.SMS.PutAppLaunchConfiguration
import Network.AWS.SMS.PutAppReplicationConfiguration
import Network.AWS.SMS.PutAppValidationConfiguration
import Network.AWS.SMS.StartAppReplication
import Network.AWS.SMS.StartOnDemandAppReplication
import Network.AWS.SMS.StartOnDemandReplicationRun
import Network.AWS.SMS.StopAppReplication
import Network.AWS.SMS.TerminateApp
import Network.AWS.SMS.Types
import Network.AWS.SMS.UpdateApp
import Network.AWS.SMS.UpdateReplicationJob
import Network.AWS.SMS.Waiters

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
