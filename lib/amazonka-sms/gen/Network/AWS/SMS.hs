{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Server Migration Service__
--
-- AWS Server Migration Service (AWS SMS) makes it easier and faster for you to migrate your on-premises workloads to AWS. To learn more about AWS SMS, see the following resources:
--
--     * <http://aws.amazon.com/server-migration-service/ AWS Server Migration Service product page>
--
--
--     * <https://docs.aws.amazon.com/server-migration-service/latest/userguide/ AWS Server Migration Service User Guide>
module Network.AWS.SMS
  ( -- * Service configuration
    smsService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteAppReplicationConfiguration
    module Network.AWS.SMS.DeleteAppReplicationConfiguration,

    -- ** PutAppReplicationConfiguration
    module Network.AWS.SMS.PutAppReplicationConfiguration,

    -- ** DeleteServerCatalog
    module Network.AWS.SMS.DeleteServerCatalog,

    -- ** ImportAppCatalog
    module Network.AWS.SMS.ImportAppCatalog,

    -- ** GetAppLaunchConfiguration
    module Network.AWS.SMS.GetAppLaunchConfiguration,

    -- ** DeleteAppLaunchConfiguration
    module Network.AWS.SMS.DeleteAppLaunchConfiguration,

    -- ** StartAppReplication
    module Network.AWS.SMS.StartAppReplication,

    -- ** PutAppLaunchConfiguration
    module Network.AWS.SMS.PutAppLaunchConfiguration,

    -- ** GetReplicationRuns (Paginated)
    module Network.AWS.SMS.GetReplicationRuns,

    -- ** TerminateApp
    module Network.AWS.SMS.TerminateApp,

    -- ** ListApps (Paginated)
    module Network.AWS.SMS.ListApps,

    -- ** GetServers (Paginated)
    module Network.AWS.SMS.GetServers,

    -- ** DeleteApp
    module Network.AWS.SMS.DeleteApp,

    -- ** UpdateApp
    module Network.AWS.SMS.UpdateApp,

    -- ** StartOnDemandAppReplication
    module Network.AWS.SMS.StartOnDemandAppReplication,

    -- ** ImportServerCatalog
    module Network.AWS.SMS.ImportServerCatalog,

    -- ** GenerateTemplate
    module Network.AWS.SMS.GenerateTemplate,

    -- ** GetConnectors (Paginated)
    module Network.AWS.SMS.GetConnectors,

    -- ** GetReplicationJobs (Paginated)
    module Network.AWS.SMS.GetReplicationJobs,

    -- ** DisassociateConnector
    module Network.AWS.SMS.DisassociateConnector,

    -- ** LaunchApp
    module Network.AWS.SMS.LaunchApp,

    -- ** GetAppValidationConfiguration
    module Network.AWS.SMS.GetAppValidationConfiguration,

    -- ** CreateReplicationJob
    module Network.AWS.SMS.CreateReplicationJob,

    -- ** GenerateChangeSet
    module Network.AWS.SMS.GenerateChangeSet,

    -- ** GetApp
    module Network.AWS.SMS.GetApp,

    -- ** UpdateReplicationJob
    module Network.AWS.SMS.UpdateReplicationJob,

    -- ** DeleteReplicationJob
    module Network.AWS.SMS.DeleteReplicationJob,

    -- ** CreateApp
    module Network.AWS.SMS.CreateApp,

    -- ** StopAppReplication
    module Network.AWS.SMS.StopAppReplication,

    -- ** DeleteAppValidationConfiguration
    module Network.AWS.SMS.DeleteAppValidationConfiguration,

    -- ** PutAppValidationConfiguration
    module Network.AWS.SMS.PutAppValidationConfiguration,

    -- ** GetAppValidationOutput
    module Network.AWS.SMS.GetAppValidationOutput,

    -- ** GetAppReplicationConfiguration
    module Network.AWS.SMS.GetAppReplicationConfiguration,

    -- ** StartOnDemandReplicationRun
    module Network.AWS.SMS.StartOnDemandReplicationRun,

    -- ** NotifyAppValidationOutput
    module Network.AWS.SMS.NotifyAppValidationOutput,

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

    -- ** VMManagerType
    VMManagerType (..),

    -- ** ValidationStatus
    ValidationStatus (..),

    -- ** AppSummary
    AppSummary (..),
    mkAppSummary,
    asCreationTime,
    asTotalServers,
    asStatus,
    asLaunchDetails,
    asLaunchStatusMessage,
    asReplicationConfigurationStatus,
    asReplicationStatusMessage,
    asTotalServerGroups,
    asRoleName,
    asLaunchConfigurationStatus,
    asLaunchStatus,
    asAppId,
    asName,
    asStatusMessage,
    asLatestReplicationTime,
    asImportedAppId,
    asReplicationStatus,
    asLastModified,
    asDescription,

    -- ** AppValidationConfiguration
    AppValidationConfiguration (..),
    mkAppValidationConfiguration,
    avcSsmValidationParameters,
    avcName,
    avcValidationId,
    avcAppValidationStrategy,

    -- ** AppValidationOutput
    AppValidationOutput (..),
    mkAppValidationOutput,
    avoSsmOutput,

    -- ** Connector
    Connector (..),
    mkConnector,
    cStatus,
    cVmManagerName,
    cIpAddress,
    cVmManagerId,
    cVmManagerType,
    cConnectorId,
    cAssociatedOn,
    cMacAddress,
    cVersion,
    cCapabilityList,

    -- ** LaunchDetails
    LaunchDetails (..),
    mkLaunchDetails,
    ldStackId,
    ldLatestLaunchTime,
    ldStackName,

    -- ** NotificationContext
    NotificationContext (..),
    mkNotificationContext,
    ncStatus,
    ncStatusMessage,
    ncValidationId,

    -- ** ReplicationJob
    ReplicationJob (..),
    mkReplicationJob,
    rjFrequency,
    rjNumberOfRecentAMIsToKeep,
    rjState,
    rjServerType,
    rjServerId,
    rjLicenseType,
    rjRoleName,
    rjVmServer,
    rjEncrypted,
    rjReplicationJobId,
    rjReplicationRunList,
    rjNextReplicationRunStartTime,
    rjStatusMessage,
    rjKmsKeyId,
    rjLatestAMIId,
    rjSeedReplicationTime,
    rjRunOnce,
    rjDescription,

    -- ** ReplicationRun
    ReplicationRun (..),
    mkReplicationRun,
    rrState,
    rrReplicationRunId,
    rrEncrypted,
    rrStageDetails,
    rrScheduledStartTime,
    rrStatusMessage,
    rrKmsKeyId,
    rrCompletedTime,
    rrAmiId,
    rrType,
    rrDescription,

    -- ** ReplicationRunStageDetails
    ReplicationRunStageDetails (..),
    mkReplicationRunStageDetails,
    rrsdStage,
    rrsdStageProgress,

    -- ** S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slKey,

    -- ** SSMOutput
    SSMOutput (..),
    mkSSMOutput,
    ssmoS3Location,

    -- ** SSMValidationParameters
    SSMValidationParameters (..),
    mkSSMValidationParameters,
    ssmvpInstanceId,
    ssmvpCommand,
    ssmvpExecutionTimeoutSeconds,
    ssmvpScriptType,
    ssmvpSource,
    ssmvpOutputS3BucketName,

    -- ** Server
    Server (..),
    mkServer,
    sServerType,
    sServerId,
    sReplicationJobTerminated,
    sVmServer,
    sReplicationJobId,

    -- ** ServerGroup
    ServerGroup (..),
    mkServerGroup,
    sgServerList,
    sgName,
    sgServerGroupId,

    -- ** ServerGroupLaunchConfiguration
    ServerGroupLaunchConfiguration (..),
    mkServerGroupLaunchConfiguration,
    sglcServerGroupId,
    sglcLaunchOrder,
    sglcServerLaunchConfigurations,

    -- ** ServerGroupReplicationConfiguration
    ServerGroupReplicationConfiguration (..),
    mkServerGroupReplicationConfiguration,
    sgrcServerGroupId,
    sgrcServerReplicationConfigurations,

    -- ** ServerGroupValidationConfiguration
    ServerGroupValidationConfiguration (..),
    mkServerGroupValidationConfiguration,
    sgvcServerValidationConfigurations,
    sgvcServerGroupId,

    -- ** ServerLaunchConfiguration
    ServerLaunchConfiguration (..),
    mkServerLaunchConfiguration,
    slcEc2KeyName,
    slcConfigureScriptType,
    slcAssociatePublicIPAddress,
    slcIamInstanceProfileName,
    slcSubnet,
    slcLogicalId,
    slcSecurityGroup,
    slcUserData,
    slcInstanceType,
    slcConfigureScript,
    slcServer,
    slcVpc,

    -- ** ServerReplicationConfiguration
    ServerReplicationConfiguration (..),
    mkServerReplicationConfiguration,
    srcServerReplicationParameters,
    srcServer,

    -- ** ServerReplicationParameters
    ServerReplicationParameters (..),
    mkServerReplicationParameters,
    srpFrequency,
    srpNumberOfRecentAMIsToKeep,
    srpSeedTime,
    srpLicenseType,
    srpEncrypted,
    srpKmsKeyId,
    srpRunOnce,

    -- ** ServerValidationConfiguration
    ServerValidationConfiguration (..),
    mkServerValidationConfiguration,
    svcServerValidationStrategy,
    svcUserDataValidationParameters,
    svcName,
    svcServer,
    svcValidationId,

    -- ** ServerValidationOutput
    ServerValidationOutput (..),
    mkServerValidationOutput,
    svoServer,

    -- ** Source
    Source (..),
    mkSource,
    sS3Location,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** UserData
    UserData (..),
    mkUserData,
    udS3Location,

    -- ** UserDataValidationParameters
    UserDataValidationParameters (..),
    mkUserDataValidationParameters,
    udvpScriptType,
    udvpSource,

    -- ** VMServer
    VMServer (..),
    mkVMServer,
    vmsVmManagerName,
    vmsVmManagerType,
    vmsVmServerAddress,
    vmsVmName,
    vmsVmPath,

    -- ** VMServerAddress
    VMServerAddress (..),
    mkVMServerAddress,
    vmsaVmManagerId,
    vmsaVmId,

    -- ** ValidationOutput
    ValidationOutput (..),
    mkValidationOutput,
    voStatus,
    voAppValidationOutput,
    voLatestValidationTime,
    voName,
    voStatusMessage,
    voValidationId,
    voServerValidationOutput,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
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
