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
    mkServiceConfig,

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

    -- ** ReplicationJobStatusMessage
    ReplicationJobStatusMessage (..),

    -- ** InstanceId
    InstanceId (..),

    -- ** NonEmptyStringWithMaxLen255
    NonEmptyStringWithMaxLen255 (..),

    -- ** ValidationOutput
    ValidationOutput (..),
    mkValidationOutput,
    voAppValidationOutput,
    voLatestValidationTime,
    voName,
    voServerValidationOutput,
    voStatus,
    voStatusMessage,
    voValidationId,

    -- ** ReplicationRunStageProgress
    ReplicationRunStageProgress (..),

    -- ** EC2KeyName
    EC2KeyName (..),

    -- ** Command
    Command (..),

    -- ** LaunchDetails
    LaunchDetails (..),
    mkLaunchDetails,
    ldLatestLaunchTime,
    ldStackId,
    ldStackName,

    -- ** ConnectorStatus
    ConnectorStatus (..),

    -- ** VmManagerName
    VmManagerName (..),

    -- ** ServerGroupLaunchConfiguration
    ServerGroupLaunchConfiguration (..),
    mkServerGroupLaunchConfiguration,
    sglcLaunchOrder,
    sglcServerGroupId,
    sglcServerLaunchConfigurations,

    -- ** IpAddress
    IpAddress (..),

    -- ** SSMOutput
    SSMOutput (..),
    mkSSMOutput,
    ssmoS3Location,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** ServerCatalogStatus
    ServerCatalogStatus (..),

    -- ** AppReplicationStatusMessage
    AppReplicationStatusMessage (..),

    -- ** ServerGroup
    ServerGroup (..),
    mkServerGroup,
    sgName,
    sgServerGroupId,
    sgServerList,

    -- ** ReplicationRunState
    ReplicationRunState (..),

    -- ** ClientToken
    ClientToken (..),

    -- ** AppValidationConfiguration
    AppValidationConfiguration (..),
    mkAppValidationConfiguration,
    avcAppValidationStrategy,
    avcName,
    avcSsmValidationParameters,
    avcValidationId,

    -- ** VmManagerId
    VmManagerId (..),

    -- ** ReplicationRunType
    ReplicationRunType (..),

    -- ** ServerValidationStrategy
    ServerValidationStrategy (..),

    -- ** AppStatus
    AppStatus (..),

    -- ** ServerType
    ServerType (..),

    -- ** AppIdWithValidation
    AppIdWithValidation (..),

    -- ** AppValidationOutput
    AppValidationOutput (..),
    mkAppValidationOutput,
    avoSsmOutput,

    -- ** ServerLaunchConfiguration
    ServerLaunchConfiguration (..),
    mkServerLaunchConfiguration,
    slcAssociatePublicIpAddress,
    slcConfigureScript,
    slcConfigureScriptType,
    slcEc2KeyName,
    slcIamInstanceProfileName,
    slcInstanceType,
    slcLogicalId,
    slcSecurityGroup,
    slcServer,
    slcSubnet,
    slcUserData,
    slcVpc,

    -- ** ServerId
    ServerId (..),

    -- ** VmManagerType
    VmManagerType (..),

    -- ** VmServerAddress
    VmServerAddress (..),
    mkVmServerAddress,
    vsaVmId,
    vsaVmManagerId,

    -- ** AppLaunchStatus
    AppLaunchStatus (..),

    -- ** UserDataValidationParameters
    UserDataValidationParameters (..),
    mkUserDataValidationParameters,
    udvpScriptType,
    udvpSource,

    -- ** ReplicationRunId
    ReplicationRunId (..),

    -- ** ReplicationJob
    ReplicationJob (..),
    mkReplicationJob,
    rjDescription,
    rjEncrypted,
    rjFrequency,
    rjKmsKeyId,
    rjLatestAmiId,
    rjLicenseType,
    rjNextReplicationRunStartTime,
    rjNumberOfRecentAmisToKeep,
    rjReplicationJobId,
    rjReplicationRunList,
    rjRoleName,
    rjRunOnce,
    rjSeedReplicationTime,
    rjServerId,
    rjServerType,
    rjState,
    rjStatusMessage,
    rjVmServer,

    -- ** LicenseType
    LicenseType (..),

    -- ** Subnet
    Subnet (..),

    -- ** ReplicationRunStageDetails
    ReplicationRunStageDetails (..),
    mkReplicationRunStageDetails,
    rrsdStage,
    rrsdStageProgress,

    -- ** ServerReplicationParameters
    ServerReplicationParameters (..),
    mkServerReplicationParameters,
    srpEncrypted,
    srpFrequency,
    srpKmsKeyId,
    srpLicenseType,
    srpNumberOfRecentAmisToKeep,
    srpRunOnce,
    srpSeedTime,

    -- ** RoleName
    RoleName (..),

    -- ** Connector
    Connector (..),
    mkConnector,
    cAssociatedOn,
    cCapabilityList,
    cConnectorId,
    cIpAddress,
    cMacAddress,
    cStatus,
    cVersion,
    cVmManagerId,
    cVmManagerName,
    cVmManagerType,

    -- ** AppName
    AppName (..),

    -- ** VmId
    VmId (..),

    -- ** VmServer
    VmServer (..),
    mkVmServer,
    vsVmManagerName,
    vsVmManagerType,
    vsVmName,
    vsVmPath,
    vsVmServerAddress,

    -- ** ServerValidationConfiguration
    ServerValidationConfiguration (..),
    mkServerValidationConfiguration,
    svcName,
    svcServer,
    svcServerValidationStrategy,
    svcUserDataValidationParameters,
    svcValidationId,

    -- ** LogicalId
    LogicalId (..),

    -- ** ConnectorId
    ConnectorId (..),

    -- ** ReplicationJobId
    ReplicationJobId (..),

    -- ** MacAddress
    MacAddress (..),

    -- ** InstanceType
    InstanceType (..),

    -- ** AppReplicationStatus
    AppReplicationStatus (..),

    -- ** SecurityGroup
    SecurityGroup (..),

    -- ** AppReplicationConfigurationStatus
    AppReplicationConfigurationStatus (..),

    -- ** BucketName
    BucketName (..),

    -- ** UserData
    UserData (..),
    mkUserData,
    udS3Location,

    -- ** NextToken
    NextToken (..),

    -- ** SSMValidationParameters
    SSMValidationParameters (..),
    mkSSMValidationParameters,
    ssmvpCommand,
    ssmvpExecutionTimeoutSeconds,
    ssmvpInstanceId,
    ssmvpOutputS3BucketName,
    ssmvpScriptType,
    ssmvpSource,

    -- ** OutputFormat
    OutputFormat (..),

    -- ** ValidationStatusMessage
    ValidationStatusMessage (..),

    -- ** AppId
    AppId (..),

    -- ** ReplicationJobState
    ReplicationJobState (..),

    -- ** VmName
    VmName (..),

    -- ** KmsKeyId
    KmsKeyId (..),

    -- ** ValidationStatus
    ValidationStatus (..),

    -- ** ScriptType
    ScriptType (..),

    -- ** AppDescription
    AppDescription (..),

    -- ** ServerGroupId
    ServerGroupId (..),

    -- ** ReplicationRunStatusMessage
    ReplicationRunStatusMessage (..),

    -- ** NotificationContext
    NotificationContext (..),
    mkNotificationContext,
    ncStatus,
    ncStatusMessage,
    ncValidationId,

    -- ** Source
    Source (..),
    mkSource,
    sS3Location,

    -- ** ReplicationRun
    ReplicationRun (..),
    mkReplicationRun,
    rrAmiId,
    rrCompletedTime,
    rrDescription,
    rrEncrypted,
    rrKmsKeyId,
    rrReplicationRunId,
    rrScheduledStartTime,
    rrStageDetails,
    rrState,
    rrStatusMessage,
    rrType,

    -- ** AppSummary
    AppSummary (..),
    mkAppSummary,
    asAppId,
    asCreationTime,
    asDescription,
    asImportedAppId,
    asLastModified,
    asLatestReplicationTime,
    asLaunchConfigurationStatus,
    asLaunchDetails,
    asLaunchStatus,
    asLaunchStatusMessage,
    asName,
    asReplicationConfigurationStatus,
    asReplicationStatus,
    asReplicationStatusMessage,
    asRoleName,
    asStatus,
    asStatusMessage,
    asTotalServerGroups,
    asTotalServers,

    -- ** S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slKey,

    -- ** Server
    Server (..),
    mkServer,
    sReplicationJobId,
    sReplicationJobTerminated,
    sServerId,
    sServerType,
    sVmServer,

    -- ** AppLaunchConfigurationStatus
    AppLaunchConfigurationStatus (..),

    -- ** AmiId
    AmiId (..),

    -- ** ImportedAppId
    ImportedAppId (..),

    -- ** ConnectorCapability
    ConnectorCapability (..),

    -- ** StackId
    StackId (..),

    -- ** ValidationId
    ValidationId (..),

    -- ** Description
    Description (..),

    -- ** VmPath
    VmPath (..),

    -- ** ServerGroupReplicationConfiguration
    ServerGroupReplicationConfiguration (..),
    mkServerGroupReplicationConfiguration,
    sgrcServerGroupId,
    sgrcServerReplicationConfigurations,

    -- ** ServerReplicationConfiguration
    ServerReplicationConfiguration (..),
    mkServerReplicationConfiguration,
    srcServer,
    srcServerReplicationParameters,

    -- ** StackName
    StackName (..),

    -- ** ServerGroupValidationConfiguration
    ServerGroupValidationConfiguration (..),
    mkServerGroupValidationConfiguration,
    sgvcServerGroupId,
    sgvcServerValidationConfigurations,

    -- ** AppValidationStrategy
    AppValidationStrategy (..),

    -- ** ServerValidationOutput
    ServerValidationOutput (..),
    mkServerValidationOutput,
    svoServer,

    -- ** StatusMessage
    StatusMessage (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** Name
    Name (..),

    -- ** IamInstanceProfileName
    IamInstanceProfileName (..),

    -- ** Vpc
    Vpc (..),

    -- ** LatestAmiId
    LatestAmiId (..),

    -- ** Stage
    Stage (..),

    -- ** Version
    Version (..),

    -- ** LaunchStatusMessage
    LaunchStatusMessage (..),

    -- ** Bucket
    Bucket (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
