-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types
  ( -- * Service configuration
    dmsService,

    -- * Errors

    -- * AuthMechanismValue
    AuthMechanismValue (..),

    -- * AuthTypeValue
    AuthTypeValue (..),

    -- * CharLengthSemantics
    CharLengthSemantics (..),

    -- * CompressionTypeValue
    CompressionTypeValue (..),

    -- * DataFormatValue
    DataFormatValue (..),

    -- * DatePartitionDelimiterValue
    DatePartitionDelimiterValue (..),

    -- * DatePartitionSequenceValue
    DatePartitionSequenceValue (..),

    -- * DmsSSLModeValue
    DmsSSLModeValue (..),

    -- * EncodingTypeValue
    EncodingTypeValue (..),

    -- * EncryptionModeValue
    EncryptionModeValue (..),

    -- * MessageFormatValue
    MessageFormatValue (..),

    -- * MigrationTypeValue
    MigrationTypeValue (..),

    -- * NestingLevelValue
    NestingLevelValue (..),

    -- * ParquetVersionValue
    ParquetVersionValue (..),

    -- * RefreshSchemasStatusTypeValue
    RefreshSchemasStatusTypeValue (..),

    -- * ReleaseStatusValues
    ReleaseStatusValues (..),

    -- * ReloadOptionValue
    ReloadOptionValue (..),

    -- * ReplicationEndpointTypeValue
    ReplicationEndpointTypeValue (..),

    -- * SafeguardPolicy
    SafeguardPolicy (..),

    -- * SourceType
    SourceType (..),

    -- * StartReplicationTaskTypeValue
    StartReplicationTaskTypeValue (..),

    -- * TargetDBType
    TargetDBType (..),

    -- * AccountQuota
    AccountQuota (..),
    mkAccountQuota,
    aqMax,
    aqUsed,
    aqAccountQuotaName,

    -- * AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,

    -- * Certificate
    Certificate (..),
    mkCertificate,
    cCertificateOwner,
    cSigningAlgorithm,
    cValidFromDate,
    cCertificatePem,
    cCertificateARN,
    cCertificateCreationDate,
    cCertificateIdentifier,
    cCertificateWallet,
    cKeyLength,
    cValidToDate,

    -- * Connection
    Connection (..),
    mkConnection,
    cStatus,
    cReplicationInstanceARN,
    cEndpointIdentifier,
    cReplicationInstanceIdentifier,
    cEndpointARN,
    cLastFailureMessage,

    -- * DmsTransferSettings
    DmsTransferSettings (..),
    mkDmsTransferSettings,
    dtsServiceAccessRoleARN,
    dtsBucketName,

    -- * DocDBSettings
    DocDBSettings (..),
    mkDocDBSettings,
    ddsServerName,
    ddsUsername,
    ddsKMSKeyId,
    ddsPassword,
    ddsNestingLevel,
    ddsDatabaseName,
    ddsDocsToInvestigate,
    ddsExtractDocId,
    ddsPort,

    -- * DynamoDBSettings
    DynamoDBSettings (..),
    mkDynamoDBSettings,
    ddsServiceAccessRoleARN,

    -- * ElasticsearchSettings
    ElasticsearchSettings (..),
    mkElasticsearchSettings,
    esFullLoadErrorPercentage,
    esErrorRetryDuration,
    esServiceAccessRoleARN,
    esEndpointURI,

    -- * Endpoint
    Endpoint (..),
    mkEndpoint,
    eStatus,
    eDmsTransferSettings,
    eMySQLSettings,
    eServerName,
    eMicrosoftSQLServerSettings,
    eCertificateARN,
    eServiceAccessRoleARN,
    eDocDBSettings,
    eEngineDisplayName,
    ePostgreSQLSettings,
    eExtraConnectionAttributes,
    eKafkaSettings,
    eOracleSettings,
    eEndpointType,
    eRedshiftSettings,
    eElasticsearchSettings,
    eUsername,
    eExternalTableDefinition,
    eEngineName,
    eNeptuneSettings,
    eIBMDB2Settings,
    eKMSKeyId,
    eMongoDBSettings,
    eSSLMode,
    eSybaseSettings,
    eDatabaseName,
    eS3Settings,
    eKinesisSettings,
    eEndpointIdentifier,
    eExternalId,
    eDynamoDBSettings,
    eEndpointARN,
    ePort,

    -- * Event
    Event (..),
    mkEvent,
    eSourceType,
    eSourceIdentifier,
    eDate,
    eEventCategories,
    eMessage,

    -- * EventCategoryGroup
    EventCategoryGroup (..),
    mkEventCategoryGroup,
    ecgSourceType,
    ecgEventCategories,

    -- * EventSubscription
    EventSubscription (..),
    mkEventSubscription,
    esStatus,
    esCustomerAWSId,
    esCustSubscriptionId,
    esSNSTopicARN,
    esEnabled,
    esSourceType,
    esSubscriptionCreationTime,
    esEventCategoriesList,
    esSourceIdsList,

    -- * Filter
    Filter (..),
    mkFilter,
    fName,
    fValues,

    -- * IBMDB2Settings
    IBMDB2Settings (..),
    mkIBMDB2Settings,
    ibmdsServerName,
    ibmdsCurrentLsn,
    ibmdsSetDataCaptureChanges,
    ibmdsUsername,
    ibmdsPassword,
    ibmdsDatabaseName,
    ibmdsMaxKBytesPerRead,
    ibmdsPort,

    -- * KafkaSettings
    KafkaSettings (..),
    mkKafkaSettings,
    ksIncludeTransactionDetails,
    ksIncludeTableAlterOperations,
    ksPartitionIncludeSchemaTable,
    ksTopic,
    ksIncludeControlDetails,
    ksIncludePartitionValue,
    ksMessageFormat,
    ksBroker,
    ksMessageMaxBytes,
    ksIncludeNullAndEmpty,

    -- * KinesisSettings
    KinesisSettings (..),
    mkKinesisSettings,
    kIncludeTransactionDetails,
    kIncludeTableAlterOperations,
    kServiceAccessRoleARN,
    kPartitionIncludeSchemaTable,
    kStreamARN,
    kIncludeControlDetails,
    kIncludePartitionValue,
    kMessageFormat,
    kIncludeNullAndEmpty,

    -- * MicrosoftSQLServerSettings
    MicrosoftSQLServerSettings (..),
    mkMicrosoftSQLServerSettings,
    msqlssBcpPacketSize,
    msqlssUseBcpFullLoad,
    msqlssServerName,
    msqlssUsername,
    msqlssSafeguardPolicy,
    msqlssPassword,
    msqlssDatabaseName,
    msqlssReadBackupOnly,
    msqlssControlTablesFileGroup,
    msqlssPort,

    -- * MongoDBSettings
    MongoDBSettings (..),
    mkMongoDBSettings,
    mdsServerName,
    mdsAuthMechanism,
    mdsUsername,
    mdsKMSKeyId,
    mdsPassword,
    mdsNestingLevel,
    mdsDatabaseName,
    mdsDocsToInvestigate,
    mdsAuthSource,
    mdsExtractDocId,
    mdsAuthType,
    mdsPort,

    -- * MySQLSettings
    MySQLSettings (..),
    mkMySQLSettings,
    msqlsMaxFileSize,
    msqlsTargetDBType,
    msqlsServerName,
    msqlsParallelLoadThreads,
    msqlsUsername,
    msqlsPassword,
    msqlsEventsPollInterval,
    msqlsDatabaseName,
    msqlsAfterConnectScript,
    msqlsServerTimezone,
    msqlsPort,

    -- * NeptuneSettings
    NeptuneSettings (..),
    mkNeptuneSettings,
    nsMaxFileSize,
    nsMaxRetryCount,
    nsServiceAccessRoleARN,
    nsIAMAuthEnabled,
    nsErrorRetryDuration,
    nsS3BucketName,
    nsS3BucketFolder,

    -- * OracleSettings
    OracleSettings (..),
    mkOracleSettings,
    osFailTasksOnLobTruncation,
    osServerName,
    osDirectPathNoLog,
    osSecurityDBEncryptionName,
    osOraclePathPrefix,
    osUsername,
    osAllowSelectNestedTables,
    osReadAheadBlocks,
    osArchivedLogDestId,
    osReplacePathPrefix,
    osAccessAlternateDirectly,
    osSecurityDBEncryption,
    osReadTableSpaceName,
    osRetryInterval,
    osPassword,
    osDatabaseName,
    osAddSupplementalLogging,
    osAsmServer,
    osCharLengthSemantics,
    osArchivedLogsOnly,
    osDirectPathParallelLoad,
    osAdditionalArchivedLogDestId,
    osAsmPassword,
    osEnableHomogenousTablespace,
    osParallelAsmReadThreads,
    osNumberDatatypeScale,
    osUsePathPrefix,
    osAsmUser,
    osUseAlternateFolderForOnline,
    osPort,

    -- * OrderableReplicationInstance
    OrderableReplicationInstance (..),
    mkOrderableReplicationInstance,
    oriEngineVersion,
    oriMinAllocatedStorage,
    oriReleaseStatus,
    oriIncludedAllocatedStorage,
    oriAvailabilityZones,
    oriMaxAllocatedStorage,
    oriReplicationInstanceClass,
    oriDefaultAllocatedStorage,
    oriStorageType,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    mkPendingMaintenanceAction,
    pmaAutoAppliedAfterDate,
    pmaAction,
    pmaOptInStatus,
    pmaDescription,
    pmaForcedApplyDate,
    pmaCurrentApplyDate,

    -- * PostgreSQLSettings
    PostgreSQLSettings (..),
    mkPostgreSQLSettings,
    psqlsExecuteTimeout,
    psqlsMaxFileSize,
    psqlsFailTasksOnLobTruncation,
    psqlsServerName,
    psqlsDdlArtifactsSchema,
    psqlsSlotName,
    psqlsUsername,
    psqlsPassword,
    psqlsDatabaseName,
    psqlsAfterConnectScript,
    psqlsCaptureDdls,
    psqlsPort,

    -- * RedshiftSettings
    RedshiftSettings (..),
    mkRedshiftSettings,
    rsEmptyAsNull,
    rsCaseSensitiveNames,
    rsMaxFileSize,
    rsReplaceChars,
    rsServerName,
    rsConnectionTimeout,
    rsLoadTimeout,
    rsServiceAccessRoleARN,
    rsExplicitIds,
    rsBucketFolder,
    rsTruncateColumns,
    rsReplaceInvalidChars,
    rsUsername,
    rsBucketName,
    rsEncryptionMode,
    rsDateFormat,
    rsRemoveQuotes,
    rsPassword,
    rsDatabaseName,
    rsAcceptAnyDate,
    rsAfterConnectScript,
    rsWriteBufferSize,
    rsCompUpdate,
    rsTrimBlanks,
    rsTimeFormat,
    rsServerSideEncryptionKMSKeyId,
    rsPort,
    rsFileTransferUploadStreams,

    -- * RefreshSchemasStatus
    RefreshSchemasStatus (..),
    mkRefreshSchemasStatus,
    rssStatus,
    rssLastRefreshDate,
    rssReplicationInstanceARN,
    rssEndpointARN,
    rssLastFailureMessage,

    -- * ReplicationInstance
    ReplicationInstance (..),
    mkReplicationInstance,
    riEngineVersion,
    riPubliclyAccessible,
    riAutoMinorVersionUpgrade,
    riReplicationInstancePublicIPAddresses,
    riReplicationSubnetGroup,
    riInstanceCreateTime,
    riFreeUntil,
    riReplicationInstanceStatus,
    riReplicationInstancePrivateIPAddresses,
    riPreferredMaintenanceWindow,
    riReplicationInstancePrivateIPAddress,
    riKMSKeyId,
    riAvailabilityZone,
    riVPCSecurityGroups,
    riMultiAZ,
    riSecondaryAvailabilityZone,
    riReplicationInstanceARN,
    riAllocatedStorage,
    riDNSNameServers,
    riReplicationInstancePublicIPAddress,
    riReplicationInstanceClass,
    riReplicationInstanceIdentifier,
    riPendingModifiedValues,

    -- * ReplicationInstanceTaskLog
    ReplicationInstanceTaskLog (..),
    mkReplicationInstanceTaskLog,
    ritlReplicationTaskName,
    ritlReplicationTaskARN,
    ritlReplicationInstanceTaskLogSize,

    -- * ReplicationPendingModifiedValues
    ReplicationPendingModifiedValues (..),
    mkReplicationPendingModifiedValues,
    rpmvEngineVersion,
    rpmvMultiAZ,
    rpmvAllocatedStorage,
    rpmvReplicationInstanceClass,

    -- * ReplicationSubnetGroup
    ReplicationSubnetGroup (..),
    mkReplicationSubnetGroup,
    rsgVPCId,
    rsgSubnets,
    rsgReplicationSubnetGroupIdentifier,
    rsgSubnetGroupStatus,
    rsgReplicationSubnetGroupDescription,

    -- * ReplicationTask
    ReplicationTask (..),
    mkReplicationTask,
    repReplicationTaskSettings,
    repStatus,
    repStopReason,
    repTargetEndpointARN,
    repReplicationTaskIdentifier,
    repCdcStartPosition,
    repReplicationTaskStartDate,
    repSourceEndpointARN,
    repRecoveryCheckpoint,
    repTableMappings,
    repTargetReplicationInstanceARN,
    repReplicationTaskCreationDate,
    repMigrationType,
    repReplicationTaskARN,
    repTaskData,
    repCdcStopPosition,
    repReplicationTaskStats,
    repReplicationInstanceARN,
    repLastFailureMessage,

    -- * ReplicationTaskAssessmentResult
    ReplicationTaskAssessmentResult (..),
    mkReplicationTaskAssessmentResult,
    rAssessmentResults,
    rAssessmentResultsFile,
    rReplicationTaskIdentifier,
    rAssessmentStatus,
    rS3ObjectURL,
    rReplicationTaskLastAssessmentDate,
    rReplicationTaskARN,

    -- * ReplicationTaskAssessmentRun
    ReplicationTaskAssessmentRun (..),
    mkReplicationTaskAssessmentRun,
    rtarStatus,
    rtarServiceAccessRoleARN,
    rtarReplicationTaskAssessmentRunCreationDate,
    rtarAssessmentProgress,
    rtarResultKMSKeyARN,
    rtarReplicationTaskARN,
    rtarResultLocationBucket,
    rtarResultLocationFolder,
    rtarResultEncryptionMode,
    rtarAssessmentRunName,
    rtarReplicationTaskAssessmentRunARN,
    rtarLastFailureMessage,

    -- * ReplicationTaskAssessmentRunProgress
    ReplicationTaskAssessmentRunProgress (..),
    mkReplicationTaskAssessmentRunProgress,
    rtarpIndividualAssessmentCount,
    rtarpIndividualAssessmentCompletedCount,

    -- * ReplicationTaskIndividualAssessment
    ReplicationTaskIndividualAssessment (..),
    mkReplicationTaskIndividualAssessment,
    rtiaStatus,
    rtiaReplicationTaskIndividualAssessmentStartDate,
    rtiaIndividualAssessmentName,
    rtiaReplicationTaskIndividualAssessmentARN,
    rtiaReplicationTaskAssessmentRunARN,

    -- * ReplicationTaskStats
    ReplicationTaskStats (..),
    mkReplicationTaskStats,
    rtsStopDate,
    rtsFullLoadProgressPercent,
    rtsFullLoadStartDate,
    rtsElapsedTimeMillis,
    rtsStartDate,
    rtsTablesErrored,
    rtsFullLoadFinishDate,
    rtsTablesLoaded,
    rtsTablesQueued,
    rtsTablesLoading,
    rtsFreshStartDate,

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    mkResourcePendingMaintenanceActions,
    rpmaPendingMaintenanceActionDetails,
    rpmaResourceIdentifier,

    -- * S3Settings
    S3Settings (..),
    mkS3Settings,
    ssParquetVersion,
    ssParquetTimestampInMillisecond,
    ssIncludeOpForFullLoad,
    ssCSVDelimiter,
    ssServiceAccessRoleARN,
    ssBucketFolder,
    ssDataFormat,
    ssDatePartitionEnabled,
    ssEncodingType,
    ssExternalTableDefinition,
    ssDictPageSizeLimit,
    ssBucketName,
    ssEncryptionMode,
    ssEnableStatistics,
    ssCdcInsertsOnly,
    ssTimestampColumnName,
    ssCSVRowDelimiter,
    ssDatePartitionDelimiter,
    ssCompressionType,
    ssServerSideEncryptionKMSKeyId,
    ssDataPageSize,
    ssCdcInsertsAndUpdates,
    ssDatePartitionSequence,
    ssRowGroupLength,

    -- * Subnet
    Subnet (..),
    mkSubnet,
    sSubnetStatus,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,

    -- * SupportedEndpointType
    SupportedEndpointType (..),
    mkSupportedEndpointType,
    setEngineDisplayName,
    setEndpointType,
    setEngineName,
    setReplicationInstanceEngineMinimumVersion,
    setSupportsCDC,

    -- * SybaseSettings
    SybaseSettings (..),
    mkSybaseSettings,
    ssServerName,
    ssUsername,
    ssPassword,
    ssDatabaseName,
    ssPort,

    -- * TableStatistics
    TableStatistics (..),
    mkTableStatistics,
    tsValidationState,
    tsFullLoadRows,
    tsInserts,
    tsFullLoadEndTime,
    tsFullLoadCondtnlChkFailedRows,
    tsFullLoadReloaded,
    tsValidationFailedRecords,
    tsValidationSuspendedRecords,
    tsSchemaName,
    tsValidationStateDetails,
    tsTableState,
    tsFullLoadErrorRows,
    tsDdls,
    tsDeletes,
    tsUpdates,
    tsValidationPendingRecords,
    tsFullLoadStartTime,
    tsLastUpdateTime,
    tsTableName,

    -- * TableToReload
    TableToReload (..),
    mkTableToReload,
    ttrSchemaName,
    ttrTableName,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * VPCSecurityGroupMembership
    VPCSecurityGroupMembership (..),
    mkVPCSecurityGroupMembership,
    vsgmStatus,
    vsgmVPCSecurityGroupId,
  )
where

import Network.AWS.DMS.Types.AccountQuota
import Network.AWS.DMS.Types.AuthMechanismValue
import Network.AWS.DMS.Types.AuthTypeValue
import Network.AWS.DMS.Types.AvailabilityZone
import Network.AWS.DMS.Types.Certificate
import Network.AWS.DMS.Types.CharLengthSemantics
import Network.AWS.DMS.Types.CompressionTypeValue
import Network.AWS.DMS.Types.Connection
import Network.AWS.DMS.Types.DataFormatValue
import Network.AWS.DMS.Types.DatePartitionDelimiterValue
import Network.AWS.DMS.Types.DatePartitionSequenceValue
import Network.AWS.DMS.Types.DmsSSLModeValue
import Network.AWS.DMS.Types.DmsTransferSettings
import Network.AWS.DMS.Types.DocDBSettings
import Network.AWS.DMS.Types.DynamoDBSettings
import Network.AWS.DMS.Types.ElasticsearchSettings
import Network.AWS.DMS.Types.EncodingTypeValue
import Network.AWS.DMS.Types.EncryptionModeValue
import Network.AWS.DMS.Types.Endpoint
import Network.AWS.DMS.Types.Event
import Network.AWS.DMS.Types.EventCategoryGroup
import Network.AWS.DMS.Types.EventSubscription
import Network.AWS.DMS.Types.Filter
import Network.AWS.DMS.Types.IBMDB2Settings
import Network.AWS.DMS.Types.KafkaSettings
import Network.AWS.DMS.Types.KinesisSettings
import Network.AWS.DMS.Types.MessageFormatValue
import Network.AWS.DMS.Types.MicrosoftSQLServerSettings
import Network.AWS.DMS.Types.MigrationTypeValue
import Network.AWS.DMS.Types.MongoDBSettings
import Network.AWS.DMS.Types.MySQLSettings
import Network.AWS.DMS.Types.NeptuneSettings
import Network.AWS.DMS.Types.NestingLevelValue
import Network.AWS.DMS.Types.OracleSettings
import Network.AWS.DMS.Types.OrderableReplicationInstance
import Network.AWS.DMS.Types.ParquetVersionValue
import Network.AWS.DMS.Types.PendingMaintenanceAction
import Network.AWS.DMS.Types.PostgreSQLSettings
import Network.AWS.DMS.Types.RedshiftSettings
import Network.AWS.DMS.Types.RefreshSchemasStatus
import Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
import Network.AWS.DMS.Types.ReleaseStatusValues
import Network.AWS.DMS.Types.ReloadOptionValue
import Network.AWS.DMS.Types.ReplicationEndpointTypeValue
import Network.AWS.DMS.Types.ReplicationInstance
import Network.AWS.DMS.Types.ReplicationInstanceTaskLog
import Network.AWS.DMS.Types.ReplicationPendingModifiedValues
import Network.AWS.DMS.Types.ReplicationSubnetGroup
import Network.AWS.DMS.Types.ReplicationTask
import Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
import Network.AWS.DMS.Types.ReplicationTaskAssessmentRun
import Network.AWS.DMS.Types.ReplicationTaskAssessmentRunProgress
import Network.AWS.DMS.Types.ReplicationTaskIndividualAssessment
import Network.AWS.DMS.Types.ReplicationTaskStats
import Network.AWS.DMS.Types.ResourcePendingMaintenanceActions
import Network.AWS.DMS.Types.S3Settings
import Network.AWS.DMS.Types.SafeguardPolicy
import Network.AWS.DMS.Types.SourceType
import Network.AWS.DMS.Types.StartReplicationTaskTypeValue
import Network.AWS.DMS.Types.Subnet
import Network.AWS.DMS.Types.SupportedEndpointType
import Network.AWS.DMS.Types.SybaseSettings
import Network.AWS.DMS.Types.TableStatistics
import Network.AWS.DMS.Types.TableToReload
import Network.AWS.DMS.Types.Tag
import Network.AWS.DMS.Types.TargetDBType
import Network.AWS.DMS.Types.VPCSecurityGroupMembership
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-01-01@ of the Amazon Database Migration Service SDK configuration.
dmsService :: Lude.Service
dmsService =
  Lude.Service
    { Lude._svcAbbrev = "DMS",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "dms",
      Lude._svcVersion = "2016-01-01",
      Lude._svcEndpoint = Lude.defaultEndpoint dmsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "DMS",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
