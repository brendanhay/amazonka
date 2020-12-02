{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types
  ( -- * Service Configuration
    dms,

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
    AccountQuota,
    accountQuota,
    aqMax,
    aqUsed,
    aqAccountQuotaName,

    -- * AvailabilityZone
    AvailabilityZone,
    availabilityZone,
    azName,

    -- * Certificate
    Certificate,
    certificate,
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
    Connection,
    connection,
    cStatus,
    cReplicationInstanceARN,
    cEndpointIdentifier,
    cReplicationInstanceIdentifier,
    cEndpointARN,
    cLastFailureMessage,

    -- * DmsTransferSettings
    DmsTransferSettings,
    dmsTransferSettings,
    dtsServiceAccessRoleARN,
    dtsBucketName,

    -- * DocDBSettings
    DocDBSettings,
    docDBSettings,
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
    DynamoDBSettings,
    dynamoDBSettings,
    ddsServiceAccessRoleARN,

    -- * ElasticsearchSettings
    ElasticsearchSettings,
    elasticsearchSettings,
    esFullLoadErrorPercentage,
    esErrorRetryDuration,
    esServiceAccessRoleARN,
    esEndpointURI,

    -- * Endpoint
    Endpoint,
    endpoint,
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
    Event,
    event,
    eSourceType,
    eSourceIdentifier,
    eDate,
    eEventCategories,
    eMessage,

    -- * EventCategoryGroup
    EventCategoryGroup,
    eventCategoryGroup,
    ecgSourceType,
    ecgEventCategories,

    -- * EventSubscription
    EventSubscription,
    eventSubscription,
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
    Filter,
    filter',
    fName,
    fValues,

    -- * IBMDB2Settings
    IBMDB2Settings,
    iBMDB2Settings,
    ibmdsServerName,
    ibmdsCurrentLsn,
    ibmdsSetDataCaptureChanges,
    ibmdsUsername,
    ibmdsPassword,
    ibmdsDatabaseName,
    ibmdsMaxKBytesPerRead,
    ibmdsPort,

    -- * KafkaSettings
    KafkaSettings,
    kafkaSettings,
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
    KinesisSettings,
    kinesisSettings,
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
    MicrosoftSQLServerSettings,
    microsoftSQLServerSettings,
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
    MongoDBSettings,
    mongoDBSettings,
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
    MySQLSettings,
    mySQLSettings,
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
    NeptuneSettings,
    neptuneSettings,
    nsMaxFileSize,
    nsMaxRetryCount,
    nsServiceAccessRoleARN,
    nsIAMAuthEnabled,
    nsErrorRetryDuration,
    nsS3BucketName,
    nsS3BucketFolder,

    -- * OracleSettings
    OracleSettings,
    oracleSettings,
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
    OrderableReplicationInstance,
    orderableReplicationInstance,
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
    PendingMaintenanceAction,
    pendingMaintenanceAction,
    pmaAutoAppliedAfterDate,
    pmaAction,
    pmaOptInStatus,
    pmaDescription,
    pmaForcedApplyDate,
    pmaCurrentApplyDate,

    -- * PostgreSQLSettings
    PostgreSQLSettings,
    postgreSQLSettings,
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
    RedshiftSettings,
    redshiftSettings,
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
    RefreshSchemasStatus,
    refreshSchemasStatus,
    rssStatus,
    rssLastRefreshDate,
    rssReplicationInstanceARN,
    rssEndpointARN,
    rssLastFailureMessage,

    -- * ReplicationInstance
    ReplicationInstance,
    replicationInstance,
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
    ReplicationInstanceTaskLog,
    replicationInstanceTaskLog,
    ritlReplicationTaskName,
    ritlReplicationTaskARN,
    ritlReplicationInstanceTaskLogSize,

    -- * ReplicationPendingModifiedValues
    ReplicationPendingModifiedValues,
    replicationPendingModifiedValues,
    rpmvEngineVersion,
    rpmvMultiAZ,
    rpmvAllocatedStorage,
    rpmvReplicationInstanceClass,

    -- * ReplicationSubnetGroup
    ReplicationSubnetGroup,
    replicationSubnetGroup,
    rsgVPCId,
    rsgSubnets,
    rsgReplicationSubnetGroupIdentifier,
    rsgSubnetGroupStatus,
    rsgReplicationSubnetGroupDescription,

    -- * ReplicationTask
    ReplicationTask,
    replicationTask,
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
    ReplicationTaskAssessmentResult,
    replicationTaskAssessmentResult,
    rAssessmentResults,
    rAssessmentResultsFile,
    rReplicationTaskIdentifier,
    rAssessmentStatus,
    rS3ObjectURL,
    rReplicationTaskLastAssessmentDate,
    rReplicationTaskARN,

    -- * ReplicationTaskAssessmentRun
    ReplicationTaskAssessmentRun,
    replicationTaskAssessmentRun,
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
    ReplicationTaskAssessmentRunProgress,
    replicationTaskAssessmentRunProgress,
    rtarpIndividualAssessmentCount,
    rtarpIndividualAssessmentCompletedCount,

    -- * ReplicationTaskIndividualAssessment
    ReplicationTaskIndividualAssessment,
    replicationTaskIndividualAssessment,
    rtiaStatus,
    rtiaReplicationTaskIndividualAssessmentStartDate,
    rtiaIndividualAssessmentName,
    rtiaReplicationTaskIndividualAssessmentARN,
    rtiaReplicationTaskAssessmentRunARN,

    -- * ReplicationTaskStats
    ReplicationTaskStats,
    replicationTaskStats,
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
    ResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions,
    rpmaPendingMaintenanceActionDetails,
    rpmaResourceIdentifier,

    -- * S3Settings
    S3Settings,
    s3Settings,
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
    Subnet,
    subnet,
    sSubnetStatus,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,

    -- * SupportedEndpointType
    SupportedEndpointType,
    supportedEndpointType,
    setEngineDisplayName,
    setEndpointType,
    setEngineName,
    setReplicationInstanceEngineMinimumVersion,
    setSupportsCDC,

    -- * SybaseSettings
    SybaseSettings,
    sybaseSettings,
    ssServerName,
    ssUsername,
    ssPassword,
    ssDatabaseName,
    ssPort,

    -- * TableStatistics
    TableStatistics,
    tableStatistics,
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
    TableToReload,
    tableToReload,
    ttrSchemaName,
    ttrTableName,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * VPCSecurityGroupMembership
    VPCSecurityGroupMembership,
    vpcSecurityGroupMembership,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-01-01@ of the Amazon Database Migration Service SDK configuration.
dms :: Service
dms =
  Service
    { _svcAbbrev = "DMS",
      _svcSigner = v4,
      _svcPrefix = "dms",
      _svcVersion = "2016-01-01",
      _svcEndpoint = defaultEndpoint dms,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "DMS",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
