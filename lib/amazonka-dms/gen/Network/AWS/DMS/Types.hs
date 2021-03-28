-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _KMSAccessDeniedFault
    , _KMSDisabledFault
    , _KMSFault
    , _InvalidSubnet
    , _KMSKeyNotAccessibleFault
    , _ReplicationSubnetGroupDoesNotCoverEnoughAZs
    , _S3ResourceNotFoundFault
    , _InvalidResourceStateFault
    , _InvalidCertificateFault
    , _SNSNoAuthorizationFault
    , _ResourceAlreadyExistsFault
    , _InsufficientResourceCapacityFault
    , _S3AccessDeniedFault
    , _SNSInvalidTopicFault
    , _KMSNotFoundFault
    , _KMSThrottlingFault
    , _ResourceQuotaExceededFault
    , _UpgradeDependencyFailureFault
    , _ResourceNotFoundFault
    , _StorageQuotaExceededFault
    , _AccessDeniedFault
    , _SubnetAlreadyInUse
    , _KMSInvalidStateFault

    -- * ReplicationEndpointTypeValue
    , ReplicationEndpointTypeValue (..)

    -- * PendingMaintenanceAction
    , PendingMaintenanceAction (..)
    , mkPendingMaintenanceAction
    , pmaAction
    , pmaAutoAppliedAfterDate
    , pmaCurrentApplyDate
    , pmaDescription
    , pmaForcedApplyDate
    , pmaOptInStatus

    -- * DmsTransferSettings
    , DmsTransferSettings (..)
    , mkDmsTransferSettings
    , dtsBucketName
    , dtsServiceAccessRoleArn

    -- * Event
    , Event (..)
    , mkEvent
    , eDate
    , eEventCategories
    , eMessage
    , eSourceIdentifier
    , eSourceType

    -- * ParquetVersionValue
    , ParquetVersionValue (..)

    -- * MySQLSettings
    , MySQLSettings (..)
    , mkMySQLSettings
    , msqlsAfterConnectScript
    , msqlsDatabaseName
    , msqlsEventsPollInterval
    , msqlsMaxFileSize
    , msqlsParallelLoadThreads
    , msqlsPassword
    , msqlsPort
    , msqlsServerName
    , msqlsServerTimezone
    , msqlsTargetDbType
    , msqlsUsername

    -- * TargetDbType
    , TargetDbType (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * DatePartitionSequenceValue
    , DatePartitionSequenceValue (..)

    -- * StartReplicationTaskTypeValue
    , StartReplicationTaskTypeValue (..)

    -- * MicrosoftSQLServerSettings
    , MicrosoftSQLServerSettings (..)
    , mkMicrosoftSQLServerSettings
    , msqlssBcpPacketSize
    , msqlssControlTablesFileGroup
    , msqlssDatabaseName
    , msqlssPassword
    , msqlssPort
    , msqlssReadBackupOnly
    , msqlssSafeguardPolicy
    , msqlssServerName
    , msqlssUseBcpFullLoad
    , msqlssUsername

    -- * ReleaseStatusValues
    , ReleaseStatusValues (..)

    -- * SourceType
    , SourceType (..)

    -- * ReplicationInstance
    , ReplicationInstance (..)
    , mkReplicationInstance
    , riAllocatedStorage
    , riAutoMinorVersionUpgrade
    , riAvailabilityZone
    , riDnsNameServers
    , riEngineVersion
    , riFreeUntil
    , riInstanceCreateTime
    , riKmsKeyId
    , riMultiAZ
    , riPendingModifiedValues
    , riPreferredMaintenanceWindow
    , riPubliclyAccessible
    , riReplicationInstanceArn
    , riReplicationInstanceClass
    , riReplicationInstanceIdentifier
    , riReplicationInstancePrivateIpAddress
    , riReplicationInstancePrivateIpAddresses
    , riReplicationInstancePublicIpAddress
    , riReplicationInstancePublicIpAddresses
    , riReplicationInstanceStatus
    , riReplicationSubnetGroup
    , riSecondaryAvailabilityZone
    , riVpcSecurityGroups

    -- * CompressionTypeValue
    , CompressionTypeValue (..)

    -- * ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions (..)
    , mkResourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- * ReplicationSubnetGroup
    , ReplicationSubnetGroup (..)
    , mkReplicationSubnetGroup
    , rsgReplicationSubnetGroupDescription
    , rsgReplicationSubnetGroupIdentifier
    , rsgSubnetGroupStatus
    , rsgSubnets
    , rsgVpcId

    -- * Subnet
    , Subnet (..)
    , mkSubnet
    , sSubnetAvailabilityZone
    , sSubnetIdentifier
    , sSubnetStatus

    -- * DocDbSettings
    , DocDbSettings (..)
    , mkDocDbSettings
    , ddsDatabaseName
    , ddsDocsToInvestigate
    , ddsExtractDocId
    , ddsKmsKeyId
    , ddsNestingLevel
    , ddsPassword
    , ddsPort
    , ddsServerName
    , ddsUsername

    -- * EventCategoryGroup
    , EventCategoryGroup (..)
    , mkEventCategoryGroup
    , ecgEventCategories
    , ecgSourceType

    -- * PostgreSQLSettings
    , PostgreSQLSettings (..)
    , mkPostgreSQLSettings
    , psqlsAfterConnectScript
    , psqlsCaptureDdls
    , psqlsDatabaseName
    , psqlsDdlArtifactsSchema
    , psqlsExecuteTimeout
    , psqlsFailTasksOnLobTruncation
    , psqlsMaxFileSize
    , psqlsPassword
    , psqlsPort
    , psqlsServerName
    , psqlsSlotName
    , psqlsUsername

    -- * KafkaSettings
    , KafkaSettings (..)
    , mkKafkaSettings
    , ksBroker
    , ksIncludeControlDetails
    , ksIncludeNullAndEmpty
    , ksIncludePartitionValue
    , ksIncludeTableAlterOperations
    , ksIncludeTransactionDetails
    , ksMessageFormat
    , ksMessageMaxBytes
    , ksPartitionIncludeSchemaTable
    , ksTopic

    -- * OracleSettings
    , OracleSettings (..)
    , mkOracleSettings
    , osAccessAlternateDirectly
    , osAddSupplementalLogging
    , osAdditionalArchivedLogDestId
    , osAllowSelectNestedTables
    , osArchivedLogDestId
    , osArchivedLogsOnly
    , osAsmPassword
    , osAsmServer
    , osAsmUser
    , osCharLengthSemantics
    , osDatabaseName
    , osDirectPathNoLog
    , osDirectPathParallelLoad
    , osEnableHomogenousTablespace
    , osFailTasksOnLobTruncation
    , osNumberDatatypeScale
    , osOraclePathPrefix
    , osParallelAsmReadThreads
    , osPassword
    , osPort
    , osReadAheadBlocks
    , osReadTableSpaceName
    , osReplacePathPrefix
    , osRetryInterval
    , osSecurityDbEncryption
    , osSecurityDbEncryptionName
    , osServerName
    , osUseAlternateFolderForOnline
    , osUsePathPrefix
    , osUsername

    -- * RedshiftSettings
    , RedshiftSettings (..)
    , mkRedshiftSettings
    , rsAcceptAnyDate
    , rsAfterConnectScript
    , rsBucketFolder
    , rsBucketName
    , rsCaseSensitiveNames
    , rsCompUpdate
    , rsConnectionTimeout
    , rsDatabaseName
    , rsDateFormat
    , rsEmptyAsNull
    , rsEncryptionMode
    , rsExplicitIds
    , rsFileTransferUploadStreams
    , rsLoadTimeout
    , rsMaxFileSize
    , rsPassword
    , rsPort
    , rsRemoveQuotes
    , rsReplaceChars
    , rsReplaceInvalidChars
    , rsServerName
    , rsServerSideEncryptionKmsKeyId
    , rsServiceAccessRoleArn
    , rsTimeFormat
    , rsTrimBlanks
    , rsTruncateColumns
    , rsUsername
    , rsWriteBufferSize

    -- * ElasticsearchSettings
    , ElasticsearchSettings (..)
    , mkElasticsearchSettings
    , esServiceAccessRoleArn
    , esEndpointUri
    , esErrorRetryDuration
    , esFullLoadErrorPercentage

    -- * DmsSslModeValue
    , DmsSslModeValue (..)

    -- * ReplicationTaskIndividualAssessment
    , ReplicationTaskIndividualAssessment (..)
    , mkReplicationTaskIndividualAssessment
    , rtiaIndividualAssessmentName
    , rtiaReplicationTaskAssessmentRunArn
    , rtiaReplicationTaskIndividualAssessmentArn
    , rtiaReplicationTaskIndividualAssessmentStartDate
    , rtiaStatus

    -- * ReplicationTaskAssessmentRun
    , ReplicationTaskAssessmentRun (..)
    , mkReplicationTaskAssessmentRun
    , rtarAssessmentProgress
    , rtarAssessmentRunName
    , rtarLastFailureMessage
    , rtarReplicationTaskArn
    , rtarReplicationTaskAssessmentRunArn
    , rtarReplicationTaskAssessmentRunCreationDate
    , rtarResultEncryptionMode
    , rtarResultKmsKeyArn
    , rtarResultLocationBucket
    , rtarResultLocationFolder
    , rtarServiceAccessRoleArn
    , rtarStatus

    -- * Connection
    , Connection (..)
    , mkConnection
    , cEndpointArn
    , cEndpointIdentifier
    , cLastFailureMessage
    , cReplicationInstanceArn
    , cReplicationInstanceIdentifier
    , cStatus

    -- * ReplicationTask
    , ReplicationTask (..)
    , mkReplicationTask
    , rtCdcStartPosition
    , rtCdcStopPosition
    , rtLastFailureMessage
    , rtMigrationType
    , rtRecoveryCheckpoint
    , rtReplicationInstanceArn
    , rtReplicationTaskArn
    , rtReplicationTaskCreationDate
    , rtReplicationTaskIdentifier
    , rtReplicationTaskSettings
    , rtReplicationTaskStartDate
    , rtReplicationTaskStats
    , rtSourceEndpointArn
    , rtStatus
    , rtStopReason
    , rtTableMappings
    , rtTargetEndpointArn
    , rtTargetReplicationInstanceArn
    , rtTaskData

    -- * ReloadOptionValue
    , ReloadOptionValue (..)

    -- * ReplicationInstanceTaskLog
    , ReplicationInstanceTaskLog (..)
    , mkReplicationInstanceTaskLog
    , ritlReplicationInstanceTaskLogSize
    , ritlReplicationTaskArn
    , ritlReplicationTaskName

    -- * TableToReload
    , TableToReload (..)
    , mkTableToReload
    , ttrSchemaName
    , ttrTableName

    -- * SafeguardPolicy
    , SafeguardPolicy (..)

    -- * NeptuneSettings
    , NeptuneSettings (..)
    , mkNeptuneSettings
    , nsS3BucketName
    , nsS3BucketFolder
    , nsErrorRetryDuration
    , nsIamAuthEnabled
    , nsMaxFileSize
    , nsMaxRetryCount
    , nsServiceAccessRoleArn

    -- * IBMDb2Settings
    , IBMDb2Settings (..)
    , mkIBMDb2Settings
    , ibmdsCurrentLsn
    , ibmdsDatabaseName
    , ibmdsMaxKBytesPerRead
    , ibmdsPassword
    , ibmdsPort
    , ibmdsServerName
    , ibmdsSetDataCaptureChanges
    , ibmdsUsername

    -- * MongoDbSettings
    , MongoDbSettings (..)
    , mkMongoDbSettings
    , mdsAuthMechanism
    , mdsAuthSource
    , mdsAuthType
    , mdsDatabaseName
    , mdsDocsToInvestigate
    , mdsExtractDocId
    , mdsKmsKeyId
    , mdsNestingLevel
    , mdsPassword
    , mdsPort
    , mdsServerName
    , mdsUsername

    -- * AccountQuota
    , AccountQuota (..)
    , mkAccountQuota
    , aqAccountQuotaName
    , aqMax
    , aqUsed

    -- * MigrationTypeValue
    , MigrationTypeValue (..)

    -- * AvailabilityZone
    , AvailabilityZone (..)
    , mkAvailabilityZone
    , azName

    -- * EncryptionModeValue
    , EncryptionModeValue (..)

    -- * EventSubscription
    , EventSubscription (..)
    , mkEventSubscription
    , esCustSubscriptionId
    , esCustomerAwsId
    , esEnabled
    , esEventCategoriesList
    , esSnsTopicArn
    , esSourceIdsList
    , esSourceType
    , esStatus
    , esSubscriptionCreationTime

    -- * SybaseSettings
    , SybaseSettings (..)
    , mkSybaseSettings
    , ssDatabaseName
    , ssPassword
    , ssPort
    , ssServerName
    , ssUsername

    -- * ReplicationPendingModifiedValues
    , ReplicationPendingModifiedValues (..)
    , mkReplicationPendingModifiedValues
    , rpmvAllocatedStorage
    , rpmvEngineVersion
    , rpmvMultiAZ
    , rpmvReplicationInstanceClass

    -- * Certificate
    , Certificate (..)
    , mkCertificate
    , cCertificateArn
    , cCertificateCreationDate
    , cCertificateIdentifier
    , cCertificateOwner
    , cCertificatePem
    , cCertificateWallet
    , cKeyLength
    , cSigningAlgorithm
    , cValidFromDate
    , cValidToDate

    -- * CharLengthSemantics
    , CharLengthSemantics (..)

    -- * S3Settings
    , S3Settings (..)
    , mkS3Settings
    , ssBucketFolder
    , ssBucketName
    , ssCdcInsertsAndUpdates
    , ssCdcInsertsOnly
    , ssCompressionType
    , ssCsvDelimiter
    , ssCsvRowDelimiter
    , ssDataFormat
    , ssDataPageSize
    , ssDatePartitionDelimiter
    , ssDatePartitionEnabled
    , ssDatePartitionSequence
    , ssDictPageSizeLimit
    , ssEnableStatistics
    , ssEncodingType
    , ssEncryptionMode
    , ssExternalTableDefinition
    , ssIncludeOpForFullLoad
    , ssParquetTimestampInMillisecond
    , ssParquetVersion
    , ssRowGroupLength
    , ssServerSideEncryptionKmsKeyId
    , ssServiceAccessRoleArn
    , ssTimestampColumnName

    -- * SupportedEndpointType
    , SupportedEndpointType (..)
    , mkSupportedEndpointType
    , setEndpointType
    , setEngineDisplayName
    , setEngineName
    , setReplicationInstanceEngineMinimumVersion
    , setSupportsCDC

    -- * KinesisSettings
    , KinesisSettings (..)
    , mkKinesisSettings
    , kIncludeControlDetails
    , kIncludeNullAndEmpty
    , kIncludePartitionValue
    , kIncludeTableAlterOperations
    , kIncludeTransactionDetails
    , kMessageFormat
    , kPartitionIncludeSchemaTable
    , kServiceAccessRoleArn
    , kStreamArn

    -- * EncodingTypeValue
    , EncodingTypeValue (..)

    -- * ReplicationTaskStats
    , ReplicationTaskStats (..)
    , mkReplicationTaskStats
    , rtsElapsedTimeMillis
    , rtsFreshStartDate
    , rtsFullLoadFinishDate
    , rtsFullLoadProgressPercent
    , rtsFullLoadStartDate
    , rtsStartDate
    , rtsStopDate
    , rtsTablesErrored
    , rtsTablesLoaded
    , rtsTablesLoading
    , rtsTablesQueued

    -- * DataFormatValue
    , DataFormatValue (..)

    -- * Filter
    , Filter (..)
    , mkFilter
    , fName
    , fValues

    -- * AuthMechanismValue
    , AuthMechanismValue (..)

    -- * RefreshSchemasStatusTypeValue
    , RefreshSchemasStatusTypeValue (..)

    -- * AuthTypeValue
    , AuthTypeValue (..)

    -- * TableStatistics
    , TableStatistics (..)
    , mkTableStatistics
    , tsDdls
    , tsDeletes
    , tsFullLoadCondtnlChkFailedRows
    , tsFullLoadEndTime
    , tsFullLoadErrorRows
    , tsFullLoadReloaded
    , tsFullLoadRows
    , tsFullLoadStartTime
    , tsInserts
    , tsLastUpdateTime
    , tsSchemaName
    , tsTableName
    , tsTableState
    , tsUpdates
    , tsValidationFailedRecords
    , tsValidationPendingRecords
    , tsValidationState
    , tsValidationStateDetails
    , tsValidationSuspendedRecords

    -- * ReplicationTaskAssessmentRunProgress
    , ReplicationTaskAssessmentRunProgress (..)
    , mkReplicationTaskAssessmentRunProgress
    , rtarpIndividualAssessmentCompletedCount
    , rtarpIndividualAssessmentCount

    -- * DynamoDbSettings
    , DynamoDbSettings (..)
    , mkDynamoDbSettings
    , ddsServiceAccessRoleArn

    -- * RefreshSchemasStatus
    , RefreshSchemasStatus (..)
    , mkRefreshSchemasStatus
    , rssEndpointArn
    , rssLastFailureMessage
    , rssLastRefreshDate
    , rssReplicationInstanceArn
    , rssStatus

    -- * Endpoint
    , Endpoint (..)
    , mkEndpoint
    , eCertificateArn
    , eDatabaseName
    , eDmsTransferSettings
    , eDocDbSettings
    , eDynamoDbSettings
    , eElasticsearchSettings
    , eEndpointArn
    , eEndpointIdentifier
    , eEndpointType
    , eEngineDisplayName
    , eEngineName
    , eExternalId
    , eExternalTableDefinition
    , eExtraConnectionAttributes
    , eIBMDb2Settings
    , eKafkaSettings
    , eKinesisSettings
    , eKmsKeyId
    , eMicrosoftSQLServerSettings
    , eMongoDbSettings
    , eMySQLSettings
    , eNeptuneSettings
    , eOracleSettings
    , ePort
    , ePostgreSQLSettings
    , eRedshiftSettings
    , eS3Settings
    , eServerName
    , eServiceAccessRoleArn
    , eSslMode
    , eStatus
    , eSybaseSettings
    , eUsername

    -- * MessageFormatValue
    , MessageFormatValue (..)

    -- * ReplicationTaskAssessmentResult
    , ReplicationTaskAssessmentResult (..)
    , mkReplicationTaskAssessmentResult
    , rAssessmentResults
    , rAssessmentResultsFile
    , rAssessmentStatus
    , rReplicationTaskArn
    , rReplicationTaskIdentifier
    , rReplicationTaskLastAssessmentDate
    , rS3ObjectUrl

    -- * VpcSecurityGroupMembership
    , VpcSecurityGroupMembership (..)
    , mkVpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVpcSecurityGroupId

    -- * DatePartitionDelimiterValue
    , DatePartitionDelimiterValue (..)

    -- * OrderableReplicationInstance
    , OrderableReplicationInstance (..)
    , mkOrderableReplicationInstance
    , oriAvailabilityZones
    , oriDefaultAllocatedStorage
    , oriEngineVersion
    , oriIncludedAllocatedStorage
    , oriMaxAllocatedStorage
    , oriMinAllocatedStorage
    , oriReleaseStatus
    , oriReplicationInstanceClass
    , oriStorageType

    -- * NestingLevelValue
    , NestingLevelValue (..)

    -- * Password
    , Password (..)

    -- * AsmPassword
    , AsmPassword (..)

    -- * SecurityDbEncryption
    , SecurityDbEncryption (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.DMS.Types.ReplicationEndpointTypeValue
  
import Network.AWS.DMS.Types.PendingMaintenanceAction
  
  
import Network.AWS.DMS.Types.DmsTransferSettings
  
import Network.AWS.DMS.Types.Event
  
  
import Network.AWS.DMS.Types.ParquetVersionValue
  
import Network.AWS.DMS.Types.MySQLSettings
  
  
import Network.AWS.DMS.Types.TargetDbType
  
import Network.AWS.DMS.Types.Tag
  
import Network.AWS.DMS.Types.DatePartitionSequenceValue
  
import Network.AWS.DMS.Types.StartReplicationTaskTypeValue
  
import Network.AWS.DMS.Types.MicrosoftSQLServerSettings
  
  
import Network.AWS.DMS.Types.ReleaseStatusValues
  
import Network.AWS.DMS.Types.SourceType
  
import Network.AWS.DMS.Types.ReplicationInstance
  
import Network.AWS.DMS.Types.CompressionTypeValue
  
import Network.AWS.DMS.Types.ResourcePendingMaintenanceActions
  
import Network.AWS.DMS.Types.ReplicationSubnetGroup
  
import Network.AWS.DMS.Types.Subnet
  
  
import Network.AWS.DMS.Types.DocDbSettings
  
import Network.AWS.DMS.Types.EventCategoryGroup
  
  
  
import Network.AWS.DMS.Types.PostgreSQLSettings
  
import Network.AWS.DMS.Types.KafkaSettings
  
import Network.AWS.DMS.Types.OracleSettings
  
import Network.AWS.DMS.Types.RedshiftSettings
  
  
  
import Network.AWS.DMS.Types.ElasticsearchSettings
  
  
  
import Network.AWS.DMS.Types.DmsSslModeValue
  
import Network.AWS.DMS.Types.ReplicationTaskIndividualAssessment
  
import Network.AWS.DMS.Types.ReplicationTaskAssessmentRun
  
import Network.AWS.DMS.Types.Connection
  
  
import Network.AWS.DMS.Types.ReplicationTask
  
  
import Network.AWS.DMS.Types.ReloadOptionValue
  
import Network.AWS.DMS.Types.ReplicationInstanceTaskLog
  
  
import Network.AWS.DMS.Types.TableToReload
  
import Network.AWS.DMS.Types.SafeguardPolicy
  
import Network.AWS.DMS.Types.NeptuneSettings
  
  
import Network.AWS.DMS.Types.IBMDb2Settings
  
import Network.AWS.DMS.Types.MongoDbSettings
  
import Network.AWS.DMS.Types.AccountQuota
  
import Network.AWS.DMS.Types.MigrationTypeValue
  
import Network.AWS.DMS.Types.AvailabilityZone
  
import Network.AWS.DMS.Types.EncryptionModeValue
  
import Network.AWS.DMS.Types.EventSubscription
  
import Network.AWS.DMS.Types.SybaseSettings
  
import Network.AWS.DMS.Types.ReplicationPendingModifiedValues
  
  
import Network.AWS.DMS.Types.Certificate
  
import Network.AWS.DMS.Types.CharLengthSemantics
  
  
import Network.AWS.DMS.Types.S3Settings
  
import Network.AWS.DMS.Types.SupportedEndpointType
  
import Network.AWS.DMS.Types.KinesisSettings
  
  
import Network.AWS.DMS.Types.EncodingTypeValue
  
import Network.AWS.DMS.Types.ReplicationTaskStats
  
import Network.AWS.DMS.Types.DataFormatValue
  
import Network.AWS.DMS.Types.Filter
  
import Network.AWS.DMS.Types.AuthMechanismValue
  
import Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
  
import Network.AWS.DMS.Types.AuthTypeValue
  
import Network.AWS.DMS.Types.TableStatistics
  
import Network.AWS.DMS.Types.ReplicationTaskAssessmentRunProgress
  
import Network.AWS.DMS.Types.DynamoDbSettings
  
  
import Network.AWS.DMS.Types.RefreshSchemasStatus
  
import Network.AWS.DMS.Types.Endpoint
  
import Network.AWS.DMS.Types.MessageFormatValue
  
  
import Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
  
import Network.AWS.DMS.Types.VpcSecurityGroupMembership
  
import Network.AWS.DMS.Types.DatePartitionDelimiterValue
  
  
import Network.AWS.DMS.Types.OrderableReplicationInstance
  
  
  
import Network.AWS.DMS.Types.NestingLevelValue
  
import Network.AWS.DMS.Types.Password
  
import Network.AWS.DMS.Types.AsmPassword
  
import Network.AWS.DMS.Types.SecurityDbEncryption
  

-- | API version @2016-01-01@ of the Amazon Database Migration Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "DMS", Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "dms", Core._svcVersion = "2016-01-01",
                 Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "DMS",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The ciphertext references a key that doesn't exist or that the DMS account doesn't have access to.
_KMSAccessDeniedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSAccessDeniedFault
  = Core._MatchServiceError mkServiceConfig "KMSAccessDeniedFault"
{-# INLINEABLE _KMSAccessDeniedFault #-}
{-# DEPRECATED _KMSAccessDeniedFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified master key (CMK) isn't enabled.
_KMSDisabledFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSDisabledFault
  = Core._MatchServiceError mkServiceConfig "KMSDisabledFault"
{-# INLINEABLE _KMSDisabledFault #-}
{-# DEPRECATED _KMSDisabledFault "Use generic-lens or generic-optics instead"  #-}

-- | An AWS Key Management Service (AWS KMS) error is preventing access to AWS KMS.
_KMSFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSFault = Core._MatchServiceError mkServiceConfig "KMSFault"
{-# INLINEABLE _KMSFault #-}
{-# DEPRECATED _KMSFault "Use generic-lens or generic-optics instead"  #-}

-- | The subnet provided is invalid.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet
  = Core._MatchServiceError mkServiceConfig "InvalidSubnet"
{-# INLINEABLE _InvalidSubnet #-}
{-# DEPRECATED _InvalidSubnet "Use generic-lens or generic-optics instead"  #-}

-- | AWS DMS cannot access the AWS KMS key.
_KMSKeyNotAccessibleFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSKeyNotAccessibleFault
  = Core._MatchServiceError mkServiceConfig
      "KMSKeyNotAccessibleFault"
{-# INLINEABLE _KMSKeyNotAccessibleFault #-}
{-# DEPRECATED _KMSKeyNotAccessibleFault "Use generic-lens or generic-optics instead"  #-}

-- | The replication subnet group does not cover enough Availability Zones (AZs). Edit the replication subnet group and add more AZs.
_ReplicationSubnetGroupDoesNotCoverEnoughAZs :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplicationSubnetGroupDoesNotCoverEnoughAZs
  = Core._MatchServiceError mkServiceConfig
      "ReplicationSubnetGroupDoesNotCoverEnoughAZs"
{-# INLINEABLE _ReplicationSubnetGroupDoesNotCoverEnoughAZs #-}
{-# DEPRECATED _ReplicationSubnetGroupDoesNotCoverEnoughAZs "Use generic-lens or generic-optics instead"  #-}

-- | A specified Amazon S3 bucket, bucket folder, or other object can't be found.
_S3ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_S3ResourceNotFoundFault
  = Core._MatchServiceError mkServiceConfig "S3ResourceNotFoundFault"
{-# INLINEABLE _S3ResourceNotFoundFault #-}
{-# DEPRECATED _S3ResourceNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The resource is in a state that prevents it from being used for database migration.
_InvalidResourceStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResourceStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidResourceStateFault"
{-# INLINEABLE _InvalidResourceStateFault #-}
{-# DEPRECATED _InvalidResourceStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The certificate was not valid.
_InvalidCertificateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateFault
  = Core._MatchServiceError mkServiceConfig "InvalidCertificateFault"
{-# INLINEABLE _InvalidCertificateFault #-}
{-# DEPRECATED _InvalidCertificateFault "Use generic-lens or generic-optics instead"  #-}

-- | You are not authorized for the SNS subscription.
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault
  = Core._MatchServiceError mkServiceConfig "SNSNoAuthorizationFault"
{-# INLINEABLE _SNSNoAuthorizationFault #-}
{-# DEPRECATED _SNSNoAuthorizationFault "Use generic-lens or generic-optics instead"  #-}

-- | The resource you are attempting to create already exists.
_ResourceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "ResourceAlreadyExistsFault"
{-# INLINEABLE _ResourceAlreadyExistsFault #-}
{-# DEPRECATED _ResourceAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | There are not enough resources allocated to the database migration.
_InsufficientResourceCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientResourceCapacityFault
  = Core._MatchServiceError mkServiceConfig
      "InsufficientResourceCapacityFault"
{-# INLINEABLE _InsufficientResourceCapacityFault #-}
{-# DEPRECATED _InsufficientResourceCapacityFault "Use generic-lens or generic-optics instead"  #-}

-- | Insufficient privileges are preventing access to an Amazon S3 object.
_S3AccessDeniedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_S3AccessDeniedFault
  = Core._MatchServiceError mkServiceConfig "S3AccessDeniedFault"
{-# INLINEABLE _S3AccessDeniedFault #-}
{-# DEPRECATED _S3AccessDeniedFault "Use generic-lens or generic-optics instead"  #-}

-- | The SNS topic is invalid.
_SNSInvalidTopicFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SNSInvalidTopicFault
  = Core._MatchServiceError mkServiceConfig "SNSInvalidTopicFault"
{-# INLINEABLE _SNSInvalidTopicFault #-}
{-# DEPRECATED _SNSInvalidTopicFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified AWS KMS entity or resource can't be found.
_KMSNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSNotFoundFault
  = Core._MatchServiceError mkServiceConfig "KMSNotFoundFault"
{-# INLINEABLE _KMSNotFoundFault #-}
{-# DEPRECATED _KMSNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | This request triggered AWS KMS request throttling.
_KMSThrottlingFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSThrottlingFault
  = Core._MatchServiceError mkServiceConfig "KMSThrottlingFault"
{-# INLINEABLE _KMSThrottlingFault #-}
{-# DEPRECATED _KMSThrottlingFault "Use generic-lens or generic-optics instead"  #-}

-- | The quota for this resource quota has been exceeded.
_ResourceQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "ResourceQuotaExceededFault"
{-# INLINEABLE _ResourceQuotaExceededFault #-}
{-# DEPRECATED _ResourceQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | An upgrade dependency is preventing the database migration.
_UpgradeDependencyFailureFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UpgradeDependencyFailureFault
  = Core._MatchServiceError mkServiceConfig
      "UpgradeDependencyFailureFault"
{-# INLINEABLE _UpgradeDependencyFailureFault #-}
{-# DEPRECATED _UpgradeDependencyFailureFault "Use generic-lens or generic-optics instead"  #-}

-- | The resource could not be found.
_ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundFault
  = Core._MatchServiceError mkServiceConfig "ResourceNotFoundFault"
{-# INLINEABLE _ResourceNotFoundFault #-}
{-# DEPRECATED _ResourceNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The storage quota has been exceeded.
_StorageQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StorageQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "StorageQuotaExceededFault"
{-# INLINEABLE _StorageQuotaExceededFault #-}
{-# DEPRECATED _StorageQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | AWS DMS was denied access to the endpoint. Check that the role is correctly configured.
_AccessDeniedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedFault
  = Core._MatchServiceError mkServiceConfig "AccessDeniedFault"
{-# INLINEABLE _AccessDeniedFault #-}
{-# DEPRECATED _AccessDeniedFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified subnet is already in use.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse
  = Core._MatchServiceError mkServiceConfig "SubnetAlreadyInUse"
{-# INLINEABLE _SubnetAlreadyInUse #-}
{-# DEPRECATED _SubnetAlreadyInUse "Use generic-lens or generic-optics instead"  #-}

-- | The state of the specified AWS KMS resource isn't valid for this request.
_KMSInvalidStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateFault
  = Core._MatchServiceError mkServiceConfig "KMSInvalidStateFault"
{-# INLINEABLE _KMSInvalidStateFault #-}
{-# DEPRECATED _KMSInvalidStateFault "Use generic-lens or generic-optics instead"  #-}
