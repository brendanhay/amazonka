{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Database Migration Service__ 
--
-- AWS Database Migration Service (AWS DMS) can migrate your data to and from the most widely used commercial and open-source databases such as Oracle, PostgreSQL, Microsoft SQL Server, Amazon Redshift, MariaDB, Amazon Aurora, MySQL, and SAP Adaptive Server Enterprise (ASE). The service supports homogeneous migrations such as Oracle to Oracle, as well as heterogeneous migrations between different database platforms, such as Oracle to MySQL or SQL Server to PostgreSQL.
-- For more information about AWS DMS, see <https://docs.aws.amazon.com/dms/latest/userguide/Welcome.html What Is AWS Database Migration Service?> in the /AWS Database Migration User Guide./ 
module Network.AWS.DMS
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** KMSAccessDeniedFault
    , _KMSAccessDeniedFault

    -- ** KMSDisabledFault
    , _KMSDisabledFault

    -- ** KMSFault
    , _KMSFault

    -- ** InvalidSubnet
    , _InvalidSubnet

    -- ** KMSKeyNotAccessibleFault
    , _KMSKeyNotAccessibleFault

    -- ** ReplicationSubnetGroupDoesNotCoverEnoughAZs
    , _ReplicationSubnetGroupDoesNotCoverEnoughAZs

    -- ** S3ResourceNotFoundFault
    , _S3ResourceNotFoundFault

    -- ** InvalidResourceStateFault
    , _InvalidResourceStateFault

    -- ** InvalidCertificateFault
    , _InvalidCertificateFault

    -- ** SNSNoAuthorizationFault
    , _SNSNoAuthorizationFault

    -- ** ResourceAlreadyExistsFault
    , _ResourceAlreadyExistsFault

    -- ** InsufficientResourceCapacityFault
    , _InsufficientResourceCapacityFault

    -- ** S3AccessDeniedFault
    , _S3AccessDeniedFault

    -- ** SNSInvalidTopicFault
    , _SNSInvalidTopicFault

    -- ** KMSNotFoundFault
    , _KMSNotFoundFault

    -- ** KMSThrottlingFault
    , _KMSThrottlingFault

    -- ** ResourceQuotaExceededFault
    , _ResourceQuotaExceededFault

    -- ** UpgradeDependencyFailureFault
    , _UpgradeDependencyFailureFault

    -- ** ResourceNotFoundFault
    , _ResourceNotFoundFault

    -- ** StorageQuotaExceededFault
    , _StorageQuotaExceededFault

    -- ** AccessDeniedFault
    , _AccessDeniedFault

    -- ** SubnetAlreadyInUse
    , _SubnetAlreadyInUse

    -- ** KMSInvalidStateFault
    , _KMSInvalidStateFault

    -- * Waiters
    -- $waiters

    -- ** ReplicationInstanceAvailable
    , mkReplicationInstanceAvailable

    -- ** ReplicationTaskDeleted
    , mkReplicationTaskDeleted

    -- ** ReplicationTaskReady
    , mkReplicationTaskReady

    -- ** ReplicationInstanceDeleted
    , mkReplicationInstanceDeleted

    -- ** EndpointDeleted
    , mkEndpointDeleted

    -- ** ReplicationTaskStopped
    , mkReplicationTaskStopped

    -- ** ReplicationTaskRunning
    , mkReplicationTaskRunning

    -- ** TestConnectionSucceeds
    , mkTestConnectionSucceeds

    -- * Operations
    -- $operations

    -- ** DeleteReplicationInstance 
    , module Network.AWS.DMS.DeleteReplicationInstance

    -- ** RebootReplicationInstance 
    , module Network.AWS.DMS.RebootReplicationInstance

    -- ** ReloadTables 
    , module Network.AWS.DMS.ReloadTables

    -- ** StartReplicationTaskAssessment 
    , module Network.AWS.DMS.StartReplicationTaskAssessment

    -- ** DeleteReplicationTaskAssessmentRun 
    , module Network.AWS.DMS.DeleteReplicationTaskAssessmentRun

    -- ** CreateEndpoint 
    , module Network.AWS.DMS.CreateEndpoint

    -- ** DescribeSchemas (Paginated)
    , module Network.AWS.DMS.DescribeSchemas

    -- ** DeleteConnection 
    , module Network.AWS.DMS.DeleteConnection

    -- ** ModifyEventSubscription 
    , module Network.AWS.DMS.ModifyEventSubscription

    -- ** DescribeReplicationInstanceTaskLogs 
    , module Network.AWS.DMS.DescribeReplicationInstanceTaskLogs

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.DMS.DescribeEvents

    -- ** DeleteEndpoint 
    , module Network.AWS.DMS.DeleteEndpoint

    -- ** ListTagsForResource 
    , module Network.AWS.DMS.ListTagsForResource

    -- ** DescribeEndpointTypes (Paginated)
    , module Network.AWS.DMS.DescribeEndpointTypes

    -- ** DeleteReplicationTask 
    , module Network.AWS.DMS.DeleteReplicationTask

    -- ** DescribeReplicationTaskAssessmentRuns 
    , module Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns

    -- ** DescribeReplicationTaskAssessmentResults (Paginated)
    , module Network.AWS.DMS.DescribeReplicationTaskAssessmentResults

    -- ** TestConnection 
    , module Network.AWS.DMS.TestConnection

    -- ** DescribeConnections (Paginated)
    , module Network.AWS.DMS.DescribeConnections

    -- ** MoveReplicationTask 
    , module Network.AWS.DMS.MoveReplicationTask

    -- ** RemoveTagsFromResource 
    , module Network.AWS.DMS.RemoveTagsFromResource

    -- ** ModifyEndpoint 
    , module Network.AWS.DMS.ModifyEndpoint

    -- ** CreateEventSubscription 
    , module Network.AWS.DMS.CreateEventSubscription

    -- ** DescribeCertificates (Paginated)
    , module Network.AWS.DMS.DescribeCertificates

    -- ** StartReplicationTaskAssessmentRun 
    , module Network.AWS.DMS.StartReplicationTaskAssessmentRun

    -- ** DeleteEventSubscription 
    , module Network.AWS.DMS.DeleteEventSubscription

    -- ** DescribeTableStatistics (Paginated)
    , module Network.AWS.DMS.DescribeTableStatistics

    -- ** DescribeReplicationSubnetGroups (Paginated)
    , module Network.AWS.DMS.DescribeReplicationSubnetGroups

    -- ** StartReplicationTask 
    , module Network.AWS.DMS.StartReplicationTask

    -- ** DescribeEventSubscriptions (Paginated)
    , module Network.AWS.DMS.DescribeEventSubscriptions

    -- ** AddTagsToResource 
    , module Network.AWS.DMS.AddTagsToResource

    -- ** CreateReplicationSubnetGroup 
    , module Network.AWS.DMS.CreateReplicationSubnetGroup

    -- ** DescribeApplicableIndividualAssessments 
    , module Network.AWS.DMS.DescribeApplicableIndividualAssessments

    -- ** DeleteCertificate 
    , module Network.AWS.DMS.DeleteCertificate

    -- ** RefreshSchemas 
    , module Network.AWS.DMS.RefreshSchemas

    -- ** DescribeReplicationTasks (Paginated)
    , module Network.AWS.DMS.DescribeReplicationTasks

    -- ** DescribeEventCategories 
    , module Network.AWS.DMS.DescribeEventCategories

    -- ** DescribeOrderableReplicationInstances (Paginated)
    , module Network.AWS.DMS.DescribeOrderableReplicationInstances

    -- ** DescribePendingMaintenanceActions 
    , module Network.AWS.DMS.DescribePendingMaintenanceActions

    -- ** CreateReplicationTask 
    , module Network.AWS.DMS.CreateReplicationTask

    -- ** DescribeEndpoints (Paginated)
    , module Network.AWS.DMS.DescribeEndpoints

    -- ** ModifyReplicationInstance 
    , module Network.AWS.DMS.ModifyReplicationInstance

    -- ** ImportCertificate 
    , module Network.AWS.DMS.ImportCertificate

    -- ** CancelReplicationTaskAssessmentRun 
    , module Network.AWS.DMS.CancelReplicationTaskAssessmentRun

    -- ** ModifyReplicationSubnetGroup 
    , module Network.AWS.DMS.ModifyReplicationSubnetGroup

    -- ** DescribeReplicationTaskIndividualAssessments 
    , module Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments

    -- ** ApplyPendingMaintenanceAction 
    , module Network.AWS.DMS.ApplyPendingMaintenanceAction

    -- ** DescribeAccountAttributes 
    , module Network.AWS.DMS.DescribeAccountAttributes

    -- ** DescribeReplicationInstances (Paginated)
    , module Network.AWS.DMS.DescribeReplicationInstances

    -- ** DescribeRefreshSchemasStatus 
    , module Network.AWS.DMS.DescribeRefreshSchemasStatus

    -- ** StopReplicationTask 
    , module Network.AWS.DMS.StopReplicationTask

    -- ** ModifyReplicationTask 
    , module Network.AWS.DMS.ModifyReplicationTask

    -- ** CreateReplicationInstance 
    , module Network.AWS.DMS.CreateReplicationInstance

    -- ** DeleteReplicationSubnetGroup 
    , module Network.AWS.DMS.DeleteReplicationSubnetGroup

    -- * Types

    -- ** ReplicationEndpointTypeValue
    , ReplicationEndpointTypeValue (..)

    -- ** PendingMaintenanceAction
    , PendingMaintenanceAction (..)
    , mkPendingMaintenanceAction
    , pmaAction
    , pmaAutoAppliedAfterDate
    , pmaCurrentApplyDate
    , pmaDescription
    , pmaForcedApplyDate
    , pmaOptInStatus

    -- ** DmsTransferSettings
    , DmsTransferSettings (..)
    , mkDmsTransferSettings
    , dtsBucketName
    , dtsServiceAccessRoleArn

    -- ** Event
    , Event (..)
    , mkEvent
    , eDate
    , eEventCategories
    , eMessage
    , eSourceIdentifier
    , eSourceType

    -- ** ParquetVersionValue
    , ParquetVersionValue (..)

    -- ** MySQLSettings
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

    -- ** TargetDbType
    , TargetDbType (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** DatePartitionSequenceValue
    , DatePartitionSequenceValue (..)

    -- ** StartReplicationTaskTypeValue
    , StartReplicationTaskTypeValue (..)

    -- ** MicrosoftSQLServerSettings
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

    -- ** ReleaseStatusValues
    , ReleaseStatusValues (..)

    -- ** SourceType
    , SourceType (..)

    -- ** ReplicationInstance
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

    -- ** CompressionTypeValue
    , CompressionTypeValue (..)

    -- ** ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions (..)
    , mkResourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- ** ReplicationSubnetGroup
    , ReplicationSubnetGroup (..)
    , mkReplicationSubnetGroup
    , rsgReplicationSubnetGroupDescription
    , rsgReplicationSubnetGroupIdentifier
    , rsgSubnetGroupStatus
    , rsgSubnets
    , rsgVpcId

    -- ** Subnet
    , Subnet (..)
    , mkSubnet
    , sSubnetAvailabilityZone
    , sSubnetIdentifier
    , sSubnetStatus

    -- ** DocDbSettings
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

    -- ** EventCategoryGroup
    , EventCategoryGroup (..)
    , mkEventCategoryGroup
    , ecgEventCategories
    , ecgSourceType

    -- ** PostgreSQLSettings
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

    -- ** KafkaSettings
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

    -- ** OracleSettings
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

    -- ** RedshiftSettings
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

    -- ** ElasticsearchSettings
    , ElasticsearchSettings (..)
    , mkElasticsearchSettings
    , esServiceAccessRoleArn
    , esEndpointUri
    , esErrorRetryDuration
    , esFullLoadErrorPercentage

    -- ** DmsSslModeValue
    , DmsSslModeValue (..)

    -- ** ReplicationTaskIndividualAssessment
    , ReplicationTaskIndividualAssessment (..)
    , mkReplicationTaskIndividualAssessment
    , rtiaIndividualAssessmentName
    , rtiaReplicationTaskAssessmentRunArn
    , rtiaReplicationTaskIndividualAssessmentArn
    , rtiaReplicationTaskIndividualAssessmentStartDate
    , rtiaStatus

    -- ** ReplicationTaskAssessmentRun
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

    -- ** Connection
    , Connection (..)
    , mkConnection
    , cEndpointArn
    , cEndpointIdentifier
    , cLastFailureMessage
    , cReplicationInstanceArn
    , cReplicationInstanceIdentifier
    , cStatus

    -- ** ReplicationTask
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

    -- ** ReloadOptionValue
    , ReloadOptionValue (..)

    -- ** ReplicationInstanceTaskLog
    , ReplicationInstanceTaskLog (..)
    , mkReplicationInstanceTaskLog
    , ritlReplicationInstanceTaskLogSize
    , ritlReplicationTaskArn
    , ritlReplicationTaskName

    -- ** TableToReload
    , TableToReload (..)
    , mkTableToReload
    , ttrSchemaName
    , ttrTableName

    -- ** SafeguardPolicy
    , SafeguardPolicy (..)

    -- ** NeptuneSettings
    , NeptuneSettings (..)
    , mkNeptuneSettings
    , nsS3BucketName
    , nsS3BucketFolder
    , nsErrorRetryDuration
    , nsIamAuthEnabled
    , nsMaxFileSize
    , nsMaxRetryCount
    , nsServiceAccessRoleArn

    -- ** IBMDb2Settings
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

    -- ** MongoDbSettings
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

    -- ** AccountQuota
    , AccountQuota (..)
    , mkAccountQuota
    , aqAccountQuotaName
    , aqMax
    , aqUsed

    -- ** MigrationTypeValue
    , MigrationTypeValue (..)

    -- ** AvailabilityZone
    , AvailabilityZone (..)
    , mkAvailabilityZone
    , azName

    -- ** EncryptionModeValue
    , EncryptionModeValue (..)

    -- ** EventSubscription
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

    -- ** SybaseSettings
    , SybaseSettings (..)
    , mkSybaseSettings
    , ssDatabaseName
    , ssPassword
    , ssPort
    , ssServerName
    , ssUsername

    -- ** ReplicationPendingModifiedValues
    , ReplicationPendingModifiedValues (..)
    , mkReplicationPendingModifiedValues
    , rpmvAllocatedStorage
    , rpmvEngineVersion
    , rpmvMultiAZ
    , rpmvReplicationInstanceClass

    -- ** Certificate
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

    -- ** CharLengthSemantics
    , CharLengthSemantics (..)

    -- ** S3Settings
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

    -- ** SupportedEndpointType
    , SupportedEndpointType (..)
    , mkSupportedEndpointType
    , setEndpointType
    , setEngineDisplayName
    , setEngineName
    , setReplicationInstanceEngineMinimumVersion
    , setSupportsCDC

    -- ** KinesisSettings
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

    -- ** EncodingTypeValue
    , EncodingTypeValue (..)

    -- ** ReplicationTaskStats
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

    -- ** DataFormatValue
    , DataFormatValue (..)

    -- ** Filter
    , Filter (..)
    , mkFilter
    , fName
    , fValues

    -- ** AuthMechanismValue
    , AuthMechanismValue (..)

    -- ** RefreshSchemasStatusTypeValue
    , RefreshSchemasStatusTypeValue (..)

    -- ** AuthTypeValue
    , AuthTypeValue (..)

    -- ** TableStatistics
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

    -- ** ReplicationTaskAssessmentRunProgress
    , ReplicationTaskAssessmentRunProgress (..)
    , mkReplicationTaskAssessmentRunProgress
    , rtarpIndividualAssessmentCompletedCount
    , rtarpIndividualAssessmentCount

    -- ** DynamoDbSettings
    , DynamoDbSettings (..)
    , mkDynamoDbSettings
    , ddsServiceAccessRoleArn

    -- ** RefreshSchemasStatus
    , RefreshSchemasStatus (..)
    , mkRefreshSchemasStatus
    , rssEndpointArn
    , rssLastFailureMessage
    , rssLastRefreshDate
    , rssReplicationInstanceArn
    , rssStatus

    -- ** Endpoint
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

    -- ** MessageFormatValue
    , MessageFormatValue (..)

    -- ** ReplicationTaskAssessmentResult
    , ReplicationTaskAssessmentResult (..)
    , mkReplicationTaskAssessmentResult
    , rAssessmentResults
    , rAssessmentResultsFile
    , rAssessmentStatus
    , rReplicationTaskArn
    , rReplicationTaskIdentifier
    , rReplicationTaskLastAssessmentDate
    , rS3ObjectUrl

    -- ** VpcSecurityGroupMembership
    , VpcSecurityGroupMembership (..)
    , mkVpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVpcSecurityGroupId

    -- ** DatePartitionDelimiterValue
    , DatePartitionDelimiterValue (..)

    -- ** OrderableReplicationInstance
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

    -- ** NestingLevelValue
    , NestingLevelValue (..)

    -- ** Password
    , Password (..)

    -- ** AsmPassword
    , AsmPassword (..)

    -- ** SecurityDbEncryption
    , SecurityDbEncryption (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Waiters
import Network.AWS.DMS.DeleteReplicationInstance
import Network.AWS.DMS.RebootReplicationInstance
import Network.AWS.DMS.ReloadTables
import Network.AWS.DMS.StartReplicationTaskAssessment
import Network.AWS.DMS.DeleteReplicationTaskAssessmentRun
import Network.AWS.DMS.CreateEndpoint
import Network.AWS.DMS.DescribeSchemas
import Network.AWS.DMS.DeleteConnection
import Network.AWS.DMS.ModifyEventSubscription
import Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
import Network.AWS.DMS.DescribeEvents
import Network.AWS.DMS.DeleteEndpoint
import Network.AWS.DMS.ListTagsForResource
import Network.AWS.DMS.DescribeEndpointTypes
import Network.AWS.DMS.DeleteReplicationTask
import Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns
import Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
import Network.AWS.DMS.TestConnection
import Network.AWS.DMS.DescribeConnections
import Network.AWS.DMS.MoveReplicationTask
import Network.AWS.DMS.RemoveTagsFromResource
import Network.AWS.DMS.ModifyEndpoint
import Network.AWS.DMS.CreateEventSubscription
import Network.AWS.DMS.DescribeCertificates
import Network.AWS.DMS.StartReplicationTaskAssessmentRun
import Network.AWS.DMS.DeleteEventSubscription
import Network.AWS.DMS.DescribeTableStatistics
import Network.AWS.DMS.DescribeReplicationSubnetGroups
import Network.AWS.DMS.StartReplicationTask
import Network.AWS.DMS.DescribeEventSubscriptions
import Network.AWS.DMS.AddTagsToResource
import Network.AWS.DMS.CreateReplicationSubnetGroup
import Network.AWS.DMS.DescribeApplicableIndividualAssessments
import Network.AWS.DMS.DeleteCertificate
import Network.AWS.DMS.RefreshSchemas
import Network.AWS.DMS.DescribeReplicationTasks
import Network.AWS.DMS.DescribeEventCategories
import Network.AWS.DMS.DescribeOrderableReplicationInstances
import Network.AWS.DMS.DescribePendingMaintenanceActions
import Network.AWS.DMS.CreateReplicationTask
import Network.AWS.DMS.DescribeEndpoints
import Network.AWS.DMS.ModifyReplicationInstance
import Network.AWS.DMS.ImportCertificate
import Network.AWS.DMS.CancelReplicationTaskAssessmentRun
import Network.AWS.DMS.ModifyReplicationSubnetGroup
import Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments
import Network.AWS.DMS.ApplyPendingMaintenanceAction
import Network.AWS.DMS.DescribeAccountAttributes
import Network.AWS.DMS.DescribeReplicationInstances
import Network.AWS.DMS.DescribeRefreshSchemasStatus
import Network.AWS.DMS.StopReplicationTask
import Network.AWS.DMS.ModifyReplicationTask
import Network.AWS.DMS.CreateReplicationInstance
import Network.AWS.DMS.DeleteReplicationSubnetGroup
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DMS'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
