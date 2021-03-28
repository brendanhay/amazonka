-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ValidationException
    , _AccessDeniedException
    , _CrawlerRunningException
    , _SchedulerTransitioningException
    , _SchedulerRunningException
    , _ConditionCheckFailureException
    , _ConcurrentRunsExceededException
    , _IllegalWorkflowStateException
    , _NoScheduleException
    , _OperationTimeoutException
    , _ConflictException
    , _CrawlerNotRunningException
    , _VersionMismatchException
    , _MLTransformNotReadyException
    , _EntityNotFoundException
    , _ConcurrentModificationException
    , _SchedulerNotRunningException
    , _InternalServiceException
    , _InvalidInputException
    , _ResourceNumberLimitExceededException
    , _GlueEncryptionException
    , _IdempotentParameterMismatchException
    , _CrawlerStoppingException
    , _AlreadyExistsException

    -- * S3EncryptionMode
    , S3EncryptionMode (..)

    -- * TaskRun
    , TaskRun (..)
    , mkTaskRun
    , trCompletedOn
    , trErrorString
    , trExecutionTime
    , trLastModifiedOn
    , trLogGroupName
    , trProperties
    , trStartedOn
    , trStatus
    , trTaskRunId
    , trTransformId

    -- * GlueResourceArn
    , GlueResourceArn (..)

    -- * GlueVersionString
    , GlueVersionString (..)

    -- * DataLakePrincipalString
    , DataLakePrincipalString (..)

    -- * BinaryColumnStatisticsData
    , BinaryColumnStatisticsData (..)
    , mkBinaryColumnStatisticsData
    , bcsdMaximumLength
    , bcsdAverageLength
    , bcsdNumberOfNulls

    -- * ResourceUri
    , ResourceUri (..)
    , mkResourceUri
    , ruResourceType
    , ruUri

    -- * VersionsString
    , VersionsString (..)

    -- * JobBookmarksEncryptionMode
    , JobBookmarksEncryptionMode (..)

    -- * CloudWatchEncryptionMode
    , CloudWatchEncryptionMode (..)

    -- * CrawlState
    , CrawlState (..)

    -- * PredicateString
    , PredicateString (..)

    -- * BackfillError
    , BackfillError (..)
    , mkBackfillError
    , beCode
    , bePartitions

    -- * Crawler
    , Crawler (..)
    , mkCrawler
    , cgClassifiers
    , cgConfiguration
    , cgCrawlElapsedTime
    , cgCrawlerSecurityConfiguration
    , cgCreationTime
    , cgDatabaseName
    , cgDescription
    , cgLastCrawl
    , cgLastUpdated
    , cgLineageConfiguration
    , cgName
    , cgRecrawlPolicy
    , cgRole
    , cgSchedule
    , cgSchemaChangePolicy
    , cgState
    , cgTablePrefix
    , cgTargets
    , cgVersion

    -- * DevEndpoint
    , DevEndpoint (..)
    , mkDevEndpoint
    , deArguments
    , deAvailabilityZone
    , deCreatedTimestamp
    , deEndpointName
    , deExtraJarsS3Path
    , deExtraPythonLibsS3Path
    , deFailureReason
    , deGlueVersion
    , deLastModifiedTimestamp
    , deLastUpdateStatus
    , deNumberOfNodes
    , deNumberOfWorkers
    , dePrivateAddress
    , dePublicAddress
    , dePublicKey
    , dePublicKeys
    , deRoleArn
    , deSecurityConfiguration
    , deSecurityGroupIds
    , deStatus
    , deSubnetId
    , deVpcId
    , deWorkerType
    , deYarnEndpointAddress
    , deZeppelinRemoteSparkInterpreterPort

    -- * PartitionInput
    , PartitionInput (..)
    , mkPartitionInput
    , piLastAccessTime
    , piLastAnalyzedTime
    , piParameters
    , piStorageDescriptor
    , piValues

    -- * HashString
    , HashString (..)

    -- * PartitionIndexDescriptor
    , PartitionIndexDescriptor (..)
    , mkPartitionIndexDescriptor
    , pidIndexName
    , pidKeys
    , pidIndexStatus
    , pidBackfillErrors

    -- * PaginationToken
    , PaginationToken (..)

    -- * SchemaColumn
    , SchemaColumn (..)
    , mkSchemaColumn
    , scDataType
    , scName

    -- * WorkflowGraph
    , WorkflowGraph (..)
    , mkWorkflowGraph
    , wgEdges
    , wgNodes

    -- * TaskRunFilterCriteria
    , TaskRunFilterCriteria (..)
    , mkTaskRunFilterCriteria
    , trfcStartedAfter
    , trfcStartedBefore
    , trfcStatus
    , trfcTaskRunType

    -- * ResourceShareType
    , ResourceShareType (..)

    -- * MLUserDataEncryption
    , MLUserDataEncryption (..)
    , mkMLUserDataEncryption
    , mludeMlUserDataEncryptionMode
    , mludeKmsKeyId

    -- * S3Encryption
    , S3Encryption (..)
    , mkS3Encryption
    , seKmsKeyArn
    , seS3EncryptionMode

    -- * ConnectionsList
    , ConnectionsList (..)
    , mkConnectionsList
    , clConnections

    -- * SchemaChangePolicy
    , SchemaChangePolicy (..)
    , mkSchemaChangePolicy
    , scpDeleteBehavior
    , scpUpdateBehavior

    -- * DateColumnStatisticsData
    , DateColumnStatisticsData (..)
    , mkDateColumnStatisticsData
    , dcsdNumberOfNulls
    , dcsdNumberOfDistinctValues
    , dcsdMaximumValue
    , dcsdMinimumValue

    -- * FindMatchesMetrics
    , FindMatchesMetrics (..)
    , mkFindMatchesMetrics
    , fmmAreaUnderPRCurve
    , fmmConfusionMatrix
    , fmmF1
    , fmmPrecision
    , fmmRecall

    -- * SortDirectionType
    , SortDirectionType (..)

    -- * SchemaVersionStatus
    , SchemaVersionStatus (..)

    -- * CatalogEntry
    , CatalogEntry (..)
    , mkCatalogEntry
    , ceDatabaseName
    , ceTableName

    -- * JobBookmarksEncryption
    , JobBookmarksEncryption (..)
    , mkJobBookmarksEncryption
    , jbeJobBookmarksEncryptionMode
    , jbeKmsKeyArn

    -- * TransformFilterCriteria
    , TransformFilterCriteria (..)
    , mkTransformFilterCriteria
    , tfcCreatedAfter
    , tfcCreatedBefore
    , tfcGlueVersion
    , tfcLastModifiedAfter
    , tfcLastModifiedBefore
    , tfcName
    , tfcSchema
    , tfcStatus
    , tfcTransformType

    -- * KmsKeyArn
    , KmsKeyArn (..)

    -- * ScriptLocationString
    , ScriptLocationString (..)

    -- * CloudWatchEncryption
    , CloudWatchEncryption (..)
    , mkCloudWatchEncryption
    , cweCloudWatchEncryptionMode
    , cweKmsKeyArn

    -- * BooleanColumnStatisticsData
    , BooleanColumnStatisticsData (..)
    , mkBooleanColumnStatisticsData
    , bNumberOfTrues
    , bNumberOfFalses
    , bNumberOfNulls

    -- * LastCrawlInfo
    , LastCrawlInfo (..)
    , mkLastCrawlInfo
    , lciErrorMessage
    , lciLogGroup
    , lciLogStream
    , lciMessagePrefix
    , lciStartTime
    , lciStatus

    -- * DynamoDBTarget
    , DynamoDBTarget (..)
    , mkDynamoDBTarget
    , ddbtPath
    , ddbtScanAll
    , ddbtScanRate

    -- * UserDefinedFunction
    , UserDefinedFunction (..)
    , mkUserDefinedFunction
    , udfCatalogId
    , udfClassName
    , udfCreateTime
    , udfDatabaseName
    , udfFunctionName
    , udfOwnerName
    , udfOwnerType
    , udfResourceUris

    -- * TriggerNodeDetails
    , TriggerNodeDetails (..)
    , mkTriggerNodeDetails
    , tndTrigger

    -- * BatchUpdatePartitionRequestEntry
    , BatchUpdatePartitionRequestEntry (..)
    , mkBatchUpdatePartitionRequestEntry
    , buprePartitionValueList
    , buprePartitionInput

    -- * WorkflowRun
    , WorkflowRun (..)
    , mkWorkflowRun
    , wrCompletedOn
    , wrErrorMessage
    , wrGraph
    , wrName
    , wrPreviousRunId
    , wrStartedOn
    , wrStatistics
    , wrStatus
    , wrWorkflowRunId
    , wrWorkflowRunProperties

    -- * EncryptionAtRest
    , EncryptionAtRest (..)
    , mkEncryptionAtRest
    , earCatalogEncryptionMode
    , earSseAwsKmsKeyId

    -- * CodeGenArgName
    , CodeGenArgName (..)

    -- * ResourceType
    , ResourceType (..)

    -- * CreateJsonClassifierRequest
    , CreateJsonClassifierRequest (..)
    , mkCreateJsonClassifierRequest
    , cjcrName
    , cjcrJsonPath

    -- * PrincipalPermissions
    , PrincipalPermissions (..)
    , mkPrincipalPermissions
    , ppPermissions
    , ppPrincipal

    -- * RegistryListItem
    , RegistryListItem (..)
    , mkRegistryListItem
    , rliCreatedTime
    , rliDescription
    , rliRegistryArn
    , rliRegistryName
    , rliStatus
    , rliUpdatedTime

    -- * NotificationProperty
    , NotificationProperty (..)
    , mkNotificationProperty
    , npNotifyDelayAfter

    -- * PrincipalType
    , PrincipalType (..)

    -- * CodeGenEdge
    , CodeGenEdge (..)
    , mkCodeGenEdge
    , cgeSource
    , cgeTarget
    , cgeTargetParameter

    -- * Location
    , Location (..)
    , mkLocation
    , lDynamoDB
    , lJdbc
    , lS3

    -- * ErrorString
    , ErrorString (..)

    -- * DeleteBehavior
    , DeleteBehavior (..)

    -- * UpdateBehavior
    , UpdateBehavior (..)

    -- * Database
    , Database (..)
    , mkDatabase
    , dName
    , dCatalogId
    , dCreateTableDefaultPermissions
    , dCreateTime
    , dDescription
    , dLocationUri
    , dParameters
    , dTargetDatabase

    -- * LastCrawlStatus
    , LastCrawlStatus (..)

    -- * Path
    , Path (..)

    -- * JsonPath
    , JsonPath (..)

    -- * MLTransform
    , MLTransform (..)
    , mkMLTransform
    , mltCreatedOn
    , mltDescription
    , mltEvaluationMetrics
    , mltGlueVersion
    , mltInputRecordTables
    , mltLabelCount
    , mltLastModifiedOn
    , mltMaxCapacity
    , mltMaxRetries
    , mltName
    , mltNumberOfWorkers
    , mltParameters
    , mltRole
    , mltSchema
    , mltStatus
    , mltTimeout
    , mltTransformEncryption
    , mltTransformId
    , mltWorkerType

    -- * SchemaStatus
    , SchemaStatus (..)

    -- * TaskType
    , TaskType (..)

    -- * Schedule
    , Schedule (..)
    , mkSchedule
    , sScheduleExpression
    , sState

    -- * TaskRunSortCriteria
    , TaskRunSortCriteria (..)
    , mkTaskRunSortCriteria
    , trscColumn
    , trscSortDirection

    -- * ParametersMapValue
    , ParametersMapValue (..)

    -- * TransformSortCriteria
    , TransformSortCriteria (..)
    , mkTransformSortCriteria
    , tscColumn
    , tscSortDirection

    -- * TableIdentifier
    , TableIdentifier (..)
    , mkTableIdentifier
    , tiCatalogId
    , tiDatabaseName
    , tiName

    -- * RegistryId
    , RegistryId (..)
    , mkRegistryId
    , riRegistryArn
    , riRegistryName

    -- * TaskRunSortColumnType
    , TaskRunSortColumnType (..)

    -- * MappingEntry
    , MappingEntry (..)
    , mkMappingEntry
    , meSourcePath
    , meSourceTable
    , meSourceType
    , meTargetPath
    , meTargetTable
    , meTargetType

    -- * Node
    , Node (..)
    , mkNode
    , nCrawlerDetails
    , nJobDetails
    , nName
    , nTriggerDetails
    , nType
    , nUniqueId

    -- * JobCommand
    , JobCommand (..)
    , mkJobCommand
    , jcfName
    , jcfPythonVersion
    , jcfScriptLocation

    -- * ConnectionInput
    , ConnectionInput (..)
    , mkConnectionInput
    , ciName
    , ciConnectionType
    , ciConnectionProperties
    , ciDescription
    , ciMatchCriteria
    , ciPhysicalConnectionRequirements

    -- * DecimalColumnStatisticsData
    , DecimalColumnStatisticsData (..)
    , mkDecimalColumnStatisticsData
    , dNumberOfNulls
    , dNumberOfDistinctValues
    , dMaximumValue
    , dMinimumValue

    -- * LocationString
    , LocationString (..)

    -- * TransformSortColumnType
    , TransformSortColumnType (..)

    -- * JobName
    , JobName (..)

    -- * ExistCondition
    , ExistCondition (..)

    -- * LogStream
    , LogStream (..)

    -- * TriggerType
    , TriggerType (..)

    -- * DecimalNumber
    , DecimalNumber (..)
    , mkDecimalNumber
    , dnUnscaledValue
    , dnScale

    -- * ColumnStatisticsError
    , ColumnStatisticsError (..)
    , mkColumnStatisticsError
    , cseColumnStatistics
    , cseError

    -- * Token
    , Token (..)

    -- * UpdatedTimestamp
    , UpdatedTimestamp (..)

    -- * SerDeInfo
    , SerDeInfo (..)
    , mkSerDeInfo
    , sdiName
    , sdiParameters
    , sdiSerializationLibrary

    -- * MongoDBTarget
    , MongoDBTarget (..)
    , mkMongoDBTarget
    , mdbtConnectionName
    , mdbtPath
    , mdbtScanAll

    -- * LogicalOperator
    , LogicalOperator (..)

    -- * CrawlerState
    , CrawlerState (..)

    -- * DataFormat
    , DataFormat (..)

    -- * MetadataValueString
    , MetadataValueString (..)

    -- * CsvColumnDelimiter
    , CsvColumnDelimiter (..)

    -- * PythonScript
    , PythonScript (..)

    -- * DatabaseInput
    , DatabaseInput (..)
    , mkDatabaseInput
    , diName
    , diCreateTableDefaultPermissions
    , diDescription
    , diLocationUri
    , diParameters
    , diTargetDatabase

    -- * MetadataKeyValuePair
    , MetadataKeyValuePair (..)
    , mkMetadataKeyValuePair
    , mkvpMetadataKey
    , mkvpMetadataValue

    -- * LogGroup
    , LogGroup (..)

    -- * ColumnStatisticsType
    , ColumnStatisticsType (..)

    -- * JsonValue
    , JsonValue (..)

    -- * ColumnValuesString
    , ColumnValuesString (..)

    -- * Logical
    , Logical (..)

    -- * CrawlerMetrics
    , CrawlerMetrics (..)
    , mkCrawlerMetrics
    , cmCrawlerName
    , cmLastRuntimeSeconds
    , cmMedianRuntimeSeconds
    , cmStillEstimating
    , cmTablesCreated
    , cmTablesDeleted
    , cmTablesUpdated
    , cmTimeLeftSeconds

    -- * GetConnectionsFilter
    , GetConnectionsFilter (..)
    , mkGetConnectionsFilter
    , gcfConnectionType
    , gcfMatchCriteria

    -- * DescriptionString
    , DescriptionString (..)

    -- * WorkerType
    , WorkerType (..)

    -- * PropertyPredicate
    , PropertyPredicate (..)
    , mkPropertyPredicate
    , ppComparator
    , ppKey
    , ppValue

    -- * GrokClassifier
    , GrokClassifier (..)
    , mkGrokClassifier
    , gcName
    , gcClassification
    , gcGrokPattern
    , gcCreationTime
    , gcCustomPatterns
    , gcLastUpdated
    , gcVersion

    -- * Action
    , Action (..)
    , mkAction
    , aArguments
    , aCrawlerName
    , aJobName
    , aNotificationProperty
    , aSecurityConfiguration
    , aTimeout

    -- * ConnectionPropertyKey
    , ConnectionPropertyKey (..)

    -- * DoubleColumnStatisticsData
    , DoubleColumnStatisticsData (..)
    , mkDoubleColumnStatisticsData
    , dcsdfNumberOfNulls
    , dcsdfNumberOfDistinctValues
    , dcsdfMaximumValue
    , dcsdfMinimumValue

    -- * StringColumnStatisticsData
    , StringColumnStatisticsData (..)
    , mkStringColumnStatisticsData
    , scsdMaximumLength
    , scsdAverageLength
    , scsdNumberOfNulls
    , scsdNumberOfDistinctValues

    -- * SecurityConfiguration
    , SecurityConfiguration (..)
    , mkSecurityConfiguration
    , sCreatedTimeStamp
    , sEncryptionConfiguration
    , sName

    -- * Classification
    , Classification (..)

    -- * UpdateXMLClassifierRequest
    , UpdateXMLClassifierRequest (..)
    , mkUpdateXMLClassifierRequest
    , uxmlcrName
    , uxmlcrClassification
    , uxmlcrRowTag

    -- * ErrorMessageString
    , ErrorMessageString (..)

    -- * CrawlerNodeDetails
    , CrawlerNodeDetails (..)
    , mkCrawlerNodeDetails
    , cndCrawls

    -- * XMLClassifier
    , XMLClassifier (..)
    , mkXMLClassifier
    , xmlcName
    , xmlcClassification
    , xmlcCreationTime
    , xmlcLastUpdated
    , xmlcRowTag
    , xmlcVersion

    -- * TransformParameters
    , TransformParameters (..)
    , mkTransformParameters
    , tpTransformType
    , tpFindMatchesParameters

    -- * SchemaListItem
    , SchemaListItem (..)
    , mkSchemaListItem
    , sliCreatedTime
    , sliDescription
    , sliRegistryName
    , sliSchemaArn
    , sliSchemaName
    , sliSchemaStatus
    , sliUpdatedTime

    -- * CommentString
    , CommentString (..)

    -- * CrawlerTargets
    , CrawlerTargets (..)
    , mkCrawlerTargets
    , ctCatalogTargets
    , ctDynamoDBTargets
    , ctJdbcTargets
    , ctMongoDBTargets
    , ctS3Targets

    -- * BatchStopJobRunSuccessfulSubmission
    , BatchStopJobRunSuccessfulSubmission (..)
    , mkBatchStopJobRunSuccessfulSubmission
    , bsjrssJobName
    , bsjrssJobRunId

    -- * ConnectionPasswordEncryption
    , ConnectionPasswordEncryption (..)
    , mkConnectionPasswordEncryption
    , cpeReturnConnectionPasswordEncrypted
    , cpeAwsKmsKeyId

    -- * Connection
    , Connection (..)
    , mkConnection
    , cfConnectionProperties
    , cfConnectionType
    , cfCreationTime
    , cfDescription
    , cfLastUpdatedBy
    , cfLastUpdatedTime
    , cfMatchCriteria
    , cfName
    , cfPhysicalConnectionRequirements

    -- * DataLakePrincipal
    , DataLakePrincipal (..)
    , mkDataLakePrincipal
    , dlpDataLakePrincipalIdentifier

    -- * TriggerUpdate
    , TriggerUpdate (..)
    , mkTriggerUpdate
    , tuActions
    , tuDescription
    , tuName
    , tuPredicate
    , tuSchedule

    -- * TagValue
    , TagValue (..)

    -- * CsvClassifier
    , CsvClassifier (..)
    , mkCsvClassifier
    , ccName
    , ccAllowSingleColumn
    , ccContainsHeader
    , ccCreationTime
    , ccDelimiter
    , ccDisableValueTrimming
    , ccHeader
    , ccLastUpdated
    , ccQuoteSymbol
    , ccVersion

    -- * SchemaDefinitionDiff
    , SchemaDefinitionDiff (..)

    -- * SchemaDefinitionString
    , SchemaDefinitionString (..)

    -- * EvaluationMetrics
    , EvaluationMetrics (..)
    , mkEvaluationMetrics
    , emTransformType
    , emFindMatchesMetrics

    -- * CodeGenNodeType
    , CodeGenNodeType (..)

    -- * ScalaCode
    , ScalaCode (..)

    -- * ConnectionName
    , ConnectionName (..)

    -- * S3Target
    , S3Target (..)
    , mkS3Target
    , stConnectionName
    , stExclusions
    , stPath

    -- * Predicate
    , Predicate (..)
    , mkPredicate
    , pConditions
    , pLogical

    -- * JobRunState
    , JobRunState (..)

    -- * TaskStatusType
    , TaskStatusType (..)

    -- * MLUserDataEncryptionModeString
    , MLUserDataEncryptionModeString (..)

    -- * CrawlerLineageSettings
    , CrawlerLineageSettings (..)

    -- * PartitionIndex
    , PartitionIndex (..)
    , mkPartitionIndex
    , piKeys
    , piIndexName

    -- * UserDefinedFunctionInput
    , UserDefinedFunctionInput (..)
    , mkUserDefinedFunctionInput
    , udfiClassName
    , udfiFunctionName
    , udfiOwnerName
    , udfiOwnerType
    , udfiResourceUris

    -- * TableVersionError
    , TableVersionError (..)
    , mkTableVersionError
    , tveErrorDetail
    , tveTableName
    , tveVersionId

    -- * CreateCsvClassifierRequest
    , CreateCsvClassifierRequest (..)
    , mkCreateCsvClassifierRequest
    , cccrName
    , cccrAllowSingleColumn
    , cccrContainsHeader
    , cccrDelimiter
    , cccrDisableValueTrimming
    , cccrHeader
    , cccrQuoteSymbol

    -- * SchemaId
    , SchemaId (..)
    , mkSchemaId
    , siRegistryName
    , siSchemaArn
    , siSchemaName

    -- * GenericString
    , GenericString (..)

    -- * RecrawlPolicy
    , RecrawlPolicy (..)
    , mkRecrawlPolicy
    , rpRecrawlBehavior

    -- * SchemaVersionListItem
    , SchemaVersionListItem (..)
    , mkSchemaVersionListItem
    , svliCreatedTime
    , svliSchemaArn
    , svliSchemaVersionId
    , svliStatus
    , svliVersionNumber

    -- * ExportLabelsTaskRunProperties
    , ExportLabelsTaskRunProperties (..)
    , mkExportLabelsTaskRunProperties
    , eltrpOutputS3Path

    -- * TransformType
    , TransformType (..)

    -- * TableVersion
    , TableVersion (..)
    , mkTableVersion
    , tvTable
    , tvVersionId

    -- * RunId
    , RunId (..)

    -- * SchemaDiffType
    , SchemaDiffType (..)

    -- * ColumnNameString
    , ColumnNameString (..)

    -- * Role
    , Role (..)

    -- * JobBookmarkEntry
    , JobBookmarkEntry (..)
    , mkJobBookmarkEntry
    , jbeAttempt
    , jbeJobBookmark
    , jbeJobName
    , jbePreviousRunId
    , jbeRun
    , jbeRunId
    , jbeVersion

    -- * MetadataKeyString
    , MetadataKeyString (..)

    -- * Job
    , Job (..)
    , mkJob
    , jAllocatedCapacity
    , jCommand
    , jConnections
    , jCreatedOn
    , jDefaultArguments
    , jDescription
    , jExecutionProperty
    , jGlueVersion
    , jLastModifiedOn
    , jLogUri
    , jMaxCapacity
    , jMaxRetries
    , jName
    , jNonOverridableArguments
    , jNotificationProperty
    , jNumberOfWorkers
    , jRole
    , jSecurityConfiguration
    , jTimeout
    , jWorkerType

    -- * PhysicalConnectionRequirements
    , PhysicalConnectionRequirements (..)
    , mkPhysicalConnectionRequirements
    , pcrAvailabilityZone
    , pcrSecurityGroupIdList
    , pcrSubnetId

    -- * WorkflowRunStatistics
    , WorkflowRunStatistics (..)
    , mkWorkflowRunStatistics
    , wrsFailedActions
    , wrsRunningActions
    , wrsStoppedActions
    , wrsSucceededActions
    , wrsTimeoutActions
    , wrsTotalActions

    -- * Classifier
    , Classifier (..)
    , mkClassifier
    , cCsvClassifier
    , cGrokClassifier
    , cJsonClassifier
    , cXMLClassifier

    -- * TriggerState
    , TriggerState (..)

    -- * JsonClassifier
    , JsonClassifier (..)
    , mkJsonClassifier
    , jcName
    , jcJsonPath
    , jcCreationTime
    , jcLastUpdated
    , jcVersion

    -- * LabelingSetGenerationTaskRunProperties
    , LabelingSetGenerationTaskRunProperties (..)
    , mkLabelingSetGenerationTaskRunProperties
    , lsgtrpOutputS3Path

    -- * VersionString
    , VersionString (..)

    -- * CatalogEncryptionMode
    , CatalogEncryptionMode (..)

    -- * UpdateJsonClassifierRequest
    , UpdateJsonClassifierRequest (..)
    , mkUpdateJsonClassifierRequest
    , ujcrName
    , ujcrJsonPath

    -- * EncryptionConfiguration
    , EncryptionConfiguration (..)
    , mkEncryptionConfiguration
    , ecCloudWatchEncryption
    , ecJobBookmarksEncryption
    , ecS3Encryption

    -- * SchemaReference
    , SchemaReference (..)
    , mkSchemaReference
    , srSchemaId
    , srSchemaVersionId
    , srSchemaVersionNumber

    -- * CodeGenIdentifier
    , CodeGenIdentifier (..)

    -- * PartitionError
    , PartitionError (..)
    , mkPartitionError
    , peErrorDetail
    , pePartitionValues

    -- * SchemaRegistryTokenString
    , SchemaRegistryTokenString (..)

    -- * NameString
    , NameString (..)

    -- * CatalogTarget
    , CatalogTarget (..)
    , mkCatalogTarget
    , ctDatabaseName
    , ctTables

    -- * Sort
    , Sort (..)

    -- * StorageDescriptor
    , StorageDescriptor (..)
    , mkStorageDescriptor
    , sdBucketColumns
    , sdColumns
    , sdCompressed
    , sdInputFormat
    , sdLocation
    , sdNumberOfBuckets
    , sdOutputFormat
    , sdParameters
    , sdSchemaReference
    , sdSerdeInfo
    , sdSkewedInfo
    , sdSortColumns
    , sdStoredAsSubDirectories

    -- * Language
    , Language (..)

    -- * FindMatchesParameters
    , FindMatchesParameters (..)
    , mkFindMatchesParameters
    , fmpAccuracyCostTradeoff
    , fmpEnforceProvidedLabels
    , fmpPrecisionRecallTradeoff
    , fmpPrimaryKeyColumnName

    -- * CsvHeaderOption
    , CsvHeaderOption (..)

    -- * CatalogImportStatus
    , CatalogImportStatus (..)
    , mkCatalogImportStatus
    , cisImportCompleted
    , cisImportTime
    , cisImportedBy

    -- * KeyString
    , KeyString (..)

    -- * ScheduleState
    , ScheduleState (..)

    -- * DatabaseName
    , DatabaseName (..)

    -- * BatchStopJobRunError
    , BatchStopJobRunError (..)
    , mkBatchStopJobRunError
    , bsjreErrorDetail
    , bsjreJobName
    , bsjreJobRunId

    -- * CsvQuoteSymbol
    , CsvQuoteSymbol (..)

    -- * ErrorDetails
    , ErrorDetails (..)
    , mkErrorDetails
    , eErrorCode
    , eErrorMessage

    -- * Partition
    , Partition (..)
    , mkPartition
    , pCatalogId
    , pCreationTime
    , pDatabaseName
    , pLastAccessTime
    , pLastAnalyzedTime
    , pParameters
    , pStorageDescriptor
    , pTableName
    , pValues

    -- * CronExpression
    , CronExpression (..)

    -- * CrawlerSecurityConfiguration
    , CrawlerSecurityConfiguration (..)

    -- * Comparator
    , Comparator (..)

    -- * LineageConfiguration
    , LineageConfiguration (..)
    , mkLineageConfiguration
    , lcCrawlerLineageSettings

    -- * PolicyJsonString
    , PolicyJsonString (..)

    -- * EnableHybridValues
    , EnableHybridValues (..)

    -- * ExecutionProperty
    , ExecutionProperty (..)
    , mkExecutionProperty
    , epMaxConcurrentRuns

    -- * JobNodeDetails
    , JobNodeDetails (..)
    , mkJobNodeDetails
    , jndJobRuns

    -- * FindMatchesTaskRunProperties
    , FindMatchesTaskRunProperties (..)
    , mkFindMatchesTaskRunProperties
    , fmtrpJobId
    , fmtrpJobName
    , fmtrpJobRunId

    -- * TaskRunProperties
    , TaskRunProperties (..)
    , mkTaskRunProperties
    , trpExportLabelsTaskRunProperties
    , trpFindMatchesTaskRunProperties
    , trpImportLabelsTaskRunProperties
    , trpLabelingSetGenerationTaskRunProperties
    , trpTaskType

    -- * Predecessor
    , Predecessor (..)
    , mkPredecessor
    , pJobName
    , pRunId

    -- * TypeString
    , TypeString (..)

    -- * ImportLabelsTaskRunProperties
    , ImportLabelsTaskRunProperties (..)
    , mkImportLabelsTaskRunProperties
    , iltrpInputS3Path
    , iltrpReplace

    -- * GlueTable
    , GlueTable (..)
    , mkGlueTable
    , gtDatabaseName
    , gtTableName
    , gtCatalogId
    , gtConnectionName

    -- * CodeGenNodeArg
    , CodeGenNodeArg (..)
    , mkCodeGenNodeArg
    , cgnaName
    , cgnaValue
    , cgnaParam

    -- * SkewedInfo
    , SkewedInfo (..)
    , mkSkewedInfo
    , siSkewedColumnNames
    , siSkewedColumnValueLocationMaps
    , siSkewedColumnValues

    -- * ViewTextString
    , ViewTextString (..)

    -- * TagKey
    , TagKey (..)

    -- * TransformStatusType
    , TransformStatusType (..)

    -- * PartitionIndexStatus
    , PartitionIndexStatus (..)

    -- * KeySchemaElement
    , KeySchemaElement (..)
    , mkKeySchemaElement
    , kseName
    , kseType

    -- * TableInput
    , TableInput (..)
    , mkTableInput
    , tifName
    , tifDescription
    , tifLastAccessTime
    , tifLastAnalyzedTime
    , tifOwner
    , tifParameters
    , tifPartitionKeys
    , tifRetention
    , tifStorageDescriptor
    , tifTableType
    , tifTargetTable
    , tifViewExpandedText
    , tifViewOriginalText

    -- * SchemaVersionNumber
    , SchemaVersionNumber (..)
    , mkSchemaVersionNumber
    , svnLatestVersion
    , svnVersionNumber

    -- * RecrawlBehavior
    , RecrawlBehavior (..)

    -- * Trigger
    , Trigger (..)
    , mkTrigger
    , tfActions
    , tfDescription
    , tfId
    , tfName
    , tfPredicate
    , tfSchedule
    , tfState
    , tfType
    , tfWorkflowName

    -- * IdString
    , IdString (..)

    -- * CustomPatterns
    , CustomPatterns (..)

    -- * RowTag
    , RowTag (..)

    -- * NodeType
    , NodeType (..)

    -- * TransformEncryption
    , TransformEncryption (..)
    , mkTransformEncryption
    , teMlUserDataEncryption
    , teTaskRunSecurityConfigurationName

    -- * ColumnStatisticsData
    , ColumnStatisticsData (..)
    , mkColumnStatisticsData
    , csdType
    , csdBinaryColumnStatisticsData
    , csdBooleanColumnStatisticsData
    , csdDateColumnStatisticsData
    , csdDecimalColumnStatisticsData
    , csdDoubleColumnStatisticsData
    , csdLongColumnStatisticsData
    , csdStringColumnStatisticsData

    -- * Condition
    , Condition (..)
    , mkCondition
    , cfCrawlState
    , cfCrawlerName
    , cfJobName
    , cfLogicalOperator
    , cfState

    -- * Segment
    , Segment (..)
    , mkSegment
    , sSegmentNumber
    , sTotalSegments

    -- * TableTypeString
    , TableTypeString (..)

    -- * Crawl
    , Crawl (..)
    , mkCrawl
    , cCompletedOn
    , cErrorMessage
    , cLogGroup
    , cLogStream
    , cStartedOn
    , cState

    -- * ColumnTypeString
    , ColumnTypeString (..)

    -- * MessagePrefix
    , MessagePrefix (..)

    -- * Edge
    , Edge (..)
    , mkEdge
    , eDestinationId
    , eSourceId

    -- * Permission
    , Permission (..)

    -- * ColumnStatistics
    , ColumnStatistics (..)
    , mkColumnStatistics
    , csColumnName
    , csColumnType
    , csAnalyzedTime
    , csStatisticsData

    -- * CreateGrokClassifierRequest
    , CreateGrokClassifierRequest (..)
    , mkCreateGrokClassifierRequest
    , cgcrClassification
    , cgcrName
    , cgcrGrokPattern
    , cgcrCustomPatterns

    -- * BatchUpdatePartitionFailureEntry
    , BatchUpdatePartitionFailureEntry (..)
    , mkBatchUpdatePartitionFailureEntry
    , bupfeErrorDetail
    , bupfePartitionValueList

    -- * GluePolicy
    , GluePolicy (..)
    , mkGluePolicy
    , gpCreateTime
    , gpPolicyHash
    , gpPolicyInJson
    , gpUpdateTime

    -- * MetadataInfo
    , MetadataInfo (..)
    , mkMetadataInfo
    , miCreatedTime
    , miMetadataValue

    -- * PartitionValueList
    , PartitionValueList (..)
    , mkPartitionValueList
    , pvlValues

    -- * UriString
    , UriString (..)

    -- * CreateXMLClassifierRequest
    , CreateXMLClassifierRequest (..)
    , mkCreateXMLClassifierRequest
    , cxmlcrClassification
    , cxmlcrName
    , cxmlcrRowTag

    -- * Workflow
    , Workflow (..)
    , mkWorkflow
    , wCreatedOn
    , wDefaultRunProperties
    , wDescription
    , wGraph
    , wLastModifiedOn
    , wLastRun
    , wMaxConcurrentRuns
    , wName

    -- * CatalogIdString
    , CatalogIdString (..)

    -- * SchemaVersionIdString
    , SchemaVersionIdString (..)

    -- * UpdateGrokClassifierRequest
    , UpdateGrokClassifierRequest (..)
    , mkUpdateGrokClassifierRequest
    , ugcrName
    , ugcrClassification
    , ugcrCustomPatterns
    , ugcrGrokPattern

    -- * TablePrefix
    , TablePrefix (..)

    -- * ColumnError
    , ColumnError (..)
    , mkColumnError
    , ceColumnName
    , ceError

    -- * DevEndpointCustomLibraries
    , DevEndpointCustomLibraries (..)
    , mkDevEndpointCustomLibraries
    , declExtraJarsS3Path
    , declExtraPythonLibsS3Path

    -- * JobRun
    , JobRun (..)
    , mkJobRun
    , jrAllocatedCapacity
    , jrArguments
    , jrAttempt
    , jrCompletedOn
    , jrErrorMessage
    , jrExecutionTime
    , jrGlueVersion
    , jrId
    , jrJobName
    , jrJobRunState
    , jrLastModifiedOn
    , jrLogGroupName
    , jrMaxCapacity
    , jrNotificationProperty
    , jrNumberOfWorkers
    , jrPredecessorRuns
    , jrPreviousRunId
    , jrSecurityConfiguration
    , jrStartedOn
    , jrTimeout
    , jrTriggerName
    , jrWorkerType

    -- * SchemaVersionErrorItem
    , SchemaVersionErrorItem (..)
    , mkSchemaVersionErrorItem
    , sveiErrorDetails
    , sveiVersionNumber

    -- * WorkflowRunStatus
    , WorkflowRunStatus (..)

    -- * JdbcTarget
    , JdbcTarget (..)
    , mkJdbcTarget
    , jtConnectionName
    , jtExclusions
    , jtPath

    -- * Table
    , Table (..)
    , mkTable
    , tName
    , tCatalogId
    , tCreateTime
    , tCreatedBy
    , tDatabaseName
    , tDescription
    , tIsRegisteredWithLakeFormation
    , tLastAccessTime
    , tLastAnalyzedTime
    , tOwner
    , tParameters
    , tPartitionKeys
    , tRetention
    , tStorageDescriptor
    , tTableType
    , tTargetTable
    , tUpdateTime
    , tViewExpandedText
    , tViewOriginalText

    -- * Order
    , Order (..)
    , mkOrder
    , oColumn
    , oSortOrder

    -- * Column
    , Column (..)
    , mkColumn
    , cName
    , cComment
    , cParameters
    , cType

    -- * ConnectionType
    , ConnectionType (..)

    -- * JobUpdate
    , JobUpdate (..)
    , mkJobUpdate
    , juAllocatedCapacity
    , juCommand
    , juConnections
    , juDefaultArguments
    , juDescription
    , juExecutionProperty
    , juGlueVersion
    , juLogUri
    , juMaxCapacity
    , juMaxRetries
    , juNonOverridableArguments
    , juNotificationProperty
    , juNumberOfWorkers
    , juRole
    , juSecurityConfiguration
    , juTimeout
    , juWorkerType

    -- * DatabaseIdentifier
    , DatabaseIdentifier (..)
    , mkDatabaseIdentifier
    , diCatalogId
    , diDatabaseName

    -- * TableError
    , TableError (..)
    , mkTableError
    , teErrorDetail
    , teTableName

    -- * Compatibility
    , Compatibility (..)

    -- * ValueString
    , ValueString (..)

    -- * RegistryStatus
    , RegistryStatus (..)

    -- * DataCatalogEncryptionSettings
    , DataCatalogEncryptionSettings (..)
    , mkDataCatalogEncryptionSettings
    , dcesConnectionPasswordEncryption
    , dcesEncryptionAtRest

    -- * TableName
    , TableName (..)

    -- * GrokPattern
    , GrokPattern (..)

    -- * SortCriterion
    , SortCriterion (..)
    , mkSortCriterion
    , scFieldName
    , scSort

    -- * BackfillErrorCode
    , BackfillErrorCode (..)

    -- * CodeGenNode
    , CodeGenNode (..)
    , mkCodeGenNode
    , cgnId
    , cgnNodeType
    , cgnArgs
    , cgnLineNumber

    -- * LongColumnStatisticsData
    , LongColumnStatisticsData (..)
    , mkLongColumnStatisticsData
    , lcsdNumberOfNulls
    , lcsdNumberOfDistinctValues
    , lcsdMaximumValue
    , lcsdMinimumValue

    -- * ErrorDetail
    , ErrorDetail (..)
    , mkErrorDetail
    , edErrorCode
    , edErrorMessage

    -- * RoleArn
    , RoleArn (..)

    -- * UpdateCsvClassifierRequest
    , UpdateCsvClassifierRequest (..)
    , mkUpdateCsvClassifierRequest
    , uccrName
    , uccrAllowSingleColumn
    , uccrContainsHeader
    , uccrDelimiter
    , uccrDisableValueTrimming
    , uccrHeader
    , uccrQuoteSymbol

    -- * ConfusionMatrix
    , ConfusionMatrix (..)
    , mkConfusionMatrix
    , cmNumFalseNegatives
    , cmNumFalsePositives
    , cmNumTrueNegatives
    , cmNumTruePositives

    -- * Name
    , Name (..)

    -- * EndpointName
    , EndpointName (..)

    -- * SchemaDefinition
    , SchemaDefinition (..)

    -- * PublicKey
    , PublicKey (..)

    -- * LogGroupName
    , LogGroupName (..)

    -- * TaskRunId
    , TaskRunId (..)

    -- * TransformId
    , TransformId (..)

    -- * CatalogId
    , CatalogId (..)

    -- * IndexName
    , IndexName (..)

    -- * Uri
    , Uri (..)

    -- * Configuration
    , Configuration (..)

    -- * Description
    , Description (..)

    -- * OutputS3Path
    , OutputS3Path (..)

    -- * Error
    , Error (..)

    -- * NextToken
    , NextToken (..)

    -- * AvailabilityZone
    , AvailabilityZone (..)

    -- * ExtraJarsS3Path
    , ExtraJarsS3Path (..)

    -- * ExtraPythonLibsS3Path
    , ExtraPythonLibsS3Path (..)

    -- * FailureReason
    , FailureReason (..)

    -- * LastUpdateStatus
    , LastUpdateStatus (..)

    -- * PrivateAddress
    , PrivateAddress (..)

    -- * PublicAddress
    , PublicAddress (..)

    -- * Status
    , Status (..)

    -- * SubnetId
    , SubnetId (..)

    -- * VpcId
    , VpcId (..)

    -- * YarnEndpointAddress
    , YarnEndpointAddress (..)

    -- * DataType
    , DataType (..)

    -- * KmsKeyId
    , KmsKeyId (..)

    -- * RegistryName
    , RegistryName (..)

    -- * ErrorMessage
    , ErrorMessage (..)

    -- * SchemaVersionId
    , SchemaVersionId (..)

    -- * ClassName
    , ClassName (..)

    -- * FunctionName
    , FunctionName (..)

    -- * OwnerName
    , OwnerName (..)

    -- * PreviousRunId
    , PreviousRunId (..)

    -- * WorkflowRunId
    , WorkflowRunId (..)

    -- * SseAwsKmsKeyId
    , SseAwsKmsKeyId (..)

    -- * LogUri
    , LogUri (..)

    -- * Expression
    , Expression (..)

    -- * CreatedTime
    , CreatedTime (..)

    -- * UpdatedTime
    , UpdatedTime (..)

    -- * Source
    , Source (..)

    -- * Target
    , Target (..)

    -- * LocationUri
    , LocationUri (..)

    -- * ScheduleExpression
    , ScheduleExpression (..)

    -- * JobRunId
    , JobRunId (..)

    -- * SourcePath
    , SourcePath (..)

    -- * SourceTable
    , SourceTable (..)

    -- * SourceType
    , SourceType (..)

    -- * TargetPath
    , TargetPath (..)

    -- * TargetTable
    , TargetTable (..)

    -- * TargetType
    , TargetType (..)

    -- * UniqueId
    , UniqueId (..)

    -- * PythonVersion
    , PythonVersion (..)

    -- * DependentJobName
    , DependentJobName (..)

    -- * CrawlerName
    , CrawlerName (..)

    -- * SerializationLibrary
    , SerializationLibrary (..)

    -- * MetadataKey
    , MetadataKey (..)

    -- * SchemaName
    , SchemaName (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * AwsKmsKeyId
    , AwsKmsKeyId (..)

    -- * LastUpdatedBy
    , LastUpdatedBy (..)

    -- * QuoteSymbol
    , QuoteSymbol (..)

    -- * PolicyInJson
    , PolicyInJson (..)

    -- * InputFormat
    , InputFormat (..)

    -- * OutputFormat
    , OutputFormat (..)

    -- * SearchText
    , SearchText (..)

    -- * ErrorCode
    , ErrorCode (..)

    -- * InputS3Path
    , InputS3Path (..)

    -- * Type
    , Type (..)

    -- * TableType
    , TableType (..)

    -- * Id
    , Id (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Glue.Types.S3EncryptionMode
  
import Network.AWS.Glue.Types.TaskRun
  
import Network.AWS.Glue.Types.GlueResourceArn
  
import Network.AWS.Glue.Types.GlueVersionString
  
import Network.AWS.Glue.Types.DataLakePrincipalString
  
import Network.AWS.Glue.Types.BinaryColumnStatisticsData
  
import Network.AWS.Glue.Types.ResourceUri
  
import Network.AWS.Glue.Types.VersionsString
  
import Network.AWS.Glue.Types.JobBookmarksEncryptionMode
  
import Network.AWS.Glue.Types.CloudWatchEncryptionMode
  
import Network.AWS.Glue.Types.CrawlState
  
import Network.AWS.Glue.Types.PredicateString
  
import Network.AWS.Glue.Types.BackfillError
  
import Network.AWS.Glue.Types.Crawler
  
  
import Network.AWS.Glue.Types.DevEndpoint
  
  
  
import Network.AWS.Glue.Types.PartitionInput
  
import Network.AWS.Glue.Types.HashString
  
import Network.AWS.Glue.Types.PartitionIndexDescriptor
  
import Network.AWS.Glue.Types.PaginationToken
  
  
import Network.AWS.Glue.Types.SchemaColumn
  
import Network.AWS.Glue.Types.WorkflowGraph
  
import Network.AWS.Glue.Types.TaskRunFilterCriteria
  
import Network.AWS.Glue.Types.ResourceShareType
  
import Network.AWS.Glue.Types.MLUserDataEncryption
  
import Network.AWS.Glue.Types.S3Encryption
  
import Network.AWS.Glue.Types.ConnectionsList
  
import Network.AWS.Glue.Types.SchemaChangePolicy
  
import Network.AWS.Glue.Types.DateColumnStatisticsData
  
import Network.AWS.Glue.Types.FindMatchesMetrics
  
import Network.AWS.Glue.Types.SortDirectionType
  
import Network.AWS.Glue.Types.SchemaVersionStatus
  
import Network.AWS.Glue.Types.CatalogEntry
  
import Network.AWS.Glue.Types.JobBookmarksEncryption
  
import Network.AWS.Glue.Types.TransformFilterCriteria
  
import Network.AWS.Glue.Types.KmsKeyArn
  
import Network.AWS.Glue.Types.ScriptLocationString
  
import Network.AWS.Glue.Types.CloudWatchEncryption
  
import Network.AWS.Glue.Types.BooleanColumnStatisticsData
  
import Network.AWS.Glue.Types.LastCrawlInfo
  
import Network.AWS.Glue.Types.DynamoDBTarget
  
  
import Network.AWS.Glue.Types.UserDefinedFunction
  
import Network.AWS.Glue.Types.TriggerNodeDetails
  
import Network.AWS.Glue.Types.BatchUpdatePartitionRequestEntry
  
import Network.AWS.Glue.Types.WorkflowRun
  
  
import Network.AWS.Glue.Types.EncryptionAtRest
  
import Network.AWS.Glue.Types.CodeGenArgName
  
import Network.AWS.Glue.Types.ResourceType
  
import Network.AWS.Glue.Types.CreateJsonClassifierRequest
  
import Network.AWS.Glue.Types.PrincipalPermissions
  
import Network.AWS.Glue.Types.RegistryListItem
  
import Network.AWS.Glue.Types.NotificationProperty
  
import Network.AWS.Glue.Types.PrincipalType
  
import Network.AWS.Glue.Types.CodeGenEdge
  
import Network.AWS.Glue.Types.Location
  
import Network.AWS.Glue.Types.ErrorString
  
import Network.AWS.Glue.Types.DeleteBehavior
  
import Network.AWS.Glue.Types.UpdateBehavior
  
import Network.AWS.Glue.Types.Database
  
import Network.AWS.Glue.Types.LastCrawlStatus
  
import Network.AWS.Glue.Types.Path
  
  
import Network.AWS.Glue.Types.JsonPath
  
import Network.AWS.Glue.Types.MLTransform
  
import Network.AWS.Glue.Types.SchemaStatus
  
import Network.AWS.Glue.Types.TaskType
  
  
import Network.AWS.Glue.Types.Schedule
  
import Network.AWS.Glue.Types.TaskRunSortCriteria
  
import Network.AWS.Glue.Types.ParametersMapValue
  
import Network.AWS.Glue.Types.TransformSortCriteria
  
import Network.AWS.Glue.Types.TableIdentifier
  
import Network.AWS.Glue.Types.RegistryId
  
import Network.AWS.Glue.Types.TaskRunSortColumnType
  
import Network.AWS.Glue.Types.MappingEntry
  
import Network.AWS.Glue.Types.Node
  
import Network.AWS.Glue.Types.JobCommand
  
import Network.AWS.Glue.Types.ConnectionInput
  
import Network.AWS.Glue.Types.DecimalColumnStatisticsData
  
import Network.AWS.Glue.Types.LocationString
  
import Network.AWS.Glue.Types.TransformSortColumnType
  
import Network.AWS.Glue.Types.JobName
  
import Network.AWS.Glue.Types.ExistCondition
  
import Network.AWS.Glue.Types.LogStream
  
import Network.AWS.Glue.Types.TriggerType
  
import Network.AWS.Glue.Types.DecimalNumber
  
import Network.AWS.Glue.Types.ColumnStatisticsError
  
import Network.AWS.Glue.Types.Token
  
import Network.AWS.Glue.Types.UpdatedTimestamp
  
import Network.AWS.Glue.Types.SerDeInfo
  
import Network.AWS.Glue.Types.MongoDBTarget
  
import Network.AWS.Glue.Types.LogicalOperator
  
import Network.AWS.Glue.Types.CrawlerState
  
import Network.AWS.Glue.Types.DataFormat
  
import Network.AWS.Glue.Types.MetadataValueString
  
import Network.AWS.Glue.Types.CsvColumnDelimiter
  
import Network.AWS.Glue.Types.PythonScript
  
import Network.AWS.Glue.Types.DatabaseInput
  
import Network.AWS.Glue.Types.MetadataKeyValuePair
  
import Network.AWS.Glue.Types.LogGroup
  
import Network.AWS.Glue.Types.ColumnStatisticsType
  
  
import Network.AWS.Glue.Types.JsonValue
  
import Network.AWS.Glue.Types.ColumnValuesString
  
import Network.AWS.Glue.Types.Logical
  
import Network.AWS.Glue.Types.CrawlerMetrics
  
import Network.AWS.Glue.Types.GetConnectionsFilter
  
import Network.AWS.Glue.Types.DescriptionString
  
import Network.AWS.Glue.Types.WorkerType
  
import Network.AWS.Glue.Types.PropertyPredicate
  
import Network.AWS.Glue.Types.GrokClassifier
  
import Network.AWS.Glue.Types.Action
  
import Network.AWS.Glue.Types.ConnectionPropertyKey
  
import Network.AWS.Glue.Types.DoubleColumnStatisticsData
  
import Network.AWS.Glue.Types.StringColumnStatisticsData
  
import Network.AWS.Glue.Types.SecurityConfiguration
  
import Network.AWS.Glue.Types.Classification
  
import Network.AWS.Glue.Types.UpdateXMLClassifierRequest
  
import Network.AWS.Glue.Types.ErrorMessageString
  
import Network.AWS.Glue.Types.CrawlerNodeDetails
  
import Network.AWS.Glue.Types.XMLClassifier
  
import Network.AWS.Glue.Types.TransformParameters
  
import Network.AWS.Glue.Types.SchemaListItem
  
import Network.AWS.Glue.Types.CommentString
  
import Network.AWS.Glue.Types.CrawlerTargets
  
import Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission
  
import Network.AWS.Glue.Types.ConnectionPasswordEncryption
  
import Network.AWS.Glue.Types.Connection
  
  
import Network.AWS.Glue.Types.DataLakePrincipal
  
import Network.AWS.Glue.Types.TriggerUpdate
  
import Network.AWS.Glue.Types.TagValue
  
import Network.AWS.Glue.Types.CsvClassifier
  
import Network.AWS.Glue.Types.SchemaDefinitionDiff
  
import Network.AWS.Glue.Types.SchemaDefinitionString
  
  
  
import Network.AWS.Glue.Types.EvaluationMetrics
  
import Network.AWS.Glue.Types.CodeGenNodeType
  
import Network.AWS.Glue.Types.ScalaCode
  
import Network.AWS.Glue.Types.ConnectionName
  
import Network.AWS.Glue.Types.S3Target
  
import Network.AWS.Glue.Types.Predicate
  
import Network.AWS.Glue.Types.JobRunState
  
import Network.AWS.Glue.Types.TaskStatusType
  
import Network.AWS.Glue.Types.MLUserDataEncryptionModeString
  
  
import Network.AWS.Glue.Types.CrawlerLineageSettings
  
import Network.AWS.Glue.Types.PartitionIndex
  
import Network.AWS.Glue.Types.UserDefinedFunctionInput
  
import Network.AWS.Glue.Types.TableVersionError
  
import Network.AWS.Glue.Types.CreateCsvClassifierRequest
  
import Network.AWS.Glue.Types.SchemaId
  
import Network.AWS.Glue.Types.GenericString
  
import Network.AWS.Glue.Types.RecrawlPolicy
  
import Network.AWS.Glue.Types.SchemaVersionListItem
  
import Network.AWS.Glue.Types.ExportLabelsTaskRunProperties
  
import Network.AWS.Glue.Types.TransformType
  
import Network.AWS.Glue.Types.TableVersion
  
import Network.AWS.Glue.Types.RunId
  
import Network.AWS.Glue.Types.SchemaDiffType
  
  
import Network.AWS.Glue.Types.ColumnNameString
  
import Network.AWS.Glue.Types.Role
  
import Network.AWS.Glue.Types.JobBookmarkEntry
  
import Network.AWS.Glue.Types.MetadataKeyString
  
import Network.AWS.Glue.Types.Job
  
import Network.AWS.Glue.Types.PhysicalConnectionRequirements
  
import Network.AWS.Glue.Types.WorkflowRunStatistics
  
import Network.AWS.Glue.Types.Classifier
  
import Network.AWS.Glue.Types.TriggerState
  
import Network.AWS.Glue.Types.JsonClassifier
  
import Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties
  
import Network.AWS.Glue.Types.VersionString
  
import Network.AWS.Glue.Types.CatalogEncryptionMode
  
import Network.AWS.Glue.Types.UpdateJsonClassifierRequest
  
import Network.AWS.Glue.Types.EncryptionConfiguration
  
  
import Network.AWS.Glue.Types.SchemaReference
  
import Network.AWS.Glue.Types.CodeGenIdentifier
  
import Network.AWS.Glue.Types.PartitionError
  
import Network.AWS.Glue.Types.SchemaRegistryTokenString
  
import Network.AWS.Glue.Types.NameString
  
  
import Network.AWS.Glue.Types.CatalogTarget
  
import Network.AWS.Glue.Types.Sort
  
import Network.AWS.Glue.Types.StorageDescriptor
  
import Network.AWS.Glue.Types.Language
  
import Network.AWS.Glue.Types.FindMatchesParameters
  
import Network.AWS.Glue.Types.CsvHeaderOption
  
import Network.AWS.Glue.Types.CatalogImportStatus
  
import Network.AWS.Glue.Types.KeyString
  
import Network.AWS.Glue.Types.ScheduleState
  
import Network.AWS.Glue.Types.DatabaseName
  
import Network.AWS.Glue.Types.BatchStopJobRunError
  
  
import Network.AWS.Glue.Types.CsvQuoteSymbol
  
import Network.AWS.Glue.Types.ErrorDetails
  
import Network.AWS.Glue.Types.Partition
  
import Network.AWS.Glue.Types.CronExpression
  
import Network.AWS.Glue.Types.CrawlerSecurityConfiguration
  
import Network.AWS.Glue.Types.Comparator
  
import Network.AWS.Glue.Types.LineageConfiguration
  
import Network.AWS.Glue.Types.PolicyJsonString
  
import Network.AWS.Glue.Types.EnableHybridValues
  
import Network.AWS.Glue.Types.ExecutionProperty
  
import Network.AWS.Glue.Types.JobNodeDetails
  
import Network.AWS.Glue.Types.FindMatchesTaskRunProperties
  
import Network.AWS.Glue.Types.TaskRunProperties
  
import Network.AWS.Glue.Types.Predecessor
  
import Network.AWS.Glue.Types.TypeString
  
import Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
  
import Network.AWS.Glue.Types.GlueTable
  
import Network.AWS.Glue.Types.CodeGenNodeArg
  
  
import Network.AWS.Glue.Types.SkewedInfo
  
import Network.AWS.Glue.Types.ViewTextString
  
import Network.AWS.Glue.Types.TagKey
  
import Network.AWS.Glue.Types.TransformStatusType
  
import Network.AWS.Glue.Types.PartitionIndexStatus
  
import Network.AWS.Glue.Types.KeySchemaElement
  
import Network.AWS.Glue.Types.TableInput
  
  
  
import Network.AWS.Glue.Types.SchemaVersionNumber
  
import Network.AWS.Glue.Types.RecrawlBehavior
  
import Network.AWS.Glue.Types.Trigger
  
import Network.AWS.Glue.Types.IdString
  
import Network.AWS.Glue.Types.CustomPatterns
  
import Network.AWS.Glue.Types.RowTag
  
import Network.AWS.Glue.Types.NodeType
  
import Network.AWS.Glue.Types.TransformEncryption
  
import Network.AWS.Glue.Types.ColumnStatisticsData
  
import Network.AWS.Glue.Types.Condition
  
import Network.AWS.Glue.Types.Segment
  
  
import Network.AWS.Glue.Types.TableTypeString
  
import Network.AWS.Glue.Types.Crawl
  
import Network.AWS.Glue.Types.ColumnTypeString
  
import Network.AWS.Glue.Types.MessagePrefix
  
import Network.AWS.Glue.Types.Edge
  
import Network.AWS.Glue.Types.Permission
  
import Network.AWS.Glue.Types.ColumnStatistics
  
import Network.AWS.Glue.Types.CreateGrokClassifierRequest
  
  
import Network.AWS.Glue.Types.BatchUpdatePartitionFailureEntry
  
import Network.AWS.Glue.Types.GluePolicy
  
import Network.AWS.Glue.Types.MetadataInfo
  
import Network.AWS.Glue.Types.PartitionValueList
  
  
import Network.AWS.Glue.Types.UriString
  
import Network.AWS.Glue.Types.CreateXMLClassifierRequest
  
import Network.AWS.Glue.Types.Workflow
  
import Network.AWS.Glue.Types.CatalogIdString
  
import Network.AWS.Glue.Types.SchemaVersionIdString
  
import Network.AWS.Glue.Types.UpdateGrokClassifierRequest
  
import Network.AWS.Glue.Types.TablePrefix
  
import Network.AWS.Glue.Types.ColumnError
  
import Network.AWS.Glue.Types.DevEndpointCustomLibraries
  
import Network.AWS.Glue.Types.JobRun
  
import Network.AWS.Glue.Types.SchemaVersionErrorItem
  
import Network.AWS.Glue.Types.WorkflowRunStatus
  
import Network.AWS.Glue.Types.JdbcTarget
  
import Network.AWS.Glue.Types.Table
  
import Network.AWS.Glue.Types.Order
  
import Network.AWS.Glue.Types.Column
  
import Network.AWS.Glue.Types.ConnectionType
  
import Network.AWS.Glue.Types.JobUpdate
  
import Network.AWS.Glue.Types.DatabaseIdentifier
  
import Network.AWS.Glue.Types.TableError
  
import Network.AWS.Glue.Types.Compatibility
  
import Network.AWS.Glue.Types.ValueString
  
import Network.AWS.Glue.Types.RegistryStatus
  
import Network.AWS.Glue.Types.DataCatalogEncryptionSettings
  
import Network.AWS.Glue.Types.TableName
  
import Network.AWS.Glue.Types.GrokPattern
  
import Network.AWS.Glue.Types.SortCriterion
  
  
import Network.AWS.Glue.Types.BackfillErrorCode
  
import Network.AWS.Glue.Types.CodeGenNode
  
import Network.AWS.Glue.Types.LongColumnStatisticsData
  
import Network.AWS.Glue.Types.ErrorDetail
  
import Network.AWS.Glue.Types.RoleArn
  
import Network.AWS.Glue.Types.UpdateCsvClassifierRequest
  
import Network.AWS.Glue.Types.ConfusionMatrix
  
import Network.AWS.Glue.Types.Name
  
import Network.AWS.Glue.Types.EndpointName
  
import Network.AWS.Glue.Types.SchemaDefinition
  
import Network.AWS.Glue.Types.PublicKey
  
import Network.AWS.Glue.Types.LogGroupName
  
import Network.AWS.Glue.Types.TaskRunId
  
import Network.AWS.Glue.Types.TransformId
  
import Network.AWS.Glue.Types.CatalogId
  
import Network.AWS.Glue.Types.IndexName
  
import Network.AWS.Glue.Types.Uri
  
import Network.AWS.Glue.Types.Configuration
  
import Network.AWS.Glue.Types.Description
  
import Network.AWS.Glue.Types.OutputS3Path
  
import Network.AWS.Glue.Types.Error
  
import Network.AWS.Glue.Types.NextToken
  
import Network.AWS.Glue.Types.AvailabilityZone
  
import Network.AWS.Glue.Types.ExtraJarsS3Path
  
import Network.AWS.Glue.Types.ExtraPythonLibsS3Path
  
import Network.AWS.Glue.Types.FailureReason
  
import Network.AWS.Glue.Types.LastUpdateStatus
  
import Network.AWS.Glue.Types.PrivateAddress
  
import Network.AWS.Glue.Types.PublicAddress
  
import Network.AWS.Glue.Types.Status
  
import Network.AWS.Glue.Types.SubnetId
  
import Network.AWS.Glue.Types.VpcId
  
import Network.AWS.Glue.Types.YarnEndpointAddress
  
import Network.AWS.Glue.Types.DataType
  
import Network.AWS.Glue.Types.KmsKeyId
  
import Network.AWS.Glue.Types.RegistryName
  
import Network.AWS.Glue.Types.ErrorMessage
  
import Network.AWS.Glue.Types.SchemaVersionId
  
import Network.AWS.Glue.Types.ClassName
  
import Network.AWS.Glue.Types.FunctionName
  
import Network.AWS.Glue.Types.OwnerName
  
import Network.AWS.Glue.Types.PreviousRunId
  
import Network.AWS.Glue.Types.WorkflowRunId
  
import Network.AWS.Glue.Types.SseAwsKmsKeyId
  
import Network.AWS.Glue.Types.LogUri
  
import Network.AWS.Glue.Types.Expression
  
import Network.AWS.Glue.Types.CreatedTime
  
import Network.AWS.Glue.Types.UpdatedTime
  
import Network.AWS.Glue.Types.Source
  
import Network.AWS.Glue.Types.Target
  
import Network.AWS.Glue.Types.LocationUri
  
import Network.AWS.Glue.Types.ScheduleExpression
  
import Network.AWS.Glue.Types.JobRunId
  
import Network.AWS.Glue.Types.SourcePath
  
import Network.AWS.Glue.Types.SourceTable
  
import Network.AWS.Glue.Types.SourceType
  
import Network.AWS.Glue.Types.TargetPath
  
import Network.AWS.Glue.Types.TargetTable
  
import Network.AWS.Glue.Types.TargetType
  
import Network.AWS.Glue.Types.UniqueId
  
import Network.AWS.Glue.Types.PythonVersion
  
import Network.AWS.Glue.Types.DependentJobName
  
import Network.AWS.Glue.Types.CrawlerName
  
import Network.AWS.Glue.Types.SerializationLibrary
  
import Network.AWS.Glue.Types.MetadataKey
  
import Network.AWS.Glue.Types.SchemaName
  
import Network.AWS.Glue.Types.Key
  
import Network.AWS.Glue.Types.Value
  
import Network.AWS.Glue.Types.AwsKmsKeyId
  
import Network.AWS.Glue.Types.LastUpdatedBy
  
import Network.AWS.Glue.Types.QuoteSymbol
  
import Network.AWS.Glue.Types.PolicyInJson
  
import Network.AWS.Glue.Types.InputFormat
  
import Network.AWS.Glue.Types.OutputFormat
  
import Network.AWS.Glue.Types.SearchText
  
import Network.AWS.Glue.Types.ErrorCode
  
import Network.AWS.Glue.Types.InputS3Path
  
import Network.AWS.Glue.Types.Type
  
import Network.AWS.Glue.Types.TableType
  
import Network.AWS.Glue.Types.Id
  

-- | API version @2017-03-31@ of the Amazon Glue SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "Glue", Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "glue", Core._svcVersion = "2017-03-31",
                 Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "Glue",
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

-- | A value could not be validated.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException
  = Core._MatchServiceError mkServiceConfig "ValidationException"
{-# INLINEABLE _ValidationException #-}
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead"  #-}

-- | Access to a resource was denied.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException
  = Core._MatchServiceError mkServiceConfig "AccessDeniedException"
{-# INLINEABLE _AccessDeniedException #-}
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | The operation cannot be performed because the crawler is already running.
_CrawlerRunningException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CrawlerRunningException
  = Core._MatchServiceError mkServiceConfig "CrawlerRunningException"
{-# INLINEABLE _CrawlerRunningException #-}
{-# DEPRECATED _CrawlerRunningException "Use generic-lens or generic-optics instead"  #-}

-- | The specified scheduler is transitioning.
_SchedulerTransitioningException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SchedulerTransitioningException
  = Core._MatchServiceError mkServiceConfig
      "SchedulerTransitioningException"
{-# INLINEABLE _SchedulerTransitioningException #-}
{-# DEPRECATED _SchedulerTransitioningException "Use generic-lens or generic-optics instead"  #-}

-- | The specified scheduler is already running.
_SchedulerRunningException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SchedulerRunningException
  = Core._MatchServiceError mkServiceConfig
      "SchedulerRunningException"
{-# INLINEABLE _SchedulerRunningException #-}
{-# DEPRECATED _SchedulerRunningException "Use generic-lens or generic-optics instead"  #-}

-- | A specified condition was not satisfied.
_ConditionCheckFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConditionCheckFailureException
  = Core._MatchServiceError mkServiceConfig
      "ConditionCheckFailureException"
{-# INLINEABLE _ConditionCheckFailureException #-}
{-# DEPRECATED _ConditionCheckFailureException "Use generic-lens or generic-optics instead"  #-}

-- | Too many jobs are being run concurrently.
_ConcurrentRunsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentRunsExceededException
  = Core._MatchServiceError mkServiceConfig
      "ConcurrentRunsExceededException"
{-# INLINEABLE _ConcurrentRunsExceededException #-}
{-# DEPRECATED _ConcurrentRunsExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The workflow is in an invalid state to perform a requested operation.
_IllegalWorkflowStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IllegalWorkflowStateException
  = Core._MatchServiceError mkServiceConfig
      "IllegalWorkflowStateException"
{-# INLINEABLE _IllegalWorkflowStateException #-}
{-# DEPRECATED _IllegalWorkflowStateException "Use generic-lens or generic-optics instead"  #-}

-- | There is no applicable schedule.
_NoScheduleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoScheduleException
  = Core._MatchServiceError mkServiceConfig "NoScheduleException"
{-# INLINEABLE _NoScheduleException #-}
{-# DEPRECATED _NoScheduleException "Use generic-lens or generic-optics instead"  #-}

-- | The operation timed out.
_OperationTimeoutException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationTimeoutException
  = Core._MatchServiceError mkServiceConfig
      "OperationTimeoutException"
{-# INLINEABLE _OperationTimeoutException #-}
{-# DEPRECATED _OperationTimeoutException "Use generic-lens or generic-optics instead"  #-}

-- | The @CreatePartitions@ API was called on a table that has indexes enabled. 
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException
  = Core._MatchServiceError mkServiceConfig "ConflictException"
{-# INLINEABLE _ConflictException #-}
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The specified crawler is not running.
_CrawlerNotRunningException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CrawlerNotRunningException
  = Core._MatchServiceError mkServiceConfig
      "CrawlerNotRunningException"
{-# INLINEABLE _CrawlerNotRunningException #-}
{-# DEPRECATED _CrawlerNotRunningException "Use generic-lens or generic-optics instead"  #-}

-- | There was a version conflict.
_VersionMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_VersionMismatchException
  = Core._MatchServiceError mkServiceConfig
      "VersionMismatchException"
{-# INLINEABLE _VersionMismatchException #-}
{-# DEPRECATED _VersionMismatchException "Use generic-lens or generic-optics instead"  #-}

-- | The machine learning transform is not ready to run.
_MLTransformNotReadyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MLTransformNotReadyException
  = Core._MatchServiceError mkServiceConfig
      "MLTransformNotReadyException"
{-# INLINEABLE _MLTransformNotReadyException #-}
{-# DEPRECATED _MLTransformNotReadyException "Use generic-lens or generic-optics instead"  #-}

-- | A specified entity does not exist
_EntityNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityNotFoundException
  = Core._MatchServiceError mkServiceConfig "EntityNotFoundException"
{-# INLINEABLE _EntityNotFoundException #-}
{-# DEPRECATED _EntityNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Two processes are trying to modify a resource simultaneously.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException
  = Core._MatchServiceError mkServiceConfig
      "ConcurrentModificationException"
{-# INLINEABLE _ConcurrentModificationException #-}
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead"  #-}

-- | The specified scheduler is not running.
_SchedulerNotRunningException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SchedulerNotRunningException
  = Core._MatchServiceError mkServiceConfig
      "SchedulerNotRunningException"
{-# INLINEABLE _SchedulerNotRunningException #-}
{-# DEPRECATED _SchedulerNotRunningException "Use generic-lens or generic-optics instead"  #-}

-- | An internal service error occurred.
_InternalServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceException
  = Core._MatchServiceError mkServiceConfig
      "InternalServiceException"
{-# INLINEABLE _InternalServiceException #-}
{-# DEPRECATED _InternalServiceException "Use generic-lens or generic-optics instead"  #-}

-- | The input provided was not valid.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException
  = Core._MatchServiceError mkServiceConfig "InvalidInputException"
{-# INLINEABLE _InvalidInputException #-}
{-# DEPRECATED _InvalidInputException "Use generic-lens or generic-optics instead"  #-}

-- | A resource numerical limit was exceeded.
_ResourceNumberLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNumberLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNumberLimitExceededException"
{-# INLINEABLE _ResourceNumberLimitExceededException #-}
{-# DEPRECATED _ResourceNumberLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | An encryption operation failed.
_GlueEncryptionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GlueEncryptionException
  = Core._MatchServiceError mkServiceConfig "GlueEncryptionException"
{-# INLINEABLE _GlueEncryptionException #-}
{-# DEPRECATED _GlueEncryptionException "Use generic-lens or generic-optics instead"  #-}

-- | The same unique identifier was associated with two different records.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException
  = Core._MatchServiceError mkServiceConfig
      "IdempotentParameterMismatchException"
{-# INLINEABLE _IdempotentParameterMismatchException #-}
{-# DEPRECATED _IdempotentParameterMismatchException "Use generic-lens or generic-optics instead"  #-}

-- | The specified crawler is stopping.
_CrawlerStoppingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CrawlerStoppingException
  = Core._MatchServiceError mkServiceConfig
      "CrawlerStoppingException"
{-# INLINEABLE _CrawlerStoppingException #-}
{-# DEPRECATED _CrawlerStoppingException "Use generic-lens or generic-optics instead"  #-}

-- | A resource to be created or added already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException
  = Core._MatchServiceError mkServiceConfig "AlreadyExistsException"
{-# INLINEABLE _AlreadyExistsException #-}
{-# DEPRECATED _AlreadyExistsException "Use generic-lens or generic-optics instead"  #-}
