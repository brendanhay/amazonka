-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types
  ( -- * Service configuration
    glueService,

    -- * Errors

    -- * BackfillErrorCode
    BackfillErrorCode (..),

    -- * CSVHeaderOption
    CSVHeaderOption (..),

    -- * CatalogEncryptionMode
    CatalogEncryptionMode (..),

    -- * CloudWatchEncryptionMode
    CloudWatchEncryptionMode (..),

    -- * ColumnStatisticsType
    ColumnStatisticsType (..),

    -- * Comparator
    Comparator (..),

    -- * Compatibility
    Compatibility (..),

    -- * ConnectionPropertyKey
    ConnectionPropertyKey (..),

    -- * ConnectionType
    ConnectionType (..),

    -- * CrawlState
    CrawlState (..),

    -- * CrawlerLineageSettings
    CrawlerLineageSettings (..),

    -- * CrawlerState
    CrawlerState (..),

    -- * DataFormat
    DataFormat (..),

    -- * DeleteBehavior
    DeleteBehavior (..),

    -- * EnableHybridValues
    EnableHybridValues (..),

    -- * ExistCondition
    ExistCondition (..),

    -- * JobBookmarksEncryptionMode
    JobBookmarksEncryptionMode (..),

    -- * JobRunState
    JobRunState (..),

    -- * Language
    Language (..),

    -- * LastCrawlStatus
    LastCrawlStatus (..),

    -- * Logical
    Logical (..),

    -- * LogicalOperator
    LogicalOperator (..),

    -- * MLUserDataEncryptionModeString
    MLUserDataEncryptionModeString (..),

    -- * NodeType
    NodeType (..),

    -- * PartitionIndexStatus
    PartitionIndexStatus (..),

    -- * Permission
    Permission (..),

    -- * PrincipalType
    PrincipalType (..),

    -- * RecrawlBehavior
    RecrawlBehavior (..),

    -- * RegistryStatus
    RegistryStatus (..),

    -- * ResourceShareType
    ResourceShareType (..),

    -- * ResourceType
    ResourceType (..),

    -- * S3EncryptionMode
    S3EncryptionMode (..),

    -- * ScheduleState
    ScheduleState (..),

    -- * SchemaDiffType
    SchemaDiffType (..),

    -- * SchemaStatus
    SchemaStatus (..),

    -- * SchemaVersionStatus
    SchemaVersionStatus (..),

    -- * Sort
    Sort (..),

    -- * SortDirectionType
    SortDirectionType (..),

    -- * TaskRunSortColumnType
    TaskRunSortColumnType (..),

    -- * TaskStatusType
    TaskStatusType (..),

    -- * TaskType
    TaskType (..),

    -- * TransformSortColumnType
    TransformSortColumnType (..),

    -- * TransformStatusType
    TransformStatusType (..),

    -- * TransformType
    TransformType (..),

    -- * TriggerState
    TriggerState (..),

    -- * TriggerType
    TriggerType (..),

    -- * UpdateBehavior
    UpdateBehavior (..),

    -- * WorkerType
    WorkerType (..),

    -- * WorkflowRunStatus
    WorkflowRunStatus (..),

    -- * Action
    Action (..),
    mkAction,
    aNotificationProperty,
    aArguments,
    aJobName,
    aSecurityConfiguration,
    aTimeout,
    aCrawlerName,

    -- * BackfillError
    BackfillError (..),
    mkBackfillError,
    bePartitions,
    beCode,

    -- * BatchStopJobRunError
    BatchStopJobRunError (..),
    mkBatchStopJobRunError,
    bsjreJobName,
    bsjreJobRunId,
    bsjreErrorDetail,

    -- * BatchStopJobRunSuccessfulSubmission
    BatchStopJobRunSuccessfulSubmission (..),
    mkBatchStopJobRunSuccessfulSubmission,
    bsjrssJobName,
    bsjrssJobRunId,

    -- * BatchUpdatePartitionFailureEntry
    BatchUpdatePartitionFailureEntry (..),
    mkBatchUpdatePartitionFailureEntry,
    bupfePartitionValueList,
    bupfeErrorDetail,

    -- * BatchUpdatePartitionRequestEntry
    BatchUpdatePartitionRequestEntry (..),
    mkBatchUpdatePartitionRequestEntry,
    buprePartitionInput,
    buprePartitionValueList,

    -- * BinaryColumnStatisticsData
    BinaryColumnStatisticsData (..),
    mkBinaryColumnStatisticsData,
    bcsdMaximumLength,
    bcsdAverageLength,
    bcsdNumberOfNulls,

    -- * BooleanColumnStatisticsData
    BooleanColumnStatisticsData (..),
    mkBooleanColumnStatisticsData,
    bNumberOfFalses,
    bNumberOfTrues,
    bNumberOfNulls,

    -- * CSVClassifier
    CSVClassifier (..),
    mkCSVClassifier,
    ccCreationTime,
    ccQuoteSymbol,
    ccContainsHeader,
    ccLastUpdated,
    ccDisableValueTrimming,
    ccName,
    ccHeader,
    ccVersion,
    ccAllowSingleColumn,
    ccDelimiter,

    -- * CatalogEntry
    CatalogEntry (..),
    mkCatalogEntry,
    ceDatabaseName,
    ceTableName,

    -- * CatalogImportStatus
    CatalogImportStatus (..),
    mkCatalogImportStatus,
    cisImportedBy,
    cisImportTime,
    cisImportCompleted,

    -- * CatalogTarget
    CatalogTarget (..),
    mkCatalogTarget,
    ctDatabaseName,
    ctTables,

    -- * Classifier
    Classifier (..),
    mkClassifier,
    cGrokClassifier,
    cXMLClassifier,
    cCSVClassifier,
    cJSONClassifier,

    -- * CloudWatchEncryption
    CloudWatchEncryption (..),
    mkCloudWatchEncryption,
    cweCloudWatchEncryptionMode,
    cweKMSKeyARN,

    -- * CodeGenEdge
    CodeGenEdge (..),
    mkCodeGenEdge,
    cgeSource,
    cgeTargetParameter,
    cgeTarget,

    -- * CodeGenNode
    CodeGenNode (..),
    mkCodeGenNode,
    cgnArgs,
    cgnLineNumber,
    cgnId,
    cgnNodeType,

    -- * CodeGenNodeArg
    CodeGenNodeArg (..),
    mkCodeGenNodeArg,
    cgnaValue,
    cgnaName,
    cgnaParam,

    -- * Column
    Column (..),
    mkColumn,
    cName,
    cParameters,
    cType,
    cComment,

    -- * ColumnError
    ColumnError (..),
    mkColumnError,
    ceError,
    ceColumnName,

    -- * ColumnStatistics
    ColumnStatistics (..),
    mkColumnStatistics,
    csAnalyzedTime,
    csColumnType,
    csStatisticsData,
    csColumnName,

    -- * ColumnStatisticsData
    ColumnStatisticsData (..),
    mkColumnStatisticsData,
    csdBinaryColumnStatisticsData,
    csdDateColumnStatisticsData,
    csdBooleanColumnStatisticsData,
    csdDecimalColumnStatisticsData,
    csdDoubleColumnStatisticsData,
    csdStringColumnStatisticsData,
    csdType,
    csdLongColumnStatisticsData,

    -- * ColumnStatisticsError
    ColumnStatisticsError (..),
    mkColumnStatisticsError,
    cseError,
    cseColumnStatistics,

    -- * Condition
    Condition (..),
    mkCondition,
    cCrawlState,
    cState,
    cJobName,
    cLogicalOperator,
    cCrawlerName,

    -- * ConfusionMatrix
    ConfusionMatrix (..),
    mkConfusionMatrix,
    cmNumTrueNegatives,
    cmNumFalseNegatives,
    cmNumTruePositives,
    cmNumFalsePositives,

    -- * Connection
    Connection (..),
    mkConnection,
    cfCreationTime,
    cfLastUpdatedBy,
    cfConnectionProperties,
    cfLastUpdatedTime,
    cfMatchCriteria,
    cfPhysicalConnectionRequirements,
    cfName,
    cfDescription,
    cfConnectionType,

    -- * ConnectionInput
    ConnectionInput (..),
    mkConnectionInput,
    ciConnectionProperties,
    ciMatchCriteria,
    ciPhysicalConnectionRequirements,
    ciName,
    ciDescription,
    ciConnectionType,

    -- * ConnectionPasswordEncryption
    ConnectionPasswordEncryption (..),
    mkConnectionPasswordEncryption,
    cpeReturnConnectionPasswordEncrypted,
    cpeAWSKMSKeyId,

    -- * ConnectionsList
    ConnectionsList (..),
    mkConnectionsList,
    clConnections,

    -- * Crawl
    Crawl (..),
    mkCrawl,
    cfCompletedOn,
    cfState,
    cfStartedOn,
    cfLogStream,
    cfLogGroup,
    cfErrorMessage,

    -- * Crawler
    Crawler (..),
    mkCrawler,
    cgCreationTime,
    cgState,
    cgSchemaChangePolicy,
    cgLastUpdated,
    cgSchedule,
    cgLastCrawl,
    cgCrawlElapsedTime,
    cgRecrawlPolicy,
    cgClassifiers,
    cgRole,
    cgName,
    cgTargets,
    cgVersion,
    cgDatabaseName,
    cgCrawlerSecurityConfiguration,
    cgLineageConfiguration,
    cgConfiguration,
    cgTablePrefix,
    cgDescription,

    -- * CrawlerMetrics
    CrawlerMetrics (..),
    mkCrawlerMetrics,
    cmLastRuntimeSeconds,
    cmTablesCreated,
    cmStillEstimating,
    cmMedianRuntimeSeconds,
    cmTimeLeftSeconds,
    cmTablesDeleted,
    cmTablesUpdated,
    cmCrawlerName,

    -- * CrawlerNodeDetails
    CrawlerNodeDetails (..),
    mkCrawlerNodeDetails,
    cndCrawls,

    -- * CrawlerTargets
    CrawlerTargets (..),
    mkCrawlerTargets,
    ctDynamoDBTargets,
    ctS3Targets,
    ctMongoDBTargets,
    ctCatalogTargets,
    ctJdbcTargets,

    -- * CreateCSVClassifierRequest
    CreateCSVClassifierRequest (..),
    mkCreateCSVClassifierRequest,
    cccrQuoteSymbol,
    cccrContainsHeader,
    cccrDisableValueTrimming,
    cccrName,
    cccrHeader,
    cccrAllowSingleColumn,
    cccrDelimiter,

    -- * CreateGrokClassifierRequest
    CreateGrokClassifierRequest (..),
    mkCreateGrokClassifierRequest,
    cgcrClassification,
    cgcrName,
    cgcrCustomPatterns,
    cgcrGrokPattern,

    -- * CreateJSONClassifierRequest
    CreateJSONClassifierRequest (..),
    mkCreateJSONClassifierRequest,
    cjcrJSONPath,
    cjcrName,

    -- * CreateXMLClassifierRequest
    CreateXMLClassifierRequest (..),
    mkCreateXMLClassifierRequest,
    cxcrClassification,
    cxcrName,
    cxcrRowTag,

    -- * DataCatalogEncryptionSettings
    DataCatalogEncryptionSettings (..),
    mkDataCatalogEncryptionSettings,
    dcesEncryptionAtRest,
    dcesConnectionPasswordEncryption,

    -- * DataLakePrincipal
    DataLakePrincipal (..),
    mkDataLakePrincipal,
    dlpDataLakePrincipalIdentifier,

    -- * Database
    Database (..),
    mkDatabase,
    dLocationURI,
    dCatalogId,
    dTargetDatabase,
    dName,
    dParameters,
    dDescription,
    dCreateTime,
    dCreateTableDefaultPermissions,

    -- * DatabaseIdentifier
    DatabaseIdentifier (..),
    mkDatabaseIdentifier,
    diCatalogId,
    diDatabaseName,

    -- * DatabaseInput
    DatabaseInput (..),
    mkDatabaseInput,
    diLocationURI,
    diTargetDatabase,
    diName,
    diParameters,
    diDescription,
    diCreateTableDefaultPermissions,

    -- * DateColumnStatisticsData
    DateColumnStatisticsData (..),
    mkDateColumnStatisticsData,
    dcsdNumberOfDistinctValues,
    dcsdMaximumValue,
    dcsdNumberOfNulls,
    dcsdMinimumValue,

    -- * DecimalColumnStatisticsData
    DecimalColumnStatisticsData (..),
    mkDecimalColumnStatisticsData,
    dNumberOfDistinctValues,
    dMaximumValue,
    dNumberOfNulls,
    dMinimumValue,

    -- * DecimalNumber
    DecimalNumber (..),
    mkDecimalNumber,
    dnScale,
    dnUnscaledValue,

    -- * DevEndpoint
    DevEndpoint (..),
    mkDevEndpoint,
    deStatus,
    deFailureReason,
    deEndpointName,
    deNumberOfWorkers,
    deExtraPythonLibsS3Path,
    deLastUpdateStatus,
    deSecurityGroupIds,
    deLastModifiedTimestamp,
    dePublicKeys,
    deVPCId,
    deArguments,
    dePrivateAddress,
    deWorkerType,
    deSecurityConfiguration,
    dePublicKey,
    deSubnetId,
    deGlueVersion,
    deNumberOfNodes,
    dePublicAddress,
    deAvailabilityZone,
    deZeppelinRemoteSparkInterpreterPort,
    deExtraJARsS3Path,
    deCreatedTimestamp,
    deYarnEndpointAddress,
    deRoleARN,

    -- * DevEndpointCustomLibraries
    DevEndpointCustomLibraries (..),
    mkDevEndpointCustomLibraries,
    declExtraPythonLibsS3Path,
    declExtraJARsS3Path,

    -- * DoubleColumnStatisticsData
    DoubleColumnStatisticsData (..),
    mkDoubleColumnStatisticsData,
    dcsdfNumberOfDistinctValues,
    dcsdfMaximumValue,
    dcsdfNumberOfNulls,
    dcsdfMinimumValue,

    -- * DynamoDBTarget
    DynamoDBTarget (..),
    mkDynamoDBTarget,
    ddtPath,
    ddtScanRate,
    ddtScanAll,

    -- * Edge
    Edge (..),
    mkEdge,
    eSourceId,
    eDestinationId,

    -- * EncryptionAtRest
    EncryptionAtRest (..),
    mkEncryptionAtRest,
    earSseAWSKMSKeyId,
    earCatalogEncryptionMode,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecS3Encryption,
    ecJobBookmarksEncryption,
    ecCloudWatchEncryption,

    -- * ErrorDetail
    ErrorDetail (..),
    mkErrorDetail,
    edErrorCode,
    edErrorMessage,

    -- * ErrorDetails
    ErrorDetails (..),
    mkErrorDetails,
    eErrorCode,
    eErrorMessage,

    -- * EvaluationMetrics
    EvaluationMetrics (..),
    mkEvaluationMetrics,
    emFindMatchesMetrics,
    emTransformType,

    -- * ExecutionProperty
    ExecutionProperty (..),
    mkExecutionProperty,
    epMaxConcurrentRuns,

    -- * ExportLabelsTaskRunProperties
    ExportLabelsTaskRunProperties (..),
    mkExportLabelsTaskRunProperties,
    eltrpOutputS3Path,

    -- * FindMatchesMetrics
    FindMatchesMetrics (..),
    mkFindMatchesMetrics,
    fmmF1,
    fmmAreaUnderPRCurve,
    fmmRecall,
    fmmPrecision,
    fmmConfusionMatrix,

    -- * FindMatchesParameters
    FindMatchesParameters (..),
    mkFindMatchesParameters,
    fmpEnforceProvidedLabels,
    fmpAccuracyCostTradeoff,
    fmpPrecisionRecallTradeoff,
    fmpPrimaryKeyColumnName,

    -- * FindMatchesTaskRunProperties
    FindMatchesTaskRunProperties (..),
    mkFindMatchesTaskRunProperties,
    fmtrpJobId,
    fmtrpJobName,
    fmtrpJobRunId,

    -- * GetConnectionsFilter
    GetConnectionsFilter (..),
    mkGetConnectionsFilter,
    gcfMatchCriteria,
    gcfConnectionType,

    -- * GluePolicy
    GluePolicy (..),
    mkGluePolicy,
    gpPolicyInJSON,
    gpUpdateTime,
    gpPolicyHash,
    gpCreateTime,

    -- * GlueTable
    GlueTable (..),
    mkGlueTable,
    gCatalogId,
    gConnectionName,
    gDatabaseName,
    gTableName,

    -- * GrokClassifier
    GrokClassifier (..),
    mkGrokClassifier,
    gcCreationTime,
    gcLastUpdated,
    gcClassification,
    gcName,
    gcVersion,
    gcCustomPatterns,
    gcGrokPattern,

    -- * ImportLabelsTaskRunProperties
    ImportLabelsTaskRunProperties (..),
    mkImportLabelsTaskRunProperties,
    iltrpReplace,
    iltrpInputS3Path,

    -- * JSONClassifier
    JSONClassifier (..),
    mkJSONClassifier,
    jsoncCreationTime,
    jsoncLastUpdated,
    jsoncJSONPath,
    jsoncName,
    jsoncVersion,

    -- * JdbcTarget
    JdbcTarget (..),
    mkJdbcTarget,
    jtPath,
    jtConnectionName,
    jtExclusions,

    -- * Job
    Job (..),
    mkJob,
    jNumberOfWorkers,
    jCommand,
    jNotificationProperty,
    jLastModifiedOn,
    jConnections,
    jWorkerType,
    jSecurityConfiguration,
    jGlueVersion,
    jNonOverridableArguments,
    jRole,
    jName,
    jLogURI,
    jMaxRetries,
    jExecutionProperty,
    jAllocatedCapacity,
    jMaxCapacity,
    jTimeout,
    jDefaultArguments,
    jDescription,
    jCreatedOn,

    -- * JobBookmarkEntry
    JobBookmarkEntry (..),
    mkJobBookmarkEntry,
    jbeJobName,
    jbeRun,
    jbeRunId,
    jbeVersion,
    jbePreviousRunId,
    jbeAttempt,
    jbeJobBookmark,

    -- * JobBookmarksEncryption
    JobBookmarksEncryption (..),
    mkJobBookmarksEncryption,
    jbeJobBookmarksEncryptionMode,
    jbeKMSKeyARN,

    -- * JobCommand
    JobCommand (..),
    mkJobCommand,
    jcScriptLocation,
    jcPythonVersion,
    jcName,

    -- * JobNodeDetails
    JobNodeDetails (..),
    mkJobNodeDetails,
    jndJobRuns,

    -- * JobRun
    JobRun (..),
    mkJobRun,
    jrCompletedOn,
    jrNumberOfWorkers,
    jrTriggerName,
    jrNotificationProperty,
    jrLastModifiedOn,
    jrArguments,
    jrJobName,
    jrStartedOn,
    jrWorkerType,
    jrSecurityConfiguration,
    jrGlueVersion,
    jrJobRunState,
    jrLogGroupName,
    jrExecutionTime,
    jrPredecessorRuns,
    jrPreviousRunId,
    jrId,
    jrAttempt,
    jrAllocatedCapacity,
    jrMaxCapacity,
    jrTimeout,
    jrErrorMessage,

    -- * JobUpdate
    JobUpdate (..),
    mkJobUpdate,
    juNumberOfWorkers,
    juCommand,
    juNotificationProperty,
    juConnections,
    juWorkerType,
    juSecurityConfiguration,
    juGlueVersion,
    juNonOverridableArguments,
    juRole,
    juLogURI,
    juMaxRetries,
    juExecutionProperty,
    juAllocatedCapacity,
    juMaxCapacity,
    juTimeout,
    juDefaultArguments,
    juDescription,

    -- * KeySchemaElement
    KeySchemaElement (..),
    mkKeySchemaElement,
    kseName,
    kseType,

    -- * LabelingSetGenerationTaskRunProperties
    LabelingSetGenerationTaskRunProperties (..),
    mkLabelingSetGenerationTaskRunProperties,
    lsgtrpOutputS3Path,

    -- * LastCrawlInfo
    LastCrawlInfo (..),
    mkLastCrawlInfo,
    lciStatus,
    lciStartTime,
    lciLogStream,
    lciLogGroup,
    lciMessagePrefix,
    lciErrorMessage,

    -- * LineageConfiguration
    LineageConfiguration (..),
    mkLineageConfiguration,
    lcCrawlerLineageSettings,

    -- * Location
    Location (..),
    mkLocation,
    lDynamoDB,
    lJdbc,
    lS3,

    -- * LongColumnStatisticsData
    LongColumnStatisticsData (..),
    mkLongColumnStatisticsData,
    lcsdNumberOfDistinctValues,
    lcsdMaximumValue,
    lcsdNumberOfNulls,
    lcsdMinimumValue,

    -- * MLTransform
    MLTransform (..),
    mkMLTransform,
    mltStatus,
    mltNumberOfWorkers,
    mltLastModifiedOn,
    mltLabelCount,
    mltWorkerType,
    mltInputRecordTables,
    mltGlueVersion,
    mltEvaluationMetrics,
    mltSchema,
    mltRole,
    mltName,
    mltParameters,
    mltMaxRetries,
    mltMaxCapacity,
    mltTimeout,
    mltTransformEncryption,
    mltDescription,
    mltCreatedOn,
    mltTransformId,

    -- * MLUserDataEncryption
    MLUserDataEncryption (..),
    mkMLUserDataEncryption,
    mludeMlUserDataEncryptionMode,
    mludeKMSKeyId,

    -- * MappingEntry
    MappingEntry (..),
    mkMappingEntry,
    meTargetTable,
    meSourceType,
    meSourceTable,
    meTargetType,
    meTargetPath,
    meSourcePath,

    -- * MetadataInfo
    MetadataInfo (..),
    mkMetadataInfo,
    miCreatedTime,
    miMetadataValue,

    -- * MetadataKeyValuePair
    MetadataKeyValuePair (..),
    mkMetadataKeyValuePair,
    mkvpMetadataKey,
    mkvpMetadataValue,

    -- * MongoDBTarget
    MongoDBTarget (..),
    mkMongoDBTarget,
    mdtPath,
    mdtConnectionName,
    mdtScanAll,

    -- * Node
    Node (..),
    mkNode,
    nTriggerDetails,
    nUniqueId,
    nCrawlerDetails,
    nName,
    nJobDetails,
    nType,

    -- * NotificationProperty
    NotificationProperty (..),
    mkNotificationProperty,
    npNotifyDelayAfter,

    -- * Order
    Order (..),
    mkOrder,
    oSortOrder,
    oColumn,

    -- * Partition
    Partition (..),
    mkPartition,
    pCreationTime,
    pValues,
    pCatalogId,
    pLastAnalyzedTime,
    pStorageDescriptor,
    pDatabaseName,
    pParameters,
    pLastAccessTime,
    pTableName,

    -- * PartitionError
    PartitionError (..),
    mkPartitionError,
    pePartitionValues,
    peErrorDetail,

    -- * PartitionIndex
    PartitionIndex (..),
    mkPartitionIndex,
    piKeys,
    piIndexName,

    -- * PartitionIndexDescriptor
    PartitionIndexDescriptor (..),
    mkPartitionIndexDescriptor,
    pidIndexStatus,
    pidKeys,
    pidBackfillErrors,
    pidIndexName,

    -- * PartitionInput
    PartitionInput (..),
    mkPartitionInput,
    piValues,
    piLastAnalyzedTime,
    piStorageDescriptor,
    piParameters,
    piLastAccessTime,

    -- * PartitionValueList
    PartitionValueList (..),
    mkPartitionValueList,
    pvlValues,

    -- * PhysicalConnectionRequirements
    PhysicalConnectionRequirements (..),
    mkPhysicalConnectionRequirements,
    pcrSecurityGroupIdList,
    pcrSubnetId,
    pcrAvailabilityZone,

    -- * Predecessor
    Predecessor (..),
    mkPredecessor,
    pJobName,
    pRunId,

    -- * Predicate
    Predicate (..),
    mkPredicate,
    pLogical,
    pConditions,

    -- * PrincipalPermissions
    PrincipalPermissions (..),
    mkPrincipalPermissions,
    ppPrincipal,
    ppPermissions,

    -- * PropertyPredicate
    PropertyPredicate (..),
    mkPropertyPredicate,
    ppValue,
    ppKey,
    ppComparator,

    -- * RecrawlPolicy
    RecrawlPolicy (..),
    mkRecrawlPolicy,
    rpRecrawlBehavior,

    -- * RegistryId
    RegistryId (..),
    mkRegistryId,
    riRegistryName,
    riRegistryARN,

    -- * RegistryListItem
    RegistryListItem (..),
    mkRegistryListItem,
    rliStatus,
    rliRegistryName,
    rliCreatedTime,
    rliRegistryARN,
    rliUpdatedTime,
    rliDescription,

    -- * ResourceURI
    ResourceURI (..),
    mkResourceURI,
    ruResourceType,
    ruURI,

    -- * S3Encryption
    S3Encryption (..),
    mkS3Encryption,
    seS3EncryptionMode,
    seKMSKeyARN,

    -- * S3Target
    S3Target (..),
    mkS3Target,
    stPath,
    stConnectionName,
    stExclusions,

    -- * Schedule
    Schedule (..),
    mkSchedule,
    sState,
    sScheduleExpression,

    -- * SchemaChangePolicy
    SchemaChangePolicy (..),
    mkSchemaChangePolicy,
    scpDeleteBehavior,
    scpUpdateBehavior,

    -- * SchemaColumn
    SchemaColumn (..),
    mkSchemaColumn,
    scName,
    scDataType,

    -- * SchemaId
    SchemaId (..),
    mkSchemaId,
    siRegistryName,
    siSchemaName,
    siSchemaARN,

    -- * SchemaListItem
    SchemaListItem (..),
    mkSchemaListItem,
    sliRegistryName,
    sliCreatedTime,
    sliSchemaStatus,
    sliSchemaName,
    sliSchemaARN,
    sliUpdatedTime,
    sliDescription,

    -- * SchemaReference
    SchemaReference (..),
    mkSchemaReference,
    srSchemaVersionId,
    srSchemaId,
    srSchemaVersionNumber,

    -- * SchemaVersionErrorItem
    SchemaVersionErrorItem (..),
    mkSchemaVersionErrorItem,
    sveiVersionNumber,
    sveiErrorDetails,

    -- * SchemaVersionListItem
    SchemaVersionListItem (..),
    mkSchemaVersionListItem,
    svliStatus,
    svliCreatedTime,
    svliSchemaVersionId,
    svliVersionNumber,
    svliSchemaARN,

    -- * SchemaVersionNumber
    SchemaVersionNumber (..),
    mkSchemaVersionNumber,
    svnVersionNumber,
    svnLatestVersion,

    -- * SecurityConfiguration
    SecurityConfiguration (..),
    mkSecurityConfiguration,
    scfName,
    scfEncryptionConfiguration,
    scfCreatedTimeStamp,

    -- * Segment
    Segment (..),
    mkSegment,
    sTotalSegments,
    sSegmentNumber,

    -- * SerDeInfo
    SerDeInfo (..),
    mkSerDeInfo,
    sdiSerializationLibrary,
    sdiName,
    sdiParameters,

    -- * SkewedInfo
    SkewedInfo (..),
    mkSkewedInfo,
    siSkewedColumnValueLocationMaps,
    siSkewedColumnValues,
    siSkewedColumnNames,

    -- * SortCriterion
    SortCriterion (..),
    mkSortCriterion,
    scSort,
    scFieldName,

    -- * StorageDescriptor
    StorageDescriptor (..),
    mkStorageDescriptor,
    sdSortColumns,
    sdCompressed,
    sdLocation,
    sdBucketColumns,
    sdSerdeInfo,
    sdOutputFormat,
    sdNumberOfBuckets,
    sdSchemaReference,
    sdStoredAsSubDirectories,
    sdParameters,
    sdInputFormat,
    sdSkewedInfo,
    sdColumns,

    -- * StringColumnStatisticsData
    StringColumnStatisticsData (..),
    mkStringColumnStatisticsData,
    scsdMaximumLength,
    scsdAverageLength,
    scsdNumberOfDistinctValues,
    scsdNumberOfNulls,

    -- * Table
    Table (..),
    mkTable,
    tRetention,
    tTargetTable,
    tIsRegisteredWithLakeFormation,
    tCreatedBy,
    tTableType,
    tCatalogId,
    tOwner,
    tViewOriginalText,
    tUpdateTime,
    tViewExpandedText,
    tLastAnalyzedTime,
    tName,
    tStorageDescriptor,
    tDatabaseName,
    tParameters,
    tLastAccessTime,
    tDescription,
    tPartitionKeys,
    tCreateTime,

    -- * TableError
    TableError (..),
    mkTableError,
    teTableName,
    teErrorDetail,

    -- * TableIdentifier
    TableIdentifier (..),
    mkTableIdentifier,
    tiCatalogId,
    tiName,
    tiDatabaseName,

    -- * TableInput
    TableInput (..),
    mkTableInput,
    tifRetention,
    tifTargetTable,
    tifTableType,
    tifOwner,
    tifViewOriginalText,
    tifViewExpandedText,
    tifLastAnalyzedTime,
    tifName,
    tifStorageDescriptor,
    tifParameters,
    tifLastAccessTime,
    tifDescription,
    tifPartitionKeys,

    -- * TableVersion
    TableVersion (..),
    mkTableVersion,
    tvVersionId,
    tvTable,

    -- * TableVersionError
    TableVersionError (..),
    mkTableVersionError,
    tveVersionId,
    tveTableName,
    tveErrorDetail,

    -- * TaskRun
    TaskRun (..),
    mkTaskRun,
    trCompletedOn,
    trStatus,
    trLastModifiedOn,
    trErrorString,
    trStartedOn,
    trLogGroupName,
    trExecutionTime,
    trProperties,
    trTransformId,
    trTaskRunId,

    -- * TaskRunFilterCriteria
    TaskRunFilterCriteria (..),
    mkTaskRunFilterCriteria,
    trfcStatus,
    trfcStartedAfter,
    trfcStartedBefore,
    trfcTaskRunType,

    -- * TaskRunProperties
    TaskRunProperties (..),
    mkTaskRunProperties,
    trpTaskType,
    trpExportLabelsTaskRunProperties,
    trpLabelingSetGenerationTaskRunProperties,
    trpFindMatchesTaskRunProperties,
    trpImportLabelsTaskRunProperties,

    -- * TaskRunSortCriteria
    TaskRunSortCriteria (..),
    mkTaskRunSortCriteria,
    trscSortDirection,
    trscColumn,

    -- * TransformEncryption
    TransformEncryption (..),
    mkTransformEncryption,
    teMlUserDataEncryption,
    teTaskRunSecurityConfigurationName,

    -- * TransformFilterCriteria
    TransformFilterCriteria (..),
    mkTransformFilterCriteria,
    tfcCreatedAfter,
    tfcStatus,
    tfcLastModifiedAfter,
    tfcLastModifiedBefore,
    tfcGlueVersion,
    tfcSchema,
    tfcTransformType,
    tfcName,
    tfcCreatedBefore,

    -- * TransformParameters
    TransformParameters (..),
    mkTransformParameters,
    tpTransformType,
    tpFindMatchesParameters,

    -- * TransformSortCriteria
    TransformSortCriteria (..),
    mkTransformSortCriteria,
    tscSortDirection,
    tscColumn,

    -- * Trigger
    Trigger (..),
    mkTrigger,
    tfWorkflowName,
    tfState,
    tfActions,
    tfSchedule,
    tfPredicate,
    tfName,
    tfId,
    tfType,
    tfDescription,

    -- * TriggerNodeDetails
    TriggerNodeDetails (..),
    mkTriggerNodeDetails,
    tndTrigger,

    -- * TriggerUpdate
    TriggerUpdate (..),
    mkTriggerUpdate,
    tuActions,
    tuSchedule,
    tuPredicate,
    tuName,
    tuDescription,

    -- * UpdateCSVClassifierRequest
    UpdateCSVClassifierRequest (..),
    mkUpdateCSVClassifierRequest,
    uccrQuoteSymbol,
    uccrContainsHeader,
    uccrDisableValueTrimming,
    uccrName,
    uccrHeader,
    uccrAllowSingleColumn,
    uccrDelimiter,

    -- * UpdateGrokClassifierRequest
    UpdateGrokClassifierRequest (..),
    mkUpdateGrokClassifierRequest,
    ugcrClassification,
    ugcrName,
    ugcrCustomPatterns,
    ugcrGrokPattern,

    -- * UpdateJSONClassifierRequest
    UpdateJSONClassifierRequest (..),
    mkUpdateJSONClassifierRequest,
    ujcrJSONPath,
    ujcrName,

    -- * UpdateXMLClassifierRequest
    UpdateXMLClassifierRequest (..),
    mkUpdateXMLClassifierRequest,
    uxcrClassification,
    uxcrName,
    uxcrRowTag,

    -- * UserDefinedFunction
    UserDefinedFunction (..),
    mkUserDefinedFunction,
    udfOwnerName,
    udfCatalogId,
    udfResourceURIs,
    udfDatabaseName,
    udfFunctionName,
    udfOwnerType,
    udfCreateTime,
    udfClassName,

    -- * UserDefinedFunctionInput
    UserDefinedFunctionInput (..),
    mkUserDefinedFunctionInput,
    udfiOwnerName,
    udfiResourceURIs,
    udfiFunctionName,
    udfiOwnerType,
    udfiClassName,

    -- * Workflow
    Workflow (..),
    mkWorkflow,
    wGraph,
    wLastModifiedOn,
    wMaxConcurrentRuns,
    wDefaultRunProperties,
    wName,
    wLastRun,
    wDescription,
    wCreatedOn,

    -- * WorkflowGraph
    WorkflowGraph (..),
    mkWorkflowGraph,
    wgEdges,
    wgNodes,

    -- * WorkflowRun
    WorkflowRun (..),
    mkWorkflowRun,
    wrCompletedOn,
    wrStatus,
    wrGraph,
    wrStartedOn,
    wrWorkflowRunId,
    wrName,
    wrPreviousRunId,
    wrStatistics,
    wrErrorMessage,
    wrWorkflowRunProperties,

    -- * WorkflowRunStatistics
    WorkflowRunStatistics (..),
    mkWorkflowRunStatistics,
    wrsRunningActions,
    wrsStoppedActions,
    wrsTotalActions,
    wrsFailedActions,
    wrsTimeoutActions,
    wrsSucceededActions,

    -- * XMLClassifier
    XMLClassifier (..),
    mkXMLClassifier,
    xcCreationTime,
    xcLastUpdated,
    xcClassification,
    xcName,
    xcVersion,
    xcRowTag,
  )
where

import Network.AWS.Glue.Types.Action
import Network.AWS.Glue.Types.BackfillError
import Network.AWS.Glue.Types.BackfillErrorCode
import Network.AWS.Glue.Types.BatchStopJobRunError
import Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission
import Network.AWS.Glue.Types.BatchUpdatePartitionFailureEntry
import Network.AWS.Glue.Types.BatchUpdatePartitionRequestEntry
import Network.AWS.Glue.Types.BinaryColumnStatisticsData
import Network.AWS.Glue.Types.BooleanColumnStatisticsData
import Network.AWS.Glue.Types.CSVClassifier
import Network.AWS.Glue.Types.CSVHeaderOption
import Network.AWS.Glue.Types.CatalogEncryptionMode
import Network.AWS.Glue.Types.CatalogEntry
import Network.AWS.Glue.Types.CatalogImportStatus
import Network.AWS.Glue.Types.CatalogTarget
import Network.AWS.Glue.Types.Classifier
import Network.AWS.Glue.Types.CloudWatchEncryption
import Network.AWS.Glue.Types.CloudWatchEncryptionMode
import Network.AWS.Glue.Types.CodeGenEdge
import Network.AWS.Glue.Types.CodeGenNode
import Network.AWS.Glue.Types.CodeGenNodeArg
import Network.AWS.Glue.Types.Column
import Network.AWS.Glue.Types.ColumnError
import Network.AWS.Glue.Types.ColumnStatistics
import Network.AWS.Glue.Types.ColumnStatisticsData
import Network.AWS.Glue.Types.ColumnStatisticsError
import Network.AWS.Glue.Types.ColumnStatisticsType
import Network.AWS.Glue.Types.Comparator
import Network.AWS.Glue.Types.Compatibility
import Network.AWS.Glue.Types.Condition
import Network.AWS.Glue.Types.ConfusionMatrix
import Network.AWS.Glue.Types.Connection
import Network.AWS.Glue.Types.ConnectionInput
import Network.AWS.Glue.Types.ConnectionPasswordEncryption
import Network.AWS.Glue.Types.ConnectionPropertyKey
import Network.AWS.Glue.Types.ConnectionType
import Network.AWS.Glue.Types.ConnectionsList
import Network.AWS.Glue.Types.Crawl
import Network.AWS.Glue.Types.CrawlState
import Network.AWS.Glue.Types.Crawler
import Network.AWS.Glue.Types.CrawlerLineageSettings
import Network.AWS.Glue.Types.CrawlerMetrics
import Network.AWS.Glue.Types.CrawlerNodeDetails
import Network.AWS.Glue.Types.CrawlerState
import Network.AWS.Glue.Types.CrawlerTargets
import Network.AWS.Glue.Types.CreateCSVClassifierRequest
import Network.AWS.Glue.Types.CreateGrokClassifierRequest
import Network.AWS.Glue.Types.CreateJSONClassifierRequest
import Network.AWS.Glue.Types.CreateXMLClassifierRequest
import Network.AWS.Glue.Types.DataCatalogEncryptionSettings
import Network.AWS.Glue.Types.DataFormat
import Network.AWS.Glue.Types.DataLakePrincipal
import Network.AWS.Glue.Types.Database
import Network.AWS.Glue.Types.DatabaseIdentifier
import Network.AWS.Glue.Types.DatabaseInput
import Network.AWS.Glue.Types.DateColumnStatisticsData
import Network.AWS.Glue.Types.DecimalColumnStatisticsData
import Network.AWS.Glue.Types.DecimalNumber
import Network.AWS.Glue.Types.DeleteBehavior
import Network.AWS.Glue.Types.DevEndpoint
import Network.AWS.Glue.Types.DevEndpointCustomLibraries
import Network.AWS.Glue.Types.DoubleColumnStatisticsData
import Network.AWS.Glue.Types.DynamoDBTarget
import Network.AWS.Glue.Types.Edge
import Network.AWS.Glue.Types.EnableHybridValues
import Network.AWS.Glue.Types.EncryptionAtRest
import Network.AWS.Glue.Types.EncryptionConfiguration
import Network.AWS.Glue.Types.ErrorDetail
import Network.AWS.Glue.Types.ErrorDetails
import Network.AWS.Glue.Types.EvaluationMetrics
import Network.AWS.Glue.Types.ExecutionProperty
import Network.AWS.Glue.Types.ExistCondition
import Network.AWS.Glue.Types.ExportLabelsTaskRunProperties
import Network.AWS.Glue.Types.FindMatchesMetrics
import Network.AWS.Glue.Types.FindMatchesParameters
import Network.AWS.Glue.Types.FindMatchesTaskRunProperties
import Network.AWS.Glue.Types.GetConnectionsFilter
import Network.AWS.Glue.Types.GluePolicy
import Network.AWS.Glue.Types.GlueTable
import Network.AWS.Glue.Types.GrokClassifier
import Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
import Network.AWS.Glue.Types.JSONClassifier
import Network.AWS.Glue.Types.JdbcTarget
import Network.AWS.Glue.Types.Job
import Network.AWS.Glue.Types.JobBookmarkEntry
import Network.AWS.Glue.Types.JobBookmarksEncryption
import Network.AWS.Glue.Types.JobBookmarksEncryptionMode
import Network.AWS.Glue.Types.JobCommand
import Network.AWS.Glue.Types.JobNodeDetails
import Network.AWS.Glue.Types.JobRun
import Network.AWS.Glue.Types.JobRunState
import Network.AWS.Glue.Types.JobUpdate
import Network.AWS.Glue.Types.KeySchemaElement
import Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties
import Network.AWS.Glue.Types.Language
import Network.AWS.Glue.Types.LastCrawlInfo
import Network.AWS.Glue.Types.LastCrawlStatus
import Network.AWS.Glue.Types.LineageConfiguration
import Network.AWS.Glue.Types.Location
import Network.AWS.Glue.Types.Logical
import Network.AWS.Glue.Types.LogicalOperator
import Network.AWS.Glue.Types.LongColumnStatisticsData
import Network.AWS.Glue.Types.MLTransform
import Network.AWS.Glue.Types.MLUserDataEncryption
import Network.AWS.Glue.Types.MLUserDataEncryptionModeString
import Network.AWS.Glue.Types.MappingEntry
import Network.AWS.Glue.Types.MetadataInfo
import Network.AWS.Glue.Types.MetadataKeyValuePair
import Network.AWS.Glue.Types.MongoDBTarget
import Network.AWS.Glue.Types.Node
import Network.AWS.Glue.Types.NodeType
import Network.AWS.Glue.Types.NotificationProperty
import Network.AWS.Glue.Types.Order
import Network.AWS.Glue.Types.Partition
import Network.AWS.Glue.Types.PartitionError
import Network.AWS.Glue.Types.PartitionIndex
import Network.AWS.Glue.Types.PartitionIndexDescriptor
import Network.AWS.Glue.Types.PartitionIndexStatus
import Network.AWS.Glue.Types.PartitionInput
import Network.AWS.Glue.Types.PartitionValueList
import Network.AWS.Glue.Types.Permission
import Network.AWS.Glue.Types.PhysicalConnectionRequirements
import Network.AWS.Glue.Types.Predecessor
import Network.AWS.Glue.Types.Predicate
import Network.AWS.Glue.Types.PrincipalPermissions
import Network.AWS.Glue.Types.PrincipalType
import Network.AWS.Glue.Types.PropertyPredicate
import Network.AWS.Glue.Types.RecrawlBehavior
import Network.AWS.Glue.Types.RecrawlPolicy
import Network.AWS.Glue.Types.RegistryId
import Network.AWS.Glue.Types.RegistryListItem
import Network.AWS.Glue.Types.RegistryStatus
import Network.AWS.Glue.Types.ResourceShareType
import Network.AWS.Glue.Types.ResourceType
import Network.AWS.Glue.Types.ResourceURI
import Network.AWS.Glue.Types.S3Encryption
import Network.AWS.Glue.Types.S3EncryptionMode
import Network.AWS.Glue.Types.S3Target
import Network.AWS.Glue.Types.Schedule
import Network.AWS.Glue.Types.ScheduleState
import Network.AWS.Glue.Types.SchemaChangePolicy
import Network.AWS.Glue.Types.SchemaColumn
import Network.AWS.Glue.Types.SchemaDiffType
import Network.AWS.Glue.Types.SchemaId
import Network.AWS.Glue.Types.SchemaListItem
import Network.AWS.Glue.Types.SchemaReference
import Network.AWS.Glue.Types.SchemaStatus
import Network.AWS.Glue.Types.SchemaVersionErrorItem
import Network.AWS.Glue.Types.SchemaVersionListItem
import Network.AWS.Glue.Types.SchemaVersionNumber
import Network.AWS.Glue.Types.SchemaVersionStatus
import Network.AWS.Glue.Types.SecurityConfiguration
import Network.AWS.Glue.Types.Segment
import Network.AWS.Glue.Types.SerDeInfo
import Network.AWS.Glue.Types.SkewedInfo
import Network.AWS.Glue.Types.Sort
import Network.AWS.Glue.Types.SortCriterion
import Network.AWS.Glue.Types.SortDirectionType
import Network.AWS.Glue.Types.StorageDescriptor
import Network.AWS.Glue.Types.StringColumnStatisticsData
import Network.AWS.Glue.Types.Table
import Network.AWS.Glue.Types.TableError
import Network.AWS.Glue.Types.TableIdentifier
import Network.AWS.Glue.Types.TableInput
import Network.AWS.Glue.Types.TableVersion
import Network.AWS.Glue.Types.TableVersionError
import Network.AWS.Glue.Types.TaskRun
import Network.AWS.Glue.Types.TaskRunFilterCriteria
import Network.AWS.Glue.Types.TaskRunProperties
import Network.AWS.Glue.Types.TaskRunSortColumnType
import Network.AWS.Glue.Types.TaskRunSortCriteria
import Network.AWS.Glue.Types.TaskStatusType
import Network.AWS.Glue.Types.TaskType
import Network.AWS.Glue.Types.TransformEncryption
import Network.AWS.Glue.Types.TransformFilterCriteria
import Network.AWS.Glue.Types.TransformParameters
import Network.AWS.Glue.Types.TransformSortColumnType
import Network.AWS.Glue.Types.TransformSortCriteria
import Network.AWS.Glue.Types.TransformStatusType
import Network.AWS.Glue.Types.TransformType
import Network.AWS.Glue.Types.Trigger
import Network.AWS.Glue.Types.TriggerNodeDetails
import Network.AWS.Glue.Types.TriggerState
import Network.AWS.Glue.Types.TriggerType
import Network.AWS.Glue.Types.TriggerUpdate
import Network.AWS.Glue.Types.UpdateBehavior
import Network.AWS.Glue.Types.UpdateCSVClassifierRequest
import Network.AWS.Glue.Types.UpdateGrokClassifierRequest
import Network.AWS.Glue.Types.UpdateJSONClassifierRequest
import Network.AWS.Glue.Types.UpdateXMLClassifierRequest
import Network.AWS.Glue.Types.UserDefinedFunction
import Network.AWS.Glue.Types.UserDefinedFunctionInput
import Network.AWS.Glue.Types.WorkerType
import Network.AWS.Glue.Types.Workflow
import Network.AWS.Glue.Types.WorkflowGraph
import Network.AWS.Glue.Types.WorkflowRun
import Network.AWS.Glue.Types.WorkflowRunStatistics
import Network.AWS.Glue.Types.WorkflowRunStatus
import Network.AWS.Glue.Types.XMLClassifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-03-31@ of the Amazon Glue SDK configuration.
glueService :: Lude.Service
glueService =
  Lude.Service
    { Lude._svcAbbrev = "Glue",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "glue",
      Lude._svcVersion = "2017-03-31",
      Lude._svcEndpoint = Lude.defaultEndpoint glueService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Glue",
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
