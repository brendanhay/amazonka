{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _CrawlerRunningException,
    _SchedulerTransitioningException,
    _SchedulerRunningException,
    _ConditionCheckFailureException,
    _ConcurrentRunsExceededException,
    _IllegalWorkflowStateException,
    _NoScheduleException,
    _OperationTimeoutException,
    _ConflictException,
    _CrawlerNotRunningException,
    _VersionMismatchException,
    _MLTransformNotReadyException,
    _EntityNotFoundException,
    _ConcurrentModificationException,
    _SchedulerNotRunningException,
    _InternalServiceException,
    _InvalidInputException,
    _ResourceNumberLimitExceededException,
    _GlueEncryptionException,
    _IdempotentParameterMismatchException,
    _CrawlerStoppingException,
    _IllegalBlueprintStateException,
    _AlreadyExistsException,

    -- * BackfillErrorCode
    BackfillErrorCode (..),

    -- * BlueprintRunState
    BlueprintRunState (..),

    -- * BlueprintStatus
    BlueprintStatus (..),

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

    -- * CsvHeaderOption
    CsvHeaderOption (..),

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
    newAction,
    action_notificationProperty,
    action_arguments,
    action_jobName,
    action_securityConfiguration,
    action_timeout,
    action_crawlerName,

    -- * BackfillError
    BackfillError (..),
    newBackfillError,
    backfillError_partitions,
    backfillError_code,

    -- * BatchStopJobRunError
    BatchStopJobRunError (..),
    newBatchStopJobRunError,
    batchStopJobRunError_jobName,
    batchStopJobRunError_jobRunId,
    batchStopJobRunError_errorDetail,

    -- * BatchStopJobRunSuccessfulSubmission
    BatchStopJobRunSuccessfulSubmission (..),
    newBatchStopJobRunSuccessfulSubmission,
    batchStopJobRunSuccessfulSubmission_jobName,
    batchStopJobRunSuccessfulSubmission_jobRunId,

    -- * BatchUpdatePartitionFailureEntry
    BatchUpdatePartitionFailureEntry (..),
    newBatchUpdatePartitionFailureEntry,
    batchUpdatePartitionFailureEntry_partitionValueList,
    batchUpdatePartitionFailureEntry_errorDetail,

    -- * BatchUpdatePartitionRequestEntry
    BatchUpdatePartitionRequestEntry (..),
    newBatchUpdatePartitionRequestEntry,
    batchUpdatePartitionRequestEntry_partitionValueList,
    batchUpdatePartitionRequestEntry_partitionInput,

    -- * BinaryColumnStatisticsData
    BinaryColumnStatisticsData (..),
    newBinaryColumnStatisticsData,
    binaryColumnStatisticsData_maximumLength,
    binaryColumnStatisticsData_averageLength,
    binaryColumnStatisticsData_numberOfNulls,

    -- * Blueprint
    Blueprint (..),
    newBlueprint,
    blueprint_status,
    blueprint_parameterSpec,
    blueprint_blueprintLocation,
    blueprint_lastModifiedOn,
    blueprint_lastActiveDefinition,
    blueprint_name,
    blueprint_blueprintServiceLocation,
    blueprint_errorMessage,
    blueprint_description,
    blueprint_createdOn,

    -- * BlueprintDetails
    BlueprintDetails (..),
    newBlueprintDetails,
    blueprintDetails_runId,
    blueprintDetails_blueprintName,

    -- * BlueprintRun
    BlueprintRun (..),
    newBlueprintRun,
    blueprintRun_workflowName,
    blueprintRun_completedOn,
    blueprintRun_state,
    blueprintRun_rollbackErrorMessage,
    blueprintRun_startedOn,
    blueprintRun_runId,
    blueprintRun_parameters,
    blueprintRun_blueprintName,
    blueprintRun_errorMessage,
    blueprintRun_roleArn,

    -- * BooleanColumnStatisticsData
    BooleanColumnStatisticsData (..),
    newBooleanColumnStatisticsData,
    booleanColumnStatisticsData_numberOfTrues,
    booleanColumnStatisticsData_numberOfFalses,
    booleanColumnStatisticsData_numberOfNulls,

    -- * CatalogEntry
    CatalogEntry (..),
    newCatalogEntry,
    catalogEntry_databaseName,
    catalogEntry_tableName,

    -- * CatalogImportStatus
    CatalogImportStatus (..),
    newCatalogImportStatus,
    catalogImportStatus_importedBy,
    catalogImportStatus_importTime,
    catalogImportStatus_importCompleted,

    -- * CatalogTarget
    CatalogTarget (..),
    newCatalogTarget,
    catalogTarget_databaseName,
    catalogTarget_tables,

    -- * Classifier
    Classifier (..),
    newClassifier,
    classifier_grokClassifier,
    classifier_xMLClassifier,
    classifier_csvClassifier,
    classifier_jsonClassifier,

    -- * CloudWatchEncryption
    CloudWatchEncryption (..),
    newCloudWatchEncryption,
    cloudWatchEncryption_cloudWatchEncryptionMode,
    cloudWatchEncryption_kmsKeyArn,

    -- * CodeGenEdge
    CodeGenEdge (..),
    newCodeGenEdge,
    codeGenEdge_targetParameter,
    codeGenEdge_source,
    codeGenEdge_target,

    -- * CodeGenNode
    CodeGenNode (..),
    newCodeGenNode,
    codeGenNode_lineNumber,
    codeGenNode_id,
    codeGenNode_nodeType,
    codeGenNode_args,

    -- * CodeGenNodeArg
    CodeGenNodeArg (..),
    newCodeGenNodeArg,
    codeGenNodeArg_param,
    codeGenNodeArg_name,
    codeGenNodeArg_value,

    -- * Column
    Column (..),
    newColumn,
    column_parameters,
    column_type,
    column_comment,
    column_name,

    -- * ColumnError
    ColumnError (..),
    newColumnError,
    columnError_error,
    columnError_columnName,

    -- * ColumnImportance
    ColumnImportance (..),
    newColumnImportance,
    columnImportance_importance,
    columnImportance_columnName,

    -- * ColumnStatistics
    ColumnStatistics (..),
    newColumnStatistics,
    columnStatistics_columnName,
    columnStatistics_columnType,
    columnStatistics_analyzedTime,
    columnStatistics_statisticsData,

    -- * ColumnStatisticsData
    ColumnStatisticsData (..),
    newColumnStatisticsData,
    columnStatisticsData_binaryColumnStatisticsData,
    columnStatisticsData_dateColumnStatisticsData,
    columnStatisticsData_booleanColumnStatisticsData,
    columnStatisticsData_decimalColumnStatisticsData,
    columnStatisticsData_doubleColumnStatisticsData,
    columnStatisticsData_stringColumnStatisticsData,
    columnStatisticsData_longColumnStatisticsData,
    columnStatisticsData_type,

    -- * ColumnStatisticsError
    ColumnStatisticsError (..),
    newColumnStatisticsError,
    columnStatisticsError_error,
    columnStatisticsError_columnStatistics,

    -- * Condition
    Condition (..),
    newCondition,
    condition_crawlState,
    condition_state,
    condition_jobName,
    condition_logicalOperator,
    condition_crawlerName,

    -- * ConfusionMatrix
    ConfusionMatrix (..),
    newConfusionMatrix,
    confusionMatrix_numTrueNegatives,
    confusionMatrix_numFalseNegatives,
    confusionMatrix_numTruePositives,
    confusionMatrix_numFalsePositives,

    -- * Connection
    Connection (..),
    newConnection,
    connection_creationTime,
    connection_lastUpdatedBy,
    connection_connectionProperties,
    connection_lastUpdatedTime,
    connection_matchCriteria,
    connection_physicalConnectionRequirements,
    connection_name,
    connection_description,
    connection_connectionType,

    -- * ConnectionInput
    ConnectionInput (..),
    newConnectionInput,
    connectionInput_matchCriteria,
    connectionInput_physicalConnectionRequirements,
    connectionInput_description,
    connectionInput_name,
    connectionInput_connectionType,
    connectionInput_connectionProperties,

    -- * ConnectionPasswordEncryption
    ConnectionPasswordEncryption (..),
    newConnectionPasswordEncryption,
    connectionPasswordEncryption_awsKmsKeyId,
    connectionPasswordEncryption_returnConnectionPasswordEncrypted,

    -- * ConnectionsList
    ConnectionsList (..),
    newConnectionsList,
    connectionsList_connections,

    -- * Crawl
    Crawl (..),
    newCrawl,
    crawl_completedOn,
    crawl_state,
    crawl_startedOn,
    crawl_logStream,
    crawl_logGroup,
    crawl_errorMessage,

    -- * Crawler
    Crawler (..),
    newCrawler,
    crawler_creationTime,
    crawler_state,
    crawler_schemaChangePolicy,
    crawler_lastUpdated,
    crawler_schedule,
    crawler_lastCrawl,
    crawler_crawlElapsedTime,
    crawler_recrawlPolicy,
    crawler_classifiers,
    crawler_role,
    crawler_name,
    crawler_targets,
    crawler_version,
    crawler_databaseName,
    crawler_crawlerSecurityConfiguration,
    crawler_lineageConfiguration,
    crawler_configuration,
    crawler_tablePrefix,
    crawler_description,

    -- * CrawlerMetrics
    CrawlerMetrics (..),
    newCrawlerMetrics,
    crawlerMetrics_lastRuntimeSeconds,
    crawlerMetrics_tablesCreated,
    crawlerMetrics_stillEstimating,
    crawlerMetrics_medianRuntimeSeconds,
    crawlerMetrics_timeLeftSeconds,
    crawlerMetrics_tablesDeleted,
    crawlerMetrics_tablesUpdated,
    crawlerMetrics_crawlerName,

    -- * CrawlerNodeDetails
    CrawlerNodeDetails (..),
    newCrawlerNodeDetails,
    crawlerNodeDetails_crawls,

    -- * CrawlerTargets
    CrawlerTargets (..),
    newCrawlerTargets,
    crawlerTargets_dynamoDBTargets,
    crawlerTargets_s3Targets,
    crawlerTargets_mongoDBTargets,
    crawlerTargets_catalogTargets,
    crawlerTargets_jdbcTargets,

    -- * CreateCsvClassifierRequest
    CreateCsvClassifierRequest (..),
    newCreateCsvClassifierRequest,
    createCsvClassifierRequest_quoteSymbol,
    createCsvClassifierRequest_containsHeader,
    createCsvClassifierRequest_disableValueTrimming,
    createCsvClassifierRequest_header,
    createCsvClassifierRequest_allowSingleColumn,
    createCsvClassifierRequest_delimiter,
    createCsvClassifierRequest_name,

    -- * CreateGrokClassifierRequest
    CreateGrokClassifierRequest (..),
    newCreateGrokClassifierRequest,
    createGrokClassifierRequest_customPatterns,
    createGrokClassifierRequest_classification,
    createGrokClassifierRequest_name,
    createGrokClassifierRequest_grokPattern,

    -- * CreateJsonClassifierRequest
    CreateJsonClassifierRequest (..),
    newCreateJsonClassifierRequest,
    createJsonClassifierRequest_name,
    createJsonClassifierRequest_jsonPath,

    -- * CreateXMLClassifierRequest
    CreateXMLClassifierRequest (..),
    newCreateXMLClassifierRequest,
    createXMLClassifierRequest_rowTag,
    createXMLClassifierRequest_classification,
    createXMLClassifierRequest_name,

    -- * CsvClassifier
    CsvClassifier (..),
    newCsvClassifier,
    csvClassifier_creationTime,
    csvClassifier_quoteSymbol,
    csvClassifier_containsHeader,
    csvClassifier_lastUpdated,
    csvClassifier_disableValueTrimming,
    csvClassifier_header,
    csvClassifier_version,
    csvClassifier_allowSingleColumn,
    csvClassifier_delimiter,
    csvClassifier_name,

    -- * DataCatalogEncryptionSettings
    DataCatalogEncryptionSettings (..),
    newDataCatalogEncryptionSettings,
    dataCatalogEncryptionSettings_encryptionAtRest,
    dataCatalogEncryptionSettings_connectionPasswordEncryption,

    -- * DataLakePrincipal
    DataLakePrincipal (..),
    newDataLakePrincipal,
    dataLakePrincipal_dataLakePrincipalIdentifier,

    -- * Database
    Database (..),
    newDatabase,
    database_locationUri,
    database_catalogId,
    database_targetDatabase,
    database_parameters,
    database_description,
    database_createTime,
    database_createTableDefaultPermissions,
    database_name,

    -- * DatabaseIdentifier
    DatabaseIdentifier (..),
    newDatabaseIdentifier,
    databaseIdentifier_catalogId,
    databaseIdentifier_databaseName,

    -- * DatabaseInput
    DatabaseInput (..),
    newDatabaseInput,
    databaseInput_locationUri,
    databaseInput_targetDatabase,
    databaseInput_parameters,
    databaseInput_description,
    databaseInput_createTableDefaultPermissions,
    databaseInput_name,

    -- * DateColumnStatisticsData
    DateColumnStatisticsData (..),
    newDateColumnStatisticsData,
    dateColumnStatisticsData_maximumValue,
    dateColumnStatisticsData_minimumValue,
    dateColumnStatisticsData_numberOfNulls,
    dateColumnStatisticsData_numberOfDistinctValues,

    -- * DecimalColumnStatisticsData
    DecimalColumnStatisticsData (..),
    newDecimalColumnStatisticsData,
    decimalColumnStatisticsData_maximumValue,
    decimalColumnStatisticsData_minimumValue,
    decimalColumnStatisticsData_numberOfNulls,
    decimalColumnStatisticsData_numberOfDistinctValues,

    -- * DecimalNumber
    DecimalNumber (..),
    newDecimalNumber,
    decimalNumber_unscaledValue,
    decimalNumber_scale,

    -- * DevEndpoint
    DevEndpoint (..),
    newDevEndpoint,
    devEndpoint_status,
    devEndpoint_failureReason,
    devEndpoint_endpointName,
    devEndpoint_numberOfWorkers,
    devEndpoint_extraPythonLibsS3Path,
    devEndpoint_lastUpdateStatus,
    devEndpoint_securityGroupIds,
    devEndpoint_lastModifiedTimestamp,
    devEndpoint_publicKeys,
    devEndpoint_vpcId,
    devEndpoint_arguments,
    devEndpoint_privateAddress,
    devEndpoint_workerType,
    devEndpoint_securityConfiguration,
    devEndpoint_publicKey,
    devEndpoint_subnetId,
    devEndpoint_glueVersion,
    devEndpoint_numberOfNodes,
    devEndpoint_publicAddress,
    devEndpoint_availabilityZone,
    devEndpoint_zeppelinRemoteSparkInterpreterPort,
    devEndpoint_extraJarsS3Path,
    devEndpoint_createdTimestamp,
    devEndpoint_yarnEndpointAddress,
    devEndpoint_roleArn,

    -- * DevEndpointCustomLibraries
    DevEndpointCustomLibraries (..),
    newDevEndpointCustomLibraries,
    devEndpointCustomLibraries_extraPythonLibsS3Path,
    devEndpointCustomLibraries_extraJarsS3Path,

    -- * DoubleColumnStatisticsData
    DoubleColumnStatisticsData (..),
    newDoubleColumnStatisticsData,
    doubleColumnStatisticsData_maximumValue,
    doubleColumnStatisticsData_minimumValue,
    doubleColumnStatisticsData_numberOfNulls,
    doubleColumnStatisticsData_numberOfDistinctValues,

    -- * DynamoDBTarget
    DynamoDBTarget (..),
    newDynamoDBTarget,
    dynamoDBTarget_path,
    dynamoDBTarget_scanRate,
    dynamoDBTarget_scanAll,

    -- * Edge
    Edge (..),
    newEdge,
    edge_sourceId,
    edge_destinationId,

    -- * EncryptionAtRest
    EncryptionAtRest (..),
    newEncryptionAtRest,
    encryptionAtRest_sseAwsKmsKeyId,
    encryptionAtRest_catalogEncryptionMode,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    newEncryptionConfiguration,
    encryptionConfiguration_s3Encryption,
    encryptionConfiguration_jobBookmarksEncryption,
    encryptionConfiguration_cloudWatchEncryption,

    -- * ErrorDetail
    ErrorDetail (..),
    newErrorDetail,
    errorDetail_errorCode,
    errorDetail_errorMessage,

    -- * ErrorDetails
    ErrorDetails (..),
    newErrorDetails,
    errorDetails_errorCode,
    errorDetails_errorMessage,

    -- * EvaluationMetrics
    EvaluationMetrics (..),
    newEvaluationMetrics,
    evaluationMetrics_findMatchesMetrics,
    evaluationMetrics_transformType,

    -- * EventBatchingCondition
    EventBatchingCondition (..),
    newEventBatchingCondition,
    eventBatchingCondition_batchWindow,
    eventBatchingCondition_batchSize,

    -- * ExecutionProperty
    ExecutionProperty (..),
    newExecutionProperty,
    executionProperty_maxConcurrentRuns,

    -- * ExportLabelsTaskRunProperties
    ExportLabelsTaskRunProperties (..),
    newExportLabelsTaskRunProperties,
    exportLabelsTaskRunProperties_outputS3Path,

    -- * FindMatchesMetrics
    FindMatchesMetrics (..),
    newFindMatchesMetrics,
    findMatchesMetrics_f1,
    findMatchesMetrics_areaUnderPRCurve,
    findMatchesMetrics_recall,
    findMatchesMetrics_precision,
    findMatchesMetrics_columnImportances,
    findMatchesMetrics_confusionMatrix,

    -- * FindMatchesParameters
    FindMatchesParameters (..),
    newFindMatchesParameters,
    findMatchesParameters_enforceProvidedLabels,
    findMatchesParameters_accuracyCostTradeoff,
    findMatchesParameters_precisionRecallTradeoff,
    findMatchesParameters_primaryKeyColumnName,

    -- * FindMatchesTaskRunProperties
    FindMatchesTaskRunProperties (..),
    newFindMatchesTaskRunProperties,
    findMatchesTaskRunProperties_jobId,
    findMatchesTaskRunProperties_jobName,
    findMatchesTaskRunProperties_jobRunId,

    -- * GetConnectionsFilter
    GetConnectionsFilter (..),
    newGetConnectionsFilter,
    getConnectionsFilter_matchCriteria,
    getConnectionsFilter_connectionType,

    -- * GluePolicy
    GluePolicy (..),
    newGluePolicy,
    gluePolicy_policyInJson,
    gluePolicy_updateTime,
    gluePolicy_policyHash,
    gluePolicy_createTime,

    -- * GlueTable
    GlueTable (..),
    newGlueTable,
    glueTable_catalogId,
    glueTable_connectionName,
    glueTable_databaseName,
    glueTable_tableName,

    -- * GrokClassifier
    GrokClassifier (..),
    newGrokClassifier,
    grokClassifier_creationTime,
    grokClassifier_lastUpdated,
    grokClassifier_version,
    grokClassifier_customPatterns,
    grokClassifier_name,
    grokClassifier_classification,
    grokClassifier_grokPattern,

    -- * ImportLabelsTaskRunProperties
    ImportLabelsTaskRunProperties (..),
    newImportLabelsTaskRunProperties,
    importLabelsTaskRunProperties_replace,
    importLabelsTaskRunProperties_inputS3Path,

    -- * JdbcTarget
    JdbcTarget (..),
    newJdbcTarget,
    jdbcTarget_path,
    jdbcTarget_connectionName,
    jdbcTarget_exclusions,

    -- * Job
    Job (..),
    newJob,
    job_numberOfWorkers,
    job_command,
    job_notificationProperty,
    job_lastModifiedOn,
    job_connections,
    job_workerType,
    job_securityConfiguration,
    job_glueVersion,
    job_nonOverridableArguments,
    job_role,
    job_name,
    job_logUri,
    job_maxRetries,
    job_executionProperty,
    job_allocatedCapacity,
    job_maxCapacity,
    job_timeout,
    job_defaultArguments,
    job_description,
    job_createdOn,

    -- * JobBookmarkEntry
    JobBookmarkEntry (..),
    newJobBookmarkEntry,
    jobBookmarkEntry_jobName,
    jobBookmarkEntry_run,
    jobBookmarkEntry_runId,
    jobBookmarkEntry_version,
    jobBookmarkEntry_previousRunId,
    jobBookmarkEntry_attempt,
    jobBookmarkEntry_jobBookmark,

    -- * JobBookmarksEncryption
    JobBookmarksEncryption (..),
    newJobBookmarksEncryption,
    jobBookmarksEncryption_jobBookmarksEncryptionMode,
    jobBookmarksEncryption_kmsKeyArn,

    -- * JobCommand
    JobCommand (..),
    newJobCommand,
    jobCommand_scriptLocation,
    jobCommand_pythonVersion,
    jobCommand_name,

    -- * JobNodeDetails
    JobNodeDetails (..),
    newJobNodeDetails,
    jobNodeDetails_jobRuns,

    -- * JobRun
    JobRun (..),
    newJobRun,
    jobRun_completedOn,
    jobRun_numberOfWorkers,
    jobRun_triggerName,
    jobRun_notificationProperty,
    jobRun_lastModifiedOn,
    jobRun_arguments,
    jobRun_jobName,
    jobRun_startedOn,
    jobRun_workerType,
    jobRun_securityConfiguration,
    jobRun_glueVersion,
    jobRun_jobRunState,
    jobRun_logGroupName,
    jobRun_executionTime,
    jobRun_predecessorRuns,
    jobRun_previousRunId,
    jobRun_id,
    jobRun_attempt,
    jobRun_allocatedCapacity,
    jobRun_maxCapacity,
    jobRun_timeout,
    jobRun_errorMessage,

    -- * JobUpdate
    JobUpdate (..),
    newJobUpdate,
    jobUpdate_numberOfWorkers,
    jobUpdate_command,
    jobUpdate_notificationProperty,
    jobUpdate_connections,
    jobUpdate_workerType,
    jobUpdate_securityConfiguration,
    jobUpdate_glueVersion,
    jobUpdate_nonOverridableArguments,
    jobUpdate_role,
    jobUpdate_logUri,
    jobUpdate_maxRetries,
    jobUpdate_executionProperty,
    jobUpdate_allocatedCapacity,
    jobUpdate_maxCapacity,
    jobUpdate_timeout,
    jobUpdate_defaultArguments,
    jobUpdate_description,

    -- * JsonClassifier
    JsonClassifier (..),
    newJsonClassifier,
    jsonClassifier_creationTime,
    jsonClassifier_lastUpdated,
    jsonClassifier_version,
    jsonClassifier_name,
    jsonClassifier_jsonPath,

    -- * KeySchemaElement
    KeySchemaElement (..),
    newKeySchemaElement,
    keySchemaElement_name,
    keySchemaElement_type,

    -- * LabelingSetGenerationTaskRunProperties
    LabelingSetGenerationTaskRunProperties (..),
    newLabelingSetGenerationTaskRunProperties,
    labelingSetGenerationTaskRunProperties_outputS3Path,

    -- * LastActiveDefinition
    LastActiveDefinition (..),
    newLastActiveDefinition,
    lastActiveDefinition_parameterSpec,
    lastActiveDefinition_blueprintLocation,
    lastActiveDefinition_lastModifiedOn,
    lastActiveDefinition_blueprintServiceLocation,
    lastActiveDefinition_description,

    -- * LastCrawlInfo
    LastCrawlInfo (..),
    newLastCrawlInfo,
    lastCrawlInfo_status,
    lastCrawlInfo_startTime,
    lastCrawlInfo_logStream,
    lastCrawlInfo_logGroup,
    lastCrawlInfo_messagePrefix,
    lastCrawlInfo_errorMessage,

    -- * LineageConfiguration
    LineageConfiguration (..),
    newLineageConfiguration,
    lineageConfiguration_crawlerLineageSettings,

    -- * Location
    Location (..),
    newLocation,
    location_dynamoDB,
    location_jdbc,
    location_s3,

    -- * LongColumnStatisticsData
    LongColumnStatisticsData (..),
    newLongColumnStatisticsData,
    longColumnStatisticsData_maximumValue,
    longColumnStatisticsData_minimumValue,
    longColumnStatisticsData_numberOfNulls,
    longColumnStatisticsData_numberOfDistinctValues,

    -- * MLTransform
    MLTransform (..),
    newMLTransform,
    mLTransform_status,
    mLTransform_numberOfWorkers,
    mLTransform_lastModifiedOn,
    mLTransform_labelCount,
    mLTransform_workerType,
    mLTransform_inputRecordTables,
    mLTransform_glueVersion,
    mLTransform_evaluationMetrics,
    mLTransform_schema,
    mLTransform_role,
    mLTransform_name,
    mLTransform_parameters,
    mLTransform_maxRetries,
    mLTransform_maxCapacity,
    mLTransform_timeout,
    mLTransform_transformEncryption,
    mLTransform_description,
    mLTransform_createdOn,
    mLTransform_transformId,

    -- * MLUserDataEncryption
    MLUserDataEncryption (..),
    newMLUserDataEncryption,
    mLUserDataEncryption_kmsKeyId,
    mLUserDataEncryption_mlUserDataEncryptionMode,

    -- * MappingEntry
    MappingEntry (..),
    newMappingEntry,
    mappingEntry_targetTable,
    mappingEntry_sourceType,
    mappingEntry_sourceTable,
    mappingEntry_targetType,
    mappingEntry_targetPath,
    mappingEntry_sourcePath,

    -- * MetadataInfo
    MetadataInfo (..),
    newMetadataInfo,
    metadataInfo_createdTime,
    metadataInfo_otherMetadataValueList,
    metadataInfo_metadataValue,

    -- * MetadataKeyValuePair
    MetadataKeyValuePair (..),
    newMetadataKeyValuePair,
    metadataKeyValuePair_metadataKey,
    metadataKeyValuePair_metadataValue,

    -- * MongoDBTarget
    MongoDBTarget (..),
    newMongoDBTarget,
    mongoDBTarget_path,
    mongoDBTarget_connectionName,
    mongoDBTarget_scanAll,

    -- * Node
    Node (..),
    newNode,
    node_triggerDetails,
    node_uniqueId,
    node_crawlerDetails,
    node_name,
    node_jobDetails,
    node_type,

    -- * NotificationProperty
    NotificationProperty (..),
    newNotificationProperty,
    notificationProperty_notifyDelayAfter,

    -- * Order
    Order (..),
    newOrder,
    order_column,
    order_sortOrder,

    -- * OtherMetadataValueListItem
    OtherMetadataValueListItem (..),
    newOtherMetadataValueListItem,
    otherMetadataValueListItem_createdTime,
    otherMetadataValueListItem_metadataValue,

    -- * Partition
    Partition (..),
    newPartition,
    partition_creationTime,
    partition_values,
    partition_catalogId,
    partition_lastAnalyzedTime,
    partition_storageDescriptor,
    partition_databaseName,
    partition_parameters,
    partition_lastAccessTime,
    partition_tableName,

    -- * PartitionError
    PartitionError (..),
    newPartitionError,
    partitionError_partitionValues,
    partitionError_errorDetail,

    -- * PartitionIndex
    PartitionIndex (..),
    newPartitionIndex,
    partitionIndex_keys,
    partitionIndex_indexName,

    -- * PartitionIndexDescriptor
    PartitionIndexDescriptor (..),
    newPartitionIndexDescriptor,
    partitionIndexDescriptor_backfillErrors,
    partitionIndexDescriptor_indexName,
    partitionIndexDescriptor_keys,
    partitionIndexDescriptor_indexStatus,

    -- * PartitionInput
    PartitionInput (..),
    newPartitionInput,
    partitionInput_values,
    partitionInput_lastAnalyzedTime,
    partitionInput_storageDescriptor,
    partitionInput_parameters,
    partitionInput_lastAccessTime,

    -- * PartitionValueList
    PartitionValueList (..),
    newPartitionValueList,
    partitionValueList_values,

    -- * PhysicalConnectionRequirements
    PhysicalConnectionRequirements (..),
    newPhysicalConnectionRequirements,
    physicalConnectionRequirements_securityGroupIdList,
    physicalConnectionRequirements_subnetId,
    physicalConnectionRequirements_availabilityZone,

    -- * Predecessor
    Predecessor (..),
    newPredecessor,
    predecessor_jobName,
    predecessor_runId,

    -- * Predicate
    Predicate (..),
    newPredicate,
    predicate_logical,
    predicate_conditions,

    -- * PrincipalPermissions
    PrincipalPermissions (..),
    newPrincipalPermissions,
    principalPermissions_principal,
    principalPermissions_permissions,

    -- * PropertyPredicate
    PropertyPredicate (..),
    newPropertyPredicate,
    propertyPredicate_value,
    propertyPredicate_key,
    propertyPredicate_comparator,

    -- * RecrawlPolicy
    RecrawlPolicy (..),
    newRecrawlPolicy,
    recrawlPolicy_recrawlBehavior,

    -- * RegistryId
    RegistryId (..),
    newRegistryId,
    registryId_registryName,
    registryId_registryArn,

    -- * RegistryListItem
    RegistryListItem (..),
    newRegistryListItem,
    registryListItem_status,
    registryListItem_registryName,
    registryListItem_createdTime,
    registryListItem_registryArn,
    registryListItem_updatedTime,
    registryListItem_description,

    -- * ResourceUri
    ResourceUri (..),
    newResourceUri,
    resourceUri_resourceType,
    resourceUri_uri,

    -- * S3Encryption
    S3Encryption (..),
    newS3Encryption,
    s3Encryption_s3EncryptionMode,
    s3Encryption_kmsKeyArn,

    -- * S3Target
    S3Target (..),
    newS3Target,
    s3Target_path,
    s3Target_sampleSize,
    s3Target_connectionName,
    s3Target_exclusions,
    s3Target_eventQueueArn,
    s3Target_dlqEventQueueArn,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_state,
    schedule_scheduleExpression,

    -- * SchemaChangePolicy
    SchemaChangePolicy (..),
    newSchemaChangePolicy,
    schemaChangePolicy_deleteBehavior,
    schemaChangePolicy_updateBehavior,

    -- * SchemaColumn
    SchemaColumn (..),
    newSchemaColumn,
    schemaColumn_name,
    schemaColumn_dataType,

    -- * SchemaId
    SchemaId (..),
    newSchemaId,
    schemaId_registryName,
    schemaId_schemaName,
    schemaId_schemaArn,

    -- * SchemaListItem
    SchemaListItem (..),
    newSchemaListItem,
    schemaListItem_registryName,
    schemaListItem_createdTime,
    schemaListItem_schemaStatus,
    schemaListItem_schemaName,
    schemaListItem_schemaArn,
    schemaListItem_updatedTime,
    schemaListItem_description,

    -- * SchemaReference
    SchemaReference (..),
    newSchemaReference,
    schemaReference_schemaVersionId,
    schemaReference_schemaId,
    schemaReference_schemaVersionNumber,

    -- * SchemaVersionErrorItem
    SchemaVersionErrorItem (..),
    newSchemaVersionErrorItem,
    schemaVersionErrorItem_versionNumber,
    schemaVersionErrorItem_errorDetails,

    -- * SchemaVersionListItem
    SchemaVersionListItem (..),
    newSchemaVersionListItem,
    schemaVersionListItem_status,
    schemaVersionListItem_createdTime,
    schemaVersionListItem_schemaVersionId,
    schemaVersionListItem_versionNumber,
    schemaVersionListItem_schemaArn,

    -- * SchemaVersionNumber
    SchemaVersionNumber (..),
    newSchemaVersionNumber,
    schemaVersionNumber_versionNumber,
    schemaVersionNumber_latestVersion,

    -- * SecurityConfiguration
    SecurityConfiguration (..),
    newSecurityConfiguration,
    securityConfiguration_name,
    securityConfiguration_encryptionConfiguration,
    securityConfiguration_createdTimeStamp,

    -- * Segment
    Segment (..),
    newSegment,
    segment_segmentNumber,
    segment_totalSegments,

    -- * SerDeInfo
    SerDeInfo (..),
    newSerDeInfo,
    serDeInfo_serializationLibrary,
    serDeInfo_name,
    serDeInfo_parameters,

    -- * SkewedInfo
    SkewedInfo (..),
    newSkewedInfo,
    skewedInfo_skewedColumnValueLocationMaps,
    skewedInfo_skewedColumnValues,
    skewedInfo_skewedColumnNames,

    -- * SortCriterion
    SortCriterion (..),
    newSortCriterion,
    sortCriterion_sort,
    sortCriterion_fieldName,

    -- * StartingEventBatchCondition
    StartingEventBatchCondition (..),
    newStartingEventBatchCondition,
    startingEventBatchCondition_batchWindow,
    startingEventBatchCondition_batchSize,

    -- * StorageDescriptor
    StorageDescriptor (..),
    newStorageDescriptor,
    storageDescriptor_sortColumns,
    storageDescriptor_compressed,
    storageDescriptor_location,
    storageDescriptor_bucketColumns,
    storageDescriptor_serdeInfo,
    storageDescriptor_outputFormat,
    storageDescriptor_numberOfBuckets,
    storageDescriptor_schemaReference,
    storageDescriptor_storedAsSubDirectories,
    storageDescriptor_parameters,
    storageDescriptor_inputFormat,
    storageDescriptor_skewedInfo,
    storageDescriptor_columns,

    -- * StringColumnStatisticsData
    StringColumnStatisticsData (..),
    newStringColumnStatisticsData,
    stringColumnStatisticsData_maximumLength,
    stringColumnStatisticsData_averageLength,
    stringColumnStatisticsData_numberOfNulls,
    stringColumnStatisticsData_numberOfDistinctValues,

    -- * Table
    Table (..),
    newTable,
    table_retention,
    table_targetTable,
    table_isRegisteredWithLakeFormation,
    table_createdBy,
    table_tableType,
    table_catalogId,
    table_owner,
    table_viewOriginalText,
    table_updateTime,
    table_viewExpandedText,
    table_lastAnalyzedTime,
    table_storageDescriptor,
    table_databaseName,
    table_parameters,
    table_lastAccessTime,
    table_description,
    table_partitionKeys,
    table_createTime,
    table_name,

    -- * TableError
    TableError (..),
    newTableError,
    tableError_tableName,
    tableError_errorDetail,

    -- * TableIdentifier
    TableIdentifier (..),
    newTableIdentifier,
    tableIdentifier_catalogId,
    tableIdentifier_name,
    tableIdentifier_databaseName,

    -- * TableInput
    TableInput (..),
    newTableInput,
    tableInput_retention,
    tableInput_targetTable,
    tableInput_tableType,
    tableInput_owner,
    tableInput_viewOriginalText,
    tableInput_viewExpandedText,
    tableInput_lastAnalyzedTime,
    tableInput_storageDescriptor,
    tableInput_parameters,
    tableInput_lastAccessTime,
    tableInput_description,
    tableInput_partitionKeys,
    tableInput_name,

    -- * TableVersion
    TableVersion (..),
    newTableVersion,
    tableVersion_versionId,
    tableVersion_table,

    -- * TableVersionError
    TableVersionError (..),
    newTableVersionError,
    tableVersionError_versionId,
    tableVersionError_tableName,
    tableVersionError_errorDetail,

    -- * TaskRun
    TaskRun (..),
    newTaskRun,
    taskRun_completedOn,
    taskRun_status,
    taskRun_lastModifiedOn,
    taskRun_errorString,
    taskRun_startedOn,
    taskRun_logGroupName,
    taskRun_executionTime,
    taskRun_properties,
    taskRun_transformId,
    taskRun_taskRunId,

    -- * TaskRunFilterCriteria
    TaskRunFilterCriteria (..),
    newTaskRunFilterCriteria,
    taskRunFilterCriteria_status,
    taskRunFilterCriteria_startedAfter,
    taskRunFilterCriteria_startedBefore,
    taskRunFilterCriteria_taskRunType,

    -- * TaskRunProperties
    TaskRunProperties (..),
    newTaskRunProperties,
    taskRunProperties_taskType,
    taskRunProperties_exportLabelsTaskRunProperties,
    taskRunProperties_labelingSetGenerationTaskRunProperties,
    taskRunProperties_findMatchesTaskRunProperties,
    taskRunProperties_importLabelsTaskRunProperties,

    -- * TaskRunSortCriteria
    TaskRunSortCriteria (..),
    newTaskRunSortCriteria,
    taskRunSortCriteria_column,
    taskRunSortCriteria_sortDirection,

    -- * TransformEncryption
    TransformEncryption (..),
    newTransformEncryption,
    transformEncryption_mlUserDataEncryption,
    transformEncryption_taskRunSecurityConfigurationName,

    -- * TransformFilterCriteria
    TransformFilterCriteria (..),
    newTransformFilterCriteria,
    transformFilterCriteria_createdAfter,
    transformFilterCriteria_status,
    transformFilterCriteria_lastModifiedAfter,
    transformFilterCriteria_lastModifiedBefore,
    transformFilterCriteria_glueVersion,
    transformFilterCriteria_schema,
    transformFilterCriteria_transformType,
    transformFilterCriteria_name,
    transformFilterCriteria_createdBefore,

    -- * TransformParameters
    TransformParameters (..),
    newTransformParameters,
    transformParameters_findMatchesParameters,
    transformParameters_transformType,

    -- * TransformSortCriteria
    TransformSortCriteria (..),
    newTransformSortCriteria,
    transformSortCriteria_column,
    transformSortCriteria_sortDirection,

    -- * Trigger
    Trigger (..),
    newTrigger,
    trigger_workflowName,
    trigger_state,
    trigger_actions,
    trigger_schedule,
    trigger_predicate,
    trigger_name,
    trigger_id,
    trigger_type,
    trigger_eventBatchingCondition,
    trigger_description,

    -- * TriggerNodeDetails
    TriggerNodeDetails (..),
    newTriggerNodeDetails,
    triggerNodeDetails_trigger,

    -- * TriggerUpdate
    TriggerUpdate (..),
    newTriggerUpdate,
    triggerUpdate_actions,
    triggerUpdate_schedule,
    triggerUpdate_predicate,
    triggerUpdate_name,
    triggerUpdate_eventBatchingCondition,
    triggerUpdate_description,

    -- * UpdateCsvClassifierRequest
    UpdateCsvClassifierRequest (..),
    newUpdateCsvClassifierRequest,
    updateCsvClassifierRequest_quoteSymbol,
    updateCsvClassifierRequest_containsHeader,
    updateCsvClassifierRequest_disableValueTrimming,
    updateCsvClassifierRequest_header,
    updateCsvClassifierRequest_allowSingleColumn,
    updateCsvClassifierRequest_delimiter,
    updateCsvClassifierRequest_name,

    -- * UpdateGrokClassifierRequest
    UpdateGrokClassifierRequest (..),
    newUpdateGrokClassifierRequest,
    updateGrokClassifierRequest_classification,
    updateGrokClassifierRequest_customPatterns,
    updateGrokClassifierRequest_grokPattern,
    updateGrokClassifierRequest_name,

    -- * UpdateJsonClassifierRequest
    UpdateJsonClassifierRequest (..),
    newUpdateJsonClassifierRequest,
    updateJsonClassifierRequest_jsonPath,
    updateJsonClassifierRequest_name,

    -- * UpdateXMLClassifierRequest
    UpdateXMLClassifierRequest (..),
    newUpdateXMLClassifierRequest,
    updateXMLClassifierRequest_classification,
    updateXMLClassifierRequest_rowTag,
    updateXMLClassifierRequest_name,

    -- * UserDefinedFunction
    UserDefinedFunction (..),
    newUserDefinedFunction,
    userDefinedFunction_ownerName,
    userDefinedFunction_catalogId,
    userDefinedFunction_resourceUris,
    userDefinedFunction_databaseName,
    userDefinedFunction_functionName,
    userDefinedFunction_ownerType,
    userDefinedFunction_createTime,
    userDefinedFunction_className,

    -- * UserDefinedFunctionInput
    UserDefinedFunctionInput (..),
    newUserDefinedFunctionInput,
    userDefinedFunctionInput_ownerName,
    userDefinedFunctionInput_resourceUris,
    userDefinedFunctionInput_functionName,
    userDefinedFunctionInput_ownerType,
    userDefinedFunctionInput_className,

    -- * Workflow
    Workflow (..),
    newWorkflow,
    workflow_graph,
    workflow_lastModifiedOn,
    workflow_blueprintDetails,
    workflow_maxConcurrentRuns,
    workflow_defaultRunProperties,
    workflow_name,
    workflow_lastRun,
    workflow_description,
    workflow_createdOn,

    -- * WorkflowGraph
    WorkflowGraph (..),
    newWorkflowGraph,
    workflowGraph_edges,
    workflowGraph_nodes,

    -- * WorkflowRun
    WorkflowRun (..),
    newWorkflowRun,
    workflowRun_completedOn,
    workflowRun_status,
    workflowRun_graph,
    workflowRun_startedOn,
    workflowRun_workflowRunId,
    workflowRun_name,
    workflowRun_previousRunId,
    workflowRun_statistics,
    workflowRun_startingEventBatchCondition,
    workflowRun_errorMessage,
    workflowRun_workflowRunProperties,

    -- * WorkflowRunStatistics
    WorkflowRunStatistics (..),
    newWorkflowRunStatistics,
    workflowRunStatistics_runningActions,
    workflowRunStatistics_stoppedActions,
    workflowRunStatistics_totalActions,
    workflowRunStatistics_failedActions,
    workflowRunStatistics_timeoutActions,
    workflowRunStatistics_succeededActions,

    -- * XMLClassifier
    XMLClassifier (..),
    newXMLClassifier,
    xMLClassifier_creationTime,
    xMLClassifier_lastUpdated,
    xMLClassifier_version,
    xMLClassifier_rowTag,
    xMLClassifier_name,
    xMLClassifier_classification,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.Action
import Amazonka.Glue.Types.BackfillError
import Amazonka.Glue.Types.BackfillErrorCode
import Amazonka.Glue.Types.BatchStopJobRunError
import Amazonka.Glue.Types.BatchStopJobRunSuccessfulSubmission
import Amazonka.Glue.Types.BatchUpdatePartitionFailureEntry
import Amazonka.Glue.Types.BatchUpdatePartitionRequestEntry
import Amazonka.Glue.Types.BinaryColumnStatisticsData
import Amazonka.Glue.Types.Blueprint
import Amazonka.Glue.Types.BlueprintDetails
import Amazonka.Glue.Types.BlueprintRun
import Amazonka.Glue.Types.BlueprintRunState
import Amazonka.Glue.Types.BlueprintStatus
import Amazonka.Glue.Types.BooleanColumnStatisticsData
import Amazonka.Glue.Types.CatalogEncryptionMode
import Amazonka.Glue.Types.CatalogEntry
import Amazonka.Glue.Types.CatalogImportStatus
import Amazonka.Glue.Types.CatalogTarget
import Amazonka.Glue.Types.Classifier
import Amazonka.Glue.Types.CloudWatchEncryption
import Amazonka.Glue.Types.CloudWatchEncryptionMode
import Amazonka.Glue.Types.CodeGenEdge
import Amazonka.Glue.Types.CodeGenNode
import Amazonka.Glue.Types.CodeGenNodeArg
import Amazonka.Glue.Types.Column
import Amazonka.Glue.Types.ColumnError
import Amazonka.Glue.Types.ColumnImportance
import Amazonka.Glue.Types.ColumnStatistics
import Amazonka.Glue.Types.ColumnStatisticsData
import Amazonka.Glue.Types.ColumnStatisticsError
import Amazonka.Glue.Types.ColumnStatisticsType
import Amazonka.Glue.Types.Comparator
import Amazonka.Glue.Types.Compatibility
import Amazonka.Glue.Types.Condition
import Amazonka.Glue.Types.ConfusionMatrix
import Amazonka.Glue.Types.Connection
import Amazonka.Glue.Types.ConnectionInput
import Amazonka.Glue.Types.ConnectionPasswordEncryption
import Amazonka.Glue.Types.ConnectionPropertyKey
import Amazonka.Glue.Types.ConnectionType
import Amazonka.Glue.Types.ConnectionsList
import Amazonka.Glue.Types.Crawl
import Amazonka.Glue.Types.CrawlState
import Amazonka.Glue.Types.Crawler
import Amazonka.Glue.Types.CrawlerLineageSettings
import Amazonka.Glue.Types.CrawlerMetrics
import Amazonka.Glue.Types.CrawlerNodeDetails
import Amazonka.Glue.Types.CrawlerState
import Amazonka.Glue.Types.CrawlerTargets
import Amazonka.Glue.Types.CreateCsvClassifierRequest
import Amazonka.Glue.Types.CreateGrokClassifierRequest
import Amazonka.Glue.Types.CreateJsonClassifierRequest
import Amazonka.Glue.Types.CreateXMLClassifierRequest
import Amazonka.Glue.Types.CsvClassifier
import Amazonka.Glue.Types.CsvHeaderOption
import Amazonka.Glue.Types.DataCatalogEncryptionSettings
import Amazonka.Glue.Types.DataFormat
import Amazonka.Glue.Types.DataLakePrincipal
import Amazonka.Glue.Types.Database
import Amazonka.Glue.Types.DatabaseIdentifier
import Amazonka.Glue.Types.DatabaseInput
import Amazonka.Glue.Types.DateColumnStatisticsData
import Amazonka.Glue.Types.DecimalColumnStatisticsData
import Amazonka.Glue.Types.DecimalNumber
import Amazonka.Glue.Types.DeleteBehavior
import Amazonka.Glue.Types.DevEndpoint
import Amazonka.Glue.Types.DevEndpointCustomLibraries
import Amazonka.Glue.Types.DoubleColumnStatisticsData
import Amazonka.Glue.Types.DynamoDBTarget
import Amazonka.Glue.Types.Edge
import Amazonka.Glue.Types.EnableHybridValues
import Amazonka.Glue.Types.EncryptionAtRest
import Amazonka.Glue.Types.EncryptionConfiguration
import Amazonka.Glue.Types.ErrorDetail
import Amazonka.Glue.Types.ErrorDetails
import Amazonka.Glue.Types.EvaluationMetrics
import Amazonka.Glue.Types.EventBatchingCondition
import Amazonka.Glue.Types.ExecutionProperty
import Amazonka.Glue.Types.ExistCondition
import Amazonka.Glue.Types.ExportLabelsTaskRunProperties
import Amazonka.Glue.Types.FindMatchesMetrics
import Amazonka.Glue.Types.FindMatchesParameters
import Amazonka.Glue.Types.FindMatchesTaskRunProperties
import Amazonka.Glue.Types.GetConnectionsFilter
import Amazonka.Glue.Types.GluePolicy
import Amazonka.Glue.Types.GlueTable
import Amazonka.Glue.Types.GrokClassifier
import Amazonka.Glue.Types.ImportLabelsTaskRunProperties
import Amazonka.Glue.Types.JdbcTarget
import Amazonka.Glue.Types.Job
import Amazonka.Glue.Types.JobBookmarkEntry
import Amazonka.Glue.Types.JobBookmarksEncryption
import Amazonka.Glue.Types.JobBookmarksEncryptionMode
import Amazonka.Glue.Types.JobCommand
import Amazonka.Glue.Types.JobNodeDetails
import Amazonka.Glue.Types.JobRun
import Amazonka.Glue.Types.JobRunState
import Amazonka.Glue.Types.JobUpdate
import Amazonka.Glue.Types.JsonClassifier
import Amazonka.Glue.Types.KeySchemaElement
import Amazonka.Glue.Types.LabelingSetGenerationTaskRunProperties
import Amazonka.Glue.Types.Language
import Amazonka.Glue.Types.LastActiveDefinition
import Amazonka.Glue.Types.LastCrawlInfo
import Amazonka.Glue.Types.LastCrawlStatus
import Amazonka.Glue.Types.LineageConfiguration
import Amazonka.Glue.Types.Location
import Amazonka.Glue.Types.Logical
import Amazonka.Glue.Types.LogicalOperator
import Amazonka.Glue.Types.LongColumnStatisticsData
import Amazonka.Glue.Types.MLTransform
import Amazonka.Glue.Types.MLUserDataEncryption
import Amazonka.Glue.Types.MLUserDataEncryptionModeString
import Amazonka.Glue.Types.MappingEntry
import Amazonka.Glue.Types.MetadataInfo
import Amazonka.Glue.Types.MetadataKeyValuePair
import Amazonka.Glue.Types.MongoDBTarget
import Amazonka.Glue.Types.Node
import Amazonka.Glue.Types.NodeType
import Amazonka.Glue.Types.NotificationProperty
import Amazonka.Glue.Types.Order
import Amazonka.Glue.Types.OtherMetadataValueListItem
import Amazonka.Glue.Types.Partition
import Amazonka.Glue.Types.PartitionError
import Amazonka.Glue.Types.PartitionIndex
import Amazonka.Glue.Types.PartitionIndexDescriptor
import Amazonka.Glue.Types.PartitionIndexStatus
import Amazonka.Glue.Types.PartitionInput
import Amazonka.Glue.Types.PartitionValueList
import Amazonka.Glue.Types.Permission
import Amazonka.Glue.Types.PhysicalConnectionRequirements
import Amazonka.Glue.Types.Predecessor
import Amazonka.Glue.Types.Predicate
import Amazonka.Glue.Types.PrincipalPermissions
import Amazonka.Glue.Types.PrincipalType
import Amazonka.Glue.Types.PropertyPredicate
import Amazonka.Glue.Types.RecrawlBehavior
import Amazonka.Glue.Types.RecrawlPolicy
import Amazonka.Glue.Types.RegistryId
import Amazonka.Glue.Types.RegistryListItem
import Amazonka.Glue.Types.RegistryStatus
import Amazonka.Glue.Types.ResourceShareType
import Amazonka.Glue.Types.ResourceType
import Amazonka.Glue.Types.ResourceUri
import Amazonka.Glue.Types.S3Encryption
import Amazonka.Glue.Types.S3EncryptionMode
import Amazonka.Glue.Types.S3Target
import Amazonka.Glue.Types.Schedule
import Amazonka.Glue.Types.ScheduleState
import Amazonka.Glue.Types.SchemaChangePolicy
import Amazonka.Glue.Types.SchemaColumn
import Amazonka.Glue.Types.SchemaDiffType
import Amazonka.Glue.Types.SchemaId
import Amazonka.Glue.Types.SchemaListItem
import Amazonka.Glue.Types.SchemaReference
import Amazonka.Glue.Types.SchemaStatus
import Amazonka.Glue.Types.SchemaVersionErrorItem
import Amazonka.Glue.Types.SchemaVersionListItem
import Amazonka.Glue.Types.SchemaVersionNumber
import Amazonka.Glue.Types.SchemaVersionStatus
import Amazonka.Glue.Types.SecurityConfiguration
import Amazonka.Glue.Types.Segment
import Amazonka.Glue.Types.SerDeInfo
import Amazonka.Glue.Types.SkewedInfo
import Amazonka.Glue.Types.Sort
import Amazonka.Glue.Types.SortCriterion
import Amazonka.Glue.Types.SortDirectionType
import Amazonka.Glue.Types.StartingEventBatchCondition
import Amazonka.Glue.Types.StorageDescriptor
import Amazonka.Glue.Types.StringColumnStatisticsData
import Amazonka.Glue.Types.Table
import Amazonka.Glue.Types.TableError
import Amazonka.Glue.Types.TableIdentifier
import Amazonka.Glue.Types.TableInput
import Amazonka.Glue.Types.TableVersion
import Amazonka.Glue.Types.TableVersionError
import Amazonka.Glue.Types.TaskRun
import Amazonka.Glue.Types.TaskRunFilterCriteria
import Amazonka.Glue.Types.TaskRunProperties
import Amazonka.Glue.Types.TaskRunSortColumnType
import Amazonka.Glue.Types.TaskRunSortCriteria
import Amazonka.Glue.Types.TaskStatusType
import Amazonka.Glue.Types.TaskType
import Amazonka.Glue.Types.TransformEncryption
import Amazonka.Glue.Types.TransformFilterCriteria
import Amazonka.Glue.Types.TransformParameters
import Amazonka.Glue.Types.TransformSortColumnType
import Amazonka.Glue.Types.TransformSortCriteria
import Amazonka.Glue.Types.TransformStatusType
import Amazonka.Glue.Types.TransformType
import Amazonka.Glue.Types.Trigger
import Amazonka.Glue.Types.TriggerNodeDetails
import Amazonka.Glue.Types.TriggerState
import Amazonka.Glue.Types.TriggerType
import Amazonka.Glue.Types.TriggerUpdate
import Amazonka.Glue.Types.UpdateBehavior
import Amazonka.Glue.Types.UpdateCsvClassifierRequest
import Amazonka.Glue.Types.UpdateGrokClassifierRequest
import Amazonka.Glue.Types.UpdateJsonClassifierRequest
import Amazonka.Glue.Types.UpdateXMLClassifierRequest
import Amazonka.Glue.Types.UserDefinedFunction
import Amazonka.Glue.Types.UserDefinedFunctionInput
import Amazonka.Glue.Types.WorkerType
import Amazonka.Glue.Types.Workflow
import Amazonka.Glue.Types.WorkflowGraph
import Amazonka.Glue.Types.WorkflowRun
import Amazonka.Glue.Types.WorkflowRunStatistics
import Amazonka.Glue.Types.WorkflowRunStatus
import Amazonka.Glue.Types.XMLClassifier
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-03-31@ of the Amazon Glue SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Glue",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "glue",
      Core._serviceSigningName = "glue",
      Core._serviceVersion = "2017-03-31",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Glue",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | A value could not be validated.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | Access to a resource was denied.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The operation cannot be performed because the crawler is already
-- running.
_CrawlerRunningException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CrawlerRunningException =
  Core._MatchServiceError
    defaultService
    "CrawlerRunningException"

-- | The specified scheduler is transitioning.
_SchedulerTransitioningException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SchedulerTransitioningException =
  Core._MatchServiceError
    defaultService
    "SchedulerTransitioningException"

-- | The specified scheduler is already running.
_SchedulerRunningException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SchedulerRunningException =
  Core._MatchServiceError
    defaultService
    "SchedulerRunningException"

-- | A specified condition was not satisfied.
_ConditionCheckFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConditionCheckFailureException =
  Core._MatchServiceError
    defaultService
    "ConditionCheckFailureException"

-- | Too many jobs are being run concurrently.
_ConcurrentRunsExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentRunsExceededException =
  Core._MatchServiceError
    defaultService
    "ConcurrentRunsExceededException"

-- | The workflow is in an invalid state to perform a requested operation.
_IllegalWorkflowStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IllegalWorkflowStateException =
  Core._MatchServiceError
    defaultService
    "IllegalWorkflowStateException"

-- | There is no applicable schedule.
_NoScheduleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoScheduleException =
  Core._MatchServiceError
    defaultService
    "NoScheduleException"

-- | The operation timed out.
_OperationTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationTimeoutException =
  Core._MatchServiceError
    defaultService
    "OperationTimeoutException"

-- | The @CreatePartitions@ API was called on a table that has indexes
-- enabled.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The specified crawler is not running.
_CrawlerNotRunningException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CrawlerNotRunningException =
  Core._MatchServiceError
    defaultService
    "CrawlerNotRunningException"

-- | There was a version conflict.
_VersionMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_VersionMismatchException =
  Core._MatchServiceError
    defaultService
    "VersionMismatchException"

-- | The machine learning transform is not ready to run.
_MLTransformNotReadyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MLTransformNotReadyException =
  Core._MatchServiceError
    defaultService
    "MLTransformNotReadyException"

-- | A specified entity does not exist
_EntityNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityNotFoundException =
  Core._MatchServiceError
    defaultService
    "EntityNotFoundException"

-- | Two processes are trying to modify a resource simultaneously.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The specified scheduler is not running.
_SchedulerNotRunningException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SchedulerNotRunningException =
  Core._MatchServiceError
    defaultService
    "SchedulerNotRunningException"

-- | An internal service error occurred.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The input provided was not valid.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | A resource numerical limit was exceeded.
_ResourceNumberLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNumberLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceNumberLimitExceededException"

-- | An encryption operation failed.
_GlueEncryptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlueEncryptionException =
  Core._MatchServiceError
    defaultService
    "GlueEncryptionException"

-- | The same unique identifier was associated with two different records.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | The specified crawler is stopping.
_CrawlerStoppingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CrawlerStoppingException =
  Core._MatchServiceError
    defaultService
    "CrawlerStoppingException"

-- | Prism for IllegalBlueprintStateException' errors.
_IllegalBlueprintStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IllegalBlueprintStateException =
  Core._MatchServiceError
    defaultService
    "IllegalBlueprintStateException"

-- | A resource to be created or added already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"
