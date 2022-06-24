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
    _CrawlerNotRunningException,
    _InvalidInputException,
    _ResourceNumberLimitExceededException,
    _ConcurrentModificationException,
    _AccessDeniedException,
    _ConcurrentRunsExceededException,
    _SchedulerNotRunningException,
    _AlreadyExistsException,
    _SchedulerRunningException,
    _MLTransformNotReadyException,
    _GlueEncryptionException,
    _IllegalWorkflowStateException,
    _ConflictException,
    _ConditionCheckFailureException,
    _VersionMismatchException,
    _EntityNotFoundException,
    _SchedulerTransitioningException,
    _IllegalBlueprintStateException,
    _ValidationException,
    _InternalServiceException,
    _OperationTimeoutException,
    _CrawlerStoppingException,
    _CrawlerRunningException,
    _NoScheduleException,
    _IdempotentParameterMismatchException,

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
    action_securityConfiguration,
    action_timeout,
    action_jobName,
    action_notificationProperty,
    action_crawlerName,
    action_arguments,

    -- * BackfillError
    BackfillError (..),
    newBackfillError,
    backfillError_code,
    backfillError_partitions,

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
    blueprint_createdOn,
    blueprint_name,
    blueprint_lastModifiedOn,
    blueprint_errorMessage,
    blueprint_lastActiveDefinition,
    blueprint_status,
    blueprint_parameterSpec,
    blueprint_description,
    blueprint_blueprintLocation,
    blueprint_blueprintServiceLocation,

    -- * BlueprintDetails
    BlueprintDetails (..),
    newBlueprintDetails,
    blueprintDetails_blueprintName,
    blueprintDetails_runId,

    -- * BlueprintRun
    BlueprintRun (..),
    newBlueprintRun,
    blueprintRun_roleArn,
    blueprintRun_startedOn,
    blueprintRun_errorMessage,
    blueprintRun_workflowName,
    blueprintRun_state,
    blueprintRun_completedOn,
    blueprintRun_blueprintName,
    blueprintRun_rollbackErrorMessage,
    blueprintRun_runId,
    blueprintRun_parameters,

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
    catalogImportStatus_importCompleted,
    catalogImportStatus_importedBy,
    catalogImportStatus_importTime,

    -- * CatalogTarget
    CatalogTarget (..),
    newCatalogTarget,
    catalogTarget_databaseName,
    catalogTarget_tables,

    -- * Classifier
    Classifier (..),
    newClassifier,
    classifier_csvClassifier,
    classifier_xMLClassifier,
    classifier_grokClassifier,
    classifier_jsonClassifier,

    -- * CloudWatchEncryption
    CloudWatchEncryption (..),
    newCloudWatchEncryption,
    cloudWatchEncryption_kmsKeyArn,
    cloudWatchEncryption_cloudWatchEncryptionMode,

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
    column_type,
    column_comment,
    column_parameters,
    column_name,

    -- * ColumnError
    ColumnError (..),
    newColumnError,
    columnError_columnName,
    columnError_error,

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
    columnStatisticsData_decimalColumnStatisticsData,
    columnStatisticsData_stringColumnStatisticsData,
    columnStatisticsData_booleanColumnStatisticsData,
    columnStatisticsData_doubleColumnStatisticsData,
    columnStatisticsData_longColumnStatisticsData,
    columnStatisticsData_dateColumnStatisticsData,
    columnStatisticsData_type,

    -- * ColumnStatisticsError
    ColumnStatisticsError (..),
    newColumnStatisticsError,
    columnStatisticsError_columnStatistics,
    columnStatisticsError_error,

    -- * Condition
    Condition (..),
    newCondition,
    condition_logicalOperator,
    condition_crawlState,
    condition_jobName,
    condition_state,
    condition_crawlerName,

    -- * ConfusionMatrix
    ConfusionMatrix (..),
    newConfusionMatrix,
    confusionMatrix_numFalseNegatives,
    confusionMatrix_numTruePositives,
    confusionMatrix_numFalsePositives,
    confusionMatrix_numTrueNegatives,

    -- * Connection
    Connection (..),
    newConnection,
    connection_name,
    connection_connectionType,
    connection_connectionProperties,
    connection_physicalConnectionRequirements,
    connection_lastUpdatedTime,
    connection_description,
    connection_matchCriteria,
    connection_creationTime,
    connection_lastUpdatedBy,

    -- * ConnectionInput
    ConnectionInput (..),
    newConnectionInput,
    connectionInput_physicalConnectionRequirements,
    connectionInput_description,
    connectionInput_matchCriteria,
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
    crawl_logGroup,
    crawl_startedOn,
    crawl_logStream,
    crawl_errorMessage,
    crawl_state,
    crawl_completedOn,

    -- * Crawler
    Crawler (..),
    newCrawler,
    crawler_schedule,
    crawler_name,
    crawler_recrawlPolicy,
    crawler_classifiers,
    crawler_schemaChangePolicy,
    crawler_databaseName,
    crawler_configuration,
    crawler_state,
    crawler_tablePrefix,
    crawler_targets,
    crawler_description,
    crawler_lastUpdated,
    crawler_crawlElapsedTime,
    crawler_lineageConfiguration,
    crawler_role,
    crawler_creationTime,
    crawler_lastCrawl,
    crawler_crawlerSecurityConfiguration,
    crawler_version,

    -- * CrawlerMetrics
    CrawlerMetrics (..),
    newCrawlerMetrics,
    crawlerMetrics_medianRuntimeSeconds,
    crawlerMetrics_lastRuntimeSeconds,
    crawlerMetrics_stillEstimating,
    crawlerMetrics_tablesUpdated,
    crawlerMetrics_crawlerName,
    crawlerMetrics_tablesCreated,
    crawlerMetrics_tablesDeleted,
    crawlerMetrics_timeLeftSeconds,

    -- * CrawlerNodeDetails
    CrawlerNodeDetails (..),
    newCrawlerNodeDetails,
    crawlerNodeDetails_crawls,

    -- * CrawlerTargets
    CrawlerTargets (..),
    newCrawlerTargets,
    crawlerTargets_mongoDBTargets,
    crawlerTargets_dynamoDBTargets,
    crawlerTargets_jdbcTargets,
    crawlerTargets_s3Targets,
    crawlerTargets_catalogTargets,

    -- * CreateCsvClassifierRequest
    CreateCsvClassifierRequest (..),
    newCreateCsvClassifierRequest,
    createCsvClassifierRequest_quoteSymbol,
    createCsvClassifierRequest_header,
    createCsvClassifierRequest_containsHeader,
    createCsvClassifierRequest_disableValueTrimming,
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
    csvClassifier_quoteSymbol,
    csvClassifier_header,
    csvClassifier_containsHeader,
    csvClassifier_disableValueTrimming,
    csvClassifier_allowSingleColumn,
    csvClassifier_lastUpdated,
    csvClassifier_delimiter,
    csvClassifier_creationTime,
    csvClassifier_version,
    csvClassifier_name,

    -- * DataCatalogEncryptionSettings
    DataCatalogEncryptionSettings (..),
    newDataCatalogEncryptionSettings,
    dataCatalogEncryptionSettings_connectionPasswordEncryption,
    dataCatalogEncryptionSettings_encryptionAtRest,

    -- * DataLakePrincipal
    DataLakePrincipal (..),
    newDataLakePrincipal,
    dataLakePrincipal_dataLakePrincipalIdentifier,

    -- * Database
    Database (..),
    newDatabase,
    database_targetDatabase,
    database_description,
    database_catalogId,
    database_locationUri,
    database_createTime,
    database_createTableDefaultPermissions,
    database_parameters,
    database_name,

    -- * DatabaseIdentifier
    DatabaseIdentifier (..),
    newDatabaseIdentifier,
    databaseIdentifier_databaseName,
    databaseIdentifier_catalogId,

    -- * DatabaseInput
    DatabaseInput (..),
    newDatabaseInput,
    databaseInput_targetDatabase,
    databaseInput_description,
    databaseInput_locationUri,
    databaseInput_createTableDefaultPermissions,
    databaseInput_parameters,
    databaseInput_name,

    -- * DateColumnStatisticsData
    DateColumnStatisticsData (..),
    newDateColumnStatisticsData,
    dateColumnStatisticsData_minimumValue,
    dateColumnStatisticsData_maximumValue,
    dateColumnStatisticsData_numberOfNulls,
    dateColumnStatisticsData_numberOfDistinctValues,

    -- * DecimalColumnStatisticsData
    DecimalColumnStatisticsData (..),
    newDecimalColumnStatisticsData,
    decimalColumnStatisticsData_minimumValue,
    decimalColumnStatisticsData_maximumValue,
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
    devEndpoint_securityConfiguration,
    devEndpoint_zeppelinRemoteSparkInterpreterPort,
    devEndpoint_roleArn,
    devEndpoint_publicKey,
    devEndpoint_numberOfWorkers,
    devEndpoint_securityGroupIds,
    devEndpoint_endpointName,
    devEndpoint_glueVersion,
    devEndpoint_createdTimestamp,
    devEndpoint_lastModifiedTimestamp,
    devEndpoint_subnetId,
    devEndpoint_workerType,
    devEndpoint_publicAddress,
    devEndpoint_status,
    devEndpoint_availabilityZone,
    devEndpoint_extraJarsS3Path,
    devEndpoint_privateAddress,
    devEndpoint_numberOfNodes,
    devEndpoint_arguments,
    devEndpoint_lastUpdateStatus,
    devEndpoint_yarnEndpointAddress,
    devEndpoint_vpcId,
    devEndpoint_extraPythonLibsS3Path,
    devEndpoint_failureReason,
    devEndpoint_publicKeys,

    -- * DevEndpointCustomLibraries
    DevEndpointCustomLibraries (..),
    newDevEndpointCustomLibraries,
    devEndpointCustomLibraries_extraJarsS3Path,
    devEndpointCustomLibraries_extraPythonLibsS3Path,

    -- * DoubleColumnStatisticsData
    DoubleColumnStatisticsData (..),
    newDoubleColumnStatisticsData,
    doubleColumnStatisticsData_minimumValue,
    doubleColumnStatisticsData_maximumValue,
    doubleColumnStatisticsData_numberOfNulls,
    doubleColumnStatisticsData_numberOfDistinctValues,

    -- * DynamoDBTarget
    DynamoDBTarget (..),
    newDynamoDBTarget,
    dynamoDBTarget_scanAll,
    dynamoDBTarget_path,
    dynamoDBTarget_scanRate,

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
    encryptionConfiguration_cloudWatchEncryption,
    encryptionConfiguration_jobBookmarksEncryption,

    -- * ErrorDetail
    ErrorDetail (..),
    newErrorDetail,
    errorDetail_errorMessage,
    errorDetail_errorCode,

    -- * ErrorDetails
    ErrorDetails (..),
    newErrorDetails,
    errorDetails_errorMessage,
    errorDetails_errorCode,

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
    findMatchesMetrics_columnImportances,
    findMatchesMetrics_recall,
    findMatchesMetrics_confusionMatrix,
    findMatchesMetrics_precision,
    findMatchesMetrics_areaUnderPRCurve,

    -- * FindMatchesParameters
    FindMatchesParameters (..),
    newFindMatchesParameters,
    findMatchesParameters_accuracyCostTradeoff,
    findMatchesParameters_primaryKeyColumnName,
    findMatchesParameters_precisionRecallTradeoff,
    findMatchesParameters_enforceProvidedLabels,

    -- * FindMatchesTaskRunProperties
    FindMatchesTaskRunProperties (..),
    newFindMatchesTaskRunProperties,
    findMatchesTaskRunProperties_jobName,
    findMatchesTaskRunProperties_jobRunId,
    findMatchesTaskRunProperties_jobId,

    -- * GetConnectionsFilter
    GetConnectionsFilter (..),
    newGetConnectionsFilter,
    getConnectionsFilter_connectionType,
    getConnectionsFilter_matchCriteria,

    -- * GluePolicy
    GluePolicy (..),
    newGluePolicy,
    gluePolicy_policyInJson,
    gluePolicy_updateTime,
    gluePolicy_createTime,
    gluePolicy_policyHash,

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
    grokClassifier_lastUpdated,
    grokClassifier_creationTime,
    grokClassifier_customPatterns,
    grokClassifier_version,
    grokClassifier_name,
    grokClassifier_classification,
    grokClassifier_grokPattern,

    -- * ImportLabelsTaskRunProperties
    ImportLabelsTaskRunProperties (..),
    newImportLabelsTaskRunProperties,
    importLabelsTaskRunProperties_inputS3Path,
    importLabelsTaskRunProperties_replace,

    -- * JdbcTarget
    JdbcTarget (..),
    newJdbcTarget,
    jdbcTarget_path,
    jdbcTarget_exclusions,
    jdbcTarget_connectionName,

    -- * Job
    Job (..),
    newJob,
    job_securityConfiguration,
    job_createdOn,
    job_timeout,
    job_name,
    job_lastModifiedOn,
    job_nonOverridableArguments,
    job_numberOfWorkers,
    job_glueVersion,
    job_notificationProperty,
    job_workerType,
    job_executionProperty,
    job_allocatedCapacity,
    job_command,
    job_description,
    job_maxRetries,
    job_defaultArguments,
    job_logUri,
    job_connections,
    job_role,
    job_maxCapacity,

    -- * JobBookmarkEntry
    JobBookmarkEntry (..),
    newJobBookmarkEntry,
    jobBookmarkEntry_attempt,
    jobBookmarkEntry_jobName,
    jobBookmarkEntry_previousRunId,
    jobBookmarkEntry_jobBookmark,
    jobBookmarkEntry_run,
    jobBookmarkEntry_runId,
    jobBookmarkEntry_version,

    -- * JobBookmarksEncryption
    JobBookmarksEncryption (..),
    newJobBookmarksEncryption,
    jobBookmarksEncryption_jobBookmarksEncryptionMode,
    jobBookmarksEncryption_kmsKeyArn,

    -- * JobCommand
    JobCommand (..),
    newJobCommand,
    jobCommand_name,
    jobCommand_scriptLocation,
    jobCommand_pythonVersion,

    -- * JobNodeDetails
    JobNodeDetails (..),
    newJobNodeDetails,
    jobNodeDetails_jobRuns,

    -- * JobRun
    JobRun (..),
    newJobRun,
    jobRun_securityConfiguration,
    jobRun_timeout,
    jobRun_triggerName,
    jobRun_lastModifiedOn,
    jobRun_numberOfWorkers,
    jobRun_startedOn,
    jobRun_errorMessage,
    jobRun_attempt,
    jobRun_glueVersion,
    jobRun_jobName,
    jobRun_previousRunId,
    jobRun_notificationProperty,
    jobRun_workerType,
    jobRun_executionTime,
    jobRun_allocatedCapacity,
    jobRun_predecessorRuns,
    jobRun_id,
    jobRun_completedOn,
    jobRun_arguments,
    jobRun_jobRunState,
    jobRun_maxCapacity,
    jobRun_logGroupName,

    -- * JobUpdate
    JobUpdate (..),
    newJobUpdate,
    jobUpdate_securityConfiguration,
    jobUpdate_timeout,
    jobUpdate_nonOverridableArguments,
    jobUpdate_numberOfWorkers,
    jobUpdate_glueVersion,
    jobUpdate_notificationProperty,
    jobUpdate_workerType,
    jobUpdate_executionProperty,
    jobUpdate_allocatedCapacity,
    jobUpdate_command,
    jobUpdate_description,
    jobUpdate_maxRetries,
    jobUpdate_defaultArguments,
    jobUpdate_logUri,
    jobUpdate_connections,
    jobUpdate_role,
    jobUpdate_maxCapacity,

    -- * JsonClassifier
    JsonClassifier (..),
    newJsonClassifier,
    jsonClassifier_lastUpdated,
    jsonClassifier_creationTime,
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
    lastActiveDefinition_lastModifiedOn,
    lastActiveDefinition_parameterSpec,
    lastActiveDefinition_description,
    lastActiveDefinition_blueprintLocation,
    lastActiveDefinition_blueprintServiceLocation,

    -- * LastCrawlInfo
    LastCrawlInfo (..),
    newLastCrawlInfo,
    lastCrawlInfo_logGroup,
    lastCrawlInfo_logStream,
    lastCrawlInfo_errorMessage,
    lastCrawlInfo_status,
    lastCrawlInfo_messagePrefix,
    lastCrawlInfo_startTime,

    -- * LineageConfiguration
    LineageConfiguration (..),
    newLineageConfiguration,
    lineageConfiguration_crawlerLineageSettings,

    -- * Location
    Location (..),
    newLocation,
    location_s3,
    location_dynamoDB,
    location_jdbc,

    -- * LongColumnStatisticsData
    LongColumnStatisticsData (..),
    newLongColumnStatisticsData,
    longColumnStatisticsData_minimumValue,
    longColumnStatisticsData_maximumValue,
    longColumnStatisticsData_numberOfNulls,
    longColumnStatisticsData_numberOfDistinctValues,

    -- * MLTransform
    MLTransform (..),
    newMLTransform,
    mLTransform_evaluationMetrics,
    mLTransform_createdOn,
    mLTransform_timeout,
    mLTransform_name,
    mLTransform_lastModifiedOn,
    mLTransform_numberOfWorkers,
    mLTransform_glueVersion,
    mLTransform_workerType,
    mLTransform_inputRecordTables,
    mLTransform_status,
    mLTransform_labelCount,
    mLTransform_description,
    mLTransform_maxRetries,
    mLTransform_transformId,
    mLTransform_transformEncryption,
    mLTransform_schema,
    mLTransform_role,
    mLTransform_maxCapacity,
    mLTransform_parameters,

    -- * MLUserDataEncryption
    MLUserDataEncryption (..),
    newMLUserDataEncryption,
    mLUserDataEncryption_kmsKeyId,
    mLUserDataEncryption_mlUserDataEncryptionMode,

    -- * MappingEntry
    MappingEntry (..),
    newMappingEntry,
    mappingEntry_targetTable,
    mappingEntry_sourceTable,
    mappingEntry_targetPath,
    mappingEntry_sourceType,
    mappingEntry_targetType,
    mappingEntry_sourcePath,

    -- * MetadataInfo
    MetadataInfo (..),
    newMetadataInfo,
    metadataInfo_createdTime,
    metadataInfo_metadataValue,
    metadataInfo_otherMetadataValueList,

    -- * MetadataKeyValuePair
    MetadataKeyValuePair (..),
    newMetadataKeyValuePair,
    metadataKeyValuePair_metadataValue,
    metadataKeyValuePair_metadataKey,

    -- * MongoDBTarget
    MongoDBTarget (..),
    newMongoDBTarget,
    mongoDBTarget_scanAll,
    mongoDBTarget_path,
    mongoDBTarget_connectionName,

    -- * Node
    Node (..),
    newNode,
    node_name,
    node_type,
    node_jobDetails,
    node_triggerDetails,
    node_uniqueId,
    node_crawlerDetails,

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
    partition_tableName,
    partition_lastAccessTime,
    partition_databaseName,
    partition_catalogId,
    partition_creationTime,
    partition_storageDescriptor,
    partition_values,
    partition_lastAnalyzedTime,
    partition_parameters,

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
    partitionInput_lastAccessTime,
    partitionInput_storageDescriptor,
    partitionInput_values,
    partitionInput_lastAnalyzedTime,
    partitionInput_parameters,

    -- * PartitionValueList
    PartitionValueList (..),
    newPartitionValueList,
    partitionValueList_values,

    -- * PhysicalConnectionRequirements
    PhysicalConnectionRequirements (..),
    newPhysicalConnectionRequirements,
    physicalConnectionRequirements_subnetId,
    physicalConnectionRequirements_availabilityZone,
    physicalConnectionRequirements_securityGroupIdList,

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
    propertyPredicate_key,
    propertyPredicate_comparator,
    propertyPredicate_value,

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
    registryListItem_registryName,
    registryListItem_createdTime,
    registryListItem_status,
    registryListItem_description,
    registryListItem_registryArn,
    registryListItem_updatedTime,

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
    s3Target_dlqEventQueueArn,
    s3Target_path,
    s3Target_sampleSize,
    s3Target_exclusions,
    s3Target_eventQueueArn,
    s3Target_connectionName,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_state,
    schedule_scheduleExpression,

    -- * SchemaChangePolicy
    SchemaChangePolicy (..),
    newSchemaChangePolicy,
    schemaChangePolicy_updateBehavior,
    schemaChangePolicy_deleteBehavior,

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
    schemaListItem_description,
    schemaListItem_schemaArn,
    schemaListItem_updatedTime,

    -- * SchemaReference
    SchemaReference (..),
    newSchemaReference,
    schemaReference_schemaVersionNumber,
    schemaReference_schemaVersionId,
    schemaReference_schemaId,

    -- * SchemaVersionErrorItem
    SchemaVersionErrorItem (..),
    newSchemaVersionErrorItem,
    schemaVersionErrorItem_errorDetails,
    schemaVersionErrorItem_versionNumber,

    -- * SchemaVersionListItem
    SchemaVersionListItem (..),
    newSchemaVersionListItem,
    schemaVersionListItem_createdTime,
    schemaVersionListItem_status,
    schemaVersionListItem_schemaArn,
    schemaVersionListItem_versionNumber,
    schemaVersionListItem_schemaVersionId,

    -- * SchemaVersionNumber
    SchemaVersionNumber (..),
    newSchemaVersionNumber,
    schemaVersionNumber_latestVersion,
    schemaVersionNumber_versionNumber,

    -- * SecurityConfiguration
    SecurityConfiguration (..),
    newSecurityConfiguration,
    securityConfiguration_name,
    securityConfiguration_createdTimeStamp,
    securityConfiguration_encryptionConfiguration,

    -- * Segment
    Segment (..),
    newSegment,
    segment_segmentNumber,
    segment_totalSegments,

    -- * SerDeInfo
    SerDeInfo (..),
    newSerDeInfo,
    serDeInfo_name,
    serDeInfo_serializationLibrary,
    serDeInfo_parameters,

    -- * SkewedInfo
    SkewedInfo (..),
    newSkewedInfo,
    skewedInfo_skewedColumnNames,
    skewedInfo_skewedColumnValueLocationMaps,
    skewedInfo_skewedColumnValues,

    -- * SortCriterion
    SortCriterion (..),
    newSortCriterion,
    sortCriterion_fieldName,
    sortCriterion_sort,

    -- * StartingEventBatchCondition
    StartingEventBatchCondition (..),
    newStartingEventBatchCondition,
    startingEventBatchCondition_batchWindow,
    startingEventBatchCondition_batchSize,

    -- * StorageDescriptor
    StorageDescriptor (..),
    newStorageDescriptor,
    storageDescriptor_sortColumns,
    storageDescriptor_storedAsSubDirectories,
    storageDescriptor_skewedInfo,
    storageDescriptor_columns,
    storageDescriptor_bucketColumns,
    storageDescriptor_serdeInfo,
    storageDescriptor_outputFormat,
    storageDescriptor_location,
    storageDescriptor_compressed,
    storageDescriptor_schemaReference,
    storageDescriptor_inputFormat,
    storageDescriptor_numberOfBuckets,
    storageDescriptor_parameters,

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
    table_targetTable,
    table_lastAccessTime,
    table_databaseName,
    table_viewOriginalText,
    table_owner,
    table_viewExpandedText,
    table_description,
    table_partitionKeys,
    table_isRegisteredWithLakeFormation,
    table_catalogId,
    table_tableType,
    table_storageDescriptor,
    table_updateTime,
    table_createTime,
    table_retention,
    table_createdBy,
    table_lastAnalyzedTime,
    table_parameters,
    table_name,

    -- * TableError
    TableError (..),
    newTableError,
    tableError_tableName,
    tableError_errorDetail,

    -- * TableIdentifier
    TableIdentifier (..),
    newTableIdentifier,
    tableIdentifier_name,
    tableIdentifier_databaseName,
    tableIdentifier_catalogId,

    -- * TableInput
    TableInput (..),
    newTableInput,
    tableInput_targetTable,
    tableInput_lastAccessTime,
    tableInput_viewOriginalText,
    tableInput_owner,
    tableInput_viewExpandedText,
    tableInput_description,
    tableInput_partitionKeys,
    tableInput_tableType,
    tableInput_storageDescriptor,
    tableInput_retention,
    tableInput_lastAnalyzedTime,
    tableInput_parameters,
    tableInput_name,

    -- * TableVersion
    TableVersion (..),
    newTableVersion,
    tableVersion_table,
    tableVersion_versionId,

    -- * TableVersionError
    TableVersionError (..),
    newTableVersionError,
    tableVersionError_tableName,
    tableVersionError_errorDetail,
    tableVersionError_versionId,

    -- * TaskRun
    TaskRun (..),
    newTaskRun,
    taskRun_lastModifiedOn,
    taskRun_startedOn,
    taskRun_properties,
    taskRun_executionTime,
    taskRun_status,
    taskRun_transformId,
    taskRun_completedOn,
    taskRun_taskRunId,
    taskRun_errorString,
    taskRun_logGroupName,

    -- * TaskRunFilterCriteria
    TaskRunFilterCriteria (..),
    newTaskRunFilterCriteria,
    taskRunFilterCriteria_startedBefore,
    taskRunFilterCriteria_status,
    taskRunFilterCriteria_startedAfter,
    taskRunFilterCriteria_taskRunType,

    -- * TaskRunProperties
    TaskRunProperties (..),
    newTaskRunProperties,
    taskRunProperties_importLabelsTaskRunProperties,
    taskRunProperties_findMatchesTaskRunProperties,
    taskRunProperties_taskType,
    taskRunProperties_labelingSetGenerationTaskRunProperties,
    taskRunProperties_exportLabelsTaskRunProperties,

    -- * TaskRunSortCriteria
    TaskRunSortCriteria (..),
    newTaskRunSortCriteria,
    taskRunSortCriteria_column,
    taskRunSortCriteria_sortDirection,

    -- * TransformEncryption
    TransformEncryption (..),
    newTransformEncryption,
    transformEncryption_taskRunSecurityConfigurationName,
    transformEncryption_mlUserDataEncryption,

    -- * TransformFilterCriteria
    TransformFilterCriteria (..),
    newTransformFilterCriteria,
    transformFilterCriteria_name,
    transformFilterCriteria_transformType,
    transformFilterCriteria_glueVersion,
    transformFilterCriteria_createdBefore,
    transformFilterCriteria_status,
    transformFilterCriteria_schema,
    transformFilterCriteria_createdAfter,
    transformFilterCriteria_lastModifiedAfter,
    transformFilterCriteria_lastModifiedBefore,

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
    trigger_eventBatchingCondition,
    trigger_schedule,
    trigger_name,
    trigger_type,
    trigger_workflowName,
    trigger_predicate,
    trigger_state,
    trigger_id,
    trigger_description,
    trigger_actions,

    -- * TriggerNodeDetails
    TriggerNodeDetails (..),
    newTriggerNodeDetails,
    triggerNodeDetails_trigger,

    -- * TriggerUpdate
    TriggerUpdate (..),
    newTriggerUpdate,
    triggerUpdate_eventBatchingCondition,
    triggerUpdate_schedule,
    triggerUpdate_name,
    triggerUpdate_predicate,
    triggerUpdate_description,
    triggerUpdate_actions,

    -- * UpdateCsvClassifierRequest
    UpdateCsvClassifierRequest (..),
    newUpdateCsvClassifierRequest,
    updateCsvClassifierRequest_quoteSymbol,
    updateCsvClassifierRequest_header,
    updateCsvClassifierRequest_containsHeader,
    updateCsvClassifierRequest_disableValueTrimming,
    updateCsvClassifierRequest_allowSingleColumn,
    updateCsvClassifierRequest_delimiter,
    updateCsvClassifierRequest_name,

    -- * UpdateGrokClassifierRequest
    UpdateGrokClassifierRequest (..),
    newUpdateGrokClassifierRequest,
    updateGrokClassifierRequest_customPatterns,
    updateGrokClassifierRequest_grokPattern,
    updateGrokClassifierRequest_classification,
    updateGrokClassifierRequest_name,

    -- * UpdateJsonClassifierRequest
    UpdateJsonClassifierRequest (..),
    newUpdateJsonClassifierRequest,
    updateJsonClassifierRequest_jsonPath,
    updateJsonClassifierRequest_name,

    -- * UpdateXMLClassifierRequest
    UpdateXMLClassifierRequest (..),
    newUpdateXMLClassifierRequest,
    updateXMLClassifierRequest_rowTag,
    updateXMLClassifierRequest_classification,
    updateXMLClassifierRequest_name,

    -- * UserDefinedFunction
    UserDefinedFunction (..),
    newUserDefinedFunction,
    userDefinedFunction_ownerType,
    userDefinedFunction_ownerName,
    userDefinedFunction_databaseName,
    userDefinedFunction_resourceUris,
    userDefinedFunction_functionName,
    userDefinedFunction_className,
    userDefinedFunction_catalogId,
    userDefinedFunction_createTime,

    -- * UserDefinedFunctionInput
    UserDefinedFunctionInput (..),
    newUserDefinedFunctionInput,
    userDefinedFunctionInput_ownerType,
    userDefinedFunctionInput_ownerName,
    userDefinedFunctionInput_resourceUris,
    userDefinedFunctionInput_functionName,
    userDefinedFunctionInput_className,

    -- * Workflow
    Workflow (..),
    newWorkflow,
    workflow_lastRun,
    workflow_createdOn,
    workflow_name,
    workflow_lastModifiedOn,
    workflow_maxConcurrentRuns,
    workflow_graph,
    workflow_defaultRunProperties,
    workflow_description,
    workflow_blueprintDetails,

    -- * WorkflowGraph
    WorkflowGraph (..),
    newWorkflowGraph,
    workflowGraph_edges,
    workflowGraph_nodes,

    -- * WorkflowRun
    WorkflowRun (..),
    newWorkflowRun,
    workflowRun_name,
    workflowRun_startedOn,
    workflowRun_graph,
    workflowRun_errorMessage,
    workflowRun_statistics,
    workflowRun_workflowRunProperties,
    workflowRun_previousRunId,
    workflowRun_startingEventBatchCondition,
    workflowRun_status,
    workflowRun_completedOn,
    workflowRun_workflowRunId,

    -- * WorkflowRunStatistics
    WorkflowRunStatistics (..),
    newWorkflowRunStatistics,
    workflowRunStatistics_timeoutActions,
    workflowRunStatistics_succeededActions,
    workflowRunStatistics_totalActions,
    workflowRunStatistics_stoppedActions,
    workflowRunStatistics_failedActions,
    workflowRunStatistics_runningActions,

    -- * XMLClassifier
    XMLClassifier (..),
    newXMLClassifier,
    xMLClassifier_rowTag,
    xMLClassifier_lastUpdated,
    xMLClassifier_creationTime,
    xMLClassifier_version,
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified crawler is not running.
_CrawlerNotRunningException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CrawlerNotRunningException =
  Core._MatchServiceError
    defaultService
    "CrawlerNotRunningException"

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

-- | Two processes are trying to modify a resource simultaneously.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | Access to a resource was denied.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Too many jobs are being run concurrently.
_ConcurrentRunsExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentRunsExceededException =
  Core._MatchServiceError
    defaultService
    "ConcurrentRunsExceededException"

-- | The specified scheduler is not running.
_SchedulerNotRunningException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SchedulerNotRunningException =
  Core._MatchServiceError
    defaultService
    "SchedulerNotRunningException"

-- | A resource to be created or added already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | The specified scheduler is already running.
_SchedulerRunningException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SchedulerRunningException =
  Core._MatchServiceError
    defaultService
    "SchedulerRunningException"

-- | The machine learning transform is not ready to run.
_MLTransformNotReadyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MLTransformNotReadyException =
  Core._MatchServiceError
    defaultService
    "MLTransformNotReadyException"

-- | An encryption operation failed.
_GlueEncryptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlueEncryptionException =
  Core._MatchServiceError
    defaultService
    "GlueEncryptionException"

-- | The workflow is in an invalid state to perform a requested operation.
_IllegalWorkflowStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IllegalWorkflowStateException =
  Core._MatchServiceError
    defaultService
    "IllegalWorkflowStateException"

-- | The @CreatePartitions@ API was called on a table that has indexes
-- enabled.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | A specified condition was not satisfied.
_ConditionCheckFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConditionCheckFailureException =
  Core._MatchServiceError
    defaultService
    "ConditionCheckFailureException"

-- | There was a version conflict.
_VersionMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_VersionMismatchException =
  Core._MatchServiceError
    defaultService
    "VersionMismatchException"

-- | A specified entity does not exist
_EntityNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityNotFoundException =
  Core._MatchServiceError
    defaultService
    "EntityNotFoundException"

-- | The specified scheduler is transitioning.
_SchedulerTransitioningException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SchedulerTransitioningException =
  Core._MatchServiceError
    defaultService
    "SchedulerTransitioningException"

-- | Prism for IllegalBlueprintStateException' errors.
_IllegalBlueprintStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IllegalBlueprintStateException =
  Core._MatchServiceError
    defaultService
    "IllegalBlueprintStateException"

-- | A value could not be validated.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | An internal service error occurred.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The operation timed out.
_OperationTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationTimeoutException =
  Core._MatchServiceError
    defaultService
    "OperationTimeoutException"

-- | The specified crawler is stopping.
_CrawlerStoppingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CrawlerStoppingException =
  Core._MatchServiceError
    defaultService
    "CrawlerStoppingException"

-- | The operation cannot be performed because the crawler is already
-- running.
_CrawlerRunningException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CrawlerRunningException =
  Core._MatchServiceError
    defaultService
    "CrawlerRunningException"

-- | There is no applicable schedule.
_NoScheduleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoScheduleException =
  Core._MatchServiceError
    defaultService
    "NoScheduleException"

-- | The same unique identifier was associated with two different records.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"
