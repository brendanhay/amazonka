{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Glue__
--
-- Defines the public endpoint for the AWS Glue service.
module Network.AWS.Glue
  ( -- * Service configuration
    glueService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StartImportLabelsTaskRun
    module Network.AWS.Glue.StartImportLabelsTaskRun,

    -- ** UpdateMLTransform
    module Network.AWS.Glue.UpdateMLTransform,

    -- ** UpdateRegistry
    module Network.AWS.Glue.UpdateRegistry,

    -- ** DeleteRegistry
    module Network.AWS.Glue.DeleteRegistry,

    -- ** DeleteMLTransform
    module Network.AWS.Glue.DeleteMLTransform,

    -- ** StartCrawler
    module Network.AWS.Glue.StartCrawler,

    -- ** GetCatalogImportStatus
    module Network.AWS.Glue.GetCatalogImportStatus,

    -- ** ListMLTransforms
    module Network.AWS.Glue.ListMLTransforms,

    -- ** GetPartition
    module Network.AWS.Glue.GetPartition,

    -- ** QuerySchemaVersionMetadata
    module Network.AWS.Glue.QuerySchemaVersionMetadata,

    -- ** CreateTrigger
    module Network.AWS.Glue.CreateTrigger,

    -- ** CheckSchemaVersionValidity
    module Network.AWS.Glue.CheckSchemaVersionValidity,

    -- ** DeleteTable
    module Network.AWS.Glue.DeleteTable,

    -- ** UpdateTable
    module Network.AWS.Glue.UpdateTable,

    -- ** GetWorkflowRuns
    module Network.AWS.Glue.GetWorkflowRuns,

    -- ** CreateWorkflow
    module Network.AWS.Glue.CreateWorkflow,

    -- ** UpdateColumnStatisticsForTable
    module Network.AWS.Glue.UpdateColumnStatisticsForTable,

    -- ** DeleteColumnStatisticsForTable
    module Network.AWS.Glue.DeleteColumnStatisticsForTable,

    -- ** DeleteConnection
    module Network.AWS.Glue.DeleteConnection,

    -- ** UpdateConnection
    module Network.AWS.Glue.UpdateConnection,

    -- ** GetUserDefinedFunctions (Paginated)
    module Network.AWS.Glue.GetUserDefinedFunctions,

    -- ** GetTags
    module Network.AWS.Glue.GetTags,

    -- ** GetDataCatalogEncryptionSettings
    module Network.AWS.Glue.GetDataCatalogEncryptionSettings,

    -- ** BatchCreatePartition
    module Network.AWS.Glue.BatchCreatePartition,

    -- ** GetMapping
    module Network.AWS.Glue.GetMapping,

    -- ** DeleteWorkflow
    module Network.AWS.Glue.DeleteWorkflow,

    -- ** UpdateWorkflow
    module Network.AWS.Glue.UpdateWorkflow,

    -- ** GetTableVersion
    module Network.AWS.Glue.GetTableVersion,

    -- ** CreateSecurityConfiguration
    module Network.AWS.Glue.CreateSecurityConfiguration,

    -- ** StartWorkflowRun
    module Network.AWS.Glue.StartWorkflowRun,

    -- ** GetJobs (Paginated)
    module Network.AWS.Glue.GetJobs,

    -- ** BatchGetWorkflows
    module Network.AWS.Glue.BatchGetWorkflows,

    -- ** GetClassifiers (Paginated)
    module Network.AWS.Glue.GetClassifiers,

    -- ** GetResourcePolicies (Paginated)
    module Network.AWS.Glue.GetResourcePolicies,

    -- ** CreateConnection
    module Network.AWS.Glue.CreateConnection,

    -- ** ListSchemaVersions (Paginated)
    module Network.AWS.Glue.ListSchemaVersions,

    -- ** GetWorkflowRunProperties
    module Network.AWS.Glue.GetWorkflowRunProperties,

    -- ** BatchGetDevEndpoints
    module Network.AWS.Glue.BatchGetDevEndpoints,

    -- ** DeletePartitionIndex
    module Network.AWS.Glue.DeletePartitionIndex,

    -- ** DeleteTableVersion
    module Network.AWS.Glue.DeleteTableVersion,

    -- ** DeleteDevEndpoint
    module Network.AWS.Glue.DeleteDevEndpoint,

    -- ** UpdateDevEndpoint
    module Network.AWS.Glue.UpdateDevEndpoint,

    -- ** GetWorkflow
    module Network.AWS.Glue.GetWorkflow,

    -- ** BatchGetCrawlers
    module Network.AWS.Glue.BatchGetCrawlers,

    -- ** GetJobBookmark
    module Network.AWS.Glue.GetJobBookmark,

    -- ** DeleteCrawler
    module Network.AWS.Glue.DeleteCrawler,

    -- ** UpdateCrawler
    module Network.AWS.Glue.UpdateCrawler,

    -- ** StartExportLabelsTaskRun
    module Network.AWS.Glue.StartExportLabelsTaskRun,

    -- ** GetSecurityConfiguration
    module Network.AWS.Glue.GetSecurityConfiguration,

    -- ** CreatePartitionIndex
    module Network.AWS.Glue.CreatePartitionIndex,

    -- ** RemoveSchemaVersionMetadata
    module Network.AWS.Glue.RemoveSchemaVersionMetadata,

    -- ** ListSchemas (Paginated)
    module Network.AWS.Glue.ListSchemas,

    -- ** GetConnection
    module Network.AWS.Glue.GetConnection,

    -- ** GetColumnStatisticsForTable
    module Network.AWS.Glue.GetColumnStatisticsForTable,

    -- ** BatchGetPartition
    module Network.AWS.Glue.BatchGetPartition,

    -- ** StopTrigger
    module Network.AWS.Glue.StopTrigger,

    -- ** UpdateCrawlerSchedule
    module Network.AWS.Glue.UpdateCrawlerSchedule,

    -- ** StartMLEvaluationTaskRun
    module Network.AWS.Glue.StartMLEvaluationTaskRun,

    -- ** DeleteUserDefinedFunction
    module Network.AWS.Glue.DeleteUserDefinedFunction,

    -- ** UpdateUserDefinedFunction
    module Network.AWS.Glue.UpdateUserDefinedFunction,

    -- ** GetRegistry
    module Network.AWS.Glue.GetRegistry,

    -- ** BatchDeleteTable
    module Network.AWS.Glue.BatchDeleteTable,

    -- ** CancelMLTaskRun
    module Network.AWS.Glue.CancelMLTaskRun,

    -- ** GetTables (Paginated)
    module Network.AWS.Glue.GetTables,

    -- ** ResumeWorkflowRun
    module Network.AWS.Glue.ResumeWorkflowRun,

    -- ** CreateClassifier
    module Network.AWS.Glue.CreateClassifier,

    -- ** BatchDeleteConnection
    module Network.AWS.Glue.BatchDeleteConnection,

    -- ** CreateJob
    module Network.AWS.Glue.CreateJob,

    -- ** GetJobRuns (Paginated)
    module Network.AWS.Glue.GetJobRuns,

    -- ** CreateUserDefinedFunction
    module Network.AWS.Glue.CreateUserDefinedFunction,

    -- ** ResetJobBookmark
    module Network.AWS.Glue.ResetJobBookmark,

    -- ** ListJobs
    module Network.AWS.Glue.ListJobs,

    -- ** DeleteJob
    module Network.AWS.Glue.DeleteJob,

    -- ** UpdateJob
    module Network.AWS.Glue.UpdateJob,

    -- ** CreateRegistry
    module Network.AWS.Glue.CreateRegistry,

    -- ** GetCrawlers (Paginated)
    module Network.AWS.Glue.GetCrawlers,

    -- ** ListTriggers
    module Network.AWS.Glue.ListTriggers,

    -- ** GetClassifier
    module Network.AWS.Glue.GetClassifier,

    -- ** GetJob
    module Network.AWS.Glue.GetJob,

    -- ** ListRegistries (Paginated)
    module Network.AWS.Glue.ListRegistries,

    -- ** BatchDeleteTableVersion
    module Network.AWS.Glue.BatchDeleteTableVersion,

    -- ** GetDevEndpoints (Paginated)
    module Network.AWS.Glue.GetDevEndpoints,

    -- ** StartCrawlerSchedule
    module Network.AWS.Glue.StartCrawlerSchedule,

    -- ** GetPartitionIndexes (Paginated)
    module Network.AWS.Glue.GetPartitionIndexes,

    -- ** GetUserDefinedFunction
    module Network.AWS.Glue.GetUserDefinedFunction,

    -- ** GetResourcePolicy
    module Network.AWS.Glue.GetResourcePolicy,

    -- ** GetWorkflowRun
    module Network.AWS.Glue.GetWorkflowRun,

    -- ** DeleteDatabase
    module Network.AWS.Glue.DeleteDatabase,

    -- ** UpdateDatabase
    module Network.AWS.Glue.UpdateDatabase,

    -- ** GetColumnStatisticsForPartition
    module Network.AWS.Glue.GetColumnStatisticsForPartition,

    -- ** StopCrawler
    module Network.AWS.Glue.StopCrawler,

    -- ** DeleteSecurityConfiguration
    module Network.AWS.Glue.DeleteSecurityConfiguration,

    -- ** GetPartitions (Paginated)
    module Network.AWS.Glue.GetPartitions,

    -- ** PutSchemaVersionMetadata
    module Network.AWS.Glue.PutSchemaVersionMetadata,

    -- ** GetSchema
    module Network.AWS.Glue.GetSchema,

    -- ** BatchDeletePartition
    module Network.AWS.Glue.BatchDeletePartition,

    -- ** StartMLLabelingSetGenerationTaskRun
    module Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun,

    -- ** BatchUpdatePartition
    module Network.AWS.Glue.BatchUpdatePartition,

    -- ** RegisterSchemaVersion
    module Network.AWS.Glue.RegisterSchemaVersion,

    -- ** StopWorkflowRun
    module Network.AWS.Glue.StopWorkflowRun,

    -- ** GetCrawler
    module Network.AWS.Glue.GetCrawler,

    -- ** ListWorkflows
    module Network.AWS.Glue.ListWorkflows,

    -- ** BatchStopJobRun
    module Network.AWS.Glue.BatchStopJobRun,

    -- ** GetDevEndpoint
    module Network.AWS.Glue.GetDevEndpoint,

    -- ** PutWorkflowRunProperties
    module Network.AWS.Glue.PutWorkflowRunProperties,

    -- ** CreateTable
    module Network.AWS.Glue.CreateTable,

    -- ** ListCrawlers
    module Network.AWS.Glue.ListCrawlers,

    -- ** GetCrawlerMetrics (Paginated)
    module Network.AWS.Glue.GetCrawlerMetrics,

    -- ** GetSchemaVersion
    module Network.AWS.Glue.GetSchemaVersion,

    -- ** GetPlan
    module Network.AWS.Glue.GetPlan,

    -- ** GetTriggers (Paginated)
    module Network.AWS.Glue.GetTriggers,

    -- ** CreateSchema
    module Network.AWS.Glue.CreateSchema,

    -- ** ListDevEndpoints
    module Network.AWS.Glue.ListDevEndpoints,

    -- ** StartTrigger
    module Network.AWS.Glue.StartTrigger,

    -- ** GetDataflowGraph
    module Network.AWS.Glue.GetDataflowGraph,

    -- ** GetDatabases (Paginated)
    module Network.AWS.Glue.GetDatabases,

    -- ** GetTable
    module Network.AWS.Glue.GetTable,

    -- ** CreateCrawler
    module Network.AWS.Glue.CreateCrawler,

    -- ** GetJobRun
    module Network.AWS.Glue.GetJobRun,

    -- ** CreateDevEndpoint
    module Network.AWS.Glue.CreateDevEndpoint,

    -- ** GetMLTaskRuns
    module Network.AWS.Glue.GetMLTaskRuns,

    -- ** TagResource
    module Network.AWS.Glue.TagResource,

    -- ** PutDataCatalogEncryptionSettings
    module Network.AWS.Glue.PutDataCatalogEncryptionSettings,

    -- ** GetMLTransforms
    module Network.AWS.Glue.GetMLTransforms,

    -- ** UpdateSchema
    module Network.AWS.Glue.UpdateSchema,

    -- ** DeleteSchema
    module Network.AWS.Glue.DeleteSchema,

    -- ** GetDatabase
    module Network.AWS.Glue.GetDatabase,

    -- ** DeleteColumnStatisticsForPartition
    module Network.AWS.Glue.DeleteColumnStatisticsForPartition,

    -- ** UpdateColumnStatisticsForPartition
    module Network.AWS.Glue.UpdateColumnStatisticsForPartition,

    -- ** GetMLTaskRun
    module Network.AWS.Glue.GetMLTaskRun,

    -- ** DeletePartition
    module Network.AWS.Glue.DeletePartition,

    -- ** UpdatePartition
    module Network.AWS.Glue.UpdatePartition,

    -- ** GetMLTransform
    module Network.AWS.Glue.GetMLTransform,

    -- ** CreateScript
    module Network.AWS.Glue.CreateScript,

    -- ** PutResourcePolicy
    module Network.AWS.Glue.PutResourcePolicy,

    -- ** GetSecurityConfigurations (Paginated)
    module Network.AWS.Glue.GetSecurityConfigurations,

    -- ** DeleteResourcePolicy
    module Network.AWS.Glue.DeleteResourcePolicy,

    -- ** GetConnections (Paginated)
    module Network.AWS.Glue.GetConnections,

    -- ** UntagResource
    module Network.AWS.Glue.UntagResource,

    -- ** GetSchemaVersionsDiff
    module Network.AWS.Glue.GetSchemaVersionsDiff,

    -- ** SearchTables
    module Network.AWS.Glue.SearchTables,

    -- ** GetTrigger
    module Network.AWS.Glue.GetTrigger,

    -- ** BatchGetJobs
    module Network.AWS.Glue.BatchGetJobs,

    -- ** ImportCatalogToGlue
    module Network.AWS.Glue.ImportCatalogToGlue,

    -- ** DeleteClassifier
    module Network.AWS.Glue.DeleteClassifier,

    -- ** UpdateClassifier
    module Network.AWS.Glue.UpdateClassifier,

    -- ** StartJobRun
    module Network.AWS.Glue.StartJobRun,

    -- ** CreatePartition
    module Network.AWS.Glue.CreatePartition,

    -- ** BatchGetTriggers
    module Network.AWS.Glue.BatchGetTriggers,

    -- ** StopCrawlerSchedule
    module Network.AWS.Glue.StopCrawlerSchedule,

    -- ** GetSchemaByDefinition
    module Network.AWS.Glue.GetSchemaByDefinition,

    -- ** CreateDatabase
    module Network.AWS.Glue.CreateDatabase,

    -- ** GetTableVersions (Paginated)
    module Network.AWS.Glue.GetTableVersions,

    -- ** CreateMLTransform
    module Network.AWS.Glue.CreateMLTransform,

    -- ** DeleteSchemaVersions
    module Network.AWS.Glue.DeleteSchemaVersions,

    -- ** DeleteTrigger
    module Network.AWS.Glue.DeleteTrigger,

    -- ** UpdateTrigger
    module Network.AWS.Glue.UpdateTrigger,

    -- * Types

    -- ** BackfillErrorCode
    BackfillErrorCode (..),

    -- ** CSVHeaderOption
    CSVHeaderOption (..),

    -- ** CatalogEncryptionMode
    CatalogEncryptionMode (..),

    -- ** CloudWatchEncryptionMode
    CloudWatchEncryptionMode (..),

    -- ** ColumnStatisticsType
    ColumnStatisticsType (..),

    -- ** Comparator
    Comparator (..),

    -- ** Compatibility
    Compatibility (..),

    -- ** ConnectionPropertyKey
    ConnectionPropertyKey (..),

    -- ** ConnectionType
    ConnectionType (..),

    -- ** CrawlState
    CrawlState (..),

    -- ** CrawlerLineageSettings
    CrawlerLineageSettings (..),

    -- ** CrawlerState
    CrawlerState (..),

    -- ** DataFormat
    DataFormat (..),

    -- ** DeleteBehavior
    DeleteBehavior (..),

    -- ** EnableHybridValues
    EnableHybridValues (..),

    -- ** ExistCondition
    ExistCondition (..),

    -- ** JobBookmarksEncryptionMode
    JobBookmarksEncryptionMode (..),

    -- ** JobRunState
    JobRunState (..),

    -- ** Language
    Language (..),

    -- ** LastCrawlStatus
    LastCrawlStatus (..),

    -- ** Logical
    Logical (..),

    -- ** LogicalOperator
    LogicalOperator (..),

    -- ** MLUserDataEncryptionModeString
    MLUserDataEncryptionModeString (..),

    -- ** NodeType
    NodeType (..),

    -- ** PartitionIndexStatus
    PartitionIndexStatus (..),

    -- ** Permission
    Permission (..),

    -- ** PrincipalType
    PrincipalType (..),

    -- ** RecrawlBehavior
    RecrawlBehavior (..),

    -- ** RegistryStatus
    RegistryStatus (..),

    -- ** ResourceShareType
    ResourceShareType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** S3EncryptionMode
    S3EncryptionMode (..),

    -- ** ScheduleState
    ScheduleState (..),

    -- ** SchemaDiffType
    SchemaDiffType (..),

    -- ** SchemaStatus
    SchemaStatus (..),

    -- ** SchemaVersionStatus
    SchemaVersionStatus (..),

    -- ** Sort
    Sort (..),

    -- ** SortDirectionType
    SortDirectionType (..),

    -- ** TaskRunSortColumnType
    TaskRunSortColumnType (..),

    -- ** TaskStatusType
    TaskStatusType (..),

    -- ** TaskType
    TaskType (..),

    -- ** TransformSortColumnType
    TransformSortColumnType (..),

    -- ** TransformStatusType
    TransformStatusType (..),

    -- ** TransformType
    TransformType (..),

    -- ** TriggerState
    TriggerState (..),

    -- ** TriggerType
    TriggerType (..),

    -- ** UpdateBehavior
    UpdateBehavior (..),

    -- ** WorkerType
    WorkerType (..),

    -- ** WorkflowRunStatus
    WorkflowRunStatus (..),

    -- ** Action
    Action (..),
    mkAction,
    aNotificationProperty,
    aArguments,
    aJobName,
    aSecurityConfiguration,
    aTimeout,
    aCrawlerName,

    -- ** BackfillError
    BackfillError (..),
    mkBackfillError,
    bePartitions,
    beCode,

    -- ** BatchStopJobRunError
    BatchStopJobRunError (..),
    mkBatchStopJobRunError,
    bsjreJobName,
    bsjreJobRunId,
    bsjreErrorDetail,

    -- ** BatchStopJobRunSuccessfulSubmission
    BatchStopJobRunSuccessfulSubmission (..),
    mkBatchStopJobRunSuccessfulSubmission,
    bsjrssJobName,
    bsjrssJobRunId,

    -- ** BatchUpdatePartitionFailureEntry
    BatchUpdatePartitionFailureEntry (..),
    mkBatchUpdatePartitionFailureEntry,
    bupfePartitionValueList,
    bupfeErrorDetail,

    -- ** BatchUpdatePartitionRequestEntry
    BatchUpdatePartitionRequestEntry (..),
    mkBatchUpdatePartitionRequestEntry,
    buprePartitionValueList,
    buprePartitionInput,

    -- ** BinaryColumnStatisticsData
    BinaryColumnStatisticsData (..),
    mkBinaryColumnStatisticsData,
    bcsdMaximumLength,
    bcsdAverageLength,
    bcsdNumberOfNulls,

    -- ** BooleanColumnStatisticsData
    BooleanColumnStatisticsData (..),
    mkBooleanColumnStatisticsData,
    bNumberOfTrues,
    bNumberOfFalses,
    bNumberOfNulls,

    -- ** CSVClassifier
    CSVClassifier (..),
    mkCSVClassifier,
    csvcCreationTime,
    csvcQuoteSymbol,
    csvcContainsHeader,
    csvcLastUpdated,
    csvcDisableValueTrimming,
    csvcHeader,
    csvcVersion,
    csvcAllowSingleColumn,
    csvcDelimiter,
    csvcName,

    -- ** CatalogEntry
    CatalogEntry (..),
    mkCatalogEntry,
    ceDatabaseName,
    ceTableName,

    -- ** CatalogImportStatus
    CatalogImportStatus (..),
    mkCatalogImportStatus,
    cisImportedBy,
    cisImportTime,
    cisImportCompleted,

    -- ** CatalogTarget
    CatalogTarget (..),
    mkCatalogTarget,
    ctDatabaseName,
    ctTables,

    -- ** Classifier
    Classifier (..),
    mkClassifier,
    cGrokClassifier,
    cXMLClassifier,
    cCSVClassifier,
    cJSONClassifier,

    -- ** CloudWatchEncryption
    CloudWatchEncryption (..),
    mkCloudWatchEncryption,
    cweCloudWatchEncryptionMode,
    cweKMSKeyARN,

    -- ** CodeGenEdge
    CodeGenEdge (..),
    mkCodeGenEdge,
    cgeTargetParameter,
    cgeSource,
    cgeTarget,

    -- ** CodeGenNode
    CodeGenNode (..),
    mkCodeGenNode,
    cgnLineNumber,
    cgnId,
    cgnNodeType,
    cgnArgs,

    -- ** CodeGenNodeArg
    CodeGenNodeArg (..),
    mkCodeGenNodeArg,
    cgnaParam,
    cgnaName,
    cgnaValue,

    -- ** Column
    Column (..),
    mkColumn,
    cParameters,
    cType,
    cComment,
    cName,

    -- ** ColumnError
    ColumnError (..),
    mkColumnError,
    ceError,
    ceColumnName,

    -- ** ColumnStatistics
    ColumnStatistics (..),
    mkColumnStatistics,
    csColumnName,
    csColumnType,
    csAnalyzedTime,
    csStatisticsData,

    -- ** ColumnStatisticsData
    ColumnStatisticsData (..),
    mkColumnStatisticsData,
    csdBinaryColumnStatisticsData,
    csdDateColumnStatisticsData,
    csdBooleanColumnStatisticsData,
    csdDecimalColumnStatisticsData,
    csdDoubleColumnStatisticsData,
    csdStringColumnStatisticsData,
    csdLongColumnStatisticsData,
    csdType,

    -- ** ColumnStatisticsError
    ColumnStatisticsError (..),
    mkColumnStatisticsError,
    cseError,
    cseColumnStatistics,

    -- ** Condition
    Condition (..),
    mkCondition,
    cCrawlState,
    cState,
    cJobName,
    cLogicalOperator,
    cCrawlerName,

    -- ** ConfusionMatrix
    ConfusionMatrix (..),
    mkConfusionMatrix,
    cmNumTrueNegatives,
    cmNumFalseNegatives,
    cmNumTruePositives,
    cmNumFalsePositives,

    -- ** Connection
    Connection (..),
    mkConnection,
    conCreationTime,
    conLastUpdatedBy,
    conConnectionProperties,
    conLastUpdatedTime,
    conMatchCriteria,
    conPhysicalConnectionRequirements,
    conName,
    conDescription,
    conConnectionType,

    -- ** ConnectionInput
    ConnectionInput (..),
    mkConnectionInput,
    ciMatchCriteria,
    ciPhysicalConnectionRequirements,
    ciDescription,
    ciName,
    ciConnectionType,
    ciConnectionProperties,

    -- ** ConnectionPasswordEncryption
    ConnectionPasswordEncryption (..),
    mkConnectionPasswordEncryption,
    cpeAWSKMSKeyId,
    cpeReturnConnectionPasswordEncrypted,

    -- ** ConnectionsList
    ConnectionsList (..),
    mkConnectionsList,
    clConnections,

    -- ** Crawl
    Crawl (..),
    mkCrawl,
    craCompletedOn,
    craState,
    craStartedOn,
    craLogStream,
    craLogGroup,
    craErrorMessage,

    -- ** Crawler
    Crawler (..),
    mkCrawler,
    ccCreationTime,
    ccState,
    ccSchemaChangePolicy,
    ccLastUpdated,
    ccSchedule,
    ccLastCrawl,
    ccCrawlElapsedTime,
    ccRecrawlPolicy,
    ccClassifiers,
    ccRole,
    ccName,
    ccTargets,
    ccVersion,
    ccDatabaseName,
    ccCrawlerSecurityConfiguration,
    ccLineageConfiguration,
    ccConfiguration,
    ccTablePrefix,
    ccDescription,

    -- ** CrawlerMetrics
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

    -- ** CrawlerNodeDetails
    CrawlerNodeDetails (..),
    mkCrawlerNodeDetails,
    cndCrawls,

    -- ** CrawlerTargets
    CrawlerTargets (..),
    mkCrawlerTargets,
    ctDynamoDBTargets,
    ctS3Targets,
    ctMongoDBTargets,
    ctCatalogTargets,
    ctJdbcTargets,

    -- ** CreateCSVClassifierRequest
    CreateCSVClassifierRequest (..),
    mkCreateCSVClassifierRequest,
    cccrQuoteSymbol,
    cccrContainsHeader,
    cccrDisableValueTrimming,
    cccrHeader,
    cccrAllowSingleColumn,
    cccrDelimiter,
    cccrName,

    -- ** CreateGrokClassifierRequest
    CreateGrokClassifierRequest (..),
    mkCreateGrokClassifierRequest,
    cgcrCustomPatterns,
    cgcrClassification,
    cgcrName,
    cgcrGrokPattern,

    -- ** CreateJSONClassifierRequest
    CreateJSONClassifierRequest (..),
    mkCreateJSONClassifierRequest,
    cjcrName,
    cjcrJSONPath,

    -- ** CreateXMLClassifierRequest
    CreateXMLClassifierRequest (..),
    mkCreateXMLClassifierRequest,
    cxcrRowTag,
    cxcrClassification,
    cxcrName,

    -- ** DataCatalogEncryptionSettings
    DataCatalogEncryptionSettings (..),
    mkDataCatalogEncryptionSettings,
    dcesEncryptionAtRest,
    dcesConnectionPasswordEncryption,

    -- ** DataLakePrincipal
    DataLakePrincipal (..),
    mkDataLakePrincipal,
    dlpDataLakePrincipalIdentifier,

    -- ** Database
    Database (..),
    mkDatabase,
    dLocationURI,
    dCatalogId,
    dTargetDatabase,
    dParameters,
    dDescription,
    dCreateTime,
    dCreateTableDefaultPermissions,
    dName,

    -- ** DatabaseIdentifier
    DatabaseIdentifier (..),
    mkDatabaseIdentifier,
    diCatalogId,
    diDatabaseName,

    -- ** DatabaseInput
    DatabaseInput (..),
    mkDatabaseInput,
    diLocationURI,
    diTargetDatabase,
    diParameters,
    diDescription,
    diCreateTableDefaultPermissions,
    diName,

    -- ** DateColumnStatisticsData
    DateColumnStatisticsData (..),
    mkDateColumnStatisticsData,
    dcsdMaximumValue,
    dcsdMinimumValue,
    dcsdNumberOfNulls,
    dcsdNumberOfDistinctValues,

    -- ** DecimalColumnStatisticsData
    DecimalColumnStatisticsData (..),
    mkDecimalColumnStatisticsData,
    dMaximumValue,
    dMinimumValue,
    dNumberOfNulls,
    dNumberOfDistinctValues,

    -- ** DecimalNumber
    DecimalNumber (..),
    mkDecimalNumber,
    dnUnscaledValue,
    dnScale,

    -- ** DevEndpoint
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

    -- ** DevEndpointCustomLibraries
    DevEndpointCustomLibraries (..),
    mkDevEndpointCustomLibraries,
    declExtraPythonLibsS3Path,
    declExtraJARsS3Path,

    -- ** DoubleColumnStatisticsData
    DoubleColumnStatisticsData (..),
    mkDoubleColumnStatisticsData,
    douMaximumValue,
    douMinimumValue,
    douNumberOfNulls,
    douNumberOfDistinctValues,

    -- ** DynamoDBTarget
    DynamoDBTarget (..),
    mkDynamoDBTarget,
    ddtPath,
    ddtScanRate,
    ddtScanAll,

    -- ** Edge
    Edge (..),
    mkEdge,
    eSourceId,
    eDestinationId,

    -- ** EncryptionAtRest
    EncryptionAtRest (..),
    mkEncryptionAtRest,
    earSseAWSKMSKeyId,
    earCatalogEncryptionMode,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecS3Encryption,
    ecJobBookmarksEncryption,
    ecCloudWatchEncryption,

    -- ** ErrorDetail
    ErrorDetail (..),
    mkErrorDetail,
    edErrorCode,
    edErrorMessage,

    -- ** ErrorDetails
    ErrorDetails (..),
    mkErrorDetails,
    eErrorCode,
    eErrorMessage,

    -- ** EvaluationMetrics
    EvaluationMetrics (..),
    mkEvaluationMetrics,
    emFindMatchesMetrics,
    emTransformType,

    -- ** ExecutionProperty
    ExecutionProperty (..),
    mkExecutionProperty,
    epMaxConcurrentRuns,

    -- ** ExportLabelsTaskRunProperties
    ExportLabelsTaskRunProperties (..),
    mkExportLabelsTaskRunProperties,
    eltrpOutputS3Path,

    -- ** FindMatchesMetrics
    FindMatchesMetrics (..),
    mkFindMatchesMetrics,
    fmmF1,
    fmmAreaUnderPRCurve,
    fmmRecall,
    fmmPrecision,
    fmmConfusionMatrix,

    -- ** FindMatchesParameters
    FindMatchesParameters (..),
    mkFindMatchesParameters,
    fmpEnforceProvidedLabels,
    fmpAccuracyCostTradeoff,
    fmpPrecisionRecallTradeoff,
    fmpPrimaryKeyColumnName,

    -- ** FindMatchesTaskRunProperties
    FindMatchesTaskRunProperties (..),
    mkFindMatchesTaskRunProperties,
    fmtrpJobId,
    fmtrpJobName,
    fmtrpJobRunId,

    -- ** GetConnectionsFilter
    GetConnectionsFilter (..),
    mkGetConnectionsFilter,
    gcfMatchCriteria,
    gcfConnectionType,

    -- ** GluePolicy
    GluePolicy (..),
    mkGluePolicy,
    gpPolicyInJSON,
    gpUpdateTime,
    gpPolicyHash,
    gpCreateTime,

    -- ** GlueTable
    GlueTable (..),
    mkGlueTable,
    gtCatalogId,
    gtConnectionName,
    gtDatabaseName,
    gtTableName,

    -- ** GrokClassifier
    GrokClassifier (..),
    mkGrokClassifier,
    gcCreationTime,
    gcLastUpdated,
    gcVersion,
    gcCustomPatterns,
    gcName,
    gcClassification,
    gcGrokPattern,

    -- ** ImportLabelsTaskRunProperties
    ImportLabelsTaskRunProperties (..),
    mkImportLabelsTaskRunProperties,
    iltrpReplace,
    iltrpInputS3Path,

    -- ** JSONClassifier
    JSONClassifier (..),
    mkJSONClassifier,
    jcCreationTime,
    jcLastUpdated,
    jcVersion,
    jcName,
    jcJSONPath,

    -- ** JdbcTarget
    JdbcTarget (..),
    mkJdbcTarget,
    jtPath,
    jtConnectionName,
    jtExclusions,

    -- ** Job
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

    -- ** JobBookmarkEntry
    JobBookmarkEntry (..),
    mkJobBookmarkEntry,
    jbeJobName,
    jbeRun,
    jbeRunId,
    jbeVersion,
    jbePreviousRunId,
    jbeAttempt,
    jbeJobBookmark,

    -- ** JobBookmarksEncryption
    JobBookmarksEncryption (..),
    mkJobBookmarksEncryption,
    jbeJobBookmarksEncryptionMode,
    jbeKMSKeyARN,

    -- ** JobCommand
    JobCommand (..),
    mkJobCommand,
    jobScriptLocation,
    jobPythonVersion,
    jobName,

    -- ** JobNodeDetails
    JobNodeDetails (..),
    mkJobNodeDetails,
    jndJobRuns,

    -- ** JobRun
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

    -- ** JobUpdate
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

    -- ** KeySchemaElement
    KeySchemaElement (..),
    mkKeySchemaElement,
    kseName,
    kseType,

    -- ** LabelingSetGenerationTaskRunProperties
    LabelingSetGenerationTaskRunProperties (..),
    mkLabelingSetGenerationTaskRunProperties,
    lsgtrpOutputS3Path,

    -- ** LastCrawlInfo
    LastCrawlInfo (..),
    mkLastCrawlInfo,
    lciStatus,
    lciStartTime,
    lciLogStream,
    lciLogGroup,
    lciMessagePrefix,
    lciErrorMessage,

    -- ** LineageConfiguration
    LineageConfiguration (..),
    mkLineageConfiguration,
    lcCrawlerLineageSettings,

    -- ** Location
    Location (..),
    mkLocation,
    lDynamoDB,
    lJdbc,
    lS3,

    -- ** LongColumnStatisticsData
    LongColumnStatisticsData (..),
    mkLongColumnStatisticsData,
    lcsdMaximumValue,
    lcsdMinimumValue,
    lcsdNumberOfNulls,
    lcsdNumberOfDistinctValues,

    -- ** MLTransform
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

    -- ** MLUserDataEncryption
    MLUserDataEncryption (..),
    mkMLUserDataEncryption,
    mludeKMSKeyId,
    mludeMlUserDataEncryptionMode,

    -- ** MappingEntry
    MappingEntry (..),
    mkMappingEntry,
    meTargetTable,
    meSourceType,
    meSourceTable,
    meTargetType,
    meTargetPath,
    meSourcePath,

    -- ** MetadataInfo
    MetadataInfo (..),
    mkMetadataInfo,
    miCreatedTime,
    miMetadataValue,

    -- ** MetadataKeyValuePair
    MetadataKeyValuePair (..),
    mkMetadataKeyValuePair,
    mkvpMetadataKey,
    mkvpMetadataValue,

    -- ** MongoDBTarget
    MongoDBTarget (..),
    mkMongoDBTarget,
    mdtPath,
    mdtConnectionName,
    mdtScanAll,

    -- ** Node
    Node (..),
    mkNode,
    nTriggerDetails,
    nUniqueId,
    nCrawlerDetails,
    nName,
    nJobDetails,
    nType,

    -- ** NotificationProperty
    NotificationProperty (..),
    mkNotificationProperty,
    npNotifyDelayAfter,

    -- ** Order
    Order (..),
    mkOrder,
    oColumn,
    oSortOrder,

    -- ** Partition
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

    -- ** PartitionError
    PartitionError (..),
    mkPartitionError,
    pePartitionValues,
    peErrorDetail,

    -- ** PartitionIndex
    PartitionIndex (..),
    mkPartitionIndex,
    piKeys,
    piIndexName,

    -- ** PartitionIndexDescriptor
    PartitionIndexDescriptor (..),
    mkPartitionIndexDescriptor,
    pidBackfillErrors,
    pidIndexName,
    pidKeys,
    pidIndexStatus,

    -- ** PartitionInput
    PartitionInput (..),
    mkPartitionInput,
    piValues,
    piLastAnalyzedTime,
    piStorageDescriptor,
    piParameters,
    piLastAccessTime,

    -- ** PartitionValueList
    PartitionValueList (..),
    mkPartitionValueList,
    pvlValues,

    -- ** PhysicalConnectionRequirements
    PhysicalConnectionRequirements (..),
    mkPhysicalConnectionRequirements,
    pcrSecurityGroupIdList,
    pcrSubnetId,
    pcrAvailabilityZone,

    -- ** Predecessor
    Predecessor (..),
    mkPredecessor,
    pJobName,
    pRunId,

    -- ** Predicate
    Predicate (..),
    mkPredicate,
    pLogical,
    pConditions,

    -- ** PrincipalPermissions
    PrincipalPermissions (..),
    mkPrincipalPermissions,
    ppPrincipal,
    ppPermissions,

    -- ** PropertyPredicate
    PropertyPredicate (..),
    mkPropertyPredicate,
    ppValue,
    ppKey,
    ppComparator,

    -- ** RecrawlPolicy
    RecrawlPolicy (..),
    mkRecrawlPolicy,
    rpRecrawlBehavior,

    -- ** RegistryId
    RegistryId (..),
    mkRegistryId,
    riRegistryName,
    riRegistryARN,

    -- ** RegistryListItem
    RegistryListItem (..),
    mkRegistryListItem,
    rliStatus,
    rliRegistryName,
    rliCreatedTime,
    rliRegistryARN,
    rliUpdatedTime,
    rliDescription,

    -- ** ResourceURI
    ResourceURI (..),
    mkResourceURI,
    ruResourceType,
    ruURI,

    -- ** S3Encryption
    S3Encryption (..),
    mkS3Encryption,
    seS3EncryptionMode,
    seKMSKeyARN,

    -- ** S3Target
    S3Target (..),
    mkS3Target,
    stPath,
    stConnectionName,
    stExclusions,

    -- ** Schedule
    Schedule (..),
    mkSchedule,
    sState,
    sScheduleExpression,

    -- ** SchemaChangePolicy
    SchemaChangePolicy (..),
    mkSchemaChangePolicy,
    scpDeleteBehavior,
    scpUpdateBehavior,

    -- ** SchemaColumn
    SchemaColumn (..),
    mkSchemaColumn,
    sName,
    sDataType,

    -- ** SchemaId
    SchemaId (..),
    mkSchemaId,
    siRegistryName,
    siSchemaName,
    siSchemaARN,

    -- ** SchemaListItem
    SchemaListItem (..),
    mkSchemaListItem,
    sliRegistryName,
    sliCreatedTime,
    sliSchemaStatus,
    sliSchemaName,
    sliSchemaARN,
    sliUpdatedTime,
    sliDescription,

    -- ** SchemaReference
    SchemaReference (..),
    mkSchemaReference,
    srSchemaVersionId,
    srSchemaId,
    srSchemaVersionNumber,

    -- ** SchemaVersionErrorItem
    SchemaVersionErrorItem (..),
    mkSchemaVersionErrorItem,
    sveiVersionNumber,
    sveiErrorDetails,

    -- ** SchemaVersionListItem
    SchemaVersionListItem (..),
    mkSchemaVersionListItem,
    svliStatus,
    svliCreatedTime,
    svliSchemaVersionId,
    svliVersionNumber,
    svliSchemaARN,

    -- ** SchemaVersionNumber
    SchemaVersionNumber (..),
    mkSchemaVersionNumber,
    svnVersionNumber,
    svnLatestVersion,

    -- ** SecurityConfiguration
    SecurityConfiguration (..),
    mkSecurityConfiguration,
    secName,
    secEncryptionConfiguration,
    secCreatedTimeStamp,

    -- ** Segment
    Segment (..),
    mkSegment,
    sSegmentNumber,
    sTotalSegments,

    -- ** SerDeInfo
    SerDeInfo (..),
    mkSerDeInfo,
    sdiSerializationLibrary,
    sdiName,
    sdiParameters,

    -- ** SkewedInfo
    SkewedInfo (..),
    mkSkewedInfo,
    siSkewedColumnValueLocationMaps,
    siSkewedColumnValues,
    siSkewedColumnNames,

    -- ** SortCriterion
    SortCriterion (..),
    mkSortCriterion,
    scSort,
    scFieldName,

    -- ** StorageDescriptor
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

    -- ** StringColumnStatisticsData
    StringColumnStatisticsData (..),
    mkStringColumnStatisticsData,
    scsdMaximumLength,
    scsdAverageLength,
    scsdNumberOfNulls,
    scsdNumberOfDistinctValues,

    -- ** Table
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
    tStorageDescriptor,
    tDatabaseName,
    tParameters,
    tLastAccessTime,
    tDescription,
    tPartitionKeys,
    tCreateTime,
    tName,

    -- ** TableError
    TableError (..),
    mkTableError,
    teTableName,
    teErrorDetail,

    -- ** TableIdentifier
    TableIdentifier (..),
    mkTableIdentifier,
    tiCatalogId,
    tiName,
    tiDatabaseName,

    -- ** TableInput
    TableInput (..),
    mkTableInput,
    tabRetention,
    tabTargetTable,
    tabTableType,
    tabOwner,
    tabViewOriginalText,
    tabViewExpandedText,
    tabLastAnalyzedTime,
    tabStorageDescriptor,
    tabParameters,
    tabLastAccessTime,
    tabDescription,
    tabPartitionKeys,
    tabName,

    -- ** TableVersion
    TableVersion (..),
    mkTableVersion,
    tvVersionId,
    tvTable,

    -- ** TableVersionError
    TableVersionError (..),
    mkTableVersionError,
    tveVersionId,
    tveTableName,
    tveErrorDetail,

    -- ** TaskRun
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

    -- ** TaskRunFilterCriteria
    TaskRunFilterCriteria (..),
    mkTaskRunFilterCriteria,
    trfcStatus,
    trfcStartedAfter,
    trfcStartedBefore,
    trfcTaskRunType,

    -- ** TaskRunProperties
    TaskRunProperties (..),
    mkTaskRunProperties,
    trpTaskType,
    trpExportLabelsTaskRunProperties,
    trpLabelingSetGenerationTaskRunProperties,
    trpFindMatchesTaskRunProperties,
    trpImportLabelsTaskRunProperties,

    -- ** TaskRunSortCriteria
    TaskRunSortCriteria (..),
    mkTaskRunSortCriteria,
    trscColumn,
    trscSortDirection,

    -- ** TransformEncryption
    TransformEncryption (..),
    mkTransformEncryption,
    teMlUserDataEncryption,
    teTaskRunSecurityConfigurationName,

    -- ** TransformFilterCriteria
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

    -- ** TransformParameters
    TransformParameters (..),
    mkTransformParameters,
    tpFindMatchesParameters,
    tpTransformType,

    -- ** TransformSortCriteria
    TransformSortCriteria (..),
    mkTransformSortCriteria,
    tscColumn,
    tscSortDirection,

    -- ** Trigger
    Trigger (..),
    mkTrigger,
    triWorkflowName,
    triState,
    triActions,
    triSchedule,
    triPredicate,
    triName,
    triId,
    triType,
    triDescription,

    -- ** TriggerNodeDetails
    TriggerNodeDetails (..),
    mkTriggerNodeDetails,
    tndTrigger,

    -- ** TriggerUpdate
    TriggerUpdate (..),
    mkTriggerUpdate,
    tuActions,
    tuSchedule,
    tuPredicate,
    tuName,
    tuDescription,

    -- ** UpdateCSVClassifierRequest
    UpdateCSVClassifierRequest (..),
    mkUpdateCSVClassifierRequest,
    uccrQuoteSymbol,
    uccrContainsHeader,
    uccrDisableValueTrimming,
    uccrHeader,
    uccrAllowSingleColumn,
    uccrDelimiter,
    uccrName,

    -- ** UpdateGrokClassifierRequest
    UpdateGrokClassifierRequest (..),
    mkUpdateGrokClassifierRequest,
    ugcrClassification,
    ugcrCustomPatterns,
    ugcrGrokPattern,
    ugcrName,

    -- ** UpdateJSONClassifierRequest
    UpdateJSONClassifierRequest (..),
    mkUpdateJSONClassifierRequest,
    ujcrJSONPath,
    ujcrName,

    -- ** UpdateXMLClassifierRequest
    UpdateXMLClassifierRequest (..),
    mkUpdateXMLClassifierRequest,
    uxcrClassification,
    uxcrRowTag,
    uxcrName,

    -- ** UserDefinedFunction
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

    -- ** UserDefinedFunctionInput
    UserDefinedFunctionInput (..),
    mkUserDefinedFunctionInput,
    udfiOwnerName,
    udfiResourceURIs,
    udfiFunctionName,
    udfiOwnerType,
    udfiClassName,

    -- ** Workflow
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

    -- ** WorkflowGraph
    WorkflowGraph (..),
    mkWorkflowGraph,
    wgEdges,
    wgNodes,

    -- ** WorkflowRun
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

    -- ** WorkflowRunStatistics
    WorkflowRunStatistics (..),
    mkWorkflowRunStatistics,
    wrsRunningActions,
    wrsStoppedActions,
    wrsTotalActions,
    wrsFailedActions,
    wrsTimeoutActions,
    wrsSucceededActions,

    -- ** XMLClassifier
    XMLClassifier (..),
    mkXMLClassifier,
    xcCreationTime,
    xcLastUpdated,
    xcVersion,
    xcRowTag,
    xcName,
    xcClassification,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import Network.AWS.Glue.BatchCreatePartition
import Network.AWS.Glue.BatchDeleteConnection
import Network.AWS.Glue.BatchDeletePartition
import Network.AWS.Glue.BatchDeleteTable
import Network.AWS.Glue.BatchDeleteTableVersion
import Network.AWS.Glue.BatchGetCrawlers
import Network.AWS.Glue.BatchGetDevEndpoints
import Network.AWS.Glue.BatchGetJobs
import Network.AWS.Glue.BatchGetPartition
import Network.AWS.Glue.BatchGetTriggers
import Network.AWS.Glue.BatchGetWorkflows
import Network.AWS.Glue.BatchStopJobRun
import Network.AWS.Glue.BatchUpdatePartition
import Network.AWS.Glue.CancelMLTaskRun
import Network.AWS.Glue.CheckSchemaVersionValidity
import Network.AWS.Glue.CreateClassifier
import Network.AWS.Glue.CreateConnection
import Network.AWS.Glue.CreateCrawler
import Network.AWS.Glue.CreateDatabase
import Network.AWS.Glue.CreateDevEndpoint
import Network.AWS.Glue.CreateJob
import Network.AWS.Glue.CreateMLTransform
import Network.AWS.Glue.CreatePartition
import Network.AWS.Glue.CreatePartitionIndex
import Network.AWS.Glue.CreateRegistry
import Network.AWS.Glue.CreateSchema
import Network.AWS.Glue.CreateScript
import Network.AWS.Glue.CreateSecurityConfiguration
import Network.AWS.Glue.CreateTable
import Network.AWS.Glue.CreateTrigger
import Network.AWS.Glue.CreateUserDefinedFunction
import Network.AWS.Glue.CreateWorkflow
import Network.AWS.Glue.DeleteClassifier
import Network.AWS.Glue.DeleteColumnStatisticsForPartition
import Network.AWS.Glue.DeleteColumnStatisticsForTable
import Network.AWS.Glue.DeleteConnection
import Network.AWS.Glue.DeleteCrawler
import Network.AWS.Glue.DeleteDatabase
import Network.AWS.Glue.DeleteDevEndpoint
import Network.AWS.Glue.DeleteJob
import Network.AWS.Glue.DeleteMLTransform
import Network.AWS.Glue.DeletePartition
import Network.AWS.Glue.DeletePartitionIndex
import Network.AWS.Glue.DeleteRegistry
import Network.AWS.Glue.DeleteResourcePolicy
import Network.AWS.Glue.DeleteSchema
import Network.AWS.Glue.DeleteSchemaVersions
import Network.AWS.Glue.DeleteSecurityConfiguration
import Network.AWS.Glue.DeleteTable
import Network.AWS.Glue.DeleteTableVersion
import Network.AWS.Glue.DeleteTrigger
import Network.AWS.Glue.DeleteUserDefinedFunction
import Network.AWS.Glue.DeleteWorkflow
import Network.AWS.Glue.GetCatalogImportStatus
import Network.AWS.Glue.GetClassifier
import Network.AWS.Glue.GetClassifiers
import Network.AWS.Glue.GetColumnStatisticsForPartition
import Network.AWS.Glue.GetColumnStatisticsForTable
import Network.AWS.Glue.GetConnection
import Network.AWS.Glue.GetConnections
import Network.AWS.Glue.GetCrawler
import Network.AWS.Glue.GetCrawlerMetrics
import Network.AWS.Glue.GetCrawlers
import Network.AWS.Glue.GetDataCatalogEncryptionSettings
import Network.AWS.Glue.GetDatabase
import Network.AWS.Glue.GetDatabases
import Network.AWS.Glue.GetDataflowGraph
import Network.AWS.Glue.GetDevEndpoint
import Network.AWS.Glue.GetDevEndpoints
import Network.AWS.Glue.GetJob
import Network.AWS.Glue.GetJobBookmark
import Network.AWS.Glue.GetJobRun
import Network.AWS.Glue.GetJobRuns
import Network.AWS.Glue.GetJobs
import Network.AWS.Glue.GetMLTaskRun
import Network.AWS.Glue.GetMLTaskRuns
import Network.AWS.Glue.GetMLTransform
import Network.AWS.Glue.GetMLTransforms
import Network.AWS.Glue.GetMapping
import Network.AWS.Glue.GetPartition
import Network.AWS.Glue.GetPartitionIndexes
import Network.AWS.Glue.GetPartitions
import Network.AWS.Glue.GetPlan
import Network.AWS.Glue.GetRegistry
import Network.AWS.Glue.GetResourcePolicies
import Network.AWS.Glue.GetResourcePolicy
import Network.AWS.Glue.GetSchema
import Network.AWS.Glue.GetSchemaByDefinition
import Network.AWS.Glue.GetSchemaVersion
import Network.AWS.Glue.GetSchemaVersionsDiff
import Network.AWS.Glue.GetSecurityConfiguration
import Network.AWS.Glue.GetSecurityConfigurations
import Network.AWS.Glue.GetTable
import Network.AWS.Glue.GetTableVersion
import Network.AWS.Glue.GetTableVersions
import Network.AWS.Glue.GetTables
import Network.AWS.Glue.GetTags
import Network.AWS.Glue.GetTrigger
import Network.AWS.Glue.GetTriggers
import Network.AWS.Glue.GetUserDefinedFunction
import Network.AWS.Glue.GetUserDefinedFunctions
import Network.AWS.Glue.GetWorkflow
import Network.AWS.Glue.GetWorkflowRun
import Network.AWS.Glue.GetWorkflowRunProperties
import Network.AWS.Glue.GetWorkflowRuns
import Network.AWS.Glue.ImportCatalogToGlue
import Network.AWS.Glue.ListCrawlers
import Network.AWS.Glue.ListDevEndpoints
import Network.AWS.Glue.ListJobs
import Network.AWS.Glue.ListMLTransforms
import Network.AWS.Glue.ListRegistries
import Network.AWS.Glue.ListSchemaVersions
import Network.AWS.Glue.ListSchemas
import Network.AWS.Glue.ListTriggers
import Network.AWS.Glue.ListWorkflows
import Network.AWS.Glue.PutDataCatalogEncryptionSettings
import Network.AWS.Glue.PutResourcePolicy
import Network.AWS.Glue.PutSchemaVersionMetadata
import Network.AWS.Glue.PutWorkflowRunProperties
import Network.AWS.Glue.QuerySchemaVersionMetadata
import Network.AWS.Glue.RegisterSchemaVersion
import Network.AWS.Glue.RemoveSchemaVersionMetadata
import Network.AWS.Glue.ResetJobBookmark
import Network.AWS.Glue.ResumeWorkflowRun
import Network.AWS.Glue.SearchTables
import Network.AWS.Glue.StartCrawler
import Network.AWS.Glue.StartCrawlerSchedule
import Network.AWS.Glue.StartExportLabelsTaskRun
import Network.AWS.Glue.StartImportLabelsTaskRun
import Network.AWS.Glue.StartJobRun
import Network.AWS.Glue.StartMLEvaluationTaskRun
import Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun
import Network.AWS.Glue.StartTrigger
import Network.AWS.Glue.StartWorkflowRun
import Network.AWS.Glue.StopCrawler
import Network.AWS.Glue.StopCrawlerSchedule
import Network.AWS.Glue.StopTrigger
import Network.AWS.Glue.StopWorkflowRun
import Network.AWS.Glue.TagResource
import Network.AWS.Glue.Types
import Network.AWS.Glue.UntagResource
import Network.AWS.Glue.UpdateClassifier
import Network.AWS.Glue.UpdateColumnStatisticsForPartition
import Network.AWS.Glue.UpdateColumnStatisticsForTable
import Network.AWS.Glue.UpdateConnection
import Network.AWS.Glue.UpdateCrawler
import Network.AWS.Glue.UpdateCrawlerSchedule
import Network.AWS.Glue.UpdateDatabase
import Network.AWS.Glue.UpdateDevEndpoint
import Network.AWS.Glue.UpdateJob
import Network.AWS.Glue.UpdateMLTransform
import Network.AWS.Glue.UpdatePartition
import Network.AWS.Glue.UpdateRegistry
import Network.AWS.Glue.UpdateSchema
import Network.AWS.Glue.UpdateTable
import Network.AWS.Glue.UpdateTrigger
import Network.AWS.Glue.UpdateUserDefinedFunction
import Network.AWS.Glue.UpdateWorkflow
import Network.AWS.Glue.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Glue'.

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
