{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

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
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** ValidationException
    , _ValidationException

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** CrawlerRunningException
    , _CrawlerRunningException

    -- ** SchedulerTransitioningException
    , _SchedulerTransitioningException

    -- ** SchedulerRunningException
    , _SchedulerRunningException

    -- ** ConditionCheckFailureException
    , _ConditionCheckFailureException

    -- ** ConcurrentRunsExceededException
    , _ConcurrentRunsExceededException

    -- ** IllegalWorkflowStateException
    , _IllegalWorkflowStateException

    -- ** NoScheduleException
    , _NoScheduleException

    -- ** OperationTimeoutException
    , _OperationTimeoutException

    -- ** ConflictException
    , _ConflictException

    -- ** CrawlerNotRunningException
    , _CrawlerNotRunningException

    -- ** VersionMismatchException
    , _VersionMismatchException

    -- ** MLTransformNotReadyException
    , _MLTransformNotReadyException

    -- ** EntityNotFoundException
    , _EntityNotFoundException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** SchedulerNotRunningException
    , _SchedulerNotRunningException

    -- ** InternalServiceException
    , _InternalServiceException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** ResourceNumberLimitExceededException
    , _ResourceNumberLimitExceededException

    -- ** GlueEncryptionException
    , _GlueEncryptionException

    -- ** IdempotentParameterMismatchException
    , _IdempotentParameterMismatchException

    -- ** CrawlerStoppingException
    , _CrawlerStoppingException

    -- ** AlreadyExistsException
    , _AlreadyExistsException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StartImportLabelsTaskRun 
    , module Network.AWS.Glue.StartImportLabelsTaskRun

    -- ** UpdateMLTransform 
    , module Network.AWS.Glue.UpdateMLTransform

    -- ** UpdateRegistry 
    , module Network.AWS.Glue.UpdateRegistry

    -- ** DeleteRegistry 
    , module Network.AWS.Glue.DeleteRegistry

    -- ** DeleteMLTransform 
    , module Network.AWS.Glue.DeleteMLTransform

    -- ** StartCrawler 
    , module Network.AWS.Glue.StartCrawler

    -- ** GetCatalogImportStatus 
    , module Network.AWS.Glue.GetCatalogImportStatus

    -- ** ListMLTransforms 
    , module Network.AWS.Glue.ListMLTransforms

    -- ** GetPartition 
    , module Network.AWS.Glue.GetPartition

    -- ** QuerySchemaVersionMetadata 
    , module Network.AWS.Glue.QuerySchemaVersionMetadata

    -- ** CreateTrigger 
    , module Network.AWS.Glue.CreateTrigger

    -- ** CheckSchemaVersionValidity 
    , module Network.AWS.Glue.CheckSchemaVersionValidity

    -- ** DeleteTable 
    , module Network.AWS.Glue.DeleteTable

    -- ** UpdateTable 
    , module Network.AWS.Glue.UpdateTable

    -- ** GetWorkflowRuns 
    , module Network.AWS.Glue.GetWorkflowRuns

    -- ** CreateWorkflow 
    , module Network.AWS.Glue.CreateWorkflow

    -- ** UpdateColumnStatisticsForTable 
    , module Network.AWS.Glue.UpdateColumnStatisticsForTable

    -- ** DeleteColumnStatisticsForTable 
    , module Network.AWS.Glue.DeleteColumnStatisticsForTable

    -- ** DeleteConnection 
    , module Network.AWS.Glue.DeleteConnection

    -- ** UpdateConnection 
    , module Network.AWS.Glue.UpdateConnection

    -- ** GetUserDefinedFunctions (Paginated)
    , module Network.AWS.Glue.GetUserDefinedFunctions

    -- ** GetTags 
    , module Network.AWS.Glue.GetTags

    -- ** GetDataCatalogEncryptionSettings 
    , module Network.AWS.Glue.GetDataCatalogEncryptionSettings

    -- ** BatchCreatePartition 
    , module Network.AWS.Glue.BatchCreatePartition

    -- ** GetMapping 
    , module Network.AWS.Glue.GetMapping

    -- ** DeleteWorkflow 
    , module Network.AWS.Glue.DeleteWorkflow

    -- ** UpdateWorkflow 
    , module Network.AWS.Glue.UpdateWorkflow

    -- ** GetTableVersion 
    , module Network.AWS.Glue.GetTableVersion

    -- ** CreateSecurityConfiguration 
    , module Network.AWS.Glue.CreateSecurityConfiguration

    -- ** StartWorkflowRun 
    , module Network.AWS.Glue.StartWorkflowRun

    -- ** GetJobs (Paginated)
    , module Network.AWS.Glue.GetJobs

    -- ** BatchGetWorkflows 
    , module Network.AWS.Glue.BatchGetWorkflows

    -- ** GetClassifiers (Paginated)
    , module Network.AWS.Glue.GetClassifiers

    -- ** GetResourcePolicies (Paginated)
    , module Network.AWS.Glue.GetResourcePolicies

    -- ** CreateConnection 
    , module Network.AWS.Glue.CreateConnection

    -- ** ListSchemaVersions (Paginated)
    , module Network.AWS.Glue.ListSchemaVersions

    -- ** GetWorkflowRunProperties 
    , module Network.AWS.Glue.GetWorkflowRunProperties

    -- ** BatchGetDevEndpoints 
    , module Network.AWS.Glue.BatchGetDevEndpoints

    -- ** DeletePartitionIndex 
    , module Network.AWS.Glue.DeletePartitionIndex

    -- ** DeleteTableVersion 
    , module Network.AWS.Glue.DeleteTableVersion

    -- ** DeleteDevEndpoint 
    , module Network.AWS.Glue.DeleteDevEndpoint

    -- ** UpdateDevEndpoint 
    , module Network.AWS.Glue.UpdateDevEndpoint

    -- ** GetWorkflow 
    , module Network.AWS.Glue.GetWorkflow

    -- ** BatchGetCrawlers 
    , module Network.AWS.Glue.BatchGetCrawlers

    -- ** GetJobBookmark 
    , module Network.AWS.Glue.GetJobBookmark

    -- ** DeleteCrawler 
    , module Network.AWS.Glue.DeleteCrawler

    -- ** UpdateCrawler 
    , module Network.AWS.Glue.UpdateCrawler

    -- ** StartExportLabelsTaskRun 
    , module Network.AWS.Glue.StartExportLabelsTaskRun

    -- ** GetSecurityConfiguration 
    , module Network.AWS.Glue.GetSecurityConfiguration

    -- ** CreatePartitionIndex 
    , module Network.AWS.Glue.CreatePartitionIndex

    -- ** RemoveSchemaVersionMetadata 
    , module Network.AWS.Glue.RemoveSchemaVersionMetadata

    -- ** ListSchemas (Paginated)
    , module Network.AWS.Glue.ListSchemas

    -- ** GetConnection 
    , module Network.AWS.Glue.GetConnection

    -- ** GetColumnStatisticsForTable 
    , module Network.AWS.Glue.GetColumnStatisticsForTable

    -- ** BatchGetPartition 
    , module Network.AWS.Glue.BatchGetPartition

    -- ** StopTrigger 
    , module Network.AWS.Glue.StopTrigger

    -- ** UpdateCrawlerSchedule 
    , module Network.AWS.Glue.UpdateCrawlerSchedule

    -- ** StartMLEvaluationTaskRun 
    , module Network.AWS.Glue.StartMLEvaluationTaskRun

    -- ** DeleteUserDefinedFunction 
    , module Network.AWS.Glue.DeleteUserDefinedFunction

    -- ** UpdateUserDefinedFunction 
    , module Network.AWS.Glue.UpdateUserDefinedFunction

    -- ** GetRegistry 
    , module Network.AWS.Glue.GetRegistry

    -- ** BatchDeleteTable 
    , module Network.AWS.Glue.BatchDeleteTable

    -- ** CancelMLTaskRun 
    , module Network.AWS.Glue.CancelMLTaskRun

    -- ** GetTables (Paginated)
    , module Network.AWS.Glue.GetTables

    -- ** ResumeWorkflowRun 
    , module Network.AWS.Glue.ResumeWorkflowRun

    -- ** CreateClassifier 
    , module Network.AWS.Glue.CreateClassifier

    -- ** BatchDeleteConnection 
    , module Network.AWS.Glue.BatchDeleteConnection

    -- ** CreateJob 
    , module Network.AWS.Glue.CreateJob

    -- ** GetJobRuns (Paginated)
    , module Network.AWS.Glue.GetJobRuns

    -- ** CreateUserDefinedFunction 
    , module Network.AWS.Glue.CreateUserDefinedFunction

    -- ** ResetJobBookmark 
    , module Network.AWS.Glue.ResetJobBookmark

    -- ** ListJobs 
    , module Network.AWS.Glue.ListJobs

    -- ** DeleteJob 
    , module Network.AWS.Glue.DeleteJob

    -- ** UpdateJob 
    , module Network.AWS.Glue.UpdateJob

    -- ** CreateRegistry 
    , module Network.AWS.Glue.CreateRegistry

    -- ** GetCrawlers (Paginated)
    , module Network.AWS.Glue.GetCrawlers

    -- ** ListTriggers 
    , module Network.AWS.Glue.ListTriggers

    -- ** GetClassifier 
    , module Network.AWS.Glue.GetClassifier

    -- ** GetJob 
    , module Network.AWS.Glue.GetJob

    -- ** ListRegistries (Paginated)
    , module Network.AWS.Glue.ListRegistries

    -- ** BatchDeleteTableVersion 
    , module Network.AWS.Glue.BatchDeleteTableVersion

    -- ** GetDevEndpoints (Paginated)
    , module Network.AWS.Glue.GetDevEndpoints

    -- ** StartCrawlerSchedule 
    , module Network.AWS.Glue.StartCrawlerSchedule

    -- ** GetPartitionIndexes (Paginated)
    , module Network.AWS.Glue.GetPartitionIndexes

    -- ** GetUserDefinedFunction 
    , module Network.AWS.Glue.GetUserDefinedFunction

    -- ** GetResourcePolicy 
    , module Network.AWS.Glue.GetResourcePolicy

    -- ** GetWorkflowRun 
    , module Network.AWS.Glue.GetWorkflowRun

    -- ** DeleteDatabase 
    , module Network.AWS.Glue.DeleteDatabase

    -- ** UpdateDatabase 
    , module Network.AWS.Glue.UpdateDatabase

    -- ** GetColumnStatisticsForPartition 
    , module Network.AWS.Glue.GetColumnStatisticsForPartition

    -- ** StopCrawler 
    , module Network.AWS.Glue.StopCrawler

    -- ** DeleteSecurityConfiguration 
    , module Network.AWS.Glue.DeleteSecurityConfiguration

    -- ** GetPartitions (Paginated)
    , module Network.AWS.Glue.GetPartitions

    -- ** PutSchemaVersionMetadata 
    , module Network.AWS.Glue.PutSchemaVersionMetadata

    -- ** GetSchema 
    , module Network.AWS.Glue.GetSchema

    -- ** BatchDeletePartition 
    , module Network.AWS.Glue.BatchDeletePartition

    -- ** StartMLLabelingSetGenerationTaskRun 
    , module Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun

    -- ** BatchUpdatePartition 
    , module Network.AWS.Glue.BatchUpdatePartition

    -- ** RegisterSchemaVersion 
    , module Network.AWS.Glue.RegisterSchemaVersion

    -- ** StopWorkflowRun 
    , module Network.AWS.Glue.StopWorkflowRun

    -- ** GetCrawler 
    , module Network.AWS.Glue.GetCrawler

    -- ** ListWorkflows 
    , module Network.AWS.Glue.ListWorkflows

    -- ** BatchStopJobRun 
    , module Network.AWS.Glue.BatchStopJobRun

    -- ** GetDevEndpoint 
    , module Network.AWS.Glue.GetDevEndpoint

    -- ** PutWorkflowRunProperties 
    , module Network.AWS.Glue.PutWorkflowRunProperties

    -- ** CreateTable 
    , module Network.AWS.Glue.CreateTable

    -- ** ListCrawlers 
    , module Network.AWS.Glue.ListCrawlers

    -- ** GetCrawlerMetrics (Paginated)
    , module Network.AWS.Glue.GetCrawlerMetrics

    -- ** GetSchemaVersion 
    , module Network.AWS.Glue.GetSchemaVersion

    -- ** GetPlan 
    , module Network.AWS.Glue.GetPlan

    -- ** GetTriggers (Paginated)
    , module Network.AWS.Glue.GetTriggers

    -- ** CreateSchema 
    , module Network.AWS.Glue.CreateSchema

    -- ** ListDevEndpoints 
    , module Network.AWS.Glue.ListDevEndpoints

    -- ** StartTrigger 
    , module Network.AWS.Glue.StartTrigger

    -- ** GetDataflowGraph 
    , module Network.AWS.Glue.GetDataflowGraph

    -- ** GetDatabases (Paginated)
    , module Network.AWS.Glue.GetDatabases

    -- ** GetTable 
    , module Network.AWS.Glue.GetTable

    -- ** CreateCrawler 
    , module Network.AWS.Glue.CreateCrawler

    -- ** GetJobRun 
    , module Network.AWS.Glue.GetJobRun

    -- ** CreateDevEndpoint 
    , module Network.AWS.Glue.CreateDevEndpoint

    -- ** GetMLTaskRuns 
    , module Network.AWS.Glue.GetMLTaskRuns

    -- ** TagResource 
    , module Network.AWS.Glue.TagResource

    -- ** PutDataCatalogEncryptionSettings 
    , module Network.AWS.Glue.PutDataCatalogEncryptionSettings

    -- ** GetMLTransforms 
    , module Network.AWS.Glue.GetMLTransforms

    -- ** UpdateSchema 
    , module Network.AWS.Glue.UpdateSchema

    -- ** DeleteSchema 
    , module Network.AWS.Glue.DeleteSchema

    -- ** GetDatabase 
    , module Network.AWS.Glue.GetDatabase

    -- ** DeleteColumnStatisticsForPartition 
    , module Network.AWS.Glue.DeleteColumnStatisticsForPartition

    -- ** UpdateColumnStatisticsForPartition 
    , module Network.AWS.Glue.UpdateColumnStatisticsForPartition

    -- ** GetMLTaskRun 
    , module Network.AWS.Glue.GetMLTaskRun

    -- ** DeletePartition 
    , module Network.AWS.Glue.DeletePartition

    -- ** UpdatePartition 
    , module Network.AWS.Glue.UpdatePartition

    -- ** GetMLTransform 
    , module Network.AWS.Glue.GetMLTransform

    -- ** CreateScript 
    , module Network.AWS.Glue.CreateScript

    -- ** PutResourcePolicy 
    , module Network.AWS.Glue.PutResourcePolicy

    -- ** GetSecurityConfigurations (Paginated)
    , module Network.AWS.Glue.GetSecurityConfigurations

    -- ** DeleteResourcePolicy 
    , module Network.AWS.Glue.DeleteResourcePolicy

    -- ** GetConnections (Paginated)
    , module Network.AWS.Glue.GetConnections

    -- ** UntagResource 
    , module Network.AWS.Glue.UntagResource

    -- ** GetSchemaVersionsDiff 
    , module Network.AWS.Glue.GetSchemaVersionsDiff

    -- ** SearchTables 
    , module Network.AWS.Glue.SearchTables

    -- ** GetTrigger 
    , module Network.AWS.Glue.GetTrigger

    -- ** BatchGetJobs 
    , module Network.AWS.Glue.BatchGetJobs

    -- ** ImportCatalogToGlue 
    , module Network.AWS.Glue.ImportCatalogToGlue

    -- ** DeleteClassifier 
    , module Network.AWS.Glue.DeleteClassifier

    -- ** UpdateClassifier 
    , module Network.AWS.Glue.UpdateClassifier

    -- ** StartJobRun 
    , module Network.AWS.Glue.StartJobRun

    -- ** CreatePartition 
    , module Network.AWS.Glue.CreatePartition

    -- ** BatchGetTriggers 
    , module Network.AWS.Glue.BatchGetTriggers

    -- ** StopCrawlerSchedule 
    , module Network.AWS.Glue.StopCrawlerSchedule

    -- ** GetSchemaByDefinition 
    , module Network.AWS.Glue.GetSchemaByDefinition

    -- ** CreateDatabase 
    , module Network.AWS.Glue.CreateDatabase

    -- ** GetTableVersions (Paginated)
    , module Network.AWS.Glue.GetTableVersions

    -- ** CreateMLTransform 
    , module Network.AWS.Glue.CreateMLTransform

    -- ** DeleteSchemaVersions 
    , module Network.AWS.Glue.DeleteSchemaVersions

    -- ** DeleteTrigger 
    , module Network.AWS.Glue.DeleteTrigger

    -- ** UpdateTrigger 
    , module Network.AWS.Glue.UpdateTrigger

    -- * Types

    -- ** S3EncryptionMode
    , S3EncryptionMode (..)

    -- ** TaskRun
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

    -- ** GlueResourceArn
    , GlueResourceArn (..)

    -- ** GlueVersionString
    , GlueVersionString (..)

    -- ** DataLakePrincipalString
    , DataLakePrincipalString (..)

    -- ** BinaryColumnStatisticsData
    , BinaryColumnStatisticsData (..)
    , mkBinaryColumnStatisticsData
    , bcsdMaximumLength
    , bcsdAverageLength
    , bcsdNumberOfNulls

    -- ** ResourceUri
    , ResourceUri (..)
    , mkResourceUri
    , ruResourceType
    , ruUri

    -- ** VersionsString
    , VersionsString (..)

    -- ** JobBookmarksEncryptionMode
    , JobBookmarksEncryptionMode (..)

    -- ** CloudWatchEncryptionMode
    , CloudWatchEncryptionMode (..)

    -- ** CrawlState
    , CrawlState (..)

    -- ** PredicateString
    , PredicateString (..)

    -- ** BackfillError
    , BackfillError (..)
    , mkBackfillError
    , beCode
    , bePartitions

    -- ** Crawler
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

    -- ** DevEndpoint
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

    -- ** PartitionInput
    , PartitionInput (..)
    , mkPartitionInput
    , piLastAccessTime
    , piLastAnalyzedTime
    , piParameters
    , piStorageDescriptor
    , piValues

    -- ** HashString
    , HashString (..)

    -- ** PartitionIndexDescriptor
    , PartitionIndexDescriptor (..)
    , mkPartitionIndexDescriptor
    , pidIndexName
    , pidKeys
    , pidIndexStatus
    , pidBackfillErrors

    -- ** PaginationToken
    , PaginationToken (..)

    -- ** SchemaColumn
    , SchemaColumn (..)
    , mkSchemaColumn
    , scDataType
    , scName

    -- ** WorkflowGraph
    , WorkflowGraph (..)
    , mkWorkflowGraph
    , wgEdges
    , wgNodes

    -- ** TaskRunFilterCriteria
    , TaskRunFilterCriteria (..)
    , mkTaskRunFilterCriteria
    , trfcStartedAfter
    , trfcStartedBefore
    , trfcStatus
    , trfcTaskRunType

    -- ** ResourceShareType
    , ResourceShareType (..)

    -- ** MLUserDataEncryption
    , MLUserDataEncryption (..)
    , mkMLUserDataEncryption
    , mludeMlUserDataEncryptionMode
    , mludeKmsKeyId

    -- ** S3Encryption
    , S3Encryption (..)
    , mkS3Encryption
    , seKmsKeyArn
    , seS3EncryptionMode

    -- ** ConnectionsList
    , ConnectionsList (..)
    , mkConnectionsList
    , clConnections

    -- ** SchemaChangePolicy
    , SchemaChangePolicy (..)
    , mkSchemaChangePolicy
    , scpDeleteBehavior
    , scpUpdateBehavior

    -- ** DateColumnStatisticsData
    , DateColumnStatisticsData (..)
    , mkDateColumnStatisticsData
    , dcsdNumberOfNulls
    , dcsdNumberOfDistinctValues
    , dcsdMaximumValue
    , dcsdMinimumValue

    -- ** FindMatchesMetrics
    , FindMatchesMetrics (..)
    , mkFindMatchesMetrics
    , fmmAreaUnderPRCurve
    , fmmConfusionMatrix
    , fmmF1
    , fmmPrecision
    , fmmRecall

    -- ** SortDirectionType
    , SortDirectionType (..)

    -- ** SchemaVersionStatus
    , SchemaVersionStatus (..)

    -- ** CatalogEntry
    , CatalogEntry (..)
    , mkCatalogEntry
    , ceDatabaseName
    , ceTableName

    -- ** JobBookmarksEncryption
    , JobBookmarksEncryption (..)
    , mkJobBookmarksEncryption
    , jbeJobBookmarksEncryptionMode
    , jbeKmsKeyArn

    -- ** TransformFilterCriteria
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

    -- ** KmsKeyArn
    , KmsKeyArn (..)

    -- ** ScriptLocationString
    , ScriptLocationString (..)

    -- ** CloudWatchEncryption
    , CloudWatchEncryption (..)
    , mkCloudWatchEncryption
    , cweCloudWatchEncryptionMode
    , cweKmsKeyArn

    -- ** BooleanColumnStatisticsData
    , BooleanColumnStatisticsData (..)
    , mkBooleanColumnStatisticsData
    , bNumberOfTrues
    , bNumberOfFalses
    , bNumberOfNulls

    -- ** LastCrawlInfo
    , LastCrawlInfo (..)
    , mkLastCrawlInfo
    , lciErrorMessage
    , lciLogGroup
    , lciLogStream
    , lciMessagePrefix
    , lciStartTime
    , lciStatus

    -- ** DynamoDBTarget
    , DynamoDBTarget (..)
    , mkDynamoDBTarget
    , ddbtPath
    , ddbtScanAll
    , ddbtScanRate

    -- ** UserDefinedFunction
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

    -- ** TriggerNodeDetails
    , TriggerNodeDetails (..)
    , mkTriggerNodeDetails
    , tndTrigger

    -- ** BatchUpdatePartitionRequestEntry
    , BatchUpdatePartitionRequestEntry (..)
    , mkBatchUpdatePartitionRequestEntry
    , buprePartitionValueList
    , buprePartitionInput

    -- ** WorkflowRun
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

    -- ** EncryptionAtRest
    , EncryptionAtRest (..)
    , mkEncryptionAtRest
    , earCatalogEncryptionMode
    , earSseAwsKmsKeyId

    -- ** CodeGenArgName
    , CodeGenArgName (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** CreateJsonClassifierRequest
    , CreateJsonClassifierRequest (..)
    , mkCreateJsonClassifierRequest
    , cjcrName
    , cjcrJsonPath

    -- ** PrincipalPermissions
    , PrincipalPermissions (..)
    , mkPrincipalPermissions
    , ppPermissions
    , ppPrincipal

    -- ** RegistryListItem
    , RegistryListItem (..)
    , mkRegistryListItem
    , rliCreatedTime
    , rliDescription
    , rliRegistryArn
    , rliRegistryName
    , rliStatus
    , rliUpdatedTime

    -- ** NotificationProperty
    , NotificationProperty (..)
    , mkNotificationProperty
    , npNotifyDelayAfter

    -- ** PrincipalType
    , PrincipalType (..)

    -- ** CodeGenEdge
    , CodeGenEdge (..)
    , mkCodeGenEdge
    , cgeSource
    , cgeTarget
    , cgeTargetParameter

    -- ** Location
    , Location (..)
    , mkLocation
    , lDynamoDB
    , lJdbc
    , lS3

    -- ** ErrorString
    , ErrorString (..)

    -- ** DeleteBehavior
    , DeleteBehavior (..)

    -- ** UpdateBehavior
    , UpdateBehavior (..)

    -- ** Database
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

    -- ** LastCrawlStatus
    , LastCrawlStatus (..)

    -- ** Path
    , Path (..)

    -- ** JsonPath
    , JsonPath (..)

    -- ** MLTransform
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

    -- ** SchemaStatus
    , SchemaStatus (..)

    -- ** TaskType
    , TaskType (..)

    -- ** Schedule
    , Schedule (..)
    , mkSchedule
    , sScheduleExpression
    , sState

    -- ** TaskRunSortCriteria
    , TaskRunSortCriteria (..)
    , mkTaskRunSortCriteria
    , trscColumn
    , trscSortDirection

    -- ** ParametersMapValue
    , ParametersMapValue (..)

    -- ** TransformSortCriteria
    , TransformSortCriteria (..)
    , mkTransformSortCriteria
    , tscColumn
    , tscSortDirection

    -- ** TableIdentifier
    , TableIdentifier (..)
    , mkTableIdentifier
    , tiCatalogId
    , tiDatabaseName
    , tiName

    -- ** RegistryId
    , RegistryId (..)
    , mkRegistryId
    , riRegistryArn
    , riRegistryName

    -- ** TaskRunSortColumnType
    , TaskRunSortColumnType (..)

    -- ** MappingEntry
    , MappingEntry (..)
    , mkMappingEntry
    , meSourcePath
    , meSourceTable
    , meSourceType
    , meTargetPath
    , meTargetTable
    , meTargetType

    -- ** Node
    , Node (..)
    , mkNode
    , nCrawlerDetails
    , nJobDetails
    , nName
    , nTriggerDetails
    , nType
    , nUniqueId

    -- ** JobCommand
    , JobCommand (..)
    , mkJobCommand
    , jcfName
    , jcfPythonVersion
    , jcfScriptLocation

    -- ** ConnectionInput
    , ConnectionInput (..)
    , mkConnectionInput
    , ciName
    , ciConnectionType
    , ciConnectionProperties
    , ciDescription
    , ciMatchCriteria
    , ciPhysicalConnectionRequirements

    -- ** DecimalColumnStatisticsData
    , DecimalColumnStatisticsData (..)
    , mkDecimalColumnStatisticsData
    , dNumberOfNulls
    , dNumberOfDistinctValues
    , dMaximumValue
    , dMinimumValue

    -- ** LocationString
    , LocationString (..)

    -- ** TransformSortColumnType
    , TransformSortColumnType (..)

    -- ** JobName
    , JobName (..)

    -- ** ExistCondition
    , ExistCondition (..)

    -- ** LogStream
    , LogStream (..)

    -- ** TriggerType
    , TriggerType (..)

    -- ** DecimalNumber
    , DecimalNumber (..)
    , mkDecimalNumber
    , dnUnscaledValue
    , dnScale

    -- ** ColumnStatisticsError
    , ColumnStatisticsError (..)
    , mkColumnStatisticsError
    , cseColumnStatistics
    , cseError

    -- ** Token
    , Token (..)

    -- ** UpdatedTimestamp
    , UpdatedTimestamp (..)

    -- ** SerDeInfo
    , SerDeInfo (..)
    , mkSerDeInfo
    , sdiName
    , sdiParameters
    , sdiSerializationLibrary

    -- ** MongoDBTarget
    , MongoDBTarget (..)
    , mkMongoDBTarget
    , mdbtConnectionName
    , mdbtPath
    , mdbtScanAll

    -- ** LogicalOperator
    , LogicalOperator (..)

    -- ** CrawlerState
    , CrawlerState (..)

    -- ** DataFormat
    , DataFormat (..)

    -- ** MetadataValueString
    , MetadataValueString (..)

    -- ** CsvColumnDelimiter
    , CsvColumnDelimiter (..)

    -- ** PythonScript
    , PythonScript (..)

    -- ** DatabaseInput
    , DatabaseInput (..)
    , mkDatabaseInput
    , diName
    , diCreateTableDefaultPermissions
    , diDescription
    , diLocationUri
    , diParameters
    , diTargetDatabase

    -- ** MetadataKeyValuePair
    , MetadataKeyValuePair (..)
    , mkMetadataKeyValuePair
    , mkvpMetadataKey
    , mkvpMetadataValue

    -- ** LogGroup
    , LogGroup (..)

    -- ** ColumnStatisticsType
    , ColumnStatisticsType (..)

    -- ** JsonValue
    , JsonValue (..)

    -- ** ColumnValuesString
    , ColumnValuesString (..)

    -- ** Logical
    , Logical (..)

    -- ** CrawlerMetrics
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

    -- ** GetConnectionsFilter
    , GetConnectionsFilter (..)
    , mkGetConnectionsFilter
    , gcfConnectionType
    , gcfMatchCriteria

    -- ** DescriptionString
    , DescriptionString (..)

    -- ** WorkerType
    , WorkerType (..)

    -- ** PropertyPredicate
    , PropertyPredicate (..)
    , mkPropertyPredicate
    , ppComparator
    , ppKey
    , ppValue

    -- ** GrokClassifier
    , GrokClassifier (..)
    , mkGrokClassifier
    , gcName
    , gcClassification
    , gcGrokPattern
    , gcCreationTime
    , gcCustomPatterns
    , gcLastUpdated
    , gcVersion

    -- ** Action
    , Action (..)
    , mkAction
    , aArguments
    , aCrawlerName
    , aJobName
    , aNotificationProperty
    , aSecurityConfiguration
    , aTimeout

    -- ** ConnectionPropertyKey
    , ConnectionPropertyKey (..)

    -- ** DoubleColumnStatisticsData
    , DoubleColumnStatisticsData (..)
    , mkDoubleColumnStatisticsData
    , dcsdfNumberOfNulls
    , dcsdfNumberOfDistinctValues
    , dcsdfMaximumValue
    , dcsdfMinimumValue

    -- ** StringColumnStatisticsData
    , StringColumnStatisticsData (..)
    , mkStringColumnStatisticsData
    , scsdMaximumLength
    , scsdAverageLength
    , scsdNumberOfNulls
    , scsdNumberOfDistinctValues

    -- ** SecurityConfiguration
    , SecurityConfiguration (..)
    , mkSecurityConfiguration
    , sCreatedTimeStamp
    , sEncryptionConfiguration
    , sName

    -- ** Classification
    , Classification (..)

    -- ** UpdateXMLClassifierRequest
    , UpdateXMLClassifierRequest (..)
    , mkUpdateXMLClassifierRequest
    , uxmlcrName
    , uxmlcrClassification
    , uxmlcrRowTag

    -- ** ErrorMessageString
    , ErrorMessageString (..)

    -- ** CrawlerNodeDetails
    , CrawlerNodeDetails (..)
    , mkCrawlerNodeDetails
    , cndCrawls

    -- ** XMLClassifier
    , XMLClassifier (..)
    , mkXMLClassifier
    , xmlcName
    , xmlcClassification
    , xmlcCreationTime
    , xmlcLastUpdated
    , xmlcRowTag
    , xmlcVersion

    -- ** TransformParameters
    , TransformParameters (..)
    , mkTransformParameters
    , tpTransformType
    , tpFindMatchesParameters

    -- ** SchemaListItem
    , SchemaListItem (..)
    , mkSchemaListItem
    , sliCreatedTime
    , sliDescription
    , sliRegistryName
    , sliSchemaArn
    , sliSchemaName
    , sliSchemaStatus
    , sliUpdatedTime

    -- ** CommentString
    , CommentString (..)

    -- ** CrawlerTargets
    , CrawlerTargets (..)
    , mkCrawlerTargets
    , ctCatalogTargets
    , ctDynamoDBTargets
    , ctJdbcTargets
    , ctMongoDBTargets
    , ctS3Targets

    -- ** BatchStopJobRunSuccessfulSubmission
    , BatchStopJobRunSuccessfulSubmission (..)
    , mkBatchStopJobRunSuccessfulSubmission
    , bsjrssJobName
    , bsjrssJobRunId

    -- ** ConnectionPasswordEncryption
    , ConnectionPasswordEncryption (..)
    , mkConnectionPasswordEncryption
    , cpeReturnConnectionPasswordEncrypted
    , cpeAwsKmsKeyId

    -- ** Connection
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

    -- ** DataLakePrincipal
    , DataLakePrincipal (..)
    , mkDataLakePrincipal
    , dlpDataLakePrincipalIdentifier

    -- ** TriggerUpdate
    , TriggerUpdate (..)
    , mkTriggerUpdate
    , tuActions
    , tuDescription
    , tuName
    , tuPredicate
    , tuSchedule

    -- ** TagValue
    , TagValue (..)

    -- ** CsvClassifier
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

    -- ** SchemaDefinitionDiff
    , SchemaDefinitionDiff (..)

    -- ** SchemaDefinitionString
    , SchemaDefinitionString (..)

    -- ** EvaluationMetrics
    , EvaluationMetrics (..)
    , mkEvaluationMetrics
    , emTransformType
    , emFindMatchesMetrics

    -- ** CodeGenNodeType
    , CodeGenNodeType (..)

    -- ** ScalaCode
    , ScalaCode (..)

    -- ** ConnectionName
    , ConnectionName (..)

    -- ** S3Target
    , S3Target (..)
    , mkS3Target
    , stConnectionName
    , stExclusions
    , stPath

    -- ** Predicate
    , Predicate (..)
    , mkPredicate
    , pConditions
    , pLogical

    -- ** JobRunState
    , JobRunState (..)

    -- ** TaskStatusType
    , TaskStatusType (..)

    -- ** MLUserDataEncryptionModeString
    , MLUserDataEncryptionModeString (..)

    -- ** CrawlerLineageSettings
    , CrawlerLineageSettings (..)

    -- ** PartitionIndex
    , PartitionIndex (..)
    , mkPartitionIndex
    , piKeys
    , piIndexName

    -- ** UserDefinedFunctionInput
    , UserDefinedFunctionInput (..)
    , mkUserDefinedFunctionInput
    , udfiClassName
    , udfiFunctionName
    , udfiOwnerName
    , udfiOwnerType
    , udfiResourceUris

    -- ** TableVersionError
    , TableVersionError (..)
    , mkTableVersionError
    , tveErrorDetail
    , tveTableName
    , tveVersionId

    -- ** CreateCsvClassifierRequest
    , CreateCsvClassifierRequest (..)
    , mkCreateCsvClassifierRequest
    , cccrName
    , cccrAllowSingleColumn
    , cccrContainsHeader
    , cccrDelimiter
    , cccrDisableValueTrimming
    , cccrHeader
    , cccrQuoteSymbol

    -- ** SchemaId
    , SchemaId (..)
    , mkSchemaId
    , siRegistryName
    , siSchemaArn
    , siSchemaName

    -- ** GenericString
    , GenericString (..)

    -- ** RecrawlPolicy
    , RecrawlPolicy (..)
    , mkRecrawlPolicy
    , rpRecrawlBehavior

    -- ** SchemaVersionListItem
    , SchemaVersionListItem (..)
    , mkSchemaVersionListItem
    , svliCreatedTime
    , svliSchemaArn
    , svliSchemaVersionId
    , svliStatus
    , svliVersionNumber

    -- ** ExportLabelsTaskRunProperties
    , ExportLabelsTaskRunProperties (..)
    , mkExportLabelsTaskRunProperties
    , eltrpOutputS3Path

    -- ** TransformType
    , TransformType (..)

    -- ** TableVersion
    , TableVersion (..)
    , mkTableVersion
    , tvTable
    , tvVersionId

    -- ** RunId
    , RunId (..)

    -- ** SchemaDiffType
    , SchemaDiffType (..)

    -- ** ColumnNameString
    , ColumnNameString (..)

    -- ** Role
    , Role (..)

    -- ** JobBookmarkEntry
    , JobBookmarkEntry (..)
    , mkJobBookmarkEntry
    , jbeAttempt
    , jbeJobBookmark
    , jbeJobName
    , jbePreviousRunId
    , jbeRun
    , jbeRunId
    , jbeVersion

    -- ** MetadataKeyString
    , MetadataKeyString (..)

    -- ** Job
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

    -- ** PhysicalConnectionRequirements
    , PhysicalConnectionRequirements (..)
    , mkPhysicalConnectionRequirements
    , pcrAvailabilityZone
    , pcrSecurityGroupIdList
    , pcrSubnetId

    -- ** WorkflowRunStatistics
    , WorkflowRunStatistics (..)
    , mkWorkflowRunStatistics
    , wrsFailedActions
    , wrsRunningActions
    , wrsStoppedActions
    , wrsSucceededActions
    , wrsTimeoutActions
    , wrsTotalActions

    -- ** Classifier
    , Classifier (..)
    , mkClassifier
    , cCsvClassifier
    , cGrokClassifier
    , cJsonClassifier
    , cXMLClassifier

    -- ** TriggerState
    , TriggerState (..)

    -- ** JsonClassifier
    , JsonClassifier (..)
    , mkJsonClassifier
    , jcName
    , jcJsonPath
    , jcCreationTime
    , jcLastUpdated
    , jcVersion

    -- ** LabelingSetGenerationTaskRunProperties
    , LabelingSetGenerationTaskRunProperties (..)
    , mkLabelingSetGenerationTaskRunProperties
    , lsgtrpOutputS3Path

    -- ** VersionString
    , VersionString (..)

    -- ** CatalogEncryptionMode
    , CatalogEncryptionMode (..)

    -- ** UpdateJsonClassifierRequest
    , UpdateJsonClassifierRequest (..)
    , mkUpdateJsonClassifierRequest
    , ujcrName
    , ujcrJsonPath

    -- ** EncryptionConfiguration
    , EncryptionConfiguration (..)
    , mkEncryptionConfiguration
    , ecCloudWatchEncryption
    , ecJobBookmarksEncryption
    , ecS3Encryption

    -- ** SchemaReference
    , SchemaReference (..)
    , mkSchemaReference
    , srSchemaId
    , srSchemaVersionId
    , srSchemaVersionNumber

    -- ** CodeGenIdentifier
    , CodeGenIdentifier (..)

    -- ** PartitionError
    , PartitionError (..)
    , mkPartitionError
    , peErrorDetail
    , pePartitionValues

    -- ** SchemaRegistryTokenString
    , SchemaRegistryTokenString (..)

    -- ** NameString
    , NameString (..)

    -- ** CatalogTarget
    , CatalogTarget (..)
    , mkCatalogTarget
    , ctDatabaseName
    , ctTables

    -- ** Sort
    , Sort (..)

    -- ** StorageDescriptor
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

    -- ** Language
    , Language (..)

    -- ** FindMatchesParameters
    , FindMatchesParameters (..)
    , mkFindMatchesParameters
    , fmpAccuracyCostTradeoff
    , fmpEnforceProvidedLabels
    , fmpPrecisionRecallTradeoff
    , fmpPrimaryKeyColumnName

    -- ** CsvHeaderOption
    , CsvHeaderOption (..)

    -- ** CatalogImportStatus
    , CatalogImportStatus (..)
    , mkCatalogImportStatus
    , cisImportCompleted
    , cisImportTime
    , cisImportedBy

    -- ** KeyString
    , KeyString (..)

    -- ** ScheduleState
    , ScheduleState (..)

    -- ** DatabaseName
    , DatabaseName (..)

    -- ** BatchStopJobRunError
    , BatchStopJobRunError (..)
    , mkBatchStopJobRunError
    , bsjreErrorDetail
    , bsjreJobName
    , bsjreJobRunId

    -- ** CsvQuoteSymbol
    , CsvQuoteSymbol (..)

    -- ** ErrorDetails
    , ErrorDetails (..)
    , mkErrorDetails
    , eErrorCode
    , eErrorMessage

    -- ** Partition
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

    -- ** CronExpression
    , CronExpression (..)

    -- ** CrawlerSecurityConfiguration
    , CrawlerSecurityConfiguration (..)

    -- ** Comparator
    , Comparator (..)

    -- ** LineageConfiguration
    , LineageConfiguration (..)
    , mkLineageConfiguration
    , lcCrawlerLineageSettings

    -- ** PolicyJsonString
    , PolicyJsonString (..)

    -- ** EnableHybridValues
    , EnableHybridValues (..)

    -- ** ExecutionProperty
    , ExecutionProperty (..)
    , mkExecutionProperty
    , epMaxConcurrentRuns

    -- ** JobNodeDetails
    , JobNodeDetails (..)
    , mkJobNodeDetails
    , jndJobRuns

    -- ** FindMatchesTaskRunProperties
    , FindMatchesTaskRunProperties (..)
    , mkFindMatchesTaskRunProperties
    , fmtrpJobId
    , fmtrpJobName
    , fmtrpJobRunId

    -- ** TaskRunProperties
    , TaskRunProperties (..)
    , mkTaskRunProperties
    , trpExportLabelsTaskRunProperties
    , trpFindMatchesTaskRunProperties
    , trpImportLabelsTaskRunProperties
    , trpLabelingSetGenerationTaskRunProperties
    , trpTaskType

    -- ** Predecessor
    , Predecessor (..)
    , mkPredecessor
    , pJobName
    , pRunId

    -- ** TypeString
    , TypeString (..)

    -- ** ImportLabelsTaskRunProperties
    , ImportLabelsTaskRunProperties (..)
    , mkImportLabelsTaskRunProperties
    , iltrpInputS3Path
    , iltrpReplace

    -- ** GlueTable
    , GlueTable (..)
    , mkGlueTable
    , gtDatabaseName
    , gtTableName
    , gtCatalogId
    , gtConnectionName

    -- ** CodeGenNodeArg
    , CodeGenNodeArg (..)
    , mkCodeGenNodeArg
    , cgnaName
    , cgnaValue
    , cgnaParam

    -- ** SkewedInfo
    , SkewedInfo (..)
    , mkSkewedInfo
    , siSkewedColumnNames
    , siSkewedColumnValueLocationMaps
    , siSkewedColumnValues

    -- ** ViewTextString
    , ViewTextString (..)

    -- ** TagKey
    , TagKey (..)

    -- ** TransformStatusType
    , TransformStatusType (..)

    -- ** PartitionIndexStatus
    , PartitionIndexStatus (..)

    -- ** KeySchemaElement
    , KeySchemaElement (..)
    , mkKeySchemaElement
    , kseName
    , kseType

    -- ** TableInput
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

    -- ** SchemaVersionNumber
    , SchemaVersionNumber (..)
    , mkSchemaVersionNumber
    , svnLatestVersion
    , svnVersionNumber

    -- ** RecrawlBehavior
    , RecrawlBehavior (..)

    -- ** Trigger
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

    -- ** IdString
    , IdString (..)

    -- ** CustomPatterns
    , CustomPatterns (..)

    -- ** RowTag
    , RowTag (..)

    -- ** NodeType
    , NodeType (..)

    -- ** TransformEncryption
    , TransformEncryption (..)
    , mkTransformEncryption
    , teMlUserDataEncryption
    , teTaskRunSecurityConfigurationName

    -- ** ColumnStatisticsData
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

    -- ** Condition
    , Condition (..)
    , mkCondition
    , cfCrawlState
    , cfCrawlerName
    , cfJobName
    , cfLogicalOperator
    , cfState

    -- ** Segment
    , Segment (..)
    , mkSegment
    , sSegmentNumber
    , sTotalSegments

    -- ** TableTypeString
    , TableTypeString (..)

    -- ** Crawl
    , Crawl (..)
    , mkCrawl
    , cCompletedOn
    , cErrorMessage
    , cLogGroup
    , cLogStream
    , cStartedOn
    , cState

    -- ** ColumnTypeString
    , ColumnTypeString (..)

    -- ** MessagePrefix
    , MessagePrefix (..)

    -- ** Edge
    , Edge (..)
    , mkEdge
    , eDestinationId
    , eSourceId

    -- ** Permission
    , Permission (..)

    -- ** ColumnStatistics
    , ColumnStatistics (..)
    , mkColumnStatistics
    , csColumnName
    , csColumnType
    , csAnalyzedTime
    , csStatisticsData

    -- ** CreateGrokClassifierRequest
    , CreateGrokClassifierRequest (..)
    , mkCreateGrokClassifierRequest
    , cgcrClassification
    , cgcrName
    , cgcrGrokPattern
    , cgcrCustomPatterns

    -- ** BatchUpdatePartitionFailureEntry
    , BatchUpdatePartitionFailureEntry (..)
    , mkBatchUpdatePartitionFailureEntry
    , bupfeErrorDetail
    , bupfePartitionValueList

    -- ** GluePolicy
    , GluePolicy (..)
    , mkGluePolicy
    , gpCreateTime
    , gpPolicyHash
    , gpPolicyInJson
    , gpUpdateTime

    -- ** MetadataInfo
    , MetadataInfo (..)
    , mkMetadataInfo
    , miCreatedTime
    , miMetadataValue

    -- ** PartitionValueList
    , PartitionValueList (..)
    , mkPartitionValueList
    , pvlValues

    -- ** UriString
    , UriString (..)

    -- ** CreateXMLClassifierRequest
    , CreateXMLClassifierRequest (..)
    , mkCreateXMLClassifierRequest
    , cxmlcrClassification
    , cxmlcrName
    , cxmlcrRowTag

    -- ** Workflow
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

    -- ** CatalogIdString
    , CatalogIdString (..)

    -- ** SchemaVersionIdString
    , SchemaVersionIdString (..)

    -- ** UpdateGrokClassifierRequest
    , UpdateGrokClassifierRequest (..)
    , mkUpdateGrokClassifierRequest
    , ugcrName
    , ugcrClassification
    , ugcrCustomPatterns
    , ugcrGrokPattern

    -- ** TablePrefix
    , TablePrefix (..)

    -- ** ColumnError
    , ColumnError (..)
    , mkColumnError
    , ceColumnName
    , ceError

    -- ** DevEndpointCustomLibraries
    , DevEndpointCustomLibraries (..)
    , mkDevEndpointCustomLibraries
    , declExtraJarsS3Path
    , declExtraPythonLibsS3Path

    -- ** JobRun
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

    -- ** SchemaVersionErrorItem
    , SchemaVersionErrorItem (..)
    , mkSchemaVersionErrorItem
    , sveiErrorDetails
    , sveiVersionNumber

    -- ** WorkflowRunStatus
    , WorkflowRunStatus (..)

    -- ** JdbcTarget
    , JdbcTarget (..)
    , mkJdbcTarget
    , jtConnectionName
    , jtExclusions
    , jtPath

    -- ** Table
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

    -- ** Order
    , Order (..)
    , mkOrder
    , oColumn
    , oSortOrder

    -- ** Column
    , Column (..)
    , mkColumn
    , cName
    , cComment
    , cParameters
    , cType

    -- ** ConnectionType
    , ConnectionType (..)

    -- ** JobUpdate
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

    -- ** DatabaseIdentifier
    , DatabaseIdentifier (..)
    , mkDatabaseIdentifier
    , diCatalogId
    , diDatabaseName

    -- ** TableError
    , TableError (..)
    , mkTableError
    , teErrorDetail
    , teTableName

    -- ** Compatibility
    , Compatibility (..)

    -- ** ValueString
    , ValueString (..)

    -- ** RegistryStatus
    , RegistryStatus (..)

    -- ** DataCatalogEncryptionSettings
    , DataCatalogEncryptionSettings (..)
    , mkDataCatalogEncryptionSettings
    , dcesConnectionPasswordEncryption
    , dcesEncryptionAtRest

    -- ** TableName
    , TableName (..)

    -- ** GrokPattern
    , GrokPattern (..)

    -- ** SortCriterion
    , SortCriterion (..)
    , mkSortCriterion
    , scFieldName
    , scSort

    -- ** BackfillErrorCode
    , BackfillErrorCode (..)

    -- ** CodeGenNode
    , CodeGenNode (..)
    , mkCodeGenNode
    , cgnId
    , cgnNodeType
    , cgnArgs
    , cgnLineNumber

    -- ** LongColumnStatisticsData
    , LongColumnStatisticsData (..)
    , mkLongColumnStatisticsData
    , lcsdNumberOfNulls
    , lcsdNumberOfDistinctValues
    , lcsdMaximumValue
    , lcsdMinimumValue

    -- ** ErrorDetail
    , ErrorDetail (..)
    , mkErrorDetail
    , edErrorCode
    , edErrorMessage

    -- ** RoleArn
    , RoleArn (..)

    -- ** UpdateCsvClassifierRequest
    , UpdateCsvClassifierRequest (..)
    , mkUpdateCsvClassifierRequest
    , uccrName
    , uccrAllowSingleColumn
    , uccrContainsHeader
    , uccrDelimiter
    , uccrDisableValueTrimming
    , uccrHeader
    , uccrQuoteSymbol

    -- ** ConfusionMatrix
    , ConfusionMatrix (..)
    , mkConfusionMatrix
    , cmNumFalseNegatives
    , cmNumFalsePositives
    , cmNumTrueNegatives
    , cmNumTruePositives

    -- ** Name
    , Name (..)

    -- ** EndpointName
    , EndpointName (..)

    -- ** SchemaDefinition
    , SchemaDefinition (..)

    -- ** PublicKey
    , PublicKey (..)

    -- ** LogGroupName
    , LogGroupName (..)

    -- ** TaskRunId
    , TaskRunId (..)

    -- ** TransformId
    , TransformId (..)

    -- ** CatalogId
    , CatalogId (..)

    -- ** IndexName
    , IndexName (..)

    -- ** Uri
    , Uri (..)

    -- ** Configuration
    , Configuration (..)

    -- ** Description
    , Description (..)

    -- ** OutputS3Path
    , OutputS3Path (..)

    -- ** Error
    , Error (..)

    -- ** NextToken
    , NextToken (..)

    -- ** AvailabilityZone
    , AvailabilityZone (..)

    -- ** ExtraJarsS3Path
    , ExtraJarsS3Path (..)

    -- ** ExtraPythonLibsS3Path
    , ExtraPythonLibsS3Path (..)

    -- ** FailureReason
    , FailureReason (..)

    -- ** LastUpdateStatus
    , LastUpdateStatus (..)

    -- ** PrivateAddress
    , PrivateAddress (..)

    -- ** PublicAddress
    , PublicAddress (..)

    -- ** Status
    , Status (..)

    -- ** SubnetId
    , SubnetId (..)

    -- ** VpcId
    , VpcId (..)

    -- ** YarnEndpointAddress
    , YarnEndpointAddress (..)

    -- ** DataType
    , DataType (..)

    -- ** KmsKeyId
    , KmsKeyId (..)

    -- ** RegistryName
    , RegistryName (..)

    -- ** ErrorMessage
    , ErrorMessage (..)

    -- ** SchemaVersionId
    , SchemaVersionId (..)

    -- ** ClassName
    , ClassName (..)

    -- ** FunctionName
    , FunctionName (..)

    -- ** OwnerName
    , OwnerName (..)

    -- ** PreviousRunId
    , PreviousRunId (..)

    -- ** WorkflowRunId
    , WorkflowRunId (..)

    -- ** SseAwsKmsKeyId
    , SseAwsKmsKeyId (..)

    -- ** LogUri
    , LogUri (..)

    -- ** Expression
    , Expression (..)

    -- ** CreatedTime
    , CreatedTime (..)

    -- ** UpdatedTime
    , UpdatedTime (..)

    -- ** Source
    , Source (..)

    -- ** Target
    , Target (..)

    -- ** LocationUri
    , LocationUri (..)

    -- ** ScheduleExpression
    , ScheduleExpression (..)

    -- ** JobRunId
    , JobRunId (..)

    -- ** SourcePath
    , SourcePath (..)

    -- ** SourceTable
    , SourceTable (..)

    -- ** SourceType
    , SourceType (..)

    -- ** TargetPath
    , TargetPath (..)

    -- ** TargetTable
    , TargetTable (..)

    -- ** TargetType
    , TargetType (..)

    -- ** UniqueId
    , UniqueId (..)

    -- ** PythonVersion
    , PythonVersion (..)

    -- ** DependentJobName
    , DependentJobName (..)

    -- ** CrawlerName
    , CrawlerName (..)

    -- ** SerializationLibrary
    , SerializationLibrary (..)

    -- ** MetadataKey
    , MetadataKey (..)

    -- ** SchemaName
    , SchemaName (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** AwsKmsKeyId
    , AwsKmsKeyId (..)

    -- ** LastUpdatedBy
    , LastUpdatedBy (..)

    -- ** QuoteSymbol
    , QuoteSymbol (..)

    -- ** PolicyInJson
    , PolicyInJson (..)

    -- ** InputFormat
    , InputFormat (..)

    -- ** OutputFormat
    , OutputFormat (..)

    -- ** SearchText
    , SearchText (..)

    -- ** ErrorCode
    , ErrorCode (..)

    -- ** InputS3Path
    , InputS3Path (..)

    -- ** Type
    , Type (..)

    -- ** TableType
    , TableType (..)

    -- ** Id
    , Id (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Waiters
import Network.AWS.Glue.StartImportLabelsTaskRun
import Network.AWS.Glue.UpdateMLTransform
import Network.AWS.Glue.UpdateRegistry
import Network.AWS.Glue.DeleteRegistry
import Network.AWS.Glue.DeleteMLTransform
import Network.AWS.Glue.StartCrawler
import Network.AWS.Glue.GetCatalogImportStatus
import Network.AWS.Glue.ListMLTransforms
import Network.AWS.Glue.GetPartition
import Network.AWS.Glue.QuerySchemaVersionMetadata
import Network.AWS.Glue.CreateTrigger
import Network.AWS.Glue.CheckSchemaVersionValidity
import Network.AWS.Glue.DeleteTable
import Network.AWS.Glue.UpdateTable
import Network.AWS.Glue.GetWorkflowRuns
import Network.AWS.Glue.CreateWorkflow
import Network.AWS.Glue.UpdateColumnStatisticsForTable
import Network.AWS.Glue.DeleteColumnStatisticsForTable
import Network.AWS.Glue.DeleteConnection
import Network.AWS.Glue.UpdateConnection
import Network.AWS.Glue.GetUserDefinedFunctions
import Network.AWS.Glue.GetTags
import Network.AWS.Glue.GetDataCatalogEncryptionSettings
import Network.AWS.Glue.BatchCreatePartition
import Network.AWS.Glue.GetMapping
import Network.AWS.Glue.DeleteWorkflow
import Network.AWS.Glue.UpdateWorkflow
import Network.AWS.Glue.GetTableVersion
import Network.AWS.Glue.CreateSecurityConfiguration
import Network.AWS.Glue.StartWorkflowRun
import Network.AWS.Glue.GetJobs
import Network.AWS.Glue.BatchGetWorkflows
import Network.AWS.Glue.GetClassifiers
import Network.AWS.Glue.GetResourcePolicies
import Network.AWS.Glue.CreateConnection
import Network.AWS.Glue.ListSchemaVersions
import Network.AWS.Glue.GetWorkflowRunProperties
import Network.AWS.Glue.BatchGetDevEndpoints
import Network.AWS.Glue.DeletePartitionIndex
import Network.AWS.Glue.DeleteTableVersion
import Network.AWS.Glue.DeleteDevEndpoint
import Network.AWS.Glue.UpdateDevEndpoint
import Network.AWS.Glue.GetWorkflow
import Network.AWS.Glue.BatchGetCrawlers
import Network.AWS.Glue.GetJobBookmark
import Network.AWS.Glue.DeleteCrawler
import Network.AWS.Glue.UpdateCrawler
import Network.AWS.Glue.StartExportLabelsTaskRun
import Network.AWS.Glue.GetSecurityConfiguration
import Network.AWS.Glue.CreatePartitionIndex
import Network.AWS.Glue.RemoveSchemaVersionMetadata
import Network.AWS.Glue.ListSchemas
import Network.AWS.Glue.GetConnection
import Network.AWS.Glue.GetColumnStatisticsForTable
import Network.AWS.Glue.BatchGetPartition
import Network.AWS.Glue.StopTrigger
import Network.AWS.Glue.UpdateCrawlerSchedule
import Network.AWS.Glue.StartMLEvaluationTaskRun
import Network.AWS.Glue.DeleteUserDefinedFunction
import Network.AWS.Glue.UpdateUserDefinedFunction
import Network.AWS.Glue.GetRegistry
import Network.AWS.Glue.BatchDeleteTable
import Network.AWS.Glue.CancelMLTaskRun
import Network.AWS.Glue.GetTables
import Network.AWS.Glue.ResumeWorkflowRun
import Network.AWS.Glue.CreateClassifier
import Network.AWS.Glue.BatchDeleteConnection
import Network.AWS.Glue.CreateJob
import Network.AWS.Glue.GetJobRuns
import Network.AWS.Glue.CreateUserDefinedFunction
import Network.AWS.Glue.ResetJobBookmark
import Network.AWS.Glue.ListJobs
import Network.AWS.Glue.DeleteJob
import Network.AWS.Glue.UpdateJob
import Network.AWS.Glue.CreateRegistry
import Network.AWS.Glue.GetCrawlers
import Network.AWS.Glue.ListTriggers
import Network.AWS.Glue.GetClassifier
import Network.AWS.Glue.GetJob
import Network.AWS.Glue.ListRegistries
import Network.AWS.Glue.BatchDeleteTableVersion
import Network.AWS.Glue.GetDevEndpoints
import Network.AWS.Glue.StartCrawlerSchedule
import Network.AWS.Glue.GetPartitionIndexes
import Network.AWS.Glue.GetUserDefinedFunction
import Network.AWS.Glue.GetResourcePolicy
import Network.AWS.Glue.GetWorkflowRun
import Network.AWS.Glue.DeleteDatabase
import Network.AWS.Glue.UpdateDatabase
import Network.AWS.Glue.GetColumnStatisticsForPartition
import Network.AWS.Glue.StopCrawler
import Network.AWS.Glue.DeleteSecurityConfiguration
import Network.AWS.Glue.GetPartitions
import Network.AWS.Glue.PutSchemaVersionMetadata
import Network.AWS.Glue.GetSchema
import Network.AWS.Glue.BatchDeletePartition
import Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun
import Network.AWS.Glue.BatchUpdatePartition
import Network.AWS.Glue.RegisterSchemaVersion
import Network.AWS.Glue.StopWorkflowRun
import Network.AWS.Glue.GetCrawler
import Network.AWS.Glue.ListWorkflows
import Network.AWS.Glue.BatchStopJobRun
import Network.AWS.Glue.GetDevEndpoint
import Network.AWS.Glue.PutWorkflowRunProperties
import Network.AWS.Glue.CreateTable
import Network.AWS.Glue.ListCrawlers
import Network.AWS.Glue.GetCrawlerMetrics
import Network.AWS.Glue.GetSchemaVersion
import Network.AWS.Glue.GetPlan
import Network.AWS.Glue.GetTriggers
import Network.AWS.Glue.CreateSchema
import Network.AWS.Glue.ListDevEndpoints
import Network.AWS.Glue.StartTrigger
import Network.AWS.Glue.GetDataflowGraph
import Network.AWS.Glue.GetDatabases
import Network.AWS.Glue.GetTable
import Network.AWS.Glue.CreateCrawler
import Network.AWS.Glue.GetJobRun
import Network.AWS.Glue.CreateDevEndpoint
import Network.AWS.Glue.GetMLTaskRuns
import Network.AWS.Glue.TagResource
import Network.AWS.Glue.PutDataCatalogEncryptionSettings
import Network.AWS.Glue.GetMLTransforms
import Network.AWS.Glue.UpdateSchema
import Network.AWS.Glue.DeleteSchema
import Network.AWS.Glue.GetDatabase
import Network.AWS.Glue.DeleteColumnStatisticsForPartition
import Network.AWS.Glue.UpdateColumnStatisticsForPartition
import Network.AWS.Glue.GetMLTaskRun
import Network.AWS.Glue.DeletePartition
import Network.AWS.Glue.UpdatePartition
import Network.AWS.Glue.GetMLTransform
import Network.AWS.Glue.CreateScript
import Network.AWS.Glue.PutResourcePolicy
import Network.AWS.Glue.GetSecurityConfigurations
import Network.AWS.Glue.DeleteResourcePolicy
import Network.AWS.Glue.GetConnections
import Network.AWS.Glue.UntagResource
import Network.AWS.Glue.GetSchemaVersionsDiff
import Network.AWS.Glue.SearchTables
import Network.AWS.Glue.GetTrigger
import Network.AWS.Glue.BatchGetJobs
import Network.AWS.Glue.ImportCatalogToGlue
import Network.AWS.Glue.DeleteClassifier
import Network.AWS.Glue.UpdateClassifier
import Network.AWS.Glue.StartJobRun
import Network.AWS.Glue.CreatePartition
import Network.AWS.Glue.BatchGetTriggers
import Network.AWS.Glue.StopCrawlerSchedule
import Network.AWS.Glue.GetSchemaByDefinition
import Network.AWS.Glue.CreateDatabase
import Network.AWS.Glue.GetTableVersions
import Network.AWS.Glue.CreateMLTransform
import Network.AWS.Glue.DeleteSchemaVersions
import Network.AWS.Glue.DeleteTrigger
import Network.AWS.Glue.UpdateTrigger
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Glue'.
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
