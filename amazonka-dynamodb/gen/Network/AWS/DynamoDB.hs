{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon DynamoDB
--
-- Amazon DynamoDB is a fully managed NoSQL database service that provides
-- fast and predictable performance with seamless scalability. DynamoDB
-- lets you offload the administrative burdens of operating and scaling a
-- distributed database, so that you don\'t have to worry about hardware
-- provisioning, setup and configuration, replication, software patching,
-- or cluster scaling.
--
-- With DynamoDB, you can create database tables that can store and
-- retrieve any amount of data, and serve any level of request traffic. You
-- can scale up or scale down your tables\' throughput capacity without
-- downtime or performance degradation, and use the AWS Management Console
-- to monitor resource utilization and performance metrics.
--
-- DynamoDB automatically spreads the data and traffic for your tables over
-- a sufficient number of servers to handle your throughput and storage
-- requirements, while maintaining consistent and fast performance. All of
-- your data is stored on solid state disks (SSDs) and automatically
-- replicated across multiple Availability Zones in an AWS region,
-- providing built-in high availability and data durability.
module Network.AWS.DynamoDB
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidExportTimeException
    _InvalidExportTimeException,

    -- ** TableAlreadyExistsException
    _TableAlreadyExistsException,

    -- ** GlobalTableAlreadyExistsException
    _GlobalTableAlreadyExistsException,

    -- ** ConditionalCheckFailedException
    _ConditionalCheckFailedException,

    -- ** ReplicaAlreadyExistsException
    _ReplicaAlreadyExistsException,

    -- ** TransactionInProgressException
    _TransactionInProgressException,

    -- ** GlobalTableNotFoundException
    _GlobalTableNotFoundException,

    -- ** InvalidRestoreTimeException
    _InvalidRestoreTimeException,

    -- ** PointInTimeRecoveryUnavailableException
    _PointInTimeRecoveryUnavailableException,

    -- ** ExportNotFoundException
    _ExportNotFoundException,

    -- ** BackupInUseException
    _BackupInUseException,

    -- ** ContinuousBackupsUnavailableException
    _ContinuousBackupsUnavailableException,

    -- ** ExportConflictException
    _ExportConflictException,

    -- ** DuplicateItemException
    _DuplicateItemException,

    -- ** TableInUseException
    _TableInUseException,

    -- ** TransactionConflictException
    _TransactionConflictException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** RequestLimitExceeded
    _RequestLimitExceeded,

    -- ** ItemCollectionSizeLimitExceededException
    _ItemCollectionSizeLimitExceededException,

    -- ** ReplicaNotFoundException
    _ReplicaNotFoundException,

    -- ** TransactionCanceledException
    _TransactionCanceledException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ProvisionedThroughputExceededException
    _ProvisionedThroughputExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** BackupNotFoundException
    _BackupNotFoundException,

    -- ** IndexNotFoundException
    _IndexNotFoundException,

    -- ** TableNotFoundException
    _TableNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** TableNotExists
    newTableNotExists,

    -- ** TableExists
    newTableExists,

    -- * Operations
    -- $operations

    -- ** BatchGetItem
    BatchGetItem (BatchGetItem'),
    newBatchGetItem,
    BatchGetItemResponse (BatchGetItemResponse'),
    newBatchGetItemResponse,

    -- ** UpdateContributorInsights
    UpdateContributorInsights (UpdateContributorInsights'),
    newUpdateContributorInsights,
    UpdateContributorInsightsResponse (UpdateContributorInsightsResponse'),
    newUpdateContributorInsightsResponse,

    -- ** DeleteBackup
    DeleteBackup (DeleteBackup'),
    newDeleteBackup,
    DeleteBackupResponse (DeleteBackupResponse'),
    newDeleteBackupResponse,

    -- ** DeleteItem
    DeleteItem (DeleteItem'),
    newDeleteItem,
    DeleteItemResponse (DeleteItemResponse'),
    newDeleteItemResponse,

    -- ** UpdateItem
    UpdateItem (UpdateItem'),
    newUpdateItem,
    UpdateItemResponse (UpdateItemResponse'),
    newUpdateItemResponse,

    -- ** ListContributorInsights
    ListContributorInsights (ListContributorInsights'),
    newListContributorInsights,
    ListContributorInsightsResponse (ListContributorInsightsResponse'),
    newListContributorInsightsResponse,

    -- ** ListGlobalTables
    ListGlobalTables (ListGlobalTables'),
    newListGlobalTables,
    ListGlobalTablesResponse (ListGlobalTablesResponse'),
    newListGlobalTablesResponse,

    -- ** DisableKinesisStreamingDestination
    DisableKinesisStreamingDestination (DisableKinesisStreamingDestination'),
    newDisableKinesisStreamingDestination,
    KinesisStreamingDestinationOutput (KinesisStreamingDestinationOutput'),
    newKinesisStreamingDestinationOutput,

    -- ** UpdateContinuousBackups
    UpdateContinuousBackups (UpdateContinuousBackups'),
    newUpdateContinuousBackups,
    UpdateContinuousBackupsResponse (UpdateContinuousBackupsResponse'),
    newUpdateContinuousBackupsResponse,

    -- ** CreateGlobalTable
    CreateGlobalTable (CreateGlobalTable'),
    newCreateGlobalTable,
    CreateGlobalTableResponse (CreateGlobalTableResponse'),
    newCreateGlobalTableResponse,

    -- ** BatchExecuteStatement
    BatchExecuteStatement (BatchExecuteStatement'),
    newBatchExecuteStatement,
    BatchExecuteStatementResponse (BatchExecuteStatementResponse'),
    newBatchExecuteStatementResponse,

    -- ** RestoreTableFromBackup
    RestoreTableFromBackup (RestoreTableFromBackup'),
    newRestoreTableFromBackup,
    RestoreTableFromBackupResponse (RestoreTableFromBackupResponse'),
    newRestoreTableFromBackupResponse,

    -- ** DescribeLimits
    DescribeLimits (DescribeLimits'),
    newDescribeLimits,
    DescribeLimitsResponse (DescribeLimitsResponse'),
    newDescribeLimitsResponse,

    -- ** ExecuteTransaction
    ExecuteTransaction (ExecuteTransaction'),
    newExecuteTransaction,
    ExecuteTransactionResponse (ExecuteTransactionResponse'),
    newExecuteTransactionResponse,

    -- ** RestoreTableToPointInTime
    RestoreTableToPointInTime (RestoreTableToPointInTime'),
    newRestoreTableToPointInTime,
    RestoreTableToPointInTimeResponse (RestoreTableToPointInTimeResponse'),
    newRestoreTableToPointInTimeResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeContributorInsights
    DescribeContributorInsights (DescribeContributorInsights'),
    newDescribeContributorInsights,
    DescribeContributorInsightsResponse (DescribeContributorInsightsResponse'),
    newDescribeContributorInsightsResponse,

    -- ** DescribeBackup
    DescribeBackup (DescribeBackup'),
    newDescribeBackup,
    DescribeBackupResponse (DescribeBackupResponse'),
    newDescribeBackupResponse,

    -- ** ListTagsOfResource (Paginated)
    ListTagsOfResource (ListTagsOfResource'),
    newListTagsOfResource,
    ListTagsOfResourceResponse (ListTagsOfResourceResponse'),
    newListTagsOfResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeGlobalTableSettings
    DescribeGlobalTableSettings (DescribeGlobalTableSettings'),
    newDescribeGlobalTableSettings,
    DescribeGlobalTableSettingsResponse (DescribeGlobalTableSettingsResponse'),
    newDescribeGlobalTableSettingsResponse,

    -- ** UpdateTableReplicaAutoScaling
    UpdateTableReplicaAutoScaling (UpdateTableReplicaAutoScaling'),
    newUpdateTableReplicaAutoScaling,
    UpdateTableReplicaAutoScalingResponse (UpdateTableReplicaAutoScalingResponse'),
    newUpdateTableReplicaAutoScalingResponse,

    -- ** DescribeTimeToLive
    DescribeTimeToLive (DescribeTimeToLive'),
    newDescribeTimeToLive,
    DescribeTimeToLiveResponse (DescribeTimeToLiveResponse'),
    newDescribeTimeToLiveResponse,

    -- ** Query (Paginated)
    Query (Query'),
    newQuery,
    QueryResponse (QueryResponse'),
    newQueryResponse,

    -- ** CreateTable
    CreateTable (CreateTable'),
    newCreateTable,
    CreateTableResponse (CreateTableResponse'),
    newCreateTableResponse,

    -- ** CreateBackup
    CreateBackup (CreateBackup'),
    newCreateBackup,
    CreateBackupResponse (CreateBackupResponse'),
    newCreateBackupResponse,

    -- ** ListTables (Paginated)
    ListTables (ListTables'),
    newListTables,
    ListTablesResponse (ListTablesResponse'),
    newListTablesResponse,

    -- ** Scan (Paginated)
    Scan (Scan'),
    newScan,
    ScanResponse (ScanResponse'),
    newScanResponse,

    -- ** UpdateTable
    UpdateTable (UpdateTable'),
    newUpdateTable,
    UpdateTableResponse (UpdateTableResponse'),
    newUpdateTableResponse,

    -- ** DeleteTable
    DeleteTable (DeleteTable'),
    newDeleteTable,
    DeleteTableResponse (DeleteTableResponse'),
    newDeleteTableResponse,

    -- ** TransactWriteItems
    TransactWriteItems (TransactWriteItems'),
    newTransactWriteItems,
    TransactWriteItemsResponse (TransactWriteItemsResponse'),
    newTransactWriteItemsResponse,

    -- ** ExportTableToPointInTime
    ExportTableToPointInTime (ExportTableToPointInTime'),
    newExportTableToPointInTime,
    ExportTableToPointInTimeResponse (ExportTableToPointInTimeResponse'),
    newExportTableToPointInTimeResponse,

    -- ** ListBackups (Paginated)
    ListBackups (ListBackups'),
    newListBackups,
    ListBackupsResponse (ListBackupsResponse'),
    newListBackupsResponse,

    -- ** TransactGetItems
    TransactGetItems (TransactGetItems'),
    newTransactGetItems,
    TransactGetItemsResponse (TransactGetItemsResponse'),
    newTransactGetItemsResponse,

    -- ** UpdateGlobalTable
    UpdateGlobalTable (UpdateGlobalTable'),
    newUpdateGlobalTable,
    UpdateGlobalTableResponse (UpdateGlobalTableResponse'),
    newUpdateGlobalTableResponse,

    -- ** BatchWriteItem
    BatchWriteItem (BatchWriteItem'),
    newBatchWriteItem,
    BatchWriteItemResponse (BatchWriteItemResponse'),
    newBatchWriteItemResponse,

    -- ** PutItem
    PutItem (PutItem'),
    newPutItem,
    PutItemResponse (PutItemResponse'),
    newPutItemResponse,

    -- ** UpdateTimeToLive
    UpdateTimeToLive (UpdateTimeToLive'),
    newUpdateTimeToLive,
    UpdateTimeToLiveResponse (UpdateTimeToLiveResponse'),
    newUpdateTimeToLiveResponse,

    -- ** UpdateGlobalTableSettings
    UpdateGlobalTableSettings (UpdateGlobalTableSettings'),
    newUpdateGlobalTableSettings,
    UpdateGlobalTableSettingsResponse (UpdateGlobalTableSettingsResponse'),
    newUpdateGlobalTableSettingsResponse,

    -- ** EnableKinesisStreamingDestination
    EnableKinesisStreamingDestination (EnableKinesisStreamingDestination'),
    newEnableKinesisStreamingDestination,
    KinesisStreamingDestinationOutput (KinesisStreamingDestinationOutput'),
    newKinesisStreamingDestinationOutput,

    -- ** DescribeExport
    DescribeExport (DescribeExport'),
    newDescribeExport,
    DescribeExportResponse (DescribeExportResponse'),
    newDescribeExportResponse,

    -- ** DescribeTableReplicaAutoScaling
    DescribeTableReplicaAutoScaling (DescribeTableReplicaAutoScaling'),
    newDescribeTableReplicaAutoScaling,
    DescribeTableReplicaAutoScalingResponse (DescribeTableReplicaAutoScalingResponse'),
    newDescribeTableReplicaAutoScalingResponse,

    -- ** GetItem
    GetItem (GetItem'),
    newGetItem,
    GetItemResponse (GetItemResponse'),
    newGetItemResponse,

    -- ** DescribeTable
    DescribeTable (DescribeTable'),
    newDescribeTable,
    DescribeTableResponse (DescribeTableResponse'),
    newDescribeTableResponse,

    -- ** DescribeGlobalTable
    DescribeGlobalTable (DescribeGlobalTable'),
    newDescribeGlobalTable,
    DescribeGlobalTableResponse (DescribeGlobalTableResponse'),
    newDescribeGlobalTableResponse,

    -- ** ListExports
    ListExports (ListExports'),
    newListExports,
    ListExportsResponse (ListExportsResponse'),
    newListExportsResponse,

    -- ** DescribeContinuousBackups
    DescribeContinuousBackups (DescribeContinuousBackups'),
    newDescribeContinuousBackups,
    DescribeContinuousBackupsResponse (DescribeContinuousBackupsResponse'),
    newDescribeContinuousBackupsResponse,

    -- ** DescribeKinesisStreamingDestination
    DescribeKinesisStreamingDestination (DescribeKinesisStreamingDestination'),
    newDescribeKinesisStreamingDestination,
    DescribeKinesisStreamingDestinationResponse (DescribeKinesisStreamingDestinationResponse'),
    newDescribeKinesisStreamingDestinationResponse,

    -- ** DescribeEndpoints
    DescribeEndpoints (DescribeEndpoints'),
    newDescribeEndpoints,
    DescribeEndpointsResponse (DescribeEndpointsResponse'),
    newDescribeEndpointsResponse,

    -- ** ExecuteStatement
    ExecuteStatement (ExecuteStatement'),
    newExecuteStatement,
    ExecuteStatementResponse (ExecuteStatementResponse'),
    newExecuteStatementResponse,

    -- * Types

    -- ** AttributeAction
    AttributeAction (..),

    -- ** BackupStatus
    BackupStatus (..),

    -- ** BackupType
    BackupType (..),

    -- ** BackupTypeFilter
    BackupTypeFilter (..),

    -- ** BatchStatementErrorCodeEnum
    BatchStatementErrorCodeEnum (..),

    -- ** BillingMode
    BillingMode (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** ConditionalOperator
    ConditionalOperator (..),

    -- ** ContinuousBackupsStatus
    ContinuousBackupsStatus (..),

    -- ** ContributorInsightsAction
    ContributorInsightsAction (..),

    -- ** ContributorInsightsStatus
    ContributorInsightsStatus (..),

    -- ** DestinationStatus
    DestinationStatus (..),

    -- ** ExportFormat
    ExportFormat (..),

    -- ** ExportStatus
    ExportStatus (..),

    -- ** GlobalTableStatus
    GlobalTableStatus (..),

    -- ** IndexStatus
    IndexStatus (..),

    -- ** KeyType
    KeyType (..),

    -- ** PointInTimeRecoveryStatus
    PointInTimeRecoveryStatus (..),

    -- ** ProjectionType
    ProjectionType (..),

    -- ** ReplicaStatus
    ReplicaStatus (..),

    -- ** ReturnConsumedCapacity
    ReturnConsumedCapacity (..),

    -- ** ReturnItemCollectionMetrics
    ReturnItemCollectionMetrics (..),

    -- ** ReturnValue
    ReturnValue (..),

    -- ** ReturnValuesOnConditionCheckFailure
    ReturnValuesOnConditionCheckFailure (..),

    -- ** S3SseAlgorithm
    S3SseAlgorithm (..),

    -- ** SSEStatus
    SSEStatus (..),

    -- ** SSEType
    SSEType (..),

    -- ** ScalarAttributeType
    ScalarAttributeType (..),

    -- ** Select
    Select (..),

    -- ** StreamViewType
    StreamViewType (..),

    -- ** TableStatus
    TableStatus (..),

    -- ** TimeToLiveStatus
    TimeToLiveStatus (..),

    -- ** ArchivalSummary
    ArchivalSummary (ArchivalSummary'),
    newArchivalSummary,

    -- ** AttributeDefinition
    AttributeDefinition (AttributeDefinition'),
    newAttributeDefinition,

    -- ** AttributeValue
    AttributeValue (AttributeValue'),
    newAttributeValue,

    -- ** AttributeValueUpdate
    AttributeValueUpdate (AttributeValueUpdate'),
    newAttributeValueUpdate,

    -- ** AutoScalingPolicyDescription
    AutoScalingPolicyDescription (AutoScalingPolicyDescription'),
    newAutoScalingPolicyDescription,

    -- ** AutoScalingPolicyUpdate
    AutoScalingPolicyUpdate (AutoScalingPolicyUpdate'),
    newAutoScalingPolicyUpdate,

    -- ** AutoScalingSettingsDescription
    AutoScalingSettingsDescription (AutoScalingSettingsDescription'),
    newAutoScalingSettingsDescription,

    -- ** AutoScalingSettingsUpdate
    AutoScalingSettingsUpdate (AutoScalingSettingsUpdate'),
    newAutoScalingSettingsUpdate,

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription (AutoScalingTargetTrackingScalingPolicyConfigurationDescription'),
    newAutoScalingTargetTrackingScalingPolicyConfigurationDescription,

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (AutoScalingTargetTrackingScalingPolicyConfigurationUpdate'),
    newAutoScalingTargetTrackingScalingPolicyConfigurationUpdate,

    -- ** BackupDescription
    BackupDescription (BackupDescription'),
    newBackupDescription,

    -- ** BackupDetails
    BackupDetails (BackupDetails'),
    newBackupDetails,

    -- ** BackupSummary
    BackupSummary (BackupSummary'),
    newBackupSummary,

    -- ** BatchStatementError
    BatchStatementError (BatchStatementError'),
    newBatchStatementError,

    -- ** BatchStatementRequest
    BatchStatementRequest (BatchStatementRequest'),
    newBatchStatementRequest,

    -- ** BatchStatementResponse
    BatchStatementResponse (BatchStatementResponse'),
    newBatchStatementResponse,

    -- ** BillingModeSummary
    BillingModeSummary (BillingModeSummary'),
    newBillingModeSummary,

    -- ** Capacity
    Capacity (Capacity'),
    newCapacity,

    -- ** Condition
    Condition (Condition'),
    newCondition,

    -- ** ConditionCheck
    ConditionCheck (ConditionCheck'),
    newConditionCheck,

    -- ** ConsumedCapacity
    ConsumedCapacity (ConsumedCapacity'),
    newConsumedCapacity,

    -- ** ContinuousBackupsDescription
    ContinuousBackupsDescription (ContinuousBackupsDescription'),
    newContinuousBackupsDescription,

    -- ** ContributorInsightsSummary
    ContributorInsightsSummary (ContributorInsightsSummary'),
    newContributorInsightsSummary,

    -- ** CreateGlobalSecondaryIndexAction
    CreateGlobalSecondaryIndexAction (CreateGlobalSecondaryIndexAction'),
    newCreateGlobalSecondaryIndexAction,

    -- ** CreateReplicaAction
    CreateReplicaAction (CreateReplicaAction'),
    newCreateReplicaAction,

    -- ** CreateReplicationGroupMemberAction
    CreateReplicationGroupMemberAction (CreateReplicationGroupMemberAction'),
    newCreateReplicationGroupMemberAction,

    -- ** Delete
    Delete (Delete'),
    newDelete,

    -- ** DeleteGlobalSecondaryIndexAction
    DeleteGlobalSecondaryIndexAction (DeleteGlobalSecondaryIndexAction'),
    newDeleteGlobalSecondaryIndexAction,

    -- ** DeleteReplicaAction
    DeleteReplicaAction (DeleteReplicaAction'),
    newDeleteReplicaAction,

    -- ** DeleteReplicationGroupMemberAction
    DeleteReplicationGroupMemberAction (DeleteReplicationGroupMemberAction'),
    newDeleteReplicationGroupMemberAction,

    -- ** DeleteRequest
    DeleteRequest (DeleteRequest'),
    newDeleteRequest,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** ExpectedAttributeValue
    ExpectedAttributeValue (ExpectedAttributeValue'),
    newExpectedAttributeValue,

    -- ** ExportDescription
    ExportDescription (ExportDescription'),
    newExportDescription,

    -- ** ExportSummary
    ExportSummary (ExportSummary'),
    newExportSummary,

    -- ** FailureException
    FailureException (FailureException'),
    newFailureException,

    -- ** Get
    Get (Get'),
    newGet,

    -- ** GlobalSecondaryIndex
    GlobalSecondaryIndex (GlobalSecondaryIndex'),
    newGlobalSecondaryIndex,

    -- ** GlobalSecondaryIndexAutoScalingUpdate
    GlobalSecondaryIndexAutoScalingUpdate (GlobalSecondaryIndexAutoScalingUpdate'),
    newGlobalSecondaryIndexAutoScalingUpdate,

    -- ** GlobalSecondaryIndexDescription
    GlobalSecondaryIndexDescription (GlobalSecondaryIndexDescription'),
    newGlobalSecondaryIndexDescription,

    -- ** GlobalSecondaryIndexInfo
    GlobalSecondaryIndexInfo (GlobalSecondaryIndexInfo'),
    newGlobalSecondaryIndexInfo,

    -- ** GlobalSecondaryIndexUpdate
    GlobalSecondaryIndexUpdate (GlobalSecondaryIndexUpdate'),
    newGlobalSecondaryIndexUpdate,

    -- ** GlobalTable
    GlobalTable (GlobalTable'),
    newGlobalTable,

    -- ** GlobalTableDescription
    GlobalTableDescription (GlobalTableDescription'),
    newGlobalTableDescription,

    -- ** GlobalTableGlobalSecondaryIndexSettingsUpdate
    GlobalTableGlobalSecondaryIndexSettingsUpdate (GlobalTableGlobalSecondaryIndexSettingsUpdate'),
    newGlobalTableGlobalSecondaryIndexSettingsUpdate,

    -- ** ItemCollectionMetrics
    ItemCollectionMetrics (ItemCollectionMetrics'),
    newItemCollectionMetrics,

    -- ** ItemResponse
    ItemResponse (ItemResponse'),
    newItemResponse,

    -- ** KeySchemaElement
    KeySchemaElement (KeySchemaElement'),
    newKeySchemaElement,

    -- ** KeysAndAttributes
    KeysAndAttributes (KeysAndAttributes'),
    newKeysAndAttributes,

    -- ** KinesisDataStreamDestination
    KinesisDataStreamDestination (KinesisDataStreamDestination'),
    newKinesisDataStreamDestination,

    -- ** KinesisStreamingDestinationInput
    KinesisStreamingDestinationInput (KinesisStreamingDestinationInput'),
    newKinesisStreamingDestinationInput,

    -- ** KinesisStreamingDestinationOutput
    KinesisStreamingDestinationOutput (KinesisStreamingDestinationOutput'),
    newKinesisStreamingDestinationOutput,

    -- ** LocalSecondaryIndex
    LocalSecondaryIndex (LocalSecondaryIndex'),
    newLocalSecondaryIndex,

    -- ** LocalSecondaryIndexDescription
    LocalSecondaryIndexDescription (LocalSecondaryIndexDescription'),
    newLocalSecondaryIndexDescription,

    -- ** LocalSecondaryIndexInfo
    LocalSecondaryIndexInfo (LocalSecondaryIndexInfo'),
    newLocalSecondaryIndexInfo,

    -- ** ParameterizedStatement
    ParameterizedStatement (ParameterizedStatement'),
    newParameterizedStatement,

    -- ** PointInTimeRecoveryDescription
    PointInTimeRecoveryDescription (PointInTimeRecoveryDescription'),
    newPointInTimeRecoveryDescription,

    -- ** PointInTimeRecoverySpecification
    PointInTimeRecoverySpecification (PointInTimeRecoverySpecification'),
    newPointInTimeRecoverySpecification,

    -- ** Projection
    Projection (Projection'),
    newProjection,

    -- ** ProvisionedThroughput
    ProvisionedThroughput (ProvisionedThroughput'),
    newProvisionedThroughput,

    -- ** ProvisionedThroughputDescription
    ProvisionedThroughputDescription (ProvisionedThroughputDescription'),
    newProvisionedThroughputDescription,

    -- ** ProvisionedThroughputOverride
    ProvisionedThroughputOverride (ProvisionedThroughputOverride'),
    newProvisionedThroughputOverride,

    -- ** Put
    Put (Put'),
    newPut,

    -- ** PutRequest
    PutRequest (PutRequest'),
    newPutRequest,

    -- ** Replica
    Replica (Replica'),
    newReplica,

    -- ** ReplicaAutoScalingDescription
    ReplicaAutoScalingDescription (ReplicaAutoScalingDescription'),
    newReplicaAutoScalingDescription,

    -- ** ReplicaAutoScalingUpdate
    ReplicaAutoScalingUpdate (ReplicaAutoScalingUpdate'),
    newReplicaAutoScalingUpdate,

    -- ** ReplicaDescription
    ReplicaDescription (ReplicaDescription'),
    newReplicaDescription,

    -- ** ReplicaGlobalSecondaryIndex
    ReplicaGlobalSecondaryIndex (ReplicaGlobalSecondaryIndex'),
    newReplicaGlobalSecondaryIndex,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingDescription
    ReplicaGlobalSecondaryIndexAutoScalingDescription (ReplicaGlobalSecondaryIndexAutoScalingDescription'),
    newReplicaGlobalSecondaryIndexAutoScalingDescription,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingUpdate
    ReplicaGlobalSecondaryIndexAutoScalingUpdate (ReplicaGlobalSecondaryIndexAutoScalingUpdate'),
    newReplicaGlobalSecondaryIndexAutoScalingUpdate,

    -- ** ReplicaGlobalSecondaryIndexDescription
    ReplicaGlobalSecondaryIndexDescription (ReplicaGlobalSecondaryIndexDescription'),
    newReplicaGlobalSecondaryIndexDescription,

    -- ** ReplicaGlobalSecondaryIndexSettingsDescription
    ReplicaGlobalSecondaryIndexSettingsDescription (ReplicaGlobalSecondaryIndexSettingsDescription'),
    newReplicaGlobalSecondaryIndexSettingsDescription,

    -- ** ReplicaGlobalSecondaryIndexSettingsUpdate
    ReplicaGlobalSecondaryIndexSettingsUpdate (ReplicaGlobalSecondaryIndexSettingsUpdate'),
    newReplicaGlobalSecondaryIndexSettingsUpdate,

    -- ** ReplicaSettingsDescription
    ReplicaSettingsDescription (ReplicaSettingsDescription'),
    newReplicaSettingsDescription,

    -- ** ReplicaSettingsUpdate
    ReplicaSettingsUpdate (ReplicaSettingsUpdate'),
    newReplicaSettingsUpdate,

    -- ** ReplicaUpdate
    ReplicaUpdate (ReplicaUpdate'),
    newReplicaUpdate,

    -- ** ReplicationGroupUpdate
    ReplicationGroupUpdate (ReplicationGroupUpdate'),
    newReplicationGroupUpdate,

    -- ** RestoreSummary
    RestoreSummary (RestoreSummary'),
    newRestoreSummary,

    -- ** SSEDescription
    SSEDescription (SSEDescription'),
    newSSEDescription,

    -- ** SSESpecification
    SSESpecification (SSESpecification'),
    newSSESpecification,

    -- ** SourceTableDetails
    SourceTableDetails (SourceTableDetails'),
    newSourceTableDetails,

    -- ** SourceTableFeatureDetails
    SourceTableFeatureDetails (SourceTableFeatureDetails'),
    newSourceTableFeatureDetails,

    -- ** StreamSpecification
    StreamSpecification (StreamSpecification'),
    newStreamSpecification,

    -- ** TableAutoScalingDescription
    TableAutoScalingDescription (TableAutoScalingDescription'),
    newTableAutoScalingDescription,

    -- ** TableDescription
    TableDescription (TableDescription'),
    newTableDescription,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TimeToLiveDescription
    TimeToLiveDescription (TimeToLiveDescription'),
    newTimeToLiveDescription,

    -- ** TimeToLiveSpecification
    TimeToLiveSpecification (TimeToLiveSpecification'),
    newTimeToLiveSpecification,

    -- ** TransactGetItem
    TransactGetItem (TransactGetItem'),
    newTransactGetItem,

    -- ** TransactWriteItem
    TransactWriteItem (TransactWriteItem'),
    newTransactWriteItem,

    -- ** Update
    Update (Update'),
    newUpdate,

    -- ** UpdateGlobalSecondaryIndexAction
    UpdateGlobalSecondaryIndexAction (UpdateGlobalSecondaryIndexAction'),
    newUpdateGlobalSecondaryIndexAction,

    -- ** UpdateReplicationGroupMemberAction
    UpdateReplicationGroupMemberAction (UpdateReplicationGroupMemberAction'),
    newUpdateReplicationGroupMemberAction,

    -- ** WriteRequest
    WriteRequest (WriteRequest'),
    newWriteRequest,
  )
where

import Network.AWS.DynamoDB.BatchExecuteStatement
import Network.AWS.DynamoDB.BatchGetItem
import Network.AWS.DynamoDB.BatchWriteItem
import Network.AWS.DynamoDB.CreateBackup
import Network.AWS.DynamoDB.CreateGlobalTable
import Network.AWS.DynamoDB.CreateTable
import Network.AWS.DynamoDB.DeleteBackup
import Network.AWS.DynamoDB.DeleteItem
import Network.AWS.DynamoDB.DeleteTable
import Network.AWS.DynamoDB.DescribeBackup
import Network.AWS.DynamoDB.DescribeContinuousBackups
import Network.AWS.DynamoDB.DescribeContributorInsights
import Network.AWS.DynamoDB.DescribeEndpoints
import Network.AWS.DynamoDB.DescribeExport
import Network.AWS.DynamoDB.DescribeGlobalTable
import Network.AWS.DynamoDB.DescribeGlobalTableSettings
import Network.AWS.DynamoDB.DescribeKinesisStreamingDestination
import Network.AWS.DynamoDB.DescribeLimits
import Network.AWS.DynamoDB.DescribeTable
import Network.AWS.DynamoDB.DescribeTableReplicaAutoScaling
import Network.AWS.DynamoDB.DescribeTimeToLive
import Network.AWS.DynamoDB.DisableKinesisStreamingDestination
import Network.AWS.DynamoDB.EnableKinesisStreamingDestination
import Network.AWS.DynamoDB.ExecuteStatement
import Network.AWS.DynamoDB.ExecuteTransaction
import Network.AWS.DynamoDB.ExportTableToPointInTime
import Network.AWS.DynamoDB.GetItem
import Network.AWS.DynamoDB.Lens
import Network.AWS.DynamoDB.ListBackups
import Network.AWS.DynamoDB.ListContributorInsights
import Network.AWS.DynamoDB.ListExports
import Network.AWS.DynamoDB.ListGlobalTables
import Network.AWS.DynamoDB.ListTables
import Network.AWS.DynamoDB.ListTagsOfResource
import Network.AWS.DynamoDB.PutItem
import Network.AWS.DynamoDB.Query
import Network.AWS.DynamoDB.RestoreTableFromBackup
import Network.AWS.DynamoDB.RestoreTableToPointInTime
import Network.AWS.DynamoDB.Scan
import Network.AWS.DynamoDB.TagResource
import Network.AWS.DynamoDB.TransactGetItems
import Network.AWS.DynamoDB.TransactWriteItems
import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.UntagResource
import Network.AWS.DynamoDB.UpdateContinuousBackups
import Network.AWS.DynamoDB.UpdateContributorInsights
import Network.AWS.DynamoDB.UpdateGlobalTable
import Network.AWS.DynamoDB.UpdateGlobalTableSettings
import Network.AWS.DynamoDB.UpdateItem
import Network.AWS.DynamoDB.UpdateTable
import Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
import Network.AWS.DynamoDB.UpdateTimeToLive
import Network.AWS.DynamoDB.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DynamoDB'.

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
