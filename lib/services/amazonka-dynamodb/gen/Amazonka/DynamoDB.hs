{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DynamoDB
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2012-08-10@ of the AWS service descriptions, licensed under Apache 2.0.
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
module Amazonka.DynamoDB
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidExportTimeException
    _InvalidExportTimeException,

    -- ** BackupNotFoundException
    _BackupNotFoundException,

    -- ** TableInUseException
    _TableInUseException,

    -- ** ExportConflictException
    _ExportConflictException,

    -- ** ContinuousBackupsUnavailableException
    _ContinuousBackupsUnavailableException,

    -- ** ProvisionedThroughputExceededException
    _ProvisionedThroughputExceededException,

    -- ** GlobalTableNotFoundException
    _GlobalTableNotFoundException,

    -- ** TransactionInProgressException
    _TransactionInProgressException,

    -- ** TransactionCanceledException
    _TransactionCanceledException,

    -- ** ConditionalCheckFailedException
    _ConditionalCheckFailedException,

    -- ** GlobalTableAlreadyExistsException
    _GlobalTableAlreadyExistsException,

    -- ** ReplicaNotFoundException
    _ReplicaNotFoundException,

    -- ** TableAlreadyExistsException
    _TableAlreadyExistsException,

    -- ** RequestLimitExceeded
    _RequestLimitExceeded,

    -- ** ItemCollectionSizeLimitExceededException
    _ItemCollectionSizeLimitExceededException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** TableNotFoundException
    _TableNotFoundException,

    -- ** IndexNotFoundException
    _IndexNotFoundException,

    -- ** TransactionConflictException
    _TransactionConflictException,

    -- ** BackupInUseException
    _BackupInUseException,

    -- ** DuplicateItemException
    _DuplicateItemException,

    -- ** ExportNotFoundException
    _ExportNotFoundException,

    -- ** PointInTimeRecoveryUnavailableException
    _PointInTimeRecoveryUnavailableException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** InvalidRestoreTimeException
    _InvalidRestoreTimeException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ReplicaAlreadyExistsException
    _ReplicaAlreadyExistsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- ** TableNotExists
    newTableNotExists,

    -- ** TableExists
    newTableExists,

    -- * Operations
    -- $operations

    -- ** PutItem
    PutItem (PutItem'),
    newPutItem,
    PutItemResponse (PutItemResponse'),
    newPutItemResponse,

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

    -- ** DisableKinesisStreamingDestination
    DisableKinesisStreamingDestination (DisableKinesisStreamingDestination'),
    newDisableKinesisStreamingDestination,
    KinesisStreamingDestinationOutput (KinesisStreamingDestinationOutput'),
    newKinesisStreamingDestinationOutput,

    -- ** ListGlobalTables
    ListGlobalTables (ListGlobalTables'),
    newListGlobalTables,
    ListGlobalTablesResponse (ListGlobalTablesResponse'),
    newListGlobalTablesResponse,

    -- ** UpdateGlobalTable
    UpdateGlobalTable (UpdateGlobalTable'),
    newUpdateGlobalTable,
    UpdateGlobalTableResponse (UpdateGlobalTableResponse'),
    newUpdateGlobalTableResponse,

    -- ** DeleteTable
    DeleteTable (DeleteTable'),
    newDeleteTable,
    DeleteTableResponse (DeleteTableResponse'),
    newDeleteTableResponse,

    -- ** UpdateTable
    UpdateTable (UpdateTable'),
    newUpdateTable,
    UpdateTableResponse (UpdateTableResponse'),
    newUpdateTableResponse,

    -- ** BatchGetItem
    BatchGetItem (BatchGetItem'),
    newBatchGetItem,
    BatchGetItemResponse (BatchGetItemResponse'),
    newBatchGetItemResponse,

    -- ** ListBackups (Paginated)
    ListBackups (ListBackups'),
    newListBackups,
    ListBackupsResponse (ListBackupsResponse'),
    newListBackupsResponse,

    -- ** DeleteBackup
    DeleteBackup (DeleteBackup'),
    newDeleteBackup,
    DeleteBackupResponse (DeleteBackupResponse'),
    newDeleteBackupResponse,

    -- ** CreateBackup
    CreateBackup (CreateBackup'),
    newCreateBackup,
    CreateBackupResponse (CreateBackupResponse'),
    newCreateBackupResponse,

    -- ** UpdateTableReplicaAutoScaling
    UpdateTableReplicaAutoScaling (UpdateTableReplicaAutoScaling'),
    newUpdateTableReplicaAutoScaling,
    UpdateTableReplicaAutoScalingResponse (UpdateTableReplicaAutoScalingResponse'),
    newUpdateTableReplicaAutoScalingResponse,

    -- ** DescribeGlobalTableSettings
    DescribeGlobalTableSettings (DescribeGlobalTableSettings'),
    newDescribeGlobalTableSettings,
    DescribeGlobalTableSettingsResponse (DescribeGlobalTableSettingsResponse'),
    newDescribeGlobalTableSettingsResponse,

    -- ** ListTagsOfResource (Paginated)
    ListTagsOfResource (ListTagsOfResource'),
    newListTagsOfResource,
    ListTagsOfResourceResponse (ListTagsOfResourceResponse'),
    newListTagsOfResourceResponse,

    -- ** DescribeGlobalTable
    DescribeGlobalTable (DescribeGlobalTable'),
    newDescribeGlobalTable,
    DescribeGlobalTableResponse (DescribeGlobalTableResponse'),
    newDescribeGlobalTableResponse,

    -- ** DescribeTable
    DescribeTable (DescribeTable'),
    newDescribeTable,
    DescribeTableResponse (DescribeTableResponse'),
    newDescribeTableResponse,

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

    -- ** GetItem
    GetItem (GetItem'),
    newGetItem,
    GetItemResponse (GetItemResponse'),
    newGetItemResponse,

    -- ** DescribeBackup
    DescribeBackup (DescribeBackup'),
    newDescribeBackup,
    DescribeBackupResponse (DescribeBackupResponse'),
    newDescribeBackupResponse,

    -- ** BatchExecuteStatement
    BatchExecuteStatement (BatchExecuteStatement'),
    newBatchExecuteStatement,
    BatchExecuteStatementResponse (BatchExecuteStatementResponse'),
    newBatchExecuteStatementResponse,

    -- ** DescribeTableReplicaAutoScaling
    DescribeTableReplicaAutoScaling (DescribeTableReplicaAutoScaling'),
    newDescribeTableReplicaAutoScaling,
    DescribeTableReplicaAutoScalingResponse (DescribeTableReplicaAutoScalingResponse'),
    newDescribeTableReplicaAutoScalingResponse,

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

    -- ** TransactGetItems
    TransactGetItems (TransactGetItems'),
    newTransactGetItems,
    TransactGetItemsResponse (TransactGetItemsResponse'),
    newTransactGetItemsResponse,

    -- ** ListContributorInsights
    ListContributorInsights (ListContributorInsights'),
    newListContributorInsights,
    ListContributorInsightsResponse (ListContributorInsightsResponse'),
    newListContributorInsightsResponse,

    -- ** BatchWriteItem
    BatchWriteItem (BatchWriteItem'),
    newBatchWriteItem,
    BatchWriteItemResponse (BatchWriteItemResponse'),
    newBatchWriteItemResponse,

    -- ** ExportTableToPointInTime
    ExportTableToPointInTime (ExportTableToPointInTime'),
    newExportTableToPointInTime,
    ExportTableToPointInTimeResponse (ExportTableToPointInTimeResponse'),
    newExportTableToPointInTimeResponse,

    -- ** TransactWriteItems
    TransactWriteItems (TransactWriteItems'),
    newTransactWriteItems,
    TransactWriteItemsResponse (TransactWriteItemsResponse'),
    newTransactWriteItemsResponse,

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

    -- ** UpdateContributorInsights
    UpdateContributorInsights (UpdateContributorInsights'),
    newUpdateContributorInsights,
    UpdateContributorInsightsResponse (UpdateContributorInsightsResponse'),
    newUpdateContributorInsightsResponse,

    -- ** ExecuteStatement
    ExecuteStatement (ExecuteStatement'),
    newExecuteStatement,
    ExecuteStatementResponse (ExecuteStatementResponse'),
    newExecuteStatementResponse,

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

    -- ** DescribeTimeToLive
    DescribeTimeToLive (DescribeTimeToLive'),
    newDescribeTimeToLive,
    DescribeTimeToLiveResponse (DescribeTimeToLiveResponse'),
    newDescribeTimeToLiveResponse,

    -- ** DescribeContinuousBackups
    DescribeContinuousBackups (DescribeContinuousBackups'),
    newDescribeContinuousBackups,
    DescribeContinuousBackupsResponse (DescribeContinuousBackupsResponse'),
    newDescribeContinuousBackupsResponse,

    -- ** ListExports
    ListExports (ListExports'),
    newListExports,
    ListExportsResponse (ListExportsResponse'),
    newListExportsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeContributorInsights
    DescribeContributorInsights (DescribeContributorInsights'),
    newDescribeContributorInsights,
    DescribeContributorInsightsResponse (DescribeContributorInsightsResponse'),
    newDescribeContributorInsightsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** RestoreTableToPointInTime
    RestoreTableToPointInTime (RestoreTableToPointInTime'),
    newRestoreTableToPointInTime,
    RestoreTableToPointInTimeResponse (RestoreTableToPointInTimeResponse'),
    newRestoreTableToPointInTimeResponse,

    -- ** RestoreTableFromBackup
    RestoreTableFromBackup (RestoreTableFromBackup'),
    newRestoreTableFromBackup,
    RestoreTableFromBackupResponse (RestoreTableFromBackupResponse'),
    newRestoreTableFromBackupResponse,

    -- ** UpdateTimeToLive
    UpdateTimeToLive (UpdateTimeToLive'),
    newUpdateTimeToLive,
    UpdateTimeToLiveResponse (UpdateTimeToLiveResponse'),
    newUpdateTimeToLiveResponse,

    -- ** CreateGlobalTable
    CreateGlobalTable (CreateGlobalTable'),
    newCreateGlobalTable,
    CreateGlobalTableResponse (CreateGlobalTableResponse'),
    newCreateGlobalTableResponse,

    -- ** UpdateContinuousBackups
    UpdateContinuousBackups (UpdateContinuousBackups'),
    newUpdateContinuousBackups,
    UpdateContinuousBackupsResponse (UpdateContinuousBackupsResponse'),
    newUpdateContinuousBackupsResponse,

    -- ** DescribeExport
    DescribeExport (DescribeExport'),
    newDescribeExport,
    DescribeExportResponse (DescribeExportResponse'),
    newDescribeExportResponse,

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

import Amazonka.DynamoDB.BatchExecuteStatement
import Amazonka.DynamoDB.BatchGetItem
import Amazonka.DynamoDB.BatchWriteItem
import Amazonka.DynamoDB.CreateBackup
import Amazonka.DynamoDB.CreateGlobalTable
import Amazonka.DynamoDB.CreateTable
import Amazonka.DynamoDB.DeleteBackup
import Amazonka.DynamoDB.DeleteItem
import Amazonka.DynamoDB.DeleteTable
import Amazonka.DynamoDB.DescribeBackup
import Amazonka.DynamoDB.DescribeContinuousBackups
import Amazonka.DynamoDB.DescribeContributorInsights
import Amazonka.DynamoDB.DescribeEndpoints
import Amazonka.DynamoDB.DescribeExport
import Amazonka.DynamoDB.DescribeGlobalTable
import Amazonka.DynamoDB.DescribeGlobalTableSettings
import Amazonka.DynamoDB.DescribeKinesisStreamingDestination
import Amazonka.DynamoDB.DescribeLimits
import Amazonka.DynamoDB.DescribeTable
import Amazonka.DynamoDB.DescribeTableReplicaAutoScaling
import Amazonka.DynamoDB.DescribeTimeToLive
import Amazonka.DynamoDB.DisableKinesisStreamingDestination
import Amazonka.DynamoDB.EnableKinesisStreamingDestination
import Amazonka.DynamoDB.ExecuteStatement
import Amazonka.DynamoDB.ExecuteTransaction
import Amazonka.DynamoDB.ExportTableToPointInTime
import Amazonka.DynamoDB.GetItem
import Amazonka.DynamoDB.Lens
import Amazonka.DynamoDB.ListBackups
import Amazonka.DynamoDB.ListContributorInsights
import Amazonka.DynamoDB.ListExports
import Amazonka.DynamoDB.ListGlobalTables
import Amazonka.DynamoDB.ListTables
import Amazonka.DynamoDB.ListTagsOfResource
import Amazonka.DynamoDB.PutItem
import Amazonka.DynamoDB.Query
import Amazonka.DynamoDB.RestoreTableFromBackup
import Amazonka.DynamoDB.RestoreTableToPointInTime
import Amazonka.DynamoDB.Scan
import Amazonka.DynamoDB.TagResource
import Amazonka.DynamoDB.TransactGetItems
import Amazonka.DynamoDB.TransactWriteItems
import Amazonka.DynamoDB.Types
import Amazonka.DynamoDB.UntagResource
import Amazonka.DynamoDB.UpdateContinuousBackups
import Amazonka.DynamoDB.UpdateContributorInsights
import Amazonka.DynamoDB.UpdateGlobalTable
import Amazonka.DynamoDB.UpdateGlobalTableSettings
import Amazonka.DynamoDB.UpdateItem
import Amazonka.DynamoDB.UpdateTable
import Amazonka.DynamoDB.UpdateTableReplicaAutoScaling
import Amazonka.DynamoDB.UpdateTimeToLive
import Amazonka.DynamoDB.Waiters

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
