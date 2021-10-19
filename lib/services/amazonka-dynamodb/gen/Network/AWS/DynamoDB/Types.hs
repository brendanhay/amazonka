{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidExportTimeException,
    _BackupNotFoundException,
    _TableInUseException,
    _ExportConflictException,
    _ContinuousBackupsUnavailableException,
    _ProvisionedThroughputExceededException,
    _GlobalTableNotFoundException,
    _TransactionInProgressException,
    _TransactionCanceledException,
    _ConditionalCheckFailedException,
    _GlobalTableAlreadyExistsException,
    _ReplicaNotFoundException,
    _TableAlreadyExistsException,
    _RequestLimitExceeded,
    _ItemCollectionSizeLimitExceededException,
    _InternalServerError,
    _TableNotFoundException,
    _IndexNotFoundException,
    _TransactionConflictException,
    _BackupInUseException,
    _DuplicateItemException,
    _ExportNotFoundException,
    _PointInTimeRecoveryUnavailableException,
    _IdempotentParameterMismatchException,
    _InvalidRestoreTimeException,
    _ResourceNotFoundException,
    _ReplicaAlreadyExistsException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * AttributeAction
    AttributeAction (..),

    -- * BackupStatus
    BackupStatus (..),

    -- * BackupType
    BackupType (..),

    -- * BackupTypeFilter
    BackupTypeFilter (..),

    -- * BatchStatementErrorCodeEnum
    BatchStatementErrorCodeEnum (..),

    -- * BillingMode
    BillingMode (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * ConditionalOperator
    ConditionalOperator (..),

    -- * ContinuousBackupsStatus
    ContinuousBackupsStatus (..),

    -- * ContributorInsightsAction
    ContributorInsightsAction (..),

    -- * ContributorInsightsStatus
    ContributorInsightsStatus (..),

    -- * DestinationStatus
    DestinationStatus (..),

    -- * ExportFormat
    ExportFormat (..),

    -- * ExportStatus
    ExportStatus (..),

    -- * GlobalTableStatus
    GlobalTableStatus (..),

    -- * IndexStatus
    IndexStatus (..),

    -- * KeyType
    KeyType (..),

    -- * PointInTimeRecoveryStatus
    PointInTimeRecoveryStatus (..),

    -- * ProjectionType
    ProjectionType (..),

    -- * ReplicaStatus
    ReplicaStatus (..),

    -- * ReturnConsumedCapacity
    ReturnConsumedCapacity (..),

    -- * ReturnItemCollectionMetrics
    ReturnItemCollectionMetrics (..),

    -- * ReturnValue
    ReturnValue (..),

    -- * ReturnValuesOnConditionCheckFailure
    ReturnValuesOnConditionCheckFailure (..),

    -- * S3SseAlgorithm
    S3SseAlgorithm (..),

    -- * SSEStatus
    SSEStatus (..),

    -- * SSEType
    SSEType (..),

    -- * ScalarAttributeType
    ScalarAttributeType (..),

    -- * Select
    Select (..),

    -- * StreamViewType
    StreamViewType (..),

    -- * TableStatus
    TableStatus (..),

    -- * TimeToLiveStatus
    TimeToLiveStatus (..),

    -- * ArchivalSummary
    ArchivalSummary (..),
    newArchivalSummary,
    archivalSummary_archivalReason,
    archivalSummary_archivalDateTime,
    archivalSummary_archivalBackupArn,

    -- * AttributeDefinition
    AttributeDefinition (..),
    newAttributeDefinition,
    attributeDefinition_attributeName,
    attributeDefinition_attributeType,

    -- * AttributeValue
    AttributeValue (..),
    newAttributeValue,
    attributeValue_l,
    attributeValue_ns,
    attributeValue_m,
    attributeValue_null,
    attributeValue_n,
    attributeValue_bs,
    attributeValue_b,
    attributeValue_ss,
    attributeValue_s,
    attributeValue_bool,

    -- * AttributeValueUpdate
    AttributeValueUpdate (..),
    newAttributeValueUpdate,
    attributeValueUpdate_value,
    attributeValueUpdate_action,

    -- * AutoScalingPolicyDescription
    AutoScalingPolicyDescription (..),
    newAutoScalingPolicyDescription,
    autoScalingPolicyDescription_policyName,
    autoScalingPolicyDescription_targetTrackingScalingPolicyConfiguration,

    -- * AutoScalingPolicyUpdate
    AutoScalingPolicyUpdate (..),
    newAutoScalingPolicyUpdate,
    autoScalingPolicyUpdate_policyName,
    autoScalingPolicyUpdate_targetTrackingScalingPolicyConfiguration,

    -- * AutoScalingSettingsDescription
    AutoScalingSettingsDescription (..),
    newAutoScalingSettingsDescription,
    autoScalingSettingsDescription_autoScalingDisabled,
    autoScalingSettingsDescription_minimumUnits,
    autoScalingSettingsDescription_maximumUnits,
    autoScalingSettingsDescription_scalingPolicies,
    autoScalingSettingsDescription_autoScalingRoleArn,

    -- * AutoScalingSettingsUpdate
    AutoScalingSettingsUpdate (..),
    newAutoScalingSettingsUpdate,
    autoScalingSettingsUpdate_autoScalingDisabled,
    autoScalingSettingsUpdate_minimumUnits,
    autoScalingSettingsUpdate_scalingPolicyUpdate,
    autoScalingSettingsUpdate_maximumUnits,
    autoScalingSettingsUpdate_autoScalingRoleArn,

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription (..),
    newAutoScalingTargetTrackingScalingPolicyConfigurationDescription,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleInCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_disableScaleIn,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleOutCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_targetValue,

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (..),
    newAutoScalingTargetTrackingScalingPolicyConfigurationUpdate,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleInCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_disableScaleIn,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleOutCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_targetValue,

    -- * BackupDescription
    BackupDescription (..),
    newBackupDescription,
    backupDescription_backupDetails,
    backupDescription_sourceTableDetails,
    backupDescription_sourceTableFeatureDetails,

    -- * BackupDetails
    BackupDetails (..),
    newBackupDetails,
    backupDetails_backupExpiryDateTime,
    backupDetails_backupSizeBytes,
    backupDetails_backupArn,
    backupDetails_backupName,
    backupDetails_backupStatus,
    backupDetails_backupType,
    backupDetails_backupCreationDateTime,

    -- * BackupSummary
    BackupSummary (..),
    newBackupSummary,
    backupSummary_backupExpiryDateTime,
    backupSummary_tableArn,
    backupSummary_backupName,
    backupSummary_backupStatus,
    backupSummary_backupSizeBytes,
    backupSummary_backupArn,
    backupSummary_tableId,
    backupSummary_backupCreationDateTime,
    backupSummary_backupType,
    backupSummary_tableName,

    -- * BatchStatementError
    BatchStatementError (..),
    newBatchStatementError,
    batchStatementError_code,
    batchStatementError_message,

    -- * BatchStatementRequest
    BatchStatementRequest (..),
    newBatchStatementRequest,
    batchStatementRequest_consistentRead,
    batchStatementRequest_parameters,
    batchStatementRequest_statement,

    -- * BatchStatementResponse
    BatchStatementResponse (..),
    newBatchStatementResponse,
    batchStatementResponse_error,
    batchStatementResponse_item,
    batchStatementResponse_tableName,

    -- * BillingModeSummary
    BillingModeSummary (..),
    newBillingModeSummary,
    billingModeSummary_lastUpdateToPayPerRequestDateTime,
    billingModeSummary_billingMode,

    -- * Capacity
    Capacity (..),
    newCapacity,
    capacity_readCapacityUnits,
    capacity_capacityUnits,
    capacity_writeCapacityUnits,

    -- * Condition
    Condition (..),
    newCondition,
    condition_attributeValueList,
    condition_comparisonOperator,

    -- * ConditionCheck
    ConditionCheck (..),
    newConditionCheck,
    conditionCheck_expressionAttributeNames,
    conditionCheck_expressionAttributeValues,
    conditionCheck_returnValuesOnConditionCheckFailure,
    conditionCheck_key,
    conditionCheck_tableName,
    conditionCheck_conditionExpression,

    -- * ConsumedCapacity
    ConsumedCapacity (..),
    newConsumedCapacity,
    consumedCapacity_readCapacityUnits,
    consumedCapacity_globalSecondaryIndexes,
    consumedCapacity_capacityUnits,
    consumedCapacity_writeCapacityUnits,
    consumedCapacity_localSecondaryIndexes,
    consumedCapacity_table,
    consumedCapacity_tableName,

    -- * ContinuousBackupsDescription
    ContinuousBackupsDescription (..),
    newContinuousBackupsDescription,
    continuousBackupsDescription_pointInTimeRecoveryDescription,
    continuousBackupsDescription_continuousBackupsStatus,

    -- * ContributorInsightsSummary
    ContributorInsightsSummary (..),
    newContributorInsightsSummary,
    contributorInsightsSummary_contributorInsightsStatus,
    contributorInsightsSummary_tableName,
    contributorInsightsSummary_indexName,

    -- * CreateGlobalSecondaryIndexAction
    CreateGlobalSecondaryIndexAction (..),
    newCreateGlobalSecondaryIndexAction,
    createGlobalSecondaryIndexAction_provisionedThroughput,
    createGlobalSecondaryIndexAction_indexName,
    createGlobalSecondaryIndexAction_keySchema,
    createGlobalSecondaryIndexAction_projection,

    -- * CreateReplicaAction
    CreateReplicaAction (..),
    newCreateReplicaAction,
    createReplicaAction_regionName,

    -- * CreateReplicationGroupMemberAction
    CreateReplicationGroupMemberAction (..),
    newCreateReplicationGroupMemberAction,
    createReplicationGroupMemberAction_kmsMasterKeyId,
    createReplicationGroupMemberAction_provisionedThroughputOverride,
    createReplicationGroupMemberAction_globalSecondaryIndexes,
    createReplicationGroupMemberAction_regionName,

    -- * Delete
    Delete (..),
    newDelete,
    delete_expressionAttributeNames,
    delete_expressionAttributeValues,
    delete_returnValuesOnConditionCheckFailure,
    delete_conditionExpression,
    delete_key,
    delete_tableName,

    -- * DeleteGlobalSecondaryIndexAction
    DeleteGlobalSecondaryIndexAction (..),
    newDeleteGlobalSecondaryIndexAction,
    deleteGlobalSecondaryIndexAction_indexName,

    -- * DeleteReplicaAction
    DeleteReplicaAction (..),
    newDeleteReplicaAction,
    deleteReplicaAction_regionName,

    -- * DeleteReplicationGroupMemberAction
    DeleteReplicationGroupMemberAction (..),
    newDeleteReplicationGroupMemberAction,
    deleteReplicationGroupMemberAction_regionName,

    -- * DeleteRequest
    DeleteRequest (..),
    newDeleteRequest,
    deleteRequest_key,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_cachePeriodInMinutes,

    -- * ExpectedAttributeValue
    ExpectedAttributeValue (..),
    newExpectedAttributeValue,
    expectedAttributeValue_attributeValueList,
    expectedAttributeValue_exists,
    expectedAttributeValue_value,
    expectedAttributeValue_comparisonOperator,

    -- * ExportDescription
    ExportDescription (..),
    newExportDescription,
    exportDescription_s3BucketOwner,
    exportDescription_exportFormat,
    exportDescription_s3SseKmsKeyId,
    exportDescription_clientToken,
    exportDescription_startTime,
    exportDescription_failureCode,
    exportDescription_exportStatus,
    exportDescription_failureMessage,
    exportDescription_tableArn,
    exportDescription_billedSizeBytes,
    exportDescription_exportArn,
    exportDescription_exportTime,
    exportDescription_s3SseAlgorithm,
    exportDescription_endTime,
    exportDescription_s3Prefix,
    exportDescription_exportManifest,
    exportDescription_tableId,
    exportDescription_itemCount,
    exportDescription_s3Bucket,

    -- * ExportSummary
    ExportSummary (..),
    newExportSummary,
    exportSummary_exportStatus,
    exportSummary_exportArn,

    -- * FailureException
    FailureException (..),
    newFailureException,
    failureException_exceptionName,
    failureException_exceptionDescription,

    -- * Get
    Get (..),
    newGet,
    get_projectionExpression,
    get_expressionAttributeNames,
    get_key,
    get_tableName,

    -- * GlobalSecondaryIndex
    GlobalSecondaryIndex (..),
    newGlobalSecondaryIndex,
    globalSecondaryIndex_provisionedThroughput,
    globalSecondaryIndex_indexName,
    globalSecondaryIndex_keySchema,
    globalSecondaryIndex_projection,

    -- * GlobalSecondaryIndexAutoScalingUpdate
    GlobalSecondaryIndexAutoScalingUpdate (..),
    newGlobalSecondaryIndexAutoScalingUpdate,
    globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate,
    globalSecondaryIndexAutoScalingUpdate_indexName,

    -- * GlobalSecondaryIndexDescription
    GlobalSecondaryIndexDescription (..),
    newGlobalSecondaryIndexDescription,
    globalSecondaryIndexDescription_backfilling,
    globalSecondaryIndexDescription_indexSizeBytes,
    globalSecondaryIndexDescription_indexStatus,
    globalSecondaryIndexDescription_provisionedThroughput,
    globalSecondaryIndexDescription_indexArn,
    globalSecondaryIndexDescription_keySchema,
    globalSecondaryIndexDescription_projection,
    globalSecondaryIndexDescription_itemCount,
    globalSecondaryIndexDescription_indexName,

    -- * GlobalSecondaryIndexInfo
    GlobalSecondaryIndexInfo (..),
    newGlobalSecondaryIndexInfo,
    globalSecondaryIndexInfo_provisionedThroughput,
    globalSecondaryIndexInfo_keySchema,
    globalSecondaryIndexInfo_projection,
    globalSecondaryIndexInfo_indexName,

    -- * GlobalSecondaryIndexUpdate
    GlobalSecondaryIndexUpdate (..),
    newGlobalSecondaryIndexUpdate,
    globalSecondaryIndexUpdate_create,
    globalSecondaryIndexUpdate_delete,
    globalSecondaryIndexUpdate_update,

    -- * GlobalTable
    GlobalTable (..),
    newGlobalTable,
    globalTable_globalTableName,
    globalTable_replicationGroup,

    -- * GlobalTableDescription
    GlobalTableDescription (..),
    newGlobalTableDescription,
    globalTableDescription_globalTableStatus,
    globalTableDescription_globalTableName,
    globalTableDescription_globalTableArn,
    globalTableDescription_creationDateTime,
    globalTableDescription_replicationGroup,

    -- * GlobalTableGlobalSecondaryIndexSettingsUpdate
    GlobalTableGlobalSecondaryIndexSettingsUpdate (..),
    newGlobalTableGlobalSecondaryIndexSettingsUpdate,
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityUnits,
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityAutoScalingSettingsUpdate,
    globalTableGlobalSecondaryIndexSettingsUpdate_indexName,

    -- * ItemCollectionMetrics
    ItemCollectionMetrics (..),
    newItemCollectionMetrics,
    itemCollectionMetrics_itemCollectionKey,
    itemCollectionMetrics_sizeEstimateRangeGB,

    -- * ItemResponse
    ItemResponse (..),
    newItemResponse,
    itemResponse_item,

    -- * KeySchemaElement
    KeySchemaElement (..),
    newKeySchemaElement,
    keySchemaElement_attributeName,
    keySchemaElement_keyType,

    -- * KeysAndAttributes
    KeysAndAttributes (..),
    newKeysAndAttributes,
    keysAndAttributes_projectionExpression,
    keysAndAttributes_attributesToGet,
    keysAndAttributes_expressionAttributeNames,
    keysAndAttributes_consistentRead,
    keysAndAttributes_keys,

    -- * KinesisDataStreamDestination
    KinesisDataStreamDestination (..),
    newKinesisDataStreamDestination,
    kinesisDataStreamDestination_destinationStatus,
    kinesisDataStreamDestination_streamArn,
    kinesisDataStreamDestination_destinationStatusDescription,

    -- * KinesisStreamingDestinationInput
    KinesisStreamingDestinationInput (..),
    newKinesisStreamingDestinationInput,
    kinesisStreamingDestinationInput_tableName,
    kinesisStreamingDestinationInput_streamArn,

    -- * KinesisStreamingDestinationOutput
    KinesisStreamingDestinationOutput (..),
    newKinesisStreamingDestinationOutput,
    kinesisStreamingDestinationOutput_destinationStatus,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_tableName,

    -- * LocalSecondaryIndex
    LocalSecondaryIndex (..),
    newLocalSecondaryIndex,
    localSecondaryIndex_indexName,
    localSecondaryIndex_keySchema,
    localSecondaryIndex_projection,

    -- * LocalSecondaryIndexDescription
    LocalSecondaryIndexDescription (..),
    newLocalSecondaryIndexDescription,
    localSecondaryIndexDescription_indexSizeBytes,
    localSecondaryIndexDescription_indexArn,
    localSecondaryIndexDescription_keySchema,
    localSecondaryIndexDescription_projection,
    localSecondaryIndexDescription_itemCount,
    localSecondaryIndexDescription_indexName,

    -- * LocalSecondaryIndexInfo
    LocalSecondaryIndexInfo (..),
    newLocalSecondaryIndexInfo,
    localSecondaryIndexInfo_keySchema,
    localSecondaryIndexInfo_projection,
    localSecondaryIndexInfo_indexName,

    -- * ParameterizedStatement
    ParameterizedStatement (..),
    newParameterizedStatement,
    parameterizedStatement_parameters,
    parameterizedStatement_statement,

    -- * PointInTimeRecoveryDescription
    PointInTimeRecoveryDescription (..),
    newPointInTimeRecoveryDescription,
    pointInTimeRecoveryDescription_pointInTimeRecoveryStatus,
    pointInTimeRecoveryDescription_earliestRestorableDateTime,
    pointInTimeRecoveryDescription_latestRestorableDateTime,

    -- * PointInTimeRecoverySpecification
    PointInTimeRecoverySpecification (..),
    newPointInTimeRecoverySpecification,
    pointInTimeRecoverySpecification_pointInTimeRecoveryEnabled,

    -- * Projection
    Projection (..),
    newProjection,
    projection_projectionType,
    projection_nonKeyAttributes,

    -- * ProvisionedThroughput
    ProvisionedThroughput (..),
    newProvisionedThroughput,
    provisionedThroughput_readCapacityUnits,
    provisionedThroughput_writeCapacityUnits,

    -- * ProvisionedThroughputDescription
    ProvisionedThroughputDescription (..),
    newProvisionedThroughputDescription,
    provisionedThroughputDescription_readCapacityUnits,
    provisionedThroughputDescription_lastDecreaseDateTime,
    provisionedThroughputDescription_writeCapacityUnits,
    provisionedThroughputDescription_numberOfDecreasesToday,
    provisionedThroughputDescription_lastIncreaseDateTime,

    -- * ProvisionedThroughputOverride
    ProvisionedThroughputOverride (..),
    newProvisionedThroughputOverride,
    provisionedThroughputOverride_readCapacityUnits,

    -- * Put
    Put (..),
    newPut,
    put_expressionAttributeNames,
    put_expressionAttributeValues,
    put_returnValuesOnConditionCheckFailure,
    put_conditionExpression,
    put_item,
    put_tableName,

    -- * PutRequest
    PutRequest (..),
    newPutRequest,
    putRequest_item,

    -- * Replica
    Replica (..),
    newReplica,
    replica_regionName,

    -- * ReplicaAutoScalingDescription
    ReplicaAutoScalingDescription (..),
    newReplicaAutoScalingDescription,
    replicaAutoScalingDescription_replicaStatus,
    replicaAutoScalingDescription_regionName,
    replicaAutoScalingDescription_globalSecondaryIndexes,
    replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings,

    -- * ReplicaAutoScalingUpdate
    ReplicaAutoScalingUpdate (..),
    newReplicaAutoScalingUpdate,
    replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate,
    replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates,
    replicaAutoScalingUpdate_regionName,

    -- * ReplicaDescription
    ReplicaDescription (..),
    newReplicaDescription,
    replicaDescription_replicaStatus,
    replicaDescription_regionName,
    replicaDescription_replicaStatusPercentProgress,
    replicaDescription_replicaStatusDescription,
    replicaDescription_replicaInaccessibleDateTime,
    replicaDescription_kmsMasterKeyId,
    replicaDescription_provisionedThroughputOverride,
    replicaDescription_globalSecondaryIndexes,

    -- * ReplicaGlobalSecondaryIndex
    ReplicaGlobalSecondaryIndex (..),
    newReplicaGlobalSecondaryIndex,
    replicaGlobalSecondaryIndex_provisionedThroughputOverride,
    replicaGlobalSecondaryIndex_indexName,

    -- * ReplicaGlobalSecondaryIndexAutoScalingDescription
    ReplicaGlobalSecondaryIndexAutoScalingDescription (..),
    newReplicaGlobalSecondaryIndexAutoScalingDescription,
    replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexAutoScalingDescription_indexName,

    -- * ReplicaGlobalSecondaryIndexAutoScalingUpdate
    ReplicaGlobalSecondaryIndexAutoScalingUpdate (..),
    newReplicaGlobalSecondaryIndexAutoScalingUpdate,
    replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate,
    replicaGlobalSecondaryIndexAutoScalingUpdate_indexName,

    -- * ReplicaGlobalSecondaryIndexDescription
    ReplicaGlobalSecondaryIndexDescription (..),
    newReplicaGlobalSecondaryIndexDescription,
    replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride,
    replicaGlobalSecondaryIndexDescription_indexName,

    -- * ReplicaGlobalSecondaryIndexSettingsDescription
    ReplicaGlobalSecondaryIndexSettingsDescription (..),
    newReplicaGlobalSecondaryIndexSettingsDescription,
    replicaGlobalSecondaryIndexSettingsDescription_indexStatus,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_indexName,

    -- * ReplicaGlobalSecondaryIndexSettingsUpdate
    ReplicaGlobalSecondaryIndexSettingsUpdate (..),
    newReplicaGlobalSecondaryIndexSettingsUpdate,
    replicaGlobalSecondaryIndexSettingsUpdate_provisionedReadCapacityAutoScalingSettingsUpdate,
    replicaGlobalSecondaryIndexSettingsUpdate_provisionedReadCapacityUnits,
    replicaGlobalSecondaryIndexSettingsUpdate_indexName,

    -- * ReplicaSettingsDescription
    ReplicaSettingsDescription (..),
    newReplicaSettingsDescription,
    replicaSettingsDescription_replicaStatus,
    replicaSettingsDescription_replicaProvisionedReadCapacityUnits,
    replicaSettingsDescription_replicaProvisionedWriteCapacityUnits,
    replicaSettingsDescription_replicaBillingModeSummary,
    replicaSettingsDescription_replicaGlobalSecondaryIndexSettings,
    replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings,
    replicaSettingsDescription_regionName,

    -- * ReplicaSettingsUpdate
    ReplicaSettingsUpdate (..),
    newReplicaSettingsUpdate,
    replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    replicaSettingsUpdate_replicaProvisionedReadCapacityUnits,
    replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate,
    replicaSettingsUpdate_regionName,

    -- * ReplicaUpdate
    ReplicaUpdate (..),
    newReplicaUpdate,
    replicaUpdate_create,
    replicaUpdate_delete,

    -- * ReplicationGroupUpdate
    ReplicationGroupUpdate (..),
    newReplicationGroupUpdate,
    replicationGroupUpdate_create,
    replicationGroupUpdate_delete,
    replicationGroupUpdate_update,

    -- * RestoreSummary
    RestoreSummary (..),
    newRestoreSummary,
    restoreSummary_sourceTableArn,
    restoreSummary_sourceBackupArn,
    restoreSummary_restoreDateTime,
    restoreSummary_restoreInProgress,

    -- * SSEDescription
    SSEDescription (..),
    newSSEDescription,
    sSEDescription_status,
    sSEDescription_inaccessibleEncryptionDateTime,
    sSEDescription_sSEType,
    sSEDescription_kmsMasterKeyArn,

    -- * SSESpecification
    SSESpecification (..),
    newSSESpecification,
    sSESpecification_enabled,
    sSESpecification_kmsMasterKeyId,
    sSESpecification_sSEType,

    -- * SourceTableDetails
    SourceTableDetails (..),
    newSourceTableDetails,
    sourceTableDetails_tableSizeBytes,
    sourceTableDetails_tableArn,
    sourceTableDetails_billingMode,
    sourceTableDetails_itemCount,
    sourceTableDetails_tableName,
    sourceTableDetails_tableId,
    sourceTableDetails_keySchema,
    sourceTableDetails_tableCreationDateTime,
    sourceTableDetails_provisionedThroughput,

    -- * SourceTableFeatureDetails
    SourceTableFeatureDetails (..),
    newSourceTableFeatureDetails,
    sourceTableFeatureDetails_streamDescription,
    sourceTableFeatureDetails_globalSecondaryIndexes,
    sourceTableFeatureDetails_localSecondaryIndexes,
    sourceTableFeatureDetails_sSEDescription,
    sourceTableFeatureDetails_timeToLiveDescription,

    -- * StreamSpecification
    StreamSpecification (..),
    newStreamSpecification,
    streamSpecification_streamViewType,
    streamSpecification_streamEnabled,

    -- * TableAutoScalingDescription
    TableAutoScalingDescription (..),
    newTableAutoScalingDescription,
    tableAutoScalingDescription_tableStatus,
    tableAutoScalingDescription_replicas,
    tableAutoScalingDescription_tableName,

    -- * TableDescription
    TableDescription (..),
    newTableDescription,
    tableDescription_restoreSummary,
    tableDescription_globalTableVersion,
    tableDescription_tableSizeBytes,
    tableDescription_attributeDefinitions,
    tableDescription_latestStreamArn,
    tableDescription_provisionedThroughput,
    tableDescription_tableStatus,
    tableDescription_tableArn,
    tableDescription_keySchema,
    tableDescription_globalSecondaryIndexes,
    tableDescription_latestStreamLabel,
    tableDescription_billingModeSummary,
    tableDescription_localSecondaryIndexes,
    tableDescription_creationDateTime,
    tableDescription_sSEDescription,
    tableDescription_tableId,
    tableDescription_replicas,
    tableDescription_itemCount,
    tableDescription_archivalSummary,
    tableDescription_tableName,
    tableDescription_streamSpecification,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TimeToLiveDescription
    TimeToLiveDescription (..),
    newTimeToLiveDescription,
    timeToLiveDescription_timeToLiveStatus,
    timeToLiveDescription_attributeName,

    -- * TimeToLiveSpecification
    TimeToLiveSpecification (..),
    newTimeToLiveSpecification,
    timeToLiveSpecification_enabled,
    timeToLiveSpecification_attributeName,

    -- * TransactGetItem
    TransactGetItem (..),
    newTransactGetItem,
    transactGetItem_get,

    -- * TransactWriteItem
    TransactWriteItem (..),
    newTransactWriteItem,
    transactWriteItem_conditionCheck,
    transactWriteItem_put,
    transactWriteItem_delete,
    transactWriteItem_update,

    -- * Update
    Update (..),
    newUpdate,
    update_expressionAttributeNames,
    update_expressionAttributeValues,
    update_returnValuesOnConditionCheckFailure,
    update_conditionExpression,
    update_key,
    update_updateExpression,
    update_tableName,

    -- * UpdateGlobalSecondaryIndexAction
    UpdateGlobalSecondaryIndexAction (..),
    newUpdateGlobalSecondaryIndexAction,
    updateGlobalSecondaryIndexAction_indexName,
    updateGlobalSecondaryIndexAction_provisionedThroughput,

    -- * UpdateReplicationGroupMemberAction
    UpdateReplicationGroupMemberAction (..),
    newUpdateReplicationGroupMemberAction,
    updateReplicationGroupMemberAction_kmsMasterKeyId,
    updateReplicationGroupMemberAction_provisionedThroughputOverride,
    updateReplicationGroupMemberAction_globalSecondaryIndexes,
    updateReplicationGroupMemberAction_regionName,

    -- * WriteRequest
    WriteRequest (..),
    newWriteRequest,
    writeRequest_deleteRequest,
    writeRequest_putRequest,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.ArchivalSummary
import Network.AWS.DynamoDB.Types.AttributeAction
import Network.AWS.DynamoDB.Types.AttributeDefinition
import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.AttributeValueUpdate
import Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
import Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate
import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
import Network.AWS.DynamoDB.Types.BackupDescription
import Network.AWS.DynamoDB.Types.BackupDetails
import Network.AWS.DynamoDB.Types.BackupStatus
import Network.AWS.DynamoDB.Types.BackupSummary
import Network.AWS.DynamoDB.Types.BackupType
import Network.AWS.DynamoDB.Types.BackupTypeFilter
import Network.AWS.DynamoDB.Types.BatchStatementError
import Network.AWS.DynamoDB.Types.BatchStatementErrorCodeEnum
import Network.AWS.DynamoDB.Types.BatchStatementRequest
import Network.AWS.DynamoDB.Types.BatchStatementResponse
import Network.AWS.DynamoDB.Types.BillingMode
import Network.AWS.DynamoDB.Types.BillingModeSummary
import Network.AWS.DynamoDB.Types.Capacity
import Network.AWS.DynamoDB.Types.ComparisonOperator
import Network.AWS.DynamoDB.Types.Condition
import Network.AWS.DynamoDB.Types.ConditionCheck
import Network.AWS.DynamoDB.Types.ConditionalOperator
import Network.AWS.DynamoDB.Types.ConsumedCapacity
import Network.AWS.DynamoDB.Types.ContinuousBackupsDescription
import Network.AWS.DynamoDB.Types.ContinuousBackupsStatus
import Network.AWS.DynamoDB.Types.ContributorInsightsAction
import Network.AWS.DynamoDB.Types.ContributorInsightsStatus
import Network.AWS.DynamoDB.Types.ContributorInsightsSummary
import Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.CreateReplicaAction
import Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.Delete
import Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.DeleteReplicaAction
import Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.DeleteRequest
import Network.AWS.DynamoDB.Types.DestinationStatus
import Network.AWS.DynamoDB.Types.Endpoint
import Network.AWS.DynamoDB.Types.ExpectedAttributeValue
import Network.AWS.DynamoDB.Types.ExportDescription
import Network.AWS.DynamoDB.Types.ExportFormat
import Network.AWS.DynamoDB.Types.ExportStatus
import Network.AWS.DynamoDB.Types.ExportSummary
import Network.AWS.DynamoDB.Types.FailureException
import Network.AWS.DynamoDB.Types.Get
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndex
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexUpdate
import Network.AWS.DynamoDB.Types.GlobalTable
import Network.AWS.DynamoDB.Types.GlobalTableDescription
import Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate
import Network.AWS.DynamoDB.Types.GlobalTableStatus
import Network.AWS.DynamoDB.Types.IndexStatus
import Network.AWS.DynamoDB.Types.ItemCollectionMetrics
import Network.AWS.DynamoDB.Types.ItemResponse
import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.KeyType
import Network.AWS.DynamoDB.Types.KeysAndAttributes
import Network.AWS.DynamoDB.Types.KinesisDataStreamDestination
import Network.AWS.DynamoDB.Types.KinesisStreamingDestinationInput
import Network.AWS.DynamoDB.Types.KinesisStreamingDestinationOutput
import Network.AWS.DynamoDB.Types.LocalSecondaryIndex
import Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.LocalSecondaryIndexInfo
import Network.AWS.DynamoDB.Types.ParameterizedStatement
import Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription
import Network.AWS.DynamoDB.Types.PointInTimeRecoverySpecification
import Network.AWS.DynamoDB.Types.PointInTimeRecoveryStatus
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProjectionType
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription
import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.Put
import Network.AWS.DynamoDB.Types.PutRequest
import Network.AWS.DynamoDB.Types.Replica
import Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription
import Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate
import Network.AWS.DynamoDB.Types.ReplicaDescription
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaSettingsDescription
import Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaStatus
import Network.AWS.DynamoDB.Types.ReplicaUpdate
import Network.AWS.DynamoDB.Types.ReplicationGroupUpdate
import Network.AWS.DynamoDB.Types.RestoreSummary
import Network.AWS.DynamoDB.Types.ReturnConsumedCapacity
import Network.AWS.DynamoDB.Types.ReturnItemCollectionMetrics
import Network.AWS.DynamoDB.Types.ReturnValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Network.AWS.DynamoDB.Types.S3SseAlgorithm
import Network.AWS.DynamoDB.Types.SSEDescription
import Network.AWS.DynamoDB.Types.SSESpecification
import Network.AWS.DynamoDB.Types.SSEStatus
import Network.AWS.DynamoDB.Types.SSEType
import Network.AWS.DynamoDB.Types.ScalarAttributeType
import Network.AWS.DynamoDB.Types.Select
import Network.AWS.DynamoDB.Types.SourceTableDetails
import Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
import Network.AWS.DynamoDB.Types.StreamSpecification
import Network.AWS.DynamoDB.Types.StreamViewType
import Network.AWS.DynamoDB.Types.TableAutoScalingDescription
import Network.AWS.DynamoDB.Types.TableDescription
import Network.AWS.DynamoDB.Types.TableStatus
import Network.AWS.DynamoDB.Types.Tag
import Network.AWS.DynamoDB.Types.TimeToLiveDescription
import Network.AWS.DynamoDB.Types.TimeToLiveSpecification
import Network.AWS.DynamoDB.Types.TimeToLiveStatus
import Network.AWS.DynamoDB.Types.TransactGetItem
import Network.AWS.DynamoDB.Types.TransactWriteItem
import Network.AWS.DynamoDB.Types.Update
import Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.WriteRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-08-10@ of the Amazon DynamoDB SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DynamoDB",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "dynamodb",
      Core._serviceSigningName = "dynamodb",
      Core._serviceVersion = "2012-08-10",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "DynamoDB",
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
          ( Core.hasCode "TransactionInProgressException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "still_processing"
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

-- | The specified @ExportTime@ is outside of the point in time recovery
-- window.
_InvalidExportTimeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidExportTimeException =
  Core._MatchServiceError
    defaultService
    "InvalidExportTimeException"

-- | Backup not found for the given BackupARN.
_BackupNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BackupNotFoundException =
  Core._MatchServiceError
    defaultService
    "BackupNotFoundException"

-- | A target table with the specified name is either being created or
-- deleted.
_TableInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TableInUseException =
  Core._MatchServiceError
    defaultService
    "TableInUseException"

-- | There was a conflict when writing to the specified S3 bucket.
_ExportConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExportConflictException =
  Core._MatchServiceError
    defaultService
    "ExportConflictException"

-- | Backups have not yet been enabled for this table.
_ContinuousBackupsUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ContinuousBackupsUnavailableException =
  Core._MatchServiceError
    defaultService
    "ContinuousBackupsUnavailableException"

-- | Your request rate is too high. The AWS SDKs for DynamoDB automatically
-- retry requests that receive this exception. Your request is eventually
-- successful, unless your retry queue is too large to finish. Reduce the
-- frequency of requests and use exponential backoff. For more information,
-- go to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Programming.Errors.html#Programming.Errors.RetryAndBackoff Error Retries and Exponential Backoff>
-- in the /Amazon DynamoDB Developer Guide/.
_ProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ProvisionedThroughputExceededException"

-- | The specified global table does not exist.
_GlobalTableNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalTableNotFoundException =
  Core._MatchServiceError
    defaultService
    "GlobalTableNotFoundException"

-- | The transaction with the given request token is already in progress.
_TransactionInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TransactionInProgressException =
  Core._MatchServiceError
    defaultService
    "TransactionInProgressException"

-- | The entire transaction request was canceled.
--
-- DynamoDB cancels a @TransactWriteItems@ request under the following
-- circumstances:
--
-- -   A condition in one of the condition expressions is not met.
--
-- -   A table in the @TransactWriteItems@ request is in a different
--     account or region.
--
-- -   More than one action in the @TransactWriteItems@ operation targets
--     the same item.
--
-- -   There is insufficient provisioned capacity for the transaction to be
--     completed.
--
-- -   An item size becomes too large (larger than 400 KB), or a local
--     secondary index (LSI) becomes too large, or a similar validation
--     error occurs because of changes made by the transaction.
--
-- -   There is a user error, such as an invalid data format.
--
-- DynamoDB cancels a @TransactGetItems@ request under the following
-- circumstances:
--
-- -   There is an ongoing @TransactGetItems@ operation that conflicts with
--     a concurrent @PutItem@, @UpdateItem@, @DeleteItem@ or
--     @TransactWriteItems@ request. In this case the @TransactGetItems@
--     operation fails with a @TransactionCanceledException@.
--
-- -   A table in the @TransactGetItems@ request is in a different account
--     or region.
--
-- -   There is insufficient provisioned capacity for the transaction to be
--     completed.
--
-- -   There is a user error, such as an invalid data format.
--
-- If using Java, DynamoDB lists the cancellation reasons on the
-- @CancellationReasons@ property. This property is not set for other
-- languages. Transaction cancellation reasons are ordered in the order of
-- requested items, if an item has no error it will have @NONE@ code and
-- @Null@ message.
--
-- Cancellation reason codes and possible error messages:
--
-- -   No Errors:
--
--     -   Code: @NONE@
--
--     -   Message: @null@
--
-- -   Conditional Check Failed:
--
--     -   Code: @ConditionalCheckFailed@
--
--     -   Message: The conditional request failed.
--
-- -   Item Collection Size Limit Exceeded:
--
--     -   Code: @ItemCollectionSizeLimitExceeded@
--
--     -   Message: Collection size exceeded.
--
-- -   Transaction Conflict:
--
--     -   Code: @TransactionConflict@
--
--     -   Message: Transaction is ongoing for the item.
--
-- -   Provisioned Throughput Exceeded:
--
--     -   Code: @ProvisionedThroughputExceeded@
--
--     -   Messages:
--
--         -   The level of configured provisioned throughput for the table
--             was exceeded. Consider increasing your provisioning level
--             with the UpdateTable API.
--
--             This Message is received when provisioned throughput is
--             exceeded is on a provisioned DynamoDB table.
--
--         -   The level of configured provisioned throughput for one or
--             more global secondary indexes of the table was exceeded.
--             Consider increasing your provisioning level for the
--             under-provisioned global secondary indexes with the
--             UpdateTable API.
--
--             This message is returned when provisioned throughput is
--             exceeded is on a provisioned GSI.
--
-- -   Throttling Error:
--
--     -   Code: @ThrottlingError@
--
--     -   Messages:
--
--         -   Throughput exceeds the current capacity of your table or
--             index. DynamoDB is automatically scaling your table or index
--             so please try again shortly. If exceptions persist, check if
--             you have a hot key:
--             https:\/\/docs.aws.amazon.com\/amazondynamodb\/latest\/developerguide\/bp-partition-key-design.html.
--
--             This message is returned when writes get throttled on an
--             On-Demand table as DynamoDB is automatically scaling the
--             table.
--
--         -   Throughput exceeds the current capacity for one or more
--             global secondary indexes. DynamoDB is automatically scaling
--             your index so please try again shortly.
--
--             This message is returned when when writes get throttled on
--             an On-Demand GSI as DynamoDB is automatically scaling the
--             GSI.
--
-- -   Validation Error:
--
--     -   Code: @ValidationError@
--
--     -   Messages:
--
--         -   One or more parameter values were invalid.
--
--         -   The update expression attempted to update the secondary
--             index key beyond allowed size limits.
--
--         -   The update expression attempted to update the secondary
--             index key to unsupported type.
--
--         -   An operand in the update expression has an incorrect data
--             type.
--
--         -   Item size to update has exceeded the maximum allowed size.
--
--         -   Number overflow. Attempting to store a number with magnitude
--             larger than supported range.
--
--         -   Type mismatch for attribute to update.
--
--         -   Nesting Levels have exceeded supported limits.
--
--         -   The document path provided in the update expression is
--             invalid for update.
--
--         -   The provided expression refers to an attribute that does not
--             exist in the item.
_TransactionCanceledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TransactionCanceledException =
  Core._MatchServiceError
    defaultService
    "TransactionCanceledException"

-- | A condition specified in the operation could not be evaluated.
_ConditionalCheckFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConditionalCheckFailedException =
  Core._MatchServiceError
    defaultService
    "ConditionalCheckFailedException"

-- | The specified global table already exists.
_GlobalTableAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalTableAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "GlobalTableAlreadyExistsException"

-- | The specified replica is no longer part of the global table.
_ReplicaNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicaNotFoundException =
  Core._MatchServiceError
    defaultService
    "ReplicaNotFoundException"

-- | A target table with the specified name already exists.
_TableAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TableAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "TableAlreadyExistsException"

-- | Throughput exceeds the current throughput quota for your account. Please
-- contact AWS Support at <https://aws.amazon.com/support AWS Support> to
-- request a quota increase.
_RequestLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestLimitExceeded =
  Core._MatchServiceError
    defaultService
    "RequestLimitExceeded"

-- | An item collection is too large. This exception is only returned for
-- tables that have one or more local secondary indexes.
_ItemCollectionSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ItemCollectionSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ItemCollectionSizeLimitExceededException"

-- | An error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | A source table with the name @TableName@ does not currently exist within
-- the subscriber\'s account.
_TableNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TableNotFoundException =
  Core._MatchServiceError
    defaultService
    "TableNotFoundException"

-- | The operation tried to access a nonexistent index.
_IndexNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IndexNotFoundException =
  Core._MatchServiceError
    defaultService
    "IndexNotFoundException"

-- | Operation was rejected because there is an ongoing transaction for the
-- item.
_TransactionConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TransactionConflictException =
  Core._MatchServiceError
    defaultService
    "TransactionConflictException"

-- | There is another ongoing conflicting backup control plane operation on
-- the table. The backup is either being created, deleted or restored to a
-- table.
_BackupInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BackupInUseException =
  Core._MatchServiceError
    defaultService
    "BackupInUseException"

-- | There was an attempt to insert an item with the same primary key as an
-- item that already exists in the DynamoDB table.
_DuplicateItemException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateItemException =
  Core._MatchServiceError
    defaultService
    "DuplicateItemException"

-- | The specified export was not found.
_ExportNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExportNotFoundException =
  Core._MatchServiceError
    defaultService
    "ExportNotFoundException"

-- | Point in time recovery has not yet been enabled for this source table.
_PointInTimeRecoveryUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PointInTimeRecoveryUnavailableException =
  Core._MatchServiceError
    defaultService
    "PointInTimeRecoveryUnavailableException"

-- | DynamoDB rejected the request because you retried a request with a
-- different payload but with an idempotent token that was already used.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | An invalid restore time was specified. RestoreDateTime must be between
-- EarliestRestorableDateTime and LatestRestorableDateTime.
_InvalidRestoreTimeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRestoreTimeException =
  Core._MatchServiceError
    defaultService
    "InvalidRestoreTimeException"

-- | The operation tried to access a nonexistent table or index. The resource
-- might not be specified correctly, or its status might not be @ACTIVE@.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The specified replica is already part of the global table.
_ReplicaAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicaAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ReplicaAlreadyExistsException"

-- | There is no limit to the number of daily on-demand backups that can be
-- taken.
--
-- Up to 50 simultaneous table operations are allowed per account. These
-- operations include @CreateTable@, @UpdateTable@,
-- @DeleteTable@,@UpdateTimeToLive@, @RestoreTableFromBackup@, and
-- @RestoreTableToPointInTime@.
--
-- The only exception is when you are creating a table with one or more
-- secondary indexes. You can have up to 25 such requests running at a
-- time; however, if the table or index specifications are complex,
-- DynamoDB might temporarily reduce the number of concurrent operations.
--
-- There is a soft account quota of 256 tables.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The operation conflicts with the resource\'s availability. For example,
-- you attempted to recreate an existing table, or tried to delete a table
-- currently in the @CREATING@ state.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
