{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDB.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BackupInUseException,
    _BackupNotFoundException,
    _ConditionalCheckFailedException,
    _ContinuousBackupsUnavailableException,
    _DuplicateItemException,
    _ExportConflictException,
    _ExportNotFoundException,
    _GlobalTableAlreadyExistsException,
    _GlobalTableNotFoundException,
    _IdempotentParameterMismatchException,
    _ImportConflictException,
    _ImportNotFoundException,
    _IndexNotFoundException,
    _InternalServerError,
    _InvalidExportTimeException,
    _InvalidRestoreTimeException,
    _ItemCollectionSizeLimitExceededException,
    _LimitExceededException,
    _PointInTimeRecoveryUnavailableException,
    _ProvisionedThroughputExceededException,
    _ReplicaAlreadyExistsException,
    _ReplicaNotFoundException,
    _RequestLimitExceeded,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _TableAlreadyExistsException,
    _TableInUseException,
    _TableNotFoundException,
    _TransactionCanceledException,
    _TransactionConflictException,
    _TransactionInProgressException,

    -- * Re-exported Types
    module Amazonka.DynamoDB.Types.AttributeValue,
    module Amazonka.DynamoDB.Types.WriteRequest,

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

    -- * ImportStatus
    ImportStatus (..),

    -- * IndexStatus
    IndexStatus (..),

    -- * InputCompressionType
    InputCompressionType (..),

    -- * InputFormat
    InputFormat (..),

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

    -- * TableClass
    TableClass (..),

    -- * TableStatus
    TableStatus (..),

    -- * TimeToLiveStatus
    TimeToLiveStatus (..),

    -- * ArchivalSummary
    ArchivalSummary (..),
    newArchivalSummary,
    archivalSummary_archivalBackupArn,
    archivalSummary_archivalDateTime,
    archivalSummary_archivalReason,

    -- * AttributeDefinition
    AttributeDefinition (..),
    newAttributeDefinition,
    attributeDefinition_attributeName,
    attributeDefinition_attributeType,

    -- * AttributeValueUpdate
    AttributeValueUpdate (..),
    newAttributeValueUpdate,
    attributeValueUpdate_action,
    attributeValueUpdate_value,

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
    autoScalingSettingsDescription_autoScalingRoleArn,
    autoScalingSettingsDescription_maximumUnits,
    autoScalingSettingsDescription_minimumUnits,
    autoScalingSettingsDescription_scalingPolicies,

    -- * AutoScalingSettingsUpdate
    AutoScalingSettingsUpdate (..),
    newAutoScalingSettingsUpdate,
    autoScalingSettingsUpdate_autoScalingDisabled,
    autoScalingSettingsUpdate_autoScalingRoleArn,
    autoScalingSettingsUpdate_maximumUnits,
    autoScalingSettingsUpdate_minimumUnits,
    autoScalingSettingsUpdate_scalingPolicyUpdate,

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription (..),
    newAutoScalingTargetTrackingScalingPolicyConfigurationDescription,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_disableScaleIn,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleInCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleOutCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_targetValue,

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (..),
    newAutoScalingTargetTrackingScalingPolicyConfigurationUpdate,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_disableScaleIn,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleInCooldown,
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
    backupSummary_backupArn,
    backupSummary_backupCreationDateTime,
    backupSummary_backupExpiryDateTime,
    backupSummary_backupName,
    backupSummary_backupSizeBytes,
    backupSummary_backupStatus,
    backupSummary_backupType,
    backupSummary_tableArn,
    backupSummary_tableId,
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
    billingModeSummary_billingMode,
    billingModeSummary_lastUpdateToPayPerRequestDateTime,

    -- * Capacity
    Capacity (..),
    newCapacity,
    capacity_capacityUnits,
    capacity_readCapacityUnits,
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
    consumedCapacity_capacityUnits,
    consumedCapacity_globalSecondaryIndexes,
    consumedCapacity_localSecondaryIndexes,
    consumedCapacity_readCapacityUnits,
    consumedCapacity_table,
    consumedCapacity_tableName,
    consumedCapacity_writeCapacityUnits,

    -- * ContinuousBackupsDescription
    ContinuousBackupsDescription (..),
    newContinuousBackupsDescription,
    continuousBackupsDescription_pointInTimeRecoveryDescription,
    continuousBackupsDescription_continuousBackupsStatus,

    -- * ContributorInsightsSummary
    ContributorInsightsSummary (..),
    newContributorInsightsSummary,
    contributorInsightsSummary_contributorInsightsStatus,
    contributorInsightsSummary_indexName,
    contributorInsightsSummary_tableName,

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
    createReplicationGroupMemberAction_globalSecondaryIndexes,
    createReplicationGroupMemberAction_kmsMasterKeyId,
    createReplicationGroupMemberAction_provisionedThroughputOverride,
    createReplicationGroupMemberAction_tableClassOverride,
    createReplicationGroupMemberAction_regionName,

    -- * CsvOptions
    CsvOptions (..),
    newCsvOptions,
    csvOptions_delimiter,
    csvOptions_headerList,

    -- * Delete
    Delete (..),
    newDelete,
    delete_conditionExpression,
    delete_expressionAttributeNames,
    delete_expressionAttributeValues,
    delete_returnValuesOnConditionCheckFailure,
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

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_cachePeriodInMinutes,

    -- * ExpectedAttributeValue
    ExpectedAttributeValue (..),
    newExpectedAttributeValue,
    expectedAttributeValue_attributeValueList,
    expectedAttributeValue_comparisonOperator,
    expectedAttributeValue_exists,
    expectedAttributeValue_value,

    -- * ExportDescription
    ExportDescription (..),
    newExportDescription,
    exportDescription_billedSizeBytes,
    exportDescription_clientToken,
    exportDescription_endTime,
    exportDescription_exportArn,
    exportDescription_exportFormat,
    exportDescription_exportManifest,
    exportDescription_exportStatus,
    exportDescription_exportTime,
    exportDescription_failureCode,
    exportDescription_failureMessage,
    exportDescription_itemCount,
    exportDescription_s3Bucket,
    exportDescription_s3BucketOwner,
    exportDescription_s3Prefix,
    exportDescription_s3SseAlgorithm,
    exportDescription_s3SseKmsKeyId,
    exportDescription_startTime,
    exportDescription_tableArn,
    exportDescription_tableId,

    -- * ExportSummary
    ExportSummary (..),
    newExportSummary,
    exportSummary_exportArn,
    exportSummary_exportStatus,

    -- * FailureException
    FailureException (..),
    newFailureException,
    failureException_exceptionDescription,
    failureException_exceptionName,

    -- * Get
    Get (..),
    newGet,
    get_expressionAttributeNames,
    get_projectionExpression,
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
    globalSecondaryIndexAutoScalingUpdate_indexName,
    globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate,

    -- * GlobalSecondaryIndexDescription
    GlobalSecondaryIndexDescription (..),
    newGlobalSecondaryIndexDescription,
    globalSecondaryIndexDescription_backfilling,
    globalSecondaryIndexDescription_indexArn,
    globalSecondaryIndexDescription_indexName,
    globalSecondaryIndexDescription_indexSizeBytes,
    globalSecondaryIndexDescription_indexStatus,
    globalSecondaryIndexDescription_itemCount,
    globalSecondaryIndexDescription_keySchema,
    globalSecondaryIndexDescription_projection,
    globalSecondaryIndexDescription_provisionedThroughput,

    -- * GlobalSecondaryIndexInfo
    GlobalSecondaryIndexInfo (..),
    newGlobalSecondaryIndexInfo,
    globalSecondaryIndexInfo_indexName,
    globalSecondaryIndexInfo_keySchema,
    globalSecondaryIndexInfo_projection,
    globalSecondaryIndexInfo_provisionedThroughput,

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
    globalTableDescription_creationDateTime,
    globalTableDescription_globalTableArn,
    globalTableDescription_globalTableName,
    globalTableDescription_globalTableStatus,
    globalTableDescription_replicationGroup,

    -- * GlobalTableGlobalSecondaryIndexSettingsUpdate
    GlobalTableGlobalSecondaryIndexSettingsUpdate (..),
    newGlobalTableGlobalSecondaryIndexSettingsUpdate,
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityAutoScalingSettingsUpdate,
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityUnits,
    globalTableGlobalSecondaryIndexSettingsUpdate_indexName,

    -- * ImportSummary
    ImportSummary (..),
    newImportSummary,
    importSummary_cloudWatchLogGroupArn,
    importSummary_endTime,
    importSummary_importArn,
    importSummary_importStatus,
    importSummary_inputFormat,
    importSummary_s3BucketSource,
    importSummary_startTime,
    importSummary_tableArn,

    -- * ImportTableDescription
    ImportTableDescription (..),
    newImportTableDescription,
    importTableDescription_clientToken,
    importTableDescription_cloudWatchLogGroupArn,
    importTableDescription_endTime,
    importTableDescription_errorCount,
    importTableDescription_failureCode,
    importTableDescription_failureMessage,
    importTableDescription_importArn,
    importTableDescription_importStatus,
    importTableDescription_importedItemCount,
    importTableDescription_inputCompressionType,
    importTableDescription_inputFormat,
    importTableDescription_inputFormatOptions,
    importTableDescription_processedItemCount,
    importTableDescription_processedSizeBytes,
    importTableDescription_s3BucketSource,
    importTableDescription_startTime,
    importTableDescription_tableArn,
    importTableDescription_tableCreationParameters,
    importTableDescription_tableId,

    -- * InputFormatOptions
    InputFormatOptions (..),
    newInputFormatOptions,
    inputFormatOptions_csv,

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
    keysAndAttributes_attributesToGet,
    keysAndAttributes_consistentRead,
    keysAndAttributes_expressionAttributeNames,
    keysAndAttributes_projectionExpression,
    keysAndAttributes_keys,

    -- * KinesisDataStreamDestination
    KinesisDataStreamDestination (..),
    newKinesisDataStreamDestination,
    kinesisDataStreamDestination_destinationStatus,
    kinesisDataStreamDestination_destinationStatusDescription,
    kinesisDataStreamDestination_streamArn,

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
    localSecondaryIndexDescription_indexArn,
    localSecondaryIndexDescription_indexName,
    localSecondaryIndexDescription_indexSizeBytes,
    localSecondaryIndexDescription_itemCount,
    localSecondaryIndexDescription_keySchema,
    localSecondaryIndexDescription_projection,

    -- * LocalSecondaryIndexInfo
    LocalSecondaryIndexInfo (..),
    newLocalSecondaryIndexInfo,
    localSecondaryIndexInfo_indexName,
    localSecondaryIndexInfo_keySchema,
    localSecondaryIndexInfo_projection,

    -- * ParameterizedStatement
    ParameterizedStatement (..),
    newParameterizedStatement,
    parameterizedStatement_parameters,
    parameterizedStatement_statement,

    -- * PointInTimeRecoveryDescription
    PointInTimeRecoveryDescription (..),
    newPointInTimeRecoveryDescription,
    pointInTimeRecoveryDescription_earliestRestorableDateTime,
    pointInTimeRecoveryDescription_latestRestorableDateTime,
    pointInTimeRecoveryDescription_pointInTimeRecoveryStatus,

    -- * PointInTimeRecoverySpecification
    PointInTimeRecoverySpecification (..),
    newPointInTimeRecoverySpecification,
    pointInTimeRecoverySpecification_pointInTimeRecoveryEnabled,

    -- * Projection
    Projection (..),
    newProjection,
    projection_nonKeyAttributes,
    projection_projectionType,

    -- * ProvisionedThroughput
    ProvisionedThroughput (..),
    newProvisionedThroughput,
    provisionedThroughput_readCapacityUnits,
    provisionedThroughput_writeCapacityUnits,

    -- * ProvisionedThroughputDescription
    ProvisionedThroughputDescription (..),
    newProvisionedThroughputDescription,
    provisionedThroughputDescription_lastDecreaseDateTime,
    provisionedThroughputDescription_lastIncreaseDateTime,
    provisionedThroughputDescription_numberOfDecreasesToday,
    provisionedThroughputDescription_readCapacityUnits,
    provisionedThroughputDescription_writeCapacityUnits,

    -- * ProvisionedThroughputOverride
    ProvisionedThroughputOverride (..),
    newProvisionedThroughputOverride,
    provisionedThroughputOverride_readCapacityUnits,

    -- * Put
    Put (..),
    newPut,
    put_conditionExpression,
    put_expressionAttributeNames,
    put_expressionAttributeValues,
    put_returnValuesOnConditionCheckFailure,
    put_item,
    put_tableName,

    -- * Replica
    Replica (..),
    newReplica,
    replica_regionName,

    -- * ReplicaAutoScalingDescription
    ReplicaAutoScalingDescription (..),
    newReplicaAutoScalingDescription,
    replicaAutoScalingDescription_globalSecondaryIndexes,
    replicaAutoScalingDescription_regionName,
    replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings,
    replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaAutoScalingDescription_replicaStatus,

    -- * ReplicaAutoScalingUpdate
    ReplicaAutoScalingUpdate (..),
    newReplicaAutoScalingUpdate,
    replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates,
    replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate,
    replicaAutoScalingUpdate_regionName,

    -- * ReplicaDescription
    ReplicaDescription (..),
    newReplicaDescription,
    replicaDescription_globalSecondaryIndexes,
    replicaDescription_kmsMasterKeyId,
    replicaDescription_provisionedThroughputOverride,
    replicaDescription_regionName,
    replicaDescription_replicaInaccessibleDateTime,
    replicaDescription_replicaStatus,
    replicaDescription_replicaStatusDescription,
    replicaDescription_replicaStatusPercentProgress,
    replicaDescription_replicaTableClassSummary,

    -- * ReplicaGlobalSecondaryIndex
    ReplicaGlobalSecondaryIndex (..),
    newReplicaGlobalSecondaryIndex,
    replicaGlobalSecondaryIndex_provisionedThroughputOverride,
    replicaGlobalSecondaryIndex_indexName,

    -- * ReplicaGlobalSecondaryIndexAutoScalingDescription
    ReplicaGlobalSecondaryIndexAutoScalingDescription (..),
    newReplicaGlobalSecondaryIndexAutoScalingDescription,
    replicaGlobalSecondaryIndexAutoScalingDescription_indexName,
    replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings,

    -- * ReplicaGlobalSecondaryIndexAutoScalingUpdate
    ReplicaGlobalSecondaryIndexAutoScalingUpdate (..),
    newReplicaGlobalSecondaryIndexAutoScalingUpdate,
    replicaGlobalSecondaryIndexAutoScalingUpdate_indexName,
    replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate,

    -- * ReplicaGlobalSecondaryIndexDescription
    ReplicaGlobalSecondaryIndexDescription (..),
    newReplicaGlobalSecondaryIndexDescription,
    replicaGlobalSecondaryIndexDescription_indexName,
    replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride,

    -- * ReplicaGlobalSecondaryIndexSettingsDescription
    ReplicaGlobalSecondaryIndexSettingsDescription (..),
    newReplicaGlobalSecondaryIndexSettingsDescription,
    replicaGlobalSecondaryIndexSettingsDescription_indexStatus,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits,
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
    replicaSettingsDescription_replicaBillingModeSummary,
    replicaSettingsDescription_replicaGlobalSecondaryIndexSettings,
    replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings,
    replicaSettingsDescription_replicaProvisionedReadCapacityUnits,
    replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaSettingsDescription_replicaProvisionedWriteCapacityUnits,
    replicaSettingsDescription_replicaStatus,
    replicaSettingsDescription_replicaTableClassSummary,
    replicaSettingsDescription_regionName,

    -- * ReplicaSettingsUpdate
    ReplicaSettingsUpdate (..),
    newReplicaSettingsUpdate,
    replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate,
    replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    replicaSettingsUpdate_replicaProvisionedReadCapacityUnits,
    replicaSettingsUpdate_replicaTableClass,
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
    restoreSummary_sourceBackupArn,
    restoreSummary_sourceTableArn,
    restoreSummary_restoreDateTime,
    restoreSummary_restoreInProgress,

    -- * S3BucketSource
    S3BucketSource (..),
    newS3BucketSource,
    s3BucketSource_s3BucketOwner,
    s3BucketSource_s3KeyPrefix,
    s3BucketSource_s3Bucket,

    -- * SSEDescription
    SSEDescription (..),
    newSSEDescription,
    sSEDescription_inaccessibleEncryptionDateTime,
    sSEDescription_kmsMasterKeyArn,
    sSEDescription_sSEType,
    sSEDescription_status,

    -- * SSESpecification
    SSESpecification (..),
    newSSESpecification,
    sSESpecification_enabled,
    sSESpecification_kmsMasterKeyId,
    sSESpecification_sSEType,

    -- * SourceTableDetails
    SourceTableDetails (..),
    newSourceTableDetails,
    sourceTableDetails_billingMode,
    sourceTableDetails_itemCount,
    sourceTableDetails_tableArn,
    sourceTableDetails_tableSizeBytes,
    sourceTableDetails_tableName,
    sourceTableDetails_tableId,
    sourceTableDetails_keySchema,
    sourceTableDetails_tableCreationDateTime,
    sourceTableDetails_provisionedThroughput,

    -- * SourceTableFeatureDetails
    SourceTableFeatureDetails (..),
    newSourceTableFeatureDetails,
    sourceTableFeatureDetails_globalSecondaryIndexes,
    sourceTableFeatureDetails_localSecondaryIndexes,
    sourceTableFeatureDetails_sSEDescription,
    sourceTableFeatureDetails_streamDescription,
    sourceTableFeatureDetails_timeToLiveDescription,

    -- * StreamSpecification
    StreamSpecification (..),
    newStreamSpecification,
    streamSpecification_streamViewType,
    streamSpecification_streamEnabled,

    -- * TableAutoScalingDescription
    TableAutoScalingDescription (..),
    newTableAutoScalingDescription,
    tableAutoScalingDescription_replicas,
    tableAutoScalingDescription_tableName,
    tableAutoScalingDescription_tableStatus,

    -- * TableClassSummary
    TableClassSummary (..),
    newTableClassSummary,
    tableClassSummary_lastUpdateDateTime,
    tableClassSummary_tableClass,

    -- * TableCreationParameters
    TableCreationParameters (..),
    newTableCreationParameters,
    tableCreationParameters_billingMode,
    tableCreationParameters_globalSecondaryIndexes,
    tableCreationParameters_provisionedThroughput,
    tableCreationParameters_sSESpecification,
    tableCreationParameters_tableName,
    tableCreationParameters_attributeDefinitions,
    tableCreationParameters_keySchema,

    -- * TableDescription
    TableDescription (..),
    newTableDescription,
    tableDescription_archivalSummary,
    tableDescription_attributeDefinitions,
    tableDescription_billingModeSummary,
    tableDescription_creationDateTime,
    tableDescription_globalSecondaryIndexes,
    tableDescription_globalTableVersion,
    tableDescription_itemCount,
    tableDescription_keySchema,
    tableDescription_latestStreamArn,
    tableDescription_latestStreamLabel,
    tableDescription_localSecondaryIndexes,
    tableDescription_provisionedThroughput,
    tableDescription_replicas,
    tableDescription_restoreSummary,
    tableDescription_sSEDescription,
    tableDescription_streamSpecification,
    tableDescription_tableArn,
    tableDescription_tableClassSummary,
    tableDescription_tableId,
    tableDescription_tableName,
    tableDescription_tableSizeBytes,
    tableDescription_tableStatus,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TimeToLiveDescription
    TimeToLiveDescription (..),
    newTimeToLiveDescription,
    timeToLiveDescription_attributeName,
    timeToLiveDescription_timeToLiveStatus,

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
    transactWriteItem_delete,
    transactWriteItem_put,
    transactWriteItem_update,

    -- * Update
    Update (..),
    newUpdate,
    update_conditionExpression,
    update_expressionAttributeNames,
    update_expressionAttributeValues,
    update_returnValuesOnConditionCheckFailure,
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
    updateReplicationGroupMemberAction_globalSecondaryIndexes,
    updateReplicationGroupMemberAction_kmsMasterKeyId,
    updateReplicationGroupMemberAction_provisionedThroughputOverride,
    updateReplicationGroupMemberAction_tableClassOverride,
    updateReplicationGroupMemberAction_regionName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.ArchivalSummary
import Amazonka.DynamoDB.Types.AttributeAction
import Amazonka.DynamoDB.Types.AttributeDefinition
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.AttributeValueUpdate
import Amazonka.DynamoDB.Types.AutoScalingPolicyDescription
import Amazonka.DynamoDB.Types.AutoScalingPolicyUpdate
import Amazonka.DynamoDB.Types.AutoScalingSettingsDescription
import Amazonka.DynamoDB.Types.AutoScalingSettingsUpdate
import Amazonka.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
import Amazonka.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
import Amazonka.DynamoDB.Types.BackupDescription
import Amazonka.DynamoDB.Types.BackupDetails
import Amazonka.DynamoDB.Types.BackupStatus
import Amazonka.DynamoDB.Types.BackupSummary
import Amazonka.DynamoDB.Types.BackupType
import Amazonka.DynamoDB.Types.BackupTypeFilter
import Amazonka.DynamoDB.Types.BatchStatementError
import Amazonka.DynamoDB.Types.BatchStatementErrorCodeEnum
import Amazonka.DynamoDB.Types.BatchStatementRequest
import Amazonka.DynamoDB.Types.BatchStatementResponse
import Amazonka.DynamoDB.Types.BillingMode
import Amazonka.DynamoDB.Types.BillingModeSummary
import Amazonka.DynamoDB.Types.Capacity
import Amazonka.DynamoDB.Types.ComparisonOperator
import Amazonka.DynamoDB.Types.Condition
import Amazonka.DynamoDB.Types.ConditionCheck
import Amazonka.DynamoDB.Types.ConditionalOperator
import Amazonka.DynamoDB.Types.ConsumedCapacity
import Amazonka.DynamoDB.Types.ContinuousBackupsDescription
import Amazonka.DynamoDB.Types.ContinuousBackupsStatus
import Amazonka.DynamoDB.Types.ContributorInsightsAction
import Amazonka.DynamoDB.Types.ContributorInsightsStatus
import Amazonka.DynamoDB.Types.ContributorInsightsSummary
import Amazonka.DynamoDB.Types.CreateGlobalSecondaryIndexAction
import Amazonka.DynamoDB.Types.CreateReplicaAction
import Amazonka.DynamoDB.Types.CreateReplicationGroupMemberAction
import Amazonka.DynamoDB.Types.CsvOptions
import Amazonka.DynamoDB.Types.Delete
import Amazonka.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
import Amazonka.DynamoDB.Types.DeleteReplicaAction
import Amazonka.DynamoDB.Types.DeleteReplicationGroupMemberAction
import Amazonka.DynamoDB.Types.DestinationStatus
import Amazonka.DynamoDB.Types.Endpoint
import Amazonka.DynamoDB.Types.ExpectedAttributeValue
import Amazonka.DynamoDB.Types.ExportDescription
import Amazonka.DynamoDB.Types.ExportFormat
import Amazonka.DynamoDB.Types.ExportStatus
import Amazonka.DynamoDB.Types.ExportSummary
import Amazonka.DynamoDB.Types.FailureException
import Amazonka.DynamoDB.Types.Get
import Amazonka.DynamoDB.Types.GlobalSecondaryIndex
import Amazonka.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate
import Amazonka.DynamoDB.Types.GlobalSecondaryIndexDescription
import Amazonka.DynamoDB.Types.GlobalSecondaryIndexInfo
import Amazonka.DynamoDB.Types.GlobalSecondaryIndexUpdate
import Amazonka.DynamoDB.Types.GlobalTable
import Amazonka.DynamoDB.Types.GlobalTableDescription
import Amazonka.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate
import Amazonka.DynamoDB.Types.GlobalTableStatus
import Amazonka.DynamoDB.Types.ImportStatus
import Amazonka.DynamoDB.Types.ImportSummary
import Amazonka.DynamoDB.Types.ImportTableDescription
import Amazonka.DynamoDB.Types.IndexStatus
import Amazonka.DynamoDB.Types.InputCompressionType
import Amazonka.DynamoDB.Types.InputFormat
import Amazonka.DynamoDB.Types.InputFormatOptions
import Amazonka.DynamoDB.Types.ItemCollectionMetrics
import Amazonka.DynamoDB.Types.ItemResponse
import Amazonka.DynamoDB.Types.KeySchemaElement
import Amazonka.DynamoDB.Types.KeyType
import Amazonka.DynamoDB.Types.KeysAndAttributes
import Amazonka.DynamoDB.Types.KinesisDataStreamDestination
import Amazonka.DynamoDB.Types.KinesisStreamingDestinationInput
import Amazonka.DynamoDB.Types.KinesisStreamingDestinationOutput
import Amazonka.DynamoDB.Types.LocalSecondaryIndex
import Amazonka.DynamoDB.Types.LocalSecondaryIndexDescription
import Amazonka.DynamoDB.Types.LocalSecondaryIndexInfo
import Amazonka.DynamoDB.Types.ParameterizedStatement
import Amazonka.DynamoDB.Types.PointInTimeRecoveryDescription
import Amazonka.DynamoDB.Types.PointInTimeRecoverySpecification
import Amazonka.DynamoDB.Types.PointInTimeRecoveryStatus
import Amazonka.DynamoDB.Types.Projection
import Amazonka.DynamoDB.Types.ProjectionType
import Amazonka.DynamoDB.Types.ProvisionedThroughput
import Amazonka.DynamoDB.Types.ProvisionedThroughputDescription
import Amazonka.DynamoDB.Types.ProvisionedThroughputOverride
import Amazonka.DynamoDB.Types.Put
import Amazonka.DynamoDB.Types.Replica
import Amazonka.DynamoDB.Types.ReplicaAutoScalingDescription
import Amazonka.DynamoDB.Types.ReplicaAutoScalingUpdate
import Amazonka.DynamoDB.Types.ReplicaDescription
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
import Amazonka.DynamoDB.Types.ReplicaSettingsDescription
import Amazonka.DynamoDB.Types.ReplicaSettingsUpdate
import Amazonka.DynamoDB.Types.ReplicaStatus
import Amazonka.DynamoDB.Types.ReplicaUpdate
import Amazonka.DynamoDB.Types.ReplicationGroupUpdate
import Amazonka.DynamoDB.Types.RestoreSummary
import Amazonka.DynamoDB.Types.ReturnConsumedCapacity
import Amazonka.DynamoDB.Types.ReturnItemCollectionMetrics
import Amazonka.DynamoDB.Types.ReturnValue
import Amazonka.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Amazonka.DynamoDB.Types.S3BucketSource
import Amazonka.DynamoDB.Types.S3SseAlgorithm
import Amazonka.DynamoDB.Types.SSEDescription
import Amazonka.DynamoDB.Types.SSESpecification
import Amazonka.DynamoDB.Types.SSEStatus
import Amazonka.DynamoDB.Types.SSEType
import Amazonka.DynamoDB.Types.ScalarAttributeType
import Amazonka.DynamoDB.Types.Select
import Amazonka.DynamoDB.Types.SourceTableDetails
import Amazonka.DynamoDB.Types.SourceTableFeatureDetails
import Amazonka.DynamoDB.Types.StreamSpecification
import Amazonka.DynamoDB.Types.StreamViewType
import Amazonka.DynamoDB.Types.TableAutoScalingDescription
import Amazonka.DynamoDB.Types.TableClass
import Amazonka.DynamoDB.Types.TableClassSummary
import Amazonka.DynamoDB.Types.TableCreationParameters
import Amazonka.DynamoDB.Types.TableDescription
import Amazonka.DynamoDB.Types.TableStatus
import Amazonka.DynamoDB.Types.Tag
import Amazonka.DynamoDB.Types.TimeToLiveDescription
import Amazonka.DynamoDB.Types.TimeToLiveSpecification
import Amazonka.DynamoDB.Types.TimeToLiveStatus
import Amazonka.DynamoDB.Types.TransactGetItem
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.Update
import Amazonka.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
import Amazonka.DynamoDB.Types.UpdateReplicationGroupMemberAction
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2012-08-10@ of the Amazon DynamoDB SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DynamoDB",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "dynamodb",
      Core.signingName = "dynamodb",
      Core.version = "2012-08-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DynamoDB",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "TransactionInProgressException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "still_processing"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | There is another ongoing conflicting backup control plane operation on
-- the table. The backup is either being created, deleted or restored to a
-- table.
_BackupInUseException :: Core.AsError a => Lens.Fold a Core.ServiceError
_BackupInUseException =
  Core._MatchServiceError
    defaultService
    "BackupInUseException"

-- | Backup not found for the given BackupARN.
_BackupNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_BackupNotFoundException =
  Core._MatchServiceError
    defaultService
    "BackupNotFoundException"

-- | A condition specified in the operation could not be evaluated.
_ConditionalCheckFailedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConditionalCheckFailedException =
  Core._MatchServiceError
    defaultService
    "ConditionalCheckFailedException"

-- | Backups have not yet been enabled for this table.
_ContinuousBackupsUnavailableException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ContinuousBackupsUnavailableException =
  Core._MatchServiceError
    defaultService
    "ContinuousBackupsUnavailableException"

-- | There was an attempt to insert an item with the same primary key as an
-- item that already exists in the DynamoDB table.
_DuplicateItemException :: Core.AsError a => Lens.Fold a Core.ServiceError
_DuplicateItemException =
  Core._MatchServiceError
    defaultService
    "DuplicateItemException"

-- | There was a conflict when writing to the specified S3 bucket.
_ExportConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ExportConflictException =
  Core._MatchServiceError
    defaultService
    "ExportConflictException"

-- | The specified export was not found.
_ExportNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ExportNotFoundException =
  Core._MatchServiceError
    defaultService
    "ExportNotFoundException"

-- | The specified global table already exists.
_GlobalTableAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_GlobalTableAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "GlobalTableAlreadyExistsException"

-- | The specified global table does not exist.
_GlobalTableNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_GlobalTableNotFoundException =
  Core._MatchServiceError
    defaultService
    "GlobalTableNotFoundException"

-- | DynamoDB rejected the request because you retried a request with a
-- different payload but with an idempotent token that was already used.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Fold a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | There was a conflict when importing from the specified S3 source. This
-- can occur when the current import conflicts with a previous import
-- request that had the same client token.
_ImportConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ImportConflictException =
  Core._MatchServiceError
    defaultService
    "ImportConflictException"

-- | The specified import was not found.
_ImportNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ImportNotFoundException =
  Core._MatchServiceError
    defaultService
    "ImportNotFoundException"

-- | The operation tried to access a nonexistent index.
_IndexNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_IndexNotFoundException =
  Core._MatchServiceError
    defaultService
    "IndexNotFoundException"

-- | An error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | The specified @ExportTime@ is outside of the point in time recovery
-- window.
_InvalidExportTimeException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidExportTimeException =
  Core._MatchServiceError
    defaultService
    "InvalidExportTimeException"

-- | An invalid restore time was specified. RestoreDateTime must be between
-- EarliestRestorableDateTime and LatestRestorableDateTime.
_InvalidRestoreTimeException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidRestoreTimeException =
  Core._MatchServiceError
    defaultService
    "InvalidRestoreTimeException"

-- | An item collection is too large. This exception is only returned for
-- tables that have one or more local secondary indexes.
_ItemCollectionSizeLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ItemCollectionSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ItemCollectionSizeLimitExceededException"

-- | There is no limit to the number of daily on-demand backups that can be
-- taken.
--
-- For most purposes, up to 500 simultaneous table operations are allowed
-- per account. These operations include @CreateTable@, @UpdateTable@,
-- @DeleteTable@,@UpdateTimeToLive@, @RestoreTableFromBackup@, and
-- @RestoreTableToPointInTime@.
--
-- When you are creating a table with one or more secondary indexes, you
-- can have up to 250 such requests running at a time. However, if the
-- table or index specifications are complex, then DynamoDB might
-- temporarily reduce the number of concurrent operations.
--
-- When importing into DynamoDB, up to 50 simultaneous import table
-- operations are allowed per account.
--
-- There is a soft account quota of 2,500 tables.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Point in time recovery has not yet been enabled for this source table.
_PointInTimeRecoveryUnavailableException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PointInTimeRecoveryUnavailableException =
  Core._MatchServiceError
    defaultService
    "PointInTimeRecoveryUnavailableException"

-- | Your request rate is too high. The Amazon Web Services SDKs for DynamoDB
-- automatically retry requests that receive this exception. Your request
-- is eventually successful, unless your retry queue is too large to
-- finish. Reduce the frequency of requests and use exponential backoff.
-- For more information, go to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Programming.Errors.html#Programming.Errors.RetryAndBackoff Error Retries and Exponential Backoff>
-- in the /Amazon DynamoDB Developer Guide/.
_ProvisionedThroughputExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ProvisionedThroughputExceededException"

-- | The specified replica is already part of the global table.
_ReplicaAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ReplicaAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ReplicaAlreadyExistsException"

-- | The specified replica is no longer part of the global table.
_ReplicaNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ReplicaNotFoundException =
  Core._MatchServiceError
    defaultService
    "ReplicaNotFoundException"

-- | Throughput exceeds the current throughput quota for your account. Please
-- contact <https://aws.amazon.com/support Amazon Web Services Support> to
-- request a quota increase.
_RequestLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_RequestLimitExceeded =
  Core._MatchServiceError
    defaultService
    "RequestLimitExceeded"

-- | The operation conflicts with the resource\'s availability. For example,
-- you attempted to recreate an existing table, or tried to delete a table
-- currently in the @CREATING@ state.
_ResourceInUseException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The operation tried to access a nonexistent table or index. The resource
-- might not be specified correctly, or its status might not be @ACTIVE@.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | A target table with the specified name already exists.
_TableAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TableAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "TableAlreadyExistsException"

-- | A target table with the specified name is either being created or
-- deleted.
_TableInUseException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TableInUseException =
  Core._MatchServiceError
    defaultService
    "TableInUseException"

-- | A source table with the name @TableName@ does not currently exist within
-- the subscriber\'s account or the subscriber is operating in the wrong
-- Amazon Web Services Region.
_TableNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TableNotFoundException =
  Core._MatchServiceError
    defaultService
    "TableNotFoundException"

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
-- requested items, if an item has no error it will have @None@ code and
-- @Null@ message.
--
-- Cancellation reason codes and possible error messages:
--
-- -   No Errors:
--
--     -   Code: @None@
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
_TransactionCanceledException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TransactionCanceledException =
  Core._MatchServiceError
    defaultService
    "TransactionCanceledException"

-- | Operation was rejected because there is an ongoing transaction for the
-- item.
_TransactionConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TransactionConflictException =
  Core._MatchServiceError
    defaultService
    "TransactionConflictException"

-- | The transaction with the given request token is already in progress.
_TransactionInProgressException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TransactionInProgressException =
  Core._MatchServiceError
    defaultService
    "TransactionInProgressException"
