{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDB.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Lens
  ( -- * Operations

    -- ** BatchExecuteStatement
    batchExecuteStatement_statements,
    batchExecuteStatementResponse_responses,
    batchExecuteStatementResponse_httpStatus,

    -- ** BatchGetItem
    batchGetItem_returnConsumedCapacity,
    batchGetItem_requestItems,
    batchGetItemResponse_consumedCapacity,
    batchGetItemResponse_httpStatus,
    batchGetItemResponse_responses,
    batchGetItemResponse_unprocessedKeys,

    -- ** BatchWriteItem
    batchWriteItem_returnConsumedCapacity,
    batchWriteItem_returnItemCollectionMetrics,
    batchWriteItem_requestItems,
    batchWriteItemResponse_consumedCapacity,
    batchWriteItemResponse_itemCollectionMetrics,
    batchWriteItemResponse_httpStatus,
    batchWriteItemResponse_unprocessedItems,

    -- ** CreateBackup
    createBackup_tableName,
    createBackup_backupName,
    createBackupResponse_backupDetails,
    createBackupResponse_httpStatus,

    -- ** CreateGlobalTable
    createGlobalTable_globalTableName,
    createGlobalTable_replicationGroup,
    createGlobalTableResponse_globalTableDescription,
    createGlobalTableResponse_httpStatus,

    -- ** CreateTable
    createTable_tags,
    createTable_localSecondaryIndexes,
    createTable_billingMode,
    createTable_provisionedThroughput,
    createTable_sSESpecification,
    createTable_globalSecondaryIndexes,
    createTable_streamSpecification,
    createTable_attributeDefinitions,
    createTable_tableName,
    createTable_keySchema,
    createTableResponse_tableDescription,
    createTableResponse_httpStatus,

    -- ** DeleteBackup
    deleteBackup_backupArn,
    deleteBackupResponse_backupDescription,
    deleteBackupResponse_httpStatus,

    -- ** DeleteItem
    deleteItem_returnValues,
    deleteItem_expressionAttributeValues,
    deleteItem_expressionAttributeNames,
    deleteItem_conditionalOperator,
    deleteItem_returnConsumedCapacity,
    deleteItem_expected,
    deleteItem_returnItemCollectionMetrics,
    deleteItem_conditionExpression,
    deleteItem_tableName,
    deleteItem_key,
    deleteItemResponse_consumedCapacity,
    deleteItemResponse_attributes,
    deleteItemResponse_itemCollectionMetrics,
    deleteItemResponse_httpStatus,

    -- ** DeleteTable
    deleteTable_tableName,
    deleteTableResponse_tableDescription,
    deleteTableResponse_httpStatus,

    -- ** DescribeBackup
    describeBackup_backupArn,
    describeBackupResponse_backupDescription,
    describeBackupResponse_httpStatus,

    -- ** DescribeContinuousBackups
    describeContinuousBackups_tableName,
    describeContinuousBackupsResponse_continuousBackupsDescription,
    describeContinuousBackupsResponse_httpStatus,

    -- ** DescribeContributorInsights
    describeContributorInsights_indexName,
    describeContributorInsights_tableName,
    describeContributorInsightsResponse_contributorInsightsStatus,
    describeContributorInsightsResponse_tableName,
    describeContributorInsightsResponse_contributorInsightsRuleList,
    describeContributorInsightsResponse_lastUpdateDateTime,
    describeContributorInsightsResponse_failureException,
    describeContributorInsightsResponse_indexName,
    describeContributorInsightsResponse_httpStatus,

    -- ** DescribeEndpoints
    describeEndpointsResponse_httpStatus,
    describeEndpointsResponse_endpoints,

    -- ** DescribeExport
    describeExport_exportArn,
    describeExportResponse_exportDescription,
    describeExportResponse_httpStatus,

    -- ** DescribeGlobalTable
    describeGlobalTable_globalTableName,
    describeGlobalTableResponse_globalTableDescription,
    describeGlobalTableResponse_httpStatus,

    -- ** DescribeGlobalTableSettings
    describeGlobalTableSettings_globalTableName,
    describeGlobalTableSettingsResponse_globalTableName,
    describeGlobalTableSettingsResponse_replicaSettings,
    describeGlobalTableSettingsResponse_httpStatus,

    -- ** DescribeKinesisStreamingDestination
    describeKinesisStreamingDestination_tableName,
    describeKinesisStreamingDestinationResponse_tableName,
    describeKinesisStreamingDestinationResponse_kinesisDataStreamDestinations,
    describeKinesisStreamingDestinationResponse_httpStatus,

    -- ** DescribeLimits
    describeLimitsResponse_tableMaxReadCapacityUnits,
    describeLimitsResponse_accountMaxWriteCapacityUnits,
    describeLimitsResponse_tableMaxWriteCapacityUnits,
    describeLimitsResponse_accountMaxReadCapacityUnits,
    describeLimitsResponse_httpStatus,

    -- ** DescribeTable
    describeTable_tableName,
    describeTableResponse_table,
    describeTableResponse_httpStatus,

    -- ** DescribeTableReplicaAutoScaling
    describeTableReplicaAutoScaling_tableName,
    describeTableReplicaAutoScalingResponse_tableAutoScalingDescription,
    describeTableReplicaAutoScalingResponse_httpStatus,

    -- ** DescribeTimeToLive
    describeTimeToLive_tableName,
    describeTimeToLiveResponse_timeToLiveDescription,
    describeTimeToLiveResponse_httpStatus,

    -- ** DisableKinesisStreamingDestination
    disableKinesisStreamingDestination_tableName,
    disableKinesisStreamingDestination_streamArn,
    kinesisStreamingDestinationOutput_tableName,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_destinationStatus,

    -- ** EnableKinesisStreamingDestination
    enableKinesisStreamingDestination_tableName,
    enableKinesisStreamingDestination_streamArn,
    kinesisStreamingDestinationOutput_tableName,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_destinationStatus,

    -- ** ExecuteStatement
    executeStatement_nextToken,
    executeStatement_consistentRead,
    executeStatement_parameters,
    executeStatement_statement,
    executeStatementResponse_items,
    executeStatementResponse_nextToken,
    executeStatementResponse_httpStatus,

    -- ** ExecuteTransaction
    executeTransaction_clientRequestToken,
    executeTransaction_transactStatements,
    executeTransactionResponse_responses,
    executeTransactionResponse_httpStatus,

    -- ** ExportTableToPointInTime
    exportTableToPointInTime_clientToken,
    exportTableToPointInTime_s3SseAlgorithm,
    exportTableToPointInTime_exportFormat,
    exportTableToPointInTime_exportTime,
    exportTableToPointInTime_s3BucketOwner,
    exportTableToPointInTime_s3SseKmsKeyId,
    exportTableToPointInTime_s3Prefix,
    exportTableToPointInTime_tableArn,
    exportTableToPointInTime_s3Bucket,
    exportTableToPointInTimeResponse_exportDescription,
    exportTableToPointInTimeResponse_httpStatus,

    -- ** GetItem
    getItem_consistentRead,
    getItem_expressionAttributeNames,
    getItem_returnConsumedCapacity,
    getItem_attributesToGet,
    getItem_projectionExpression,
    getItem_tableName,
    getItem_key,
    getItemResponse_consumedCapacity,
    getItemResponse_httpStatus,
    getItemResponse_item,

    -- ** ListBackups
    listBackups_tableName,
    listBackups_exclusiveStartBackupArn,
    listBackups_timeRangeUpperBound,
    listBackups_limit,
    listBackups_backupType,
    listBackups_timeRangeLowerBound,
    listBackupsResponse_backupSummaries,
    listBackupsResponse_lastEvaluatedBackupArn,
    listBackupsResponse_httpStatus,

    -- ** ListContributorInsights
    listContributorInsights_tableName,
    listContributorInsights_nextToken,
    listContributorInsights_maxResults,
    listContributorInsightsResponse_nextToken,
    listContributorInsightsResponse_contributorInsightsSummaries,
    listContributorInsightsResponse_httpStatus,

    -- ** ListExports
    listExports_tableArn,
    listExports_nextToken,
    listExports_maxResults,
    listExportsResponse_nextToken,
    listExportsResponse_exportSummaries,
    listExportsResponse_httpStatus,

    -- ** ListGlobalTables
    listGlobalTables_exclusiveStartGlobalTableName,
    listGlobalTables_regionName,
    listGlobalTables_limit,
    listGlobalTablesResponse_globalTables,
    listGlobalTablesResponse_lastEvaluatedGlobalTableName,
    listGlobalTablesResponse_httpStatus,

    -- ** ListTables
    listTables_exclusiveStartTableName,
    listTables_limit,
    listTablesResponse_lastEvaluatedTableName,
    listTablesResponse_tableNames,
    listTablesResponse_httpStatus,

    -- ** ListTagsOfResource
    listTagsOfResource_nextToken,
    listTagsOfResource_resourceArn,
    listTagsOfResourceResponse_tags,
    listTagsOfResourceResponse_nextToken,
    listTagsOfResourceResponse_httpStatus,

    -- ** PutItem
    putItem_returnValues,
    putItem_expressionAttributeValues,
    putItem_expressionAttributeNames,
    putItem_conditionalOperator,
    putItem_returnConsumedCapacity,
    putItem_expected,
    putItem_returnItemCollectionMetrics,
    putItem_conditionExpression,
    putItem_tableName,
    putItem_item,
    putItemResponse_consumedCapacity,
    putItemResponse_attributes,
    putItemResponse_itemCollectionMetrics,
    putItemResponse_httpStatus,

    -- ** Query
    query_consistentRead,
    query_expressionAttributeValues,
    query_expressionAttributeNames,
    query_scanIndexForward,
    query_conditionalOperator,
    query_returnConsumedCapacity,
    query_exclusiveStartKey,
    query_filterExpression,
    query_attributesToGet,
    query_keyConditionExpression,
    query_select,
    query_limit,
    query_indexName,
    query_queryFilter,
    query_keyConditions,
    query_projectionExpression,
    query_tableName,
    queryResponse_lastEvaluatedKey,
    queryResponse_count,
    queryResponse_consumedCapacity,
    queryResponse_scannedCount,
    queryResponse_httpStatus,
    queryResponse_items,

    -- ** RestoreTableFromBackup
    restoreTableFromBackup_provisionedThroughputOverride,
    restoreTableFromBackup_localSecondaryIndexOverride,
    restoreTableFromBackup_sSESpecificationOverride,
    restoreTableFromBackup_billingModeOverride,
    restoreTableFromBackup_globalSecondaryIndexOverride,
    restoreTableFromBackup_targetTableName,
    restoreTableFromBackup_backupArn,
    restoreTableFromBackupResponse_tableDescription,
    restoreTableFromBackupResponse_httpStatus,

    -- ** RestoreTableToPointInTime
    restoreTableToPointInTime_provisionedThroughputOverride,
    restoreTableToPointInTime_localSecondaryIndexOverride,
    restoreTableToPointInTime_sSESpecificationOverride,
    restoreTableToPointInTime_sourceTableArn,
    restoreTableToPointInTime_useLatestRestorableTime,
    restoreTableToPointInTime_billingModeOverride,
    restoreTableToPointInTime_restoreDateTime,
    restoreTableToPointInTime_sourceTableName,
    restoreTableToPointInTime_globalSecondaryIndexOverride,
    restoreTableToPointInTime_targetTableName,
    restoreTableToPointInTimeResponse_tableDescription,
    restoreTableToPointInTimeResponse_httpStatus,

    -- ** Scan
    scan_scanFilter,
    scan_consistentRead,
    scan_expressionAttributeValues,
    scan_expressionAttributeNames,
    scan_conditionalOperator,
    scan_returnConsumedCapacity,
    scan_exclusiveStartKey,
    scan_filterExpression,
    scan_attributesToGet,
    scan_select,
    scan_segment,
    scan_limit,
    scan_indexName,
    scan_totalSegments,
    scan_projectionExpression,
    scan_tableName,
    scanResponse_items,
    scanResponse_lastEvaluatedKey,
    scanResponse_count,
    scanResponse_consumedCapacity,
    scanResponse_scannedCount,
    scanResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** TransactGetItems
    transactGetItems_returnConsumedCapacity,
    transactGetItems_transactItems,
    transactGetItemsResponse_consumedCapacity,
    transactGetItemsResponse_responses,
    transactGetItemsResponse_httpStatus,

    -- ** TransactWriteItems
    transactWriteItems_clientRequestToken,
    transactWriteItems_returnConsumedCapacity,
    transactWriteItems_returnItemCollectionMetrics,
    transactWriteItems_transactItems,
    transactWriteItemsResponse_consumedCapacity,
    transactWriteItemsResponse_itemCollectionMetrics,
    transactWriteItemsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateContinuousBackups
    updateContinuousBackups_tableName,
    updateContinuousBackups_pointInTimeRecoverySpecification,
    updateContinuousBackupsResponse_continuousBackupsDescription,
    updateContinuousBackupsResponse_httpStatus,

    -- ** UpdateContributorInsights
    updateContributorInsights_indexName,
    updateContributorInsights_tableName,
    updateContributorInsights_contributorInsightsAction,
    updateContributorInsightsResponse_contributorInsightsStatus,
    updateContributorInsightsResponse_tableName,
    updateContributorInsightsResponse_indexName,
    updateContributorInsightsResponse_httpStatus,

    -- ** UpdateGlobalTable
    updateGlobalTable_globalTableName,
    updateGlobalTable_replicaUpdates,
    updateGlobalTableResponse_globalTableDescription,
    updateGlobalTableResponse_httpStatus,

    -- ** UpdateGlobalTableSettings
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits,
    updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate,
    updateGlobalTableSettings_globalTableBillingMode,
    updateGlobalTableSettings_replicaSettingsUpdate,
    updateGlobalTableSettings_globalTableName,
    updateGlobalTableSettingsResponse_globalTableName,
    updateGlobalTableSettingsResponse_replicaSettings,
    updateGlobalTableSettingsResponse_httpStatus,

    -- ** UpdateItem
    updateItem_returnValues,
    updateItem_expressionAttributeValues,
    updateItem_expressionAttributeNames,
    updateItem_conditionalOperator,
    updateItem_returnConsumedCapacity,
    updateItem_expected,
    updateItem_attributeUpdates,
    updateItem_returnItemCollectionMetrics,
    updateItem_updateExpression,
    updateItem_conditionExpression,
    updateItem_tableName,
    updateItem_key,
    updateItemResponse_consumedCapacity,
    updateItemResponse_attributes,
    updateItemResponse_itemCollectionMetrics,
    updateItemResponse_httpStatus,

    -- ** UpdateTable
    updateTable_globalSecondaryIndexUpdates,
    updateTable_replicaUpdates,
    updateTable_billingMode,
    updateTable_provisionedThroughput,
    updateTable_sSESpecification,
    updateTable_streamSpecification,
    updateTable_attributeDefinitions,
    updateTable_tableName,
    updateTableResponse_tableDescription,
    updateTableResponse_httpStatus,

    -- ** UpdateTableReplicaAutoScaling
    updateTableReplicaAutoScaling_globalSecondaryIndexUpdates,
    updateTableReplicaAutoScaling_replicaUpdates,
    updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate,
    updateTableReplicaAutoScaling_tableName,
    updateTableReplicaAutoScalingResponse_tableAutoScalingDescription,
    updateTableReplicaAutoScalingResponse_httpStatus,

    -- ** UpdateTimeToLive
    updateTimeToLive_tableName,
    updateTimeToLive_timeToLiveSpecification,
    updateTimeToLiveResponse_timeToLiveSpecification,
    updateTimeToLiveResponse_httpStatus,

    -- * Types

    -- ** ArchivalSummary
    archivalSummary_archivalReason,
    archivalSummary_archivalDateTime,
    archivalSummary_archivalBackupArn,

    -- ** AttributeDefinition
    attributeDefinition_attributeName,
    attributeDefinition_attributeType,

    -- ** AttributeValueUpdate
    attributeValueUpdate_action,
    attributeValueUpdate_value,

    -- ** AutoScalingPolicyDescription
    autoScalingPolicyDescription_policyName,
    autoScalingPolicyDescription_targetTrackingScalingPolicyConfiguration,

    -- ** AutoScalingPolicyUpdate
    autoScalingPolicyUpdate_policyName,
    autoScalingPolicyUpdate_targetTrackingScalingPolicyConfiguration,

    -- ** AutoScalingSettingsDescription
    autoScalingSettingsDescription_minimumUnits,
    autoScalingSettingsDescription_autoScalingRoleArn,
    autoScalingSettingsDescription_scalingPolicies,
    autoScalingSettingsDescription_autoScalingDisabled,
    autoScalingSettingsDescription_maximumUnits,

    -- ** AutoScalingSettingsUpdate
    autoScalingSettingsUpdate_minimumUnits,
    autoScalingSettingsUpdate_scalingPolicyUpdate,
    autoScalingSettingsUpdate_autoScalingRoleArn,
    autoScalingSettingsUpdate_autoScalingDisabled,
    autoScalingSettingsUpdate_maximumUnits,

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_disableScaleIn,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleInCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleOutCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_targetValue,

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_disableScaleIn,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleInCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleOutCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_targetValue,

    -- ** BackupDescription
    backupDescription_sourceTableDetails,
    backupDescription_backupDetails,
    backupDescription_sourceTableFeatureDetails,

    -- ** BackupDetails
    backupDetails_backupSizeBytes,
    backupDetails_backupExpiryDateTime,
    backupDetails_backupArn,
    backupDetails_backupName,
    backupDetails_backupStatus,
    backupDetails_backupType,
    backupDetails_backupCreationDateTime,

    -- ** BackupSummary
    backupSummary_tableName,
    backupSummary_tableArn,
    backupSummary_backupSizeBytes,
    backupSummary_backupStatus,
    backupSummary_tableId,
    backupSummary_backupName,
    backupSummary_backupType,
    backupSummary_backupArn,
    backupSummary_backupExpiryDateTime,
    backupSummary_backupCreationDateTime,

    -- ** BatchStatementError
    batchStatementError_message,
    batchStatementError_code,

    -- ** BatchStatementRequest
    batchStatementRequest_consistentRead,
    batchStatementRequest_parameters,
    batchStatementRequest_statement,

    -- ** BatchStatementResponse
    batchStatementResponse_tableName,
    batchStatementResponse_item,
    batchStatementResponse_error,

    -- ** BillingModeSummary
    billingModeSummary_billingMode,
    billingModeSummary_lastUpdateToPayPerRequestDateTime,

    -- ** Capacity
    capacity_readCapacityUnits,
    capacity_capacityUnits,
    capacity_writeCapacityUnits,

    -- ** Condition
    condition_attributeValueList,
    condition_comparisonOperator,

    -- ** ConditionCheck
    conditionCheck_expressionAttributeValues,
    conditionCheck_expressionAttributeNames,
    conditionCheck_returnValuesOnConditionCheckFailure,
    conditionCheck_key,
    conditionCheck_tableName,
    conditionCheck_conditionExpression,

    -- ** ConsumedCapacity
    consumedCapacity_tableName,
    consumedCapacity_localSecondaryIndexes,
    consumedCapacity_readCapacityUnits,
    consumedCapacity_capacityUnits,
    consumedCapacity_writeCapacityUnits,
    consumedCapacity_globalSecondaryIndexes,
    consumedCapacity_table,

    -- ** ContinuousBackupsDescription
    continuousBackupsDescription_pointInTimeRecoveryDescription,
    continuousBackupsDescription_continuousBackupsStatus,

    -- ** ContributorInsightsSummary
    contributorInsightsSummary_contributorInsightsStatus,
    contributorInsightsSummary_tableName,
    contributorInsightsSummary_indexName,

    -- ** CreateGlobalSecondaryIndexAction
    createGlobalSecondaryIndexAction_provisionedThroughput,
    createGlobalSecondaryIndexAction_indexName,
    createGlobalSecondaryIndexAction_keySchema,
    createGlobalSecondaryIndexAction_projection,

    -- ** CreateReplicaAction
    createReplicaAction_regionName,

    -- ** CreateReplicationGroupMemberAction
    createReplicationGroupMemberAction_kmsMasterKeyId,
    createReplicationGroupMemberAction_provisionedThroughputOverride,
    createReplicationGroupMemberAction_globalSecondaryIndexes,
    createReplicationGroupMemberAction_regionName,

    -- ** Delete
    delete_expressionAttributeValues,
    delete_expressionAttributeNames,
    delete_returnValuesOnConditionCheckFailure,
    delete_conditionExpression,
    delete_key,
    delete_tableName,

    -- ** DeleteGlobalSecondaryIndexAction
    deleteGlobalSecondaryIndexAction_indexName,

    -- ** DeleteReplicaAction
    deleteReplicaAction_regionName,

    -- ** DeleteReplicationGroupMemberAction
    deleteReplicationGroupMemberAction_regionName,

    -- ** Endpoint
    endpoint_address,
    endpoint_cachePeriodInMinutes,

    -- ** ExpectedAttributeValue
    expectedAttributeValue_exists,
    expectedAttributeValue_attributeValueList,
    expectedAttributeValue_comparisonOperator,
    expectedAttributeValue_value,

    -- ** ExportDescription
    exportDescription_s3Bucket,
    exportDescription_clientToken,
    exportDescription_tableArn,
    exportDescription_failureCode,
    exportDescription_exportArn,
    exportDescription_billedSizeBytes,
    exportDescription_s3SseAlgorithm,
    exportDescription_itemCount,
    exportDescription_endTime,
    exportDescription_failureMessage,
    exportDescription_tableId,
    exportDescription_exportFormat,
    exportDescription_exportTime,
    exportDescription_s3BucketOwner,
    exportDescription_exportStatus,
    exportDescription_s3SseKmsKeyId,
    exportDescription_exportManifest,
    exportDescription_startTime,
    exportDescription_s3Prefix,

    -- ** ExportSummary
    exportSummary_exportArn,
    exportSummary_exportStatus,

    -- ** FailureException
    failureException_exceptionDescription,
    failureException_exceptionName,

    -- ** Get
    get_expressionAttributeNames,
    get_projectionExpression,
    get_key,
    get_tableName,

    -- ** GlobalSecondaryIndex
    globalSecondaryIndex_provisionedThroughput,
    globalSecondaryIndex_indexName,
    globalSecondaryIndex_keySchema,
    globalSecondaryIndex_projection,

    -- ** GlobalSecondaryIndexAutoScalingUpdate
    globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate,
    globalSecondaryIndexAutoScalingUpdate_indexName,

    -- ** GlobalSecondaryIndexDescription
    globalSecondaryIndexDescription_itemCount,
    globalSecondaryIndexDescription_provisionedThroughput,
    globalSecondaryIndexDescription_backfilling,
    globalSecondaryIndexDescription_indexName,
    globalSecondaryIndexDescription_indexArn,
    globalSecondaryIndexDescription_indexStatus,
    globalSecondaryIndexDescription_indexSizeBytes,
    globalSecondaryIndexDescription_keySchema,
    globalSecondaryIndexDescription_projection,

    -- ** GlobalSecondaryIndexInfo
    globalSecondaryIndexInfo_provisionedThroughput,
    globalSecondaryIndexInfo_indexName,
    globalSecondaryIndexInfo_keySchema,
    globalSecondaryIndexInfo_projection,

    -- ** GlobalSecondaryIndexUpdate
    globalSecondaryIndexUpdate_create,
    globalSecondaryIndexUpdate_delete,
    globalSecondaryIndexUpdate_update,

    -- ** GlobalTable
    globalTable_replicationGroup,
    globalTable_globalTableName,

    -- ** GlobalTableDescription
    globalTableDescription_globalTableStatus,
    globalTableDescription_replicationGroup,
    globalTableDescription_globalTableName,
    globalTableDescription_creationDateTime,
    globalTableDescription_globalTableArn,

    -- ** GlobalTableGlobalSecondaryIndexSettingsUpdate
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityUnits,
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityAutoScalingSettingsUpdate,
    globalTableGlobalSecondaryIndexSettingsUpdate_indexName,

    -- ** ItemCollectionMetrics
    itemCollectionMetrics_sizeEstimateRangeGB,
    itemCollectionMetrics_itemCollectionKey,

    -- ** ItemResponse
    itemResponse_item,

    -- ** KeySchemaElement
    keySchemaElement_attributeName,
    keySchemaElement_keyType,

    -- ** KeysAndAttributes
    keysAndAttributes_consistentRead,
    keysAndAttributes_expressionAttributeNames,
    keysAndAttributes_attributesToGet,
    keysAndAttributes_projectionExpression,
    keysAndAttributes_keys,

    -- ** KinesisDataStreamDestination
    kinesisDataStreamDestination_destinationStatusDescription,
    kinesisDataStreamDestination_streamArn,
    kinesisDataStreamDestination_destinationStatus,

    -- ** KinesisStreamingDestinationInput
    kinesisStreamingDestinationInput_tableName,
    kinesisStreamingDestinationInput_streamArn,

    -- ** KinesisStreamingDestinationOutput
    kinesisStreamingDestinationOutput_tableName,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_destinationStatus,

    -- ** LocalSecondaryIndex
    localSecondaryIndex_indexName,
    localSecondaryIndex_keySchema,
    localSecondaryIndex_projection,

    -- ** LocalSecondaryIndexDescription
    localSecondaryIndexDescription_itemCount,
    localSecondaryIndexDescription_indexName,
    localSecondaryIndexDescription_indexArn,
    localSecondaryIndexDescription_indexSizeBytes,
    localSecondaryIndexDescription_keySchema,
    localSecondaryIndexDescription_projection,

    -- ** LocalSecondaryIndexInfo
    localSecondaryIndexInfo_indexName,
    localSecondaryIndexInfo_keySchema,
    localSecondaryIndexInfo_projection,

    -- ** ParameterizedStatement
    parameterizedStatement_parameters,
    parameterizedStatement_statement,

    -- ** PointInTimeRecoveryDescription
    pointInTimeRecoveryDescription_earliestRestorableDateTime,
    pointInTimeRecoveryDescription_pointInTimeRecoveryStatus,
    pointInTimeRecoveryDescription_latestRestorableDateTime,

    -- ** PointInTimeRecoverySpecification
    pointInTimeRecoverySpecification_pointInTimeRecoveryEnabled,

    -- ** Projection
    projection_projectionType,
    projection_nonKeyAttributes,

    -- ** ProvisionedThroughput
    provisionedThroughput_readCapacityUnits,
    provisionedThroughput_writeCapacityUnits,

    -- ** ProvisionedThroughputDescription
    provisionedThroughputDescription_readCapacityUnits,
    provisionedThroughputDescription_numberOfDecreasesToday,
    provisionedThroughputDescription_writeCapacityUnits,
    provisionedThroughputDescription_lastIncreaseDateTime,
    provisionedThroughputDescription_lastDecreaseDateTime,

    -- ** ProvisionedThroughputOverride
    provisionedThroughputOverride_readCapacityUnits,

    -- ** Put
    put_expressionAttributeValues,
    put_expressionAttributeNames,
    put_returnValuesOnConditionCheckFailure,
    put_conditionExpression,
    put_item,
    put_tableName,

    -- ** Replica
    replica_regionName,

    -- ** ReplicaAutoScalingDescription
    replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings,
    replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaAutoScalingDescription_regionName,
    replicaAutoScalingDescription_globalSecondaryIndexes,
    replicaAutoScalingDescription_replicaStatus,

    -- ** ReplicaAutoScalingUpdate
    replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate,
    replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates,
    replicaAutoScalingUpdate_regionName,

    -- ** ReplicaDescription
    replicaDescription_kmsMasterKeyId,
    replicaDescription_replicaInaccessibleDateTime,
    replicaDescription_provisionedThroughputOverride,
    replicaDescription_regionName,
    replicaDescription_replicaStatusPercentProgress,
    replicaDescription_replicaStatusDescription,
    replicaDescription_globalSecondaryIndexes,
    replicaDescription_replicaStatus,

    -- ** ReplicaGlobalSecondaryIndex
    replicaGlobalSecondaryIndex_provisionedThroughputOverride,
    replicaGlobalSecondaryIndex_indexName,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingDescription
    replicaGlobalSecondaryIndexAutoScalingDescription_indexName,
    replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingUpdate
    replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate,
    replicaGlobalSecondaryIndexAutoScalingUpdate_indexName,

    -- ** ReplicaGlobalSecondaryIndexDescription
    replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride,
    replicaGlobalSecondaryIndexDescription_indexName,

    -- ** ReplicaGlobalSecondaryIndexSettingsDescription
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits,
    replicaGlobalSecondaryIndexSettingsDescription_indexStatus,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_indexName,

    -- ** ReplicaGlobalSecondaryIndexSettingsUpdate
    replicaGlobalSecondaryIndexSettingsUpdate_provisionedReadCapacityUnits,
    replicaGlobalSecondaryIndexSettingsUpdate_provisionedReadCapacityAutoScalingSettingsUpdate,
    replicaGlobalSecondaryIndexSettingsUpdate_indexName,

    -- ** ReplicaSettingsDescription
    replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings,
    replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaSettingsDescription_replicaGlobalSecondaryIndexSettings,
    replicaSettingsDescription_replicaBillingModeSummary,
    replicaSettingsDescription_replicaProvisionedWriteCapacityUnits,
    replicaSettingsDescription_replicaProvisionedReadCapacityUnits,
    replicaSettingsDescription_replicaStatus,
    replicaSettingsDescription_regionName,

    -- ** ReplicaSettingsUpdate
    replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate,
    replicaSettingsUpdate_replicaProvisionedReadCapacityUnits,
    replicaSettingsUpdate_regionName,

    -- ** ReplicaUpdate
    replicaUpdate_create,
    replicaUpdate_delete,

    -- ** ReplicationGroupUpdate
    replicationGroupUpdate_create,
    replicationGroupUpdate_delete,
    replicationGroupUpdate_update,

    -- ** RestoreSummary
    restoreSummary_sourceBackupArn,
    restoreSummary_sourceTableArn,
    restoreSummary_restoreDateTime,
    restoreSummary_restoreInProgress,

    -- ** SSEDescription
    sSEDescription_inaccessibleEncryptionDateTime,
    sSEDescription_status,
    sSEDescription_sSEType,
    sSEDescription_kmsMasterKeyArn,

    -- ** SSESpecification
    sSESpecification_kmsMasterKeyId,
    sSESpecification_enabled,
    sSESpecification_sSEType,

    -- ** SourceTableDetails
    sourceTableDetails_tableArn,
    sourceTableDetails_tableSizeBytes,
    sourceTableDetails_billingMode,
    sourceTableDetails_itemCount,
    sourceTableDetails_tableName,
    sourceTableDetails_tableId,
    sourceTableDetails_keySchema,
    sourceTableDetails_tableCreationDateTime,
    sourceTableDetails_provisionedThroughput,

    -- ** SourceTableFeatureDetails
    sourceTableFeatureDetails_localSecondaryIndexes,
    sourceTableFeatureDetails_timeToLiveDescription,
    sourceTableFeatureDetails_streamDescription,
    sourceTableFeatureDetails_globalSecondaryIndexes,
    sourceTableFeatureDetails_sSEDescription,

    -- ** StreamSpecification
    streamSpecification_streamViewType,
    streamSpecification_streamEnabled,

    -- ** TableAutoScalingDescription
    tableAutoScalingDescription_tableName,
    tableAutoScalingDescription_tableStatus,
    tableAutoScalingDescription_replicas,

    -- ** TableDescription
    tableDescription_tableName,
    tableDescription_latestStreamLabel,
    tableDescription_billingModeSummary,
    tableDescription_tableStatus,
    tableDescription_archivalSummary,
    tableDescription_tableArn,
    tableDescription_localSecondaryIndexes,
    tableDescription_tableSizeBytes,
    tableDescription_creationDateTime,
    tableDescription_replicas,
    tableDescription_itemCount,
    tableDescription_provisionedThroughput,
    tableDescription_latestStreamArn,
    tableDescription_tableId,
    tableDescription_keySchema,
    tableDescription_restoreSummary,
    tableDescription_globalSecondaryIndexes,
    tableDescription_streamSpecification,
    tableDescription_globalTableVersion,
    tableDescription_sSEDescription,
    tableDescription_attributeDefinitions,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TimeToLiveDescription
    timeToLiveDescription_timeToLiveStatus,
    timeToLiveDescription_attributeName,

    -- ** TimeToLiveSpecification
    timeToLiveSpecification_enabled,
    timeToLiveSpecification_attributeName,

    -- ** TransactGetItem
    transactGetItem_get,

    -- ** TransactWriteItem
    transactWriteItem_conditionCheck,
    transactWriteItem_delete,
    transactWriteItem_put,
    transactWriteItem_update,

    -- ** Update
    update_expressionAttributeValues,
    update_expressionAttributeNames,
    update_returnValuesOnConditionCheckFailure,
    update_conditionExpression,
    update_key,
    update_updateExpression,
    update_tableName,

    -- ** UpdateGlobalSecondaryIndexAction
    updateGlobalSecondaryIndexAction_indexName,
    updateGlobalSecondaryIndexAction_provisionedThroughput,

    -- ** UpdateReplicationGroupMemberAction
    updateReplicationGroupMemberAction_kmsMasterKeyId,
    updateReplicationGroupMemberAction_provisionedThroughputOverride,
    updateReplicationGroupMemberAction_globalSecondaryIndexes,
    updateReplicationGroupMemberAction_regionName,
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
import Amazonka.DynamoDB.Types.ArchivalSummary
import Amazonka.DynamoDB.Types.AttributeDefinition
import Amazonka.DynamoDB.Types.AttributeValueUpdate
import Amazonka.DynamoDB.Types.AutoScalingPolicyDescription
import Amazonka.DynamoDB.Types.AutoScalingPolicyUpdate
import Amazonka.DynamoDB.Types.AutoScalingSettingsDescription
import Amazonka.DynamoDB.Types.AutoScalingSettingsUpdate
import Amazonka.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
import Amazonka.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
import Amazonka.DynamoDB.Types.BackupDescription
import Amazonka.DynamoDB.Types.BackupDetails
import Amazonka.DynamoDB.Types.BackupSummary
import Amazonka.DynamoDB.Types.BatchStatementError
import Amazonka.DynamoDB.Types.BatchStatementRequest
import Amazonka.DynamoDB.Types.BatchStatementResponse
import Amazonka.DynamoDB.Types.BillingModeSummary
import Amazonka.DynamoDB.Types.Capacity
import Amazonka.DynamoDB.Types.Condition
import Amazonka.DynamoDB.Types.ConditionCheck
import Amazonka.DynamoDB.Types.ConsumedCapacity
import Amazonka.DynamoDB.Types.ContinuousBackupsDescription
import Amazonka.DynamoDB.Types.ContributorInsightsSummary
import Amazonka.DynamoDB.Types.CreateGlobalSecondaryIndexAction
import Amazonka.DynamoDB.Types.CreateReplicaAction
import Amazonka.DynamoDB.Types.CreateReplicationGroupMemberAction
import Amazonka.DynamoDB.Types.Delete
import Amazonka.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
import Amazonka.DynamoDB.Types.DeleteReplicaAction
import Amazonka.DynamoDB.Types.DeleteReplicationGroupMemberAction
import Amazonka.DynamoDB.Types.Endpoint
import Amazonka.DynamoDB.Types.ExpectedAttributeValue
import Amazonka.DynamoDB.Types.ExportDescription
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
import Amazonka.DynamoDB.Types.ItemCollectionMetrics
import Amazonka.DynamoDB.Types.ItemResponse
import Amazonka.DynamoDB.Types.KeySchemaElement
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
import Amazonka.DynamoDB.Types.Projection
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
import Amazonka.DynamoDB.Types.ReplicaUpdate
import Amazonka.DynamoDB.Types.ReplicationGroupUpdate
import Amazonka.DynamoDB.Types.RestoreSummary
import Amazonka.DynamoDB.Types.SSEDescription
import Amazonka.DynamoDB.Types.SSESpecification
import Amazonka.DynamoDB.Types.SourceTableDetails
import Amazonka.DynamoDB.Types.SourceTableFeatureDetails
import Amazonka.DynamoDB.Types.StreamSpecification
import Amazonka.DynamoDB.Types.TableAutoScalingDescription
import Amazonka.DynamoDB.Types.TableDescription
import Amazonka.DynamoDB.Types.Tag
import Amazonka.DynamoDB.Types.TimeToLiveDescription
import Amazonka.DynamoDB.Types.TimeToLiveSpecification
import Amazonka.DynamoDB.Types.TransactGetItem
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.Update
import Amazonka.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
import Amazonka.DynamoDB.Types.UpdateReplicationGroupMemberAction
import Amazonka.DynamoDB.UntagResource
import Amazonka.DynamoDB.UpdateContinuousBackups
import Amazonka.DynamoDB.UpdateContributorInsights
import Amazonka.DynamoDB.UpdateGlobalTable
import Amazonka.DynamoDB.UpdateGlobalTableSettings
import Amazonka.DynamoDB.UpdateItem
import Amazonka.DynamoDB.UpdateTable
import Amazonka.DynamoDB.UpdateTableReplicaAutoScaling
import Amazonka.DynamoDB.UpdateTimeToLive
