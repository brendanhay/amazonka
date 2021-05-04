{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Lens
  ( -- * Operations

    -- ** BatchGetItem
    batchGetItem_returnConsumedCapacity,
    batchGetItem_requestItems,
    batchGetItemResponse_unprocessedKeys,
    batchGetItemResponse_consumedCapacity,
    batchGetItemResponse_responses,
    batchGetItemResponse_httpStatus,

    -- ** UpdateContributorInsights
    updateContributorInsights_indexName,
    updateContributorInsights_tableName,
    updateContributorInsights_contributorInsightsAction,
    updateContributorInsightsResponse_tableName,
    updateContributorInsightsResponse_indexName,
    updateContributorInsightsResponse_contributorInsightsStatus,
    updateContributorInsightsResponse_httpStatus,

    -- ** DeleteBackup
    deleteBackup_backupArn,
    deleteBackupResponse_backupDescription,
    deleteBackupResponse_httpStatus,

    -- ** DeleteItem
    deleteItem_expected,
    deleteItem_expressionAttributeValues,
    deleteItem_returnItemCollectionMetrics,
    deleteItem_expressionAttributeNames,
    deleteItem_returnValues,
    deleteItem_conditionExpression,
    deleteItem_returnConsumedCapacity,
    deleteItem_conditionalOperator,
    deleteItem_tableName,
    deleteItem_key,
    deleteItemResponse_itemCollectionMetrics,
    deleteItemResponse_attributes,
    deleteItemResponse_consumedCapacity,
    deleteItemResponse_httpStatus,

    -- ** UpdateItem
    updateItem_expected,
    updateItem_expressionAttributeValues,
    updateItem_returnItemCollectionMetrics,
    updateItem_updateExpression,
    updateItem_expressionAttributeNames,
    updateItem_returnValues,
    updateItem_conditionExpression,
    updateItem_attributeUpdates,
    updateItem_returnConsumedCapacity,
    updateItem_conditionalOperator,
    updateItem_tableName,
    updateItem_key,
    updateItemResponse_itemCollectionMetrics,
    updateItemResponse_attributes,
    updateItemResponse_consumedCapacity,
    updateItemResponse_httpStatus,

    -- ** ListContributorInsights
    listContributorInsights_nextToken,
    listContributorInsights_tableName,
    listContributorInsights_maxResults,
    listContributorInsightsResponse_contributorInsightsSummaries,
    listContributorInsightsResponse_nextToken,
    listContributorInsightsResponse_httpStatus,

    -- ** ListGlobalTables
    listGlobalTables_regionName,
    listGlobalTables_exclusiveStartGlobalTableName,
    listGlobalTables_limit,
    listGlobalTablesResponse_lastEvaluatedGlobalTableName,
    listGlobalTablesResponse_globalTables,
    listGlobalTablesResponse_httpStatus,

    -- ** DisableKinesisStreamingDestination
    disableKinesisStreamingDestination_tableName,
    disableKinesisStreamingDestination_streamArn,
    kinesisStreamingDestinationOutput_tableName,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_destinationStatus,

    -- ** UpdateContinuousBackups
    updateContinuousBackups_tableName,
    updateContinuousBackups_pointInTimeRecoverySpecification,
    updateContinuousBackupsResponse_continuousBackupsDescription,
    updateContinuousBackupsResponse_httpStatus,

    -- ** CreateGlobalTable
    createGlobalTable_globalTableName,
    createGlobalTable_replicationGroup,
    createGlobalTableResponse_globalTableDescription,
    createGlobalTableResponse_httpStatus,

    -- ** BatchExecuteStatement
    batchExecuteStatement_statements,
    batchExecuteStatementResponse_responses,
    batchExecuteStatementResponse_httpStatus,

    -- ** RestoreTableFromBackup
    restoreTableFromBackup_provisionedThroughputOverride,
    restoreTableFromBackup_globalSecondaryIndexOverride,
    restoreTableFromBackup_billingModeOverride,
    restoreTableFromBackup_sSESpecificationOverride,
    restoreTableFromBackup_localSecondaryIndexOverride,
    restoreTableFromBackup_targetTableName,
    restoreTableFromBackup_backupArn,
    restoreTableFromBackupResponse_tableDescription,
    restoreTableFromBackupResponse_httpStatus,

    -- ** DescribeLimits
    describeLimitsResponse_accountMaxWriteCapacityUnits,
    describeLimitsResponse_tableMaxReadCapacityUnits,
    describeLimitsResponse_accountMaxReadCapacityUnits,
    describeLimitsResponse_tableMaxWriteCapacityUnits,
    describeLimitsResponse_httpStatus,

    -- ** ExecuteTransaction
    executeTransaction_clientRequestToken,
    executeTransaction_transactStatements,
    executeTransactionResponse_responses,
    executeTransactionResponse_httpStatus,

    -- ** RestoreTableToPointInTime
    restoreTableToPointInTime_sourceTableName,
    restoreTableToPointInTime_restoreDateTime,
    restoreTableToPointInTime_provisionedThroughputOverride,
    restoreTableToPointInTime_globalSecondaryIndexOverride,
    restoreTableToPointInTime_billingModeOverride,
    restoreTableToPointInTime_sSESpecificationOverride,
    restoreTableToPointInTime_sourceTableArn,
    restoreTableToPointInTime_localSecondaryIndexOverride,
    restoreTableToPointInTime_useLatestRestorableTime,
    restoreTableToPointInTime_targetTableName,
    restoreTableToPointInTimeResponse_tableDescription,
    restoreTableToPointInTimeResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** DescribeContributorInsights
    describeContributorInsights_indexName,
    describeContributorInsights_tableName,
    describeContributorInsightsResponse_tableName,
    describeContributorInsightsResponse_indexName,
    describeContributorInsightsResponse_contributorInsightsStatus,
    describeContributorInsightsResponse_failureException,
    describeContributorInsightsResponse_contributorInsightsRuleList,
    describeContributorInsightsResponse_lastUpdateDateTime,
    describeContributorInsightsResponse_httpStatus,

    -- ** DescribeBackup
    describeBackup_backupArn,
    describeBackupResponse_backupDescription,
    describeBackupResponse_httpStatus,

    -- ** ListTagsOfResource
    listTagsOfResource_nextToken,
    listTagsOfResource_resourceArn,
    listTagsOfResourceResponse_nextToken,
    listTagsOfResourceResponse_tags,
    listTagsOfResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** DescribeGlobalTableSettings
    describeGlobalTableSettings_globalTableName,
    describeGlobalTableSettingsResponse_replicaSettings,
    describeGlobalTableSettingsResponse_globalTableName,
    describeGlobalTableSettingsResponse_httpStatus,

    -- ** UpdateTableReplicaAutoScaling
    updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate,
    updateTableReplicaAutoScaling_globalSecondaryIndexUpdates,
    updateTableReplicaAutoScaling_replicaUpdates,
    updateTableReplicaAutoScaling_tableName,
    updateTableReplicaAutoScalingResponse_tableAutoScalingDescription,
    updateTableReplicaAutoScalingResponse_httpStatus,

    -- ** DescribeTimeToLive
    describeTimeToLive_tableName,
    describeTimeToLiveResponse_timeToLiveDescription,
    describeTimeToLiveResponse_httpStatus,

    -- ** Query
    query_projectionExpression,
    query_exclusiveStartKey,
    query_indexName,
    query_expressionAttributeValues,
    query_keyConditionExpression,
    query_consistentRead,
    query_expressionAttributeNames,
    query_filterExpression,
    query_keyConditions,
    query_returnConsumedCapacity,
    query_conditionalOperator,
    query_select,
    query_limit,
    query_attributesToGet,
    query_scanIndexForward,
    query_queryFilter,
    query_tableName,
    queryResponse_items,
    queryResponse_scannedCount,
    queryResponse_lastEvaluatedKey,
    queryResponse_consumedCapacity,
    queryResponse_count,
    queryResponse_httpStatus,

    -- ** CreateTable
    createTable_localSecondaryIndexes,
    createTable_streamSpecification,
    createTable_globalSecondaryIndexes,
    createTable_sSESpecification,
    createTable_billingMode,
    createTable_tags,
    createTable_provisionedThroughput,
    createTable_attributeDefinitions,
    createTable_tableName,
    createTable_keySchema,
    createTableResponse_tableDescription,
    createTableResponse_httpStatus,

    -- ** CreateBackup
    createBackup_tableName,
    createBackup_backupName,
    createBackupResponse_backupDetails,
    createBackupResponse_httpStatus,

    -- ** ListTables
    listTables_exclusiveStartTableName,
    listTables_limit,
    listTablesResponse_lastEvaluatedTableName,
    listTablesResponse_tableNames,
    listTablesResponse_httpStatus,

    -- ** Scan
    scan_scanFilter,
    scan_projectionExpression,
    scan_exclusiveStartKey,
    scan_indexName,
    scan_expressionAttributeValues,
    scan_segment,
    scan_consistentRead,
    scan_expressionAttributeNames,
    scan_filterExpression,
    scan_returnConsumedCapacity,
    scan_conditionalOperator,
    scan_select,
    scan_limit,
    scan_attributesToGet,
    scan_totalSegments,
    scan_tableName,
    scanResponse_items,
    scanResponse_scannedCount,
    scanResponse_lastEvaluatedKey,
    scanResponse_consumedCapacity,
    scanResponse_count,
    scanResponse_httpStatus,

    -- ** UpdateTable
    updateTable_streamSpecification,
    updateTable_sSESpecification,
    updateTable_billingMode,
    updateTable_attributeDefinitions,
    updateTable_globalSecondaryIndexUpdates,
    updateTable_provisionedThroughput,
    updateTable_replicaUpdates,
    updateTable_tableName,
    updateTableResponse_tableDescription,
    updateTableResponse_httpStatus,

    -- ** DeleteTable
    deleteTable_tableName,
    deleteTableResponse_tableDescription,
    deleteTableResponse_httpStatus,

    -- ** TransactWriteItems
    transactWriteItems_returnItemCollectionMetrics,
    transactWriteItems_returnConsumedCapacity,
    transactWriteItems_clientRequestToken,
    transactWriteItems_transactItems,
    transactWriteItemsResponse_itemCollectionMetrics,
    transactWriteItemsResponse_consumedCapacity,
    transactWriteItemsResponse_httpStatus,

    -- ** ExportTableToPointInTime
    exportTableToPointInTime_exportFormat,
    exportTableToPointInTime_exportTime,
    exportTableToPointInTime_s3BucketOwner,
    exportTableToPointInTime_s3Prefix,
    exportTableToPointInTime_s3SseKmsKeyId,
    exportTableToPointInTime_clientToken,
    exportTableToPointInTime_s3SseAlgorithm,
    exportTableToPointInTime_tableArn,
    exportTableToPointInTime_s3Bucket,
    exportTableToPointInTimeResponse_exportDescription,
    exportTableToPointInTimeResponse_httpStatus,

    -- ** ListBackups
    listBackups_tableName,
    listBackups_backupType,
    listBackups_timeRangeLowerBound,
    listBackups_limit,
    listBackups_exclusiveStartBackupArn,
    listBackups_timeRangeUpperBound,
    listBackupsResponse_lastEvaluatedBackupArn,
    listBackupsResponse_backupSummaries,
    listBackupsResponse_httpStatus,

    -- ** TransactGetItems
    transactGetItems_returnConsumedCapacity,
    transactGetItems_transactItems,
    transactGetItemsResponse_consumedCapacity,
    transactGetItemsResponse_responses,
    transactGetItemsResponse_httpStatus,

    -- ** UpdateGlobalTable
    updateGlobalTable_globalTableName,
    updateGlobalTable_replicaUpdates,
    updateGlobalTableResponse_globalTableDescription,
    updateGlobalTableResponse_httpStatus,

    -- ** BatchWriteItem
    batchWriteItem_returnItemCollectionMetrics,
    batchWriteItem_returnConsumedCapacity,
    batchWriteItem_requestItems,
    batchWriteItemResponse_itemCollectionMetrics,
    batchWriteItemResponse_unprocessedItems,
    batchWriteItemResponse_consumedCapacity,
    batchWriteItemResponse_httpStatus,

    -- ** PutItem
    putItem_expected,
    putItem_expressionAttributeValues,
    putItem_returnItemCollectionMetrics,
    putItem_expressionAttributeNames,
    putItem_returnValues,
    putItem_conditionExpression,
    putItem_returnConsumedCapacity,
    putItem_conditionalOperator,
    putItem_tableName,
    putItem_item,
    putItemResponse_itemCollectionMetrics,
    putItemResponse_attributes,
    putItemResponse_consumedCapacity,
    putItemResponse_httpStatus,

    -- ** UpdateTimeToLive
    updateTimeToLive_tableName,
    updateTimeToLive_timeToLiveSpecification,
    updateTimeToLiveResponse_timeToLiveSpecification,
    updateTimeToLiveResponse_httpStatus,

    -- ** UpdateGlobalTableSettings
    updateGlobalTableSettings_replicaSettingsUpdate,
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits,
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
    updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate,
    updateGlobalTableSettings_globalTableBillingMode,
    updateGlobalTableSettings_globalTableName,
    updateGlobalTableSettingsResponse_replicaSettings,
    updateGlobalTableSettingsResponse_globalTableName,
    updateGlobalTableSettingsResponse_httpStatus,

    -- ** EnableKinesisStreamingDestination
    enableKinesisStreamingDestination_tableName,
    enableKinesisStreamingDestination_streamArn,
    kinesisStreamingDestinationOutput_tableName,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_destinationStatus,

    -- ** DescribeExport
    describeExport_exportArn,
    describeExportResponse_exportDescription,
    describeExportResponse_httpStatus,

    -- ** DescribeTableReplicaAutoScaling
    describeTableReplicaAutoScaling_tableName,
    describeTableReplicaAutoScalingResponse_tableAutoScalingDescription,
    describeTableReplicaAutoScalingResponse_httpStatus,

    -- ** GetItem
    getItem_projectionExpression,
    getItem_consistentRead,
    getItem_expressionAttributeNames,
    getItem_returnConsumedCapacity,
    getItem_attributesToGet,
    getItem_tableName,
    getItem_key,
    getItemResponse_item,
    getItemResponse_consumedCapacity,
    getItemResponse_httpStatus,

    -- ** DescribeTable
    describeTable_tableName,
    describeTableResponse_table,
    describeTableResponse_httpStatus,

    -- ** DescribeGlobalTable
    describeGlobalTable_globalTableName,
    describeGlobalTableResponse_globalTableDescription,
    describeGlobalTableResponse_httpStatus,

    -- ** ListExports
    listExports_nextToken,
    listExports_maxResults,
    listExports_tableArn,
    listExportsResponse_nextToken,
    listExportsResponse_exportSummaries,
    listExportsResponse_httpStatus,

    -- ** DescribeContinuousBackups
    describeContinuousBackups_tableName,
    describeContinuousBackupsResponse_continuousBackupsDescription,
    describeContinuousBackupsResponse_httpStatus,

    -- ** DescribeKinesisStreamingDestination
    describeKinesisStreamingDestination_tableName,
    describeKinesisStreamingDestinationResponse_tableName,
    describeKinesisStreamingDestinationResponse_kinesisDataStreamDestinations,
    describeKinesisStreamingDestinationResponse_httpStatus,

    -- ** DescribeEndpoints
    describeEndpointsResponse_httpStatus,
    describeEndpointsResponse_endpoints,

    -- ** ExecuteStatement
    executeStatement_nextToken,
    executeStatement_consistentRead,
    executeStatement_parameters,
    executeStatement_statement,
    executeStatementResponse_nextToken,
    executeStatementResponse_items,
    executeStatementResponse_httpStatus,

    -- * Types

    -- ** ArchivalSummary
    archivalSummary_archivalBackupArn,
    archivalSummary_archivalReason,
    archivalSummary_archivalDateTime,

    -- ** AttributeDefinition
    attributeDefinition_attributeName,
    attributeDefinition_attributeType,

    -- ** AttributeValue
    attributeValue_bs,
    attributeValue_bool,
    attributeValue_n,
    attributeValue_s,
    attributeValue_null,
    attributeValue_m,
    attributeValue_b,
    attributeValue_l,
    attributeValue_ss,
    attributeValue_ns,

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
    autoScalingSettingsDescription_scalingPolicies,
    autoScalingSettingsDescription_minimumUnits,
    autoScalingSettingsDescription_maximumUnits,
    autoScalingSettingsDescription_autoScalingRoleArn,
    autoScalingSettingsDescription_autoScalingDisabled,

    -- ** AutoScalingSettingsUpdate
    autoScalingSettingsUpdate_scalingPolicyUpdate,
    autoScalingSettingsUpdate_minimumUnits,
    autoScalingSettingsUpdate_maximumUnits,
    autoScalingSettingsUpdate_autoScalingRoleArn,
    autoScalingSettingsUpdate_autoScalingDisabled,

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_disableScaleIn,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleOutCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleInCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_targetValue,

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_disableScaleIn,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleOutCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleInCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_targetValue,

    -- ** BackupDescription
    backupDescription_sourceTableDetails,
    backupDescription_backupDetails,
    backupDescription_sourceTableFeatureDetails,

    -- ** BackupDetails
    backupDetails_backupExpiryDateTime,
    backupDetails_backupSizeBytes,
    backupDetails_backupArn,
    backupDetails_backupName,
    backupDetails_backupStatus,
    backupDetails_backupType,
    backupDetails_backupCreationDateTime,

    -- ** BackupSummary
    backupSummary_tableName,
    backupSummary_backupName,
    backupSummary_backupType,
    backupSummary_backupCreationDateTime,
    backupSummary_tableArn,
    backupSummary_tableId,
    backupSummary_backupArn,
    backupSummary_backupExpiryDateTime,
    backupSummary_backupSizeBytes,
    backupSummary_backupStatus,

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
    capacity_writeCapacityUnits,
    capacity_capacityUnits,
    capacity_readCapacityUnits,

    -- ** Condition
    condition_attributeValueList,
    condition_comparisonOperator,

    -- ** ConditionCheck
    conditionCheck_expressionAttributeValues,
    conditionCheck_returnValuesOnConditionCheckFailure,
    conditionCheck_expressionAttributeNames,
    conditionCheck_key,
    conditionCheck_tableName,
    conditionCheck_conditionExpression,

    -- ** ConsumedCapacity
    consumedCapacity_localSecondaryIndexes,
    consumedCapacity_globalSecondaryIndexes,
    consumedCapacity_tableName,
    consumedCapacity_writeCapacityUnits,
    consumedCapacity_capacityUnits,
    consumedCapacity_table,
    consumedCapacity_readCapacityUnits,

    -- ** ContinuousBackupsDescription
    continuousBackupsDescription_pointInTimeRecoveryDescription,
    continuousBackupsDescription_continuousBackupsStatus,

    -- ** ContributorInsightsSummary
    contributorInsightsSummary_tableName,
    contributorInsightsSummary_indexName,
    contributorInsightsSummary_contributorInsightsStatus,

    -- ** CreateGlobalSecondaryIndexAction
    createGlobalSecondaryIndexAction_provisionedThroughput,
    createGlobalSecondaryIndexAction_indexName,
    createGlobalSecondaryIndexAction_keySchema,
    createGlobalSecondaryIndexAction_projection,

    -- ** CreateReplicaAction
    createReplicaAction_regionName,

    -- ** CreateReplicationGroupMemberAction
    createReplicationGroupMemberAction_globalSecondaryIndexes,
    createReplicationGroupMemberAction_provisionedThroughputOverride,
    createReplicationGroupMemberAction_kmsMasterKeyId,
    createReplicationGroupMemberAction_regionName,

    -- ** Delete
    delete_expressionAttributeValues,
    delete_returnValuesOnConditionCheckFailure,
    delete_expressionAttributeNames,
    delete_conditionExpression,
    delete_key,
    delete_tableName,

    -- ** DeleteGlobalSecondaryIndexAction
    deleteGlobalSecondaryIndexAction_indexName,

    -- ** DeleteReplicaAction
    deleteReplicaAction_regionName,

    -- ** DeleteReplicationGroupMemberAction
    deleteReplicationGroupMemberAction_regionName,

    -- ** DeleteRequest
    deleteRequest_key,

    -- ** Endpoint
    endpoint_address,
    endpoint_cachePeriodInMinutes,

    -- ** ExpectedAttributeValue
    expectedAttributeValue_comparisonOperator,
    expectedAttributeValue_exists,
    expectedAttributeValue_value,
    expectedAttributeValue_attributeValueList,

    -- ** ExportDescription
    exportDescription_exportFormat,
    exportDescription_exportTime,
    exportDescription_billedSizeBytes,
    exportDescription_s3Bucket,
    exportDescription_tableArn,
    exportDescription_tableId,
    exportDescription_failureMessage,
    exportDescription_exportStatus,
    exportDescription_startTime,
    exportDescription_failureCode,
    exportDescription_endTime,
    exportDescription_s3BucketOwner,
    exportDescription_exportArn,
    exportDescription_itemCount,
    exportDescription_exportManifest,
    exportDescription_s3Prefix,
    exportDescription_s3SseKmsKeyId,
    exportDescription_clientToken,
    exportDescription_s3SseAlgorithm,

    -- ** ExportSummary
    exportSummary_exportStatus,
    exportSummary_exportArn,

    -- ** FailureException
    failureException_exceptionName,
    failureException_exceptionDescription,

    -- ** Get
    get_projectionExpression,
    get_expressionAttributeNames,
    get_key,
    get_tableName,

    -- ** GlobalSecondaryIndex
    globalSecondaryIndex_provisionedThroughput,
    globalSecondaryIndex_indexName,
    globalSecondaryIndex_keySchema,
    globalSecondaryIndex_projection,

    -- ** GlobalSecondaryIndexAutoScalingUpdate
    globalSecondaryIndexAutoScalingUpdate_indexName,
    globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate,

    -- ** GlobalSecondaryIndexDescription
    globalSecondaryIndexDescription_indexName,
    globalSecondaryIndexDescription_keySchema,
    globalSecondaryIndexDescription_indexArn,
    globalSecondaryIndexDescription_projection,
    globalSecondaryIndexDescription_indexSizeBytes,
    globalSecondaryIndexDescription_backfilling,
    globalSecondaryIndexDescription_itemCount,
    globalSecondaryIndexDescription_provisionedThroughput,
    globalSecondaryIndexDescription_indexStatus,

    -- ** GlobalSecondaryIndexInfo
    globalSecondaryIndexInfo_indexName,
    globalSecondaryIndexInfo_keySchema,
    globalSecondaryIndexInfo_projection,
    globalSecondaryIndexInfo_provisionedThroughput,

    -- ** GlobalSecondaryIndexUpdate
    globalSecondaryIndexUpdate_create,
    globalSecondaryIndexUpdate_update,
    globalSecondaryIndexUpdate_delete,

    -- ** GlobalTable
    globalTable_globalTableName,
    globalTable_replicationGroup,

    -- ** GlobalTableDescription
    globalTableDescription_globalTableName,
    globalTableDescription_globalTableStatus,
    globalTableDescription_replicationGroup,
    globalTableDescription_creationDateTime,
    globalTableDescription_globalTableArn,

    -- ** GlobalTableGlobalSecondaryIndexSettingsUpdate
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityAutoScalingSettingsUpdate,
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityUnits,
    globalTableGlobalSecondaryIndexSettingsUpdate_indexName,

    -- ** ItemCollectionMetrics
    itemCollectionMetrics_itemCollectionKey,
    itemCollectionMetrics_sizeEstimateRangeGB,

    -- ** ItemResponse
    itemResponse_item,

    -- ** KeySchemaElement
    keySchemaElement_attributeName,
    keySchemaElement_keyType,

    -- ** KeysAndAttributes
    keysAndAttributes_projectionExpression,
    keysAndAttributes_consistentRead,
    keysAndAttributes_expressionAttributeNames,
    keysAndAttributes_attributesToGet,
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
    localSecondaryIndexDescription_indexName,
    localSecondaryIndexDescription_keySchema,
    localSecondaryIndexDescription_indexArn,
    localSecondaryIndexDescription_projection,
    localSecondaryIndexDescription_indexSizeBytes,
    localSecondaryIndexDescription_itemCount,

    -- ** LocalSecondaryIndexInfo
    localSecondaryIndexInfo_indexName,
    localSecondaryIndexInfo_keySchema,
    localSecondaryIndexInfo_projection,

    -- ** ParameterizedStatement
    parameterizedStatement_parameters,
    parameterizedStatement_statement,

    -- ** PointInTimeRecoveryDescription
    pointInTimeRecoveryDescription_latestRestorableDateTime,
    pointInTimeRecoveryDescription_earliestRestorableDateTime,
    pointInTimeRecoveryDescription_pointInTimeRecoveryStatus,

    -- ** PointInTimeRecoverySpecification
    pointInTimeRecoverySpecification_pointInTimeRecoveryEnabled,

    -- ** Projection
    projection_nonKeyAttributes,
    projection_projectionType,

    -- ** ProvisionedThroughput
    provisionedThroughput_readCapacityUnits,
    provisionedThroughput_writeCapacityUnits,

    -- ** ProvisionedThroughputDescription
    provisionedThroughputDescription_lastDecreaseDateTime,
    provisionedThroughputDescription_lastIncreaseDateTime,
    provisionedThroughputDescription_writeCapacityUnits,
    provisionedThroughputDescription_numberOfDecreasesToday,
    provisionedThroughputDescription_readCapacityUnits,

    -- ** ProvisionedThroughputOverride
    provisionedThroughputOverride_readCapacityUnits,

    -- ** Put
    put_expressionAttributeValues,
    put_returnValuesOnConditionCheckFailure,
    put_expressionAttributeNames,
    put_conditionExpression,
    put_item,
    put_tableName,

    -- ** PutRequest
    putRequest_item,

    -- ** Replica
    replica_regionName,

    -- ** ReplicaAutoScalingDescription
    replicaAutoScalingDescription_regionName,
    replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings,
    replicaAutoScalingDescription_globalSecondaryIndexes,
    replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaAutoScalingDescription_replicaStatus,

    -- ** ReplicaAutoScalingUpdate
    replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate,
    replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates,
    replicaAutoScalingUpdate_regionName,

    -- ** ReplicaDescription
    replicaDescription_regionName,
    replicaDescription_globalSecondaryIndexes,
    replicaDescription_provisionedThroughputOverride,
    replicaDescription_kmsMasterKeyId,
    replicaDescription_replicaStatusDescription,
    replicaDescription_replicaStatusPercentProgress,
    replicaDescription_replicaStatus,
    replicaDescription_replicaInaccessibleDateTime,

    -- ** ReplicaGlobalSecondaryIndex
    replicaGlobalSecondaryIndex_provisionedThroughputOverride,
    replicaGlobalSecondaryIndex_indexName,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingDescription
    replicaGlobalSecondaryIndexAutoScalingDescription_indexName,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingUpdate
    replicaGlobalSecondaryIndexAutoScalingUpdate_indexName,
    replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate,

    -- ** ReplicaGlobalSecondaryIndexDescription
    replicaGlobalSecondaryIndexDescription_indexName,
    replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride,

    -- ** ReplicaGlobalSecondaryIndexSettingsDescription
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_indexStatus,
    replicaGlobalSecondaryIndexSettingsDescription_indexName,

    -- ** ReplicaGlobalSecondaryIndexSettingsUpdate
    replicaGlobalSecondaryIndexSettingsUpdate_provisionedReadCapacityUnits,
    replicaGlobalSecondaryIndexSettingsUpdate_provisionedReadCapacityAutoScalingSettingsUpdate,
    replicaGlobalSecondaryIndexSettingsUpdate_indexName,

    -- ** ReplicaSettingsDescription
    replicaSettingsDescription_replicaBillingModeSummary,
    replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings,
    replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaSettingsDescription_replicaGlobalSecondaryIndexSettings,
    replicaSettingsDescription_replicaProvisionedWriteCapacityUnits,
    replicaSettingsDescription_replicaStatus,
    replicaSettingsDescription_replicaProvisionedReadCapacityUnits,
    replicaSettingsDescription_regionName,

    -- ** ReplicaSettingsUpdate
    replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate,
    replicaSettingsUpdate_replicaProvisionedReadCapacityUnits,
    replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    replicaSettingsUpdate_regionName,

    -- ** ReplicaUpdate
    replicaUpdate_create,
    replicaUpdate_delete,

    -- ** ReplicationGroupUpdate
    replicationGroupUpdate_create,
    replicationGroupUpdate_update,
    replicationGroupUpdate_delete,

    -- ** RestoreSummary
    restoreSummary_sourceBackupArn,
    restoreSummary_sourceTableArn,
    restoreSummary_restoreDateTime,
    restoreSummary_restoreInProgress,

    -- ** SSEDescription
    sSEDescription_status,
    sSEDescription_kmsMasterKeyArn,
    sSEDescription_inaccessibleEncryptionDateTime,
    sSEDescription_sSEType,

    -- ** SSESpecification
    sSESpecification_kmsMasterKeyId,
    sSESpecification_enabled,
    sSESpecification_sSEType,

    -- ** SourceTableDetails
    sourceTableDetails_tableArn,
    sourceTableDetails_billingMode,
    sourceTableDetails_tableSizeBytes,
    sourceTableDetails_itemCount,
    sourceTableDetails_tableName,
    sourceTableDetails_tableId,
    sourceTableDetails_keySchema,
    sourceTableDetails_tableCreationDateTime,
    sourceTableDetails_provisionedThroughput,

    -- ** SourceTableFeatureDetails
    sourceTableFeatureDetails_localSecondaryIndexes,
    sourceTableFeatureDetails_globalSecondaryIndexes,
    sourceTableFeatureDetails_timeToLiveDescription,
    sourceTableFeatureDetails_sSEDescription,
    sourceTableFeatureDetails_streamDescription,

    -- ** StreamSpecification
    streamSpecification_streamViewType,
    streamSpecification_streamEnabled,

    -- ** TableAutoScalingDescription
    tableAutoScalingDescription_tableName,
    tableAutoScalingDescription_replicas,
    tableAutoScalingDescription_tableStatus,

    -- ** TableDescription
    tableDescription_globalTableVersion,
    tableDescription_localSecondaryIndexes,
    tableDescription_restoreSummary,
    tableDescription_streamSpecification,
    tableDescription_globalSecondaryIndexes,
    tableDescription_tableName,
    tableDescription_keySchema,
    tableDescription_tableArn,
    tableDescription_tableId,
    tableDescription_attributeDefinitions,
    tableDescription_tableSizeBytes,
    tableDescription_billingModeSummary,
    tableDescription_latestStreamLabel,
    tableDescription_archivalSummary,
    tableDescription_itemCount,
    tableDescription_sSEDescription,
    tableDescription_replicas,
    tableDescription_creationDateTime,
    tableDescription_tableStatus,
    tableDescription_provisionedThroughput,
    tableDescription_latestStreamArn,

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
    transactWriteItem_put,
    transactWriteItem_conditionCheck,
    transactWriteItem_update,
    transactWriteItem_delete,

    -- ** Update
    update_expressionAttributeValues,
    update_returnValuesOnConditionCheckFailure,
    update_expressionAttributeNames,
    update_conditionExpression,
    update_key,
    update_updateExpression,
    update_tableName,

    -- ** UpdateGlobalSecondaryIndexAction
    updateGlobalSecondaryIndexAction_indexName,
    updateGlobalSecondaryIndexAction_provisionedThroughput,

    -- ** UpdateReplicationGroupMemberAction
    updateReplicationGroupMemberAction_globalSecondaryIndexes,
    updateReplicationGroupMemberAction_provisionedThroughputOverride,
    updateReplicationGroupMemberAction_kmsMasterKeyId,
    updateReplicationGroupMemberAction_regionName,

    -- ** WriteRequest
    writeRequest_deleteRequest,
    writeRequest_putRequest,
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
import Network.AWS.DynamoDB.Types.ArchivalSummary
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
import Network.AWS.DynamoDB.Types.BackupSummary
import Network.AWS.DynamoDB.Types.BatchStatementError
import Network.AWS.DynamoDB.Types.BatchStatementRequest
import Network.AWS.DynamoDB.Types.BatchStatementResponse
import Network.AWS.DynamoDB.Types.BillingModeSummary
import Network.AWS.DynamoDB.Types.Capacity
import Network.AWS.DynamoDB.Types.Condition
import Network.AWS.DynamoDB.Types.ConditionCheck
import Network.AWS.DynamoDB.Types.ConsumedCapacity
import Network.AWS.DynamoDB.Types.ContinuousBackupsDescription
import Network.AWS.DynamoDB.Types.ContributorInsightsSummary
import Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.CreateReplicaAction
import Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.Delete
import Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.DeleteReplicaAction
import Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.DeleteRequest
import Network.AWS.DynamoDB.Types.Endpoint
import Network.AWS.DynamoDB.Types.ExpectedAttributeValue
import Network.AWS.DynamoDB.Types.ExportDescription
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
import Network.AWS.DynamoDB.Types.ItemCollectionMetrics
import Network.AWS.DynamoDB.Types.ItemResponse
import Network.AWS.DynamoDB.Types.KeySchemaElement
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
import Network.AWS.DynamoDB.Types.Projection
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
import Network.AWS.DynamoDB.Types.ReplicaUpdate
import Network.AWS.DynamoDB.Types.ReplicationGroupUpdate
import Network.AWS.DynamoDB.Types.RestoreSummary
import Network.AWS.DynamoDB.Types.SSEDescription
import Network.AWS.DynamoDB.Types.SSESpecification
import Network.AWS.DynamoDB.Types.SourceTableDetails
import Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
import Network.AWS.DynamoDB.Types.StreamSpecification
import Network.AWS.DynamoDB.Types.TableAutoScalingDescription
import Network.AWS.DynamoDB.Types.TableDescription
import Network.AWS.DynamoDB.Types.Tag
import Network.AWS.DynamoDB.Types.TimeToLiveDescription
import Network.AWS.DynamoDB.Types.TimeToLiveSpecification
import Network.AWS.DynamoDB.Types.TransactGetItem
import Network.AWS.DynamoDB.Types.TransactWriteItem
import Network.AWS.DynamoDB.Types.Update
import Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.WriteRequest
import Network.AWS.DynamoDB.UntagResource
import Network.AWS.DynamoDB.UpdateContinuousBackups
import Network.AWS.DynamoDB.UpdateContributorInsights
import Network.AWS.DynamoDB.UpdateGlobalTable
import Network.AWS.DynamoDB.UpdateGlobalTableSettings
import Network.AWS.DynamoDB.UpdateItem
import Network.AWS.DynamoDB.UpdateTable
import Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
import Network.AWS.DynamoDB.UpdateTimeToLive
