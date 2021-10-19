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

    -- ** PutItem
    putItem_expressionAttributeNames,
    putItem_returnValues,
    putItem_expressionAttributeValues,
    putItem_returnConsumedCapacity,
    putItem_returnItemCollectionMetrics,
    putItem_conditionExpression,
    putItem_conditionalOperator,
    putItem_expected,
    putItem_tableName,
    putItem_item,
    putItemResponse_itemCollectionMetrics,
    putItemResponse_consumedCapacity,
    putItemResponse_attributes,
    putItemResponse_httpStatus,

    -- ** DeleteItem
    deleteItem_expressionAttributeNames,
    deleteItem_returnValues,
    deleteItem_expressionAttributeValues,
    deleteItem_returnConsumedCapacity,
    deleteItem_returnItemCollectionMetrics,
    deleteItem_conditionExpression,
    deleteItem_conditionalOperator,
    deleteItem_expected,
    deleteItem_tableName,
    deleteItem_key,
    deleteItemResponse_itemCollectionMetrics,
    deleteItemResponse_consumedCapacity,
    deleteItemResponse_attributes,
    deleteItemResponse_httpStatus,

    -- ** UpdateItem
    updateItem_expressionAttributeNames,
    updateItem_returnValues,
    updateItem_updateExpression,
    updateItem_expressionAttributeValues,
    updateItem_attributeUpdates,
    updateItem_returnConsumedCapacity,
    updateItem_returnItemCollectionMetrics,
    updateItem_conditionExpression,
    updateItem_conditionalOperator,
    updateItem_expected,
    updateItem_tableName,
    updateItem_key,
    updateItemResponse_itemCollectionMetrics,
    updateItemResponse_consumedCapacity,
    updateItemResponse_attributes,
    updateItemResponse_httpStatus,

    -- ** DisableKinesisStreamingDestination
    disableKinesisStreamingDestination_tableName,
    disableKinesisStreamingDestination_streamArn,
    kinesisStreamingDestinationOutput_destinationStatus,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_tableName,

    -- ** ListGlobalTables
    listGlobalTables_regionName,
    listGlobalTables_exclusiveStartGlobalTableName,
    listGlobalTables_limit,
    listGlobalTablesResponse_lastEvaluatedGlobalTableName,
    listGlobalTablesResponse_globalTables,
    listGlobalTablesResponse_httpStatus,

    -- ** UpdateGlobalTable
    updateGlobalTable_globalTableName,
    updateGlobalTable_replicaUpdates,
    updateGlobalTableResponse_globalTableDescription,
    updateGlobalTableResponse_httpStatus,

    -- ** DeleteTable
    deleteTable_tableName,
    deleteTableResponse_tableDescription,
    deleteTableResponse_httpStatus,

    -- ** UpdateTable
    updateTable_attributeDefinitions,
    updateTable_provisionedThroughput,
    updateTable_sSESpecification,
    updateTable_replicaUpdates,
    updateTable_globalSecondaryIndexUpdates,
    updateTable_billingMode,
    updateTable_streamSpecification,
    updateTable_tableName,
    updateTableResponse_tableDescription,
    updateTableResponse_httpStatus,

    -- ** BatchGetItem
    batchGetItem_returnConsumedCapacity,
    batchGetItem_requestItems,
    batchGetItemResponse_unprocessedKeys,
    batchGetItemResponse_responses,
    batchGetItemResponse_consumedCapacity,
    batchGetItemResponse_httpStatus,

    -- ** ListBackups
    listBackups_timeRangeUpperBound,
    listBackups_timeRangeLowerBound,
    listBackups_limit,
    listBackups_exclusiveStartBackupArn,
    listBackups_backupType,
    listBackups_tableName,
    listBackupsResponse_backupSummaries,
    listBackupsResponse_lastEvaluatedBackupArn,
    listBackupsResponse_httpStatus,

    -- ** DeleteBackup
    deleteBackup_backupArn,
    deleteBackupResponse_backupDescription,
    deleteBackupResponse_httpStatus,

    -- ** CreateBackup
    createBackup_tableName,
    createBackup_backupName,
    createBackupResponse_backupDetails,
    createBackupResponse_httpStatus,

    -- ** UpdateTableReplicaAutoScaling
    updateTableReplicaAutoScaling_replicaUpdates,
    updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate,
    updateTableReplicaAutoScaling_globalSecondaryIndexUpdates,
    updateTableReplicaAutoScaling_tableName,
    updateTableReplicaAutoScalingResponse_tableAutoScalingDescription,
    updateTableReplicaAutoScalingResponse_httpStatus,

    -- ** DescribeGlobalTableSettings
    describeGlobalTableSettings_globalTableName,
    describeGlobalTableSettingsResponse_replicaSettings,
    describeGlobalTableSettingsResponse_globalTableName,
    describeGlobalTableSettingsResponse_httpStatus,

    -- ** ListTagsOfResource
    listTagsOfResource_nextToken,
    listTagsOfResource_resourceArn,
    listTagsOfResourceResponse_nextToken,
    listTagsOfResourceResponse_tags,
    listTagsOfResourceResponse_httpStatus,

    -- ** DescribeGlobalTable
    describeGlobalTable_globalTableName,
    describeGlobalTableResponse_globalTableDescription,
    describeGlobalTableResponse_httpStatus,

    -- ** DescribeTable
    describeTable_tableName,
    describeTableResponse_table,
    describeTableResponse_httpStatus,

    -- ** DescribeLimits
    describeLimitsResponse_tableMaxWriteCapacityUnits,
    describeLimitsResponse_tableMaxReadCapacityUnits,
    describeLimitsResponse_accountMaxWriteCapacityUnits,
    describeLimitsResponse_accountMaxReadCapacityUnits,
    describeLimitsResponse_httpStatus,

    -- ** ExecuteTransaction
    executeTransaction_clientRequestToken,
    executeTransaction_transactStatements,
    executeTransactionResponse_responses,
    executeTransactionResponse_httpStatus,

    -- ** GetItem
    getItem_projectionExpression,
    getItem_attributesToGet,
    getItem_expressionAttributeNames,
    getItem_consistentRead,
    getItem_returnConsumedCapacity,
    getItem_tableName,
    getItem_key,
    getItemResponse_consumedCapacity,
    getItemResponse_item,
    getItemResponse_httpStatus,

    -- ** DescribeBackup
    describeBackup_backupArn,
    describeBackupResponse_backupDescription,
    describeBackupResponse_httpStatus,

    -- ** BatchExecuteStatement
    batchExecuteStatement_statements,
    batchExecuteStatementResponse_responses,
    batchExecuteStatementResponse_httpStatus,

    -- ** DescribeTableReplicaAutoScaling
    describeTableReplicaAutoScaling_tableName,
    describeTableReplicaAutoScalingResponse_tableAutoScalingDescription,
    describeTableReplicaAutoScalingResponse_httpStatus,

    -- ** UpdateGlobalTableSettings
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
    updateGlobalTableSettings_globalTableBillingMode,
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits,
    updateGlobalTableSettings_replicaSettingsUpdate,
    updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate,
    updateGlobalTableSettings_globalTableName,
    updateGlobalTableSettingsResponse_replicaSettings,
    updateGlobalTableSettingsResponse_globalTableName,
    updateGlobalTableSettingsResponse_httpStatus,

    -- ** EnableKinesisStreamingDestination
    enableKinesisStreamingDestination_tableName,
    enableKinesisStreamingDestination_streamArn,
    kinesisStreamingDestinationOutput_destinationStatus,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_tableName,

    -- ** TransactGetItems
    transactGetItems_returnConsumedCapacity,
    transactGetItems_transactItems,
    transactGetItemsResponse_responses,
    transactGetItemsResponse_consumedCapacity,
    transactGetItemsResponse_httpStatus,

    -- ** ListContributorInsights
    listContributorInsights_nextToken,
    listContributorInsights_maxResults,
    listContributorInsights_tableName,
    listContributorInsightsResponse_contributorInsightsSummaries,
    listContributorInsightsResponse_nextToken,
    listContributorInsightsResponse_httpStatus,

    -- ** BatchWriteItem
    batchWriteItem_returnConsumedCapacity,
    batchWriteItem_returnItemCollectionMetrics,
    batchWriteItem_requestItems,
    batchWriteItemResponse_itemCollectionMetrics,
    batchWriteItemResponse_consumedCapacity,
    batchWriteItemResponse_unprocessedItems,
    batchWriteItemResponse_httpStatus,

    -- ** ExportTableToPointInTime
    exportTableToPointInTime_s3BucketOwner,
    exportTableToPointInTime_exportFormat,
    exportTableToPointInTime_s3SseKmsKeyId,
    exportTableToPointInTime_clientToken,
    exportTableToPointInTime_exportTime,
    exportTableToPointInTime_s3SseAlgorithm,
    exportTableToPointInTime_s3Prefix,
    exportTableToPointInTime_tableArn,
    exportTableToPointInTime_s3Bucket,
    exportTableToPointInTimeResponse_exportDescription,
    exportTableToPointInTimeResponse_httpStatus,

    -- ** TransactWriteItems
    transactWriteItems_returnConsumedCapacity,
    transactWriteItems_returnItemCollectionMetrics,
    transactWriteItems_clientRequestToken,
    transactWriteItems_transactItems,
    transactWriteItemsResponse_itemCollectionMetrics,
    transactWriteItemsResponse_consumedCapacity,
    transactWriteItemsResponse_httpStatus,

    -- ** ListTables
    listTables_exclusiveStartTableName,
    listTables_limit,
    listTablesResponse_lastEvaluatedTableName,
    listTablesResponse_tableNames,
    listTablesResponse_httpStatus,

    -- ** Scan
    scan_projectionExpression,
    scan_scanFilter,
    scan_attributesToGet,
    scan_totalSegments,
    scan_expressionAttributeNames,
    scan_filterExpression,
    scan_consistentRead,
    scan_expressionAttributeValues,
    scan_returnConsumedCapacity,
    scan_limit,
    scan_select,
    scan_segment,
    scan_conditionalOperator,
    scan_exclusiveStartKey,
    scan_indexName,
    scan_tableName,
    scanResponse_lastEvaluatedKey,
    scanResponse_count,
    scanResponse_scannedCount,
    scanResponse_items,
    scanResponse_consumedCapacity,
    scanResponse_httpStatus,

    -- ** UpdateContributorInsights
    updateContributorInsights_indexName,
    updateContributorInsights_tableName,
    updateContributorInsights_contributorInsightsAction,
    updateContributorInsightsResponse_contributorInsightsStatus,
    updateContributorInsightsResponse_tableName,
    updateContributorInsightsResponse_indexName,
    updateContributorInsightsResponse_httpStatus,

    -- ** ExecuteStatement
    executeStatement_consistentRead,
    executeStatement_nextToken,
    executeStatement_parameters,
    executeStatement_statement,
    executeStatementResponse_items,
    executeStatementResponse_nextToken,
    executeStatementResponse_httpStatus,

    -- ** Query
    query_keyConditions,
    query_projectionExpression,
    query_attributesToGet,
    query_expressionAttributeNames,
    query_filterExpression,
    query_queryFilter,
    query_consistentRead,
    query_expressionAttributeValues,
    query_returnConsumedCapacity,
    query_scanIndexForward,
    query_limit,
    query_select,
    query_keyConditionExpression,
    query_conditionalOperator,
    query_exclusiveStartKey,
    query_indexName,
    query_tableName,
    queryResponse_lastEvaluatedKey,
    queryResponse_count,
    queryResponse_scannedCount,
    queryResponse_items,
    queryResponse_consumedCapacity,
    queryResponse_httpStatus,

    -- ** CreateTable
    createTable_provisionedThroughput,
    createTable_sSESpecification,
    createTable_globalSecondaryIndexes,
    createTable_localSecondaryIndexes,
    createTable_billingMode,
    createTable_tags,
    createTable_streamSpecification,
    createTable_attributeDefinitions,
    createTable_tableName,
    createTable_keySchema,
    createTableResponse_tableDescription,
    createTableResponse_httpStatus,

    -- ** DescribeKinesisStreamingDestination
    describeKinesisStreamingDestination_tableName,
    describeKinesisStreamingDestinationResponse_kinesisDataStreamDestinations,
    describeKinesisStreamingDestinationResponse_tableName,
    describeKinesisStreamingDestinationResponse_httpStatus,

    -- ** DescribeEndpoints
    describeEndpointsResponse_httpStatus,
    describeEndpointsResponse_endpoints,

    -- ** DescribeTimeToLive
    describeTimeToLive_tableName,
    describeTimeToLiveResponse_timeToLiveDescription,
    describeTimeToLiveResponse_httpStatus,

    -- ** DescribeContinuousBackups
    describeContinuousBackups_tableName,
    describeContinuousBackupsResponse_continuousBackupsDescription,
    describeContinuousBackupsResponse_httpStatus,

    -- ** ListExports
    listExports_tableArn,
    listExports_nextToken,
    listExports_maxResults,
    listExportsResponse_exportSummaries,
    listExportsResponse_nextToken,
    listExportsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** DescribeContributorInsights
    describeContributorInsights_indexName,
    describeContributorInsights_tableName,
    describeContributorInsightsResponse_contributorInsightsRuleList,
    describeContributorInsightsResponse_failureException,
    describeContributorInsightsResponse_contributorInsightsStatus,
    describeContributorInsightsResponse_lastUpdateDateTime,
    describeContributorInsightsResponse_tableName,
    describeContributorInsightsResponse_indexName,
    describeContributorInsightsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** RestoreTableToPointInTime
    restoreTableToPointInTime_billingModeOverride,
    restoreTableToPointInTime_useLatestRestorableTime,
    restoreTableToPointInTime_globalSecondaryIndexOverride,
    restoreTableToPointInTime_provisionedThroughputOverride,
    restoreTableToPointInTime_sourceTableArn,
    restoreTableToPointInTime_sSESpecificationOverride,
    restoreTableToPointInTime_sourceTableName,
    restoreTableToPointInTime_localSecondaryIndexOverride,
    restoreTableToPointInTime_restoreDateTime,
    restoreTableToPointInTime_targetTableName,
    restoreTableToPointInTimeResponse_tableDescription,
    restoreTableToPointInTimeResponse_httpStatus,

    -- ** RestoreTableFromBackup
    restoreTableFromBackup_billingModeOverride,
    restoreTableFromBackup_globalSecondaryIndexOverride,
    restoreTableFromBackup_provisionedThroughputOverride,
    restoreTableFromBackup_sSESpecificationOverride,
    restoreTableFromBackup_localSecondaryIndexOverride,
    restoreTableFromBackup_targetTableName,
    restoreTableFromBackup_backupArn,
    restoreTableFromBackupResponse_tableDescription,
    restoreTableFromBackupResponse_httpStatus,

    -- ** UpdateTimeToLive
    updateTimeToLive_tableName,
    updateTimeToLive_timeToLiveSpecification,
    updateTimeToLiveResponse_timeToLiveSpecification,
    updateTimeToLiveResponse_httpStatus,

    -- ** CreateGlobalTable
    createGlobalTable_globalTableName,
    createGlobalTable_replicationGroup,
    createGlobalTableResponse_globalTableDescription,
    createGlobalTableResponse_httpStatus,

    -- ** UpdateContinuousBackups
    updateContinuousBackups_tableName,
    updateContinuousBackups_pointInTimeRecoverySpecification,
    updateContinuousBackupsResponse_continuousBackupsDescription,
    updateContinuousBackupsResponse_httpStatus,

    -- ** DescribeExport
    describeExport_exportArn,
    describeExportResponse_exportDescription,
    describeExportResponse_httpStatus,

    -- * Types

    -- ** ArchivalSummary
    archivalSummary_archivalReason,
    archivalSummary_archivalDateTime,
    archivalSummary_archivalBackupArn,

    -- ** AttributeDefinition
    attributeDefinition_attributeName,
    attributeDefinition_attributeType,

    -- ** AttributeValue
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

    -- ** AttributeValueUpdate
    attributeValueUpdate_value,
    attributeValueUpdate_action,

    -- ** AutoScalingPolicyDescription
    autoScalingPolicyDescription_policyName,
    autoScalingPolicyDescription_targetTrackingScalingPolicyConfiguration,

    -- ** AutoScalingPolicyUpdate
    autoScalingPolicyUpdate_policyName,
    autoScalingPolicyUpdate_targetTrackingScalingPolicyConfiguration,

    -- ** AutoScalingSettingsDescription
    autoScalingSettingsDescription_autoScalingDisabled,
    autoScalingSettingsDescription_minimumUnits,
    autoScalingSettingsDescription_maximumUnits,
    autoScalingSettingsDescription_scalingPolicies,
    autoScalingSettingsDescription_autoScalingRoleArn,

    -- ** AutoScalingSettingsUpdate
    autoScalingSettingsUpdate_autoScalingDisabled,
    autoScalingSettingsUpdate_minimumUnits,
    autoScalingSettingsUpdate_scalingPolicyUpdate,
    autoScalingSettingsUpdate_maximumUnits,
    autoScalingSettingsUpdate_autoScalingRoleArn,

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleInCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_disableScaleIn,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_scaleOutCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription_targetValue,

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleInCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_disableScaleIn,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_scaleOutCooldown,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate_targetValue,

    -- ** BackupDescription
    backupDescription_backupDetails,
    backupDescription_sourceTableDetails,
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

    -- ** BatchStatementError
    batchStatementError_code,
    batchStatementError_message,

    -- ** BatchStatementRequest
    batchStatementRequest_consistentRead,
    batchStatementRequest_parameters,
    batchStatementRequest_statement,

    -- ** BatchStatementResponse
    batchStatementResponse_error,
    batchStatementResponse_item,
    batchStatementResponse_tableName,

    -- ** BillingModeSummary
    billingModeSummary_lastUpdateToPayPerRequestDateTime,
    billingModeSummary_billingMode,

    -- ** Capacity
    capacity_readCapacityUnits,
    capacity_capacityUnits,
    capacity_writeCapacityUnits,

    -- ** Condition
    condition_attributeValueList,
    condition_comparisonOperator,

    -- ** ConditionCheck
    conditionCheck_expressionAttributeNames,
    conditionCheck_expressionAttributeValues,
    conditionCheck_returnValuesOnConditionCheckFailure,
    conditionCheck_key,
    conditionCheck_tableName,
    conditionCheck_conditionExpression,

    -- ** ConsumedCapacity
    consumedCapacity_readCapacityUnits,
    consumedCapacity_globalSecondaryIndexes,
    consumedCapacity_capacityUnits,
    consumedCapacity_writeCapacityUnits,
    consumedCapacity_localSecondaryIndexes,
    consumedCapacity_table,
    consumedCapacity_tableName,

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
    delete_expressionAttributeNames,
    delete_expressionAttributeValues,
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

    -- ** DeleteRequest
    deleteRequest_key,

    -- ** Endpoint
    endpoint_address,
    endpoint_cachePeriodInMinutes,

    -- ** ExpectedAttributeValue
    expectedAttributeValue_attributeValueList,
    expectedAttributeValue_exists,
    expectedAttributeValue_value,
    expectedAttributeValue_comparisonOperator,

    -- ** ExportDescription
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
    globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate,
    globalSecondaryIndexAutoScalingUpdate_indexName,

    -- ** GlobalSecondaryIndexDescription
    globalSecondaryIndexDescription_backfilling,
    globalSecondaryIndexDescription_indexSizeBytes,
    globalSecondaryIndexDescription_indexStatus,
    globalSecondaryIndexDescription_provisionedThroughput,
    globalSecondaryIndexDescription_indexArn,
    globalSecondaryIndexDescription_keySchema,
    globalSecondaryIndexDescription_projection,
    globalSecondaryIndexDescription_itemCount,
    globalSecondaryIndexDescription_indexName,

    -- ** GlobalSecondaryIndexInfo
    globalSecondaryIndexInfo_provisionedThroughput,
    globalSecondaryIndexInfo_keySchema,
    globalSecondaryIndexInfo_projection,
    globalSecondaryIndexInfo_indexName,

    -- ** GlobalSecondaryIndexUpdate
    globalSecondaryIndexUpdate_create,
    globalSecondaryIndexUpdate_delete,
    globalSecondaryIndexUpdate_update,

    -- ** GlobalTable
    globalTable_globalTableName,
    globalTable_replicationGroup,

    -- ** GlobalTableDescription
    globalTableDescription_globalTableStatus,
    globalTableDescription_globalTableName,
    globalTableDescription_globalTableArn,
    globalTableDescription_creationDateTime,
    globalTableDescription_replicationGroup,

    -- ** GlobalTableGlobalSecondaryIndexSettingsUpdate
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityUnits,
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityAutoScalingSettingsUpdate,
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
    keysAndAttributes_attributesToGet,
    keysAndAttributes_expressionAttributeNames,
    keysAndAttributes_consistentRead,
    keysAndAttributes_keys,

    -- ** KinesisDataStreamDestination
    kinesisDataStreamDestination_destinationStatus,
    kinesisDataStreamDestination_streamArn,
    kinesisDataStreamDestination_destinationStatusDescription,

    -- ** KinesisStreamingDestinationInput
    kinesisStreamingDestinationInput_tableName,
    kinesisStreamingDestinationInput_streamArn,

    -- ** KinesisStreamingDestinationOutput
    kinesisStreamingDestinationOutput_destinationStatus,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_tableName,

    -- ** LocalSecondaryIndex
    localSecondaryIndex_indexName,
    localSecondaryIndex_keySchema,
    localSecondaryIndex_projection,

    -- ** LocalSecondaryIndexDescription
    localSecondaryIndexDescription_indexSizeBytes,
    localSecondaryIndexDescription_indexArn,
    localSecondaryIndexDescription_keySchema,
    localSecondaryIndexDescription_projection,
    localSecondaryIndexDescription_itemCount,
    localSecondaryIndexDescription_indexName,

    -- ** LocalSecondaryIndexInfo
    localSecondaryIndexInfo_keySchema,
    localSecondaryIndexInfo_projection,
    localSecondaryIndexInfo_indexName,

    -- ** ParameterizedStatement
    parameterizedStatement_parameters,
    parameterizedStatement_statement,

    -- ** PointInTimeRecoveryDescription
    pointInTimeRecoveryDescription_pointInTimeRecoveryStatus,
    pointInTimeRecoveryDescription_earliestRestorableDateTime,
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
    provisionedThroughputDescription_lastDecreaseDateTime,
    provisionedThroughputDescription_writeCapacityUnits,
    provisionedThroughputDescription_numberOfDecreasesToday,
    provisionedThroughputDescription_lastIncreaseDateTime,

    -- ** ProvisionedThroughputOverride
    provisionedThroughputOverride_readCapacityUnits,

    -- ** Put
    put_expressionAttributeNames,
    put_expressionAttributeValues,
    put_returnValuesOnConditionCheckFailure,
    put_conditionExpression,
    put_item,
    put_tableName,

    -- ** PutRequest
    putRequest_item,

    -- ** Replica
    replica_regionName,

    -- ** ReplicaAutoScalingDescription
    replicaAutoScalingDescription_replicaStatus,
    replicaAutoScalingDescription_regionName,
    replicaAutoScalingDescription_globalSecondaryIndexes,
    replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings,

    -- ** ReplicaAutoScalingUpdate
    replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate,
    replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates,
    replicaAutoScalingUpdate_regionName,

    -- ** ReplicaDescription
    replicaDescription_replicaStatus,
    replicaDescription_regionName,
    replicaDescription_replicaStatusPercentProgress,
    replicaDescription_replicaStatusDescription,
    replicaDescription_replicaInaccessibleDateTime,
    replicaDescription_kmsMasterKeyId,
    replicaDescription_provisionedThroughputOverride,
    replicaDescription_globalSecondaryIndexes,

    -- ** ReplicaGlobalSecondaryIndex
    replicaGlobalSecondaryIndex_provisionedThroughputOverride,
    replicaGlobalSecondaryIndex_indexName,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingDescription
    replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexAutoScalingDescription_indexName,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingUpdate
    replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate,
    replicaGlobalSecondaryIndexAutoScalingUpdate_indexName,

    -- ** ReplicaGlobalSecondaryIndexDescription
    replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride,
    replicaGlobalSecondaryIndexDescription_indexName,

    -- ** ReplicaGlobalSecondaryIndexSettingsDescription
    replicaGlobalSecondaryIndexSettingsDescription_indexStatus,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_indexName,

    -- ** ReplicaGlobalSecondaryIndexSettingsUpdate
    replicaGlobalSecondaryIndexSettingsUpdate_provisionedReadCapacityAutoScalingSettingsUpdate,
    replicaGlobalSecondaryIndexSettingsUpdate_provisionedReadCapacityUnits,
    replicaGlobalSecondaryIndexSettingsUpdate_indexName,

    -- ** ReplicaSettingsDescription
    replicaSettingsDescription_replicaStatus,
    replicaSettingsDescription_replicaProvisionedReadCapacityUnits,
    replicaSettingsDescription_replicaProvisionedWriteCapacityUnits,
    replicaSettingsDescription_replicaBillingModeSummary,
    replicaSettingsDescription_replicaGlobalSecondaryIndexSettings,
    replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings,
    replicaSettingsDescription_regionName,

    -- ** ReplicaSettingsUpdate
    replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    replicaSettingsUpdate_replicaProvisionedReadCapacityUnits,
    replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate,
    replicaSettingsUpdate_regionName,

    -- ** ReplicaUpdate
    replicaUpdate_create,
    replicaUpdate_delete,

    -- ** ReplicationGroupUpdate
    replicationGroupUpdate_create,
    replicationGroupUpdate_delete,
    replicationGroupUpdate_update,

    -- ** RestoreSummary
    restoreSummary_sourceTableArn,
    restoreSummary_sourceBackupArn,
    restoreSummary_restoreDateTime,
    restoreSummary_restoreInProgress,

    -- ** SSEDescription
    sSEDescription_status,
    sSEDescription_inaccessibleEncryptionDateTime,
    sSEDescription_sSEType,
    sSEDescription_kmsMasterKeyArn,

    -- ** SSESpecification
    sSESpecification_enabled,
    sSESpecification_kmsMasterKeyId,
    sSESpecification_sSEType,

    -- ** SourceTableDetails
    sourceTableDetails_tableSizeBytes,
    sourceTableDetails_tableArn,
    sourceTableDetails_billingMode,
    sourceTableDetails_itemCount,
    sourceTableDetails_tableName,
    sourceTableDetails_tableId,
    sourceTableDetails_keySchema,
    sourceTableDetails_tableCreationDateTime,
    sourceTableDetails_provisionedThroughput,

    -- ** SourceTableFeatureDetails
    sourceTableFeatureDetails_streamDescription,
    sourceTableFeatureDetails_globalSecondaryIndexes,
    sourceTableFeatureDetails_localSecondaryIndexes,
    sourceTableFeatureDetails_sSEDescription,
    sourceTableFeatureDetails_timeToLiveDescription,

    -- ** StreamSpecification
    streamSpecification_streamViewType,
    streamSpecification_streamEnabled,

    -- ** TableAutoScalingDescription
    tableAutoScalingDescription_tableStatus,
    tableAutoScalingDescription_replicas,
    tableAutoScalingDescription_tableName,

    -- ** TableDescription
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
    transactWriteItem_put,
    transactWriteItem_delete,
    transactWriteItem_update,

    -- ** Update
    update_expressionAttributeNames,
    update_expressionAttributeValues,
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
