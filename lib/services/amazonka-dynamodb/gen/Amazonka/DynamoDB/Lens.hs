{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDB.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Lens
  ( -- * Operations

    -- ** BatchExecuteStatement
    batchExecuteStatement_returnConsumedCapacity,
    batchExecuteStatement_statements,
    batchExecuteStatementResponse_consumedCapacity,
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
    createTable_billingMode,
    createTable_deletionProtectionEnabled,
    createTable_globalSecondaryIndexes,
    createTable_localSecondaryIndexes,
    createTable_provisionedThroughput,
    createTable_sSESpecification,
    createTable_streamSpecification,
    createTable_tableClass,
    createTable_tags,
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
    deleteItem_conditionExpression,
    deleteItem_conditionalOperator,
    deleteItem_expected,
    deleteItem_expressionAttributeNames,
    deleteItem_expressionAttributeValues,
    deleteItem_returnConsumedCapacity,
    deleteItem_returnItemCollectionMetrics,
    deleteItem_returnValues,
    deleteItem_tableName,
    deleteItem_key,
    deleteItemResponse_attributes,
    deleteItemResponse_consumedCapacity,
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
    describeContributorInsightsResponse_contributorInsightsRuleList,
    describeContributorInsightsResponse_contributorInsightsStatus,
    describeContributorInsightsResponse_failureException,
    describeContributorInsightsResponse_indexName,
    describeContributorInsightsResponse_lastUpdateDateTime,
    describeContributorInsightsResponse_tableName,
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

    -- ** DescribeImport
    describeImport_importArn,
    describeImportResponse_httpStatus,
    describeImportResponse_importTableDescription,

    -- ** DescribeKinesisStreamingDestination
    describeKinesisStreamingDestination_tableName,
    describeKinesisStreamingDestinationResponse_kinesisDataStreamDestinations,
    describeKinesisStreamingDestinationResponse_tableName,
    describeKinesisStreamingDestinationResponse_httpStatus,

    -- ** DescribeLimits
    describeLimitsResponse_accountMaxReadCapacityUnits,
    describeLimitsResponse_accountMaxWriteCapacityUnits,
    describeLimitsResponse_tableMaxReadCapacityUnits,
    describeLimitsResponse_tableMaxWriteCapacityUnits,
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
    kinesisStreamingDestinationOutput_destinationStatus,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_tableName,

    -- ** EnableKinesisStreamingDestination
    enableKinesisStreamingDestination_tableName,
    enableKinesisStreamingDestination_streamArn,
    kinesisStreamingDestinationOutput_destinationStatus,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_tableName,

    -- ** ExecuteStatement
    executeStatement_consistentRead,
    executeStatement_limit,
    executeStatement_nextToken,
    executeStatement_parameters,
    executeStatement_returnConsumedCapacity,
    executeStatement_statement,
    executeStatementResponse_consumedCapacity,
    executeStatementResponse_items,
    executeStatementResponse_lastEvaluatedKey,
    executeStatementResponse_nextToken,
    executeStatementResponse_httpStatus,

    -- ** ExecuteTransaction
    executeTransaction_clientRequestToken,
    executeTransaction_returnConsumedCapacity,
    executeTransaction_transactStatements,
    executeTransactionResponse_consumedCapacity,
    executeTransactionResponse_responses,
    executeTransactionResponse_httpStatus,

    -- ** ExportTableToPointInTime
    exportTableToPointInTime_clientToken,
    exportTableToPointInTime_exportFormat,
    exportTableToPointInTime_exportTime,
    exportTableToPointInTime_s3BucketOwner,
    exportTableToPointInTime_s3Prefix,
    exportTableToPointInTime_s3SseAlgorithm,
    exportTableToPointInTime_s3SseKmsKeyId,
    exportTableToPointInTime_tableArn,
    exportTableToPointInTime_s3Bucket,
    exportTableToPointInTimeResponse_exportDescription,
    exportTableToPointInTimeResponse_httpStatus,

    -- ** GetItem
    getItem_attributesToGet,
    getItem_consistentRead,
    getItem_expressionAttributeNames,
    getItem_projectionExpression,
    getItem_returnConsumedCapacity,
    getItem_tableName,
    getItem_key,
    getItemResponse_consumedCapacity,
    getItemResponse_item,
    getItemResponse_httpStatus,

    -- ** ImportTable
    importTable_clientToken,
    importTable_inputCompressionType,
    importTable_inputFormatOptions,
    importTable_s3BucketSource,
    importTable_inputFormat,
    importTable_tableCreationParameters,
    importTableResponse_httpStatus,
    importTableResponse_importTableDescription,

    -- ** ListBackups
    listBackups_backupType,
    listBackups_exclusiveStartBackupArn,
    listBackups_limit,
    listBackups_tableName,
    listBackups_timeRangeLowerBound,
    listBackups_timeRangeUpperBound,
    listBackupsResponse_backupSummaries,
    listBackupsResponse_lastEvaluatedBackupArn,
    listBackupsResponse_httpStatus,

    -- ** ListContributorInsights
    listContributorInsights_maxResults,
    listContributorInsights_nextToken,
    listContributorInsights_tableName,
    listContributorInsightsResponse_contributorInsightsSummaries,
    listContributorInsightsResponse_nextToken,
    listContributorInsightsResponse_httpStatus,

    -- ** ListExports
    listExports_maxResults,
    listExports_nextToken,
    listExports_tableArn,
    listExportsResponse_exportSummaries,
    listExportsResponse_nextToken,
    listExportsResponse_httpStatus,

    -- ** ListGlobalTables
    listGlobalTables_exclusiveStartGlobalTableName,
    listGlobalTables_limit,
    listGlobalTables_regionName,
    listGlobalTablesResponse_globalTables,
    listGlobalTablesResponse_lastEvaluatedGlobalTableName,
    listGlobalTablesResponse_httpStatus,

    -- ** ListImports
    listImports_nextToken,
    listImports_pageSize,
    listImports_tableArn,
    listImportsResponse_importSummaryList,
    listImportsResponse_nextToken,
    listImportsResponse_httpStatus,

    -- ** ListTables
    listTables_exclusiveStartTableName,
    listTables_limit,
    listTablesResponse_lastEvaluatedTableName,
    listTablesResponse_tableNames,
    listTablesResponse_httpStatus,

    -- ** ListTagsOfResource
    listTagsOfResource_nextToken,
    listTagsOfResource_resourceArn,
    listTagsOfResourceResponse_nextToken,
    listTagsOfResourceResponse_tags,
    listTagsOfResourceResponse_httpStatus,

    -- ** PutItem
    putItem_conditionExpression,
    putItem_conditionalOperator,
    putItem_expected,
    putItem_expressionAttributeNames,
    putItem_expressionAttributeValues,
    putItem_returnConsumedCapacity,
    putItem_returnItemCollectionMetrics,
    putItem_returnValues,
    putItem_tableName,
    putItem_item,
    putItemResponse_attributes,
    putItemResponse_consumedCapacity,
    putItemResponse_itemCollectionMetrics,
    putItemResponse_httpStatus,

    -- ** Query
    query_attributesToGet,
    query_conditionalOperator,
    query_consistentRead,
    query_exclusiveStartKey,
    query_expressionAttributeNames,
    query_expressionAttributeValues,
    query_filterExpression,
    query_indexName,
    query_keyConditionExpression,
    query_keyConditions,
    query_limit,
    query_projectionExpression,
    query_queryFilter,
    query_returnConsumedCapacity,
    query_scanIndexForward,
    query_select,
    query_tableName,
    queryResponse_consumedCapacity,
    queryResponse_count,
    queryResponse_lastEvaluatedKey,
    queryResponse_scannedCount,
    queryResponse_httpStatus,
    queryResponse_items,

    -- ** RestoreTableFromBackup
    restoreTableFromBackup_billingModeOverride,
    restoreTableFromBackup_globalSecondaryIndexOverride,
    restoreTableFromBackup_localSecondaryIndexOverride,
    restoreTableFromBackup_provisionedThroughputOverride,
    restoreTableFromBackup_sSESpecificationOverride,
    restoreTableFromBackup_targetTableName,
    restoreTableFromBackup_backupArn,
    restoreTableFromBackupResponse_tableDescription,
    restoreTableFromBackupResponse_httpStatus,

    -- ** RestoreTableToPointInTime
    restoreTableToPointInTime_billingModeOverride,
    restoreTableToPointInTime_globalSecondaryIndexOverride,
    restoreTableToPointInTime_localSecondaryIndexOverride,
    restoreTableToPointInTime_provisionedThroughputOverride,
    restoreTableToPointInTime_restoreDateTime,
    restoreTableToPointInTime_sSESpecificationOverride,
    restoreTableToPointInTime_sourceTableArn,
    restoreTableToPointInTime_sourceTableName,
    restoreTableToPointInTime_useLatestRestorableTime,
    restoreTableToPointInTime_targetTableName,
    restoreTableToPointInTimeResponse_tableDescription,
    restoreTableToPointInTimeResponse_httpStatus,

    -- ** Scan
    scan_attributesToGet,
    scan_conditionalOperator,
    scan_consistentRead,
    scan_exclusiveStartKey,
    scan_expressionAttributeNames,
    scan_expressionAttributeValues,
    scan_filterExpression,
    scan_indexName,
    scan_limit,
    scan_projectionExpression,
    scan_returnConsumedCapacity,
    scan_scanFilter,
    scan_segment,
    scan_select,
    scan_totalSegments,
    scan_tableName,
    scanResponse_consumedCapacity,
    scanResponse_count,
    scanResponse_items,
    scanResponse_lastEvaluatedKey,
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
    updateContributorInsightsResponse_indexName,
    updateContributorInsightsResponse_tableName,
    updateContributorInsightsResponse_httpStatus,

    -- ** UpdateGlobalTable
    updateGlobalTable_globalTableName,
    updateGlobalTable_replicaUpdates,
    updateGlobalTableResponse_globalTableDescription,
    updateGlobalTableResponse_httpStatus,

    -- ** UpdateGlobalTableSettings
    updateGlobalTableSettings_globalTableBillingMode,
    updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate,
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits,
    updateGlobalTableSettings_replicaSettingsUpdate,
    updateGlobalTableSettings_globalTableName,
    updateGlobalTableSettingsResponse_globalTableName,
    updateGlobalTableSettingsResponse_replicaSettings,
    updateGlobalTableSettingsResponse_httpStatus,

    -- ** UpdateItem
    updateItem_attributeUpdates,
    updateItem_conditionExpression,
    updateItem_conditionalOperator,
    updateItem_expected,
    updateItem_expressionAttributeNames,
    updateItem_expressionAttributeValues,
    updateItem_returnConsumedCapacity,
    updateItem_returnItemCollectionMetrics,
    updateItem_returnValues,
    updateItem_updateExpression,
    updateItem_tableName,
    updateItem_key,
    updateItemResponse_attributes,
    updateItemResponse_consumedCapacity,
    updateItemResponse_itemCollectionMetrics,
    updateItemResponse_httpStatus,

    -- ** UpdateTable
    updateTable_attributeDefinitions,
    updateTable_billingMode,
    updateTable_deletionProtectionEnabled,
    updateTable_globalSecondaryIndexUpdates,
    updateTable_provisionedThroughput,
    updateTable_replicaUpdates,
    updateTable_sSESpecification,
    updateTable_streamSpecification,
    updateTable_tableClass,
    updateTable_tableName,
    updateTableResponse_tableDescription,
    updateTableResponse_httpStatus,

    -- ** UpdateTableReplicaAutoScaling
    updateTableReplicaAutoScaling_globalSecondaryIndexUpdates,
    updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate,
    updateTableReplicaAutoScaling_replicaUpdates,
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
    archivalSummary_archivalBackupArn,
    archivalSummary_archivalDateTime,
    archivalSummary_archivalReason,

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
    autoScalingSettingsDescription_autoScalingDisabled,
    autoScalingSettingsDescription_autoScalingRoleArn,
    autoScalingSettingsDescription_maximumUnits,
    autoScalingSettingsDescription_minimumUnits,
    autoScalingSettingsDescription_scalingPolicies,

    -- ** AutoScalingSettingsUpdate
    autoScalingSettingsUpdate_autoScalingDisabled,
    autoScalingSettingsUpdate_autoScalingRoleArn,
    autoScalingSettingsUpdate_maximumUnits,
    autoScalingSettingsUpdate_minimumUnits,
    autoScalingSettingsUpdate_scalingPolicyUpdate,

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
    billingModeSummary_billingMode,
    billingModeSummary_lastUpdateToPayPerRequestDateTime,

    -- ** Capacity
    capacity_capacityUnits,
    capacity_readCapacityUnits,
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
    consumedCapacity_capacityUnits,
    consumedCapacity_globalSecondaryIndexes,
    consumedCapacity_localSecondaryIndexes,
    consumedCapacity_readCapacityUnits,
    consumedCapacity_table,
    consumedCapacity_tableName,
    consumedCapacity_writeCapacityUnits,

    -- ** ContinuousBackupsDescription
    continuousBackupsDescription_pointInTimeRecoveryDescription,
    continuousBackupsDescription_continuousBackupsStatus,

    -- ** ContributorInsightsSummary
    contributorInsightsSummary_contributorInsightsStatus,
    contributorInsightsSummary_indexName,
    contributorInsightsSummary_tableName,

    -- ** CreateGlobalSecondaryIndexAction
    createGlobalSecondaryIndexAction_provisionedThroughput,
    createGlobalSecondaryIndexAction_indexName,
    createGlobalSecondaryIndexAction_keySchema,
    createGlobalSecondaryIndexAction_projection,

    -- ** CreateReplicaAction
    createReplicaAction_regionName,

    -- ** CreateReplicationGroupMemberAction
    createReplicationGroupMemberAction_globalSecondaryIndexes,
    createReplicationGroupMemberAction_kmsMasterKeyId,
    createReplicationGroupMemberAction_provisionedThroughputOverride,
    createReplicationGroupMemberAction_tableClassOverride,
    createReplicationGroupMemberAction_regionName,

    -- ** CsvOptions
    csvOptions_delimiter,
    csvOptions_headerList,

    -- ** Delete
    delete_conditionExpression,
    delete_expressionAttributeNames,
    delete_expressionAttributeValues,
    delete_returnValuesOnConditionCheckFailure,
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
    expectedAttributeValue_attributeValueList,
    expectedAttributeValue_comparisonOperator,
    expectedAttributeValue_exists,
    expectedAttributeValue_value,

    -- ** ExportDescription
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
    globalSecondaryIndexAutoScalingUpdate_indexName,
    globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate,

    -- ** GlobalSecondaryIndexDescription
    globalSecondaryIndexDescription_backfilling,
    globalSecondaryIndexDescription_indexArn,
    globalSecondaryIndexDescription_indexName,
    globalSecondaryIndexDescription_indexSizeBytes,
    globalSecondaryIndexDescription_indexStatus,
    globalSecondaryIndexDescription_itemCount,
    globalSecondaryIndexDescription_keySchema,
    globalSecondaryIndexDescription_projection,
    globalSecondaryIndexDescription_provisionedThroughput,

    -- ** GlobalSecondaryIndexInfo
    globalSecondaryIndexInfo_indexName,
    globalSecondaryIndexInfo_keySchema,
    globalSecondaryIndexInfo_projection,
    globalSecondaryIndexInfo_provisionedThroughput,

    -- ** GlobalSecondaryIndexUpdate
    globalSecondaryIndexUpdate_create,
    globalSecondaryIndexUpdate_delete,
    globalSecondaryIndexUpdate_update,

    -- ** GlobalTable
    globalTable_globalTableName,
    globalTable_replicationGroup,

    -- ** GlobalTableDescription
    globalTableDescription_creationDateTime,
    globalTableDescription_globalTableArn,
    globalTableDescription_globalTableName,
    globalTableDescription_globalTableStatus,
    globalTableDescription_replicationGroup,

    -- ** GlobalTableGlobalSecondaryIndexSettingsUpdate
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityAutoScalingSettingsUpdate,
    globalTableGlobalSecondaryIndexSettingsUpdate_provisionedWriteCapacityUnits,
    globalTableGlobalSecondaryIndexSettingsUpdate_indexName,

    -- ** ImportSummary
    importSummary_cloudWatchLogGroupArn,
    importSummary_endTime,
    importSummary_importArn,
    importSummary_importStatus,
    importSummary_inputFormat,
    importSummary_s3BucketSource,
    importSummary_startTime,
    importSummary_tableArn,

    -- ** ImportTableDescription
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

    -- ** InputFormatOptions
    inputFormatOptions_csv,

    -- ** ItemCollectionMetrics
    itemCollectionMetrics_itemCollectionKey,
    itemCollectionMetrics_sizeEstimateRangeGB,

    -- ** ItemResponse
    itemResponse_item,

    -- ** KeySchemaElement
    keySchemaElement_attributeName,
    keySchemaElement_keyType,

    -- ** KeysAndAttributes
    keysAndAttributes_attributesToGet,
    keysAndAttributes_consistentRead,
    keysAndAttributes_expressionAttributeNames,
    keysAndAttributes_projectionExpression,
    keysAndAttributes_keys,

    -- ** KinesisDataStreamDestination
    kinesisDataStreamDestination_destinationStatus,
    kinesisDataStreamDestination_destinationStatusDescription,
    kinesisDataStreamDestination_streamArn,

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
    localSecondaryIndexDescription_indexArn,
    localSecondaryIndexDescription_indexName,
    localSecondaryIndexDescription_indexSizeBytes,
    localSecondaryIndexDescription_itemCount,
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
    pointInTimeRecoveryDescription_latestRestorableDateTime,
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
    provisionedThroughputDescription_numberOfDecreasesToday,
    provisionedThroughputDescription_readCapacityUnits,
    provisionedThroughputDescription_writeCapacityUnits,

    -- ** ProvisionedThroughputOverride
    provisionedThroughputOverride_readCapacityUnits,

    -- ** Put
    put_conditionExpression,
    put_expressionAttributeNames,
    put_expressionAttributeValues,
    put_returnValuesOnConditionCheckFailure,
    put_item,
    put_tableName,

    -- ** Replica
    replica_regionName,

    -- ** ReplicaAutoScalingDescription
    replicaAutoScalingDescription_globalSecondaryIndexes,
    replicaAutoScalingDescription_regionName,
    replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings,
    replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaAutoScalingDescription_replicaStatus,

    -- ** ReplicaAutoScalingUpdate
    replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates,
    replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate,
    replicaAutoScalingUpdate_regionName,

    -- ** ReplicaDescription
    replicaDescription_globalSecondaryIndexes,
    replicaDescription_kmsMasterKeyId,
    replicaDescription_provisionedThroughputOverride,
    replicaDescription_regionName,
    replicaDescription_replicaInaccessibleDateTime,
    replicaDescription_replicaStatus,
    replicaDescription_replicaStatusDescription,
    replicaDescription_replicaStatusPercentProgress,
    replicaDescription_replicaTableClassSummary,

    -- ** ReplicaGlobalSecondaryIndex
    replicaGlobalSecondaryIndex_provisionedThroughputOverride,
    replicaGlobalSecondaryIndex_indexName,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingDescription
    replicaGlobalSecondaryIndexAutoScalingDescription_indexName,
    replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingUpdate
    replicaGlobalSecondaryIndexAutoScalingUpdate_indexName,
    replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate,

    -- ** ReplicaGlobalSecondaryIndexDescription
    replicaGlobalSecondaryIndexDescription_indexName,
    replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride,

    -- ** ReplicaGlobalSecondaryIndexSettingsDescription
    replicaGlobalSecondaryIndexSettingsDescription_indexStatus,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings,
    replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits,
    replicaGlobalSecondaryIndexSettingsDescription_indexName,

    -- ** ReplicaGlobalSecondaryIndexSettingsUpdate
    replicaGlobalSecondaryIndexSettingsUpdate_provisionedReadCapacityAutoScalingSettingsUpdate,
    replicaGlobalSecondaryIndexSettingsUpdate_provisionedReadCapacityUnits,
    replicaGlobalSecondaryIndexSettingsUpdate_indexName,

    -- ** ReplicaSettingsDescription
    replicaSettingsDescription_replicaBillingModeSummary,
    replicaSettingsDescription_replicaGlobalSecondaryIndexSettings,
    replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings,
    replicaSettingsDescription_replicaProvisionedReadCapacityUnits,
    replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings,
    replicaSettingsDescription_replicaProvisionedWriteCapacityUnits,
    replicaSettingsDescription_replicaStatus,
    replicaSettingsDescription_replicaTableClassSummary,
    replicaSettingsDescription_regionName,

    -- ** ReplicaSettingsUpdate
    replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate,
    replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    replicaSettingsUpdate_replicaProvisionedReadCapacityUnits,
    replicaSettingsUpdate_replicaTableClass,
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

    -- ** S3BucketSource
    s3BucketSource_s3BucketOwner,
    s3BucketSource_s3KeyPrefix,
    s3BucketSource_s3Bucket,

    -- ** SSEDescription
    sSEDescription_inaccessibleEncryptionDateTime,
    sSEDescription_kmsMasterKeyArn,
    sSEDescription_sSEType,
    sSEDescription_status,

    -- ** SSESpecification
    sSESpecification_enabled,
    sSESpecification_kmsMasterKeyId,
    sSESpecification_sSEType,

    -- ** SourceTableDetails
    sourceTableDetails_billingMode,
    sourceTableDetails_itemCount,
    sourceTableDetails_tableArn,
    sourceTableDetails_tableSizeBytes,
    sourceTableDetails_tableName,
    sourceTableDetails_tableId,
    sourceTableDetails_keySchema,
    sourceTableDetails_tableCreationDateTime,
    sourceTableDetails_provisionedThroughput,

    -- ** SourceTableFeatureDetails
    sourceTableFeatureDetails_globalSecondaryIndexes,
    sourceTableFeatureDetails_localSecondaryIndexes,
    sourceTableFeatureDetails_sSEDescription,
    sourceTableFeatureDetails_streamDescription,
    sourceTableFeatureDetails_timeToLiveDescription,

    -- ** StreamSpecification
    streamSpecification_streamViewType,
    streamSpecification_streamEnabled,

    -- ** TableAutoScalingDescription
    tableAutoScalingDescription_replicas,
    tableAutoScalingDescription_tableName,
    tableAutoScalingDescription_tableStatus,

    -- ** TableClassSummary
    tableClassSummary_lastUpdateDateTime,
    tableClassSummary_tableClass,

    -- ** TableCreationParameters
    tableCreationParameters_billingMode,
    tableCreationParameters_globalSecondaryIndexes,
    tableCreationParameters_provisionedThroughput,
    tableCreationParameters_sSESpecification,
    tableCreationParameters_tableName,
    tableCreationParameters_attributeDefinitions,
    tableCreationParameters_keySchema,

    -- ** TableDescription
    tableDescription_archivalSummary,
    tableDescription_attributeDefinitions,
    tableDescription_billingModeSummary,
    tableDescription_creationDateTime,
    tableDescription_deletionProtectionEnabled,
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

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TimeToLiveDescription
    timeToLiveDescription_attributeName,
    timeToLiveDescription_timeToLiveStatus,

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
    update_conditionExpression,
    update_expressionAttributeNames,
    update_expressionAttributeValues,
    update_returnValuesOnConditionCheckFailure,
    update_key,
    update_updateExpression,
    update_tableName,

    -- ** UpdateGlobalSecondaryIndexAction
    updateGlobalSecondaryIndexAction_indexName,
    updateGlobalSecondaryIndexAction_provisionedThroughput,

    -- ** UpdateReplicationGroupMemberAction
    updateReplicationGroupMemberAction_globalSecondaryIndexes,
    updateReplicationGroupMemberAction_kmsMasterKeyId,
    updateReplicationGroupMemberAction_provisionedThroughputOverride,
    updateReplicationGroupMemberAction_tableClassOverride,
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
import Amazonka.DynamoDB.DescribeImport
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
import Amazonka.DynamoDB.ImportTable
import Amazonka.DynamoDB.ListBackups
import Amazonka.DynamoDB.ListContributorInsights
import Amazonka.DynamoDB.ListExports
import Amazonka.DynamoDB.ListGlobalTables
import Amazonka.DynamoDB.ListImports
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
import Amazonka.DynamoDB.Types.CsvOptions
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
import Amazonka.DynamoDB.Types.ImportSummary
import Amazonka.DynamoDB.Types.ImportTableDescription
import Amazonka.DynamoDB.Types.InputFormatOptions
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
import Amazonka.DynamoDB.Types.S3BucketSource
import Amazonka.DynamoDB.Types.SSEDescription
import Amazonka.DynamoDB.Types.SSESpecification
import Amazonka.DynamoDB.Types.SourceTableDetails
import Amazonka.DynamoDB.Types.SourceTableFeatureDetails
import Amazonka.DynamoDB.Types.StreamSpecification
import Amazonka.DynamoDB.Types.TableAutoScalingDescription
import Amazonka.DynamoDB.Types.TableClassSummary
import Amazonka.DynamoDB.Types.TableCreationParameters
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
