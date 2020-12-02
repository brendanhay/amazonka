{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types
  ( -- * Service Configuration
    dynamoDB,

    -- * Errors

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
    ArchivalSummary,
    archivalSummary,
    asArchivalReason,
    asArchivalDateTime,
    asArchivalBackupARN,

    -- * AttributeDefinition
    AttributeDefinition,
    attributeDefinition,
    adAttributeName,
    adAttributeType,

    -- * AttributeValue
    AttributeValue,
    attributeValue,
    avL,
    avNS,
    avM,
    avNULL,
    avN,
    avBS,
    avB,
    avSS,
    avS,
    avBOOL,

    -- * AttributeValueUpdate
    AttributeValueUpdate,
    attributeValueUpdate,
    avuValue,
    avuAction,

    -- * AutoScalingPolicyDescription
    AutoScalingPolicyDescription,
    autoScalingPolicyDescription,
    aspdPolicyName,
    aspdTargetTrackingScalingPolicyConfiguration,

    -- * AutoScalingPolicyUpdate
    AutoScalingPolicyUpdate,
    autoScalingPolicyUpdate,
    aspuPolicyName,
    aspuTargetTrackingScalingPolicyConfiguration,

    -- * AutoScalingSettingsDescription
    AutoScalingSettingsDescription,
    autoScalingSettingsDescription,
    assdAutoScalingDisabled,
    assdMinimumUnits,
    assdMaximumUnits,
    assdScalingPolicies,
    assdAutoScalingRoleARN,

    -- * AutoScalingSettingsUpdate
    AutoScalingSettingsUpdate,
    autoScalingSettingsUpdate,
    assuAutoScalingDisabled,
    assuMinimumUnits,
    assuScalingPolicyUpdate,
    assuMaximumUnits,
    assuAutoScalingRoleARN,

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription,
    autoScalingTargetTrackingScalingPolicyConfigurationDescription,
    asttspcdScaleInCooldown,
    asttspcdDisableScaleIn,
    asttspcdScaleOutCooldown,
    asttspcdTargetValue,

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate,
    autoScalingTargetTrackingScalingPolicyConfigurationUpdate,
    asttspcuScaleInCooldown,
    asttspcuDisableScaleIn,
    asttspcuScaleOutCooldown,
    asttspcuTargetValue,

    -- * BackupDescription
    BackupDescription,
    backupDescription,
    bdBackupDetails,
    bdSourceTableDetails,
    bdSourceTableFeatureDetails,

    -- * BackupDetails
    BackupDetails,
    backupDetails,
    bdBackupExpiryDateTime,
    bdBackupSizeBytes,
    bdBackupARN,
    bdBackupName,
    bdBackupStatus,
    bdBackupType,
    bdBackupCreationDateTime,

    -- * BackupSummary
    BackupSummary,
    backupSummary,
    bsBackupExpiryDateTime,
    bsTableARN,
    bsBackupName,
    bsBackupStatus,
    bsBackupSizeBytes,
    bsBackupARN,
    bsTableId,
    bsBackupCreationDateTime,
    bsBackupType,
    bsTableName,

    -- * BatchStatementError
    BatchStatementError,
    batchStatementError,
    bseCode,
    bseMessage,

    -- * BatchStatementRequest
    BatchStatementRequest,
    batchStatementRequest,
    bsrConsistentRead,
    bsrParameters,
    bsrStatement,

    -- * BatchStatementResponse
    BatchStatementResponse,
    batchStatementResponse,
    bError,
    bItem,
    bTableName,

    -- * BillingModeSummary
    BillingModeSummary,
    billingModeSummary,
    bmsLastUpdateToPayPerRequestDateTime,
    bmsBillingMode,

    -- * Capacity
    Capacity,
    capacity,
    capReadCapacityUnits,
    capCapacityUnits,
    capWriteCapacityUnits,

    -- * Condition
    Condition,
    condition,
    cAttributeValueList,
    cComparisonOperator,

    -- * ConditionCheck
    ConditionCheck,
    conditionCheck,
    ccExpressionAttributeNames,
    ccExpressionAttributeValues,
    ccReturnValuesOnConditionCheckFailure,
    ccKey,
    ccTableName,
    ccConditionExpression,

    -- * ConsumedCapacity
    ConsumedCapacity,
    consumedCapacity,
    cReadCapacityUnits,
    cGlobalSecondaryIndexes,
    cCapacityUnits,
    cWriteCapacityUnits,
    cLocalSecondaryIndexes,
    cTable,
    cTableName,

    -- * ContinuousBackupsDescription
    ContinuousBackupsDescription,
    continuousBackupsDescription,
    cbdPointInTimeRecoveryDescription,
    cbdContinuousBackupsStatus,

    -- * ContributorInsightsSummary
    ContributorInsightsSummary,
    contributorInsightsSummary,
    cisContributorInsightsStatus,
    cisTableName,
    cisIndexName,

    -- * CreateGlobalSecondaryIndexAction
    CreateGlobalSecondaryIndexAction,
    createGlobalSecondaryIndexAction,
    cgsiaProvisionedThroughput,
    cgsiaIndexName,
    cgsiaKeySchema,
    cgsiaProjection,

    -- * CreateReplicaAction
    CreateReplicaAction,
    createReplicaAction,
    craRegionName,

    -- * CreateReplicationGroupMemberAction
    CreateReplicationGroupMemberAction,
    createReplicationGroupMemberAction,
    crgmaKMSMasterKeyId,
    crgmaProvisionedThroughputOverride,
    crgmaGlobalSecondaryIndexes,
    crgmaRegionName,

    -- * Delete
    Delete,
    delete',
    dExpressionAttributeNames,
    dExpressionAttributeValues,
    dReturnValuesOnConditionCheckFailure,
    dConditionExpression,
    dKey,
    dTableName,

    -- * DeleteGlobalSecondaryIndexAction
    DeleteGlobalSecondaryIndexAction,
    deleteGlobalSecondaryIndexAction,
    dgsiaIndexName,

    -- * DeleteReplicaAction
    DeleteReplicaAction,
    deleteReplicaAction,
    draRegionName,

    -- * DeleteReplicationGroupMemberAction
    DeleteReplicationGroupMemberAction,
    deleteReplicationGroupMemberAction,
    drgmaRegionName,

    -- * DeleteRequest
    DeleteRequest,
    deleteRequest,
    drKey,

    -- * Endpoint
    Endpoint,
    endpoint,
    eAddress,
    eCachePeriodInMinutes,

    -- * ExpectedAttributeValue
    ExpectedAttributeValue,
    expectedAttributeValue,
    eavAttributeValueList,
    eavExists,
    eavValue,
    eavComparisonOperator,

    -- * ExportDescription
    ExportDescription,
    exportDescription,
    edS3BucketOwner,
    edExportFormat,
    edS3SseKMSKeyId,
    edClientToken,
    edStartTime,
    edFailureCode,
    edExportStatus,
    edFailureMessage,
    edTableARN,
    edBilledSizeBytes,
    edExportARN,
    edExportTime,
    edS3SseAlgorithm,
    edEndTime,
    edS3Prefix,
    edExportManifest,
    edTableId,
    edItemCount,
    edS3Bucket,

    -- * ExportSummary
    ExportSummary,
    exportSummary,
    esExportStatus,
    esExportARN,

    -- * FailureException
    FailureException,
    failureException,
    feExceptionName,
    feExceptionDescription,

    -- * Get
    Get,
    get',
    getProjectionExpression,
    getExpressionAttributeNames,
    getKey,
    getTableName,

    -- * GlobalSecondaryIndex
    GlobalSecondaryIndex,
    globalSecondaryIndex,
    gsiProvisionedThroughput,
    gsiIndexName,
    gsiKeySchema,
    gsiProjection,

    -- * GlobalSecondaryIndexAutoScalingUpdate
    GlobalSecondaryIndexAutoScalingUpdate,
    globalSecondaryIndexAutoScalingUpdate,
    gsiasuProvisionedWriteCapacityAutoScalingUpdate,
    gsiasuIndexName,

    -- * GlobalSecondaryIndexDescription
    GlobalSecondaryIndexDescription,
    globalSecondaryIndexDescription,
    gsidBackfilling,
    gsidIndexSizeBytes,
    gsidIndexStatus,
    gsidProvisionedThroughput,
    gsidIndexARN,
    gsidKeySchema,
    gsidProjection,
    gsidItemCount,
    gsidIndexName,

    -- * GlobalSecondaryIndexInfo
    GlobalSecondaryIndexInfo,
    globalSecondaryIndexInfo,
    gsiiProvisionedThroughput,
    gsiiKeySchema,
    gsiiProjection,
    gsiiIndexName,

    -- * GlobalSecondaryIndexUpdate
    GlobalSecondaryIndexUpdate,
    globalSecondaryIndexUpdate,
    gsiuCreate,
    gsiuDelete,
    gsiuUpdate,

    -- * GlobalTable
    GlobalTable,
    globalTable,
    gtGlobalTableName,
    gtReplicationGroup,

    -- * GlobalTableDescription
    GlobalTableDescription,
    globalTableDescription,
    gtdGlobalTableStatus,
    gtdGlobalTableName,
    gtdGlobalTableARN,
    gtdCreationDateTime,
    gtdReplicationGroup,

    -- * GlobalTableGlobalSecondaryIndexSettingsUpdate
    GlobalTableGlobalSecondaryIndexSettingsUpdate,
    globalTableGlobalSecondaryIndexSettingsUpdate,
    gtgsisuProvisionedWriteCapacityUnits,
    gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate,
    gtgsisuIndexName,

    -- * ItemCollectionMetrics
    ItemCollectionMetrics,
    itemCollectionMetrics,
    icmItemCollectionKey,
    icmSizeEstimateRangeGB,

    -- * ItemResponse
    ItemResponse,
    itemResponse,
    iItem,

    -- * KeySchemaElement
    KeySchemaElement,
    keySchemaElement,
    kseAttributeName,
    kseKeyType,

    -- * KeysAndAttributes
    KeysAndAttributes,
    keysAndAttributes,
    kaaProjectionExpression,
    kaaAttributesToGet,
    kaaExpressionAttributeNames,
    kaaConsistentRead,
    kaaKeys,

    -- * KinesisDataStreamDestination
    KinesisDataStreamDestination,
    kinesisDataStreamDestination,
    kdsdDestinationStatus,
    kdsdStreamARN,
    kdsdDestinationStatusDescription,

    -- * KinesisStreamingDestinationInput
    KinesisStreamingDestinationInput,
    kinesisStreamingDestinationInput,
    ksdiTableName,
    ksdiStreamARN,

    -- * KinesisStreamingDestinationOutput
    KinesisStreamingDestinationOutput,
    kinesisStreamingDestinationOutput,
    ksdoDestinationStatus,
    ksdoStreamARN,
    ksdoTableName,

    -- * LocalSecondaryIndex
    LocalSecondaryIndex,
    localSecondaryIndex,
    lsiIndexName,
    lsiKeySchema,
    lsiProjection,

    -- * LocalSecondaryIndexDescription
    LocalSecondaryIndexDescription,
    localSecondaryIndexDescription,
    lsidIndexSizeBytes,
    lsidIndexARN,
    lsidKeySchema,
    lsidProjection,
    lsidItemCount,
    lsidIndexName,

    -- * LocalSecondaryIndexInfo
    LocalSecondaryIndexInfo,
    localSecondaryIndexInfo,
    lsiiKeySchema,
    lsiiProjection,
    lsiiIndexName,

    -- * ParameterizedStatement
    ParameterizedStatement,
    parameterizedStatement,
    psParameters,
    psStatement,

    -- * PointInTimeRecoveryDescription
    PointInTimeRecoveryDescription,
    pointInTimeRecoveryDescription,
    pitrdPointInTimeRecoveryStatus,
    pitrdEarliestRestorableDateTime,
    pitrdLatestRestorableDateTime,

    -- * PointInTimeRecoverySpecification
    PointInTimeRecoverySpecification,
    pointInTimeRecoverySpecification,
    pitrsPointInTimeRecoveryEnabled,

    -- * Projection
    Projection,
    projection,
    pProjectionType,
    pNonKeyAttributes,

    -- * ProvisionedThroughput
    ProvisionedThroughput,
    provisionedThroughput,
    ptReadCapacityUnits,
    ptWriteCapacityUnits,

    -- * ProvisionedThroughputDescription
    ProvisionedThroughputDescription,
    provisionedThroughputDescription,
    ptdReadCapacityUnits,
    ptdLastDecreaseDateTime,
    ptdWriteCapacityUnits,
    ptdNumberOfDecreasesToday,
    ptdLastIncreaseDateTime,

    -- * ProvisionedThroughputOverride
    ProvisionedThroughputOverride,
    provisionedThroughputOverride,
    ptoReadCapacityUnits,

    -- * Put
    Put,
    put,
    pExpressionAttributeNames,
    pExpressionAttributeValues,
    pReturnValuesOnConditionCheckFailure,
    pConditionExpression,
    pItem,
    pTableName,

    -- * PutRequest
    PutRequest,
    putRequest,
    prItem,

    -- * Replica
    Replica,
    replica,
    rRegionName,

    -- * ReplicaAutoScalingDescription
    ReplicaAutoScalingDescription,
    replicaAutoScalingDescription,
    rasdReplicaStatus,
    rasdRegionName,
    rasdGlobalSecondaryIndexes,
    rasdReplicaProvisionedWriteCapacityAutoScalingSettings,
    rasdReplicaProvisionedReadCapacityAutoScalingSettings,

    -- * ReplicaAutoScalingUpdate
    ReplicaAutoScalingUpdate,
    replicaAutoScalingUpdate,
    rasuReplicaProvisionedReadCapacityAutoScalingUpdate,
    rasuReplicaGlobalSecondaryIndexUpdates,
    rasuRegionName,

    -- * ReplicaDescription
    ReplicaDescription,
    replicaDescription,
    rdReplicaStatus,
    rdRegionName,
    rdReplicaStatusPercentProgress,
    rdReplicaStatusDescription,
    rdReplicaInaccessibleDateTime,
    rdKMSMasterKeyId,
    rdProvisionedThroughputOverride,
    rdGlobalSecondaryIndexes,

    -- * ReplicaGlobalSecondaryIndex
    ReplicaGlobalSecondaryIndex,
    replicaGlobalSecondaryIndex,
    rgsiProvisionedThroughputOverride,
    rgsiIndexName,

    -- * ReplicaGlobalSecondaryIndexAutoScalingDescription
    ReplicaGlobalSecondaryIndexAutoScalingDescription,
    replicaGlobalSecondaryIndexAutoScalingDescription,
    rgsiasdIndexStatus,
    rgsiasdProvisionedWriteCapacityAutoScalingSettings,
    rgsiasdProvisionedReadCapacityAutoScalingSettings,
    rgsiasdIndexName,

    -- * ReplicaGlobalSecondaryIndexAutoScalingUpdate
    ReplicaGlobalSecondaryIndexAutoScalingUpdate,
    replicaGlobalSecondaryIndexAutoScalingUpdate,
    rgsiasuProvisionedReadCapacityAutoScalingUpdate,
    rgsiasuIndexName,

    -- * ReplicaGlobalSecondaryIndexDescription
    ReplicaGlobalSecondaryIndexDescription,
    replicaGlobalSecondaryIndexDescription,
    rgsidProvisionedThroughputOverride,
    rgsidIndexName,

    -- * ReplicaGlobalSecondaryIndexSettingsDescription
    ReplicaGlobalSecondaryIndexSettingsDescription,
    replicaGlobalSecondaryIndexSettingsDescription,
    rgsisdIndexStatus,
    rgsisdProvisionedReadCapacityUnits,
    rgsisdProvisionedWriteCapacityUnits,
    rgsisdProvisionedWriteCapacityAutoScalingSettings,
    rgsisdProvisionedReadCapacityAutoScalingSettings,
    rgsisdIndexName,

    -- * ReplicaGlobalSecondaryIndexSettingsUpdate
    ReplicaGlobalSecondaryIndexSettingsUpdate,
    replicaGlobalSecondaryIndexSettingsUpdate,
    rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate,
    rgsisuProvisionedReadCapacityUnits,
    rgsisuIndexName,

    -- * ReplicaSettingsDescription
    ReplicaSettingsDescription,
    replicaSettingsDescription,
    rsdReplicaStatus,
    rsdReplicaProvisionedReadCapacityUnits,
    rsdReplicaProvisionedWriteCapacityUnits,
    rsdReplicaBillingModeSummary,
    rsdReplicaGlobalSecondaryIndexSettings,
    rsdReplicaProvisionedWriteCapacityAutoScalingSettings,
    rsdReplicaProvisionedReadCapacityAutoScalingSettings,
    rsdRegionName,

    -- * ReplicaSettingsUpdate
    ReplicaSettingsUpdate,
    replicaSettingsUpdate,
    rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    rsuReplicaProvisionedReadCapacityUnits,
    rsuReplicaGlobalSecondaryIndexSettingsUpdate,
    rsuRegionName,

    -- * ReplicaUpdate
    ReplicaUpdate,
    replicaUpdate,
    ruCreate,
    ruDelete,

    -- * ReplicationGroupUpdate
    ReplicationGroupUpdate,
    replicationGroupUpdate,
    rguCreate,
    rguDelete,
    rguUpdate,

    -- * RestoreSummary
    RestoreSummary,
    restoreSummary,
    rsSourceTableARN,
    rsSourceBackupARN,
    rsRestoreDateTime,
    rsRestoreInProgress,

    -- * SSEDescription
    SSEDescription,
    sSEDescription,
    ssedStatus,
    ssedInaccessibleEncryptionDateTime,
    ssedSSEType,
    ssedKMSMasterKeyARN,

    -- * SSESpecification
    SSESpecification,
    sSESpecification,
    ssesEnabled,
    ssesKMSMasterKeyId,
    ssesSSEType,

    -- * SourceTableDetails
    SourceTableDetails,
    sourceTableDetails,
    stdTableSizeBytes,
    stdTableARN,
    stdBillingMode,
    stdItemCount,
    stdTableName,
    stdTableId,
    stdKeySchema,
    stdTableCreationDateTime,
    stdProvisionedThroughput,

    -- * SourceTableFeatureDetails
    SourceTableFeatureDetails,
    sourceTableFeatureDetails,
    stfdStreamDescription,
    stfdGlobalSecondaryIndexes,
    stfdLocalSecondaryIndexes,
    stfdSSEDescription,
    stfdTimeToLiveDescription,

    -- * StreamSpecification
    StreamSpecification,
    streamSpecification,
    ssStreamViewType,
    ssStreamEnabled,

    -- * TableAutoScalingDescription
    TableAutoScalingDescription,
    tableAutoScalingDescription,
    tasdTableStatus,
    tasdReplicas,
    tasdTableName,

    -- * TableDescription
    TableDescription,
    tableDescription,
    tdRestoreSummary,
    tdGlobalTableVersion,
    tdTableSizeBytes,
    tdAttributeDefinitions,
    tdLatestStreamARN,
    tdProvisionedThroughput,
    tdTableStatus,
    tdTableARN,
    tdKeySchema,
    tdGlobalSecondaryIndexes,
    tdLatestStreamLabel,
    tdBillingModeSummary,
    tdLocalSecondaryIndexes,
    tdCreationDateTime,
    tdSSEDescription,
    tdTableId,
    tdReplicas,
    tdItemCount,
    tdArchivalSummary,
    tdTableName,
    tdStreamSpecification,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * TimeToLiveDescription
    TimeToLiveDescription,
    timeToLiveDescription,
    ttldTimeToLiveStatus,
    ttldAttributeName,

    -- * TimeToLiveSpecification
    TimeToLiveSpecification,
    timeToLiveSpecification,
    ttlsEnabled,
    ttlsAttributeName,

    -- * TransactGetItem
    TransactGetItem,
    transactGetItem,
    tgiGet,

    -- * TransactWriteItem
    TransactWriteItem,
    transactWriteItem,
    twiConditionCheck,
    twiPut,
    twiDelete,
    twiUpdate,

    -- * Update
    Update,
    update,
    uExpressionAttributeNames,
    uExpressionAttributeValues,
    uReturnValuesOnConditionCheckFailure,
    uConditionExpression,
    uKey,
    uUpdateExpression,
    uTableName,

    -- * UpdateGlobalSecondaryIndexAction
    UpdateGlobalSecondaryIndexAction,
    updateGlobalSecondaryIndexAction,
    ugsiaIndexName,
    ugsiaProvisionedThroughput,

    -- * UpdateReplicationGroupMemberAction
    UpdateReplicationGroupMemberAction,
    updateReplicationGroupMemberAction,
    urgmaKMSMasterKeyId,
    urgmaProvisionedThroughputOverride,
    urgmaGlobalSecondaryIndexes,
    urgmaRegionName,

    -- * WriteRequest
    WriteRequest,
    writeRequest,
    wrDeleteRequest,
    wrPutRequest,
  )
where

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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-08-10@ of the Amazon DynamoDB SDK configuration.
dynamoDB :: Service
dynamoDB =
  Service
    { _svcAbbrev = "DynamoDB",
      _svcSigner = v4,
      _svcPrefix = "dynamodb",
      _svcVersion = "2012-08-10",
      _svcEndpoint = defaultEndpoint dynamoDB,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "DynamoDB",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "TransactionInProgressException" . hasStatus 400) e =
        Just "still_processing"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
