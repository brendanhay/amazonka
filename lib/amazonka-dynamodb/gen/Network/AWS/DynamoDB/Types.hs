-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types
  ( -- * Service configuration
    dynamoDBService,

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
    ArchivalSummary (..),
    mkArchivalSummary,
    asArchivalReason,
    asArchivalDateTime,
    asArchivalBackupARN,

    -- * AttributeDefinition
    AttributeDefinition (..),
    mkAttributeDefinition,
    adAttributeName,
    adAttributeType,

    -- * AttributeValue
    AttributeValue (..),
    mkAttributeValue,
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
    AttributeValueUpdate (..),
    mkAttributeValueUpdate,
    avuValue,
    avuAction,

    -- * AutoScalingPolicyDescription
    AutoScalingPolicyDescription (..),
    mkAutoScalingPolicyDescription,
    aspdPolicyName,
    aspdTargetTrackingScalingPolicyConfiguration,

    -- * AutoScalingPolicyUpdate
    AutoScalingPolicyUpdate (..),
    mkAutoScalingPolicyUpdate,
    aspuPolicyName,
    aspuTargetTrackingScalingPolicyConfiguration,

    -- * AutoScalingSettingsDescription
    AutoScalingSettingsDescription (..),
    mkAutoScalingSettingsDescription,
    assdAutoScalingDisabled,
    assdMinimumUnits,
    assdMaximumUnits,
    assdScalingPolicies,
    assdAutoScalingRoleARN,

    -- * AutoScalingSettingsUpdate
    AutoScalingSettingsUpdate (..),
    mkAutoScalingSettingsUpdate,
    assuAutoScalingDisabled,
    assuMinimumUnits,
    assuScalingPolicyUpdate,
    assuMaximumUnits,
    assuAutoScalingRoleARN,

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription (..),
    mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription,
    asttspcdScaleInCooldown,
    asttspcdDisableScaleIn,
    asttspcdScaleOutCooldown,
    asttspcdTargetValue,

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (..),
    mkAutoScalingTargetTrackingScalingPolicyConfigurationUpdate,
    asttspcuScaleInCooldown,
    asttspcuDisableScaleIn,
    asttspcuScaleOutCooldown,
    asttspcuTargetValue,

    -- * BackupDescription
    BackupDescription (..),
    mkBackupDescription,
    bdBackupDetails,
    bdSourceTableDetails,
    bdSourceTableFeatureDetails,

    -- * BackupDetails
    BackupDetails (..),
    mkBackupDetails,
    bdBackupExpiryDateTime,
    bdBackupSizeBytes,
    bdBackupARN,
    bdBackupName,
    bdBackupStatus,
    bdBackupType,
    bdBackupCreationDateTime,

    -- * BackupSummary
    BackupSummary (..),
    mkBackupSummary,
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
    BatchStatementError (..),
    mkBatchStatementError,
    bseCode,
    bseMessage,

    -- * BatchStatementRequest
    BatchStatementRequest (..),
    mkBatchStatementRequest,
    bsrConsistentRead,
    bsrParameters,
    bsrStatement,

    -- * BatchStatementResponse
    BatchStatementResponse (..),
    mkBatchStatementResponse,
    bError,
    bItem,
    bTableName,

    -- * BillingModeSummary
    BillingModeSummary (..),
    mkBillingModeSummary,
    bmsLastUpdateToPayPerRequestDateTime,
    bmsBillingMode,

    -- * Capacity
    Capacity (..),
    mkCapacity,
    capReadCapacityUnits,
    capCapacityUnits,
    capWriteCapacityUnits,

    -- * Condition
    Condition (..),
    mkCondition,
    cAttributeValueList,
    cComparisonOperator,

    -- * ConditionCheck
    ConditionCheck (..),
    mkConditionCheck,
    ccExpressionAttributeNames,
    ccExpressionAttributeValues,
    ccReturnValuesOnConditionCheckFailure,
    ccKey,
    ccTableName,
    ccConditionExpression,

    -- * ConsumedCapacity
    ConsumedCapacity (..),
    mkConsumedCapacity,
    cReadCapacityUnits,
    cGlobalSecondaryIndexes,
    cCapacityUnits,
    cWriteCapacityUnits,
    cLocalSecondaryIndexes,
    cTable,
    cTableName,

    -- * ContinuousBackupsDescription
    ContinuousBackupsDescription (..),
    mkContinuousBackupsDescription,
    cbdPointInTimeRecoveryDescription,
    cbdContinuousBackupsStatus,

    -- * ContributorInsightsSummary
    ContributorInsightsSummary (..),
    mkContributorInsightsSummary,
    cisContributorInsightsStatus,
    cisTableName,
    cisIndexName,

    -- * CreateGlobalSecondaryIndexAction
    CreateGlobalSecondaryIndexAction (..),
    mkCreateGlobalSecondaryIndexAction,
    cgsiaProvisionedThroughput,
    cgsiaIndexName,
    cgsiaKeySchema,
    cgsiaProjection,

    -- * CreateReplicaAction
    CreateReplicaAction (..),
    mkCreateReplicaAction,
    craRegionName,

    -- * CreateReplicationGroupMemberAction
    CreateReplicationGroupMemberAction (..),
    mkCreateReplicationGroupMemberAction,
    crgmaKMSMasterKeyId,
    crgmaProvisionedThroughputOverride,
    crgmaGlobalSecondaryIndexes,
    crgmaRegionName,

    -- * Delete
    Delete (..),
    mkDelete,
    dExpressionAttributeNames,
    dExpressionAttributeValues,
    dReturnValuesOnConditionCheckFailure,
    dConditionExpression,
    dKey,
    dTableName,

    -- * DeleteGlobalSecondaryIndexAction
    DeleteGlobalSecondaryIndexAction (..),
    mkDeleteGlobalSecondaryIndexAction,
    dgsiaIndexName,

    -- * DeleteReplicaAction
    DeleteReplicaAction (..),
    mkDeleteReplicaAction,
    draRegionName,

    -- * DeleteReplicationGroupMemberAction
    DeleteReplicationGroupMemberAction (..),
    mkDeleteReplicationGroupMemberAction,
    drgmaRegionName,

    -- * DeleteRequest
    DeleteRequest (..),
    mkDeleteRequest,
    drKey,

    -- * Endpoint
    Endpoint (..),
    mkEndpoint,
    eAddress,
    eCachePeriodInMinutes,

    -- * ExpectedAttributeValue
    ExpectedAttributeValue (..),
    mkExpectedAttributeValue,
    eavAttributeValueList,
    eavExists,
    eavValue,
    eavComparisonOperator,

    -- * ExportDescription
    ExportDescription (..),
    mkExportDescription,
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
    ExportSummary (..),
    mkExportSummary,
    esExportStatus,
    esExportARN,

    -- * FailureException
    FailureException (..),
    mkFailureException,
    feExceptionName,
    feExceptionDescription,

    -- * Get
    Get (..),
    mkGet,
    gProjectionExpression,
    gExpressionAttributeNames,
    gKey,
    gTableName,

    -- * GlobalSecondaryIndex
    GlobalSecondaryIndex (..),
    mkGlobalSecondaryIndex,
    gsiProvisionedThroughput,
    gsiIndexName,
    gsiKeySchema,
    gsiProjection,

    -- * GlobalSecondaryIndexAutoScalingUpdate
    GlobalSecondaryIndexAutoScalingUpdate (..),
    mkGlobalSecondaryIndexAutoScalingUpdate,
    gsiasuProvisionedWriteCapacityAutoScalingUpdate,
    gsiasuIndexName,

    -- * GlobalSecondaryIndexDescription
    GlobalSecondaryIndexDescription (..),
    mkGlobalSecondaryIndexDescription,
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
    GlobalSecondaryIndexInfo (..),
    mkGlobalSecondaryIndexInfo,
    gsiiProvisionedThroughput,
    gsiiKeySchema,
    gsiiProjection,
    gsiiIndexName,

    -- * GlobalSecondaryIndexUpdate
    GlobalSecondaryIndexUpdate (..),
    mkGlobalSecondaryIndexUpdate,
    gsiuCreate,
    gsiuDelete,
    gsiuUpdate,

    -- * GlobalTable
    GlobalTable (..),
    mkGlobalTable,
    gtGlobalTableName,
    gtReplicationGroup,

    -- * GlobalTableDescription
    GlobalTableDescription (..),
    mkGlobalTableDescription,
    gtdGlobalTableStatus,
    gtdGlobalTableName,
    gtdGlobalTableARN,
    gtdCreationDateTime,
    gtdReplicationGroup,

    -- * GlobalTableGlobalSecondaryIndexSettingsUpdate
    GlobalTableGlobalSecondaryIndexSettingsUpdate (..),
    mkGlobalTableGlobalSecondaryIndexSettingsUpdate,
    gtgsisuProvisionedWriteCapacityUnits,
    gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate,
    gtgsisuIndexName,

    -- * ItemCollectionMetrics
    ItemCollectionMetrics (..),
    mkItemCollectionMetrics,
    icmItemCollectionKey,
    icmSizeEstimateRangeGB,

    -- * ItemResponse
    ItemResponse (..),
    mkItemResponse,
    iItem,

    -- * KeySchemaElement
    KeySchemaElement (..),
    mkKeySchemaElement,
    kseAttributeName,
    kseKeyType,

    -- * KeysAndAttributes
    KeysAndAttributes (..),
    mkKeysAndAttributes,
    kaaProjectionExpression,
    kaaAttributesToGet,
    kaaExpressionAttributeNames,
    kaaConsistentRead,
    kaaKeys,

    -- * KinesisDataStreamDestination
    KinesisDataStreamDestination (..),
    mkKinesisDataStreamDestination,
    kdsdDestinationStatus,
    kdsdStreamARN,
    kdsdDestinationStatusDescription,

    -- * KinesisStreamingDestinationInput
    KinesisStreamingDestinationInput (..),
    mkKinesisStreamingDestinationInput,
    ksdiTableName,
    ksdiStreamARN,

    -- * KinesisStreamingDestinationOutput
    KinesisStreamingDestinationOutput (..),
    mkKinesisStreamingDestinationOutput,
    ksdoDestinationStatus,
    ksdoStreamARN,
    ksdoTableName,

    -- * LocalSecondaryIndex
    LocalSecondaryIndex (..),
    mkLocalSecondaryIndex,
    lsiIndexName,
    lsiKeySchema,
    lsiProjection,

    -- * LocalSecondaryIndexDescription
    LocalSecondaryIndexDescription (..),
    mkLocalSecondaryIndexDescription,
    lsidIndexSizeBytes,
    lsidIndexARN,
    lsidKeySchema,
    lsidProjection,
    lsidItemCount,
    lsidIndexName,

    -- * LocalSecondaryIndexInfo
    LocalSecondaryIndexInfo (..),
    mkLocalSecondaryIndexInfo,
    lsiiKeySchema,
    lsiiProjection,
    lsiiIndexName,

    -- * ParameterizedStatement
    ParameterizedStatement (..),
    mkParameterizedStatement,
    psParameters,
    psStatement,

    -- * PointInTimeRecoveryDescription
    PointInTimeRecoveryDescription (..),
    mkPointInTimeRecoveryDescription,
    pitrdPointInTimeRecoveryStatus,
    pitrdEarliestRestorableDateTime,
    pitrdLatestRestorableDateTime,

    -- * PointInTimeRecoverySpecification
    PointInTimeRecoverySpecification (..),
    mkPointInTimeRecoverySpecification,
    pitrsPointInTimeRecoveryEnabled,

    -- * Projection
    Projection (..),
    mkProjection,
    pProjectionType,
    pNonKeyAttributes,

    -- * ProvisionedThroughput
    ProvisionedThroughput (..),
    mkProvisionedThroughput,
    ptReadCapacityUnits,
    ptWriteCapacityUnits,

    -- * ProvisionedThroughputDescription
    ProvisionedThroughputDescription (..),
    mkProvisionedThroughputDescription,
    ptdReadCapacityUnits,
    ptdLastDecreaseDateTime,
    ptdWriteCapacityUnits,
    ptdNumberOfDecreasesToday,
    ptdLastIncreaseDateTime,

    -- * ProvisionedThroughputOverride
    ProvisionedThroughputOverride (..),
    mkProvisionedThroughputOverride,
    ptoReadCapacityUnits,

    -- * Put
    Put (..),
    mkPut,
    pExpressionAttributeNames,
    pExpressionAttributeValues,
    pReturnValuesOnConditionCheckFailure,
    pConditionExpression,
    pItem,
    pTableName,

    -- * PutRequest
    PutRequest (..),
    mkPutRequest,
    prItem,

    -- * Replica
    Replica (..),
    mkReplica,
    rRegionName,

    -- * ReplicaAutoScalingDescription
    ReplicaAutoScalingDescription (..),
    mkReplicaAutoScalingDescription,
    rasdReplicaStatus,
    rasdRegionName,
    rasdGlobalSecondaryIndexes,
    rasdReplicaProvisionedWriteCapacityAutoScalingSettings,
    rasdReplicaProvisionedReadCapacityAutoScalingSettings,

    -- * ReplicaAutoScalingUpdate
    ReplicaAutoScalingUpdate (..),
    mkReplicaAutoScalingUpdate,
    rasuReplicaProvisionedReadCapacityAutoScalingUpdate,
    rasuReplicaGlobalSecondaryIndexUpdates,
    rasuRegionName,

    -- * ReplicaDescription
    ReplicaDescription (..),
    mkReplicaDescription,
    rdReplicaStatus,
    rdRegionName,
    rdReplicaStatusPercentProgress,
    rdReplicaStatusDescription,
    rdReplicaInaccessibleDateTime,
    rdKMSMasterKeyId,
    rdProvisionedThroughputOverride,
    rdGlobalSecondaryIndexes,

    -- * ReplicaGlobalSecondaryIndex
    ReplicaGlobalSecondaryIndex (..),
    mkReplicaGlobalSecondaryIndex,
    rgsiProvisionedThroughputOverride,
    rgsiIndexName,

    -- * ReplicaGlobalSecondaryIndexAutoScalingDescription
    ReplicaGlobalSecondaryIndexAutoScalingDescription (..),
    mkReplicaGlobalSecondaryIndexAutoScalingDescription,
    rgsiasdIndexStatus,
    rgsiasdProvisionedWriteCapacityAutoScalingSettings,
    rgsiasdProvisionedReadCapacityAutoScalingSettings,
    rgsiasdIndexName,

    -- * ReplicaGlobalSecondaryIndexAutoScalingUpdate
    ReplicaGlobalSecondaryIndexAutoScalingUpdate (..),
    mkReplicaGlobalSecondaryIndexAutoScalingUpdate,
    rgsiasuProvisionedReadCapacityAutoScalingUpdate,
    rgsiasuIndexName,

    -- * ReplicaGlobalSecondaryIndexDescription
    ReplicaGlobalSecondaryIndexDescription (..),
    mkReplicaGlobalSecondaryIndexDescription,
    rgsidProvisionedThroughputOverride,
    rgsidIndexName,

    -- * ReplicaGlobalSecondaryIndexSettingsDescription
    ReplicaGlobalSecondaryIndexSettingsDescription (..),
    mkReplicaGlobalSecondaryIndexSettingsDescription,
    rgsisdIndexStatus,
    rgsisdProvisionedReadCapacityUnits,
    rgsisdProvisionedWriteCapacityUnits,
    rgsisdProvisionedWriteCapacityAutoScalingSettings,
    rgsisdProvisionedReadCapacityAutoScalingSettings,
    rgsisdIndexName,

    -- * ReplicaGlobalSecondaryIndexSettingsUpdate
    ReplicaGlobalSecondaryIndexSettingsUpdate (..),
    mkReplicaGlobalSecondaryIndexSettingsUpdate,
    rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate,
    rgsisuProvisionedReadCapacityUnits,
    rgsisuIndexName,

    -- * ReplicaSettingsDescription
    ReplicaSettingsDescription (..),
    mkReplicaSettingsDescription,
    rsdReplicaStatus,
    rsdReplicaProvisionedReadCapacityUnits,
    rsdReplicaProvisionedWriteCapacityUnits,
    rsdReplicaBillingModeSummary,
    rsdReplicaGlobalSecondaryIndexSettings,
    rsdReplicaProvisionedWriteCapacityAutoScalingSettings,
    rsdReplicaProvisionedReadCapacityAutoScalingSettings,
    rsdRegionName,

    -- * ReplicaSettingsUpdate
    ReplicaSettingsUpdate (..),
    mkReplicaSettingsUpdate,
    rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    rsuReplicaProvisionedReadCapacityUnits,
    rsuReplicaGlobalSecondaryIndexSettingsUpdate,
    rsuRegionName,

    -- * ReplicaUpdate
    ReplicaUpdate (..),
    mkReplicaUpdate,
    ruCreate,
    ruDelete,

    -- * ReplicationGroupUpdate
    ReplicationGroupUpdate (..),
    mkReplicationGroupUpdate,
    rguCreate,
    rguDelete,
    rguUpdate,

    -- * RestoreSummary
    RestoreSummary (..),
    mkRestoreSummary,
    rsSourceTableARN,
    rsSourceBackupARN,
    rsRestoreDateTime,
    rsRestoreInProgress,

    -- * SSEDescription
    SSEDescription (..),
    mkSSEDescription,
    ssedStatus,
    ssedInaccessibleEncryptionDateTime,
    ssedSSEType,
    ssedKMSMasterKeyARN,

    -- * SSESpecification
    SSESpecification (..),
    mkSSESpecification,
    ssesEnabled,
    ssesKMSMasterKeyId,
    ssesSSEType,

    -- * SourceTableDetails
    SourceTableDetails (..),
    mkSourceTableDetails,
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
    SourceTableFeatureDetails (..),
    mkSourceTableFeatureDetails,
    stfdStreamDescription,
    stfdGlobalSecondaryIndexes,
    stfdLocalSecondaryIndexes,
    stfdSSEDescription,
    stfdTimeToLiveDescription,

    -- * StreamSpecification
    StreamSpecification (..),
    mkStreamSpecification,
    ssStreamViewType,
    ssStreamEnabled,

    -- * TableAutoScalingDescription
    TableAutoScalingDescription (..),
    mkTableAutoScalingDescription,
    tasdTableStatus,
    tasdReplicas,
    tasdTableName,

    -- * TableDescription
    TableDescription (..),
    mkTableDescription,
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
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TimeToLiveDescription
    TimeToLiveDescription (..),
    mkTimeToLiveDescription,
    ttldTimeToLiveStatus,
    ttldAttributeName,

    -- * TimeToLiveSpecification
    TimeToLiveSpecification (..),
    mkTimeToLiveSpecification,
    ttlsEnabled,
    ttlsAttributeName,

    -- * TransactGetItem
    TransactGetItem (..),
    mkTransactGetItem,
    tgiGet,

    -- * TransactWriteItem
    TransactWriteItem (..),
    mkTransactWriteItem,
    twiConditionCheck,
    twiPut,
    twiDelete,
    twiUpdate,

    -- * Update
    Update (..),
    mkUpdate,
    uExpressionAttributeNames,
    uExpressionAttributeValues,
    uReturnValuesOnConditionCheckFailure,
    uConditionExpression,
    uKey,
    uUpdateExpression,
    uTableName,

    -- * UpdateGlobalSecondaryIndexAction
    UpdateGlobalSecondaryIndexAction (..),
    mkUpdateGlobalSecondaryIndexAction,
    ugsiaIndexName,
    ugsiaProvisionedThroughput,

    -- * UpdateReplicationGroupMemberAction
    UpdateReplicationGroupMemberAction (..),
    mkUpdateReplicationGroupMemberAction,
    urgmaKMSMasterKeyId,
    urgmaProvisionedThroughputOverride,
    urgmaGlobalSecondaryIndexes,
    urgmaRegionName,

    -- * WriteRequest
    WriteRequest (..),
    mkWriteRequest,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-08-10@ of the Amazon DynamoDB SDK configuration.
dynamoDBService :: Lude.Service
dynamoDBService =
  Lude.Service
    { Lude._svcAbbrev = "DynamoDB",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "dynamodb",
      Lude._svcVersion = "2012-08-10",
      Lude._svcEndpoint = Lude.defaultEndpoint dynamoDBService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "DynamoDB",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "TransactionInProgressException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "still_processing"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
