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
    mkServiceConfig,

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

    -- * TimeToLiveStatus
    TimeToLiveStatus (..),

    -- * BackupDetails
    BackupDetails (..),
    mkBackupDetails,
    bdBackupArn,
    bdBackupName,
    bdBackupStatus,
    bdBackupType,
    bdBackupCreationDateTime,
    bdBackupExpiryDateTime,
    bdBackupSizeBytes,

    -- * ReplicaAutoScalingDescription
    ReplicaAutoScalingDescription (..),
    mkReplicaAutoScalingDescription,
    rasdGlobalSecondaryIndexes,
    rasdRegionName,
    rasdReplicaProvisionedReadCapacityAutoScalingSettings,
    rasdReplicaProvisionedWriteCapacityAutoScalingSettings,
    rasdReplicaStatus,

    -- * TransactWriteItem
    TransactWriteItem (..),
    mkTransactWriteItem,
    twiConditionCheck,
    twiDelete,
    twiPut,
    twiUpdate,

    -- * WriteRequest
    WriteRequest (..),
    mkWriteRequest,
    wrDeleteRequest,
    wrPutRequest,

    -- * ProvisionedThroughputDescription
    ProvisionedThroughputDescription (..),
    mkProvisionedThroughputDescription,
    ptdLastDecreaseDateTime,
    ptdLastIncreaseDateTime,
    ptdNumberOfDecreasesToday,
    ptdReadCapacityUnits,
    ptdWriteCapacityUnits,

    -- * RestoreSummary
    RestoreSummary (..),
    mkRestoreSummary,
    rsRestoreDateTime,
    rsRestoreInProgress,
    rsSourceBackupArn,
    rsSourceTableArn,

    -- * TagValueString
    TagValueString (..),

    -- * DeleteReplicationGroupMemberAction
    DeleteReplicationGroupMemberAction (..),
    mkDeleteReplicationGroupMemberAction,
    drgmaRegionName,

    -- * UpdateReplicationGroupMemberAction
    UpdateReplicationGroupMemberAction (..),
    mkUpdateReplicationGroupMemberAction,
    urgmaRegionName,
    urgmaGlobalSecondaryIndexes,
    urgmaKMSMasterKeyId,
    urgmaProvisionedThroughputOverride,

    -- * ReplicaStatus
    ReplicaStatus (..),

    -- * KinesisStreamingDestinationOutput
    KinesisStreamingDestinationOutput (..),
    mkKinesisStreamingDestinationOutput,
    ksdoDestinationStatus,
    ksdoStreamArn,
    ksdoTableName,

    -- * LocalSecondaryIndexInfo
    LocalSecondaryIndexInfo (..),
    mkLocalSecondaryIndexInfo,
    lsiiIndexName,
    lsiiKeySchema,
    lsiiProjection,

    -- * S3BucketOwner
    S3BucketOwner (..),

    -- * ProjectionExpression
    ProjectionExpression (..),

    -- * KeyType
    KeyType (..),

    -- * ExportFormat
    ExportFormat (..),

    -- * AttributeValue
    AttributeValue (..),
    mkAttributeValue,
    avB,
    avBOOL,
    avBS,
    avL,
    avM,
    avN,
    avNS,
    avNULL,
    avS,
    avSS,

    -- * RegionName
    RegionName (..),

    -- * TableAutoScalingDescription
    TableAutoScalingDescription (..),
    mkTableAutoScalingDescription,
    tasdReplicas,
    tasdTableName,
    tasdTableStatus,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * ReplicaStatusPercentProgress
    ReplicaStatusPercentProgress (..),

    -- * StringAttributeValue
    StringAttributeValue (..),

    -- * BatchStatementResponse
    BatchStatementResponse (..),
    mkBatchStatementResponse,
    bsrError,
    bsrItem,
    bsrTableName,

    -- * ReplicaStatusDescription
    ReplicaStatusDescription (..),

    -- * ExportDescription
    ExportDescription (..),
    mkExportDescription,
    edBilledSizeBytes,
    edClientToken,
    edEndTime,
    edExportArn,
    edExportFormat,
    edExportManifest,
    edExportStatus,
    edExportTime,
    edFailureCode,
    edFailureMessage,
    edItemCount,
    edS3Bucket,
    edS3BucketOwner,
    edS3Prefix,
    edS3SseAlgorithm,
    edS3SseKmsKeyId,
    edStartTime,
    edTableArn,
    edTableId,

    -- * CreateReplicaAction
    CreateReplicaAction (..),
    mkCreateReplicaAction,
    craRegionName,

    -- * IndexStatus
    IndexStatus (..),

    -- * S3SseKmsKeyId
    S3SseKmsKeyId (..),

    -- * ProvisionedThroughput
    ProvisionedThroughput (..),
    mkProvisionedThroughput,
    ptReadCapacityUnits,
    ptWriteCapacityUnits,

    -- * ClientToken
    ClientToken (..),

    -- * ReplicationGroupUpdate
    ReplicationGroupUpdate (..),
    mkReplicationGroupUpdate,
    rguCreate,
    rguDelete,
    rguUpdate,

    -- * TableStatus
    TableStatus (..),

    -- * ProjectionType
    ProjectionType (..),

    -- * String
    String (..),

    -- * GlobalTable
    GlobalTable (..),
    mkGlobalTable,
    gtGlobalTableName,
    gtReplicationGroup,

    -- * FailureCode
    FailureCode (..),

    -- * AutoScalingPolicyName
    AutoScalingPolicyName (..),

    -- * KeySchemaAttributeName
    KeySchemaAttributeName (..),

    -- * ContributorInsightsAction
    ContributorInsightsAction (..),

    -- * ExportStatus
    ExportStatus (..),

    -- * ExportSummary
    ExportSummary (..),
    mkExportSummary,
    esExportArn,
    esExportStatus,

    -- * Replica
    Replica (..),
    mkReplica,
    rRegionName,

    -- * ContributorInsightsRule
    ContributorInsightsRule (..),

    -- * BatchStatementRequest
    BatchStatementRequest (..),
    mkBatchStatementRequest,
    bsrStatement,
    bsrConsistentRead,
    bsrParameters,

    -- * DestinationStatus
    DestinationStatus (..),

    -- * TableDescription
    TableDescription (..),
    mkTableDescription,
    tdArchivalSummary,
    tdAttributeDefinitions,
    tdBillingModeSummary,
    tdCreationDateTime,
    tdGlobalSecondaryIndexes,
    tdGlobalTableVersion,
    tdItemCount,
    tdKeySchema,
    tdLatestStreamArn,
    tdLatestStreamLabel,
    tdLocalSecondaryIndexes,
    tdProvisionedThroughput,
    tdReplicas,
    tdRestoreSummary,
    tdSSEDescription,
    tdStreamSpecification,
    tdTableArn,
    tdTableId,
    tdTableName,
    tdTableSizeBytes,
    tdTableStatus,

    -- * SSESpecification
    SSESpecification (..),
    mkSSESpecification,
    ssesEnabled,
    ssesKMSMasterKeyId,
    ssesSSEType,

    -- * KeysAndAttributes
    KeysAndAttributes (..),
    mkKeysAndAttributes,
    kaaKeys,
    kaaAttributesToGet,
    kaaConsistentRead,
    kaaExpressionAttributeNames,
    kaaProjectionExpression,

    -- * PointInTimeRecoverySpecification
    PointInTimeRecoverySpecification (..),
    mkPointInTimeRecoverySpecification,
    pitrsPointInTimeRecoveryEnabled,

    -- * UpdateExpression
    UpdateExpression (..),

    -- * FailureMessage
    FailureMessage (..),

    -- * TableArn
    TableArn (..),

    -- * KMSMasterKeyId
    KMSMasterKeyId (..),

    -- * ReplicaGlobalSecondaryIndexSettingsUpdate
    ReplicaGlobalSecondaryIndexSettingsUpdate (..),
    mkReplicaGlobalSecondaryIndexSettingsUpdate,
    rgsisuIndexName,
    rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate,
    rgsisuProvisionedReadCapacityUnits,

    -- * AutoScalingPolicyUpdate
    AutoScalingPolicyUpdate (..),
    mkAutoScalingPolicyUpdate,
    aspuTargetTrackingScalingPolicyConfiguration,
    aspuPolicyName,

    -- * ExpressionAttributeValueVariable
    ExpressionAttributeValueVariable (..),

    -- * ArchivalReason
    ArchivalReason (..),

    -- * ItemResponse
    ItemResponse (..),
    mkItemResponse,
    irItem,

    -- * ReturnConsumedCapacity
    ReturnConsumedCapacity (..),

    -- * AutoScalingSettingsUpdate
    AutoScalingSettingsUpdate (..),
    mkAutoScalingSettingsUpdate,
    assuAutoScalingDisabled,
    assuAutoScalingRoleArn,
    assuMaximumUnits,
    assuMinimumUnits,
    assuScalingPolicyUpdate,

    -- * ReturnValuesOnConditionCheckFailure
    ReturnValuesOnConditionCheckFailure (..),

    -- * ProvisionedThroughputOverride
    ProvisionedThroughputOverride (..),
    mkProvisionedThroughputOverride,
    ptoReadCapacityUnits,

    -- * BackupTypeFilter
    BackupTypeFilter (..),

    -- * Get
    Get (..),
    mkGet,
    gKey,
    gTableName,
    gExpressionAttributeNames,
    gProjectionExpression,

    -- * ParameterizedStatement
    ParameterizedStatement (..),
    mkParameterizedStatement,
    psStatement,
    psParameters,

    -- * ReturnItemCollectionMetrics
    ReturnItemCollectionMetrics (..),

    -- * AttributeValueUpdate
    AttributeValueUpdate (..),
    mkAttributeValueUpdate,
    avuAction,
    avuValue,

    -- * ContinuousBackupsDescription
    ContinuousBackupsDescription (..),
    mkContinuousBackupsDescription,
    cbdContinuousBackupsStatus,
    cbdPointInTimeRecoveryDescription,

    -- * SourceTableDetails
    SourceTableDetails (..),
    mkSourceTableDetails,
    stdTableName,
    stdTableId,
    stdKeySchema,
    stdTableCreationDateTime,
    stdProvisionedThroughput,
    stdBillingMode,
    stdItemCount,
    stdTableArn,
    stdTableSizeBytes,

    -- * ExpectedAttributeValue
    ExpectedAttributeValue (..),
    mkExpectedAttributeValue,
    eavAttributeValueList,
    eavComparisonOperator,
    eavExists,
    eavValue,

    -- * SSEStatus
    SSEStatus (..),

    -- * BackupName
    BackupName (..),

    -- * StreamViewType
    StreamViewType (..),

    -- * ConditionExpression
    ConditionExpression (..),

    -- * GlobalTableStatus
    GlobalTableStatus (..),

    -- * StreamArn
    StreamArn (..),

    -- * AttributeDefinition
    AttributeDefinition (..),
    mkAttributeDefinition,
    adAttributeName,
    adAttributeType,

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * ExportArn
    ExportArn (..),

    -- * CreateReplicationGroupMemberAction
    CreateReplicationGroupMemberAction (..),
    mkCreateReplicationGroupMemberAction,
    crgmaRegionName,
    crgmaGlobalSecondaryIndexes,
    crgmaKMSMasterKeyId,
    crgmaProvisionedThroughputOverride,

    -- * ReturnValue
    ReturnValue (..),

    -- * ReplicaGlobalSecondaryIndexDescription
    ReplicaGlobalSecondaryIndexDescription (..),
    mkReplicaGlobalSecondaryIndexDescription,
    rgsidIndexName,
    rgsidProvisionedThroughputOverride,

    -- * LocalSecondaryIndex
    LocalSecondaryIndex (..),
    mkLocalSecondaryIndex,
    lsiIndexName,
    lsiKeySchema,
    lsiProjection,

    -- * PointInTimeRecoveryStatus
    PointInTimeRecoveryStatus (..),

    -- * GlobalSecondaryIndexDescription
    GlobalSecondaryIndexDescription (..),
    mkGlobalSecondaryIndexDescription,
    gsidBackfilling,
    gsidIndexArn,
    gsidIndexName,
    gsidIndexSizeBytes,
    gsidIndexStatus,
    gsidItemCount,
    gsidKeySchema,
    gsidProjection,
    gsidProvisionedThroughput,

    -- * ItemCollectionMetrics
    ItemCollectionMetrics (..),
    mkItemCollectionMetrics,
    icmItemCollectionKey,
    icmSizeEstimateRangeGB,

    -- * Capacity
    Capacity (..),
    mkCapacity,
    cCapacityUnits,
    cReadCapacityUnits,
    cWriteCapacityUnits,

    -- * ReplicaSettingsUpdate
    ReplicaSettingsUpdate (..),
    mkReplicaSettingsUpdate,
    rsuRegionName,
    rsuReplicaGlobalSecondaryIndexSettingsUpdate,
    rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    rsuReplicaProvisionedReadCapacityUnits,

    -- * NumberAttributeValue
    NumberAttributeValue (..),

    -- * ConsumedCapacity
    ConsumedCapacity (..),
    mkConsumedCapacity,
    ccfCapacityUnits,
    ccfGlobalSecondaryIndexes,
    ccfLocalSecondaryIndexes,
    ccfReadCapacityUnits,
    ccfTable,
    ccfTableName,
    ccfWriteCapacityUnits,

    -- * ReplicaGlobalSecondaryIndexAutoScalingUpdate
    ReplicaGlobalSecondaryIndexAutoScalingUpdate (..),
    mkReplicaGlobalSecondaryIndexAutoScalingUpdate,
    rgsiasuIndexName,
    rgsiasuProvisionedReadCapacityAutoScalingUpdate,

    -- * GlobalSecondaryIndexAutoScalingUpdate
    GlobalSecondaryIndexAutoScalingUpdate (..),
    mkGlobalSecondaryIndexAutoScalingUpdate,
    gsiasuIndexName,
    gsiasuProvisionedWriteCapacityAutoScalingUpdate,

    -- * ContinuousBackupsStatus
    ContinuousBackupsStatus (..),

    -- * BillingModeSummary
    BillingModeSummary (..),
    mkBillingModeSummary,
    bmsBillingMode,
    bmsLastUpdateToPayPerRequestDateTime,

    -- * AutoScalingPolicyDescription
    AutoScalingPolicyDescription (..),
    mkAutoScalingPolicyDescription,
    aspdPolicyName,
    aspdTargetTrackingScalingPolicyConfiguration,

    -- * ReplicaGlobalSecondaryIndexSettingsDescription
    ReplicaGlobalSecondaryIndexSettingsDescription (..),
    mkReplicaGlobalSecondaryIndexSettingsDescription,
    rgsisdIndexName,
    rgsisdIndexStatus,
    rgsisdProvisionedReadCapacityAutoScalingSettings,
    rgsisdProvisionedReadCapacityUnits,
    rgsisdProvisionedWriteCapacityAutoScalingSettings,
    rgsisdProvisionedWriteCapacityUnits,

    -- * ReplicaGlobalSecondaryIndex
    ReplicaGlobalSecondaryIndex (..),
    mkReplicaGlobalSecondaryIndex,
    rgsiIndexName,
    rgsiProvisionedThroughputOverride,

    -- * ExpressionAttributeNameVariable
    ExpressionAttributeNameVariable (..),

    -- * DeleteReplicaAction
    DeleteReplicaAction (..),
    mkDeleteReplicaAction,
    draRegionName,

    -- * BatchStatementErrorCodeEnum
    BatchStatementErrorCodeEnum (..),

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription (..),
    mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription,
    asttspcdTargetValue,
    asttspcdDisableScaleIn,
    asttspcdScaleInCooldown,
    asttspcdScaleOutCooldown,

    -- * TransactGetItem
    TransactGetItem (..),
    mkTransactGetItem,
    tgiGet,

    -- * GlobalSecondaryIndex
    GlobalSecondaryIndex (..),
    mkGlobalSecondaryIndex,
    gsiIndexName,
    gsiKeySchema,
    gsiProjection,
    gsiProvisionedThroughput,

    -- * LocalSecondaryIndexDescription
    LocalSecondaryIndexDescription (..),
    mkLocalSecondaryIndexDescription,
    lsidIndexArn,
    lsidIndexName,
    lsidIndexSizeBytes,
    lsidItemCount,
    lsidKeySchema,
    lsidProjection,

    -- * FailureException
    FailureException (..),
    mkFailureException,
    feExceptionDescription,
    feExceptionName,

    -- * AttributeAction
    AttributeAction (..),

    -- * BackupStatus
    BackupStatus (..),

    -- * S3SseAlgorithm
    S3SseAlgorithm (..),

    -- * ContributorInsightsSummary
    ContributorInsightsSummary (..),
    mkContributorInsightsSummary,
    cisContributorInsightsStatus,
    cisIndexName,
    cisTableName,

    -- * ScalarAttributeType
    ScalarAttributeType (..),

    -- * ContributorInsightsStatus
    ContributorInsightsStatus (..),

    -- * ResourceArnString
    ResourceArnString (..),

    -- * BackupSummary
    BackupSummary (..),
    mkBackupSummary,
    bsBackupArn,
    bsBackupCreationDateTime,
    bsBackupExpiryDateTime,
    bsBackupName,
    bsBackupSizeBytes,
    bsBackupStatus,
    bsBackupType,
    bsTableArn,
    bsTableId,
    bsTableName,

    -- * SourceTableFeatureDetails
    SourceTableFeatureDetails (..),
    mkSourceTableFeatureDetails,
    stfdGlobalSecondaryIndexes,
    stfdLocalSecondaryIndexes,
    stfdSSEDescription,
    stfdStreamDescription,
    stfdTimeToLiveDescription,

    -- * SSEType
    SSEType (..),

    -- * Projection
    Projection (..),
    mkProjection,
    pNonKeyAttributes,
    pProjectionType,

    -- * TimeToLiveSpecification
    TimeToLiveSpecification (..),
    mkTimeToLiveSpecification,
    ttlsEnabled,
    ttlsAttributeName,

    -- * CreateGlobalSecondaryIndexAction
    CreateGlobalSecondaryIndexAction (..),
    mkCreateGlobalSecondaryIndexAction,
    cgsiaIndexName,
    cgsiaKeySchema,
    cgsiaProjection,
    cgsiaProvisionedThroughput,

    -- * ConditionCheck
    ConditionCheck (..),
    mkConditionCheck,
    ccKey,
    ccTableName,
    ccConditionExpression,
    ccExpressionAttributeNames,
    ccExpressionAttributeValues,
    ccReturnValuesOnConditionCheckFailure,

    -- * GlobalTableArnString
    GlobalTableArnString (..),

    -- * KinesisDataStreamDestination
    KinesisDataStreamDestination (..),
    mkKinesisDataStreamDestination,
    kdsdDestinationStatus,
    kdsdDestinationStatusDescription,
    kdsdStreamArn,

    -- * S3Prefix
    S3Prefix (..),

    -- * ExportNextToken
    ExportNextToken (..),

    -- * Select
    Select (..),

    -- * KeySchemaElement
    KeySchemaElement (..),
    mkKeySchemaElement,
    kseAttributeName,
    kseKeyType,

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (..),
    mkAutoScalingTargetTrackingScalingPolicyConfigurationUpdate,
    asttspcuTargetValue,
    asttspcuDisableScaleIn,
    asttspcuScaleInCooldown,
    asttspcuScaleOutCooldown,

    -- * BillingMode
    BillingMode (..),

    -- * DeleteGlobalSecondaryIndexAction
    DeleteGlobalSecondaryIndexAction (..),
    mkDeleteGlobalSecondaryIndexAction,
    dgsiaIndexName,

    -- * DeleteRequest
    DeleteRequest (..),
    mkDeleteRequest,
    drKey,

    -- * UpdateGlobalSecondaryIndexAction
    UpdateGlobalSecondaryIndexAction (..),
    mkUpdateGlobalSecondaryIndexAction,
    ugsiaIndexName,
    ugsiaProvisionedThroughput,

    -- * PutRequest
    PutRequest (..),
    mkPutRequest,
    prItem,

    -- * ExceptionName
    ExceptionName (..),

    -- * TagKeyString
    TagKeyString (..),

    -- * BackupDescription
    BackupDescription (..),
    mkBackupDescription,
    bdBackupDetails,
    bdSourceTableDetails,
    bdSourceTableFeatureDetails,

    -- * Condition
    Condition (..),
    mkCondition,
    cComparisonOperator,
    cAttributeValueList,

    -- * ReplicaAutoScalingUpdate
    ReplicaAutoScalingUpdate (..),
    mkReplicaAutoScalingUpdate,
    rasuRegionName,
    rasuReplicaGlobalSecondaryIndexUpdates,
    rasuReplicaProvisionedReadCapacityAutoScalingUpdate,

    -- * TimeToLiveAttributeName
    TimeToLiveAttributeName (..),

    -- * ReplicaSettingsDescription
    ReplicaSettingsDescription (..),
    mkReplicaSettingsDescription,
    rsdRegionName,
    rsdReplicaBillingModeSummary,
    rsdReplicaGlobalSecondaryIndexSettings,
    rsdReplicaProvisionedReadCapacityAutoScalingSettings,
    rsdReplicaProvisionedReadCapacityUnits,
    rsdReplicaProvisionedWriteCapacityAutoScalingSettings,
    rsdReplicaProvisionedWriteCapacityUnits,
    rsdReplicaStatus,

    -- * GlobalTableDescription
    GlobalTableDescription (..),
    mkGlobalTableDescription,
    gtdCreationDateTime,
    gtdGlobalTableArn,
    gtdGlobalTableName,
    gtdGlobalTableStatus,
    gtdReplicationGroup,

    -- * BackupArn
    BackupArn (..),

    -- * KMSMasterKeyArn
    KMSMasterKeyArn (..),

    -- * AutoScalingRoleArn
    AutoScalingRoleArn (..),

    -- * SSEDescription
    SSEDescription (..),
    mkSSEDescription,
    ssedInaccessibleEncryptionDateTime,
    ssedKMSMasterKeyArn,
    ssedSSEType,
    ssedStatus,

    -- * AttributeName
    AttributeName (..),

    -- * Endpoint
    Endpoint (..),
    mkEndpoint,
    eAddress,
    eCachePeriodInMinutes,

    -- * ReplicaGlobalSecondaryIndexAutoScalingDescription
    ReplicaGlobalSecondaryIndexAutoScalingDescription (..),
    mkReplicaGlobalSecondaryIndexAutoScalingDescription,
    rgsiasdIndexName,
    rgsiasdIndexStatus,
    rgsiasdProvisionedReadCapacityAutoScalingSettings,
    rgsiasdProvisionedWriteCapacityAutoScalingSettings,

    -- * PointInTimeRecoveryDescription
    PointInTimeRecoveryDescription (..),
    mkPointInTimeRecoveryDescription,
    pitrdEarliestRestorableDateTime,
    pitrdLatestRestorableDateTime,
    pitrdPointInTimeRecoveryStatus,

    -- * ExportManifest
    ExportManifest (..),

    -- * NonKeyAttributeName
    NonKeyAttributeName (..),

    -- * TableId
    TableId (..),

    -- * ClientRequestToken
    ClientRequestToken (..),

    -- * ConditionalOperator
    ConditionalOperator (..),

    -- * GlobalSecondaryIndexUpdate
    GlobalSecondaryIndexUpdate (..),
    mkGlobalSecondaryIndexUpdate,
    gsiuCreate,
    gsiuDelete,
    gsiuUpdate,

    -- * GlobalTableGlobalSecondaryIndexSettingsUpdate
    GlobalTableGlobalSecondaryIndexSettingsUpdate (..),
    mkGlobalTableGlobalSecondaryIndexSettingsUpdate,
    gtgsisuIndexName,
    gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate,
    gtgsisuProvisionedWriteCapacityUnits,

    -- * TimeToLiveDescription
    TimeToLiveDescription (..),
    mkTimeToLiveDescription,
    ttldAttributeName,
    ttldTimeToLiveStatus,

    -- * ArchivalSummary
    ArchivalSummary (..),
    mkArchivalSummary,
    asArchivalBackupArn,
    asArchivalDateTime,
    asArchivalReason,

    -- * BackupType
    BackupType (..),

    -- * S3Bucket
    S3Bucket (..),

    -- * KinesisStreamingDestinationInput
    KinesisStreamingDestinationInput (..),
    mkKinesisStreamingDestinationInput,
    ksdiTableName,
    ksdiStreamArn,

    -- * ReplicaDescription
    ReplicaDescription (..),
    mkReplicaDescription,
    rdGlobalSecondaryIndexes,
    rdKMSMasterKeyId,
    rdProvisionedThroughputOverride,
    rdRegionName,
    rdReplicaInaccessibleDateTime,
    rdReplicaStatus,
    rdReplicaStatusDescription,
    rdReplicaStatusPercentProgress,

    -- * ReplicaUpdate
    ReplicaUpdate (..),
    mkReplicaUpdate,
    ruCreate,
    ruDelete,

    -- * TableName
    TableName (..),

    -- * Put
    Put (..),
    mkPut,
    pItem,
    pTableName,
    pConditionExpression,
    pExpressionAttributeNames,
    pExpressionAttributeValues,
    pReturnValuesOnConditionCheckFailure,

    -- * ExceptionDescription
    ExceptionDescription (..),

    -- * BatchStatementError
    BatchStatementError (..),
    mkBatchStatementError,
    bseCode,
    bseMessage,

    -- * GlobalSecondaryIndexInfo
    GlobalSecondaryIndexInfo (..),
    mkGlobalSecondaryIndexInfo,
    gsiiIndexName,
    gsiiKeySchema,
    gsiiProjection,
    gsiiProvisionedThroughput,

    -- * Delete
    Delete (..),
    mkDelete,
    dKey,
    dTableName,
    dConditionExpression,
    dExpressionAttributeNames,
    dExpressionAttributeValues,
    dReturnValuesOnConditionCheckFailure,

    -- * Update
    Update (..),
    mkUpdate,
    uKey,
    uUpdateExpression,
    uTableName,
    uConditionExpression,
    uExpressionAttributeNames,
    uExpressionAttributeValues,
    uReturnValuesOnConditionCheckFailure,

    -- * StreamSpecification
    StreamSpecification (..),
    mkStreamSpecification,
    ssStreamEnabled,
    ssStreamViewType,

    -- * AutoScalingSettingsDescription
    AutoScalingSettingsDescription (..),
    mkAutoScalingSettingsDescription,
    assdAutoScalingDisabled,
    assdAutoScalingRoleArn,
    assdMaximumUnits,
    assdMinimumUnits,
    assdScalingPolicies,

    -- * IndexName
    IndexName (..),

    -- * FilterExpression
    FilterExpression (..),

    -- * KeyConditionExpression
    KeyConditionExpression (..),

    -- * Statement
    Statement (..),

    -- * NextToken
    NextToken (..),

    -- * SourceBackupArn
    SourceBackupArn (..),

    -- * SourceTableArn
    SourceTableArn (..),

    -- * GlobalTableName
    GlobalTableName (..),

    -- * N
    N (..),

    -- * S
    S (..),

    -- * LastEvaluatedTableName
    LastEvaluatedTableName (..),

    -- * LatestStreamArn
    LatestStreamArn (..),

    -- * ResourceArn
    ResourceArn (..),

    -- * ExclusiveStartBackupArn
    ExclusiveStartBackupArn (..),

    -- * ExclusiveStartGlobalTableName
    ExclusiveStartGlobalTableName (..),

    -- * TargetTableName
    TargetTableName (..),

    -- * SourceTableName
    SourceTableName (..),

    -- * LastEvaluatedBackupArn
    LastEvaluatedBackupArn (..),

    -- * LastEvaluatedGlobalTableName
    LastEvaluatedGlobalTableName (..),
  )
where

import Network.AWS.DynamoDB.Types.ArchivalReason
import Network.AWS.DynamoDB.Types.ArchivalSummary
import Network.AWS.DynamoDB.Types.AttributeAction
import Network.AWS.DynamoDB.Types.AttributeDefinition
import Network.AWS.DynamoDB.Types.AttributeName
import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.AttributeValueUpdate
import Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
import Network.AWS.DynamoDB.Types.AutoScalingPolicyName
import Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate
import Network.AWS.DynamoDB.Types.AutoScalingRoleArn
import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
import Network.AWS.DynamoDB.Types.BackupArn
import Network.AWS.DynamoDB.Types.BackupDescription
import Network.AWS.DynamoDB.Types.BackupDetails
import Network.AWS.DynamoDB.Types.BackupName
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
import Network.AWS.DynamoDB.Types.ClientRequestToken
import Network.AWS.DynamoDB.Types.ClientToken
import Network.AWS.DynamoDB.Types.ComparisonOperator
import Network.AWS.DynamoDB.Types.Condition
import Network.AWS.DynamoDB.Types.ConditionCheck
import Network.AWS.DynamoDB.Types.ConditionExpression
import Network.AWS.DynamoDB.Types.ConditionalOperator
import Network.AWS.DynamoDB.Types.ConsumedCapacity
import Network.AWS.DynamoDB.Types.ContinuousBackupsDescription
import Network.AWS.DynamoDB.Types.ContinuousBackupsStatus
import Network.AWS.DynamoDB.Types.ContributorInsightsAction
import Network.AWS.DynamoDB.Types.ContributorInsightsRule
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
import Network.AWS.DynamoDB.Types.ExceptionDescription
import Network.AWS.DynamoDB.Types.ExceptionName
import Network.AWS.DynamoDB.Types.ExclusiveStartBackupArn
import Network.AWS.DynamoDB.Types.ExclusiveStartGlobalTableName
import Network.AWS.DynamoDB.Types.ExpectedAttributeValue
import Network.AWS.DynamoDB.Types.ExportArn
import Network.AWS.DynamoDB.Types.ExportDescription
import Network.AWS.DynamoDB.Types.ExportFormat
import Network.AWS.DynamoDB.Types.ExportManifest
import Network.AWS.DynamoDB.Types.ExportNextToken
import Network.AWS.DynamoDB.Types.ExportStatus
import Network.AWS.DynamoDB.Types.ExportSummary
import Network.AWS.DynamoDB.Types.ExpressionAttributeNameVariable
import Network.AWS.DynamoDB.Types.ExpressionAttributeValueVariable
import Network.AWS.DynamoDB.Types.FailureCode
import Network.AWS.DynamoDB.Types.FailureException
import Network.AWS.DynamoDB.Types.FailureMessage
import Network.AWS.DynamoDB.Types.FilterExpression
import Network.AWS.DynamoDB.Types.Get
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndex
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexUpdate
import Network.AWS.DynamoDB.Types.GlobalTable
import Network.AWS.DynamoDB.Types.GlobalTableArnString
import Network.AWS.DynamoDB.Types.GlobalTableDescription
import Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate
import Network.AWS.DynamoDB.Types.GlobalTableName
import Network.AWS.DynamoDB.Types.GlobalTableStatus
import Network.AWS.DynamoDB.Types.IndexName
import Network.AWS.DynamoDB.Types.IndexStatus
import Network.AWS.DynamoDB.Types.ItemCollectionMetrics
import Network.AWS.DynamoDB.Types.ItemResponse
import Network.AWS.DynamoDB.Types.KMSMasterKeyArn
import Network.AWS.DynamoDB.Types.KMSMasterKeyId
import Network.AWS.DynamoDB.Types.KeyConditionExpression
import Network.AWS.DynamoDB.Types.KeySchemaAttributeName
import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.KeyType
import Network.AWS.DynamoDB.Types.KeysAndAttributes
import Network.AWS.DynamoDB.Types.KinesisDataStreamDestination
import Network.AWS.DynamoDB.Types.KinesisStreamingDestinationInput
import Network.AWS.DynamoDB.Types.KinesisStreamingDestinationOutput
import Network.AWS.DynamoDB.Types.LastEvaluatedBackupArn
import Network.AWS.DynamoDB.Types.LastEvaluatedGlobalTableName
import Network.AWS.DynamoDB.Types.LastEvaluatedTableName
import Network.AWS.DynamoDB.Types.LatestStreamArn
import Network.AWS.DynamoDB.Types.LocalSecondaryIndex
import Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.LocalSecondaryIndexInfo
import Network.AWS.DynamoDB.Types.N
import Network.AWS.DynamoDB.Types.NextToken
import Network.AWS.DynamoDB.Types.NonKeyAttributeName
import Network.AWS.DynamoDB.Types.NumberAttributeValue
import Network.AWS.DynamoDB.Types.ParameterizedStatement
import Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription
import Network.AWS.DynamoDB.Types.PointInTimeRecoverySpecification
import Network.AWS.DynamoDB.Types.PointInTimeRecoveryStatus
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProjectionExpression
import Network.AWS.DynamoDB.Types.ProjectionType
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription
import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.Put
import Network.AWS.DynamoDB.Types.PutRequest
import Network.AWS.DynamoDB.Types.RegionName
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
import Network.AWS.DynamoDB.Types.ReplicaStatusDescription
import Network.AWS.DynamoDB.Types.ReplicaStatusPercentProgress
import Network.AWS.DynamoDB.Types.ReplicaUpdate
import Network.AWS.DynamoDB.Types.ReplicationGroupUpdate
import Network.AWS.DynamoDB.Types.ResourceArn
import Network.AWS.DynamoDB.Types.ResourceArnString
import Network.AWS.DynamoDB.Types.RestoreSummary
import Network.AWS.DynamoDB.Types.ReturnConsumedCapacity
import Network.AWS.DynamoDB.Types.ReturnItemCollectionMetrics
import Network.AWS.DynamoDB.Types.ReturnValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Network.AWS.DynamoDB.Types.S
import Network.AWS.DynamoDB.Types.S3Bucket
import Network.AWS.DynamoDB.Types.S3BucketOwner
import Network.AWS.DynamoDB.Types.S3Prefix
import Network.AWS.DynamoDB.Types.S3SseAlgorithm
import Network.AWS.DynamoDB.Types.S3SseKmsKeyId
import Network.AWS.DynamoDB.Types.SSEDescription
import Network.AWS.DynamoDB.Types.SSESpecification
import Network.AWS.DynamoDB.Types.SSEStatus
import Network.AWS.DynamoDB.Types.SSEType
import Network.AWS.DynamoDB.Types.ScalarAttributeType
import Network.AWS.DynamoDB.Types.Select
import Network.AWS.DynamoDB.Types.SourceBackupArn
import Network.AWS.DynamoDB.Types.SourceTableArn
import Network.AWS.DynamoDB.Types.SourceTableDetails
import Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
import Network.AWS.DynamoDB.Types.SourceTableName
import Network.AWS.DynamoDB.Types.Statement
import Network.AWS.DynamoDB.Types.StreamArn
import Network.AWS.DynamoDB.Types.StreamSpecification
import Network.AWS.DynamoDB.Types.StreamViewType
import Network.AWS.DynamoDB.Types.String
import Network.AWS.DynamoDB.Types.StringAttributeValue
import Network.AWS.DynamoDB.Types.TableArn
import Network.AWS.DynamoDB.Types.TableAutoScalingDescription
import Network.AWS.DynamoDB.Types.TableDescription
import Network.AWS.DynamoDB.Types.TableId
import Network.AWS.DynamoDB.Types.TableName
import Network.AWS.DynamoDB.Types.TableStatus
import Network.AWS.DynamoDB.Types.Tag
import Network.AWS.DynamoDB.Types.TagKeyString
import Network.AWS.DynamoDB.Types.TagValueString
import Network.AWS.DynamoDB.Types.TargetTableName
import Network.AWS.DynamoDB.Types.TimeToLiveAttributeName
import Network.AWS.DynamoDB.Types.TimeToLiveDescription
import Network.AWS.DynamoDB.Types.TimeToLiveSpecification
import Network.AWS.DynamoDB.Types.TimeToLiveStatus
import Network.AWS.DynamoDB.Types.TransactGetItem
import Network.AWS.DynamoDB.Types.TransactWriteItem
import Network.AWS.DynamoDB.Types.Update
import Network.AWS.DynamoDB.Types.UpdateExpression
import Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.WriteRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-08-10@ of the Amazon DynamoDB SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "DynamoDB",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "dynamodb",
      Core._svcVersion = "2012-08-10",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "DynamoDB",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "TransactionInProgressException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "still_processing"
      | Core.otherwise = Core.Nothing

-- | The specified @ExportTime@ is outside of the point in time recovery window.
_InvalidExportTimeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidExportTimeException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidExportTimeException"
{-# DEPRECATED _InvalidExportTimeException "Use generic-lens or generic-optics instead." #-}

-- | Backup not found for the given BackupARN.
_BackupNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BackupNotFoundException =
  Core._MatchServiceError mkServiceConfig "BackupNotFoundException"
{-# DEPRECATED _BackupNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | A target table with the specified name is either being created or deleted.
_TableInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TableInUseException =
  Core._MatchServiceError mkServiceConfig "TableInUseException"
{-# DEPRECATED _TableInUseException "Use generic-lens or generic-optics instead." #-}

-- | There was a conflict when writing to the specified S3 bucket.
_ExportConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExportConflictException =
  Core._MatchServiceError mkServiceConfig "ExportConflictException"
{-# DEPRECATED _ExportConflictException "Use generic-lens or generic-optics instead." #-}

-- | Backups have not yet been enabled for this table.
_ContinuousBackupsUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ContinuousBackupsUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "ContinuousBackupsUnavailableException"
{-# DEPRECATED _ContinuousBackupsUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | Your request rate is too high. The AWS SDKs for DynamoDB automatically retry requests that receive this exception. Your request is eventually successful, unless your retry queue is too large to finish. Reduce the frequency of requests and use exponential backoff. For more information, go to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Programming.Errors.html#Programming.Errors.RetryAndBackoff Error Retries and Exponential Backoff> in the /Amazon DynamoDB Developer Guide/ .
_ProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProvisionedThroughputExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ProvisionedThroughputExceededException"
{-# DEPRECATED _ProvisionedThroughputExceededException "Use generic-lens or generic-optics instead." #-}

-- | The specified global table does not exist.
_GlobalTableNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GlobalTableNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "GlobalTableNotFoundException"
{-# DEPRECATED _GlobalTableNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The transaction with the given request token is already in progress.
_TransactionInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TransactionInProgressException =
  Core._MatchServiceError
    mkServiceConfig
    "TransactionInProgressException"
{-# DEPRECATED _TransactionInProgressException "Use generic-lens or generic-optics instead." #-}

-- | The entire transaction request was canceled.
--
-- DynamoDB cancels a @TransactWriteItems@ request under the following circumstances:
--
--     * A condition in one of the condition expressions is not met.
--
--
--     * A table in the @TransactWriteItems@ request is in a different account or region.
--
--
--     * More than one action in the @TransactWriteItems@ operation targets the same item.
--
--
--     * There is insufficient provisioned capacity for the transaction to be completed.
--
--
--     * An item size becomes too large (larger than 400 KB), or a local secondary index (LSI) becomes too large, or a similar validation error occurs because of changes made by the transaction.
--
--
--     * There is a user error, such as an invalid data format.
--
--
-- DynamoDB cancels a @TransactGetItems@ request under the following circumstances:
--
--     * There is an ongoing @TransactGetItems@ operation that conflicts with a concurrent @PutItem@ , @UpdateItem@ , @DeleteItem@ or @TransactWriteItems@ request. In this case the @TransactGetItems@ operation fails with a @TransactionCanceledException@ .
--
--
--     * A table in the @TransactGetItems@ request is in a different account or region.
--
--
--     * There is insufficient provisioned capacity for the transaction to be completed.
--
--
--     * There is a user error, such as an invalid data format.
--
--
-- Cancellation reason codes and possible error messages:
--
--     * No Errors:
--
--     * Code: @NONE@
--
--
--     * Message: @null@
--
--
--
--
--     * Conditional Check Failed:
--
--     * Code: @ConditionalCheckFailed@
--
--
--     * Message: The conditional request failed.
--
--
--
--
--     * Item Collection Size Limit Exceeded:
--
--     * Code: @ItemCollectionSizeLimitExceeded@
--
--
--     * Message: Collection size exceeded.
--
--
--
--
--     * Transaction Conflict:
--
--     * Code: @TransactionConflict@
--
--
--     * Message: Transaction is ongoing for the item.
--
--
--
--
--     * Provisioned Throughput Exceeded:
--
--     * Code: @ProvisionedThroughputExceeded@
--
--
--     * Messages:
--
--     * The level of configured provisioned throughput for the table was exceeded. Consider increasing your provisioning level with the UpdateTable API.
--
--
--     * The level of configured provisioned throughput for one or more global secondary indexes of the table was exceeded. Consider increasing your provisioning level for the under-provisioned global secondary indexes with the UpdateTable API.
--
--
--
--
--
--
--     * Throttling Error:
--
--     * Code: @ThrottlingError@
--
--
--     * Messages:
--
--     * Throughput exceeds the current capacity of your table or index. DynamoDB is automatically scaling your table or index so please try again shortly. If exceptions persist, check if you have a hot key: https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/bp-partition-key-design.html.
--
--
--     * Throughput exceeds the current capacity for one or more global secondary indexes. DynamoDB is automatically scaling your index so please try again shortly.
--
--
--
--
--
--
--     * Validation Error:
--
--     * Code: @ValidationError@
--
--
--     * Messages:
--
--     * One or more parameter values were invalid.
--
--
--     * The update expression attempted to update the secondary index key beyond allowed size limits.
--
--
--     * The update expression attempted to update the secondary index key to unsupported type.
--
--
--     * An operand in the update expression has an incorrect data type.
--
--
--     * Item size to update has exceeded the maximum allowed size.
--
--
--     * Number overflow. Attempting to store a number with magnitude larger than supported range.
--
--
--     * Type mismatch for attribute to update.
--
--
--     * Nesting Levels have exceeded supported limits.
--
--
--     * The document path provided in the update expression is invalid for update.
--
--
--     * The provided expression refers to an attribute that does not exist in the item.
_TransactionCanceledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TransactionCanceledException =
  Core._MatchServiceError
    mkServiceConfig
    "TransactionCanceledException"
{-# DEPRECATED _TransactionCanceledException "Use generic-lens or generic-optics instead." #-}

-- | A condition specified in the operation could not be evaluated.
_ConditionalCheckFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConditionalCheckFailedException =
  Core._MatchServiceError
    mkServiceConfig
    "ConditionalCheckFailedException"
{-# DEPRECATED _ConditionalCheckFailedException "Use generic-lens or generic-optics instead." #-}

-- | The specified global table already exists.
_GlobalTableAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GlobalTableAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "GlobalTableAlreadyExistsException"
{-# DEPRECATED _GlobalTableAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The specified replica is no longer part of the global table.
_ReplicaNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplicaNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ReplicaNotFoundException"
{-# DEPRECATED _ReplicaNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | A target table with the specified name already exists.
_TableAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TableAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "TableAlreadyExistsException"
{-# DEPRECATED _TableAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | Throughput exceeds the current throughput quota for your account. Please contact AWS Support at <https://aws.amazon.com/support AWS Support> to request a quota increase.
_RequestLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestLimitExceeded =
  Core._MatchServiceError mkServiceConfig "RequestLimitExceeded"
{-# DEPRECATED _RequestLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | An item collection is too large. This exception is only returned for tables that have one or more local secondary indexes.
_ItemCollectionSizeLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ItemCollectionSizeLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ItemCollectionSizeLimitExceededException"
{-# DEPRECATED _ItemCollectionSizeLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | An error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError mkServiceConfig "InternalServerError"
{-# DEPRECATED _InternalServerError "Use generic-lens or generic-optics instead." #-}

-- | A source table with the name @TableName@ does not currently exist within the subscriber's account.
_TableNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TableNotFoundException =
  Core._MatchServiceError mkServiceConfig "TableNotFoundException"
{-# DEPRECATED _TableNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The operation tried to access a nonexistent index.
_IndexNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IndexNotFoundException =
  Core._MatchServiceError mkServiceConfig "IndexNotFoundException"
{-# DEPRECATED _IndexNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Operation was rejected because there is an ongoing transaction for the item.
_TransactionConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TransactionConflictException =
  Core._MatchServiceError
    mkServiceConfig
    "TransactionConflictException"
{-# DEPRECATED _TransactionConflictException "Use generic-lens or generic-optics instead." #-}

-- | There is another ongoing conflicting backup control plane operation on the table. The backup is either being created, deleted or restored to a table.
_BackupInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BackupInUseException =
  Core._MatchServiceError mkServiceConfig "BackupInUseException"
{-# DEPRECATED _BackupInUseException "Use generic-lens or generic-optics instead." #-}

-- | There was an attempt to insert an item with the same primary key as an item that already exists in the DynamoDB table.
_DuplicateItemException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateItemException =
  Core._MatchServiceError mkServiceConfig "DuplicateItemException"
{-# DEPRECATED _DuplicateItemException "Use generic-lens or generic-optics instead." #-}

-- | The specified export was not found.
_ExportNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExportNotFoundException =
  Core._MatchServiceError mkServiceConfig "ExportNotFoundException"
{-# DEPRECATED _ExportNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Point in time recovery has not yet been enabled for this source table.
_PointInTimeRecoveryUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PointInTimeRecoveryUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "PointInTimeRecoveryUnavailableException"
{-# DEPRECATED _PointInTimeRecoveryUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | DynamoDB rejected the request because you retried a request with a different payload but with an idempotent token that was already used.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    mkServiceConfig
    "IdempotentParameterMismatchException"
{-# DEPRECATED _IdempotentParameterMismatchException "Use generic-lens or generic-optics instead." #-}

-- | An invalid restore time was specified. RestoreDateTime must be between EarliestRestorableDateTime and LatestRestorableDateTime.
_InvalidRestoreTimeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRestoreTimeException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRestoreTimeException"
{-# DEPRECATED _InvalidRestoreTimeException "Use generic-lens or generic-optics instead." #-}

-- | The operation tried to access a nonexistent table or index. The resource might not be specified correctly, or its status might not be @ACTIVE@ .
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified replica is already part of the global table.
_ReplicaAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplicaAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ReplicaAlreadyExistsException"
{-# DEPRECATED _ReplicaAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | There is no limit to the number of daily on-demand backups that can be taken.
--
-- Up to 50 simultaneous table operations are allowed per account. These operations include @CreateTable@ , @UpdateTable@ , @DeleteTable@ ,@UpdateTimeToLive@ , @RestoreTableFromBackup@ , and @RestoreTableToPointInTime@ .
-- The only exception is when you are creating a table with one or more secondary indexes. You can have up to 25 such requests running at a time; however, if the table or index specifications are complex, DynamoDB might temporarily reduce the number of concurrent operations.
-- There is a soft account quota of 256 tables.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The operation conflicts with the resource's availability. For example, you attempted to recreate an existing table, or tried to delete a table currently in the @CREATING@ state.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead." #-}
