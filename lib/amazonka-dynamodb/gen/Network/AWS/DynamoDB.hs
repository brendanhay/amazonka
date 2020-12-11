{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon DynamoDB__
--
-- Amazon DynamoDB is a fully managed NoSQL database service that provides fast and predictable performance with seamless scalability. DynamoDB lets you offload the administrative burdens of operating and scaling a distributed database, so that you don't have to worry about hardware provisioning, setup and configuration, replication, software patching, or cluster scaling.
-- With DynamoDB, you can create database tables that can store and retrieve any amount of data, and serve any level of request traffic. You can scale up or scale down your tables' throughput capacity without downtime or performance degradation, and use the AWS Management Console to monitor resource utilization and performance metrics.
-- DynamoDB automatically spreads the data and traffic for your tables over a sufficient number of servers to handle your throughput and storage requirements, while maintaining consistent and fast performance. All of your data is stored on solid state disks (SSDs) and automatically replicated across multiple Availability Zones in an AWS region, providing built-in high availability and data durability.
module Network.AWS.DynamoDB
  ( -- * Service configuration
    dynamoDBService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** TableNotExists
    mkTableNotExists,

    -- ** TableExists
    mkTableExists,

    -- * Operations
    -- $operations

    -- ** PutItem
    module Network.AWS.DynamoDB.PutItem,

    -- ** DeleteItem
    module Network.AWS.DynamoDB.DeleteItem,

    -- ** UpdateItem
    module Network.AWS.DynamoDB.UpdateItem,

    -- ** DisableKinesisStreamingDestination
    module Network.AWS.DynamoDB.DisableKinesisStreamingDestination,

    -- ** ListGlobalTables
    module Network.AWS.DynamoDB.ListGlobalTables,

    -- ** UpdateGlobalTable
    module Network.AWS.DynamoDB.UpdateGlobalTable,

    -- ** DeleteTable
    module Network.AWS.DynamoDB.DeleteTable,

    -- ** UpdateTable
    module Network.AWS.DynamoDB.UpdateTable,

    -- ** BatchGetItem
    module Network.AWS.DynamoDB.BatchGetItem,

    -- ** ListBackups (Paginated)
    module Network.AWS.DynamoDB.ListBackups,

    -- ** DeleteBackup
    module Network.AWS.DynamoDB.DeleteBackup,

    -- ** CreateBackup
    module Network.AWS.DynamoDB.CreateBackup,

    -- ** UpdateTableReplicaAutoScaling
    module Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling,

    -- ** DescribeGlobalTableSettings
    module Network.AWS.DynamoDB.DescribeGlobalTableSettings,

    -- ** ListTagsOfResource (Paginated)
    module Network.AWS.DynamoDB.ListTagsOfResource,

    -- ** DescribeGlobalTable
    module Network.AWS.DynamoDB.DescribeGlobalTable,

    -- ** DescribeTable
    module Network.AWS.DynamoDB.DescribeTable,

    -- ** DescribeLimits
    module Network.AWS.DynamoDB.DescribeLimits,

    -- ** ExecuteTransaction
    module Network.AWS.DynamoDB.ExecuteTransaction,

    -- ** GetItem
    module Network.AWS.DynamoDB.GetItem,

    -- ** DescribeBackup
    module Network.AWS.DynamoDB.DescribeBackup,

    -- ** BatchExecuteStatement
    module Network.AWS.DynamoDB.BatchExecuteStatement,

    -- ** DescribeTableReplicaAutoScaling
    module Network.AWS.DynamoDB.DescribeTableReplicaAutoScaling,

    -- ** UpdateGlobalTableSettings
    module Network.AWS.DynamoDB.UpdateGlobalTableSettings,

    -- ** EnableKinesisStreamingDestination
    module Network.AWS.DynamoDB.EnableKinesisStreamingDestination,

    -- ** TransactGetItems
    module Network.AWS.DynamoDB.TransactGetItems,

    -- ** ListContributorInsights
    module Network.AWS.DynamoDB.ListContributorInsights,

    -- ** BatchWriteItem
    module Network.AWS.DynamoDB.BatchWriteItem,

    -- ** ExportTableToPointInTime
    module Network.AWS.DynamoDB.ExportTableToPointInTime,

    -- ** TransactWriteItems
    module Network.AWS.DynamoDB.TransactWriteItems,

    -- ** ListTables (Paginated)
    module Network.AWS.DynamoDB.ListTables,

    -- ** Scan (Paginated)
    module Network.AWS.DynamoDB.Scan,

    -- ** UpdateContributorInsights
    module Network.AWS.DynamoDB.UpdateContributorInsights,

    -- ** ExecuteStatement
    module Network.AWS.DynamoDB.ExecuteStatement,

    -- ** Query (Paginated)
    module Network.AWS.DynamoDB.Query,

    -- ** CreateTable
    module Network.AWS.DynamoDB.CreateTable,

    -- ** DescribeKinesisStreamingDestination
    module Network.AWS.DynamoDB.DescribeKinesisStreamingDestination,

    -- ** DescribeEndpoints
    module Network.AWS.DynamoDB.DescribeEndpoints,

    -- ** DescribeTimeToLive
    module Network.AWS.DynamoDB.DescribeTimeToLive,

    -- ** DescribeContinuousBackups
    module Network.AWS.DynamoDB.DescribeContinuousBackups,

    -- ** ListExports
    module Network.AWS.DynamoDB.ListExports,

    -- ** TagResource
    module Network.AWS.DynamoDB.TagResource,

    -- ** DescribeContributorInsights
    module Network.AWS.DynamoDB.DescribeContributorInsights,

    -- ** UntagResource
    module Network.AWS.DynamoDB.UntagResource,

    -- ** RestoreTableToPointInTime
    module Network.AWS.DynamoDB.RestoreTableToPointInTime,

    -- ** RestoreTableFromBackup
    module Network.AWS.DynamoDB.RestoreTableFromBackup,

    -- ** UpdateTimeToLive
    module Network.AWS.DynamoDB.UpdateTimeToLive,

    -- ** CreateGlobalTable
    module Network.AWS.DynamoDB.CreateGlobalTable,

    -- ** UpdateContinuousBackups
    module Network.AWS.DynamoDB.UpdateContinuousBackups,

    -- ** DescribeExport
    module Network.AWS.DynamoDB.DescribeExport,

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
    ArchivalSummary (..),
    mkArchivalSummary,
    asArchivalReason,
    asArchivalDateTime,
    asArchivalBackupARN,

    -- ** AttributeDefinition
    AttributeDefinition (..),
    mkAttributeDefinition,
    adAttributeName,
    adAttributeType,

    -- ** AttributeValue
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

    -- ** AttributeValueUpdate
    AttributeValueUpdate (..),
    mkAttributeValueUpdate,
    avuValue,
    avuAction,

    -- ** AutoScalingPolicyDescription
    AutoScalingPolicyDescription (..),
    mkAutoScalingPolicyDescription,
    aspdPolicyName,
    aspdTargetTrackingScalingPolicyConfiguration,

    -- ** AutoScalingPolicyUpdate
    AutoScalingPolicyUpdate (..),
    mkAutoScalingPolicyUpdate,
    aspuPolicyName,
    aspuTargetTrackingScalingPolicyConfiguration,

    -- ** AutoScalingSettingsDescription
    AutoScalingSettingsDescription (..),
    mkAutoScalingSettingsDescription,
    assdAutoScalingDisabled,
    assdMinimumUnits,
    assdMaximumUnits,
    assdScalingPolicies,
    assdAutoScalingRoleARN,

    -- ** AutoScalingSettingsUpdate
    AutoScalingSettingsUpdate (..),
    mkAutoScalingSettingsUpdate,
    assuAutoScalingDisabled,
    assuMinimumUnits,
    assuScalingPolicyUpdate,
    assuMaximumUnits,
    assuAutoScalingRoleARN,

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription (..),
    mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription,
    asttspcdScaleInCooldown,
    asttspcdDisableScaleIn,
    asttspcdScaleOutCooldown,
    asttspcdTargetValue,

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (..),
    mkAutoScalingTargetTrackingScalingPolicyConfigurationUpdate,
    asttspcuScaleInCooldown,
    asttspcuDisableScaleIn,
    asttspcuScaleOutCooldown,
    asttspcuTargetValue,

    -- ** BackupDescription
    BackupDescription (..),
    mkBackupDescription,
    bdBackupDetails,
    bdSourceTableDetails,
    bdSourceTableFeatureDetails,

    -- ** BackupDetails
    BackupDetails (..),
    mkBackupDetails,
    bdBackupExpiryDateTime,
    bdBackupSizeBytes,
    bdBackupARN,
    bdBackupName,
    bdBackupStatus,
    bdBackupType,
    bdBackupCreationDateTime,

    -- ** BackupSummary
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

    -- ** BatchStatementError
    BatchStatementError (..),
    mkBatchStatementError,
    bseCode,
    bseMessage,

    -- ** BatchStatementRequest
    BatchStatementRequest (..),
    mkBatchStatementRequest,
    bsrConsistentRead,
    bsrParameters,
    bsrStatement,

    -- ** BatchStatementResponse
    BatchStatementResponse (..),
    mkBatchStatementResponse,
    bError,
    bItem,
    bTableName,

    -- ** BillingModeSummary
    BillingModeSummary (..),
    mkBillingModeSummary,
    bmsLastUpdateToPayPerRequestDateTime,
    bmsBillingMode,

    -- ** Capacity
    Capacity (..),
    mkCapacity,
    capReadCapacityUnits,
    capCapacityUnits,
    capWriteCapacityUnits,

    -- ** Condition
    Condition (..),
    mkCondition,
    cAttributeValueList,
    cComparisonOperator,

    -- ** ConditionCheck
    ConditionCheck (..),
    mkConditionCheck,
    ccExpressionAttributeNames,
    ccExpressionAttributeValues,
    ccReturnValuesOnConditionCheckFailure,
    ccKey,
    ccTableName,
    ccConditionExpression,

    -- ** ConsumedCapacity
    ConsumedCapacity (..),
    mkConsumedCapacity,
    cReadCapacityUnits,
    cGlobalSecondaryIndexes,
    cCapacityUnits,
    cWriteCapacityUnits,
    cLocalSecondaryIndexes,
    cTable,
    cTableName,

    -- ** ContinuousBackupsDescription
    ContinuousBackupsDescription (..),
    mkContinuousBackupsDescription,
    cbdPointInTimeRecoveryDescription,
    cbdContinuousBackupsStatus,

    -- ** ContributorInsightsSummary
    ContributorInsightsSummary (..),
    mkContributorInsightsSummary,
    cisContributorInsightsStatus,
    cisTableName,
    cisIndexName,

    -- ** CreateGlobalSecondaryIndexAction
    CreateGlobalSecondaryIndexAction (..),
    mkCreateGlobalSecondaryIndexAction,
    cgsiaProvisionedThroughput,
    cgsiaIndexName,
    cgsiaKeySchema,
    cgsiaProjection,

    -- ** CreateReplicaAction
    CreateReplicaAction (..),
    mkCreateReplicaAction,
    craRegionName,

    -- ** CreateReplicationGroupMemberAction
    CreateReplicationGroupMemberAction (..),
    mkCreateReplicationGroupMemberAction,
    crgmaKMSMasterKeyId,
    crgmaProvisionedThroughputOverride,
    crgmaGlobalSecondaryIndexes,
    crgmaRegionName,

    -- ** Delete
    Delete (..),
    mkDelete,
    dExpressionAttributeNames,
    dExpressionAttributeValues,
    dReturnValuesOnConditionCheckFailure,
    dConditionExpression,
    dKey,
    dTableName,

    -- ** DeleteGlobalSecondaryIndexAction
    DeleteGlobalSecondaryIndexAction (..),
    mkDeleteGlobalSecondaryIndexAction,
    dgsiaIndexName,

    -- ** DeleteReplicaAction
    DeleteReplicaAction (..),
    mkDeleteReplicaAction,
    draRegionName,

    -- ** DeleteReplicationGroupMemberAction
    DeleteReplicationGroupMemberAction (..),
    mkDeleteReplicationGroupMemberAction,
    drgmaRegionName,

    -- ** DeleteRequest
    DeleteRequest (..),
    mkDeleteRequest,
    drKey,

    -- ** Endpoint
    Endpoint (..),
    mkEndpoint,
    eAddress,
    eCachePeriodInMinutes,

    -- ** ExpectedAttributeValue
    ExpectedAttributeValue (..),
    mkExpectedAttributeValue,
    eavAttributeValueList,
    eavExists,
    eavValue,
    eavComparisonOperator,

    -- ** ExportDescription
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

    -- ** ExportSummary
    ExportSummary (..),
    mkExportSummary,
    esExportStatus,
    esExportARN,

    -- ** FailureException
    FailureException (..),
    mkFailureException,
    feExceptionName,
    feExceptionDescription,

    -- ** Get
    Get (..),
    mkGet,
    gProjectionExpression,
    gExpressionAttributeNames,
    gKey,
    gTableName,

    -- ** GlobalSecondaryIndex
    GlobalSecondaryIndex (..),
    mkGlobalSecondaryIndex,
    gsiProvisionedThroughput,
    gsiIndexName,
    gsiKeySchema,
    gsiProjection,

    -- ** GlobalSecondaryIndexAutoScalingUpdate
    GlobalSecondaryIndexAutoScalingUpdate (..),
    mkGlobalSecondaryIndexAutoScalingUpdate,
    gsiasuProvisionedWriteCapacityAutoScalingUpdate,
    gsiasuIndexName,

    -- ** GlobalSecondaryIndexDescription
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

    -- ** GlobalSecondaryIndexInfo
    GlobalSecondaryIndexInfo (..),
    mkGlobalSecondaryIndexInfo,
    gsiiProvisionedThroughput,
    gsiiKeySchema,
    gsiiProjection,
    gsiiIndexName,

    -- ** GlobalSecondaryIndexUpdate
    GlobalSecondaryIndexUpdate (..),
    mkGlobalSecondaryIndexUpdate,
    gsiuCreate,
    gsiuDelete,
    gsiuUpdate,

    -- ** GlobalTable
    GlobalTable (..),
    mkGlobalTable,
    gtGlobalTableName,
    gtReplicationGroup,

    -- ** GlobalTableDescription
    GlobalTableDescription (..),
    mkGlobalTableDescription,
    gtdGlobalTableStatus,
    gtdGlobalTableName,
    gtdGlobalTableARN,
    gtdCreationDateTime,
    gtdReplicationGroup,

    -- ** GlobalTableGlobalSecondaryIndexSettingsUpdate
    GlobalTableGlobalSecondaryIndexSettingsUpdate (..),
    mkGlobalTableGlobalSecondaryIndexSettingsUpdate,
    gtgsisuProvisionedWriteCapacityUnits,
    gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate,
    gtgsisuIndexName,

    -- ** ItemCollectionMetrics
    ItemCollectionMetrics (..),
    mkItemCollectionMetrics,
    icmItemCollectionKey,
    icmSizeEstimateRangeGB,

    -- ** ItemResponse
    ItemResponse (..),
    mkItemResponse,
    iItem,

    -- ** KeySchemaElement
    KeySchemaElement (..),
    mkKeySchemaElement,
    kseAttributeName,
    kseKeyType,

    -- ** KeysAndAttributes
    KeysAndAttributes (..),
    mkKeysAndAttributes,
    kaaProjectionExpression,
    kaaAttributesToGet,
    kaaExpressionAttributeNames,
    kaaConsistentRead,
    kaaKeys,

    -- ** KinesisDataStreamDestination
    KinesisDataStreamDestination (..),
    mkKinesisDataStreamDestination,
    kdsdDestinationStatus,
    kdsdStreamARN,
    kdsdDestinationStatusDescription,

    -- ** KinesisStreamingDestinationInput
    KinesisStreamingDestinationInput (..),
    mkKinesisStreamingDestinationInput,
    ksdiTableName,
    ksdiStreamARN,

    -- ** KinesisStreamingDestinationOutput
    KinesisStreamingDestinationOutput (..),
    mkKinesisStreamingDestinationOutput,
    ksdoDestinationStatus,
    ksdoStreamARN,
    ksdoTableName,

    -- ** LocalSecondaryIndex
    LocalSecondaryIndex (..),
    mkLocalSecondaryIndex,
    lsiIndexName,
    lsiKeySchema,
    lsiProjection,

    -- ** LocalSecondaryIndexDescription
    LocalSecondaryIndexDescription (..),
    mkLocalSecondaryIndexDescription,
    lsidIndexSizeBytes,
    lsidIndexARN,
    lsidKeySchema,
    lsidProjection,
    lsidItemCount,
    lsidIndexName,

    -- ** LocalSecondaryIndexInfo
    LocalSecondaryIndexInfo (..),
    mkLocalSecondaryIndexInfo,
    lsiiKeySchema,
    lsiiProjection,
    lsiiIndexName,

    -- ** ParameterizedStatement
    ParameterizedStatement (..),
    mkParameterizedStatement,
    psParameters,
    psStatement,

    -- ** PointInTimeRecoveryDescription
    PointInTimeRecoveryDescription (..),
    mkPointInTimeRecoveryDescription,
    pitrdPointInTimeRecoveryStatus,
    pitrdEarliestRestorableDateTime,
    pitrdLatestRestorableDateTime,

    -- ** PointInTimeRecoverySpecification
    PointInTimeRecoverySpecification (..),
    mkPointInTimeRecoverySpecification,
    pitrsPointInTimeRecoveryEnabled,

    -- ** Projection
    Projection (..),
    mkProjection,
    pProjectionType,
    pNonKeyAttributes,

    -- ** ProvisionedThroughput
    ProvisionedThroughput (..),
    mkProvisionedThroughput,
    ptReadCapacityUnits,
    ptWriteCapacityUnits,

    -- ** ProvisionedThroughputDescription
    ProvisionedThroughputDescription (..),
    mkProvisionedThroughputDescription,
    ptdReadCapacityUnits,
    ptdLastDecreaseDateTime,
    ptdWriteCapacityUnits,
    ptdNumberOfDecreasesToday,
    ptdLastIncreaseDateTime,

    -- ** ProvisionedThroughputOverride
    ProvisionedThroughputOverride (..),
    mkProvisionedThroughputOverride,
    ptoReadCapacityUnits,

    -- ** Put
    Put (..),
    mkPut,
    pExpressionAttributeNames,
    pExpressionAttributeValues,
    pReturnValuesOnConditionCheckFailure,
    pConditionExpression,
    pItem,
    pTableName,

    -- ** PutRequest
    PutRequest (..),
    mkPutRequest,
    prItem,

    -- ** Replica
    Replica (..),
    mkReplica,
    rRegionName,

    -- ** ReplicaAutoScalingDescription
    ReplicaAutoScalingDescription (..),
    mkReplicaAutoScalingDescription,
    rasdReplicaStatus,
    rasdRegionName,
    rasdGlobalSecondaryIndexes,
    rasdReplicaProvisionedWriteCapacityAutoScalingSettings,
    rasdReplicaProvisionedReadCapacityAutoScalingSettings,

    -- ** ReplicaAutoScalingUpdate
    ReplicaAutoScalingUpdate (..),
    mkReplicaAutoScalingUpdate,
    rasuReplicaProvisionedReadCapacityAutoScalingUpdate,
    rasuReplicaGlobalSecondaryIndexUpdates,
    rasuRegionName,

    -- ** ReplicaDescription
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

    -- ** ReplicaGlobalSecondaryIndex
    ReplicaGlobalSecondaryIndex (..),
    mkReplicaGlobalSecondaryIndex,
    rgsiProvisionedThroughputOverride,
    rgsiIndexName,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingDescription
    ReplicaGlobalSecondaryIndexAutoScalingDescription (..),
    mkReplicaGlobalSecondaryIndexAutoScalingDescription,
    rgsiasdIndexStatus,
    rgsiasdProvisionedWriteCapacityAutoScalingSettings,
    rgsiasdProvisionedReadCapacityAutoScalingSettings,
    rgsiasdIndexName,

    -- ** ReplicaGlobalSecondaryIndexAutoScalingUpdate
    ReplicaGlobalSecondaryIndexAutoScalingUpdate (..),
    mkReplicaGlobalSecondaryIndexAutoScalingUpdate,
    rgsiasuProvisionedReadCapacityAutoScalingUpdate,
    rgsiasuIndexName,

    -- ** ReplicaGlobalSecondaryIndexDescription
    ReplicaGlobalSecondaryIndexDescription (..),
    mkReplicaGlobalSecondaryIndexDescription,
    rgsidProvisionedThroughputOverride,
    rgsidIndexName,

    -- ** ReplicaGlobalSecondaryIndexSettingsDescription
    ReplicaGlobalSecondaryIndexSettingsDescription (..),
    mkReplicaGlobalSecondaryIndexSettingsDescription,
    rgsisdIndexStatus,
    rgsisdProvisionedReadCapacityUnits,
    rgsisdProvisionedWriteCapacityUnits,
    rgsisdProvisionedWriteCapacityAutoScalingSettings,
    rgsisdProvisionedReadCapacityAutoScalingSettings,
    rgsisdIndexName,

    -- ** ReplicaGlobalSecondaryIndexSettingsUpdate
    ReplicaGlobalSecondaryIndexSettingsUpdate (..),
    mkReplicaGlobalSecondaryIndexSettingsUpdate,
    rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate,
    rgsisuProvisionedReadCapacityUnits,
    rgsisuIndexName,

    -- ** ReplicaSettingsDescription
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

    -- ** ReplicaSettingsUpdate
    ReplicaSettingsUpdate (..),
    mkReplicaSettingsUpdate,
    rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    rsuReplicaProvisionedReadCapacityUnits,
    rsuReplicaGlobalSecondaryIndexSettingsUpdate,
    rsuRegionName,

    -- ** ReplicaUpdate
    ReplicaUpdate (..),
    mkReplicaUpdate,
    ruCreate,
    ruDelete,

    -- ** ReplicationGroupUpdate
    ReplicationGroupUpdate (..),
    mkReplicationGroupUpdate,
    rguCreate,
    rguDelete,
    rguUpdate,

    -- ** RestoreSummary
    RestoreSummary (..),
    mkRestoreSummary,
    rsSourceTableARN,
    rsSourceBackupARN,
    rsRestoreDateTime,
    rsRestoreInProgress,

    -- ** SSEDescription
    SSEDescription (..),
    mkSSEDescription,
    ssedStatus,
    ssedInaccessibleEncryptionDateTime,
    ssedSSEType,
    ssedKMSMasterKeyARN,

    -- ** SSESpecification
    SSESpecification (..),
    mkSSESpecification,
    ssesEnabled,
    ssesKMSMasterKeyId,
    ssesSSEType,

    -- ** SourceTableDetails
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

    -- ** SourceTableFeatureDetails
    SourceTableFeatureDetails (..),
    mkSourceTableFeatureDetails,
    stfdStreamDescription,
    stfdGlobalSecondaryIndexes,
    stfdLocalSecondaryIndexes,
    stfdSSEDescription,
    stfdTimeToLiveDescription,

    -- ** StreamSpecification
    StreamSpecification (..),
    mkStreamSpecification,
    ssStreamViewType,
    ssStreamEnabled,

    -- ** TableAutoScalingDescription
    TableAutoScalingDescription (..),
    mkTableAutoScalingDescription,
    tasdTableStatus,
    tasdReplicas,
    tasdTableName,

    -- ** TableDescription
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

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** TimeToLiveDescription
    TimeToLiveDescription (..),
    mkTimeToLiveDescription,
    ttldTimeToLiveStatus,
    ttldAttributeName,

    -- ** TimeToLiveSpecification
    TimeToLiveSpecification (..),
    mkTimeToLiveSpecification,
    ttlsEnabled,
    ttlsAttributeName,

    -- ** TransactGetItem
    TransactGetItem (..),
    mkTransactGetItem,
    tgiGet,

    -- ** TransactWriteItem
    TransactWriteItem (..),
    mkTransactWriteItem,
    twiConditionCheck,
    twiPut,
    twiDelete,
    twiUpdate,

    -- ** Update
    Update (..),
    mkUpdate,
    uExpressionAttributeNames,
    uExpressionAttributeValues,
    uReturnValuesOnConditionCheckFailure,
    uConditionExpression,
    uKey,
    uUpdateExpression,
    uTableName,

    -- ** UpdateGlobalSecondaryIndexAction
    UpdateGlobalSecondaryIndexAction (..),
    mkUpdateGlobalSecondaryIndexAction,
    ugsiaIndexName,
    ugsiaProvisionedThroughput,

    -- ** UpdateReplicationGroupMemberAction
    UpdateReplicationGroupMemberAction (..),
    mkUpdateReplicationGroupMemberAction,
    urgmaKMSMasterKeyId,
    urgmaProvisionedThroughputOverride,
    urgmaGlobalSecondaryIndexes,
    urgmaRegionName,

    -- ** WriteRequest
    WriteRequest (..),
    mkWriteRequest,
    wrDeleteRequest,
    wrPutRequest,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
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
import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.UntagResource
import Network.AWS.DynamoDB.UpdateContinuousBackups
import Network.AWS.DynamoDB.UpdateContributorInsights
import Network.AWS.DynamoDB.UpdateGlobalTable
import Network.AWS.DynamoDB.UpdateGlobalTableSettings
import Network.AWS.DynamoDB.UpdateItem
import Network.AWS.DynamoDB.UpdateTable
import Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
import Network.AWS.DynamoDB.UpdateTimeToLive
import Network.AWS.DynamoDB.Waiters
import qualified Network.AWS.Prelude as Lude

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
