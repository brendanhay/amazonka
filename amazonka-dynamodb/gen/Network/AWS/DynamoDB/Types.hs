{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types
    (
    -- * Service Configuration
      dynamoDB

    -- * Errors
    , _BackupNotFoundException
    , _TableInUseException
    , _ContinuousBackupsUnavailableException
    , _ProvisionedThroughputExceededException
    , _GlobalTableNotFoundException
    , _TransactionInProgressException
    , _TransactionCanceledException
    , _ConditionalCheckFailedException
    , _GlobalTableAlreadyExistsException
    , _ReplicaNotFoundException
    , _TableAlreadyExistsException
    , _RequestLimitExceeded
    , _ItemCollectionSizeLimitExceededException
    , _InternalServerError
    , _TableNotFoundException
    , _IndexNotFoundException
    , _TransactionConflictException
    , _BackupInUseException
    , _PointInTimeRecoveryUnavailableException
    , _IdempotentParameterMismatchException
    , _InvalidRestoreTimeException
    , _ResourceNotFoundException
    , _ReplicaAlreadyExistsException
    , _LimitExceededException
    , _ResourceInUseException

    -- * AttributeAction
    , AttributeAction (..)

    -- * BackupStatus
    , BackupStatus (..)

    -- * BackupType
    , BackupType (..)

    -- * BackupTypeFilter
    , BackupTypeFilter (..)

    -- * BillingMode
    , BillingMode (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * ConditionalOperator
    , ConditionalOperator (..)

    -- * ContinuousBackupsStatus
    , ContinuousBackupsStatus (..)

    -- * GlobalTableStatus
    , GlobalTableStatus (..)

    -- * IndexStatus
    , IndexStatus (..)

    -- * KeyType
    , KeyType (..)

    -- * PointInTimeRecoveryStatus
    , PointInTimeRecoveryStatus (..)

    -- * ProjectionType
    , ProjectionType (..)

    -- * ReplicaStatus
    , ReplicaStatus (..)

    -- * ReturnConsumedCapacity
    , ReturnConsumedCapacity (..)

    -- * ReturnItemCollectionMetrics
    , ReturnItemCollectionMetrics (..)

    -- * ReturnValue
    , ReturnValue (..)

    -- * ReturnValuesOnConditionCheckFailure
    , ReturnValuesOnConditionCheckFailure (..)

    -- * SSEStatus
    , SSEStatus (..)

    -- * SSEType
    , SSEType (..)

    -- * ScalarAttributeType
    , ScalarAttributeType (..)

    -- * Select
    , Select (..)

    -- * StreamViewType
    , StreamViewType (..)

    -- * TableStatus
    , TableStatus (..)

    -- * TimeToLiveStatus
    , TimeToLiveStatus (..)

    -- * AttributeDefinition
    , AttributeDefinition
    , attributeDefinition
    , adAttributeName
    , adAttributeType

    -- * AttributeValue
    , AttributeValue
    , attributeValue
    , avL
    , avNS
    , avM
    , avNULL
    , avN
    , avBS
    , avB
    , avSS
    , avS
    , avBOOL

    -- * AttributeValueUpdate
    , AttributeValueUpdate
    , attributeValueUpdate
    , avuValue
    , avuAction

    -- * AutoScalingPolicyDescription
    , AutoScalingPolicyDescription
    , autoScalingPolicyDescription
    , aspdPolicyName
    , aspdTargetTrackingScalingPolicyConfiguration

    -- * AutoScalingPolicyUpdate
    , AutoScalingPolicyUpdate
    , autoScalingPolicyUpdate
    , aspuPolicyName
    , aspuTargetTrackingScalingPolicyConfiguration

    -- * AutoScalingSettingsDescription
    , AutoScalingSettingsDescription
    , autoScalingSettingsDescription
    , assdAutoScalingDisabled
    , assdMinimumUnits
    , assdMaximumUnits
    , assdScalingPolicies
    , assdAutoScalingRoleARN

    -- * AutoScalingSettingsUpdate
    , AutoScalingSettingsUpdate
    , autoScalingSettingsUpdate
    , assuAutoScalingDisabled
    , assuMinimumUnits
    , assuScalingPolicyUpdate
    , assuMaximumUnits
    , assuAutoScalingRoleARN

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    , AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    , autoScalingTargetTrackingScalingPolicyConfigurationDescription
    , asttspcdScaleInCooldown
    , asttspcdDisableScaleIn
    , asttspcdScaleOutCooldown
    , asttspcdTargetValue

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    , AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    , autoScalingTargetTrackingScalingPolicyConfigurationUpdate
    , asttspcuScaleInCooldown
    , asttspcuDisableScaleIn
    , asttspcuScaleOutCooldown
    , asttspcuTargetValue

    -- * BackupDescription
    , BackupDescription
    , backupDescription
    , bdBackupDetails
    , bdSourceTableDetails
    , bdSourceTableFeatureDetails

    -- * BackupDetails
    , BackupDetails
    , backupDetails
    , bdBackupExpiryDateTime
    , bdBackupSizeBytes
    , bdBackupARN
    , bdBackupName
    , bdBackupStatus
    , bdBackupType
    , bdBackupCreationDateTime

    -- * BackupSummary
    , BackupSummary
    , backupSummary
    , bsBackupExpiryDateTime
    , bsTableARN
    , bsBackupName
    , bsBackupStatus
    , bsBackupSizeBytes
    , bsBackupARN
    , bsTableId
    , bsBackupCreationDateTime
    , bsBackupType
    , bsTableName

    -- * BillingModeSummary
    , BillingModeSummary
    , billingModeSummary
    , bmsLastUpdateToPayPerRequestDateTime
    , bmsBillingMode

    -- * Capacity
    , Capacity
    , capacity
    , capReadCapacityUnits
    , capCapacityUnits
    , capWriteCapacityUnits

    -- * Condition
    , Condition
    , condition
    , cAttributeValueList
    , cComparisonOperator

    -- * ConditionCheck
    , ConditionCheck
    , conditionCheck
    , ccExpressionAttributeNames
    , ccExpressionAttributeValues
    , ccReturnValuesOnConditionCheckFailure
    , ccKey
    , ccTableName
    , ccConditionExpression

    -- * ConsumedCapacity
    , ConsumedCapacity
    , consumedCapacity
    , cReadCapacityUnits
    , cGlobalSecondaryIndexes
    , cCapacityUnits
    , cWriteCapacityUnits
    , cLocalSecondaryIndexes
    , cTable
    , cTableName

    -- * ContinuousBackupsDescription
    , ContinuousBackupsDescription
    , continuousBackupsDescription
    , cbdPointInTimeRecoveryDescription
    , cbdContinuousBackupsStatus

    -- * CreateGlobalSecondaryIndexAction
    , CreateGlobalSecondaryIndexAction
    , createGlobalSecondaryIndexAction
    , cgsiaProvisionedThroughput
    , cgsiaIndexName
    , cgsiaKeySchema
    , cgsiaProjection

    -- * CreateReplicaAction
    , CreateReplicaAction
    , createReplicaAction
    , craRegionName

    -- * Delete
    , Delete
    , delete'
    , dExpressionAttributeNames
    , dExpressionAttributeValues
    , dReturnValuesOnConditionCheckFailure
    , dConditionExpression
    , dKey
    , dTableName

    -- * DeleteGlobalSecondaryIndexAction
    , DeleteGlobalSecondaryIndexAction
    , deleteGlobalSecondaryIndexAction
    , dgsiaIndexName

    -- * DeleteReplicaAction
    , DeleteReplicaAction
    , deleteReplicaAction
    , draRegionName

    -- * DeleteRequest
    , DeleteRequest
    , deleteRequest
    , drKey

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , eCachePeriodInMinutes

    -- * ExpectedAttributeValue
    , ExpectedAttributeValue
    , expectedAttributeValue
    , eavAttributeValueList
    , eavExists
    , eavValue
    , eavComparisonOperator

    -- * Get
    , Get
    , get'
    , getProjectionExpression
    , getExpressionAttributeNames
    , getKey
    , getTableName

    -- * GlobalSecondaryIndex
    , GlobalSecondaryIndex
    , globalSecondaryIndex
    , gsiProvisionedThroughput
    , gsiIndexName
    , gsiKeySchema
    , gsiProjection

    -- * GlobalSecondaryIndexDescription
    , GlobalSecondaryIndexDescription
    , globalSecondaryIndexDescription
    , gsidBackfilling
    , gsidIndexSizeBytes
    , gsidIndexStatus
    , gsidProvisionedThroughput
    , gsidIndexARN
    , gsidKeySchema
    , gsidProjection
    , gsidItemCount
    , gsidIndexName

    -- * GlobalSecondaryIndexInfo
    , GlobalSecondaryIndexInfo
    , globalSecondaryIndexInfo
    , gsiiProvisionedThroughput
    , gsiiKeySchema
    , gsiiProjection
    , gsiiIndexName

    -- * GlobalSecondaryIndexUpdate
    , GlobalSecondaryIndexUpdate
    , globalSecondaryIndexUpdate
    , gsiuCreate
    , gsiuDelete
    , gsiuUpdate

    -- * GlobalTable
    , GlobalTable
    , globalTable
    , gtGlobalTableName
    , gtReplicationGroup

    -- * GlobalTableDescription
    , GlobalTableDescription
    , globalTableDescription
    , gtdGlobalTableStatus
    , gtdGlobalTableName
    , gtdGlobalTableARN
    , gtdCreationDateTime
    , gtdReplicationGroup

    -- * GlobalTableGlobalSecondaryIndexSettingsUpdate
    , GlobalTableGlobalSecondaryIndexSettingsUpdate
    , globalTableGlobalSecondaryIndexSettingsUpdate
    , gtgsisuProvisionedWriteCapacityUnits
    , gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate
    , gtgsisuIndexName

    -- * ItemCollectionMetrics
    , ItemCollectionMetrics
    , itemCollectionMetrics
    , icmItemCollectionKey
    , icmSizeEstimateRangeGB

    -- * ItemResponse
    , ItemResponse
    , itemResponse
    , iItem

    -- * KeySchemaElement
    , KeySchemaElement
    , keySchemaElement
    , kseAttributeName
    , kseKeyType

    -- * KeysAndAttributes
    , KeysAndAttributes
    , keysAndAttributes
    , kaaProjectionExpression
    , kaaAttributesToGet
    , kaaExpressionAttributeNames
    , kaaConsistentRead
    , kaaKeys

    -- * LocalSecondaryIndex
    , LocalSecondaryIndex
    , localSecondaryIndex
    , lsiIndexName
    , lsiKeySchema
    , lsiProjection

    -- * LocalSecondaryIndexDescription
    , LocalSecondaryIndexDescription
    , localSecondaryIndexDescription
    , lsidIndexSizeBytes
    , lsidIndexARN
    , lsidKeySchema
    , lsidProjection
    , lsidItemCount
    , lsidIndexName

    -- * LocalSecondaryIndexInfo
    , LocalSecondaryIndexInfo
    , localSecondaryIndexInfo
    , lsiiKeySchema
    , lsiiProjection
    , lsiiIndexName

    -- * PointInTimeRecoveryDescription
    , PointInTimeRecoveryDescription
    , pointInTimeRecoveryDescription
    , pitrdPointInTimeRecoveryStatus
    , pitrdEarliestRestorableDateTime
    , pitrdLatestRestorableDateTime

    -- * PointInTimeRecoverySpecification
    , PointInTimeRecoverySpecification
    , pointInTimeRecoverySpecification
    , pitrsPointInTimeRecoveryEnabled

    -- * Projection
    , Projection
    , projection
    , pProjectionType
    , pNonKeyAttributes

    -- * ProvisionedThroughput
    , ProvisionedThroughput
    , provisionedThroughput
    , ptReadCapacityUnits
    , ptWriteCapacityUnits

    -- * ProvisionedThroughputDescription
    , ProvisionedThroughputDescription
    , provisionedThroughputDescription
    , ptdReadCapacityUnits
    , ptdLastDecreaseDateTime
    , ptdWriteCapacityUnits
    , ptdNumberOfDecreasesToday
    , ptdLastIncreaseDateTime

    -- * Put
    , Put
    , put
    , pExpressionAttributeNames
    , pExpressionAttributeValues
    , pReturnValuesOnConditionCheckFailure
    , pConditionExpression
    , pItem
    , pTableName

    -- * PutRequest
    , PutRequest
    , putRequest
    , prItem

    -- * Replica
    , Replica
    , replica
    , rRegionName

    -- * ReplicaDescription
    , ReplicaDescription
    , replicaDescription
    , rdRegionName

    -- * ReplicaGlobalSecondaryIndexSettingsDescription
    , ReplicaGlobalSecondaryIndexSettingsDescription
    , replicaGlobalSecondaryIndexSettingsDescription
    , rgsisdIndexStatus
    , rgsisdProvisionedReadCapacityUnits
    , rgsisdProvisionedWriteCapacityUnits
    , rgsisdProvisionedWriteCapacityAutoScalingSettings
    , rgsisdProvisionedReadCapacityAutoScalingSettings
    , rgsisdIndexName

    -- * ReplicaGlobalSecondaryIndexSettingsUpdate
    , ReplicaGlobalSecondaryIndexSettingsUpdate
    , replicaGlobalSecondaryIndexSettingsUpdate
    , rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate
    , rgsisuProvisionedReadCapacityUnits
    , rgsisuIndexName

    -- * ReplicaSettingsDescription
    , ReplicaSettingsDescription
    , replicaSettingsDescription
    , rsdReplicaStatus
    , rsdReplicaProvisionedReadCapacityUnits
    , rsdReplicaProvisionedWriteCapacityUnits
    , rsdReplicaBillingModeSummary
    , rsdReplicaGlobalSecondaryIndexSettings
    , rsdReplicaProvisionedWriteCapacityAutoScalingSettings
    , rsdReplicaProvisionedReadCapacityAutoScalingSettings
    , rsdRegionName

    -- * ReplicaSettingsUpdate
    , ReplicaSettingsUpdate
    , replicaSettingsUpdate
    , rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate
    , rsuReplicaProvisionedReadCapacityUnits
    , rsuReplicaGlobalSecondaryIndexSettingsUpdate
    , rsuRegionName

    -- * ReplicaUpdate
    , ReplicaUpdate
    , replicaUpdate
    , ruCreate
    , ruDelete

    -- * RestoreSummary
    , RestoreSummary
    , restoreSummary
    , rsSourceTableARN
    , rsSourceBackupARN
    , rsRestoreDateTime
    , rsRestoreInProgress

    -- * SSEDescription
    , SSEDescription
    , sSEDescription
    , ssedStatus
    , ssedSSEType
    , ssedKMSMasterKeyARN

    -- * SSESpecification
    , SSESpecification
    , sSESpecification
    , ssesEnabled
    , ssesKMSMasterKeyId
    , ssesSSEType

    -- * SourceTableDetails
    , SourceTableDetails
    , sourceTableDetails
    , stdTableSizeBytes
    , stdTableARN
    , stdBillingMode
    , stdItemCount
    , stdTableName
    , stdTableId
    , stdKeySchema
    , stdTableCreationDateTime
    , stdProvisionedThroughput

    -- * SourceTableFeatureDetails
    , SourceTableFeatureDetails
    , sourceTableFeatureDetails
    , stfdStreamDescription
    , stfdGlobalSecondaryIndexes
    , stfdLocalSecondaryIndexes
    , stfdSSEDescription
    , stfdTimeToLiveDescription

    -- * StreamSpecification
    , StreamSpecification
    , streamSpecification
    , ssStreamViewType
    , ssStreamEnabled

    -- * TableDescription
    , TableDescription
    , tableDescription
    , tdRestoreSummary
    , tdTableSizeBytes
    , tdAttributeDefinitions
    , tdLatestStreamARN
    , tdProvisionedThroughput
    , tdTableStatus
    , tdTableARN
    , tdKeySchema
    , tdGlobalSecondaryIndexes
    , tdLatestStreamLabel
    , tdBillingModeSummary
    , tdLocalSecondaryIndexes
    , tdCreationDateTime
    , tdSSEDescription
    , tdTableId
    , tdItemCount
    , tdTableName
    , tdStreamSpecification

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * TimeToLiveDescription
    , TimeToLiveDescription
    , timeToLiveDescription
    , ttldTimeToLiveStatus
    , ttldAttributeName

    -- * TimeToLiveSpecification
    , TimeToLiveSpecification
    , timeToLiveSpecification
    , ttlsEnabled
    , ttlsAttributeName

    -- * TransactGetItem
    , TransactGetItem
    , transactGetItem
    , tgiGet

    -- * TransactWriteItem
    , TransactWriteItem
    , transactWriteItem
    , twiConditionCheck
    , twiPut
    , twiDelete
    , twiUpdate

    -- * Update
    , Update
    , update
    , uExpressionAttributeNames
    , uExpressionAttributeValues
    , uReturnValuesOnConditionCheckFailure
    , uConditionExpression
    , uKey
    , uUpdateExpression
    , uTableName

    -- * UpdateGlobalSecondaryIndexAction
    , UpdateGlobalSecondaryIndexAction
    , updateGlobalSecondaryIndexAction
    , ugsiaIndexName
    , ugsiaProvisionedThroughput

    -- * WriteRequest
    , WriteRequest
    , writeRequest
    , wrDeleteRequest
    , wrPutRequest
    ) where

import Network.AWS.DynamoDB.Types.Product
import Network.AWS.DynamoDB.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-08-10@ of the Amazon DynamoDB SDK configuration.
dynamoDB :: Service
dynamoDB =
  Service
    { _svcAbbrev = "DynamoDB"
    , _svcSigner = v4
    , _svcPrefix = "dynamodb"
    , _svcVersion = "2012-08-10"
    , _svcEndpoint = defaultEndpoint dynamoDB
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "DynamoDB"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasCode "ProvisionedThroughputExceededException" . hasStatus 400) e =
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


-- | Backup not found for the given BackupARN.
--
--
_BackupNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_BackupNotFoundException = _MatchServiceError dynamoDB "BackupNotFoundException"


-- | A target table with the specified name is either being created or deleted.
--
--
_TableInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_TableInUseException = _MatchServiceError dynamoDB "TableInUseException"


-- | Backups have not yet been enabled for this table.
--
--
_ContinuousBackupsUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ContinuousBackupsUnavailableException =
  _MatchServiceError dynamoDB "ContinuousBackupsUnavailableException"


-- | Your request rate is too high. The AWS SDKs for DynamoDB automatically retry requests that receive this exception. Your request is eventually successful, unless your retry queue is too large to finish. Reduce the frequency of requests and use exponential backoff. For more information, go to <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Programming.Errors.html#Programming.Errors.RetryAndBackoff Error Retries and Exponential Backoff> in the /Amazon DynamoDB Developer Guide/ .
--
--
_ProvisionedThroughputExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedThroughputExceededException =
  _MatchServiceError dynamoDB "ProvisionedThroughputExceededException"


-- | The specified global table does not exist.
--
--
_GlobalTableNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_GlobalTableNotFoundException =
  _MatchServiceError dynamoDB "GlobalTableNotFoundException"


-- | The transaction with the given request token is already in progress.
--
--
_TransactionInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_TransactionInProgressException =
  _MatchServiceError dynamoDB "TransactionInProgressException"


-- | The entire transaction request was rejected.
--
--
-- DynamoDB rejects a @TransactWriteItems@ request under the following circumstances:
--
--     * A condition in one of the condition expressions is not met.
--
--     * A table in the @TransactWriteItems@ request is in a different account or region.
--
--     * More than one action in the @TransactWriteItems@ operation targets the same item.
--
--     * There is insufficient provisioned capacity for the transaction to be completed.
--
--     * An item size becomes too large (larger than 400 KB), or a local secondary index (LSI) becomes too large, or a similar validation error occurs because of changes made by the transaction.
--
--     * There is a user error, such as an invalid data format.
--
--
--
-- DynamoDB rejects a @TransactGetItems@ request under the following circumstances:
--
--     * There is an ongoing @TransactGetItems@ operation that conflicts with a concurrent @PutItem@ , @UpdateItem@ , @DeleteItem@ or @TransactWriteItems@ request. In this case the @TransactGetItems@ operation fails with a @TransactionCanceledException@ .
--
--     * A table in the @TransactGetItems@ request is in a different account or region.
--
--     * There is insufficient provisioned capacity for the transaction to be completed.
--
--     * There is a user error, such as an invalid data format.
--
--
--
_TransactionCanceledException :: AsError a => Getting (First ServiceError) a ServiceError
_TransactionCanceledException =
  _MatchServiceError dynamoDB "TransactionCanceledException"


-- | A condition specified in the operation could not be evaluated.
--
--
_ConditionalCheckFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_ConditionalCheckFailedException =
  _MatchServiceError dynamoDB "ConditionalCheckFailedException"


-- | The specified global table already exists.
--
--
_GlobalTableAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_GlobalTableAlreadyExistsException =
  _MatchServiceError dynamoDB "GlobalTableAlreadyExistsException"


-- | The specified replica is no longer part of the global table.
--
--
_ReplicaNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicaNotFoundException =
  _MatchServiceError dynamoDB "ReplicaNotFoundException"


-- | A target table with the specified name already exists.
--
--
_TableAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_TableAlreadyExistsException =
  _MatchServiceError dynamoDB "TableAlreadyExistsException"


-- | Throughput exceeds the current throughput limit for your account. Please contact AWS Support at <http://docs.aws.amazon.com/https:/aws.amazon.com/support AWS Support> to request a limit increase.
--
--
_RequestLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_RequestLimitExceeded = _MatchServiceError dynamoDB "RequestLimitExceeded"


-- | An item collection is too large. This exception is only returned for tables that have one or more local secondary indexes.
--
--
_ItemCollectionSizeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ItemCollectionSizeLimitExceededException =
  _MatchServiceError dynamoDB "ItemCollectionSizeLimitExceededException"


-- | An error occurred on the server side.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _MatchServiceError dynamoDB "InternalServerError"


-- | A source table with the name @TableName@ does not currently exist within the subscriber's account.
--
--
_TableNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TableNotFoundException = _MatchServiceError dynamoDB "TableNotFoundException"


-- | The operation tried to access a nonexistent index.
--
--
_IndexNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_IndexNotFoundException = _MatchServiceError dynamoDB "IndexNotFoundException"


-- | Operation was rejected because there is an ongoing transaction for the item.
--
--
_TransactionConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_TransactionConflictException =
  _MatchServiceError dynamoDB "TransactionConflictException"


-- | There is another ongoing conflicting backup control plane operation on the table. The backup is either being created, deleted or restored to a table.
--
--
_BackupInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_BackupInUseException = _MatchServiceError dynamoDB "BackupInUseException"


-- | Point in time recovery has not yet been enabled for this source table.
--
--
_PointInTimeRecoveryUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_PointInTimeRecoveryUnavailableException =
  _MatchServiceError dynamoDB "PointInTimeRecoveryUnavailableException"


-- | DynamoDB rejected the request because you retried a request with a different payload but with an idempotent token that was already used.
--
--
_IdempotentParameterMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_IdempotentParameterMismatchException =
  _MatchServiceError dynamoDB "IdempotentParameterMismatchException"


-- | An invalid restore time was specified. RestoreDateTime must be between EarliestRestorableDateTime and LatestRestorableDateTime.
--
--
_InvalidRestoreTimeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRestoreTimeException =
  _MatchServiceError dynamoDB "InvalidRestoreTimeException"


-- | The operation tried to access a nonexistent table or index. The resource might not be specified correctly, or its status might not be @ACTIVE@ .
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError dynamoDB "ResourceNotFoundException"


-- | The specified replica is already part of the global table.
--
--
_ReplicaAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicaAlreadyExistsException =
  _MatchServiceError dynamoDB "ReplicaAlreadyExistsException"


-- | There is no limit to the number of daily on-demand backups that can be taken.
--
--
-- Up to 10 simultaneous table operations are allowed per account. These operations include @CreateTable@ , @UpdateTable@ , @DeleteTable@ ,@UpdateTimeToLive@ , @RestoreTableFromBackup@ , and @RestoreTableToPointInTime@ .
--
-- For tables with secondary indexes, only one of those tables can be in the @CREATING@ state at any point in time. Do not attempt to create more than one such table simultaneously.
--
-- The total limit of tables in the @ACTIVE@ state is 250.
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError dynamoDB "LimitExceededException"


-- | The operation conflicts with the resource's availability. For example, you attempted to recreate an existing table, or tried to delete a table currently in the @CREATING@ state.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException = _MatchServiceError dynamoDB "ResourceInUseException"

