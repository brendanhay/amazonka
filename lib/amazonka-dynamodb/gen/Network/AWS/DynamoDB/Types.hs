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
    , _ConditionalCheckFailedException
    , _GlobalTableAlreadyExistsException
    , _ReplicaNotFoundException
    , _TableAlreadyExistsException
    , _ItemCollectionSizeLimitExceededException
    , _InternalServerError
    , _TableNotFoundException
    , _IndexNotFoundException
    , _BackupInUseException
    , _PointInTimeRecoveryUnavailableException
    , _InvalidRestoreTimeException
    , _ResourceNotFoundException
    , _ReplicaAlreadyExistsException
    , _LimitExceededException
    , _ResourceInUseException

    -- * AttributeAction
    , AttributeAction (..)

    -- * BackupStatus
    , BackupStatus (..)

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

    -- * SSEStatus
    , SSEStatus (..)

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

    -- * BackupDescription
    , BackupDescription
    , backupDescription
    , bdBackupDetails
    , bdSourceTableDetails
    , bdSourceTableFeatureDetails

    -- * BackupDetails
    , BackupDetails
    , backupDetails
    , bdBackupSizeBytes
    , bdBackupARN
    , bdBackupName
    , bdBackupStatus
    , bdBackupCreationDateTime

    -- * BackupSummary
    , BackupSummary
    , backupSummary
    , bsTableARN
    , bsBackupName
    , bsBackupStatus
    , bsBackupSizeBytes
    , bsBackupARN
    , bsTableId
    , bsBackupCreationDateTime
    , bsTableName

    -- * Capacity
    , Capacity
    , capacity
    , cCapacityUnits

    -- * Condition
    , Condition
    , condition
    , cAttributeValueList
    , cComparisonOperator

    -- * ConsumedCapacity
    , ConsumedCapacity
    , consumedCapacity
    , ccGlobalSecondaryIndexes
    , ccCapacityUnits
    , ccLocalSecondaryIndexes
    , ccTable
    , ccTableName

    -- * ContinuousBackupsDescription
    , ContinuousBackupsDescription
    , continuousBackupsDescription
    , cbdPointInTimeRecoveryDescription
    , cbdContinuousBackupsStatus

    -- * CreateGlobalSecondaryIndexAction
    , CreateGlobalSecondaryIndexAction
    , createGlobalSecondaryIndexAction
    , cgsiaIndexName
    , cgsiaKeySchema
    , cgsiaProjection
    , cgsiaProvisionedThroughput

    -- * CreateReplicaAction
    , CreateReplicaAction
    , createReplicaAction
    , craRegionName

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

    -- * ExpectedAttributeValue
    , ExpectedAttributeValue
    , expectedAttributeValue
    , eavAttributeValueList
    , eavExists
    , eavValue
    , eavComparisonOperator

    -- * GlobalSecondaryIndex
    , GlobalSecondaryIndex
    , globalSecondaryIndex
    , gsiIndexName
    , gsiKeySchema
    , gsiProjection
    , gsiProvisionedThroughput

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
    , gtgsisuIndexName

    -- * ItemCollectionMetrics
    , ItemCollectionMetrics
    , itemCollectionMetrics
    , icmItemCollectionKey
    , icmSizeEstimateRangeGB

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
    , rgsisdIndexName

    -- * ReplicaGlobalSecondaryIndexSettingsUpdate
    , ReplicaGlobalSecondaryIndexSettingsUpdate
    , replicaGlobalSecondaryIndexSettingsUpdate
    , rgsisuProvisionedReadCapacityUnits
    , rgsisuIndexName

    -- * ReplicaSettingsDescription
    , ReplicaSettingsDescription
    , replicaSettingsDescription
    , rsdReplicaStatus
    , rsdReplicaProvisionedReadCapacityUnits
    , rsdReplicaProvisionedWriteCapacityUnits
    , rsdReplicaGlobalSecondaryIndexSettings
    , rsdRegionName

    -- * ReplicaSettingsUpdate
    , ReplicaSettingsUpdate
    , replicaSettingsUpdate
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

    -- * SSESpecification
    , SSESpecification
    , sSESpecification
    , ssesEnabled

    -- * SourceTableDetails
    , SourceTableDetails
    , sourceTableDetails
    , stdTableSizeBytes
    , stdTableARN
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


-- | There is another ongoing conflicting backup control plane operation on the table. The backups is either being created, deleted or restored to a table.
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


-- | Up to 50 @CreateBackup@ operations are allowed per second, per account. There is no limit to the number of daily on-demand backups that can be taken.
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

