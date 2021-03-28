{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

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
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidExportTimeException
    , _InvalidExportTimeException

    -- ** BackupNotFoundException
    , _BackupNotFoundException

    -- ** TableInUseException
    , _TableInUseException

    -- ** ExportConflictException
    , _ExportConflictException

    -- ** ContinuousBackupsUnavailableException
    , _ContinuousBackupsUnavailableException

    -- ** ProvisionedThroughputExceededException
    , _ProvisionedThroughputExceededException

    -- ** GlobalTableNotFoundException
    , _GlobalTableNotFoundException

    -- ** TransactionInProgressException
    , _TransactionInProgressException

    -- ** TransactionCanceledException
    , _TransactionCanceledException

    -- ** ConditionalCheckFailedException
    , _ConditionalCheckFailedException

    -- ** GlobalTableAlreadyExistsException
    , _GlobalTableAlreadyExistsException

    -- ** ReplicaNotFoundException
    , _ReplicaNotFoundException

    -- ** TableAlreadyExistsException
    , _TableAlreadyExistsException

    -- ** RequestLimitExceeded
    , _RequestLimitExceeded

    -- ** ItemCollectionSizeLimitExceededException
    , _ItemCollectionSizeLimitExceededException

    -- ** InternalServerError
    , _InternalServerError

    -- ** TableNotFoundException
    , _TableNotFoundException

    -- ** IndexNotFoundException
    , _IndexNotFoundException

    -- ** TransactionConflictException
    , _TransactionConflictException

    -- ** BackupInUseException
    , _BackupInUseException

    -- ** DuplicateItemException
    , _DuplicateItemException

    -- ** ExportNotFoundException
    , _ExportNotFoundException

    -- ** PointInTimeRecoveryUnavailableException
    , _PointInTimeRecoveryUnavailableException

    -- ** IdempotentParameterMismatchException
    , _IdempotentParameterMismatchException

    -- ** InvalidRestoreTimeException
    , _InvalidRestoreTimeException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** ReplicaAlreadyExistsException
    , _ReplicaAlreadyExistsException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- ** TableNotExists
    , mkTableNotExists

    -- ** TableExists
    , mkTableExists

    -- * Operations
    -- $operations

    -- ** PutItem 
    , module Network.AWS.DynamoDB.PutItem

    -- ** DeleteItem 
    , module Network.AWS.DynamoDB.DeleteItem

    -- ** UpdateItem 
    , module Network.AWS.DynamoDB.UpdateItem

    -- ** DisableKinesisStreamingDestination 
    , module Network.AWS.DynamoDB.DisableKinesisStreamingDestination

    -- ** ListGlobalTables 
    , module Network.AWS.DynamoDB.ListGlobalTables

    -- ** UpdateGlobalTable 
    , module Network.AWS.DynamoDB.UpdateGlobalTable

    -- ** DeleteTable 
    , module Network.AWS.DynamoDB.DeleteTable

    -- ** UpdateTable 
    , module Network.AWS.DynamoDB.UpdateTable

    -- ** BatchGetItem 
    , module Network.AWS.DynamoDB.BatchGetItem

    -- ** ListBackups (Paginated)
    , module Network.AWS.DynamoDB.ListBackups

    -- ** DeleteBackup 
    , module Network.AWS.DynamoDB.DeleteBackup

    -- ** CreateBackup 
    , module Network.AWS.DynamoDB.CreateBackup

    -- ** UpdateTableReplicaAutoScaling 
    , module Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling

    -- ** DescribeGlobalTableSettings 
    , module Network.AWS.DynamoDB.DescribeGlobalTableSettings

    -- ** ListTagsOfResource (Paginated)
    , module Network.AWS.DynamoDB.ListTagsOfResource

    -- ** DescribeGlobalTable 
    , module Network.AWS.DynamoDB.DescribeGlobalTable

    -- ** DescribeTable 
    , module Network.AWS.DynamoDB.DescribeTable

    -- ** DescribeLimits 
    , module Network.AWS.DynamoDB.DescribeLimits

    -- ** ExecuteTransaction 
    , module Network.AWS.DynamoDB.ExecuteTransaction

    -- ** GetItem 
    , module Network.AWS.DynamoDB.GetItem

    -- ** DescribeBackup 
    , module Network.AWS.DynamoDB.DescribeBackup

    -- ** BatchExecuteStatement 
    , module Network.AWS.DynamoDB.BatchExecuteStatement

    -- ** DescribeTableReplicaAutoScaling 
    , module Network.AWS.DynamoDB.DescribeTableReplicaAutoScaling

    -- ** UpdateGlobalTableSettings 
    , module Network.AWS.DynamoDB.UpdateGlobalTableSettings

    -- ** EnableKinesisStreamingDestination 
    , module Network.AWS.DynamoDB.EnableKinesisStreamingDestination

    -- ** TransactGetItems 
    , module Network.AWS.DynamoDB.TransactGetItems

    -- ** ListContributorInsights 
    , module Network.AWS.DynamoDB.ListContributorInsights

    -- ** BatchWriteItem 
    , module Network.AWS.DynamoDB.BatchWriteItem

    -- ** ExportTableToPointInTime 
    , module Network.AWS.DynamoDB.ExportTableToPointInTime

    -- ** TransactWriteItems 
    , module Network.AWS.DynamoDB.TransactWriteItems

    -- ** ListTables (Paginated)
    , module Network.AWS.DynamoDB.ListTables

    -- ** Scan (Paginated)
    , module Network.AWS.DynamoDB.Scan

    -- ** UpdateContributorInsights 
    , module Network.AWS.DynamoDB.UpdateContributorInsights

    -- ** ExecuteStatement 
    , module Network.AWS.DynamoDB.ExecuteStatement

    -- ** Query (Paginated)
    , module Network.AWS.DynamoDB.Query

    -- ** CreateTable 
    , module Network.AWS.DynamoDB.CreateTable

    -- ** DescribeKinesisStreamingDestination 
    , module Network.AWS.DynamoDB.DescribeKinesisStreamingDestination

    -- ** DescribeEndpoints 
    , module Network.AWS.DynamoDB.DescribeEndpoints

    -- ** DescribeTimeToLive 
    , module Network.AWS.DynamoDB.DescribeTimeToLive

    -- ** DescribeContinuousBackups 
    , module Network.AWS.DynamoDB.DescribeContinuousBackups

    -- ** ListExports 
    , module Network.AWS.DynamoDB.ListExports

    -- ** TagResource 
    , module Network.AWS.DynamoDB.TagResource

    -- ** DescribeContributorInsights 
    , module Network.AWS.DynamoDB.DescribeContributorInsights

    -- ** UntagResource 
    , module Network.AWS.DynamoDB.UntagResource

    -- ** RestoreTableToPointInTime 
    , module Network.AWS.DynamoDB.RestoreTableToPointInTime

    -- ** RestoreTableFromBackup 
    , module Network.AWS.DynamoDB.RestoreTableFromBackup

    -- ** UpdateTimeToLive 
    , module Network.AWS.DynamoDB.UpdateTimeToLive

    -- ** CreateGlobalTable 
    , module Network.AWS.DynamoDB.CreateGlobalTable

    -- ** UpdateContinuousBackups 
    , module Network.AWS.DynamoDB.UpdateContinuousBackups

    -- ** DescribeExport 
    , module Network.AWS.DynamoDB.DescribeExport

    -- * Types

    -- ** TimeToLiveStatus
    , TimeToLiveStatus (..)

    -- ** BackupDetails
    , BackupDetails (..)
    , mkBackupDetails
    , bdBackupArn
    , bdBackupName
    , bdBackupStatus
    , bdBackupType
    , bdBackupCreationDateTime
    , bdBackupExpiryDateTime
    , bdBackupSizeBytes

    -- ** ReplicaAutoScalingDescription
    , ReplicaAutoScalingDescription (..)
    , mkReplicaAutoScalingDescription
    , rasdGlobalSecondaryIndexes
    , rasdRegionName
    , rasdReplicaProvisionedReadCapacityAutoScalingSettings
    , rasdReplicaProvisionedWriteCapacityAutoScalingSettings
    , rasdReplicaStatus

    -- ** TransactWriteItem
    , TransactWriteItem (..)
    , mkTransactWriteItem
    , twiConditionCheck
    , twiDelete
    , twiPut
    , twiUpdate

    -- ** WriteRequest
    , WriteRequest (..)
    , mkWriteRequest
    , wrDeleteRequest
    , wrPutRequest

    -- ** ProvisionedThroughputDescription
    , ProvisionedThroughputDescription (..)
    , mkProvisionedThroughputDescription
    , ptdLastDecreaseDateTime
    , ptdLastIncreaseDateTime
    , ptdNumberOfDecreasesToday
    , ptdReadCapacityUnits
    , ptdWriteCapacityUnits

    -- ** RestoreSummary
    , RestoreSummary (..)
    , mkRestoreSummary
    , rsRestoreDateTime
    , rsRestoreInProgress
    , rsSourceBackupArn
    , rsSourceTableArn

    -- ** TagValueString
    , TagValueString (..)

    -- ** DeleteReplicationGroupMemberAction
    , DeleteReplicationGroupMemberAction (..)
    , mkDeleteReplicationGroupMemberAction
    , drgmaRegionName

    -- ** UpdateReplicationGroupMemberAction
    , UpdateReplicationGroupMemberAction (..)
    , mkUpdateReplicationGroupMemberAction
    , urgmaRegionName
    , urgmaGlobalSecondaryIndexes
    , urgmaKMSMasterKeyId
    , urgmaProvisionedThroughputOverride

    -- ** ReplicaStatus
    , ReplicaStatus (..)

    -- ** KinesisStreamingDestinationOutput
    , KinesisStreamingDestinationOutput (..)
    , mkKinesisStreamingDestinationOutput
    , ksdoDestinationStatus
    , ksdoStreamArn
    , ksdoTableName

    -- ** LocalSecondaryIndexInfo
    , LocalSecondaryIndexInfo (..)
    , mkLocalSecondaryIndexInfo
    , lsiiIndexName
    , lsiiKeySchema
    , lsiiProjection

    -- ** S3BucketOwner
    , S3BucketOwner (..)

    -- ** ProjectionExpression
    , ProjectionExpression (..)

    -- ** KeyType
    , KeyType (..)

    -- ** ExportFormat
    , ExportFormat (..)

    -- ** AttributeValue
    , AttributeValue (..)
    , mkAttributeValue
    , avB
    , avBOOL
    , avBS
    , avL
    , avM
    , avN
    , avNS
    , avNULL
    , avS
    , avSS

    -- ** RegionName
    , RegionName (..)

    -- ** TableAutoScalingDescription
    , TableAutoScalingDescription (..)
    , mkTableAutoScalingDescription
    , tasdReplicas
    , tasdTableName
    , tasdTableStatus

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** ReplicaStatusPercentProgress
    , ReplicaStatusPercentProgress (..)

    -- ** StringAttributeValue
    , StringAttributeValue (..)

    -- ** BatchStatementResponse
    , BatchStatementResponse (..)
    , mkBatchStatementResponse
    , bsrError
    , bsrItem
    , bsrTableName

    -- ** ReplicaStatusDescription
    , ReplicaStatusDescription (..)

    -- ** ExportDescription
    , ExportDescription (..)
    , mkExportDescription
    , edBilledSizeBytes
    , edClientToken
    , edEndTime
    , edExportArn
    , edExportFormat
    , edExportManifest
    , edExportStatus
    , edExportTime
    , edFailureCode
    , edFailureMessage
    , edItemCount
    , edS3Bucket
    , edS3BucketOwner
    , edS3Prefix
    , edS3SseAlgorithm
    , edS3SseKmsKeyId
    , edStartTime
    , edTableArn
    , edTableId

    -- ** CreateReplicaAction
    , CreateReplicaAction (..)
    , mkCreateReplicaAction
    , craRegionName

    -- ** IndexStatus
    , IndexStatus (..)

    -- ** S3SseKmsKeyId
    , S3SseKmsKeyId (..)

    -- ** ProvisionedThroughput
    , ProvisionedThroughput (..)
    , mkProvisionedThroughput
    , ptReadCapacityUnits
    , ptWriteCapacityUnits

    -- ** ClientToken
    , ClientToken (..)

    -- ** ReplicationGroupUpdate
    , ReplicationGroupUpdate (..)
    , mkReplicationGroupUpdate
    , rguCreate
    , rguDelete
    , rguUpdate

    -- ** TableStatus
    , TableStatus (..)

    -- ** ProjectionType
    , ProjectionType (..)

    -- ** GlobalTable
    , GlobalTable (..)
    , mkGlobalTable
    , gtGlobalTableName
    , gtReplicationGroup

    -- ** FailureCode
    , FailureCode (..)

    -- ** AutoScalingPolicyName
    , AutoScalingPolicyName (..)

    -- ** KeySchemaAttributeName
    , KeySchemaAttributeName (..)

    -- ** ContributorInsightsAction
    , ContributorInsightsAction (..)

    -- ** ExportStatus
    , ExportStatus (..)

    -- ** ExportSummary
    , ExportSummary (..)
    , mkExportSummary
    , esExportArn
    , esExportStatus

    -- ** Replica
    , Replica (..)
    , mkReplica
    , rRegionName

    -- ** ContributorInsightsRule
    , ContributorInsightsRule (..)

    -- ** BatchStatementRequest
    , BatchStatementRequest (..)
    , mkBatchStatementRequest
    , bsrStatement
    , bsrConsistentRead
    , bsrParameters

    -- ** DestinationStatus
    , DestinationStatus (..)

    -- ** TableDescription
    , TableDescription (..)
    , mkTableDescription
    , tdArchivalSummary
    , tdAttributeDefinitions
    , tdBillingModeSummary
    , tdCreationDateTime
    , tdGlobalSecondaryIndexes
    , tdGlobalTableVersion
    , tdItemCount
    , tdKeySchema
    , tdLatestStreamArn
    , tdLatestStreamLabel
    , tdLocalSecondaryIndexes
    , tdProvisionedThroughput
    , tdReplicas
    , tdRestoreSummary
    , tdSSEDescription
    , tdStreamSpecification
    , tdTableArn
    , tdTableId
    , tdTableName
    , tdTableSizeBytes
    , tdTableStatus

    -- ** SSESpecification
    , SSESpecification (..)
    , mkSSESpecification
    , ssesEnabled
    , ssesKMSMasterKeyId
    , ssesSSEType

    -- ** KeysAndAttributes
    , KeysAndAttributes (..)
    , mkKeysAndAttributes
    , kaaKeys
    , kaaAttributesToGet
    , kaaConsistentRead
    , kaaExpressionAttributeNames
    , kaaProjectionExpression

    -- ** PointInTimeRecoverySpecification
    , PointInTimeRecoverySpecification (..)
    , mkPointInTimeRecoverySpecification
    , pitrsPointInTimeRecoveryEnabled

    -- ** UpdateExpression
    , UpdateExpression (..)

    -- ** FailureMessage
    , FailureMessage (..)

    -- ** TableArn
    , TableArn (..)

    -- ** KMSMasterKeyId
    , KMSMasterKeyId (..)

    -- ** ReplicaGlobalSecondaryIndexSettingsUpdate
    , ReplicaGlobalSecondaryIndexSettingsUpdate (..)
    , mkReplicaGlobalSecondaryIndexSettingsUpdate
    , rgsisuIndexName
    , rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate
    , rgsisuProvisionedReadCapacityUnits

    -- ** AutoScalingPolicyUpdate
    , AutoScalingPolicyUpdate (..)
    , mkAutoScalingPolicyUpdate
    , aspuTargetTrackingScalingPolicyConfiguration
    , aspuPolicyName

    -- ** ExpressionAttributeValueVariable
    , ExpressionAttributeValueVariable (..)

    -- ** ArchivalReason
    , ArchivalReason (..)

    -- ** ItemResponse
    , ItemResponse (..)
    , mkItemResponse
    , irItem

    -- ** ReturnConsumedCapacity
    , ReturnConsumedCapacity (..)

    -- ** AutoScalingSettingsUpdate
    , AutoScalingSettingsUpdate (..)
    , mkAutoScalingSettingsUpdate
    , assuAutoScalingDisabled
    , assuAutoScalingRoleArn
    , assuMaximumUnits
    , assuMinimumUnits
    , assuScalingPolicyUpdate

    -- ** ReturnValuesOnConditionCheckFailure
    , ReturnValuesOnConditionCheckFailure (..)

    -- ** ProvisionedThroughputOverride
    , ProvisionedThroughputOverride (..)
    , mkProvisionedThroughputOverride
    , ptoReadCapacityUnits

    -- ** BackupTypeFilter
    , BackupTypeFilter (..)

    -- ** Get
    , Get (..)
    , mkGet
    , gKey
    , gTableName
    , gExpressionAttributeNames
    , gProjectionExpression

    -- ** ParameterizedStatement
    , ParameterizedStatement (..)
    , mkParameterizedStatement
    , psStatement
    , psParameters

    -- ** ReturnItemCollectionMetrics
    , ReturnItemCollectionMetrics (..)

    -- ** AttributeValueUpdate
    , AttributeValueUpdate (..)
    , mkAttributeValueUpdate
    , avuAction
    , avuValue

    -- ** ContinuousBackupsDescription
    , ContinuousBackupsDescription (..)
    , mkContinuousBackupsDescription
    , cbdContinuousBackupsStatus
    , cbdPointInTimeRecoveryDescription

    -- ** SourceTableDetails
    , SourceTableDetails (..)
    , mkSourceTableDetails
    , stdTableName
    , stdTableId
    , stdKeySchema
    , stdTableCreationDateTime
    , stdProvisionedThroughput
    , stdBillingMode
    , stdItemCount
    , stdTableArn
    , stdTableSizeBytes

    -- ** ExpectedAttributeValue
    , ExpectedAttributeValue (..)
    , mkExpectedAttributeValue
    , eavAttributeValueList
    , eavComparisonOperator
    , eavExists
    , eavValue

    -- ** SSEStatus
    , SSEStatus (..)

    -- ** BackupName
    , BackupName (..)

    -- ** StreamViewType
    , StreamViewType (..)

    -- ** ConditionExpression
    , ConditionExpression (..)

    -- ** GlobalTableStatus
    , GlobalTableStatus (..)

    -- ** StreamArn
    , StreamArn (..)

    -- ** AttributeDefinition
    , AttributeDefinition (..)
    , mkAttributeDefinition
    , adAttributeName
    , adAttributeType

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** ExportArn
    , ExportArn (..)

    -- ** CreateReplicationGroupMemberAction
    , CreateReplicationGroupMemberAction (..)
    , mkCreateReplicationGroupMemberAction
    , crgmaRegionName
    , crgmaGlobalSecondaryIndexes
    , crgmaKMSMasterKeyId
    , crgmaProvisionedThroughputOverride

    -- ** ReturnValue
    , ReturnValue (..)

    -- ** ReplicaGlobalSecondaryIndexDescription
    , ReplicaGlobalSecondaryIndexDescription (..)
    , mkReplicaGlobalSecondaryIndexDescription
    , rgsidIndexName
    , rgsidProvisionedThroughputOverride

    -- ** LocalSecondaryIndex
    , LocalSecondaryIndex (..)
    , mkLocalSecondaryIndex
    , lsiIndexName
    , lsiKeySchema
    , lsiProjection

    -- ** PointInTimeRecoveryStatus
    , PointInTimeRecoveryStatus (..)

    -- ** GlobalSecondaryIndexDescription
    , GlobalSecondaryIndexDescription (..)
    , mkGlobalSecondaryIndexDescription
    , gsidBackfilling
    , gsidIndexArn
    , gsidIndexName
    , gsidIndexSizeBytes
    , gsidIndexStatus
    , gsidItemCount
    , gsidKeySchema
    , gsidProjection
    , gsidProvisionedThroughput

    -- ** ItemCollectionMetrics
    , ItemCollectionMetrics (..)
    , mkItemCollectionMetrics
    , icmItemCollectionKey
    , icmSizeEstimateRangeGB

    -- ** Capacity
    , Capacity (..)
    , mkCapacity
    , cCapacityUnits
    , cReadCapacityUnits
    , cWriteCapacityUnits

    -- ** ReplicaSettingsUpdate
    , ReplicaSettingsUpdate (..)
    , mkReplicaSettingsUpdate
    , rsuRegionName
    , rsuReplicaGlobalSecondaryIndexSettingsUpdate
    , rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate
    , rsuReplicaProvisionedReadCapacityUnits

    -- ** NumberAttributeValue
    , NumberAttributeValue (..)

    -- ** ConsumedCapacity
    , ConsumedCapacity (..)
    , mkConsumedCapacity
    , ccfCapacityUnits
    , ccfGlobalSecondaryIndexes
    , ccfLocalSecondaryIndexes
    , ccfReadCapacityUnits
    , ccfTable
    , ccfTableName
    , ccfWriteCapacityUnits

    -- ** ReplicaGlobalSecondaryIndexAutoScalingUpdate
    , ReplicaGlobalSecondaryIndexAutoScalingUpdate (..)
    , mkReplicaGlobalSecondaryIndexAutoScalingUpdate
    , rgsiasuIndexName
    , rgsiasuProvisionedReadCapacityAutoScalingUpdate

    -- ** GlobalSecondaryIndexAutoScalingUpdate
    , GlobalSecondaryIndexAutoScalingUpdate (..)
    , mkGlobalSecondaryIndexAutoScalingUpdate
    , gsiasuIndexName
    , gsiasuProvisionedWriteCapacityAutoScalingUpdate

    -- ** ContinuousBackupsStatus
    , ContinuousBackupsStatus (..)

    -- ** BillingModeSummary
    , BillingModeSummary (..)
    , mkBillingModeSummary
    , bmsBillingMode
    , bmsLastUpdateToPayPerRequestDateTime

    -- ** AutoScalingPolicyDescription
    , AutoScalingPolicyDescription (..)
    , mkAutoScalingPolicyDescription
    , aspdPolicyName
    , aspdTargetTrackingScalingPolicyConfiguration

    -- ** ReplicaGlobalSecondaryIndexSettingsDescription
    , ReplicaGlobalSecondaryIndexSettingsDescription (..)
    , mkReplicaGlobalSecondaryIndexSettingsDescription
    , rgsisdIndexName
    , rgsisdIndexStatus
    , rgsisdProvisionedReadCapacityAutoScalingSettings
    , rgsisdProvisionedReadCapacityUnits
    , rgsisdProvisionedWriteCapacityAutoScalingSettings
    , rgsisdProvisionedWriteCapacityUnits

    -- ** ReplicaGlobalSecondaryIndex
    , ReplicaGlobalSecondaryIndex (..)
    , mkReplicaGlobalSecondaryIndex
    , rgsiIndexName
    , rgsiProvisionedThroughputOverride

    -- ** ExpressionAttributeNameVariable
    , ExpressionAttributeNameVariable (..)

    -- ** DeleteReplicaAction
    , DeleteReplicaAction (..)
    , mkDeleteReplicaAction
    , draRegionName

    -- ** BatchStatementErrorCodeEnum
    , BatchStatementErrorCodeEnum (..)

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    , AutoScalingTargetTrackingScalingPolicyConfigurationDescription (..)
    , mkAutoScalingTargetTrackingScalingPolicyConfigurationDescription
    , asttspcdTargetValue
    , asttspcdDisableScaleIn
    , asttspcdScaleInCooldown
    , asttspcdScaleOutCooldown

    -- ** TransactGetItem
    , TransactGetItem (..)
    , mkTransactGetItem
    , tgiGet

    -- ** GlobalSecondaryIndex
    , GlobalSecondaryIndex (..)
    , mkGlobalSecondaryIndex
    , gsiIndexName
    , gsiKeySchema
    , gsiProjection
    , gsiProvisionedThroughput

    -- ** LocalSecondaryIndexDescription
    , LocalSecondaryIndexDescription (..)
    , mkLocalSecondaryIndexDescription
    , lsidIndexArn
    , lsidIndexName
    , lsidIndexSizeBytes
    , lsidItemCount
    , lsidKeySchema
    , lsidProjection

    -- ** FailureException
    , FailureException (..)
    , mkFailureException
    , feExceptionDescription
    , feExceptionName

    -- ** AttributeAction
    , AttributeAction (..)

    -- ** BackupStatus
    , BackupStatus (..)

    -- ** S3SseAlgorithm
    , S3SseAlgorithm (..)

    -- ** ContributorInsightsSummary
    , ContributorInsightsSummary (..)
    , mkContributorInsightsSummary
    , cisContributorInsightsStatus
    , cisIndexName
    , cisTableName

    -- ** ScalarAttributeType
    , ScalarAttributeType (..)

    -- ** ContributorInsightsStatus
    , ContributorInsightsStatus (..)

    -- ** ResourceArnString
    , ResourceArnString (..)

    -- ** BackupSummary
    , BackupSummary (..)
    , mkBackupSummary
    , bsBackupArn
    , bsBackupCreationDateTime
    , bsBackupExpiryDateTime
    , bsBackupName
    , bsBackupSizeBytes
    , bsBackupStatus
    , bsBackupType
    , bsTableArn
    , bsTableId
    , bsTableName

    -- ** SourceTableFeatureDetails
    , SourceTableFeatureDetails (..)
    , mkSourceTableFeatureDetails
    , stfdGlobalSecondaryIndexes
    , stfdLocalSecondaryIndexes
    , stfdSSEDescription
    , stfdStreamDescription
    , stfdTimeToLiveDescription

    -- ** SSEType
    , SSEType (..)

    -- ** Projection
    , Projection (..)
    , mkProjection
    , pNonKeyAttributes
    , pProjectionType

    -- ** TimeToLiveSpecification
    , TimeToLiveSpecification (..)
    , mkTimeToLiveSpecification
    , ttlsEnabled
    , ttlsAttributeName

    -- ** CreateGlobalSecondaryIndexAction
    , CreateGlobalSecondaryIndexAction (..)
    , mkCreateGlobalSecondaryIndexAction
    , cgsiaIndexName
    , cgsiaKeySchema
    , cgsiaProjection
    , cgsiaProvisionedThroughput

    -- ** ConditionCheck
    , ConditionCheck (..)
    , mkConditionCheck
    , ccKey
    , ccTableName
    , ccConditionExpression
    , ccExpressionAttributeNames
    , ccExpressionAttributeValues
    , ccReturnValuesOnConditionCheckFailure

    -- ** GlobalTableArnString
    , GlobalTableArnString (..)

    -- ** KinesisDataStreamDestination
    , KinesisDataStreamDestination (..)
    , mkKinesisDataStreamDestination
    , kdsdDestinationStatus
    , kdsdDestinationStatusDescription
    , kdsdStreamArn

    -- ** S3Prefix
    , S3Prefix (..)

    -- ** ExportNextToken
    , ExportNextToken (..)

    -- ** Select
    , Select (..)

    -- ** KeySchemaElement
    , KeySchemaElement (..)
    , mkKeySchemaElement
    , kseAttributeName
    , kseKeyType

    -- ** AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    , AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (..)
    , mkAutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    , asttspcuTargetValue
    , asttspcuDisableScaleIn
    , asttspcuScaleInCooldown
    , asttspcuScaleOutCooldown

    -- ** BillingMode
    , BillingMode (..)

    -- ** DeleteGlobalSecondaryIndexAction
    , DeleteGlobalSecondaryIndexAction (..)
    , mkDeleteGlobalSecondaryIndexAction
    , dgsiaIndexName

    -- ** DeleteRequest
    , DeleteRequest (..)
    , mkDeleteRequest
    , drKey

    -- ** UpdateGlobalSecondaryIndexAction
    , UpdateGlobalSecondaryIndexAction (..)
    , mkUpdateGlobalSecondaryIndexAction
    , ugsiaIndexName
    , ugsiaProvisionedThroughput

    -- ** PutRequest
    , PutRequest (..)
    , mkPutRequest
    , prItem

    -- ** ExceptionName
    , ExceptionName (..)

    -- ** TagKeyString
    , TagKeyString (..)

    -- ** BackupDescription
    , BackupDescription (..)
    , mkBackupDescription
    , bdBackupDetails
    , bdSourceTableDetails
    , bdSourceTableFeatureDetails

    -- ** Condition
    , Condition (..)
    , mkCondition
    , cComparisonOperator
    , cAttributeValueList

    -- ** ReplicaAutoScalingUpdate
    , ReplicaAutoScalingUpdate (..)
    , mkReplicaAutoScalingUpdate
    , rasuRegionName
    , rasuReplicaGlobalSecondaryIndexUpdates
    , rasuReplicaProvisionedReadCapacityAutoScalingUpdate

    -- ** TimeToLiveAttributeName
    , TimeToLiveAttributeName (..)

    -- ** ReplicaSettingsDescription
    , ReplicaSettingsDescription (..)
    , mkReplicaSettingsDescription
    , rsdRegionName
    , rsdReplicaBillingModeSummary
    , rsdReplicaGlobalSecondaryIndexSettings
    , rsdReplicaProvisionedReadCapacityAutoScalingSettings
    , rsdReplicaProvisionedReadCapacityUnits
    , rsdReplicaProvisionedWriteCapacityAutoScalingSettings
    , rsdReplicaProvisionedWriteCapacityUnits
    , rsdReplicaStatus

    -- ** GlobalTableDescription
    , GlobalTableDescription (..)
    , mkGlobalTableDescription
    , gtdCreationDateTime
    , gtdGlobalTableArn
    , gtdGlobalTableName
    , gtdGlobalTableStatus
    , gtdReplicationGroup

    -- ** BackupArn
    , BackupArn (..)

    -- ** KMSMasterKeyArn
    , KMSMasterKeyArn (..)

    -- ** AutoScalingRoleArn
    , AutoScalingRoleArn (..)

    -- ** SSEDescription
    , SSEDescription (..)
    , mkSSEDescription
    , ssedInaccessibleEncryptionDateTime
    , ssedKMSMasterKeyArn
    , ssedSSEType
    , ssedStatus

    -- ** AttributeName
    , AttributeName (..)

    -- ** Endpoint
    , Endpoint (..)
    , mkEndpoint
    , eAddress
    , eCachePeriodInMinutes

    -- ** ReplicaGlobalSecondaryIndexAutoScalingDescription
    , ReplicaGlobalSecondaryIndexAutoScalingDescription (..)
    , mkReplicaGlobalSecondaryIndexAutoScalingDescription
    , rgsiasdIndexName
    , rgsiasdIndexStatus
    , rgsiasdProvisionedReadCapacityAutoScalingSettings
    , rgsiasdProvisionedWriteCapacityAutoScalingSettings

    -- ** PointInTimeRecoveryDescription
    , PointInTimeRecoveryDescription (..)
    , mkPointInTimeRecoveryDescription
    , pitrdEarliestRestorableDateTime
    , pitrdLatestRestorableDateTime
    , pitrdPointInTimeRecoveryStatus

    -- ** ExportManifest
    , ExportManifest (..)

    -- ** NonKeyAttributeName
    , NonKeyAttributeName (..)

    -- ** TableId
    , TableId (..)

    -- ** ClientRequestToken
    , ClientRequestToken (..)

    -- ** ConditionalOperator
    , ConditionalOperator (..)

    -- ** GlobalSecondaryIndexUpdate
    , GlobalSecondaryIndexUpdate (..)
    , mkGlobalSecondaryIndexUpdate
    , gsiuCreate
    , gsiuDelete
    , gsiuUpdate

    -- ** GlobalTableGlobalSecondaryIndexSettingsUpdate
    , GlobalTableGlobalSecondaryIndexSettingsUpdate (..)
    , mkGlobalTableGlobalSecondaryIndexSettingsUpdate
    , gtgsisuIndexName
    , gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate
    , gtgsisuProvisionedWriteCapacityUnits

    -- ** TimeToLiveDescription
    , TimeToLiveDescription (..)
    , mkTimeToLiveDescription
    , ttldAttributeName
    , ttldTimeToLiveStatus

    -- ** ArchivalSummary
    , ArchivalSummary (..)
    , mkArchivalSummary
    , asArchivalBackupArn
    , asArchivalDateTime
    , asArchivalReason

    -- ** BackupType
    , BackupType (..)

    -- ** S3Bucket
    , S3Bucket (..)

    -- ** KinesisStreamingDestinationInput
    , KinesisStreamingDestinationInput (..)
    , mkKinesisStreamingDestinationInput
    , ksdiTableName
    , ksdiStreamArn

    -- ** ReplicaDescription
    , ReplicaDescription (..)
    , mkReplicaDescription
    , rdGlobalSecondaryIndexes
    , rdKMSMasterKeyId
    , rdProvisionedThroughputOverride
    , rdRegionName
    , rdReplicaInaccessibleDateTime
    , rdReplicaStatus
    , rdReplicaStatusDescription
    , rdReplicaStatusPercentProgress

    -- ** ReplicaUpdate
    , ReplicaUpdate (..)
    , mkReplicaUpdate
    , ruCreate
    , ruDelete

    -- ** TableName
    , TableName (..)

    -- ** Put
    , Put (..)
    , mkPut
    , pItem
    , pTableName
    , pConditionExpression
    , pExpressionAttributeNames
    , pExpressionAttributeValues
    , pReturnValuesOnConditionCheckFailure

    -- ** ExceptionDescription
    , ExceptionDescription (..)

    -- ** BatchStatementError
    , BatchStatementError (..)
    , mkBatchStatementError
    , bseCode
    , bseMessage

    -- ** GlobalSecondaryIndexInfo
    , GlobalSecondaryIndexInfo (..)
    , mkGlobalSecondaryIndexInfo
    , gsiiIndexName
    , gsiiKeySchema
    , gsiiProjection
    , gsiiProvisionedThroughput

    -- ** Delete
    , Delete (..)
    , mkDelete
    , dKey
    , dTableName
    , dConditionExpression
    , dExpressionAttributeNames
    , dExpressionAttributeValues
    , dReturnValuesOnConditionCheckFailure

    -- ** Update
    , Update (..)
    , mkUpdate
    , uKey
    , uUpdateExpression
    , uTableName
    , uConditionExpression
    , uExpressionAttributeNames
    , uExpressionAttributeValues
    , uReturnValuesOnConditionCheckFailure

    -- ** StreamSpecification
    , StreamSpecification (..)
    , mkStreamSpecification
    , ssStreamEnabled
    , ssStreamViewType

    -- ** AutoScalingSettingsDescription
    , AutoScalingSettingsDescription (..)
    , mkAutoScalingSettingsDescription
    , assdAutoScalingDisabled
    , assdAutoScalingRoleArn
    , assdMaximumUnits
    , assdMinimumUnits
    , assdScalingPolicies

    -- ** IndexName
    , IndexName (..)

    -- ** FilterExpression
    , FilterExpression (..)

    -- ** KeyConditionExpression
    , KeyConditionExpression (..)

    -- ** Statement
    , Statement (..)

    -- ** NextToken
    , NextToken (..)

    -- ** SourceBackupArn
    , SourceBackupArn (..)

    -- ** SourceTableArn
    , SourceTableArn (..)

    -- ** GlobalTableName
    , GlobalTableName (..)

    -- ** N
    , N (..)

    -- ** S
    , S (..)

    -- ** LastEvaluatedTableName
    , LastEvaluatedTableName (..)

    -- ** LatestStreamArn
    , LatestStreamArn (..)

    -- ** ResourceArn
    , ResourceArn (..)

    -- ** ExclusiveStartBackupArn
    , ExclusiveStartBackupArn (..)

    -- ** ExclusiveStartGlobalTableName
    , ExclusiveStartGlobalTableName (..)

    -- ** TargetTableName
    , TargetTableName (..)

    -- ** SourceTableName
    , SourceTableName (..)

    -- ** LastEvaluatedBackupArn
    , LastEvaluatedBackupArn (..)

    -- ** LastEvaluatedGlobalTableName
    , LastEvaluatedGlobalTableName (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Waiters
import Network.AWS.DynamoDB.PutItem
import Network.AWS.DynamoDB.DeleteItem
import Network.AWS.DynamoDB.UpdateItem
import Network.AWS.DynamoDB.DisableKinesisStreamingDestination
import Network.AWS.DynamoDB.ListGlobalTables
import Network.AWS.DynamoDB.UpdateGlobalTable
import Network.AWS.DynamoDB.DeleteTable
import Network.AWS.DynamoDB.UpdateTable
import Network.AWS.DynamoDB.BatchGetItem
import Network.AWS.DynamoDB.ListBackups
import Network.AWS.DynamoDB.DeleteBackup
import Network.AWS.DynamoDB.CreateBackup
import Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
import Network.AWS.DynamoDB.DescribeGlobalTableSettings
import Network.AWS.DynamoDB.ListTagsOfResource
import Network.AWS.DynamoDB.DescribeGlobalTable
import Network.AWS.DynamoDB.DescribeTable
import Network.AWS.DynamoDB.DescribeLimits
import Network.AWS.DynamoDB.ExecuteTransaction
import Network.AWS.DynamoDB.GetItem
import Network.AWS.DynamoDB.DescribeBackup
import Network.AWS.DynamoDB.BatchExecuteStatement
import Network.AWS.DynamoDB.DescribeTableReplicaAutoScaling
import Network.AWS.DynamoDB.UpdateGlobalTableSettings
import Network.AWS.DynamoDB.EnableKinesisStreamingDestination
import Network.AWS.DynamoDB.TransactGetItems
import Network.AWS.DynamoDB.ListContributorInsights
import Network.AWS.DynamoDB.BatchWriteItem
import Network.AWS.DynamoDB.ExportTableToPointInTime
import Network.AWS.DynamoDB.TransactWriteItems
import Network.AWS.DynamoDB.ListTables
import Network.AWS.DynamoDB.Scan
import Network.AWS.DynamoDB.UpdateContributorInsights
import Network.AWS.DynamoDB.ExecuteStatement
import Network.AWS.DynamoDB.Query
import Network.AWS.DynamoDB.CreateTable
import Network.AWS.DynamoDB.DescribeKinesisStreamingDestination
import Network.AWS.DynamoDB.DescribeEndpoints
import Network.AWS.DynamoDB.DescribeTimeToLive
import Network.AWS.DynamoDB.DescribeContinuousBackups
import Network.AWS.DynamoDB.ListExports
import Network.AWS.DynamoDB.TagResource
import Network.AWS.DynamoDB.DescribeContributorInsights
import Network.AWS.DynamoDB.UntagResource
import Network.AWS.DynamoDB.RestoreTableToPointInTime
import Network.AWS.DynamoDB.RestoreTableFromBackup
import Network.AWS.DynamoDB.UpdateTimeToLive
import Network.AWS.DynamoDB.CreateGlobalTable
import Network.AWS.DynamoDB.UpdateContinuousBackups
import Network.AWS.DynamoDB.DescribeExport
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DynamoDB'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
