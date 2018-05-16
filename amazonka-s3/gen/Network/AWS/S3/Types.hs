{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types
    (
    -- * Service Configuration
      s3

    -- * Errors
    , _BucketAlreadyOwnedByYou
    , _ObjectAlreadyInActiveTierError
    , _BucketAlreadyExists
    , _ObjectNotInActiveTierError
    , _NoSuchUpload
    , _NoSuchBucket
    , _NoSuchKey

    -- * Re-exported Types
    , module Network.AWS.S3.Internal

    -- * AnalyticsS3ExportFileFormat
    , AnalyticsS3ExportFileFormat (..)

    -- * BucketAccelerateStatus
    , BucketAccelerateStatus (..)

    -- * BucketCannedACL
    , BucketCannedACL (..)

    -- * BucketLogsPermission
    , BucketLogsPermission (..)

    -- * BucketVersioningStatus
    , BucketVersioningStatus (..)

    -- * CompressionType
    , CompressionType (..)

    -- * EncodingType
    , EncodingType (..)

    -- * Event
    , Event (..)

    -- * ExpirationStatus
    , ExpirationStatus (..)

    -- * ExpressionType
    , ExpressionType (..)

    -- * FileHeaderInfo
    , FileHeaderInfo (..)

    -- * FilterRuleName
    , FilterRuleName (..)

    -- * InventoryFormat
    , InventoryFormat (..)

    -- * InventoryFrequency
    , InventoryFrequency (..)

    -- * InventoryIncludedObjectVersions
    , InventoryIncludedObjectVersions (..)

    -- * InventoryOptionalField
    , InventoryOptionalField (..)

    -- * JSONType
    , JSONType (..)

    -- * MFADelete
    , MFADelete (..)

    -- * MFADeleteStatus
    , MFADeleteStatus (..)

    -- * MetadataDirective
    , MetadataDirective (..)

    -- * ObjectCannedACL
    , ObjectCannedACL (..)

    -- * ObjectStorageClass
    , ObjectStorageClass (..)

    -- * ObjectVersionStorageClass
    , ObjectVersionStorageClass (..)

    -- * OwnerOverride
    , OwnerOverride (..)

    -- * Payer
    , Payer (..)

    -- * Permission
    , Permission (..)

    -- * Protocol
    , Protocol (..)

    -- * QuoteFields
    , QuoteFields (..)

    -- * ReplicationRuleStatus
    , ReplicationRuleStatus (..)

    -- * ReplicationStatus
    , ReplicationStatus (..)

    -- * RequestCharged
    , RequestCharged (..)

    -- * RequestPayer
    , RequestPayer (..)

    -- * RestoreRequestType
    , RestoreRequestType (..)

    -- * ServerSideEncryption
    , ServerSideEncryption (..)

    -- * SseKMSEncryptedObjectsStatus
    , SseKMSEncryptedObjectsStatus (..)

    -- * StorageClass
    , StorageClass (..)

    -- * StorageClassAnalysisSchemaVersion
    , StorageClassAnalysisSchemaVersion (..)

    -- * TaggingDirective
    , TaggingDirective (..)

    -- * Tier
    , Tier (..)

    -- * TransitionStorageClass
    , TransitionStorageClass (..)

    -- * Type
    , Type (..)

    -- * AbortIncompleteMultipartUpload
    , AbortIncompleteMultipartUpload
    , abortIncompleteMultipartUpload
    , aimuDaysAfterInitiation

    -- * AccelerateConfiguration
    , AccelerateConfiguration
    , accelerateConfiguration
    , acStatus

    -- * AccessControlPolicy
    , AccessControlPolicy
    , accessControlPolicy
    , acpGrants
    , acpOwner

    -- * AccessControlTranslation
    , AccessControlTranslation
    , accessControlTranslation
    , actOwner

    -- * AnalyticsAndOperator
    , AnalyticsAndOperator
    , analyticsAndOperator
    , aaoPrefix
    , aaoTags

    -- * AnalyticsConfiguration
    , AnalyticsConfiguration
    , analyticsConfiguration
    , acFilter
    , acId
    , acStorageClassAnalysis

    -- * AnalyticsExportDestination
    , AnalyticsExportDestination
    , analyticsExportDestination
    , aedS3BucketDestination

    -- * AnalyticsFilter
    , AnalyticsFilter
    , analyticsFilter
    , afTag
    , afPrefix
    , afAnd

    -- * AnalyticsS3BucketDestination
    , AnalyticsS3BucketDestination
    , analyticsS3BucketDestination
    , asbdBucketAccountId
    , asbdPrefix
    , asbdFormat
    , asbdBucket

    -- * Bucket
    , Bucket
    , bucket
    , bCreationDate
    , bName

    -- * BucketLifecycleConfiguration
    , BucketLifecycleConfiguration
    , bucketLifecycleConfiguration
    , blcRules

    -- * BucketLoggingStatus
    , BucketLoggingStatus
    , bucketLoggingStatus
    , blsLoggingEnabled

    -- * CORSConfiguration
    , CORSConfiguration
    , corsConfiguration
    , ccCORSRules

    -- * CORSRule
    , CORSRule
    , corsRule
    , crMaxAgeSeconds
    , crAllowedHeaders
    , crExposeHeaders
    , crAllowedMethods
    , crAllowedOrigins

    -- * CSVInput
    , CSVInput
    , csvInput
    , ciQuoteCharacter
    , ciRecordDelimiter
    , ciFileHeaderInfo
    , ciQuoteEscapeCharacter
    , ciComments
    , ciFieldDelimiter

    -- * CSVOutput
    , CSVOutput
    , csvOutput
    , coQuoteCharacter
    , coQuoteFields
    , coRecordDelimiter
    , coQuoteEscapeCharacter
    , coFieldDelimiter

    -- * CommonPrefix
    , CommonPrefix
    , commonPrefix
    , cpPrefix

    -- * CompletedMultipartUpload
    , CompletedMultipartUpload
    , completedMultipartUpload
    , cmuParts

    -- * CompletedPart
    , CompletedPart
    , completedPart
    , cpPartNumber
    , cpETag

    -- * Condition
    , Condition
    , condition
    , cKeyPrefixEquals
    , cHTTPErrorCodeReturnedEquals

    -- * ContinuationEvent
    , ContinuationEvent
    , continuationEvent

    -- * CopyObjectResult
    , CopyObjectResult
    , copyObjectResult
    , corETag
    , corLastModified

    -- * CopyPartResult
    , CopyPartResult
    , copyPartResult
    , cprETag
    , cprLastModified

    -- * CreateBucketConfiguration
    , CreateBucketConfiguration
    , createBucketConfiguration
    , cbcLocationConstraint

    -- * Delete
    , Delete
    , delete'
    , dQuiet
    , dObjects

    -- * DeleteMarkerEntry
    , DeleteMarkerEntry
    , deleteMarkerEntry
    , dmeVersionId
    , dmeIsLatest
    , dmeOwner
    , dmeKey
    , dmeLastModified

    -- * DeletedObject
    , DeletedObject
    , deletedObject
    , dVersionId
    , dDeleteMarker
    , dDeleteMarkerVersionId
    , dKey

    -- * Destination
    , Destination
    , destination
    , dAccessControlTranslation
    , dAccount
    , dStorageClass
    , dEncryptionConfiguration
    , dBucket

    -- * Encryption
    , Encryption
    , encryption
    , eKMSKeyId
    , eKMSContext
    , eEncryptionType

    -- * EncryptionConfiguration
    , EncryptionConfiguration
    , encryptionConfiguration
    , ecReplicaKMSKeyId

    -- * EndEvent
    , EndEvent
    , endEvent

    -- * ErrorDocument
    , ErrorDocument
    , errorDocument
    , edKey

    -- * FilterRule
    , FilterRule
    , filterRule
    , frValue
    , frName

    -- * GlacierJobParameters
    , GlacierJobParameters
    , glacierJobParameters
    , gjpTier

    -- * Grant
    , Grant
    , grant
    , gPermission
    , gGrantee

    -- * Grantee
    , Grantee
    , grantee
    , gURI
    , gEmailAddress
    , gDisplayName
    , gId
    , gType

    -- * IndexDocument
    , IndexDocument
    , indexDocument
    , idSuffix

    -- * Initiator
    , Initiator
    , initiator
    , iDisplayName
    , iId

    -- * InputSerialization
    , InputSerialization
    , inputSerialization
    , isJSON
    , isCSV
    , isCompressionType

    -- * InventoryConfiguration
    , InventoryConfiguration
    , inventoryConfiguration
    , icOptionalFields
    , icFilter
    , icDestination
    , icIsEnabled
    , icId
    , icIncludedObjectVersions
    , icSchedule

    -- * InventoryDestination
    , InventoryDestination
    , inventoryDestination
    , idS3BucketDestination

    -- * InventoryEncryption
    , InventoryEncryption
    , inventoryEncryption
    , ieSSES3
    , ieSSEKMS

    -- * InventoryFilter
    , InventoryFilter
    , inventoryFilter
    , ifPrefix

    -- * InventoryS3BucketDestination
    , InventoryS3BucketDestination
    , inventoryS3BucketDestination
    , isbdPrefix
    , isbdAccountId
    , isbdEncryption
    , isbdBucket
    , isbdFormat

    -- * InventorySchedule
    , InventorySchedule
    , inventorySchedule
    , isFrequency

    -- * JSONInput
    , JSONInput
    , jsonInput
    , jiType

    -- * JSONOutput
    , JSONOutput
    , jsonOutput
    , joRecordDelimiter

    -- * LambdaFunctionConfiguration
    , LambdaFunctionConfiguration
    , lambdaFunctionConfiguration
    , lfcId
    , lfcFilter
    , lfcLambdaFunctionARN
    , lfcEvents

    -- * LifecycleExpiration
    , LifecycleExpiration
    , lifecycleExpiration
    , leDays
    , leDate
    , leExpiredObjectDeleteMarker

    -- * LifecycleRule
    , LifecycleRule
    , lifecycleRule
    , lrTransitions
    , lrNoncurrentVersionExpiration
    , lrPrefix
    , lrNoncurrentVersionTransitions
    , lrExpiration
    , lrId
    , lrFilter
    , lrAbortIncompleteMultipartUpload
    , lrStatus

    -- * LifecycleRuleAndOperator
    , LifecycleRuleAndOperator
    , lifecycleRuleAndOperator
    , lraoPrefix
    , lraoTags

    -- * LifecycleRuleFilter
    , LifecycleRuleFilter
    , lifecycleRuleFilter
    , lrfTag
    , lrfPrefix
    , lrfAnd

    -- * LoggingEnabled
    , LoggingEnabled
    , loggingEnabled
    , leTargetGrants
    , leTargetBucket
    , leTargetPrefix

    -- * MetadataEntry
    , MetadataEntry
    , metadataEntry
    , meValue
    , meName

    -- * MetricsAndOperator
    , MetricsAndOperator
    , metricsAndOperator
    , maoPrefix
    , maoTags

    -- * MetricsConfiguration
    , MetricsConfiguration
    , metricsConfiguration
    , mcFilter
    , mcId

    -- * MetricsFilter
    , MetricsFilter
    , metricsFilter
    , mfTag
    , mfPrefix
    , mfAnd

    -- * MultipartUpload
    , MultipartUpload
    , multipartUpload
    , muInitiated
    , muInitiator
    , muOwner
    , muKey
    , muStorageClass
    , muUploadId

    -- * NoncurrentVersionExpiration
    , NoncurrentVersionExpiration
    , noncurrentVersionExpiration
    , nveNoncurrentDays

    -- * NoncurrentVersionTransition
    , NoncurrentVersionTransition
    , noncurrentVersionTransition
    , nvtNoncurrentDays
    , nvtStorageClass

    -- * NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncQueueConfigurations
    , ncTopicConfigurations
    , ncLambdaFunctionConfigurations

    -- * NotificationConfigurationFilter
    , NotificationConfigurationFilter
    , notificationConfigurationFilter
    , ncfKey

    -- * Object
    , Object
    , object'
    , oOwner
    , oETag
    , oSize
    , oKey
    , oStorageClass
    , oLastModified

    -- * ObjectIdentifier
    , ObjectIdentifier
    , objectIdentifier
    , oiVersionId
    , oiKey

    -- * ObjectVersion
    , ObjectVersion
    , objectVersion
    , ovETag
    , ovVersionId
    , ovSize
    , ovIsLatest
    , ovOwner
    , ovKey
    , ovStorageClass
    , ovLastModified

    -- * OutputLocation
    , OutputLocation
    , outputLocation
    , olS3

    -- * OutputSerialization
    , OutputSerialization
    , outputSerialization
    , osJSON
    , osCSV

    -- * Owner
    , Owner
    , owner
    , oDisplayName
    , oId

    -- * Part
    , Part
    , part
    , pETag
    , pSize
    , pPartNumber
    , pLastModified

    -- * Progress
    , Progress
    , progress
    , pBytesReturned
    , pBytesScanned
    , pBytesProcessed

    -- * ProgressEvent
    , ProgressEvent
    , progressEvent
    , peDetails

    -- * QueueConfiguration
    , QueueConfiguration
    , queueConfiguration
    , qcId
    , qcFilter
    , qcQueueARN
    , qcEvents

    -- * RecordsEvent
    , RecordsEvent
    , recordsEvent
    , rePayload

    -- * Redirect
    , Redirect
    , redirect
    , rHostName
    , rProtocol
    , rHTTPRedirectCode
    , rReplaceKeyWith
    , rReplaceKeyPrefixWith

    -- * RedirectAllRequestsTo
    , RedirectAllRequestsTo
    , redirectAllRequestsTo
    , rartProtocol
    , rartHostName

    -- * ReplicationConfiguration
    , ReplicationConfiguration
    , replicationConfiguration
    , rcRole
    , rcRules

    -- * ReplicationRule
    , ReplicationRule
    , replicationRule
    , rrId
    , rrSourceSelectionCriteria
    , rrPrefix
    , rrStatus
    , rrDestination

    -- * RequestPaymentConfiguration
    , RequestPaymentConfiguration
    , requestPaymentConfiguration
    , rpcPayer

    -- * RequestProgress
    , RequestProgress
    , requestProgress
    , rpEnabled

    -- * RestoreRequest
    , RestoreRequest
    , restoreRequest
    , rrDays
    , rrSelectParameters
    , rrOutputLocation
    , rrTier
    , rrGlacierJobParameters
    , rrType
    , rrDescription

    -- * RoutingRule
    , RoutingRule
    , routingRule
    , rrCondition
    , rrRedirect

    -- * S3KeyFilter
    , S3KeyFilter
    , s3KeyFilter
    , skfFilterRules

    -- * S3Location
    , S3Location
    , s3Location
    , slCannedACL
    , slAccessControlList
    , slUserMetadata
    , slEncryption
    , slStorageClass
    , slTagging
    , slBucketName
    , slPrefix

    -- * S3ServiceError
    , S3ServiceError
    , s3ServiceError
    , sseVersionId
    , sseKey
    , sseCode
    , sseMessage

    -- * SSEKMS
    , SSEKMS
    , sSEKMS
    , ssekKeyId

    -- * SSES3
    , SSES3
    , sSES3

    -- * SelectObjectContentEventStream
    , SelectObjectContentEventStream
    , selectObjectContentEventStream
    , socesProgress
    , socesRecords
    , socesCont
    , socesStats
    , socesEnd

    -- * SelectParameters
    , SelectParameters
    , selectParameters
    , spInputSerialization
    , spExpressionType
    , spExpression
    , spOutputSerialization

    -- * ServerSideEncryptionByDefault
    , ServerSideEncryptionByDefault
    , serverSideEncryptionByDefault
    , ssebdKMSMasterKeyId
    , ssebdSSEAlgorithm

    -- * ServerSideEncryptionConfiguration
    , ServerSideEncryptionConfiguration
    , serverSideEncryptionConfiguration
    , ssecRules

    -- * ServerSideEncryptionRule
    , ServerSideEncryptionRule
    , serverSideEncryptionRule
    , sserApplyServerSideEncryptionByDefault

    -- * SourceSelectionCriteria
    , SourceSelectionCriteria
    , sourceSelectionCriteria
    , sscSseKMSEncryptedObjects

    -- * SseKMSEncryptedObjects
    , SseKMSEncryptedObjects
    , sseKMSEncryptedObjects
    , skeoStatus

    -- * Stats
    , Stats
    , stats
    , sBytesReturned
    , sBytesScanned
    , sBytesProcessed

    -- * StatsEvent
    , StatsEvent
    , statsEvent
    , seDetails

    -- * StorageClassAnalysis
    , StorageClassAnalysis
    , storageClassAnalysis
    , scaDataExport

    -- * StorageClassAnalysisDataExport
    , StorageClassAnalysisDataExport
    , storageClassAnalysisDataExport
    , scadeOutputSchemaVersion
    , scadeDestination

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * Tagging
    , Tagging
    , tagging
    , tTagSet

    -- * TargetGrant
    , TargetGrant
    , targetGrant
    , tgPermission
    , tgGrantee

    -- * TopicConfiguration
    , TopicConfiguration
    , topicConfiguration
    , tcId
    , tcFilter
    , tcTopicARN
    , tcEvents

    -- * Transition
    , Transition
    , transition
    , tDays
    , tDate
    , tStorageClass

    -- * VersioningConfiguration
    , VersioningConfiguration
    , versioningConfiguration
    , vcStatus
    , vcMFADelete

    -- * WebsiteConfiguration
    , WebsiteConfiguration
    , websiteConfiguration
    , wcRedirectAllRequestsTo
    , wcErrorDocument
    , wcIndexDocument
    , wcRoutingRules
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Product
import Network.AWS.S3.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2006-03-01@ of the Amazon Simple Storage Service SDK configuration.
s3 :: Service
s3 =
  Service
    { _svcAbbrev = "S3"
    , _svcSigner = v4
    , _svcPrefix = "s3"
    , _svcVersion = "2006-03-01"
    , _svcEndpoint = defaultEndpoint s3
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "S3"
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
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasCode "BadDigest" . hasStatus 400) e = Just "contentmd5"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasCode "RequestTimeout" . hasStatus 400) e = Just "timeouts"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | Prism for BucketAlreadyOwnedByYou' errors.
_BucketAlreadyOwnedByYou :: AsError a => Getting (First ServiceError) a ServiceError
_BucketAlreadyOwnedByYou = _MatchServiceError s3 "BucketAlreadyOwnedByYou"


-- | This operation is not allowed against this storage tier
_ObjectAlreadyInActiveTierError :: AsError a => Getting (First ServiceError) a ServiceError
_ObjectAlreadyInActiveTierError =
  _MatchServiceError s3 "ObjectAlreadyInActiveTierError"


-- | The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again.
_BucketAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_BucketAlreadyExists = _MatchServiceError s3 "BucketAlreadyExists"


-- | The source object of the COPY operation is not in the active tier and is only stored in Amazon Glacier.
_ObjectNotInActiveTierError :: AsError a => Getting (First ServiceError) a ServiceError
_ObjectNotInActiveTierError = _MatchServiceError s3 "ObjectNotInActiveTierError"


-- | The specified multipart upload does not exist.
_NoSuchUpload :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchUpload = _MatchServiceError s3 "NoSuchUpload"


-- | The specified bucket does not exist.
_NoSuchBucket :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchBucket = _MatchServiceError s3 "NoSuchBucket"


-- | The specified key does not exist.
_NoSuchKey :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchKey = _MatchServiceError s3 "NoSuchKey"

