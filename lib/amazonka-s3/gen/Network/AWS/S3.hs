{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Simple Storage Service is storage for the Internet. Amazon S3 has a simple web services interface that you can use to store and retrieve any amount of data, at any time, from anywhere on the web. It gives any developer access to the same highly scalable, reliable, fast, inexpensive data storage infrastructure that Amazon uses to run its own global network of web sites. The service aims to maximize benefits of scale and to pass those benefits on to developers.
module Network.AWS.S3
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** BucketAlreadyOwnedByYou
    , _BucketAlreadyOwnedByYou

    -- ** ObjectAlreadyInActiveTierError
    , _ObjectAlreadyInActiveTierError

    -- ** BucketAlreadyExists
    , _BucketAlreadyExists

    -- ** ObjectNotInActiveTierError
    , _ObjectNotInActiveTierError

    -- ** NoSuchUpload
    , _NoSuchUpload

    -- ** NoSuchBucket
    , _NoSuchBucket

    -- ** NoSuchKey
    , _NoSuchKey

    -- ** InvalidObjectState
    , _InvalidObjectState

    -- * Waiters
    -- $waiters

    -- ** ObjectNotExists
    , mkObjectNotExists

    -- ** BucketExists
    , mkBucketExists

    -- ** ObjectExists
    , mkObjectExists

    -- ** BucketNotExists
    , mkBucketNotExists

    -- * Operations
    -- $operations

    -- ** PutBucketRequestPayment 
    , module Network.AWS.S3.PutBucketRequestPayment

    -- ** PutObject 
    , module Network.AWS.S3.PutObject

    -- ** DeleteObject 
    , module Network.AWS.S3.DeleteObject

    -- ** PutBucketLogging 
    , module Network.AWS.S3.PutBucketLogging

    -- ** GetBucketMetricsConfiguration 
    , module Network.AWS.S3.GetBucketMetricsConfiguration

    -- ** ListBuckets 
    , module Network.AWS.S3.ListBuckets

    -- ** DeleteBucket 
    , module Network.AWS.S3.DeleteBucket

    -- ** CreateBucket 
    , module Network.AWS.S3.CreateBucket

    -- ** DeleteBucketTagging 
    , module Network.AWS.S3.DeleteBucketTagging

    -- ** PutObjectAcl 
    , module Network.AWS.S3.PutObjectAcl

    -- ** PutBucketTagging 
    , module Network.AWS.S3.PutBucketTagging

    -- ** GetBucketInventoryConfiguration 
    , module Network.AWS.S3.GetBucketInventoryConfiguration

    -- ** DeletePublicAccessBlock 
    , module Network.AWS.S3.DeletePublicAccessBlock

    -- ** PutBucketInventoryConfiguration 
    , module Network.AWS.S3.PutBucketInventoryConfiguration

    -- ** GetBucketLocation 
    , module Network.AWS.S3.GetBucketLocation

    -- ** ListBucketInventoryConfigurations 
    , module Network.AWS.S3.ListBucketInventoryConfigurations

    -- ** PutPublicAccessBlock 
    , module Network.AWS.S3.PutPublicAccessBlock

    -- ** DeleteBucketInventoryConfiguration 
    , module Network.AWS.S3.DeleteBucketInventoryConfiguration

    -- ** GetBucketIntelligentTieringConfiguration 
    , module Network.AWS.S3.GetBucketIntelligentTieringConfiguration

    -- ** GetBucketNotificationConfiguration 
    , module Network.AWS.S3.GetBucketNotificationConfiguration

    -- ** GetObjectLockConfiguration 
    , module Network.AWS.S3.GetObjectLockConfiguration

    -- ** PutObjectRetention 
    , module Network.AWS.S3.PutObjectRetention

    -- ** PutBucketAccelerateConfiguration 
    , module Network.AWS.S3.PutBucketAccelerateConfiguration

    -- ** PutObjectLegalHold 
    , module Network.AWS.S3.PutObjectLegalHold

    -- ** PutBucketOwnershipControls 
    , module Network.AWS.S3.PutBucketOwnershipControls

    -- ** DeleteBucketOwnershipControls 
    , module Network.AWS.S3.DeleteBucketOwnershipControls

    -- ** PutBucketMetricsConfiguration 
    , module Network.AWS.S3.PutBucketMetricsConfiguration

    -- ** DeleteBucketMetricsConfiguration 
    , module Network.AWS.S3.DeleteBucketMetricsConfiguration

    -- ** ListObjectsV2 (Paginated)
    , module Network.AWS.S3.ListObjectsV2

    -- ** GetObject 
    , module Network.AWS.S3.GetObject

    -- ** PutBucketReplication 
    , module Network.AWS.S3.PutBucketReplication

    -- ** GetBucketWebsite 
    , module Network.AWS.S3.GetBucketWebsite

    -- ** GetBucketRequestPayment 
    , module Network.AWS.S3.GetBucketRequestPayment

    -- ** DeleteBucketReplication 
    , module Network.AWS.S3.DeleteBucketReplication

    -- ** ListObjectVersions (Paginated)
    , module Network.AWS.S3.ListObjectVersions

    -- ** HeadBucket 
    , module Network.AWS.S3.HeadBucket

    -- ** DeleteBucketLifecycle 
    , module Network.AWS.S3.DeleteBucketLifecycle

    -- ** PutBucketLifecycleConfiguration 
    , module Network.AWS.S3.PutBucketLifecycleConfiguration

    -- ** PutBucketAnalyticsConfiguration 
    , module Network.AWS.S3.PutBucketAnalyticsConfiguration

    -- ** ListBucketAnalyticsConfigurations 
    , module Network.AWS.S3.ListBucketAnalyticsConfigurations

    -- ** DeleteBucketAnalyticsConfiguration 
    , module Network.AWS.S3.DeleteBucketAnalyticsConfiguration

    -- ** CreateMultipartUpload 
    , module Network.AWS.S3.CreateMultipartUpload

    -- ** GetBucketPolicyStatus 
    , module Network.AWS.S3.GetBucketPolicyStatus

    -- ** UploadPart 
    , module Network.AWS.S3.UploadPart

    -- ** SelectObjectContent 
    , module Network.AWS.S3.SelectObjectContent

    -- ** GetBucketReplication 
    , module Network.AWS.S3.GetBucketReplication

    -- ** PutBucketWebsite 
    , module Network.AWS.S3.PutBucketWebsite

    -- ** DeleteBucketWebsite 
    , module Network.AWS.S3.DeleteBucketWebsite

    -- ** CompleteMultipartUpload 
    , module Network.AWS.S3.CompleteMultipartUpload

    -- ** ListMultipartUploads (Paginated)
    , module Network.AWS.S3.ListMultipartUploads

    -- ** ListObjects (Paginated)
    , module Network.AWS.S3.ListObjects

    -- ** GetBucketOwnershipControls 
    , module Network.AWS.S3.GetBucketOwnershipControls

    -- ** GetObjectLegalHold 
    , module Network.AWS.S3.GetObjectLegalHold

    -- ** GetObjectRetention 
    , module Network.AWS.S3.GetObjectRetention

    -- ** DeleteBucketPolicy 
    , module Network.AWS.S3.DeleteBucketPolicy

    -- ** GetBucketEncryption 
    , module Network.AWS.S3.GetBucketEncryption

    -- ** AbortMultipartUpload 
    , module Network.AWS.S3.AbortMultipartUpload

    -- ** PutBucketPolicy 
    , module Network.AWS.S3.PutBucketPolicy

    -- ** GetBucketAccelerateConfiguration 
    , module Network.AWS.S3.GetBucketAccelerateConfiguration

    -- ** GetObjectTorrent 
    , module Network.AWS.S3.GetObjectTorrent

    -- ** DeleteObjects 
    , module Network.AWS.S3.DeleteObjects

    -- ** PutObjectLockConfiguration 
    , module Network.AWS.S3.PutObjectLockConfiguration

    -- ** PutBucketNotificationConfiguration 
    , module Network.AWS.S3.PutBucketNotificationConfiguration

    -- ** GetBucketVersioning 
    , module Network.AWS.S3.GetBucketVersioning

    -- ** DeleteBucketCors 
    , module Network.AWS.S3.DeleteBucketCors

    -- ** DeleteBucketIntelligentTieringConfiguration 
    , module Network.AWS.S3.DeleteBucketIntelligentTieringConfiguration

    -- ** ListBucketIntelligentTieringConfigurations 
    , module Network.AWS.S3.ListBucketIntelligentTieringConfigurations

    -- ** PutBucketCors 
    , module Network.AWS.S3.PutBucketCors

    -- ** GetPublicAccessBlock 
    , module Network.AWS.S3.GetPublicAccessBlock

    -- ** PutBucketIntelligentTieringConfiguration 
    , module Network.AWS.S3.PutBucketIntelligentTieringConfiguration

    -- ** GetBucketCors 
    , module Network.AWS.S3.GetBucketCors

    -- ** GetObjectAcl 
    , module Network.AWS.S3.GetObjectAcl

    -- ** RestoreObject 
    , module Network.AWS.S3.RestoreObject

    -- ** HeadObject 
    , module Network.AWS.S3.HeadObject

    -- ** PutBucketVersioning 
    , module Network.AWS.S3.PutBucketVersioning

    -- ** GetBucketTagging 
    , module Network.AWS.S3.GetBucketTagging

    -- ** CopyObject 
    , module Network.AWS.S3.CopyObject

    -- ** ListBucketMetricsConfigurations 
    , module Network.AWS.S3.ListBucketMetricsConfigurations

    -- ** GetBucketPolicy 
    , module Network.AWS.S3.GetBucketPolicy

    -- ** PutBucketEncryption 
    , module Network.AWS.S3.PutBucketEncryption

    -- ** DeleteBucketEncryption 
    , module Network.AWS.S3.DeleteBucketEncryption

    -- ** GetBucketLogging 
    , module Network.AWS.S3.GetBucketLogging

    -- ** GetBucketAcl 
    , module Network.AWS.S3.GetBucketAcl

    -- ** GetBucketLifecycleConfiguration 
    , module Network.AWS.S3.GetBucketLifecycleConfiguration

    -- ** GetBucketAnalyticsConfiguration 
    , module Network.AWS.S3.GetBucketAnalyticsConfiguration

    -- ** GetObjectTagging 
    , module Network.AWS.S3.GetObjectTagging

    -- ** ListParts (Paginated)
    , module Network.AWS.S3.ListParts

    -- ** DeleteObjectTagging 
    , module Network.AWS.S3.DeleteObjectTagging

    -- ** UploadPartCopy 
    , module Network.AWS.S3.UploadPartCopy

    -- ** PutObjectTagging 
    , module Network.AWS.S3.PutObjectTagging

    -- ** PutBucketAcl 
    , module Network.AWS.S3.PutBucketAcl

    -- * Types

    -- ** Common
    , module Network.AWS.S3.Internal

    -- ** StartAfter
    , StartAfter (..)

    -- ** ReplicationConfiguration
    , ReplicationConfiguration (..)
    , mkReplicationConfiguration
    , rcRole
    , rcRules

    -- ** IfMatch
    , IfMatch (..)

    -- ** Destination
    , Destination (..)
    , mkDestination
    , dBucket
    , dAccessControlTranslation
    , dAccount
    , dEncryptionConfiguration
    , dMetrics
    , dReplicationTime
    , dStorageClass

    -- ** Event
    , Event (..)

    -- ** RequestCharged
    , RequestCharged (..)

    -- ** DeleteMarkerReplication
    , DeleteMarkerReplication (..)
    , mkDeleteMarkerReplication
    , dmrStatus

    -- ** Suffix
    , Suffix (..)

    -- ** NoncurrentVersionExpiration
    , NoncurrentVersionExpiration (..)
    , mkNoncurrentVersionExpiration
    , nveNoncurrentDays

    -- ** AllowedMethod
    , AllowedMethod (..)

    -- ** Transition
    , Transition (..)
    , mkTransition
    , tfDate
    , tfDays
    , tfStorageClass

    -- ** DeleteMarkerEntry
    , DeleteMarkerEntry (..)
    , mkDeleteMarkerEntry
    , dmeIsLatest
    , dmeKey
    , dmeLastModified
    , dmeOwner
    , dmeVersionId

    -- ** AnalyticsS3ExportFileFormat
    , AnalyticsS3ExportFileFormat (..)

    -- ** ExpirationStatus
    , ExpirationStatus (..)

    -- ** TargetBucket
    , TargetBucket (..)

    -- ** ServerSideEncryptionRule
    , ServerSideEncryptionRule (..)
    , mkServerSideEncryptionRule
    , sserApplyServerSideEncryptionByDefault

    -- ** AllowedHeader
    , AllowedHeader (..)

    -- ** InventoryId
    , InventoryId (..)

    -- ** MetricsConfiguration
    , MetricsConfiguration (..)
    , mkMetricsConfiguration
    , mcId
    , mcFilter

    -- ** CopySourceRange
    , CopySourceRange (..)

    -- ** Metrics
    , Metrics (..)
    , mkMetrics
    , mStatus
    , mEventThreshold

    -- ** Part
    , Part (..)
    , mkPart
    , pETag
    , pLastModified
    , pPartNumber
    , pSize

    -- ** HostName
    , HostName (..)

    -- ** ReplicationTimeValue
    , ReplicationTimeValue (..)
    , mkReplicationTimeValue
    , rtvMinutes

    -- ** CopySourceSSECustomerKeyMD5
    , CopySourceSSECustomerKeyMD5 (..)

    -- ** VersioningConfiguration
    , VersioningConfiguration (..)
    , mkVersioningConfiguration
    , vcMFADelete
    , vcStatus

    -- ** TaggingDirective
    , TaggingDirective (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** MFA
    , MFA (..)

    -- ** ExpressionType
    , ExpressionType (..)

    -- ** NextVersionIdMarker
    , NextVersionIdMarker (..)

    -- ** TaggingHeader
    , TaggingHeader (..)

    -- ** ObjectStorageClass
    , ObjectStorageClass (..)

    -- ** InventoryFilter
    , InventoryFilter (..)
    , mkInventoryFilter
    , ifPrefix

    -- ** MetadataDirective
    , MetadataDirective (..)

    -- ** ReplicationRule
    , ReplicationRule (..)
    , mkReplicationRule
    , rrStatus
    , rrDestination
    , rrDeleteMarkerReplication
    , rrExistingObjectReplication
    , rrFilter
    , rrID
    , rrPrefix
    , rrPriority
    , rrSourceSelectionCriteria

    -- ** ResponseContentType
    , ResponseContentType (..)

    -- ** InventoryConfiguration
    , InventoryConfiguration (..)
    , mkInventoryConfiguration
    , icDestination
    , icIsEnabled
    , icId
    , icIncludedObjectVersions
    , icSchedule
    , icFilter
    , icOptionalFields

    -- ** ObjectLockRetention
    , ObjectLockRetention (..)
    , mkObjectLockRetention
    , olrMode
    , olrRetainUntilDate

    -- ** RedirectAllRequestsTo
    , RedirectAllRequestsTo (..)
    , mkRedirectAllRequestsTo
    , rartHostName
    , rartProtocol

    -- ** SelectParameters
    , SelectParameters (..)
    , mkSelectParameters
    , spInputSerialization
    , spExpressionType
    , spExpression
    , spOutputSerialization

    -- ** RoutingRule
    , RoutingRule (..)
    , mkRoutingRule
    , rrRedirect
    , rrCondition

    -- ** ObjectLockLegalHold
    , ObjectLockLegalHold (..)
    , mkObjectLockLegalHold
    , ollhStatus

    -- ** Location
    , Location (..)

    -- ** IntelligentTieringConfiguration
    , IntelligentTieringConfiguration (..)
    , mkIntelligentTieringConfiguration
    , itcId
    , itcStatus
    , itcTierings
    , itcFilter

    -- ** NotificationConfiguration
    , NotificationConfiguration (..)
    , mkNotificationConfiguration
    , ncLambdaFunctionConfigurations
    , ncQueueConfigurations
    , ncTopicConfigurations

    -- ** Progress
    , Progress (..)
    , mkProgress
    , pBytesProcessed
    , pBytesReturned
    , pBytesScanned

    -- ** ResponseContentDisposition
    , ResponseContentDisposition (..)

    -- ** FilterRuleValue
    , FilterRuleValue (..)

    -- ** ContinuationEvent
    , ContinuationEvent (..)
    , mkContinuationEvent

    -- ** StorageClassAnalysis
    , StorageClassAnalysis (..)
    , mkStorageClassAnalysis
    , scaDataExport

    -- ** BucketAccelerateStatus
    , BucketAccelerateStatus (..)

    -- ** InventoryEncryption
    , InventoryEncryption (..)
    , mkInventoryEncryption
    , ieSSEKMS
    , ieSSES3

    -- ** LocationPrefix
    , LocationPrefix (..)

    -- ** ObjectLockToken
    , ObjectLockToken (..)

    -- ** ObjectLockMode
    , ObjectLockMode (..)

    -- ** KeyMarker
    , KeyMarker (..)

    -- ** LifecycleRule
    , LifecycleRule (..)
    , mkLifecycleRule
    , lrStatus
    , lrAbortIncompleteMultipartUpload
    , lrExpiration
    , lrFilter
    , lrID
    , lrNoncurrentVersionExpiration
    , lrNoncurrentVersionTransitions
    , lrPrefix
    , lrTransitions

    -- ** ObjectCannedACL
    , ObjectCannedACL (..)

    -- ** AccessControlTranslation
    , AccessControlTranslation (..)
    , mkAccessControlTranslation
    , actOwner

    -- ** BucketVersioningStatus
    , BucketVersioningStatus (..)

    -- ** DeletedObject
    , DeletedObject (..)
    , mkDeletedObject
    , doDeleteMarker
    , doDeleteMarkerVersionId
    , doKey
    , doVersionId

    -- ** ObjectVersionStorageClass
    , ObjectVersionStorageClass (..)

    -- ** LambdaFunctionArn
    , LambdaFunctionArn (..)

    -- ** CSVInput
    , CSVInput (..)
    , mkCSVInput
    , csviAllowQuotedRecordDelimiter
    , csviComments
    , csviFieldDelimiter
    , csviFileHeaderInfo
    , csviQuoteCharacter
    , csviQuoteEscapeCharacter
    , csviRecordDelimiter

    -- ** ResponseContentLanguage
    , ResponseContentLanguage (..)

    -- ** MultipartUploadId
    , MultipartUploadId (..)

    -- ** Prefix
    , Prefix (..)

    -- ** ParquetInput
    , ParquetInput (..)
    , mkParquetInput

    -- ** S3ServiceError
    , S3ServiceError (..)
    , mkS3ServiceError
    , sseCode
    , sseKey
    , sseMessage
    , sseVersionId

    -- ** KeyPrefixEquals
    , KeyPrefixEquals (..)

    -- ** Restore
    , Restore (..)

    -- ** ObjectLockEnabled
    , ObjectLockEnabled (..)

    -- ** EndEvent
    , EndEvent (..)
    , mkEndEvent

    -- ** Token
    , Token (..)

    -- ** MetricsStatus
    , MetricsStatus (..)

    -- ** AnalyticsId
    , AnalyticsId (..)

    -- ** Expiration
    , Expiration (..)

    -- ** QuoteCharacter
    , QuoteCharacter (..)

    -- ** CopyPartResult
    , CopyPartResult (..)
    , mkCopyPartResult
    , cprETag
    , cprLastModified

    -- ** EncodingType
    , EncodingType (..)

    -- ** ExposeHeader
    , ExposeHeader (..)

    -- ** LifecycleRuleAndOperator
    , LifecycleRuleAndOperator (..)
    , mkLifecycleRuleAndOperator
    , lraoPrefix
    , lraoTags

    -- ** RequestPaymentConfiguration
    , RequestPaymentConfiguration (..)
    , mkRequestPaymentConfiguration
    , rpcPayer

    -- ** CORSRule
    , CORSRule (..)
    , mkCORSRule
    , corsrAllowedMethods
    , corsrAllowedOrigins
    , corsrAllowedHeaders
    , corsrExposeHeaders
    , corsrMaxAgeSeconds

    -- ** Value
    , Value (..)

    -- ** WebsiteConfiguration
    , WebsiteConfiguration (..)
    , mkWebsiteConfiguration
    , wcErrorDocument
    , wcIndexDocument
    , wcRedirectAllRequestsTo
    , wcRoutingRules

    -- ** NoncurrentVersionTransition
    , NoncurrentVersionTransition (..)
    , mkNoncurrentVersionTransition
    , nvtNoncurrentDays
    , nvtStorageClass

    -- ** DeleteMarkerReplicationStatus
    , DeleteMarkerReplicationStatus (..)

    -- ** QuoteFields
    , QuoteFields (..)

    -- ** Initiator
    , Initiator (..)
    , mkInitiator
    , iDisplayName
    , iID

    -- ** InventoryFrequency
    , InventoryFrequency (..)

    -- ** GrantReadACP
    , GrantReadACP (..)

    -- ** ObjectIdentifier
    , ObjectIdentifier (..)
    , mkObjectIdentifier
    , oiKey
    , oiVersionId

    -- ** CopySourceIfNoneMatch
    , CopySourceIfNoneMatch (..)

    -- ** Bucket
    , Bucket (..)
    , mkBucket
    , bCreationDate
    , bName

    -- ** ExistingObjectReplicationStatus
    , ExistingObjectReplicationStatus (..)

    -- ** ArchiveStatus
    , ArchiveStatus (..)

    -- ** SSECustomerAlgorithm
    , SSECustomerAlgorithm (..)

    -- ** Protocol
    , Protocol (..)

    -- ** InventoryDestination
    , InventoryDestination (..)
    , mkInventoryDestination
    , idS3BucketDestination

    -- ** JSONInput
    , JSONInput (..)
    , mkJSONInput
    , jsoniType

    -- ** RecordDelimiter
    , RecordDelimiter (..)

    -- ** AnalyticsExportDestination
    , AnalyticsExportDestination (..)
    , mkAnalyticsExportDestination
    , aedS3BucketDestination

    -- ** Grant
    , Grant (..)
    , mkGrant
    , gGrantee
    , gPermission

    -- ** ObjectOwnership
    , ObjectOwnership (..)

    -- ** InventoryOptionalField
    , InventoryOptionalField (..)

    -- ** SSECustomerKey
    , SSECustomerKey (..)

    -- ** URI
    , URI (..)

    -- ** IntelligentTieringAndOperator
    , IntelligentTieringAndOperator (..)
    , mkIntelligentTieringAndOperator
    , itaoPrefix
    , itaoTags

    -- ** ObjectLockConfiguration
    , ObjectLockConfiguration (..)
    , mkObjectLockConfiguration
    , olcObjectLockEnabled
    , olcRule

    -- ** StorageClassAnalysisSchemaVersion
    , StorageClassAnalysisSchemaVersion (..)

    -- ** InventoryIncludedObjectVersions
    , InventoryIncludedObjectVersions (..)

    -- ** RequestPayer
    , RequestPayer (..)

    -- ** TopicConfiguration
    , TopicConfiguration (..)
    , mkTopicConfiguration
    , tcTopicArn
    , tcEvents
    , tcFilter
    , tcId

    -- ** OwnershipControlsRule
    , OwnershipControlsRule (..)
    , mkOwnershipControlsRule
    , ocrObjectOwnership

    -- ** Stats
    , Stats (..)
    , mkStats
    , sBytesProcessed
    , sBytesReturned
    , sBytesScanned

    -- ** GrantWriteACP
    , GrantWriteACP (..)

    -- ** AbortRuleId
    , AbortRuleId (..)

    -- ** ObjectLockRetentionMode
    , ObjectLockRetentionMode (..)

    -- ** OutputSerialization
    , OutputSerialization (..)
    , mkOutputSerialization
    , osCSV
    , osJSON

    -- ** CSVOutput
    , CSVOutput (..)
    , mkCSVOutput
    , csvoFieldDelimiter
    , csvoQuoteCharacter
    , csvoQuoteEscapeCharacter
    , csvoQuoteFields
    , csvoRecordDelimiter

    -- ** QueueConfiguration
    , QueueConfiguration (..)
    , mkQueueConfiguration
    , qcQueueArn
    , qcEvents
    , qcFilter
    , qcId

    -- ** IntelligentTieringStatus
    , IntelligentTieringStatus (..)

    -- ** Owner
    , Owner (..)
    , mkOwner
    , oDisplayName
    , oID

    -- ** BucketLoggingStatus
    , BucketLoggingStatus (..)
    , mkBucketLoggingStatus
    , blsLoggingEnabled

    -- ** RestoreRequestType
    , RestoreRequestType (..)

    -- ** ReplicationRuleAndOperator
    , ReplicationRuleAndOperator (..)
    , mkReplicationRuleAndOperator
    , rraoPrefix
    , rraoTags

    -- ** HttpErrorCodeReturnedEquals
    , HttpErrorCodeReturnedEquals (..)

    -- ** StorageClassAnalysisDataExport
    , StorageClassAnalysisDataExport (..)
    , mkStorageClassAnalysisDataExport
    , scadeOutputSchemaVersion
    , scadeDestination

    -- ** ResponseContentEncoding
    , ResponseContentEncoding (..)

    -- ** SelectObjectContentEventStream
    , SelectObjectContentEventStream (..)
    , mkSelectObjectContentEventStream
    , socesCont
    , socesEnd
    , socesProgress
    , socesRecords
    , socesStats

    -- ** QueueArn
    , QueueArn (..)

    -- ** AccountId
    , AccountId (..)

    -- ** CopySourceIfMatch
    , CopySourceIfMatch (..)

    -- ** AllowedOrigin
    , AllowedOrigin (..)

    -- ** NextToken
    , NextToken (..)

    -- ** TopicArn
    , TopicArn (..)

    -- ** UploadIdMarker
    , UploadIdMarker (..)

    -- ** SseKmsEncryptedObjectsStatus
    , SseKmsEncryptedObjectsStatus (..)

    -- ** WebsiteRedirectLocation
    , WebsiteRedirectLocation (..)

    -- ** RequestProgress
    , RequestProgress (..)
    , mkRequestProgress
    , rpEnabled

    -- ** GrantRead
    , GrantRead (..)

    -- ** PublicAccessBlockConfiguration
    , PublicAccessBlockConfiguration (..)
    , mkPublicAccessBlockConfiguration
    , pabcBlockPublicAcls
    , pabcBlockPublicPolicy
    , pabcIgnorePublicAcls
    , pabcRestrictPublicBuckets

    -- ** DeleteMarkerVersionId
    , DeleteMarkerVersionId (..)

    -- ** AnalyticsS3BucketDestination
    , AnalyticsS3BucketDestination (..)
    , mkAnalyticsS3BucketDestination
    , asbdFormat
    , asbdBucket
    , asbdBucketAccountId
    , asbdPrefix

    -- ** MetricsAndOperator
    , MetricsAndOperator (..)
    , mkMetricsAndOperator
    , maoPrefix
    , maoTags

    -- ** PolicyStatus
    , PolicyStatus (..)
    , mkPolicyStatus
    , psIsPublic

    -- ** Role
    , Role (..)

    -- ** AcceptRanges
    , AcceptRanges (..)

    -- ** ReplicaKmsKeyID
    , ReplicaKmsKeyID (..)

    -- ** Range
    , Range (..)

    -- ** Encryption
    , Encryption (..)
    , mkEncryption
    , eEncryptionType
    , eKMSContext
    , eKMSKeyId

    -- ** MetricsFilter
    , MetricsFilter (..)
    , mkMetricsFilter
    , mfAnd
    , mfPrefix
    , mfTag

    -- ** S3KeyFilter
    , S3KeyFilter (..)
    , mkS3KeyFilter
    , skfFilterRules

    -- ** DefaultRetention
    , DefaultRetention (..)
    , mkDefaultRetention
    , drDays
    , drMode
    , drYears

    -- ** OwnershipControls
    , OwnershipControls (..)
    , mkOwnershipControls
    , ocRules

    -- ** JSONOutput
    , JSONOutput (..)
    , mkJSONOutput
    , jsonoRecordDelimiter

    -- ** InventorySchedule
    , InventorySchedule (..)
    , mkInventorySchedule
    , isFrequency

    -- ** FileHeaderInfo
    , FileHeaderInfo (..)

    -- ** ErrorDocument
    , ErrorDocument (..)
    , mkErrorDocument
    , edKey

    -- ** StorageClass
    , StorageClass (..)

    -- ** ObjectVersion
    , ObjectVersion (..)
    , mkObjectVersion
    , ovETag
    , ovIsLatest
    , ovKey
    , ovLastModified
    , ovOwner
    , ovSize
    , ovStorageClass
    , ovVersionId

    -- ** TargetGrant
    , TargetGrant (..)
    , mkTargetGrant
    , tgGrantee
    , tgPermission

    -- ** CopySourceVersionId
    , CopySourceVersionId (..)

    -- ** MFADeleteStatus
    , MFADeleteStatus (..)

    -- ** HttpRedirectCode
    , HttpRedirectCode (..)

    -- ** Payer
    , Payer (..)

    -- ** EncryptionConfiguration
    , EncryptionConfiguration (..)
    , mkEncryptionConfiguration
    , ecReplicaKmsKeyID

    -- ** AccelerateConfiguration
    , AccelerateConfiguration (..)
    , mkAccelerateConfiguration
    , acStatus

    -- ** Tiering
    , Tiering (..)
    , mkTiering
    , tDays
    , tAccessTier

    -- ** ExistingObjectReplication
    , ExistingObjectReplication (..)
    , mkExistingObjectReplication
    , eorStatus

    -- ** Redirect
    , Redirect (..)
    , mkRedirect
    , rHostName
    , rHttpRedirectCode
    , rProtocol
    , rReplaceKeyPrefixWith
    , rReplaceKeyWith

    -- ** MetricsId
    , MetricsId (..)

    -- ** SSECustomerKeyMD5
    , SSECustomerKeyMD5 (..)

    -- ** NextKeyMarker
    , NextKeyMarker (..)

    -- ** OutputLocation
    , OutputLocation (..)
    , mkOutputLocation
    , olS3

    -- ** Tier
    , Tier (..)

    -- ** BucketLogsPermission
    , BucketLogsPermission (..)

    -- ** EmailAddress
    , EmailAddress (..)

    -- ** Marker
    , Marker (..)

    -- ** SSEKMSKeyId
    , SSEKMSKeyId (..)

    -- ** KMSContext
    , KMSContext (..)

    -- ** MetadataKey
    , MetadataKey (..)

    -- ** Expression
    , Expression (..)

    -- ** RecordsEvent
    , RecordsEvent (..)
    , mkRecordsEvent
    , rePayload

    -- ** GlacierJobParameters
    , GlacierJobParameters (..)
    , mkGlacierJobParameters
    , gjpTier

    -- ** QuoteEscapeCharacter
    , QuoteEscapeCharacter (..)

    -- ** RestoreOutputPath
    , RestoreOutputPath (..)

    -- ** CompletedPart
    , CompletedPart (..)
    , mkCompletedPart
    , cpETag
    , cpPartNumber

    -- ** ReplicationRuleFilter
    , ReplicationRuleFilter (..)
    , mkReplicationRuleFilter
    , rrfAnd
    , rrfPrefix
    , rrfTag

    -- ** GrantFullControl
    , GrantFullControl (..)

    -- ** ContentEncoding
    , ContentEncoding (..)

    -- ** CreateBucketConfiguration
    , CreateBucketConfiguration (..)
    , mkCreateBucketConfiguration
    , cbcLocationConstraint

    -- ** Tagging
    , Tagging (..)
    , mkTagging
    , tTagSet

    -- ** NextMarker
    , NextMarker (..)

    -- ** LifecycleExpiration
    , LifecycleExpiration (..)
    , mkLifecycleExpiration
    , leDate
    , leDays
    , leExpiredObjectDeleteMarker

    -- ** ContentMD5
    , ContentMD5 (..)

    -- ** StatsEvent
    , StatsEvent (..)
    , mkStatsEvent
    , seDetails

    -- ** IntelligentTieringFilter
    , IntelligentTieringFilter (..)
    , mkIntelligentTieringFilter
    , itfAnd
    , itfPrefix
    , itfTag

    -- ** CORSConfiguration
    , CORSConfiguration (..)
    , mkCORSConfiguration
    , corscCORSRules

    -- ** ReplicationTimeStatus
    , ReplicationTimeStatus (..)

    -- ** AnalyticsAndOperator
    , AnalyticsAndOperator (..)
    , mkAnalyticsAndOperator
    , aaoPrefix
    , aaoTags

    -- ** NotificationConfigurationFilter
    , NotificationConfigurationFilter (..)
    , mkNotificationConfigurationFilter
    , ncfKey

    -- ** DisplayName
    , DisplayName (..)

    -- ** ID
    , ID (..)

    -- ** VersionIdMarker
    , VersionIdMarker (..)

    -- ** SSES3
    , SSES3 (..)
    , mkSSES3

    -- ** Object
    , Object (..)
    , mkObject
    , oETag
    , oKey
    , oLastModified
    , oOwner
    , oSize
    , oStorageClass

    -- ** CommonPrefix
    , CommonPrefix (..)
    , mkCommonPrefix
    , cpPrefix

    -- ** MultipartUpload
    , MultipartUpload (..)
    , mkMultipartUpload
    , muInitiated
    , muInitiator
    , muKey
    , muOwner
    , muStorageClass
    , muUploadId

    -- ** LambdaFunctionConfiguration
    , LambdaFunctionConfiguration (..)
    , mkLambdaFunctionConfiguration
    , lfcLambdaFunctionArn
    , lfcEvents
    , lfcFilter
    , lfcId

    -- ** Code
    , Code (..)

    -- ** OwnerOverride
    , OwnerOverride (..)

    -- ** S3Location
    , S3Location (..)
    , mkS3Location
    , slBucketName
    , slPrefix
    , slAccessControlList
    , slCannedACL
    , slEncryption
    , slStorageClass
    , slTagging
    , slUserMetadata

    -- ** SseKmsEncryptedObjects
    , SseKmsEncryptedObjects (..)
    , mkSseKmsEncryptedObjects
    , skeoStatus

    -- ** CopySource
    , CopySource (..)

    -- ** SSEKMS
    , SSEKMS (..)
    , mkSSEKMS
    , ssekmsKeyId

    -- ** ResponseCacheControl
    , ResponseCacheControl (..)

    -- ** Type
    , Type (..)

    -- ** BucketLifecycleConfiguration
    , BucketLifecycleConfiguration (..)
    , mkBucketLifecycleConfiguration
    , blcRules

    -- ** JSONType
    , JSONType (..)

    -- ** ReplicationStatus
    , ReplicationStatus (..)

    -- ** InputSerialization
    , InputSerialization (..)
    , mkInputSerialization
    , isCSV
    , isCompressionType
    , isJSON
    , isParquet

    -- ** ScanRange
    , ScanRange (..)
    , mkScanRange
    , srEnd
    , srStart

    -- ** InventoryS3BucketDestination
    , InventoryS3BucketDestination (..)
    , mkInventoryS3BucketDestination
    , isbdBucket
    , isbdFormat
    , isbdAccountId
    , isbdEncryption
    , isbdPrefix

    -- ** ObjectLockRule
    , ObjectLockRule (..)
    , mkObjectLockRule
    , olrDefaultRetention

    -- ** LifecycleRuleFilter
    , LifecycleRuleFilter (..)
    , mkLifecycleRuleFilter
    , lrfAnd
    , lrfPrefix
    , lrfTag

    -- ** SSEKMSEncryptionContext
    , SSEKMSEncryptionContext (..)

    -- ** TransitionStorageClass
    , TransitionStorageClass (..)

    -- ** CompletedMultipartUpload
    , CompletedMultipartUpload (..)
    , mkCompletedMultipartUpload
    , cmuParts

    -- ** Comments
    , Comments (..)

    -- ** CacheControl
    , CacheControl (..)

    -- ** Condition
    , Condition (..)
    , mkCondition
    , cHttpErrorCodeReturnedEquals
    , cKeyPrefixEquals

    -- ** IntelligentTieringAccessTier
    , IntelligentTieringAccessTier (..)

    -- ** ContentLanguage
    , ContentLanguage (..)

    -- ** NextUploadIdMarker
    , NextUploadIdMarker (..)

    -- ** Permission
    , Permission (..)

    -- ** AccessControlPolicy
    , AccessControlPolicy (..)
    , mkAccessControlPolicy
    , acpGrants
    , acpOwner

    -- ** Message
    , Message (..)

    -- ** BucketCannedACL
    , BucketCannedACL (..)

    -- ** MFADelete
    , MFADelete (..)

    -- ** GrantWrite
    , GrantWrite (..)

    -- ** SourceSelectionCriteria
    , SourceSelectionCriteria (..)
    , mkSourceSelectionCriteria
    , sscSseKmsEncryptedObjects

    -- ** Grantee
    , Grantee (..)
    , mkGrantee
    , gType
    , gDisplayName
    , gEmailAddress
    , gID
    , gURI

    -- ** CompressionType
    , CompressionType (..)

    -- ** CopySourceSSECustomerKey
    , CopySourceSSECustomerKey (..)

    -- ** ObjectLockLegalHoldStatus
    , ObjectLockLegalHoldStatus (..)

    -- ** LoggingEnabled
    , LoggingEnabled (..)
    , mkLoggingEnabled
    , leTargetBucket
    , leTargetPrefix
    , leTargetGrants

    -- ** Description
    , Description (..)

    -- ** AnalyticsConfiguration
    , AnalyticsConfiguration (..)
    , mkAnalyticsConfiguration
    , acId
    , acStorageClassAnalysis
    , acFilter

    -- ** MetadataValue
    , MetadataValue (..)

    -- ** CopySourceSSECustomerAlgorithm
    , CopySourceSSECustomerAlgorithm (..)

    -- ** FilterRule
    , FilterRule (..)
    , mkFilterRule
    , frName
    , frValue

    -- ** ContentDisposition
    , ContentDisposition (..)

    -- ** AnalyticsFilter
    , AnalyticsFilter (..)
    , mkAnalyticsFilter
    , afAnd
    , afPrefix
    , afTag

    -- ** IfNoneMatch
    , IfNoneMatch (..)

    -- ** ContentRange
    , ContentRange (..)

    -- ** ReplicationRuleStatus
    , ReplicationRuleStatus (..)

    -- ** ProgressEvent
    , ProgressEvent (..)
    , mkProgressEvent
    , peDetails

    -- ** ServerSideEncryption
    , ServerSideEncryption (..)

    -- ** IndexDocument
    , IndexDocument (..)
    , mkIndexDocument
    , idSuffix

    -- ** CopyObjectResult
    , CopyObjectResult (..)
    , mkCopyObjectResult
    , corETag
    , corLastModified

    -- ** TargetPrefix
    , TargetPrefix (..)

    -- ** InventoryFormat
    , InventoryFormat (..)

    -- ** FieldDelimiter
    , FieldDelimiter (..)

    -- ** MetadataEntry
    , MetadataEntry (..)
    , mkMetadataEntry
    , meName
    , meValue

    -- ** ContentType
    , ContentType (..)

    -- ** AbortIncompleteMultipartUpload
    , AbortIncompleteMultipartUpload (..)
    , mkAbortIncompleteMultipartUpload
    , aimuDaysAfterInitiation

    -- ** ReplaceKeyWith
    , ReplaceKeyWith (..)

    -- ** ServerSideEncryptionConfiguration
    , ServerSideEncryptionConfiguration (..)
    , mkServerSideEncryptionConfiguration
    , ssecRules

    -- ** Delete
    , Delete (..)
    , mkDelete
    , dObjects
    , dQuiet

    -- ** ReplicationTime
    , ReplicationTime (..)
    , mkReplicationTime
    , rtStatus
    , rtTime

    -- ** ReplaceKeyPrefixWith
    , ReplaceKeyPrefixWith (..)

    -- ** RestoreRequest
    , RestoreRequest (..)
    , mkRestoreRequest
    , rrDays
    , rrDescription
    , rrGlacierJobParameters
    , rrOutputLocation
    , rrSelectParameters
    , rrTier
    , rrType

    -- ** FilterRuleName
    , FilterRuleName (..)

    -- ** ServerSideEncryptionByDefault
    , ServerSideEncryptionByDefault (..)
    , mkServerSideEncryptionByDefault
    , ssebdSSEAlgorithm
    , ssebdKMSMasterKeyID

    -- ** ExpectedBucketOwner
    , ExpectedBucketOwner (..)

    -- ** ContinuationToken
    , ContinuationToken (..)

    -- ** Id
    , Id (..)

    -- ** Account
    , Account (..)

    -- ** NextContinuationToken
    , NextContinuationToken (..)

    -- ** Key
    , Key (..)

    -- ** VersionId
    , VersionId (..)

    -- ** UploadId
    , UploadId (..)

    -- ** Name
    , Name (..)

    -- ** ExpectedSourceBucketOwner
    , ExpectedSourceBucketOwner (..)

    -- ** KMSKeyId
    , KMSKeyId (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.S3.Types
import Network.AWS.S3.Waiters
import Network.AWS.S3.PutBucketRequestPayment
import Network.AWS.S3.PutObject
import Network.AWS.S3.DeleteObject
import Network.AWS.S3.PutBucketLogging
import Network.AWS.S3.GetBucketMetricsConfiguration
import Network.AWS.S3.ListBuckets
import Network.AWS.S3.DeleteBucket
import Network.AWS.S3.CreateBucket
import Network.AWS.S3.DeleteBucketTagging
import Network.AWS.S3.PutObjectAcl
import Network.AWS.S3.PutBucketTagging
import Network.AWS.S3.GetBucketInventoryConfiguration
import Network.AWS.S3.DeletePublicAccessBlock
import Network.AWS.S3.PutBucketInventoryConfiguration
import Network.AWS.S3.GetBucketLocation
import Network.AWS.S3.ListBucketInventoryConfigurations
import Network.AWS.S3.PutPublicAccessBlock
import Network.AWS.S3.DeleteBucketInventoryConfiguration
import Network.AWS.S3.GetBucketIntelligentTieringConfiguration
import Network.AWS.S3.GetBucketNotificationConfiguration
import Network.AWS.S3.GetObjectLockConfiguration
import Network.AWS.S3.PutObjectRetention
import Network.AWS.S3.PutBucketAccelerateConfiguration
import Network.AWS.S3.PutObjectLegalHold
import Network.AWS.S3.PutBucketOwnershipControls
import Network.AWS.S3.DeleteBucketOwnershipControls
import Network.AWS.S3.PutBucketMetricsConfiguration
import Network.AWS.S3.DeleteBucketMetricsConfiguration
import Network.AWS.S3.ListObjectsV2
import Network.AWS.S3.GetObject
import Network.AWS.S3.PutBucketReplication
import Network.AWS.S3.GetBucketWebsite
import Network.AWS.S3.GetBucketRequestPayment
import Network.AWS.S3.DeleteBucketReplication
import Network.AWS.S3.ListObjectVersions
import Network.AWS.S3.HeadBucket
import Network.AWS.S3.DeleteBucketLifecycle
import Network.AWS.S3.PutBucketLifecycleConfiguration
import Network.AWS.S3.PutBucketAnalyticsConfiguration
import Network.AWS.S3.ListBucketAnalyticsConfigurations
import Network.AWS.S3.DeleteBucketAnalyticsConfiguration
import Network.AWS.S3.CreateMultipartUpload
import Network.AWS.S3.GetBucketPolicyStatus
import Network.AWS.S3.UploadPart
import Network.AWS.S3.SelectObjectContent
import Network.AWS.S3.GetBucketReplication
import Network.AWS.S3.PutBucketWebsite
import Network.AWS.S3.DeleteBucketWebsite
import Network.AWS.S3.CompleteMultipartUpload
import Network.AWS.S3.ListMultipartUploads
import Network.AWS.S3.ListObjects
import Network.AWS.S3.GetBucketOwnershipControls
import Network.AWS.S3.GetObjectLegalHold
import Network.AWS.S3.GetObjectRetention
import Network.AWS.S3.DeleteBucketPolicy
import Network.AWS.S3.GetBucketEncryption
import Network.AWS.S3.AbortMultipartUpload
import Network.AWS.S3.PutBucketPolicy
import Network.AWS.S3.GetBucketAccelerateConfiguration
import Network.AWS.S3.GetObjectTorrent
import Network.AWS.S3.DeleteObjects
import Network.AWS.S3.PutObjectLockConfiguration
import Network.AWS.S3.PutBucketNotificationConfiguration
import Network.AWS.S3.GetBucketVersioning
import Network.AWS.S3.DeleteBucketCors
import Network.AWS.S3.DeleteBucketIntelligentTieringConfiguration
import Network.AWS.S3.ListBucketIntelligentTieringConfigurations
import Network.AWS.S3.PutBucketCors
import Network.AWS.S3.GetPublicAccessBlock
import Network.AWS.S3.PutBucketIntelligentTieringConfiguration
import Network.AWS.S3.GetBucketCors
import Network.AWS.S3.GetObjectAcl
import Network.AWS.S3.RestoreObject
import Network.AWS.S3.HeadObject
import Network.AWS.S3.PutBucketVersioning
import Network.AWS.S3.GetBucketTagging
import Network.AWS.S3.CopyObject
import Network.AWS.S3.ListBucketMetricsConfigurations
import Network.AWS.S3.GetBucketPolicy
import Network.AWS.S3.PutBucketEncryption
import Network.AWS.S3.DeleteBucketEncryption
import Network.AWS.S3.GetBucketLogging
import Network.AWS.S3.GetBucketAcl
import Network.AWS.S3.GetBucketLifecycleConfiguration
import Network.AWS.S3.GetBucketAnalyticsConfiguration
import Network.AWS.S3.GetObjectTagging
import Network.AWS.S3.ListParts
import Network.AWS.S3.DeleteObjectTagging
import Network.AWS.S3.UploadPartCopy
import Network.AWS.S3.PutObjectTagging
import Network.AWS.S3.PutBucketAcl
import Network.AWS.S3.Internal
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'S3'.
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
