{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
  ( -- * Service configuration
    s3Service,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** ObjectNotExists
    mkObjectNotExists,

    -- ** BucketExists
    mkBucketExists,

    -- ** ObjectExists
    mkObjectExists,

    -- ** BucketNotExists
    mkBucketNotExists,

    -- * Operations
    -- $operations

    -- ** PutBucketRequestPayment
    module Network.AWS.S3.PutBucketRequestPayment,

    -- ** PutObject
    module Network.AWS.S3.PutObject,

    -- ** DeleteObject
    module Network.AWS.S3.DeleteObject,

    -- ** PutBucketLogging
    module Network.AWS.S3.PutBucketLogging,

    -- ** GetBucketMetricsConfiguration
    module Network.AWS.S3.GetBucketMetricsConfiguration,

    -- ** ListBuckets
    module Network.AWS.S3.ListBuckets,

    -- ** DeleteBucket
    module Network.AWS.S3.DeleteBucket,

    -- ** CreateBucket
    module Network.AWS.S3.CreateBucket,

    -- ** DeleteBucketTagging
    module Network.AWS.S3.DeleteBucketTagging,

    -- ** PutObjectACL
    module Network.AWS.S3.PutObjectACL,

    -- ** PutBucketTagging
    module Network.AWS.S3.PutBucketTagging,

    -- ** GetBucketInventoryConfiguration
    module Network.AWS.S3.GetBucketInventoryConfiguration,

    -- ** DeletePublicAccessBlock
    module Network.AWS.S3.DeletePublicAccessBlock,

    -- ** PutBucketInventoryConfiguration
    module Network.AWS.S3.PutBucketInventoryConfiguration,

    -- ** GetBucketLocation
    module Network.AWS.S3.GetBucketLocation,

    -- ** ListBucketInventoryConfigurations
    module Network.AWS.S3.ListBucketInventoryConfigurations,

    -- ** PutPublicAccessBlock
    module Network.AWS.S3.PutPublicAccessBlock,

    -- ** DeleteBucketInventoryConfiguration
    module Network.AWS.S3.DeleteBucketInventoryConfiguration,

    -- ** GetBucketIntelligentTieringConfiguration
    module Network.AWS.S3.GetBucketIntelligentTieringConfiguration,

    -- ** GetBucketNotificationConfiguration
    module Network.AWS.S3.GetBucketNotificationConfiguration,

    -- ** GetObjectLockConfiguration
    module Network.AWS.S3.GetObjectLockConfiguration,

    -- ** PutObjectRetention
    module Network.AWS.S3.PutObjectRetention,

    -- ** PutBucketAccelerateConfiguration
    module Network.AWS.S3.PutBucketAccelerateConfiguration,

    -- ** PutObjectLegalHold
    module Network.AWS.S3.PutObjectLegalHold,

    -- ** PutBucketOwnershipControls
    module Network.AWS.S3.PutBucketOwnershipControls,

    -- ** DeleteBucketOwnershipControls
    module Network.AWS.S3.DeleteBucketOwnershipControls,

    -- ** PutBucketMetricsConfiguration
    module Network.AWS.S3.PutBucketMetricsConfiguration,

    -- ** DeleteBucketMetricsConfiguration
    module Network.AWS.S3.DeleteBucketMetricsConfiguration,

    -- ** ListObjectsV2 (Paginated)
    module Network.AWS.S3.ListObjectsV2,

    -- ** GetObject
    module Network.AWS.S3.GetObject,

    -- ** PutBucketReplication
    module Network.AWS.S3.PutBucketReplication,

    -- ** GetBucketWebsite
    module Network.AWS.S3.GetBucketWebsite,

    -- ** GetBucketRequestPayment
    module Network.AWS.S3.GetBucketRequestPayment,

    -- ** DeleteBucketReplication
    module Network.AWS.S3.DeleteBucketReplication,

    -- ** ListObjectVersions (Paginated)
    module Network.AWS.S3.ListObjectVersions,

    -- ** HeadBucket
    module Network.AWS.S3.HeadBucket,

    -- ** DeleteBucketLifecycle
    module Network.AWS.S3.DeleteBucketLifecycle,

    -- ** PutBucketLifecycleConfiguration
    module Network.AWS.S3.PutBucketLifecycleConfiguration,

    -- ** PutBucketAnalyticsConfiguration
    module Network.AWS.S3.PutBucketAnalyticsConfiguration,

    -- ** ListBucketAnalyticsConfigurations
    module Network.AWS.S3.ListBucketAnalyticsConfigurations,

    -- ** DeleteBucketAnalyticsConfiguration
    module Network.AWS.S3.DeleteBucketAnalyticsConfiguration,

    -- ** CreateMultipartUpload
    module Network.AWS.S3.CreateMultipartUpload,

    -- ** GetBucketPolicyStatus
    module Network.AWS.S3.GetBucketPolicyStatus,

    -- ** UploadPart
    module Network.AWS.S3.UploadPart,

    -- ** SelectObjectContent
    module Network.AWS.S3.SelectObjectContent,

    -- ** GetBucketReplication
    module Network.AWS.S3.GetBucketReplication,

    -- ** PutBucketWebsite
    module Network.AWS.S3.PutBucketWebsite,

    -- ** DeleteBucketWebsite
    module Network.AWS.S3.DeleteBucketWebsite,

    -- ** CompleteMultipartUpload
    module Network.AWS.S3.CompleteMultipartUpload,

    -- ** ListMultipartUploads (Paginated)
    module Network.AWS.S3.ListMultipartUploads,

    -- ** ListObjects (Paginated)
    module Network.AWS.S3.ListObjects,

    -- ** GetBucketOwnershipControls
    module Network.AWS.S3.GetBucketOwnershipControls,

    -- ** GetObjectLegalHold
    module Network.AWS.S3.GetObjectLegalHold,

    -- ** GetObjectRetention
    module Network.AWS.S3.GetObjectRetention,

    -- ** DeleteBucketPolicy
    module Network.AWS.S3.DeleteBucketPolicy,

    -- ** GetBucketEncryption
    module Network.AWS.S3.GetBucketEncryption,

    -- ** AbortMultipartUpload
    module Network.AWS.S3.AbortMultipartUpload,

    -- ** PutBucketPolicy
    module Network.AWS.S3.PutBucketPolicy,

    -- ** GetBucketAccelerateConfiguration
    module Network.AWS.S3.GetBucketAccelerateConfiguration,

    -- ** GetObjectTorrent
    module Network.AWS.S3.GetObjectTorrent,

    -- ** DeleteObjects
    module Network.AWS.S3.DeleteObjects,

    -- ** PutObjectLockConfiguration
    module Network.AWS.S3.PutObjectLockConfiguration,

    -- ** PutBucketNotificationConfiguration
    module Network.AWS.S3.PutBucketNotificationConfiguration,

    -- ** GetBucketVersioning
    module Network.AWS.S3.GetBucketVersioning,

    -- ** DeleteBucketCORS
    module Network.AWS.S3.DeleteBucketCORS,

    -- ** DeleteBucketIntelligentTieringConfiguration
    module Network.AWS.S3.DeleteBucketIntelligentTieringConfiguration,

    -- ** ListBucketIntelligentTieringConfigurations
    module Network.AWS.S3.ListBucketIntelligentTieringConfigurations,

    -- ** PutBucketCORS
    module Network.AWS.S3.PutBucketCORS,

    -- ** GetPublicAccessBlock
    module Network.AWS.S3.GetPublicAccessBlock,

    -- ** PutBucketIntelligentTieringConfiguration
    module Network.AWS.S3.PutBucketIntelligentTieringConfiguration,

    -- ** GetBucketCORS
    module Network.AWS.S3.GetBucketCORS,

    -- ** GetObjectACL
    module Network.AWS.S3.GetObjectACL,

    -- ** RestoreObject
    module Network.AWS.S3.RestoreObject,

    -- ** HeadObject
    module Network.AWS.S3.HeadObject,

    -- ** PutBucketVersioning
    module Network.AWS.S3.PutBucketVersioning,

    -- ** GetBucketTagging
    module Network.AWS.S3.GetBucketTagging,

    -- ** CopyObject
    module Network.AWS.S3.CopyObject,

    -- ** ListBucketMetricsConfigurations
    module Network.AWS.S3.ListBucketMetricsConfigurations,

    -- ** GetBucketPolicy
    module Network.AWS.S3.GetBucketPolicy,

    -- ** PutBucketEncryption
    module Network.AWS.S3.PutBucketEncryption,

    -- ** DeleteBucketEncryption
    module Network.AWS.S3.DeleteBucketEncryption,

    -- ** GetBucketLogging
    module Network.AWS.S3.GetBucketLogging,

    -- ** GetBucketACL
    module Network.AWS.S3.GetBucketACL,

    -- ** GetBucketLifecycleConfiguration
    module Network.AWS.S3.GetBucketLifecycleConfiguration,

    -- ** GetBucketAnalyticsConfiguration
    module Network.AWS.S3.GetBucketAnalyticsConfiguration,

    -- ** GetObjectTagging
    module Network.AWS.S3.GetObjectTagging,

    -- ** ListParts (Paginated)
    module Network.AWS.S3.ListParts,

    -- ** DeleteObjectTagging
    module Network.AWS.S3.DeleteObjectTagging,

    -- ** UploadPartCopy
    module Network.AWS.S3.UploadPartCopy,

    -- ** PutObjectTagging
    module Network.AWS.S3.PutObjectTagging,

    -- ** PutBucketACL
    module Network.AWS.S3.PutBucketACL,

    -- * Types

    -- ** Common
    module Network.AWS.S3.Internal,

    -- ** AnalyticsS3ExportFileFormat
    AnalyticsS3ExportFileFormat (..),

    -- ** ArchiveStatus
    ArchiveStatus (..),

    -- ** BucketAccelerateStatus
    BucketAccelerateStatus (..),

    -- ** BucketCannedACL
    BucketCannedACL (..),

    -- ** BucketLogsPermission
    BucketLogsPermission (..),

    -- ** BucketVersioningStatus
    BucketVersioningStatus (..),

    -- ** CompressionType
    CompressionType (..),

    -- ** DeleteMarkerReplicationStatus
    DeleteMarkerReplicationStatus (..),

    -- ** EncodingType
    EncodingType (..),

    -- ** Event
    Event (..),

    -- ** ExistingObjectReplicationStatus
    ExistingObjectReplicationStatus (..),

    -- ** ExpirationStatus
    ExpirationStatus (..),

    -- ** ExpressionType
    ExpressionType (..),

    -- ** FileHeaderInfo
    FileHeaderInfo (..),

    -- ** FilterRuleName
    FilterRuleName (..),

    -- ** IntelligentTieringAccessTier
    IntelligentTieringAccessTier (..),

    -- ** IntelligentTieringStatus
    IntelligentTieringStatus (..),

    -- ** InventoryFormat
    InventoryFormat (..),

    -- ** InventoryFrequency
    InventoryFrequency (..),

    -- ** InventoryIncludedObjectVersions
    InventoryIncludedObjectVersions (..),

    -- ** InventoryOptionalField
    InventoryOptionalField (..),

    -- ** JSONType
    JSONType (..),

    -- ** MFADelete
    MFADelete (..),

    -- ** MFADeleteStatus
    MFADeleteStatus (..),

    -- ** MetadataDirective
    MetadataDirective (..),

    -- ** MetricsStatus
    MetricsStatus (..),

    -- ** ObjectCannedACL
    ObjectCannedACL (..),

    -- ** ObjectLockEnabled
    ObjectLockEnabled (..),

    -- ** ObjectLockLegalHoldStatus
    ObjectLockLegalHoldStatus (..),

    -- ** ObjectLockMode
    ObjectLockMode (..),

    -- ** ObjectLockRetentionMode
    ObjectLockRetentionMode (..),

    -- ** ObjectOwnership
    ObjectOwnership (..),

    -- ** ObjectStorageClass
    ObjectStorageClass (..),

    -- ** ObjectVersionStorageClass
    ObjectVersionStorageClass (..),

    -- ** OwnerOverride
    OwnerOverride (..),

    -- ** Payer
    Payer (..),

    -- ** Permission
    Permission (..),

    -- ** Protocol
    Protocol (..),

    -- ** QuoteFields
    QuoteFields (..),

    -- ** ReplicationRuleStatus
    ReplicationRuleStatus (..),

    -- ** ReplicationStatus
    ReplicationStatus (..),

    -- ** ReplicationTimeStatus
    ReplicationTimeStatus (..),

    -- ** RequestCharged
    RequestCharged (..),

    -- ** RequestPayer
    RequestPayer (..),

    -- ** RestoreRequestType
    RestoreRequestType (..),

    -- ** ServerSideEncryption
    ServerSideEncryption (..),

    -- ** SseKMSEncryptedObjectsStatus
    SseKMSEncryptedObjectsStatus (..),

    -- ** StorageClass
    StorageClass (..),

    -- ** StorageClassAnalysisSchemaVersion
    StorageClassAnalysisSchemaVersion (..),

    -- ** TaggingDirective
    TaggingDirective (..),

    -- ** Tier
    Tier (..),

    -- ** TransitionStorageClass
    TransitionStorageClass (..),

    -- ** Type
    Type (..),

    -- ** AbortIncompleteMultipartUpload
    AbortIncompleteMultipartUpload (..),
    mkAbortIncompleteMultipartUpload,
    aimuDaysAfterInitiation,

    -- ** AccelerateConfiguration
    AccelerateConfiguration (..),
    mkAccelerateConfiguration,
    acStatus,

    -- ** AccessControlPolicy
    AccessControlPolicy (..),
    mkAccessControlPolicy,
    acpGrants,
    acpOwner,

    -- ** AccessControlTranslation
    AccessControlTranslation (..),
    mkAccessControlTranslation,
    actOwner,

    -- ** AnalyticsAndOperator
    AnalyticsAndOperator (..),
    mkAnalyticsAndOperator,
    aaoPrefix,
    aaoTags,

    -- ** AnalyticsConfiguration
    AnalyticsConfiguration (..),
    mkAnalyticsConfiguration,
    acStorageClassAnalysis,
    acId,
    acFilter,

    -- ** AnalyticsExportDestination
    AnalyticsExportDestination (..),
    mkAnalyticsExportDestination,
    aedS3BucketDestination,

    -- ** AnalyticsFilter
    AnalyticsFilter (..),
    mkAnalyticsFilter,
    afTag,
    afPrefix,
    afAnd,

    -- ** AnalyticsS3BucketDestination
    AnalyticsS3BucketDestination (..),
    mkAnalyticsS3BucketDestination,
    asbdBucketAccountId,
    asbdPrefix,
    asbdFormat,
    asbdBucket,

    -- ** Bucket
    Bucket (..),
    mkBucket,
    bName,
    bCreationDate,

    -- ** BucketLifecycleConfiguration
    BucketLifecycleConfiguration (..),
    mkBucketLifecycleConfiguration,
    blcRules,

    -- ** BucketLoggingStatus
    BucketLoggingStatus (..),
    mkBucketLoggingStatus,
    blsLoggingEnabled,

    -- ** CORSConfiguration
    CORSConfiguration (..),
    mkCORSConfiguration,
    ccCORSRules,

    -- ** CORSRule
    CORSRule (..),
    mkCORSRule,
    crAllowedMethods,
    crMaxAgeSeconds,
    crAllowedHeaders,
    crAllowedOrigins,
    crExposeHeaders,

    -- ** CSVInput
    CSVInput (..),
    mkCSVInput,
    ciQuoteCharacter,
    ciRecordDelimiter,
    ciAllowQuotedRecordDelimiter,
    ciFileHeaderInfo,
    ciQuoteEscapeCharacter,
    ciComments,
    ciFieldDelimiter,

    -- ** CSVOutput
    CSVOutput (..),
    mkCSVOutput,
    coQuoteCharacter,
    coQuoteFields,
    coRecordDelimiter,
    coQuoteEscapeCharacter,
    coFieldDelimiter,

    -- ** CommonPrefix
    CommonPrefix (..),
    mkCommonPrefix,
    cpPrefix,

    -- ** CompletedMultipartUpload
    CompletedMultipartUpload (..),
    mkCompletedMultipartUpload,
    cmuParts,

    -- ** CompletedPart
    CompletedPart (..),
    mkCompletedPart,
    cpETag,
    cpPartNumber,

    -- ** Condition
    Condition (..),
    mkCondition,
    cKeyPrefixEquals,
    cHTTPErrorCodeReturnedEquals,

    -- ** ContinuationEvent
    ContinuationEvent (..),
    mkContinuationEvent,

    -- ** CopyObjectResult
    CopyObjectResult (..),
    mkCopyObjectResult,
    corETag,
    corLastModified,

    -- ** CopyPartResult
    CopyPartResult (..),
    mkCopyPartResult,
    cprETag,
    cprLastModified,

    -- ** CreateBucketConfiguration
    CreateBucketConfiguration (..),
    mkCreateBucketConfiguration,
    cbcLocationConstraint,

    -- ** DefaultRetention
    DefaultRetention (..),
    mkDefaultRetention,
    drDays,
    drMode,
    drYears,

    -- ** Delete
    Delete (..),
    mkDelete,
    dQuiet,
    dObjects,

    -- ** DeleteMarkerEntry
    DeleteMarkerEntry (..),
    mkDeleteMarkerEntry,
    dmeVersionId,
    dmeIsLatest,
    dmeOwner,
    dmeKey,
    dmeLastModified,

    -- ** DeleteMarkerReplication
    DeleteMarkerReplication (..),
    mkDeleteMarkerReplication,
    dmrStatus,

    -- ** DeletedObject
    DeletedObject (..),
    mkDeletedObject,
    doVersionId,
    doDeleteMarker,
    doDeleteMarkerVersionId,
    doKey,

    -- ** Destination
    Destination (..),
    mkDestination,
    dMetrics,
    dAccessControlTranslation,
    dBucket,
    dAccount,
    dStorageClass,
    dEncryptionConfiguration,
    dReplicationTime,

    -- ** Encryption
    Encryption (..),
    mkEncryption,
    eEncryptionType,
    eKMSKeyId,
    eKMSContext,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecReplicaKMSKeyId,

    -- ** EndEvent
    EndEvent (..),
    mkEndEvent,

    -- ** ErrorDocument
    ErrorDocument (..),
    mkErrorDocument,
    edKey,

    -- ** ExistingObjectReplication
    ExistingObjectReplication (..),
    mkExistingObjectReplication,
    eorStatus,

    -- ** FilterRule
    FilterRule (..),
    mkFilterRule,
    frValue,
    frName,

    -- ** GlacierJobParameters
    GlacierJobParameters (..),
    mkGlacierJobParameters,
    gjpTier,

    -- ** Grant
    Grant (..),
    mkGrant,
    gPermission,
    gGrantee,

    -- ** Grantee
    Grantee (..),
    mkGrantee,
    gURI,
    gEmailAddress,
    gDisplayName,
    gId,
    gType,

    -- ** IndexDocument
    IndexDocument (..),
    mkIndexDocument,
    idSuffix,

    -- ** Initiator
    Initiator (..),
    mkInitiator,
    iDisplayName,
    iId,

    -- ** InputSerialization
    InputSerialization (..),
    mkInputSerialization,
    isJSON,
    isCSV,
    isParquet,
    isCompressionType,

    -- ** IntelligentTieringAndOperator
    IntelligentTieringAndOperator (..),
    mkIntelligentTieringAndOperator,
    itaoPrefix,
    itaoTags,

    -- ** IntelligentTieringConfiguration
    IntelligentTieringConfiguration (..),
    mkIntelligentTieringConfiguration,
    itcStatus,
    itcTierings,
    itcId,
    itcFilter,

    -- ** IntelligentTieringFilter
    IntelligentTieringFilter (..),
    mkIntelligentTieringFilter,
    itfTag,
    itfPrefix,
    itfAnd,

    -- ** InventoryConfiguration
    InventoryConfiguration (..),
    mkInventoryConfiguration,
    icIncludedObjectVersions,
    icDestination,
    icSchedule,
    icIsEnabled,
    icOptionalFields,
    icId,
    icFilter,

    -- ** InventoryDestination
    InventoryDestination (..),
    mkInventoryDestination,
    idS3BucketDestination,

    -- ** InventoryEncryption
    InventoryEncryption (..),
    mkInventoryEncryption,
    ieSSES3,
    ieSSEKMS,

    -- ** InventoryFilter
    InventoryFilter (..),
    mkInventoryFilter,
    ifPrefix,

    -- ** InventoryS3BucketDestination
    InventoryS3BucketDestination (..),
    mkInventoryS3BucketDestination,
    isbdPrefix,
    isbdFormat,
    isbdBucket,
    isbdAccountId,
    isbdEncryption,

    -- ** InventorySchedule
    InventorySchedule (..),
    mkInventorySchedule,
    isFrequency,

    -- ** JSONInput
    JSONInput (..),
    mkJSONInput,
    jiType,

    -- ** JSONOutput
    JSONOutput (..),
    mkJSONOutput,
    joRecordDelimiter,

    -- ** LambdaFunctionConfiguration
    LambdaFunctionConfiguration (..),
    mkLambdaFunctionConfiguration,
    lfcLambdaFunctionARN,
    lfcEvents,
    lfcId,
    lfcFilter,

    -- ** LifecycleExpiration
    LifecycleExpiration (..),
    mkLifecycleExpiration,
    leDays,
    leDate,
    leExpiredObjectDeleteMarker,

    -- ** LifecycleRule
    LifecycleRule (..),
    mkLifecycleRule,
    lrStatus,
    lrTransitions,
    lrNoncurrentVersionExpiration,
    lrPrefix,
    lrNoncurrentVersionTransitions,
    lrExpiration,
    lrId,
    lrFilter,
    lrAbortIncompleteMultipartUpload,

    -- ** LifecycleRuleAndOperator
    LifecycleRuleAndOperator (..),
    mkLifecycleRuleAndOperator,
    lraoPrefix,
    lraoTags,

    -- ** LifecycleRuleFilter
    LifecycleRuleFilter (..),
    mkLifecycleRuleFilter,
    lrfTag,
    lrfPrefix,
    lrfAnd,

    -- ** LoggingEnabled
    LoggingEnabled (..),
    mkLoggingEnabled,
    leTargetBucket,
    leTargetGrants,
    leTargetPrefix,

    -- ** MetadataEntry
    MetadataEntry (..),
    mkMetadataEntry,
    meValue,
    meName,

    -- ** Metrics
    Metrics (..),
    mkMetrics,
    mStatus,
    mEventThreshold,

    -- ** MetricsAndOperator
    MetricsAndOperator (..),
    mkMetricsAndOperator,
    maoPrefix,
    maoTags,

    -- ** MetricsConfiguration
    MetricsConfiguration (..),
    mkMetricsConfiguration,
    mcId,
    mcFilter,

    -- ** MetricsFilter
    MetricsFilter (..),
    mkMetricsFilter,
    mfTag,
    mfPrefix,
    mfAnd,

    -- ** MultipartUpload
    MultipartUpload (..),
    mkMultipartUpload,
    muInitiated,
    muInitiator,
    muOwner,
    muKey,
    muStorageClass,
    muUploadId,

    -- ** NoncurrentVersionExpiration
    NoncurrentVersionExpiration (..),
    mkNoncurrentVersionExpiration,
    nveNoncurrentDays,

    -- ** NoncurrentVersionTransition
    NoncurrentVersionTransition (..),
    mkNoncurrentVersionTransition,
    nvtStorageClass,
    nvtNoncurrentDays,

    -- ** NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncQueueConfigurations,
    ncTopicConfigurations,
    ncLambdaFunctionConfigurations,

    -- ** NotificationConfigurationFilter
    NotificationConfigurationFilter (..),
    mkNotificationConfigurationFilter,
    ncfKey,

    -- ** Object
    Object (..),
    mkObject,
    oETag,
    oSize,
    oOwner,
    oKey,
    oStorageClass,
    oLastModified,

    -- ** ObjectIdentifier
    ObjectIdentifier (..),
    mkObjectIdentifier,
    oiVersionId,
    oiKey,

    -- ** ObjectLockConfiguration
    ObjectLockConfiguration (..),
    mkObjectLockConfiguration,
    olcObjectLockEnabled,
    olcRule,

    -- ** ObjectLockLegalHold
    ObjectLockLegalHold (..),
    mkObjectLockLegalHold,
    ollhStatus,

    -- ** ObjectLockRetention
    ObjectLockRetention (..),
    mkObjectLockRetention,
    olrMode,
    olrRetainUntilDate,

    -- ** ObjectLockRule
    ObjectLockRule (..),
    mkObjectLockRule,
    olrDefaultRetention,

    -- ** ObjectVersion
    ObjectVersion (..),
    mkObjectVersion,
    ovETag,
    ovVersionId,
    ovSize,
    ovIsLatest,
    ovOwner,
    ovKey,
    ovStorageClass,
    ovLastModified,

    -- ** OutputLocation
    OutputLocation (..),
    mkOutputLocation,
    olS3,

    -- ** OutputSerialization
    OutputSerialization (..),
    mkOutputSerialization,
    osJSON,
    osCSV,

    -- ** Owner
    Owner (..),
    mkOwner,
    oDisplayName,
    oId,

    -- ** OwnershipControls
    OwnershipControls (..),
    mkOwnershipControls,
    ocRules,

    -- ** OwnershipControlsRule
    OwnershipControlsRule (..),
    mkOwnershipControlsRule,
    ocrObjectOwnership,

    -- ** ParquetInput
    ParquetInput (..),
    mkParquetInput,

    -- ** Part
    Part (..),
    mkPart,
    pETag,
    pSize,
    pPartNumber,
    pLastModified,

    -- ** PolicyStatus
    PolicyStatus (..),
    mkPolicyStatus,
    psIsPublic,

    -- ** Progress
    Progress (..),
    mkProgress,
    pBytesReturned,
    pBytesScanned,
    pBytesProcessed,

    -- ** ProgressEvent
    ProgressEvent (..),
    mkProgressEvent,
    peDetails,

    -- ** PublicAccessBlockConfiguration
    PublicAccessBlockConfiguration (..),
    mkPublicAccessBlockConfiguration,
    pabcIgnorePublicACLs,
    pabcBlockPublicACLs,
    pabcRestrictPublicBuckets,
    pabcBlockPublicPolicy,

    -- ** QueueConfiguration
    QueueConfiguration (..),
    mkQueueConfiguration,
    qcQueueARN,
    qcEvents,
    qcId,
    qcFilter,

    -- ** RecordsEvent
    RecordsEvent (..),
    mkRecordsEvent,
    rePayload,

    -- ** Redirect
    Redirect (..),
    mkRedirect,
    rHostName,
    rProtocol,
    rHTTPRedirectCode,
    rReplaceKeyWith,
    rReplaceKeyPrefixWith,

    -- ** RedirectAllRequestsTo
    RedirectAllRequestsTo (..),
    mkRedirectAllRequestsTo,
    rartHostName,
    rartProtocol,

    -- ** ReplicationConfiguration
    ReplicationConfiguration (..),
    mkReplicationConfiguration,
    rcRules,
    rcRole,

    -- ** ReplicationRule
    ReplicationRule (..),
    mkReplicationRule,
    rrStatus,
    rrDestination,
    rrDeleteMarkerReplication,
    rrPriority,
    rrPrefix,
    rrExistingObjectReplication,
    rrId,
    rrFilter,
    rrSourceSelectionCriteria,

    -- ** ReplicationRuleAndOperator
    ReplicationRuleAndOperator (..),
    mkReplicationRuleAndOperator,
    rraoPrefix,
    rraoTags,

    -- ** ReplicationRuleFilter
    ReplicationRuleFilter (..),
    mkReplicationRuleFilter,
    rrfTag,
    rrfPrefix,
    rrfAnd,

    -- ** ReplicationTime
    ReplicationTime (..),
    mkReplicationTime,
    rtStatus,
    rtTime,

    -- ** ReplicationTimeValue
    ReplicationTimeValue (..),
    mkReplicationTimeValue,
    rtvMinutes,

    -- ** RequestPaymentConfiguration
    RequestPaymentConfiguration (..),
    mkRequestPaymentConfiguration,
    rpcPayer,

    -- ** RequestProgress
    RequestProgress (..),
    mkRequestProgress,
    rpEnabled,

    -- ** RestoreRequest
    RestoreRequest (..),
    mkRestoreRequest,
    rrDays,
    rrSelectParameters,
    rrOutputLocation,
    rrTier,
    rrGlacierJobParameters,
    rrType,
    rrDescription,

    -- ** RoutingRule
    RoutingRule (..),
    mkRoutingRule,
    rrRedirect,
    rrCondition,

    -- ** S3KeyFilter
    S3KeyFilter (..),
    mkS3KeyFilter,
    skfFilterRules,

    -- ** S3Location
    S3Location (..),
    mkS3Location,
    slCannedACL,
    slPrefix,
    slBucketName,
    slAccessControlList,
    slUserMetadata,
    slEncryption,
    slStorageClass,
    slTagging,

    -- ** S3ServiceError
    S3ServiceError (..),
    mkS3ServiceError,
    sseVersionId,
    sseKey,
    sseCode,
    sseMessage,

    -- ** SSEKMS
    SSEKMS (..),
    mkSSEKMS,
    ssekKeyId,

    -- ** SSES3
    SSES3 (..),
    mkSSES3,

    -- ** ScanRange
    ScanRange (..),
    mkScanRange,
    srStart,
    srEnd,

    -- ** SelectObjectContentEventStream
    SelectObjectContentEventStream (..),
    mkSelectObjectContentEventStream,
    socesProgress,
    socesRecords,
    socesCont,
    socesStats,
    socesEnd,

    -- ** SelectParameters
    SelectParameters (..),
    mkSelectParameters,
    spExpressionType,
    spOutputSerialization,
    spExpression,
    spInputSerialization,

    -- ** ServerSideEncryptionByDefault
    ServerSideEncryptionByDefault (..),
    mkServerSideEncryptionByDefault,
    ssebdSSEAlgorithm,
    ssebdKMSMasterKeyId,

    -- ** ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration (..),
    mkServerSideEncryptionConfiguration,
    ssecRules,

    -- ** ServerSideEncryptionRule
    ServerSideEncryptionRule (..),
    mkServerSideEncryptionRule,
    sserApplyServerSideEncryptionByDefault,

    -- ** SourceSelectionCriteria
    SourceSelectionCriteria (..),
    mkSourceSelectionCriteria,
    sscSseKMSEncryptedObjects,

    -- ** SseKMSEncryptedObjects
    SseKMSEncryptedObjects (..),
    mkSseKMSEncryptedObjects,
    skeoStatus,

    -- ** Stats
    Stats (..),
    mkStats,
    sBytesReturned,
    sBytesScanned,
    sBytesProcessed,

    -- ** StatsEvent
    StatsEvent (..),
    mkStatsEvent,
    seDetails,

    -- ** StorageClassAnalysis
    StorageClassAnalysis (..),
    mkStorageClassAnalysis,
    scaDataExport,

    -- ** StorageClassAnalysisDataExport
    StorageClassAnalysisDataExport (..),
    mkStorageClassAnalysisDataExport,
    scadeOutputSchemaVersion,
    scadeDestination,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** Tagging
    Tagging (..),
    mkTagging,
    tTagSet,

    -- ** TargetGrant
    TargetGrant (..),
    mkTargetGrant,
    tgPermission,
    tgGrantee,

    -- ** Tiering
    Tiering (..),
    mkTiering,
    tDays,
    tAccessTier,

    -- ** TopicConfiguration
    TopicConfiguration (..),
    mkTopicConfiguration,
    tcTopicARN,
    tcEvents,
    tcId,
    tcFilter,

    -- ** Transition
    Transition (..),
    mkTransition,
    tfDays,
    tfDate,
    tfStorageClass,

    -- ** VersioningConfiguration
    VersioningConfiguration (..),
    mkVersioningConfiguration,
    vcStatus,
    vcMFADelete,

    -- ** WebsiteConfiguration
    WebsiteConfiguration (..),
    mkWebsiteConfiguration,
    wcRedirectAllRequestsTo,
    wcErrorDocument,
    wcIndexDocument,
    wcRoutingRules,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.AbortMultipartUpload
import Network.AWS.S3.CompleteMultipartUpload
import Network.AWS.S3.CopyObject
import Network.AWS.S3.CreateBucket
import Network.AWS.S3.CreateMultipartUpload
import Network.AWS.S3.DeleteBucket
import Network.AWS.S3.DeleteBucketAnalyticsConfiguration
import Network.AWS.S3.DeleteBucketCORS
import Network.AWS.S3.DeleteBucketEncryption
import Network.AWS.S3.DeleteBucketIntelligentTieringConfiguration
import Network.AWS.S3.DeleteBucketInventoryConfiguration
import Network.AWS.S3.DeleteBucketLifecycle
import Network.AWS.S3.DeleteBucketMetricsConfiguration
import Network.AWS.S3.DeleteBucketOwnershipControls
import Network.AWS.S3.DeleteBucketPolicy
import Network.AWS.S3.DeleteBucketReplication
import Network.AWS.S3.DeleteBucketTagging
import Network.AWS.S3.DeleteBucketWebsite
import Network.AWS.S3.DeleteObject
import Network.AWS.S3.DeleteObjectTagging
import Network.AWS.S3.DeleteObjects
import Network.AWS.S3.DeletePublicAccessBlock
import Network.AWS.S3.GetBucketACL
import Network.AWS.S3.GetBucketAccelerateConfiguration
import Network.AWS.S3.GetBucketAnalyticsConfiguration
import Network.AWS.S3.GetBucketCORS
import Network.AWS.S3.GetBucketEncryption
import Network.AWS.S3.GetBucketIntelligentTieringConfiguration
import Network.AWS.S3.GetBucketInventoryConfiguration
import Network.AWS.S3.GetBucketLifecycleConfiguration
import Network.AWS.S3.GetBucketLocation
import Network.AWS.S3.GetBucketLogging
import Network.AWS.S3.GetBucketMetricsConfiguration
import Network.AWS.S3.GetBucketNotificationConfiguration
import Network.AWS.S3.GetBucketOwnershipControls
import Network.AWS.S3.GetBucketPolicy
import Network.AWS.S3.GetBucketPolicyStatus
import Network.AWS.S3.GetBucketReplication
import Network.AWS.S3.GetBucketRequestPayment
import Network.AWS.S3.GetBucketTagging
import Network.AWS.S3.GetBucketVersioning
import Network.AWS.S3.GetBucketWebsite
import Network.AWS.S3.GetObject
import Network.AWS.S3.GetObjectACL
import Network.AWS.S3.GetObjectLegalHold
import Network.AWS.S3.GetObjectLockConfiguration
import Network.AWS.S3.GetObjectRetention
import Network.AWS.S3.GetObjectTagging
import Network.AWS.S3.GetObjectTorrent
import Network.AWS.S3.GetPublicAccessBlock
import Network.AWS.S3.HeadBucket
import Network.AWS.S3.HeadObject
import Network.AWS.S3.Internal
import Network.AWS.S3.ListBucketAnalyticsConfigurations
import Network.AWS.S3.ListBucketIntelligentTieringConfigurations
import Network.AWS.S3.ListBucketInventoryConfigurations
import Network.AWS.S3.ListBucketMetricsConfigurations
import Network.AWS.S3.ListBuckets
import Network.AWS.S3.ListMultipartUploads
import Network.AWS.S3.ListObjectVersions
import Network.AWS.S3.ListObjects
import Network.AWS.S3.ListObjectsV2
import Network.AWS.S3.ListParts
import Network.AWS.S3.PutBucketACL
import Network.AWS.S3.PutBucketAccelerateConfiguration
import Network.AWS.S3.PutBucketAnalyticsConfiguration
import Network.AWS.S3.PutBucketCORS
import Network.AWS.S3.PutBucketEncryption
import Network.AWS.S3.PutBucketIntelligentTieringConfiguration
import Network.AWS.S3.PutBucketInventoryConfiguration
import Network.AWS.S3.PutBucketLifecycleConfiguration
import Network.AWS.S3.PutBucketLogging
import Network.AWS.S3.PutBucketMetricsConfiguration
import Network.AWS.S3.PutBucketNotificationConfiguration
import Network.AWS.S3.PutBucketOwnershipControls
import Network.AWS.S3.PutBucketPolicy
import Network.AWS.S3.PutBucketReplication
import Network.AWS.S3.PutBucketRequestPayment
import Network.AWS.S3.PutBucketTagging
import Network.AWS.S3.PutBucketVersioning
import Network.AWS.S3.PutBucketWebsite
import Network.AWS.S3.PutObject
import Network.AWS.S3.PutObjectACL
import Network.AWS.S3.PutObjectLegalHold
import Network.AWS.S3.PutObjectLockConfiguration
import Network.AWS.S3.PutObjectRetention
import Network.AWS.S3.PutObjectTagging
import Network.AWS.S3.PutPublicAccessBlock
import Network.AWS.S3.RestoreObject
import Network.AWS.S3.SelectObjectContent
import Network.AWS.S3.Types
import Network.AWS.S3.UploadPart
import Network.AWS.S3.UploadPartCopy
import Network.AWS.S3.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'S3'.

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
