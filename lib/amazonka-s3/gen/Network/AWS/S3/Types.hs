-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _BucketAlreadyOwnedByYou,
    _ObjectAlreadyInActiveTierError,
    _BucketAlreadyExists,
    _ObjectNotInActiveTierError,
    _NoSuchUpload,
    _NoSuchBucket,
    _NoSuchKey,
    _InvalidObjectState,

    -- * Re-exported types
    module Network.AWS.S3.Internal,

    -- * StartAfter
    StartAfter (..),

    -- * ReplicationConfiguration
    ReplicationConfiguration (..),
    mkReplicationConfiguration,
    rcRole,
    rcRules,

    -- * IfMatch
    IfMatch (..),

    -- * Destination
    Destination (..),
    mkDestination,
    dBucket,
    dAccessControlTranslation,
    dAccount,
    dEncryptionConfiguration,
    dMetrics,
    dReplicationTime,
    dStorageClass,

    -- * Event
    Event (..),

    -- * RequestCharged
    RequestCharged (..),

    -- * DeleteMarkerReplication
    DeleteMarkerReplication (..),
    mkDeleteMarkerReplication,
    dmrStatus,

    -- * Suffix
    Suffix (..),

    -- * NoncurrentVersionExpiration
    NoncurrentVersionExpiration (..),
    mkNoncurrentVersionExpiration,
    nveNoncurrentDays,

    -- * AllowedMethod
    AllowedMethod (..),

    -- * Transition
    Transition (..),
    mkTransition,
    tfDate,
    tfDays,
    tfStorageClass,

    -- * DeleteMarkerEntry
    DeleteMarkerEntry (..),
    mkDeleteMarkerEntry,
    dmeIsLatest,
    dmeKey,
    dmeLastModified,
    dmeOwner,
    dmeVersionId,

    -- * AnalyticsS3ExportFileFormat
    AnalyticsS3ExportFileFormat (..),

    -- * ExpirationStatus
    ExpirationStatus (..),

    -- * TargetBucket
    TargetBucket (..),

    -- * ServerSideEncryptionRule
    ServerSideEncryptionRule (..),
    mkServerSideEncryptionRule,
    sserApplyServerSideEncryptionByDefault,

    -- * AllowedHeader
    AllowedHeader (..),

    -- * InventoryId
    InventoryId (..),

    -- * MetricsConfiguration
    MetricsConfiguration (..),
    mkMetricsConfiguration,
    mcId,
    mcFilter,

    -- * CopySourceRange
    CopySourceRange (..),

    -- * Metrics
    Metrics (..),
    mkMetrics,
    mStatus,
    mEventThreshold,

    -- * Part
    Part (..),
    mkPart,
    pETag,
    pLastModified,
    pPartNumber,
    pSize,

    -- * HostName
    HostName (..),

    -- * ReplicationTimeValue
    ReplicationTimeValue (..),
    mkReplicationTimeValue,
    rtvMinutes,

    -- * CopySourceSSECustomerKeyMD5
    CopySourceSSECustomerKeyMD5 (..),

    -- * VersioningConfiguration
    VersioningConfiguration (..),
    mkVersioningConfiguration,
    vcMFADelete,
    vcStatus,

    -- * TaggingDirective
    TaggingDirective (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * MFA
    MFA (..),

    -- * ExpressionType
    ExpressionType (..),

    -- * NextVersionIdMarker
    NextVersionIdMarker (..),

    -- * TaggingHeader
    TaggingHeader (..),

    -- * ObjectStorageClass
    ObjectStorageClass (..),

    -- * InventoryFilter
    InventoryFilter (..),
    mkInventoryFilter,
    ifPrefix,

    -- * MetadataDirective
    MetadataDirective (..),

    -- * ReplicationRule
    ReplicationRule (..),
    mkReplicationRule,
    rrStatus,
    rrDestination,
    rrDeleteMarkerReplication,
    rrExistingObjectReplication,
    rrFilter,
    rrID,
    rrPrefix,
    rrPriority,
    rrSourceSelectionCriteria,

    -- * ResponseContentType
    ResponseContentType (..),

    -- * InventoryConfiguration
    InventoryConfiguration (..),
    mkInventoryConfiguration,
    icDestination,
    icIsEnabled,
    icId,
    icIncludedObjectVersions,
    icSchedule,
    icFilter,
    icOptionalFields,

    -- * ObjectLockRetention
    ObjectLockRetention (..),
    mkObjectLockRetention,
    olrMode,
    olrRetainUntilDate,

    -- * RedirectAllRequestsTo
    RedirectAllRequestsTo (..),
    mkRedirectAllRequestsTo,
    rartHostName,
    rartProtocol,

    -- * SelectParameters
    SelectParameters (..),
    mkSelectParameters,
    spInputSerialization,
    spExpressionType,
    spExpression,
    spOutputSerialization,

    -- * RoutingRule
    RoutingRule (..),
    mkRoutingRule,
    rrRedirect,
    rrCondition,

    -- * ObjectLockLegalHold
    ObjectLockLegalHold (..),
    mkObjectLockLegalHold,
    ollhStatus,

    -- * Location
    Location (..),

    -- * IntelligentTieringConfiguration
    IntelligentTieringConfiguration (..),
    mkIntelligentTieringConfiguration,
    itcId,
    itcStatus,
    itcTierings,
    itcFilter,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncLambdaFunctionConfigurations,
    ncQueueConfigurations,
    ncTopicConfigurations,

    -- * Progress
    Progress (..),
    mkProgress,
    pBytesProcessed,
    pBytesReturned,
    pBytesScanned,

    -- * ResponseContentDisposition
    ResponseContentDisposition (..),

    -- * FilterRuleValue
    FilterRuleValue (..),

    -- * ContinuationEvent
    ContinuationEvent (..),
    mkContinuationEvent,

    -- * StorageClassAnalysis
    StorageClassAnalysis (..),
    mkStorageClassAnalysis,
    scaDataExport,

    -- * BucketAccelerateStatus
    BucketAccelerateStatus (..),

    -- * InventoryEncryption
    InventoryEncryption (..),
    mkInventoryEncryption,
    ieSSEKMS,
    ieSSES3,

    -- * LocationPrefix
    LocationPrefix (..),

    -- * ObjectLockToken
    ObjectLockToken (..),

    -- * ObjectLockMode
    ObjectLockMode (..),

    -- * KeyMarker
    KeyMarker (..),

    -- * LifecycleRule
    LifecycleRule (..),
    mkLifecycleRule,
    lrStatus,
    lrAbortIncompleteMultipartUpload,
    lrExpiration,
    lrFilter,
    lrID,
    lrNoncurrentVersionExpiration,
    lrNoncurrentVersionTransitions,
    lrPrefix,
    lrTransitions,

    -- * ObjectCannedACL
    ObjectCannedACL (..),

    -- * AccessControlTranslation
    AccessControlTranslation (..),
    mkAccessControlTranslation,
    actOwner,

    -- * BucketVersioningStatus
    BucketVersioningStatus (..),

    -- * DeletedObject
    DeletedObject (..),
    mkDeletedObject,
    doDeleteMarker,
    doDeleteMarkerVersionId,
    doKey,
    doVersionId,

    -- * ObjectVersionStorageClass
    ObjectVersionStorageClass (..),

    -- * LambdaFunctionArn
    LambdaFunctionArn (..),

    -- * CSVInput
    CSVInput (..),
    mkCSVInput,
    csviAllowQuotedRecordDelimiter,
    csviComments,
    csviFieldDelimiter,
    csviFileHeaderInfo,
    csviQuoteCharacter,
    csviQuoteEscapeCharacter,
    csviRecordDelimiter,

    -- * ResponseContentLanguage
    ResponseContentLanguage (..),

    -- * MultipartUploadId
    MultipartUploadId (..),

    -- * Prefix
    Prefix (..),

    -- * ParquetInput
    ParquetInput (..),
    mkParquetInput,

    -- * S3ServiceError
    S3ServiceError (..),
    mkS3ServiceError,
    sseCode,
    sseKey,
    sseMessage,
    sseVersionId,

    -- * KeyPrefixEquals
    KeyPrefixEquals (..),

    -- * Restore
    Restore (..),

    -- * ObjectLockEnabled
    ObjectLockEnabled (..),

    -- * EndEvent
    EndEvent (..),
    mkEndEvent,

    -- * Token
    Token (..),

    -- * MetricsStatus
    MetricsStatus (..),

    -- * AnalyticsId
    AnalyticsId (..),

    -- * Expiration
    Expiration (..),

    -- * QuoteCharacter
    QuoteCharacter (..),

    -- * CopyPartResult
    CopyPartResult (..),
    mkCopyPartResult,
    cprETag,
    cprLastModified,

    -- * EncodingType
    EncodingType (..),

    -- * ExposeHeader
    ExposeHeader (..),

    -- * LifecycleRuleAndOperator
    LifecycleRuleAndOperator (..),
    mkLifecycleRuleAndOperator,
    lraoPrefix,
    lraoTags,

    -- * RequestPaymentConfiguration
    RequestPaymentConfiguration (..),
    mkRequestPaymentConfiguration,
    rpcPayer,

    -- * CORSRule
    CORSRule (..),
    mkCORSRule,
    corsrAllowedMethods,
    corsrAllowedOrigins,
    corsrAllowedHeaders,
    corsrExposeHeaders,
    corsrMaxAgeSeconds,

    -- * Value
    Value (..),

    -- * WebsiteConfiguration
    WebsiteConfiguration (..),
    mkWebsiteConfiguration,
    wcErrorDocument,
    wcIndexDocument,
    wcRedirectAllRequestsTo,
    wcRoutingRules,

    -- * NoncurrentVersionTransition
    NoncurrentVersionTransition (..),
    mkNoncurrentVersionTransition,
    nvtNoncurrentDays,
    nvtStorageClass,

    -- * DeleteMarkerReplicationStatus
    DeleteMarkerReplicationStatus (..),

    -- * QuoteFields
    QuoteFields (..),

    -- * Initiator
    Initiator (..),
    mkInitiator,
    iDisplayName,
    iID,

    -- * InventoryFrequency
    InventoryFrequency (..),

    -- * GrantReadACP
    GrantReadACP (..),

    -- * ObjectIdentifier
    ObjectIdentifier (..),
    mkObjectIdentifier,
    oiKey,
    oiVersionId,

    -- * CopySourceIfNoneMatch
    CopySourceIfNoneMatch (..),

    -- * Bucket
    Bucket (..),
    mkBucket,
    bCreationDate,
    bName,

    -- * ExistingObjectReplicationStatus
    ExistingObjectReplicationStatus (..),

    -- * ArchiveStatus
    ArchiveStatus (..),

    -- * SSECustomerAlgorithm
    SSECustomerAlgorithm (..),

    -- * Protocol
    Protocol (..),

    -- * InventoryDestination
    InventoryDestination (..),
    mkInventoryDestination,
    idS3BucketDestination,

    -- * JSONInput
    JSONInput (..),
    mkJSONInput,
    jsoniType,

    -- * RecordDelimiter
    RecordDelimiter (..),

    -- * AnalyticsExportDestination
    AnalyticsExportDestination (..),
    mkAnalyticsExportDestination,
    aedS3BucketDestination,

    -- * Grant
    Grant (..),
    mkGrant,
    gGrantee,
    gPermission,

    -- * ObjectOwnership
    ObjectOwnership (..),

    -- * InventoryOptionalField
    InventoryOptionalField (..),

    -- * SSECustomerKey
    SSECustomerKey (..),

    -- * URI
    URI (..),

    -- * IntelligentTieringAndOperator
    IntelligentTieringAndOperator (..),
    mkIntelligentTieringAndOperator,
    itaoPrefix,
    itaoTags,

    -- * ObjectLockConfiguration
    ObjectLockConfiguration (..),
    mkObjectLockConfiguration,
    olcObjectLockEnabled,
    olcRule,

    -- * StorageClassAnalysisSchemaVersion
    StorageClassAnalysisSchemaVersion (..),

    -- * InventoryIncludedObjectVersions
    InventoryIncludedObjectVersions (..),

    -- * RequestPayer
    RequestPayer (..),

    -- * TopicConfiguration
    TopicConfiguration (..),
    mkTopicConfiguration,
    tcTopicArn,
    tcEvents,
    tcFilter,
    tcId,

    -- * OwnershipControlsRule
    OwnershipControlsRule (..),
    mkOwnershipControlsRule,
    ocrObjectOwnership,

    -- * Stats
    Stats (..),
    mkStats,
    sBytesProcessed,
    sBytesReturned,
    sBytesScanned,

    -- * GrantWriteACP
    GrantWriteACP (..),

    -- * AbortRuleId
    AbortRuleId (..),

    -- * ObjectLockRetentionMode
    ObjectLockRetentionMode (..),

    -- * OutputSerialization
    OutputSerialization (..),
    mkOutputSerialization,
    osCSV,
    osJSON,

    -- * CSVOutput
    CSVOutput (..),
    mkCSVOutput,
    csvoFieldDelimiter,
    csvoQuoteCharacter,
    csvoQuoteEscapeCharacter,
    csvoQuoteFields,
    csvoRecordDelimiter,

    -- * QueueConfiguration
    QueueConfiguration (..),
    mkQueueConfiguration,
    qcQueueArn,
    qcEvents,
    qcFilter,
    qcId,

    -- * IntelligentTieringStatus
    IntelligentTieringStatus (..),

    -- * Owner
    Owner (..),
    mkOwner,
    oDisplayName,
    oID,

    -- * BucketLoggingStatus
    BucketLoggingStatus (..),
    mkBucketLoggingStatus,
    blsLoggingEnabled,

    -- * RestoreRequestType
    RestoreRequestType (..),

    -- * ReplicationRuleAndOperator
    ReplicationRuleAndOperator (..),
    mkReplicationRuleAndOperator,
    rraoPrefix,
    rraoTags,

    -- * HttpErrorCodeReturnedEquals
    HttpErrorCodeReturnedEquals (..),

    -- * StorageClassAnalysisDataExport
    StorageClassAnalysisDataExport (..),
    mkStorageClassAnalysisDataExport,
    scadeOutputSchemaVersion,
    scadeDestination,

    -- * ResponseContentEncoding
    ResponseContentEncoding (..),

    -- * SelectObjectContentEventStream
    SelectObjectContentEventStream (..),
    mkSelectObjectContentEventStream,
    socesCont,
    socesEnd,
    socesProgress,
    socesRecords,
    socesStats,

    -- * QueueArn
    QueueArn (..),

    -- * AccountId
    AccountId (..),

    -- * CopySourceIfMatch
    CopySourceIfMatch (..),

    -- * AllowedOrigin
    AllowedOrigin (..),

    -- * NextToken
    NextToken (..),

    -- * TopicArn
    TopicArn (..),

    -- * UploadIdMarker
    UploadIdMarker (..),

    -- * SseKmsEncryptedObjectsStatus
    SseKmsEncryptedObjectsStatus (..),

    -- * WebsiteRedirectLocation
    WebsiteRedirectLocation (..),

    -- * RequestProgress
    RequestProgress (..),
    mkRequestProgress,
    rpEnabled,

    -- * GrantRead
    GrantRead (..),

    -- * PublicAccessBlockConfiguration
    PublicAccessBlockConfiguration (..),
    mkPublicAccessBlockConfiguration,
    pabcBlockPublicAcls,
    pabcBlockPublicPolicy,
    pabcIgnorePublicAcls,
    pabcRestrictPublicBuckets,

    -- * DeleteMarkerVersionId
    DeleteMarkerVersionId (..),

    -- * AnalyticsS3BucketDestination
    AnalyticsS3BucketDestination (..),
    mkAnalyticsS3BucketDestination,
    asbdFormat,
    asbdBucket,
    asbdBucketAccountId,
    asbdPrefix,

    -- * MetricsAndOperator
    MetricsAndOperator (..),
    mkMetricsAndOperator,
    maoPrefix,
    maoTags,

    -- * PolicyStatus
    PolicyStatus (..),
    mkPolicyStatus,
    psIsPublic,

    -- * Role
    Role (..),

    -- * AcceptRanges
    AcceptRanges (..),

    -- * ReplicaKmsKeyID
    ReplicaKmsKeyID (..),

    -- * Range
    Range (..),

    -- * Encryption
    Encryption (..),
    mkEncryption,
    eEncryptionType,
    eKMSContext,
    eKMSKeyId,

    -- * MetricsFilter
    MetricsFilter (..),
    mkMetricsFilter,
    mfAnd,
    mfPrefix,
    mfTag,

    -- * S3KeyFilter
    S3KeyFilter (..),
    mkS3KeyFilter,
    skfFilterRules,

    -- * DefaultRetention
    DefaultRetention (..),
    mkDefaultRetention,
    drDays,
    drMode,
    drYears,

    -- * OwnershipControls
    OwnershipControls (..),
    mkOwnershipControls,
    ocRules,

    -- * JSONOutput
    JSONOutput (..),
    mkJSONOutput,
    jsonoRecordDelimiter,

    -- * InventorySchedule
    InventorySchedule (..),
    mkInventorySchedule,
    isFrequency,

    -- * FileHeaderInfo
    FileHeaderInfo (..),

    -- * ErrorDocument
    ErrorDocument (..),
    mkErrorDocument,
    edKey,

    -- * StorageClass
    StorageClass (..),

    -- * ObjectVersion
    ObjectVersion (..),
    mkObjectVersion,
    ovETag,
    ovIsLatest,
    ovKey,
    ovLastModified,
    ovOwner,
    ovSize,
    ovStorageClass,
    ovVersionId,

    -- * TargetGrant
    TargetGrant (..),
    mkTargetGrant,
    tgGrantee,
    tgPermission,

    -- * CopySourceVersionId
    CopySourceVersionId (..),

    -- * MFADeleteStatus
    MFADeleteStatus (..),

    -- * HttpRedirectCode
    HttpRedirectCode (..),

    -- * Payer
    Payer (..),

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecReplicaKmsKeyID,

    -- * AccelerateConfiguration
    AccelerateConfiguration (..),
    mkAccelerateConfiguration,
    acStatus,

    -- * Tiering
    Tiering (..),
    mkTiering,
    tDays,
    tAccessTier,

    -- * ExistingObjectReplication
    ExistingObjectReplication (..),
    mkExistingObjectReplication,
    eorStatus,

    -- * Redirect
    Redirect (..),
    mkRedirect,
    rHostName,
    rHttpRedirectCode,
    rProtocol,
    rReplaceKeyPrefixWith,
    rReplaceKeyWith,

    -- * MetricsId
    MetricsId (..),

    -- * SSECustomerKeyMD5
    SSECustomerKeyMD5 (..),

    -- * NextKeyMarker
    NextKeyMarker (..),

    -- * OutputLocation
    OutputLocation (..),
    mkOutputLocation,
    olS3,

    -- * Tier
    Tier (..),

    -- * BucketLogsPermission
    BucketLogsPermission (..),

    -- * EmailAddress
    EmailAddress (..),

    -- * Marker
    Marker (..),

    -- * SSEKMSKeyId
    SSEKMSKeyId (..),

    -- * KMSContext
    KMSContext (..),

    -- * MetadataKey
    MetadataKey (..),

    -- * Expression
    Expression (..),

    -- * RecordsEvent
    RecordsEvent (..),
    mkRecordsEvent,
    rePayload,

    -- * GlacierJobParameters
    GlacierJobParameters (..),
    mkGlacierJobParameters,
    gjpTier,

    -- * QuoteEscapeCharacter
    QuoteEscapeCharacter (..),

    -- * RestoreOutputPath
    RestoreOutputPath (..),

    -- * CompletedPart
    CompletedPart (..),
    mkCompletedPart,
    cpETag,
    cpPartNumber,

    -- * ReplicationRuleFilter
    ReplicationRuleFilter (..),
    mkReplicationRuleFilter,
    rrfAnd,
    rrfPrefix,
    rrfTag,

    -- * GrantFullControl
    GrantFullControl (..),

    -- * ContentEncoding
    ContentEncoding (..),

    -- * CreateBucketConfiguration
    CreateBucketConfiguration (..),
    mkCreateBucketConfiguration,
    cbcLocationConstraint,

    -- * Tagging
    Tagging (..),
    mkTagging,
    tTagSet,

    -- * NextMarker
    NextMarker (..),

    -- * LifecycleExpiration
    LifecycleExpiration (..),
    mkLifecycleExpiration,
    leDate,
    leDays,
    leExpiredObjectDeleteMarker,

    -- * ContentMD5
    ContentMD5 (..),

    -- * StatsEvent
    StatsEvent (..),
    mkStatsEvent,
    seDetails,

    -- * IntelligentTieringFilter
    IntelligentTieringFilter (..),
    mkIntelligentTieringFilter,
    itfAnd,
    itfPrefix,
    itfTag,

    -- * CORSConfiguration
    CORSConfiguration (..),
    mkCORSConfiguration,
    corscCORSRules,

    -- * ReplicationTimeStatus
    ReplicationTimeStatus (..),

    -- * AnalyticsAndOperator
    AnalyticsAndOperator (..),
    mkAnalyticsAndOperator,
    aaoPrefix,
    aaoTags,

    -- * NotificationConfigurationFilter
    NotificationConfigurationFilter (..),
    mkNotificationConfigurationFilter,
    ncfKey,

    -- * DisplayName
    DisplayName (..),

    -- * ID
    ID (..),

    -- * VersionIdMarker
    VersionIdMarker (..),

    -- * SSES3
    SSES3 (..),
    mkSSES3,

    -- * Object
    Object (..),
    mkObject,
    oETag,
    oKey,
    oLastModified,
    oOwner,
    oSize,
    oStorageClass,

    -- * CommonPrefix
    CommonPrefix (..),
    mkCommonPrefix,
    cpPrefix,

    -- * MultipartUpload
    MultipartUpload (..),
    mkMultipartUpload,
    muInitiated,
    muInitiator,
    muKey,
    muOwner,
    muStorageClass,
    muUploadId,

    -- * LambdaFunctionConfiguration
    LambdaFunctionConfiguration (..),
    mkLambdaFunctionConfiguration,
    lfcLambdaFunctionArn,
    lfcEvents,
    lfcFilter,
    lfcId,

    -- * Code
    Code (..),

    -- * OwnerOverride
    OwnerOverride (..),

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slBucketName,
    slPrefix,
    slAccessControlList,
    slCannedACL,
    slEncryption,
    slStorageClass,
    slTagging,
    slUserMetadata,

    -- * SseKmsEncryptedObjects
    SseKmsEncryptedObjects (..),
    mkSseKmsEncryptedObjects,
    skeoStatus,

    -- * CopySource
    CopySource (..),

    -- * SSEKMS
    SSEKMS (..),
    mkSSEKMS,
    ssekmsKeyId,

    -- * ResponseCacheControl
    ResponseCacheControl (..),

    -- * Type
    Type (..),

    -- * BucketLifecycleConfiguration
    BucketLifecycleConfiguration (..),
    mkBucketLifecycleConfiguration,
    blcRules,

    -- * JSONType
    JSONType (..),

    -- * ReplicationStatus
    ReplicationStatus (..),

    -- * InputSerialization
    InputSerialization (..),
    mkInputSerialization,
    isCSV,
    isCompressionType,
    isJSON,
    isParquet,

    -- * ScanRange
    ScanRange (..),
    mkScanRange,
    srEnd,
    srStart,

    -- * InventoryS3BucketDestination
    InventoryS3BucketDestination (..),
    mkInventoryS3BucketDestination,
    isbdBucket,
    isbdFormat,
    isbdAccountId,
    isbdEncryption,
    isbdPrefix,

    -- * ObjectLockRule
    ObjectLockRule (..),
    mkObjectLockRule,
    olrDefaultRetention,

    -- * LifecycleRuleFilter
    LifecycleRuleFilter (..),
    mkLifecycleRuleFilter,
    lrfAnd,
    lrfPrefix,
    lrfTag,

    -- * SSEKMSEncryptionContext
    SSEKMSEncryptionContext (..),

    -- * TransitionStorageClass
    TransitionStorageClass (..),

    -- * CompletedMultipartUpload
    CompletedMultipartUpload (..),
    mkCompletedMultipartUpload,
    cmuParts,

    -- * Comments
    Comments (..),

    -- * CacheControl
    CacheControl (..),

    -- * Condition
    Condition (..),
    mkCondition,
    cHttpErrorCodeReturnedEquals,
    cKeyPrefixEquals,

    -- * IntelligentTieringAccessTier
    IntelligentTieringAccessTier (..),

    -- * ContentLanguage
    ContentLanguage (..),

    -- * NextUploadIdMarker
    NextUploadIdMarker (..),

    -- * Permission
    Permission (..),

    -- * AccessControlPolicy
    AccessControlPolicy (..),
    mkAccessControlPolicy,
    acpGrants,
    acpOwner,

    -- * Message
    Message (..),

    -- * BucketCannedACL
    BucketCannedACL (..),

    -- * MFADelete
    MFADelete (..),

    -- * GrantWrite
    GrantWrite (..),

    -- * SourceSelectionCriteria
    SourceSelectionCriteria (..),
    mkSourceSelectionCriteria,
    sscSseKmsEncryptedObjects,

    -- * Grantee
    Grantee (..),
    mkGrantee,
    gType,
    gDisplayName,
    gEmailAddress,
    gID,
    gURI,

    -- * CompressionType
    CompressionType (..),

    -- * CopySourceSSECustomerKey
    CopySourceSSECustomerKey (..),

    -- * ObjectLockLegalHoldStatus
    ObjectLockLegalHoldStatus (..),

    -- * LoggingEnabled
    LoggingEnabled (..),
    mkLoggingEnabled,
    leTargetBucket,
    leTargetPrefix,
    leTargetGrants,

    -- * Description
    Description (..),

    -- * AnalyticsConfiguration
    AnalyticsConfiguration (..),
    mkAnalyticsConfiguration,
    acId,
    acStorageClassAnalysis,
    acFilter,

    -- * MetadataValue
    MetadataValue (..),

    -- * CopySourceSSECustomerAlgorithm
    CopySourceSSECustomerAlgorithm (..),

    -- * FilterRule
    FilterRule (..),
    mkFilterRule,
    frName,
    frValue,

    -- * ContentDisposition
    ContentDisposition (..),

    -- * AnalyticsFilter
    AnalyticsFilter (..),
    mkAnalyticsFilter,
    afAnd,
    afPrefix,
    afTag,

    -- * IfNoneMatch
    IfNoneMatch (..),

    -- * ContentRange
    ContentRange (..),

    -- * ReplicationRuleStatus
    ReplicationRuleStatus (..),

    -- * ProgressEvent
    ProgressEvent (..),
    mkProgressEvent,
    peDetails,

    -- * ServerSideEncryption
    ServerSideEncryption (..),

    -- * IndexDocument
    IndexDocument (..),
    mkIndexDocument,
    idSuffix,

    -- * CopyObjectResult
    CopyObjectResult (..),
    mkCopyObjectResult,
    corETag,
    corLastModified,

    -- * TargetPrefix
    TargetPrefix (..),

    -- * InventoryFormat
    InventoryFormat (..),

    -- * FieldDelimiter
    FieldDelimiter (..),

    -- * MetadataEntry
    MetadataEntry (..),
    mkMetadataEntry,
    meName,
    meValue,

    -- * ContentType
    ContentType (..),

    -- * AbortIncompleteMultipartUpload
    AbortIncompleteMultipartUpload (..),
    mkAbortIncompleteMultipartUpload,
    aimuDaysAfterInitiation,

    -- * ReplaceKeyWith
    ReplaceKeyWith (..),

    -- * ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration (..),
    mkServerSideEncryptionConfiguration,
    ssecRules,

    -- * Delete
    Delete (..),
    mkDelete,
    dObjects,
    dQuiet,

    -- * ReplicationTime
    ReplicationTime (..),
    mkReplicationTime,
    rtStatus,
    rtTime,

    -- * ReplaceKeyPrefixWith
    ReplaceKeyPrefixWith (..),

    -- * RestoreRequest
    RestoreRequest (..),
    mkRestoreRequest,
    rrDays,
    rrDescription,
    rrGlacierJobParameters,
    rrOutputLocation,
    rrSelectParameters,
    rrTier,
    rrType,

    -- * FilterRuleName
    FilterRuleName (..),

    -- * ServerSideEncryptionByDefault
    ServerSideEncryptionByDefault (..),
    mkServerSideEncryptionByDefault,
    ssebdSSEAlgorithm,
    ssebdKMSMasterKeyID,

    -- * ExpectedBucketOwner
    ExpectedBucketOwner (..),

    -- * ContinuationToken
    ContinuationToken (..),

    -- * Id
    Id (..),

    -- * Account
    Account (..),

    -- * NextContinuationToken
    NextContinuationToken (..),

    -- * Key
    Key (..),

    -- * VersionId
    VersionId (..),

    -- * UploadId
    UploadId (..),

    -- * Name
    Name (..),

    -- * ExpectedSourceBucketOwner
    ExpectedSourceBucketOwner (..),

    -- * KMSKeyId
    KMSKeyId (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AbortIncompleteMultipartUpload
import Network.AWS.S3.Types.AbortRuleId
import Network.AWS.S3.Types.AccelerateConfiguration
import Network.AWS.S3.Types.AcceptRanges
import Network.AWS.S3.Types.AccessControlPolicy
import Network.AWS.S3.Types.AccessControlTranslation
import Network.AWS.S3.Types.Account
import Network.AWS.S3.Types.AccountId
import Network.AWS.S3.Types.AllowedHeader
import Network.AWS.S3.Types.AllowedMethod
import Network.AWS.S3.Types.AllowedOrigin
import Network.AWS.S3.Types.AnalyticsAndOperator
import Network.AWS.S3.Types.AnalyticsConfiguration
import Network.AWS.S3.Types.AnalyticsExportDestination
import Network.AWS.S3.Types.AnalyticsFilter
import Network.AWS.S3.Types.AnalyticsId
import Network.AWS.S3.Types.AnalyticsS3BucketDestination
import Network.AWS.S3.Types.AnalyticsS3ExportFileFormat
import Network.AWS.S3.Types.ArchiveStatus
import Network.AWS.S3.Types.Bucket
import Network.AWS.S3.Types.BucketAccelerateStatus
import Network.AWS.S3.Types.BucketCannedACL
import Network.AWS.S3.Types.BucketLifecycleConfiguration
import Network.AWS.S3.Types.BucketLoggingStatus
import Network.AWS.S3.Types.BucketLogsPermission
import Network.AWS.S3.Types.BucketVersioningStatus
import Network.AWS.S3.Types.CORSConfiguration
import Network.AWS.S3.Types.CORSRule
import Network.AWS.S3.Types.CSVInput
import Network.AWS.S3.Types.CSVOutput
import Network.AWS.S3.Types.CacheControl
import Network.AWS.S3.Types.Code
import Network.AWS.S3.Types.Comments
import Network.AWS.S3.Types.CommonPrefix
import Network.AWS.S3.Types.CompletedMultipartUpload
import Network.AWS.S3.Types.CompletedPart
import Network.AWS.S3.Types.CompressionType
import Network.AWS.S3.Types.Condition
import Network.AWS.S3.Types.ContentDisposition
import Network.AWS.S3.Types.ContentEncoding
import Network.AWS.S3.Types.ContentLanguage
import Network.AWS.S3.Types.ContentMD5
import Network.AWS.S3.Types.ContentRange
import Network.AWS.S3.Types.ContentType
import Network.AWS.S3.Types.ContinuationEvent
import Network.AWS.S3.Types.ContinuationToken
import Network.AWS.S3.Types.CopyObjectResult
import Network.AWS.S3.Types.CopyPartResult
import Network.AWS.S3.Types.CopySource
import Network.AWS.S3.Types.CopySourceIfMatch
import Network.AWS.S3.Types.CopySourceIfNoneMatch
import Network.AWS.S3.Types.CopySourceRange
import Network.AWS.S3.Types.CopySourceSSECustomerAlgorithm
import Network.AWS.S3.Types.CopySourceSSECustomerKey
import Network.AWS.S3.Types.CopySourceSSECustomerKeyMD5
import Network.AWS.S3.Types.CopySourceVersionId
import Network.AWS.S3.Types.CreateBucketConfiguration
import Network.AWS.S3.Types.DefaultRetention
import Network.AWS.S3.Types.Delete
import Network.AWS.S3.Types.DeleteMarkerEntry
import Network.AWS.S3.Types.DeleteMarkerReplication
import Network.AWS.S3.Types.DeleteMarkerReplicationStatus
import Network.AWS.S3.Types.DeleteMarkerVersionId
import Network.AWS.S3.Types.DeletedObject
import Network.AWS.S3.Types.Description
import Network.AWS.S3.Types.Destination
import Network.AWS.S3.Types.DisplayName
import Network.AWS.S3.Types.EmailAddress
import Network.AWS.S3.Types.EncodingType
import Network.AWS.S3.Types.Encryption
import Network.AWS.S3.Types.EncryptionConfiguration
import Network.AWS.S3.Types.EndEvent
import Network.AWS.S3.Types.ErrorDocument
import Network.AWS.S3.Types.Event
import Network.AWS.S3.Types.ExistingObjectReplication
import Network.AWS.S3.Types.ExistingObjectReplicationStatus
import Network.AWS.S3.Types.ExpectedBucketOwner
import Network.AWS.S3.Types.ExpectedSourceBucketOwner
import Network.AWS.S3.Types.Expiration
import Network.AWS.S3.Types.ExpirationStatus
import Network.AWS.S3.Types.ExposeHeader
import Network.AWS.S3.Types.Expression
import Network.AWS.S3.Types.ExpressionType
import Network.AWS.S3.Types.FieldDelimiter
import Network.AWS.S3.Types.FileHeaderInfo
import Network.AWS.S3.Types.FilterRule
import Network.AWS.S3.Types.FilterRuleName
import Network.AWS.S3.Types.FilterRuleValue
import Network.AWS.S3.Types.GlacierJobParameters
import Network.AWS.S3.Types.Grant
import Network.AWS.S3.Types.GrantFullControl
import Network.AWS.S3.Types.GrantRead
import Network.AWS.S3.Types.GrantReadACP
import Network.AWS.S3.Types.GrantWrite
import Network.AWS.S3.Types.GrantWriteACP
import Network.AWS.S3.Types.Grantee
import Network.AWS.S3.Types.HostName
import Network.AWS.S3.Types.HttpErrorCodeReturnedEquals
import Network.AWS.S3.Types.HttpRedirectCode
import Network.AWS.S3.Types.ID
import Network.AWS.S3.Types.Id
import Network.AWS.S3.Types.IfMatch
import Network.AWS.S3.Types.IfNoneMatch
import Network.AWS.S3.Types.IndexDocument
import Network.AWS.S3.Types.Initiator
import Network.AWS.S3.Types.InputSerialization
import Network.AWS.S3.Types.IntelligentTieringAccessTier
import Network.AWS.S3.Types.IntelligentTieringAndOperator
import Network.AWS.S3.Types.IntelligentTieringConfiguration
import Network.AWS.S3.Types.IntelligentTieringFilter
import Network.AWS.S3.Types.IntelligentTieringStatus
import Network.AWS.S3.Types.InventoryConfiguration
import Network.AWS.S3.Types.InventoryDestination
import Network.AWS.S3.Types.InventoryEncryption
import Network.AWS.S3.Types.InventoryFilter
import Network.AWS.S3.Types.InventoryFormat
import Network.AWS.S3.Types.InventoryFrequency
import Network.AWS.S3.Types.InventoryId
import Network.AWS.S3.Types.InventoryIncludedObjectVersions
import Network.AWS.S3.Types.InventoryOptionalField
import Network.AWS.S3.Types.InventoryS3BucketDestination
import Network.AWS.S3.Types.InventorySchedule
import Network.AWS.S3.Types.JSONInput
import Network.AWS.S3.Types.JSONOutput
import Network.AWS.S3.Types.JSONType
import Network.AWS.S3.Types.KMSContext
import Network.AWS.S3.Types.KMSKeyId
import Network.AWS.S3.Types.Key
import Network.AWS.S3.Types.KeyMarker
import Network.AWS.S3.Types.KeyPrefixEquals
import Network.AWS.S3.Types.LambdaFunctionArn
import Network.AWS.S3.Types.LambdaFunctionConfiguration
import Network.AWS.S3.Types.LifecycleExpiration
import Network.AWS.S3.Types.LifecycleRule
import Network.AWS.S3.Types.LifecycleRuleAndOperator
import Network.AWS.S3.Types.LifecycleRuleFilter
import Network.AWS.S3.Types.Location
import Network.AWS.S3.Types.LocationPrefix
import Network.AWS.S3.Types.LoggingEnabled
import Network.AWS.S3.Types.MFA
import Network.AWS.S3.Types.MFADelete
import Network.AWS.S3.Types.MFADeleteStatus
import Network.AWS.S3.Types.Marker
import Network.AWS.S3.Types.Message
import Network.AWS.S3.Types.MetadataDirective
import Network.AWS.S3.Types.MetadataEntry
import Network.AWS.S3.Types.MetadataKey
import Network.AWS.S3.Types.MetadataValue
import Network.AWS.S3.Types.Metrics
import Network.AWS.S3.Types.MetricsAndOperator
import Network.AWS.S3.Types.MetricsConfiguration
import Network.AWS.S3.Types.MetricsFilter
import Network.AWS.S3.Types.MetricsId
import Network.AWS.S3.Types.MetricsStatus
import Network.AWS.S3.Types.MultipartUpload
import Network.AWS.S3.Types.MultipartUploadId
import Network.AWS.S3.Types.Name
import Network.AWS.S3.Types.NextContinuationToken
import Network.AWS.S3.Types.NextKeyMarker
import Network.AWS.S3.Types.NextMarker
import Network.AWS.S3.Types.NextToken
import Network.AWS.S3.Types.NextUploadIdMarker
import Network.AWS.S3.Types.NextVersionIdMarker
import Network.AWS.S3.Types.NoncurrentVersionExpiration
import Network.AWS.S3.Types.NoncurrentVersionTransition
import Network.AWS.S3.Types.NotificationConfiguration
import Network.AWS.S3.Types.NotificationConfigurationFilter
import Network.AWS.S3.Types.Object
import Network.AWS.S3.Types.ObjectCannedACL
import Network.AWS.S3.Types.ObjectIdentifier
import Network.AWS.S3.Types.ObjectLockConfiguration
import Network.AWS.S3.Types.ObjectLockEnabled
import Network.AWS.S3.Types.ObjectLockLegalHold
import Network.AWS.S3.Types.ObjectLockLegalHoldStatus
import Network.AWS.S3.Types.ObjectLockMode
import Network.AWS.S3.Types.ObjectLockRetention
import Network.AWS.S3.Types.ObjectLockRetentionMode
import Network.AWS.S3.Types.ObjectLockRule
import Network.AWS.S3.Types.ObjectLockToken
import Network.AWS.S3.Types.ObjectOwnership
import Network.AWS.S3.Types.ObjectStorageClass
import Network.AWS.S3.Types.ObjectVersion
import Network.AWS.S3.Types.ObjectVersionStorageClass
import Network.AWS.S3.Types.OutputLocation
import Network.AWS.S3.Types.OutputSerialization
import Network.AWS.S3.Types.Owner
import Network.AWS.S3.Types.OwnerOverride
import Network.AWS.S3.Types.OwnershipControls
import Network.AWS.S3.Types.OwnershipControlsRule
import Network.AWS.S3.Types.ParquetInput
import Network.AWS.S3.Types.Part
import Network.AWS.S3.Types.Payer
import Network.AWS.S3.Types.Permission
import Network.AWS.S3.Types.PolicyStatus
import Network.AWS.S3.Types.Prefix
import Network.AWS.S3.Types.Progress
import Network.AWS.S3.Types.ProgressEvent
import Network.AWS.S3.Types.Protocol
import Network.AWS.S3.Types.PublicAccessBlockConfiguration
import Network.AWS.S3.Types.QueueArn
import Network.AWS.S3.Types.QueueConfiguration
import Network.AWS.S3.Types.QuoteCharacter
import Network.AWS.S3.Types.QuoteEscapeCharacter
import Network.AWS.S3.Types.QuoteFields
import Network.AWS.S3.Types.Range
import Network.AWS.S3.Types.RecordDelimiter
import Network.AWS.S3.Types.RecordsEvent
import Network.AWS.S3.Types.Redirect
import Network.AWS.S3.Types.RedirectAllRequestsTo
import Network.AWS.S3.Types.ReplaceKeyPrefixWith
import Network.AWS.S3.Types.ReplaceKeyWith
import Network.AWS.S3.Types.ReplicaKmsKeyID
import Network.AWS.S3.Types.ReplicationConfiguration
import Network.AWS.S3.Types.ReplicationRule
import Network.AWS.S3.Types.ReplicationRuleAndOperator
import Network.AWS.S3.Types.ReplicationRuleFilter
import Network.AWS.S3.Types.ReplicationRuleStatus
import Network.AWS.S3.Types.ReplicationStatus
import Network.AWS.S3.Types.ReplicationTime
import Network.AWS.S3.Types.ReplicationTimeStatus
import Network.AWS.S3.Types.ReplicationTimeValue
import Network.AWS.S3.Types.RequestCharged
import Network.AWS.S3.Types.RequestPayer
import Network.AWS.S3.Types.RequestPaymentConfiguration
import Network.AWS.S3.Types.RequestProgress
import Network.AWS.S3.Types.ResponseCacheControl
import Network.AWS.S3.Types.ResponseContentDisposition
import Network.AWS.S3.Types.ResponseContentEncoding
import Network.AWS.S3.Types.ResponseContentLanguage
import Network.AWS.S3.Types.ResponseContentType
import Network.AWS.S3.Types.Restore
import Network.AWS.S3.Types.RestoreOutputPath
import Network.AWS.S3.Types.RestoreRequest
import Network.AWS.S3.Types.RestoreRequestType
import Network.AWS.S3.Types.Role
import Network.AWS.S3.Types.RoutingRule
import Network.AWS.S3.Types.S3KeyFilter
import Network.AWS.S3.Types.S3Location
import Network.AWS.S3.Types.S3ServiceError
import Network.AWS.S3.Types.SSECustomerAlgorithm
import Network.AWS.S3.Types.SSECustomerKey
import Network.AWS.S3.Types.SSECustomerKeyMD5
import Network.AWS.S3.Types.SSEKMS
import Network.AWS.S3.Types.SSEKMSEncryptionContext
import Network.AWS.S3.Types.SSEKMSKeyId
import Network.AWS.S3.Types.SSES3
import Network.AWS.S3.Types.ScanRange
import Network.AWS.S3.Types.SelectObjectContentEventStream
import Network.AWS.S3.Types.SelectParameters
import Network.AWS.S3.Types.ServerSideEncryption
import Network.AWS.S3.Types.ServerSideEncryptionByDefault
import Network.AWS.S3.Types.ServerSideEncryptionConfiguration
import Network.AWS.S3.Types.ServerSideEncryptionRule
import Network.AWS.S3.Types.SourceSelectionCriteria
import Network.AWS.S3.Types.SseKmsEncryptedObjects
import Network.AWS.S3.Types.SseKmsEncryptedObjectsStatus
import Network.AWS.S3.Types.StartAfter
import Network.AWS.S3.Types.Stats
import Network.AWS.S3.Types.StatsEvent
import Network.AWS.S3.Types.StorageClass
import Network.AWS.S3.Types.StorageClassAnalysis
import Network.AWS.S3.Types.StorageClassAnalysisDataExport
import Network.AWS.S3.Types.StorageClassAnalysisSchemaVersion
import Network.AWS.S3.Types.Suffix
import Network.AWS.S3.Types.Tag
import Network.AWS.S3.Types.Tagging
import Network.AWS.S3.Types.TaggingDirective
import Network.AWS.S3.Types.TaggingHeader
import Network.AWS.S3.Types.TargetBucket
import Network.AWS.S3.Types.TargetGrant
import Network.AWS.S3.Types.TargetPrefix
import Network.AWS.S3.Types.Tier
import Network.AWS.S3.Types.Tiering
import Network.AWS.S3.Types.Token
import Network.AWS.S3.Types.TopicArn
import Network.AWS.S3.Types.TopicConfiguration
import Network.AWS.S3.Types.Transition
import Network.AWS.S3.Types.TransitionStorageClass
import Network.AWS.S3.Types.Type
import Network.AWS.S3.Types.URI
import Network.AWS.S3.Types.UploadId
import Network.AWS.S3.Types.UploadIdMarker
import Network.AWS.S3.Types.Value
import Network.AWS.S3.Types.VersionId
import Network.AWS.S3.Types.VersionIdMarker
import Network.AWS.S3.Types.VersioningConfiguration
import Network.AWS.S3.Types.WebsiteConfiguration
import Network.AWS.S3.Types.WebsiteRedirectLocation
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2006-03-01@ of the Amazon Simple Storage Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "S3",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "s3",
      Core._svcVersion = "2006-03-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseXMLError "S3",
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
      | Lens.has (Core.hasCode "BadDigest" Core.. Core.hasStatus 400) e =
        Core.Just "contentmd5"
      | Lens.has
          (Core.hasCode "RequestTimeout" Core.. Core.hasStatus 400)
          e =
        Core.Just "timeouts"
      | Core.otherwise = Core.Nothing

-- | The bucket you tried to create already exists, and you own it. Amazon S3 returns this error in all AWS Regions except in the North Virginia Region. For legacy compatibility, if you re-create an existing bucket that you already own in the North Virginia Region, Amazon S3 returns 200 OK and resets the bucket access control lists (ACLs).
_BucketAlreadyOwnedByYou :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BucketAlreadyOwnedByYou =
  Core._MatchServiceError mkServiceConfig "BucketAlreadyOwnedByYou"
{-# DEPRECATED _BucketAlreadyOwnedByYou "Use generic-lens or generic-optics instead." #-}

-- | This operation is not allowed against this storage tier.
_ObjectAlreadyInActiveTierError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ObjectAlreadyInActiveTierError =
  Core._MatchServiceError
    mkServiceConfig
    "ObjectAlreadyInActiveTierError"
{-# DEPRECATED _ObjectAlreadyInActiveTierError "Use generic-lens or generic-optics instead." #-}

-- | The requested bucket name is not available. The bucket namespace is shared by all users of the system. Select a different name and try again.
_BucketAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BucketAlreadyExists =
  Core._MatchServiceError mkServiceConfig "BucketAlreadyExists"
{-# DEPRECATED _BucketAlreadyExists "Use generic-lens or generic-optics instead." #-}

-- | The source object of the COPY operation is not in the active tier and is only stored in Amazon S3 Glacier.
_ObjectNotInActiveTierError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ObjectNotInActiveTierError =
  Core._MatchServiceError
    mkServiceConfig
    "ObjectNotInActiveTierError"
{-# DEPRECATED _ObjectNotInActiveTierError "Use generic-lens or generic-optics instead." #-}

-- | The specified multipart upload does not exist.
_NoSuchUpload :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchUpload =
  Core._MatchServiceError mkServiceConfig "NoSuchUpload"
{-# DEPRECATED _NoSuchUpload "Use generic-lens or generic-optics instead." #-}

-- | The specified bucket does not exist.
_NoSuchBucket :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchBucket =
  Core._MatchServiceError mkServiceConfig "NoSuchBucket"
{-# DEPRECATED _NoSuchBucket "Use generic-lens or generic-optics instead." #-}

-- | The specified key does not exist.
_NoSuchKey :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchKey = Core._MatchServiceError mkServiceConfig "NoSuchKey"
{-# DEPRECATED _NoSuchKey "Use generic-lens or generic-optics instead." #-}

-- | Object is archived and inaccessible until restored.
_InvalidObjectState :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidObjectState =
  Core._MatchServiceError mkServiceConfig "InvalidObjectState"
{-# DEPRECATED _InvalidObjectState "Use generic-lens or generic-optics instead." #-}
