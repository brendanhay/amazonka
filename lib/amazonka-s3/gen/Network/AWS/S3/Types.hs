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
    s3Service,

    -- * Errors

    -- * Re-exported types
    module Network.AWS.S3.Internal,

    -- * AnalyticsS3ExportFileFormat
    AnalyticsS3ExportFileFormat (..),

    -- * ArchiveStatus
    ArchiveStatus (..),

    -- * BucketAccelerateStatus
    BucketAccelerateStatus (..),

    -- * BucketCannedACL
    BucketCannedACL (..),

    -- * BucketLogsPermission
    BucketLogsPermission (..),

    -- * BucketVersioningStatus
    BucketVersioningStatus (..),

    -- * CompressionType
    CompressionType (..),

    -- * DeleteMarkerReplicationStatus
    DeleteMarkerReplicationStatus (..),

    -- * EncodingType
    EncodingType (..),

    -- * Event
    Event (..),

    -- * ExistingObjectReplicationStatus
    ExistingObjectReplicationStatus (..),

    -- * ExpirationStatus
    ExpirationStatus (..),

    -- * ExpressionType
    ExpressionType (..),

    -- * FileHeaderInfo
    FileHeaderInfo (..),

    -- * FilterRuleName
    FilterRuleName (..),

    -- * IntelligentTieringAccessTier
    IntelligentTieringAccessTier (..),

    -- * IntelligentTieringStatus
    IntelligentTieringStatus (..),

    -- * InventoryFormat
    InventoryFormat (..),

    -- * InventoryFrequency
    InventoryFrequency (..),

    -- * InventoryIncludedObjectVersions
    InventoryIncludedObjectVersions (..),

    -- * InventoryOptionalField
    InventoryOptionalField (..),

    -- * JSONType
    JSONType (..),

    -- * MFADelete
    MFADelete (..),

    -- * MFADeleteStatus
    MFADeleteStatus (..),

    -- * MetadataDirective
    MetadataDirective (..),

    -- * MetricsStatus
    MetricsStatus (..),

    -- * ObjectCannedACL
    ObjectCannedACL (..),

    -- * ObjectLockEnabled
    ObjectLockEnabled (..),

    -- * ObjectLockLegalHoldStatus
    ObjectLockLegalHoldStatus (..),

    -- * ObjectLockMode
    ObjectLockMode (..),

    -- * ObjectLockRetentionMode
    ObjectLockRetentionMode (..),

    -- * ObjectOwnership
    ObjectOwnership (..),

    -- * ObjectStorageClass
    ObjectStorageClass (..),

    -- * ObjectVersionStorageClass
    ObjectVersionStorageClass (..),

    -- * OwnerOverride
    OwnerOverride (..),

    -- * Payer
    Payer (..),

    -- * Permission
    Permission (..),

    -- * Protocol
    Protocol (..),

    -- * QuoteFields
    QuoteFields (..),

    -- * ReplicationRuleStatus
    ReplicationRuleStatus (..),

    -- * ReplicationStatus
    ReplicationStatus (..),

    -- * ReplicationTimeStatus
    ReplicationTimeStatus (..),

    -- * RequestCharged
    RequestCharged (..),

    -- * RequestPayer
    RequestPayer (..),

    -- * RestoreRequestType
    RestoreRequestType (..),

    -- * ServerSideEncryption
    ServerSideEncryption (..),

    -- * SseKMSEncryptedObjectsStatus
    SseKMSEncryptedObjectsStatus (..),

    -- * StorageClass
    StorageClass (..),

    -- * StorageClassAnalysisSchemaVersion
    StorageClassAnalysisSchemaVersion (..),

    -- * TaggingDirective
    TaggingDirective (..),

    -- * Tier
    Tier (..),

    -- * TransitionStorageClass
    TransitionStorageClass (..),

    -- * Type
    Type (..),

    -- * AbortIncompleteMultipartUpload
    AbortIncompleteMultipartUpload (..),
    mkAbortIncompleteMultipartUpload,
    aimuDaysAfterInitiation,

    -- * AccelerateConfiguration
    AccelerateConfiguration (..),
    mkAccelerateConfiguration,
    acStatus,

    -- * AccessControlPolicy
    AccessControlPolicy (..),
    mkAccessControlPolicy,
    acpGrants,
    acpOwner,

    -- * AccessControlTranslation
    AccessControlTranslation (..),
    mkAccessControlTranslation,
    actOwner,

    -- * AnalyticsAndOperator
    AnalyticsAndOperator (..),
    mkAnalyticsAndOperator,
    aaoPrefix,
    aaoTags,

    -- * AnalyticsConfiguration
    AnalyticsConfiguration (..),
    mkAnalyticsConfiguration,
    acStorageClassAnalysis,
    acId,
    acFilter,

    -- * AnalyticsExportDestination
    AnalyticsExportDestination (..),
    mkAnalyticsExportDestination,
    aedS3BucketDestination,

    -- * AnalyticsFilter
    AnalyticsFilter (..),
    mkAnalyticsFilter,
    afTag,
    afPrefix,
    afAnd,

    -- * AnalyticsS3BucketDestination
    AnalyticsS3BucketDestination (..),
    mkAnalyticsS3BucketDestination,
    asbdBucketAccountId,
    asbdPrefix,
    asbdFormat,
    asbdBucket,

    -- * Bucket
    Bucket (..),
    mkBucket,
    bName,
    bCreationDate,

    -- * BucketLifecycleConfiguration
    BucketLifecycleConfiguration (..),
    mkBucketLifecycleConfiguration,
    blcRules,

    -- * BucketLoggingStatus
    BucketLoggingStatus (..),
    mkBucketLoggingStatus,
    blsLoggingEnabled,

    -- * CORSConfiguration
    CORSConfiguration (..),
    mkCORSConfiguration,
    ccCORSRules,

    -- * CORSRule
    CORSRule (..),
    mkCORSRule,
    crAllowedMethods,
    crMaxAgeSeconds,
    crAllowedHeaders,
    crAllowedOrigins,
    crExposeHeaders,

    -- * CSVInput
    CSVInput (..),
    mkCSVInput,
    ciQuoteCharacter,
    ciRecordDelimiter,
    ciAllowQuotedRecordDelimiter,
    ciFileHeaderInfo,
    ciQuoteEscapeCharacter,
    ciComments,
    ciFieldDelimiter,

    -- * CSVOutput
    CSVOutput (..),
    mkCSVOutput,
    coQuoteCharacter,
    coQuoteFields,
    coRecordDelimiter,
    coQuoteEscapeCharacter,
    coFieldDelimiter,

    -- * CommonPrefix
    CommonPrefix (..),
    mkCommonPrefix,
    cpPrefix,

    -- * CompletedMultipartUpload
    CompletedMultipartUpload (..),
    mkCompletedMultipartUpload,
    cmuParts,

    -- * CompletedPart
    CompletedPart (..),
    mkCompletedPart,
    cpETag,
    cpPartNumber,

    -- * Condition
    Condition (..),
    mkCondition,
    cKeyPrefixEquals,
    cHTTPErrorCodeReturnedEquals,

    -- * ContinuationEvent
    ContinuationEvent (..),
    mkContinuationEvent,

    -- * CopyObjectResult
    CopyObjectResult (..),
    mkCopyObjectResult,
    corETag,
    corLastModified,

    -- * CopyPartResult
    CopyPartResult (..),
    mkCopyPartResult,
    cprETag,
    cprLastModified,

    -- * CreateBucketConfiguration
    CreateBucketConfiguration (..),
    mkCreateBucketConfiguration,
    cbcLocationConstraint,

    -- * DefaultRetention
    DefaultRetention (..),
    mkDefaultRetention,
    drDays,
    drMode,
    drYears,

    -- * Delete
    Delete (..),
    mkDelete,
    dQuiet,
    dObjects,

    -- * DeleteMarkerEntry
    DeleteMarkerEntry (..),
    mkDeleteMarkerEntry,
    dmeVersionId,
    dmeIsLatest,
    dmeOwner,
    dmeKey,
    dmeLastModified,

    -- * DeleteMarkerReplication
    DeleteMarkerReplication (..),
    mkDeleteMarkerReplication,
    dmrStatus,

    -- * DeletedObject
    DeletedObject (..),
    mkDeletedObject,
    doVersionId,
    doDeleteMarker,
    doDeleteMarkerVersionId,
    doKey,

    -- * Destination
    Destination (..),
    mkDestination,
    dMetrics,
    dAccessControlTranslation,
    dBucket,
    dAccount,
    dStorageClass,
    dEncryptionConfiguration,
    dReplicationTime,

    -- * Encryption
    Encryption (..),
    mkEncryption,
    eEncryptionType,
    eKMSKeyId,
    eKMSContext,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecReplicaKMSKeyId,

    -- * EndEvent
    EndEvent (..),
    mkEndEvent,

    -- * ErrorDocument
    ErrorDocument (..),
    mkErrorDocument,
    edKey,

    -- * ExistingObjectReplication
    ExistingObjectReplication (..),
    mkExistingObjectReplication,
    eorStatus,

    -- * FilterRule
    FilterRule (..),
    mkFilterRule,
    frValue,
    frName,

    -- * GlacierJobParameters
    GlacierJobParameters (..),
    mkGlacierJobParameters,
    gjpTier,

    -- * Grant
    Grant (..),
    mkGrant,
    gPermission,
    gGrantee,

    -- * Grantee
    Grantee (..),
    mkGrantee,
    gURI,
    gEmailAddress,
    gDisplayName,
    gId,
    gType,

    -- * IndexDocument
    IndexDocument (..),
    mkIndexDocument,
    idSuffix,

    -- * Initiator
    Initiator (..),
    mkInitiator,
    iDisplayName,
    iId,

    -- * InputSerialization
    InputSerialization (..),
    mkInputSerialization,
    isJSON,
    isCSV,
    isParquet,
    isCompressionType,

    -- * IntelligentTieringAndOperator
    IntelligentTieringAndOperator (..),
    mkIntelligentTieringAndOperator,
    itaoPrefix,
    itaoTags,

    -- * IntelligentTieringConfiguration
    IntelligentTieringConfiguration (..),
    mkIntelligentTieringConfiguration,
    itcStatus,
    itcTierings,
    itcId,
    itcFilter,

    -- * IntelligentTieringFilter
    IntelligentTieringFilter (..),
    mkIntelligentTieringFilter,
    itfTag,
    itfPrefix,
    itfAnd,

    -- * InventoryConfiguration
    InventoryConfiguration (..),
    mkInventoryConfiguration,
    icIncludedObjectVersions,
    icDestination,
    icSchedule,
    icIsEnabled,
    icOptionalFields,
    icId,
    icFilter,

    -- * InventoryDestination
    InventoryDestination (..),
    mkInventoryDestination,
    idS3BucketDestination,

    -- * InventoryEncryption
    InventoryEncryption (..),
    mkInventoryEncryption,
    ieSSES3,
    ieSSEKMS,

    -- * InventoryFilter
    InventoryFilter (..),
    mkInventoryFilter,
    ifPrefix,

    -- * InventoryS3BucketDestination
    InventoryS3BucketDestination (..),
    mkInventoryS3BucketDestination,
    isbdPrefix,
    isbdFormat,
    isbdBucket,
    isbdAccountId,
    isbdEncryption,

    -- * InventorySchedule
    InventorySchedule (..),
    mkInventorySchedule,
    isFrequency,

    -- * JSONInput
    JSONInput (..),
    mkJSONInput,
    jiType,

    -- * JSONOutput
    JSONOutput (..),
    mkJSONOutput,
    joRecordDelimiter,

    -- * LambdaFunctionConfiguration
    LambdaFunctionConfiguration (..),
    mkLambdaFunctionConfiguration,
    lfcLambdaFunctionARN,
    lfcEvents,
    lfcId,
    lfcFilter,

    -- * LifecycleExpiration
    LifecycleExpiration (..),
    mkLifecycleExpiration,
    leDays,
    leDate,
    leExpiredObjectDeleteMarker,

    -- * LifecycleRule
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

    -- * LifecycleRuleAndOperator
    LifecycleRuleAndOperator (..),
    mkLifecycleRuleAndOperator,
    lraoPrefix,
    lraoTags,

    -- * LifecycleRuleFilter
    LifecycleRuleFilter (..),
    mkLifecycleRuleFilter,
    lrfTag,
    lrfPrefix,
    lrfAnd,

    -- * LoggingEnabled
    LoggingEnabled (..),
    mkLoggingEnabled,
    leTargetBucket,
    leTargetGrants,
    leTargetPrefix,

    -- * MetadataEntry
    MetadataEntry (..),
    mkMetadataEntry,
    meValue,
    meName,

    -- * Metrics
    Metrics (..),
    mkMetrics,
    mStatus,
    mEventThreshold,

    -- * MetricsAndOperator
    MetricsAndOperator (..),
    mkMetricsAndOperator,
    maoPrefix,
    maoTags,

    -- * MetricsConfiguration
    MetricsConfiguration (..),
    mkMetricsConfiguration,
    mcId,
    mcFilter,

    -- * MetricsFilter
    MetricsFilter (..),
    mkMetricsFilter,
    mfTag,
    mfPrefix,
    mfAnd,

    -- * MultipartUpload
    MultipartUpload (..),
    mkMultipartUpload,
    muInitiated,
    muInitiator,
    muOwner,
    muKey,
    muStorageClass,
    muUploadId,

    -- * NoncurrentVersionExpiration
    NoncurrentVersionExpiration (..),
    mkNoncurrentVersionExpiration,
    nveNoncurrentDays,

    -- * NoncurrentVersionTransition
    NoncurrentVersionTransition (..),
    mkNoncurrentVersionTransition,
    nvtStorageClass,
    nvtNoncurrentDays,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncQueueConfigurations,
    ncTopicConfigurations,
    ncLambdaFunctionConfigurations,

    -- * NotificationConfigurationFilter
    NotificationConfigurationFilter (..),
    mkNotificationConfigurationFilter,
    ncfKey,

    -- * Object
    Object (..),
    mkObject,
    oETag,
    oSize,
    oOwner,
    oKey,
    oStorageClass,
    oLastModified,

    -- * ObjectIdentifier
    ObjectIdentifier (..),
    mkObjectIdentifier,
    oiVersionId,
    oiKey,

    -- * ObjectLockConfiguration
    ObjectLockConfiguration (..),
    mkObjectLockConfiguration,
    olcObjectLockEnabled,
    olcRule,

    -- * ObjectLockLegalHold
    ObjectLockLegalHold (..),
    mkObjectLockLegalHold,
    ollhStatus,

    -- * ObjectLockRetention
    ObjectLockRetention (..),
    mkObjectLockRetention,
    olrMode,
    olrRetainUntilDate,

    -- * ObjectLockRule
    ObjectLockRule (..),
    mkObjectLockRule,
    olrDefaultRetention,

    -- * ObjectVersion
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

    -- * OutputLocation
    OutputLocation (..),
    mkOutputLocation,
    olS3,

    -- * OutputSerialization
    OutputSerialization (..),
    mkOutputSerialization,
    osJSON,
    osCSV,

    -- * Owner
    Owner (..),
    mkOwner,
    oDisplayName,
    oId,

    -- * OwnershipControls
    OwnershipControls (..),
    mkOwnershipControls,
    ocRules,

    -- * OwnershipControlsRule
    OwnershipControlsRule (..),
    mkOwnershipControlsRule,
    ocrObjectOwnership,

    -- * ParquetInput
    ParquetInput (..),
    mkParquetInput,

    -- * Part
    Part (..),
    mkPart,
    pETag,
    pSize,
    pPartNumber,
    pLastModified,

    -- * PolicyStatus
    PolicyStatus (..),
    mkPolicyStatus,
    psIsPublic,

    -- * Progress
    Progress (..),
    mkProgress,
    pBytesReturned,
    pBytesScanned,
    pBytesProcessed,

    -- * ProgressEvent
    ProgressEvent (..),
    mkProgressEvent,
    peDetails,

    -- * PublicAccessBlockConfiguration
    PublicAccessBlockConfiguration (..),
    mkPublicAccessBlockConfiguration,
    pabcIgnorePublicACLs,
    pabcBlockPublicACLs,
    pabcRestrictPublicBuckets,
    pabcBlockPublicPolicy,

    -- * QueueConfiguration
    QueueConfiguration (..),
    mkQueueConfiguration,
    qcQueueARN,
    qcEvents,
    qcId,
    qcFilter,

    -- * RecordsEvent
    RecordsEvent (..),
    mkRecordsEvent,
    rePayload,

    -- * Redirect
    Redirect (..),
    mkRedirect,
    rHostName,
    rProtocol,
    rHTTPRedirectCode,
    rReplaceKeyWith,
    rReplaceKeyPrefixWith,

    -- * RedirectAllRequestsTo
    RedirectAllRequestsTo (..),
    mkRedirectAllRequestsTo,
    rartHostName,
    rartProtocol,

    -- * ReplicationConfiguration
    ReplicationConfiguration (..),
    mkReplicationConfiguration,
    rcRules,
    rcRole,

    -- * ReplicationRule
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

    -- * ReplicationRuleAndOperator
    ReplicationRuleAndOperator (..),
    mkReplicationRuleAndOperator,
    rraoPrefix,
    rraoTags,

    -- * ReplicationRuleFilter
    ReplicationRuleFilter (..),
    mkReplicationRuleFilter,
    rrfTag,
    rrfPrefix,
    rrfAnd,

    -- * ReplicationTime
    ReplicationTime (..),
    mkReplicationTime,
    rtStatus,
    rtTime,

    -- * ReplicationTimeValue
    ReplicationTimeValue (..),
    mkReplicationTimeValue,
    rtvMinutes,

    -- * RequestPaymentConfiguration
    RequestPaymentConfiguration (..),
    mkRequestPaymentConfiguration,
    rpcPayer,

    -- * RequestProgress
    RequestProgress (..),
    mkRequestProgress,
    rpEnabled,

    -- * RestoreRequest
    RestoreRequest (..),
    mkRestoreRequest,
    rrDays,
    rrSelectParameters,
    rrOutputLocation,
    rrTier,
    rrGlacierJobParameters,
    rrType,
    rrDescription,

    -- * RoutingRule
    RoutingRule (..),
    mkRoutingRule,
    rrRedirect,
    rrCondition,

    -- * S3KeyFilter
    S3KeyFilter (..),
    mkS3KeyFilter,
    skfFilterRules,

    -- * S3Location
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

    -- * S3ServiceError
    S3ServiceError (..),
    mkS3ServiceError,
    sseVersionId,
    sseKey,
    sseCode,
    sseMessage,

    -- * SSEKMS
    SSEKMS (..),
    mkSSEKMS,
    ssekKeyId,

    -- * SSES3
    SSES3 (..),
    mkSSES3,

    -- * ScanRange
    ScanRange (..),
    mkScanRange,
    srStart,
    srEnd,

    -- * SelectObjectContentEventStream
    SelectObjectContentEventStream (..),
    mkSelectObjectContentEventStream,
    socesProgress,
    socesRecords,
    socesCont,
    socesStats,
    socesEnd,

    -- * SelectParameters
    SelectParameters (..),
    mkSelectParameters,
    spExpressionType,
    spOutputSerialization,
    spExpression,
    spInputSerialization,

    -- * ServerSideEncryptionByDefault
    ServerSideEncryptionByDefault (..),
    mkServerSideEncryptionByDefault,
    ssebdSSEAlgorithm,
    ssebdKMSMasterKeyId,

    -- * ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration (..),
    mkServerSideEncryptionConfiguration,
    ssecRules,

    -- * ServerSideEncryptionRule
    ServerSideEncryptionRule (..),
    mkServerSideEncryptionRule,
    sserApplyServerSideEncryptionByDefault,

    -- * SourceSelectionCriteria
    SourceSelectionCriteria (..),
    mkSourceSelectionCriteria,
    sscSseKMSEncryptedObjects,

    -- * SseKMSEncryptedObjects
    SseKMSEncryptedObjects (..),
    mkSseKMSEncryptedObjects,
    skeoStatus,

    -- * Stats
    Stats (..),
    mkStats,
    sBytesReturned,
    sBytesScanned,
    sBytesProcessed,

    -- * StatsEvent
    StatsEvent (..),
    mkStatsEvent,
    seDetails,

    -- * StorageClassAnalysis
    StorageClassAnalysis (..),
    mkStorageClassAnalysis,
    scaDataExport,

    -- * StorageClassAnalysisDataExport
    StorageClassAnalysisDataExport (..),
    mkStorageClassAnalysisDataExport,
    scadeOutputSchemaVersion,
    scadeDestination,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Tagging
    Tagging (..),
    mkTagging,
    tTagSet,

    -- * TargetGrant
    TargetGrant (..),
    mkTargetGrant,
    tgPermission,
    tgGrantee,

    -- * Tiering
    Tiering (..),
    mkTiering,
    tDays,
    tAccessTier,

    -- * TopicConfiguration
    TopicConfiguration (..),
    mkTopicConfiguration,
    tcTopicARN,
    tcEvents,
    tcId,
    tcFilter,

    -- * Transition
    Transition (..),
    mkTransition,
    tfDays,
    tfDate,
    tfStorageClass,

    -- * VersioningConfiguration
    VersioningConfiguration (..),
    mkVersioningConfiguration,
    vcStatus,
    vcMFADelete,

    -- * WebsiteConfiguration
    WebsiteConfiguration (..),
    mkWebsiteConfiguration,
    wcRedirectAllRequestsTo,
    wcErrorDocument,
    wcIndexDocument,
    wcRoutingRules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AbortIncompleteMultipartUpload
import Network.AWS.S3.Types.AccelerateConfiguration
import Network.AWS.S3.Types.AccessControlPolicy
import Network.AWS.S3.Types.AccessControlTranslation
import Network.AWS.S3.Types.AnalyticsAndOperator
import Network.AWS.S3.Types.AnalyticsConfiguration
import Network.AWS.S3.Types.AnalyticsExportDestination
import Network.AWS.S3.Types.AnalyticsFilter
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
import Network.AWS.S3.Types.CommonPrefix
import Network.AWS.S3.Types.CompletedMultipartUpload
import Network.AWS.S3.Types.CompletedPart
import Network.AWS.S3.Types.CompressionType
import Network.AWS.S3.Types.Condition
import Network.AWS.S3.Types.ContinuationEvent
import Network.AWS.S3.Types.CopyObjectResult
import Network.AWS.S3.Types.CopyPartResult
import Network.AWS.S3.Types.CreateBucketConfiguration
import Network.AWS.S3.Types.DefaultRetention
import Network.AWS.S3.Types.Delete
import Network.AWS.S3.Types.DeleteMarkerEntry
import Network.AWS.S3.Types.DeleteMarkerReplication
import Network.AWS.S3.Types.DeleteMarkerReplicationStatus
import Network.AWS.S3.Types.DeletedObject
import Network.AWS.S3.Types.Destination
import Network.AWS.S3.Types.EncodingType
import Network.AWS.S3.Types.Encryption
import Network.AWS.S3.Types.EncryptionConfiguration
import Network.AWS.S3.Types.EndEvent
import Network.AWS.S3.Types.ErrorDocument
import Network.AWS.S3.Types.Event
import Network.AWS.S3.Types.ExistingObjectReplication
import Network.AWS.S3.Types.ExistingObjectReplicationStatus
import Network.AWS.S3.Types.ExpirationStatus
import Network.AWS.S3.Types.ExpressionType
import Network.AWS.S3.Types.FileHeaderInfo
import Network.AWS.S3.Types.FilterRule
import Network.AWS.S3.Types.FilterRuleName
import Network.AWS.S3.Types.GlacierJobParameters
import Network.AWS.S3.Types.Grant
import Network.AWS.S3.Types.Grantee
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
import Network.AWS.S3.Types.InventoryIncludedObjectVersions
import Network.AWS.S3.Types.InventoryOptionalField
import Network.AWS.S3.Types.InventoryS3BucketDestination
import Network.AWS.S3.Types.InventorySchedule
import Network.AWS.S3.Types.JSONInput
import Network.AWS.S3.Types.JSONOutput
import Network.AWS.S3.Types.JSONType
import Network.AWS.S3.Types.LambdaFunctionConfiguration
import Network.AWS.S3.Types.LifecycleExpiration
import Network.AWS.S3.Types.LifecycleRule
import Network.AWS.S3.Types.LifecycleRuleAndOperator
import Network.AWS.S3.Types.LifecycleRuleFilter
import Network.AWS.S3.Types.LoggingEnabled
import Network.AWS.S3.Types.MFADelete
import Network.AWS.S3.Types.MFADeleteStatus
import Network.AWS.S3.Types.MetadataDirective
import Network.AWS.S3.Types.MetadataEntry
import Network.AWS.S3.Types.Metrics
import Network.AWS.S3.Types.MetricsAndOperator
import Network.AWS.S3.Types.MetricsConfiguration
import Network.AWS.S3.Types.MetricsFilter
import Network.AWS.S3.Types.MetricsStatus
import Network.AWS.S3.Types.MultipartUpload
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
import Network.AWS.S3.Types.Progress
import Network.AWS.S3.Types.ProgressEvent
import Network.AWS.S3.Types.Protocol
import Network.AWS.S3.Types.PublicAccessBlockConfiguration
import Network.AWS.S3.Types.QueueConfiguration
import Network.AWS.S3.Types.QuoteFields
import Network.AWS.S3.Types.RecordsEvent
import Network.AWS.S3.Types.Redirect
import Network.AWS.S3.Types.RedirectAllRequestsTo
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
import Network.AWS.S3.Types.RestoreRequest
import Network.AWS.S3.Types.RestoreRequestType
import Network.AWS.S3.Types.RoutingRule
import Network.AWS.S3.Types.S3KeyFilter
import Network.AWS.S3.Types.S3Location
import Network.AWS.S3.Types.S3ServiceError
import Network.AWS.S3.Types.SSEKMS
import Network.AWS.S3.Types.SSES3
import Network.AWS.S3.Types.ScanRange
import Network.AWS.S3.Types.SelectObjectContentEventStream
import Network.AWS.S3.Types.SelectParameters
import Network.AWS.S3.Types.ServerSideEncryption
import Network.AWS.S3.Types.ServerSideEncryptionByDefault
import Network.AWS.S3.Types.ServerSideEncryptionConfiguration
import Network.AWS.S3.Types.ServerSideEncryptionRule
import Network.AWS.S3.Types.SourceSelectionCriteria
import Network.AWS.S3.Types.SseKMSEncryptedObjects
import Network.AWS.S3.Types.SseKMSEncryptedObjectsStatus
import Network.AWS.S3.Types.Stats
import Network.AWS.S3.Types.StatsEvent
import Network.AWS.S3.Types.StorageClass
import Network.AWS.S3.Types.StorageClassAnalysis
import Network.AWS.S3.Types.StorageClassAnalysisDataExport
import Network.AWS.S3.Types.StorageClassAnalysisSchemaVersion
import Network.AWS.S3.Types.Tag
import Network.AWS.S3.Types.Tagging
import Network.AWS.S3.Types.TaggingDirective
import Network.AWS.S3.Types.TargetGrant
import Network.AWS.S3.Types.Tier
import Network.AWS.S3.Types.Tiering
import Network.AWS.S3.Types.TopicConfiguration
import Network.AWS.S3.Types.Transition
import Network.AWS.S3.Types.TransitionStorageClass
import Network.AWS.S3.Types.Type
import Network.AWS.S3.Types.VersioningConfiguration
import Network.AWS.S3.Types.WebsiteConfiguration
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2006-03-01@ of the Amazon Simple Storage Service SDK configuration.
s3Service :: Lude.Service
s3Service =
  Lude.Service
    { Lude._svcAbbrev = "S3",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "s3",
      Lude._svcVersion = "2006-03-01",
      Lude._svcEndpoint = Lude.defaultEndpoint s3Service,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "S3",
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
      | Lens.has (Lude.hasCode "BadDigest" Lude.. Lude.hasStatus 400) e =
        Lude.Just "contentmd5"
      | Lens.has
          (Lude.hasCode "RequestTimeout" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "timeouts"
      | Lude.otherwise = Lude.Nothing
