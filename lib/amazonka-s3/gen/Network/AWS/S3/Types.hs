{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types
  ( -- * Service Configuration
    s3,

    -- * Errors

    -- * Re-exported Types
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
    AbortIncompleteMultipartUpload,
    abortIncompleteMultipartUpload,
    aimuDaysAfterInitiation,

    -- * AccelerateConfiguration
    AccelerateConfiguration,
    accelerateConfiguration,
    acStatus,

    -- * AccessControlPolicy
    AccessControlPolicy,
    accessControlPolicy,
    acpGrants,
    acpOwner,

    -- * AccessControlTranslation
    AccessControlTranslation,
    accessControlTranslation,
    actOwner,

    -- * AnalyticsAndOperator
    AnalyticsAndOperator,
    analyticsAndOperator,
    aaoPrefix,
    aaoTags,

    -- * AnalyticsConfiguration
    AnalyticsConfiguration,
    analyticsConfiguration,
    acFilter,
    acId,
    acStorageClassAnalysis,

    -- * AnalyticsExportDestination
    AnalyticsExportDestination,
    analyticsExportDestination,
    aedS3BucketDestination,

    -- * AnalyticsFilter
    AnalyticsFilter,
    analyticsFilter,
    afTag,
    afPrefix,
    afAnd,

    -- * AnalyticsS3BucketDestination
    AnalyticsS3BucketDestination,
    analyticsS3BucketDestination,
    asbdBucketAccountId,
    asbdPrefix,
    asbdFormat,
    asbdBucket,

    -- * Bucket
    Bucket,
    bucket,
    bCreationDate,
    bName,

    -- * BucketLifecycleConfiguration
    BucketLifecycleConfiguration,
    bucketLifecycleConfiguration,
    blcRules,

    -- * BucketLoggingStatus
    BucketLoggingStatus,
    bucketLoggingStatus,
    blsLoggingEnabled,

    -- * CORSConfiguration
    CORSConfiguration,
    corsConfiguration,
    ccCORSRules,

    -- * CORSRule
    CORSRule,
    corsRule,
    crMaxAgeSeconds,
    crAllowedHeaders,
    crExposeHeaders,
    crAllowedMethods,
    crAllowedOrigins,

    -- * CSVInput
    CSVInput,
    csvInput,
    ciQuoteCharacter,
    ciRecordDelimiter,
    ciAllowQuotedRecordDelimiter,
    ciFileHeaderInfo,
    ciQuoteEscapeCharacter,
    ciComments,
    ciFieldDelimiter,

    -- * CSVOutput
    CSVOutput,
    csvOutput,
    coQuoteCharacter,
    coQuoteFields,
    coRecordDelimiter,
    coQuoteEscapeCharacter,
    coFieldDelimiter,

    -- * CommonPrefix
    CommonPrefix,
    commonPrefix,
    cpPrefix,

    -- * CompletedMultipartUpload
    CompletedMultipartUpload,
    completedMultipartUpload,
    cmuParts,

    -- * CompletedPart
    CompletedPart,
    completedPart,
    cpPartNumber,
    cpETag,

    -- * Condition
    Condition,
    condition,
    cKeyPrefixEquals,
    cHTTPErrorCodeReturnedEquals,

    -- * ContinuationEvent
    ContinuationEvent,
    continuationEvent,

    -- * CopyObjectResult
    CopyObjectResult,
    copyObjectResult,
    corETag,
    corLastModified,

    -- * CopyPartResult
    CopyPartResult,
    copyPartResult,
    cprETag,
    cprLastModified,

    -- * CreateBucketConfiguration
    CreateBucketConfiguration,
    createBucketConfiguration,
    cbcLocationConstraint,

    -- * DefaultRetention
    DefaultRetention,
    defaultRetention,
    drDays,
    drMode,
    drYears,

    -- * Delete
    Delete,
    delete',
    dQuiet,
    dObjects,

    -- * DeleteMarkerEntry
    DeleteMarkerEntry,
    deleteMarkerEntry,
    dmeVersionId,
    dmeIsLatest,
    dmeOwner,
    dmeKey,
    dmeLastModified,

    -- * DeleteMarkerReplication
    DeleteMarkerReplication,
    deleteMarkerReplication,
    dmrStatus,

    -- * DeletedObject
    DeletedObject,
    deletedObject,
    dVersionId,
    dDeleteMarker,
    dDeleteMarkerVersionId,
    dKey,

    -- * Destination
    Destination,
    destination,
    dMetrics,
    dAccessControlTranslation,
    dAccount,
    dStorageClass,
    dEncryptionConfiguration,
    dReplicationTime,
    dBucket,

    -- * Encryption
    Encryption,
    encryption,
    eKMSKeyId,
    eKMSContext,
    eEncryptionType,

    -- * EncryptionConfiguration
    EncryptionConfiguration,
    encryptionConfiguration,
    ecReplicaKMSKeyId,

    -- * EndEvent
    EndEvent,
    endEvent,

    -- * ErrorDocument
    ErrorDocument,
    errorDocument,
    edKey,

    -- * ExistingObjectReplication
    ExistingObjectReplication,
    existingObjectReplication,
    eorStatus,

    -- * FilterRule
    FilterRule,
    filterRule,
    frValue,
    frName,

    -- * GlacierJobParameters
    GlacierJobParameters,
    glacierJobParameters,
    gjpTier,

    -- * Grant
    Grant,
    grant,
    gPermission,
    gGrantee,

    -- * Grantee
    Grantee,
    grantee,
    gURI,
    gEmailAddress,
    gDisplayName,
    gId,
    gType,

    -- * IndexDocument
    IndexDocument,
    indexDocument,
    idSuffix,

    -- * Initiator
    Initiator,
    initiator,
    iDisplayName,
    iId,

    -- * InputSerialization
    InputSerialization,
    inputSerialization,
    isJSON,
    isCSV,
    isParquet,
    isCompressionType,

    -- * IntelligentTieringAndOperator
    IntelligentTieringAndOperator,
    intelligentTieringAndOperator,
    itaoPrefix,
    itaoTags,

    -- * IntelligentTieringConfiguration
    IntelligentTieringConfiguration,
    intelligentTieringConfiguration,
    itcFilter,
    itcId,
    itcStatus,
    itcTierings,

    -- * IntelligentTieringFilter
    IntelligentTieringFilter,
    intelligentTieringFilter,
    itfTag,
    itfPrefix,
    itfAnd,

    -- * InventoryConfiguration
    InventoryConfiguration,
    inventoryConfiguration,
    icOptionalFields,
    icFilter,
    icDestination,
    icIsEnabled,
    icId,
    icIncludedObjectVersions,
    icSchedule,

    -- * InventoryDestination
    InventoryDestination,
    inventoryDestination,
    idS3BucketDestination,

    -- * InventoryEncryption
    InventoryEncryption,
    inventoryEncryption,
    ieSSES3,
    ieSSEKMS,

    -- * InventoryFilter
    InventoryFilter,
    inventoryFilter,
    ifPrefix,

    -- * InventoryS3BucketDestination
    InventoryS3BucketDestination,
    inventoryS3BucketDestination,
    isbdPrefix,
    isbdAccountId,
    isbdEncryption,
    isbdBucket,
    isbdFormat,

    -- * InventorySchedule
    InventorySchedule,
    inventorySchedule,
    isFrequency,

    -- * JSONInput
    JSONInput,
    jsonInput,
    jiType,

    -- * JSONOutput
    JSONOutput,
    jsonOutput,
    joRecordDelimiter,

    -- * LambdaFunctionConfiguration
    LambdaFunctionConfiguration,
    lambdaFunctionConfiguration,
    lfcId,
    lfcFilter,
    lfcLambdaFunctionARN,
    lfcEvents,

    -- * LifecycleExpiration
    LifecycleExpiration,
    lifecycleExpiration,
    leDays,
    leDate,
    leExpiredObjectDeleteMarker,

    -- * LifecycleRule
    LifecycleRule,
    lifecycleRule,
    lrTransitions,
    lrNoncurrentVersionExpiration,
    lrPrefix,
    lrNoncurrentVersionTransitions,
    lrExpiration,
    lrId,
    lrFilter,
    lrAbortIncompleteMultipartUpload,
    lrStatus,

    -- * LifecycleRuleAndOperator
    LifecycleRuleAndOperator,
    lifecycleRuleAndOperator,
    lraoPrefix,
    lraoTags,

    -- * LifecycleRuleFilter
    LifecycleRuleFilter,
    lifecycleRuleFilter,
    lrfTag,
    lrfPrefix,
    lrfAnd,

    -- * LoggingEnabled
    LoggingEnabled,
    loggingEnabled,
    leTargetGrants,
    leTargetBucket,
    leTargetPrefix,

    -- * MetadataEntry
    MetadataEntry,
    metadataEntry,
    meValue,
    meName,

    -- * Metrics
    Metrics,
    metrics,
    mEventThreshold,
    mStatus,

    -- * MetricsAndOperator
    MetricsAndOperator,
    metricsAndOperator,
    maoPrefix,
    maoTags,

    -- * MetricsConfiguration
    MetricsConfiguration,
    metricsConfiguration,
    mcFilter,
    mcId,

    -- * MetricsFilter
    MetricsFilter,
    metricsFilter,
    mfTag,
    mfPrefix,
    mfAnd,

    -- * MultipartUpload
    MultipartUpload,
    multipartUpload,
    muInitiated,
    muInitiator,
    muOwner,
    muKey,
    muStorageClass,
    muUploadId,

    -- * NoncurrentVersionExpiration
    NoncurrentVersionExpiration,
    noncurrentVersionExpiration,
    nveNoncurrentDays,

    -- * NoncurrentVersionTransition
    NoncurrentVersionTransition,
    noncurrentVersionTransition,
    nvtNoncurrentDays,
    nvtStorageClass,

    -- * NotificationConfiguration
    NotificationConfiguration,
    notificationConfiguration,
    ncQueueConfigurations,
    ncTopicConfigurations,
    ncLambdaFunctionConfigurations,

    -- * NotificationConfigurationFilter
    NotificationConfigurationFilter,
    notificationConfigurationFilter,
    ncfKey,

    -- * Object
    Object,
    object',
    oOwner,
    oETag,
    oSize,
    oKey,
    oStorageClass,
    oLastModified,

    -- * ObjectIdentifier
    ObjectIdentifier,
    objectIdentifier,
    oiVersionId,
    oiKey,

    -- * ObjectLockConfiguration
    ObjectLockConfiguration,
    objectLockConfiguration,
    olcObjectLockEnabled,
    olcRule,

    -- * ObjectLockLegalHold
    ObjectLockLegalHold,
    objectLockLegalHold,
    ollhStatus,

    -- * ObjectLockRetention
    ObjectLockRetention,
    objectLockRetention,
    olrMode,
    olrRetainUntilDate,

    -- * ObjectLockRule
    ObjectLockRule,
    objectLockRule,
    olrDefaultRetention,

    -- * ObjectVersion
    ObjectVersion,
    objectVersion,
    ovETag,
    ovVersionId,
    ovSize,
    ovIsLatest,
    ovOwner,
    ovKey,
    ovStorageClass,
    ovLastModified,

    -- * OutputLocation
    OutputLocation,
    outputLocation,
    olS3,

    -- * OutputSerialization
    OutputSerialization,
    outputSerialization,
    osJSON,
    osCSV,

    -- * Owner
    Owner,
    owner,
    oDisplayName,
    oId,

    -- * OwnershipControls
    OwnershipControls,
    ownershipControls,
    ocRules,

    -- * OwnershipControlsRule
    OwnershipControlsRule,
    ownershipControlsRule,
    ocrObjectOwnership,

    -- * ParquetInput
    ParquetInput,
    parquetInput,

    -- * Part
    Part,
    part,
    pETag,
    pSize,
    pPartNumber,
    pLastModified,

    -- * PolicyStatus
    PolicyStatus,
    policyStatus,
    psIsPublic,

    -- * Progress
    Progress,
    progress,
    pBytesReturned,
    pBytesScanned,
    pBytesProcessed,

    -- * ProgressEvent
    ProgressEvent,
    progressEvent,
    peDetails,

    -- * PublicAccessBlockConfiguration
    PublicAccessBlockConfiguration,
    publicAccessBlockConfiguration,
    pabcIgnorePublicACLs,
    pabcBlockPublicACLs,
    pabcRestrictPublicBuckets,
    pabcBlockPublicPolicy,

    -- * QueueConfiguration
    QueueConfiguration,
    queueConfiguration,
    qcId,
    qcFilter,
    qcQueueARN,
    qcEvents,

    -- * RecordsEvent
    RecordsEvent,
    recordsEvent,
    rePayload,

    -- * Redirect
    Redirect,
    redirect,
    rHostName,
    rProtocol,
    rHTTPRedirectCode,
    rReplaceKeyWith,
    rReplaceKeyPrefixWith,

    -- * RedirectAllRequestsTo
    RedirectAllRequestsTo,
    redirectAllRequestsTo,
    rartProtocol,
    rartHostName,

    -- * ReplicationConfiguration
    ReplicationConfiguration,
    replicationConfiguration,
    rcRole,
    rcRules,

    -- * ReplicationRule
    ReplicationRule,
    replicationRule,
    rrDeleteMarkerReplication,
    rrPriority,
    rrPrefix,
    rrExistingObjectReplication,
    rrId,
    rrFilter,
    rrSourceSelectionCriteria,
    rrStatus,
    rrDestination,

    -- * ReplicationRuleAndOperator
    ReplicationRuleAndOperator,
    replicationRuleAndOperator,
    rraoPrefix,
    rraoTags,

    -- * ReplicationRuleFilter
    ReplicationRuleFilter,
    replicationRuleFilter,
    rrfTag,
    rrfPrefix,
    rrfAnd,

    -- * ReplicationTime
    ReplicationTime,
    replicationTime,
    rtStatus,
    rtTime,

    -- * ReplicationTimeValue
    ReplicationTimeValue,
    replicationTimeValue,
    rtvMinutes,

    -- * RequestPaymentConfiguration
    RequestPaymentConfiguration,
    requestPaymentConfiguration,
    rpcPayer,

    -- * RequestProgress
    RequestProgress,
    requestProgress,
    rpEnabled,

    -- * RestoreRequest
    RestoreRequest,
    restoreRequest,
    rrDays,
    rrSelectParameters,
    rrOutputLocation,
    rrTier,
    rrGlacierJobParameters,
    rrType,
    rrDescription,

    -- * RoutingRule
    RoutingRule,
    routingRule,
    rrCondition,
    rrRedirect,

    -- * S3KeyFilter
    S3KeyFilter,
    s3KeyFilter,
    skfFilterRules,

    -- * S3Location
    S3Location,
    s3Location,
    slCannedACL,
    slAccessControlList,
    slUserMetadata,
    slEncryption,
    slStorageClass,
    slTagging,
    slBucketName,
    slPrefix,

    -- * S3ServiceError
    S3ServiceError,
    s3ServiceError,
    sseVersionId,
    sseKey,
    sseCode,
    sseMessage,

    -- * SSEKMS
    SSEKMS,
    sSEKMS,
    ssekKeyId,

    -- * SSES3
    SSES3,
    sSES3,

    -- * ScanRange
    ScanRange,
    scanRange,
    srStart,
    srEnd,

    -- * SelectObjectContentEventStream
    SelectObjectContentEventStream,
    selectObjectContentEventStream,
    socesProgress,
    socesRecords,
    socesCont,
    socesStats,
    socesEnd,

    -- * SelectParameters
    SelectParameters,
    selectParameters,
    spInputSerialization,
    spExpressionType,
    spExpression,
    spOutputSerialization,

    -- * ServerSideEncryptionByDefault
    ServerSideEncryptionByDefault,
    serverSideEncryptionByDefault,
    ssebdKMSMasterKeyId,
    ssebdSSEAlgorithm,

    -- * ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration,
    serverSideEncryptionConfiguration,
    ssecRules,

    -- * ServerSideEncryptionRule
    ServerSideEncryptionRule,
    serverSideEncryptionRule,
    sserApplyServerSideEncryptionByDefault,

    -- * SourceSelectionCriteria
    SourceSelectionCriteria,
    sourceSelectionCriteria,
    sscSseKMSEncryptedObjects,

    -- * SseKMSEncryptedObjects
    SseKMSEncryptedObjects,
    sseKMSEncryptedObjects,
    skeoStatus,

    -- * Stats
    Stats,
    stats,
    sBytesReturned,
    sBytesScanned,
    sBytesProcessed,

    -- * StatsEvent
    StatsEvent,
    statsEvent,
    seDetails,

    -- * StorageClassAnalysis
    StorageClassAnalysis,
    storageClassAnalysis,
    scaDataExport,

    -- * StorageClassAnalysisDataExport
    StorageClassAnalysisDataExport,
    storageClassAnalysisDataExport,
    scadeOutputSchemaVersion,
    scadeDestination,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * Tagging
    Tagging,
    tagging,
    tTagSet,

    -- * TargetGrant
    TargetGrant,
    targetGrant,
    tgPermission,
    tgGrantee,

    -- * Tiering
    Tiering,
    tiering,
    tDays,
    tAccessTier,

    -- * TopicConfiguration
    TopicConfiguration,
    topicConfiguration,
    tcId,
    tcFilter,
    tcTopicARN,
    tcEvents,

    -- * Transition
    Transition,
    transition,
    traDays,
    traDate,
    traStorageClass,

    -- * VersioningConfiguration
    VersioningConfiguration,
    versioningConfiguration,
    vcStatus,
    vcMFADelete,

    -- * WebsiteConfiguration
    WebsiteConfiguration,
    websiteConfiguration,
    wcRedirectAllRequestsTo,
    wcErrorDocument,
    wcIndexDocument,
    wcRoutingRules,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
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
import Network.AWS.Sign.V4

-- | API version @2006-03-01@ of the Amazon Simple Storage Service SDK configuration.
s3 :: Service
s3 =
  Service
    { _svcAbbrev = "S3",
      _svcSigner = v4,
      _svcPrefix = "s3",
      _svcVersion = "2006-03-01",
      _svcEndpoint = defaultEndpoint s3,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "S3",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasCode "BadDigest" . hasStatus 400) e = Just "contentmd5"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasCode "RequestTimeout" . hasStatus 400) e =
        Just "timeouts"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
