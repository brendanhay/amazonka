{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ObjectNotInActiveTierError,
    _NoSuchKey,
    _ObjectAlreadyInActiveTierError,
    _BucketAlreadyOwnedByYou,
    _NoSuchUpload,
    _InvalidObjectState,
    _BucketAlreadyExists,
    _NoSuchBucket,

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

    -- * ReplicaModificationsStatus
    ReplicaModificationsStatus (..),

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

    -- * SseKmsEncryptedObjectsStatus
    SseKmsEncryptedObjectsStatus (..),

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
    newAbortIncompleteMultipartUpload,
    abortIncompleteMultipartUpload_daysAfterInitiation,

    -- * AccelerateConfiguration
    AccelerateConfiguration (..),
    newAccelerateConfiguration,
    accelerateConfiguration_status,

    -- * AccessControlPolicy
    AccessControlPolicy (..),
    newAccessControlPolicy,
    accessControlPolicy_owner,
    accessControlPolicy_grants,

    -- * AccessControlTranslation
    AccessControlTranslation (..),
    newAccessControlTranslation,
    accessControlTranslation_owner,

    -- * AnalyticsAndOperator
    AnalyticsAndOperator (..),
    newAnalyticsAndOperator,
    analyticsAndOperator_prefix,
    analyticsAndOperator_tags,

    -- * AnalyticsConfiguration
    AnalyticsConfiguration (..),
    newAnalyticsConfiguration,
    analyticsConfiguration_filter,
    analyticsConfiguration_id,
    analyticsConfiguration_storageClassAnalysis,

    -- * AnalyticsExportDestination
    AnalyticsExportDestination (..),
    newAnalyticsExportDestination,
    analyticsExportDestination_s3BucketDestination,

    -- * AnalyticsFilter
    AnalyticsFilter (..),
    newAnalyticsFilter,
    analyticsFilter_prefix,
    analyticsFilter_and,
    analyticsFilter_tag,

    -- * AnalyticsS3BucketDestination
    AnalyticsS3BucketDestination (..),
    newAnalyticsS3BucketDestination,
    analyticsS3BucketDestination_prefix,
    analyticsS3BucketDestination_bucketAccountId,
    analyticsS3BucketDestination_format,
    analyticsS3BucketDestination_bucket,

    -- * Bucket
    Bucket (..),
    newBucket,
    bucket_creationDate,
    bucket_name,

    -- * BucketLifecycleConfiguration
    BucketLifecycleConfiguration (..),
    newBucketLifecycleConfiguration,
    bucketLifecycleConfiguration_rules,

    -- * BucketLoggingStatus
    BucketLoggingStatus (..),
    newBucketLoggingStatus,
    bucketLoggingStatus_loggingEnabled,

    -- * CORSConfiguration
    CORSConfiguration (..),
    newCORSConfiguration,
    cORSConfiguration_cORSRules,

    -- * CORSRule
    CORSRule (..),
    newCORSRule,
    cORSRule_allowedHeaders,
    cORSRule_maxAgeSeconds,
    cORSRule_exposeHeaders,
    cORSRule_allowedMethods,
    cORSRule_allowedOrigins,

    -- * CSVInput
    CSVInput (..),
    newCSVInput,
    cSVInput_allowQuotedRecordDelimiter,
    cSVInput_recordDelimiter,
    cSVInput_quoteCharacter,
    cSVInput_fileHeaderInfo,
    cSVInput_fieldDelimiter,
    cSVInput_comments,
    cSVInput_quoteEscapeCharacter,

    -- * CSVOutput
    CSVOutput (..),
    newCSVOutput,
    cSVOutput_recordDelimiter,
    cSVOutput_quoteCharacter,
    cSVOutput_fieldDelimiter,
    cSVOutput_quoteFields,
    cSVOutput_quoteEscapeCharacter,

    -- * CommonPrefix
    CommonPrefix (..),
    newCommonPrefix,
    commonPrefix_prefix,

    -- * CompletedMultipartUpload
    CompletedMultipartUpload (..),
    newCompletedMultipartUpload,
    completedMultipartUpload_parts,

    -- * CompletedPart
    CompletedPart (..),
    newCompletedPart,
    completedPart_partNumber,
    completedPart_eTag,

    -- * Condition
    Condition (..),
    newCondition,
    condition_httpErrorCodeReturnedEquals,
    condition_keyPrefixEquals,

    -- * ContinuationEvent
    ContinuationEvent (..),
    newContinuationEvent,

    -- * CopyObjectResult
    CopyObjectResult (..),
    newCopyObjectResult,
    copyObjectResult_eTag,
    copyObjectResult_lastModified,

    -- * CopyPartResult
    CopyPartResult (..),
    newCopyPartResult,
    copyPartResult_eTag,
    copyPartResult_lastModified,

    -- * CreateBucketConfiguration
    CreateBucketConfiguration (..),
    newCreateBucketConfiguration,
    createBucketConfiguration_locationConstraint,

    -- * DefaultRetention
    DefaultRetention (..),
    newDefaultRetention,
    defaultRetention_days,
    defaultRetention_years,
    defaultRetention_mode,

    -- * Delete
    Delete (..),
    newDelete,
    delete_quiet,
    delete_objects,

    -- * DeleteMarkerEntry
    DeleteMarkerEntry (..),
    newDeleteMarkerEntry,
    deleteMarkerEntry_key,
    deleteMarkerEntry_isLatest,
    deleteMarkerEntry_versionId,
    deleteMarkerEntry_owner,
    deleteMarkerEntry_lastModified,

    -- * DeleteMarkerReplication
    DeleteMarkerReplication (..),
    newDeleteMarkerReplication,
    deleteMarkerReplication_status,

    -- * DeletedObject
    DeletedObject (..),
    newDeletedObject,
    deletedObject_key,
    deletedObject_deleteMarkerVersionId,
    deletedObject_deleteMarker,
    deletedObject_versionId,

    -- * Destination
    Destination (..),
    newDestination,
    destination_encryptionConfiguration,
    destination_replicationTime,
    destination_accessControlTranslation,
    destination_storageClass,
    destination_metrics,
    destination_account,
    destination_bucket,

    -- * Encryption
    Encryption (..),
    newEncryption,
    encryption_kmsKeyId,
    encryption_kmsContext,
    encryption_encryptionType,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    newEncryptionConfiguration,
    encryptionConfiguration_replicaKmsKeyID,

    -- * EndEvent
    EndEvent (..),
    newEndEvent,

    -- * ErrorDocument
    ErrorDocument (..),
    newErrorDocument,
    errorDocument_key,

    -- * ExistingObjectReplication
    ExistingObjectReplication (..),
    newExistingObjectReplication,
    existingObjectReplication_status,

    -- * FilterRule
    FilterRule (..),
    newFilterRule,
    filterRule_name,
    filterRule_value,

    -- * GlacierJobParameters
    GlacierJobParameters (..),
    newGlacierJobParameters,
    glacierJobParameters_tier,

    -- * Grant
    Grant (..),
    newGrant,
    grant_grantee,
    grant_permission,

    -- * Grantee
    Grantee (..),
    newGrantee,
    grantee_uri,
    grantee_id,
    grantee_displayName,
    grantee_emailAddress,
    grantee_type,

    -- * IndexDocument
    IndexDocument (..),
    newIndexDocument,
    indexDocument_suffix,

    -- * Initiator
    Initiator (..),
    newInitiator,
    initiator_id,
    initiator_displayName,

    -- * InputSerialization
    InputSerialization (..),
    newInputSerialization,
    inputSerialization_parquet,
    inputSerialization_csv,
    inputSerialization_json,
    inputSerialization_compressionType,

    -- * IntelligentTieringAndOperator
    IntelligentTieringAndOperator (..),
    newIntelligentTieringAndOperator,
    intelligentTieringAndOperator_prefix,
    intelligentTieringAndOperator_tags,

    -- * IntelligentTieringConfiguration
    IntelligentTieringConfiguration (..),
    newIntelligentTieringConfiguration,
    intelligentTieringConfiguration_filter,
    intelligentTieringConfiguration_id,
    intelligentTieringConfiguration_status,
    intelligentTieringConfiguration_tierings,

    -- * IntelligentTieringFilter
    IntelligentTieringFilter (..),
    newIntelligentTieringFilter,
    intelligentTieringFilter_prefix,
    intelligentTieringFilter_and,
    intelligentTieringFilter_tag,

    -- * InventoryConfiguration
    InventoryConfiguration (..),
    newInventoryConfiguration,
    inventoryConfiguration_optionalFields,
    inventoryConfiguration_filter,
    inventoryConfiguration_destination,
    inventoryConfiguration_isEnabled,
    inventoryConfiguration_id,
    inventoryConfiguration_includedObjectVersions,
    inventoryConfiguration_schedule,

    -- * InventoryDestination
    InventoryDestination (..),
    newInventoryDestination,
    inventoryDestination_s3BucketDestination,

    -- * InventoryEncryption
    InventoryEncryption (..),
    newInventoryEncryption,
    inventoryEncryption_ssekms,
    inventoryEncryption_sses3,

    -- * InventoryFilter
    InventoryFilter (..),
    newInventoryFilter,
    inventoryFilter_prefix,

    -- * InventoryS3BucketDestination
    InventoryS3BucketDestination (..),
    newInventoryS3BucketDestination,
    inventoryS3BucketDestination_accountId,
    inventoryS3BucketDestination_prefix,
    inventoryS3BucketDestination_encryption,
    inventoryS3BucketDestination_bucket,
    inventoryS3BucketDestination_format,

    -- * InventorySchedule
    InventorySchedule (..),
    newInventorySchedule,
    inventorySchedule_frequency,

    -- * JSONInput
    JSONInput (..),
    newJSONInput,
    jSONInput_type,

    -- * JSONOutput
    JSONOutput (..),
    newJSONOutput,
    jSONOutput_recordDelimiter,

    -- * LambdaFunctionConfiguration
    LambdaFunctionConfiguration (..),
    newLambdaFunctionConfiguration,
    lambdaFunctionConfiguration_id,
    lambdaFunctionConfiguration_filter,
    lambdaFunctionConfiguration_lambdaFunctionArn,
    lambdaFunctionConfiguration_events,

    -- * LifecycleExpiration
    LifecycleExpiration (..),
    newLifecycleExpiration,
    lifecycleExpiration_days,
    lifecycleExpiration_expiredObjectDeleteMarker,
    lifecycleExpiration_date,

    -- * LifecycleRule
    LifecycleRule (..),
    newLifecycleRule,
    lifecycleRule_expiration,
    lifecycleRule_prefix,
    lifecycleRule_noncurrentVersionTransitions,
    lifecycleRule_id,
    lifecycleRule_noncurrentVersionExpiration,
    lifecycleRule_transitions,
    lifecycleRule_abortIncompleteMultipartUpload,
    lifecycleRule_filter,
    lifecycleRule_status,

    -- * LifecycleRuleAndOperator
    LifecycleRuleAndOperator (..),
    newLifecycleRuleAndOperator,
    lifecycleRuleAndOperator_prefix,
    lifecycleRuleAndOperator_tags,

    -- * LifecycleRuleFilter
    LifecycleRuleFilter (..),
    newLifecycleRuleFilter,
    lifecycleRuleFilter_prefix,
    lifecycleRuleFilter_and,
    lifecycleRuleFilter_tag,

    -- * LoggingEnabled
    LoggingEnabled (..),
    newLoggingEnabled,
    loggingEnabled_targetGrants,
    loggingEnabled_targetBucket,
    loggingEnabled_targetPrefix,

    -- * MetadataEntry
    MetadataEntry (..),
    newMetadataEntry,
    metadataEntry_name,
    metadataEntry_value,

    -- * Metrics
    Metrics (..),
    newMetrics,
    metrics_eventThreshold,
    metrics_status,

    -- * MetricsAndOperator
    MetricsAndOperator (..),
    newMetricsAndOperator,
    metricsAndOperator_prefix,
    metricsAndOperator_tags,

    -- * MetricsConfiguration
    MetricsConfiguration (..),
    newMetricsConfiguration,
    metricsConfiguration_filter,
    metricsConfiguration_id,

    -- * MetricsFilter
    MetricsFilter (..),
    newMetricsFilter,
    metricsFilter_prefix,
    metricsFilter_and,
    metricsFilter_tag,

    -- * MultipartUpload
    MultipartUpload (..),
    newMultipartUpload,
    multipartUpload_key,
    multipartUpload_uploadId,
    multipartUpload_storageClass,
    multipartUpload_initiated,
    multipartUpload_owner,
    multipartUpload_initiator,

    -- * NoncurrentVersionExpiration
    NoncurrentVersionExpiration (..),
    newNoncurrentVersionExpiration,
    noncurrentVersionExpiration_noncurrentDays,

    -- * NoncurrentVersionTransition
    NoncurrentVersionTransition (..),
    newNoncurrentVersionTransition,
    noncurrentVersionTransition_noncurrentDays,
    noncurrentVersionTransition_storageClass,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_lambdaFunctionConfigurations,
    notificationConfiguration_queueConfigurations,
    notificationConfiguration_topicConfigurations,

    -- * NotificationConfigurationFilter
    NotificationConfigurationFilter (..),
    newNotificationConfigurationFilter,
    notificationConfigurationFilter_key,

    -- * Object
    Object (..),
    newObject,
    object_owner,
    object_eTag,
    object_size,
    object_key,
    object_storageClass,
    object_lastModified,

    -- * ObjectIdentifier
    ObjectIdentifier (..),
    newObjectIdentifier,
    objectIdentifier_versionId,
    objectIdentifier_key,

    -- * ObjectLockConfiguration
    ObjectLockConfiguration (..),
    newObjectLockConfiguration,
    objectLockConfiguration_rule,
    objectLockConfiguration_objectLockEnabled,

    -- * ObjectLockLegalHold
    ObjectLockLegalHold (..),
    newObjectLockLegalHold,
    objectLockLegalHold_status,

    -- * ObjectLockRetention
    ObjectLockRetention (..),
    newObjectLockRetention,
    objectLockRetention_mode,
    objectLockRetention_retainUntilDate,

    -- * ObjectLockRule
    ObjectLockRule (..),
    newObjectLockRule,
    objectLockRule_defaultRetention,

    -- * ObjectVersion
    ObjectVersion (..),
    newObjectVersion,
    objectVersion_eTag,
    objectVersion_key,
    objectVersion_isLatest,
    objectVersion_storageClass,
    objectVersion_versionId,
    objectVersion_owner,
    objectVersion_lastModified,
    objectVersion_size,

    -- * OutputLocation
    OutputLocation (..),
    newOutputLocation,
    outputLocation_s3,

    -- * OutputSerialization
    OutputSerialization (..),
    newOutputSerialization,
    outputSerialization_csv,
    outputSerialization_json,

    -- * Owner
    Owner (..),
    newOwner,
    owner_id,
    owner_displayName,

    -- * OwnershipControls
    OwnershipControls (..),
    newOwnershipControls,
    ownershipControls_rules,

    -- * OwnershipControlsRule
    OwnershipControlsRule (..),
    newOwnershipControlsRule,
    ownershipControlsRule_objectOwnership,

    -- * ParquetInput
    ParquetInput (..),
    newParquetInput,

    -- * Part
    Part (..),
    newPart,
    part_eTag,
    part_partNumber,
    part_lastModified,
    part_size,

    -- * PolicyStatus
    PolicyStatus (..),
    newPolicyStatus,
    policyStatus_isPublic,

    -- * Progress
    Progress (..),
    newProgress,
    progress_bytesScanned,
    progress_bytesProcessed,
    progress_bytesReturned,

    -- * ProgressEvent
    ProgressEvent (..),
    newProgressEvent,
    progressEvent_details,

    -- * PublicAccessBlockConfiguration
    PublicAccessBlockConfiguration (..),
    newPublicAccessBlockConfiguration,
    publicAccessBlockConfiguration_ignorePublicAcls,
    publicAccessBlockConfiguration_blockPublicAcls,
    publicAccessBlockConfiguration_restrictPublicBuckets,
    publicAccessBlockConfiguration_blockPublicPolicy,

    -- * QueueConfiguration
    QueueConfiguration (..),
    newQueueConfiguration,
    queueConfiguration_id,
    queueConfiguration_filter,
    queueConfiguration_queueArn,
    queueConfiguration_events,

    -- * RecordsEvent
    RecordsEvent (..),
    newRecordsEvent,
    recordsEvent_payload,

    -- * Redirect
    Redirect (..),
    newRedirect,
    redirect_hostName,
    redirect_httpRedirectCode,
    redirect_replaceKeyPrefixWith,
    redirect_replaceKeyWith,
    redirect_protocol,

    -- * RedirectAllRequestsTo
    RedirectAllRequestsTo (..),
    newRedirectAllRequestsTo,
    redirectAllRequestsTo_protocol,
    redirectAllRequestsTo_hostName,

    -- * ReplicaModifications
    ReplicaModifications (..),
    newReplicaModifications,
    replicaModifications_status,

    -- * ReplicationConfiguration
    ReplicationConfiguration (..),
    newReplicationConfiguration,
    replicationConfiguration_role,
    replicationConfiguration_rules,

    -- * ReplicationRule
    ReplicationRule (..),
    newReplicationRule,
    replicationRule_prefix,
    replicationRule_id,
    replicationRule_existingObjectReplication,
    replicationRule_priority,
    replicationRule_deleteMarkerReplication,
    replicationRule_sourceSelectionCriteria,
    replicationRule_filter,
    replicationRule_status,
    replicationRule_destination,

    -- * ReplicationRuleAndOperator
    ReplicationRuleAndOperator (..),
    newReplicationRuleAndOperator,
    replicationRuleAndOperator_prefix,
    replicationRuleAndOperator_tags,

    -- * ReplicationRuleFilter
    ReplicationRuleFilter (..),
    newReplicationRuleFilter,
    replicationRuleFilter_prefix,
    replicationRuleFilter_and,
    replicationRuleFilter_tag,

    -- * ReplicationTime
    ReplicationTime (..),
    newReplicationTime,
    replicationTime_status,
    replicationTime_time,

    -- * ReplicationTimeValue
    ReplicationTimeValue (..),
    newReplicationTimeValue,
    replicationTimeValue_minutes,

    -- * RequestPaymentConfiguration
    RequestPaymentConfiguration (..),
    newRequestPaymentConfiguration,
    requestPaymentConfiguration_payer,

    -- * RequestProgress
    RequestProgress (..),
    newRequestProgress,
    requestProgress_enabled,

    -- * RestoreRequest
    RestoreRequest (..),
    newRestoreRequest,
    restoreRequest_days,
    restoreRequest_selectParameters,
    restoreRequest_description,
    restoreRequest_type,
    restoreRequest_outputLocation,
    restoreRequest_tier,
    restoreRequest_glacierJobParameters,

    -- * RoutingRule
    RoutingRule (..),
    newRoutingRule,
    routingRule_condition,
    routingRule_redirect,

    -- * S3KeyFilter
    S3KeyFilter (..),
    newS3KeyFilter,
    s3KeyFilter_filterRules,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_cannedACL,
    s3Location_encryption,
    s3Location_storageClass,
    s3Location_userMetadata,
    s3Location_accessControlList,
    s3Location_tagging,
    s3Location_bucketName,
    s3Location_prefix,

    -- * S3ServiceError
    S3ServiceError (..),
    newS3ServiceError,
    s3ServiceError_key,
    s3ServiceError_message,
    s3ServiceError_code,
    s3ServiceError_versionId,

    -- * SSEKMS
    SSEKMS (..),
    newSSEKMS,
    ssekms_keyId,

    -- * SSES3
    SSES3 (..),
    newSSES3,

    -- * ScanRange
    ScanRange (..),
    newScanRange,
    scanRange_end,
    scanRange_start,

    -- * SelectObjectContentEventStream
    SelectObjectContentEventStream (..),
    newSelectObjectContentEventStream,
    selectObjectContentEventStream_end,
    selectObjectContentEventStream_records,
    selectObjectContentEventStream_stats,
    selectObjectContentEventStream_cont,
    selectObjectContentEventStream_progress,

    -- * SelectParameters
    SelectParameters (..),
    newSelectParameters,
    selectParameters_inputSerialization,
    selectParameters_expressionType,
    selectParameters_expression,
    selectParameters_outputSerialization,

    -- * ServerSideEncryptionByDefault
    ServerSideEncryptionByDefault (..),
    newServerSideEncryptionByDefault,
    serverSideEncryptionByDefault_kmsMasterKeyID,
    serverSideEncryptionByDefault_sSEAlgorithm,

    -- * ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration (..),
    newServerSideEncryptionConfiguration,
    serverSideEncryptionConfiguration_rules,

    -- * ServerSideEncryptionRule
    ServerSideEncryptionRule (..),
    newServerSideEncryptionRule,
    serverSideEncryptionRule_bucketKeyEnabled,
    serverSideEncryptionRule_applyServerSideEncryptionByDefault,

    -- * SourceSelectionCriteria
    SourceSelectionCriteria (..),
    newSourceSelectionCriteria,
    sourceSelectionCriteria_replicaModifications,
    sourceSelectionCriteria_sseKmsEncryptedObjects,

    -- * SseKmsEncryptedObjects
    SseKmsEncryptedObjects (..),
    newSseKmsEncryptedObjects,
    sseKmsEncryptedObjects_status,

    -- * Stats
    Stats (..),
    newStats,
    stats_bytesScanned,
    stats_bytesProcessed,
    stats_bytesReturned,

    -- * StatsEvent
    StatsEvent (..),
    newStatsEvent,
    statsEvent_details,

    -- * StorageClassAnalysis
    StorageClassAnalysis (..),
    newStorageClassAnalysis,
    storageClassAnalysis_dataExport,

    -- * StorageClassAnalysisDataExport
    StorageClassAnalysisDataExport (..),
    newStorageClassAnalysisDataExport,
    storageClassAnalysisDataExport_outputSchemaVersion,
    storageClassAnalysisDataExport_destination,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Tagging
    Tagging (..),
    newTagging,
    tagging_tagSet,

    -- * TargetGrant
    TargetGrant (..),
    newTargetGrant,
    targetGrant_grantee,
    targetGrant_permission,

    -- * Tiering
    Tiering (..),
    newTiering,
    tiering_days,
    tiering_accessTier,

    -- * TopicConfiguration
    TopicConfiguration (..),
    newTopicConfiguration,
    topicConfiguration_id,
    topicConfiguration_filter,
    topicConfiguration_topicArn,
    topicConfiguration_events,

    -- * Transition
    Transition (..),
    newTransition,
    transition_days,
    transition_storageClass,
    transition_date,

    -- * VersioningConfiguration
    VersioningConfiguration (..),
    newVersioningConfiguration,
    versioningConfiguration_status,
    versioningConfiguration_mfaDelete,

    -- * WebsiteConfiguration
    WebsiteConfiguration (..),
    newWebsiteConfiguration,
    websiteConfiguration_errorDocument,
    websiteConfiguration_indexDocument,
    websiteConfiguration_routingRules,
    websiteConfiguration_redirectAllRequestsTo,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
import Network.AWS.S3.Types.ReplicaModifications
import Network.AWS.S3.Types.ReplicaModificationsStatus
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
import Network.AWS.S3.Types.SseKmsEncryptedObjects
import Network.AWS.S3.Types.SseKmsEncryptedObjectsStatus
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
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "S3",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "s3",
      Core._serviceSigningName = "s3",
      Core._serviceVersion = "2006-03-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "S3",
      Core._serviceRetry = retry
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "BadDigest"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "contentmd5"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "RequestTimeout"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "timeouts"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The source object of the COPY operation is not in the active tier and is
-- only stored in Amazon S3 Glacier.
_ObjectNotInActiveTierError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ObjectNotInActiveTierError =
  Core._MatchServiceError
    defaultService
    "ObjectNotInActiveTierError"

-- | The specified key does not exist.
_NoSuchKey :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoSuchKey =
  Core._MatchServiceError defaultService "NoSuchKey"

-- | This operation is not allowed against this storage tier.
_ObjectAlreadyInActiveTierError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ObjectAlreadyInActiveTierError =
  Core._MatchServiceError
    defaultService
    "ObjectAlreadyInActiveTierError"

-- | The bucket you tried to create already exists, and you own it. Amazon S3
-- returns this error in all AWS Regions except in the North Virginia
-- Region. For legacy compatibility, if you re-create an existing bucket
-- that you already own in the North Virginia Region, Amazon S3 returns 200
-- OK and resets the bucket access control lists (ACLs).
_BucketAlreadyOwnedByYou :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BucketAlreadyOwnedByYou =
  Core._MatchServiceError
    defaultService
    "BucketAlreadyOwnedByYou"

-- | The specified multipart upload does not exist.
_NoSuchUpload :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoSuchUpload =
  Core._MatchServiceError
    defaultService
    "NoSuchUpload"

-- | Object is archived and inaccessible until restored.
_InvalidObjectState :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidObjectState =
  Core._MatchServiceError
    defaultService
    "InvalidObjectState"

-- | The requested bucket name is not available. The bucket namespace is
-- shared by all users of the system. Select a different name and try
-- again.
_BucketAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BucketAlreadyExists =
  Core._MatchServiceError
    defaultService
    "BucketAlreadyExists"

-- | The specified bucket does not exist.
_NoSuchBucket :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoSuchBucket =
  Core._MatchServiceError
    defaultService
    "NoSuchBucket"
