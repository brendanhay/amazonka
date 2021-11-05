{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.S3
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2006-03-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Simple Storage Service is storage for the Internet. Amazon S3 has
-- a simple web services interface that you can use to store and retrieve
-- any amount of data, at any time, from anywhere on the web. It gives any
-- developer access to the same highly scalable, reliable, fast,
-- inexpensive data storage infrastructure that Amazon uses to run its own
-- global network of web sites. The service aims to maximize benefits of
-- scale and to pass those benefits on to developers.
module Amazonka.S3
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BucketAlreadyOwnedByYou
    _BucketAlreadyOwnedByYou,

    -- ** ObjectAlreadyInActiveTierError
    _ObjectAlreadyInActiveTierError,

    -- ** BucketAlreadyExists
    _BucketAlreadyExists,

    -- ** ObjectNotInActiveTierError
    _ObjectNotInActiveTierError,

    -- ** NoSuchUpload
    _NoSuchUpload,

    -- ** NoSuchBucket
    _NoSuchBucket,

    -- ** NoSuchKey
    _NoSuchKey,

    -- ** InvalidObjectState
    _InvalidObjectState,

    -- * Waiters
    -- $waiters

    -- ** ObjectNotExists
    newObjectNotExists,

    -- ** BucketExists
    newBucketExists,

    -- ** ObjectExists
    newObjectExists,

    -- ** BucketNotExists
    newBucketNotExists,

    -- * Operations
    -- $operations

    -- ** PutBucketRequestPayment
    PutBucketRequestPayment (PutBucketRequestPayment'),
    newPutBucketRequestPayment,
    PutBucketRequestPaymentResponse (PutBucketRequestPaymentResponse'),
    newPutBucketRequestPaymentResponse,

    -- ** PutObject
    PutObject (PutObject'),
    newPutObject,
    PutObjectResponse (PutObjectResponse'),
    newPutObjectResponse,

    -- ** DeleteObject
    DeleteObject (DeleteObject'),
    newDeleteObject,
    DeleteObjectResponse (DeleteObjectResponse'),
    newDeleteObjectResponse,

    -- ** PutBucketLogging
    PutBucketLogging (PutBucketLogging'),
    newPutBucketLogging,
    PutBucketLoggingResponse (PutBucketLoggingResponse'),
    newPutBucketLoggingResponse,

    -- ** GetBucketMetricsConfiguration
    GetBucketMetricsConfiguration (GetBucketMetricsConfiguration'),
    newGetBucketMetricsConfiguration,
    GetBucketMetricsConfigurationResponse (GetBucketMetricsConfigurationResponse'),
    newGetBucketMetricsConfigurationResponse,

    -- ** ListBuckets
    ListBuckets (ListBuckets'),
    newListBuckets,
    ListBucketsResponse (ListBucketsResponse'),
    newListBucketsResponse,

    -- ** DeleteBucket
    DeleteBucket (DeleteBucket'),
    newDeleteBucket,
    DeleteBucketResponse (DeleteBucketResponse'),
    newDeleteBucketResponse,

    -- ** CreateBucket
    CreateBucket (CreateBucket'),
    newCreateBucket,
    CreateBucketResponse (CreateBucketResponse'),
    newCreateBucketResponse,

    -- ** DeleteBucketTagging
    DeleteBucketTagging (DeleteBucketTagging'),
    newDeleteBucketTagging,
    DeleteBucketTaggingResponse (DeleteBucketTaggingResponse'),
    newDeleteBucketTaggingResponse,

    -- ** PutObjectAcl
    PutObjectAcl (PutObjectAcl'),
    newPutObjectAcl,
    PutObjectAclResponse (PutObjectAclResponse'),
    newPutObjectAclResponse,

    -- ** PutBucketTagging
    PutBucketTagging (PutBucketTagging'),
    newPutBucketTagging,
    PutBucketTaggingResponse (PutBucketTaggingResponse'),
    newPutBucketTaggingResponse,

    -- ** GetBucketInventoryConfiguration
    GetBucketInventoryConfiguration (GetBucketInventoryConfiguration'),
    newGetBucketInventoryConfiguration,
    GetBucketInventoryConfigurationResponse (GetBucketInventoryConfigurationResponse'),
    newGetBucketInventoryConfigurationResponse,

    -- ** DeletePublicAccessBlock
    DeletePublicAccessBlock (DeletePublicAccessBlock'),
    newDeletePublicAccessBlock,
    DeletePublicAccessBlockResponse (DeletePublicAccessBlockResponse'),
    newDeletePublicAccessBlockResponse,

    -- ** PutBucketInventoryConfiguration
    PutBucketInventoryConfiguration (PutBucketInventoryConfiguration'),
    newPutBucketInventoryConfiguration,
    PutBucketInventoryConfigurationResponse (PutBucketInventoryConfigurationResponse'),
    newPutBucketInventoryConfigurationResponse,

    -- ** GetBucketLocation
    GetBucketLocation (GetBucketLocation'),
    newGetBucketLocation,
    GetBucketLocationResponse (GetBucketLocationResponse'),
    newGetBucketLocationResponse,

    -- ** ListBucketInventoryConfigurations
    ListBucketInventoryConfigurations (ListBucketInventoryConfigurations'),
    newListBucketInventoryConfigurations,
    ListBucketInventoryConfigurationsResponse (ListBucketInventoryConfigurationsResponse'),
    newListBucketInventoryConfigurationsResponse,

    -- ** PutPublicAccessBlock
    PutPublicAccessBlock (PutPublicAccessBlock'),
    newPutPublicAccessBlock,
    PutPublicAccessBlockResponse (PutPublicAccessBlockResponse'),
    newPutPublicAccessBlockResponse,

    -- ** DeleteBucketInventoryConfiguration
    DeleteBucketInventoryConfiguration (DeleteBucketInventoryConfiguration'),
    newDeleteBucketInventoryConfiguration,
    DeleteBucketInventoryConfigurationResponse (DeleteBucketInventoryConfigurationResponse'),
    newDeleteBucketInventoryConfigurationResponse,

    -- ** GetBucketIntelligentTieringConfiguration
    GetBucketIntelligentTieringConfiguration (GetBucketIntelligentTieringConfiguration'),
    newGetBucketIntelligentTieringConfiguration,
    GetBucketIntelligentTieringConfigurationResponse (GetBucketIntelligentTieringConfigurationResponse'),
    newGetBucketIntelligentTieringConfigurationResponse,

    -- ** GetBucketNotificationConfiguration
    GetBucketNotificationConfiguration (GetBucketNotificationConfiguration'),
    newGetBucketNotificationConfiguration,
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

    -- ** GetObjectLockConfiguration
    GetObjectLockConfiguration (GetObjectLockConfiguration'),
    newGetObjectLockConfiguration,
    GetObjectLockConfigurationResponse (GetObjectLockConfigurationResponse'),
    newGetObjectLockConfigurationResponse,

    -- ** PutObjectRetention
    PutObjectRetention (PutObjectRetention'),
    newPutObjectRetention,
    PutObjectRetentionResponse (PutObjectRetentionResponse'),
    newPutObjectRetentionResponse,

    -- ** PutBucketAccelerateConfiguration
    PutBucketAccelerateConfiguration (PutBucketAccelerateConfiguration'),
    newPutBucketAccelerateConfiguration,
    PutBucketAccelerateConfigurationResponse (PutBucketAccelerateConfigurationResponse'),
    newPutBucketAccelerateConfigurationResponse,

    -- ** PutObjectLegalHold
    PutObjectLegalHold (PutObjectLegalHold'),
    newPutObjectLegalHold,
    PutObjectLegalHoldResponse (PutObjectLegalHoldResponse'),
    newPutObjectLegalHoldResponse,

    -- ** PutBucketOwnershipControls
    PutBucketOwnershipControls (PutBucketOwnershipControls'),
    newPutBucketOwnershipControls,
    PutBucketOwnershipControlsResponse (PutBucketOwnershipControlsResponse'),
    newPutBucketOwnershipControlsResponse,

    -- ** DeleteBucketOwnershipControls
    DeleteBucketOwnershipControls (DeleteBucketOwnershipControls'),
    newDeleteBucketOwnershipControls,
    DeleteBucketOwnershipControlsResponse (DeleteBucketOwnershipControlsResponse'),
    newDeleteBucketOwnershipControlsResponse,

    -- ** PutBucketMetricsConfiguration
    PutBucketMetricsConfiguration (PutBucketMetricsConfiguration'),
    newPutBucketMetricsConfiguration,
    PutBucketMetricsConfigurationResponse (PutBucketMetricsConfigurationResponse'),
    newPutBucketMetricsConfigurationResponse,

    -- ** DeleteBucketMetricsConfiguration
    DeleteBucketMetricsConfiguration (DeleteBucketMetricsConfiguration'),
    newDeleteBucketMetricsConfiguration,
    DeleteBucketMetricsConfigurationResponse (DeleteBucketMetricsConfigurationResponse'),
    newDeleteBucketMetricsConfigurationResponse,

    -- ** ListObjectsV2 (Paginated)
    ListObjectsV2 (ListObjectsV2'),
    newListObjectsV2,
    ListObjectsV2Response (ListObjectsV2Response'),
    newListObjectsV2Response,

    -- ** GetObject
    GetObject (GetObject'),
    newGetObject,
    GetObjectResponse (GetObjectResponse'),
    newGetObjectResponse,

    -- ** PutBucketReplication
    PutBucketReplication (PutBucketReplication'),
    newPutBucketReplication,
    PutBucketReplicationResponse (PutBucketReplicationResponse'),
    newPutBucketReplicationResponse,

    -- ** GetBucketWebsite
    GetBucketWebsite (GetBucketWebsite'),
    newGetBucketWebsite,
    GetBucketWebsiteResponse (GetBucketWebsiteResponse'),
    newGetBucketWebsiteResponse,

    -- ** GetBucketRequestPayment
    GetBucketRequestPayment (GetBucketRequestPayment'),
    newGetBucketRequestPayment,
    GetBucketRequestPaymentResponse (GetBucketRequestPaymentResponse'),
    newGetBucketRequestPaymentResponse,

    -- ** DeleteBucketReplication
    DeleteBucketReplication (DeleteBucketReplication'),
    newDeleteBucketReplication,
    DeleteBucketReplicationResponse (DeleteBucketReplicationResponse'),
    newDeleteBucketReplicationResponse,

    -- ** ListObjectVersions (Paginated)
    ListObjectVersions (ListObjectVersions'),
    newListObjectVersions,
    ListObjectVersionsResponse (ListObjectVersionsResponse'),
    newListObjectVersionsResponse,

    -- ** HeadBucket
    HeadBucket (HeadBucket'),
    newHeadBucket,
    HeadBucketResponse (HeadBucketResponse'),
    newHeadBucketResponse,

    -- ** DeleteBucketLifecycle
    DeleteBucketLifecycle (DeleteBucketLifecycle'),
    newDeleteBucketLifecycle,
    DeleteBucketLifecycleResponse (DeleteBucketLifecycleResponse'),
    newDeleteBucketLifecycleResponse,

    -- ** PutBucketLifecycleConfiguration
    PutBucketLifecycleConfiguration (PutBucketLifecycleConfiguration'),
    newPutBucketLifecycleConfiguration,
    PutBucketLifecycleConfigurationResponse (PutBucketLifecycleConfigurationResponse'),
    newPutBucketLifecycleConfigurationResponse,

    -- ** PutBucketAnalyticsConfiguration
    PutBucketAnalyticsConfiguration (PutBucketAnalyticsConfiguration'),
    newPutBucketAnalyticsConfiguration,
    PutBucketAnalyticsConfigurationResponse (PutBucketAnalyticsConfigurationResponse'),
    newPutBucketAnalyticsConfigurationResponse,

    -- ** ListBucketAnalyticsConfigurations
    ListBucketAnalyticsConfigurations (ListBucketAnalyticsConfigurations'),
    newListBucketAnalyticsConfigurations,
    ListBucketAnalyticsConfigurationsResponse (ListBucketAnalyticsConfigurationsResponse'),
    newListBucketAnalyticsConfigurationsResponse,

    -- ** DeleteBucketAnalyticsConfiguration
    DeleteBucketAnalyticsConfiguration (DeleteBucketAnalyticsConfiguration'),
    newDeleteBucketAnalyticsConfiguration,
    DeleteBucketAnalyticsConfigurationResponse (DeleteBucketAnalyticsConfigurationResponse'),
    newDeleteBucketAnalyticsConfigurationResponse,

    -- ** CreateMultipartUpload
    CreateMultipartUpload (CreateMultipartUpload'),
    newCreateMultipartUpload,
    CreateMultipartUploadResponse (CreateMultipartUploadResponse'),
    newCreateMultipartUploadResponse,

    -- ** GetBucketPolicyStatus
    GetBucketPolicyStatus (GetBucketPolicyStatus'),
    newGetBucketPolicyStatus,
    GetBucketPolicyStatusResponse (GetBucketPolicyStatusResponse'),
    newGetBucketPolicyStatusResponse,

    -- ** UploadPart
    UploadPart (UploadPart'),
    newUploadPart,
    UploadPartResponse (UploadPartResponse'),
    newUploadPartResponse,

    -- ** SelectObjectContent
    SelectObjectContent (SelectObjectContent'),
    newSelectObjectContent,
    SelectObjectContentResponse (SelectObjectContentResponse'),
    newSelectObjectContentResponse,

    -- ** GetBucketReplication
    GetBucketReplication (GetBucketReplication'),
    newGetBucketReplication,
    GetBucketReplicationResponse (GetBucketReplicationResponse'),
    newGetBucketReplicationResponse,

    -- ** PutBucketWebsite
    PutBucketWebsite (PutBucketWebsite'),
    newPutBucketWebsite,
    PutBucketWebsiteResponse (PutBucketWebsiteResponse'),
    newPutBucketWebsiteResponse,

    -- ** DeleteBucketWebsite
    DeleteBucketWebsite (DeleteBucketWebsite'),
    newDeleteBucketWebsite,
    DeleteBucketWebsiteResponse (DeleteBucketWebsiteResponse'),
    newDeleteBucketWebsiteResponse,

    -- ** CompleteMultipartUpload
    CompleteMultipartUpload (CompleteMultipartUpload'),
    newCompleteMultipartUpload,
    CompleteMultipartUploadResponse (CompleteMultipartUploadResponse'),
    newCompleteMultipartUploadResponse,

    -- ** ListMultipartUploads (Paginated)
    ListMultipartUploads (ListMultipartUploads'),
    newListMultipartUploads,
    ListMultipartUploadsResponse (ListMultipartUploadsResponse'),
    newListMultipartUploadsResponse,

    -- ** ListObjects (Paginated)
    ListObjects (ListObjects'),
    newListObjects,
    ListObjectsResponse (ListObjectsResponse'),
    newListObjectsResponse,

    -- ** GetBucketOwnershipControls
    GetBucketOwnershipControls (GetBucketOwnershipControls'),
    newGetBucketOwnershipControls,
    GetBucketOwnershipControlsResponse (GetBucketOwnershipControlsResponse'),
    newGetBucketOwnershipControlsResponse,

    -- ** GetObjectLegalHold
    GetObjectLegalHold (GetObjectLegalHold'),
    newGetObjectLegalHold,
    GetObjectLegalHoldResponse (GetObjectLegalHoldResponse'),
    newGetObjectLegalHoldResponse,

    -- ** GetObjectRetention
    GetObjectRetention (GetObjectRetention'),
    newGetObjectRetention,
    GetObjectRetentionResponse (GetObjectRetentionResponse'),
    newGetObjectRetentionResponse,

    -- ** DeleteBucketPolicy
    DeleteBucketPolicy (DeleteBucketPolicy'),
    newDeleteBucketPolicy,
    DeleteBucketPolicyResponse (DeleteBucketPolicyResponse'),
    newDeleteBucketPolicyResponse,

    -- ** GetBucketEncryption
    GetBucketEncryption (GetBucketEncryption'),
    newGetBucketEncryption,
    GetBucketEncryptionResponse (GetBucketEncryptionResponse'),
    newGetBucketEncryptionResponse,

    -- ** AbortMultipartUpload
    AbortMultipartUpload (AbortMultipartUpload'),
    newAbortMultipartUpload,
    AbortMultipartUploadResponse (AbortMultipartUploadResponse'),
    newAbortMultipartUploadResponse,

    -- ** PutBucketPolicy
    PutBucketPolicy (PutBucketPolicy'),
    newPutBucketPolicy,
    PutBucketPolicyResponse (PutBucketPolicyResponse'),
    newPutBucketPolicyResponse,

    -- ** GetBucketAccelerateConfiguration
    GetBucketAccelerateConfiguration (GetBucketAccelerateConfiguration'),
    newGetBucketAccelerateConfiguration,
    GetBucketAccelerateConfigurationResponse (GetBucketAccelerateConfigurationResponse'),
    newGetBucketAccelerateConfigurationResponse,

    -- ** GetObjectTorrent
    GetObjectTorrent (GetObjectTorrent'),
    newGetObjectTorrent,
    GetObjectTorrentResponse (GetObjectTorrentResponse'),
    newGetObjectTorrentResponse,

    -- ** DeleteObjects
    DeleteObjects (DeleteObjects'),
    newDeleteObjects,
    DeleteObjectsResponse (DeleteObjectsResponse'),
    newDeleteObjectsResponse,

    -- ** PutObjectLockConfiguration
    PutObjectLockConfiguration (PutObjectLockConfiguration'),
    newPutObjectLockConfiguration,
    PutObjectLockConfigurationResponse (PutObjectLockConfigurationResponse'),
    newPutObjectLockConfigurationResponse,

    -- ** PutBucketNotificationConfiguration
    PutBucketNotificationConfiguration (PutBucketNotificationConfiguration'),
    newPutBucketNotificationConfiguration,
    PutBucketNotificationConfigurationResponse (PutBucketNotificationConfigurationResponse'),
    newPutBucketNotificationConfigurationResponse,

    -- ** GetBucketVersioning
    GetBucketVersioning (GetBucketVersioning'),
    newGetBucketVersioning,
    GetBucketVersioningResponse (GetBucketVersioningResponse'),
    newGetBucketVersioningResponse,

    -- ** DeleteBucketCors
    DeleteBucketCors (DeleteBucketCors'),
    newDeleteBucketCors,
    DeleteBucketCorsResponse (DeleteBucketCorsResponse'),
    newDeleteBucketCorsResponse,

    -- ** DeleteBucketIntelligentTieringConfiguration
    DeleteBucketIntelligentTieringConfiguration (DeleteBucketIntelligentTieringConfiguration'),
    newDeleteBucketIntelligentTieringConfiguration,
    DeleteBucketIntelligentTieringConfigurationResponse (DeleteBucketIntelligentTieringConfigurationResponse'),
    newDeleteBucketIntelligentTieringConfigurationResponse,

    -- ** ListBucketIntelligentTieringConfigurations
    ListBucketIntelligentTieringConfigurations (ListBucketIntelligentTieringConfigurations'),
    newListBucketIntelligentTieringConfigurations,
    ListBucketIntelligentTieringConfigurationsResponse (ListBucketIntelligentTieringConfigurationsResponse'),
    newListBucketIntelligentTieringConfigurationsResponse,

    -- ** PutBucketCors
    PutBucketCors (PutBucketCors'),
    newPutBucketCors,
    PutBucketCorsResponse (PutBucketCorsResponse'),
    newPutBucketCorsResponse,

    -- ** GetPublicAccessBlock
    GetPublicAccessBlock (GetPublicAccessBlock'),
    newGetPublicAccessBlock,
    GetPublicAccessBlockResponse (GetPublicAccessBlockResponse'),
    newGetPublicAccessBlockResponse,

    -- ** PutBucketIntelligentTieringConfiguration
    PutBucketIntelligentTieringConfiguration (PutBucketIntelligentTieringConfiguration'),
    newPutBucketIntelligentTieringConfiguration,
    PutBucketIntelligentTieringConfigurationResponse (PutBucketIntelligentTieringConfigurationResponse'),
    newPutBucketIntelligentTieringConfigurationResponse,

    -- ** GetBucketCors
    GetBucketCors (GetBucketCors'),
    newGetBucketCors,
    GetBucketCorsResponse (GetBucketCorsResponse'),
    newGetBucketCorsResponse,

    -- ** WriteGetObjectResponse
    WriteGetObjectResponse (WriteGetObjectResponse'),
    newWriteGetObjectResponse,
    WriteGetObjectResponseResponse (WriteGetObjectResponseResponse'),
    newWriteGetObjectResponseResponse,

    -- ** GetObjectAcl
    GetObjectAcl (GetObjectAcl'),
    newGetObjectAcl,
    GetObjectAclResponse (GetObjectAclResponse'),
    newGetObjectAclResponse,

    -- ** RestoreObject
    RestoreObject (RestoreObject'),
    newRestoreObject,
    RestoreObjectResponse (RestoreObjectResponse'),
    newRestoreObjectResponse,

    -- ** HeadObject
    HeadObject (HeadObject'),
    newHeadObject,
    HeadObjectResponse (HeadObjectResponse'),
    newHeadObjectResponse,

    -- ** PutBucketVersioning
    PutBucketVersioning (PutBucketVersioning'),
    newPutBucketVersioning,
    PutBucketVersioningResponse (PutBucketVersioningResponse'),
    newPutBucketVersioningResponse,

    -- ** GetBucketTagging
    GetBucketTagging (GetBucketTagging'),
    newGetBucketTagging,
    GetBucketTaggingResponse (GetBucketTaggingResponse'),
    newGetBucketTaggingResponse,

    -- ** CopyObject
    CopyObject (CopyObject'),
    newCopyObject,
    CopyObjectResponse (CopyObjectResponse'),
    newCopyObjectResponse,

    -- ** ListBucketMetricsConfigurations
    ListBucketMetricsConfigurations (ListBucketMetricsConfigurations'),
    newListBucketMetricsConfigurations,
    ListBucketMetricsConfigurationsResponse (ListBucketMetricsConfigurationsResponse'),
    newListBucketMetricsConfigurationsResponse,

    -- ** GetBucketPolicy
    GetBucketPolicy (GetBucketPolicy'),
    newGetBucketPolicy,
    GetBucketPolicyResponse (GetBucketPolicyResponse'),
    newGetBucketPolicyResponse,

    -- ** PutBucketEncryption
    PutBucketEncryption (PutBucketEncryption'),
    newPutBucketEncryption,
    PutBucketEncryptionResponse (PutBucketEncryptionResponse'),
    newPutBucketEncryptionResponse,

    -- ** DeleteBucketEncryption
    DeleteBucketEncryption (DeleteBucketEncryption'),
    newDeleteBucketEncryption,
    DeleteBucketEncryptionResponse (DeleteBucketEncryptionResponse'),
    newDeleteBucketEncryptionResponse,

    -- ** GetBucketLogging
    GetBucketLogging (GetBucketLogging'),
    newGetBucketLogging,
    GetBucketLoggingResponse (GetBucketLoggingResponse'),
    newGetBucketLoggingResponse,

    -- ** GetBucketAcl
    GetBucketAcl (GetBucketAcl'),
    newGetBucketAcl,
    GetBucketAclResponse (GetBucketAclResponse'),
    newGetBucketAclResponse,

    -- ** GetBucketLifecycleConfiguration
    GetBucketLifecycleConfiguration (GetBucketLifecycleConfiguration'),
    newGetBucketLifecycleConfiguration,
    GetBucketLifecycleConfigurationResponse (GetBucketLifecycleConfigurationResponse'),
    newGetBucketLifecycleConfigurationResponse,

    -- ** GetBucketAnalyticsConfiguration
    GetBucketAnalyticsConfiguration (GetBucketAnalyticsConfiguration'),
    newGetBucketAnalyticsConfiguration,
    GetBucketAnalyticsConfigurationResponse (GetBucketAnalyticsConfigurationResponse'),
    newGetBucketAnalyticsConfigurationResponse,

    -- ** GetObjectTagging
    GetObjectTagging (GetObjectTagging'),
    newGetObjectTagging,
    GetObjectTaggingResponse (GetObjectTaggingResponse'),
    newGetObjectTaggingResponse,

    -- ** ListParts (Paginated)
    ListParts (ListParts'),
    newListParts,
    ListPartsResponse (ListPartsResponse'),
    newListPartsResponse,

    -- ** DeleteObjectTagging
    DeleteObjectTagging (DeleteObjectTagging'),
    newDeleteObjectTagging,
    DeleteObjectTaggingResponse (DeleteObjectTaggingResponse'),
    newDeleteObjectTaggingResponse,

    -- ** UploadPartCopy
    UploadPartCopy (UploadPartCopy'),
    newUploadPartCopy,
    UploadPartCopyResponse (UploadPartCopyResponse'),
    newUploadPartCopyResponse,

    -- ** PutObjectTagging
    PutObjectTagging (PutObjectTagging'),
    newPutObjectTagging,
    PutObjectTaggingResponse (PutObjectTaggingResponse'),
    newPutObjectTaggingResponse,

    -- ** PutBucketAcl
    PutBucketAcl (PutBucketAcl'),
    newPutBucketAcl,
    PutBucketAclResponse (PutBucketAclResponse'),
    newPutBucketAclResponse,

    -- * Types

    -- ** Common
    module Amazonka.S3.Internal,

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

    -- ** ReplicaModificationsStatus
    ReplicaModificationsStatus (..),

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

    -- ** SseKmsEncryptedObjectsStatus
    SseKmsEncryptedObjectsStatus (..),

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
    AbortIncompleteMultipartUpload (AbortIncompleteMultipartUpload'),
    newAbortIncompleteMultipartUpload,

    -- ** AccelerateConfiguration
    AccelerateConfiguration (AccelerateConfiguration'),
    newAccelerateConfiguration,

    -- ** AccessControlPolicy
    AccessControlPolicy (AccessControlPolicy'),
    newAccessControlPolicy,

    -- ** AccessControlTranslation
    AccessControlTranslation (AccessControlTranslation'),
    newAccessControlTranslation,

    -- ** AnalyticsAndOperator
    AnalyticsAndOperator (AnalyticsAndOperator'),
    newAnalyticsAndOperator,

    -- ** AnalyticsConfiguration
    AnalyticsConfiguration (AnalyticsConfiguration'),
    newAnalyticsConfiguration,

    -- ** AnalyticsExportDestination
    AnalyticsExportDestination (AnalyticsExportDestination'),
    newAnalyticsExportDestination,

    -- ** AnalyticsFilter
    AnalyticsFilter (AnalyticsFilter'),
    newAnalyticsFilter,

    -- ** AnalyticsS3BucketDestination
    AnalyticsS3BucketDestination (AnalyticsS3BucketDestination'),
    newAnalyticsS3BucketDestination,

    -- ** Bucket
    Bucket (Bucket'),
    newBucket,

    -- ** BucketLifecycleConfiguration
    BucketLifecycleConfiguration (BucketLifecycleConfiguration'),
    newBucketLifecycleConfiguration,

    -- ** BucketLoggingStatus
    BucketLoggingStatus (BucketLoggingStatus'),
    newBucketLoggingStatus,

    -- ** CORSConfiguration
    CORSConfiguration (CORSConfiguration'),
    newCORSConfiguration,

    -- ** CORSRule
    CORSRule (CORSRule'),
    newCORSRule,

    -- ** CSVInput
    CSVInput (CSVInput'),
    newCSVInput,

    -- ** CSVOutput
    CSVOutput (CSVOutput'),
    newCSVOutput,

    -- ** CommonPrefix
    CommonPrefix (CommonPrefix'),
    newCommonPrefix,

    -- ** CompletedMultipartUpload
    CompletedMultipartUpload (CompletedMultipartUpload'),
    newCompletedMultipartUpload,

    -- ** CompletedPart
    CompletedPart (CompletedPart'),
    newCompletedPart,

    -- ** Condition
    Condition (Condition'),
    newCondition,

    -- ** ContinuationEvent
    ContinuationEvent (ContinuationEvent'),
    newContinuationEvent,

    -- ** CopyObjectResult
    CopyObjectResult (CopyObjectResult'),
    newCopyObjectResult,

    -- ** CopyPartResult
    CopyPartResult (CopyPartResult'),
    newCopyPartResult,

    -- ** CreateBucketConfiguration
    CreateBucketConfiguration (CreateBucketConfiguration'),
    newCreateBucketConfiguration,

    -- ** DefaultRetention
    DefaultRetention (DefaultRetention'),
    newDefaultRetention,

    -- ** Delete
    Delete (Delete'),
    newDelete,

    -- ** DeleteMarkerEntry
    DeleteMarkerEntry (DeleteMarkerEntry'),
    newDeleteMarkerEntry,

    -- ** DeleteMarkerReplication
    DeleteMarkerReplication (DeleteMarkerReplication'),
    newDeleteMarkerReplication,

    -- ** DeletedObject
    DeletedObject (DeletedObject'),
    newDeletedObject,

    -- ** Destination
    Destination (Destination'),
    newDestination,

    -- ** Encryption
    Encryption (Encryption'),
    newEncryption,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (EncryptionConfiguration'),
    newEncryptionConfiguration,

    -- ** EndEvent
    EndEvent (EndEvent'),
    newEndEvent,

    -- ** ErrorDocument
    ErrorDocument (ErrorDocument'),
    newErrorDocument,

    -- ** ExistingObjectReplication
    ExistingObjectReplication (ExistingObjectReplication'),
    newExistingObjectReplication,

    -- ** FilterRule
    FilterRule (FilterRule'),
    newFilterRule,

    -- ** GlacierJobParameters
    GlacierJobParameters (GlacierJobParameters'),
    newGlacierJobParameters,

    -- ** Grant
    Grant (Grant'),
    newGrant,

    -- ** Grantee
    Grantee (Grantee'),
    newGrantee,

    -- ** IndexDocument
    IndexDocument (IndexDocument'),
    newIndexDocument,

    -- ** Initiator
    Initiator (Initiator'),
    newInitiator,

    -- ** InputSerialization
    InputSerialization (InputSerialization'),
    newInputSerialization,

    -- ** IntelligentTieringAndOperator
    IntelligentTieringAndOperator (IntelligentTieringAndOperator'),
    newIntelligentTieringAndOperator,

    -- ** IntelligentTieringConfiguration
    IntelligentTieringConfiguration (IntelligentTieringConfiguration'),
    newIntelligentTieringConfiguration,

    -- ** IntelligentTieringFilter
    IntelligentTieringFilter (IntelligentTieringFilter'),
    newIntelligentTieringFilter,

    -- ** InventoryConfiguration
    InventoryConfiguration (InventoryConfiguration'),
    newInventoryConfiguration,

    -- ** InventoryDestination
    InventoryDestination (InventoryDestination'),
    newInventoryDestination,

    -- ** InventoryEncryption
    InventoryEncryption (InventoryEncryption'),
    newInventoryEncryption,

    -- ** InventoryFilter
    InventoryFilter (InventoryFilter'),
    newInventoryFilter,

    -- ** InventoryS3BucketDestination
    InventoryS3BucketDestination (InventoryS3BucketDestination'),
    newInventoryS3BucketDestination,

    -- ** InventorySchedule
    InventorySchedule (InventorySchedule'),
    newInventorySchedule,

    -- ** JSONInput
    JSONInput (JSONInput'),
    newJSONInput,

    -- ** JSONOutput
    JSONOutput (JSONOutput'),
    newJSONOutput,

    -- ** LambdaFunctionConfiguration
    LambdaFunctionConfiguration (LambdaFunctionConfiguration'),
    newLambdaFunctionConfiguration,

    -- ** LifecycleExpiration
    LifecycleExpiration (LifecycleExpiration'),
    newLifecycleExpiration,

    -- ** LifecycleRule
    LifecycleRule (LifecycleRule'),
    newLifecycleRule,

    -- ** LifecycleRuleAndOperator
    LifecycleRuleAndOperator (LifecycleRuleAndOperator'),
    newLifecycleRuleAndOperator,

    -- ** LifecycleRuleFilter
    LifecycleRuleFilter (LifecycleRuleFilter'),
    newLifecycleRuleFilter,

    -- ** LoggingEnabled
    LoggingEnabled (LoggingEnabled'),
    newLoggingEnabled,

    -- ** MetadataEntry
    MetadataEntry (MetadataEntry'),
    newMetadataEntry,

    -- ** Metrics
    Metrics (Metrics'),
    newMetrics,

    -- ** MetricsAndOperator
    MetricsAndOperator (MetricsAndOperator'),
    newMetricsAndOperator,

    -- ** MetricsConfiguration
    MetricsConfiguration (MetricsConfiguration'),
    newMetricsConfiguration,

    -- ** MetricsFilter
    MetricsFilter (MetricsFilter'),
    newMetricsFilter,

    -- ** MultipartUpload
    MultipartUpload (MultipartUpload'),
    newMultipartUpload,

    -- ** NoncurrentVersionExpiration
    NoncurrentVersionExpiration (NoncurrentVersionExpiration'),
    newNoncurrentVersionExpiration,

    -- ** NoncurrentVersionTransition
    NoncurrentVersionTransition (NoncurrentVersionTransition'),
    newNoncurrentVersionTransition,

    -- ** NotificationConfiguration
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

    -- ** NotificationConfigurationFilter
    NotificationConfigurationFilter (NotificationConfigurationFilter'),
    newNotificationConfigurationFilter,

    -- ** Object
    Object (Object'),
    newObject,

    -- ** ObjectIdentifier
    ObjectIdentifier (ObjectIdentifier'),
    newObjectIdentifier,

    -- ** ObjectLockConfiguration
    ObjectLockConfiguration (ObjectLockConfiguration'),
    newObjectLockConfiguration,

    -- ** ObjectLockLegalHold
    ObjectLockLegalHold (ObjectLockLegalHold'),
    newObjectLockLegalHold,

    -- ** ObjectLockRetention
    ObjectLockRetention (ObjectLockRetention'),
    newObjectLockRetention,

    -- ** ObjectLockRule
    ObjectLockRule (ObjectLockRule'),
    newObjectLockRule,

    -- ** ObjectVersion
    ObjectVersion (ObjectVersion'),
    newObjectVersion,

    -- ** OutputLocation
    OutputLocation (OutputLocation'),
    newOutputLocation,

    -- ** OutputSerialization
    OutputSerialization (OutputSerialization'),
    newOutputSerialization,

    -- ** Owner
    Owner (Owner'),
    newOwner,

    -- ** OwnershipControls
    OwnershipControls (OwnershipControls'),
    newOwnershipControls,

    -- ** OwnershipControlsRule
    OwnershipControlsRule (OwnershipControlsRule'),
    newOwnershipControlsRule,

    -- ** ParquetInput
    ParquetInput (ParquetInput'),
    newParquetInput,

    -- ** Part
    Part (Part'),
    newPart,

    -- ** PolicyStatus
    PolicyStatus (PolicyStatus'),
    newPolicyStatus,

    -- ** Progress
    Progress (Progress'),
    newProgress,

    -- ** ProgressEvent
    ProgressEvent (ProgressEvent'),
    newProgressEvent,

    -- ** PublicAccessBlockConfiguration
    PublicAccessBlockConfiguration (PublicAccessBlockConfiguration'),
    newPublicAccessBlockConfiguration,

    -- ** QueueConfiguration
    QueueConfiguration (QueueConfiguration'),
    newQueueConfiguration,

    -- ** RecordsEvent
    RecordsEvent (RecordsEvent'),
    newRecordsEvent,

    -- ** Redirect
    Redirect (Redirect'),
    newRedirect,

    -- ** RedirectAllRequestsTo
    RedirectAllRequestsTo (RedirectAllRequestsTo'),
    newRedirectAllRequestsTo,

    -- ** ReplicaModifications
    ReplicaModifications (ReplicaModifications'),
    newReplicaModifications,

    -- ** ReplicationConfiguration
    ReplicationConfiguration (ReplicationConfiguration'),
    newReplicationConfiguration,

    -- ** ReplicationRule
    ReplicationRule (ReplicationRule'),
    newReplicationRule,

    -- ** ReplicationRuleAndOperator
    ReplicationRuleAndOperator (ReplicationRuleAndOperator'),
    newReplicationRuleAndOperator,

    -- ** ReplicationRuleFilter
    ReplicationRuleFilter (ReplicationRuleFilter'),
    newReplicationRuleFilter,

    -- ** ReplicationTime
    ReplicationTime (ReplicationTime'),
    newReplicationTime,

    -- ** ReplicationTimeValue
    ReplicationTimeValue (ReplicationTimeValue'),
    newReplicationTimeValue,

    -- ** RequestPaymentConfiguration
    RequestPaymentConfiguration (RequestPaymentConfiguration'),
    newRequestPaymentConfiguration,

    -- ** RequestProgress
    RequestProgress (RequestProgress'),
    newRequestProgress,

    -- ** RestoreRequest
    RestoreRequest (RestoreRequest'),
    newRestoreRequest,

    -- ** RoutingRule
    RoutingRule (RoutingRule'),
    newRoutingRule,

    -- ** S3KeyFilter
    S3KeyFilter (S3KeyFilter'),
    newS3KeyFilter,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** S3ServiceError
    S3ServiceError (S3ServiceError'),
    newS3ServiceError,

    -- ** SSEKMS
    SSEKMS (SSEKMS'),
    newSSEKMS,

    -- ** SSES3
    SSES3 (SSES3'),
    newSSES3,

    -- ** ScanRange
    ScanRange (ScanRange'),
    newScanRange,

    -- ** SelectObjectContentEventStream
    SelectObjectContentEventStream (SelectObjectContentEventStream'),
    newSelectObjectContentEventStream,

    -- ** SelectParameters
    SelectParameters (SelectParameters'),
    newSelectParameters,

    -- ** ServerSideEncryptionByDefault
    ServerSideEncryptionByDefault (ServerSideEncryptionByDefault'),
    newServerSideEncryptionByDefault,

    -- ** ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration (ServerSideEncryptionConfiguration'),
    newServerSideEncryptionConfiguration,

    -- ** ServerSideEncryptionRule
    ServerSideEncryptionRule (ServerSideEncryptionRule'),
    newServerSideEncryptionRule,

    -- ** SourceSelectionCriteria
    SourceSelectionCriteria (SourceSelectionCriteria'),
    newSourceSelectionCriteria,

    -- ** SseKmsEncryptedObjects
    SseKmsEncryptedObjects (SseKmsEncryptedObjects'),
    newSseKmsEncryptedObjects,

    -- ** Stats
    Stats (Stats'),
    newStats,

    -- ** StatsEvent
    StatsEvent (StatsEvent'),
    newStatsEvent,

    -- ** StorageClassAnalysis
    StorageClassAnalysis (StorageClassAnalysis'),
    newStorageClassAnalysis,

    -- ** StorageClassAnalysisDataExport
    StorageClassAnalysisDataExport (StorageClassAnalysisDataExport'),
    newStorageClassAnalysisDataExport,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Tagging
    Tagging (Tagging'),
    newTagging,

    -- ** TargetGrant
    TargetGrant (TargetGrant'),
    newTargetGrant,

    -- ** Tiering
    Tiering (Tiering'),
    newTiering,

    -- ** TopicConfiguration
    TopicConfiguration (TopicConfiguration'),
    newTopicConfiguration,

    -- ** Transition
    Transition (Transition'),
    newTransition,

    -- ** VersioningConfiguration
    VersioningConfiguration (VersioningConfiguration'),
    newVersioningConfiguration,

    -- ** WebsiteConfiguration
    WebsiteConfiguration (WebsiteConfiguration'),
    newWebsiteConfiguration,
  )
where

import Amazonka.S3.AbortMultipartUpload
import Amazonka.S3.CompleteMultipartUpload
import Amazonka.S3.CopyObject
import Amazonka.S3.CreateBucket
import Amazonka.S3.CreateMultipartUpload
import Amazonka.S3.DeleteBucket
import Amazonka.S3.DeleteBucketAnalyticsConfiguration
import Amazonka.S3.DeleteBucketCors
import Amazonka.S3.DeleteBucketEncryption
import Amazonka.S3.DeleteBucketIntelligentTieringConfiguration
import Amazonka.S3.DeleteBucketInventoryConfiguration
import Amazonka.S3.DeleteBucketLifecycle
import Amazonka.S3.DeleteBucketMetricsConfiguration
import Amazonka.S3.DeleteBucketOwnershipControls
import Amazonka.S3.DeleteBucketPolicy
import Amazonka.S3.DeleteBucketReplication
import Amazonka.S3.DeleteBucketTagging
import Amazonka.S3.DeleteBucketWebsite
import Amazonka.S3.DeleteObject
import Amazonka.S3.DeleteObjectTagging
import Amazonka.S3.DeleteObjects
import Amazonka.S3.DeletePublicAccessBlock
import Amazonka.S3.GetBucketAccelerateConfiguration
import Amazonka.S3.GetBucketAcl
import Amazonka.S3.GetBucketAnalyticsConfiguration
import Amazonka.S3.GetBucketCors
import Amazonka.S3.GetBucketEncryption
import Amazonka.S3.GetBucketIntelligentTieringConfiguration
import Amazonka.S3.GetBucketInventoryConfiguration
import Amazonka.S3.GetBucketLifecycleConfiguration
import Amazonka.S3.GetBucketLocation
import Amazonka.S3.GetBucketLogging
import Amazonka.S3.GetBucketMetricsConfiguration
import Amazonka.S3.GetBucketNotificationConfiguration
import Amazonka.S3.GetBucketOwnershipControls
import Amazonka.S3.GetBucketPolicy
import Amazonka.S3.GetBucketPolicyStatus
import Amazonka.S3.GetBucketReplication
import Amazonka.S3.GetBucketRequestPayment
import Amazonka.S3.GetBucketTagging
import Amazonka.S3.GetBucketVersioning
import Amazonka.S3.GetBucketWebsite
import Amazonka.S3.GetObject
import Amazonka.S3.GetObjectAcl
import Amazonka.S3.GetObjectLegalHold
import Amazonka.S3.GetObjectLockConfiguration
import Amazonka.S3.GetObjectRetention
import Amazonka.S3.GetObjectTagging
import Amazonka.S3.GetObjectTorrent
import Amazonka.S3.GetPublicAccessBlock
import Amazonka.S3.HeadBucket
import Amazonka.S3.HeadObject
import Amazonka.S3.Internal
import Amazonka.S3.Lens
import Amazonka.S3.ListBucketAnalyticsConfigurations
import Amazonka.S3.ListBucketIntelligentTieringConfigurations
import Amazonka.S3.ListBucketInventoryConfigurations
import Amazonka.S3.ListBucketMetricsConfigurations
import Amazonka.S3.ListBuckets
import Amazonka.S3.ListMultipartUploads
import Amazonka.S3.ListObjectVersions
import Amazonka.S3.ListObjects
import Amazonka.S3.ListObjectsV2
import Amazonka.S3.ListParts
import Amazonka.S3.PutBucketAccelerateConfiguration
import Amazonka.S3.PutBucketAcl
import Amazonka.S3.PutBucketAnalyticsConfiguration
import Amazonka.S3.PutBucketCors
import Amazonka.S3.PutBucketEncryption
import Amazonka.S3.PutBucketIntelligentTieringConfiguration
import Amazonka.S3.PutBucketInventoryConfiguration
import Amazonka.S3.PutBucketLifecycleConfiguration
import Amazonka.S3.PutBucketLogging
import Amazonka.S3.PutBucketMetricsConfiguration
import Amazonka.S3.PutBucketNotificationConfiguration
import Amazonka.S3.PutBucketOwnershipControls
import Amazonka.S3.PutBucketPolicy
import Amazonka.S3.PutBucketReplication
import Amazonka.S3.PutBucketRequestPayment
import Amazonka.S3.PutBucketTagging
import Amazonka.S3.PutBucketVersioning
import Amazonka.S3.PutBucketWebsite
import Amazonka.S3.PutObject
import Amazonka.S3.PutObjectAcl
import Amazonka.S3.PutObjectLegalHold
import Amazonka.S3.PutObjectLockConfiguration
import Amazonka.S3.PutObjectRetention
import Amazonka.S3.PutObjectTagging
import Amazonka.S3.PutPublicAccessBlock
import Amazonka.S3.RestoreObject
import Amazonka.S3.SelectObjectContent
import Amazonka.S3.Types
import Amazonka.S3.UploadPart
import Amazonka.S3.UploadPartCopy
import Amazonka.S3.Waiters
import Amazonka.S3.WriteGetObjectResponse

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
