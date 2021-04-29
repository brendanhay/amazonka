{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Simple Storage Service is storage for the Internet. Amazon S3 has
-- a simple web services interface that you can use to store and retrieve
-- any amount of data, at any time, from anywhere on the web. It gives any
-- developer access to the same highly scalable, reliable, fast,
-- inexpensive data storage infrastructure that Amazon uses to run its own
-- global network of web sites. The service aims to maximize benefits of
-- scale and to pass those benefits on to developers.
module Network.AWS.S3
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ObjectNotInActiveTierError
    _ObjectNotInActiveTierError,

    -- ** NoSuchKey
    _NoSuchKey,

    -- ** ObjectAlreadyInActiveTierError
    _ObjectAlreadyInActiveTierError,

    -- ** BucketAlreadyOwnedByYou
    _BucketAlreadyOwnedByYou,

    -- ** NoSuchUpload
    _NoSuchUpload,

    -- ** InvalidObjectState
    _InvalidObjectState,

    -- ** BucketAlreadyExists
    _BucketAlreadyExists,

    -- ** NoSuchBucket
    _NoSuchBucket,

    -- * Waiters
    -- $waiters

    -- ** BucketNotExists
    newBucketNotExists,

    -- ** ObjectExists
    newObjectExists,

    -- ** BucketExists
    newBucketExists,

    -- ** ObjectNotExists
    newObjectNotExists,

    -- * Operations
    -- $operations

    -- ** PutBucketPolicy
    PutBucketPolicy (PutBucketPolicy'),
    newPutBucketPolicy,
    PutBucketPolicyResponse (PutBucketPolicyResponse'),
    newPutBucketPolicyResponse,

    -- ** GetBucketEncryption
    GetBucketEncryption (GetBucketEncryption'),
    newGetBucketEncryption,
    GetBucketEncryptionResponse (GetBucketEncryptionResponse'),
    newGetBucketEncryptionResponse,

    -- ** DeleteBucket
    DeleteBucket (DeleteBucket'),
    newDeleteBucket,
    DeleteBucketResponse (DeleteBucketResponse'),
    newDeleteBucketResponse,

    -- ** DeleteObjects
    DeleteObjects (DeleteObjects'),
    newDeleteObjects,
    DeleteObjectsResponse (DeleteObjectsResponse'),
    newDeleteObjectsResponse,

    -- ** PutBucketLogging
    PutBucketLogging (PutBucketLogging'),
    newPutBucketLogging,
    PutBucketLoggingResponse (PutBucketLoggingResponse'),
    newPutBucketLoggingResponse,

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

    -- ** SelectObjectContent
    SelectObjectContent (SelectObjectContent'),
    newSelectObjectContent,
    SelectObjectContentResponse (SelectObjectContentResponse'),
    newSelectObjectContentResponse,

    -- ** GetBucketPolicyStatus
    GetBucketPolicyStatus (GetBucketPolicyStatus'),
    newGetBucketPolicyStatus,
    GetBucketPolicyStatusResponse (GetBucketPolicyStatusResponse'),
    newGetBucketPolicyStatusResponse,

    -- ** ListObjects (Paginated)
    ListObjects (ListObjects'),
    newListObjects,
    ListObjectsResponse (ListObjectsResponse'),
    newListObjectsResponse,

    -- ** DeleteObject
    DeleteObject (DeleteObject'),
    newDeleteObject,
    DeleteObjectResponse (DeleteObjectResponse'),
    newDeleteObjectResponse,

    -- ** DeleteBucketAnalyticsConfiguration
    DeleteBucketAnalyticsConfiguration (DeleteBucketAnalyticsConfiguration'),
    newDeleteBucketAnalyticsConfiguration,
    DeleteBucketAnalyticsConfigurationResponse (DeleteBucketAnalyticsConfigurationResponse'),
    newDeleteBucketAnalyticsConfigurationResponse,

    -- ** DeleteObjectTagging
    DeleteObjectTagging (DeleteObjectTagging'),
    newDeleteObjectTagging,
    DeleteObjectTaggingResponse (DeleteObjectTaggingResponse'),
    newDeleteObjectTaggingResponse,

    -- ** GetBucketAcl
    GetBucketAcl (GetBucketAcl'),
    newGetBucketAcl,
    GetBucketAclResponse (GetBucketAclResponse'),
    newGetBucketAclResponse,

    -- ** GetObjectTagging
    GetObjectTagging (GetObjectTagging'),
    newGetObjectTagging,
    GetObjectTaggingResponse (GetObjectTaggingResponse'),
    newGetObjectTaggingResponse,

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

    -- ** GetObjectLockConfiguration
    GetObjectLockConfiguration (GetObjectLockConfiguration'),
    newGetObjectLockConfiguration,
    GetObjectLockConfigurationResponse (GetObjectLockConfigurationResponse'),
    newGetObjectLockConfigurationResponse,

    -- ** DeleteBucketMetricsConfiguration
    DeleteBucketMetricsConfiguration (DeleteBucketMetricsConfiguration'),
    newDeleteBucketMetricsConfiguration,
    DeleteBucketMetricsConfigurationResponse (DeleteBucketMetricsConfigurationResponse'),
    newDeleteBucketMetricsConfigurationResponse,

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

    -- ** ListBucketMetricsConfigurations
    ListBucketMetricsConfigurations (ListBucketMetricsConfigurations'),
    newListBucketMetricsConfigurations,
    ListBucketMetricsConfigurationsResponse (ListBucketMetricsConfigurationsResponse'),
    newListBucketMetricsConfigurationsResponse,

    -- ** PutBucketAccelerateConfiguration
    PutBucketAccelerateConfiguration (PutBucketAccelerateConfiguration'),
    newPutBucketAccelerateConfiguration,
    PutBucketAccelerateConfigurationResponse (PutBucketAccelerateConfigurationResponse'),
    newPutBucketAccelerateConfigurationResponse,

    -- ** PutBucketOwnershipControls
    PutBucketOwnershipControls (PutBucketOwnershipControls'),
    newPutBucketOwnershipControls,
    PutBucketOwnershipControlsResponse (PutBucketOwnershipControlsResponse'),
    newPutBucketOwnershipControlsResponse,

    -- ** PutObjectRetention
    PutObjectRetention (PutObjectRetention'),
    newPutObjectRetention,
    PutObjectRetentionResponse (PutObjectRetentionResponse'),
    newPutObjectRetentionResponse,

    -- ** PutObjectLegalHold
    PutObjectLegalHold (PutObjectLegalHold'),
    newPutObjectLegalHold,
    PutObjectLegalHoldResponse (PutObjectLegalHoldResponse'),
    newPutObjectLegalHoldResponse,

    -- ** HeadObject
    HeadObject (HeadObject'),
    newHeadObject,
    HeadObjectResponse (HeadObjectResponse'),
    newHeadObjectResponse,

    -- ** GetBucketTagging
    GetBucketTagging (GetBucketTagging'),
    newGetBucketTagging,
    GetBucketTaggingResponse (GetBucketTaggingResponse'),
    newGetBucketTaggingResponse,

    -- ** GetBucketLocation
    GetBucketLocation (GetBucketLocation'),
    newGetBucketLocation,
    GetBucketLocationResponse (GetBucketLocationResponse'),
    newGetBucketLocationResponse,

    -- ** PutBucketInventoryConfiguration
    PutBucketInventoryConfiguration (PutBucketInventoryConfiguration'),
    newPutBucketInventoryConfiguration,
    PutBucketInventoryConfigurationResponse (PutBucketInventoryConfigurationResponse'),
    newPutBucketInventoryConfigurationResponse,

    -- ** ListBucketInventoryConfigurations
    ListBucketInventoryConfigurations (ListBucketInventoryConfigurations'),
    newListBucketInventoryConfigurations,
    ListBucketInventoryConfigurationsResponse (ListBucketInventoryConfigurationsResponse'),
    newListBucketInventoryConfigurationsResponse,

    -- ** GetObjectAcl
    GetObjectAcl (GetObjectAcl'),
    newGetObjectAcl,
    GetObjectAclResponse (GetObjectAclResponse'),
    newGetObjectAclResponse,

    -- ** DeletePublicAccessBlock
    DeletePublicAccessBlock (DeletePublicAccessBlock'),
    newDeletePublicAccessBlock,
    DeletePublicAccessBlockResponse (DeletePublicAccessBlockResponse'),
    newDeletePublicAccessBlockResponse,

    -- ** DeleteBucketIntelligentTieringConfiguration
    DeleteBucketIntelligentTieringConfiguration (DeleteBucketIntelligentTieringConfiguration'),
    newDeleteBucketIntelligentTieringConfiguration,
    DeleteBucketIntelligentTieringConfigurationResponse (DeleteBucketIntelligentTieringConfigurationResponse'),
    newDeleteBucketIntelligentTieringConfigurationResponse,

    -- ** GetBucketVersioning
    GetBucketVersioning (GetBucketVersioning'),
    newGetBucketVersioning,
    GetBucketVersioningResponse (GetBucketVersioningResponse'),
    newGetBucketVersioningResponse,

    -- ** PutBucketTagging
    PutBucketTagging (PutBucketTagging'),
    newPutBucketTagging,
    PutBucketTaggingResponse (PutBucketTaggingResponse'),
    newPutBucketTaggingResponse,

    -- ** CreateBucket
    CreateBucket (CreateBucket'),
    newCreateBucket,
    CreateBucketResponse (CreateBucketResponse'),
    newCreateBucketResponse,

    -- ** PutObjectAcl
    PutObjectAcl (PutObjectAcl'),
    newPutObjectAcl,
    PutObjectAclResponse (PutObjectAclResponse'),
    newPutObjectAclResponse,

    -- ** PutBucketCors
    PutBucketCors (PutBucketCors'),
    newPutBucketCors,
    PutBucketCorsResponse (PutBucketCorsResponse'),
    newPutBucketCorsResponse,

    -- ** GetObjectRetention
    GetObjectRetention (GetObjectRetention'),
    newGetObjectRetention,
    GetObjectRetentionResponse (GetObjectRetentionResponse'),
    newGetObjectRetentionResponse,

    -- ** GetObjectTorrent
    GetObjectTorrent (GetObjectTorrent'),
    newGetObjectTorrent,
    GetObjectTorrentResponse (GetObjectTorrentResponse'),
    newGetObjectTorrentResponse,

    -- ** GetBucketOwnershipControls
    GetBucketOwnershipControls (GetBucketOwnershipControls'),
    newGetBucketOwnershipControls,
    GetBucketOwnershipControlsResponse (GetBucketOwnershipControlsResponse'),
    newGetBucketOwnershipControlsResponse,

    -- ** GetBucketMetricsConfiguration
    GetBucketMetricsConfiguration (GetBucketMetricsConfiguration'),
    newGetBucketMetricsConfiguration,
    GetBucketMetricsConfigurationResponse (GetBucketMetricsConfigurationResponse'),
    newGetBucketMetricsConfigurationResponse,

    -- ** GetBucketAccelerateConfiguration
    GetBucketAccelerateConfiguration (GetBucketAccelerateConfiguration'),
    newGetBucketAccelerateConfiguration,
    GetBucketAccelerateConfigurationResponse (GetBucketAccelerateConfigurationResponse'),
    newGetBucketAccelerateConfigurationResponse,

    -- ** GetObjectLegalHold
    GetObjectLegalHold (GetObjectLegalHold'),
    newGetObjectLegalHold,
    GetObjectLegalHoldResponse (GetObjectLegalHoldResponse'),
    newGetObjectLegalHoldResponse,

    -- ** ListBuckets
    ListBuckets (ListBuckets'),
    newListBuckets,
    ListBucketsResponse (ListBucketsResponse'),
    newListBucketsResponse,

    -- ** DeleteBucketPolicy
    DeleteBucketPolicy (DeleteBucketPolicy'),
    newDeleteBucketPolicy,
    DeleteBucketPolicyResponse (DeleteBucketPolicyResponse'),
    newDeleteBucketPolicyResponse,

    -- ** PutObjectLockConfiguration
    PutObjectLockConfiguration (PutObjectLockConfiguration'),
    newPutObjectLockConfiguration,
    PutObjectLockConfigurationResponse (PutObjectLockConfigurationResponse'),
    newPutObjectLockConfigurationResponse,

    -- ** AbortMultipartUpload
    AbortMultipartUpload (AbortMultipartUpload'),
    newAbortMultipartUpload,
    AbortMultipartUploadResponse (AbortMultipartUploadResponse'),
    newAbortMultipartUploadResponse,

    -- ** UploadPart
    UploadPart (UploadPart'),
    newUploadPart,
    UploadPartResponse (UploadPartResponse'),
    newUploadPartResponse,

    -- ** PutObject
    PutObject (PutObject'),
    newPutObject,
    PutObjectResponse (PutObjectResponse'),
    newPutObjectResponse,

    -- ** PutBucketRequestPayment
    PutBucketRequestPayment (PutBucketRequestPayment'),
    newPutBucketRequestPayment,
    PutBucketRequestPaymentResponse (PutBucketRequestPaymentResponse'),
    newPutBucketRequestPaymentResponse,

    -- ** ListMultipartUploads (Paginated)
    ListMultipartUploads (ListMultipartUploads'),
    newListMultipartUploads,
    ListMultipartUploadsResponse (ListMultipartUploadsResponse'),
    newListMultipartUploadsResponse,

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

    -- ** PutBucketAnalyticsConfiguration
    PutBucketAnalyticsConfiguration (PutBucketAnalyticsConfiguration'),
    newPutBucketAnalyticsConfiguration,
    PutBucketAnalyticsConfigurationResponse (PutBucketAnalyticsConfigurationResponse'),
    newPutBucketAnalyticsConfigurationResponse,

    -- ** PutObjectTagging
    PutObjectTagging (PutObjectTagging'),
    newPutObjectTagging,
    PutObjectTaggingResponse (PutObjectTaggingResponse'),
    newPutObjectTaggingResponse,

    -- ** UploadPartCopy
    UploadPartCopy (UploadPartCopy'),
    newUploadPartCopy,
    UploadPartCopyResponse (UploadPartCopyResponse'),
    newUploadPartCopyResponse,

    -- ** CreateMultipartUpload
    CreateMultipartUpload (CreateMultipartUpload'),
    newCreateMultipartUpload,
    CreateMultipartUploadResponse (CreateMultipartUploadResponse'),
    newCreateMultipartUploadResponse,

    -- ** PutBucketLifecycleConfiguration
    PutBucketLifecycleConfiguration (PutBucketLifecycleConfiguration'),
    newPutBucketLifecycleConfiguration,
    PutBucketLifecycleConfigurationResponse (PutBucketLifecycleConfigurationResponse'),
    newPutBucketLifecycleConfigurationResponse,

    -- ** PutBucketAcl
    PutBucketAcl (PutBucketAcl'),
    newPutBucketAcl,
    PutBucketAclResponse (PutBucketAclResponse'),
    newPutBucketAclResponse,

    -- ** DeleteBucketLifecycle
    DeleteBucketLifecycle (DeleteBucketLifecycle'),
    newDeleteBucketLifecycle,
    DeleteBucketLifecycleResponse (DeleteBucketLifecycleResponse'),
    newDeleteBucketLifecycleResponse,

    -- ** ListBucketAnalyticsConfigurations
    ListBucketAnalyticsConfigurations (ListBucketAnalyticsConfigurations'),
    newListBucketAnalyticsConfigurations,
    ListBucketAnalyticsConfigurationsResponse (ListBucketAnalyticsConfigurationsResponse'),
    newListBucketAnalyticsConfigurationsResponse,

    -- ** GetBucketAnalyticsConfiguration
    GetBucketAnalyticsConfiguration (GetBucketAnalyticsConfiguration'),
    newGetBucketAnalyticsConfiguration,
    GetBucketAnalyticsConfigurationResponse (GetBucketAnalyticsConfigurationResponse'),
    newGetBucketAnalyticsConfigurationResponse,

    -- ** HeadBucket
    HeadBucket (HeadBucket'),
    newHeadBucket,
    HeadBucketResponse (HeadBucketResponse'),
    newHeadBucketResponse,

    -- ** ListObjectVersions (Paginated)
    ListObjectVersions (ListObjectVersions'),
    newListObjectVersions,
    ListObjectVersionsResponse (ListObjectVersionsResponse'),
    newListObjectVersionsResponse,

    -- ** GetBucketLifecycleConfiguration
    GetBucketLifecycleConfiguration (GetBucketLifecycleConfiguration'),
    newGetBucketLifecycleConfiguration,
    GetBucketLifecycleConfigurationResponse (GetBucketLifecycleConfigurationResponse'),
    newGetBucketLifecycleConfigurationResponse,

    -- ** ListParts (Paginated)
    ListParts (ListParts'),
    newListParts,
    ListPartsResponse (ListPartsResponse'),
    newListPartsResponse,

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

    -- ** GetBucketLogging
    GetBucketLogging (GetBucketLogging'),
    newGetBucketLogging,
    GetBucketLoggingResponse (GetBucketLoggingResponse'),
    newGetBucketLoggingResponse,

    -- ** GetObject
    GetObject (GetObject'),
    newGetObject,
    GetObjectResponse (GetObjectResponse'),
    newGetObjectResponse,

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

    -- ** ListObjectsV
    ListObjectsV (ListObjectsV'),
    newListObjectsV,
    ListObjectsVResponse (ListObjectsVResponse'),
    newListObjectsVResponse,

    -- ** CopyObject
    CopyObject (CopyObject'),
    newCopyObject,
    CopyObjectResponse (CopyObjectResponse'),
    newCopyObjectResponse,

    -- ** DeleteBucketEncryption
    DeleteBucketEncryption (DeleteBucketEncryption'),
    newDeleteBucketEncryption,
    DeleteBucketEncryptionResponse (DeleteBucketEncryptionResponse'),
    newDeleteBucketEncryptionResponse,

    -- ** PutBucketVersioning
    PutBucketVersioning (PutBucketVersioning'),
    newPutBucketVersioning,
    PutBucketVersioningResponse (PutBucketVersioningResponse'),
    newPutBucketVersioningResponse,

    -- ** GetBucketNotificationConfiguration
    GetBucketNotificationConfiguration (GetBucketNotificationConfiguration'),
    newGetBucketNotificationConfiguration,
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

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

    -- ** RestoreObject
    RestoreObject (RestoreObject'),
    newRestoreObject,
    RestoreObjectResponse (RestoreObjectResponse'),
    newRestoreObjectResponse,

    -- ** GetBucketCors
    GetBucketCors (GetBucketCors'),
    newGetBucketCors,
    GetBucketCorsResponse (GetBucketCorsResponse'),
    newGetBucketCorsResponse,

    -- ** GetBucketInventoryConfiguration
    GetBucketInventoryConfiguration (GetBucketInventoryConfiguration'),
    newGetBucketInventoryConfiguration,
    GetBucketInventoryConfigurationResponse (GetBucketInventoryConfigurationResponse'),
    newGetBucketInventoryConfigurationResponse,

    -- ** GetPublicAccessBlock
    GetPublicAccessBlock (GetPublicAccessBlock'),
    newGetPublicAccessBlock,
    GetPublicAccessBlockResponse (GetPublicAccessBlockResponse'),
    newGetPublicAccessBlockResponse,

    -- ** DeleteBucketCors
    DeleteBucketCors (DeleteBucketCors'),
    newDeleteBucketCors,
    DeleteBucketCorsResponse (DeleteBucketCorsResponse'),
    newDeleteBucketCorsResponse,

    -- ** DeleteBucketTagging
    DeleteBucketTagging (DeleteBucketTagging'),
    newDeleteBucketTagging,
    DeleteBucketTaggingResponse (DeleteBucketTaggingResponse'),
    newDeleteBucketTaggingResponse,

    -- ** ListBucketIntelligentTieringConfigurations
    ListBucketIntelligentTieringConfigurations (ListBucketIntelligentTieringConfigurations'),
    newListBucketIntelligentTieringConfigurations,
    ListBucketIntelligentTieringConfigurationsResponse (ListBucketIntelligentTieringConfigurationsResponse'),
    newListBucketIntelligentTieringConfigurationsResponse,

    -- ** PutBucketNotificationConfiguration
    PutBucketNotificationConfiguration (PutBucketNotificationConfiguration'),
    newPutBucketNotificationConfiguration,
    PutBucketNotificationConfigurationResponse (PutBucketNotificationConfigurationResponse'),
    newPutBucketNotificationConfigurationResponse,

    -- ** PutBucketIntelligentTieringConfiguration
    PutBucketIntelligentTieringConfiguration (PutBucketIntelligentTieringConfiguration'),
    newPutBucketIntelligentTieringConfiguration,
    PutBucketIntelligentTieringConfigurationResponse (PutBucketIntelligentTieringConfigurationResponse'),
    newPutBucketIntelligentTieringConfigurationResponse,

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

import Network.AWS.S3.AbortMultipartUpload
import Network.AWS.S3.CompleteMultipartUpload
import Network.AWS.S3.CopyObject
import Network.AWS.S3.CreateBucket
import Network.AWS.S3.CreateMultipartUpload
import Network.AWS.S3.DeleteBucket
import Network.AWS.S3.DeleteBucketAnalyticsConfiguration
import Network.AWS.S3.DeleteBucketCors
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
import Network.AWS.S3.GetBucketAccelerateConfiguration
import Network.AWS.S3.GetBucketAcl
import Network.AWS.S3.GetBucketAnalyticsConfiguration
import Network.AWS.S3.GetBucketCors
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
import Network.AWS.S3.GetObjectAcl
import Network.AWS.S3.GetObjectLegalHold
import Network.AWS.S3.GetObjectLockConfiguration
import Network.AWS.S3.GetObjectRetention
import Network.AWS.S3.GetObjectTagging
import Network.AWS.S3.GetObjectTorrent
import Network.AWS.S3.GetPublicAccessBlock
import Network.AWS.S3.HeadBucket
import Network.AWS.S3.HeadObject
import Network.AWS.S3.Internal
import Network.AWS.S3.Lens
import Network.AWS.S3.ListBucketAnalyticsConfigurations
import Network.AWS.S3.ListBucketIntelligentTieringConfigurations
import Network.AWS.S3.ListBucketInventoryConfigurations
import Network.AWS.S3.ListBucketMetricsConfigurations
import Network.AWS.S3.ListBuckets
import Network.AWS.S3.ListMultipartUploads
import Network.AWS.S3.ListObjectVersions
import Network.AWS.S3.ListObjects
import Network.AWS.S3.ListObjectsV
import Network.AWS.S3.ListParts
import Network.AWS.S3.PutBucketAccelerateConfiguration
import Network.AWS.S3.PutBucketAcl
import Network.AWS.S3.PutBucketAnalyticsConfiguration
import Network.AWS.S3.PutBucketCors
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
import Network.AWS.S3.PutObjectAcl
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
