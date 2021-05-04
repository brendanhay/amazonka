{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Lens
  ( -- * Operations

    -- ** PutBucketPolicy
    putBucketPolicy_expectedBucketOwner,
    putBucketPolicy_contentMD5,
    putBucketPolicy_confirmRemoveSelfBucketAccess,
    putBucketPolicy_bucket,
    putBucketPolicy_policy,

    -- ** GetBucketEncryption
    getBucketEncryption_expectedBucketOwner,
    getBucketEncryption_bucket,
    getBucketEncryptionResponse_serverSideEncryptionConfiguration,
    getBucketEncryptionResponse_httpStatus,

    -- ** DeleteBucket
    deleteBucket_expectedBucketOwner,
    deleteBucket_bucket,

    -- ** DeleteObjects
    deleteObjects_expectedBucketOwner,
    deleteObjects_bypassGovernanceRetention,
    deleteObjects_requestPayer,
    deleteObjects_mfa,
    deleteObjects_bucket,
    deleteObjects_delete,
    deleteObjectsResponse_requestCharged,
    deleteObjectsResponse_errors,
    deleteObjectsResponse_deleted,
    deleteObjectsResponse_httpStatus,

    -- ** PutBucketLogging
    putBucketLogging_expectedBucketOwner,
    putBucketLogging_contentMD5,
    putBucketLogging_bucket,
    putBucketLogging_bucketLoggingStatus,

    -- ** DeleteBucketWebsite
    deleteBucketWebsite_expectedBucketOwner,
    deleteBucketWebsite_bucket,

    -- ** CompleteMultipartUpload
    completeMultipartUpload_expectedBucketOwner,
    completeMultipartUpload_requestPayer,
    completeMultipartUpload_multipartUpload,
    completeMultipartUpload_bucket,
    completeMultipartUpload_key,
    completeMultipartUpload_uploadId,
    completeMultipartUploadResponse_eTag,
    completeMultipartUploadResponse_requestCharged,
    completeMultipartUploadResponse_key,
    completeMultipartUploadResponse_expiration,
    completeMultipartUploadResponse_sSEKMSKeyId,
    completeMultipartUploadResponse_versionId,
    completeMultipartUploadResponse_bucketKeyEnabled,
    completeMultipartUploadResponse_serverSideEncryption,
    completeMultipartUploadResponse_bucket,
    completeMultipartUploadResponse_location,
    completeMultipartUploadResponse_httpStatus,

    -- ** SelectObjectContent
    selectObjectContent_requestProgress,
    selectObjectContent_expectedBucketOwner,
    selectObjectContent_scanRange,
    selectObjectContent_sSECustomerKeyMD5,
    selectObjectContent_sSECustomerAlgorithm,
    selectObjectContent_sSECustomerKey,
    selectObjectContent_bucket,
    selectObjectContent_key,
    selectObjectContent_expression,
    selectObjectContent_expressionType,
    selectObjectContent_inputSerialization,
    selectObjectContent_outputSerialization,
    selectObjectContentResponse_payload,
    selectObjectContentResponse_httpStatus,

    -- ** GetBucketPolicyStatus
    getBucketPolicyStatus_expectedBucketOwner,
    getBucketPolicyStatus_bucket,
    getBucketPolicyStatusResponse_policyStatus,
    getBucketPolicyStatusResponse_httpStatus,

    -- ** ListObjects
    listObjects_expectedBucketOwner,
    listObjects_encodingType,
    listObjects_delimiter,
    listObjects_prefix,
    listObjects_maxKeys,
    listObjects_requestPayer,
    listObjects_marker,
    listObjects_bucket,
    listObjectsResponse_commonPrefixes,
    listObjectsResponse_encodingType,
    listObjectsResponse_delimiter,
    listObjectsResponse_prefix,
    listObjectsResponse_isTruncated,
    listObjectsResponse_maxKeys,
    listObjectsResponse_nextMarker,
    listObjectsResponse_contents,
    listObjectsResponse_name,
    listObjectsResponse_marker,
    listObjectsResponse_httpStatus,

    -- ** DeleteObject
    deleteObject_expectedBucketOwner,
    deleteObject_bypassGovernanceRetention,
    deleteObject_versionId,
    deleteObject_requestPayer,
    deleteObject_mfa,
    deleteObject_bucket,
    deleteObject_key,
    deleteObjectResponse_requestCharged,
    deleteObjectResponse_deleteMarker,
    deleteObjectResponse_versionId,
    deleteObjectResponse_httpStatus,

    -- ** DeleteBucketAnalyticsConfiguration
    deleteBucketAnalyticsConfiguration_expectedBucketOwner,
    deleteBucketAnalyticsConfiguration_bucket,
    deleteBucketAnalyticsConfiguration_id,

    -- ** DeleteObjectTagging
    deleteObjectTagging_expectedBucketOwner,
    deleteObjectTagging_versionId,
    deleteObjectTagging_bucket,
    deleteObjectTagging_key,
    deleteObjectTaggingResponse_versionId,
    deleteObjectTaggingResponse_httpStatus,

    -- ** GetBucketAcl
    getBucketAcl_expectedBucketOwner,
    getBucketAcl_bucket,
    getBucketAclResponse_owner,
    getBucketAclResponse_grants,
    getBucketAclResponse_httpStatus,

    -- ** GetObjectTagging
    getObjectTagging_expectedBucketOwner,
    getObjectTagging_versionId,
    getObjectTagging_requestPayer,
    getObjectTagging_bucket,
    getObjectTagging_key,
    getObjectTaggingResponse_versionId,
    getObjectTaggingResponse_httpStatus,
    getObjectTaggingResponse_tagSet,

    -- ** PutBucketReplication
    putBucketReplication_expectedBucketOwner,
    putBucketReplication_contentMD5,
    putBucketReplication_token,
    putBucketReplication_bucket,
    putBucketReplication_replicationConfiguration,

    -- ** GetBucketWebsite
    getBucketWebsite_expectedBucketOwner,
    getBucketWebsite_bucket,
    getBucketWebsiteResponse_errorDocument,
    getBucketWebsiteResponse_indexDocument,
    getBucketWebsiteResponse_routingRules,
    getBucketWebsiteResponse_redirectAllRequestsTo,
    getBucketWebsiteResponse_httpStatus,

    -- ** GetObjectLockConfiguration
    getObjectLockConfiguration_expectedBucketOwner,
    getObjectLockConfiguration_bucket,
    getObjectLockConfigurationResponse_objectLockConfiguration,
    getObjectLockConfigurationResponse_httpStatus,

    -- ** DeleteBucketMetricsConfiguration
    deleteBucketMetricsConfiguration_expectedBucketOwner,
    deleteBucketMetricsConfiguration_bucket,
    deleteBucketMetricsConfiguration_id,

    -- ** GetBucketPolicy
    getBucketPolicy_expectedBucketOwner,
    getBucketPolicy_bucket,
    getBucketPolicyResponse_httpStatus,
    getBucketPolicyResponse_policy,

    -- ** PutBucketEncryption
    putBucketEncryption_expectedBucketOwner,
    putBucketEncryption_contentMD5,
    putBucketEncryption_bucket,
    putBucketEncryption_serverSideEncryptionConfiguration,

    -- ** ListBucketMetricsConfigurations
    listBucketMetricsConfigurations_expectedBucketOwner,
    listBucketMetricsConfigurations_continuationToken,
    listBucketMetricsConfigurations_bucket,
    listBucketMetricsConfigurationsResponse_isTruncated,
    listBucketMetricsConfigurationsResponse_nextContinuationToken,
    listBucketMetricsConfigurationsResponse_metricsConfigurationList,
    listBucketMetricsConfigurationsResponse_continuationToken,
    listBucketMetricsConfigurationsResponse_httpStatus,

    -- ** PutBucketAccelerateConfiguration
    putBucketAccelerateConfiguration_expectedBucketOwner,
    putBucketAccelerateConfiguration_bucket,
    putBucketAccelerateConfiguration_accelerateConfiguration,

    -- ** PutBucketOwnershipControls
    putBucketOwnershipControls_expectedBucketOwner,
    putBucketOwnershipControls_contentMD5,
    putBucketOwnershipControls_bucket,
    putBucketOwnershipControls_ownershipControls,

    -- ** PutObjectRetention
    putObjectRetention_expectedBucketOwner,
    putObjectRetention_bypassGovernanceRetention,
    putObjectRetention_contentMD5,
    putObjectRetention_versionId,
    putObjectRetention_retention,
    putObjectRetention_requestPayer,
    putObjectRetention_bucket,
    putObjectRetention_key,
    putObjectRetentionResponse_requestCharged,
    putObjectRetentionResponse_httpStatus,

    -- ** PutObjectLegalHold
    putObjectLegalHold_expectedBucketOwner,
    putObjectLegalHold_contentMD5,
    putObjectLegalHold_versionId,
    putObjectLegalHold_legalHold,
    putObjectLegalHold_requestPayer,
    putObjectLegalHold_bucket,
    putObjectLegalHold_key,
    putObjectLegalHoldResponse_requestCharged,
    putObjectLegalHoldResponse_httpStatus,

    -- ** HeadObject
    headObject_ifUnmodifiedSince,
    headObject_range,
    headObject_ifModifiedSince,
    headObject_expectedBucketOwner,
    headObject_sSECustomerKeyMD5,
    headObject_versionId,
    headObject_ifMatch,
    headObject_partNumber,
    headObject_ifNoneMatch,
    headObject_sSECustomerAlgorithm,
    headObject_requestPayer,
    headObject_sSECustomerKey,
    headObject_bucket,
    headObject_key,
    headObjectResponse_eTag,
    headObjectResponse_requestCharged,
    headObjectResponse_partsCount,
    headObjectResponse_websiteRedirectLocation,
    headObjectResponse_contentType,
    headObjectResponse_contentDisposition,
    headObjectResponse_archiveStatus,
    headObjectResponse_deleteMarker,
    headObjectResponse_expiration,
    headObjectResponse_contentLanguage,
    headObjectResponse_replicationStatus,
    headObjectResponse_metadata,
    headObjectResponse_contentLength,
    headObjectResponse_contentEncoding,
    headObjectResponse_sSEKMSKeyId,
    headObjectResponse_sSECustomerKeyMD5,
    headObjectResponse_storageClass,
    headObjectResponse_versionId,
    headObjectResponse_acceptRanges,
    headObjectResponse_bucketKeyEnabled,
    headObjectResponse_serverSideEncryption,
    headObjectResponse_missingMeta,
    headObjectResponse_objectLockLegalHoldStatus,
    headObjectResponse_sSECustomerAlgorithm,
    headObjectResponse_lastModified,
    headObjectResponse_cacheControl,
    headObjectResponse_expires,
    headObjectResponse_restore,
    headObjectResponse_objectLockMode,
    headObjectResponse_objectLockRetainUntilDate,
    headObjectResponse_httpStatus,

    -- ** GetBucketTagging
    getBucketTagging_expectedBucketOwner,
    getBucketTagging_bucket,
    getBucketTaggingResponse_httpStatus,
    getBucketTaggingResponse_tagSet,

    -- ** GetBucketLocation
    getBucketLocation_expectedBucketOwner,
    getBucketLocation_bucket,
    getBucketLocationResponse_httpStatus,
    getBucketLocationResponse_locationConstraint,

    -- ** PutBucketInventoryConfiguration
    putBucketInventoryConfiguration_expectedBucketOwner,
    putBucketInventoryConfiguration_bucket,
    putBucketInventoryConfiguration_id,
    putBucketInventoryConfiguration_inventoryConfiguration,

    -- ** ListBucketInventoryConfigurations
    listBucketInventoryConfigurations_expectedBucketOwner,
    listBucketInventoryConfigurations_continuationToken,
    listBucketInventoryConfigurations_bucket,
    listBucketInventoryConfigurationsResponse_inventoryConfigurationList,
    listBucketInventoryConfigurationsResponse_isTruncated,
    listBucketInventoryConfigurationsResponse_nextContinuationToken,
    listBucketInventoryConfigurationsResponse_continuationToken,
    listBucketInventoryConfigurationsResponse_httpStatus,

    -- ** GetObjectAcl
    getObjectAcl_expectedBucketOwner,
    getObjectAcl_versionId,
    getObjectAcl_requestPayer,
    getObjectAcl_bucket,
    getObjectAcl_key,
    getObjectAclResponse_requestCharged,
    getObjectAclResponse_owner,
    getObjectAclResponse_grants,
    getObjectAclResponse_httpStatus,

    -- ** DeletePublicAccessBlock
    deletePublicAccessBlock_expectedBucketOwner,
    deletePublicAccessBlock_bucket,

    -- ** DeleteBucketIntelligentTieringConfiguration
    deleteBucketIntelligentTieringConfiguration_bucket,
    deleteBucketIntelligentTieringConfiguration_id,

    -- ** GetBucketVersioning
    getBucketVersioning_expectedBucketOwner,
    getBucketVersioning_bucket,
    getBucketVersioningResponse_status,
    getBucketVersioningResponse_mfaDelete,
    getBucketVersioningResponse_httpStatus,

    -- ** PutBucketTagging
    putBucketTagging_expectedBucketOwner,
    putBucketTagging_contentMD5,
    putBucketTagging_bucket,
    putBucketTagging_tagging,

    -- ** CreateBucket
    createBucket_grantRead,
    createBucket_createBucketConfiguration,
    createBucket_grantWriteACP,
    createBucket_objectLockEnabledForBucket,
    createBucket_grantReadACP,
    createBucket_acl,
    createBucket_grantWrite,
    createBucket_grantFullControl,
    createBucket_bucket,
    createBucketResponse_location,
    createBucketResponse_httpStatus,

    -- ** PutObjectAcl
    putObjectAcl_grantRead,
    putObjectAcl_expectedBucketOwner,
    putObjectAcl_accessControlPolicy,
    putObjectAcl_contentMD5,
    putObjectAcl_versionId,
    putObjectAcl_grantWriteACP,
    putObjectAcl_grantReadACP,
    putObjectAcl_acl,
    putObjectAcl_requestPayer,
    putObjectAcl_grantWrite,
    putObjectAcl_grantFullControl,
    putObjectAcl_bucket,
    putObjectAcl_key,
    putObjectAclResponse_requestCharged,
    putObjectAclResponse_httpStatus,

    -- ** PutBucketCors
    putBucketCors_expectedBucketOwner,
    putBucketCors_contentMD5,
    putBucketCors_bucket,
    putBucketCors_cORSConfiguration,

    -- ** GetObjectRetention
    getObjectRetention_expectedBucketOwner,
    getObjectRetention_versionId,
    getObjectRetention_requestPayer,
    getObjectRetention_bucket,
    getObjectRetention_key,
    getObjectRetentionResponse_retention,
    getObjectRetentionResponse_httpStatus,

    -- ** GetObjectTorrent
    getObjectTorrent_expectedBucketOwner,
    getObjectTorrent_requestPayer,
    getObjectTorrent_bucket,
    getObjectTorrent_key,
    getObjectTorrentResponse_requestCharged,
    getObjectTorrentResponse_httpStatus,
    getObjectTorrentResponse_body,

    -- ** GetBucketOwnershipControls
    getBucketOwnershipControls_expectedBucketOwner,
    getBucketOwnershipControls_bucket,
    getBucketOwnershipControlsResponse_ownershipControls,
    getBucketOwnershipControlsResponse_httpStatus,

    -- ** GetBucketMetricsConfiguration
    getBucketMetricsConfiguration_expectedBucketOwner,
    getBucketMetricsConfiguration_bucket,
    getBucketMetricsConfiguration_id,
    getBucketMetricsConfigurationResponse_metricsConfiguration,
    getBucketMetricsConfigurationResponse_httpStatus,

    -- ** GetBucketAccelerateConfiguration
    getBucketAccelerateConfiguration_expectedBucketOwner,
    getBucketAccelerateConfiguration_bucket,
    getBucketAccelerateConfigurationResponse_status,
    getBucketAccelerateConfigurationResponse_httpStatus,

    -- ** GetObjectLegalHold
    getObjectLegalHold_expectedBucketOwner,
    getObjectLegalHold_versionId,
    getObjectLegalHold_requestPayer,
    getObjectLegalHold_bucket,
    getObjectLegalHold_key,
    getObjectLegalHoldResponse_legalHold,
    getObjectLegalHoldResponse_httpStatus,

    -- ** ListBuckets
    listBucketsResponse_buckets,
    listBucketsResponse_owner,
    listBucketsResponse_httpStatus,

    -- ** DeleteBucketPolicy
    deleteBucketPolicy_expectedBucketOwner,
    deleteBucketPolicy_bucket,

    -- ** PutObjectLockConfiguration
    putObjectLockConfiguration_expectedBucketOwner,
    putObjectLockConfiguration_objectLockConfiguration,
    putObjectLockConfiguration_contentMD5,
    putObjectLockConfiguration_requestPayer,
    putObjectLockConfiguration_token,
    putObjectLockConfiguration_bucket,
    putObjectLockConfigurationResponse_requestCharged,
    putObjectLockConfigurationResponse_httpStatus,

    -- ** AbortMultipartUpload
    abortMultipartUpload_expectedBucketOwner,
    abortMultipartUpload_requestPayer,
    abortMultipartUpload_bucket,
    abortMultipartUpload_key,
    abortMultipartUpload_uploadId,
    abortMultipartUploadResponse_requestCharged,
    abortMultipartUploadResponse_httpStatus,

    -- ** UploadPart
    uploadPart_expectedBucketOwner,
    uploadPart_contentMD5,
    uploadPart_contentLength,
    uploadPart_sSECustomerKeyMD5,
    uploadPart_sSECustomerAlgorithm,
    uploadPart_requestPayer,
    uploadPart_sSECustomerKey,
    uploadPart_bucket,
    uploadPart_key,
    uploadPart_partNumber,
    uploadPart_uploadId,
    uploadPart_body,
    uploadPartResponse_eTag,
    uploadPartResponse_requestCharged,
    uploadPartResponse_sSEKMSKeyId,
    uploadPartResponse_sSECustomerKeyMD5,
    uploadPartResponse_bucketKeyEnabled,
    uploadPartResponse_serverSideEncryption,
    uploadPartResponse_sSECustomerAlgorithm,
    uploadPartResponse_httpStatus,

    -- ** PutObject
    putObject_websiteRedirectLocation,
    putObject_grantRead,
    putObject_contentType,
    putObject_expectedBucketOwner,
    putObject_contentDisposition,
    putObject_contentLanguage,
    putObject_sSEKMSEncryptionContext,
    putObject_contentMD5,
    putObject_metadata,
    putObject_contentLength,
    putObject_contentEncoding,
    putObject_sSEKMSKeyId,
    putObject_sSECustomerKeyMD5,
    putObject_storageClass,
    putObject_bucketKeyEnabled,
    putObject_grantWriteACP,
    putObject_serverSideEncryption,
    putObject_objectLockLegalHoldStatus,
    putObject_grantReadACP,
    putObject_acl,
    putObject_sSECustomerAlgorithm,
    putObject_requestPayer,
    putObject_sSECustomerKey,
    putObject_cacheControl,
    putObject_expires,
    putObject_objectLockMode,
    putObject_objectLockRetainUntilDate,
    putObject_tagging,
    putObject_grantFullControl,
    putObject_bucket,
    putObject_key,
    putObject_body,
    putObjectResponse_eTag,
    putObjectResponse_requestCharged,
    putObjectResponse_expiration,
    putObjectResponse_sSEKMSEncryptionContext,
    putObjectResponse_sSEKMSKeyId,
    putObjectResponse_sSECustomerKeyMD5,
    putObjectResponse_versionId,
    putObjectResponse_bucketKeyEnabled,
    putObjectResponse_serverSideEncryption,
    putObjectResponse_sSECustomerAlgorithm,
    putObjectResponse_httpStatus,

    -- ** PutBucketRequestPayment
    putBucketRequestPayment_expectedBucketOwner,
    putBucketRequestPayment_contentMD5,
    putBucketRequestPayment_bucket,
    putBucketRequestPayment_requestPaymentConfiguration,

    -- ** ListMultipartUploads
    listMultipartUploads_expectedBucketOwner,
    listMultipartUploads_encodingType,
    listMultipartUploads_delimiter,
    listMultipartUploads_prefix,
    listMultipartUploads_keyMarker,
    listMultipartUploads_maxUploads,
    listMultipartUploads_uploadIdMarker,
    listMultipartUploads_bucket,
    listMultipartUploadsResponse_commonPrefixes,
    listMultipartUploadsResponse_encodingType,
    listMultipartUploadsResponse_delimiter,
    listMultipartUploadsResponse_uploads,
    listMultipartUploadsResponse_prefix,
    listMultipartUploadsResponse_isTruncated,
    listMultipartUploadsResponse_keyMarker,
    listMultipartUploadsResponse_nextKeyMarker,
    listMultipartUploadsResponse_maxUploads,
    listMultipartUploadsResponse_uploadIdMarker,
    listMultipartUploadsResponse_bucket,
    listMultipartUploadsResponse_nextUploadIdMarker,
    listMultipartUploadsResponse_httpStatus,

    -- ** GetBucketReplication
    getBucketReplication_expectedBucketOwner,
    getBucketReplication_bucket,
    getBucketReplicationResponse_replicationConfiguration,
    getBucketReplicationResponse_httpStatus,

    -- ** PutBucketWebsite
    putBucketWebsite_expectedBucketOwner,
    putBucketWebsite_contentMD5,
    putBucketWebsite_bucket,
    putBucketWebsite_websiteConfiguration,

    -- ** PutBucketAnalyticsConfiguration
    putBucketAnalyticsConfiguration_expectedBucketOwner,
    putBucketAnalyticsConfiguration_bucket,
    putBucketAnalyticsConfiguration_id,
    putBucketAnalyticsConfiguration_analyticsConfiguration,

    -- ** PutObjectTagging
    putObjectTagging_expectedBucketOwner,
    putObjectTagging_contentMD5,
    putObjectTagging_versionId,
    putObjectTagging_requestPayer,
    putObjectTagging_bucket,
    putObjectTagging_key,
    putObjectTagging_tagging,
    putObjectTaggingResponse_versionId,
    putObjectTaggingResponse_httpStatus,

    -- ** UploadPartCopy
    uploadPartCopy_copySourceIfMatch,
    uploadPartCopy_expectedSourceBucketOwner,
    uploadPartCopy_expectedBucketOwner,
    uploadPartCopy_copySourceSSECustomerKey,
    uploadPartCopy_copySourceSSECustomerAlgorithm,
    uploadPartCopy_copySourceIfNoneMatch,
    uploadPartCopy_sSECustomerKeyMD5,
    uploadPartCopy_copySourceIfUnmodifiedSince,
    uploadPartCopy_copySourceRange,
    uploadPartCopy_copySourceIfModifiedSince,
    uploadPartCopy_sSECustomerAlgorithm,
    uploadPartCopy_requestPayer,
    uploadPartCopy_sSECustomerKey,
    uploadPartCopy_copySourceSSECustomerKeyMD5,
    uploadPartCopy_bucket,
    uploadPartCopy_copySource,
    uploadPartCopy_key,
    uploadPartCopy_partNumber,
    uploadPartCopy_uploadId,
    uploadPartCopyResponse_requestCharged,
    uploadPartCopyResponse_copySourceVersionId,
    uploadPartCopyResponse_copyPartResult,
    uploadPartCopyResponse_sSEKMSKeyId,
    uploadPartCopyResponse_sSECustomerKeyMD5,
    uploadPartCopyResponse_bucketKeyEnabled,
    uploadPartCopyResponse_serverSideEncryption,
    uploadPartCopyResponse_sSECustomerAlgorithm,
    uploadPartCopyResponse_httpStatus,

    -- ** CreateMultipartUpload
    createMultipartUpload_websiteRedirectLocation,
    createMultipartUpload_grantRead,
    createMultipartUpload_contentType,
    createMultipartUpload_expectedBucketOwner,
    createMultipartUpload_contentDisposition,
    createMultipartUpload_contentLanguage,
    createMultipartUpload_sSEKMSEncryptionContext,
    createMultipartUpload_metadata,
    createMultipartUpload_contentEncoding,
    createMultipartUpload_sSEKMSKeyId,
    createMultipartUpload_sSECustomerKeyMD5,
    createMultipartUpload_storageClass,
    createMultipartUpload_bucketKeyEnabled,
    createMultipartUpload_grantWriteACP,
    createMultipartUpload_serverSideEncryption,
    createMultipartUpload_objectLockLegalHoldStatus,
    createMultipartUpload_grantReadACP,
    createMultipartUpload_acl,
    createMultipartUpload_sSECustomerAlgorithm,
    createMultipartUpload_requestPayer,
    createMultipartUpload_sSECustomerKey,
    createMultipartUpload_cacheControl,
    createMultipartUpload_expires,
    createMultipartUpload_objectLockMode,
    createMultipartUpload_objectLockRetainUntilDate,
    createMultipartUpload_tagging,
    createMultipartUpload_grantFullControl,
    createMultipartUpload_bucket,
    createMultipartUpload_key,
    createMultipartUploadResponse_requestCharged,
    createMultipartUploadResponse_key,
    createMultipartUploadResponse_uploadId,
    createMultipartUploadResponse_abortDate,
    createMultipartUploadResponse_sSEKMSEncryptionContext,
    createMultipartUploadResponse_sSEKMSKeyId,
    createMultipartUploadResponse_sSECustomerKeyMD5,
    createMultipartUploadResponse_bucketKeyEnabled,
    createMultipartUploadResponse_serverSideEncryption,
    createMultipartUploadResponse_abortRuleId,
    createMultipartUploadResponse_sSECustomerAlgorithm,
    createMultipartUploadResponse_bucket,
    createMultipartUploadResponse_httpStatus,

    -- ** PutBucketLifecycleConfiguration
    putBucketLifecycleConfiguration_expectedBucketOwner,
    putBucketLifecycleConfiguration_lifecycleConfiguration,
    putBucketLifecycleConfiguration_bucket,

    -- ** PutBucketAcl
    putBucketAcl_grantRead,
    putBucketAcl_expectedBucketOwner,
    putBucketAcl_accessControlPolicy,
    putBucketAcl_contentMD5,
    putBucketAcl_grantWriteACP,
    putBucketAcl_grantReadACP,
    putBucketAcl_acl,
    putBucketAcl_grantWrite,
    putBucketAcl_grantFullControl,
    putBucketAcl_bucket,

    -- ** DeleteBucketLifecycle
    deleteBucketLifecycle_expectedBucketOwner,
    deleteBucketLifecycle_bucket,

    -- ** ListBucketAnalyticsConfigurations
    listBucketAnalyticsConfigurations_expectedBucketOwner,
    listBucketAnalyticsConfigurations_continuationToken,
    listBucketAnalyticsConfigurations_bucket,
    listBucketAnalyticsConfigurationsResponse_analyticsConfigurationList,
    listBucketAnalyticsConfigurationsResponse_isTruncated,
    listBucketAnalyticsConfigurationsResponse_nextContinuationToken,
    listBucketAnalyticsConfigurationsResponse_continuationToken,
    listBucketAnalyticsConfigurationsResponse_httpStatus,

    -- ** GetBucketAnalyticsConfiguration
    getBucketAnalyticsConfiguration_expectedBucketOwner,
    getBucketAnalyticsConfiguration_bucket,
    getBucketAnalyticsConfiguration_id,
    getBucketAnalyticsConfigurationResponse_analyticsConfiguration,
    getBucketAnalyticsConfigurationResponse_httpStatus,

    -- ** HeadBucket
    headBucket_expectedBucketOwner,
    headBucket_bucket,

    -- ** ListObjectVersions
    listObjectVersions_expectedBucketOwner,
    listObjectVersions_encodingType,
    listObjectVersions_delimiter,
    listObjectVersions_prefix,
    listObjectVersions_maxKeys,
    listObjectVersions_keyMarker,
    listObjectVersions_versionIdMarker,
    listObjectVersions_bucket,
    listObjectVersionsResponse_versions,
    listObjectVersionsResponse_commonPrefixes,
    listObjectVersionsResponse_encodingType,
    listObjectVersionsResponse_delimiter,
    listObjectVersionsResponse_prefix,
    listObjectVersionsResponse_isTruncated,
    listObjectVersionsResponse_maxKeys,
    listObjectVersionsResponse_keyMarker,
    listObjectVersionsResponse_nextKeyMarker,
    listObjectVersionsResponse_name,
    listObjectVersionsResponse_deleteMarkers,
    listObjectVersionsResponse_versionIdMarker,
    listObjectVersionsResponse_nextVersionIdMarker,
    listObjectVersionsResponse_httpStatus,

    -- ** GetBucketLifecycleConfiguration
    getBucketLifecycleConfiguration_expectedBucketOwner,
    getBucketLifecycleConfiguration_bucket,
    getBucketLifecycleConfigurationResponse_rules,
    getBucketLifecycleConfigurationResponse_httpStatus,

    -- ** ListParts
    listParts_expectedBucketOwner,
    listParts_partNumberMarker,
    listParts_maxParts,
    listParts_requestPayer,
    listParts_bucket,
    listParts_key,
    listParts_uploadId,
    listPartsResponse_requestCharged,
    listPartsResponse_key,
    listPartsResponse_nextPartNumberMarker,
    listPartsResponse_uploadId,
    listPartsResponse_abortDate,
    listPartsResponse_partNumberMarker,
    listPartsResponse_maxParts,
    listPartsResponse_isTruncated,
    listPartsResponse_storageClass,
    listPartsResponse_parts,
    listPartsResponse_abortRuleId,
    listPartsResponse_owner,
    listPartsResponse_bucket,
    listPartsResponse_initiator,
    listPartsResponse_httpStatus,

    -- ** GetBucketRequestPayment
    getBucketRequestPayment_expectedBucketOwner,
    getBucketRequestPayment_bucket,
    getBucketRequestPaymentResponse_payer,
    getBucketRequestPaymentResponse_httpStatus,

    -- ** DeleteBucketReplication
    deleteBucketReplication_expectedBucketOwner,
    deleteBucketReplication_bucket,

    -- ** GetBucketLogging
    getBucketLogging_expectedBucketOwner,
    getBucketLogging_bucket,
    getBucketLoggingResponse_loggingEnabled,
    getBucketLoggingResponse_httpStatus,

    -- ** GetObject
    getObject_ifUnmodifiedSince,
    getObject_range,
    getObject_ifModifiedSince,
    getObject_expectedBucketOwner,
    getObject_responseExpires,
    getObject_sSECustomerKeyMD5,
    getObject_versionId,
    getObject_ifMatch,
    getObject_partNumber,
    getObject_responseContentEncoding,
    getObject_ifNoneMatch,
    getObject_sSECustomerAlgorithm,
    getObject_requestPayer,
    getObject_sSECustomerKey,
    getObject_responseContentLanguage,
    getObject_responseCacheControl,
    getObject_responseContentDisposition,
    getObject_responseContentType,
    getObject_bucket,
    getObject_key,
    getObjectResponse_eTag,
    getObjectResponse_requestCharged,
    getObjectResponse_partsCount,
    getObjectResponse_websiteRedirectLocation,
    getObjectResponse_contentType,
    getObjectResponse_tagCount,
    getObjectResponse_contentRange,
    getObjectResponse_contentDisposition,
    getObjectResponse_deleteMarker,
    getObjectResponse_expiration,
    getObjectResponse_contentLanguage,
    getObjectResponse_replicationStatus,
    getObjectResponse_metadata,
    getObjectResponse_contentLength,
    getObjectResponse_contentEncoding,
    getObjectResponse_sSEKMSKeyId,
    getObjectResponse_sSECustomerKeyMD5,
    getObjectResponse_storageClass,
    getObjectResponse_versionId,
    getObjectResponse_acceptRanges,
    getObjectResponse_bucketKeyEnabled,
    getObjectResponse_serverSideEncryption,
    getObjectResponse_missingMeta,
    getObjectResponse_objectLockLegalHoldStatus,
    getObjectResponse_sSECustomerAlgorithm,
    getObjectResponse_lastModified,
    getObjectResponse_cacheControl,
    getObjectResponse_expires,
    getObjectResponse_restore,
    getObjectResponse_objectLockMode,
    getObjectResponse_objectLockRetainUntilDate,
    getObjectResponse_httpStatus,
    getObjectResponse_body,

    -- ** DeleteBucketOwnershipControls
    deleteBucketOwnershipControls_expectedBucketOwner,
    deleteBucketOwnershipControls_bucket,

    -- ** PutBucketMetricsConfiguration
    putBucketMetricsConfiguration_expectedBucketOwner,
    putBucketMetricsConfiguration_bucket,
    putBucketMetricsConfiguration_id,
    putBucketMetricsConfiguration_metricsConfiguration,

    -- ** ListObjectsV
    listObjectsV_startAfter,
    listObjectsV_expectedBucketOwner,
    listObjectsV_encodingType,
    listObjectsV_delimiter,
    listObjectsV_prefix,
    listObjectsV_maxKeys,
    listObjectsV_requestPayer,
    listObjectsV_fetchOwner,
    listObjectsV_continuationToken,
    listObjectsV_bucket,
    listObjectsVResponse_startAfter,
    listObjectsVResponse_keyCount,
    listObjectsVResponse_commonPrefixes,
    listObjectsVResponse_encodingType,
    listObjectsVResponse_delimiter,
    listObjectsVResponse_prefix,
    listObjectsVResponse_isTruncated,
    listObjectsVResponse_maxKeys,
    listObjectsVResponse_contents,
    listObjectsVResponse_name,
    listObjectsVResponse_nextContinuationToken,
    listObjectsVResponse_continuationToken,
    listObjectsVResponse_httpStatus,

    -- ** CopyObject
    copyObject_copySourceIfMatch,
    copyObject_websiteRedirectLocation,
    copyObject_grantRead,
    copyObject_expectedSourceBucketOwner,
    copyObject_contentType,
    copyObject_expectedBucketOwner,
    copyObject_contentDisposition,
    copyObject_copySourceSSECustomerKey,
    copyObject_copySourceSSECustomerAlgorithm,
    copyObject_copySourceIfNoneMatch,
    copyObject_contentLanguage,
    copyObject_sSEKMSEncryptionContext,
    copyObject_metadata,
    copyObject_contentEncoding,
    copyObject_sSEKMSKeyId,
    copyObject_sSECustomerKeyMD5,
    copyObject_taggingDirective,
    copyObject_storageClass,
    copyObject_copySourceIfUnmodifiedSince,
    copyObject_copySourceIfModifiedSince,
    copyObject_bucketKeyEnabled,
    copyObject_grantWriteACP,
    copyObject_serverSideEncryption,
    copyObject_objectLockLegalHoldStatus,
    copyObject_grantReadACP,
    copyObject_acl,
    copyObject_sSECustomerAlgorithm,
    copyObject_requestPayer,
    copyObject_sSECustomerKey,
    copyObject_cacheControl,
    copyObject_expires,
    copyObject_objectLockMode,
    copyObject_objectLockRetainUntilDate,
    copyObject_tagging,
    copyObject_grantFullControl,
    copyObject_copySourceSSECustomerKeyMD5,
    copyObject_metadataDirective,
    copyObject_bucket,
    copyObject_copySource,
    copyObject_key,
    copyObjectResponse_requestCharged,
    copyObjectResponse_copySourceVersionId,
    copyObjectResponse_expiration,
    copyObjectResponse_sSEKMSEncryptionContext,
    copyObjectResponse_sSEKMSKeyId,
    copyObjectResponse_sSECustomerKeyMD5,
    copyObjectResponse_versionId,
    copyObjectResponse_bucketKeyEnabled,
    copyObjectResponse_copyObjectResult,
    copyObjectResponse_serverSideEncryption,
    copyObjectResponse_sSECustomerAlgorithm,
    copyObjectResponse_httpStatus,

    -- ** DeleteBucketEncryption
    deleteBucketEncryption_expectedBucketOwner,
    deleteBucketEncryption_bucket,

    -- ** PutBucketVersioning
    putBucketVersioning_expectedBucketOwner,
    putBucketVersioning_contentMD5,
    putBucketVersioning_mfa,
    putBucketVersioning_bucket,
    putBucketVersioning_versioningConfiguration,

    -- ** GetBucketNotificationConfiguration
    getBucketNotificationConfiguration_expectedBucketOwner,
    getBucketNotificationConfiguration_bucket,
    notificationConfiguration_lambdaFunctionConfigurations,
    notificationConfiguration_queueConfigurations,
    notificationConfiguration_topicConfigurations,

    -- ** PutPublicAccessBlock
    putPublicAccessBlock_expectedBucketOwner,
    putPublicAccessBlock_contentMD5,
    putPublicAccessBlock_bucket,
    putPublicAccessBlock_publicAccessBlockConfiguration,

    -- ** DeleteBucketInventoryConfiguration
    deleteBucketInventoryConfiguration_expectedBucketOwner,
    deleteBucketInventoryConfiguration_bucket,
    deleteBucketInventoryConfiguration_id,

    -- ** GetBucketIntelligentTieringConfiguration
    getBucketIntelligentTieringConfiguration_bucket,
    getBucketIntelligentTieringConfiguration_id,
    getBucketIntelligentTieringConfigurationResponse_intelligentTieringConfiguration,
    getBucketIntelligentTieringConfigurationResponse_httpStatus,

    -- ** RestoreObject
    restoreObject_expectedBucketOwner,
    restoreObject_versionId,
    restoreObject_restoreRequest,
    restoreObject_requestPayer,
    restoreObject_bucket,
    restoreObject_key,
    restoreObjectResponse_requestCharged,
    restoreObjectResponse_restoreOutputPath,
    restoreObjectResponse_httpStatus,

    -- ** GetBucketCors
    getBucketCors_expectedBucketOwner,
    getBucketCors_bucket,
    getBucketCorsResponse_cORSRules,
    getBucketCorsResponse_httpStatus,

    -- ** GetBucketInventoryConfiguration
    getBucketInventoryConfiguration_expectedBucketOwner,
    getBucketInventoryConfiguration_bucket,
    getBucketInventoryConfiguration_id,
    getBucketInventoryConfigurationResponse_inventoryConfiguration,
    getBucketInventoryConfigurationResponse_httpStatus,

    -- ** GetPublicAccessBlock
    getPublicAccessBlock_expectedBucketOwner,
    getPublicAccessBlock_bucket,
    getPublicAccessBlockResponse_publicAccessBlockConfiguration,
    getPublicAccessBlockResponse_httpStatus,

    -- ** DeleteBucketCors
    deleteBucketCors_expectedBucketOwner,
    deleteBucketCors_bucket,

    -- ** DeleteBucketTagging
    deleteBucketTagging_expectedBucketOwner,
    deleteBucketTagging_bucket,

    -- ** ListBucketIntelligentTieringConfigurations
    listBucketIntelligentTieringConfigurations_continuationToken,
    listBucketIntelligentTieringConfigurations_bucket,
    listBucketIntelligentTieringConfigurationsResponse_isTruncated,
    listBucketIntelligentTieringConfigurationsResponse_intelligentTieringConfigurationList,
    listBucketIntelligentTieringConfigurationsResponse_nextContinuationToken,
    listBucketIntelligentTieringConfigurationsResponse_continuationToken,
    listBucketIntelligentTieringConfigurationsResponse_httpStatus,

    -- ** PutBucketNotificationConfiguration
    putBucketNotificationConfiguration_expectedBucketOwner,
    putBucketNotificationConfiguration_bucket,
    putBucketNotificationConfiguration_notificationConfiguration,

    -- ** PutBucketIntelligentTieringConfiguration
    putBucketIntelligentTieringConfiguration_bucket,
    putBucketIntelligentTieringConfiguration_id,
    putBucketIntelligentTieringConfiguration_intelligentTieringConfiguration,

    -- * Types

    -- ** AbortIncompleteMultipartUpload
    abortIncompleteMultipartUpload_daysAfterInitiation,

    -- ** AccelerateConfiguration
    accelerateConfiguration_status,

    -- ** AccessControlPolicy
    accessControlPolicy_owner,
    accessControlPolicy_grants,

    -- ** AccessControlTranslation
    accessControlTranslation_owner,

    -- ** AnalyticsAndOperator
    analyticsAndOperator_prefix,
    analyticsAndOperator_tags,

    -- ** AnalyticsConfiguration
    analyticsConfiguration_filter,
    analyticsConfiguration_id,
    analyticsConfiguration_storageClassAnalysis,

    -- ** AnalyticsExportDestination
    analyticsExportDestination_s3BucketDestination,

    -- ** AnalyticsFilter
    analyticsFilter_prefix,
    analyticsFilter_and,
    analyticsFilter_tag,

    -- ** AnalyticsS3BucketDestination
    analyticsS3BucketDestination_prefix,
    analyticsS3BucketDestination_bucketAccountId,
    analyticsS3BucketDestination_format,
    analyticsS3BucketDestination_bucket,

    -- ** Bucket
    bucket_creationDate,
    bucket_name,

    -- ** BucketLifecycleConfiguration
    bucketLifecycleConfiguration_rules,

    -- ** BucketLoggingStatus
    bucketLoggingStatus_loggingEnabled,

    -- ** CORSConfiguration
    cORSConfiguration_cORSRules,

    -- ** CORSRule
    cORSRule_allowedHeaders,
    cORSRule_maxAgeSeconds,
    cORSRule_exposeHeaders,
    cORSRule_allowedMethods,
    cORSRule_allowedOrigins,

    -- ** CSVInput
    cSVInput_allowQuotedRecordDelimiter,
    cSVInput_recordDelimiter,
    cSVInput_quoteCharacter,
    cSVInput_fileHeaderInfo,
    cSVInput_fieldDelimiter,
    cSVInput_comments,
    cSVInput_quoteEscapeCharacter,

    -- ** CSVOutput
    cSVOutput_recordDelimiter,
    cSVOutput_quoteCharacter,
    cSVOutput_fieldDelimiter,
    cSVOutput_quoteFields,
    cSVOutput_quoteEscapeCharacter,

    -- ** CommonPrefix
    commonPrefix_prefix,

    -- ** CompletedMultipartUpload
    completedMultipartUpload_parts,

    -- ** CompletedPart
    completedPart_partNumber,
    completedPart_eTag,

    -- ** Condition
    condition_httpErrorCodeReturnedEquals,
    condition_keyPrefixEquals,

    -- ** ContinuationEvent

    -- ** CopyObjectResult
    copyObjectResult_eTag,
    copyObjectResult_lastModified,

    -- ** CopyPartResult
    copyPartResult_eTag,
    copyPartResult_lastModified,

    -- ** CreateBucketConfiguration
    createBucketConfiguration_locationConstraint,

    -- ** DefaultRetention
    defaultRetention_days,
    defaultRetention_years,
    defaultRetention_mode,

    -- ** Delete
    delete_quiet,
    delete_objects,

    -- ** DeleteMarkerEntry
    deleteMarkerEntry_key,
    deleteMarkerEntry_isLatest,
    deleteMarkerEntry_versionId,
    deleteMarkerEntry_owner,
    deleteMarkerEntry_lastModified,

    -- ** DeleteMarkerReplication
    deleteMarkerReplication_status,

    -- ** DeletedObject
    deletedObject_key,
    deletedObject_deleteMarkerVersionId,
    deletedObject_deleteMarker,
    deletedObject_versionId,

    -- ** Destination
    destination_encryptionConfiguration,
    destination_replicationTime,
    destination_accessControlTranslation,
    destination_storageClass,
    destination_metrics,
    destination_account,
    destination_bucket,

    -- ** Encryption
    encryption_kmsKeyId,
    encryption_kmsContext,
    encryption_encryptionType,

    -- ** EncryptionConfiguration
    encryptionConfiguration_replicaKmsKeyID,

    -- ** EndEvent

    -- ** ErrorDocument
    errorDocument_key,

    -- ** ExistingObjectReplication
    existingObjectReplication_status,

    -- ** FilterRule
    filterRule_name,
    filterRule_value,

    -- ** GlacierJobParameters
    glacierJobParameters_tier,

    -- ** Grant
    grant_grantee,
    grant_permission,

    -- ** Grantee
    grantee_uri,
    grantee_id,
    grantee_displayName,
    grantee_emailAddress,
    grantee_type,

    -- ** IndexDocument
    indexDocument_suffix,

    -- ** Initiator
    initiator_id,
    initiator_displayName,

    -- ** InputSerialization
    inputSerialization_parquet,
    inputSerialization_csv,
    inputSerialization_json,
    inputSerialization_compressionType,

    -- ** IntelligentTieringAndOperator
    intelligentTieringAndOperator_prefix,
    intelligentTieringAndOperator_tags,

    -- ** IntelligentTieringConfiguration
    intelligentTieringConfiguration_filter,
    intelligentTieringConfiguration_id,
    intelligentTieringConfiguration_status,
    intelligentTieringConfiguration_tierings,

    -- ** IntelligentTieringFilter
    intelligentTieringFilter_prefix,
    intelligentTieringFilter_and,
    intelligentTieringFilter_tag,

    -- ** InventoryConfiguration
    inventoryConfiguration_optionalFields,
    inventoryConfiguration_filter,
    inventoryConfiguration_destination,
    inventoryConfiguration_isEnabled,
    inventoryConfiguration_id,
    inventoryConfiguration_includedObjectVersions,
    inventoryConfiguration_schedule,

    -- ** InventoryDestination
    inventoryDestination_s3BucketDestination,

    -- ** InventoryEncryption
    inventoryEncryption_ssekms,
    inventoryEncryption_sses3,

    -- ** InventoryFilter
    inventoryFilter_prefix,

    -- ** InventoryS3BucketDestination
    inventoryS3BucketDestination_accountId,
    inventoryS3BucketDestination_prefix,
    inventoryS3BucketDestination_encryption,
    inventoryS3BucketDestination_bucket,
    inventoryS3BucketDestination_format,

    -- ** InventorySchedule
    inventorySchedule_frequency,

    -- ** JSONInput
    jSONInput_type,

    -- ** JSONOutput
    jSONOutput_recordDelimiter,

    -- ** LambdaFunctionConfiguration
    lambdaFunctionConfiguration_id,
    lambdaFunctionConfiguration_filter,
    lambdaFunctionConfiguration_lambdaFunctionArn,
    lambdaFunctionConfiguration_events,

    -- ** LifecycleExpiration
    lifecycleExpiration_days,
    lifecycleExpiration_expiredObjectDeleteMarker,
    lifecycleExpiration_date,

    -- ** LifecycleRule
    lifecycleRule_expiration,
    lifecycleRule_prefix,
    lifecycleRule_noncurrentVersionTransitions,
    lifecycleRule_id,
    lifecycleRule_noncurrentVersionExpiration,
    lifecycleRule_transitions,
    lifecycleRule_abortIncompleteMultipartUpload,
    lifecycleRule_filter,
    lifecycleRule_status,

    -- ** LifecycleRuleAndOperator
    lifecycleRuleAndOperator_prefix,
    lifecycleRuleAndOperator_tags,

    -- ** LifecycleRuleFilter
    lifecycleRuleFilter_prefix,
    lifecycleRuleFilter_and,
    lifecycleRuleFilter_tag,

    -- ** LoggingEnabled
    loggingEnabled_targetGrants,
    loggingEnabled_targetBucket,
    loggingEnabled_targetPrefix,

    -- ** MetadataEntry
    metadataEntry_name,
    metadataEntry_value,

    -- ** Metrics
    metrics_eventThreshold,
    metrics_status,

    -- ** MetricsAndOperator
    metricsAndOperator_prefix,
    metricsAndOperator_tags,

    -- ** MetricsConfiguration
    metricsConfiguration_filter,
    metricsConfiguration_id,

    -- ** MetricsFilter
    metricsFilter_prefix,
    metricsFilter_and,
    metricsFilter_tag,

    -- ** MultipartUpload
    multipartUpload_key,
    multipartUpload_uploadId,
    multipartUpload_storageClass,
    multipartUpload_initiated,
    multipartUpload_owner,
    multipartUpload_initiator,

    -- ** NoncurrentVersionExpiration
    noncurrentVersionExpiration_noncurrentDays,

    -- ** NoncurrentVersionTransition
    noncurrentVersionTransition_noncurrentDays,
    noncurrentVersionTransition_storageClass,

    -- ** NotificationConfiguration
    notificationConfiguration_lambdaFunctionConfigurations,
    notificationConfiguration_queueConfigurations,
    notificationConfiguration_topicConfigurations,

    -- ** NotificationConfigurationFilter
    notificationConfigurationFilter_key,

    -- ** Object
    object_owner,
    object_eTag,
    object_size,
    object_key,
    object_storageClass,
    object_lastModified,

    -- ** ObjectIdentifier
    objectIdentifier_versionId,
    objectIdentifier_key,

    -- ** ObjectLockConfiguration
    objectLockConfiguration_rule,
    objectLockConfiguration_objectLockEnabled,

    -- ** ObjectLockLegalHold
    objectLockLegalHold_status,

    -- ** ObjectLockRetention
    objectLockRetention_mode,
    objectLockRetention_retainUntilDate,

    -- ** ObjectLockRule
    objectLockRule_defaultRetention,

    -- ** ObjectVersion
    objectVersion_eTag,
    objectVersion_key,
    objectVersion_isLatest,
    objectVersion_storageClass,
    objectVersion_versionId,
    objectVersion_owner,
    objectVersion_lastModified,
    objectVersion_size,

    -- ** OutputLocation
    outputLocation_s3,

    -- ** OutputSerialization
    outputSerialization_csv,
    outputSerialization_json,

    -- ** Owner
    owner_id,
    owner_displayName,

    -- ** OwnershipControls
    ownershipControls_rules,

    -- ** OwnershipControlsRule
    ownershipControlsRule_objectOwnership,

    -- ** ParquetInput

    -- ** Part
    part_eTag,
    part_partNumber,
    part_lastModified,
    part_size,

    -- ** PolicyStatus
    policyStatus_isPublic,

    -- ** Progress
    progress_bytesScanned,
    progress_bytesProcessed,
    progress_bytesReturned,

    -- ** ProgressEvent
    progressEvent_details,

    -- ** PublicAccessBlockConfiguration
    publicAccessBlockConfiguration_ignorePublicAcls,
    publicAccessBlockConfiguration_blockPublicAcls,
    publicAccessBlockConfiguration_restrictPublicBuckets,
    publicAccessBlockConfiguration_blockPublicPolicy,

    -- ** QueueConfiguration
    queueConfiguration_id,
    queueConfiguration_filter,
    queueConfiguration_queueArn,
    queueConfiguration_events,

    -- ** RecordsEvent
    recordsEvent_payload,

    -- ** Redirect
    redirect_hostName,
    redirect_httpRedirectCode,
    redirect_replaceKeyPrefixWith,
    redirect_replaceKeyWith,
    redirect_protocol,

    -- ** RedirectAllRequestsTo
    redirectAllRequestsTo_protocol,
    redirectAllRequestsTo_hostName,

    -- ** ReplicaModifications
    replicaModifications_status,

    -- ** ReplicationConfiguration
    replicationConfiguration_role,
    replicationConfiguration_rules,

    -- ** ReplicationRule
    replicationRule_prefix,
    replicationRule_id,
    replicationRule_existingObjectReplication,
    replicationRule_priority,
    replicationRule_deleteMarkerReplication,
    replicationRule_sourceSelectionCriteria,
    replicationRule_filter,
    replicationRule_status,
    replicationRule_destination,

    -- ** ReplicationRuleAndOperator
    replicationRuleAndOperator_prefix,
    replicationRuleAndOperator_tags,

    -- ** ReplicationRuleFilter
    replicationRuleFilter_prefix,
    replicationRuleFilter_and,
    replicationRuleFilter_tag,

    -- ** ReplicationTime
    replicationTime_status,
    replicationTime_time,

    -- ** ReplicationTimeValue
    replicationTimeValue_minutes,

    -- ** RequestPaymentConfiguration
    requestPaymentConfiguration_payer,

    -- ** RequestProgress
    requestProgress_enabled,

    -- ** RestoreRequest
    restoreRequest_days,
    restoreRequest_selectParameters,
    restoreRequest_description,
    restoreRequest_type,
    restoreRequest_outputLocation,
    restoreRequest_tier,
    restoreRequest_glacierJobParameters,

    -- ** RoutingRule
    routingRule_condition,
    routingRule_redirect,

    -- ** S3KeyFilter
    s3KeyFilter_filterRules,

    -- ** S3Location
    s3Location_cannedACL,
    s3Location_encryption,
    s3Location_storageClass,
    s3Location_userMetadata,
    s3Location_accessControlList,
    s3Location_tagging,
    s3Location_bucketName,
    s3Location_prefix,

    -- ** S3ServiceError
    s3ServiceError_key,
    s3ServiceError_message,
    s3ServiceError_code,
    s3ServiceError_versionId,

    -- ** SSEKMS
    ssekms_keyId,

    -- ** SSES3

    -- ** ScanRange
    scanRange_end,
    scanRange_start,

    -- ** SelectObjectContentEventStream
    selectObjectContentEventStream_end,
    selectObjectContentEventStream_records,
    selectObjectContentEventStream_stats,
    selectObjectContentEventStream_cont,
    selectObjectContentEventStream_progress,

    -- ** SelectParameters
    selectParameters_inputSerialization,
    selectParameters_expressionType,
    selectParameters_expression,
    selectParameters_outputSerialization,

    -- ** ServerSideEncryptionByDefault
    serverSideEncryptionByDefault_kmsMasterKeyID,
    serverSideEncryptionByDefault_sSEAlgorithm,

    -- ** ServerSideEncryptionConfiguration
    serverSideEncryptionConfiguration_rules,

    -- ** ServerSideEncryptionRule
    serverSideEncryptionRule_bucketKeyEnabled,
    serverSideEncryptionRule_applyServerSideEncryptionByDefault,

    -- ** SourceSelectionCriteria
    sourceSelectionCriteria_replicaModifications,
    sourceSelectionCriteria_sseKmsEncryptedObjects,

    -- ** SseKmsEncryptedObjects
    sseKmsEncryptedObjects_status,

    -- ** Stats
    stats_bytesScanned,
    stats_bytesProcessed,
    stats_bytesReturned,

    -- ** StatsEvent
    statsEvent_details,

    -- ** StorageClassAnalysis
    storageClassAnalysis_dataExport,

    -- ** StorageClassAnalysisDataExport
    storageClassAnalysisDataExport_outputSchemaVersion,
    storageClassAnalysisDataExport_destination,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Tagging
    tagging_tagSet,

    -- ** TargetGrant
    targetGrant_grantee,
    targetGrant_permission,

    -- ** Tiering
    tiering_days,
    tiering_accessTier,

    -- ** TopicConfiguration
    topicConfiguration_id,
    topicConfiguration_filter,
    topicConfiguration_topicArn,
    topicConfiguration_events,

    -- ** Transition
    transition_days,
    transition_storageClass,
    transition_date,

    -- ** VersioningConfiguration
    versioningConfiguration_status,
    versioningConfiguration_mfaDelete,

    -- ** WebsiteConfiguration
    websiteConfiguration_errorDocument,
    websiteConfiguration_indexDocument,
    websiteConfiguration_routingRules,
    websiteConfiguration_redirectAllRequestsTo,
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
import Network.AWS.S3.Types.AbortIncompleteMultipartUpload
import Network.AWS.S3.Types.AccelerateConfiguration
import Network.AWS.S3.Types.AccessControlPolicy
import Network.AWS.S3.Types.AccessControlTranslation
import Network.AWS.S3.Types.AnalyticsAndOperator
import Network.AWS.S3.Types.AnalyticsConfiguration
import Network.AWS.S3.Types.AnalyticsExportDestination
import Network.AWS.S3.Types.AnalyticsFilter
import Network.AWS.S3.Types.AnalyticsS3BucketDestination
import Network.AWS.S3.Types.Bucket
import Network.AWS.S3.Types.BucketLifecycleConfiguration
import Network.AWS.S3.Types.BucketLoggingStatus
import Network.AWS.S3.Types.CORSConfiguration
import Network.AWS.S3.Types.CORSRule
import Network.AWS.S3.Types.CSVInput
import Network.AWS.S3.Types.CSVOutput
import Network.AWS.S3.Types.CommonPrefix
import Network.AWS.S3.Types.CompletedMultipartUpload
import Network.AWS.S3.Types.CompletedPart
import Network.AWS.S3.Types.Condition
import Network.AWS.S3.Types.ContinuationEvent
import Network.AWS.S3.Types.CopyObjectResult
import Network.AWS.S3.Types.CopyPartResult
import Network.AWS.S3.Types.CreateBucketConfiguration
import Network.AWS.S3.Types.DefaultRetention
import Network.AWS.S3.Types.Delete
import Network.AWS.S3.Types.DeleteMarkerEntry
import Network.AWS.S3.Types.DeleteMarkerReplication
import Network.AWS.S3.Types.DeletedObject
import Network.AWS.S3.Types.Destination
import Network.AWS.S3.Types.Encryption
import Network.AWS.S3.Types.EncryptionConfiguration
import Network.AWS.S3.Types.EndEvent
import Network.AWS.S3.Types.ErrorDocument
import Network.AWS.S3.Types.ExistingObjectReplication
import Network.AWS.S3.Types.FilterRule
import Network.AWS.S3.Types.GlacierJobParameters
import Network.AWS.S3.Types.Grant
import Network.AWS.S3.Types.Grantee
import Network.AWS.S3.Types.IndexDocument
import Network.AWS.S3.Types.Initiator
import Network.AWS.S3.Types.InputSerialization
import Network.AWS.S3.Types.IntelligentTieringAndOperator
import Network.AWS.S3.Types.IntelligentTieringConfiguration
import Network.AWS.S3.Types.IntelligentTieringFilter
import Network.AWS.S3.Types.InventoryConfiguration
import Network.AWS.S3.Types.InventoryDestination
import Network.AWS.S3.Types.InventoryEncryption
import Network.AWS.S3.Types.InventoryFilter
import Network.AWS.S3.Types.InventoryS3BucketDestination
import Network.AWS.S3.Types.InventorySchedule
import Network.AWS.S3.Types.JSONInput
import Network.AWS.S3.Types.JSONOutput
import Network.AWS.S3.Types.LambdaFunctionConfiguration
import Network.AWS.S3.Types.LifecycleExpiration
import Network.AWS.S3.Types.LifecycleRule
import Network.AWS.S3.Types.LifecycleRuleAndOperator
import Network.AWS.S3.Types.LifecycleRuleFilter
import Network.AWS.S3.Types.LoggingEnabled
import Network.AWS.S3.Types.MetadataEntry
import Network.AWS.S3.Types.Metrics
import Network.AWS.S3.Types.MetricsAndOperator
import Network.AWS.S3.Types.MetricsConfiguration
import Network.AWS.S3.Types.MetricsFilter
import Network.AWS.S3.Types.MultipartUpload
import Network.AWS.S3.Types.NoncurrentVersionExpiration
import Network.AWS.S3.Types.NoncurrentVersionTransition
import Network.AWS.S3.Types.NotificationConfiguration
import Network.AWS.S3.Types.NotificationConfigurationFilter
import Network.AWS.S3.Types.Object
import Network.AWS.S3.Types.ObjectIdentifier
import Network.AWS.S3.Types.ObjectLockConfiguration
import Network.AWS.S3.Types.ObjectLockLegalHold
import Network.AWS.S3.Types.ObjectLockRetention
import Network.AWS.S3.Types.ObjectLockRule
import Network.AWS.S3.Types.ObjectVersion
import Network.AWS.S3.Types.OutputLocation
import Network.AWS.S3.Types.OutputSerialization
import Network.AWS.S3.Types.Owner
import Network.AWS.S3.Types.OwnershipControls
import Network.AWS.S3.Types.OwnershipControlsRule
import Network.AWS.S3.Types.ParquetInput
import Network.AWS.S3.Types.Part
import Network.AWS.S3.Types.PolicyStatus
import Network.AWS.S3.Types.Progress
import Network.AWS.S3.Types.ProgressEvent
import Network.AWS.S3.Types.PublicAccessBlockConfiguration
import Network.AWS.S3.Types.QueueConfiguration
import Network.AWS.S3.Types.RecordsEvent
import Network.AWS.S3.Types.Redirect
import Network.AWS.S3.Types.RedirectAllRequestsTo
import Network.AWS.S3.Types.ReplicaModifications
import Network.AWS.S3.Types.ReplicationConfiguration
import Network.AWS.S3.Types.ReplicationRule
import Network.AWS.S3.Types.ReplicationRuleAndOperator
import Network.AWS.S3.Types.ReplicationRuleFilter
import Network.AWS.S3.Types.ReplicationTime
import Network.AWS.S3.Types.ReplicationTimeValue
import Network.AWS.S3.Types.RequestPaymentConfiguration
import Network.AWS.S3.Types.RequestProgress
import Network.AWS.S3.Types.RestoreRequest
import Network.AWS.S3.Types.RoutingRule
import Network.AWS.S3.Types.S3KeyFilter
import Network.AWS.S3.Types.S3Location
import Network.AWS.S3.Types.S3ServiceError
import Network.AWS.S3.Types.SSEKMS
import Network.AWS.S3.Types.SSES3
import Network.AWS.S3.Types.ScanRange
import Network.AWS.S3.Types.SelectObjectContentEventStream
import Network.AWS.S3.Types.SelectParameters
import Network.AWS.S3.Types.ServerSideEncryptionByDefault
import Network.AWS.S3.Types.ServerSideEncryptionConfiguration
import Network.AWS.S3.Types.ServerSideEncryptionRule
import Network.AWS.S3.Types.SourceSelectionCriteria
import Network.AWS.S3.Types.SseKmsEncryptedObjects
import Network.AWS.S3.Types.Stats
import Network.AWS.S3.Types.StatsEvent
import Network.AWS.S3.Types.StorageClassAnalysis
import Network.AWS.S3.Types.StorageClassAnalysisDataExport
import Network.AWS.S3.Types.Tag
import Network.AWS.S3.Types.Tagging
import Network.AWS.S3.Types.TargetGrant
import Network.AWS.S3.Types.Tiering
import Network.AWS.S3.Types.TopicConfiguration
import Network.AWS.S3.Types.Transition
import Network.AWS.S3.Types.VersioningConfiguration
import Network.AWS.S3.Types.WebsiteConfiguration
import Network.AWS.S3.UploadPart
import Network.AWS.S3.UploadPartCopy
