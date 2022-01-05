{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.S3.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Lens
  ( -- * Operations

    -- ** PutBucketRequestPayment
    putBucketRequestPayment_contentMD5,
    putBucketRequestPayment_expectedBucketOwner,
    putBucketRequestPayment_bucket,
    putBucketRequestPayment_requestPaymentConfiguration,

    -- ** PutObject
    putObject_contentLength,
    putObject_objectLockMode,
    putObject_expires,
    putObject_grantReadACP,
    putObject_sSECustomerAlgorithm,
    putObject_sSECustomerKey,
    putObject_requestPayer,
    putObject_grantWriteACP,
    putObject_bucketKeyEnabled,
    putObject_websiteRedirectLocation,
    putObject_grantRead,
    putObject_storageClass,
    putObject_sSECustomerKeyMD5,
    putObject_sSEKMSKeyId,
    putObject_grantFullControl,
    putObject_contentEncoding,
    putObject_tagging,
    putObject_contentMD5,
    putObject_objectLockRetainUntilDate,
    putObject_metadata,
    putObject_sSEKMSEncryptionContext,
    putObject_cacheControl,
    putObject_contentLanguage,
    putObject_objectLockLegalHoldStatus,
    putObject_acl,
    putObject_contentDisposition,
    putObject_expectedBucketOwner,
    putObject_serverSideEncryption,
    putObject_contentType,
    putObject_bucket,
    putObject_key,
    putObject_body,
    putObjectResponse_requestCharged,
    putObjectResponse_eTag,
    putObjectResponse_versionId,
    putObjectResponse_expiration,
    putObjectResponse_sSECustomerAlgorithm,
    putObjectResponse_bucketKeyEnabled,
    putObjectResponse_sSECustomerKeyMD5,
    putObjectResponse_sSEKMSKeyId,
    putObjectResponse_sSEKMSEncryptionContext,
    putObjectResponse_serverSideEncryption,
    putObjectResponse_httpStatus,

    -- ** DeleteObject
    deleteObject_versionId,
    deleteObject_mfa,
    deleteObject_requestPayer,
    deleteObject_bypassGovernanceRetention,
    deleteObject_expectedBucketOwner,
    deleteObject_bucket,
    deleteObject_key,
    deleteObjectResponse_requestCharged,
    deleteObjectResponse_versionId,
    deleteObjectResponse_deleteMarker,
    deleteObjectResponse_httpStatus,

    -- ** PutBucketLogging
    putBucketLogging_contentMD5,
    putBucketLogging_expectedBucketOwner,
    putBucketLogging_bucket,
    putBucketLogging_bucketLoggingStatus,

    -- ** GetBucketMetricsConfiguration
    getBucketMetricsConfiguration_expectedBucketOwner,
    getBucketMetricsConfiguration_bucket,
    getBucketMetricsConfiguration_id,
    getBucketMetricsConfigurationResponse_metricsConfiguration,
    getBucketMetricsConfigurationResponse_httpStatus,

    -- ** ListBuckets
    listBucketsResponse_buckets,
    listBucketsResponse_owner,
    listBucketsResponse_httpStatus,

    -- ** DeleteBucket
    deleteBucket_expectedBucketOwner,
    deleteBucket_bucket,

    -- ** CreateBucket
    createBucket_grantReadACP,
    createBucket_objectLockEnabledForBucket,
    createBucket_grantWriteACP,
    createBucket_grantRead,
    createBucket_grantFullControl,
    createBucket_createBucketConfiguration,
    createBucket_grantWrite,
    createBucket_acl,
    createBucket_bucket,
    createBucketResponse_location,
    createBucketResponse_httpStatus,

    -- ** DeleteBucketTagging
    deleteBucketTagging_expectedBucketOwner,
    deleteBucketTagging_bucket,

    -- ** PutObjectAcl
    putObjectAcl_versionId,
    putObjectAcl_grantReadACP,
    putObjectAcl_requestPayer,
    putObjectAcl_grantWriteACP,
    putObjectAcl_grantRead,
    putObjectAcl_grantFullControl,
    putObjectAcl_contentMD5,
    putObjectAcl_accessControlPolicy,
    putObjectAcl_grantWrite,
    putObjectAcl_acl,
    putObjectAcl_expectedBucketOwner,
    putObjectAcl_bucket,
    putObjectAcl_key,
    putObjectAclResponse_requestCharged,
    putObjectAclResponse_httpStatus,

    -- ** PutBucketTagging
    putBucketTagging_contentMD5,
    putBucketTagging_expectedBucketOwner,
    putBucketTagging_bucket,
    putBucketTagging_tagging,

    -- ** GetBucketInventoryConfiguration
    getBucketInventoryConfiguration_expectedBucketOwner,
    getBucketInventoryConfiguration_bucket,
    getBucketInventoryConfiguration_id,
    getBucketInventoryConfigurationResponse_inventoryConfiguration,
    getBucketInventoryConfigurationResponse_httpStatus,

    -- ** DeletePublicAccessBlock
    deletePublicAccessBlock_expectedBucketOwner,
    deletePublicAccessBlock_bucket,

    -- ** PutBucketInventoryConfiguration
    putBucketInventoryConfiguration_expectedBucketOwner,
    putBucketInventoryConfiguration_bucket,
    putBucketInventoryConfiguration_id,
    putBucketInventoryConfiguration_inventoryConfiguration,

    -- ** GetBucketLocation
    getBucketLocation_expectedBucketOwner,
    getBucketLocation_bucket,
    getBucketLocationResponse_httpStatus,
    getBucketLocationResponse_locationConstraint,

    -- ** ListBucketInventoryConfigurations
    listBucketInventoryConfigurations_continuationToken,
    listBucketInventoryConfigurations_expectedBucketOwner,
    listBucketInventoryConfigurations_bucket,
    listBucketInventoryConfigurationsResponse_continuationToken,
    listBucketInventoryConfigurationsResponse_inventoryConfigurationList,
    listBucketInventoryConfigurationsResponse_nextContinuationToken,
    listBucketInventoryConfigurationsResponse_isTruncated,
    listBucketInventoryConfigurationsResponse_httpStatus,

    -- ** PutPublicAccessBlock
    putPublicAccessBlock_contentMD5,
    putPublicAccessBlock_expectedBucketOwner,
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

    -- ** GetBucketNotificationConfiguration
    getBucketNotificationConfiguration_expectedBucketOwner,
    getBucketNotificationConfiguration_bucket,
    notificationConfiguration_queueConfigurations,
    notificationConfiguration_topicConfigurations,
    notificationConfiguration_lambdaFunctionConfigurations,

    -- ** GetObjectLockConfiguration
    getObjectLockConfiguration_expectedBucketOwner,
    getObjectLockConfiguration_bucket,
    getObjectLockConfigurationResponse_objectLockConfiguration,
    getObjectLockConfigurationResponse_httpStatus,

    -- ** PutObjectRetention
    putObjectRetention_retention,
    putObjectRetention_versionId,
    putObjectRetention_requestPayer,
    putObjectRetention_contentMD5,
    putObjectRetention_bypassGovernanceRetention,
    putObjectRetention_expectedBucketOwner,
    putObjectRetention_bucket,
    putObjectRetention_key,
    putObjectRetentionResponse_requestCharged,
    putObjectRetentionResponse_httpStatus,

    -- ** PutBucketAccelerateConfiguration
    putBucketAccelerateConfiguration_expectedBucketOwner,
    putBucketAccelerateConfiguration_bucket,
    putBucketAccelerateConfiguration_accelerateConfiguration,

    -- ** PutObjectLegalHold
    putObjectLegalHold_legalHold,
    putObjectLegalHold_versionId,
    putObjectLegalHold_requestPayer,
    putObjectLegalHold_contentMD5,
    putObjectLegalHold_expectedBucketOwner,
    putObjectLegalHold_bucket,
    putObjectLegalHold_key,
    putObjectLegalHoldResponse_requestCharged,
    putObjectLegalHoldResponse_httpStatus,

    -- ** PutBucketOwnershipControls
    putBucketOwnershipControls_contentMD5,
    putBucketOwnershipControls_expectedBucketOwner,
    putBucketOwnershipControls_bucket,
    putBucketOwnershipControls_ownershipControls,

    -- ** DeleteBucketOwnershipControls
    deleteBucketOwnershipControls_expectedBucketOwner,
    deleteBucketOwnershipControls_bucket,

    -- ** PutBucketMetricsConfiguration
    putBucketMetricsConfiguration_expectedBucketOwner,
    putBucketMetricsConfiguration_bucket,
    putBucketMetricsConfiguration_id,
    putBucketMetricsConfiguration_metricsConfiguration,

    -- ** DeleteBucketMetricsConfiguration
    deleteBucketMetricsConfiguration_expectedBucketOwner,
    deleteBucketMetricsConfiguration_bucket,
    deleteBucketMetricsConfiguration_id,

    -- ** ListObjectsV2
    listObjectsV2_startAfter,
    listObjectsV2_continuationToken,
    listObjectsV2_fetchOwner,
    listObjectsV2_prefix,
    listObjectsV2_encodingType,
    listObjectsV2_requestPayer,
    listObjectsV2_maxKeys,
    listObjectsV2_delimiter,
    listObjectsV2_expectedBucketOwner,
    listObjectsV2_bucket,
    listObjectsV2Response_startAfter,
    listObjectsV2Response_keyCount,
    listObjectsV2Response_contents,
    listObjectsV2Response_continuationToken,
    listObjectsV2Response_prefix,
    listObjectsV2Response_commonPrefixes,
    listObjectsV2Response_encodingType,
    listObjectsV2Response_name,
    listObjectsV2Response_nextContinuationToken,
    listObjectsV2Response_maxKeys,
    listObjectsV2Response_isTruncated,
    listObjectsV2Response_delimiter,
    listObjectsV2Response_httpStatus,

    -- ** GetObject
    getObject_ifMatch,
    getObject_versionId,
    getObject_responseContentType,
    getObject_responseContentDisposition,
    getObject_responseContentLanguage,
    getObject_sSECustomerAlgorithm,
    getObject_sSECustomerKey,
    getObject_requestPayer,
    getObject_responseContentEncoding,
    getObject_ifModifiedSince,
    getObject_partNumber,
    getObject_range,
    getObject_ifUnmodifiedSince,
    getObject_sSECustomerKeyMD5,
    getObject_responseCacheControl,
    getObject_responseExpires,
    getObject_ifNoneMatch,
    getObject_expectedBucketOwner,
    getObject_bucket,
    getObject_key,
    getObjectResponse_requestCharged,
    getObjectResponse_partsCount,
    getObjectResponse_eTag,
    getObjectResponse_versionId,
    getObjectResponse_contentLength,
    getObjectResponse_objectLockMode,
    getObjectResponse_expires,
    getObjectResponse_restore,
    getObjectResponse_expiration,
    getObjectResponse_deleteMarker,
    getObjectResponse_sSECustomerAlgorithm,
    getObjectResponse_tagCount,
    getObjectResponse_missingMeta,
    getObjectResponse_bucketKeyEnabled,
    getObjectResponse_websiteRedirectLocation,
    getObjectResponse_acceptRanges,
    getObjectResponse_storageClass,
    getObjectResponse_sSECustomerKeyMD5,
    getObjectResponse_sSEKMSKeyId,
    getObjectResponse_contentEncoding,
    getObjectResponse_objectLockRetainUntilDate,
    getObjectResponse_metadata,
    getObjectResponse_replicationStatus,
    getObjectResponse_cacheControl,
    getObjectResponse_contentLanguage,
    getObjectResponse_lastModified,
    getObjectResponse_objectLockLegalHoldStatus,
    getObjectResponse_contentDisposition,
    getObjectResponse_contentRange,
    getObjectResponse_serverSideEncryption,
    getObjectResponse_contentType,
    getObjectResponse_httpStatus,
    getObjectResponse_body,

    -- ** PutBucketReplication
    putBucketReplication_token,
    putBucketReplication_contentMD5,
    putBucketReplication_expectedBucketOwner,
    putBucketReplication_bucket,
    putBucketReplication_replicationConfiguration,

    -- ** GetBucketWebsite
    getBucketWebsite_expectedBucketOwner,
    getBucketWebsite_bucket,
    getBucketWebsiteResponse_redirectAllRequestsTo,
    getBucketWebsiteResponse_errorDocument,
    getBucketWebsiteResponse_indexDocument,
    getBucketWebsiteResponse_routingRules,
    getBucketWebsiteResponse_httpStatus,

    -- ** GetBucketRequestPayment
    getBucketRequestPayment_expectedBucketOwner,
    getBucketRequestPayment_bucket,
    getBucketRequestPaymentResponse_payer,
    getBucketRequestPaymentResponse_httpStatus,

    -- ** DeleteBucketReplication
    deleteBucketReplication_expectedBucketOwner,
    deleteBucketReplication_bucket,

    -- ** ListObjectVersions
    listObjectVersions_keyMarker,
    listObjectVersions_prefix,
    listObjectVersions_encodingType,
    listObjectVersions_versionIdMarker,
    listObjectVersions_maxKeys,
    listObjectVersions_delimiter,
    listObjectVersions_expectedBucketOwner,
    listObjectVersions_bucket,
    listObjectVersionsResponse_nextVersionIdMarker,
    listObjectVersionsResponse_keyMarker,
    listObjectVersionsResponse_deleteMarkers,
    listObjectVersionsResponse_prefix,
    listObjectVersionsResponse_commonPrefixes,
    listObjectVersionsResponse_encodingType,
    listObjectVersionsResponse_versions,
    listObjectVersionsResponse_name,
    listObjectVersionsResponse_nextKeyMarker,
    listObjectVersionsResponse_versionIdMarker,
    listObjectVersionsResponse_maxKeys,
    listObjectVersionsResponse_isTruncated,
    listObjectVersionsResponse_delimiter,
    listObjectVersionsResponse_httpStatus,

    -- ** HeadBucket
    headBucket_expectedBucketOwner,
    headBucket_bucket,

    -- ** DeleteBucketLifecycle
    deleteBucketLifecycle_expectedBucketOwner,
    deleteBucketLifecycle_bucket,

    -- ** PutBucketLifecycleConfiguration
    putBucketLifecycleConfiguration_lifecycleConfiguration,
    putBucketLifecycleConfiguration_expectedBucketOwner,
    putBucketLifecycleConfiguration_bucket,

    -- ** PutBucketAnalyticsConfiguration
    putBucketAnalyticsConfiguration_expectedBucketOwner,
    putBucketAnalyticsConfiguration_bucket,
    putBucketAnalyticsConfiguration_id,
    putBucketAnalyticsConfiguration_analyticsConfiguration,

    -- ** ListBucketAnalyticsConfigurations
    listBucketAnalyticsConfigurations_continuationToken,
    listBucketAnalyticsConfigurations_expectedBucketOwner,
    listBucketAnalyticsConfigurations_bucket,
    listBucketAnalyticsConfigurationsResponse_analyticsConfigurationList,
    listBucketAnalyticsConfigurationsResponse_continuationToken,
    listBucketAnalyticsConfigurationsResponse_nextContinuationToken,
    listBucketAnalyticsConfigurationsResponse_isTruncated,
    listBucketAnalyticsConfigurationsResponse_httpStatus,

    -- ** DeleteBucketAnalyticsConfiguration
    deleteBucketAnalyticsConfiguration_expectedBucketOwner,
    deleteBucketAnalyticsConfiguration_bucket,
    deleteBucketAnalyticsConfiguration_id,

    -- ** CreateMultipartUpload
    createMultipartUpload_objectLockMode,
    createMultipartUpload_expires,
    createMultipartUpload_grantReadACP,
    createMultipartUpload_sSECustomerAlgorithm,
    createMultipartUpload_sSECustomerKey,
    createMultipartUpload_requestPayer,
    createMultipartUpload_grantWriteACP,
    createMultipartUpload_bucketKeyEnabled,
    createMultipartUpload_websiteRedirectLocation,
    createMultipartUpload_grantRead,
    createMultipartUpload_storageClass,
    createMultipartUpload_sSECustomerKeyMD5,
    createMultipartUpload_sSEKMSKeyId,
    createMultipartUpload_grantFullControl,
    createMultipartUpload_contentEncoding,
    createMultipartUpload_tagging,
    createMultipartUpload_objectLockRetainUntilDate,
    createMultipartUpload_metadata,
    createMultipartUpload_sSEKMSEncryptionContext,
    createMultipartUpload_cacheControl,
    createMultipartUpload_contentLanguage,
    createMultipartUpload_objectLockLegalHoldStatus,
    createMultipartUpload_acl,
    createMultipartUpload_contentDisposition,
    createMultipartUpload_expectedBucketOwner,
    createMultipartUpload_serverSideEncryption,
    createMultipartUpload_contentType,
    createMultipartUpload_bucket,
    createMultipartUpload_key,
    createMultipartUploadResponse_requestCharged,
    createMultipartUploadResponse_bucket,
    createMultipartUploadResponse_sSECustomerAlgorithm,
    createMultipartUploadResponse_abortDate,
    createMultipartUploadResponse_abortRuleId,
    createMultipartUploadResponse_bucketKeyEnabled,
    createMultipartUploadResponse_key,
    createMultipartUploadResponse_sSECustomerKeyMD5,
    createMultipartUploadResponse_sSEKMSKeyId,
    createMultipartUploadResponse_sSEKMSEncryptionContext,
    createMultipartUploadResponse_serverSideEncryption,
    createMultipartUploadResponse_httpStatus,
    createMultipartUploadResponse_uploadId,

    -- ** GetBucketPolicyStatus
    getBucketPolicyStatus_expectedBucketOwner,
    getBucketPolicyStatus_bucket,
    getBucketPolicyStatusResponse_policyStatus,
    getBucketPolicyStatusResponse_httpStatus,

    -- ** UploadPart
    uploadPart_contentLength,
    uploadPart_sSECustomerAlgorithm,
    uploadPart_sSECustomerKey,
    uploadPart_requestPayer,
    uploadPart_sSECustomerKeyMD5,
    uploadPart_contentMD5,
    uploadPart_expectedBucketOwner,
    uploadPart_bucket,
    uploadPart_key,
    uploadPart_partNumber,
    uploadPart_uploadId,
    uploadPart_body,
    uploadPartResponse_requestCharged,
    uploadPartResponse_eTag,
    uploadPartResponse_sSECustomerAlgorithm,
    uploadPartResponse_bucketKeyEnabled,
    uploadPartResponse_sSECustomerKeyMD5,
    uploadPartResponse_sSEKMSKeyId,
    uploadPartResponse_serverSideEncryption,
    uploadPartResponse_httpStatus,

    -- ** SelectObjectContent
    selectObjectContent_sSECustomerAlgorithm,
    selectObjectContent_sSECustomerKey,
    selectObjectContent_requestProgress,
    selectObjectContent_sSECustomerKeyMD5,
    selectObjectContent_scanRange,
    selectObjectContent_expectedBucketOwner,
    selectObjectContent_bucket,
    selectObjectContent_key,
    selectObjectContent_expression,
    selectObjectContent_expressionType,
    selectObjectContent_inputSerialization,
    selectObjectContent_outputSerialization,
    selectObjectContentResponse_payload,
    selectObjectContentResponse_httpStatus,

    -- ** GetBucketReplication
    getBucketReplication_expectedBucketOwner,
    getBucketReplication_bucket,
    getBucketReplicationResponse_replicationConfiguration,
    getBucketReplicationResponse_httpStatus,

    -- ** PutBucketWebsite
    putBucketWebsite_contentMD5,
    putBucketWebsite_expectedBucketOwner,
    putBucketWebsite_bucket,
    putBucketWebsite_websiteConfiguration,

    -- ** DeleteBucketWebsite
    deleteBucketWebsite_expectedBucketOwner,
    deleteBucketWebsite_bucket,

    -- ** CompleteMultipartUpload
    completeMultipartUpload_requestPayer,
    completeMultipartUpload_multipartUpload,
    completeMultipartUpload_expectedBucketOwner,
    completeMultipartUpload_bucket,
    completeMultipartUpload_key,
    completeMultipartUpload_uploadId,
    completeMultipartUploadResponse_requestCharged,
    completeMultipartUploadResponse_eTag,
    completeMultipartUploadResponse_versionId,
    completeMultipartUploadResponse_location,
    completeMultipartUploadResponse_expiration,
    completeMultipartUploadResponse_bucket,
    completeMultipartUploadResponse_bucketKeyEnabled,
    completeMultipartUploadResponse_key,
    completeMultipartUploadResponse_sSEKMSKeyId,
    completeMultipartUploadResponse_serverSideEncryption,
    completeMultipartUploadResponse_httpStatus,

    -- ** ListMultipartUploads
    listMultipartUploads_keyMarker,
    listMultipartUploads_prefix,
    listMultipartUploads_encodingType,
    listMultipartUploads_uploadIdMarker,
    listMultipartUploads_maxUploads,
    listMultipartUploads_delimiter,
    listMultipartUploads_expectedBucketOwner,
    listMultipartUploads_bucket,
    listMultipartUploadsResponse_keyMarker,
    listMultipartUploadsResponse_prefix,
    listMultipartUploadsResponse_commonPrefixes,
    listMultipartUploadsResponse_encodingType,
    listMultipartUploadsResponse_bucket,
    listMultipartUploadsResponse_uploadIdMarker,
    listMultipartUploadsResponse_maxUploads,
    listMultipartUploadsResponse_nextKeyMarker,
    listMultipartUploadsResponse_uploads,
    listMultipartUploadsResponse_isTruncated,
    listMultipartUploadsResponse_nextUploadIdMarker,
    listMultipartUploadsResponse_delimiter,
    listMultipartUploadsResponse_httpStatus,

    -- ** ListObjects
    listObjects_prefix,
    listObjects_encodingType,
    listObjects_requestPayer,
    listObjects_marker,
    listObjects_maxKeys,
    listObjects_delimiter,
    listObjects_expectedBucketOwner,
    listObjects_bucket,
    listObjectsResponse_contents,
    listObjectsResponse_prefix,
    listObjectsResponse_commonPrefixes,
    listObjectsResponse_encodingType,
    listObjectsResponse_name,
    listObjectsResponse_marker,
    listObjectsResponse_nextMarker,
    listObjectsResponse_maxKeys,
    listObjectsResponse_isTruncated,
    listObjectsResponse_delimiter,
    listObjectsResponse_httpStatus,

    -- ** GetBucketOwnershipControls
    getBucketOwnershipControls_expectedBucketOwner,
    getBucketOwnershipControls_bucket,
    getBucketOwnershipControlsResponse_ownershipControls,
    getBucketOwnershipControlsResponse_httpStatus,

    -- ** GetObjectLegalHold
    getObjectLegalHold_versionId,
    getObjectLegalHold_requestPayer,
    getObjectLegalHold_expectedBucketOwner,
    getObjectLegalHold_bucket,
    getObjectLegalHold_key,
    getObjectLegalHoldResponse_legalHold,
    getObjectLegalHoldResponse_httpStatus,

    -- ** GetObjectRetention
    getObjectRetention_versionId,
    getObjectRetention_requestPayer,
    getObjectRetention_expectedBucketOwner,
    getObjectRetention_bucket,
    getObjectRetention_key,
    getObjectRetentionResponse_retention,
    getObjectRetentionResponse_httpStatus,

    -- ** DeleteBucketPolicy
    deleteBucketPolicy_expectedBucketOwner,
    deleteBucketPolicy_bucket,

    -- ** GetBucketEncryption
    getBucketEncryption_expectedBucketOwner,
    getBucketEncryption_bucket,
    getBucketEncryptionResponse_serverSideEncryptionConfiguration,
    getBucketEncryptionResponse_httpStatus,

    -- ** AbortMultipartUpload
    abortMultipartUpload_requestPayer,
    abortMultipartUpload_expectedBucketOwner,
    abortMultipartUpload_bucket,
    abortMultipartUpload_key,
    abortMultipartUpload_uploadId,
    abortMultipartUploadResponse_requestCharged,
    abortMultipartUploadResponse_httpStatus,

    -- ** PutBucketPolicy
    putBucketPolicy_confirmRemoveSelfBucketAccess,
    putBucketPolicy_contentMD5,
    putBucketPolicy_expectedBucketOwner,
    putBucketPolicy_bucket,
    putBucketPolicy_policy,

    -- ** GetBucketAccelerateConfiguration
    getBucketAccelerateConfiguration_expectedBucketOwner,
    getBucketAccelerateConfiguration_bucket,
    getBucketAccelerateConfigurationResponse_status,
    getBucketAccelerateConfigurationResponse_httpStatus,

    -- ** GetObjectTorrent
    getObjectTorrent_requestPayer,
    getObjectTorrent_expectedBucketOwner,
    getObjectTorrent_bucket,
    getObjectTorrent_key,
    getObjectTorrentResponse_requestCharged,
    getObjectTorrentResponse_httpStatus,
    getObjectTorrentResponse_body,

    -- ** DeleteObjects
    deleteObjects_mfa,
    deleteObjects_requestPayer,
    deleteObjects_bypassGovernanceRetention,
    deleteObjects_expectedBucketOwner,
    deleteObjects_bucket,
    deleteObjects_delete,
    deleteObjectsResponse_requestCharged,
    deleteObjectsResponse_deleted,
    deleteObjectsResponse_errors,
    deleteObjectsResponse_httpStatus,

    -- ** PutObjectLockConfiguration
    putObjectLockConfiguration_token,
    putObjectLockConfiguration_objectLockConfiguration,
    putObjectLockConfiguration_requestPayer,
    putObjectLockConfiguration_contentMD5,
    putObjectLockConfiguration_expectedBucketOwner,
    putObjectLockConfiguration_bucket,
    putObjectLockConfigurationResponse_requestCharged,
    putObjectLockConfigurationResponse_httpStatus,

    -- ** PutBucketNotificationConfiguration
    putBucketNotificationConfiguration_expectedBucketOwner,
    putBucketNotificationConfiguration_bucket,
    putBucketNotificationConfiguration_notificationConfiguration,

    -- ** GetBucketVersioning
    getBucketVersioning_expectedBucketOwner,
    getBucketVersioning_bucket,
    getBucketVersioningResponse_status,
    getBucketVersioningResponse_mfaDelete,
    getBucketVersioningResponse_httpStatus,

    -- ** DeleteBucketCors
    deleteBucketCors_expectedBucketOwner,
    deleteBucketCors_bucket,

    -- ** DeleteBucketIntelligentTieringConfiguration
    deleteBucketIntelligentTieringConfiguration_bucket,
    deleteBucketIntelligentTieringConfiguration_id,

    -- ** ListBucketIntelligentTieringConfigurations
    listBucketIntelligentTieringConfigurations_continuationToken,
    listBucketIntelligentTieringConfigurations_bucket,
    listBucketIntelligentTieringConfigurationsResponse_intelligentTieringConfigurationList,
    listBucketIntelligentTieringConfigurationsResponse_continuationToken,
    listBucketIntelligentTieringConfigurationsResponse_nextContinuationToken,
    listBucketIntelligentTieringConfigurationsResponse_isTruncated,
    listBucketIntelligentTieringConfigurationsResponse_httpStatus,

    -- ** PutBucketCors
    putBucketCors_contentMD5,
    putBucketCors_expectedBucketOwner,
    putBucketCors_bucket,
    putBucketCors_cORSConfiguration,

    -- ** GetPublicAccessBlock
    getPublicAccessBlock_expectedBucketOwner,
    getPublicAccessBlock_bucket,
    getPublicAccessBlockResponse_publicAccessBlockConfiguration,
    getPublicAccessBlockResponse_httpStatus,

    -- ** PutBucketIntelligentTieringConfiguration
    putBucketIntelligentTieringConfiguration_bucket,
    putBucketIntelligentTieringConfiguration_id,
    putBucketIntelligentTieringConfiguration_intelligentTieringConfiguration,

    -- ** GetBucketCors
    getBucketCors_expectedBucketOwner,
    getBucketCors_bucket,
    getBucketCorsResponse_cORSRules,
    getBucketCorsResponse_httpStatus,

    -- ** WriteGetObjectResponse
    writeGetObjectResponse_requestCharged,
    writeGetObjectResponse_partsCount,
    writeGetObjectResponse_eTag,
    writeGetObjectResponse_versionId,
    writeGetObjectResponse_contentLength,
    writeGetObjectResponse_objectLockMode,
    writeGetObjectResponse_expires,
    writeGetObjectResponse_restore,
    writeGetObjectResponse_expiration,
    writeGetObjectResponse_deleteMarker,
    writeGetObjectResponse_sSECustomerAlgorithm,
    writeGetObjectResponse_tagCount,
    writeGetObjectResponse_missingMeta,
    writeGetObjectResponse_bucketKeyEnabled,
    writeGetObjectResponse_acceptRanges,
    writeGetObjectResponse_storageClass,
    writeGetObjectResponse_sSECustomerKeyMD5,
    writeGetObjectResponse_sSEKMSKeyId,
    writeGetObjectResponse_contentEncoding,
    writeGetObjectResponse_errorCode,
    writeGetObjectResponse_objectLockRetainUntilDate,
    writeGetObjectResponse_metadata,
    writeGetObjectResponse_replicationStatus,
    writeGetObjectResponse_cacheControl,
    writeGetObjectResponse_contentLanguage,
    writeGetObjectResponse_errorMessage,
    writeGetObjectResponse_lastModified,
    writeGetObjectResponse_objectLockLegalHoldStatus,
    writeGetObjectResponse_contentDisposition,
    writeGetObjectResponse_contentRange,
    writeGetObjectResponse_serverSideEncryption,
    writeGetObjectResponse_contentType,
    writeGetObjectResponse_statusCode,
    writeGetObjectResponse_requestRoute,
    writeGetObjectResponse_requestToken,
    writeGetObjectResponse_body,

    -- ** GetObjectAcl
    getObjectAcl_versionId,
    getObjectAcl_requestPayer,
    getObjectAcl_expectedBucketOwner,
    getObjectAcl_bucket,
    getObjectAcl_key,
    getObjectAclResponse_requestCharged,
    getObjectAclResponse_grants,
    getObjectAclResponse_owner,
    getObjectAclResponse_httpStatus,

    -- ** RestoreObject
    restoreObject_versionId,
    restoreObject_requestPayer,
    restoreObject_expectedBucketOwner,
    restoreObject_restoreRequest,
    restoreObject_bucket,
    restoreObject_key,
    restoreObjectResponse_requestCharged,
    restoreObjectResponse_restoreOutputPath,
    restoreObjectResponse_httpStatus,

    -- ** HeadObject
    headObject_ifMatch,
    headObject_versionId,
    headObject_sSECustomerAlgorithm,
    headObject_sSECustomerKey,
    headObject_requestPayer,
    headObject_ifModifiedSince,
    headObject_partNumber,
    headObject_range,
    headObject_ifUnmodifiedSince,
    headObject_sSECustomerKeyMD5,
    headObject_ifNoneMatch,
    headObject_expectedBucketOwner,
    headObject_bucket,
    headObject_key,
    headObjectResponse_requestCharged,
    headObjectResponse_partsCount,
    headObjectResponse_eTag,
    headObjectResponse_versionId,
    headObjectResponse_contentLength,
    headObjectResponse_objectLockMode,
    headObjectResponse_expires,
    headObjectResponse_restore,
    headObjectResponse_expiration,
    headObjectResponse_deleteMarker,
    headObjectResponse_archiveStatus,
    headObjectResponse_sSECustomerAlgorithm,
    headObjectResponse_missingMeta,
    headObjectResponse_bucketKeyEnabled,
    headObjectResponse_websiteRedirectLocation,
    headObjectResponse_acceptRanges,
    headObjectResponse_storageClass,
    headObjectResponse_sSECustomerKeyMD5,
    headObjectResponse_sSEKMSKeyId,
    headObjectResponse_contentEncoding,
    headObjectResponse_objectLockRetainUntilDate,
    headObjectResponse_metadata,
    headObjectResponse_replicationStatus,
    headObjectResponse_cacheControl,
    headObjectResponse_contentLanguage,
    headObjectResponse_lastModified,
    headObjectResponse_objectLockLegalHoldStatus,
    headObjectResponse_contentDisposition,
    headObjectResponse_serverSideEncryption,
    headObjectResponse_contentType,
    headObjectResponse_httpStatus,

    -- ** PutBucketVersioning
    putBucketVersioning_mfa,
    putBucketVersioning_contentMD5,
    putBucketVersioning_expectedBucketOwner,
    putBucketVersioning_bucket,
    putBucketVersioning_versioningConfiguration,

    -- ** GetBucketTagging
    getBucketTagging_expectedBucketOwner,
    getBucketTagging_bucket,
    getBucketTaggingResponse_httpStatus,
    getBucketTaggingResponse_tagSet,

    -- ** CopyObject
    copyObject_copySourceIfModifiedSince,
    copyObject_copySourceIfUnmodifiedSince,
    copyObject_copySourceSSECustomerKeyMD5,
    copyObject_taggingDirective,
    copyObject_metadataDirective,
    copyObject_objectLockMode,
    copyObject_expires,
    copyObject_grantReadACP,
    copyObject_copySourceIfNoneMatch,
    copyObject_sSECustomerAlgorithm,
    copyObject_sSECustomerKey,
    copyObject_requestPayer,
    copyObject_grantWriteACP,
    copyObject_copySourceIfMatch,
    copyObject_bucketKeyEnabled,
    copyObject_websiteRedirectLocation,
    copyObject_grantRead,
    copyObject_expectedSourceBucketOwner,
    copyObject_storageClass,
    copyObject_sSECustomerKeyMD5,
    copyObject_sSEKMSKeyId,
    copyObject_grantFullControl,
    copyObject_contentEncoding,
    copyObject_tagging,
    copyObject_objectLockRetainUntilDate,
    copyObject_metadata,
    copyObject_sSEKMSEncryptionContext,
    copyObject_cacheControl,
    copyObject_contentLanguage,
    copyObject_copySourceSSECustomerKey,
    copyObject_objectLockLegalHoldStatus,
    copyObject_copySourceSSECustomerAlgorithm,
    copyObject_acl,
    copyObject_contentDisposition,
    copyObject_expectedBucketOwner,
    copyObject_serverSideEncryption,
    copyObject_contentType,
    copyObject_bucket,
    copyObject_copySource,
    copyObject_key,
    copyObjectResponse_requestCharged,
    copyObjectResponse_versionId,
    copyObjectResponse_expiration,
    copyObjectResponse_sSECustomerAlgorithm,
    copyObjectResponse_bucketKeyEnabled,
    copyObjectResponse_copySourceVersionId,
    copyObjectResponse_sSECustomerKeyMD5,
    copyObjectResponse_sSEKMSKeyId,
    copyObjectResponse_sSEKMSEncryptionContext,
    copyObjectResponse_serverSideEncryption,
    copyObjectResponse_copyObjectResult,
    copyObjectResponse_httpStatus,

    -- ** ListBucketMetricsConfigurations
    listBucketMetricsConfigurations_continuationToken,
    listBucketMetricsConfigurations_expectedBucketOwner,
    listBucketMetricsConfigurations_bucket,
    listBucketMetricsConfigurationsResponse_continuationToken,
    listBucketMetricsConfigurationsResponse_metricsConfigurationList,
    listBucketMetricsConfigurationsResponse_nextContinuationToken,
    listBucketMetricsConfigurationsResponse_isTruncated,
    listBucketMetricsConfigurationsResponse_httpStatus,

    -- ** GetBucketPolicy
    getBucketPolicy_expectedBucketOwner,
    getBucketPolicy_bucket,
    getBucketPolicyResponse_httpStatus,
    getBucketPolicyResponse_policy,

    -- ** PutBucketEncryption
    putBucketEncryption_contentMD5,
    putBucketEncryption_expectedBucketOwner,
    putBucketEncryption_bucket,
    putBucketEncryption_serverSideEncryptionConfiguration,

    -- ** DeleteBucketEncryption
    deleteBucketEncryption_expectedBucketOwner,
    deleteBucketEncryption_bucket,

    -- ** GetBucketLogging
    getBucketLogging_expectedBucketOwner,
    getBucketLogging_bucket,
    getBucketLoggingResponse_loggingEnabled,
    getBucketLoggingResponse_httpStatus,

    -- ** GetBucketAcl
    getBucketAcl_expectedBucketOwner,
    getBucketAcl_bucket,
    getBucketAclResponse_grants,
    getBucketAclResponse_owner,
    getBucketAclResponse_httpStatus,

    -- ** GetBucketLifecycleConfiguration
    getBucketLifecycleConfiguration_expectedBucketOwner,
    getBucketLifecycleConfiguration_bucket,
    getBucketLifecycleConfigurationResponse_rules,
    getBucketLifecycleConfigurationResponse_httpStatus,

    -- ** GetBucketAnalyticsConfiguration
    getBucketAnalyticsConfiguration_expectedBucketOwner,
    getBucketAnalyticsConfiguration_bucket,
    getBucketAnalyticsConfiguration_id,
    getBucketAnalyticsConfigurationResponse_analyticsConfiguration,
    getBucketAnalyticsConfigurationResponse_httpStatus,

    -- ** GetObjectTagging
    getObjectTagging_versionId,
    getObjectTagging_requestPayer,
    getObjectTagging_expectedBucketOwner,
    getObjectTagging_bucket,
    getObjectTagging_key,
    getObjectTaggingResponse_versionId,
    getObjectTaggingResponse_httpStatus,
    getObjectTaggingResponse_tagSet,

    -- ** ListParts
    listParts_maxParts,
    listParts_requestPayer,
    listParts_partNumberMarker,
    listParts_expectedBucketOwner,
    listParts_bucket,
    listParts_key,
    listParts_uploadId,
    listPartsResponse_parts,
    listPartsResponse_requestCharged,
    listPartsResponse_maxParts,
    listPartsResponse_initiator,
    listPartsResponse_bucket,
    listPartsResponse_abortDate,
    listPartsResponse_nextPartNumberMarker,
    listPartsResponse_abortRuleId,
    listPartsResponse_owner,
    listPartsResponse_key,
    listPartsResponse_storageClass,
    listPartsResponse_isTruncated,
    listPartsResponse_partNumberMarker,
    listPartsResponse_uploadId,
    listPartsResponse_httpStatus,

    -- ** DeleteObjectTagging
    deleteObjectTagging_versionId,
    deleteObjectTagging_expectedBucketOwner,
    deleteObjectTagging_bucket,
    deleteObjectTagging_key,
    deleteObjectTaggingResponse_versionId,
    deleteObjectTaggingResponse_httpStatus,

    -- ** UploadPartCopy
    uploadPartCopy_copySourceIfModifiedSince,
    uploadPartCopy_copySourceIfUnmodifiedSince,
    uploadPartCopy_copySourceRange,
    uploadPartCopy_copySourceSSECustomerKeyMD5,
    uploadPartCopy_copySourceIfNoneMatch,
    uploadPartCopy_sSECustomerAlgorithm,
    uploadPartCopy_sSECustomerKey,
    uploadPartCopy_requestPayer,
    uploadPartCopy_copySourceIfMatch,
    uploadPartCopy_expectedSourceBucketOwner,
    uploadPartCopy_sSECustomerKeyMD5,
    uploadPartCopy_copySourceSSECustomerKey,
    uploadPartCopy_copySourceSSECustomerAlgorithm,
    uploadPartCopy_expectedBucketOwner,
    uploadPartCopy_bucket,
    uploadPartCopy_copySource,
    uploadPartCopy_key,
    uploadPartCopy_partNumber,
    uploadPartCopy_uploadId,
    uploadPartCopyResponse_requestCharged,
    uploadPartCopyResponse_copyPartResult,
    uploadPartCopyResponse_sSECustomerAlgorithm,
    uploadPartCopyResponse_bucketKeyEnabled,
    uploadPartCopyResponse_copySourceVersionId,
    uploadPartCopyResponse_sSECustomerKeyMD5,
    uploadPartCopyResponse_sSEKMSKeyId,
    uploadPartCopyResponse_serverSideEncryption,
    uploadPartCopyResponse_httpStatus,

    -- ** PutObjectTagging
    putObjectTagging_versionId,
    putObjectTagging_requestPayer,
    putObjectTagging_contentMD5,
    putObjectTagging_expectedBucketOwner,
    putObjectTagging_bucket,
    putObjectTagging_key,
    putObjectTagging_tagging,
    putObjectTaggingResponse_versionId,
    putObjectTaggingResponse_httpStatus,

    -- ** PutBucketAcl
    putBucketAcl_grantReadACP,
    putBucketAcl_grantWriteACP,
    putBucketAcl_grantRead,
    putBucketAcl_grantFullControl,
    putBucketAcl_contentMD5,
    putBucketAcl_accessControlPolicy,
    putBucketAcl_grantWrite,
    putBucketAcl_acl,
    putBucketAcl_expectedBucketOwner,
    putBucketAcl_bucket,

    -- * Types

    -- ** AbortIncompleteMultipartUpload
    abortIncompleteMultipartUpload_daysAfterInitiation,

    -- ** AccelerateConfiguration
    accelerateConfiguration_status,

    -- ** AccessControlPolicy
    accessControlPolicy_grants,
    accessControlPolicy_owner,

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
    analyticsFilter_tag,
    analyticsFilter_prefix,
    analyticsFilter_and,

    -- ** AnalyticsS3BucketDestination
    analyticsS3BucketDestination_bucketAccountId,
    analyticsS3BucketDestination_prefix,
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
    cORSRule_maxAgeSeconds,
    cORSRule_allowedHeaders,
    cORSRule_exposeHeaders,
    cORSRule_id,
    cORSRule_allowedMethods,
    cORSRule_allowedOrigins,

    -- ** CSVInput
    cSVInput_quoteCharacter,
    cSVInput_recordDelimiter,
    cSVInput_allowQuotedRecordDelimiter,
    cSVInput_fileHeaderInfo,
    cSVInput_quoteEscapeCharacter,
    cSVInput_comments,
    cSVInput_fieldDelimiter,

    -- ** CSVOutput
    cSVOutput_quoteCharacter,
    cSVOutput_quoteFields,
    cSVOutput_recordDelimiter,
    cSVOutput_quoteEscapeCharacter,
    cSVOutput_fieldDelimiter,

    -- ** CommonPrefix
    commonPrefix_prefix,

    -- ** CompletedMultipartUpload
    completedMultipartUpload_parts,

    -- ** CompletedPart
    completedPart_partNumber,
    completedPart_eTag,

    -- ** Condition
    condition_keyPrefixEquals,
    condition_httpErrorCodeReturnedEquals,

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
    defaultRetention_mode,
    defaultRetention_years,

    -- ** Delete
    delete_quiet,
    delete_objects,

    -- ** DeleteMarkerEntry
    deleteMarkerEntry_versionId,
    deleteMarkerEntry_isLatest,
    deleteMarkerEntry_owner,
    deleteMarkerEntry_key,
    deleteMarkerEntry_lastModified,

    -- ** DeleteMarkerReplication
    deleteMarkerReplication_status,

    -- ** DeletedObject
    deletedObject_versionId,
    deletedObject_deleteMarker,
    deletedObject_deleteMarkerVersionId,
    deletedObject_key,

    -- ** Destination
    destination_metrics,
    destination_accessControlTranslation,
    destination_account,
    destination_storageClass,
    destination_encryptionConfiguration,
    destination_replicationTime,
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
    filterRule_value,
    filterRule_name,

    -- ** GlacierJobParameters
    glacierJobParameters_tier,

    -- ** Grant
    grant_permission,
    grant_grantee,

    -- ** Grantee
    grantee_uri,
    grantee_emailAddress,
    grantee_displayName,
    grantee_id,
    grantee_type,

    -- ** IndexDocument
    indexDocument_suffix,

    -- ** Initiator
    initiator_displayName,
    initiator_id,

    -- ** InputSerialization
    inputSerialization_json,
    inputSerialization_csv,
    inputSerialization_parquet,
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
    intelligentTieringFilter_tag,
    intelligentTieringFilter_prefix,
    intelligentTieringFilter_and,

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
    inventoryEncryption_sses3,
    inventoryEncryption_ssekms,

    -- ** InventoryFilter
    inventoryFilter_prefix,

    -- ** InventoryS3BucketDestination
    inventoryS3BucketDestination_prefix,
    inventoryS3BucketDestination_accountId,
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
    lifecycleExpiration_date,
    lifecycleExpiration_expiredObjectDeleteMarker,

    -- ** LifecycleRule
    lifecycleRule_transitions,
    lifecycleRule_noncurrentVersionExpiration,
    lifecycleRule_prefix,
    lifecycleRule_noncurrentVersionTransitions,
    lifecycleRule_expiration,
    lifecycleRule_id,
    lifecycleRule_filter,
    lifecycleRule_abortIncompleteMultipartUpload,
    lifecycleRule_status,

    -- ** LifecycleRuleAndOperator
    lifecycleRuleAndOperator_prefix,
    lifecycleRuleAndOperator_tags,

    -- ** LifecycleRuleFilter
    lifecycleRuleFilter_tag,
    lifecycleRuleFilter_prefix,
    lifecycleRuleFilter_and,

    -- ** LoggingEnabled
    loggingEnabled_targetGrants,
    loggingEnabled_targetBucket,
    loggingEnabled_targetPrefix,

    -- ** MetadataEntry
    metadataEntry_value,
    metadataEntry_name,

    -- ** Metrics
    metrics_eventThreshold,
    metrics_status,

    -- ** MetricsAndOperator
    metricsAndOperator_prefix,
    metricsAndOperator_accessPointArn,
    metricsAndOperator_tags,

    -- ** MetricsConfiguration
    metricsConfiguration_filter,
    metricsConfiguration_id,

    -- ** MetricsFilter
    metricsFilter_tag,
    metricsFilter_prefix,
    metricsFilter_and,
    metricsFilter_accessPointArn,

    -- ** MultipartUpload
    multipartUpload_initiated,
    multipartUpload_initiator,
    multipartUpload_owner,
    multipartUpload_key,
    multipartUpload_storageClass,
    multipartUpload_uploadId,

    -- ** NoncurrentVersionExpiration
    noncurrentVersionExpiration_noncurrentDays,

    -- ** NoncurrentVersionTransition
    noncurrentVersionTransition_noncurrentDays,
    noncurrentVersionTransition_storageClass,

    -- ** NotificationConfiguration
    notificationConfiguration_queueConfigurations,
    notificationConfiguration_topicConfigurations,
    notificationConfiguration_lambdaFunctionConfigurations,

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
    objectLockConfiguration_objectLockEnabled,
    objectLockConfiguration_rule,

    -- ** ObjectLockLegalHold
    objectLockLegalHold_status,

    -- ** ObjectLockRetention
    objectLockRetention_mode,
    objectLockRetention_retainUntilDate,

    -- ** ObjectLockRule
    objectLockRule_defaultRetention,

    -- ** ObjectVersion
    objectVersion_eTag,
    objectVersion_versionId,
    objectVersion_size,
    objectVersion_isLatest,
    objectVersion_owner,
    objectVersion_key,
    objectVersion_storageClass,
    objectVersion_lastModified,

    -- ** OutputLocation
    outputLocation_s3,

    -- ** OutputSerialization
    outputSerialization_json,
    outputSerialization_csv,

    -- ** Owner
    owner_displayName,
    owner_id,

    -- ** OwnershipControls
    ownershipControls_rules,

    -- ** OwnershipControlsRule
    ownershipControlsRule_objectOwnership,

    -- ** ParquetInput

    -- ** Part
    part_eTag,
    part_size,
    part_partNumber,
    part_lastModified,

    -- ** PolicyStatus
    policyStatus_isPublic,

    -- ** Progress
    progress_bytesReturned,
    progress_bytesScanned,
    progress_bytesProcessed,

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
    redirect_protocol,
    redirect_httpRedirectCode,
    redirect_replaceKeyWith,
    redirect_replaceKeyPrefixWith,

    -- ** RedirectAllRequestsTo
    redirectAllRequestsTo_protocol,
    redirectAllRequestsTo_hostName,

    -- ** ReplicaModifications
    replicaModifications_status,

    -- ** ReplicationConfiguration
    replicationConfiguration_role,
    replicationConfiguration_rules,

    -- ** ReplicationRule
    replicationRule_deleteMarkerReplication,
    replicationRule_priority,
    replicationRule_prefix,
    replicationRule_existingObjectReplication,
    replicationRule_id,
    replicationRule_filter,
    replicationRule_sourceSelectionCriteria,
    replicationRule_status,
    replicationRule_destination,

    -- ** ReplicationRuleAndOperator
    replicationRuleAndOperator_prefix,
    replicationRuleAndOperator_tags,

    -- ** ReplicationRuleFilter
    replicationRuleFilter_tag,
    replicationRuleFilter_prefix,
    replicationRuleFilter_and,

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
    restoreRequest_outputLocation,
    restoreRequest_tier,
    restoreRequest_glacierJobParameters,
    restoreRequest_type,
    restoreRequest_description,

    -- ** RoutingRule
    routingRule_condition,
    routingRule_redirect,

    -- ** S3KeyFilter
    s3KeyFilter_filterRules,

    -- ** S3Location
    s3Location_cannedACL,
    s3Location_accessControlList,
    s3Location_userMetadata,
    s3Location_encryption,
    s3Location_storageClass,
    s3Location_tagging,
    s3Location_bucketName,
    s3Location_prefix,

    -- ** S3ServiceError
    s3ServiceError_versionId,
    s3ServiceError_key,
    s3ServiceError_code,
    s3ServiceError_message,

    -- ** SSEKMS
    ssekms_keyId,

    -- ** SSES3

    -- ** ScanRange
    scanRange_start,
    scanRange_end,

    -- ** SelectObjectContentEventStream
    selectObjectContentEventStream_progress,
    selectObjectContentEventStream_records,
    selectObjectContentEventStream_cont,
    selectObjectContentEventStream_stats,
    selectObjectContentEventStream_end,

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
    serverSideEncryptionRule_applyServerSideEncryptionByDefault,
    serverSideEncryptionRule_bucketKeyEnabled,

    -- ** SourceSelectionCriteria
    sourceSelectionCriteria_replicaModifications,
    sourceSelectionCriteria_sseKmsEncryptedObjects,

    -- ** SseKmsEncryptedObjects
    sseKmsEncryptedObjects_status,

    -- ** Stats
    stats_bytesReturned,
    stats_bytesScanned,
    stats_bytesProcessed,

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
    targetGrant_permission,
    targetGrant_grantee,

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
    transition_date,
    transition_storageClass,

    -- ** VersioningConfiguration
    versioningConfiguration_status,
    versioningConfiguration_mfaDelete,

    -- ** WebsiteConfiguration
    websiteConfiguration_redirectAllRequestsTo,
    websiteConfiguration_errorDocument,
    websiteConfiguration_indexDocument,
    websiteConfiguration_routingRules,
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
import Amazonka.S3.Types.AbortIncompleteMultipartUpload
import Amazonka.S3.Types.AccelerateConfiguration
import Amazonka.S3.Types.AccessControlPolicy
import Amazonka.S3.Types.AccessControlTranslation
import Amazonka.S3.Types.AnalyticsAndOperator
import Amazonka.S3.Types.AnalyticsConfiguration
import Amazonka.S3.Types.AnalyticsExportDestination
import Amazonka.S3.Types.AnalyticsFilter
import Amazonka.S3.Types.AnalyticsS3BucketDestination
import Amazonka.S3.Types.Bucket
import Amazonka.S3.Types.BucketLifecycleConfiguration
import Amazonka.S3.Types.BucketLoggingStatus
import Amazonka.S3.Types.CORSConfiguration
import Amazonka.S3.Types.CORSRule
import Amazonka.S3.Types.CSVInput
import Amazonka.S3.Types.CSVOutput
import Amazonka.S3.Types.CommonPrefix
import Amazonka.S3.Types.CompletedMultipartUpload
import Amazonka.S3.Types.CompletedPart
import Amazonka.S3.Types.Condition
import Amazonka.S3.Types.ContinuationEvent
import Amazonka.S3.Types.CopyObjectResult
import Amazonka.S3.Types.CopyPartResult
import Amazonka.S3.Types.CreateBucketConfiguration
import Amazonka.S3.Types.DefaultRetention
import Amazonka.S3.Types.Delete
import Amazonka.S3.Types.DeleteMarkerEntry
import Amazonka.S3.Types.DeleteMarkerReplication
import Amazonka.S3.Types.DeletedObject
import Amazonka.S3.Types.Destination
import Amazonka.S3.Types.Encryption
import Amazonka.S3.Types.EncryptionConfiguration
import Amazonka.S3.Types.EndEvent
import Amazonka.S3.Types.ErrorDocument
import Amazonka.S3.Types.ExistingObjectReplication
import Amazonka.S3.Types.FilterRule
import Amazonka.S3.Types.GlacierJobParameters
import Amazonka.S3.Types.Grant
import Amazonka.S3.Types.Grantee
import Amazonka.S3.Types.IndexDocument
import Amazonka.S3.Types.Initiator
import Amazonka.S3.Types.InputSerialization
import Amazonka.S3.Types.IntelligentTieringAndOperator
import Amazonka.S3.Types.IntelligentTieringConfiguration
import Amazonka.S3.Types.IntelligentTieringFilter
import Amazonka.S3.Types.InventoryConfiguration
import Amazonka.S3.Types.InventoryDestination
import Amazonka.S3.Types.InventoryEncryption
import Amazonka.S3.Types.InventoryFilter
import Amazonka.S3.Types.InventoryS3BucketDestination
import Amazonka.S3.Types.InventorySchedule
import Amazonka.S3.Types.JSONInput
import Amazonka.S3.Types.JSONOutput
import Amazonka.S3.Types.LambdaFunctionConfiguration
import Amazonka.S3.Types.LifecycleExpiration
import Amazonka.S3.Types.LifecycleRule
import Amazonka.S3.Types.LifecycleRuleAndOperator
import Amazonka.S3.Types.LifecycleRuleFilter
import Amazonka.S3.Types.LoggingEnabled
import Amazonka.S3.Types.MetadataEntry
import Amazonka.S3.Types.Metrics
import Amazonka.S3.Types.MetricsAndOperator
import Amazonka.S3.Types.MetricsConfiguration
import Amazonka.S3.Types.MetricsFilter
import Amazonka.S3.Types.MultipartUpload
import Amazonka.S3.Types.NoncurrentVersionExpiration
import Amazonka.S3.Types.NoncurrentVersionTransition
import Amazonka.S3.Types.NotificationConfiguration
import Amazonka.S3.Types.NotificationConfigurationFilter
import Amazonka.S3.Types.Object
import Amazonka.S3.Types.ObjectIdentifier
import Amazonka.S3.Types.ObjectLockConfiguration
import Amazonka.S3.Types.ObjectLockLegalHold
import Amazonka.S3.Types.ObjectLockRetention
import Amazonka.S3.Types.ObjectLockRule
import Amazonka.S3.Types.ObjectVersion
import Amazonka.S3.Types.OutputLocation
import Amazonka.S3.Types.OutputSerialization
import Amazonka.S3.Types.Owner
import Amazonka.S3.Types.OwnershipControls
import Amazonka.S3.Types.OwnershipControlsRule
import Amazonka.S3.Types.ParquetInput
import Amazonka.S3.Types.Part
import Amazonka.S3.Types.PolicyStatus
import Amazonka.S3.Types.Progress
import Amazonka.S3.Types.ProgressEvent
import Amazonka.S3.Types.PublicAccessBlockConfiguration
import Amazonka.S3.Types.QueueConfiguration
import Amazonka.S3.Types.RecordsEvent
import Amazonka.S3.Types.Redirect
import Amazonka.S3.Types.RedirectAllRequestsTo
import Amazonka.S3.Types.ReplicaModifications
import Amazonka.S3.Types.ReplicationConfiguration
import Amazonka.S3.Types.ReplicationRule
import Amazonka.S3.Types.ReplicationRuleAndOperator
import Amazonka.S3.Types.ReplicationRuleFilter
import Amazonka.S3.Types.ReplicationTime
import Amazonka.S3.Types.ReplicationTimeValue
import Amazonka.S3.Types.RequestPaymentConfiguration
import Amazonka.S3.Types.RequestProgress
import Amazonka.S3.Types.RestoreRequest
import Amazonka.S3.Types.RoutingRule
import Amazonka.S3.Types.S3KeyFilter
import Amazonka.S3.Types.S3Location
import Amazonka.S3.Types.S3ServiceError
import Amazonka.S3.Types.SSEKMS
import Amazonka.S3.Types.SSES3
import Amazonka.S3.Types.ScanRange
import Amazonka.S3.Types.SelectObjectContentEventStream
import Amazonka.S3.Types.SelectParameters
import Amazonka.S3.Types.ServerSideEncryptionByDefault
import Amazonka.S3.Types.ServerSideEncryptionConfiguration
import Amazonka.S3.Types.ServerSideEncryptionRule
import Amazonka.S3.Types.SourceSelectionCriteria
import Amazonka.S3.Types.SseKmsEncryptedObjects
import Amazonka.S3.Types.Stats
import Amazonka.S3.Types.StatsEvent
import Amazonka.S3.Types.StorageClassAnalysis
import Amazonka.S3.Types.StorageClassAnalysisDataExport
import Amazonka.S3.Types.Tag
import Amazonka.S3.Types.Tagging
import Amazonka.S3.Types.TargetGrant
import Amazonka.S3.Types.Tiering
import Amazonka.S3.Types.TopicConfiguration
import Amazonka.S3.Types.Transition
import Amazonka.S3.Types.VersioningConfiguration
import Amazonka.S3.Types.WebsiteConfiguration
import Amazonka.S3.UploadPart
import Amazonka.S3.UploadPartCopy
import Amazonka.S3.WriteGetObjectResponse
