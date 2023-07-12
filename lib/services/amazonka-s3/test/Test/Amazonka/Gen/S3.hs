{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.S3
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.S3 where

import Amazonka.S3
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.S3.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAbortMultipartUpload $
--             newAbortMultipartUpload
--
--         , requestCompleteMultipartUpload $
--             newCompleteMultipartUpload
--
--         , requestCopyObject $
--             newCopyObject
--
--         , requestCreateBucket $
--             newCreateBucket
--
--         , requestCreateMultipartUpload $
--             newCreateMultipartUpload
--
--         , requestDeleteBucket $
--             newDeleteBucket
--
--         , requestDeleteBucketAnalyticsConfiguration $
--             newDeleteBucketAnalyticsConfiguration
--
--         , requestDeleteBucketCors $
--             newDeleteBucketCors
--
--         , requestDeleteBucketEncryption $
--             newDeleteBucketEncryption
--
--         , requestDeleteBucketIntelligentTieringConfiguration $
--             newDeleteBucketIntelligentTieringConfiguration
--
--         , requestDeleteBucketInventoryConfiguration $
--             newDeleteBucketInventoryConfiguration
--
--         , requestDeleteBucketLifecycle $
--             newDeleteBucketLifecycle
--
--         , requestDeleteBucketMetricsConfiguration $
--             newDeleteBucketMetricsConfiguration
--
--         , requestDeleteBucketOwnershipControls $
--             newDeleteBucketOwnershipControls
--
--         , requestDeleteBucketPolicy $
--             newDeleteBucketPolicy
--
--         , requestDeleteBucketReplication $
--             newDeleteBucketReplication
--
--         , requestDeleteBucketTagging $
--             newDeleteBucketTagging
--
--         , requestDeleteBucketWebsite $
--             newDeleteBucketWebsite
--
--         , requestDeleteObject $
--             newDeleteObject
--
--         , requestDeleteObjectTagging $
--             newDeleteObjectTagging
--
--         , requestDeleteObjects $
--             newDeleteObjects
--
--         , requestDeletePublicAccessBlock $
--             newDeletePublicAccessBlock
--
--         , requestGetBucketAccelerateConfiguration $
--             newGetBucketAccelerateConfiguration
--
--         , requestGetBucketAcl $
--             newGetBucketAcl
--
--         , requestGetBucketAnalyticsConfiguration $
--             newGetBucketAnalyticsConfiguration
--
--         , requestGetBucketCors $
--             newGetBucketCors
--
--         , requestGetBucketEncryption $
--             newGetBucketEncryption
--
--         , requestGetBucketIntelligentTieringConfiguration $
--             newGetBucketIntelligentTieringConfiguration
--
--         , requestGetBucketInventoryConfiguration $
--             newGetBucketInventoryConfiguration
--
--         , requestGetBucketLifecycleConfiguration $
--             newGetBucketLifecycleConfiguration
--
--         , requestGetBucketLocation $
--             newGetBucketLocation
--
--         , requestGetBucketLogging $
--             newGetBucketLogging
--
--         , requestGetBucketMetricsConfiguration $
--             newGetBucketMetricsConfiguration
--
--         , requestGetBucketNotificationConfiguration $
--             newGetBucketNotificationConfiguration
--
--         , requestGetBucketOwnershipControls $
--             newGetBucketOwnershipControls
--
--         , requestGetBucketPolicy $
--             newGetBucketPolicy
--
--         , requestGetBucketPolicyStatus $
--             newGetBucketPolicyStatus
--
--         , requestGetBucketReplication $
--             newGetBucketReplication
--
--         , requestGetBucketRequestPayment $
--             newGetBucketRequestPayment
--
--         , requestGetBucketTagging $
--             newGetBucketTagging
--
--         , requestGetBucketVersioning $
--             newGetBucketVersioning
--
--         , requestGetBucketWebsite $
--             newGetBucketWebsite
--
--         , requestGetObject $
--             newGetObject
--
--         , requestGetObjectAcl $
--             newGetObjectAcl
--
--         , requestGetObjectAttributes $
--             newGetObjectAttributes
--
--         , requestGetObjectLegalHold $
--             newGetObjectLegalHold
--
--         , requestGetObjectLockConfiguration $
--             newGetObjectLockConfiguration
--
--         , requestGetObjectRetention $
--             newGetObjectRetention
--
--         , requestGetObjectTagging $
--             newGetObjectTagging
--
--         , requestGetObjectTorrent $
--             newGetObjectTorrent
--
--         , requestGetPublicAccessBlock $
--             newGetPublicAccessBlock
--
--         , requestHeadBucket $
--             newHeadBucket
--
--         , requestHeadObject $
--             newHeadObject
--
--         , requestListBucketAnalyticsConfigurations $
--             newListBucketAnalyticsConfigurations
--
--         , requestListBucketIntelligentTieringConfigurations $
--             newListBucketIntelligentTieringConfigurations
--
--         , requestListBucketInventoryConfigurations $
--             newListBucketInventoryConfigurations
--
--         , requestListBucketMetricsConfigurations $
--             newListBucketMetricsConfigurations
--
--         , requestListBuckets $
--             newListBuckets
--
--         , requestListMultipartUploads $
--             newListMultipartUploads
--
--         , requestListObjectVersions $
--             newListObjectVersions
--
--         , requestListObjects $
--             newListObjects
--
--         , requestListObjectsV2 $
--             newListObjectsV2
--
--         , requestListParts $
--             newListParts
--
--         , requestPutBucketAccelerateConfiguration $
--             newPutBucketAccelerateConfiguration
--
--         , requestPutBucketAcl $
--             newPutBucketAcl
--
--         , requestPutBucketAnalyticsConfiguration $
--             newPutBucketAnalyticsConfiguration
--
--         , requestPutBucketCors $
--             newPutBucketCors
--
--         , requestPutBucketEncryption $
--             newPutBucketEncryption
--
--         , requestPutBucketIntelligentTieringConfiguration $
--             newPutBucketIntelligentTieringConfiguration
--
--         , requestPutBucketInventoryConfiguration $
--             newPutBucketInventoryConfiguration
--
--         , requestPutBucketLifecycleConfiguration $
--             newPutBucketLifecycleConfiguration
--
--         , requestPutBucketLogging $
--             newPutBucketLogging
--
--         , requestPutBucketMetricsConfiguration $
--             newPutBucketMetricsConfiguration
--
--         , requestPutBucketNotificationConfiguration $
--             newPutBucketNotificationConfiguration
--
--         , requestPutBucketOwnershipControls $
--             newPutBucketOwnershipControls
--
--         , requestPutBucketPolicy $
--             newPutBucketPolicy
--
--         , requestPutBucketReplication $
--             newPutBucketReplication
--
--         , requestPutBucketRequestPayment $
--             newPutBucketRequestPayment
--
--         , requestPutBucketTagging $
--             newPutBucketTagging
--
--         , requestPutBucketVersioning $
--             newPutBucketVersioning
--
--         , requestPutBucketWebsite $
--             newPutBucketWebsite
--
--         , requestPutObject $
--             newPutObject
--
--         , requestPutObjectAcl $
--             newPutObjectAcl
--
--         , requestPutObjectLegalHold $
--             newPutObjectLegalHold
--
--         , requestPutObjectLockConfiguration $
--             newPutObjectLockConfiguration
--
--         , requestPutObjectRetention $
--             newPutObjectRetention
--
--         , requestPutObjectTagging $
--             newPutObjectTagging
--
--         , requestPutPublicAccessBlock $
--             newPutPublicAccessBlock
--
--         , requestRestoreObject $
--             newRestoreObject
--
--         , requestSelectObjectContent $
--             newSelectObjectContent
--
--         , requestUploadPart $
--             newUploadPart
--
--         , requestUploadPartCopy $
--             newUploadPartCopy
--
--         , requestWriteGetObjectResponse $
--             newWriteGetObjectResponse
--
--           ]

--     , testGroup "response"
--         [ responseAbortMultipartUpload $
--             newAbortMultipartUploadResponse
--
--         , responseCompleteMultipartUpload $
--             newCompleteMultipartUploadResponse
--
--         , responseCopyObject $
--             newCopyObjectResponse
--
--         , responseCreateBucket $
--             newCreateBucketResponse
--
--         , responseCreateMultipartUpload $
--             newCreateMultipartUploadResponse
--
--         , responseDeleteBucket $
--             newDeleteBucketResponse
--
--         , responseDeleteBucketAnalyticsConfiguration $
--             newDeleteBucketAnalyticsConfigurationResponse
--
--         , responseDeleteBucketCors $
--             newDeleteBucketCorsResponse
--
--         , responseDeleteBucketEncryption $
--             newDeleteBucketEncryptionResponse
--
--         , responseDeleteBucketIntelligentTieringConfiguration $
--             newDeleteBucketIntelligentTieringConfigurationResponse
--
--         , responseDeleteBucketInventoryConfiguration $
--             newDeleteBucketInventoryConfigurationResponse
--
--         , responseDeleteBucketLifecycle $
--             newDeleteBucketLifecycleResponse
--
--         , responseDeleteBucketMetricsConfiguration $
--             newDeleteBucketMetricsConfigurationResponse
--
--         , responseDeleteBucketOwnershipControls $
--             newDeleteBucketOwnershipControlsResponse
--
--         , responseDeleteBucketPolicy $
--             newDeleteBucketPolicyResponse
--
--         , responseDeleteBucketReplication $
--             newDeleteBucketReplicationResponse
--
--         , responseDeleteBucketTagging $
--             newDeleteBucketTaggingResponse
--
--         , responseDeleteBucketWebsite $
--             newDeleteBucketWebsiteResponse
--
--         , responseDeleteObject $
--             newDeleteObjectResponse
--
--         , responseDeleteObjectTagging $
--             newDeleteObjectTaggingResponse
--
--         , responseDeleteObjects $
--             newDeleteObjectsResponse
--
--         , responseDeletePublicAccessBlock $
--             newDeletePublicAccessBlockResponse
--
--         , responseGetBucketAccelerateConfiguration $
--             newGetBucketAccelerateConfigurationResponse
--
--         , responseGetBucketAcl $
--             newGetBucketAclResponse
--
--         , responseGetBucketAnalyticsConfiguration $
--             newGetBucketAnalyticsConfigurationResponse
--
--         , responseGetBucketCors $
--             newGetBucketCorsResponse
--
--         , responseGetBucketEncryption $
--             newGetBucketEncryptionResponse
--
--         , responseGetBucketIntelligentTieringConfiguration $
--             newGetBucketIntelligentTieringConfigurationResponse
--
--         , responseGetBucketInventoryConfiguration $
--             newGetBucketInventoryConfigurationResponse
--
--         , responseGetBucketLifecycleConfiguration $
--             newGetBucketLifecycleConfigurationResponse
--
--         , responseGetBucketLocation $
--             newGetBucketLocationResponse
--
--         , responseGetBucketLogging $
--             newGetBucketLoggingResponse
--
--         , responseGetBucketMetricsConfiguration $
--             newGetBucketMetricsConfigurationResponse
--
--         , responseGetBucketNotificationConfiguration $
--             newNotificationConfiguration
--
--         , responseGetBucketOwnershipControls $
--             newGetBucketOwnershipControlsResponse
--
--         , responseGetBucketPolicy $
--             newGetBucketPolicyResponse
--
--         , responseGetBucketPolicyStatus $
--             newGetBucketPolicyStatusResponse
--
--         , responseGetBucketReplication $
--             newGetBucketReplicationResponse
--
--         , responseGetBucketRequestPayment $
--             newGetBucketRequestPaymentResponse
--
--         , responseGetBucketTagging $
--             newGetBucketTaggingResponse
--
--         , responseGetBucketVersioning $
--             newGetBucketVersioningResponse
--
--         , responseGetBucketWebsite $
--             newGetBucketWebsiteResponse
--
--         , responseGetObject $
--             newGetObjectResponse
--
--         , responseGetObjectAcl $
--             newGetObjectAclResponse
--
--         , responseGetObjectAttributes $
--             newGetObjectAttributesResponse
--
--         , responseGetObjectLegalHold $
--             newGetObjectLegalHoldResponse
--
--         , responseGetObjectLockConfiguration $
--             newGetObjectLockConfigurationResponse
--
--         , responseGetObjectRetention $
--             newGetObjectRetentionResponse
--
--         , responseGetObjectTagging $
--             newGetObjectTaggingResponse
--
--         , responseGetObjectTorrent $
--             newGetObjectTorrentResponse
--
--         , responseGetPublicAccessBlock $
--             newGetPublicAccessBlockResponse
--
--         , responseHeadBucket $
--             newHeadBucketResponse
--
--         , responseHeadObject $
--             newHeadObjectResponse
--
--         , responseListBucketAnalyticsConfigurations $
--             newListBucketAnalyticsConfigurationsResponse
--
--         , responseListBucketIntelligentTieringConfigurations $
--             newListBucketIntelligentTieringConfigurationsResponse
--
--         , responseListBucketInventoryConfigurations $
--             newListBucketInventoryConfigurationsResponse
--
--         , responseListBucketMetricsConfigurations $
--             newListBucketMetricsConfigurationsResponse
--
--         , responseListBuckets $
--             newListBucketsResponse
--
--         , responseListMultipartUploads $
--             newListMultipartUploadsResponse
--
--         , responseListObjectVersions $
--             newListObjectVersionsResponse
--
--         , responseListObjects $
--             newListObjectsResponse
--
--         , responseListObjectsV2 $
--             newListObjectsV2Response
--
--         , responseListParts $
--             newListPartsResponse
--
--         , responsePutBucketAccelerateConfiguration $
--             newPutBucketAccelerateConfigurationResponse
--
--         , responsePutBucketAcl $
--             newPutBucketAclResponse
--
--         , responsePutBucketAnalyticsConfiguration $
--             newPutBucketAnalyticsConfigurationResponse
--
--         , responsePutBucketCors $
--             newPutBucketCorsResponse
--
--         , responsePutBucketEncryption $
--             newPutBucketEncryptionResponse
--
--         , responsePutBucketIntelligentTieringConfiguration $
--             newPutBucketIntelligentTieringConfigurationResponse
--
--         , responsePutBucketInventoryConfiguration $
--             newPutBucketInventoryConfigurationResponse
--
--         , responsePutBucketLifecycleConfiguration $
--             newPutBucketLifecycleConfigurationResponse
--
--         , responsePutBucketLogging $
--             newPutBucketLoggingResponse
--
--         , responsePutBucketMetricsConfiguration $
--             newPutBucketMetricsConfigurationResponse
--
--         , responsePutBucketNotificationConfiguration $
--             newPutBucketNotificationConfigurationResponse
--
--         , responsePutBucketOwnershipControls $
--             newPutBucketOwnershipControlsResponse
--
--         , responsePutBucketPolicy $
--             newPutBucketPolicyResponse
--
--         , responsePutBucketReplication $
--             newPutBucketReplicationResponse
--
--         , responsePutBucketRequestPayment $
--             newPutBucketRequestPaymentResponse
--
--         , responsePutBucketTagging $
--             newPutBucketTaggingResponse
--
--         , responsePutBucketVersioning $
--             newPutBucketVersioningResponse
--
--         , responsePutBucketWebsite $
--             newPutBucketWebsiteResponse
--
--         , responsePutObject $
--             newPutObjectResponse
--
--         , responsePutObjectAcl $
--             newPutObjectAclResponse
--
--         , responsePutObjectLegalHold $
--             newPutObjectLegalHoldResponse
--
--         , responsePutObjectLockConfiguration $
--             newPutObjectLockConfigurationResponse
--
--         , responsePutObjectRetention $
--             newPutObjectRetentionResponse
--
--         , responsePutObjectTagging $
--             newPutObjectTaggingResponse
--
--         , responsePutPublicAccessBlock $
--             newPutPublicAccessBlockResponse
--
--         , responseRestoreObject $
--             newRestoreObjectResponse
--
--         , responseSelectObjectContent $
--             newSelectObjectContentResponse
--
--         , responseUploadPart $
--             newUploadPartResponse
--
--         , responseUploadPartCopy $
--             newUploadPartCopyResponse
--
--         , responseWriteGetObjectResponse $
--             newWriteGetObjectResponseResponse
--
--           ]
--     ]

-- Requests

requestAbortMultipartUpload :: AbortMultipartUpload -> TestTree
requestAbortMultipartUpload =
  req
    "AbortMultipartUpload"
    "fixture/AbortMultipartUpload.yaml"

requestCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
requestCompleteMultipartUpload =
  req
    "CompleteMultipartUpload"
    "fixture/CompleteMultipartUpload.yaml"

requestCopyObject :: CopyObject -> TestTree
requestCopyObject =
  req
    "CopyObject"
    "fixture/CopyObject.yaml"

requestCreateBucket :: CreateBucket -> TestTree
requestCreateBucket =
  req
    "CreateBucket"
    "fixture/CreateBucket.yaml"

requestCreateMultipartUpload :: CreateMultipartUpload -> TestTree
requestCreateMultipartUpload =
  req
    "CreateMultipartUpload"
    "fixture/CreateMultipartUpload.yaml"

requestDeleteBucket :: DeleteBucket -> TestTree
requestDeleteBucket =
  req
    "DeleteBucket"
    "fixture/DeleteBucket.yaml"

requestDeleteBucketAnalyticsConfiguration :: DeleteBucketAnalyticsConfiguration -> TestTree
requestDeleteBucketAnalyticsConfiguration =
  req
    "DeleteBucketAnalyticsConfiguration"
    "fixture/DeleteBucketAnalyticsConfiguration.yaml"

requestDeleteBucketCors :: DeleteBucketCors -> TestTree
requestDeleteBucketCors =
  req
    "DeleteBucketCors"
    "fixture/DeleteBucketCors.yaml"

requestDeleteBucketEncryption :: DeleteBucketEncryption -> TestTree
requestDeleteBucketEncryption =
  req
    "DeleteBucketEncryption"
    "fixture/DeleteBucketEncryption.yaml"

requestDeleteBucketIntelligentTieringConfiguration :: DeleteBucketIntelligentTieringConfiguration -> TestTree
requestDeleteBucketIntelligentTieringConfiguration =
  req
    "DeleteBucketIntelligentTieringConfiguration"
    "fixture/DeleteBucketIntelligentTieringConfiguration.yaml"

requestDeleteBucketInventoryConfiguration :: DeleteBucketInventoryConfiguration -> TestTree
requestDeleteBucketInventoryConfiguration =
  req
    "DeleteBucketInventoryConfiguration"
    "fixture/DeleteBucketInventoryConfiguration.yaml"

requestDeleteBucketLifecycle :: DeleteBucketLifecycle -> TestTree
requestDeleteBucketLifecycle =
  req
    "DeleteBucketLifecycle"
    "fixture/DeleteBucketLifecycle.yaml"

requestDeleteBucketMetricsConfiguration :: DeleteBucketMetricsConfiguration -> TestTree
requestDeleteBucketMetricsConfiguration =
  req
    "DeleteBucketMetricsConfiguration"
    "fixture/DeleteBucketMetricsConfiguration.yaml"

requestDeleteBucketOwnershipControls :: DeleteBucketOwnershipControls -> TestTree
requestDeleteBucketOwnershipControls =
  req
    "DeleteBucketOwnershipControls"
    "fixture/DeleteBucketOwnershipControls.yaml"

requestDeleteBucketPolicy :: DeleteBucketPolicy -> TestTree
requestDeleteBucketPolicy =
  req
    "DeleteBucketPolicy"
    "fixture/DeleteBucketPolicy.yaml"

requestDeleteBucketReplication :: DeleteBucketReplication -> TestTree
requestDeleteBucketReplication =
  req
    "DeleteBucketReplication"
    "fixture/DeleteBucketReplication.yaml"

requestDeleteBucketTagging :: DeleteBucketTagging -> TestTree
requestDeleteBucketTagging =
  req
    "DeleteBucketTagging"
    "fixture/DeleteBucketTagging.yaml"

requestDeleteBucketWebsite :: DeleteBucketWebsite -> TestTree
requestDeleteBucketWebsite =
  req
    "DeleteBucketWebsite"
    "fixture/DeleteBucketWebsite.yaml"

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject =
  req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestDeleteObjectTagging :: DeleteObjectTagging -> TestTree
requestDeleteObjectTagging =
  req
    "DeleteObjectTagging"
    "fixture/DeleteObjectTagging.yaml"

requestDeleteObjects :: DeleteObjects -> TestTree
requestDeleteObjects =
  req
    "DeleteObjects"
    "fixture/DeleteObjects.yaml"

requestDeletePublicAccessBlock :: DeletePublicAccessBlock -> TestTree
requestDeletePublicAccessBlock =
  req
    "DeletePublicAccessBlock"
    "fixture/DeletePublicAccessBlock.yaml"

requestGetBucketAccelerateConfiguration :: GetBucketAccelerateConfiguration -> TestTree
requestGetBucketAccelerateConfiguration =
  req
    "GetBucketAccelerateConfiguration"
    "fixture/GetBucketAccelerateConfiguration.yaml"

requestGetBucketAcl :: GetBucketAcl -> TestTree
requestGetBucketAcl =
  req
    "GetBucketAcl"
    "fixture/GetBucketAcl.yaml"

requestGetBucketAnalyticsConfiguration :: GetBucketAnalyticsConfiguration -> TestTree
requestGetBucketAnalyticsConfiguration =
  req
    "GetBucketAnalyticsConfiguration"
    "fixture/GetBucketAnalyticsConfiguration.yaml"

requestGetBucketCors :: GetBucketCors -> TestTree
requestGetBucketCors =
  req
    "GetBucketCors"
    "fixture/GetBucketCors.yaml"

requestGetBucketEncryption :: GetBucketEncryption -> TestTree
requestGetBucketEncryption =
  req
    "GetBucketEncryption"
    "fixture/GetBucketEncryption.yaml"

requestGetBucketIntelligentTieringConfiguration :: GetBucketIntelligentTieringConfiguration -> TestTree
requestGetBucketIntelligentTieringConfiguration =
  req
    "GetBucketIntelligentTieringConfiguration"
    "fixture/GetBucketIntelligentTieringConfiguration.yaml"

requestGetBucketInventoryConfiguration :: GetBucketInventoryConfiguration -> TestTree
requestGetBucketInventoryConfiguration =
  req
    "GetBucketInventoryConfiguration"
    "fixture/GetBucketInventoryConfiguration.yaml"

requestGetBucketLifecycleConfiguration :: GetBucketLifecycleConfiguration -> TestTree
requestGetBucketLifecycleConfiguration =
  req
    "GetBucketLifecycleConfiguration"
    "fixture/GetBucketLifecycleConfiguration.yaml"

requestGetBucketLocation :: GetBucketLocation -> TestTree
requestGetBucketLocation =
  req
    "GetBucketLocation"
    "fixture/GetBucketLocation.yaml"

requestGetBucketLogging :: GetBucketLogging -> TestTree
requestGetBucketLogging =
  req
    "GetBucketLogging"
    "fixture/GetBucketLogging.yaml"

requestGetBucketMetricsConfiguration :: GetBucketMetricsConfiguration -> TestTree
requestGetBucketMetricsConfiguration =
  req
    "GetBucketMetricsConfiguration"
    "fixture/GetBucketMetricsConfiguration.yaml"

requestGetBucketNotificationConfiguration :: GetBucketNotificationConfiguration -> TestTree
requestGetBucketNotificationConfiguration =
  req
    "GetBucketNotificationConfiguration"
    "fixture/GetBucketNotificationConfiguration.yaml"

requestGetBucketOwnershipControls :: GetBucketOwnershipControls -> TestTree
requestGetBucketOwnershipControls =
  req
    "GetBucketOwnershipControls"
    "fixture/GetBucketOwnershipControls.yaml"

requestGetBucketPolicy :: GetBucketPolicy -> TestTree
requestGetBucketPolicy =
  req
    "GetBucketPolicy"
    "fixture/GetBucketPolicy.yaml"

requestGetBucketPolicyStatus :: GetBucketPolicyStatus -> TestTree
requestGetBucketPolicyStatus =
  req
    "GetBucketPolicyStatus"
    "fixture/GetBucketPolicyStatus.yaml"

requestGetBucketReplication :: GetBucketReplication -> TestTree
requestGetBucketReplication =
  req
    "GetBucketReplication"
    "fixture/GetBucketReplication.yaml"

requestGetBucketRequestPayment :: GetBucketRequestPayment -> TestTree
requestGetBucketRequestPayment =
  req
    "GetBucketRequestPayment"
    "fixture/GetBucketRequestPayment.yaml"

requestGetBucketTagging :: GetBucketTagging -> TestTree
requestGetBucketTagging =
  req
    "GetBucketTagging"
    "fixture/GetBucketTagging.yaml"

requestGetBucketVersioning :: GetBucketVersioning -> TestTree
requestGetBucketVersioning =
  req
    "GetBucketVersioning"
    "fixture/GetBucketVersioning.yaml"

requestGetBucketWebsite :: GetBucketWebsite -> TestTree
requestGetBucketWebsite =
  req
    "GetBucketWebsite"
    "fixture/GetBucketWebsite.yaml"

requestGetObject :: GetObject -> TestTree
requestGetObject =
  req
    "GetObject"
    "fixture/GetObject.yaml"

requestGetObjectAcl :: GetObjectAcl -> TestTree
requestGetObjectAcl =
  req
    "GetObjectAcl"
    "fixture/GetObjectAcl.yaml"

requestGetObjectAttributes :: GetObjectAttributes -> TestTree
requestGetObjectAttributes =
  req
    "GetObjectAttributes"
    "fixture/GetObjectAttributes.yaml"

requestGetObjectLegalHold :: GetObjectLegalHold -> TestTree
requestGetObjectLegalHold =
  req
    "GetObjectLegalHold"
    "fixture/GetObjectLegalHold.yaml"

requestGetObjectLockConfiguration :: GetObjectLockConfiguration -> TestTree
requestGetObjectLockConfiguration =
  req
    "GetObjectLockConfiguration"
    "fixture/GetObjectLockConfiguration.yaml"

requestGetObjectRetention :: GetObjectRetention -> TestTree
requestGetObjectRetention =
  req
    "GetObjectRetention"
    "fixture/GetObjectRetention.yaml"

requestGetObjectTagging :: GetObjectTagging -> TestTree
requestGetObjectTagging =
  req
    "GetObjectTagging"
    "fixture/GetObjectTagging.yaml"

requestGetObjectTorrent :: GetObjectTorrent -> TestTree
requestGetObjectTorrent =
  req
    "GetObjectTorrent"
    "fixture/GetObjectTorrent.yaml"

requestGetPublicAccessBlock :: GetPublicAccessBlock -> TestTree
requestGetPublicAccessBlock =
  req
    "GetPublicAccessBlock"
    "fixture/GetPublicAccessBlock.yaml"

requestHeadBucket :: HeadBucket -> TestTree
requestHeadBucket =
  req
    "HeadBucket"
    "fixture/HeadBucket.yaml"

requestHeadObject :: HeadObject -> TestTree
requestHeadObject =
  req
    "HeadObject"
    "fixture/HeadObject.yaml"

requestListBucketAnalyticsConfigurations :: ListBucketAnalyticsConfigurations -> TestTree
requestListBucketAnalyticsConfigurations =
  req
    "ListBucketAnalyticsConfigurations"
    "fixture/ListBucketAnalyticsConfigurations.yaml"

requestListBucketIntelligentTieringConfigurations :: ListBucketIntelligentTieringConfigurations -> TestTree
requestListBucketIntelligentTieringConfigurations =
  req
    "ListBucketIntelligentTieringConfigurations"
    "fixture/ListBucketIntelligentTieringConfigurations.yaml"

requestListBucketInventoryConfigurations :: ListBucketInventoryConfigurations -> TestTree
requestListBucketInventoryConfigurations =
  req
    "ListBucketInventoryConfigurations"
    "fixture/ListBucketInventoryConfigurations.yaml"

requestListBucketMetricsConfigurations :: ListBucketMetricsConfigurations -> TestTree
requestListBucketMetricsConfigurations =
  req
    "ListBucketMetricsConfigurations"
    "fixture/ListBucketMetricsConfigurations.yaml"

requestListBuckets :: ListBuckets -> TestTree
requestListBuckets =
  req
    "ListBuckets"
    "fixture/ListBuckets.yaml"

requestListMultipartUploads :: ListMultipartUploads -> TestTree
requestListMultipartUploads =
  req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads.yaml"

requestListObjectVersions :: ListObjectVersions -> TestTree
requestListObjectVersions =
  req
    "ListObjectVersions"
    "fixture/ListObjectVersions.yaml"

requestListObjects :: ListObjects -> TestTree
requestListObjects =
  req
    "ListObjects"
    "fixture/ListObjects.yaml"

requestListObjectsV2 :: ListObjectsV2 -> TestTree
requestListObjectsV2 =
  req
    "ListObjectsV2"
    "fixture/ListObjectsV2.yaml"

requestListParts :: ListParts -> TestTree
requestListParts =
  req
    "ListParts"
    "fixture/ListParts.yaml"

requestPutBucketAccelerateConfiguration :: PutBucketAccelerateConfiguration -> TestTree
requestPutBucketAccelerateConfiguration =
  req
    "PutBucketAccelerateConfiguration"
    "fixture/PutBucketAccelerateConfiguration.yaml"

requestPutBucketAcl :: PutBucketAcl -> TestTree
requestPutBucketAcl =
  req
    "PutBucketAcl"
    "fixture/PutBucketAcl.yaml"

requestPutBucketAnalyticsConfiguration :: PutBucketAnalyticsConfiguration -> TestTree
requestPutBucketAnalyticsConfiguration =
  req
    "PutBucketAnalyticsConfiguration"
    "fixture/PutBucketAnalyticsConfiguration.yaml"

requestPutBucketCors :: PutBucketCors -> TestTree
requestPutBucketCors =
  req
    "PutBucketCors"
    "fixture/PutBucketCors.yaml"

requestPutBucketEncryption :: PutBucketEncryption -> TestTree
requestPutBucketEncryption =
  req
    "PutBucketEncryption"
    "fixture/PutBucketEncryption.yaml"

requestPutBucketIntelligentTieringConfiguration :: PutBucketIntelligentTieringConfiguration -> TestTree
requestPutBucketIntelligentTieringConfiguration =
  req
    "PutBucketIntelligentTieringConfiguration"
    "fixture/PutBucketIntelligentTieringConfiguration.yaml"

requestPutBucketInventoryConfiguration :: PutBucketInventoryConfiguration -> TestTree
requestPutBucketInventoryConfiguration =
  req
    "PutBucketInventoryConfiguration"
    "fixture/PutBucketInventoryConfiguration.yaml"

requestPutBucketLifecycleConfiguration :: PutBucketLifecycleConfiguration -> TestTree
requestPutBucketLifecycleConfiguration =
  req
    "PutBucketLifecycleConfiguration"
    "fixture/PutBucketLifecycleConfiguration.yaml"

requestPutBucketLogging :: PutBucketLogging -> TestTree
requestPutBucketLogging =
  req
    "PutBucketLogging"
    "fixture/PutBucketLogging.yaml"

requestPutBucketMetricsConfiguration :: PutBucketMetricsConfiguration -> TestTree
requestPutBucketMetricsConfiguration =
  req
    "PutBucketMetricsConfiguration"
    "fixture/PutBucketMetricsConfiguration.yaml"

requestPutBucketNotificationConfiguration :: PutBucketNotificationConfiguration -> TestTree
requestPutBucketNotificationConfiguration =
  req
    "PutBucketNotificationConfiguration"
    "fixture/PutBucketNotificationConfiguration.yaml"

requestPutBucketOwnershipControls :: PutBucketOwnershipControls -> TestTree
requestPutBucketOwnershipControls =
  req
    "PutBucketOwnershipControls"
    "fixture/PutBucketOwnershipControls.yaml"

requestPutBucketPolicy :: PutBucketPolicy -> TestTree
requestPutBucketPolicy =
  req
    "PutBucketPolicy"
    "fixture/PutBucketPolicy.yaml"

requestPutBucketReplication :: PutBucketReplication -> TestTree
requestPutBucketReplication =
  req
    "PutBucketReplication"
    "fixture/PutBucketReplication.yaml"

requestPutBucketRequestPayment :: PutBucketRequestPayment -> TestTree
requestPutBucketRequestPayment =
  req
    "PutBucketRequestPayment"
    "fixture/PutBucketRequestPayment.yaml"

requestPutBucketTagging :: PutBucketTagging -> TestTree
requestPutBucketTagging =
  req
    "PutBucketTagging"
    "fixture/PutBucketTagging.yaml"

requestPutBucketVersioning :: PutBucketVersioning -> TestTree
requestPutBucketVersioning =
  req
    "PutBucketVersioning"
    "fixture/PutBucketVersioning.yaml"

requestPutBucketWebsite :: PutBucketWebsite -> TestTree
requestPutBucketWebsite =
  req
    "PutBucketWebsite"
    "fixture/PutBucketWebsite.yaml"

requestPutObjectAcl :: PutObjectAcl -> TestTree
requestPutObjectAcl =
  req
    "PutObjectAcl"
    "fixture/PutObjectAcl.yaml"

requestPutObjectLegalHold :: PutObjectLegalHold -> TestTree
requestPutObjectLegalHold =
  req
    "PutObjectLegalHold"
    "fixture/PutObjectLegalHold.yaml"

requestPutObjectLockConfiguration :: PutObjectLockConfiguration -> TestTree
requestPutObjectLockConfiguration =
  req
    "PutObjectLockConfiguration"
    "fixture/PutObjectLockConfiguration.yaml"

requestPutObjectRetention :: PutObjectRetention -> TestTree
requestPutObjectRetention =
  req
    "PutObjectRetention"
    "fixture/PutObjectRetention.yaml"

requestPutObjectTagging :: PutObjectTagging -> TestTree
requestPutObjectTagging =
  req
    "PutObjectTagging"
    "fixture/PutObjectTagging.yaml"

requestPutPublicAccessBlock :: PutPublicAccessBlock -> TestTree
requestPutPublicAccessBlock =
  req
    "PutPublicAccessBlock"
    "fixture/PutPublicAccessBlock.yaml"

requestRestoreObject :: RestoreObject -> TestTree
requestRestoreObject =
  req
    "RestoreObject"
    "fixture/RestoreObject.yaml"

requestSelectObjectContent :: SelectObjectContent -> TestTree
requestSelectObjectContent =
  req
    "SelectObjectContent"
    "fixture/SelectObjectContent.yaml"

requestUploadPartCopy :: UploadPartCopy -> TestTree
requestUploadPartCopy =
  req
    "UploadPartCopy"
    "fixture/UploadPartCopy.yaml"

-- Responses

responseAbortMultipartUpload :: AbortMultipartUploadResponse -> TestTree
responseAbortMultipartUpload =
  res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AbortMultipartUpload)

responseCompleteMultipartUpload :: CompleteMultipartUploadResponse -> TestTree
responseCompleteMultipartUpload =
  res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteMultipartUpload)

responseCopyObject :: CopyObjectResponse -> TestTree
responseCopyObject =
  res
    "CopyObjectResponse"
    "fixture/CopyObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyObject)

responseCreateBucket :: CreateBucketResponse -> TestTree
responseCreateBucket =
  res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBucket)

responseCreateMultipartUpload :: CreateMultipartUploadResponse -> TestTree
responseCreateMultipartUpload =
  res
    "CreateMultipartUploadResponse"
    "fixture/CreateMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMultipartUpload)

responseDeleteBucket :: DeleteBucketResponse -> TestTree
responseDeleteBucket =
  res
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucket)

responseDeleteBucketAnalyticsConfiguration :: DeleteBucketAnalyticsConfigurationResponse -> TestTree
responseDeleteBucketAnalyticsConfiguration =
  res
    "DeleteBucketAnalyticsConfigurationResponse"
    "fixture/DeleteBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketAnalyticsConfiguration)

responseDeleteBucketCors :: DeleteBucketCorsResponse -> TestTree
responseDeleteBucketCors =
  res
    "DeleteBucketCorsResponse"
    "fixture/DeleteBucketCorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketCors)

responseDeleteBucketEncryption :: DeleteBucketEncryptionResponse -> TestTree
responseDeleteBucketEncryption =
  res
    "DeleteBucketEncryptionResponse"
    "fixture/DeleteBucketEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketEncryption)

responseDeleteBucketIntelligentTieringConfiguration :: DeleteBucketIntelligentTieringConfigurationResponse -> TestTree
responseDeleteBucketIntelligentTieringConfiguration =
  res
    "DeleteBucketIntelligentTieringConfigurationResponse"
    "fixture/DeleteBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketIntelligentTieringConfiguration)

responseDeleteBucketInventoryConfiguration :: DeleteBucketInventoryConfigurationResponse -> TestTree
responseDeleteBucketInventoryConfiguration =
  res
    "DeleteBucketInventoryConfigurationResponse"
    "fixture/DeleteBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketInventoryConfiguration)

responseDeleteBucketLifecycle :: DeleteBucketLifecycleResponse -> TestTree
responseDeleteBucketLifecycle =
  res
    "DeleteBucketLifecycleResponse"
    "fixture/DeleteBucketLifecycleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketLifecycle)

responseDeleteBucketMetricsConfiguration :: DeleteBucketMetricsConfigurationResponse -> TestTree
responseDeleteBucketMetricsConfiguration =
  res
    "DeleteBucketMetricsConfigurationResponse"
    "fixture/DeleteBucketMetricsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketMetricsConfiguration)

responseDeleteBucketOwnershipControls :: DeleteBucketOwnershipControlsResponse -> TestTree
responseDeleteBucketOwnershipControls =
  res
    "DeleteBucketOwnershipControlsResponse"
    "fixture/DeleteBucketOwnershipControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketOwnershipControls)

responseDeleteBucketPolicy :: DeleteBucketPolicyResponse -> TestTree
responseDeleteBucketPolicy =
  res
    "DeleteBucketPolicyResponse"
    "fixture/DeleteBucketPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketPolicy)

responseDeleteBucketReplication :: DeleteBucketReplicationResponse -> TestTree
responseDeleteBucketReplication =
  res
    "DeleteBucketReplicationResponse"
    "fixture/DeleteBucketReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketReplication)

responseDeleteBucketTagging :: DeleteBucketTaggingResponse -> TestTree
responseDeleteBucketTagging =
  res
    "DeleteBucketTaggingResponse"
    "fixture/DeleteBucketTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketTagging)

responseDeleteBucketWebsite :: DeleteBucketWebsiteResponse -> TestTree
responseDeleteBucketWebsite =
  res
    "DeleteBucketWebsiteResponse"
    "fixture/DeleteBucketWebsiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketWebsite)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject =
  res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObject)

responseDeleteObjectTagging :: DeleteObjectTaggingResponse -> TestTree
responseDeleteObjectTagging =
  res
    "DeleteObjectTaggingResponse"
    "fixture/DeleteObjectTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObjectTagging)

responseDeleteObjects :: DeleteObjectsResponse -> TestTree
responseDeleteObjects =
  res
    "DeleteObjectsResponse"
    "fixture/DeleteObjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObjects)

responseDeletePublicAccessBlock :: DeletePublicAccessBlockResponse -> TestTree
responseDeletePublicAccessBlock =
  res
    "DeletePublicAccessBlockResponse"
    "fixture/DeletePublicAccessBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePublicAccessBlock)

responseGetBucketAccelerateConfiguration :: GetBucketAccelerateConfigurationResponse -> TestTree
responseGetBucketAccelerateConfiguration =
  res
    "GetBucketAccelerateConfigurationResponse"
    "fixture/GetBucketAccelerateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketAccelerateConfiguration)

responseGetBucketAcl :: GetBucketAclResponse -> TestTree
responseGetBucketAcl =
  res
    "GetBucketAclResponse"
    "fixture/GetBucketAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketAcl)

responseGetBucketAnalyticsConfiguration :: GetBucketAnalyticsConfigurationResponse -> TestTree
responseGetBucketAnalyticsConfiguration =
  res
    "GetBucketAnalyticsConfigurationResponse"
    "fixture/GetBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketAnalyticsConfiguration)

responseGetBucketCors :: GetBucketCorsResponse -> TestTree
responseGetBucketCors =
  res
    "GetBucketCorsResponse"
    "fixture/GetBucketCorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketCors)

responseGetBucketEncryption :: GetBucketEncryptionResponse -> TestTree
responseGetBucketEncryption =
  res
    "GetBucketEncryptionResponse"
    "fixture/GetBucketEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketEncryption)

responseGetBucketIntelligentTieringConfiguration :: GetBucketIntelligentTieringConfigurationResponse -> TestTree
responseGetBucketIntelligentTieringConfiguration =
  res
    "GetBucketIntelligentTieringConfigurationResponse"
    "fixture/GetBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketIntelligentTieringConfiguration)

responseGetBucketInventoryConfiguration :: GetBucketInventoryConfigurationResponse -> TestTree
responseGetBucketInventoryConfiguration =
  res
    "GetBucketInventoryConfigurationResponse"
    "fixture/GetBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketInventoryConfiguration)

responseGetBucketLifecycleConfiguration :: GetBucketLifecycleConfigurationResponse -> TestTree
responseGetBucketLifecycleConfiguration =
  res
    "GetBucketLifecycleConfigurationResponse"
    "fixture/GetBucketLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketLifecycleConfiguration)

responseGetBucketLocation :: GetBucketLocationResponse -> TestTree
responseGetBucketLocation =
  res
    "GetBucketLocationResponse"
    "fixture/GetBucketLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketLocation)

responseGetBucketLogging :: GetBucketLoggingResponse -> TestTree
responseGetBucketLogging =
  res
    "GetBucketLoggingResponse"
    "fixture/GetBucketLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketLogging)

responseGetBucketMetricsConfiguration :: GetBucketMetricsConfigurationResponse -> TestTree
responseGetBucketMetricsConfiguration =
  res
    "GetBucketMetricsConfigurationResponse"
    "fixture/GetBucketMetricsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketMetricsConfiguration)

responseGetBucketNotificationConfiguration :: NotificationConfiguration -> TestTree
responseGetBucketNotificationConfiguration =
  res
    "GetBucketNotificationConfigurationResponse"
    "fixture/GetBucketNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketNotificationConfiguration)

responseGetBucketOwnershipControls :: GetBucketOwnershipControlsResponse -> TestTree
responseGetBucketOwnershipControls =
  res
    "GetBucketOwnershipControlsResponse"
    "fixture/GetBucketOwnershipControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketOwnershipControls)

responseGetBucketPolicy :: GetBucketPolicyResponse -> TestTree
responseGetBucketPolicy =
  res
    "GetBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketPolicy)

responseGetBucketPolicyStatus :: GetBucketPolicyStatusResponse -> TestTree
responseGetBucketPolicyStatus =
  res
    "GetBucketPolicyStatusResponse"
    "fixture/GetBucketPolicyStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketPolicyStatus)

responseGetBucketReplication :: GetBucketReplicationResponse -> TestTree
responseGetBucketReplication =
  res
    "GetBucketReplicationResponse"
    "fixture/GetBucketReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketReplication)

responseGetBucketRequestPayment :: GetBucketRequestPaymentResponse -> TestTree
responseGetBucketRequestPayment =
  res
    "GetBucketRequestPaymentResponse"
    "fixture/GetBucketRequestPaymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketRequestPayment)

responseGetBucketTagging :: GetBucketTaggingResponse -> TestTree
responseGetBucketTagging =
  res
    "GetBucketTaggingResponse"
    "fixture/GetBucketTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketTagging)

responseGetBucketVersioning :: GetBucketVersioningResponse -> TestTree
responseGetBucketVersioning =
  res
    "GetBucketVersioningResponse"
    "fixture/GetBucketVersioningResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketVersioning)

responseGetBucketWebsite :: GetBucketWebsiteResponse -> TestTree
responseGetBucketWebsite =
  res
    "GetBucketWebsiteResponse"
    "fixture/GetBucketWebsiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketWebsite)

responseGetObjectAcl :: GetObjectAclResponse -> TestTree
responseGetObjectAcl =
  res
    "GetObjectAclResponse"
    "fixture/GetObjectAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectAcl)

responseGetObjectAttributes :: GetObjectAttributesResponse -> TestTree
responseGetObjectAttributes =
  res
    "GetObjectAttributesResponse"
    "fixture/GetObjectAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectAttributes)

responseGetObjectLegalHold :: GetObjectLegalHoldResponse -> TestTree
responseGetObjectLegalHold =
  res
    "GetObjectLegalHoldResponse"
    "fixture/GetObjectLegalHoldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectLegalHold)

responseGetObjectLockConfiguration :: GetObjectLockConfigurationResponse -> TestTree
responseGetObjectLockConfiguration =
  res
    "GetObjectLockConfigurationResponse"
    "fixture/GetObjectLockConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectLockConfiguration)

responseGetObjectRetention :: GetObjectRetentionResponse -> TestTree
responseGetObjectRetention =
  res
    "GetObjectRetentionResponse"
    "fixture/GetObjectRetentionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectRetention)

responseGetObjectTagging :: GetObjectTaggingResponse -> TestTree
responseGetObjectTagging =
  res
    "GetObjectTaggingResponse"
    "fixture/GetObjectTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectTagging)

responseGetPublicAccessBlock :: GetPublicAccessBlockResponse -> TestTree
responseGetPublicAccessBlock =
  res
    "GetPublicAccessBlockResponse"
    "fixture/GetPublicAccessBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPublicAccessBlock)

responseHeadBucket :: HeadBucketResponse -> TestTree
responseHeadBucket =
  res
    "HeadBucketResponse"
    "fixture/HeadBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy HeadBucket)

responseHeadObject :: HeadObjectResponse -> TestTree
responseHeadObject =
  res
    "HeadObjectResponse"
    "fixture/HeadObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy HeadObject)

responseListBucketAnalyticsConfigurations :: ListBucketAnalyticsConfigurationsResponse -> TestTree
responseListBucketAnalyticsConfigurations =
  res
    "ListBucketAnalyticsConfigurationsResponse"
    "fixture/ListBucketAnalyticsConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBucketAnalyticsConfigurations)

responseListBucketIntelligentTieringConfigurations :: ListBucketIntelligentTieringConfigurationsResponse -> TestTree
responseListBucketIntelligentTieringConfigurations =
  res
    "ListBucketIntelligentTieringConfigurationsResponse"
    "fixture/ListBucketIntelligentTieringConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBucketIntelligentTieringConfigurations)

responseListBucketInventoryConfigurations :: ListBucketInventoryConfigurationsResponse -> TestTree
responseListBucketInventoryConfigurations =
  res
    "ListBucketInventoryConfigurationsResponse"
    "fixture/ListBucketInventoryConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBucketInventoryConfigurations)

responseListBucketMetricsConfigurations :: ListBucketMetricsConfigurationsResponse -> TestTree
responseListBucketMetricsConfigurations =
  res
    "ListBucketMetricsConfigurationsResponse"
    "fixture/ListBucketMetricsConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBucketMetricsConfigurations)

responseListBuckets :: ListBucketsResponse -> TestTree
responseListBuckets =
  res
    "ListBucketsResponse"
    "fixture/ListBucketsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuckets)

responseListMultipartUploads :: ListMultipartUploadsResponse -> TestTree
responseListMultipartUploads =
  res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMultipartUploads)

responseListObjectVersions :: ListObjectVersionsResponse -> TestTree
responseListObjectVersions =
  res
    "ListObjectVersionsResponse"
    "fixture/ListObjectVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectVersions)

responseListObjects :: ListObjectsResponse -> TestTree
responseListObjects =
  res
    "ListObjectsResponse"
    "fixture/ListObjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjects)

responseListObjectsV2 :: ListObjectsV2Response -> TestTree
responseListObjectsV2 =
  res
    "ListObjectsV2Response"
    "fixture/ListObjectsV2Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectsV2)

responseListParts :: ListPartsResponse -> TestTree
responseListParts =
  res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListParts)

responsePutBucketAccelerateConfiguration :: PutBucketAccelerateConfigurationResponse -> TestTree
responsePutBucketAccelerateConfiguration =
  res
    "PutBucketAccelerateConfigurationResponse"
    "fixture/PutBucketAccelerateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketAccelerateConfiguration)

responsePutBucketAcl :: PutBucketAclResponse -> TestTree
responsePutBucketAcl =
  res
    "PutBucketAclResponse"
    "fixture/PutBucketAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketAcl)

responsePutBucketAnalyticsConfiguration :: PutBucketAnalyticsConfigurationResponse -> TestTree
responsePutBucketAnalyticsConfiguration =
  res
    "PutBucketAnalyticsConfigurationResponse"
    "fixture/PutBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketAnalyticsConfiguration)

responsePutBucketCors :: PutBucketCorsResponse -> TestTree
responsePutBucketCors =
  res
    "PutBucketCorsResponse"
    "fixture/PutBucketCorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketCors)

responsePutBucketEncryption :: PutBucketEncryptionResponse -> TestTree
responsePutBucketEncryption =
  res
    "PutBucketEncryptionResponse"
    "fixture/PutBucketEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketEncryption)

responsePutBucketIntelligentTieringConfiguration :: PutBucketIntelligentTieringConfigurationResponse -> TestTree
responsePutBucketIntelligentTieringConfiguration =
  res
    "PutBucketIntelligentTieringConfigurationResponse"
    "fixture/PutBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketIntelligentTieringConfiguration)

responsePutBucketInventoryConfiguration :: PutBucketInventoryConfigurationResponse -> TestTree
responsePutBucketInventoryConfiguration =
  res
    "PutBucketInventoryConfigurationResponse"
    "fixture/PutBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketInventoryConfiguration)

responsePutBucketLifecycleConfiguration :: PutBucketLifecycleConfigurationResponse -> TestTree
responsePutBucketLifecycleConfiguration =
  res
    "PutBucketLifecycleConfigurationResponse"
    "fixture/PutBucketLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketLifecycleConfiguration)

responsePutBucketLogging :: PutBucketLoggingResponse -> TestTree
responsePutBucketLogging =
  res
    "PutBucketLoggingResponse"
    "fixture/PutBucketLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketLogging)

responsePutBucketMetricsConfiguration :: PutBucketMetricsConfigurationResponse -> TestTree
responsePutBucketMetricsConfiguration =
  res
    "PutBucketMetricsConfigurationResponse"
    "fixture/PutBucketMetricsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketMetricsConfiguration)

responsePutBucketNotificationConfiguration :: PutBucketNotificationConfigurationResponse -> TestTree
responsePutBucketNotificationConfiguration =
  res
    "PutBucketNotificationConfigurationResponse"
    "fixture/PutBucketNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketNotificationConfiguration)

responsePutBucketOwnershipControls :: PutBucketOwnershipControlsResponse -> TestTree
responsePutBucketOwnershipControls =
  res
    "PutBucketOwnershipControlsResponse"
    "fixture/PutBucketOwnershipControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketOwnershipControls)

responsePutBucketPolicy :: PutBucketPolicyResponse -> TestTree
responsePutBucketPolicy =
  res
    "PutBucketPolicyResponse"
    "fixture/PutBucketPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketPolicy)

responsePutBucketReplication :: PutBucketReplicationResponse -> TestTree
responsePutBucketReplication =
  res
    "PutBucketReplicationResponse"
    "fixture/PutBucketReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketReplication)

responsePutBucketRequestPayment :: PutBucketRequestPaymentResponse -> TestTree
responsePutBucketRequestPayment =
  res
    "PutBucketRequestPaymentResponse"
    "fixture/PutBucketRequestPaymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketRequestPayment)

responsePutBucketTagging :: PutBucketTaggingResponse -> TestTree
responsePutBucketTagging =
  res
    "PutBucketTaggingResponse"
    "fixture/PutBucketTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketTagging)

responsePutBucketVersioning :: PutBucketVersioningResponse -> TestTree
responsePutBucketVersioning =
  res
    "PutBucketVersioningResponse"
    "fixture/PutBucketVersioningResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketVersioning)

responsePutBucketWebsite :: PutBucketWebsiteResponse -> TestTree
responsePutBucketWebsite =
  res
    "PutBucketWebsiteResponse"
    "fixture/PutBucketWebsiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketWebsite)

responsePutObject :: PutObjectResponse -> TestTree
responsePutObject =
  res
    "PutObjectResponse"
    "fixture/PutObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObject)

responsePutObjectAcl :: PutObjectAclResponse -> TestTree
responsePutObjectAcl =
  res
    "PutObjectAclResponse"
    "fixture/PutObjectAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObjectAcl)

responsePutObjectLegalHold :: PutObjectLegalHoldResponse -> TestTree
responsePutObjectLegalHold =
  res
    "PutObjectLegalHoldResponse"
    "fixture/PutObjectLegalHoldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObjectLegalHold)

responsePutObjectLockConfiguration :: PutObjectLockConfigurationResponse -> TestTree
responsePutObjectLockConfiguration =
  res
    "PutObjectLockConfigurationResponse"
    "fixture/PutObjectLockConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObjectLockConfiguration)

responsePutObjectRetention :: PutObjectRetentionResponse -> TestTree
responsePutObjectRetention =
  res
    "PutObjectRetentionResponse"
    "fixture/PutObjectRetentionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObjectRetention)

responsePutObjectTagging :: PutObjectTaggingResponse -> TestTree
responsePutObjectTagging =
  res
    "PutObjectTaggingResponse"
    "fixture/PutObjectTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObjectTagging)

responsePutPublicAccessBlock :: PutPublicAccessBlockResponse -> TestTree
responsePutPublicAccessBlock =
  res
    "PutPublicAccessBlockResponse"
    "fixture/PutPublicAccessBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPublicAccessBlock)

responseRestoreObject :: RestoreObjectResponse -> TestTree
responseRestoreObject =
  res
    "RestoreObjectResponse"
    "fixture/RestoreObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreObject)

responseSelectObjectContent :: SelectObjectContentResponse -> TestTree
responseSelectObjectContent =
  res
    "SelectObjectContentResponse"
    "fixture/SelectObjectContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SelectObjectContent)

responseUploadPart :: UploadPartResponse -> TestTree
responseUploadPart =
  res
    "UploadPartResponse"
    "fixture/UploadPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadPart)

responseUploadPartCopy :: UploadPartCopyResponse -> TestTree
responseUploadPartCopy =
  res
    "UploadPartCopyResponse"
    "fixture/UploadPartCopyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadPartCopy)

responseWriteGetObjectResponse :: WriteGetObjectResponseResponse -> TestTree
responseWriteGetObjectResponse =
  res
    "WriteGetObjectResponseResponse"
    "fixture/WriteGetObjectResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy WriteGetObjectResponse)
