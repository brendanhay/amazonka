{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.S3
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.S3 where

import Data.Proxy
import Network.AWS.S3
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.S3.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutBucketPolicy $
--             newPutBucketPolicy
--
--         , requestDeleteBucket $
--             newDeleteBucket
--
--         , requestDeleteObjects $
--             newDeleteObjects
--
--         , requestGetBucketEncryption $
--             newGetBucketEncryption
--
--         , requestCompleteMultipartUpload $
--             newCompleteMultipartUpload
--
--         , requestGetBucketPolicyStatus $
--             newGetBucketPolicyStatus
--
--         , requestDeleteObject $
--             newDeleteObject
--
--         , requestSelectObjectContent $
--             newSelectObjectContent
--
--         , requestDeleteBucketWebsite $
--             newDeleteBucketWebsite
--
--         , requestPutBucketLogging $
--             newPutBucketLogging
--
--         , requestListObjects $
--             newListObjects
--
--         , requestDeleteObjectTagging $
--             newDeleteObjectTagging
--
--         , requestDeleteBucketAnalyticsConfiguration $
--             newDeleteBucketAnalyticsConfiguration
--
--         , requestGetObjectTagging $
--             newGetObjectTagging
--
--         , requestGetBucketAcl $
--             newGetBucketAcl
--
--         , requestGetBucketWebsite $
--             newGetBucketWebsite
--
--         , requestPutBucketReplication $
--             newPutBucketReplication
--
--         , requestDeleteBucketMetricsConfiguration $
--             newDeleteBucketMetricsConfiguration
--
--         , requestPutBucketOwnershipControls $
--             newPutBucketOwnershipControls
--
--         , requestPutObjectLegalHold $
--             newPutObjectLegalHold
--
--         , requestGetBucketPolicy $
--             newGetBucketPolicy
--
--         , requestPutObjectRetention $
--             newPutObjectRetention
--
--         , requestPutBucketEncryption $
--             newPutBucketEncryption
--
--         , requestGetObjectLockConfiguration $
--             newGetObjectLockConfiguration
--
--         , requestListBucketMetricsConfigurations $
--             newListBucketMetricsConfigurations
--
--         , requestPutBucketAccelerateConfiguration $
--             newPutBucketAccelerateConfiguration
--
--         , requestHeadObject $
--             newHeadObject
--
--         , requestPutBucketInventoryConfiguration $
--             newPutBucketInventoryConfiguration
--
--         , requestWriteGetObjectResponse $
--             newWriteGetObjectResponse
--
--         , requestGetBucketLocation $
--             newGetBucketLocation
--
--         , requestDeletePublicAccessBlock $
--             newDeletePublicAccessBlock
--
--         , requestListBucketInventoryConfigurations $
--             newListBucketInventoryConfigurations
--
--         , requestGetBucketTagging $
--             newGetBucketTagging
--
--         , requestGetObjectAcl $
--             newGetObjectAcl
--
--         , requestPutObjectAcl $
--             newPutObjectAcl
--
--         , requestCreateBucket $
--             newCreateBucket
--
--         , requestGetBucketVersioning $
--             newGetBucketVersioning
--
--         , requestPutBucketTagging $
--             newPutBucketTagging
--
--         , requestPutBucketCors $
--             newPutBucketCors
--
--         , requestDeleteBucketIntelligentTieringConfiguration $
--             newDeleteBucketIntelligentTieringConfiguration
--
--         , requestDeleteBucketPolicy $
--             newDeleteBucketPolicy
--
--         , requestGetObjectRetention $
--             newGetObjectRetention
--
--         , requestPutObjectLockConfiguration $
--             newPutObjectLockConfiguration
--
--         , requestAbortMultipartUpload $
--             newAbortMultipartUpload
--
--         , requestGetBucketOwnershipControls $
--             newGetBucketOwnershipControls
--
--         , requestGetObjectLegalHold $
--             newGetObjectLegalHold
--
--         , requestGetBucketMetricsConfiguration $
--             newGetBucketMetricsConfiguration
--
--         , requestGetBucketAccelerateConfiguration $
--             newGetBucketAccelerateConfiguration
--
--         , requestListBuckets $
--             newListBuckets
--
--         , requestGetObjectTorrent $
--             newGetObjectTorrent
--
--         , requestPutObject $
--             newPutObject
--
--         , requestPutBucketWebsite $
--             newPutBucketWebsite
--
--         , requestListMultipartUploads $
--             newListMultipartUploads
--
--         , requestPutBucketRequestPayment $
--             newPutBucketRequestPayment
--
--         , requestUploadPart $
--             newUploadPart
--
--         , requestGetBucketReplication $
--             newGetBucketReplication
--
--         , requestPutObjectTagging $
--             newPutObjectTagging
--
--         , requestUploadPartCopy $
--             newUploadPartCopy
--
--         , requestPutBucketAnalyticsConfiguration $
--             newPutBucketAnalyticsConfiguration
--
--         , requestListBucketAnalyticsConfigurations $
--             newListBucketAnalyticsConfigurations
--
--         , requestPutBucketAcl $
--             newPutBucketAcl
--
--         , requestDeleteBucketLifecycle $
--             newDeleteBucketLifecycle
--
--         , requestPutBucketLifecycleConfiguration $
--             newPutBucketLifecycleConfiguration
--
--         , requestCreateMultipartUpload $
--             newCreateMultipartUpload
--
--         , requestGetBucketLifecycleConfiguration $
--             newGetBucketLifecycleConfiguration
--
--         , requestListParts $
--             newListParts
--
--         , requestGetBucketAnalyticsConfiguration $
--             newGetBucketAnalyticsConfiguration
--
--         , requestListObjectVersions $
--             newListObjectVersions
--
--         , requestHeadBucket $
--             newHeadBucket
--
--         , requestGetObject $
--             newGetObject
--
--         , requestGetBucketLogging $
--             newGetBucketLogging
--
--         , requestGetBucketRequestPayment $
--             newGetBucketRequestPayment
--
--         , requestDeleteBucketReplication $
--             newDeleteBucketReplication
--
--         , requestListObjectsV $
--             newListObjectsV
--
--         , requestDeleteBucketEncryption $
--             newDeleteBucketEncryption
--
--         , requestDeleteBucketOwnershipControls $
--             newDeleteBucketOwnershipControls
--
--         , requestPutBucketMetricsConfiguration $
--             newPutBucketMetricsConfiguration
--
--         , requestCopyObject $
--             newCopyObject
--
--         , requestPutPublicAccessBlock $
--             newPutPublicAccessBlock
--
--         , requestGetBucketNotificationConfiguration $
--             newGetBucketNotificationConfiguration
--
--         , requestGetBucketCors $
--             newGetBucketCors
--
--         , requestPutBucketVersioning $
--             newPutBucketVersioning
--
--         , requestRestoreObject $
--             newRestoreObject
--
--         , requestGetBucketIntelligentTieringConfiguration $
--             newGetBucketIntelligentTieringConfiguration
--
--         , requestDeleteBucketInventoryConfiguration $
--             newDeleteBucketInventoryConfiguration
--
--         , requestDeleteBucketTagging $
--             newDeleteBucketTagging
--
--         , requestGetPublicAccessBlock $
--             newGetPublicAccessBlock
--
--         , requestPutBucketIntelligentTieringConfiguration $
--             newPutBucketIntelligentTieringConfiguration
--
--         , requestGetBucketInventoryConfiguration $
--             newGetBucketInventoryConfiguration
--
--         , requestDeleteBucketCors $
--             newDeleteBucketCors
--
--         , requestListBucketIntelligentTieringConfigurations $
--             newListBucketIntelligentTieringConfigurations
--
--         , requestPutBucketNotificationConfiguration $
--             newPutBucketNotificationConfiguration
--
--           ]

--     , testGroup "response"
--         [ responsePutBucketPolicy $
--             newPutBucketPolicyResponse
--
--         , responseDeleteBucket $
--             newDeleteBucketResponse
--
--         , responseDeleteObjects $
--             newDeleteObjectsResponse
--
--         , responseGetBucketEncryption $
--             newGetBucketEncryptionResponse
--
--         , responseCompleteMultipartUpload $
--             newCompleteMultipartUploadResponse
--
--         , responseGetBucketPolicyStatus $
--             newGetBucketPolicyStatusResponse
--
--         , responseDeleteObject $
--             newDeleteObjectResponse
--
--         , responseSelectObjectContent $
--             newSelectObjectContentResponse
--
--         , responseDeleteBucketWebsite $
--             newDeleteBucketWebsiteResponse
--
--         , responsePutBucketLogging $
--             newPutBucketLoggingResponse
--
--         , responseListObjects $
--             newListObjectsResponse
--
--         , responseDeleteObjectTagging $
--             newDeleteObjectTaggingResponse
--
--         , responseDeleteBucketAnalyticsConfiguration $
--             newDeleteBucketAnalyticsConfigurationResponse
--
--         , responseGetObjectTagging $
--             newGetObjectTaggingResponse
--
--         , responseGetBucketAcl $
--             newGetBucketAclResponse
--
--         , responseGetBucketWebsite $
--             newGetBucketWebsiteResponse
--
--         , responsePutBucketReplication $
--             newPutBucketReplicationResponse
--
--         , responseDeleteBucketMetricsConfiguration $
--             newDeleteBucketMetricsConfigurationResponse
--
--         , responsePutBucketOwnershipControls $
--             newPutBucketOwnershipControlsResponse
--
--         , responsePutObjectLegalHold $
--             newPutObjectLegalHoldResponse
--
--         , responseGetBucketPolicy $
--             newGetBucketPolicyResponse
--
--         , responsePutObjectRetention $
--             newPutObjectRetentionResponse
--
--         , responsePutBucketEncryption $
--             newPutBucketEncryptionResponse
--
--         , responseGetObjectLockConfiguration $
--             newGetObjectLockConfigurationResponse
--
--         , responseListBucketMetricsConfigurations $
--             newListBucketMetricsConfigurationsResponse
--
--         , responsePutBucketAccelerateConfiguration $
--             newPutBucketAccelerateConfigurationResponse
--
--         , responseHeadObject $
--             newHeadObjectResponse
--
--         , responsePutBucketInventoryConfiguration $
--             newPutBucketInventoryConfigurationResponse
--
--         , responseWriteGetObjectResponse $
--             newWriteGetObjectResponseResponse
--
--         , responseGetBucketLocation $
--             newGetBucketLocationResponse
--
--         , responseDeletePublicAccessBlock $
--             newDeletePublicAccessBlockResponse
--
--         , responseListBucketInventoryConfigurations $
--             newListBucketInventoryConfigurationsResponse
--
--         , responseGetBucketTagging $
--             newGetBucketTaggingResponse
--
--         , responseGetObjectAcl $
--             newGetObjectAclResponse
--
--         , responsePutObjectAcl $
--             newPutObjectAclResponse
--
--         , responseCreateBucket $
--             newCreateBucketResponse
--
--         , responseGetBucketVersioning $
--             newGetBucketVersioningResponse
--
--         , responsePutBucketTagging $
--             newPutBucketTaggingResponse
--
--         , responsePutBucketCors $
--             newPutBucketCorsResponse
--
--         , responseDeleteBucketIntelligentTieringConfiguration $
--             newDeleteBucketIntelligentTieringConfigurationResponse
--
--         , responseDeleteBucketPolicy $
--             newDeleteBucketPolicyResponse
--
--         , responseGetObjectRetention $
--             newGetObjectRetentionResponse
--
--         , responsePutObjectLockConfiguration $
--             newPutObjectLockConfigurationResponse
--
--         , responseAbortMultipartUpload $
--             newAbortMultipartUploadResponse
--
--         , responseGetBucketOwnershipControls $
--             newGetBucketOwnershipControlsResponse
--
--         , responseGetObjectLegalHold $
--             newGetObjectLegalHoldResponse
--
--         , responseGetBucketMetricsConfiguration $
--             newGetBucketMetricsConfigurationResponse
--
--         , responseGetBucketAccelerateConfiguration $
--             newGetBucketAccelerateConfigurationResponse
--
--         , responseListBuckets $
--             newListBucketsResponse
--
--         , responseGetObjectTorrent $
--             newGetObjectTorrentResponse
--
--         , responsePutObject $
--             newPutObjectResponse
--
--         , responsePutBucketWebsite $
--             newPutBucketWebsiteResponse
--
--         , responseListMultipartUploads $
--             newListMultipartUploadsResponse
--
--         , responsePutBucketRequestPayment $
--             newPutBucketRequestPaymentResponse
--
--         , responseUploadPart $
--             newUploadPartResponse
--
--         , responseGetBucketReplication $
--             newGetBucketReplicationResponse
--
--         , responsePutObjectTagging $
--             newPutObjectTaggingResponse
--
--         , responseUploadPartCopy $
--             newUploadPartCopyResponse
--
--         , responsePutBucketAnalyticsConfiguration $
--             newPutBucketAnalyticsConfigurationResponse
--
--         , responseListBucketAnalyticsConfigurations $
--             newListBucketAnalyticsConfigurationsResponse
--
--         , responsePutBucketAcl $
--             newPutBucketAclResponse
--
--         , responseDeleteBucketLifecycle $
--             newDeleteBucketLifecycleResponse
--
--         , responsePutBucketLifecycleConfiguration $
--             newPutBucketLifecycleConfigurationResponse
--
--         , responseCreateMultipartUpload $
--             newCreateMultipartUploadResponse
--
--         , responseGetBucketLifecycleConfiguration $
--             newGetBucketLifecycleConfigurationResponse
--
--         , responseListParts $
--             newListPartsResponse
--
--         , responseGetBucketAnalyticsConfiguration $
--             newGetBucketAnalyticsConfigurationResponse
--
--         , responseListObjectVersions $
--             newListObjectVersionsResponse
--
--         , responseHeadBucket $
--             newHeadBucketResponse
--
--         , responseGetObject $
--             newGetObjectResponse
--
--         , responseGetBucketLogging $
--             newGetBucketLoggingResponse
--
--         , responseGetBucketRequestPayment $
--             newGetBucketRequestPaymentResponse
--
--         , responseDeleteBucketReplication $
--             newDeleteBucketReplicationResponse
--
--         , responseListObjectsV $
--             newListObjectsVResponse
--
--         , responseDeleteBucketEncryption $
--             newDeleteBucketEncryptionResponse
--
--         , responseDeleteBucketOwnershipControls $
--             newDeleteBucketOwnershipControlsResponse
--
--         , responsePutBucketMetricsConfiguration $
--             newPutBucketMetricsConfigurationResponse
--
--         , responseCopyObject $
--             newCopyObjectResponse
--
--         , responsePutPublicAccessBlock $
--             newPutPublicAccessBlockResponse
--
--         , responseGetBucketNotificationConfiguration $
--             newNotificationConfiguration
--
--         , responseGetBucketCors $
--             newGetBucketCorsResponse
--
--         , responsePutBucketVersioning $
--             newPutBucketVersioningResponse
--
--         , responseRestoreObject $
--             newRestoreObjectResponse
--
--         , responseGetBucketIntelligentTieringConfiguration $
--             newGetBucketIntelligentTieringConfigurationResponse
--
--         , responseDeleteBucketInventoryConfiguration $
--             newDeleteBucketInventoryConfigurationResponse
--
--         , responseDeleteBucketTagging $
--             newDeleteBucketTaggingResponse
--
--         , responseGetPublicAccessBlock $
--             newGetPublicAccessBlockResponse
--
--         , responsePutBucketIntelligentTieringConfiguration $
--             newPutBucketIntelligentTieringConfigurationResponse
--
--         , responseGetBucketInventoryConfiguration $
--             newGetBucketInventoryConfigurationResponse
--
--         , responseDeleteBucketCors $
--             newDeleteBucketCorsResponse
--
--         , responseListBucketIntelligentTieringConfigurations $
--             newListBucketIntelligentTieringConfigurationsResponse
--
--         , responsePutBucketNotificationConfiguration $
--             newPutBucketNotificationConfigurationResponse
--
--           ]
--     ]

-- Requests

requestPutBucketPolicy :: PutBucketPolicy -> TestTree
requestPutBucketPolicy =
  req
    "PutBucketPolicy"
    "fixture/PutBucketPolicy.yaml"

requestDeleteBucket :: DeleteBucket -> TestTree
requestDeleteBucket =
  req
    "DeleteBucket"
    "fixture/DeleteBucket.yaml"

requestDeleteObjects :: DeleteObjects -> TestTree
requestDeleteObjects =
  req
    "DeleteObjects"
    "fixture/DeleteObjects.yaml"

requestGetBucketEncryption :: GetBucketEncryption -> TestTree
requestGetBucketEncryption =
  req
    "GetBucketEncryption"
    "fixture/GetBucketEncryption.yaml"

requestCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
requestCompleteMultipartUpload =
  req
    "CompleteMultipartUpload"
    "fixture/CompleteMultipartUpload.yaml"

requestGetBucketPolicyStatus :: GetBucketPolicyStatus -> TestTree
requestGetBucketPolicyStatus =
  req
    "GetBucketPolicyStatus"
    "fixture/GetBucketPolicyStatus.yaml"

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject =
  req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestSelectObjectContent :: SelectObjectContent -> TestTree
requestSelectObjectContent =
  req
    "SelectObjectContent"
    "fixture/SelectObjectContent.yaml"

requestDeleteBucketWebsite :: DeleteBucketWebsite -> TestTree
requestDeleteBucketWebsite =
  req
    "DeleteBucketWebsite"
    "fixture/DeleteBucketWebsite.yaml"

requestPutBucketLogging :: PutBucketLogging -> TestTree
requestPutBucketLogging =
  req
    "PutBucketLogging"
    "fixture/PutBucketLogging.yaml"

requestListObjects :: ListObjects -> TestTree
requestListObjects =
  req
    "ListObjects"
    "fixture/ListObjects.yaml"

requestDeleteObjectTagging :: DeleteObjectTagging -> TestTree
requestDeleteObjectTagging =
  req
    "DeleteObjectTagging"
    "fixture/DeleteObjectTagging.yaml"

requestDeleteBucketAnalyticsConfiguration :: DeleteBucketAnalyticsConfiguration -> TestTree
requestDeleteBucketAnalyticsConfiguration =
  req
    "DeleteBucketAnalyticsConfiguration"
    "fixture/DeleteBucketAnalyticsConfiguration.yaml"

requestGetObjectTagging :: GetObjectTagging -> TestTree
requestGetObjectTagging =
  req
    "GetObjectTagging"
    "fixture/GetObjectTagging.yaml"

requestGetBucketAcl :: GetBucketAcl -> TestTree
requestGetBucketAcl =
  req
    "GetBucketAcl"
    "fixture/GetBucketAcl.yaml"

requestGetBucketWebsite :: GetBucketWebsite -> TestTree
requestGetBucketWebsite =
  req
    "GetBucketWebsite"
    "fixture/GetBucketWebsite.yaml"

requestPutBucketReplication :: PutBucketReplication -> TestTree
requestPutBucketReplication =
  req
    "PutBucketReplication"
    "fixture/PutBucketReplication.yaml"

requestDeleteBucketMetricsConfiguration :: DeleteBucketMetricsConfiguration -> TestTree
requestDeleteBucketMetricsConfiguration =
  req
    "DeleteBucketMetricsConfiguration"
    "fixture/DeleteBucketMetricsConfiguration.yaml"

requestPutBucketOwnershipControls :: PutBucketOwnershipControls -> TestTree
requestPutBucketOwnershipControls =
  req
    "PutBucketOwnershipControls"
    "fixture/PutBucketOwnershipControls.yaml"

requestPutObjectLegalHold :: PutObjectLegalHold -> TestTree
requestPutObjectLegalHold =
  req
    "PutObjectLegalHold"
    "fixture/PutObjectLegalHold.yaml"

requestGetBucketPolicy :: GetBucketPolicy -> TestTree
requestGetBucketPolicy =
  req
    "GetBucketPolicy"
    "fixture/GetBucketPolicy.yaml"

requestPutObjectRetention :: PutObjectRetention -> TestTree
requestPutObjectRetention =
  req
    "PutObjectRetention"
    "fixture/PutObjectRetention.yaml"

requestPutBucketEncryption :: PutBucketEncryption -> TestTree
requestPutBucketEncryption =
  req
    "PutBucketEncryption"
    "fixture/PutBucketEncryption.yaml"

requestGetObjectLockConfiguration :: GetObjectLockConfiguration -> TestTree
requestGetObjectLockConfiguration =
  req
    "GetObjectLockConfiguration"
    "fixture/GetObjectLockConfiguration.yaml"

requestListBucketMetricsConfigurations :: ListBucketMetricsConfigurations -> TestTree
requestListBucketMetricsConfigurations =
  req
    "ListBucketMetricsConfigurations"
    "fixture/ListBucketMetricsConfigurations.yaml"

requestPutBucketAccelerateConfiguration :: PutBucketAccelerateConfiguration -> TestTree
requestPutBucketAccelerateConfiguration =
  req
    "PutBucketAccelerateConfiguration"
    "fixture/PutBucketAccelerateConfiguration.yaml"

requestHeadObject :: HeadObject -> TestTree
requestHeadObject =
  req
    "HeadObject"
    "fixture/HeadObject.yaml"

requestPutBucketInventoryConfiguration :: PutBucketInventoryConfiguration -> TestTree
requestPutBucketInventoryConfiguration =
  req
    "PutBucketInventoryConfiguration"
    "fixture/PutBucketInventoryConfiguration.yaml"

requestGetBucketLocation :: GetBucketLocation -> TestTree
requestGetBucketLocation =
  req
    "GetBucketLocation"
    "fixture/GetBucketLocation.yaml"

requestDeletePublicAccessBlock :: DeletePublicAccessBlock -> TestTree
requestDeletePublicAccessBlock =
  req
    "DeletePublicAccessBlock"
    "fixture/DeletePublicAccessBlock.yaml"

requestListBucketInventoryConfigurations :: ListBucketInventoryConfigurations -> TestTree
requestListBucketInventoryConfigurations =
  req
    "ListBucketInventoryConfigurations"
    "fixture/ListBucketInventoryConfigurations.yaml"

requestGetBucketTagging :: GetBucketTagging -> TestTree
requestGetBucketTagging =
  req
    "GetBucketTagging"
    "fixture/GetBucketTagging.yaml"

requestGetObjectAcl :: GetObjectAcl -> TestTree
requestGetObjectAcl =
  req
    "GetObjectAcl"
    "fixture/GetObjectAcl.yaml"

requestPutObjectAcl :: PutObjectAcl -> TestTree
requestPutObjectAcl =
  req
    "PutObjectAcl"
    "fixture/PutObjectAcl.yaml"

requestCreateBucket :: CreateBucket -> TestTree
requestCreateBucket =
  req
    "CreateBucket"
    "fixture/CreateBucket.yaml"

requestGetBucketVersioning :: GetBucketVersioning -> TestTree
requestGetBucketVersioning =
  req
    "GetBucketVersioning"
    "fixture/GetBucketVersioning.yaml"

requestPutBucketTagging :: PutBucketTagging -> TestTree
requestPutBucketTagging =
  req
    "PutBucketTagging"
    "fixture/PutBucketTagging.yaml"

requestPutBucketCors :: PutBucketCors -> TestTree
requestPutBucketCors =
  req
    "PutBucketCors"
    "fixture/PutBucketCors.yaml"

requestDeleteBucketIntelligentTieringConfiguration :: DeleteBucketIntelligentTieringConfiguration -> TestTree
requestDeleteBucketIntelligentTieringConfiguration =
  req
    "DeleteBucketIntelligentTieringConfiguration"
    "fixture/DeleteBucketIntelligentTieringConfiguration.yaml"

requestDeleteBucketPolicy :: DeleteBucketPolicy -> TestTree
requestDeleteBucketPolicy =
  req
    "DeleteBucketPolicy"
    "fixture/DeleteBucketPolicy.yaml"

requestGetObjectRetention :: GetObjectRetention -> TestTree
requestGetObjectRetention =
  req
    "GetObjectRetention"
    "fixture/GetObjectRetention.yaml"

requestPutObjectLockConfiguration :: PutObjectLockConfiguration -> TestTree
requestPutObjectLockConfiguration =
  req
    "PutObjectLockConfiguration"
    "fixture/PutObjectLockConfiguration.yaml"

requestAbortMultipartUpload :: AbortMultipartUpload -> TestTree
requestAbortMultipartUpload =
  req
    "AbortMultipartUpload"
    "fixture/AbortMultipartUpload.yaml"

requestGetBucketOwnershipControls :: GetBucketOwnershipControls -> TestTree
requestGetBucketOwnershipControls =
  req
    "GetBucketOwnershipControls"
    "fixture/GetBucketOwnershipControls.yaml"

requestGetObjectLegalHold :: GetObjectLegalHold -> TestTree
requestGetObjectLegalHold =
  req
    "GetObjectLegalHold"
    "fixture/GetObjectLegalHold.yaml"

requestGetBucketMetricsConfiguration :: GetBucketMetricsConfiguration -> TestTree
requestGetBucketMetricsConfiguration =
  req
    "GetBucketMetricsConfiguration"
    "fixture/GetBucketMetricsConfiguration.yaml"

requestGetBucketAccelerateConfiguration :: GetBucketAccelerateConfiguration -> TestTree
requestGetBucketAccelerateConfiguration =
  req
    "GetBucketAccelerateConfiguration"
    "fixture/GetBucketAccelerateConfiguration.yaml"

requestListBuckets :: ListBuckets -> TestTree
requestListBuckets =
  req
    "ListBuckets"
    "fixture/ListBuckets.yaml"

requestGetObjectTorrent :: GetObjectTorrent -> TestTree
requestGetObjectTorrent =
  req
    "GetObjectTorrent"
    "fixture/GetObjectTorrent.yaml"

requestPutBucketWebsite :: PutBucketWebsite -> TestTree
requestPutBucketWebsite =
  req
    "PutBucketWebsite"
    "fixture/PutBucketWebsite.yaml"

requestListMultipartUploads :: ListMultipartUploads -> TestTree
requestListMultipartUploads =
  req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads.yaml"

requestPutBucketRequestPayment :: PutBucketRequestPayment -> TestTree
requestPutBucketRequestPayment =
  req
    "PutBucketRequestPayment"
    "fixture/PutBucketRequestPayment.yaml"

requestGetBucketReplication :: GetBucketReplication -> TestTree
requestGetBucketReplication =
  req
    "GetBucketReplication"
    "fixture/GetBucketReplication.yaml"

requestPutObjectTagging :: PutObjectTagging -> TestTree
requestPutObjectTagging =
  req
    "PutObjectTagging"
    "fixture/PutObjectTagging.yaml"

requestUploadPartCopy :: UploadPartCopy -> TestTree
requestUploadPartCopy =
  req
    "UploadPartCopy"
    "fixture/UploadPartCopy.yaml"

requestPutBucketAnalyticsConfiguration :: PutBucketAnalyticsConfiguration -> TestTree
requestPutBucketAnalyticsConfiguration =
  req
    "PutBucketAnalyticsConfiguration"
    "fixture/PutBucketAnalyticsConfiguration.yaml"

requestListBucketAnalyticsConfigurations :: ListBucketAnalyticsConfigurations -> TestTree
requestListBucketAnalyticsConfigurations =
  req
    "ListBucketAnalyticsConfigurations"
    "fixture/ListBucketAnalyticsConfigurations.yaml"

requestPutBucketAcl :: PutBucketAcl -> TestTree
requestPutBucketAcl =
  req
    "PutBucketAcl"
    "fixture/PutBucketAcl.yaml"

requestDeleteBucketLifecycle :: DeleteBucketLifecycle -> TestTree
requestDeleteBucketLifecycle =
  req
    "DeleteBucketLifecycle"
    "fixture/DeleteBucketLifecycle.yaml"

requestPutBucketLifecycleConfiguration :: PutBucketLifecycleConfiguration -> TestTree
requestPutBucketLifecycleConfiguration =
  req
    "PutBucketLifecycleConfiguration"
    "fixture/PutBucketLifecycleConfiguration.yaml"

requestCreateMultipartUpload :: CreateMultipartUpload -> TestTree
requestCreateMultipartUpload =
  req
    "CreateMultipartUpload"
    "fixture/CreateMultipartUpload.yaml"

requestGetBucketLifecycleConfiguration :: GetBucketLifecycleConfiguration -> TestTree
requestGetBucketLifecycleConfiguration =
  req
    "GetBucketLifecycleConfiguration"
    "fixture/GetBucketLifecycleConfiguration.yaml"

requestListParts :: ListParts -> TestTree
requestListParts =
  req
    "ListParts"
    "fixture/ListParts.yaml"

requestGetBucketAnalyticsConfiguration :: GetBucketAnalyticsConfiguration -> TestTree
requestGetBucketAnalyticsConfiguration =
  req
    "GetBucketAnalyticsConfiguration"
    "fixture/GetBucketAnalyticsConfiguration.yaml"

requestListObjectVersions :: ListObjectVersions -> TestTree
requestListObjectVersions =
  req
    "ListObjectVersions"
    "fixture/ListObjectVersions.yaml"

requestHeadBucket :: HeadBucket -> TestTree
requestHeadBucket =
  req
    "HeadBucket"
    "fixture/HeadBucket.yaml"

requestGetObject :: GetObject -> TestTree
requestGetObject =
  req
    "GetObject"
    "fixture/GetObject.yaml"

requestGetBucketLogging :: GetBucketLogging -> TestTree
requestGetBucketLogging =
  req
    "GetBucketLogging"
    "fixture/GetBucketLogging.yaml"

requestGetBucketRequestPayment :: GetBucketRequestPayment -> TestTree
requestGetBucketRequestPayment =
  req
    "GetBucketRequestPayment"
    "fixture/GetBucketRequestPayment.yaml"

requestDeleteBucketReplication :: DeleteBucketReplication -> TestTree
requestDeleteBucketReplication =
  req
    "DeleteBucketReplication"
    "fixture/DeleteBucketReplication.yaml"

requestListObjectsV :: ListObjectsV -> TestTree
requestListObjectsV =
  req
    "ListObjectsV"
    "fixture/ListObjectsV.yaml"

requestDeleteBucketEncryption :: DeleteBucketEncryption -> TestTree
requestDeleteBucketEncryption =
  req
    "DeleteBucketEncryption"
    "fixture/DeleteBucketEncryption.yaml"

requestDeleteBucketOwnershipControls :: DeleteBucketOwnershipControls -> TestTree
requestDeleteBucketOwnershipControls =
  req
    "DeleteBucketOwnershipControls"
    "fixture/DeleteBucketOwnershipControls.yaml"

requestPutBucketMetricsConfiguration :: PutBucketMetricsConfiguration -> TestTree
requestPutBucketMetricsConfiguration =
  req
    "PutBucketMetricsConfiguration"
    "fixture/PutBucketMetricsConfiguration.yaml"

requestCopyObject :: CopyObject -> TestTree
requestCopyObject =
  req
    "CopyObject"
    "fixture/CopyObject.yaml"

requestPutPublicAccessBlock :: PutPublicAccessBlock -> TestTree
requestPutPublicAccessBlock =
  req
    "PutPublicAccessBlock"
    "fixture/PutPublicAccessBlock.yaml"

requestGetBucketNotificationConfiguration :: GetBucketNotificationConfiguration -> TestTree
requestGetBucketNotificationConfiguration =
  req
    "GetBucketNotificationConfiguration"
    "fixture/GetBucketNotificationConfiguration.yaml"

requestGetBucketCors :: GetBucketCors -> TestTree
requestGetBucketCors =
  req
    "GetBucketCors"
    "fixture/GetBucketCors.yaml"

requestPutBucketVersioning :: PutBucketVersioning -> TestTree
requestPutBucketVersioning =
  req
    "PutBucketVersioning"
    "fixture/PutBucketVersioning.yaml"

requestRestoreObject :: RestoreObject -> TestTree
requestRestoreObject =
  req
    "RestoreObject"
    "fixture/RestoreObject.yaml"

requestGetBucketIntelligentTieringConfiguration :: GetBucketIntelligentTieringConfiguration -> TestTree
requestGetBucketIntelligentTieringConfiguration =
  req
    "GetBucketIntelligentTieringConfiguration"
    "fixture/GetBucketIntelligentTieringConfiguration.yaml"

requestDeleteBucketInventoryConfiguration :: DeleteBucketInventoryConfiguration -> TestTree
requestDeleteBucketInventoryConfiguration =
  req
    "DeleteBucketInventoryConfiguration"
    "fixture/DeleteBucketInventoryConfiguration.yaml"

requestDeleteBucketTagging :: DeleteBucketTagging -> TestTree
requestDeleteBucketTagging =
  req
    "DeleteBucketTagging"
    "fixture/DeleteBucketTagging.yaml"

requestGetPublicAccessBlock :: GetPublicAccessBlock -> TestTree
requestGetPublicAccessBlock =
  req
    "GetPublicAccessBlock"
    "fixture/GetPublicAccessBlock.yaml"

requestPutBucketIntelligentTieringConfiguration :: PutBucketIntelligentTieringConfiguration -> TestTree
requestPutBucketIntelligentTieringConfiguration =
  req
    "PutBucketIntelligentTieringConfiguration"
    "fixture/PutBucketIntelligentTieringConfiguration.yaml"

requestGetBucketInventoryConfiguration :: GetBucketInventoryConfiguration -> TestTree
requestGetBucketInventoryConfiguration =
  req
    "GetBucketInventoryConfiguration"
    "fixture/GetBucketInventoryConfiguration.yaml"

requestDeleteBucketCors :: DeleteBucketCors -> TestTree
requestDeleteBucketCors =
  req
    "DeleteBucketCors"
    "fixture/DeleteBucketCors.yaml"

requestListBucketIntelligentTieringConfigurations :: ListBucketIntelligentTieringConfigurations -> TestTree
requestListBucketIntelligentTieringConfigurations =
  req
    "ListBucketIntelligentTieringConfigurations"
    "fixture/ListBucketIntelligentTieringConfigurations.yaml"

requestPutBucketNotificationConfiguration :: PutBucketNotificationConfiguration -> TestTree
requestPutBucketNotificationConfiguration =
  req
    "PutBucketNotificationConfiguration"
    "fixture/PutBucketNotificationConfiguration.yaml"

-- Responses

responsePutBucketPolicy :: PutBucketPolicyResponse -> TestTree
responsePutBucketPolicy =
  res
    "PutBucketPolicyResponse"
    "fixture/PutBucketPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketPolicy)

responseDeleteBucket :: DeleteBucketResponse -> TestTree
responseDeleteBucket =
  res
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucket)

responseDeleteObjects :: DeleteObjectsResponse -> TestTree
responseDeleteObjects =
  res
    "DeleteObjectsResponse"
    "fixture/DeleteObjectsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteObjects)

responseGetBucketEncryption :: GetBucketEncryptionResponse -> TestTree
responseGetBucketEncryption =
  res
    "GetBucketEncryptionResponse"
    "fixture/GetBucketEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketEncryption)

responseCompleteMultipartUpload :: CompleteMultipartUploadResponse -> TestTree
responseCompleteMultipartUpload =
  res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    defaultService
    (Proxy :: Proxy CompleteMultipartUpload)

responseGetBucketPolicyStatus :: GetBucketPolicyStatusResponse -> TestTree
responseGetBucketPolicyStatus =
  res
    "GetBucketPolicyStatusResponse"
    "fixture/GetBucketPolicyStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketPolicyStatus)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject =
  res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteObject)

responseSelectObjectContent :: SelectObjectContentResponse -> TestTree
responseSelectObjectContent =
  res
    "SelectObjectContentResponse"
    "fixture/SelectObjectContentResponse.proto"
    defaultService
    (Proxy :: Proxy SelectObjectContent)

responseDeleteBucketWebsite :: DeleteBucketWebsiteResponse -> TestTree
responseDeleteBucketWebsite =
  res
    "DeleteBucketWebsiteResponse"
    "fixture/DeleteBucketWebsiteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketWebsite)

responsePutBucketLogging :: PutBucketLoggingResponse -> TestTree
responsePutBucketLogging =
  res
    "PutBucketLoggingResponse"
    "fixture/PutBucketLoggingResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketLogging)

responseListObjects :: ListObjectsResponse -> TestTree
responseListObjects =
  res
    "ListObjectsResponse"
    "fixture/ListObjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjects)

responseDeleteObjectTagging :: DeleteObjectTaggingResponse -> TestTree
responseDeleteObjectTagging =
  res
    "DeleteObjectTaggingResponse"
    "fixture/DeleteObjectTaggingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteObjectTagging)

responseDeleteBucketAnalyticsConfiguration :: DeleteBucketAnalyticsConfigurationResponse -> TestTree
responseDeleteBucketAnalyticsConfiguration =
  res
    "DeleteBucketAnalyticsConfigurationResponse"
    "fixture/DeleteBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketAnalyticsConfiguration)

responseGetObjectTagging :: GetObjectTaggingResponse -> TestTree
responseGetObjectTagging =
  res
    "GetObjectTaggingResponse"
    "fixture/GetObjectTaggingResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectTagging)

responseGetBucketAcl :: GetBucketAclResponse -> TestTree
responseGetBucketAcl =
  res
    "GetBucketAclResponse"
    "fixture/GetBucketAclResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketAcl)

responseGetBucketWebsite :: GetBucketWebsiteResponse -> TestTree
responseGetBucketWebsite =
  res
    "GetBucketWebsiteResponse"
    "fixture/GetBucketWebsiteResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketWebsite)

responsePutBucketReplication :: PutBucketReplicationResponse -> TestTree
responsePutBucketReplication =
  res
    "PutBucketReplicationResponse"
    "fixture/PutBucketReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketReplication)

responseDeleteBucketMetricsConfiguration :: DeleteBucketMetricsConfigurationResponse -> TestTree
responseDeleteBucketMetricsConfiguration =
  res
    "DeleteBucketMetricsConfigurationResponse"
    "fixture/DeleteBucketMetricsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketMetricsConfiguration)

responsePutBucketOwnershipControls :: PutBucketOwnershipControlsResponse -> TestTree
responsePutBucketOwnershipControls =
  res
    "PutBucketOwnershipControlsResponse"
    "fixture/PutBucketOwnershipControlsResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketOwnershipControls)

responsePutObjectLegalHold :: PutObjectLegalHoldResponse -> TestTree
responsePutObjectLegalHold =
  res
    "PutObjectLegalHoldResponse"
    "fixture/PutObjectLegalHoldResponse.proto"
    defaultService
    (Proxy :: Proxy PutObjectLegalHold)

responseGetBucketPolicy :: GetBucketPolicyResponse -> TestTree
responseGetBucketPolicy =
  res
    "GetBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketPolicy)

responsePutObjectRetention :: PutObjectRetentionResponse -> TestTree
responsePutObjectRetention =
  res
    "PutObjectRetentionResponse"
    "fixture/PutObjectRetentionResponse.proto"
    defaultService
    (Proxy :: Proxy PutObjectRetention)

responsePutBucketEncryption :: PutBucketEncryptionResponse -> TestTree
responsePutBucketEncryption =
  res
    "PutBucketEncryptionResponse"
    "fixture/PutBucketEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketEncryption)

responseGetObjectLockConfiguration :: GetObjectLockConfigurationResponse -> TestTree
responseGetObjectLockConfiguration =
  res
    "GetObjectLockConfigurationResponse"
    "fixture/GetObjectLockConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectLockConfiguration)

responseListBucketMetricsConfigurations :: ListBucketMetricsConfigurationsResponse -> TestTree
responseListBucketMetricsConfigurations =
  res
    "ListBucketMetricsConfigurationsResponse"
    "fixture/ListBucketMetricsConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBucketMetricsConfigurations)

responsePutBucketAccelerateConfiguration :: PutBucketAccelerateConfigurationResponse -> TestTree
responsePutBucketAccelerateConfiguration =
  res
    "PutBucketAccelerateConfigurationResponse"
    "fixture/PutBucketAccelerateConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketAccelerateConfiguration)

responseHeadObject :: HeadObjectResponse -> TestTree
responseHeadObject =
  res
    "HeadObjectResponse"
    "fixture/HeadObjectResponse.proto"
    defaultService
    (Proxy :: Proxy HeadObject)

responsePutBucketInventoryConfiguration :: PutBucketInventoryConfigurationResponse -> TestTree
responsePutBucketInventoryConfiguration =
  res
    "PutBucketInventoryConfigurationResponse"
    "fixture/PutBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketInventoryConfiguration)

responseWriteGetObjectResponse :: WriteGetObjectResponseResponse -> TestTree
responseWriteGetObjectResponse =
  res
    "WriteGetObjectResponseResponse"
    "fixture/WriteGetObjectResponseResponse.proto"
    defaultService
    (Proxy :: Proxy WriteGetObjectResponse)

responseGetBucketLocation :: GetBucketLocationResponse -> TestTree
responseGetBucketLocation =
  res
    "GetBucketLocationResponse"
    "fixture/GetBucketLocationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketLocation)

responseDeletePublicAccessBlock :: DeletePublicAccessBlockResponse -> TestTree
responseDeletePublicAccessBlock =
  res
    "DeletePublicAccessBlockResponse"
    "fixture/DeletePublicAccessBlockResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePublicAccessBlock)

responseListBucketInventoryConfigurations :: ListBucketInventoryConfigurationsResponse -> TestTree
responseListBucketInventoryConfigurations =
  res
    "ListBucketInventoryConfigurationsResponse"
    "fixture/ListBucketInventoryConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBucketInventoryConfigurations)

responseGetBucketTagging :: GetBucketTaggingResponse -> TestTree
responseGetBucketTagging =
  res
    "GetBucketTaggingResponse"
    "fixture/GetBucketTaggingResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketTagging)

responseGetObjectAcl :: GetObjectAclResponse -> TestTree
responseGetObjectAcl =
  res
    "GetObjectAclResponse"
    "fixture/GetObjectAclResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectAcl)

responsePutObjectAcl :: PutObjectAclResponse -> TestTree
responsePutObjectAcl =
  res
    "PutObjectAclResponse"
    "fixture/PutObjectAclResponse.proto"
    defaultService
    (Proxy :: Proxy PutObjectAcl)

responseCreateBucket :: CreateBucketResponse -> TestTree
responseCreateBucket =
  res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBucket)

responseGetBucketVersioning :: GetBucketVersioningResponse -> TestTree
responseGetBucketVersioning =
  res
    "GetBucketVersioningResponse"
    "fixture/GetBucketVersioningResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketVersioning)

responsePutBucketTagging :: PutBucketTaggingResponse -> TestTree
responsePutBucketTagging =
  res
    "PutBucketTaggingResponse"
    "fixture/PutBucketTaggingResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketTagging)

responsePutBucketCors :: PutBucketCorsResponse -> TestTree
responsePutBucketCors =
  res
    "PutBucketCorsResponse"
    "fixture/PutBucketCorsResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketCors)

responseDeleteBucketIntelligentTieringConfiguration :: DeleteBucketIntelligentTieringConfigurationResponse -> TestTree
responseDeleteBucketIntelligentTieringConfiguration =
  res
    "DeleteBucketIntelligentTieringConfigurationResponse"
    "fixture/DeleteBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketIntelligentTieringConfiguration)

responseDeleteBucketPolicy :: DeleteBucketPolicyResponse -> TestTree
responseDeleteBucketPolicy =
  res
    "DeleteBucketPolicyResponse"
    "fixture/DeleteBucketPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketPolicy)

responseGetObjectRetention :: GetObjectRetentionResponse -> TestTree
responseGetObjectRetention =
  res
    "GetObjectRetentionResponse"
    "fixture/GetObjectRetentionResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectRetention)

responsePutObjectLockConfiguration :: PutObjectLockConfigurationResponse -> TestTree
responsePutObjectLockConfiguration =
  res
    "PutObjectLockConfigurationResponse"
    "fixture/PutObjectLockConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutObjectLockConfiguration)

responseAbortMultipartUpload :: AbortMultipartUploadResponse -> TestTree
responseAbortMultipartUpload =
  res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse.proto"
    defaultService
    (Proxy :: Proxy AbortMultipartUpload)

responseGetBucketOwnershipControls :: GetBucketOwnershipControlsResponse -> TestTree
responseGetBucketOwnershipControls =
  res
    "GetBucketOwnershipControlsResponse"
    "fixture/GetBucketOwnershipControlsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketOwnershipControls)

responseGetObjectLegalHold :: GetObjectLegalHoldResponse -> TestTree
responseGetObjectLegalHold =
  res
    "GetObjectLegalHoldResponse"
    "fixture/GetObjectLegalHoldResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectLegalHold)

responseGetBucketMetricsConfiguration :: GetBucketMetricsConfigurationResponse -> TestTree
responseGetBucketMetricsConfiguration =
  res
    "GetBucketMetricsConfigurationResponse"
    "fixture/GetBucketMetricsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketMetricsConfiguration)

responseGetBucketAccelerateConfiguration :: GetBucketAccelerateConfigurationResponse -> TestTree
responseGetBucketAccelerateConfiguration =
  res
    "GetBucketAccelerateConfigurationResponse"
    "fixture/GetBucketAccelerateConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketAccelerateConfiguration)

responseListBuckets :: ListBucketsResponse -> TestTree
responseListBuckets =
  res
    "ListBucketsResponse"
    "fixture/ListBucketsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuckets)

responsePutObject :: PutObjectResponse -> TestTree
responsePutObject =
  res
    "PutObjectResponse"
    "fixture/PutObjectResponse.proto"
    defaultService
    (Proxy :: Proxy PutObject)

responsePutBucketWebsite :: PutBucketWebsiteResponse -> TestTree
responsePutBucketWebsite =
  res
    "PutBucketWebsiteResponse"
    "fixture/PutBucketWebsiteResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketWebsite)

responseListMultipartUploads :: ListMultipartUploadsResponse -> TestTree
responseListMultipartUploads =
  res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMultipartUploads)

responsePutBucketRequestPayment :: PutBucketRequestPaymentResponse -> TestTree
responsePutBucketRequestPayment =
  res
    "PutBucketRequestPaymentResponse"
    "fixture/PutBucketRequestPaymentResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketRequestPayment)

responseUploadPart :: UploadPartResponse -> TestTree
responseUploadPart =
  res
    "UploadPartResponse"
    "fixture/UploadPartResponse.proto"
    defaultService
    (Proxy :: Proxy UploadPart)

responseGetBucketReplication :: GetBucketReplicationResponse -> TestTree
responseGetBucketReplication =
  res
    "GetBucketReplicationResponse"
    "fixture/GetBucketReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketReplication)

responsePutObjectTagging :: PutObjectTaggingResponse -> TestTree
responsePutObjectTagging =
  res
    "PutObjectTaggingResponse"
    "fixture/PutObjectTaggingResponse.proto"
    defaultService
    (Proxy :: Proxy PutObjectTagging)

responseUploadPartCopy :: UploadPartCopyResponse -> TestTree
responseUploadPartCopy =
  res
    "UploadPartCopyResponse"
    "fixture/UploadPartCopyResponse.proto"
    defaultService
    (Proxy :: Proxy UploadPartCopy)

responsePutBucketAnalyticsConfiguration :: PutBucketAnalyticsConfigurationResponse -> TestTree
responsePutBucketAnalyticsConfiguration =
  res
    "PutBucketAnalyticsConfigurationResponse"
    "fixture/PutBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketAnalyticsConfiguration)

responseListBucketAnalyticsConfigurations :: ListBucketAnalyticsConfigurationsResponse -> TestTree
responseListBucketAnalyticsConfigurations =
  res
    "ListBucketAnalyticsConfigurationsResponse"
    "fixture/ListBucketAnalyticsConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBucketAnalyticsConfigurations)

responsePutBucketAcl :: PutBucketAclResponse -> TestTree
responsePutBucketAcl =
  res
    "PutBucketAclResponse"
    "fixture/PutBucketAclResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketAcl)

responseDeleteBucketLifecycle :: DeleteBucketLifecycleResponse -> TestTree
responseDeleteBucketLifecycle =
  res
    "DeleteBucketLifecycleResponse"
    "fixture/DeleteBucketLifecycleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketLifecycle)

responsePutBucketLifecycleConfiguration :: PutBucketLifecycleConfigurationResponse -> TestTree
responsePutBucketLifecycleConfiguration =
  res
    "PutBucketLifecycleConfigurationResponse"
    "fixture/PutBucketLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketLifecycleConfiguration)

responseCreateMultipartUpload :: CreateMultipartUploadResponse -> TestTree
responseCreateMultipartUpload =
  res
    "CreateMultipartUploadResponse"
    "fixture/CreateMultipartUploadResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMultipartUpload)

responseGetBucketLifecycleConfiguration :: GetBucketLifecycleConfigurationResponse -> TestTree
responseGetBucketLifecycleConfiguration =
  res
    "GetBucketLifecycleConfigurationResponse"
    "fixture/GetBucketLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketLifecycleConfiguration)

responseListParts :: ListPartsResponse -> TestTree
responseListParts =
  res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    defaultService
    (Proxy :: Proxy ListParts)

responseGetBucketAnalyticsConfiguration :: GetBucketAnalyticsConfigurationResponse -> TestTree
responseGetBucketAnalyticsConfiguration =
  res
    "GetBucketAnalyticsConfigurationResponse"
    "fixture/GetBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketAnalyticsConfiguration)

responseListObjectVersions :: ListObjectVersionsResponse -> TestTree
responseListObjectVersions =
  res
    "ListObjectVersionsResponse"
    "fixture/ListObjectVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjectVersions)

responseHeadBucket :: HeadBucketResponse -> TestTree
responseHeadBucket =
  res
    "HeadBucketResponse"
    "fixture/HeadBucketResponse.proto"
    defaultService
    (Proxy :: Proxy HeadBucket)

responseGetBucketLogging :: GetBucketLoggingResponse -> TestTree
responseGetBucketLogging =
  res
    "GetBucketLoggingResponse"
    "fixture/GetBucketLoggingResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketLogging)

responseGetBucketRequestPayment :: GetBucketRequestPaymentResponse -> TestTree
responseGetBucketRequestPayment =
  res
    "GetBucketRequestPaymentResponse"
    "fixture/GetBucketRequestPaymentResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketRequestPayment)

responseDeleteBucketReplication :: DeleteBucketReplicationResponse -> TestTree
responseDeleteBucketReplication =
  res
    "DeleteBucketReplicationResponse"
    "fixture/DeleteBucketReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketReplication)

responseListObjectsV :: ListObjectsVResponse -> TestTree
responseListObjectsV =
  res
    "ListObjectsVResponse"
    "fixture/ListObjectsVResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjectsV)

responseDeleteBucketEncryption :: DeleteBucketEncryptionResponse -> TestTree
responseDeleteBucketEncryption =
  res
    "DeleteBucketEncryptionResponse"
    "fixture/DeleteBucketEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketEncryption)

responseDeleteBucketOwnershipControls :: DeleteBucketOwnershipControlsResponse -> TestTree
responseDeleteBucketOwnershipControls =
  res
    "DeleteBucketOwnershipControlsResponse"
    "fixture/DeleteBucketOwnershipControlsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketOwnershipControls)

responsePutBucketMetricsConfiguration :: PutBucketMetricsConfigurationResponse -> TestTree
responsePutBucketMetricsConfiguration =
  res
    "PutBucketMetricsConfigurationResponse"
    "fixture/PutBucketMetricsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketMetricsConfiguration)

responseCopyObject :: CopyObjectResponse -> TestTree
responseCopyObject =
  res
    "CopyObjectResponse"
    "fixture/CopyObjectResponse.proto"
    defaultService
    (Proxy :: Proxy CopyObject)

responsePutPublicAccessBlock :: PutPublicAccessBlockResponse -> TestTree
responsePutPublicAccessBlock =
  res
    "PutPublicAccessBlockResponse"
    "fixture/PutPublicAccessBlockResponse.proto"
    defaultService
    (Proxy :: Proxy PutPublicAccessBlock)

responseGetBucketNotificationConfiguration :: NotificationConfiguration -> TestTree
responseGetBucketNotificationConfiguration =
  res
    "GetBucketNotificationConfigurationResponse"
    "fixture/GetBucketNotificationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketNotificationConfiguration)

responseGetBucketCors :: GetBucketCorsResponse -> TestTree
responseGetBucketCors =
  res
    "GetBucketCorsResponse"
    "fixture/GetBucketCorsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketCors)

responsePutBucketVersioning :: PutBucketVersioningResponse -> TestTree
responsePutBucketVersioning =
  res
    "PutBucketVersioningResponse"
    "fixture/PutBucketVersioningResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketVersioning)

responseRestoreObject :: RestoreObjectResponse -> TestTree
responseRestoreObject =
  res
    "RestoreObjectResponse"
    "fixture/RestoreObjectResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreObject)

responseGetBucketIntelligentTieringConfiguration :: GetBucketIntelligentTieringConfigurationResponse -> TestTree
responseGetBucketIntelligentTieringConfiguration =
  res
    "GetBucketIntelligentTieringConfigurationResponse"
    "fixture/GetBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketIntelligentTieringConfiguration)

responseDeleteBucketInventoryConfiguration :: DeleteBucketInventoryConfigurationResponse -> TestTree
responseDeleteBucketInventoryConfiguration =
  res
    "DeleteBucketInventoryConfigurationResponse"
    "fixture/DeleteBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketInventoryConfiguration)

responseDeleteBucketTagging :: DeleteBucketTaggingResponse -> TestTree
responseDeleteBucketTagging =
  res
    "DeleteBucketTaggingResponse"
    "fixture/DeleteBucketTaggingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketTagging)

responseGetPublicAccessBlock :: GetPublicAccessBlockResponse -> TestTree
responseGetPublicAccessBlock =
  res
    "GetPublicAccessBlockResponse"
    "fixture/GetPublicAccessBlockResponse.proto"
    defaultService
    (Proxy :: Proxy GetPublicAccessBlock)

responsePutBucketIntelligentTieringConfiguration :: PutBucketIntelligentTieringConfigurationResponse -> TestTree
responsePutBucketIntelligentTieringConfiguration =
  res
    "PutBucketIntelligentTieringConfigurationResponse"
    "fixture/PutBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketIntelligentTieringConfiguration)

responseGetBucketInventoryConfiguration :: GetBucketInventoryConfigurationResponse -> TestTree
responseGetBucketInventoryConfiguration =
  res
    "GetBucketInventoryConfigurationResponse"
    "fixture/GetBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketInventoryConfiguration)

responseDeleteBucketCors :: DeleteBucketCorsResponse -> TestTree
responseDeleteBucketCors =
  res
    "DeleteBucketCorsResponse"
    "fixture/DeleteBucketCorsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketCors)

responseListBucketIntelligentTieringConfigurations :: ListBucketIntelligentTieringConfigurationsResponse -> TestTree
responseListBucketIntelligentTieringConfigurations =
  res
    "ListBucketIntelligentTieringConfigurationsResponse"
    "fixture/ListBucketIntelligentTieringConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBucketIntelligentTieringConfigurations)

responsePutBucketNotificationConfiguration :: PutBucketNotificationConfigurationResponse -> TestTree
responsePutBucketNotificationConfiguration =
  res
    "PutBucketNotificationConfigurationResponse"
    "fixture/PutBucketNotificationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketNotificationConfiguration)
