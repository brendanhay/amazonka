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
--         , requestGetBucketEncryption $
--             newGetBucketEncryption
--
--         , requestDeleteBucket $
--             newDeleteBucket
--
--         , requestDeleteObjects $
--             newDeleteObjects
--
--         , requestPutBucketLogging $
--             newPutBucketLogging
--
--         , requestDeleteBucketWebsite $
--             newDeleteBucketWebsite
--
--         , requestCompleteMultipartUpload $
--             newCompleteMultipartUpload
--
--         , requestSelectObjectContent $
--             newSelectObjectContent
--
--         , requestGetBucketPolicyStatus $
--             newGetBucketPolicyStatus
--
--         , requestListObjects $
--             newListObjects
--
--         , requestDeleteObject $
--             newDeleteObject
--
--         , requestDeleteBucketAnalyticsConfiguration $
--             newDeleteBucketAnalyticsConfiguration
--
--         , requestDeleteObjectTagging $
--             newDeleteObjectTagging
--
--         , requestGetBucketAcl $
--             newGetBucketAcl
--
--         , requestGetObjectTagging $
--             newGetObjectTagging
--
--         , requestPutBucketReplication $
--             newPutBucketReplication
--
--         , requestGetBucketWebsite $
--             newGetBucketWebsite
--
--         , requestGetObjectLockConfiguration $
--             newGetObjectLockConfiguration
--
--         , requestDeleteBucketMetricsConfiguration $
--             newDeleteBucketMetricsConfiguration
--
--         , requestGetBucketPolicy $
--             newGetBucketPolicy
--
--         , requestPutBucketEncryption $
--             newPutBucketEncryption
--
--         , requestListBucketMetricsConfigurations $
--             newListBucketMetricsConfigurations
--
--         , requestPutBucketAccelerateConfiguration $
--             newPutBucketAccelerateConfiguration
--
--         , requestPutBucketOwnershipControls $
--             newPutBucketOwnershipControls
--
--         , requestPutObjectRetention $
--             newPutObjectRetention
--
--         , requestPutObjectLegalHold $
--             newPutObjectLegalHold
--
--         , requestHeadObject $
--             newHeadObject
--
--         , requestGetBucketTagging $
--             newGetBucketTagging
--
--         , requestGetBucketLocation $
--             newGetBucketLocation
--
--         , requestPutBucketInventoryConfiguration $
--             newPutBucketInventoryConfiguration
--
--         , requestListBucketInventoryConfigurations $
--             newListBucketInventoryConfigurations
--
--         , requestGetObjectAcl $
--             newGetObjectAcl
--
--         , requestDeletePublicAccessBlock $
--             newDeletePublicAccessBlock
--
--         , requestDeleteBucketIntelligentTieringConfiguration $
--             newDeleteBucketIntelligentTieringConfiguration
--
--         , requestGetBucketVersioning $
--             newGetBucketVersioning
--
--         , requestPutBucketTagging $
--             newPutBucketTagging
--
--         , requestCreateBucket $
--             newCreateBucket
--
--         , requestPutObjectAcl $
--             newPutObjectAcl
--
--         , requestPutBucketCors $
--             newPutBucketCors
--
--         , requestGetObjectRetention $
--             newGetObjectRetention
--
--         , requestGetObjectTorrent $
--             newGetObjectTorrent
--
--         , requestGetBucketOwnershipControls $
--             newGetBucketOwnershipControls
--
--         , requestGetBucketMetricsConfiguration $
--             newGetBucketMetricsConfiguration
--
--         , requestGetBucketAccelerateConfiguration $
--             newGetBucketAccelerateConfiguration
--
--         , requestGetObjectLegalHold $
--             newGetObjectLegalHold
--
--         , requestListBuckets $
--             newListBuckets
--
--         , requestDeleteBucketPolicy $
--             newDeleteBucketPolicy
--
--         , requestPutObjectLockConfiguration $
--             newPutObjectLockConfiguration
--
--         , requestAbortMultipartUpload $
--             newAbortMultipartUpload
--
--         , requestUploadPart $
--             newUploadPart
--
--         , requestPutObject $
--             newPutObject
--
--         , requestPutBucketRequestPayment $
--             newPutBucketRequestPayment
--
--         , requestListMultipartUploads $
--             newListMultipartUploads
--
--         , requestGetBucketReplication $
--             newGetBucketReplication
--
--         , requestPutBucketWebsite $
--             newPutBucketWebsite
--
--         , requestPutBucketAnalyticsConfiguration $
--             newPutBucketAnalyticsConfiguration
--
--         , requestPutObjectTagging $
--             newPutObjectTagging
--
--         , requestUploadPartCopy $
--             newUploadPartCopy
--
--         , requestCreateMultipartUpload $
--             newCreateMultipartUpload
--
--         , requestPutBucketLifecycleConfiguration $
--             newPutBucketLifecycleConfiguration
--
--         , requestPutBucketAcl $
--             newPutBucketAcl
--
--         , requestDeleteBucketLifecycle $
--             newDeleteBucketLifecycle
--
--         , requestListBucketAnalyticsConfigurations $
--             newListBucketAnalyticsConfigurations
--
--         , requestGetBucketAnalyticsConfiguration $
--             newGetBucketAnalyticsConfiguration
--
--         , requestHeadBucket $
--             newHeadBucket
--
--         , requestListObjectVersions $
--             newListObjectVersions
--
--         , requestGetBucketLifecycleConfiguration $
--             newGetBucketLifecycleConfiguration
--
--         , requestListParts $
--             newListParts
--
--         , requestGetBucketRequestPayment $
--             newGetBucketRequestPayment
--
--         , requestDeleteBucketReplication $
--             newDeleteBucketReplication
--
--         , requestGetBucketLogging $
--             newGetBucketLogging
--
--         , requestGetObject $
--             newGetObject
--
--         , requestDeleteBucketOwnershipControls $
--             newDeleteBucketOwnershipControls
--
--         , requestPutBucketMetricsConfiguration $
--             newPutBucketMetricsConfiguration
--
--         , requestListObjectsV $
--             newListObjectsV
--
--         , requestCopyObject $
--             newCopyObject
--
--         , requestDeleteBucketEncryption $
--             newDeleteBucketEncryption
--
--         , requestPutBucketVersioning $
--             newPutBucketVersioning
--
--         , requestGetBucketNotificationConfiguration $
--             newGetBucketNotificationConfiguration
--
--         , requestPutPublicAccessBlock $
--             newPutPublicAccessBlock
--
--         , requestDeleteBucketInventoryConfiguration $
--             newDeleteBucketInventoryConfiguration
--
--         , requestGetBucketIntelligentTieringConfiguration $
--             newGetBucketIntelligentTieringConfiguration
--
--         , requestRestoreObject $
--             newRestoreObject
--
--         , requestGetBucketCors $
--             newGetBucketCors
--
--         , requestGetBucketInventoryConfiguration $
--             newGetBucketInventoryConfiguration
--
--         , requestGetPublicAccessBlock $
--             newGetPublicAccessBlock
--
--         , requestDeleteBucketCors $
--             newDeleteBucketCors
--
--         , requestDeleteBucketTagging $
--             newDeleteBucketTagging
--
--         , requestListBucketIntelligentTieringConfigurations $
--             newListBucketIntelligentTieringConfigurations
--
--         , requestPutBucketNotificationConfiguration $
--             newPutBucketNotificationConfiguration
--
--         , requestPutBucketIntelligentTieringConfiguration $
--             newPutBucketIntelligentTieringConfiguration
--
--           ]

--     , testGroup "response"
--         [ responsePutBucketPolicy $
--             newPutBucketPolicyResponse
--
--         , responseGetBucketEncryption $
--             newGetBucketEncryptionResponse
--
--         , responseDeleteBucket $
--             newDeleteBucketResponse
--
--         , responseDeleteObjects $
--             newDeleteObjectsResponse
--
--         , responsePutBucketLogging $
--             newPutBucketLoggingResponse
--
--         , responseDeleteBucketWebsite $
--             newDeleteBucketWebsiteResponse
--
--         , responseCompleteMultipartUpload $
--             newCompleteMultipartUploadResponse
--
--         , responseSelectObjectContent $
--             newSelectObjectContentResponse
--
--         , responseGetBucketPolicyStatus $
--             newGetBucketPolicyStatusResponse
--
--         , responseListObjects $
--             newListObjectsResponse
--
--         , responseDeleteObject $
--             newDeleteObjectResponse
--
--         , responseDeleteBucketAnalyticsConfiguration $
--             newDeleteBucketAnalyticsConfigurationResponse
--
--         , responseDeleteObjectTagging $
--             newDeleteObjectTaggingResponse
--
--         , responseGetBucketAcl $
--             newGetBucketAclResponse
--
--         , responseGetObjectTagging $
--             newGetObjectTaggingResponse
--
--         , responsePutBucketReplication $
--             newPutBucketReplicationResponse
--
--         , responseGetBucketWebsite $
--             newGetBucketWebsiteResponse
--
--         , responseGetObjectLockConfiguration $
--             newGetObjectLockConfigurationResponse
--
--         , responseDeleteBucketMetricsConfiguration $
--             newDeleteBucketMetricsConfigurationResponse
--
--         , responseGetBucketPolicy $
--             newGetBucketPolicyResponse
--
--         , responsePutBucketEncryption $
--             newPutBucketEncryptionResponse
--
--         , responseListBucketMetricsConfigurations $
--             newListBucketMetricsConfigurationsResponse
--
--         , responsePutBucketAccelerateConfiguration $
--             newPutBucketAccelerateConfigurationResponse
--
--         , responsePutBucketOwnershipControls $
--             newPutBucketOwnershipControlsResponse
--
--         , responsePutObjectRetention $
--             newPutObjectRetentionResponse
--
--         , responsePutObjectLegalHold $
--             newPutObjectLegalHoldResponse
--
--         , responseHeadObject $
--             newHeadObjectResponse
--
--         , responseGetBucketTagging $
--             newGetBucketTaggingResponse
--
--         , responseGetBucketLocation $
--             newGetBucketLocationResponse
--
--         , responsePutBucketInventoryConfiguration $
--             newPutBucketInventoryConfigurationResponse
--
--         , responseListBucketInventoryConfigurations $
--             newListBucketInventoryConfigurationsResponse
--
--         , responseGetObjectAcl $
--             newGetObjectAclResponse
--
--         , responseDeletePublicAccessBlock $
--             newDeletePublicAccessBlockResponse
--
--         , responseDeleteBucketIntelligentTieringConfiguration $
--             newDeleteBucketIntelligentTieringConfigurationResponse
--
--         , responseGetBucketVersioning $
--             newGetBucketVersioningResponse
--
--         , responsePutBucketTagging $
--             newPutBucketTaggingResponse
--
--         , responseCreateBucket $
--             newCreateBucketResponse
--
--         , responsePutObjectAcl $
--             newPutObjectAclResponse
--
--         , responsePutBucketCors $
--             newPutBucketCorsResponse
--
--         , responseGetObjectRetention $
--             newGetObjectRetentionResponse
--
--         , responseGetObjectTorrent $
--             newGetObjectTorrentResponse
--
--         , responseGetBucketOwnershipControls $
--             newGetBucketOwnershipControlsResponse
--
--         , responseGetBucketMetricsConfiguration $
--             newGetBucketMetricsConfigurationResponse
--
--         , responseGetBucketAccelerateConfiguration $
--             newGetBucketAccelerateConfigurationResponse
--
--         , responseGetObjectLegalHold $
--             newGetObjectLegalHoldResponse
--
--         , responseListBuckets $
--             newListBucketsResponse
--
--         , responseDeleteBucketPolicy $
--             newDeleteBucketPolicyResponse
--
--         , responsePutObjectLockConfiguration $
--             newPutObjectLockConfigurationResponse
--
--         , responseAbortMultipartUpload $
--             newAbortMultipartUploadResponse
--
--         , responseUploadPart $
--             newUploadPartResponse
--
--         , responsePutObject $
--             newPutObjectResponse
--
--         , responsePutBucketRequestPayment $
--             newPutBucketRequestPaymentResponse
--
--         , responseListMultipartUploads $
--             newListMultipartUploadsResponse
--
--         , responseGetBucketReplication $
--             newGetBucketReplicationResponse
--
--         , responsePutBucketWebsite $
--             newPutBucketWebsiteResponse
--
--         , responsePutBucketAnalyticsConfiguration $
--             newPutBucketAnalyticsConfigurationResponse
--
--         , responsePutObjectTagging $
--             newPutObjectTaggingResponse
--
--         , responseUploadPartCopy $
--             newUploadPartCopyResponse
--
--         , responseCreateMultipartUpload $
--             newCreateMultipartUploadResponse
--
--         , responsePutBucketLifecycleConfiguration $
--             newPutBucketLifecycleConfigurationResponse
--
--         , responsePutBucketAcl $
--             newPutBucketAclResponse
--
--         , responseDeleteBucketLifecycle $
--             newDeleteBucketLifecycleResponse
--
--         , responseListBucketAnalyticsConfigurations $
--             newListBucketAnalyticsConfigurationsResponse
--
--         , responseGetBucketAnalyticsConfiguration $
--             newGetBucketAnalyticsConfigurationResponse
--
--         , responseHeadBucket $
--             newHeadBucketResponse
--
--         , responseListObjectVersions $
--             newListObjectVersionsResponse
--
--         , responseGetBucketLifecycleConfiguration $
--             newGetBucketLifecycleConfigurationResponse
--
--         , responseListParts $
--             newListPartsResponse
--
--         , responseGetBucketRequestPayment $
--             newGetBucketRequestPaymentResponse
--
--         , responseDeleteBucketReplication $
--             newDeleteBucketReplicationResponse
--
--         , responseGetBucketLogging $
--             newGetBucketLoggingResponse
--
--         , responseGetObject $
--             newGetObjectResponse
--
--         , responseDeleteBucketOwnershipControls $
--             newDeleteBucketOwnershipControlsResponse
--
--         , responsePutBucketMetricsConfiguration $
--             newPutBucketMetricsConfigurationResponse
--
--         , responseListObjectsV $
--             newListObjectsVResponse
--
--         , responseCopyObject $
--             newCopyObjectResponse
--
--         , responseDeleteBucketEncryption $
--             newDeleteBucketEncryptionResponse
--
--         , responsePutBucketVersioning $
--             newPutBucketVersioningResponse
--
--         , responseGetBucketNotificationConfiguration $
--             newNotificationConfiguration
--
--         , responsePutPublicAccessBlock $
--             newPutPublicAccessBlockResponse
--
--         , responseDeleteBucketInventoryConfiguration $
--             newDeleteBucketInventoryConfigurationResponse
--
--         , responseGetBucketIntelligentTieringConfiguration $
--             newGetBucketIntelligentTieringConfigurationResponse
--
--         , responseRestoreObject $
--             newRestoreObjectResponse
--
--         , responseGetBucketCors $
--             newGetBucketCorsResponse
--
--         , responseGetBucketInventoryConfiguration $
--             newGetBucketInventoryConfigurationResponse
--
--         , responseGetPublicAccessBlock $
--             newGetPublicAccessBlockResponse
--
--         , responseDeleteBucketCors $
--             newDeleteBucketCorsResponse
--
--         , responseDeleteBucketTagging $
--             newDeleteBucketTaggingResponse
--
--         , responseListBucketIntelligentTieringConfigurations $
--             newListBucketIntelligentTieringConfigurationsResponse
--
--         , responsePutBucketNotificationConfiguration $
--             newPutBucketNotificationConfigurationResponse
--
--         , responsePutBucketIntelligentTieringConfiguration $
--             newPutBucketIntelligentTieringConfigurationResponse
--
--           ]
--     ]

-- Requests

requestPutBucketPolicy :: PutBucketPolicy -> TestTree
requestPutBucketPolicy =
  req
    "PutBucketPolicy"
    "fixture/PutBucketPolicy.yaml"

requestGetBucketEncryption :: GetBucketEncryption -> TestTree
requestGetBucketEncryption =
  req
    "GetBucketEncryption"
    "fixture/GetBucketEncryption.yaml"

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

requestPutBucketLogging :: PutBucketLogging -> TestTree
requestPutBucketLogging =
  req
    "PutBucketLogging"
    "fixture/PutBucketLogging.yaml"

requestDeleteBucketWebsite :: DeleteBucketWebsite -> TestTree
requestDeleteBucketWebsite =
  req
    "DeleteBucketWebsite"
    "fixture/DeleteBucketWebsite.yaml"

requestCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
requestCompleteMultipartUpload =
  req
    "CompleteMultipartUpload"
    "fixture/CompleteMultipartUpload.yaml"

requestSelectObjectContent :: SelectObjectContent -> TestTree
requestSelectObjectContent =
  req
    "SelectObjectContent"
    "fixture/SelectObjectContent.yaml"

requestGetBucketPolicyStatus :: GetBucketPolicyStatus -> TestTree
requestGetBucketPolicyStatus =
  req
    "GetBucketPolicyStatus"
    "fixture/GetBucketPolicyStatus.yaml"

requestListObjects :: ListObjects -> TestTree
requestListObjects =
  req
    "ListObjects"
    "fixture/ListObjects.yaml"

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject =
  req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestDeleteBucketAnalyticsConfiguration :: DeleteBucketAnalyticsConfiguration -> TestTree
requestDeleteBucketAnalyticsConfiguration =
  req
    "DeleteBucketAnalyticsConfiguration"
    "fixture/DeleteBucketAnalyticsConfiguration.yaml"

requestDeleteObjectTagging :: DeleteObjectTagging -> TestTree
requestDeleteObjectTagging =
  req
    "DeleteObjectTagging"
    "fixture/DeleteObjectTagging.yaml"

requestGetBucketAcl :: GetBucketAcl -> TestTree
requestGetBucketAcl =
  req
    "GetBucketAcl"
    "fixture/GetBucketAcl.yaml"

requestGetObjectTagging :: GetObjectTagging -> TestTree
requestGetObjectTagging =
  req
    "GetObjectTagging"
    "fixture/GetObjectTagging.yaml"

requestPutBucketReplication :: PutBucketReplication -> TestTree
requestPutBucketReplication =
  req
    "PutBucketReplication"
    "fixture/PutBucketReplication.yaml"

requestGetBucketWebsite :: GetBucketWebsite -> TestTree
requestGetBucketWebsite =
  req
    "GetBucketWebsite"
    "fixture/GetBucketWebsite.yaml"

requestGetObjectLockConfiguration :: GetObjectLockConfiguration -> TestTree
requestGetObjectLockConfiguration =
  req
    "GetObjectLockConfiguration"
    "fixture/GetObjectLockConfiguration.yaml"

requestDeleteBucketMetricsConfiguration :: DeleteBucketMetricsConfiguration -> TestTree
requestDeleteBucketMetricsConfiguration =
  req
    "DeleteBucketMetricsConfiguration"
    "fixture/DeleteBucketMetricsConfiguration.yaml"

requestGetBucketPolicy :: GetBucketPolicy -> TestTree
requestGetBucketPolicy =
  req
    "GetBucketPolicy"
    "fixture/GetBucketPolicy.yaml"

requestPutBucketEncryption :: PutBucketEncryption -> TestTree
requestPutBucketEncryption =
  req
    "PutBucketEncryption"
    "fixture/PutBucketEncryption.yaml"

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

requestPutBucketOwnershipControls :: PutBucketOwnershipControls -> TestTree
requestPutBucketOwnershipControls =
  req
    "PutBucketOwnershipControls"
    "fixture/PutBucketOwnershipControls.yaml"

requestPutObjectRetention :: PutObjectRetention -> TestTree
requestPutObjectRetention =
  req
    "PutObjectRetention"
    "fixture/PutObjectRetention.yaml"

requestPutObjectLegalHold :: PutObjectLegalHold -> TestTree
requestPutObjectLegalHold =
  req
    "PutObjectLegalHold"
    "fixture/PutObjectLegalHold.yaml"

requestHeadObject :: HeadObject -> TestTree
requestHeadObject =
  req
    "HeadObject"
    "fixture/HeadObject.yaml"

requestGetBucketTagging :: GetBucketTagging -> TestTree
requestGetBucketTagging =
  req
    "GetBucketTagging"
    "fixture/GetBucketTagging.yaml"

requestGetBucketLocation :: GetBucketLocation -> TestTree
requestGetBucketLocation =
  req
    "GetBucketLocation"
    "fixture/GetBucketLocation.yaml"

requestPutBucketInventoryConfiguration :: PutBucketInventoryConfiguration -> TestTree
requestPutBucketInventoryConfiguration =
  req
    "PutBucketInventoryConfiguration"
    "fixture/PutBucketInventoryConfiguration.yaml"

requestListBucketInventoryConfigurations :: ListBucketInventoryConfigurations -> TestTree
requestListBucketInventoryConfigurations =
  req
    "ListBucketInventoryConfigurations"
    "fixture/ListBucketInventoryConfigurations.yaml"

requestGetObjectAcl :: GetObjectAcl -> TestTree
requestGetObjectAcl =
  req
    "GetObjectAcl"
    "fixture/GetObjectAcl.yaml"

requestDeletePublicAccessBlock :: DeletePublicAccessBlock -> TestTree
requestDeletePublicAccessBlock =
  req
    "DeletePublicAccessBlock"
    "fixture/DeletePublicAccessBlock.yaml"

requestDeleteBucketIntelligentTieringConfiguration :: DeleteBucketIntelligentTieringConfiguration -> TestTree
requestDeleteBucketIntelligentTieringConfiguration =
  req
    "DeleteBucketIntelligentTieringConfiguration"
    "fixture/DeleteBucketIntelligentTieringConfiguration.yaml"

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

requestCreateBucket :: CreateBucket -> TestTree
requestCreateBucket =
  req
    "CreateBucket"
    "fixture/CreateBucket.yaml"

requestPutObjectAcl :: PutObjectAcl -> TestTree
requestPutObjectAcl =
  req
    "PutObjectAcl"
    "fixture/PutObjectAcl.yaml"

requestPutBucketCors :: PutBucketCors -> TestTree
requestPutBucketCors =
  req
    "PutBucketCors"
    "fixture/PutBucketCors.yaml"

requestGetObjectRetention :: GetObjectRetention -> TestTree
requestGetObjectRetention =
  req
    "GetObjectRetention"
    "fixture/GetObjectRetention.yaml"

requestGetObjectTorrent :: GetObjectTorrent -> TestTree
requestGetObjectTorrent =
  req
    "GetObjectTorrent"
    "fixture/GetObjectTorrent.yaml"

requestGetBucketOwnershipControls :: GetBucketOwnershipControls -> TestTree
requestGetBucketOwnershipControls =
  req
    "GetBucketOwnershipControls"
    "fixture/GetBucketOwnershipControls.yaml"

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

requestGetObjectLegalHold :: GetObjectLegalHold -> TestTree
requestGetObjectLegalHold =
  req
    "GetObjectLegalHold"
    "fixture/GetObjectLegalHold.yaml"

requestListBuckets :: ListBuckets -> TestTree
requestListBuckets =
  req
    "ListBuckets"
    "fixture/ListBuckets.yaml"

requestDeleteBucketPolicy :: DeleteBucketPolicy -> TestTree
requestDeleteBucketPolicy =
  req
    "DeleteBucketPolicy"
    "fixture/DeleteBucketPolicy.yaml"

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

requestPutBucketRequestPayment :: PutBucketRequestPayment -> TestTree
requestPutBucketRequestPayment =
  req
    "PutBucketRequestPayment"
    "fixture/PutBucketRequestPayment.yaml"

requestListMultipartUploads :: ListMultipartUploads -> TestTree
requestListMultipartUploads =
  req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads.yaml"

requestGetBucketReplication :: GetBucketReplication -> TestTree
requestGetBucketReplication =
  req
    "GetBucketReplication"
    "fixture/GetBucketReplication.yaml"

requestPutBucketWebsite :: PutBucketWebsite -> TestTree
requestPutBucketWebsite =
  req
    "PutBucketWebsite"
    "fixture/PutBucketWebsite.yaml"

requestPutBucketAnalyticsConfiguration :: PutBucketAnalyticsConfiguration -> TestTree
requestPutBucketAnalyticsConfiguration =
  req
    "PutBucketAnalyticsConfiguration"
    "fixture/PutBucketAnalyticsConfiguration.yaml"

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

requestCreateMultipartUpload :: CreateMultipartUpload -> TestTree
requestCreateMultipartUpload =
  req
    "CreateMultipartUpload"
    "fixture/CreateMultipartUpload.yaml"

requestPutBucketLifecycleConfiguration :: PutBucketLifecycleConfiguration -> TestTree
requestPutBucketLifecycleConfiguration =
  req
    "PutBucketLifecycleConfiguration"
    "fixture/PutBucketLifecycleConfiguration.yaml"

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

requestListBucketAnalyticsConfigurations :: ListBucketAnalyticsConfigurations -> TestTree
requestListBucketAnalyticsConfigurations =
  req
    "ListBucketAnalyticsConfigurations"
    "fixture/ListBucketAnalyticsConfigurations.yaml"

requestGetBucketAnalyticsConfiguration :: GetBucketAnalyticsConfiguration -> TestTree
requestGetBucketAnalyticsConfiguration =
  req
    "GetBucketAnalyticsConfiguration"
    "fixture/GetBucketAnalyticsConfiguration.yaml"

requestHeadBucket :: HeadBucket -> TestTree
requestHeadBucket =
  req
    "HeadBucket"
    "fixture/HeadBucket.yaml"

requestListObjectVersions :: ListObjectVersions -> TestTree
requestListObjectVersions =
  req
    "ListObjectVersions"
    "fixture/ListObjectVersions.yaml"

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

requestGetBucketLogging :: GetBucketLogging -> TestTree
requestGetBucketLogging =
  req
    "GetBucketLogging"
    "fixture/GetBucketLogging.yaml"

requestGetObject :: GetObject -> TestTree
requestGetObject =
  req
    "GetObject"
    "fixture/GetObject.yaml"

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

requestListObjectsV :: ListObjectsV -> TestTree
requestListObjectsV =
  req
    "ListObjectsV"
    "fixture/ListObjectsV.yaml"

requestCopyObject :: CopyObject -> TestTree
requestCopyObject =
  req
    "CopyObject"
    "fixture/CopyObject.yaml"

requestDeleteBucketEncryption :: DeleteBucketEncryption -> TestTree
requestDeleteBucketEncryption =
  req
    "DeleteBucketEncryption"
    "fixture/DeleteBucketEncryption.yaml"

requestPutBucketVersioning :: PutBucketVersioning -> TestTree
requestPutBucketVersioning =
  req
    "PutBucketVersioning"
    "fixture/PutBucketVersioning.yaml"

requestGetBucketNotificationConfiguration :: GetBucketNotificationConfiguration -> TestTree
requestGetBucketNotificationConfiguration =
  req
    "GetBucketNotificationConfiguration"
    "fixture/GetBucketNotificationConfiguration.yaml"

requestPutPublicAccessBlock :: PutPublicAccessBlock -> TestTree
requestPutPublicAccessBlock =
  req
    "PutPublicAccessBlock"
    "fixture/PutPublicAccessBlock.yaml"

requestDeleteBucketInventoryConfiguration :: DeleteBucketInventoryConfiguration -> TestTree
requestDeleteBucketInventoryConfiguration =
  req
    "DeleteBucketInventoryConfiguration"
    "fixture/DeleteBucketInventoryConfiguration.yaml"

requestGetBucketIntelligentTieringConfiguration :: GetBucketIntelligentTieringConfiguration -> TestTree
requestGetBucketIntelligentTieringConfiguration =
  req
    "GetBucketIntelligentTieringConfiguration"
    "fixture/GetBucketIntelligentTieringConfiguration.yaml"

requestRestoreObject :: RestoreObject -> TestTree
requestRestoreObject =
  req
    "RestoreObject"
    "fixture/RestoreObject.yaml"

requestGetBucketCors :: GetBucketCors -> TestTree
requestGetBucketCors =
  req
    "GetBucketCors"
    "fixture/GetBucketCors.yaml"

requestGetBucketInventoryConfiguration :: GetBucketInventoryConfiguration -> TestTree
requestGetBucketInventoryConfiguration =
  req
    "GetBucketInventoryConfiguration"
    "fixture/GetBucketInventoryConfiguration.yaml"

requestGetPublicAccessBlock :: GetPublicAccessBlock -> TestTree
requestGetPublicAccessBlock =
  req
    "GetPublicAccessBlock"
    "fixture/GetPublicAccessBlock.yaml"

requestDeleteBucketCors :: DeleteBucketCors -> TestTree
requestDeleteBucketCors =
  req
    "DeleteBucketCors"
    "fixture/DeleteBucketCors.yaml"

requestDeleteBucketTagging :: DeleteBucketTagging -> TestTree
requestDeleteBucketTagging =
  req
    "DeleteBucketTagging"
    "fixture/DeleteBucketTagging.yaml"

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

requestPutBucketIntelligentTieringConfiguration :: PutBucketIntelligentTieringConfiguration -> TestTree
requestPutBucketIntelligentTieringConfiguration =
  req
    "PutBucketIntelligentTieringConfiguration"
    "fixture/PutBucketIntelligentTieringConfiguration.yaml"

-- Responses

responsePutBucketPolicy :: PutBucketPolicyResponse -> TestTree
responsePutBucketPolicy =
  res
    "PutBucketPolicyResponse"
    "fixture/PutBucketPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketPolicy)

responseGetBucketEncryption :: GetBucketEncryptionResponse -> TestTree
responseGetBucketEncryption =
  res
    "GetBucketEncryptionResponse"
    "fixture/GetBucketEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketEncryption)

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

responsePutBucketLogging :: PutBucketLoggingResponse -> TestTree
responsePutBucketLogging =
  res
    "PutBucketLoggingResponse"
    "fixture/PutBucketLoggingResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketLogging)

responseDeleteBucketWebsite :: DeleteBucketWebsiteResponse -> TestTree
responseDeleteBucketWebsite =
  res
    "DeleteBucketWebsiteResponse"
    "fixture/DeleteBucketWebsiteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketWebsite)

responseCompleteMultipartUpload :: CompleteMultipartUploadResponse -> TestTree
responseCompleteMultipartUpload =
  res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    defaultService
    (Proxy :: Proxy CompleteMultipartUpload)

responseSelectObjectContent :: SelectObjectContentResponse -> TestTree
responseSelectObjectContent =
  res
    "SelectObjectContentResponse"
    "fixture/SelectObjectContentResponse.proto"
    defaultService
    (Proxy :: Proxy SelectObjectContent)

responseGetBucketPolicyStatus :: GetBucketPolicyStatusResponse -> TestTree
responseGetBucketPolicyStatus =
  res
    "GetBucketPolicyStatusResponse"
    "fixture/GetBucketPolicyStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketPolicyStatus)

responseListObjects :: ListObjectsResponse -> TestTree
responseListObjects =
  res
    "ListObjectsResponse"
    "fixture/ListObjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjects)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject =
  res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteObject)

responseDeleteBucketAnalyticsConfiguration :: DeleteBucketAnalyticsConfigurationResponse -> TestTree
responseDeleteBucketAnalyticsConfiguration =
  res
    "DeleteBucketAnalyticsConfigurationResponse"
    "fixture/DeleteBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketAnalyticsConfiguration)

responseDeleteObjectTagging :: DeleteObjectTaggingResponse -> TestTree
responseDeleteObjectTagging =
  res
    "DeleteObjectTaggingResponse"
    "fixture/DeleteObjectTaggingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteObjectTagging)

responseGetBucketAcl :: GetBucketAclResponse -> TestTree
responseGetBucketAcl =
  res
    "GetBucketAclResponse"
    "fixture/GetBucketAclResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketAcl)

responseGetObjectTagging :: GetObjectTaggingResponse -> TestTree
responseGetObjectTagging =
  res
    "GetObjectTaggingResponse"
    "fixture/GetObjectTaggingResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectTagging)

responsePutBucketReplication :: PutBucketReplicationResponse -> TestTree
responsePutBucketReplication =
  res
    "PutBucketReplicationResponse"
    "fixture/PutBucketReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketReplication)

responseGetBucketWebsite :: GetBucketWebsiteResponse -> TestTree
responseGetBucketWebsite =
  res
    "GetBucketWebsiteResponse"
    "fixture/GetBucketWebsiteResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketWebsite)

responseGetObjectLockConfiguration :: GetObjectLockConfigurationResponse -> TestTree
responseGetObjectLockConfiguration =
  res
    "GetObjectLockConfigurationResponse"
    "fixture/GetObjectLockConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectLockConfiguration)

responseDeleteBucketMetricsConfiguration :: DeleteBucketMetricsConfigurationResponse -> TestTree
responseDeleteBucketMetricsConfiguration =
  res
    "DeleteBucketMetricsConfigurationResponse"
    "fixture/DeleteBucketMetricsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketMetricsConfiguration)

responseGetBucketPolicy :: GetBucketPolicyResponse -> TestTree
responseGetBucketPolicy =
  res
    "GetBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketPolicy)

responsePutBucketEncryption :: PutBucketEncryptionResponse -> TestTree
responsePutBucketEncryption =
  res
    "PutBucketEncryptionResponse"
    "fixture/PutBucketEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketEncryption)

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

responsePutBucketOwnershipControls :: PutBucketOwnershipControlsResponse -> TestTree
responsePutBucketOwnershipControls =
  res
    "PutBucketOwnershipControlsResponse"
    "fixture/PutBucketOwnershipControlsResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketOwnershipControls)

responsePutObjectRetention :: PutObjectRetentionResponse -> TestTree
responsePutObjectRetention =
  res
    "PutObjectRetentionResponse"
    "fixture/PutObjectRetentionResponse.proto"
    defaultService
    (Proxy :: Proxy PutObjectRetention)

responsePutObjectLegalHold :: PutObjectLegalHoldResponse -> TestTree
responsePutObjectLegalHold =
  res
    "PutObjectLegalHoldResponse"
    "fixture/PutObjectLegalHoldResponse.proto"
    defaultService
    (Proxy :: Proxy PutObjectLegalHold)

responseHeadObject :: HeadObjectResponse -> TestTree
responseHeadObject =
  res
    "HeadObjectResponse"
    "fixture/HeadObjectResponse.proto"
    defaultService
    (Proxy :: Proxy HeadObject)

responseGetBucketTagging :: GetBucketTaggingResponse -> TestTree
responseGetBucketTagging =
  res
    "GetBucketTaggingResponse"
    "fixture/GetBucketTaggingResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketTagging)

responseGetBucketLocation :: GetBucketLocationResponse -> TestTree
responseGetBucketLocation =
  res
    "GetBucketLocationResponse"
    "fixture/GetBucketLocationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketLocation)

responsePutBucketInventoryConfiguration :: PutBucketInventoryConfigurationResponse -> TestTree
responsePutBucketInventoryConfiguration =
  res
    "PutBucketInventoryConfigurationResponse"
    "fixture/PutBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketInventoryConfiguration)

responseListBucketInventoryConfigurations :: ListBucketInventoryConfigurationsResponse -> TestTree
responseListBucketInventoryConfigurations =
  res
    "ListBucketInventoryConfigurationsResponse"
    "fixture/ListBucketInventoryConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBucketInventoryConfigurations)

responseGetObjectAcl :: GetObjectAclResponse -> TestTree
responseGetObjectAcl =
  res
    "GetObjectAclResponse"
    "fixture/GetObjectAclResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectAcl)

responseDeletePublicAccessBlock :: DeletePublicAccessBlockResponse -> TestTree
responseDeletePublicAccessBlock =
  res
    "DeletePublicAccessBlockResponse"
    "fixture/DeletePublicAccessBlockResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePublicAccessBlock)

responseDeleteBucketIntelligentTieringConfiguration :: DeleteBucketIntelligentTieringConfigurationResponse -> TestTree
responseDeleteBucketIntelligentTieringConfiguration =
  res
    "DeleteBucketIntelligentTieringConfigurationResponse"
    "fixture/DeleteBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketIntelligentTieringConfiguration)

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

responseCreateBucket :: CreateBucketResponse -> TestTree
responseCreateBucket =
  res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBucket)

responsePutObjectAcl :: PutObjectAclResponse -> TestTree
responsePutObjectAcl =
  res
    "PutObjectAclResponse"
    "fixture/PutObjectAclResponse.proto"
    defaultService
    (Proxy :: Proxy PutObjectAcl)

responsePutBucketCors :: PutBucketCorsResponse -> TestTree
responsePutBucketCors =
  res
    "PutBucketCorsResponse"
    "fixture/PutBucketCorsResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketCors)

responseGetObjectRetention :: GetObjectRetentionResponse -> TestTree
responseGetObjectRetention =
  res
    "GetObjectRetentionResponse"
    "fixture/GetObjectRetentionResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectRetention)

responseGetBucketOwnershipControls :: GetBucketOwnershipControlsResponse -> TestTree
responseGetBucketOwnershipControls =
  res
    "GetBucketOwnershipControlsResponse"
    "fixture/GetBucketOwnershipControlsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketOwnershipControls)

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

responseGetObjectLegalHold :: GetObjectLegalHoldResponse -> TestTree
responseGetObjectLegalHold =
  res
    "GetObjectLegalHoldResponse"
    "fixture/GetObjectLegalHoldResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectLegalHold)

responseListBuckets :: ListBucketsResponse -> TestTree
responseListBuckets =
  res
    "ListBucketsResponse"
    "fixture/ListBucketsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuckets)

responseDeleteBucketPolicy :: DeleteBucketPolicyResponse -> TestTree
responseDeleteBucketPolicy =
  res
    "DeleteBucketPolicyResponse"
    "fixture/DeleteBucketPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketPolicy)

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

responseUploadPart :: UploadPartResponse -> TestTree
responseUploadPart =
  res
    "UploadPartResponse"
    "fixture/UploadPartResponse.proto"
    defaultService
    (Proxy :: Proxy UploadPart)

responsePutObject :: PutObjectResponse -> TestTree
responsePutObject =
  res
    "PutObjectResponse"
    "fixture/PutObjectResponse.proto"
    defaultService
    (Proxy :: Proxy PutObject)

responsePutBucketRequestPayment :: PutBucketRequestPaymentResponse -> TestTree
responsePutBucketRequestPayment =
  res
    "PutBucketRequestPaymentResponse"
    "fixture/PutBucketRequestPaymentResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketRequestPayment)

responseListMultipartUploads :: ListMultipartUploadsResponse -> TestTree
responseListMultipartUploads =
  res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMultipartUploads)

responseGetBucketReplication :: GetBucketReplicationResponse -> TestTree
responseGetBucketReplication =
  res
    "GetBucketReplicationResponse"
    "fixture/GetBucketReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketReplication)

responsePutBucketWebsite :: PutBucketWebsiteResponse -> TestTree
responsePutBucketWebsite =
  res
    "PutBucketWebsiteResponse"
    "fixture/PutBucketWebsiteResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketWebsite)

responsePutBucketAnalyticsConfiguration :: PutBucketAnalyticsConfigurationResponse -> TestTree
responsePutBucketAnalyticsConfiguration =
  res
    "PutBucketAnalyticsConfigurationResponse"
    "fixture/PutBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketAnalyticsConfiguration)

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

responseCreateMultipartUpload :: CreateMultipartUploadResponse -> TestTree
responseCreateMultipartUpload =
  res
    "CreateMultipartUploadResponse"
    "fixture/CreateMultipartUploadResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMultipartUpload)

responsePutBucketLifecycleConfiguration :: PutBucketLifecycleConfigurationResponse -> TestTree
responsePutBucketLifecycleConfiguration =
  res
    "PutBucketLifecycleConfigurationResponse"
    "fixture/PutBucketLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketLifecycleConfiguration)

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

responseListBucketAnalyticsConfigurations :: ListBucketAnalyticsConfigurationsResponse -> TestTree
responseListBucketAnalyticsConfigurations =
  res
    "ListBucketAnalyticsConfigurationsResponse"
    "fixture/ListBucketAnalyticsConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBucketAnalyticsConfigurations)

responseGetBucketAnalyticsConfiguration :: GetBucketAnalyticsConfigurationResponse -> TestTree
responseGetBucketAnalyticsConfiguration =
  res
    "GetBucketAnalyticsConfigurationResponse"
    "fixture/GetBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketAnalyticsConfiguration)

responseHeadBucket :: HeadBucketResponse -> TestTree
responseHeadBucket =
  res
    "HeadBucketResponse"
    "fixture/HeadBucketResponse.proto"
    defaultService
    (Proxy :: Proxy HeadBucket)

responseListObjectVersions :: ListObjectVersionsResponse -> TestTree
responseListObjectVersions =
  res
    "ListObjectVersionsResponse"
    "fixture/ListObjectVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjectVersions)

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

responseGetBucketLogging :: GetBucketLoggingResponse -> TestTree
responseGetBucketLogging =
  res
    "GetBucketLoggingResponse"
    "fixture/GetBucketLoggingResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketLogging)

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

responseListObjectsV :: ListObjectsVResponse -> TestTree
responseListObjectsV =
  res
    "ListObjectsVResponse"
    "fixture/ListObjectsVResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjectsV)

responseCopyObject :: CopyObjectResponse -> TestTree
responseCopyObject =
  res
    "CopyObjectResponse"
    "fixture/CopyObjectResponse.proto"
    defaultService
    (Proxy :: Proxy CopyObject)

responseDeleteBucketEncryption :: DeleteBucketEncryptionResponse -> TestTree
responseDeleteBucketEncryption =
  res
    "DeleteBucketEncryptionResponse"
    "fixture/DeleteBucketEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketEncryption)

responsePutBucketVersioning :: PutBucketVersioningResponse -> TestTree
responsePutBucketVersioning =
  res
    "PutBucketVersioningResponse"
    "fixture/PutBucketVersioningResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketVersioning)

responseGetBucketNotificationConfiguration :: NotificationConfiguration -> TestTree
responseGetBucketNotificationConfiguration =
  res
    "GetBucketNotificationConfigurationResponse"
    "fixture/GetBucketNotificationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketNotificationConfiguration)

responsePutPublicAccessBlock :: PutPublicAccessBlockResponse -> TestTree
responsePutPublicAccessBlock =
  res
    "PutPublicAccessBlockResponse"
    "fixture/PutPublicAccessBlockResponse.proto"
    defaultService
    (Proxy :: Proxy PutPublicAccessBlock)

responseDeleteBucketInventoryConfiguration :: DeleteBucketInventoryConfigurationResponse -> TestTree
responseDeleteBucketInventoryConfiguration =
  res
    "DeleteBucketInventoryConfigurationResponse"
    "fixture/DeleteBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketInventoryConfiguration)

responseGetBucketIntelligentTieringConfiguration :: GetBucketIntelligentTieringConfigurationResponse -> TestTree
responseGetBucketIntelligentTieringConfiguration =
  res
    "GetBucketIntelligentTieringConfigurationResponse"
    "fixture/GetBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketIntelligentTieringConfiguration)

responseRestoreObject :: RestoreObjectResponse -> TestTree
responseRestoreObject =
  res
    "RestoreObjectResponse"
    "fixture/RestoreObjectResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreObject)

responseGetBucketCors :: GetBucketCorsResponse -> TestTree
responseGetBucketCors =
  res
    "GetBucketCorsResponse"
    "fixture/GetBucketCorsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketCors)

responseGetBucketInventoryConfiguration :: GetBucketInventoryConfigurationResponse -> TestTree
responseGetBucketInventoryConfiguration =
  res
    "GetBucketInventoryConfigurationResponse"
    "fixture/GetBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketInventoryConfiguration)

responseGetPublicAccessBlock :: GetPublicAccessBlockResponse -> TestTree
responseGetPublicAccessBlock =
  res
    "GetPublicAccessBlockResponse"
    "fixture/GetPublicAccessBlockResponse.proto"
    defaultService
    (Proxy :: Proxy GetPublicAccessBlock)

responseDeleteBucketCors :: DeleteBucketCorsResponse -> TestTree
responseDeleteBucketCors =
  res
    "DeleteBucketCorsResponse"
    "fixture/DeleteBucketCorsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketCors)

responseDeleteBucketTagging :: DeleteBucketTaggingResponse -> TestTree
responseDeleteBucketTagging =
  res
    "DeleteBucketTaggingResponse"
    "fixture/DeleteBucketTaggingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketTagging)

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

responsePutBucketIntelligentTieringConfiguration :: PutBucketIntelligentTieringConfigurationResponse -> TestTree
responsePutBucketIntelligentTieringConfiguration =
  res
    "PutBucketIntelligentTieringConfigurationResponse"
    "fixture/PutBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutBucketIntelligentTieringConfiguration)
