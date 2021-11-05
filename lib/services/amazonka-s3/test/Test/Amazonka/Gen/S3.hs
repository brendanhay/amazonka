{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.S3
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestPutBucketRequestPayment $
--             newPutBucketRequestPayment
--
--         , requestPutObject $
--             newPutObject
--
--         , requestDeleteObject $
--             newDeleteObject
--
--         , requestPutBucketLogging $
--             newPutBucketLogging
--
--         , requestGetBucketMetricsConfiguration $
--             newGetBucketMetricsConfiguration
--
--         , requestListBuckets $
--             newListBuckets
--
--         , requestDeleteBucket $
--             newDeleteBucket
--
--         , requestCreateBucket $
--             newCreateBucket
--
--         , requestDeleteBucketTagging $
--             newDeleteBucketTagging
--
--         , requestPutObjectAcl $
--             newPutObjectAcl
--
--         , requestPutBucketTagging $
--             newPutBucketTagging
--
--         , requestGetBucketInventoryConfiguration $
--             newGetBucketInventoryConfiguration
--
--         , requestDeletePublicAccessBlock $
--             newDeletePublicAccessBlock
--
--         , requestPutBucketInventoryConfiguration $
--             newPutBucketInventoryConfiguration
--
--         , requestGetBucketLocation $
--             newGetBucketLocation
--
--         , requestListBucketInventoryConfigurations $
--             newListBucketInventoryConfigurations
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
--         , requestGetBucketNotificationConfiguration $
--             newGetBucketNotificationConfiguration
--
--         , requestGetObjectLockConfiguration $
--             newGetObjectLockConfiguration
--
--         , requestPutObjectRetention $
--             newPutObjectRetention
--
--         , requestPutBucketAccelerateConfiguration $
--             newPutBucketAccelerateConfiguration
--
--         , requestPutObjectLegalHold $
--             newPutObjectLegalHold
--
--         , requestPutBucketOwnershipControls $
--             newPutBucketOwnershipControls
--
--         , requestDeleteBucketOwnershipControls $
--             newDeleteBucketOwnershipControls
--
--         , requestPutBucketMetricsConfiguration $
--             newPutBucketMetricsConfiguration
--
--         , requestDeleteBucketMetricsConfiguration $
--             newDeleteBucketMetricsConfiguration
--
--         , requestListObjectsV2 $
--             newListObjectsV2
--
--         , requestGetObject $
--             newGetObject
--
--         , requestPutBucketReplication $
--             newPutBucketReplication
--
--         , requestGetBucketWebsite $
--             newGetBucketWebsite
--
--         , requestGetBucketRequestPayment $
--             newGetBucketRequestPayment
--
--         , requestDeleteBucketReplication $
--             newDeleteBucketReplication
--
--         , requestListObjectVersions $
--             newListObjectVersions
--
--         , requestHeadBucket $
--             newHeadBucket
--
--         , requestDeleteBucketLifecycle $
--             newDeleteBucketLifecycle
--
--         , requestPutBucketLifecycleConfiguration $
--             newPutBucketLifecycleConfiguration
--
--         , requestPutBucketAnalyticsConfiguration $
--             newPutBucketAnalyticsConfiguration
--
--         , requestListBucketAnalyticsConfigurations $
--             newListBucketAnalyticsConfigurations
--
--         , requestDeleteBucketAnalyticsConfiguration $
--             newDeleteBucketAnalyticsConfiguration
--
--         , requestCreateMultipartUpload $
--             newCreateMultipartUpload
--
--         , requestGetBucketPolicyStatus $
--             newGetBucketPolicyStatus
--
--         , requestUploadPart $
--             newUploadPart
--
--         , requestSelectObjectContent $
--             newSelectObjectContent
--
--         , requestGetBucketReplication $
--             newGetBucketReplication
--
--         , requestPutBucketWebsite $
--             newPutBucketWebsite
--
--         , requestDeleteBucketWebsite $
--             newDeleteBucketWebsite
--
--         , requestCompleteMultipartUpload $
--             newCompleteMultipartUpload
--
--         , requestListMultipartUploads $
--             newListMultipartUploads
--
--         , requestListObjects $
--             newListObjects
--
--         , requestGetBucketOwnershipControls $
--             newGetBucketOwnershipControls
--
--         , requestGetObjectLegalHold $
--             newGetObjectLegalHold
--
--         , requestGetObjectRetention $
--             newGetObjectRetention
--
--         , requestDeleteBucketPolicy $
--             newDeleteBucketPolicy
--
--         , requestGetBucketEncryption $
--             newGetBucketEncryption
--
--         , requestAbortMultipartUpload $
--             newAbortMultipartUpload
--
--         , requestPutBucketPolicy $
--             newPutBucketPolicy
--
--         , requestGetBucketAccelerateConfiguration $
--             newGetBucketAccelerateConfiguration
--
--         , requestGetObjectTorrent $
--             newGetObjectTorrent
--
--         , requestDeleteObjects $
--             newDeleteObjects
--
--         , requestPutObjectLockConfiguration $
--             newPutObjectLockConfiguration
--
--         , requestPutBucketNotificationConfiguration $
--             newPutBucketNotificationConfiguration
--
--         , requestGetBucketVersioning $
--             newGetBucketVersioning
--
--         , requestDeleteBucketCors $
--             newDeleteBucketCors
--
--         , requestDeleteBucketIntelligentTieringConfiguration $
--             newDeleteBucketIntelligentTieringConfiguration
--
--         , requestListBucketIntelligentTieringConfigurations $
--             newListBucketIntelligentTieringConfigurations
--
--         , requestPutBucketCors $
--             newPutBucketCors
--
--         , requestGetPublicAccessBlock $
--             newGetPublicAccessBlock
--
--         , requestPutBucketIntelligentTieringConfiguration $
--             newPutBucketIntelligentTieringConfiguration
--
--         , requestGetBucketCors $
--             newGetBucketCors
--
--         , requestWriteGetObjectResponse $
--             newWriteGetObjectResponse
--
--         , requestGetObjectAcl $
--             newGetObjectAcl
--
--         , requestRestoreObject $
--             newRestoreObject
--
--         , requestHeadObject $
--             newHeadObject
--
--         , requestPutBucketVersioning $
--             newPutBucketVersioning
--
--         , requestGetBucketTagging $
--             newGetBucketTagging
--
--         , requestCopyObject $
--             newCopyObject
--
--         , requestListBucketMetricsConfigurations $
--             newListBucketMetricsConfigurations
--
--         , requestGetBucketPolicy $
--             newGetBucketPolicy
--
--         , requestPutBucketEncryption $
--             newPutBucketEncryption
--
--         , requestDeleteBucketEncryption $
--             newDeleteBucketEncryption
--
--         , requestGetBucketLogging $
--             newGetBucketLogging
--
--         , requestGetBucketAcl $
--             newGetBucketAcl
--
--         , requestGetBucketLifecycleConfiguration $
--             newGetBucketLifecycleConfiguration
--
--         , requestGetBucketAnalyticsConfiguration $
--             newGetBucketAnalyticsConfiguration
--
--         , requestGetObjectTagging $
--             newGetObjectTagging
--
--         , requestListParts $
--             newListParts
--
--         , requestDeleteObjectTagging $
--             newDeleteObjectTagging
--
--         , requestUploadPartCopy $
--             newUploadPartCopy
--
--         , requestPutObjectTagging $
--             newPutObjectTagging
--
--         , requestPutBucketAcl $
--             newPutBucketAcl
--
--           ]

--     , testGroup "response"
--         [ responsePutBucketRequestPayment $
--             newPutBucketRequestPaymentResponse
--
--         , responsePutObject $
--             newPutObjectResponse
--
--         , responseDeleteObject $
--             newDeleteObjectResponse
--
--         , responsePutBucketLogging $
--             newPutBucketLoggingResponse
--
--         , responseGetBucketMetricsConfiguration $
--             newGetBucketMetricsConfigurationResponse
--
--         , responseListBuckets $
--             newListBucketsResponse
--
--         , responseDeleteBucket $
--             newDeleteBucketResponse
--
--         , responseCreateBucket $
--             newCreateBucketResponse
--
--         , responseDeleteBucketTagging $
--             newDeleteBucketTaggingResponse
--
--         , responsePutObjectAcl $
--             newPutObjectAclResponse
--
--         , responsePutBucketTagging $
--             newPutBucketTaggingResponse
--
--         , responseGetBucketInventoryConfiguration $
--             newGetBucketInventoryConfigurationResponse
--
--         , responseDeletePublicAccessBlock $
--             newDeletePublicAccessBlockResponse
--
--         , responsePutBucketInventoryConfiguration $
--             newPutBucketInventoryConfigurationResponse
--
--         , responseGetBucketLocation $
--             newGetBucketLocationResponse
--
--         , responseListBucketInventoryConfigurations $
--             newListBucketInventoryConfigurationsResponse
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
--         , responseGetBucketNotificationConfiguration $
--             newNotificationConfiguration
--
--         , responseGetObjectLockConfiguration $
--             newGetObjectLockConfigurationResponse
--
--         , responsePutObjectRetention $
--             newPutObjectRetentionResponse
--
--         , responsePutBucketAccelerateConfiguration $
--             newPutBucketAccelerateConfigurationResponse
--
--         , responsePutObjectLegalHold $
--             newPutObjectLegalHoldResponse
--
--         , responsePutBucketOwnershipControls $
--             newPutBucketOwnershipControlsResponse
--
--         , responseDeleteBucketOwnershipControls $
--             newDeleteBucketOwnershipControlsResponse
--
--         , responsePutBucketMetricsConfiguration $
--             newPutBucketMetricsConfigurationResponse
--
--         , responseDeleteBucketMetricsConfiguration $
--             newDeleteBucketMetricsConfigurationResponse
--
--         , responseListObjectsV2 $
--             newListObjectsV2Response
--
--         , responseGetObject $
--             newGetObjectResponse
--
--         , responsePutBucketReplication $
--             newPutBucketReplicationResponse
--
--         , responseGetBucketWebsite $
--             newGetBucketWebsiteResponse
--
--         , responseGetBucketRequestPayment $
--             newGetBucketRequestPaymentResponse
--
--         , responseDeleteBucketReplication $
--             newDeleteBucketReplicationResponse
--
--         , responseListObjectVersions $
--             newListObjectVersionsResponse
--
--         , responseHeadBucket $
--             newHeadBucketResponse
--
--         , responseDeleteBucketLifecycle $
--             newDeleteBucketLifecycleResponse
--
--         , responsePutBucketLifecycleConfiguration $
--             newPutBucketLifecycleConfigurationResponse
--
--         , responsePutBucketAnalyticsConfiguration $
--             newPutBucketAnalyticsConfigurationResponse
--
--         , responseListBucketAnalyticsConfigurations $
--             newListBucketAnalyticsConfigurationsResponse
--
--         , responseDeleteBucketAnalyticsConfiguration $
--             newDeleteBucketAnalyticsConfigurationResponse
--
--         , responseCreateMultipartUpload $
--             newCreateMultipartUploadResponse
--
--         , responseGetBucketPolicyStatus $
--             newGetBucketPolicyStatusResponse
--
--         , responseUploadPart $
--             newUploadPartResponse
--
--         , responseSelectObjectContent $
--             newSelectObjectContentResponse
--
--         , responseGetBucketReplication $
--             newGetBucketReplicationResponse
--
--         , responsePutBucketWebsite $
--             newPutBucketWebsiteResponse
--
--         , responseDeleteBucketWebsite $
--             newDeleteBucketWebsiteResponse
--
--         , responseCompleteMultipartUpload $
--             newCompleteMultipartUploadResponse
--
--         , responseListMultipartUploads $
--             newListMultipartUploadsResponse
--
--         , responseListObjects $
--             newListObjectsResponse
--
--         , responseGetBucketOwnershipControls $
--             newGetBucketOwnershipControlsResponse
--
--         , responseGetObjectLegalHold $
--             newGetObjectLegalHoldResponse
--
--         , responseGetObjectRetention $
--             newGetObjectRetentionResponse
--
--         , responseDeleteBucketPolicy $
--             newDeleteBucketPolicyResponse
--
--         , responseGetBucketEncryption $
--             newGetBucketEncryptionResponse
--
--         , responseAbortMultipartUpload $
--             newAbortMultipartUploadResponse
--
--         , responsePutBucketPolicy $
--             newPutBucketPolicyResponse
--
--         , responseGetBucketAccelerateConfiguration $
--             newGetBucketAccelerateConfigurationResponse
--
--         , responseGetObjectTorrent $
--             newGetObjectTorrentResponse
--
--         , responseDeleteObjects $
--             newDeleteObjectsResponse
--
--         , responsePutObjectLockConfiguration $
--             newPutObjectLockConfigurationResponse
--
--         , responsePutBucketNotificationConfiguration $
--             newPutBucketNotificationConfigurationResponse
--
--         , responseGetBucketVersioning $
--             newGetBucketVersioningResponse
--
--         , responseDeleteBucketCors $
--             newDeleteBucketCorsResponse
--
--         , responseDeleteBucketIntelligentTieringConfiguration $
--             newDeleteBucketIntelligentTieringConfigurationResponse
--
--         , responseListBucketIntelligentTieringConfigurations $
--             newListBucketIntelligentTieringConfigurationsResponse
--
--         , responsePutBucketCors $
--             newPutBucketCorsResponse
--
--         , responseGetPublicAccessBlock $
--             newGetPublicAccessBlockResponse
--
--         , responsePutBucketIntelligentTieringConfiguration $
--             newPutBucketIntelligentTieringConfigurationResponse
--
--         , responseGetBucketCors $
--             newGetBucketCorsResponse
--
--         , responseWriteGetObjectResponse $
--             newWriteGetObjectResponseResponse
--
--         , responseGetObjectAcl $
--             newGetObjectAclResponse
--
--         , responseRestoreObject $
--             newRestoreObjectResponse
--
--         , responseHeadObject $
--             newHeadObjectResponse
--
--         , responsePutBucketVersioning $
--             newPutBucketVersioningResponse
--
--         , responseGetBucketTagging $
--             newGetBucketTaggingResponse
--
--         , responseCopyObject $
--             newCopyObjectResponse
--
--         , responseListBucketMetricsConfigurations $
--             newListBucketMetricsConfigurationsResponse
--
--         , responseGetBucketPolicy $
--             newGetBucketPolicyResponse
--
--         , responsePutBucketEncryption $
--             newPutBucketEncryptionResponse
--
--         , responseDeleteBucketEncryption $
--             newDeleteBucketEncryptionResponse
--
--         , responseGetBucketLogging $
--             newGetBucketLoggingResponse
--
--         , responseGetBucketAcl $
--             newGetBucketAclResponse
--
--         , responseGetBucketLifecycleConfiguration $
--             newGetBucketLifecycleConfigurationResponse
--
--         , responseGetBucketAnalyticsConfiguration $
--             newGetBucketAnalyticsConfigurationResponse
--
--         , responseGetObjectTagging $
--             newGetObjectTaggingResponse
--
--         , responseListParts $
--             newListPartsResponse
--
--         , responseDeleteObjectTagging $
--             newDeleteObjectTaggingResponse
--
--         , responseUploadPartCopy $
--             newUploadPartCopyResponse
--
--         , responsePutObjectTagging $
--             newPutObjectTaggingResponse
--
--         , responsePutBucketAcl $
--             newPutBucketAclResponse
--
--           ]
--     ]

-- Requests

requestPutBucketRequestPayment :: PutBucketRequestPayment -> TestTree
requestPutBucketRequestPayment =
  req
    "PutBucketRequestPayment"
    "fixture/PutBucketRequestPayment.yaml"

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject =
  req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestPutBucketLogging :: PutBucketLogging -> TestTree
requestPutBucketLogging =
  req
    "PutBucketLogging"
    "fixture/PutBucketLogging.yaml"

requestGetBucketMetricsConfiguration :: GetBucketMetricsConfiguration -> TestTree
requestGetBucketMetricsConfiguration =
  req
    "GetBucketMetricsConfiguration"
    "fixture/GetBucketMetricsConfiguration.yaml"

requestListBuckets :: ListBuckets -> TestTree
requestListBuckets =
  req
    "ListBuckets"
    "fixture/ListBuckets.yaml"

requestDeleteBucket :: DeleteBucket -> TestTree
requestDeleteBucket =
  req
    "DeleteBucket"
    "fixture/DeleteBucket.yaml"

requestCreateBucket :: CreateBucket -> TestTree
requestCreateBucket =
  req
    "CreateBucket"
    "fixture/CreateBucket.yaml"

requestDeleteBucketTagging :: DeleteBucketTagging -> TestTree
requestDeleteBucketTagging =
  req
    "DeleteBucketTagging"
    "fixture/DeleteBucketTagging.yaml"

requestPutObjectAcl :: PutObjectAcl -> TestTree
requestPutObjectAcl =
  req
    "PutObjectAcl"
    "fixture/PutObjectAcl.yaml"

requestPutBucketTagging :: PutBucketTagging -> TestTree
requestPutBucketTagging =
  req
    "PutBucketTagging"
    "fixture/PutBucketTagging.yaml"

requestGetBucketInventoryConfiguration :: GetBucketInventoryConfiguration -> TestTree
requestGetBucketInventoryConfiguration =
  req
    "GetBucketInventoryConfiguration"
    "fixture/GetBucketInventoryConfiguration.yaml"

requestDeletePublicAccessBlock :: DeletePublicAccessBlock -> TestTree
requestDeletePublicAccessBlock =
  req
    "DeletePublicAccessBlock"
    "fixture/DeletePublicAccessBlock.yaml"

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

requestListBucketInventoryConfigurations :: ListBucketInventoryConfigurations -> TestTree
requestListBucketInventoryConfigurations =
  req
    "ListBucketInventoryConfigurations"
    "fixture/ListBucketInventoryConfigurations.yaml"

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

requestGetBucketNotificationConfiguration :: GetBucketNotificationConfiguration -> TestTree
requestGetBucketNotificationConfiguration =
  req
    "GetBucketNotificationConfiguration"
    "fixture/GetBucketNotificationConfiguration.yaml"

requestGetObjectLockConfiguration :: GetObjectLockConfiguration -> TestTree
requestGetObjectLockConfiguration =
  req
    "GetObjectLockConfiguration"
    "fixture/GetObjectLockConfiguration.yaml"

requestPutObjectRetention :: PutObjectRetention -> TestTree
requestPutObjectRetention =
  req
    "PutObjectRetention"
    "fixture/PutObjectRetention.yaml"

requestPutBucketAccelerateConfiguration :: PutBucketAccelerateConfiguration -> TestTree
requestPutBucketAccelerateConfiguration =
  req
    "PutBucketAccelerateConfiguration"
    "fixture/PutBucketAccelerateConfiguration.yaml"

requestPutObjectLegalHold :: PutObjectLegalHold -> TestTree
requestPutObjectLegalHold =
  req
    "PutObjectLegalHold"
    "fixture/PutObjectLegalHold.yaml"

requestPutBucketOwnershipControls :: PutBucketOwnershipControls -> TestTree
requestPutBucketOwnershipControls =
  req
    "PutBucketOwnershipControls"
    "fixture/PutBucketOwnershipControls.yaml"

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

requestDeleteBucketMetricsConfiguration :: DeleteBucketMetricsConfiguration -> TestTree
requestDeleteBucketMetricsConfiguration =
  req
    "DeleteBucketMetricsConfiguration"
    "fixture/DeleteBucketMetricsConfiguration.yaml"

requestListObjectsV2 :: ListObjectsV2 -> TestTree
requestListObjectsV2 =
  req
    "ListObjectsV2"
    "fixture/ListObjectsV2.yaml"

requestGetObject :: GetObject -> TestTree
requestGetObject =
  req
    "GetObject"
    "fixture/GetObject.yaml"

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

requestDeleteBucketAnalyticsConfiguration :: DeleteBucketAnalyticsConfiguration -> TestTree
requestDeleteBucketAnalyticsConfiguration =
  req
    "DeleteBucketAnalyticsConfiguration"
    "fixture/DeleteBucketAnalyticsConfiguration.yaml"

requestCreateMultipartUpload :: CreateMultipartUpload -> TestTree
requestCreateMultipartUpload =
  req
    "CreateMultipartUpload"
    "fixture/CreateMultipartUpload.yaml"

requestGetBucketPolicyStatus :: GetBucketPolicyStatus -> TestTree
requestGetBucketPolicyStatus =
  req
    "GetBucketPolicyStatus"
    "fixture/GetBucketPolicyStatus.yaml"

requestSelectObjectContent :: SelectObjectContent -> TestTree
requestSelectObjectContent =
  req
    "SelectObjectContent"
    "fixture/SelectObjectContent.yaml"

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

requestListMultipartUploads :: ListMultipartUploads -> TestTree
requestListMultipartUploads =
  req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads.yaml"

requestListObjects :: ListObjects -> TestTree
requestListObjects =
  req
    "ListObjects"
    "fixture/ListObjects.yaml"

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

requestGetObjectRetention :: GetObjectRetention -> TestTree
requestGetObjectRetention =
  req
    "GetObjectRetention"
    "fixture/GetObjectRetention.yaml"

requestDeleteBucketPolicy :: DeleteBucketPolicy -> TestTree
requestDeleteBucketPolicy =
  req
    "DeleteBucketPolicy"
    "fixture/DeleteBucketPolicy.yaml"

requestGetBucketEncryption :: GetBucketEncryption -> TestTree
requestGetBucketEncryption =
  req
    "GetBucketEncryption"
    "fixture/GetBucketEncryption.yaml"

requestAbortMultipartUpload :: AbortMultipartUpload -> TestTree
requestAbortMultipartUpload =
  req
    "AbortMultipartUpload"
    "fixture/AbortMultipartUpload.yaml"

requestPutBucketPolicy :: PutBucketPolicy -> TestTree
requestPutBucketPolicy =
  req
    "PutBucketPolicy"
    "fixture/PutBucketPolicy.yaml"

requestGetBucketAccelerateConfiguration :: GetBucketAccelerateConfiguration -> TestTree
requestGetBucketAccelerateConfiguration =
  req
    "GetBucketAccelerateConfiguration"
    "fixture/GetBucketAccelerateConfiguration.yaml"

requestGetObjectTorrent :: GetObjectTorrent -> TestTree
requestGetObjectTorrent =
  req
    "GetObjectTorrent"
    "fixture/GetObjectTorrent.yaml"

requestDeleteObjects :: DeleteObjects -> TestTree
requestDeleteObjects =
  req
    "DeleteObjects"
    "fixture/DeleteObjects.yaml"

requestPutObjectLockConfiguration :: PutObjectLockConfiguration -> TestTree
requestPutObjectLockConfiguration =
  req
    "PutObjectLockConfiguration"
    "fixture/PutObjectLockConfiguration.yaml"

requestPutBucketNotificationConfiguration :: PutBucketNotificationConfiguration -> TestTree
requestPutBucketNotificationConfiguration =
  req
    "PutBucketNotificationConfiguration"
    "fixture/PutBucketNotificationConfiguration.yaml"

requestGetBucketVersioning :: GetBucketVersioning -> TestTree
requestGetBucketVersioning =
  req
    "GetBucketVersioning"
    "fixture/GetBucketVersioning.yaml"

requestDeleteBucketCors :: DeleteBucketCors -> TestTree
requestDeleteBucketCors =
  req
    "DeleteBucketCors"
    "fixture/DeleteBucketCors.yaml"

requestDeleteBucketIntelligentTieringConfiguration :: DeleteBucketIntelligentTieringConfiguration -> TestTree
requestDeleteBucketIntelligentTieringConfiguration =
  req
    "DeleteBucketIntelligentTieringConfiguration"
    "fixture/DeleteBucketIntelligentTieringConfiguration.yaml"

requestListBucketIntelligentTieringConfigurations :: ListBucketIntelligentTieringConfigurations -> TestTree
requestListBucketIntelligentTieringConfigurations =
  req
    "ListBucketIntelligentTieringConfigurations"
    "fixture/ListBucketIntelligentTieringConfigurations.yaml"

requestPutBucketCors :: PutBucketCors -> TestTree
requestPutBucketCors =
  req
    "PutBucketCors"
    "fixture/PutBucketCors.yaml"

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

requestGetBucketCors :: GetBucketCors -> TestTree
requestGetBucketCors =
  req
    "GetBucketCors"
    "fixture/GetBucketCors.yaml"

requestGetObjectAcl :: GetObjectAcl -> TestTree
requestGetObjectAcl =
  req
    "GetObjectAcl"
    "fixture/GetObjectAcl.yaml"

requestRestoreObject :: RestoreObject -> TestTree
requestRestoreObject =
  req
    "RestoreObject"
    "fixture/RestoreObject.yaml"

requestHeadObject :: HeadObject -> TestTree
requestHeadObject =
  req
    "HeadObject"
    "fixture/HeadObject.yaml"

requestPutBucketVersioning :: PutBucketVersioning -> TestTree
requestPutBucketVersioning =
  req
    "PutBucketVersioning"
    "fixture/PutBucketVersioning.yaml"

requestGetBucketTagging :: GetBucketTagging -> TestTree
requestGetBucketTagging =
  req
    "GetBucketTagging"
    "fixture/GetBucketTagging.yaml"

requestCopyObject :: CopyObject -> TestTree
requestCopyObject =
  req
    "CopyObject"
    "fixture/CopyObject.yaml"

requestListBucketMetricsConfigurations :: ListBucketMetricsConfigurations -> TestTree
requestListBucketMetricsConfigurations =
  req
    "ListBucketMetricsConfigurations"
    "fixture/ListBucketMetricsConfigurations.yaml"

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

requestDeleteBucketEncryption :: DeleteBucketEncryption -> TestTree
requestDeleteBucketEncryption =
  req
    "DeleteBucketEncryption"
    "fixture/DeleteBucketEncryption.yaml"

requestGetBucketLogging :: GetBucketLogging -> TestTree
requestGetBucketLogging =
  req
    "GetBucketLogging"
    "fixture/GetBucketLogging.yaml"

requestGetBucketAcl :: GetBucketAcl -> TestTree
requestGetBucketAcl =
  req
    "GetBucketAcl"
    "fixture/GetBucketAcl.yaml"

requestGetBucketLifecycleConfiguration :: GetBucketLifecycleConfiguration -> TestTree
requestGetBucketLifecycleConfiguration =
  req
    "GetBucketLifecycleConfiguration"
    "fixture/GetBucketLifecycleConfiguration.yaml"

requestGetBucketAnalyticsConfiguration :: GetBucketAnalyticsConfiguration -> TestTree
requestGetBucketAnalyticsConfiguration =
  req
    "GetBucketAnalyticsConfiguration"
    "fixture/GetBucketAnalyticsConfiguration.yaml"

requestGetObjectTagging :: GetObjectTagging -> TestTree
requestGetObjectTagging =
  req
    "GetObjectTagging"
    "fixture/GetObjectTagging.yaml"

requestListParts :: ListParts -> TestTree
requestListParts =
  req
    "ListParts"
    "fixture/ListParts.yaml"

requestDeleteObjectTagging :: DeleteObjectTagging -> TestTree
requestDeleteObjectTagging =
  req
    "DeleteObjectTagging"
    "fixture/DeleteObjectTagging.yaml"

requestUploadPartCopy :: UploadPartCopy -> TestTree
requestUploadPartCopy =
  req
    "UploadPartCopy"
    "fixture/UploadPartCopy.yaml"

requestPutObjectTagging :: PutObjectTagging -> TestTree
requestPutObjectTagging =
  req
    "PutObjectTagging"
    "fixture/PutObjectTagging.yaml"

requestPutBucketAcl :: PutBucketAcl -> TestTree
requestPutBucketAcl =
  req
    "PutBucketAcl"
    "fixture/PutBucketAcl.yaml"

-- Responses

responsePutBucketRequestPayment :: PutBucketRequestPaymentResponse -> TestTree
responsePutBucketRequestPayment =
  res
    "PutBucketRequestPaymentResponse"
    "fixture/PutBucketRequestPaymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketRequestPayment)

responsePutObject :: PutObjectResponse -> TestTree
responsePutObject =
  res
    "PutObjectResponse"
    "fixture/PutObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObject)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject =
  res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObject)

responsePutBucketLogging :: PutBucketLoggingResponse -> TestTree
responsePutBucketLogging =
  res
    "PutBucketLoggingResponse"
    "fixture/PutBucketLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketLogging)

responseGetBucketMetricsConfiguration :: GetBucketMetricsConfigurationResponse -> TestTree
responseGetBucketMetricsConfiguration =
  res
    "GetBucketMetricsConfigurationResponse"
    "fixture/GetBucketMetricsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketMetricsConfiguration)

responseListBuckets :: ListBucketsResponse -> TestTree
responseListBuckets =
  res
    "ListBucketsResponse"
    "fixture/ListBucketsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuckets)

responseDeleteBucket :: DeleteBucketResponse -> TestTree
responseDeleteBucket =
  res
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucket)

responseCreateBucket :: CreateBucketResponse -> TestTree
responseCreateBucket =
  res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBucket)

responseDeleteBucketTagging :: DeleteBucketTaggingResponse -> TestTree
responseDeleteBucketTagging =
  res
    "DeleteBucketTaggingResponse"
    "fixture/DeleteBucketTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketTagging)

responsePutObjectAcl :: PutObjectAclResponse -> TestTree
responsePutObjectAcl =
  res
    "PutObjectAclResponse"
    "fixture/PutObjectAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObjectAcl)

responsePutBucketTagging :: PutBucketTaggingResponse -> TestTree
responsePutBucketTagging =
  res
    "PutBucketTaggingResponse"
    "fixture/PutBucketTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketTagging)

responseGetBucketInventoryConfiguration :: GetBucketInventoryConfigurationResponse -> TestTree
responseGetBucketInventoryConfiguration =
  res
    "GetBucketInventoryConfigurationResponse"
    "fixture/GetBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketInventoryConfiguration)

responseDeletePublicAccessBlock :: DeletePublicAccessBlockResponse -> TestTree
responseDeletePublicAccessBlock =
  res
    "DeletePublicAccessBlockResponse"
    "fixture/DeletePublicAccessBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePublicAccessBlock)

responsePutBucketInventoryConfiguration :: PutBucketInventoryConfigurationResponse -> TestTree
responsePutBucketInventoryConfiguration =
  res
    "PutBucketInventoryConfigurationResponse"
    "fixture/PutBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketInventoryConfiguration)

responseGetBucketLocation :: GetBucketLocationResponse -> TestTree
responseGetBucketLocation =
  res
    "GetBucketLocationResponse"
    "fixture/GetBucketLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketLocation)

responseListBucketInventoryConfigurations :: ListBucketInventoryConfigurationsResponse -> TestTree
responseListBucketInventoryConfigurations =
  res
    "ListBucketInventoryConfigurationsResponse"
    "fixture/ListBucketInventoryConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBucketInventoryConfigurations)

responsePutPublicAccessBlock :: PutPublicAccessBlockResponse -> TestTree
responsePutPublicAccessBlock =
  res
    "PutPublicAccessBlockResponse"
    "fixture/PutPublicAccessBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPublicAccessBlock)

responseDeleteBucketInventoryConfiguration :: DeleteBucketInventoryConfigurationResponse -> TestTree
responseDeleteBucketInventoryConfiguration =
  res
    "DeleteBucketInventoryConfigurationResponse"
    "fixture/DeleteBucketInventoryConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketInventoryConfiguration)

responseGetBucketIntelligentTieringConfiguration :: GetBucketIntelligentTieringConfigurationResponse -> TestTree
responseGetBucketIntelligentTieringConfiguration =
  res
    "GetBucketIntelligentTieringConfigurationResponse"
    "fixture/GetBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketIntelligentTieringConfiguration)

responseGetBucketNotificationConfiguration :: NotificationConfiguration -> TestTree
responseGetBucketNotificationConfiguration =
  res
    "GetBucketNotificationConfigurationResponse"
    "fixture/GetBucketNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketNotificationConfiguration)

responseGetObjectLockConfiguration :: GetObjectLockConfigurationResponse -> TestTree
responseGetObjectLockConfiguration =
  res
    "GetObjectLockConfigurationResponse"
    "fixture/GetObjectLockConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectLockConfiguration)

responsePutObjectRetention :: PutObjectRetentionResponse -> TestTree
responsePutObjectRetention =
  res
    "PutObjectRetentionResponse"
    "fixture/PutObjectRetentionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObjectRetention)

responsePutBucketAccelerateConfiguration :: PutBucketAccelerateConfigurationResponse -> TestTree
responsePutBucketAccelerateConfiguration =
  res
    "PutBucketAccelerateConfigurationResponse"
    "fixture/PutBucketAccelerateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketAccelerateConfiguration)

responsePutObjectLegalHold :: PutObjectLegalHoldResponse -> TestTree
responsePutObjectLegalHold =
  res
    "PutObjectLegalHoldResponse"
    "fixture/PutObjectLegalHoldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObjectLegalHold)

responsePutBucketOwnershipControls :: PutBucketOwnershipControlsResponse -> TestTree
responsePutBucketOwnershipControls =
  res
    "PutBucketOwnershipControlsResponse"
    "fixture/PutBucketOwnershipControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketOwnershipControls)

responseDeleteBucketOwnershipControls :: DeleteBucketOwnershipControlsResponse -> TestTree
responseDeleteBucketOwnershipControls =
  res
    "DeleteBucketOwnershipControlsResponse"
    "fixture/DeleteBucketOwnershipControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketOwnershipControls)

responsePutBucketMetricsConfiguration :: PutBucketMetricsConfigurationResponse -> TestTree
responsePutBucketMetricsConfiguration =
  res
    "PutBucketMetricsConfigurationResponse"
    "fixture/PutBucketMetricsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketMetricsConfiguration)

responseDeleteBucketMetricsConfiguration :: DeleteBucketMetricsConfigurationResponse -> TestTree
responseDeleteBucketMetricsConfiguration =
  res
    "DeleteBucketMetricsConfigurationResponse"
    "fixture/DeleteBucketMetricsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketMetricsConfiguration)

responseListObjectsV2 :: ListObjectsV2Response -> TestTree
responseListObjectsV2 =
  res
    "ListObjectsV2Response"
    "fixture/ListObjectsV2Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectsV2)

responsePutBucketReplication :: PutBucketReplicationResponse -> TestTree
responsePutBucketReplication =
  res
    "PutBucketReplicationResponse"
    "fixture/PutBucketReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketReplication)

responseGetBucketWebsite :: GetBucketWebsiteResponse -> TestTree
responseGetBucketWebsite =
  res
    "GetBucketWebsiteResponse"
    "fixture/GetBucketWebsiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketWebsite)

responseGetBucketRequestPayment :: GetBucketRequestPaymentResponse -> TestTree
responseGetBucketRequestPayment =
  res
    "GetBucketRequestPaymentResponse"
    "fixture/GetBucketRequestPaymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketRequestPayment)

responseDeleteBucketReplication :: DeleteBucketReplicationResponse -> TestTree
responseDeleteBucketReplication =
  res
    "DeleteBucketReplicationResponse"
    "fixture/DeleteBucketReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketReplication)

responseListObjectVersions :: ListObjectVersionsResponse -> TestTree
responseListObjectVersions =
  res
    "ListObjectVersionsResponse"
    "fixture/ListObjectVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectVersions)

responseHeadBucket :: HeadBucketResponse -> TestTree
responseHeadBucket =
  res
    "HeadBucketResponse"
    "fixture/HeadBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy HeadBucket)

responseDeleteBucketLifecycle :: DeleteBucketLifecycleResponse -> TestTree
responseDeleteBucketLifecycle =
  res
    "DeleteBucketLifecycleResponse"
    "fixture/DeleteBucketLifecycleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketLifecycle)

responsePutBucketLifecycleConfiguration :: PutBucketLifecycleConfigurationResponse -> TestTree
responsePutBucketLifecycleConfiguration =
  res
    "PutBucketLifecycleConfigurationResponse"
    "fixture/PutBucketLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketLifecycleConfiguration)

responsePutBucketAnalyticsConfiguration :: PutBucketAnalyticsConfigurationResponse -> TestTree
responsePutBucketAnalyticsConfiguration =
  res
    "PutBucketAnalyticsConfigurationResponse"
    "fixture/PutBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketAnalyticsConfiguration)

responseListBucketAnalyticsConfigurations :: ListBucketAnalyticsConfigurationsResponse -> TestTree
responseListBucketAnalyticsConfigurations =
  res
    "ListBucketAnalyticsConfigurationsResponse"
    "fixture/ListBucketAnalyticsConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBucketAnalyticsConfigurations)

responseDeleteBucketAnalyticsConfiguration :: DeleteBucketAnalyticsConfigurationResponse -> TestTree
responseDeleteBucketAnalyticsConfiguration =
  res
    "DeleteBucketAnalyticsConfigurationResponse"
    "fixture/DeleteBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketAnalyticsConfiguration)

responseCreateMultipartUpload :: CreateMultipartUploadResponse -> TestTree
responseCreateMultipartUpload =
  res
    "CreateMultipartUploadResponse"
    "fixture/CreateMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMultipartUpload)

responseGetBucketPolicyStatus :: GetBucketPolicyStatusResponse -> TestTree
responseGetBucketPolicyStatus =
  res
    "GetBucketPolicyStatusResponse"
    "fixture/GetBucketPolicyStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketPolicyStatus)

responseUploadPart :: UploadPartResponse -> TestTree
responseUploadPart =
  res
    "UploadPartResponse"
    "fixture/UploadPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadPart)

responseSelectObjectContent :: SelectObjectContentResponse -> TestTree
responseSelectObjectContent =
  res
    "SelectObjectContentResponse"
    "fixture/SelectObjectContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SelectObjectContent)

responseGetBucketReplication :: GetBucketReplicationResponse -> TestTree
responseGetBucketReplication =
  res
    "GetBucketReplicationResponse"
    "fixture/GetBucketReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketReplication)

responsePutBucketWebsite :: PutBucketWebsiteResponse -> TestTree
responsePutBucketWebsite =
  res
    "PutBucketWebsiteResponse"
    "fixture/PutBucketWebsiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketWebsite)

responseDeleteBucketWebsite :: DeleteBucketWebsiteResponse -> TestTree
responseDeleteBucketWebsite =
  res
    "DeleteBucketWebsiteResponse"
    "fixture/DeleteBucketWebsiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketWebsite)

responseCompleteMultipartUpload :: CompleteMultipartUploadResponse -> TestTree
responseCompleteMultipartUpload =
  res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteMultipartUpload)

responseListMultipartUploads :: ListMultipartUploadsResponse -> TestTree
responseListMultipartUploads =
  res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMultipartUploads)

responseListObjects :: ListObjectsResponse -> TestTree
responseListObjects =
  res
    "ListObjectsResponse"
    "fixture/ListObjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjects)

responseGetBucketOwnershipControls :: GetBucketOwnershipControlsResponse -> TestTree
responseGetBucketOwnershipControls =
  res
    "GetBucketOwnershipControlsResponse"
    "fixture/GetBucketOwnershipControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketOwnershipControls)

responseGetObjectLegalHold :: GetObjectLegalHoldResponse -> TestTree
responseGetObjectLegalHold =
  res
    "GetObjectLegalHoldResponse"
    "fixture/GetObjectLegalHoldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectLegalHold)

responseGetObjectRetention :: GetObjectRetentionResponse -> TestTree
responseGetObjectRetention =
  res
    "GetObjectRetentionResponse"
    "fixture/GetObjectRetentionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectRetention)

responseDeleteBucketPolicy :: DeleteBucketPolicyResponse -> TestTree
responseDeleteBucketPolicy =
  res
    "DeleteBucketPolicyResponse"
    "fixture/DeleteBucketPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketPolicy)

responseGetBucketEncryption :: GetBucketEncryptionResponse -> TestTree
responseGetBucketEncryption =
  res
    "GetBucketEncryptionResponse"
    "fixture/GetBucketEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketEncryption)

responseAbortMultipartUpload :: AbortMultipartUploadResponse -> TestTree
responseAbortMultipartUpload =
  res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AbortMultipartUpload)

responsePutBucketPolicy :: PutBucketPolicyResponse -> TestTree
responsePutBucketPolicy =
  res
    "PutBucketPolicyResponse"
    "fixture/PutBucketPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketPolicy)

responseGetBucketAccelerateConfiguration :: GetBucketAccelerateConfigurationResponse -> TestTree
responseGetBucketAccelerateConfiguration =
  res
    "GetBucketAccelerateConfigurationResponse"
    "fixture/GetBucketAccelerateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketAccelerateConfiguration)

responseDeleteObjects :: DeleteObjectsResponse -> TestTree
responseDeleteObjects =
  res
    "DeleteObjectsResponse"
    "fixture/DeleteObjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObjects)

responsePutObjectLockConfiguration :: PutObjectLockConfigurationResponse -> TestTree
responsePutObjectLockConfiguration =
  res
    "PutObjectLockConfigurationResponse"
    "fixture/PutObjectLockConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObjectLockConfiguration)

responsePutBucketNotificationConfiguration :: PutBucketNotificationConfigurationResponse -> TestTree
responsePutBucketNotificationConfiguration =
  res
    "PutBucketNotificationConfigurationResponse"
    "fixture/PutBucketNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketNotificationConfiguration)

responseGetBucketVersioning :: GetBucketVersioningResponse -> TestTree
responseGetBucketVersioning =
  res
    "GetBucketVersioningResponse"
    "fixture/GetBucketVersioningResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketVersioning)

responseDeleteBucketCors :: DeleteBucketCorsResponse -> TestTree
responseDeleteBucketCors =
  res
    "DeleteBucketCorsResponse"
    "fixture/DeleteBucketCorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketCors)

responseDeleteBucketIntelligentTieringConfiguration :: DeleteBucketIntelligentTieringConfigurationResponse -> TestTree
responseDeleteBucketIntelligentTieringConfiguration =
  res
    "DeleteBucketIntelligentTieringConfigurationResponse"
    "fixture/DeleteBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketIntelligentTieringConfiguration)

responseListBucketIntelligentTieringConfigurations :: ListBucketIntelligentTieringConfigurationsResponse -> TestTree
responseListBucketIntelligentTieringConfigurations =
  res
    "ListBucketIntelligentTieringConfigurationsResponse"
    "fixture/ListBucketIntelligentTieringConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBucketIntelligentTieringConfigurations)

responsePutBucketCors :: PutBucketCorsResponse -> TestTree
responsePutBucketCors =
  res
    "PutBucketCorsResponse"
    "fixture/PutBucketCorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketCors)

responseGetPublicAccessBlock :: GetPublicAccessBlockResponse -> TestTree
responseGetPublicAccessBlock =
  res
    "GetPublicAccessBlockResponse"
    "fixture/GetPublicAccessBlockResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPublicAccessBlock)

responsePutBucketIntelligentTieringConfiguration :: PutBucketIntelligentTieringConfigurationResponse -> TestTree
responsePutBucketIntelligentTieringConfiguration =
  res
    "PutBucketIntelligentTieringConfigurationResponse"
    "fixture/PutBucketIntelligentTieringConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketIntelligentTieringConfiguration)

responseGetBucketCors :: GetBucketCorsResponse -> TestTree
responseGetBucketCors =
  res
    "GetBucketCorsResponse"
    "fixture/GetBucketCorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketCors)

responseWriteGetObjectResponse :: WriteGetObjectResponseResponse -> TestTree
responseWriteGetObjectResponse =
  res
    "WriteGetObjectResponseResponse"
    "fixture/WriteGetObjectResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy WriteGetObjectResponse)

responseGetObjectAcl :: GetObjectAclResponse -> TestTree
responseGetObjectAcl =
  res
    "GetObjectAclResponse"
    "fixture/GetObjectAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectAcl)

responseRestoreObject :: RestoreObjectResponse -> TestTree
responseRestoreObject =
  res
    "RestoreObjectResponse"
    "fixture/RestoreObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreObject)

responseHeadObject :: HeadObjectResponse -> TestTree
responseHeadObject =
  res
    "HeadObjectResponse"
    "fixture/HeadObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy HeadObject)

responsePutBucketVersioning :: PutBucketVersioningResponse -> TestTree
responsePutBucketVersioning =
  res
    "PutBucketVersioningResponse"
    "fixture/PutBucketVersioningResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketVersioning)

responseGetBucketTagging :: GetBucketTaggingResponse -> TestTree
responseGetBucketTagging =
  res
    "GetBucketTaggingResponse"
    "fixture/GetBucketTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketTagging)

responseCopyObject :: CopyObjectResponse -> TestTree
responseCopyObject =
  res
    "CopyObjectResponse"
    "fixture/CopyObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyObject)

responseListBucketMetricsConfigurations :: ListBucketMetricsConfigurationsResponse -> TestTree
responseListBucketMetricsConfigurations =
  res
    "ListBucketMetricsConfigurationsResponse"
    "fixture/ListBucketMetricsConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBucketMetricsConfigurations)

responseGetBucketPolicy :: GetBucketPolicyResponse -> TestTree
responseGetBucketPolicy =
  res
    "GetBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketPolicy)

responsePutBucketEncryption :: PutBucketEncryptionResponse -> TestTree
responsePutBucketEncryption =
  res
    "PutBucketEncryptionResponse"
    "fixture/PutBucketEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketEncryption)

responseDeleteBucketEncryption :: DeleteBucketEncryptionResponse -> TestTree
responseDeleteBucketEncryption =
  res
    "DeleteBucketEncryptionResponse"
    "fixture/DeleteBucketEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketEncryption)

responseGetBucketLogging :: GetBucketLoggingResponse -> TestTree
responseGetBucketLogging =
  res
    "GetBucketLoggingResponse"
    "fixture/GetBucketLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketLogging)

responseGetBucketAcl :: GetBucketAclResponse -> TestTree
responseGetBucketAcl =
  res
    "GetBucketAclResponse"
    "fixture/GetBucketAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketAcl)

responseGetBucketLifecycleConfiguration :: GetBucketLifecycleConfigurationResponse -> TestTree
responseGetBucketLifecycleConfiguration =
  res
    "GetBucketLifecycleConfigurationResponse"
    "fixture/GetBucketLifecycleConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketLifecycleConfiguration)

responseGetBucketAnalyticsConfiguration :: GetBucketAnalyticsConfigurationResponse -> TestTree
responseGetBucketAnalyticsConfiguration =
  res
    "GetBucketAnalyticsConfigurationResponse"
    "fixture/GetBucketAnalyticsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketAnalyticsConfiguration)

responseGetObjectTagging :: GetObjectTaggingResponse -> TestTree
responseGetObjectTagging =
  res
    "GetObjectTaggingResponse"
    "fixture/GetObjectTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectTagging)

responseListParts :: ListPartsResponse -> TestTree
responseListParts =
  res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListParts)

responseDeleteObjectTagging :: DeleteObjectTaggingResponse -> TestTree
responseDeleteObjectTagging =
  res
    "DeleteObjectTaggingResponse"
    "fixture/DeleteObjectTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObjectTagging)

responseUploadPartCopy :: UploadPartCopyResponse -> TestTree
responseUploadPartCopy =
  res
    "UploadPartCopyResponse"
    "fixture/UploadPartCopyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadPartCopy)

responsePutObjectTagging :: PutObjectTaggingResponse -> TestTree
responsePutObjectTagging =
  res
    "PutObjectTaggingResponse"
    "fixture/PutObjectTaggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObjectTagging)

responsePutBucketAcl :: PutBucketAclResponse -> TestTree
responsePutBucketAcl =
  res
    "PutBucketAclResponse"
    "fixture/PutBucketAclResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBucketAcl)
