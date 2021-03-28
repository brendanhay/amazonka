{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.S3
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.S3 where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.S3
import Test.AWS.S3.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutBucketRequestPayment $
--             mkPutBucketRequestPayment
--
--         , requestPutObject $
--             mkPutObject
--
--         , requestDeleteObject $
--             mkDeleteObject
--
--         , requestPutBucketLogging $
--             mkPutBucketLogging
--
--         , requestGetBucketMetricsConfiguration $
--             mkGetBucketMetricsConfiguration
--
--         , requestListBuckets $
--             mkListBuckets
--
--         , requestDeleteBucket $
--             mkDeleteBucket
--
--         , requestCreateBucket $
--             mkCreateBucket
--
--         , requestDeleteBucketTagging $
--             mkDeleteBucketTagging
--
--         , requestPutObjectAcl $
--             mkPutObjectAcl
--
--         , requestPutBucketTagging $
--             mkPutBucketTagging
--
--         , requestGetBucketInventoryConfiguration $
--             mkGetBucketInventoryConfiguration
--
--         , requestDeletePublicAccessBlock $
--             mkDeletePublicAccessBlock
--
--         , requestPutBucketInventoryConfiguration $
--             mkPutBucketInventoryConfiguration
--
--         , requestGetBucketLocation $
--             mkGetBucketLocation
--
--         , requestListBucketInventoryConfigurations $
--             mkListBucketInventoryConfigurations
--
--         , requestPutPublicAccessBlock $
--             mkPutPublicAccessBlock
--
--         , requestDeleteBucketInventoryConfiguration $
--             mkDeleteBucketInventoryConfiguration
--
--         , requestGetBucketIntelligentTieringConfiguration $
--             mkGetBucketIntelligentTieringConfiguration
--
--         , requestGetBucketNotificationConfiguration $
--             mkGetBucketNotificationConfiguration
--
--         , requestGetObjectLockConfiguration $
--             mkGetObjectLockConfiguration
--
--         , requestPutObjectRetention $
--             mkPutObjectRetention
--
--         , requestPutBucketAccelerateConfiguration $
--             mkPutBucketAccelerateConfiguration
--
--         , requestPutObjectLegalHold $
--             mkPutObjectLegalHold
--
--         , requestPutBucketOwnershipControls $
--             mkPutBucketOwnershipControls
--
--         , requestDeleteBucketOwnershipControls $
--             mkDeleteBucketOwnershipControls
--
--         , requestPutBucketMetricsConfiguration $
--             mkPutBucketMetricsConfiguration
--
--         , requestDeleteBucketMetricsConfiguration $
--             mkDeleteBucketMetricsConfiguration
--
--         , requestListObjectsV2 $
--             mkListObjectsV2
--
--         , requestGetObject $
--             mkGetObject
--
--         , requestPutBucketReplication $
--             mkPutBucketReplication
--
--         , requestGetBucketWebsite $
--             mkGetBucketWebsite
--
--         , requestGetBucketRequestPayment $
--             mkGetBucketRequestPayment
--
--         , requestDeleteBucketReplication $
--             mkDeleteBucketReplication
--
--         , requestListObjectVersions $
--             mkListObjectVersions
--
--         , requestHeadBucket $
--             mkHeadBucket
--
--         , requestDeleteBucketLifecycle $
--             mkDeleteBucketLifecycle
--
--         , requestPutBucketLifecycleConfiguration $
--             mkPutBucketLifecycleConfiguration
--
--         , requestPutBucketAnalyticsConfiguration $
--             mkPutBucketAnalyticsConfiguration
--
--         , requestListBucketAnalyticsConfigurations $
--             mkListBucketAnalyticsConfigurations
--
--         , requestDeleteBucketAnalyticsConfiguration $
--             mkDeleteBucketAnalyticsConfiguration
--
--         , requestCreateMultipartUpload $
--             mkCreateMultipartUpload
--
--         , requestGetBucketPolicyStatus $
--             mkGetBucketPolicyStatus
--
--         , requestUploadPart $
--             mkUploadPart
--
--         , requestSelectObjectContent $
--             mkSelectObjectContent
--
--         , requestGetBucketReplication $
--             mkGetBucketReplication
--
--         , requestPutBucketWebsite $
--             mkPutBucketWebsite
--
--         , requestDeleteBucketWebsite $
--             mkDeleteBucketWebsite
--
--         , requestCompleteMultipartUpload $
--             mkCompleteMultipartUpload
--
--         , requestListMultipartUploads $
--             mkListMultipartUploads
--
--         , requestListObjects $
--             mkListObjects
--
--         , requestGetBucketOwnershipControls $
--             mkGetBucketOwnershipControls
--
--         , requestGetObjectLegalHold $
--             mkGetObjectLegalHold
--
--         , requestGetObjectRetention $
--             mkGetObjectRetention
--
--         , requestDeleteBucketPolicy $
--             mkDeleteBucketPolicy
--
--         , requestGetBucketEncryption $
--             mkGetBucketEncryption
--
--         , requestAbortMultipartUpload $
--             mkAbortMultipartUpload
--
--         , requestPutBucketPolicy $
--             mkPutBucketPolicy
--
--         , requestGetBucketAccelerateConfiguration $
--             mkGetBucketAccelerateConfiguration
--
--         , requestGetObjectTorrent $
--             mkGetObjectTorrent
--
--         , requestDeleteObjects $
--             mkDeleteObjects
--
--         , requestPutObjectLockConfiguration $
--             mkPutObjectLockConfiguration
--
--         , requestPutBucketNotificationConfiguration $
--             mkPutBucketNotificationConfiguration
--
--         , requestGetBucketVersioning $
--             mkGetBucketVersioning
--
--         , requestDeleteBucketCors $
--             mkDeleteBucketCors
--
--         , requestDeleteBucketIntelligentTieringConfiguration $
--             mkDeleteBucketIntelligentTieringConfiguration
--
--         , requestListBucketIntelligentTieringConfigurations $
--             mkListBucketIntelligentTieringConfigurations
--
--         , requestPutBucketCors $
--             mkPutBucketCors
--
--         , requestGetPublicAccessBlock $
--             mkGetPublicAccessBlock
--
--         , requestPutBucketIntelligentTieringConfiguration $
--             mkPutBucketIntelligentTieringConfiguration
--
--         , requestGetBucketCors $
--             mkGetBucketCors
--
--         , requestGetObjectAcl $
--             mkGetObjectAcl
--
--         , requestRestoreObject $
--             mkRestoreObject
--
--         , requestHeadObject $
--             mkHeadObject
--
--         , requestPutBucketVersioning $
--             mkPutBucketVersioning
--
--         , requestGetBucketTagging $
--             mkGetBucketTagging
--
--         , requestCopyObject $
--             mkCopyObject
--
--         , requestListBucketMetricsConfigurations $
--             mkListBucketMetricsConfigurations
--
--         , requestGetBucketPolicy $
--             mkGetBucketPolicy
--
--         , requestPutBucketEncryption $
--             mkPutBucketEncryption
--
--         , requestDeleteBucketEncryption $
--             mkDeleteBucketEncryption
--
--         , requestGetBucketLogging $
--             mkGetBucketLogging
--
--         , requestGetBucketAcl $
--             mkGetBucketAcl
--
--         , requestGetBucketLifecycleConfiguration $
--             mkGetBucketLifecycleConfiguration
--
--         , requestGetBucketAnalyticsConfiguration $
--             mkGetBucketAnalyticsConfiguration
--
--         , requestGetObjectTagging $
--             mkGetObjectTagging
--
--         , requestListParts $
--             mkListParts
--
--         , requestDeleteObjectTagging $
--             mkDeleteObjectTagging
--
--         , requestUploadPartCopy $
--             mkUploadPartCopy
--
--         , requestPutObjectTagging $
--             mkPutObjectTagging
--
--         , requestPutBucketAcl $
--             mkPutBucketAcl
--
--           ]

--     , testGroup "response"
--         [ responsePutBucketRequestPayment $
--             mkPutBucketRequestPaymentResponse
--
--         , responsePutObject $
--             mkPutObjectResponse
--
--         , responseDeleteObject $
--             mkDeleteObjectResponse
--
--         , responsePutBucketLogging $
--             mkPutBucketLoggingResponse
--
--         , responseGetBucketMetricsConfiguration $
--             mkGetBucketMetricsConfigurationResponse
--
--         , responseListBuckets $
--             mkListBucketsResponse
--
--         , responseDeleteBucket $
--             mkDeleteBucketResponse
--
--         , responseCreateBucket $
--             mkCreateBucketResponse
--
--         , responseDeleteBucketTagging $
--             mkDeleteBucketTaggingResponse
--
--         , responsePutObjectAcl $
--             mkPutObjectAclResponse
--
--         , responsePutBucketTagging $
--             mkPutBucketTaggingResponse
--
--         , responseGetBucketInventoryConfiguration $
--             mkGetBucketInventoryConfigurationResponse
--
--         , responseDeletePublicAccessBlock $
--             mkDeletePublicAccessBlockResponse
--
--         , responsePutBucketInventoryConfiguration $
--             mkPutBucketInventoryConfigurationResponse
--
--         , responseGetBucketLocation $
--             mkGetBucketLocationResponse
--
--         , responseListBucketInventoryConfigurations $
--             mkListBucketInventoryConfigurationsResponse
--
--         , responsePutPublicAccessBlock $
--             mkPutPublicAccessBlockResponse
--
--         , responseDeleteBucketInventoryConfiguration $
--             mkDeleteBucketInventoryConfigurationResponse
--
--         , responseGetBucketIntelligentTieringConfiguration $
--             mkGetBucketIntelligentTieringConfigurationResponse
--
--         , responseGetBucketNotificationConfiguration $
--             mkNotificationConfiguration
--
--         , responseGetObjectLockConfiguration $
--             mkGetObjectLockConfigurationResponse
--
--         , responsePutObjectRetention $
--             mkPutObjectRetentionResponse
--
--         , responsePutBucketAccelerateConfiguration $
--             mkPutBucketAccelerateConfigurationResponse
--
--         , responsePutObjectLegalHold $
--             mkPutObjectLegalHoldResponse
--
--         , responsePutBucketOwnershipControls $
--             mkPutBucketOwnershipControlsResponse
--
--         , responseDeleteBucketOwnershipControls $
--             mkDeleteBucketOwnershipControlsResponse
--
--         , responsePutBucketMetricsConfiguration $
--             mkPutBucketMetricsConfigurationResponse
--
--         , responseDeleteBucketMetricsConfiguration $
--             mkDeleteBucketMetricsConfigurationResponse
--
--         , responseListObjectsV2 $
--             mkListObjectsV2Response
--
--         , responseGetObject $
--             mkGetObjectResponse
--
--         , responsePutBucketReplication $
--             mkPutBucketReplicationResponse
--
--         , responseGetBucketWebsite $
--             mkGetBucketWebsiteResponse
--
--         , responseGetBucketRequestPayment $
--             mkGetBucketRequestPaymentResponse
--
--         , responseDeleteBucketReplication $
--             mkDeleteBucketReplicationResponse
--
--         , responseListObjectVersions $
--             mkListObjectVersionsResponse
--
--         , responseHeadBucket $
--             mkHeadBucketResponse
--
--         , responseDeleteBucketLifecycle $
--             mkDeleteBucketLifecycleResponse
--
--         , responsePutBucketLifecycleConfiguration $
--             mkPutBucketLifecycleConfigurationResponse
--
--         , responsePutBucketAnalyticsConfiguration $
--             mkPutBucketAnalyticsConfigurationResponse
--
--         , responseListBucketAnalyticsConfigurations $
--             mkListBucketAnalyticsConfigurationsResponse
--
--         , responseDeleteBucketAnalyticsConfiguration $
--             mkDeleteBucketAnalyticsConfigurationResponse
--
--         , responseCreateMultipartUpload $
--             mkCreateMultipartUploadResponse
--
--         , responseGetBucketPolicyStatus $
--             mkGetBucketPolicyStatusResponse
--
--         , responseUploadPart $
--             mkUploadPartResponse
--
--         , responseSelectObjectContent $
--             mkSelectObjectContentResponse
--
--         , responseGetBucketReplication $
--             mkGetBucketReplicationResponse
--
--         , responsePutBucketWebsite $
--             mkPutBucketWebsiteResponse
--
--         , responseDeleteBucketWebsite $
--             mkDeleteBucketWebsiteResponse
--
--         , responseCompleteMultipartUpload $
--             mkCompleteMultipartUploadResponse
--
--         , responseListMultipartUploads $
--             mkListMultipartUploadsResponse
--
--         , responseListObjects $
--             mkListObjectsResponse
--
--         , responseGetBucketOwnershipControls $
--             mkGetBucketOwnershipControlsResponse
--
--         , responseGetObjectLegalHold $
--             mkGetObjectLegalHoldResponse
--
--         , responseGetObjectRetention $
--             mkGetObjectRetentionResponse
--
--         , responseDeleteBucketPolicy $
--             mkDeleteBucketPolicyResponse
--
--         , responseGetBucketEncryption $
--             mkGetBucketEncryptionResponse
--
--         , responseAbortMultipartUpload $
--             mkAbortMultipartUploadResponse
--
--         , responsePutBucketPolicy $
--             mkPutBucketPolicyResponse
--
--         , responseGetBucketAccelerateConfiguration $
--             mkGetBucketAccelerateConfigurationResponse
--
--         , responseGetObjectTorrent $
--             mkGetObjectTorrentResponse
--
--         , responseDeleteObjects $
--             mkDeleteObjectsResponse
--
--         , responsePutObjectLockConfiguration $
--             mkPutObjectLockConfigurationResponse
--
--         , responsePutBucketNotificationConfiguration $
--             mkPutBucketNotificationConfigurationResponse
--
--         , responseGetBucketVersioning $
--             mkGetBucketVersioningResponse
--
--         , responseDeleteBucketCors $
--             mkDeleteBucketCorsResponse
--
--         , responseDeleteBucketIntelligentTieringConfiguration $
--             mkDeleteBucketIntelligentTieringConfigurationResponse
--
--         , responseListBucketIntelligentTieringConfigurations $
--             mkListBucketIntelligentTieringConfigurationsResponse
--
--         , responsePutBucketCors $
--             mkPutBucketCorsResponse
--
--         , responseGetPublicAccessBlock $
--             mkGetPublicAccessBlockResponse
--
--         , responsePutBucketIntelligentTieringConfiguration $
--             mkPutBucketIntelligentTieringConfigurationResponse
--
--         , responseGetBucketCors $
--             mkGetBucketCorsResponse
--
--         , responseGetObjectAcl $
--             mkGetObjectAclResponse
--
--         , responseRestoreObject $
--             mkRestoreObjectResponse
--
--         , responseHeadObject $
--             mkHeadObjectResponse
--
--         , responsePutBucketVersioning $
--             mkPutBucketVersioningResponse
--
--         , responseGetBucketTagging $
--             mkGetBucketTaggingResponse
--
--         , responseCopyObject $
--             mkCopyObjectResponse
--
--         , responseListBucketMetricsConfigurations $
--             mkListBucketMetricsConfigurationsResponse
--
--         , responseGetBucketPolicy $
--             mkGetBucketPolicyResponse
--
--         , responsePutBucketEncryption $
--             mkPutBucketEncryptionResponse
--
--         , responseDeleteBucketEncryption $
--             mkDeleteBucketEncryptionResponse
--
--         , responseGetBucketLogging $
--             mkGetBucketLoggingResponse
--
--         , responseGetBucketAcl $
--             mkGetBucketAclResponse
--
--         , responseGetBucketLifecycleConfiguration $
--             mkGetBucketLifecycleConfigurationResponse
--
--         , responseGetBucketAnalyticsConfiguration $
--             mkGetBucketAnalyticsConfigurationResponse
--
--         , responseGetObjectTagging $
--             mkGetObjectTaggingResponse
--
--         , responseListParts $
--             mkListPartsResponse
--
--         , responseDeleteObjectTagging $
--             mkDeleteObjectTaggingResponse
--
--         , responseUploadPartCopy $
--             mkUploadPartCopyResponse
--
--         , responsePutObjectTagging $
--             mkPutObjectTaggingResponse
--
--         , responsePutBucketAcl $
--             mkPutBucketAclResponse
--
--           ]
--     ]

-- Requests

requestPutBucketRequestPayment :: PutBucketRequestPayment -> TestTree
requestPutBucketRequestPayment = req
    "PutBucketRequestPayment"
    "fixture/PutBucketRequestPayment.yaml"

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject = req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestPutBucketLogging :: PutBucketLogging -> TestTree
requestPutBucketLogging = req
    "PutBucketLogging"
    "fixture/PutBucketLogging.yaml"

requestGetBucketMetricsConfiguration :: GetBucketMetricsConfiguration -> TestTree
requestGetBucketMetricsConfiguration = req
    "GetBucketMetricsConfiguration"
    "fixture/GetBucketMetricsConfiguration.yaml"

requestListBuckets :: ListBuckets -> TestTree
requestListBuckets = req
    "ListBuckets"
    "fixture/ListBuckets.yaml"

requestDeleteBucket :: DeleteBucket -> TestTree
requestDeleteBucket = req
    "DeleteBucket"
    "fixture/DeleteBucket.yaml"

requestCreateBucket :: CreateBucket -> TestTree
requestCreateBucket = req
    "CreateBucket"
    "fixture/CreateBucket.yaml"

requestDeleteBucketTagging :: DeleteBucketTagging -> TestTree
requestDeleteBucketTagging = req
    "DeleteBucketTagging"
    "fixture/DeleteBucketTagging.yaml"

requestPutObjectAcl :: PutObjectAcl -> TestTree
requestPutObjectAcl = req
    "PutObjectAcl"
    "fixture/PutObjectAcl.yaml"

requestPutBucketTagging :: PutBucketTagging -> TestTree
requestPutBucketTagging = req
    "PutBucketTagging"
    "fixture/PutBucketTagging.yaml"

requestGetBucketInventoryConfiguration :: GetBucketInventoryConfiguration -> TestTree
requestGetBucketInventoryConfiguration = req
    "GetBucketInventoryConfiguration"
    "fixture/GetBucketInventoryConfiguration.yaml"

requestDeletePublicAccessBlock :: DeletePublicAccessBlock -> TestTree
requestDeletePublicAccessBlock = req
    "DeletePublicAccessBlock"
    "fixture/DeletePublicAccessBlock.yaml"

requestPutBucketInventoryConfiguration :: PutBucketInventoryConfiguration -> TestTree
requestPutBucketInventoryConfiguration = req
    "PutBucketInventoryConfiguration"
    "fixture/PutBucketInventoryConfiguration.yaml"

requestGetBucketLocation :: GetBucketLocation -> TestTree
requestGetBucketLocation = req
    "GetBucketLocation"
    "fixture/GetBucketLocation.yaml"

requestListBucketInventoryConfigurations :: ListBucketInventoryConfigurations -> TestTree
requestListBucketInventoryConfigurations = req
    "ListBucketInventoryConfigurations"
    "fixture/ListBucketInventoryConfigurations.yaml"

requestPutPublicAccessBlock :: PutPublicAccessBlock -> TestTree
requestPutPublicAccessBlock = req
    "PutPublicAccessBlock"
    "fixture/PutPublicAccessBlock.yaml"

requestDeleteBucketInventoryConfiguration :: DeleteBucketInventoryConfiguration -> TestTree
requestDeleteBucketInventoryConfiguration = req
    "DeleteBucketInventoryConfiguration"
    "fixture/DeleteBucketInventoryConfiguration.yaml"

requestGetBucketIntelligentTieringConfiguration :: GetBucketIntelligentTieringConfiguration -> TestTree
requestGetBucketIntelligentTieringConfiguration = req
    "GetBucketIntelligentTieringConfiguration"
    "fixture/GetBucketIntelligentTieringConfiguration.yaml"

requestGetBucketNotificationConfiguration :: GetBucketNotificationConfiguration -> TestTree
requestGetBucketNotificationConfiguration = req
    "GetBucketNotificationConfiguration"
    "fixture/GetBucketNotificationConfiguration.yaml"

requestGetObjectLockConfiguration :: GetObjectLockConfiguration -> TestTree
requestGetObjectLockConfiguration = req
    "GetObjectLockConfiguration"
    "fixture/GetObjectLockConfiguration.yaml"

requestPutObjectRetention :: PutObjectRetention -> TestTree
requestPutObjectRetention = req
    "PutObjectRetention"
    "fixture/PutObjectRetention.yaml"

requestPutBucketAccelerateConfiguration :: PutBucketAccelerateConfiguration -> TestTree
requestPutBucketAccelerateConfiguration = req
    "PutBucketAccelerateConfiguration"
    "fixture/PutBucketAccelerateConfiguration.yaml"

requestPutObjectLegalHold :: PutObjectLegalHold -> TestTree
requestPutObjectLegalHold = req
    "PutObjectLegalHold"
    "fixture/PutObjectLegalHold.yaml"

requestPutBucketOwnershipControls :: PutBucketOwnershipControls -> TestTree
requestPutBucketOwnershipControls = req
    "PutBucketOwnershipControls"
    "fixture/PutBucketOwnershipControls.yaml"

requestDeleteBucketOwnershipControls :: DeleteBucketOwnershipControls -> TestTree
requestDeleteBucketOwnershipControls = req
    "DeleteBucketOwnershipControls"
    "fixture/DeleteBucketOwnershipControls.yaml"

requestPutBucketMetricsConfiguration :: PutBucketMetricsConfiguration -> TestTree
requestPutBucketMetricsConfiguration = req
    "PutBucketMetricsConfiguration"
    "fixture/PutBucketMetricsConfiguration.yaml"

requestDeleteBucketMetricsConfiguration :: DeleteBucketMetricsConfiguration -> TestTree
requestDeleteBucketMetricsConfiguration = req
    "DeleteBucketMetricsConfiguration"
    "fixture/DeleteBucketMetricsConfiguration.yaml"

requestListObjectsV2 :: ListObjectsV2 -> TestTree
requestListObjectsV2 = req
    "ListObjectsV2"
    "fixture/ListObjectsV2.yaml"

requestGetObject :: GetObject -> TestTree
requestGetObject = req
    "GetObject"
    "fixture/GetObject.yaml"

requestPutBucketReplication :: PutBucketReplication -> TestTree
requestPutBucketReplication = req
    "PutBucketReplication"
    "fixture/PutBucketReplication.yaml"

requestGetBucketWebsite :: GetBucketWebsite -> TestTree
requestGetBucketWebsite = req
    "GetBucketWebsite"
    "fixture/GetBucketWebsite.yaml"

requestGetBucketRequestPayment :: GetBucketRequestPayment -> TestTree
requestGetBucketRequestPayment = req
    "GetBucketRequestPayment"
    "fixture/GetBucketRequestPayment.yaml"

requestDeleteBucketReplication :: DeleteBucketReplication -> TestTree
requestDeleteBucketReplication = req
    "DeleteBucketReplication"
    "fixture/DeleteBucketReplication.yaml"

requestListObjectVersions :: ListObjectVersions -> TestTree
requestListObjectVersions = req
    "ListObjectVersions"
    "fixture/ListObjectVersions.yaml"

requestHeadBucket :: HeadBucket -> TestTree
requestHeadBucket = req
    "HeadBucket"
    "fixture/HeadBucket.yaml"

requestDeleteBucketLifecycle :: DeleteBucketLifecycle -> TestTree
requestDeleteBucketLifecycle = req
    "DeleteBucketLifecycle"
    "fixture/DeleteBucketLifecycle.yaml"

requestPutBucketLifecycleConfiguration :: PutBucketLifecycleConfiguration -> TestTree
requestPutBucketLifecycleConfiguration = req
    "PutBucketLifecycleConfiguration"
    "fixture/PutBucketLifecycleConfiguration.yaml"

requestPutBucketAnalyticsConfiguration :: PutBucketAnalyticsConfiguration -> TestTree
requestPutBucketAnalyticsConfiguration = req
    "PutBucketAnalyticsConfiguration"
    "fixture/PutBucketAnalyticsConfiguration.yaml"

requestListBucketAnalyticsConfigurations :: ListBucketAnalyticsConfigurations -> TestTree
requestListBucketAnalyticsConfigurations = req
    "ListBucketAnalyticsConfigurations"
    "fixture/ListBucketAnalyticsConfigurations.yaml"

requestDeleteBucketAnalyticsConfiguration :: DeleteBucketAnalyticsConfiguration -> TestTree
requestDeleteBucketAnalyticsConfiguration = req
    "DeleteBucketAnalyticsConfiguration"
    "fixture/DeleteBucketAnalyticsConfiguration.yaml"

requestCreateMultipartUpload :: CreateMultipartUpload -> TestTree
requestCreateMultipartUpload = req
    "CreateMultipartUpload"
    "fixture/CreateMultipartUpload.yaml"

requestGetBucketPolicyStatus :: GetBucketPolicyStatus -> TestTree
requestGetBucketPolicyStatus = req
    "GetBucketPolicyStatus"
    "fixture/GetBucketPolicyStatus.yaml"

requestSelectObjectContent :: SelectObjectContent -> TestTree
requestSelectObjectContent = req
    "SelectObjectContent"
    "fixture/SelectObjectContent.yaml"

requestGetBucketReplication :: GetBucketReplication -> TestTree
requestGetBucketReplication = req
    "GetBucketReplication"
    "fixture/GetBucketReplication.yaml"

requestPutBucketWebsite :: PutBucketWebsite -> TestTree
requestPutBucketWebsite = req
    "PutBucketWebsite"
    "fixture/PutBucketWebsite.yaml"

requestDeleteBucketWebsite :: DeleteBucketWebsite -> TestTree
requestDeleteBucketWebsite = req
    "DeleteBucketWebsite"
    "fixture/DeleteBucketWebsite.yaml"

requestCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
requestCompleteMultipartUpload = req
    "CompleteMultipartUpload"
    "fixture/CompleteMultipartUpload.yaml"

requestListMultipartUploads :: ListMultipartUploads -> TestTree
requestListMultipartUploads = req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads.yaml"

requestListObjects :: ListObjects -> TestTree
requestListObjects = req
    "ListObjects"
    "fixture/ListObjects.yaml"

requestGetBucketOwnershipControls :: GetBucketOwnershipControls -> TestTree
requestGetBucketOwnershipControls = req
    "GetBucketOwnershipControls"
    "fixture/GetBucketOwnershipControls.yaml"

requestGetObjectLegalHold :: GetObjectLegalHold -> TestTree
requestGetObjectLegalHold = req
    "GetObjectLegalHold"
    "fixture/GetObjectLegalHold.yaml"

requestGetObjectRetention :: GetObjectRetention -> TestTree
requestGetObjectRetention = req
    "GetObjectRetention"
    "fixture/GetObjectRetention.yaml"

requestDeleteBucketPolicy :: DeleteBucketPolicy -> TestTree
requestDeleteBucketPolicy = req
    "DeleteBucketPolicy"
    "fixture/DeleteBucketPolicy.yaml"

requestGetBucketEncryption :: GetBucketEncryption -> TestTree
requestGetBucketEncryption = req
    "GetBucketEncryption"
    "fixture/GetBucketEncryption.yaml"

requestAbortMultipartUpload :: AbortMultipartUpload -> TestTree
requestAbortMultipartUpload = req
    "AbortMultipartUpload"
    "fixture/AbortMultipartUpload.yaml"

requestPutBucketPolicy :: PutBucketPolicy -> TestTree
requestPutBucketPolicy = req
    "PutBucketPolicy"
    "fixture/PutBucketPolicy.yaml"

requestGetBucketAccelerateConfiguration :: GetBucketAccelerateConfiguration -> TestTree
requestGetBucketAccelerateConfiguration = req
    "GetBucketAccelerateConfiguration"
    "fixture/GetBucketAccelerateConfiguration.yaml"

requestGetObjectTorrent :: GetObjectTorrent -> TestTree
requestGetObjectTorrent = req
    "GetObjectTorrent"
    "fixture/GetObjectTorrent.yaml"

requestDeleteObjects :: DeleteObjects -> TestTree
requestDeleteObjects = req
    "DeleteObjects"
    "fixture/DeleteObjects.yaml"

requestPutObjectLockConfiguration :: PutObjectLockConfiguration -> TestTree
requestPutObjectLockConfiguration = req
    "PutObjectLockConfiguration"
    "fixture/PutObjectLockConfiguration.yaml"

requestPutBucketNotificationConfiguration :: PutBucketNotificationConfiguration -> TestTree
requestPutBucketNotificationConfiguration = req
    "PutBucketNotificationConfiguration"
    "fixture/PutBucketNotificationConfiguration.yaml"

requestGetBucketVersioning :: GetBucketVersioning -> TestTree
requestGetBucketVersioning = req
    "GetBucketVersioning"
    "fixture/GetBucketVersioning.yaml"

requestDeleteBucketCors :: DeleteBucketCors -> TestTree
requestDeleteBucketCors = req
    "DeleteBucketCors"
    "fixture/DeleteBucketCors.yaml"

requestDeleteBucketIntelligentTieringConfiguration :: DeleteBucketIntelligentTieringConfiguration -> TestTree
requestDeleteBucketIntelligentTieringConfiguration = req
    "DeleteBucketIntelligentTieringConfiguration"
    "fixture/DeleteBucketIntelligentTieringConfiguration.yaml"

requestListBucketIntelligentTieringConfigurations :: ListBucketIntelligentTieringConfigurations -> TestTree
requestListBucketIntelligentTieringConfigurations = req
    "ListBucketIntelligentTieringConfigurations"
    "fixture/ListBucketIntelligentTieringConfigurations.yaml"

requestPutBucketCors :: PutBucketCors -> TestTree
requestPutBucketCors = req
    "PutBucketCors"
    "fixture/PutBucketCors.yaml"

requestGetPublicAccessBlock :: GetPublicAccessBlock -> TestTree
requestGetPublicAccessBlock = req
    "GetPublicAccessBlock"
    "fixture/GetPublicAccessBlock.yaml"

requestPutBucketIntelligentTieringConfiguration :: PutBucketIntelligentTieringConfiguration -> TestTree
requestPutBucketIntelligentTieringConfiguration = req
    "PutBucketIntelligentTieringConfiguration"
    "fixture/PutBucketIntelligentTieringConfiguration.yaml"

requestGetBucketCors :: GetBucketCors -> TestTree
requestGetBucketCors = req
    "GetBucketCors"
    "fixture/GetBucketCors.yaml"

requestGetObjectAcl :: GetObjectAcl -> TestTree
requestGetObjectAcl = req
    "GetObjectAcl"
    "fixture/GetObjectAcl.yaml"

requestRestoreObject :: RestoreObject -> TestTree
requestRestoreObject = req
    "RestoreObject"
    "fixture/RestoreObject.yaml"

requestHeadObject :: HeadObject -> TestTree
requestHeadObject = req
    "HeadObject"
    "fixture/HeadObject.yaml"

requestPutBucketVersioning :: PutBucketVersioning -> TestTree
requestPutBucketVersioning = req
    "PutBucketVersioning"
    "fixture/PutBucketVersioning.yaml"

requestGetBucketTagging :: GetBucketTagging -> TestTree
requestGetBucketTagging = req
    "GetBucketTagging"
    "fixture/GetBucketTagging.yaml"

requestCopyObject :: CopyObject -> TestTree
requestCopyObject = req
    "CopyObject"
    "fixture/CopyObject.yaml"

requestListBucketMetricsConfigurations :: ListBucketMetricsConfigurations -> TestTree
requestListBucketMetricsConfigurations = req
    "ListBucketMetricsConfigurations"
    "fixture/ListBucketMetricsConfigurations.yaml"

requestGetBucketPolicy :: GetBucketPolicy -> TestTree
requestGetBucketPolicy = req
    "GetBucketPolicy"
    "fixture/GetBucketPolicy.yaml"

requestPutBucketEncryption :: PutBucketEncryption -> TestTree
requestPutBucketEncryption = req
    "PutBucketEncryption"
    "fixture/PutBucketEncryption.yaml"

requestDeleteBucketEncryption :: DeleteBucketEncryption -> TestTree
requestDeleteBucketEncryption = req
    "DeleteBucketEncryption"
    "fixture/DeleteBucketEncryption.yaml"

requestGetBucketLogging :: GetBucketLogging -> TestTree
requestGetBucketLogging = req
    "GetBucketLogging"
    "fixture/GetBucketLogging.yaml"

requestGetBucketAcl :: GetBucketAcl -> TestTree
requestGetBucketAcl = req
    "GetBucketAcl"
    "fixture/GetBucketAcl.yaml"

requestGetBucketLifecycleConfiguration :: GetBucketLifecycleConfiguration -> TestTree
requestGetBucketLifecycleConfiguration = req
    "GetBucketLifecycleConfiguration"
    "fixture/GetBucketLifecycleConfiguration.yaml"

requestGetBucketAnalyticsConfiguration :: GetBucketAnalyticsConfiguration -> TestTree
requestGetBucketAnalyticsConfiguration = req
    "GetBucketAnalyticsConfiguration"
    "fixture/GetBucketAnalyticsConfiguration.yaml"

requestGetObjectTagging :: GetObjectTagging -> TestTree
requestGetObjectTagging = req
    "GetObjectTagging"
    "fixture/GetObjectTagging.yaml"

requestListParts :: ListParts -> TestTree
requestListParts = req
    "ListParts"
    "fixture/ListParts.yaml"

requestDeleteObjectTagging :: DeleteObjectTagging -> TestTree
requestDeleteObjectTagging = req
    "DeleteObjectTagging"
    "fixture/DeleteObjectTagging.yaml"

requestUploadPartCopy :: UploadPartCopy -> TestTree
requestUploadPartCopy = req
    "UploadPartCopy"
    "fixture/UploadPartCopy.yaml"

requestPutObjectTagging :: PutObjectTagging -> TestTree
requestPutObjectTagging = req
    "PutObjectTagging"
    "fixture/PutObjectTagging.yaml"

requestPutBucketAcl :: PutBucketAcl -> TestTree
requestPutBucketAcl = req
    "PutBucketAcl"
    "fixture/PutBucketAcl.yaml"

-- Responses

responsePutBucketRequestPayment :: PutBucketRequestPaymentResponse -> TestTree
responsePutBucketRequestPayment = res
    "PutBucketRequestPaymentResponse"
    "fixture/PutBucketRequestPaymentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketRequestPayment)

responsePutObject :: PutObjectResponse -> TestTree
responsePutObject = res
    "PutObjectResponse"
    "fixture/PutObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutObject)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject = res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteObject)

responsePutBucketLogging :: PutBucketLoggingResponse -> TestTree
responsePutBucketLogging = res
    "PutBucketLoggingResponse"
    "fixture/PutBucketLoggingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketLogging)

responseGetBucketMetricsConfiguration :: GetBucketMetricsConfigurationResponse -> TestTree
responseGetBucketMetricsConfiguration = res
    "GetBucketMetricsConfigurationResponse"
    "fixture/GetBucketMetricsConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketMetricsConfiguration)

responseListBuckets :: ListBucketsResponse -> TestTree
responseListBuckets = res
    "ListBucketsResponse"
    "fixture/ListBucketsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBuckets)

responseDeleteBucket :: DeleteBucketResponse -> TestTree
responseDeleteBucket = res
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucket)

responseCreateBucket :: CreateBucketResponse -> TestTree
responseCreateBucket = res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateBucket)

responseDeleteBucketTagging :: DeleteBucketTaggingResponse -> TestTree
responseDeleteBucketTagging = res
    "DeleteBucketTaggingResponse"
    "fixture/DeleteBucketTaggingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketTagging)

responsePutObjectAcl :: PutObjectAclResponse -> TestTree
responsePutObjectAcl = res
    "PutObjectAclResponse"
    "fixture/PutObjectAclResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutObjectAcl)

responsePutBucketTagging :: PutBucketTaggingResponse -> TestTree
responsePutBucketTagging = res
    "PutBucketTaggingResponse"
    "fixture/PutBucketTaggingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketTagging)

responseGetBucketInventoryConfiguration :: GetBucketInventoryConfigurationResponse -> TestTree
responseGetBucketInventoryConfiguration = res
    "GetBucketInventoryConfigurationResponse"
    "fixture/GetBucketInventoryConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketInventoryConfiguration)

responseDeletePublicAccessBlock :: DeletePublicAccessBlockResponse -> TestTree
responseDeletePublicAccessBlock = res
    "DeletePublicAccessBlockResponse"
    "fixture/DeletePublicAccessBlockResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePublicAccessBlock)

responsePutBucketInventoryConfiguration :: PutBucketInventoryConfigurationResponse -> TestTree
responsePutBucketInventoryConfiguration = res
    "PutBucketInventoryConfigurationResponse"
    "fixture/PutBucketInventoryConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketInventoryConfiguration)

responseGetBucketLocation :: GetBucketLocationResponse -> TestTree
responseGetBucketLocation = res
    "GetBucketLocationResponse"
    "fixture/GetBucketLocationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketLocation)

responseListBucketInventoryConfigurations :: ListBucketInventoryConfigurationsResponse -> TestTree
responseListBucketInventoryConfigurations = res
    "ListBucketInventoryConfigurationsResponse"
    "fixture/ListBucketInventoryConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBucketInventoryConfigurations)

responsePutPublicAccessBlock :: PutPublicAccessBlockResponse -> TestTree
responsePutPublicAccessBlock = res
    "PutPublicAccessBlockResponse"
    "fixture/PutPublicAccessBlockResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutPublicAccessBlock)

responseDeleteBucketInventoryConfiguration :: DeleteBucketInventoryConfigurationResponse -> TestTree
responseDeleteBucketInventoryConfiguration = res
    "DeleteBucketInventoryConfigurationResponse"
    "fixture/DeleteBucketInventoryConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketInventoryConfiguration)

responseGetBucketIntelligentTieringConfiguration :: GetBucketIntelligentTieringConfigurationResponse -> TestTree
responseGetBucketIntelligentTieringConfiguration = res
    "GetBucketIntelligentTieringConfigurationResponse"
    "fixture/GetBucketIntelligentTieringConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketIntelligentTieringConfiguration)

responseGetBucketNotificationConfiguration :: NotificationConfiguration -> TestTree
responseGetBucketNotificationConfiguration = res
    "GetBucketNotificationConfigurationResponse"
    "fixture/GetBucketNotificationConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketNotificationConfiguration)

responseGetObjectLockConfiguration :: GetObjectLockConfigurationResponse -> TestTree
responseGetObjectLockConfiguration = res
    "GetObjectLockConfigurationResponse"
    "fixture/GetObjectLockConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetObjectLockConfiguration)

responsePutObjectRetention :: PutObjectRetentionResponse -> TestTree
responsePutObjectRetention = res
    "PutObjectRetentionResponse"
    "fixture/PutObjectRetentionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutObjectRetention)

responsePutBucketAccelerateConfiguration :: PutBucketAccelerateConfigurationResponse -> TestTree
responsePutBucketAccelerateConfiguration = res
    "PutBucketAccelerateConfigurationResponse"
    "fixture/PutBucketAccelerateConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketAccelerateConfiguration)

responsePutObjectLegalHold :: PutObjectLegalHoldResponse -> TestTree
responsePutObjectLegalHold = res
    "PutObjectLegalHoldResponse"
    "fixture/PutObjectLegalHoldResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutObjectLegalHold)

responsePutBucketOwnershipControls :: PutBucketOwnershipControlsResponse -> TestTree
responsePutBucketOwnershipControls = res
    "PutBucketOwnershipControlsResponse"
    "fixture/PutBucketOwnershipControlsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketOwnershipControls)

responseDeleteBucketOwnershipControls :: DeleteBucketOwnershipControlsResponse -> TestTree
responseDeleteBucketOwnershipControls = res
    "DeleteBucketOwnershipControlsResponse"
    "fixture/DeleteBucketOwnershipControlsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketOwnershipControls)

responsePutBucketMetricsConfiguration :: PutBucketMetricsConfigurationResponse -> TestTree
responsePutBucketMetricsConfiguration = res
    "PutBucketMetricsConfigurationResponse"
    "fixture/PutBucketMetricsConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketMetricsConfiguration)

responseDeleteBucketMetricsConfiguration :: DeleteBucketMetricsConfigurationResponse -> TestTree
responseDeleteBucketMetricsConfiguration = res
    "DeleteBucketMetricsConfigurationResponse"
    "fixture/DeleteBucketMetricsConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketMetricsConfiguration)

responseListObjectsV2 :: ListObjectsV2Response -> TestTree
responseListObjectsV2 = res
    "ListObjectsV2Response"
    "fixture/ListObjectsV2Response.proto"
    mkServiceConfig
    (Proxy :: Proxy ListObjectsV2)

responsePutBucketReplication :: PutBucketReplicationResponse -> TestTree
responsePutBucketReplication = res
    "PutBucketReplicationResponse"
    "fixture/PutBucketReplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketReplication)

responseGetBucketWebsite :: GetBucketWebsiteResponse -> TestTree
responseGetBucketWebsite = res
    "GetBucketWebsiteResponse"
    "fixture/GetBucketWebsiteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketWebsite)

responseGetBucketRequestPayment :: GetBucketRequestPaymentResponse -> TestTree
responseGetBucketRequestPayment = res
    "GetBucketRequestPaymentResponse"
    "fixture/GetBucketRequestPaymentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketRequestPayment)

responseDeleteBucketReplication :: DeleteBucketReplicationResponse -> TestTree
responseDeleteBucketReplication = res
    "DeleteBucketReplicationResponse"
    "fixture/DeleteBucketReplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketReplication)

responseListObjectVersions :: ListObjectVersionsResponse -> TestTree
responseListObjectVersions = res
    "ListObjectVersionsResponse"
    "fixture/ListObjectVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListObjectVersions)

responseHeadBucket :: HeadBucketResponse -> TestTree
responseHeadBucket = res
    "HeadBucketResponse"
    "fixture/HeadBucketResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy HeadBucket)

responseDeleteBucketLifecycle :: DeleteBucketLifecycleResponse -> TestTree
responseDeleteBucketLifecycle = res
    "DeleteBucketLifecycleResponse"
    "fixture/DeleteBucketLifecycleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketLifecycle)

responsePutBucketLifecycleConfiguration :: PutBucketLifecycleConfigurationResponse -> TestTree
responsePutBucketLifecycleConfiguration = res
    "PutBucketLifecycleConfigurationResponse"
    "fixture/PutBucketLifecycleConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketLifecycleConfiguration)

responsePutBucketAnalyticsConfiguration :: PutBucketAnalyticsConfigurationResponse -> TestTree
responsePutBucketAnalyticsConfiguration = res
    "PutBucketAnalyticsConfigurationResponse"
    "fixture/PutBucketAnalyticsConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketAnalyticsConfiguration)

responseListBucketAnalyticsConfigurations :: ListBucketAnalyticsConfigurationsResponse -> TestTree
responseListBucketAnalyticsConfigurations = res
    "ListBucketAnalyticsConfigurationsResponse"
    "fixture/ListBucketAnalyticsConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBucketAnalyticsConfigurations)

responseDeleteBucketAnalyticsConfiguration :: DeleteBucketAnalyticsConfigurationResponse -> TestTree
responseDeleteBucketAnalyticsConfiguration = res
    "DeleteBucketAnalyticsConfigurationResponse"
    "fixture/DeleteBucketAnalyticsConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketAnalyticsConfiguration)

responseCreateMultipartUpload :: CreateMultipartUploadResponse -> TestTree
responseCreateMultipartUpload = res
    "CreateMultipartUploadResponse"
    "fixture/CreateMultipartUploadResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateMultipartUpload)

responseGetBucketPolicyStatus :: GetBucketPolicyStatusResponse -> TestTree
responseGetBucketPolicyStatus = res
    "GetBucketPolicyStatusResponse"
    "fixture/GetBucketPolicyStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketPolicyStatus)

responseUploadPart :: UploadPartResponse -> TestTree
responseUploadPart = res
    "UploadPartResponse"
    "fixture/UploadPartResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UploadPart)

responseSelectObjectContent :: SelectObjectContentResponse -> TestTree
responseSelectObjectContent = res
    "SelectObjectContentResponse"
    "fixture/SelectObjectContentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SelectObjectContent)

responseGetBucketReplication :: GetBucketReplicationResponse -> TestTree
responseGetBucketReplication = res
    "GetBucketReplicationResponse"
    "fixture/GetBucketReplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketReplication)

responsePutBucketWebsite :: PutBucketWebsiteResponse -> TestTree
responsePutBucketWebsite = res
    "PutBucketWebsiteResponse"
    "fixture/PutBucketWebsiteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketWebsite)

responseDeleteBucketWebsite :: DeleteBucketWebsiteResponse -> TestTree
responseDeleteBucketWebsite = res
    "DeleteBucketWebsiteResponse"
    "fixture/DeleteBucketWebsiteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketWebsite)

responseCompleteMultipartUpload :: CompleteMultipartUploadResponse -> TestTree
responseCompleteMultipartUpload = res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CompleteMultipartUpload)

responseListMultipartUploads :: ListMultipartUploadsResponse -> TestTree
responseListMultipartUploads = res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListMultipartUploads)

responseListObjects :: ListObjectsResponse -> TestTree
responseListObjects = res
    "ListObjectsResponse"
    "fixture/ListObjectsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListObjects)

responseGetBucketOwnershipControls :: GetBucketOwnershipControlsResponse -> TestTree
responseGetBucketOwnershipControls = res
    "GetBucketOwnershipControlsResponse"
    "fixture/GetBucketOwnershipControlsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketOwnershipControls)

responseGetObjectLegalHold :: GetObjectLegalHoldResponse -> TestTree
responseGetObjectLegalHold = res
    "GetObjectLegalHoldResponse"
    "fixture/GetObjectLegalHoldResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetObjectLegalHold)

responseGetObjectRetention :: GetObjectRetentionResponse -> TestTree
responseGetObjectRetention = res
    "GetObjectRetentionResponse"
    "fixture/GetObjectRetentionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetObjectRetention)

responseDeleteBucketPolicy :: DeleteBucketPolicyResponse -> TestTree
responseDeleteBucketPolicy = res
    "DeleteBucketPolicyResponse"
    "fixture/DeleteBucketPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketPolicy)

responseGetBucketEncryption :: GetBucketEncryptionResponse -> TestTree
responseGetBucketEncryption = res
    "GetBucketEncryptionResponse"
    "fixture/GetBucketEncryptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketEncryption)

responseAbortMultipartUpload :: AbortMultipartUploadResponse -> TestTree
responseAbortMultipartUpload = res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AbortMultipartUpload)

responsePutBucketPolicy :: PutBucketPolicyResponse -> TestTree
responsePutBucketPolicy = res
    "PutBucketPolicyResponse"
    "fixture/PutBucketPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketPolicy)

responseGetBucketAccelerateConfiguration :: GetBucketAccelerateConfigurationResponse -> TestTree
responseGetBucketAccelerateConfiguration = res
    "GetBucketAccelerateConfigurationResponse"
    "fixture/GetBucketAccelerateConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketAccelerateConfiguration)

responseDeleteObjects :: DeleteObjectsResponse -> TestTree
responseDeleteObjects = res
    "DeleteObjectsResponse"
    "fixture/DeleteObjectsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteObjects)

responsePutObjectLockConfiguration :: PutObjectLockConfigurationResponse -> TestTree
responsePutObjectLockConfiguration = res
    "PutObjectLockConfigurationResponse"
    "fixture/PutObjectLockConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutObjectLockConfiguration)

responsePutBucketNotificationConfiguration :: PutBucketNotificationConfigurationResponse -> TestTree
responsePutBucketNotificationConfiguration = res
    "PutBucketNotificationConfigurationResponse"
    "fixture/PutBucketNotificationConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketNotificationConfiguration)

responseGetBucketVersioning :: GetBucketVersioningResponse -> TestTree
responseGetBucketVersioning = res
    "GetBucketVersioningResponse"
    "fixture/GetBucketVersioningResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketVersioning)

responseDeleteBucketCors :: DeleteBucketCorsResponse -> TestTree
responseDeleteBucketCors = res
    "DeleteBucketCorsResponse"
    "fixture/DeleteBucketCorsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketCors)

responseDeleteBucketIntelligentTieringConfiguration :: DeleteBucketIntelligentTieringConfigurationResponse -> TestTree
responseDeleteBucketIntelligentTieringConfiguration = res
    "DeleteBucketIntelligentTieringConfigurationResponse"
    "fixture/DeleteBucketIntelligentTieringConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketIntelligentTieringConfiguration)

responseListBucketIntelligentTieringConfigurations :: ListBucketIntelligentTieringConfigurationsResponse -> TestTree
responseListBucketIntelligentTieringConfigurations = res
    "ListBucketIntelligentTieringConfigurationsResponse"
    "fixture/ListBucketIntelligentTieringConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBucketIntelligentTieringConfigurations)

responsePutBucketCors :: PutBucketCorsResponse -> TestTree
responsePutBucketCors = res
    "PutBucketCorsResponse"
    "fixture/PutBucketCorsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketCors)

responseGetPublicAccessBlock :: GetPublicAccessBlockResponse -> TestTree
responseGetPublicAccessBlock = res
    "GetPublicAccessBlockResponse"
    "fixture/GetPublicAccessBlockResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPublicAccessBlock)

responsePutBucketIntelligentTieringConfiguration :: PutBucketIntelligentTieringConfigurationResponse -> TestTree
responsePutBucketIntelligentTieringConfiguration = res
    "PutBucketIntelligentTieringConfigurationResponse"
    "fixture/PutBucketIntelligentTieringConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketIntelligentTieringConfiguration)

responseGetBucketCors :: GetBucketCorsResponse -> TestTree
responseGetBucketCors = res
    "GetBucketCorsResponse"
    "fixture/GetBucketCorsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketCors)

responseGetObjectAcl :: GetObjectAclResponse -> TestTree
responseGetObjectAcl = res
    "GetObjectAclResponse"
    "fixture/GetObjectAclResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetObjectAcl)

responseRestoreObject :: RestoreObjectResponse -> TestTree
responseRestoreObject = res
    "RestoreObjectResponse"
    "fixture/RestoreObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RestoreObject)

responseHeadObject :: HeadObjectResponse -> TestTree
responseHeadObject = res
    "HeadObjectResponse"
    "fixture/HeadObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy HeadObject)

responsePutBucketVersioning :: PutBucketVersioningResponse -> TestTree
responsePutBucketVersioning = res
    "PutBucketVersioningResponse"
    "fixture/PutBucketVersioningResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketVersioning)

responseGetBucketTagging :: GetBucketTaggingResponse -> TestTree
responseGetBucketTagging = res
    "GetBucketTaggingResponse"
    "fixture/GetBucketTaggingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketTagging)

responseCopyObject :: CopyObjectResponse -> TestTree
responseCopyObject = res
    "CopyObjectResponse"
    "fixture/CopyObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CopyObject)

responseListBucketMetricsConfigurations :: ListBucketMetricsConfigurationsResponse -> TestTree
responseListBucketMetricsConfigurations = res
    "ListBucketMetricsConfigurationsResponse"
    "fixture/ListBucketMetricsConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBucketMetricsConfigurations)

responseGetBucketPolicy :: GetBucketPolicyResponse -> TestTree
responseGetBucketPolicy = res
    "GetBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketPolicy)

responsePutBucketEncryption :: PutBucketEncryptionResponse -> TestTree
responsePutBucketEncryption = res
    "PutBucketEncryptionResponse"
    "fixture/PutBucketEncryptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketEncryption)

responseDeleteBucketEncryption :: DeleteBucketEncryptionResponse -> TestTree
responseDeleteBucketEncryption = res
    "DeleteBucketEncryptionResponse"
    "fixture/DeleteBucketEncryptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBucketEncryption)

responseGetBucketLogging :: GetBucketLoggingResponse -> TestTree
responseGetBucketLogging = res
    "GetBucketLoggingResponse"
    "fixture/GetBucketLoggingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketLogging)

responseGetBucketAcl :: GetBucketAclResponse -> TestTree
responseGetBucketAcl = res
    "GetBucketAclResponse"
    "fixture/GetBucketAclResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketAcl)

responseGetBucketLifecycleConfiguration :: GetBucketLifecycleConfigurationResponse -> TestTree
responseGetBucketLifecycleConfiguration = res
    "GetBucketLifecycleConfigurationResponse"
    "fixture/GetBucketLifecycleConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketLifecycleConfiguration)

responseGetBucketAnalyticsConfiguration :: GetBucketAnalyticsConfigurationResponse -> TestTree
responseGetBucketAnalyticsConfiguration = res
    "GetBucketAnalyticsConfigurationResponse"
    "fixture/GetBucketAnalyticsConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBucketAnalyticsConfiguration)

responseGetObjectTagging :: GetObjectTaggingResponse -> TestTree
responseGetObjectTagging = res
    "GetObjectTaggingResponse"
    "fixture/GetObjectTaggingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetObjectTagging)

responseListParts :: ListPartsResponse -> TestTree
responseListParts = res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListParts)

responseDeleteObjectTagging :: DeleteObjectTaggingResponse -> TestTree
responseDeleteObjectTagging = res
    "DeleteObjectTaggingResponse"
    "fixture/DeleteObjectTaggingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteObjectTagging)

responseUploadPartCopy :: UploadPartCopyResponse -> TestTree
responseUploadPartCopy = res
    "UploadPartCopyResponse"
    "fixture/UploadPartCopyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UploadPartCopy)

responsePutObjectTagging :: PutObjectTaggingResponse -> TestTree
responsePutObjectTagging = res
    "PutObjectTaggingResponse"
    "fixture/PutObjectTaggingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutObjectTagging)

responsePutBucketAcl :: PutBucketAclResponse -> TestTree
responsePutBucketAcl = res
    "PutBucketAclResponse"
    "fixture/PutBucketAclResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBucketAcl)
