{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.S3
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
--         [ testPutBucketRequestPayment $
--             putBucketRequestPayment
--
--         , testPutObject $
--             putObject
--
--         , testDeleteObject $
--             deleteObject
--
--         , testPutBucketLogging $
--             putBucketLogging
--
--         , testListBuckets $
--             listBuckets
--
--         , testDeleteBucket $
--             deleteBucket
--
--         , testCreateBucket $
--             createBucket
--
--         , testDeleteBucketTagging $
--             deleteBucketTagging
--
--         , testPutObjectACL $
--             putObjectACL
--
--         , testPutBucketTagging $
--             putBucketTagging
--
--         , testGetBucketLocation $
--             getBucketLocation
--
--         , testGetBucketNotificationConfiguration $
--             getBucketNotificationConfiguration
--
--         , testGetObject $
--             getObject
--
--         , testPutBucketReplication $
--             putBucketReplication
--
--         , testGetBucketWebsite $
--             getBucketWebsite
--
--         , testGetBucketRequestPayment $
--             getBucketRequestPayment
--
--         , testDeleteBucketReplication $
--             deleteBucketReplication
--
--         , testGetBucketLifecycle $
--             getBucketLifecycle
--
--         , testListObjectVersions $
--             listObjectVersions
--
--         , testHeadBucket $
--             headBucket
--
--         , testPutBucketLifecycle $
--             putBucketLifecycle
--
--         , testDeleteBucketLifecycle $
--             deleteBucketLifecycle
--
--         , testCreateMultipartUpload $
--             createMultipartUpload
--
--         , testUploadPart $
--             uploadPart
--
--         , testGetBucketReplication $
--             getBucketReplication
--
--         , testPutBucketWebsite $
--             putBucketWebsite
--
--         , testDeleteBucketWebsite $
--             deleteBucketWebsite
--
--         , testCompleteMultipartUpload $
--             completeMultipartUpload
--
--         , testListMultipartUploads $
--             listMultipartUploads
--
--         , testListObjects $
--             listObjects
--
--         , testDeleteBucketPolicy $
--             deleteBucketPolicy
--
--         , testAbortMultipartUpload $
--             abortMultipartUpload
--
--         , testPutBucketPolicy $
--             putBucketPolicy
--
--         , testGetObjectTorrent $
--             getObjectTorrent
--
--         , testDeleteObjects $
--             deleteObjects
--
--         , testPutBucketNotificationConfiguration $
--             putBucketNotificationConfiguration
--
--         , testGetBucketVersioning $
--             getBucketVersioning
--
--         , testDeleteBucketCORS $
--             deleteBucketCORS
--
--         , testPutBucketCORS $
--             putBucketCORS
--
--         , testGetBucketCORS $
--             getBucketCORS
--
--         , testGetObjectACL $
--             getObjectACL
--
--         , testRestoreObject $
--             restoreObject
--
--         , testHeadObject $
--             headObject
--
--         , testPutBucketVersioning $
--             putBucketVersioning
--
--         , testGetBucketTagging $
--             getBucketTagging
--
--         , testCopyObject $
--             copyObject
--
--         , testGetBucketPolicy $
--             getBucketPolicy
--
--         , testGetBucketLogging $
--             getBucketLogging
--
--         , testGetBucketACL $
--             getBucketACL
--
--         , testListParts $
--             listParts
--
--         , testUploadPartCopy $
--             uploadPartCopy
--
--         , testPutBucketACL $
--             putBucketACL
--
--           ]

--     , testGroup "response"
--         [ testPutBucketRequestPaymentResponse $
--             putBucketRequestPaymentResponse
--
--         , testPutObjectResponse $
--             putObjectResponse
--
--         , testDeleteObjectResponse $
--             deleteObjectResponse
--
--         , testPutBucketLoggingResponse $
--             putBucketLoggingResponse
--
--         , testListBucketsResponse $
--             listBucketsResponse
--
--         , testDeleteBucketResponse $
--             deleteBucketResponse
--
--         , testCreateBucketResponse $
--             createBucketResponse
--
--         , testDeleteBucketTaggingResponse $
--             deleteBucketTaggingResponse
--
--         , testPutObjectACLResponse $
--             putObjectACLResponse
--
--         , testPutBucketTaggingResponse $
--             putBucketTaggingResponse
--
--         , testGetBucketLocationResponse $
--             getBucketLocationResponse
--
--         , testGetBucketNotificationConfigurationResponse $
--             notificationConfiguration
--
--         , testGetObjectResponse $
--             getObjectResponse
--
--         , testPutBucketReplicationResponse $
--             putBucketReplicationResponse
--
--         , testGetBucketWebsiteResponse $
--             getBucketWebsiteResponse
--
--         , testGetBucketRequestPaymentResponse $
--             getBucketRequestPaymentResponse
--
--         , testDeleteBucketReplicationResponse $
--             deleteBucketReplicationResponse
--
--         , testGetBucketLifecycleResponse $
--             getBucketLifecycleResponse
--
--         , testListObjectVersionsResponse $
--             listObjectVersionsResponse
--
--         , testHeadBucketResponse $
--             headBucketResponse
--
--         , testPutBucketLifecycleResponse $
--             putBucketLifecycleResponse
--
--         , testDeleteBucketLifecycleResponse $
--             deleteBucketLifecycleResponse
--
--         , testCreateMultipartUploadResponse $
--             createMultipartUploadResponse
--
--         , testUploadPartResponse $
--             uploadPartResponse
--
--         , testGetBucketReplicationResponse $
--             getBucketReplicationResponse
--
--         , testPutBucketWebsiteResponse $
--             putBucketWebsiteResponse
--
--         , testDeleteBucketWebsiteResponse $
--             deleteBucketWebsiteResponse
--
--         , testCompleteMultipartUploadResponse $
--             completeMultipartUploadResponse
--
--         , testListMultipartUploadsResponse $
--             listMultipartUploadsResponse
--
--         , testListObjectsResponse $
--             listObjectsResponse
--
--         , testDeleteBucketPolicyResponse $
--             deleteBucketPolicyResponse
--
--         , testAbortMultipartUploadResponse $
--             abortMultipartUploadResponse
--
--         , testPutBucketPolicyResponse $
--             putBucketPolicyResponse
--
--         , testGetObjectTorrentResponse $
--             getObjectTorrentResponse
--
--         , testDeleteObjectsResponse $
--             deleteObjectsResponse
--
--         , testPutBucketNotificationConfigurationResponse $
--             putBucketNotificationConfigurationResponse
--
--         , testGetBucketVersioningResponse $
--             getBucketVersioningResponse
--
--         , testDeleteBucketCORSResponse $
--             deleteBucketCORSResponse
--
--         , testPutBucketCORSResponse $
--             putBucketCORSResponse
--
--         , testGetBucketCORSResponse $
--             getBucketCORSResponse
--
--         , testGetObjectACLResponse $
--             getObjectACLResponse
--
--         , testRestoreObjectResponse $
--             restoreObjectResponse
--
--         , testHeadObjectResponse $
--             headObjectResponse
--
--         , testPutBucketVersioningResponse $
--             putBucketVersioningResponse
--
--         , testGetBucketTaggingResponse $
--             getBucketTaggingResponse
--
--         , testCopyObjectResponse $
--             copyObjectResponse
--
--         , testGetBucketPolicyResponse $
--             getBucketPolicyResponse
--
--         , testGetBucketLoggingResponse $
--             getBucketLoggingResponse
--
--         , testGetBucketACLResponse $
--             getBucketACLResponse
--
--         , testListPartsResponse $
--             listPartsResponse
--
--         , testUploadPartCopyResponse $
--             uploadPartCopyResponse
--
--         , testPutBucketACLResponse $
--             putBucketACLResponse
--
--           ]
--     ]

-- Requests

testPutBucketRequestPayment :: PutBucketRequestPayment -> TestTree
testPutBucketRequestPayment = req
    "PutBucketRequestPayment"
    "fixture/PutBucketRequestPayment.yaml"

testDeleteObject :: DeleteObject -> TestTree
testDeleteObject = req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

testPutBucketLogging :: PutBucketLogging -> TestTree
testPutBucketLogging = req
    "PutBucketLogging"
    "fixture/PutBucketLogging.yaml"

testListBuckets :: ListBuckets -> TestTree
testListBuckets = req
    "ListBuckets"
    "fixture/ListBuckets.yaml"

testDeleteBucket :: DeleteBucket -> TestTree
testDeleteBucket = req
    "DeleteBucket"
    "fixture/DeleteBucket.yaml"

testCreateBucket :: CreateBucket -> TestTree
testCreateBucket = req
    "CreateBucket"
    "fixture/CreateBucket.yaml"

testDeleteBucketTagging :: DeleteBucketTagging -> TestTree
testDeleteBucketTagging = req
    "DeleteBucketTagging"
    "fixture/DeleteBucketTagging.yaml"

testPutObjectACL :: PutObjectACL -> TestTree
testPutObjectACL = req
    "PutObjectACL"
    "fixture/PutObjectACL.yaml"

testPutBucketTagging :: PutBucketTagging -> TestTree
testPutBucketTagging = req
    "PutBucketTagging"
    "fixture/PutBucketTagging.yaml"

testGetBucketLocation :: GetBucketLocation -> TestTree
testGetBucketLocation = req
    "GetBucketLocation"
    "fixture/GetBucketLocation.yaml"

testGetBucketNotificationConfiguration :: GetBucketNotificationConfiguration -> TestTree
testGetBucketNotificationConfiguration = req
    "GetBucketNotificationConfiguration"
    "fixture/GetBucketNotificationConfiguration.yaml"

testGetObject :: GetObject -> TestTree
testGetObject = req
    "GetObject"
    "fixture/GetObject.yaml"

testPutBucketReplication :: PutBucketReplication -> TestTree
testPutBucketReplication = req
    "PutBucketReplication"
    "fixture/PutBucketReplication.yaml"

testGetBucketWebsite :: GetBucketWebsite -> TestTree
testGetBucketWebsite = req
    "GetBucketWebsite"
    "fixture/GetBucketWebsite.yaml"

testGetBucketRequestPayment :: GetBucketRequestPayment -> TestTree
testGetBucketRequestPayment = req
    "GetBucketRequestPayment"
    "fixture/GetBucketRequestPayment.yaml"

testDeleteBucketReplication :: DeleteBucketReplication -> TestTree
testDeleteBucketReplication = req
    "DeleteBucketReplication"
    "fixture/DeleteBucketReplication.yaml"

testGetBucketLifecycle :: GetBucketLifecycle -> TestTree
testGetBucketLifecycle = req
    "GetBucketLifecycle"
    "fixture/GetBucketLifecycle.yaml"

testListObjectVersions :: ListObjectVersions -> TestTree
testListObjectVersions = req
    "ListObjectVersions"
    "fixture/ListObjectVersions.yaml"

testHeadBucket :: HeadBucket -> TestTree
testHeadBucket = req
    "HeadBucket"
    "fixture/HeadBucket.yaml"

testPutBucketLifecycle :: PutBucketLifecycle -> TestTree
testPutBucketLifecycle = req
    "PutBucketLifecycle"
    "fixture/PutBucketLifecycle.yaml"

testDeleteBucketLifecycle :: DeleteBucketLifecycle -> TestTree
testDeleteBucketLifecycle = req
    "DeleteBucketLifecycle"
    "fixture/DeleteBucketLifecycle.yaml"

testCreateMultipartUpload :: CreateMultipartUpload -> TestTree
testCreateMultipartUpload = req
    "CreateMultipartUpload"
    "fixture/CreateMultipartUpload.yaml"

testGetBucketReplication :: GetBucketReplication -> TestTree
testGetBucketReplication = req
    "GetBucketReplication"
    "fixture/GetBucketReplication.yaml"

testPutBucketWebsite :: PutBucketWebsite -> TestTree
testPutBucketWebsite = req
    "PutBucketWebsite"
    "fixture/PutBucketWebsite.yaml"

testDeleteBucketWebsite :: DeleteBucketWebsite -> TestTree
testDeleteBucketWebsite = req
    "DeleteBucketWebsite"
    "fixture/DeleteBucketWebsite.yaml"

testCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
testCompleteMultipartUpload = req
    "CompleteMultipartUpload"
    "fixture/CompleteMultipartUpload.yaml"

testListMultipartUploads :: ListMultipartUploads -> TestTree
testListMultipartUploads = req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads.yaml"

testListObjects :: ListObjects -> TestTree
testListObjects = req
    "ListObjects"
    "fixture/ListObjects.yaml"

testDeleteBucketPolicy :: DeleteBucketPolicy -> TestTree
testDeleteBucketPolicy = req
    "DeleteBucketPolicy"
    "fixture/DeleteBucketPolicy.yaml"

testAbortMultipartUpload :: AbortMultipartUpload -> TestTree
testAbortMultipartUpload = req
    "AbortMultipartUpload"
    "fixture/AbortMultipartUpload.yaml"

testPutBucketPolicy :: PutBucketPolicy -> TestTree
testPutBucketPolicy = req
    "PutBucketPolicy"
    "fixture/PutBucketPolicy.yaml"

testGetObjectTorrent :: GetObjectTorrent -> TestTree
testGetObjectTorrent = req
    "GetObjectTorrent"
    "fixture/GetObjectTorrent.yaml"

testDeleteObjects :: DeleteObjects -> TestTree
testDeleteObjects = req
    "DeleteObjects"
    "fixture/DeleteObjects.yaml"

testPutBucketNotificationConfiguration :: PutBucketNotificationConfiguration -> TestTree
testPutBucketNotificationConfiguration = req
    "PutBucketNotificationConfiguration"
    "fixture/PutBucketNotificationConfiguration.yaml"

testGetBucketVersioning :: GetBucketVersioning -> TestTree
testGetBucketVersioning = req
    "GetBucketVersioning"
    "fixture/GetBucketVersioning.yaml"

testDeleteBucketCORS :: DeleteBucketCORS -> TestTree
testDeleteBucketCORS = req
    "DeleteBucketCORS"
    "fixture/DeleteBucketCORS.yaml"

testPutBucketCORS :: PutBucketCORS -> TestTree
testPutBucketCORS = req
    "PutBucketCORS"
    "fixture/PutBucketCORS.yaml"

testGetBucketCORS :: GetBucketCORS -> TestTree
testGetBucketCORS = req
    "GetBucketCORS"
    "fixture/GetBucketCORS.yaml"

testGetObjectACL :: GetObjectACL -> TestTree
testGetObjectACL = req
    "GetObjectACL"
    "fixture/GetObjectACL.yaml"

testRestoreObject :: RestoreObject -> TestTree
testRestoreObject = req
    "RestoreObject"
    "fixture/RestoreObject.yaml"

testHeadObject :: HeadObject -> TestTree
testHeadObject = req
    "HeadObject"
    "fixture/HeadObject.yaml"

testPutBucketVersioning :: PutBucketVersioning -> TestTree
testPutBucketVersioning = req
    "PutBucketVersioning"
    "fixture/PutBucketVersioning.yaml"

testGetBucketTagging :: GetBucketTagging -> TestTree
testGetBucketTagging = req
    "GetBucketTagging"
    "fixture/GetBucketTagging.yaml"

testCopyObject :: CopyObject -> TestTree
testCopyObject = req
    "CopyObject"
    "fixture/CopyObject.yaml"

testGetBucketPolicy :: GetBucketPolicy -> TestTree
testGetBucketPolicy = req
    "GetBucketPolicy"
    "fixture/GetBucketPolicy.yaml"

testGetBucketLogging :: GetBucketLogging -> TestTree
testGetBucketLogging = req
    "GetBucketLogging"
    "fixture/GetBucketLogging.yaml"

testGetBucketACL :: GetBucketACL -> TestTree
testGetBucketACL = req
    "GetBucketACL"
    "fixture/GetBucketACL.yaml"

testListParts :: ListParts -> TestTree
testListParts = req
    "ListParts"
    "fixture/ListParts.yaml"

testUploadPartCopy :: UploadPartCopy -> TestTree
testUploadPartCopy = req
    "UploadPartCopy"
    "fixture/UploadPartCopy.yaml"

testPutBucketACL :: PutBucketACL -> TestTree
testPutBucketACL = req
    "PutBucketACL"
    "fixture/PutBucketACL.yaml"

-- Responses

testPutBucketRequestPaymentResponse :: PutBucketRequestPaymentResponse -> TestTree
testPutBucketRequestPaymentResponse = res
    "PutBucketRequestPaymentResponse"
    "fixture/PutBucketRequestPaymentResponse.proto"
    s3
    (Proxy :: Proxy PutBucketRequestPayment)

testPutObjectResponse :: PutObjectResponse -> TestTree
testPutObjectResponse = res
    "PutObjectResponse"
    "fixture/PutObjectResponse.proto"
    s3
    (Proxy :: Proxy PutObject)

testDeleteObjectResponse :: DeleteObjectResponse -> TestTree
testDeleteObjectResponse = res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    s3
    (Proxy :: Proxy DeleteObject)

testPutBucketLoggingResponse :: PutBucketLoggingResponse -> TestTree
testPutBucketLoggingResponse = res
    "PutBucketLoggingResponse"
    "fixture/PutBucketLoggingResponse.proto"
    s3
    (Proxy :: Proxy PutBucketLogging)

testListBucketsResponse :: ListBucketsResponse -> TestTree
testListBucketsResponse = res
    "ListBucketsResponse"
    "fixture/ListBucketsResponse.proto"
    s3
    (Proxy :: Proxy ListBuckets)

testDeleteBucketResponse :: DeleteBucketResponse -> TestTree
testDeleteBucketResponse = res
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucket)

testCreateBucketResponse :: CreateBucketResponse -> TestTree
testCreateBucketResponse = res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse.proto"
    s3
    (Proxy :: Proxy CreateBucket)

testDeleteBucketTaggingResponse :: DeleteBucketTaggingResponse -> TestTree
testDeleteBucketTaggingResponse = res
    "DeleteBucketTaggingResponse"
    "fixture/DeleteBucketTaggingResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketTagging)

testPutObjectACLResponse :: PutObjectACLResponse -> TestTree
testPutObjectACLResponse = res
    "PutObjectACLResponse"
    "fixture/PutObjectACLResponse.proto"
    s3
    (Proxy :: Proxy PutObjectACL)

testPutBucketTaggingResponse :: PutBucketTaggingResponse -> TestTree
testPutBucketTaggingResponse = res
    "PutBucketTaggingResponse"
    "fixture/PutBucketTaggingResponse.proto"
    s3
    (Proxy :: Proxy PutBucketTagging)

testGetBucketLocationResponse :: GetBucketLocationResponse -> TestTree
testGetBucketLocationResponse = res
    "GetBucketLocationResponse"
    "fixture/GetBucketLocationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketLocation)

testGetBucketNotificationConfigurationResponse :: NotificationConfiguration -> TestTree
testGetBucketNotificationConfigurationResponse = res
    "GetBucketNotificationConfigurationResponse"
    "fixture/GetBucketNotificationConfigurationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketNotificationConfiguration)

testPutBucketReplicationResponse :: PutBucketReplicationResponse -> TestTree
testPutBucketReplicationResponse = res
    "PutBucketReplicationResponse"
    "fixture/PutBucketReplicationResponse.proto"
    s3
    (Proxy :: Proxy PutBucketReplication)

testGetBucketWebsiteResponse :: GetBucketWebsiteResponse -> TestTree
testGetBucketWebsiteResponse = res
    "GetBucketWebsiteResponse"
    "fixture/GetBucketWebsiteResponse.proto"
    s3
    (Proxy :: Proxy GetBucketWebsite)

testGetBucketRequestPaymentResponse :: GetBucketRequestPaymentResponse -> TestTree
testGetBucketRequestPaymentResponse = res
    "GetBucketRequestPaymentResponse"
    "fixture/GetBucketRequestPaymentResponse.proto"
    s3
    (Proxy :: Proxy GetBucketRequestPayment)

testDeleteBucketReplicationResponse :: DeleteBucketReplicationResponse -> TestTree
testDeleteBucketReplicationResponse = res
    "DeleteBucketReplicationResponse"
    "fixture/DeleteBucketReplicationResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketReplication)

testGetBucketLifecycleResponse :: GetBucketLifecycleResponse -> TestTree
testGetBucketLifecycleResponse = res
    "GetBucketLifecycleResponse"
    "fixture/GetBucketLifecycleResponse.proto"
    s3
    (Proxy :: Proxy GetBucketLifecycle)

testListObjectVersionsResponse :: ListObjectVersionsResponse -> TestTree
testListObjectVersionsResponse = res
    "ListObjectVersionsResponse"
    "fixture/ListObjectVersionsResponse.proto"
    s3
    (Proxy :: Proxy ListObjectVersions)

testHeadBucketResponse :: HeadBucketResponse -> TestTree
testHeadBucketResponse = res
    "HeadBucketResponse"
    "fixture/HeadBucketResponse.proto"
    s3
    (Proxy :: Proxy HeadBucket)

testPutBucketLifecycleResponse :: PutBucketLifecycleResponse -> TestTree
testPutBucketLifecycleResponse = res
    "PutBucketLifecycleResponse"
    "fixture/PutBucketLifecycleResponse.proto"
    s3
    (Proxy :: Proxy PutBucketLifecycle)

testDeleteBucketLifecycleResponse :: DeleteBucketLifecycleResponse -> TestTree
testDeleteBucketLifecycleResponse = res
    "DeleteBucketLifecycleResponse"
    "fixture/DeleteBucketLifecycleResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketLifecycle)

testCreateMultipartUploadResponse :: CreateMultipartUploadResponse -> TestTree
testCreateMultipartUploadResponse = res
    "CreateMultipartUploadResponse"
    "fixture/CreateMultipartUploadResponse.proto"
    s3
    (Proxy :: Proxy CreateMultipartUpload)

testUploadPartResponse :: UploadPartResponse -> TestTree
testUploadPartResponse = res
    "UploadPartResponse"
    "fixture/UploadPartResponse.proto"
    s3
    (Proxy :: Proxy UploadPart)

testGetBucketReplicationResponse :: GetBucketReplicationResponse -> TestTree
testGetBucketReplicationResponse = res
    "GetBucketReplicationResponse"
    "fixture/GetBucketReplicationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketReplication)

testPutBucketWebsiteResponse :: PutBucketWebsiteResponse -> TestTree
testPutBucketWebsiteResponse = res
    "PutBucketWebsiteResponse"
    "fixture/PutBucketWebsiteResponse.proto"
    s3
    (Proxy :: Proxy PutBucketWebsite)

testDeleteBucketWebsiteResponse :: DeleteBucketWebsiteResponse -> TestTree
testDeleteBucketWebsiteResponse = res
    "DeleteBucketWebsiteResponse"
    "fixture/DeleteBucketWebsiteResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketWebsite)

testCompleteMultipartUploadResponse :: CompleteMultipartUploadResponse -> TestTree
testCompleteMultipartUploadResponse = res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    s3
    (Proxy :: Proxy CompleteMultipartUpload)

testListMultipartUploadsResponse :: ListMultipartUploadsResponse -> TestTree
testListMultipartUploadsResponse = res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    s3
    (Proxy :: Proxy ListMultipartUploads)

testListObjectsResponse :: ListObjectsResponse -> TestTree
testListObjectsResponse = res
    "ListObjectsResponse"
    "fixture/ListObjectsResponse.proto"
    s3
    (Proxy :: Proxy ListObjects)

testDeleteBucketPolicyResponse :: DeleteBucketPolicyResponse -> TestTree
testDeleteBucketPolicyResponse = res
    "DeleteBucketPolicyResponse"
    "fixture/DeleteBucketPolicyResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketPolicy)

testAbortMultipartUploadResponse :: AbortMultipartUploadResponse -> TestTree
testAbortMultipartUploadResponse = res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse.proto"
    s3
    (Proxy :: Proxy AbortMultipartUpload)

testPutBucketPolicyResponse :: PutBucketPolicyResponse -> TestTree
testPutBucketPolicyResponse = res
    "PutBucketPolicyResponse"
    "fixture/PutBucketPolicyResponse.proto"
    s3
    (Proxy :: Proxy PutBucketPolicy)

testDeleteObjectsResponse :: DeleteObjectsResponse -> TestTree
testDeleteObjectsResponse = res
    "DeleteObjectsResponse"
    "fixture/DeleteObjectsResponse.proto"
    s3
    (Proxy :: Proxy DeleteObjects)

testPutBucketNotificationConfigurationResponse :: PutBucketNotificationConfigurationResponse -> TestTree
testPutBucketNotificationConfigurationResponse = res
    "PutBucketNotificationConfigurationResponse"
    "fixture/PutBucketNotificationConfigurationResponse.proto"
    s3
    (Proxy :: Proxy PutBucketNotificationConfiguration)

testGetBucketVersioningResponse :: GetBucketVersioningResponse -> TestTree
testGetBucketVersioningResponse = res
    "GetBucketVersioningResponse"
    "fixture/GetBucketVersioningResponse.proto"
    s3
    (Proxy :: Proxy GetBucketVersioning)

testDeleteBucketCORSResponse :: DeleteBucketCORSResponse -> TestTree
testDeleteBucketCORSResponse = res
    "DeleteBucketCORSResponse"
    "fixture/DeleteBucketCORSResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketCORS)

testPutBucketCORSResponse :: PutBucketCORSResponse -> TestTree
testPutBucketCORSResponse = res
    "PutBucketCORSResponse"
    "fixture/PutBucketCORSResponse.proto"
    s3
    (Proxy :: Proxy PutBucketCORS)

testGetBucketCORSResponse :: GetBucketCORSResponse -> TestTree
testGetBucketCORSResponse = res
    "GetBucketCORSResponse"
    "fixture/GetBucketCORSResponse.proto"
    s3
    (Proxy :: Proxy GetBucketCORS)

testGetObjectACLResponse :: GetObjectACLResponse -> TestTree
testGetObjectACLResponse = res
    "GetObjectACLResponse"
    "fixture/GetObjectACLResponse.proto"
    s3
    (Proxy :: Proxy GetObjectACL)

testRestoreObjectResponse :: RestoreObjectResponse -> TestTree
testRestoreObjectResponse = res
    "RestoreObjectResponse"
    "fixture/RestoreObjectResponse.proto"
    s3
    (Proxy :: Proxy RestoreObject)

testHeadObjectResponse :: HeadObjectResponse -> TestTree
testHeadObjectResponse = res
    "HeadObjectResponse"
    "fixture/HeadObjectResponse.proto"
    s3
    (Proxy :: Proxy HeadObject)

testPutBucketVersioningResponse :: PutBucketVersioningResponse -> TestTree
testPutBucketVersioningResponse = res
    "PutBucketVersioningResponse"
    "fixture/PutBucketVersioningResponse.proto"
    s3
    (Proxy :: Proxy PutBucketVersioning)

testGetBucketTaggingResponse :: GetBucketTaggingResponse -> TestTree
testGetBucketTaggingResponse = res
    "GetBucketTaggingResponse"
    "fixture/GetBucketTaggingResponse.proto"
    s3
    (Proxy :: Proxy GetBucketTagging)

testCopyObjectResponse :: CopyObjectResponse -> TestTree
testCopyObjectResponse = res
    "CopyObjectResponse"
    "fixture/CopyObjectResponse.proto"
    s3
    (Proxy :: Proxy CopyObject)

testGetBucketPolicyResponse :: GetBucketPolicyResponse -> TestTree
testGetBucketPolicyResponse = res
    "GetBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse.proto"
    s3
    (Proxy :: Proxy GetBucketPolicy)

testGetBucketLoggingResponse :: GetBucketLoggingResponse -> TestTree
testGetBucketLoggingResponse = res
    "GetBucketLoggingResponse"
    "fixture/GetBucketLoggingResponse.proto"
    s3
    (Proxy :: Proxy GetBucketLogging)

testGetBucketACLResponse :: GetBucketACLResponse -> TestTree
testGetBucketACLResponse = res
    "GetBucketACLResponse"
    "fixture/GetBucketACLResponse.proto"
    s3
    (Proxy :: Proxy GetBucketACL)

testListPartsResponse :: ListPartsResponse -> TestTree
testListPartsResponse = res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    s3
    (Proxy :: Proxy ListParts)

testUploadPartCopyResponse :: UploadPartCopyResponse -> TestTree
testUploadPartCopyResponse = res
    "UploadPartCopyResponse"
    "fixture/UploadPartCopyResponse.proto"
    s3
    (Proxy :: Proxy UploadPartCopy)

testPutBucketACLResponse :: PutBucketACLResponse -> TestTree
testPutBucketACLResponse = res
    "PutBucketACLResponse"
    "fixture/PutBucketACLResponse.proto"
    s3
    (Proxy :: Proxy PutBucketACL)
