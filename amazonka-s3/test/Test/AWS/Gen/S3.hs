{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.S3
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
--         , testPutBucketLogging $
--             putBucketLogging
--
--         , testDeleteObject $
--             deleteObject
--
--         , testDeleteBucket $
--             deleteBucket
--
--         , testListBuckets $
--             listBuckets
--
--         , testCreateBucket $
--             createBucket
--
--         , testPutBucketTagging $
--             putBucketTagging
--
--         , testDeleteBucketTagging $
--             deleteBucketTagging
--
--         , testPutObjectACL $
--             putObjectACL
--
--         , testGetBucketNotificationConfiguration $
--             getBucketNotificationConfiguration
--
--         , testGetBucketLocation $
--             getBucketLocation
--
--         , testPutBucketReplication $
--             putBucketReplication
--
--         , testGetBucketWebsite $
--             getBucketWebsite
--
--         , testGetObject $
--             getObject
--
--         , testDeleteBucketReplication $
--             deleteBucketReplication
--
--         , testGetBucketRequestPayment $
--             getBucketRequestPayment
--
--         , testListObjectVersions $
--             listObjectVersions
--
--         , testGetBucketLifecycle $
--             getBucketLifecycle
--
--         , testHeadBucket $
--             headBucket
--
--         , testPutBucketLifecycle $
--             putBucketLifecycle
--
--         , testCreateMultipartUpload $
--             createMultipartUpload
--
--         , testDeleteBucketLifecycle $
--             deleteBucketLifecycle
--
--         , testGetBucketReplication $
--             getBucketReplication
--
--         , testPutBucketWebsite $
--             putBucketWebsite
--
--         , testCompleteMultipartUpload $
--             completeMultipartUpload
--
--         , testUploadPart $
--             uploadPart
--
--         , testListMultipartUploads $
--             listMultipartUploads
--
--         , testDeleteBucketWebsite $
--             deleteBucketWebsite
--
--         , testListObjects $
--             listObjects
--
--         , testDeleteObjects $
--             deleteObjects
--
--         , testPutBucketPolicy $
--             putBucketPolicy
--
--         , testDeleteBucketPolicy $
--             deleteBucketPolicy
--
--         , testAbortMultipartUpload $
--             abortMultipartUpload
--
--         , testGetObjectTorrent $
--             getObjectTorrent
--
--         , testPutBucketCORS $
--             putBucketCORS
--
--         , testDeleteBucketCORS $
--             deleteBucketCORS
--
--         , testGetBucketVersioning $
--             getBucketVersioning
--
--         , testPutBucketNotificationConfiguration $
--             putBucketNotificationConfiguration
--
--         , testGetBucketTagging $
--             getBucketTagging
--
--         , testHeadObject $
--             headObject
--
--         , testPutBucketVersioning $
--             putBucketVersioning
--
--         , testGetObjectACL $
--             getObjectACL
--
--         , testRestoreObject $
--             restoreObject
--
--         , testGetBucketCORS $
--             getBucketCORS
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
--         , testListParts $
--             listParts
--
--         , testGetBucketACL $
--             getBucketACL
--
--         , testPutBucketACL $
--             putBucketACL
--
--         , testUploadPartCopy $
--             uploadPartCopy
--
--           ]

--     , testGroup "response"
--         [ testPutBucketRequestPaymentResponse $
--             putBucketRequestPaymentResponse
--
--         , testPutObjectResponse $
--             putObjectResponse
--
--         , testPutBucketLoggingResponse $
--             putBucketLoggingResponse
--
--         , testDeleteObjectResponse $
--             deleteObjectResponse
--
--         , testDeleteBucketResponse $
--             deleteBucketResponse
--
--         , testListBucketsResponse $
--             listBucketsResponse
--
--         , testCreateBucketResponse $
--             createBucketResponse
--
--         , testPutBucketTaggingResponse $
--             putBucketTaggingResponse
--
--         , testDeleteBucketTaggingResponse $
--             deleteBucketTaggingResponse
--
--         , testPutObjectACLResponse $
--             putObjectACLResponse
--
--         , testGetBucketNotificationConfigurationResponse $
--             notificationConfiguration
--
--         , testGetBucketLocationResponse $
--             getBucketLocationResponse
--
--         , testPutBucketReplicationResponse $
--             putBucketReplicationResponse
--
--         , testGetBucketWebsiteResponse $
--             getBucketWebsiteResponse
--
--         , testGetObjectResponse $
--             getObjectResponse
--
--         , testDeleteBucketReplicationResponse $
--             deleteBucketReplicationResponse
--
--         , testGetBucketRequestPaymentResponse $
--             getBucketRequestPaymentResponse
--
--         , testListObjectVersionsResponse $
--             listObjectVersionsResponse
--
--         , testGetBucketLifecycleResponse $
--             getBucketLifecycleResponse
--
--         , testHeadBucketResponse $
--             headBucketResponse
--
--         , testPutBucketLifecycleResponse $
--             putBucketLifecycleResponse
--
--         , testCreateMultipartUploadResponse $
--             createMultipartUploadResponse
--
--         , testDeleteBucketLifecycleResponse $
--             deleteBucketLifecycleResponse
--
--         , testGetBucketReplicationResponse $
--             getBucketReplicationResponse
--
--         , testPutBucketWebsiteResponse $
--             putBucketWebsiteResponse
--
--         , testCompleteMultipartUploadResponse $
--             completeMultipartUploadResponse
--
--         , testUploadPartResponse $
--             uploadPartResponse
--
--         , testListMultipartUploadsResponse $
--             listMultipartUploadsResponse
--
--         , testDeleteBucketWebsiteResponse $
--             deleteBucketWebsiteResponse
--
--         , testListObjectsResponse $
--             listObjectsResponse
--
--         , testDeleteObjectsResponse $
--             deleteObjectsResponse
--
--         , testPutBucketPolicyResponse $
--             putBucketPolicyResponse
--
--         , testDeleteBucketPolicyResponse $
--             deleteBucketPolicyResponse
--
--         , testAbortMultipartUploadResponse $
--             abortMultipartUploadResponse
--
--         , testGetObjectTorrentResponse $
--             getObjectTorrentResponse
--
--         , testPutBucketCORSResponse $
--             putBucketCORSResponse
--
--         , testDeleteBucketCORSResponse $
--             deleteBucketCORSResponse
--
--         , testGetBucketVersioningResponse $
--             getBucketVersioningResponse
--
--         , testPutBucketNotificationConfigurationResponse $
--             putBucketNotificationConfigurationResponse
--
--         , testGetBucketTaggingResponse $
--             getBucketTaggingResponse
--
--         , testHeadObjectResponse $
--             headObjectResponse
--
--         , testPutBucketVersioningResponse $
--             putBucketVersioningResponse
--
--         , testGetObjectACLResponse $
--             getObjectACLResponse
--
--         , testRestoreObjectResponse $
--             restoreObjectResponse
--
--         , testGetBucketCORSResponse $
--             getBucketCORSResponse
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
--         , testListPartsResponse $
--             listPartsResponse
--
--         , testGetBucketACLResponse $
--             getBucketACLResponse
--
--         , testPutBucketACLResponse $
--             putBucketACLResponse
--
--         , testUploadPartCopyResponse $
--             uploadPartCopyResponse
--
--           ]
--     ]

-- Requests

testPutBucketRequestPayment :: PutBucketRequestPayment -> TestTree
testPutBucketRequestPayment = req
    "PutBucketRequestPayment"
    "fixture/PutBucketRequestPayment"

testPutBucketLogging :: PutBucketLogging -> TestTree
testPutBucketLogging = req
    "PutBucketLogging"
    "fixture/PutBucketLogging"

testDeleteObject :: DeleteObject -> TestTree
testDeleteObject = req
    "DeleteObject"
    "fixture/DeleteObject"

testDeleteBucket :: DeleteBucket -> TestTree
testDeleteBucket = req
    "DeleteBucket"
    "fixture/DeleteBucket"

testListBuckets :: ListBuckets -> TestTree
testListBuckets = req
    "ListBuckets"
    "fixture/ListBuckets"

testCreateBucket :: CreateBucket -> TestTree
testCreateBucket = req
    "CreateBucket"
    "fixture/CreateBucket"

testPutBucketTagging :: PutBucketTagging -> TestTree
testPutBucketTagging = req
    "PutBucketTagging"
    "fixture/PutBucketTagging"

testDeleteBucketTagging :: DeleteBucketTagging -> TestTree
testDeleteBucketTagging = req
    "DeleteBucketTagging"
    "fixture/DeleteBucketTagging"

testPutObjectACL :: PutObjectACL -> TestTree
testPutObjectACL = req
    "PutObjectACL"
    "fixture/PutObjectACL"

testGetBucketNotificationConfiguration :: GetBucketNotificationConfiguration -> TestTree
testGetBucketNotificationConfiguration = req
    "GetBucketNotificationConfiguration"
    "fixture/GetBucketNotificationConfiguration"

testGetBucketLocation :: GetBucketLocation -> TestTree
testGetBucketLocation = req
    "GetBucketLocation"
    "fixture/GetBucketLocation"

testPutBucketReplication :: PutBucketReplication -> TestTree
testPutBucketReplication = req
    "PutBucketReplication"
    "fixture/PutBucketReplication"

testGetBucketWebsite :: GetBucketWebsite -> TestTree
testGetBucketWebsite = req
    "GetBucketWebsite"
    "fixture/GetBucketWebsite"

testGetObject :: GetObject -> TestTree
testGetObject = req
    "GetObject"
    "fixture/GetObject"

testDeleteBucketReplication :: DeleteBucketReplication -> TestTree
testDeleteBucketReplication = req
    "DeleteBucketReplication"
    "fixture/DeleteBucketReplication"

testGetBucketRequestPayment :: GetBucketRequestPayment -> TestTree
testGetBucketRequestPayment = req
    "GetBucketRequestPayment"
    "fixture/GetBucketRequestPayment"

testListObjectVersions :: ListObjectVersions -> TestTree
testListObjectVersions = req
    "ListObjectVersions"
    "fixture/ListObjectVersions"

testGetBucketLifecycle :: GetBucketLifecycle -> TestTree
testGetBucketLifecycle = req
    "GetBucketLifecycle"
    "fixture/GetBucketLifecycle"

testHeadBucket :: HeadBucket -> TestTree
testHeadBucket = req
    "HeadBucket"
    "fixture/HeadBucket"

testPutBucketLifecycle :: PutBucketLifecycle -> TestTree
testPutBucketLifecycle = req
    "PutBucketLifecycle"
    "fixture/PutBucketLifecycle"

testCreateMultipartUpload :: CreateMultipartUpload -> TestTree
testCreateMultipartUpload = req
    "CreateMultipartUpload"
    "fixture/CreateMultipartUpload"

testDeleteBucketLifecycle :: DeleteBucketLifecycle -> TestTree
testDeleteBucketLifecycle = req
    "DeleteBucketLifecycle"
    "fixture/DeleteBucketLifecycle"

testGetBucketReplication :: GetBucketReplication -> TestTree
testGetBucketReplication = req
    "GetBucketReplication"
    "fixture/GetBucketReplication"

testPutBucketWebsite :: PutBucketWebsite -> TestTree
testPutBucketWebsite = req
    "PutBucketWebsite"
    "fixture/PutBucketWebsite"

testCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
testCompleteMultipartUpload = req
    "CompleteMultipartUpload"
    "fixture/CompleteMultipartUpload"

testListMultipartUploads :: ListMultipartUploads -> TestTree
testListMultipartUploads = req
    "ListMultipartUploads"
    "fixture/ListMultipartUploads"

testDeleteBucketWebsite :: DeleteBucketWebsite -> TestTree
testDeleteBucketWebsite = req
    "DeleteBucketWebsite"
    "fixture/DeleteBucketWebsite"

testListObjects :: ListObjects -> TestTree
testListObjects = req
    "ListObjects"
    "fixture/ListObjects"

testDeleteObjects :: DeleteObjects -> TestTree
testDeleteObjects = req
    "DeleteObjects"
    "fixture/DeleteObjects"

testPutBucketPolicy :: PutBucketPolicy -> TestTree
testPutBucketPolicy = req
    "PutBucketPolicy"
    "fixture/PutBucketPolicy"

testDeleteBucketPolicy :: DeleteBucketPolicy -> TestTree
testDeleteBucketPolicy = req
    "DeleteBucketPolicy"
    "fixture/DeleteBucketPolicy"

testAbortMultipartUpload :: AbortMultipartUpload -> TestTree
testAbortMultipartUpload = req
    "AbortMultipartUpload"
    "fixture/AbortMultipartUpload"

testGetObjectTorrent :: GetObjectTorrent -> TestTree
testGetObjectTorrent = req
    "GetObjectTorrent"
    "fixture/GetObjectTorrent"

testPutBucketCORS :: PutBucketCORS -> TestTree
testPutBucketCORS = req
    "PutBucketCORS"
    "fixture/PutBucketCORS"

testDeleteBucketCORS :: DeleteBucketCORS -> TestTree
testDeleteBucketCORS = req
    "DeleteBucketCORS"
    "fixture/DeleteBucketCORS"

testGetBucketVersioning :: GetBucketVersioning -> TestTree
testGetBucketVersioning = req
    "GetBucketVersioning"
    "fixture/GetBucketVersioning"

testPutBucketNotificationConfiguration :: PutBucketNotificationConfiguration -> TestTree
testPutBucketNotificationConfiguration = req
    "PutBucketNotificationConfiguration"
    "fixture/PutBucketNotificationConfiguration"

testGetBucketTagging :: GetBucketTagging -> TestTree
testGetBucketTagging = req
    "GetBucketTagging"
    "fixture/GetBucketTagging"

testHeadObject :: HeadObject -> TestTree
testHeadObject = req
    "HeadObject"
    "fixture/HeadObject"

testPutBucketVersioning :: PutBucketVersioning -> TestTree
testPutBucketVersioning = req
    "PutBucketVersioning"
    "fixture/PutBucketVersioning"

testGetObjectACL :: GetObjectACL -> TestTree
testGetObjectACL = req
    "GetObjectACL"
    "fixture/GetObjectACL"

testRestoreObject :: RestoreObject -> TestTree
testRestoreObject = req
    "RestoreObject"
    "fixture/RestoreObject"

testGetBucketCORS :: GetBucketCORS -> TestTree
testGetBucketCORS = req
    "GetBucketCORS"
    "fixture/GetBucketCORS"

testCopyObject :: CopyObject -> TestTree
testCopyObject = req
    "CopyObject"
    "fixture/CopyObject"

testGetBucketPolicy :: GetBucketPolicy -> TestTree
testGetBucketPolicy = req
    "GetBucketPolicy"
    "fixture/GetBucketPolicy"

testGetBucketLogging :: GetBucketLogging -> TestTree
testGetBucketLogging = req
    "GetBucketLogging"
    "fixture/GetBucketLogging"

testListParts :: ListParts -> TestTree
testListParts = req
    "ListParts"
    "fixture/ListParts"

testGetBucketACL :: GetBucketACL -> TestTree
testGetBucketACL = req
    "GetBucketACL"
    "fixture/GetBucketACL"

testPutBucketACL :: PutBucketACL -> TestTree
testPutBucketACL = req
    "PutBucketACL"
    "fixture/PutBucketACL"

testUploadPartCopy :: UploadPartCopy -> TestTree
testUploadPartCopy = req
    "UploadPartCopy"
    "fixture/UploadPartCopy"

-- Responses

testPutBucketRequestPaymentResponse :: PutBucketRequestPaymentResponse -> TestTree
testPutBucketRequestPaymentResponse = res
    "PutBucketRequestPaymentResponse"
    "fixture/PutBucketRequestPaymentResponse"
    (Proxy :: Proxy PutBucketRequestPayment)

testPutObjectResponse :: PutObjectResponse -> TestTree
testPutObjectResponse = res
    "PutObjectResponse"
    "fixture/PutObjectResponse"
    (Proxy :: Proxy PutObject)

testPutBucketLoggingResponse :: PutBucketLoggingResponse -> TestTree
testPutBucketLoggingResponse = res
    "PutBucketLoggingResponse"
    "fixture/PutBucketLoggingResponse"
    (Proxy :: Proxy PutBucketLogging)

testDeleteObjectResponse :: DeleteObjectResponse -> TestTree
testDeleteObjectResponse = res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse"
    (Proxy :: Proxy DeleteObject)

testDeleteBucketResponse :: DeleteBucketResponse -> TestTree
testDeleteBucketResponse = res
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse"
    (Proxy :: Proxy DeleteBucket)

testListBucketsResponse :: ListBucketsResponse -> TestTree
testListBucketsResponse = res
    "ListBucketsResponse"
    "fixture/ListBucketsResponse"
    (Proxy :: Proxy ListBuckets)

testCreateBucketResponse :: CreateBucketResponse -> TestTree
testCreateBucketResponse = res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse"
    (Proxy :: Proxy CreateBucket)

testPutBucketTaggingResponse :: PutBucketTaggingResponse -> TestTree
testPutBucketTaggingResponse = res
    "PutBucketTaggingResponse"
    "fixture/PutBucketTaggingResponse"
    (Proxy :: Proxy PutBucketTagging)

testDeleteBucketTaggingResponse :: DeleteBucketTaggingResponse -> TestTree
testDeleteBucketTaggingResponse = res
    "DeleteBucketTaggingResponse"
    "fixture/DeleteBucketTaggingResponse"
    (Proxy :: Proxy DeleteBucketTagging)

testPutObjectACLResponse :: PutObjectACLResponse -> TestTree
testPutObjectACLResponse = res
    "PutObjectACLResponse"
    "fixture/PutObjectACLResponse"
    (Proxy :: Proxy PutObjectACL)

testGetBucketNotificationConfigurationResponse :: NotificationConfiguration -> TestTree
testGetBucketNotificationConfigurationResponse = res
    "GetBucketNotificationConfigurationResponse"
    "fixture/GetBucketNotificationConfigurationResponse"
    (Proxy :: Proxy GetBucketNotificationConfiguration)

testGetBucketLocationResponse :: GetBucketLocationResponse -> TestTree
testGetBucketLocationResponse = res
    "GetBucketLocationResponse"
    "fixture/GetBucketLocationResponse"
    (Proxy :: Proxy GetBucketLocation)

testPutBucketReplicationResponse :: PutBucketReplicationResponse -> TestTree
testPutBucketReplicationResponse = res
    "PutBucketReplicationResponse"
    "fixture/PutBucketReplicationResponse"
    (Proxy :: Proxy PutBucketReplication)

testGetBucketWebsiteResponse :: GetBucketWebsiteResponse -> TestTree
testGetBucketWebsiteResponse = res
    "GetBucketWebsiteResponse"
    "fixture/GetBucketWebsiteResponse"
    (Proxy :: Proxy GetBucketWebsite)

testDeleteBucketReplicationResponse :: DeleteBucketReplicationResponse -> TestTree
testDeleteBucketReplicationResponse = res
    "DeleteBucketReplicationResponse"
    "fixture/DeleteBucketReplicationResponse"
    (Proxy :: Proxy DeleteBucketReplication)

testGetBucketRequestPaymentResponse :: GetBucketRequestPaymentResponse -> TestTree
testGetBucketRequestPaymentResponse = res
    "GetBucketRequestPaymentResponse"
    "fixture/GetBucketRequestPaymentResponse"
    (Proxy :: Proxy GetBucketRequestPayment)

testListObjectVersionsResponse :: ListObjectVersionsResponse -> TestTree
testListObjectVersionsResponse = res
    "ListObjectVersionsResponse"
    "fixture/ListObjectVersionsResponse"
    (Proxy :: Proxy ListObjectVersions)

testGetBucketLifecycleResponse :: GetBucketLifecycleResponse -> TestTree
testGetBucketLifecycleResponse = res
    "GetBucketLifecycleResponse"
    "fixture/GetBucketLifecycleResponse"
    (Proxy :: Proxy GetBucketLifecycle)

testHeadBucketResponse :: HeadBucketResponse -> TestTree
testHeadBucketResponse = res
    "HeadBucketResponse"
    "fixture/HeadBucketResponse"
    (Proxy :: Proxy HeadBucket)

testPutBucketLifecycleResponse :: PutBucketLifecycleResponse -> TestTree
testPutBucketLifecycleResponse = res
    "PutBucketLifecycleResponse"
    "fixture/PutBucketLifecycleResponse"
    (Proxy :: Proxy PutBucketLifecycle)

testCreateMultipartUploadResponse :: CreateMultipartUploadResponse -> TestTree
testCreateMultipartUploadResponse = res
    "CreateMultipartUploadResponse"
    "fixture/CreateMultipartUploadResponse"
    (Proxy :: Proxy CreateMultipartUpload)

testDeleteBucketLifecycleResponse :: DeleteBucketLifecycleResponse -> TestTree
testDeleteBucketLifecycleResponse = res
    "DeleteBucketLifecycleResponse"
    "fixture/DeleteBucketLifecycleResponse"
    (Proxy :: Proxy DeleteBucketLifecycle)

testGetBucketReplicationResponse :: GetBucketReplicationResponse -> TestTree
testGetBucketReplicationResponse = res
    "GetBucketReplicationResponse"
    "fixture/GetBucketReplicationResponse"
    (Proxy :: Proxy GetBucketReplication)

testPutBucketWebsiteResponse :: PutBucketWebsiteResponse -> TestTree
testPutBucketWebsiteResponse = res
    "PutBucketWebsiteResponse"
    "fixture/PutBucketWebsiteResponse"
    (Proxy :: Proxy PutBucketWebsite)

testCompleteMultipartUploadResponse :: CompleteMultipartUploadResponse -> TestTree
testCompleteMultipartUploadResponse = res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse"
    (Proxy :: Proxy CompleteMultipartUpload)

testUploadPartResponse :: UploadPartResponse -> TestTree
testUploadPartResponse = res
    "UploadPartResponse"
    "fixture/UploadPartResponse"
    (Proxy :: Proxy UploadPart)

testListMultipartUploadsResponse :: ListMultipartUploadsResponse -> TestTree
testListMultipartUploadsResponse = res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse"
    (Proxy :: Proxy ListMultipartUploads)

testDeleteBucketWebsiteResponse :: DeleteBucketWebsiteResponse -> TestTree
testDeleteBucketWebsiteResponse = res
    "DeleteBucketWebsiteResponse"
    "fixture/DeleteBucketWebsiteResponse"
    (Proxy :: Proxy DeleteBucketWebsite)

testListObjectsResponse :: ListObjectsResponse -> TestTree
testListObjectsResponse = res
    "ListObjectsResponse"
    "fixture/ListObjectsResponse"
    (Proxy :: Proxy ListObjects)

testDeleteObjectsResponse :: DeleteObjectsResponse -> TestTree
testDeleteObjectsResponse = res
    "DeleteObjectsResponse"
    "fixture/DeleteObjectsResponse"
    (Proxy :: Proxy DeleteObjects)

testPutBucketPolicyResponse :: PutBucketPolicyResponse -> TestTree
testPutBucketPolicyResponse = res
    "PutBucketPolicyResponse"
    "fixture/PutBucketPolicyResponse"
    (Proxy :: Proxy PutBucketPolicy)

testDeleteBucketPolicyResponse :: DeleteBucketPolicyResponse -> TestTree
testDeleteBucketPolicyResponse = res
    "DeleteBucketPolicyResponse"
    "fixture/DeleteBucketPolicyResponse"
    (Proxy :: Proxy DeleteBucketPolicy)

testAbortMultipartUploadResponse :: AbortMultipartUploadResponse -> TestTree
testAbortMultipartUploadResponse = res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse"
    (Proxy :: Proxy AbortMultipartUpload)

testPutBucketCORSResponse :: PutBucketCORSResponse -> TestTree
testPutBucketCORSResponse = res
    "PutBucketCORSResponse"
    "fixture/PutBucketCORSResponse"
    (Proxy :: Proxy PutBucketCORS)

testDeleteBucketCORSResponse :: DeleteBucketCORSResponse -> TestTree
testDeleteBucketCORSResponse = res
    "DeleteBucketCORSResponse"
    "fixture/DeleteBucketCORSResponse"
    (Proxy :: Proxy DeleteBucketCORS)

testGetBucketVersioningResponse :: GetBucketVersioningResponse -> TestTree
testGetBucketVersioningResponse = res
    "GetBucketVersioningResponse"
    "fixture/GetBucketVersioningResponse"
    (Proxy :: Proxy GetBucketVersioning)

testPutBucketNotificationConfigurationResponse :: PutBucketNotificationConfigurationResponse -> TestTree
testPutBucketNotificationConfigurationResponse = res
    "PutBucketNotificationConfigurationResponse"
    "fixture/PutBucketNotificationConfigurationResponse"
    (Proxy :: Proxy PutBucketNotificationConfiguration)

testGetBucketTaggingResponse :: GetBucketTaggingResponse -> TestTree
testGetBucketTaggingResponse = res
    "GetBucketTaggingResponse"
    "fixture/GetBucketTaggingResponse"
    (Proxy :: Proxy GetBucketTagging)

testHeadObjectResponse :: HeadObjectResponse -> TestTree
testHeadObjectResponse = res
    "HeadObjectResponse"
    "fixture/HeadObjectResponse"
    (Proxy :: Proxy HeadObject)

testPutBucketVersioningResponse :: PutBucketVersioningResponse -> TestTree
testPutBucketVersioningResponse = res
    "PutBucketVersioningResponse"
    "fixture/PutBucketVersioningResponse"
    (Proxy :: Proxy PutBucketVersioning)

testGetObjectACLResponse :: GetObjectACLResponse -> TestTree
testGetObjectACLResponse = res
    "GetObjectACLResponse"
    "fixture/GetObjectACLResponse"
    (Proxy :: Proxy GetObjectACL)

testRestoreObjectResponse :: RestoreObjectResponse -> TestTree
testRestoreObjectResponse = res
    "RestoreObjectResponse"
    "fixture/RestoreObjectResponse"
    (Proxy :: Proxy RestoreObject)

testGetBucketCORSResponse :: GetBucketCORSResponse -> TestTree
testGetBucketCORSResponse = res
    "GetBucketCORSResponse"
    "fixture/GetBucketCORSResponse"
    (Proxy :: Proxy GetBucketCORS)

testCopyObjectResponse :: CopyObjectResponse -> TestTree
testCopyObjectResponse = res
    "CopyObjectResponse"
    "fixture/CopyObjectResponse"
    (Proxy :: Proxy CopyObject)

testGetBucketPolicyResponse :: GetBucketPolicyResponse -> TestTree
testGetBucketPolicyResponse = res
    "GetBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse"
    (Proxy :: Proxy GetBucketPolicy)

testGetBucketLoggingResponse :: GetBucketLoggingResponse -> TestTree
testGetBucketLoggingResponse = res
    "GetBucketLoggingResponse"
    "fixture/GetBucketLoggingResponse"
    (Proxy :: Proxy GetBucketLogging)

testListPartsResponse :: ListPartsResponse -> TestTree
testListPartsResponse = res
    "ListPartsResponse"
    "fixture/ListPartsResponse"
    (Proxy :: Proxy ListParts)

testGetBucketACLResponse :: GetBucketACLResponse -> TestTree
testGetBucketACLResponse = res
    "GetBucketACLResponse"
    "fixture/GetBucketACLResponse"
    (Proxy :: Proxy GetBucketACL)

testPutBucketACLResponse :: PutBucketACLResponse -> TestTree
testPutBucketACLResponse = res
    "PutBucketACLResponse"
    "fixture/PutBucketACLResponse"
    (Proxy :: Proxy PutBucketACL)

testUploadPartCopyResponse :: UploadPartCopyResponse -> TestTree
testUploadPartCopyResponse = res
    "UploadPartCopyResponse"
    "fixture/UploadPartCopyResponse"
    (Proxy :: Proxy UploadPartCopy)

instance Out AbortMultipartUpload
instance Out AbortMultipartUploadResponse
instance Out AccessControlPolicy
instance Out Bucket
instance Out BucketCannedACL
instance Out BucketLoggingStatus
instance Out BucketLogsPermission
instance Out BucketVersioningStatus
instance Out CORSConfiguration
instance Out CORSRule
instance Out CommonPrefix
instance Out CompleteMultipartUpload
instance Out CompleteMultipartUploadResponse
instance Out CompletedMultipartUpload
instance Out CompletedPart
instance Out Condition
instance Out CopyObject
instance Out CopyObjectResponse
instance Out CopyObjectResult
instance Out CopyPartResult
instance Out CreateBucket
instance Out CreateBucketConfiguration
instance Out CreateBucketResponse
instance Out CreateMultipartUpload
instance Out CreateMultipartUploadResponse
instance Out Delete
instance Out DeleteBucket
instance Out DeleteBucketCORS
instance Out DeleteBucketCORSResponse
instance Out DeleteBucketLifecycle
instance Out DeleteBucketLifecycleResponse
instance Out DeleteBucketPolicy
instance Out DeleteBucketPolicyResponse
instance Out DeleteBucketReplication
instance Out DeleteBucketReplicationResponse
instance Out DeleteBucketResponse
instance Out DeleteBucketTagging
instance Out DeleteBucketTaggingResponse
instance Out DeleteBucketWebsite
instance Out DeleteBucketWebsiteResponse
instance Out DeleteMarkerEntry
instance Out DeleteObject
instance Out DeleteObjectResponse
instance Out DeleteObjects
instance Out DeleteObjectsResponse
instance Out DeletedObject
instance Out Destination
instance Out EncodingType
instance Out ErrorDocument
instance Out Event
instance Out ExpirationStatus
instance Out GetBucketACL
instance Out GetBucketACLResponse
instance Out GetBucketCORS
instance Out GetBucketCORSResponse
instance Out GetBucketLifecycle
instance Out GetBucketLifecycleResponse
instance Out GetBucketLocation
instance Out GetBucketLocationResponse
instance Out GetBucketLogging
instance Out GetBucketLoggingResponse
instance Out GetBucketNotificationConfiguration
instance Out GetBucketPolicy
instance Out GetBucketPolicyResponse
instance Out GetBucketReplication
instance Out GetBucketReplicationResponse
instance Out GetBucketRequestPayment
instance Out GetBucketRequestPaymentResponse
instance Out GetBucketTagging
instance Out GetBucketTaggingResponse
instance Out GetBucketVersioning
instance Out GetBucketVersioningResponse
instance Out GetBucketWebsite
instance Out GetBucketWebsiteResponse
instance Out GetObject
instance Out GetObjectACL
instance Out GetObjectACLResponse
instance Out GetObjectResponse
instance Out GetObjectTorrent
instance Out GetObjectTorrentResponse
instance Out Grant
instance Out Grantee
instance Out HeadBucket
instance Out HeadBucketResponse
instance Out HeadObject
instance Out HeadObjectResponse
instance Out IndexDocument
instance Out Initiator
instance Out LambdaFunctionConfiguration
instance Out LifecycleConfiguration
instance Out LifecycleExpiration
instance Out ListBuckets
instance Out ListBucketsResponse
instance Out ListMultipartUploads
instance Out ListMultipartUploadsResponse
instance Out ListObjectVersions
instance Out ListObjectVersionsResponse
instance Out ListObjects
instance Out ListObjectsResponse
instance Out ListParts
instance Out ListPartsResponse
instance Out LoggingEnabled
instance Out MFADelete
instance Out MFADeleteStatus
instance Out MetadataDirective
instance Out MultipartUpload
instance Out NoncurrentVersionExpiration
instance Out NoncurrentVersionTransition
instance Out NotificationConfiguration
instance Out Object
instance Out ObjectCannedACL
instance Out ObjectIdentifier
instance Out ObjectStorageClass
instance Out ObjectVersion
instance Out ObjectVersionStorageClass
instance Out Owner
instance Out Part
instance Out Payer
instance Out Permission
instance Out Protocol
instance Out PutBucketACL
instance Out PutBucketACLResponse
instance Out PutBucketCORS
instance Out PutBucketCORSResponse
instance Out PutBucketLifecycle
instance Out PutBucketLifecycleResponse
instance Out PutBucketLogging
instance Out PutBucketLoggingResponse
instance Out PutBucketNotificationConfiguration
instance Out PutBucketNotificationConfigurationResponse
instance Out PutBucketPolicy
instance Out PutBucketPolicyResponse
instance Out PutBucketReplication
instance Out PutBucketReplicationResponse
instance Out PutBucketRequestPayment
instance Out PutBucketRequestPaymentResponse
instance Out PutBucketTagging
instance Out PutBucketTaggingResponse
instance Out PutBucketVersioning
instance Out PutBucketVersioningResponse
instance Out PutBucketWebsite
instance Out PutBucketWebsiteResponse
instance Out PutObject
instance Out PutObjectACL
instance Out PutObjectACLResponse
instance Out PutObjectResponse
instance Out QueueConfiguration
instance Out Redirect
instance Out RedirectAllRequestsTo
instance Out ReplicationConfiguration
instance Out ReplicationRule
instance Out ReplicationRuleStatus
instance Out ReplicationStatus
instance Out RequestCharged
instance Out RequestPayer
instance Out RequestPaymentConfiguration
instance Out RestoreObject
instance Out RestoreObjectResponse
instance Out RestoreRequest
instance Out RoutingRule
instance Out Rule
instance Out S3ServiceError
instance Out ServerSideEncryption
instance Out StorageClass
instance Out Tag
instance Out Tagging
instance Out TargetGrant
instance Out TopicConfiguration
instance Out Transition
instance Out TransitionStorageClass
instance Out Type
instance Out UploadPart
instance Out UploadPartCopy
instance Out UploadPartCopyResponse
instance Out UploadPartResponse
instance Out VersioningConfiguration
instance Out WebsiteConfiguration
