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
    s3
    (Proxy :: Proxy PutBucketRequestPayment)

testPutObjectResponse :: PutObjectResponse -> TestTree
testPutObjectResponse = res
    "PutObjectResponse"
    "fixture/PutObjectResponse"
    s3
    (Proxy :: Proxy PutObject)

testPutBucketLoggingResponse :: PutBucketLoggingResponse -> TestTree
testPutBucketLoggingResponse = res
    "PutBucketLoggingResponse"
    "fixture/PutBucketLoggingResponse"
    s3
    (Proxy :: Proxy PutBucketLogging)

testDeleteObjectResponse :: DeleteObjectResponse -> TestTree
testDeleteObjectResponse = res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse"
    s3
    (Proxy :: Proxy DeleteObject)

testDeleteBucketResponse :: DeleteBucketResponse -> TestTree
testDeleteBucketResponse = res
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse"
    s3
    (Proxy :: Proxy DeleteBucket)

testListBucketsResponse :: ListBucketsResponse -> TestTree
testListBucketsResponse = res
    "ListBucketsResponse"
    "fixture/ListBucketsResponse"
    s3
    (Proxy :: Proxy ListBuckets)

testCreateBucketResponse :: CreateBucketResponse -> TestTree
testCreateBucketResponse = res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse"
    s3
    (Proxy :: Proxy CreateBucket)

testPutBucketTaggingResponse :: PutBucketTaggingResponse -> TestTree
testPutBucketTaggingResponse = res
    "PutBucketTaggingResponse"
    "fixture/PutBucketTaggingResponse"
    s3
    (Proxy :: Proxy PutBucketTagging)

testDeleteBucketTaggingResponse :: DeleteBucketTaggingResponse -> TestTree
testDeleteBucketTaggingResponse = res
    "DeleteBucketTaggingResponse"
    "fixture/DeleteBucketTaggingResponse"
    s3
    (Proxy :: Proxy DeleteBucketTagging)

testPutObjectACLResponse :: PutObjectACLResponse -> TestTree
testPutObjectACLResponse = res
    "PutObjectACLResponse"
    "fixture/PutObjectACLResponse"
    s3
    (Proxy :: Proxy PutObjectACL)

testGetBucketNotificationConfigurationResponse :: NotificationConfiguration -> TestTree
testGetBucketNotificationConfigurationResponse = res
    "GetBucketNotificationConfigurationResponse"
    "fixture/GetBucketNotificationConfigurationResponse"
    s3
    (Proxy :: Proxy GetBucketNotificationConfiguration)

testGetBucketLocationResponse :: GetBucketLocationResponse -> TestTree
testGetBucketLocationResponse = res
    "GetBucketLocationResponse"
    "fixture/GetBucketLocationResponse"
    s3
    (Proxy :: Proxy GetBucketLocation)

testPutBucketReplicationResponse :: PutBucketReplicationResponse -> TestTree
testPutBucketReplicationResponse = res
    "PutBucketReplicationResponse"
    "fixture/PutBucketReplicationResponse"
    s3
    (Proxy :: Proxy PutBucketReplication)

testGetBucketWebsiteResponse :: GetBucketWebsiteResponse -> TestTree
testGetBucketWebsiteResponse = res
    "GetBucketWebsiteResponse"
    "fixture/GetBucketWebsiteResponse"
    s3
    (Proxy :: Proxy GetBucketWebsite)

testDeleteBucketReplicationResponse :: DeleteBucketReplicationResponse -> TestTree
testDeleteBucketReplicationResponse = res
    "DeleteBucketReplicationResponse"
    "fixture/DeleteBucketReplicationResponse"
    s3
    (Proxy :: Proxy DeleteBucketReplication)

testGetBucketRequestPaymentResponse :: GetBucketRequestPaymentResponse -> TestTree
testGetBucketRequestPaymentResponse = res
    "GetBucketRequestPaymentResponse"
    "fixture/GetBucketRequestPaymentResponse"
    s3
    (Proxy :: Proxy GetBucketRequestPayment)

testListObjectVersionsResponse :: ListObjectVersionsResponse -> TestTree
testListObjectVersionsResponse = res
    "ListObjectVersionsResponse"
    "fixture/ListObjectVersionsResponse"
    s3
    (Proxy :: Proxy ListObjectVersions)

testGetBucketLifecycleResponse :: GetBucketLifecycleResponse -> TestTree
testGetBucketLifecycleResponse = res
    "GetBucketLifecycleResponse"
    "fixture/GetBucketLifecycleResponse"
    s3
    (Proxy :: Proxy GetBucketLifecycle)

testHeadBucketResponse :: HeadBucketResponse -> TestTree
testHeadBucketResponse = res
    "HeadBucketResponse"
    "fixture/HeadBucketResponse"
    s3
    (Proxy :: Proxy HeadBucket)

testPutBucketLifecycleResponse :: PutBucketLifecycleResponse -> TestTree
testPutBucketLifecycleResponse = res
    "PutBucketLifecycleResponse"
    "fixture/PutBucketLifecycleResponse"
    s3
    (Proxy :: Proxy PutBucketLifecycle)

testCreateMultipartUploadResponse :: CreateMultipartUploadResponse -> TestTree
testCreateMultipartUploadResponse = res
    "CreateMultipartUploadResponse"
    "fixture/CreateMultipartUploadResponse"
    s3
    (Proxy :: Proxy CreateMultipartUpload)

testDeleteBucketLifecycleResponse :: DeleteBucketLifecycleResponse -> TestTree
testDeleteBucketLifecycleResponse = res
    "DeleteBucketLifecycleResponse"
    "fixture/DeleteBucketLifecycleResponse"
    s3
    (Proxy :: Proxy DeleteBucketLifecycle)

testGetBucketReplicationResponse :: GetBucketReplicationResponse -> TestTree
testGetBucketReplicationResponse = res
    "GetBucketReplicationResponse"
    "fixture/GetBucketReplicationResponse"
    s3
    (Proxy :: Proxy GetBucketReplication)

testPutBucketWebsiteResponse :: PutBucketWebsiteResponse -> TestTree
testPutBucketWebsiteResponse = res
    "PutBucketWebsiteResponse"
    "fixture/PutBucketWebsiteResponse"
    s3
    (Proxy :: Proxy PutBucketWebsite)

testCompleteMultipartUploadResponse :: CompleteMultipartUploadResponse -> TestTree
testCompleteMultipartUploadResponse = res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse"
    s3
    (Proxy :: Proxy CompleteMultipartUpload)

testUploadPartResponse :: UploadPartResponse -> TestTree
testUploadPartResponse = res
    "UploadPartResponse"
    "fixture/UploadPartResponse"
    s3
    (Proxy :: Proxy UploadPart)

testListMultipartUploadsResponse :: ListMultipartUploadsResponse -> TestTree
testListMultipartUploadsResponse = res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse"
    s3
    (Proxy :: Proxy ListMultipartUploads)

testDeleteBucketWebsiteResponse :: DeleteBucketWebsiteResponse -> TestTree
testDeleteBucketWebsiteResponse = res
    "DeleteBucketWebsiteResponse"
    "fixture/DeleteBucketWebsiteResponse"
    s3
    (Proxy :: Proxy DeleteBucketWebsite)

testListObjectsResponse :: ListObjectsResponse -> TestTree
testListObjectsResponse = res
    "ListObjectsResponse"
    "fixture/ListObjectsResponse"
    s3
    (Proxy :: Proxy ListObjects)

testDeleteObjectsResponse :: DeleteObjectsResponse -> TestTree
testDeleteObjectsResponse = res
    "DeleteObjectsResponse"
    "fixture/DeleteObjectsResponse"
    s3
    (Proxy :: Proxy DeleteObjects)

testPutBucketPolicyResponse :: PutBucketPolicyResponse -> TestTree
testPutBucketPolicyResponse = res
    "PutBucketPolicyResponse"
    "fixture/PutBucketPolicyResponse"
    s3
    (Proxy :: Proxy PutBucketPolicy)

testDeleteBucketPolicyResponse :: DeleteBucketPolicyResponse -> TestTree
testDeleteBucketPolicyResponse = res
    "DeleteBucketPolicyResponse"
    "fixture/DeleteBucketPolicyResponse"
    s3
    (Proxy :: Proxy DeleteBucketPolicy)

testAbortMultipartUploadResponse :: AbortMultipartUploadResponse -> TestTree
testAbortMultipartUploadResponse = res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse"
    s3
    (Proxy :: Proxy AbortMultipartUpload)

testPutBucketCORSResponse :: PutBucketCORSResponse -> TestTree
testPutBucketCORSResponse = res
    "PutBucketCORSResponse"
    "fixture/PutBucketCORSResponse"
    s3
    (Proxy :: Proxy PutBucketCORS)

testDeleteBucketCORSResponse :: DeleteBucketCORSResponse -> TestTree
testDeleteBucketCORSResponse = res
    "DeleteBucketCORSResponse"
    "fixture/DeleteBucketCORSResponse"
    s3
    (Proxy :: Proxy DeleteBucketCORS)

testGetBucketVersioningResponse :: GetBucketVersioningResponse -> TestTree
testGetBucketVersioningResponse = res
    "GetBucketVersioningResponse"
    "fixture/GetBucketVersioningResponse"
    s3
    (Proxy :: Proxy GetBucketVersioning)

testPutBucketNotificationConfigurationResponse :: PutBucketNotificationConfigurationResponse -> TestTree
testPutBucketNotificationConfigurationResponse = res
    "PutBucketNotificationConfigurationResponse"
    "fixture/PutBucketNotificationConfigurationResponse"
    s3
    (Proxy :: Proxy PutBucketNotificationConfiguration)

testGetBucketTaggingResponse :: GetBucketTaggingResponse -> TestTree
testGetBucketTaggingResponse = res
    "GetBucketTaggingResponse"
    "fixture/GetBucketTaggingResponse"
    s3
    (Proxy :: Proxy GetBucketTagging)

testHeadObjectResponse :: HeadObjectResponse -> TestTree
testHeadObjectResponse = res
    "HeadObjectResponse"
    "fixture/HeadObjectResponse"
    s3
    (Proxy :: Proxy HeadObject)

testPutBucketVersioningResponse :: PutBucketVersioningResponse -> TestTree
testPutBucketVersioningResponse = res
    "PutBucketVersioningResponse"
    "fixture/PutBucketVersioningResponse"
    s3
    (Proxy :: Proxy PutBucketVersioning)

testGetObjectACLResponse :: GetObjectACLResponse -> TestTree
testGetObjectACLResponse = res
    "GetObjectACLResponse"
    "fixture/GetObjectACLResponse"
    s3
    (Proxy :: Proxy GetObjectACL)

testRestoreObjectResponse :: RestoreObjectResponse -> TestTree
testRestoreObjectResponse = res
    "RestoreObjectResponse"
    "fixture/RestoreObjectResponse"
    s3
    (Proxy :: Proxy RestoreObject)

testGetBucketCORSResponse :: GetBucketCORSResponse -> TestTree
testGetBucketCORSResponse = res
    "GetBucketCORSResponse"
    "fixture/GetBucketCORSResponse"
    s3
    (Proxy :: Proxy GetBucketCORS)

testCopyObjectResponse :: CopyObjectResponse -> TestTree
testCopyObjectResponse = res
    "CopyObjectResponse"
    "fixture/CopyObjectResponse"
    s3
    (Proxy :: Proxy CopyObject)

testGetBucketPolicyResponse :: GetBucketPolicyResponse -> TestTree
testGetBucketPolicyResponse = res
    "GetBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse"
    s3
    (Proxy :: Proxy GetBucketPolicy)

testGetBucketLoggingResponse :: GetBucketLoggingResponse -> TestTree
testGetBucketLoggingResponse = res
    "GetBucketLoggingResponse"
    "fixture/GetBucketLoggingResponse"
    s3
    (Proxy :: Proxy GetBucketLogging)

testListPartsResponse :: ListPartsResponse -> TestTree
testListPartsResponse = res
    "ListPartsResponse"
    "fixture/ListPartsResponse"
    s3
    (Proxy :: Proxy ListParts)

testGetBucketACLResponse :: GetBucketACLResponse -> TestTree
testGetBucketACLResponse = res
    "GetBucketACLResponse"
    "fixture/GetBucketACLResponse"
    s3
    (Proxy :: Proxy GetBucketACL)

testPutBucketACLResponse :: PutBucketACLResponse -> TestTree
testPutBucketACLResponse = res
    "PutBucketACLResponse"
    "fixture/PutBucketACLResponse"
    s3
    (Proxy :: Proxy PutBucketACL)

testUploadPartCopyResponse :: UploadPartCopyResponse -> TestTree
testUploadPartCopyResponse = res
    "UploadPartCopyResponse"
    "fixture/UploadPartCopyResponse"
    s3
    (Proxy :: Proxy UploadPartCopy)
