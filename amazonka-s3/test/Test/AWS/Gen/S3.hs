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
testPutBucketRequestPayment = undefined

testPutBucketLogging :: PutBucketLogging -> TestTree
testPutBucketLogging = undefined

testDeleteObject :: DeleteObject -> TestTree
testDeleteObject = undefined

testDeleteBucket :: DeleteBucket -> TestTree
testDeleteBucket = undefined

testListBuckets :: ListBuckets -> TestTree
testListBuckets = undefined

testCreateBucket :: CreateBucket -> TestTree
testCreateBucket = undefined

testPutBucketTagging :: PutBucketTagging -> TestTree
testPutBucketTagging = undefined

testDeleteBucketTagging :: DeleteBucketTagging -> TestTree
testDeleteBucketTagging = undefined

testPutObjectACL :: PutObjectACL -> TestTree
testPutObjectACL = undefined

testGetBucketNotificationConfiguration :: GetBucketNotificationConfiguration -> TestTree
testGetBucketNotificationConfiguration = undefined

testGetBucketLocation :: GetBucketLocation -> TestTree
testGetBucketLocation = undefined

testPutBucketReplication :: PutBucketReplication -> TestTree
testPutBucketReplication = undefined

testGetBucketWebsite :: GetBucketWebsite -> TestTree
testGetBucketWebsite = undefined

testGetObject :: GetObject -> TestTree
testGetObject = undefined

testDeleteBucketReplication :: DeleteBucketReplication -> TestTree
testDeleteBucketReplication = undefined

testGetBucketRequestPayment :: GetBucketRequestPayment -> TestTree
testGetBucketRequestPayment = undefined

testListObjectVersions :: ListObjectVersions -> TestTree
testListObjectVersions = undefined

testGetBucketLifecycle :: GetBucketLifecycle -> TestTree
testGetBucketLifecycle = undefined

testHeadBucket :: HeadBucket -> TestTree
testHeadBucket = undefined

testPutBucketLifecycle :: PutBucketLifecycle -> TestTree
testPutBucketLifecycle = undefined

testCreateMultipartUpload :: CreateMultipartUpload -> TestTree
testCreateMultipartUpload = undefined

testDeleteBucketLifecycle :: DeleteBucketLifecycle -> TestTree
testDeleteBucketLifecycle = undefined

testGetBucketReplication :: GetBucketReplication -> TestTree
testGetBucketReplication = undefined

testPutBucketWebsite :: PutBucketWebsite -> TestTree
testPutBucketWebsite = undefined

testCompleteMultipartUpload :: CompleteMultipartUpload -> TestTree
testCompleteMultipartUpload = undefined

testListMultipartUploads :: ListMultipartUploads -> TestTree
testListMultipartUploads = undefined

testDeleteBucketWebsite :: DeleteBucketWebsite -> TestTree
testDeleteBucketWebsite = undefined

testListObjects :: ListObjects -> TestTree
testListObjects = undefined

testDeleteObjects :: DeleteObjects -> TestTree
testDeleteObjects = undefined

testPutBucketPolicy :: PutBucketPolicy -> TestTree
testPutBucketPolicy = undefined

testDeleteBucketPolicy :: DeleteBucketPolicy -> TestTree
testDeleteBucketPolicy = undefined

testAbortMultipartUpload :: AbortMultipartUpload -> TestTree
testAbortMultipartUpload = undefined

testGetObjectTorrent :: GetObjectTorrent -> TestTree
testGetObjectTorrent = undefined

testPutBucketCORS :: PutBucketCORS -> TestTree
testPutBucketCORS = undefined

testDeleteBucketCORS :: DeleteBucketCORS -> TestTree
testDeleteBucketCORS = undefined

testGetBucketVersioning :: GetBucketVersioning -> TestTree
testGetBucketVersioning = undefined

testPutBucketNotificationConfiguration :: PutBucketNotificationConfiguration -> TestTree
testPutBucketNotificationConfiguration = undefined

testGetBucketTagging :: GetBucketTagging -> TestTree
testGetBucketTagging = undefined

testHeadObject :: HeadObject -> TestTree
testHeadObject = undefined

testPutBucketVersioning :: PutBucketVersioning -> TestTree
testPutBucketVersioning = undefined

testGetObjectACL :: GetObjectACL -> TestTree
testGetObjectACL = undefined

testRestoreObject :: RestoreObject -> TestTree
testRestoreObject = undefined

testGetBucketCORS :: GetBucketCORS -> TestTree
testGetBucketCORS = undefined

testCopyObject :: CopyObject -> TestTree
testCopyObject = undefined

testGetBucketPolicy :: GetBucketPolicy -> TestTree
testGetBucketPolicy = undefined

testGetBucketLogging :: GetBucketLogging -> TestTree
testGetBucketLogging = undefined

testListParts :: ListParts -> TestTree
testListParts = undefined

testGetBucketACL :: GetBucketACL -> TestTree
testGetBucketACL = undefined

testPutBucketACL :: PutBucketACL -> TestTree
testPutBucketACL = undefined

testUploadPartCopy :: UploadPartCopy -> TestTree
testUploadPartCopy = undefined

-- Responses

testPutBucketRequestPaymentResponse :: PutBucketRequestPaymentResponse -> TestTree
testPutBucketRequestPaymentResponse = resp
    "PutBucketRequestPaymentResponse"
    "fixture/PutBucketRequestPaymentResponse"
    (Proxy :: Proxy PutBucketRequestPayment)

testPutObjectResponse :: PutObjectResponse -> TestTree
testPutObjectResponse = resp
    "PutObjectResponse"
    "fixture/PutObjectResponse"
    (Proxy :: Proxy PutObject)

testPutBucketLoggingResponse :: PutBucketLoggingResponse -> TestTree
testPutBucketLoggingResponse = resp
    "PutBucketLoggingResponse"
    "fixture/PutBucketLoggingResponse"
    (Proxy :: Proxy PutBucketLogging)

testDeleteObjectResponse :: DeleteObjectResponse -> TestTree
testDeleteObjectResponse = resp
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse"
    (Proxy :: Proxy DeleteObject)

testDeleteBucketResponse :: DeleteBucketResponse -> TestTree
testDeleteBucketResponse = resp
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse"
    (Proxy :: Proxy DeleteBucket)

testListBucketsResponse :: ListBucketsResponse -> TestTree
testListBucketsResponse = resp
    "ListBucketsResponse"
    "fixture/ListBucketsResponse"
    (Proxy :: Proxy ListBuckets)

testCreateBucketResponse :: CreateBucketResponse -> TestTree
testCreateBucketResponse = resp
    "CreateBucketResponse"
    "fixture/CreateBucketResponse"
    (Proxy :: Proxy CreateBucket)

testPutBucketTaggingResponse :: PutBucketTaggingResponse -> TestTree
testPutBucketTaggingResponse = resp
    "PutBucketTaggingResponse"
    "fixture/PutBucketTaggingResponse"
    (Proxy :: Proxy PutBucketTagging)

testDeleteBucketTaggingResponse :: DeleteBucketTaggingResponse -> TestTree
testDeleteBucketTaggingResponse = resp
    "DeleteBucketTaggingResponse"
    "fixture/DeleteBucketTaggingResponse"
    (Proxy :: Proxy DeleteBucketTagging)

testPutObjectACLResponse :: PutObjectACLResponse -> TestTree
testPutObjectACLResponse = resp
    "PutObjectACLResponse"
    "fixture/PutObjectACLResponse"
    (Proxy :: Proxy PutObjectACL)

testGetBucketNotificationConfigurationResponse :: NotificationConfiguration -> TestTree
testGetBucketNotificationConfigurationResponse = resp
    "GetBucketNotificationConfigurationResponse"
    "fixture/GetBucketNotificationConfigurationResponse"
    (Proxy :: Proxy GetBucketNotificationConfiguration)

testGetBucketLocationResponse :: GetBucketLocationResponse -> TestTree
testGetBucketLocationResponse = resp
    "GetBucketLocationResponse"
    "fixture/GetBucketLocationResponse"
    (Proxy :: Proxy GetBucketLocation)

testPutBucketReplicationResponse :: PutBucketReplicationResponse -> TestTree
testPutBucketReplicationResponse = resp
    "PutBucketReplicationResponse"
    "fixture/PutBucketReplicationResponse"
    (Proxy :: Proxy PutBucketReplication)

testGetBucketWebsiteResponse :: GetBucketWebsiteResponse -> TestTree
testGetBucketWebsiteResponse = resp
    "GetBucketWebsiteResponse"
    "fixture/GetBucketWebsiteResponse"
    (Proxy :: Proxy GetBucketWebsite)

testDeleteBucketReplicationResponse :: DeleteBucketReplicationResponse -> TestTree
testDeleteBucketReplicationResponse = resp
    "DeleteBucketReplicationResponse"
    "fixture/DeleteBucketReplicationResponse"
    (Proxy :: Proxy DeleteBucketReplication)

testGetBucketRequestPaymentResponse :: GetBucketRequestPaymentResponse -> TestTree
testGetBucketRequestPaymentResponse = resp
    "GetBucketRequestPaymentResponse"
    "fixture/GetBucketRequestPaymentResponse"
    (Proxy :: Proxy GetBucketRequestPayment)

testListObjectVersionsResponse :: ListObjectVersionsResponse -> TestTree
testListObjectVersionsResponse = resp
    "ListObjectVersionsResponse"
    "fixture/ListObjectVersionsResponse"
    (Proxy :: Proxy ListObjectVersions)

testGetBucketLifecycleResponse :: GetBucketLifecycleResponse -> TestTree
testGetBucketLifecycleResponse = resp
    "GetBucketLifecycleResponse"
    "fixture/GetBucketLifecycleResponse"
    (Proxy :: Proxy GetBucketLifecycle)

testHeadBucketResponse :: HeadBucketResponse -> TestTree
testHeadBucketResponse = resp
    "HeadBucketResponse"
    "fixture/HeadBucketResponse"
    (Proxy :: Proxy HeadBucket)

testPutBucketLifecycleResponse :: PutBucketLifecycleResponse -> TestTree
testPutBucketLifecycleResponse = resp
    "PutBucketLifecycleResponse"
    "fixture/PutBucketLifecycleResponse"
    (Proxy :: Proxy PutBucketLifecycle)

testCreateMultipartUploadResponse :: CreateMultipartUploadResponse -> TestTree
testCreateMultipartUploadResponse = resp
    "CreateMultipartUploadResponse"
    "fixture/CreateMultipartUploadResponse"
    (Proxy :: Proxy CreateMultipartUpload)

testDeleteBucketLifecycleResponse :: DeleteBucketLifecycleResponse -> TestTree
testDeleteBucketLifecycleResponse = resp
    "DeleteBucketLifecycleResponse"
    "fixture/DeleteBucketLifecycleResponse"
    (Proxy :: Proxy DeleteBucketLifecycle)

testGetBucketReplicationResponse :: GetBucketReplicationResponse -> TestTree
testGetBucketReplicationResponse = resp
    "GetBucketReplicationResponse"
    "fixture/GetBucketReplicationResponse"
    (Proxy :: Proxy GetBucketReplication)

testPutBucketWebsiteResponse :: PutBucketWebsiteResponse -> TestTree
testPutBucketWebsiteResponse = resp
    "PutBucketWebsiteResponse"
    "fixture/PutBucketWebsiteResponse"
    (Proxy :: Proxy PutBucketWebsite)

testCompleteMultipartUploadResponse :: CompleteMultipartUploadResponse -> TestTree
testCompleteMultipartUploadResponse = resp
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse"
    (Proxy :: Proxy CompleteMultipartUpload)

testUploadPartResponse :: UploadPartResponse -> TestTree
testUploadPartResponse = resp
    "UploadPartResponse"
    "fixture/UploadPartResponse"
    (Proxy :: Proxy UploadPart)

testListMultipartUploadsResponse :: ListMultipartUploadsResponse -> TestTree
testListMultipartUploadsResponse = resp
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse"
    (Proxy :: Proxy ListMultipartUploads)

testDeleteBucketWebsiteResponse :: DeleteBucketWebsiteResponse -> TestTree
testDeleteBucketWebsiteResponse = resp
    "DeleteBucketWebsiteResponse"
    "fixture/DeleteBucketWebsiteResponse"
    (Proxy :: Proxy DeleteBucketWebsite)

testListObjectsResponse :: ListObjectsResponse -> TestTree
testListObjectsResponse = resp
    "ListObjectsResponse"
    "fixture/ListObjectsResponse"
    (Proxy :: Proxy ListObjects)

testDeleteObjectsResponse :: DeleteObjectsResponse -> TestTree
testDeleteObjectsResponse = resp
    "DeleteObjectsResponse"
    "fixture/DeleteObjectsResponse"
    (Proxy :: Proxy DeleteObjects)

testPutBucketPolicyResponse :: PutBucketPolicyResponse -> TestTree
testPutBucketPolicyResponse = resp
    "PutBucketPolicyResponse"
    "fixture/PutBucketPolicyResponse"
    (Proxy :: Proxy PutBucketPolicy)

testDeleteBucketPolicyResponse :: DeleteBucketPolicyResponse -> TestTree
testDeleteBucketPolicyResponse = resp
    "DeleteBucketPolicyResponse"
    "fixture/DeleteBucketPolicyResponse"
    (Proxy :: Proxy DeleteBucketPolicy)

testAbortMultipartUploadResponse :: AbortMultipartUploadResponse -> TestTree
testAbortMultipartUploadResponse = resp
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse"
    (Proxy :: Proxy AbortMultipartUpload)

testPutBucketCORSResponse :: PutBucketCORSResponse -> TestTree
testPutBucketCORSResponse = resp
    "PutBucketCORSResponse"
    "fixture/PutBucketCORSResponse"
    (Proxy :: Proxy PutBucketCORS)

testDeleteBucketCORSResponse :: DeleteBucketCORSResponse -> TestTree
testDeleteBucketCORSResponse = resp
    "DeleteBucketCORSResponse"
    "fixture/DeleteBucketCORSResponse"
    (Proxy :: Proxy DeleteBucketCORS)

testGetBucketVersioningResponse :: GetBucketVersioningResponse -> TestTree
testGetBucketVersioningResponse = resp
    "GetBucketVersioningResponse"
    "fixture/GetBucketVersioningResponse"
    (Proxy :: Proxy GetBucketVersioning)

testPutBucketNotificationConfigurationResponse :: PutBucketNotificationConfigurationResponse -> TestTree
testPutBucketNotificationConfigurationResponse = resp
    "PutBucketNotificationConfigurationResponse"
    "fixture/PutBucketNotificationConfigurationResponse"
    (Proxy :: Proxy PutBucketNotificationConfiguration)

testGetBucketTaggingResponse :: GetBucketTaggingResponse -> TestTree
testGetBucketTaggingResponse = resp
    "GetBucketTaggingResponse"
    "fixture/GetBucketTaggingResponse"
    (Proxy :: Proxy GetBucketTagging)

testHeadObjectResponse :: HeadObjectResponse -> TestTree
testHeadObjectResponse = resp
    "HeadObjectResponse"
    "fixture/HeadObjectResponse"
    (Proxy :: Proxy HeadObject)

testPutBucketVersioningResponse :: PutBucketVersioningResponse -> TestTree
testPutBucketVersioningResponse = resp
    "PutBucketVersioningResponse"
    "fixture/PutBucketVersioningResponse"
    (Proxy :: Proxy PutBucketVersioning)

testGetObjectACLResponse :: GetObjectACLResponse -> TestTree
testGetObjectACLResponse = resp
    "GetObjectACLResponse"
    "fixture/GetObjectACLResponse"
    (Proxy :: Proxy GetObjectACL)

testRestoreObjectResponse :: RestoreObjectResponse -> TestTree
testRestoreObjectResponse = resp
    "RestoreObjectResponse"
    "fixture/RestoreObjectResponse"
    (Proxy :: Proxy RestoreObject)

testGetBucketCORSResponse :: GetBucketCORSResponse -> TestTree
testGetBucketCORSResponse = resp
    "GetBucketCORSResponse"
    "fixture/GetBucketCORSResponse"
    (Proxy :: Proxy GetBucketCORS)

testCopyObjectResponse :: CopyObjectResponse -> TestTree
testCopyObjectResponse = resp
    "CopyObjectResponse"
    "fixture/CopyObjectResponse"
    (Proxy :: Proxy CopyObject)

testGetBucketPolicyResponse :: GetBucketPolicyResponse -> TestTree
testGetBucketPolicyResponse = resp
    "GetBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse"
    (Proxy :: Proxy GetBucketPolicy)

testGetBucketLoggingResponse :: GetBucketLoggingResponse -> TestTree
testGetBucketLoggingResponse = resp
    "GetBucketLoggingResponse"
    "fixture/GetBucketLoggingResponse"
    (Proxy :: Proxy GetBucketLogging)

testListPartsResponse :: ListPartsResponse -> TestTree
testListPartsResponse = resp
    "ListPartsResponse"
    "fixture/ListPartsResponse"
    (Proxy :: Proxy ListParts)

testGetBucketACLResponse :: GetBucketACLResponse -> TestTree
testGetBucketACLResponse = resp
    "GetBucketACLResponse"
    "fixture/GetBucketACLResponse"
    (Proxy :: Proxy GetBucketACL)

testPutBucketACLResponse :: PutBucketACLResponse -> TestTree
testPutBucketACLResponse = resp
    "PutBucketACLResponse"
    "fixture/PutBucketACLResponse"
    (Proxy :: Proxy PutBucketACL)

testUploadPartCopyResponse :: UploadPartCopyResponse -> TestTree
testUploadPartCopyResponse = resp
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
