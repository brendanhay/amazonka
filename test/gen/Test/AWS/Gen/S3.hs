-- Module      : Test.AWS.Gen.S3
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.S3 where

import           Data.Proxy
import           Network.AWS.S3
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ putBucketRequestPaymentTest $
--             putBucketRequestPayment
--
--         , putObjectTest $
--             putObject
--
--         , putBucketLoggingTest $
--             putBucketLogging
--
--         , deleteObjectTest $
--             deleteObject
--
--         , deleteBucketTest $
--             deleteBucket
--
--         , listBucketsTest $
--             listBuckets
--
--         , createBucketTest $
--             createBucket
--
--         , putBucketTaggingTest $
--             putBucketTagging
--
--         , deleteBucketTaggingTest $
--             deleteBucketTagging
--
--         , putObjectACLTest $
--             putObjectACL
--
--         , getBucketNotificationConfigurationTest $
--             getBucketNotificationConfiguration
--
--         , getBucketLocationTest $
--             getBucketLocation
--
--         , putBucketReplicationTest $
--             putBucketReplication
--
--         , getBucketWebsiteTest $
--             getBucketWebsite
--
--         , getObjectTest $
--             getObject
--
--         , deleteBucketReplicationTest $
--             deleteBucketReplication
--
--         , getBucketRequestPaymentTest $
--             getBucketRequestPayment
--
--         , listObjectVersionsTest $
--             listObjectVersions
--
--         , getBucketLifecycleTest $
--             getBucketLifecycle
--
--         , headBucketTest $
--             headBucket
--
--         , putBucketLifecycleTest $
--             putBucketLifecycle
--
--         , createMultipartUploadTest $
--             createMultipartUpload
--
--         , deleteBucketLifecycleTest $
--             deleteBucketLifecycle
--
--         , getBucketReplicationTest $
--             getBucketReplication
--
--         , putBucketWebsiteTest $
--             putBucketWebsite
--
--         , completeMultipartUploadTest $
--             completeMultipartUpload
--
--         , uploadPartTest $
--             uploadPart
--
--         , listMultipartUploadsTest $
--             listMultipartUploads
--
--         , deleteBucketWebsiteTest $
--             deleteBucketWebsite
--
--         , listObjectsTest $
--             listObjects
--
--         , deleteObjectsTest $
--             deleteObjects
--
--         , putBucketPolicyTest $
--             putBucketPolicy
--
--         , deleteBucketPolicyTest $
--             deleteBucketPolicy
--
--         , abortMultipartUploadTest $
--             abortMultipartUpload
--
--         , getObjectTorrentTest $
--             getObjectTorrent
--
--         , putBucketCORSTest $
--             putBucketCORS
--
--         , deleteBucketCORSTest $
--             deleteBucketCORS
--
--         , getBucketVersioningTest $
--             getBucketVersioning
--
--         , putBucketNotificationConfigurationTest $
--             putBucketNotificationConfiguration
--
--         , getBucketTaggingTest $
--             getBucketTagging
--
--         , headObjectTest $
--             headObject
--
--         , putBucketVersioningTest $
--             putBucketVersioning
--
--         , getObjectACLTest $
--             getObjectACL
--
--         , restoreObjectTest $
--             restoreObject
--
--         , getBucketCORSTest $
--             getBucketCORS
--
--         , copyObjectTest $
--             copyObject
--
--         , getBucketPolicyTest $
--             getBucketPolicy
--
--         , getBucketLoggingTest $
--             getBucketLogging
--
--         , listPartsTest $
--             listParts
--
--         , getBucketACLTest $
--             getBucketACL
--
--         , putBucketACLTest $
--             putBucketACL
--
--         , uploadPartCopyTest $
--             uploadPartCopy
--
--           ]

--     , testGroup "response"
--         [ putBucketRequestPaymentResponseTest $
--             putBucketRequestPaymentResponse
--
--         , putObjectResponseTest $
--             putObjectResponse
--
--         , putBucketLoggingResponseTest $
--             putBucketLoggingResponse
--
--         , deleteObjectResponseTest $
--             deleteObjectResponse
--
--         , deleteBucketResponseTest $
--             deleteBucketResponse
--
--         , listBucketsResponseTest $
--             listBucketsResponse
--
--         , createBucketResponseTest $
--             createBucketResponse
--
--         , putBucketTaggingResponseTest $
--             putBucketTaggingResponse
--
--         , deleteBucketTaggingResponseTest $
--             deleteBucketTaggingResponse
--
--         , putObjectACLResponseTest $
--             putObjectACLResponse
--
--         , notificationConfigurationTest $
--             notificationConfiguration
--
--         , getBucketLocationResponseTest $
--             getBucketLocationResponse
--
--         , putBucketReplicationResponseTest $
--             putBucketReplicationResponse
--
--         , getBucketWebsiteResponseTest $
--             getBucketWebsiteResponse
--
--         , getObjectResponseTest $
--             getObjectResponse
--
--         , deleteBucketReplicationResponseTest $
--             deleteBucketReplicationResponse
--
--         , getBucketRequestPaymentResponseTest $
--             getBucketRequestPaymentResponse
--
--         , listObjectVersionsResponseTest $
--             listObjectVersionsResponse
--
--         , getBucketLifecycleResponseTest $
--             getBucketLifecycleResponse
--
--         , headBucketResponseTest $
--             headBucketResponse
--
--         , putBucketLifecycleResponseTest $
--             putBucketLifecycleResponse
--
--         , createMultipartUploadResponseTest $
--             createMultipartUploadResponse
--
--         , deleteBucketLifecycleResponseTest $
--             deleteBucketLifecycleResponse
--
--         , getBucketReplicationResponseTest $
--             getBucketReplicationResponse
--
--         , putBucketWebsiteResponseTest $
--             putBucketWebsiteResponse
--
--         , completeMultipartUploadResponseTest $
--             completeMultipartUploadResponse
--
--         , uploadPartResponseTest $
--             uploadPartResponse
--
--         , listMultipartUploadsResponseTest $
--             listMultipartUploadsResponse
--
--         , deleteBucketWebsiteResponseTest $
--             deleteBucketWebsiteResponse
--
--         , listObjectsResponseTest $
--             listObjectsResponse
--
--         , deleteObjectsResponseTest $
--             deleteObjectsResponse
--
--         , putBucketPolicyResponseTest $
--             putBucketPolicyResponse
--
--         , deleteBucketPolicyResponseTest $
--             deleteBucketPolicyResponse
--
--         , abortMultipartUploadResponseTest $
--             abortMultipartUploadResponse
--
--         , getObjectTorrentResponseTest $
--             getObjectTorrentResponse
--
--         , putBucketCORSResponseTest $
--             putBucketCORSResponse
--
--         , deleteBucketCORSResponseTest $
--             deleteBucketCORSResponse
--
--         , getBucketVersioningResponseTest $
--             getBucketVersioningResponse
--
--         , putBucketNotificationConfigurationResponseTest $
--             putBucketNotificationConfigurationResponse
--
--         , getBucketTaggingResponseTest $
--             getBucketTaggingResponse
--
--         , headObjectResponseTest $
--             headObjectResponse
--
--         , putBucketVersioningResponseTest $
--             putBucketVersioningResponse
--
--         , getObjectACLResponseTest $
--             getObjectACLResponse
--
--         , restoreObjectResponseTest $
--             restoreObjectResponse
--
--         , getBucketCORSResponseTest $
--             getBucketCORSResponse
--
--         , copyObjectResponseTest $
--             copyObjectResponse
--
--         , getBucketPolicyResponseTest $
--             getBucketPolicyResponse
--
--         , getBucketLoggingResponseTest $
--             getBucketLoggingResponse
--
--         , listPartsResponseTest $
--             listPartsResponse
--
--         , getBucketACLResponseTest $
--             getBucketACLResponse
--
--         , putBucketACLResponseTest $
--             putBucketACLResponse
--
--         , uploadPartCopyResponseTest $
--             uploadPartCopyResponse
--
--           ]
--     ]

-- Requests

putBucketRequestPaymentTest :: PutBucketRequestPayment -> TestTree
putBucketRequestPaymentTest = undefined

putObjectTest :: PutObject -> TestTree
putObjectTest = undefined

putBucketLoggingTest :: PutBucketLogging -> TestTree
putBucketLoggingTest = undefined

deleteObjectTest :: DeleteObject -> TestTree
deleteObjectTest = undefined

deleteBucketTest :: DeleteBucket -> TestTree
deleteBucketTest = undefined

listBucketsTest :: ListBuckets -> TestTree
listBucketsTest = undefined

createBucketTest :: CreateBucket -> TestTree
createBucketTest = undefined

putBucketTaggingTest :: PutBucketTagging -> TestTree
putBucketTaggingTest = undefined

deleteBucketTaggingTest :: DeleteBucketTagging -> TestTree
deleteBucketTaggingTest = undefined

putObjectACLTest :: PutObjectACL -> TestTree
putObjectACLTest = undefined

getBucketNotificationConfigurationTest :: GetBucketNotificationConfiguration -> TestTree
getBucketNotificationConfigurationTest = undefined

getBucketLocationTest :: GetBucketLocation -> TestTree
getBucketLocationTest = undefined

putBucketReplicationTest :: PutBucketReplication -> TestTree
putBucketReplicationTest = undefined

getBucketWebsiteTest :: GetBucketWebsite -> TestTree
getBucketWebsiteTest = undefined

getObjectTest :: GetObject -> TestTree
getObjectTest = undefined

deleteBucketReplicationTest :: DeleteBucketReplication -> TestTree
deleteBucketReplicationTest = undefined

getBucketRequestPaymentTest :: GetBucketRequestPayment -> TestTree
getBucketRequestPaymentTest = undefined

listObjectVersionsTest :: ListObjectVersions -> TestTree
listObjectVersionsTest = undefined

getBucketLifecycleTest :: GetBucketLifecycle -> TestTree
getBucketLifecycleTest = undefined

headBucketTest :: HeadBucket -> TestTree
headBucketTest = undefined

putBucketLifecycleTest :: PutBucketLifecycle -> TestTree
putBucketLifecycleTest = undefined

createMultipartUploadTest :: CreateMultipartUpload -> TestTree
createMultipartUploadTest = undefined

deleteBucketLifecycleTest :: DeleteBucketLifecycle -> TestTree
deleteBucketLifecycleTest = undefined

getBucketReplicationTest :: GetBucketReplication -> TestTree
getBucketReplicationTest = undefined

putBucketWebsiteTest :: PutBucketWebsite -> TestTree
putBucketWebsiteTest = undefined

completeMultipartUploadTest :: CompleteMultipartUpload -> TestTree
completeMultipartUploadTest = undefined

uploadPartTest :: UploadPart -> TestTree
uploadPartTest = undefined

listMultipartUploadsTest :: ListMultipartUploads -> TestTree
listMultipartUploadsTest = undefined

deleteBucketWebsiteTest :: DeleteBucketWebsite -> TestTree
deleteBucketWebsiteTest = undefined

listObjectsTest :: ListObjects -> TestTree
listObjectsTest = undefined

deleteObjectsTest :: DeleteObjects -> TestTree
deleteObjectsTest = undefined

putBucketPolicyTest :: PutBucketPolicy -> TestTree
putBucketPolicyTest = undefined

deleteBucketPolicyTest :: DeleteBucketPolicy -> TestTree
deleteBucketPolicyTest = undefined

abortMultipartUploadTest :: AbortMultipartUpload -> TestTree
abortMultipartUploadTest = undefined

getObjectTorrentTest :: GetObjectTorrent -> TestTree
getObjectTorrentTest = undefined

putBucketCORSTest :: PutBucketCORS -> TestTree
putBucketCORSTest = undefined

deleteBucketCORSTest :: DeleteBucketCORS -> TestTree
deleteBucketCORSTest = undefined

getBucketVersioningTest :: GetBucketVersioning -> TestTree
getBucketVersioningTest = undefined

putBucketNotificationConfigurationTest :: PutBucketNotificationConfiguration -> TestTree
putBucketNotificationConfigurationTest = undefined

getBucketTaggingTest :: GetBucketTagging -> TestTree
getBucketTaggingTest = undefined

headObjectTest :: HeadObject -> TestTree
headObjectTest = undefined

putBucketVersioningTest :: PutBucketVersioning -> TestTree
putBucketVersioningTest = undefined

getObjectACLTest :: GetObjectACL -> TestTree
getObjectACLTest = undefined

restoreObjectTest :: RestoreObject -> TestTree
restoreObjectTest = undefined

getBucketCORSTest :: GetBucketCORS -> TestTree
getBucketCORSTest = undefined

copyObjectTest :: CopyObject -> TestTree
copyObjectTest = undefined

getBucketPolicyTest :: GetBucketPolicy -> TestTree
getBucketPolicyTest = undefined

getBucketLoggingTest :: GetBucketLogging -> TestTree
getBucketLoggingTest = undefined

listPartsTest :: ListParts -> TestTree
listPartsTest = undefined

getBucketACLTest :: GetBucketACL -> TestTree
getBucketACLTest = undefined

putBucketACLTest :: PutBucketACL -> TestTree
putBucketACLTest = undefined

uploadPartCopyTest :: UploadPartCopy -> TestTree
uploadPartCopyTest = undefined

-- Responses

putBucketRequestPaymentResponseTest :: PutBucketRequestPaymentResponse -> TestTree
putBucketRequestPaymentResponseTest = resp
    "PutBucketRequestPaymentResponse"
    "fixture/S3/PutBucketRequestPaymentResponse"
    (Proxy :: Proxy PutBucketRequestPayment)

putObjectResponseTest :: PutObjectResponse -> TestTree
putObjectResponseTest = resp
    "PutObjectResponse"
    "fixture/S3/PutObjectResponse"
    (Proxy :: Proxy PutObject)

putBucketLoggingResponseTest :: PutBucketLoggingResponse -> TestTree
putBucketLoggingResponseTest = resp
    "PutBucketLoggingResponse"
    "fixture/S3/PutBucketLoggingResponse"
    (Proxy :: Proxy PutBucketLogging)

deleteObjectResponseTest :: DeleteObjectResponse -> TestTree
deleteObjectResponseTest = resp
    "DeleteObjectResponse"
    "fixture/S3/DeleteObjectResponse"
    (Proxy :: Proxy DeleteObject)

deleteBucketResponseTest :: DeleteBucketResponse -> TestTree
deleteBucketResponseTest = resp
    "DeleteBucketResponse"
    "fixture/S3/DeleteBucketResponse"
    (Proxy :: Proxy DeleteBucket)

listBucketsResponseTest :: ListBucketsResponse -> TestTree
listBucketsResponseTest = resp
    "ListBucketsResponse"
    "fixture/S3/ListBucketsResponse"
    (Proxy :: Proxy ListBuckets)

createBucketResponseTest :: CreateBucketResponse -> TestTree
createBucketResponseTest = resp
    "CreateBucketResponse"
    "fixture/S3/CreateBucketResponse"
    (Proxy :: Proxy CreateBucket)

putBucketTaggingResponseTest :: PutBucketTaggingResponse -> TestTree
putBucketTaggingResponseTest = resp
    "PutBucketTaggingResponse"
    "fixture/S3/PutBucketTaggingResponse"
    (Proxy :: Proxy PutBucketTagging)

deleteBucketTaggingResponseTest :: DeleteBucketTaggingResponse -> TestTree
deleteBucketTaggingResponseTest = resp
    "DeleteBucketTaggingResponse"
    "fixture/S3/DeleteBucketTaggingResponse"
    (Proxy :: Proxy DeleteBucketTagging)

putObjectACLResponseTest :: PutObjectACLResponse -> TestTree
putObjectACLResponseTest = resp
    "PutObjectACLResponse"
    "fixture/S3/PutObjectACLResponse"
    (Proxy :: Proxy PutObjectACL)

notificationConfigurationTest :: NotificationConfiguration -> TestTree
notificationConfigurationTest = resp
    "NotificationConfiguration"
    "fixture/S3/NotificationConfiguration"
    (Proxy :: Proxy GetBucketNotificationConfiguration)

getBucketLocationResponseTest :: GetBucketLocationResponse -> TestTree
getBucketLocationResponseTest = resp
    "GetBucketLocationResponse"
    "fixture/S3/GetBucketLocationResponse"
    (Proxy :: Proxy GetBucketLocation)

putBucketReplicationResponseTest :: PutBucketReplicationResponse -> TestTree
putBucketReplicationResponseTest = resp
    "PutBucketReplicationResponse"
    "fixture/S3/PutBucketReplicationResponse"
    (Proxy :: Proxy PutBucketReplication)

getBucketWebsiteResponseTest :: GetBucketWebsiteResponse -> TestTree
getBucketWebsiteResponseTest = resp
    "GetBucketWebsiteResponse"
    "fixture/S3/GetBucketWebsiteResponse"
    (Proxy :: Proxy GetBucketWebsite)

getObjectResponseTest :: GetObjectResponse -> TestTree
getObjectResponseTest = resp
    "GetObjectResponse"
    "fixture/S3/GetObjectResponse"
    (Proxy :: Proxy GetObject)

deleteBucketReplicationResponseTest :: DeleteBucketReplicationResponse -> TestTree
deleteBucketReplicationResponseTest = resp
    "DeleteBucketReplicationResponse"
    "fixture/S3/DeleteBucketReplicationResponse"
    (Proxy :: Proxy DeleteBucketReplication)

getBucketRequestPaymentResponseTest :: GetBucketRequestPaymentResponse -> TestTree
getBucketRequestPaymentResponseTest = resp
    "GetBucketRequestPaymentResponse"
    "fixture/S3/GetBucketRequestPaymentResponse"
    (Proxy :: Proxy GetBucketRequestPayment)

listObjectVersionsResponseTest :: ListObjectVersionsResponse -> TestTree
listObjectVersionsResponseTest = resp
    "ListObjectVersionsResponse"
    "fixture/S3/ListObjectVersionsResponse"
    (Proxy :: Proxy ListObjectVersions)

getBucketLifecycleResponseTest :: GetBucketLifecycleResponse -> TestTree
getBucketLifecycleResponseTest = resp
    "GetBucketLifecycleResponse"
    "fixture/S3/GetBucketLifecycleResponse"
    (Proxy :: Proxy GetBucketLifecycle)

headBucketResponseTest :: HeadBucketResponse -> TestTree
headBucketResponseTest = resp
    "HeadBucketResponse"
    "fixture/S3/HeadBucketResponse"
    (Proxy :: Proxy HeadBucket)

putBucketLifecycleResponseTest :: PutBucketLifecycleResponse -> TestTree
putBucketLifecycleResponseTest = resp
    "PutBucketLifecycleResponse"
    "fixture/S3/PutBucketLifecycleResponse"
    (Proxy :: Proxy PutBucketLifecycle)

createMultipartUploadResponseTest :: CreateMultipartUploadResponse -> TestTree
createMultipartUploadResponseTest = resp
    "CreateMultipartUploadResponse"
    "fixture/S3/CreateMultipartUploadResponse"
    (Proxy :: Proxy CreateMultipartUpload)

deleteBucketLifecycleResponseTest :: DeleteBucketLifecycleResponse -> TestTree
deleteBucketLifecycleResponseTest = resp
    "DeleteBucketLifecycleResponse"
    "fixture/S3/DeleteBucketLifecycleResponse"
    (Proxy :: Proxy DeleteBucketLifecycle)

getBucketReplicationResponseTest :: GetBucketReplicationResponse -> TestTree
getBucketReplicationResponseTest = resp
    "GetBucketReplicationResponse"
    "fixture/S3/GetBucketReplicationResponse"
    (Proxy :: Proxy GetBucketReplication)

putBucketWebsiteResponseTest :: PutBucketWebsiteResponse -> TestTree
putBucketWebsiteResponseTest = resp
    "PutBucketWebsiteResponse"
    "fixture/S3/PutBucketWebsiteResponse"
    (Proxy :: Proxy PutBucketWebsite)

completeMultipartUploadResponseTest :: CompleteMultipartUploadResponse -> TestTree
completeMultipartUploadResponseTest = resp
    "CompleteMultipartUploadResponse"
    "fixture/S3/CompleteMultipartUploadResponse"
    (Proxy :: Proxy CompleteMultipartUpload)

uploadPartResponseTest :: UploadPartResponse -> TestTree
uploadPartResponseTest = resp
    "UploadPartResponse"
    "fixture/S3/UploadPartResponse"
    (Proxy :: Proxy UploadPart)

listMultipartUploadsResponseTest :: ListMultipartUploadsResponse -> TestTree
listMultipartUploadsResponseTest = resp
    "ListMultipartUploadsResponse"
    "fixture/S3/ListMultipartUploadsResponse"
    (Proxy :: Proxy ListMultipartUploads)

deleteBucketWebsiteResponseTest :: DeleteBucketWebsiteResponse -> TestTree
deleteBucketWebsiteResponseTest = resp
    "DeleteBucketWebsiteResponse"
    "fixture/S3/DeleteBucketWebsiteResponse"
    (Proxy :: Proxy DeleteBucketWebsite)

listObjectsResponseTest :: ListObjectsResponse -> TestTree
listObjectsResponseTest = resp
    "ListObjectsResponse"
    "fixture/S3/ListObjectsResponse"
    (Proxy :: Proxy ListObjects)

deleteObjectsResponseTest :: DeleteObjectsResponse -> TestTree
deleteObjectsResponseTest = resp
    "DeleteObjectsResponse"
    "fixture/S3/DeleteObjectsResponse"
    (Proxy :: Proxy DeleteObjects)

putBucketPolicyResponseTest :: PutBucketPolicyResponse -> TestTree
putBucketPolicyResponseTest = resp
    "PutBucketPolicyResponse"
    "fixture/S3/PutBucketPolicyResponse"
    (Proxy :: Proxy PutBucketPolicy)

deleteBucketPolicyResponseTest :: DeleteBucketPolicyResponse -> TestTree
deleteBucketPolicyResponseTest = resp
    "DeleteBucketPolicyResponse"
    "fixture/S3/DeleteBucketPolicyResponse"
    (Proxy :: Proxy DeleteBucketPolicy)

abortMultipartUploadResponseTest :: AbortMultipartUploadResponse -> TestTree
abortMultipartUploadResponseTest = resp
    "AbortMultipartUploadResponse"
    "fixture/S3/AbortMultipartUploadResponse"
    (Proxy :: Proxy AbortMultipartUpload)

getObjectTorrentResponseTest :: GetObjectTorrentResponse -> TestTree
getObjectTorrentResponseTest = resp
    "GetObjectTorrentResponse"
    "fixture/S3/GetObjectTorrentResponse"
    (Proxy :: Proxy GetObjectTorrent)

putBucketCORSResponseTest :: PutBucketCORSResponse -> TestTree
putBucketCORSResponseTest = resp
    "PutBucketCORSResponse"
    "fixture/S3/PutBucketCORSResponse"
    (Proxy :: Proxy PutBucketCORS)

deleteBucketCORSResponseTest :: DeleteBucketCORSResponse -> TestTree
deleteBucketCORSResponseTest = resp
    "DeleteBucketCORSResponse"
    "fixture/S3/DeleteBucketCORSResponse"
    (Proxy :: Proxy DeleteBucketCORS)

getBucketVersioningResponseTest :: GetBucketVersioningResponse -> TestTree
getBucketVersioningResponseTest = resp
    "GetBucketVersioningResponse"
    "fixture/S3/GetBucketVersioningResponse"
    (Proxy :: Proxy GetBucketVersioning)

putBucketNotificationConfigurationResponseTest :: PutBucketNotificationConfigurationResponse -> TestTree
putBucketNotificationConfigurationResponseTest = resp
    "PutBucketNotificationConfigurationResponse"
    "fixture/S3/PutBucketNotificationConfigurationResponse"
    (Proxy :: Proxy PutBucketNotificationConfiguration)

getBucketTaggingResponseTest :: GetBucketTaggingResponse -> TestTree
getBucketTaggingResponseTest = resp
    "GetBucketTaggingResponse"
    "fixture/S3/GetBucketTaggingResponse"
    (Proxy :: Proxy GetBucketTagging)

headObjectResponseTest :: HeadObjectResponse -> TestTree
headObjectResponseTest = resp
    "HeadObjectResponse"
    "fixture/S3/HeadObjectResponse"
    (Proxy :: Proxy HeadObject)

putBucketVersioningResponseTest :: PutBucketVersioningResponse -> TestTree
putBucketVersioningResponseTest = resp
    "PutBucketVersioningResponse"
    "fixture/S3/PutBucketVersioningResponse"
    (Proxy :: Proxy PutBucketVersioning)

getObjectACLResponseTest :: GetObjectACLResponse -> TestTree
getObjectACLResponseTest = resp
    "GetObjectACLResponse"
    "fixture/S3/GetObjectACLResponse"
    (Proxy :: Proxy GetObjectACL)

restoreObjectResponseTest :: RestoreObjectResponse -> TestTree
restoreObjectResponseTest = resp
    "RestoreObjectResponse"
    "fixture/S3/RestoreObjectResponse"
    (Proxy :: Proxy RestoreObject)

getBucketCORSResponseTest :: GetBucketCORSResponse -> TestTree
getBucketCORSResponseTest = resp
    "GetBucketCORSResponse"
    "fixture/S3/GetBucketCORSResponse"
    (Proxy :: Proxy GetBucketCORS)

copyObjectResponseTest :: CopyObjectResponse -> TestTree
copyObjectResponseTest = resp
    "CopyObjectResponse"
    "fixture/S3/CopyObjectResponse"
    (Proxy :: Proxy CopyObject)

getBucketPolicyResponseTest :: GetBucketPolicyResponse -> TestTree
getBucketPolicyResponseTest = resp
    "GetBucketPolicyResponse"
    "fixture/S3/GetBucketPolicyResponse"
    (Proxy :: Proxy GetBucketPolicy)

getBucketLoggingResponseTest :: GetBucketLoggingResponse -> TestTree
getBucketLoggingResponseTest = resp
    "GetBucketLoggingResponse"
    "fixture/S3/GetBucketLoggingResponse"
    (Proxy :: Proxy GetBucketLogging)

listPartsResponseTest :: ListPartsResponse -> TestTree
listPartsResponseTest = resp
    "ListPartsResponse"
    "fixture/S3/ListPartsResponse"
    (Proxy :: Proxy ListParts)

getBucketACLResponseTest :: GetBucketACLResponse -> TestTree
getBucketACLResponseTest = resp
    "GetBucketACLResponse"
    "fixture/S3/GetBucketACLResponse"
    (Proxy :: Proxy GetBucketACL)

putBucketACLResponseTest :: PutBucketACLResponse -> TestTree
putBucketACLResponseTest = resp
    "PutBucketACLResponse"
    "fixture/S3/PutBucketACLResponse"
    (Proxy :: Proxy PutBucketACL)

uploadPartCopyResponseTest :: UploadPartCopyResponse -> TestTree
uploadPartCopyResponseTest = resp
    "UploadPartCopyResponse"
    "fixture/S3/UploadPartCopyResponse"
    (Proxy :: Proxy UploadPartCopy)
