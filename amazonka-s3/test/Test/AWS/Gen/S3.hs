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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.S3

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ abortMultipartUploadTest $
--             abortMultipartUpload
--
--         , completeMultipartUploadTest $
--             completeMultipartUpload
--
--         , copyObjectTest $
--             copyObject
--
--         , createBucketTest $
--             createBucket
--
--         , createMultipartUploadTest $
--             createMultipartUpload
--
--         , deleteBucketTest $
--             deleteBucket
--
--         , deleteBucketCORSTest $
--             deleteBucketCORS
--
--         , deleteBucketLifecycleTest $
--             deleteBucketLifecycle
--
--         , deleteBucketPolicyTest $
--             deleteBucketPolicy
--
--         , deleteBucketReplicationTest $
--             deleteBucketReplication
--
--         , deleteBucketTaggingTest $
--             deleteBucketTagging
--
--         , deleteBucketWebsiteTest $
--             deleteBucketWebsite
--
--         , deleteObjectTest $
--             deleteObject
--
--         , deleteObjectsTest $
--             deleteObjects
--
--         , getBucketACLTest $
--             getBucketACL
--
--         , getBucketCORSTest $
--             getBucketCORS
--
--         , getBucketLifecycleTest $
--             getBucketLifecycle
--
--         , getBucketLocationTest $
--             getBucketLocation
--
--         , getBucketLoggingTest $
--             getBucketLogging
--
--         , getBucketNotificationConfigurationTest $
--             getBucketNotificationConfiguration
--
--         , getBucketPolicyTest $
--             getBucketPolicy
--
--         , getBucketReplicationTest $
--             getBucketReplication
--
--         , getBucketRequestPaymentTest $
--             getBucketRequestPayment
--
--         , getBucketTaggingTest $
--             getBucketTagging
--
--         , getBucketVersioningTest $
--             getBucketVersioning
--
--         , getBucketWebsiteTest $
--             getBucketWebsite
--
--         , getObjectTest $
--             getObject
--
--         , getObjectACLTest $
--             getObjectACL
--
--         , getObjectTorrentTest $
--             getObjectTorrent
--
--         , headBucketTest $
--             headBucket
--
--         , headObjectTest $
--             headObject
--
--         , listBucketsTest $
--             listBuckets
--
--         , listMultipartUploadsTest $
--             listMultipartUploads
--
--         , listObjectVersionsTest $
--             listObjectVersions
--
--         , listObjectsTest $
--             listObjects
--
--         , listPartsTest $
--             listParts
--
--         , putBucketACLTest $
--             putBucketACL
--
--         , putBucketCORSTest $
--             putBucketCORS
--
--         , putBucketLifecycleTest $
--             putBucketLifecycle
--
--         , putBucketLoggingTest $
--             putBucketLogging
--
--         , putBucketNotificationConfigurationTest $
--             putBucketNotificationConfiguration
--
--         , putBucketPolicyTest $
--             putBucketPolicy
--
--         , putBucketReplicationTest $
--             putBucketReplication
--
--         , putBucketRequestPaymentTest $
--             putBucketRequestPayment
--
--         , putBucketTaggingTest $
--             putBucketTagging
--
--         , putBucketVersioningTest $
--             putBucketVersioning
--
--         , putBucketWebsiteTest $
--             putBucketWebsite
--
--         , putObjectTest $
--             putObject
--
--         , putObjectACLTest $
--             putObjectACL
--
--         , restoreObjectTest $
--             restoreObject
--
--         , uploadPartTest $
--             uploadPart
--
--         , uploadPartCopyTest $
--             uploadPartCopy
--
--           ]

--     , testGroup "response"
--         [ abortMultipartUploadResponseTest $
--             abortMultipartUploadResponse
--
--         , completeMultipartUploadResponseTest $
--             completeMultipartUploadResponse
--
--         , copyObjectResponseTest $
--             copyObjectResponse
--
--         , createBucketResponseTest $
--             createBucketResponse
--
--         , createMultipartUploadResponseTest $
--             createMultipartUploadResponse
--
--         , deleteBucketResponseTest $
--             deleteBucketResponse
--
--         , deleteBucketCORSResponseTest $
--             deleteBucketCORSResponse
--
--         , deleteBucketLifecycleResponseTest $
--             deleteBucketLifecycleResponse
--
--         , deleteBucketPolicyResponseTest $
--             deleteBucketPolicyResponse
--
--         , deleteBucketReplicationResponseTest $
--             deleteBucketReplicationResponse
--
--         , deleteBucketTaggingResponseTest $
--             deleteBucketTaggingResponse
--
--         , deleteBucketWebsiteResponseTest $
--             deleteBucketWebsiteResponse
--
--         , deleteObjectResponseTest $
--             deleteObjectResponse
--
--         , deleteObjectsResponseTest $
--             deleteObjectsResponse
--
--         , getBucketACLResponseTest $
--             getBucketACLResponse
--
--         , getBucketCORSResponseTest $
--             getBucketCORSResponse
--
--         , getBucketLifecycleResponseTest $
--             getBucketLifecycleResponse
--
--         , getBucketLocationResponseTest $
--             getBucketLocationResponse
--
--         , getBucketLoggingResponseTest $
--             getBucketLoggingResponse
--
--         , getBucketNotificationConfigurationResponseTest $
--             notificationConfiguration
--
--         , getBucketPolicyResponseTest $
--             getBucketPolicyResponse
--
--         , getBucketReplicationResponseTest $
--             getBucketReplicationResponse
--
--         , getBucketRequestPaymentResponseTest $
--             getBucketRequestPaymentResponse
--
--         , getBucketTaggingResponseTest $
--             getBucketTaggingResponse
--
--         , getBucketVersioningResponseTest $
--             getBucketVersioningResponse
--
--         , getBucketWebsiteResponseTest $
--             getBucketWebsiteResponse
--
--         , getObjectResponseTest $
--             getObjectResponse
--
--         , getObjectACLResponseTest $
--             getObjectACLResponse
--
--         , getObjectTorrentResponseTest $
--             getObjectTorrentResponse
--
--         , headBucketResponseTest $
--             headBucketResponse
--
--         , headObjectResponseTest $
--             headObjectResponse
--
--         , listBucketsResponseTest $
--             listBucketsResponse
--
--         , listMultipartUploadsResponseTest $
--             listMultipartUploadsResponse
--
--         , listObjectVersionsResponseTest $
--             listObjectVersionsResponse
--
--         , listObjectsResponseTest $
--             listObjectsResponse
--
--         , listPartsResponseTest $
--             listPartsResponse
--
--         , putBucketACLResponseTest $
--             putBucketACLResponse
--
--         , putBucketCORSResponseTest $
--             putBucketCORSResponse
--
--         , putBucketLifecycleResponseTest $
--             putBucketLifecycleResponse
--
--         , putBucketLoggingResponseTest $
--             putBucketLoggingResponse
--
--         , putBucketNotificationConfigurationResponseTest $
--             putBucketNotificationConfigurationResponse
--
--         , putBucketPolicyResponseTest $
--             putBucketPolicyResponse
--
--         , putBucketReplicationResponseTest $
--             putBucketReplicationResponse
--
--         , putBucketRequestPaymentResponseTest $
--             putBucketRequestPaymentResponse
--
--         , putBucketTaggingResponseTest $
--             putBucketTaggingResponse
--
--         , putBucketVersioningResponseTest $
--             putBucketVersioningResponse
--
--         , putBucketWebsiteResponseTest $
--             putBucketWebsiteResponse
--
--         , putObjectResponseTest $
--             putObjectResponse
--
--         , putObjectACLResponseTest $
--             putObjectACLResponse
--
--         , restoreObjectResponseTest $
--             restoreObjectResponse
--
--         , uploadPartResponseTest $
--             uploadPartResponse
--
--         , uploadPartCopyResponseTest $
--             uploadPartCopyResponse
--
--           ]
--     ]

-- Requests

abortMultipartUploadTest :: AbortMultipartUpload -> TestTree
abortMultipartUploadTest = undefined

completeMultipartUploadTest :: CompleteMultipartUpload -> TestTree
completeMultipartUploadTest = undefined

copyObjectTest :: CopyObject -> TestTree
copyObjectTest = undefined

createBucketTest :: CreateBucket -> TestTree
createBucketTest = undefined

createMultipartUploadTest :: CreateMultipartUpload -> TestTree
createMultipartUploadTest = undefined

deleteBucketTest :: DeleteBucket -> TestTree
deleteBucketTest = undefined

deleteBucketCORSTest :: DeleteBucketCORS -> TestTree
deleteBucketCORSTest = undefined

deleteBucketLifecycleTest :: DeleteBucketLifecycle -> TestTree
deleteBucketLifecycleTest = undefined

deleteBucketPolicyTest :: DeleteBucketPolicy -> TestTree
deleteBucketPolicyTest = undefined

deleteBucketReplicationTest :: DeleteBucketReplication -> TestTree
deleteBucketReplicationTest = undefined

deleteBucketTaggingTest :: DeleteBucketTagging -> TestTree
deleteBucketTaggingTest = undefined

deleteBucketWebsiteTest :: DeleteBucketWebsite -> TestTree
deleteBucketWebsiteTest = undefined

deleteObjectTest :: DeleteObject -> TestTree
deleteObjectTest = undefined

deleteObjectsTest :: DeleteObjects -> TestTree
deleteObjectsTest = undefined

getBucketACLTest :: GetBucketACL -> TestTree
getBucketACLTest = undefined

getBucketCORSTest :: GetBucketCORS -> TestTree
getBucketCORSTest = undefined

getBucketLifecycleTest :: GetBucketLifecycle -> TestTree
getBucketLifecycleTest = undefined

getBucketLocationTest :: GetBucketLocation -> TestTree
getBucketLocationTest = undefined

getBucketLoggingTest :: GetBucketLogging -> TestTree
getBucketLoggingTest = undefined

getBucketNotificationConfigurationTest :: GetBucketNotificationConfiguration -> TestTree
getBucketNotificationConfigurationTest = undefined

getBucketPolicyTest :: GetBucketPolicy -> TestTree
getBucketPolicyTest = undefined

getBucketReplicationTest :: GetBucketReplication -> TestTree
getBucketReplicationTest = undefined

getBucketRequestPaymentTest :: GetBucketRequestPayment -> TestTree
getBucketRequestPaymentTest = undefined

getBucketTaggingTest :: GetBucketTagging -> TestTree
getBucketTaggingTest = undefined

getBucketVersioningTest :: GetBucketVersioning -> TestTree
getBucketVersioningTest = undefined

getBucketWebsiteTest :: GetBucketWebsite -> TestTree
getBucketWebsiteTest = undefined

getObjectTest :: GetObject -> TestTree
getObjectTest = undefined

getObjectACLTest :: GetObjectACL -> TestTree
getObjectACLTest = undefined

getObjectTorrentTest :: GetObjectTorrent -> TestTree
getObjectTorrentTest = undefined

headBucketTest :: HeadBucket -> TestTree
headBucketTest = undefined

headObjectTest :: HeadObject -> TestTree
headObjectTest = undefined

listBucketsTest :: ListBuckets -> TestTree
listBucketsTest = undefined

listMultipartUploadsTest :: ListMultipartUploads -> TestTree
listMultipartUploadsTest = undefined

listObjectVersionsTest :: ListObjectVersions -> TestTree
listObjectVersionsTest = undefined

listObjectsTest :: ListObjects -> TestTree
listObjectsTest = undefined

listPartsTest :: ListParts -> TestTree
listPartsTest = undefined

putBucketACLTest :: PutBucketACL -> TestTree
putBucketACLTest = undefined

putBucketCORSTest :: PutBucketCORS -> TestTree
putBucketCORSTest = undefined

putBucketLifecycleTest :: PutBucketLifecycle -> TestTree
putBucketLifecycleTest = undefined

putBucketLoggingTest :: PutBucketLogging -> TestTree
putBucketLoggingTest = undefined

putBucketNotificationConfigurationTest :: PutBucketNotificationConfiguration -> TestTree
putBucketNotificationConfigurationTest = undefined

putBucketPolicyTest :: PutBucketPolicy -> TestTree
putBucketPolicyTest = undefined

putBucketReplicationTest :: PutBucketReplication -> TestTree
putBucketReplicationTest = undefined

putBucketRequestPaymentTest :: PutBucketRequestPayment -> TestTree
putBucketRequestPaymentTest = undefined

putBucketTaggingTest :: PutBucketTagging -> TestTree
putBucketTaggingTest = undefined

putBucketVersioningTest :: PutBucketVersioning -> TestTree
putBucketVersioningTest = undefined

putBucketWebsiteTest :: PutBucketWebsite -> TestTree
putBucketWebsiteTest = undefined

putObjectTest :: PutObject -> TestTree
putObjectTest = undefined

putObjectACLTest :: PutObjectACL -> TestTree
putObjectACLTest = undefined

restoreObjectTest :: RestoreObject -> TestTree
restoreObjectTest = undefined

uploadPartTest :: UploadPart -> TestTree
uploadPartTest = undefined

uploadPartCopyTest :: UploadPartCopy -> TestTree
uploadPartCopyTest = undefined

-- Responses

abortMultipartUploadResponseTest :: AbortMultipartUploadResponse -> TestTree
abortMultipartUploadResponseTest = resp
    "abortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse"
    (Proxy :: Proxy AbortMultipartUpload)

completeMultipartUploadResponseTest :: CompleteMultipartUploadResponse -> TestTree
completeMultipartUploadResponseTest = resp
    "completeMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse"
    (Proxy :: Proxy CompleteMultipartUpload)

copyObjectResponseTest :: CopyObjectResponse -> TestTree
copyObjectResponseTest = resp
    "copyObjectResponse"
    "fixture/CopyObjectResponse"
    (Proxy :: Proxy CopyObject)

createBucketResponseTest :: CreateBucketResponse -> TestTree
createBucketResponseTest = resp
    "createBucketResponse"
    "fixture/CreateBucketResponse"
    (Proxy :: Proxy CreateBucket)

createMultipartUploadResponseTest :: CreateMultipartUploadResponse -> TestTree
createMultipartUploadResponseTest = resp
    "createMultipartUploadResponse"
    "fixture/CreateMultipartUploadResponse"
    (Proxy :: Proxy CreateMultipartUpload)

deleteBucketResponseTest :: DeleteBucketResponse -> TestTree
deleteBucketResponseTest = resp
    "deleteBucketResponse"
    "fixture/DeleteBucketResponse"
    (Proxy :: Proxy DeleteBucket)

deleteBucketCORSResponseTest :: DeleteBucketCORSResponse -> TestTree
deleteBucketCORSResponseTest = resp
    "deleteBucketCORSResponse"
    "fixture/DeleteBucketCORSResponse"
    (Proxy :: Proxy DeleteBucketCORS)

deleteBucketLifecycleResponseTest :: DeleteBucketLifecycleResponse -> TestTree
deleteBucketLifecycleResponseTest = resp
    "deleteBucketLifecycleResponse"
    "fixture/DeleteBucketLifecycleResponse"
    (Proxy :: Proxy DeleteBucketLifecycle)

deleteBucketPolicyResponseTest :: DeleteBucketPolicyResponse -> TestTree
deleteBucketPolicyResponseTest = resp
    "deleteBucketPolicyResponse"
    "fixture/DeleteBucketPolicyResponse"
    (Proxy :: Proxy DeleteBucketPolicy)

deleteBucketReplicationResponseTest :: DeleteBucketReplicationResponse -> TestTree
deleteBucketReplicationResponseTest = resp
    "deleteBucketReplicationResponse"
    "fixture/DeleteBucketReplicationResponse"
    (Proxy :: Proxy DeleteBucketReplication)

deleteBucketTaggingResponseTest :: DeleteBucketTaggingResponse -> TestTree
deleteBucketTaggingResponseTest = resp
    "deleteBucketTaggingResponse"
    "fixture/DeleteBucketTaggingResponse"
    (Proxy :: Proxy DeleteBucketTagging)

deleteBucketWebsiteResponseTest :: DeleteBucketWebsiteResponse -> TestTree
deleteBucketWebsiteResponseTest = resp
    "deleteBucketWebsiteResponse"
    "fixture/DeleteBucketWebsiteResponse"
    (Proxy :: Proxy DeleteBucketWebsite)

deleteObjectResponseTest :: DeleteObjectResponse -> TestTree
deleteObjectResponseTest = resp
    "deleteObjectResponse"
    "fixture/DeleteObjectResponse"
    (Proxy :: Proxy DeleteObject)

deleteObjectsResponseTest :: DeleteObjectsResponse -> TestTree
deleteObjectsResponseTest = resp
    "deleteObjectsResponse"
    "fixture/DeleteObjectsResponse"
    (Proxy :: Proxy DeleteObjects)

getBucketACLResponseTest :: GetBucketACLResponse -> TestTree
getBucketACLResponseTest = resp
    "getBucketACLResponse"
    "fixture/GetBucketACLResponse"
    (Proxy :: Proxy GetBucketACL)

getBucketCORSResponseTest :: GetBucketCORSResponse -> TestTree
getBucketCORSResponseTest = resp
    "getBucketCORSResponse"
    "fixture/GetBucketCORSResponse"
    (Proxy :: Proxy GetBucketCORS)

getBucketLifecycleResponseTest :: GetBucketLifecycleResponse -> TestTree
getBucketLifecycleResponseTest = resp
    "getBucketLifecycleResponse"
    "fixture/GetBucketLifecycleResponse"
    (Proxy :: Proxy GetBucketLifecycle)

getBucketLocationResponseTest :: GetBucketLocationResponse -> TestTree
getBucketLocationResponseTest = resp
    "getBucketLocationResponse"
    "fixture/GetBucketLocationResponse"
    (Proxy :: Proxy GetBucketLocation)

getBucketLoggingResponseTest :: GetBucketLoggingResponse -> TestTree
getBucketLoggingResponseTest = resp
    "getBucketLoggingResponse"
    "fixture/GetBucketLoggingResponse"
    (Proxy :: Proxy GetBucketLogging)

getBucketNotificationConfigurationResponseTest :: NotificationConfiguration -> TestTree
getBucketNotificationConfigurationResponseTest = resp
    "getBucketNotificationConfigurationResponse"
    "fixture/NotificationConfiguration"
    (Proxy :: Proxy GetBucketNotificationConfiguration)

getBucketPolicyResponseTest :: GetBucketPolicyResponse -> TestTree
getBucketPolicyResponseTest = resp
    "getBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse"
    (Proxy :: Proxy GetBucketPolicy)

getBucketReplicationResponseTest :: GetBucketReplicationResponse -> TestTree
getBucketReplicationResponseTest = resp
    "getBucketReplicationResponse"
    "fixture/GetBucketReplicationResponse"
    (Proxy :: Proxy GetBucketReplication)

getBucketRequestPaymentResponseTest :: GetBucketRequestPaymentResponse -> TestTree
getBucketRequestPaymentResponseTest = resp
    "getBucketRequestPaymentResponse"
    "fixture/GetBucketRequestPaymentResponse"
    (Proxy :: Proxy GetBucketRequestPayment)

getBucketTaggingResponseTest :: GetBucketTaggingResponse -> TestTree
getBucketTaggingResponseTest = resp
    "getBucketTaggingResponse"
    "fixture/GetBucketTaggingResponse"
    (Proxy :: Proxy GetBucketTagging)

getBucketVersioningResponseTest :: GetBucketVersioningResponse -> TestTree
getBucketVersioningResponseTest = resp
    "getBucketVersioningResponse"
    "fixture/GetBucketVersioningResponse"
    (Proxy :: Proxy GetBucketVersioning)

getBucketWebsiteResponseTest :: GetBucketWebsiteResponse -> TestTree
getBucketWebsiteResponseTest = resp
    "getBucketWebsiteResponse"
    "fixture/GetBucketWebsiteResponse"
    (Proxy :: Proxy GetBucketWebsite)

getObjectResponseTest :: GetObjectResponse -> TestTree
getObjectResponseTest = resp
    "getObjectResponse"
    "fixture/GetObjectResponse"
    (Proxy :: Proxy GetObject)

getObjectACLResponseTest :: GetObjectACLResponse -> TestTree
getObjectACLResponseTest = resp
    "getObjectACLResponse"
    "fixture/GetObjectACLResponse"
    (Proxy :: Proxy GetObjectACL)

getObjectTorrentResponseTest :: GetObjectTorrentResponse -> TestTree
getObjectTorrentResponseTest = resp
    "getObjectTorrentResponse"
    "fixture/GetObjectTorrentResponse"
    (Proxy :: Proxy GetObjectTorrent)

headBucketResponseTest :: HeadBucketResponse -> TestTree
headBucketResponseTest = resp
    "headBucketResponse"
    "fixture/HeadBucketResponse"
    (Proxy :: Proxy HeadBucket)

headObjectResponseTest :: HeadObjectResponse -> TestTree
headObjectResponseTest = resp
    "headObjectResponse"
    "fixture/HeadObjectResponse"
    (Proxy :: Proxy HeadObject)

listBucketsResponseTest :: ListBucketsResponse -> TestTree
listBucketsResponseTest = resp
    "listBucketsResponse"
    "fixture/ListBucketsResponse"
    (Proxy :: Proxy ListBuckets)

listMultipartUploadsResponseTest :: ListMultipartUploadsResponse -> TestTree
listMultipartUploadsResponseTest = resp
    "listMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse"
    (Proxy :: Proxy ListMultipartUploads)

listObjectVersionsResponseTest :: ListObjectVersionsResponse -> TestTree
listObjectVersionsResponseTest = resp
    "listObjectVersionsResponse"
    "fixture/ListObjectVersionsResponse"
    (Proxy :: Proxy ListObjectVersions)

listObjectsResponseTest :: ListObjectsResponse -> TestTree
listObjectsResponseTest = resp
    "listObjectsResponse"
    "fixture/ListObjectsResponse"
    (Proxy :: Proxy ListObjects)

listPartsResponseTest :: ListPartsResponse -> TestTree
listPartsResponseTest = resp
    "listPartsResponse"
    "fixture/ListPartsResponse"
    (Proxy :: Proxy ListParts)

putBucketACLResponseTest :: PutBucketACLResponse -> TestTree
putBucketACLResponseTest = resp
    "putBucketACLResponse"
    "fixture/PutBucketACLResponse"
    (Proxy :: Proxy PutBucketACL)

putBucketCORSResponseTest :: PutBucketCORSResponse -> TestTree
putBucketCORSResponseTest = resp
    "putBucketCORSResponse"
    "fixture/PutBucketCORSResponse"
    (Proxy :: Proxy PutBucketCORS)

putBucketLifecycleResponseTest :: PutBucketLifecycleResponse -> TestTree
putBucketLifecycleResponseTest = resp
    "putBucketLifecycleResponse"
    "fixture/PutBucketLifecycleResponse"
    (Proxy :: Proxy PutBucketLifecycle)

putBucketLoggingResponseTest :: PutBucketLoggingResponse -> TestTree
putBucketLoggingResponseTest = resp
    "putBucketLoggingResponse"
    "fixture/PutBucketLoggingResponse"
    (Proxy :: Proxy PutBucketLogging)

putBucketNotificationConfigurationResponseTest :: PutBucketNotificationConfigurationResponse -> TestTree
putBucketNotificationConfigurationResponseTest = resp
    "putBucketNotificationConfigurationResponse"
    "fixture/PutBucketNotificationConfigurationResponse"
    (Proxy :: Proxy PutBucketNotificationConfiguration)

putBucketPolicyResponseTest :: PutBucketPolicyResponse -> TestTree
putBucketPolicyResponseTest = resp
    "putBucketPolicyResponse"
    "fixture/PutBucketPolicyResponse"
    (Proxy :: Proxy PutBucketPolicy)

putBucketReplicationResponseTest :: PutBucketReplicationResponse -> TestTree
putBucketReplicationResponseTest = resp
    "putBucketReplicationResponse"
    "fixture/PutBucketReplicationResponse"
    (Proxy :: Proxy PutBucketReplication)

putBucketRequestPaymentResponseTest :: PutBucketRequestPaymentResponse -> TestTree
putBucketRequestPaymentResponseTest = resp
    "putBucketRequestPaymentResponse"
    "fixture/PutBucketRequestPaymentResponse"
    (Proxy :: Proxy PutBucketRequestPayment)

putBucketTaggingResponseTest :: PutBucketTaggingResponse -> TestTree
putBucketTaggingResponseTest = resp
    "putBucketTaggingResponse"
    "fixture/PutBucketTaggingResponse"
    (Proxy :: Proxy PutBucketTagging)

putBucketVersioningResponseTest :: PutBucketVersioningResponse -> TestTree
putBucketVersioningResponseTest = resp
    "putBucketVersioningResponse"
    "fixture/PutBucketVersioningResponse"
    (Proxy :: Proxy PutBucketVersioning)

putBucketWebsiteResponseTest :: PutBucketWebsiteResponse -> TestTree
putBucketWebsiteResponseTest = resp
    "putBucketWebsiteResponse"
    "fixture/PutBucketWebsiteResponse"
    (Proxy :: Proxy PutBucketWebsite)

putObjectResponseTest :: PutObjectResponse -> TestTree
putObjectResponseTest = resp
    "putObjectResponse"
    "fixture/PutObjectResponse"
    (Proxy :: Proxy PutObject)

putObjectACLResponseTest :: PutObjectACLResponse -> TestTree
putObjectACLResponseTest = resp
    "putObjectACLResponse"
    "fixture/PutObjectACLResponse"
    (Proxy :: Proxy PutObjectACL)

restoreObjectResponseTest :: RestoreObjectResponse -> TestTree
restoreObjectResponseTest = resp
    "restoreObjectResponse"
    "fixture/RestoreObjectResponse"
    (Proxy :: Proxy RestoreObject)

uploadPartResponseTest :: UploadPartResponse -> TestTree
uploadPartResponseTest = resp
    "uploadPartResponse"
    "fixture/UploadPartResponse"
    (Proxy :: Proxy UploadPart)

uploadPartCopyResponseTest :: UploadPartCopyResponse -> TestTree
uploadPartCopyResponseTest = resp
    "uploadPartCopyResponse"
    "fixture/UploadPartCopyResponse"
    (Proxy :: Proxy UploadPartCopy)
