{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.S3
-- Copyright   : (c) 2013-2016 Brendan Hay
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
--         [ requestPutBucketRequestPayment $
--             putBucketRequestPayment
--
--         , requestPutObject $
--             putObject
--
--         , requestDeleteObject $
--             deleteObject
--
--         , requestPutBucketLogging $
--             putBucketLogging
--
--         , requestListBuckets $
--             listBuckets
--
--         , requestDeleteBucket $
--             deleteBucket
--
--         , requestCreateBucket $
--             createBucket
--
--         , requestDeleteBucketTagging $
--             deleteBucketTagging
--
--         , requestPutObjectACL $
--             putObjectACL
--
--         , requestPutBucketTagging $
--             putBucketTagging
--
--         , requestGetBucketLocation $
--             getBucketLocation
--
--         , requestGetBucketNotificationConfiguration $
--             getBucketNotificationConfiguration
--
--         , requestPutBucketAccelerateConfiguration $
--             putBucketAccelerateConfiguration
--
--         , requestListObjectsV $
--             listObjectsV
--
--         , requestGetObject $
--             getObject
--
--         , requestPutBucketReplication $
--             putBucketReplication
--
--         , requestGetBucketWebsite $
--             getBucketWebsite
--
--         , requestGetBucketRequestPayment $
--             getBucketRequestPayment
--
--         , requestDeleteBucketReplication $
--             deleteBucketReplication
--
--         , requestListObjectVersions $
--             listObjectVersions
--
--         , requestHeadBucket $
--             headBucket
--
--         , requestDeleteBucketLifecycle $
--             deleteBucketLifecycle
--
--         , requestPutBucketLifecycleConfiguration $
--             putBucketLifecycleConfiguration
--
--         , requestCreateMultipartUpload $
--             createMultipartUpload
--
--         , requestUploadPart $
--             uploadPart
--
--         , requestGetBucketReplication $
--             getBucketReplication
--
--         , requestPutBucketWebsite $
--             putBucketWebsite
--
--         , requestDeleteBucketWebsite $
--             deleteBucketWebsite
--
--         , requestCompleteMultipartUpload $
--             completeMultipartUpload
--
--         , requestListMultipartUploads $
--             listMultipartUploads
--
--         , requestListObjects $
--             listObjects
--
--         , requestDeleteBucketPolicy $
--             deleteBucketPolicy
--
--         , requestAbortMultipartUpload $
--             abortMultipartUpload
--
--         , requestPutBucketPolicy $
--             putBucketPolicy
--
--         , requestGetBucketAccelerateConfiguration $
--             getBucketAccelerateConfiguration
--
--         , requestGetObjectTorrent $
--             getObjectTorrent
--
--         , requestDeleteObjects $
--             deleteObjects
--
--         , requestPutBucketNotificationConfiguration $
--             putBucketNotificationConfiguration
--
--         , requestGetBucketVersioning $
--             getBucketVersioning
--
--         , requestDeleteBucketCORS $
--             deleteBucketCORS
--
--         , requestPutBucketCORS $
--             putBucketCORS
--
--         , requestGetBucketCORS $
--             getBucketCORS
--
--         , requestGetObjectACL $
--             getObjectACL
--
--         , requestRestoreObject $
--             restoreObject
--
--         , requestHeadObject $
--             headObject
--
--         , requestPutBucketVersioning $
--             putBucketVersioning
--
--         , requestGetBucketTagging $
--             getBucketTagging
--
--         , requestCopyObject $
--             copyObject
--
--         , requestGetBucketPolicy $
--             getBucketPolicy
--
--         , requestGetBucketLogging $
--             getBucketLogging
--
--         , requestGetBucketACL $
--             getBucketACL
--
--         , requestGetBucketLifecycleConfiguration $
--             getBucketLifecycleConfiguration
--
--         , requestListParts $
--             listParts
--
--         , requestUploadPartCopy $
--             uploadPartCopy
--
--         , requestPutBucketACL $
--             putBucketACL
--
--           ]

--     , testGroup "response"
--         [ responsePutBucketRequestPayment $
--             putBucketRequestPaymentResponse
--
--         , responsePutObject $
--             putObjectResponse
--
--         , responseDeleteObject $
--             deleteObjectResponse
--
--         , responsePutBucketLogging $
--             putBucketLoggingResponse
--
--         , responseListBuckets $
--             listBucketsResponse
--
--         , responseDeleteBucket $
--             deleteBucketResponse
--
--         , responseCreateBucket $
--             createBucketResponse
--
--         , responseDeleteBucketTagging $
--             deleteBucketTaggingResponse
--
--         , responsePutObjectACL $
--             putObjectACLResponse
--
--         , responsePutBucketTagging $
--             putBucketTaggingResponse
--
--         , responseGetBucketLocation $
--             getBucketLocationResponse
--
--         , responseGetBucketNotificationConfiguration $
--             notificationConfiguration
--
--         , responsePutBucketAccelerateConfiguration $
--             putBucketAccelerateConfigurationResponse
--
--         , responseListObjectsV $
--             listObjectsVResponse
--
--         , responseGetObject $
--             getObjectResponse
--
--         , responsePutBucketReplication $
--             putBucketReplicationResponse
--
--         , responseGetBucketWebsite $
--             getBucketWebsiteResponse
--
--         , responseGetBucketRequestPayment $
--             getBucketRequestPaymentResponse
--
--         , responseDeleteBucketReplication $
--             deleteBucketReplicationResponse
--
--         , responseListObjectVersions $
--             listObjectVersionsResponse
--
--         , responseHeadBucket $
--             headBucketResponse
--
--         , responseDeleteBucketLifecycle $
--             deleteBucketLifecycleResponse
--
--         , responsePutBucketLifecycleConfiguration $
--             putBucketLifecycleConfigurationResponse
--
--         , responseCreateMultipartUpload $
--             createMultipartUploadResponse
--
--         , responseUploadPart $
--             uploadPartResponse
--
--         , responseGetBucketReplication $
--             getBucketReplicationResponse
--
--         , responsePutBucketWebsite $
--             putBucketWebsiteResponse
--
--         , responseDeleteBucketWebsite $
--             deleteBucketWebsiteResponse
--
--         , responseCompleteMultipartUpload $
--             completeMultipartUploadResponse
--
--         , responseListMultipartUploads $
--             listMultipartUploadsResponse
--
--         , responseListObjects $
--             listObjectsResponse
--
--         , responseDeleteBucketPolicy $
--             deleteBucketPolicyResponse
--
--         , responseAbortMultipartUpload $
--             abortMultipartUploadResponse
--
--         , responsePutBucketPolicy $
--             putBucketPolicyResponse
--
--         , responseGetBucketAccelerateConfiguration $
--             getBucketAccelerateConfigurationResponse
--
--         , responseGetObjectTorrent $
--             getObjectTorrentResponse
--
--         , responseDeleteObjects $
--             deleteObjectsResponse
--
--         , responsePutBucketNotificationConfiguration $
--             putBucketNotificationConfigurationResponse
--
--         , responseGetBucketVersioning $
--             getBucketVersioningResponse
--
--         , responseDeleteBucketCORS $
--             deleteBucketCORSResponse
--
--         , responsePutBucketCORS $
--             putBucketCORSResponse
--
--         , responseGetBucketCORS $
--             getBucketCORSResponse
--
--         , responseGetObjectACL $
--             getObjectACLResponse
--
--         , responseRestoreObject $
--             restoreObjectResponse
--
--         , responseHeadObject $
--             headObjectResponse
--
--         , responsePutBucketVersioning $
--             putBucketVersioningResponse
--
--         , responseGetBucketTagging $
--             getBucketTaggingResponse
--
--         , responseCopyObject $
--             copyObjectResponse
--
--         , responseGetBucketPolicy $
--             getBucketPolicyResponse
--
--         , responseGetBucketLogging $
--             getBucketLoggingResponse
--
--         , responseGetBucketACL $
--             getBucketACLResponse
--
--         , responseGetBucketLifecycleConfiguration $
--             getBucketLifecycleConfigurationResponse
--
--         , responseListParts $
--             listPartsResponse
--
--         , responseUploadPartCopy $
--             uploadPartCopyResponse
--
--         , responsePutBucketACL $
--             putBucketACLResponse
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

requestPutObjectACL :: PutObjectACL -> TestTree
requestPutObjectACL = req
    "PutObjectACL"
    "fixture/PutObjectACL.yaml"

requestPutBucketTagging :: PutBucketTagging -> TestTree
requestPutBucketTagging = req
    "PutBucketTagging"
    "fixture/PutBucketTagging.yaml"

requestGetBucketLocation :: GetBucketLocation -> TestTree
requestGetBucketLocation = req
    "GetBucketLocation"
    "fixture/GetBucketLocation.yaml"

requestGetBucketNotificationConfiguration :: GetBucketNotificationConfiguration -> TestTree
requestGetBucketNotificationConfiguration = req
    "GetBucketNotificationConfiguration"
    "fixture/GetBucketNotificationConfiguration.yaml"

requestPutBucketAccelerateConfiguration :: PutBucketAccelerateConfiguration -> TestTree
requestPutBucketAccelerateConfiguration = req
    "PutBucketAccelerateConfiguration"
    "fixture/PutBucketAccelerateConfiguration.yaml"

requestListObjectsV :: ListObjectsV -> TestTree
requestListObjectsV = req
    "ListObjectsV"
    "fixture/ListObjectsV.yaml"

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

requestCreateMultipartUpload :: CreateMultipartUpload -> TestTree
requestCreateMultipartUpload = req
    "CreateMultipartUpload"
    "fixture/CreateMultipartUpload.yaml"

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

requestDeleteBucketPolicy :: DeleteBucketPolicy -> TestTree
requestDeleteBucketPolicy = req
    "DeleteBucketPolicy"
    "fixture/DeleteBucketPolicy.yaml"

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

requestPutBucketNotificationConfiguration :: PutBucketNotificationConfiguration -> TestTree
requestPutBucketNotificationConfiguration = req
    "PutBucketNotificationConfiguration"
    "fixture/PutBucketNotificationConfiguration.yaml"

requestGetBucketVersioning :: GetBucketVersioning -> TestTree
requestGetBucketVersioning = req
    "GetBucketVersioning"
    "fixture/GetBucketVersioning.yaml"

requestDeleteBucketCORS :: DeleteBucketCORS -> TestTree
requestDeleteBucketCORS = req
    "DeleteBucketCORS"
    "fixture/DeleteBucketCORS.yaml"

requestPutBucketCORS :: PutBucketCORS -> TestTree
requestPutBucketCORS = req
    "PutBucketCORS"
    "fixture/PutBucketCORS.yaml"

requestGetBucketCORS :: GetBucketCORS -> TestTree
requestGetBucketCORS = req
    "GetBucketCORS"
    "fixture/GetBucketCORS.yaml"

requestGetObjectACL :: GetObjectACL -> TestTree
requestGetObjectACL = req
    "GetObjectACL"
    "fixture/GetObjectACL.yaml"

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

requestGetBucketPolicy :: GetBucketPolicy -> TestTree
requestGetBucketPolicy = req
    "GetBucketPolicy"
    "fixture/GetBucketPolicy.yaml"

requestGetBucketLogging :: GetBucketLogging -> TestTree
requestGetBucketLogging = req
    "GetBucketLogging"
    "fixture/GetBucketLogging.yaml"

requestGetBucketACL :: GetBucketACL -> TestTree
requestGetBucketACL = req
    "GetBucketACL"
    "fixture/GetBucketACL.yaml"

requestGetBucketLifecycleConfiguration :: GetBucketLifecycleConfiguration -> TestTree
requestGetBucketLifecycleConfiguration = req
    "GetBucketLifecycleConfiguration"
    "fixture/GetBucketLifecycleConfiguration.yaml"

requestListParts :: ListParts -> TestTree
requestListParts = req
    "ListParts"
    "fixture/ListParts.yaml"

requestUploadPartCopy :: UploadPartCopy -> TestTree
requestUploadPartCopy = req
    "UploadPartCopy"
    "fixture/UploadPartCopy.yaml"

requestPutBucketACL :: PutBucketACL -> TestTree
requestPutBucketACL = req
    "PutBucketACL"
    "fixture/PutBucketACL.yaml"

-- Responses

responsePutBucketRequestPayment :: PutBucketRequestPaymentResponse -> TestTree
responsePutBucketRequestPayment = res
    "PutBucketRequestPaymentResponse"
    "fixture/PutBucketRequestPaymentResponse.proto"
    s3
    (Proxy :: Proxy PutBucketRequestPayment)

responsePutObject :: PutObjectResponse -> TestTree
responsePutObject = res
    "PutObjectResponse"
    "fixture/PutObjectResponse.proto"
    s3
    (Proxy :: Proxy PutObject)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject = res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    s3
    (Proxy :: Proxy DeleteObject)

responsePutBucketLogging :: PutBucketLoggingResponse -> TestTree
responsePutBucketLogging = res
    "PutBucketLoggingResponse"
    "fixture/PutBucketLoggingResponse.proto"
    s3
    (Proxy :: Proxy PutBucketLogging)

responseListBuckets :: ListBucketsResponse -> TestTree
responseListBuckets = res
    "ListBucketsResponse"
    "fixture/ListBucketsResponse.proto"
    s3
    (Proxy :: Proxy ListBuckets)

responseDeleteBucket :: DeleteBucketResponse -> TestTree
responseDeleteBucket = res
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucket)

responseCreateBucket :: CreateBucketResponse -> TestTree
responseCreateBucket = res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse.proto"
    s3
    (Proxy :: Proxy CreateBucket)

responseDeleteBucketTagging :: DeleteBucketTaggingResponse -> TestTree
responseDeleteBucketTagging = res
    "DeleteBucketTaggingResponse"
    "fixture/DeleteBucketTaggingResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketTagging)

responsePutObjectACL :: PutObjectACLResponse -> TestTree
responsePutObjectACL = res
    "PutObjectACLResponse"
    "fixture/PutObjectACLResponse.proto"
    s3
    (Proxy :: Proxy PutObjectACL)

responsePutBucketTagging :: PutBucketTaggingResponse -> TestTree
responsePutBucketTagging = res
    "PutBucketTaggingResponse"
    "fixture/PutBucketTaggingResponse.proto"
    s3
    (Proxy :: Proxy PutBucketTagging)

responseGetBucketLocation :: GetBucketLocationResponse -> TestTree
responseGetBucketLocation = res
    "GetBucketLocationResponse"
    "fixture/GetBucketLocationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketLocation)

responseGetBucketNotificationConfiguration :: NotificationConfiguration -> TestTree
responseGetBucketNotificationConfiguration = res
    "GetBucketNotificationConfigurationResponse"
    "fixture/GetBucketNotificationConfigurationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketNotificationConfiguration)

responsePutBucketAccelerateConfiguration :: PutBucketAccelerateConfigurationResponse -> TestTree
responsePutBucketAccelerateConfiguration = res
    "PutBucketAccelerateConfigurationResponse"
    "fixture/PutBucketAccelerateConfigurationResponse.proto"
    s3
    (Proxy :: Proxy PutBucketAccelerateConfiguration)

responseListObjectsV :: ListObjectsVResponse -> TestTree
responseListObjectsV = res
    "ListObjectsVResponse"
    "fixture/ListObjectsVResponse.proto"
    s3
    (Proxy :: Proxy ListObjectsV)

responsePutBucketReplication :: PutBucketReplicationResponse -> TestTree
responsePutBucketReplication = res
    "PutBucketReplicationResponse"
    "fixture/PutBucketReplicationResponse.proto"
    s3
    (Proxy :: Proxy PutBucketReplication)

responseGetBucketWebsite :: GetBucketWebsiteResponse -> TestTree
responseGetBucketWebsite = res
    "GetBucketWebsiteResponse"
    "fixture/GetBucketWebsiteResponse.proto"
    s3
    (Proxy :: Proxy GetBucketWebsite)

responseGetBucketRequestPayment :: GetBucketRequestPaymentResponse -> TestTree
responseGetBucketRequestPayment = res
    "GetBucketRequestPaymentResponse"
    "fixture/GetBucketRequestPaymentResponse.proto"
    s3
    (Proxy :: Proxy GetBucketRequestPayment)

responseDeleteBucketReplication :: DeleteBucketReplicationResponse -> TestTree
responseDeleteBucketReplication = res
    "DeleteBucketReplicationResponse"
    "fixture/DeleteBucketReplicationResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketReplication)

responseListObjectVersions :: ListObjectVersionsResponse -> TestTree
responseListObjectVersions = res
    "ListObjectVersionsResponse"
    "fixture/ListObjectVersionsResponse.proto"
    s3
    (Proxy :: Proxy ListObjectVersions)

responseHeadBucket :: HeadBucketResponse -> TestTree
responseHeadBucket = res
    "HeadBucketResponse"
    "fixture/HeadBucketResponse.proto"
    s3
    (Proxy :: Proxy HeadBucket)

responseDeleteBucketLifecycle :: DeleteBucketLifecycleResponse -> TestTree
responseDeleteBucketLifecycle = res
    "DeleteBucketLifecycleResponse"
    "fixture/DeleteBucketLifecycleResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketLifecycle)

responsePutBucketLifecycleConfiguration :: PutBucketLifecycleConfigurationResponse -> TestTree
responsePutBucketLifecycleConfiguration = res
    "PutBucketLifecycleConfigurationResponse"
    "fixture/PutBucketLifecycleConfigurationResponse.proto"
    s3
    (Proxy :: Proxy PutBucketLifecycleConfiguration)

responseCreateMultipartUpload :: CreateMultipartUploadResponse -> TestTree
responseCreateMultipartUpload = res
    "CreateMultipartUploadResponse"
    "fixture/CreateMultipartUploadResponse.proto"
    s3
    (Proxy :: Proxy CreateMultipartUpload)

responseUploadPart :: UploadPartResponse -> TestTree
responseUploadPart = res
    "UploadPartResponse"
    "fixture/UploadPartResponse.proto"
    s3
    (Proxy :: Proxy UploadPart)

responseGetBucketReplication :: GetBucketReplicationResponse -> TestTree
responseGetBucketReplication = res
    "GetBucketReplicationResponse"
    "fixture/GetBucketReplicationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketReplication)

responsePutBucketWebsite :: PutBucketWebsiteResponse -> TestTree
responsePutBucketWebsite = res
    "PutBucketWebsiteResponse"
    "fixture/PutBucketWebsiteResponse.proto"
    s3
    (Proxy :: Proxy PutBucketWebsite)

responseDeleteBucketWebsite :: DeleteBucketWebsiteResponse -> TestTree
responseDeleteBucketWebsite = res
    "DeleteBucketWebsiteResponse"
    "fixture/DeleteBucketWebsiteResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketWebsite)

responseCompleteMultipartUpload :: CompleteMultipartUploadResponse -> TestTree
responseCompleteMultipartUpload = res
    "CompleteMultipartUploadResponse"
    "fixture/CompleteMultipartUploadResponse.proto"
    s3
    (Proxy :: Proxy CompleteMultipartUpload)

responseListMultipartUploads :: ListMultipartUploadsResponse -> TestTree
responseListMultipartUploads = res
    "ListMultipartUploadsResponse"
    "fixture/ListMultipartUploadsResponse.proto"
    s3
    (Proxy :: Proxy ListMultipartUploads)

responseListObjects :: ListObjectsResponse -> TestTree
responseListObjects = res
    "ListObjectsResponse"
    "fixture/ListObjectsResponse.proto"
    s3
    (Proxy :: Proxy ListObjects)

responseDeleteBucketPolicy :: DeleteBucketPolicyResponse -> TestTree
responseDeleteBucketPolicy = res
    "DeleteBucketPolicyResponse"
    "fixture/DeleteBucketPolicyResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketPolicy)

responseAbortMultipartUpload :: AbortMultipartUploadResponse -> TestTree
responseAbortMultipartUpload = res
    "AbortMultipartUploadResponse"
    "fixture/AbortMultipartUploadResponse.proto"
    s3
    (Proxy :: Proxy AbortMultipartUpload)

responsePutBucketPolicy :: PutBucketPolicyResponse -> TestTree
responsePutBucketPolicy = res
    "PutBucketPolicyResponse"
    "fixture/PutBucketPolicyResponse.proto"
    s3
    (Proxy :: Proxy PutBucketPolicy)

responseGetBucketAccelerateConfiguration :: GetBucketAccelerateConfigurationResponse -> TestTree
responseGetBucketAccelerateConfiguration = res
    "GetBucketAccelerateConfigurationResponse"
    "fixture/GetBucketAccelerateConfigurationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketAccelerateConfiguration)

responseDeleteObjects :: DeleteObjectsResponse -> TestTree
responseDeleteObjects = res
    "DeleteObjectsResponse"
    "fixture/DeleteObjectsResponse.proto"
    s3
    (Proxy :: Proxy DeleteObjects)

responsePutBucketNotificationConfiguration :: PutBucketNotificationConfigurationResponse -> TestTree
responsePutBucketNotificationConfiguration = res
    "PutBucketNotificationConfigurationResponse"
    "fixture/PutBucketNotificationConfigurationResponse.proto"
    s3
    (Proxy :: Proxy PutBucketNotificationConfiguration)

responseGetBucketVersioning :: GetBucketVersioningResponse -> TestTree
responseGetBucketVersioning = res
    "GetBucketVersioningResponse"
    "fixture/GetBucketVersioningResponse.proto"
    s3
    (Proxy :: Proxy GetBucketVersioning)

responseDeleteBucketCORS :: DeleteBucketCORSResponse -> TestTree
responseDeleteBucketCORS = res
    "DeleteBucketCORSResponse"
    "fixture/DeleteBucketCORSResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketCORS)

responsePutBucketCORS :: PutBucketCORSResponse -> TestTree
responsePutBucketCORS = res
    "PutBucketCORSResponse"
    "fixture/PutBucketCORSResponse.proto"
    s3
    (Proxy :: Proxy PutBucketCORS)

responseGetBucketCORS :: GetBucketCORSResponse -> TestTree
responseGetBucketCORS = res
    "GetBucketCORSResponse"
    "fixture/GetBucketCORSResponse.proto"
    s3
    (Proxy :: Proxy GetBucketCORS)

responseGetObjectACL :: GetObjectACLResponse -> TestTree
responseGetObjectACL = res
    "GetObjectACLResponse"
    "fixture/GetObjectACLResponse.proto"
    s3
    (Proxy :: Proxy GetObjectACL)

responseRestoreObject :: RestoreObjectResponse -> TestTree
responseRestoreObject = res
    "RestoreObjectResponse"
    "fixture/RestoreObjectResponse.proto"
    s3
    (Proxy :: Proxy RestoreObject)

responseHeadObject :: HeadObjectResponse -> TestTree
responseHeadObject = res
    "HeadObjectResponse"
    "fixture/HeadObjectResponse.proto"
    s3
    (Proxy :: Proxy HeadObject)

responsePutBucketVersioning :: PutBucketVersioningResponse -> TestTree
responsePutBucketVersioning = res
    "PutBucketVersioningResponse"
    "fixture/PutBucketVersioningResponse.proto"
    s3
    (Proxy :: Proxy PutBucketVersioning)

responseGetBucketTagging :: GetBucketTaggingResponse -> TestTree
responseGetBucketTagging = res
    "GetBucketTaggingResponse"
    "fixture/GetBucketTaggingResponse.proto"
    s3
    (Proxy :: Proxy GetBucketTagging)

responseCopyObject :: CopyObjectResponse -> TestTree
responseCopyObject = res
    "CopyObjectResponse"
    "fixture/CopyObjectResponse.proto"
    s3
    (Proxy :: Proxy CopyObject)

responseGetBucketPolicy :: GetBucketPolicyResponse -> TestTree
responseGetBucketPolicy = res
    "GetBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse.proto"
    s3
    (Proxy :: Proxy GetBucketPolicy)

responseGetBucketLogging :: GetBucketLoggingResponse -> TestTree
responseGetBucketLogging = res
    "GetBucketLoggingResponse"
    "fixture/GetBucketLoggingResponse.proto"
    s3
    (Proxy :: Proxy GetBucketLogging)

responseGetBucketACL :: GetBucketACLResponse -> TestTree
responseGetBucketACL = res
    "GetBucketACLResponse"
    "fixture/GetBucketACLResponse.proto"
    s3
    (Proxy :: Proxy GetBucketACL)

responseGetBucketLifecycleConfiguration :: GetBucketLifecycleConfigurationResponse -> TestTree
responseGetBucketLifecycleConfiguration = res
    "GetBucketLifecycleConfigurationResponse"
    "fixture/GetBucketLifecycleConfigurationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketLifecycleConfiguration)

responseListParts :: ListPartsResponse -> TestTree
responseListParts = res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    s3
    (Proxy :: Proxy ListParts)

responseUploadPartCopy :: UploadPartCopyResponse -> TestTree
responseUploadPartCopy = res
    "UploadPartCopyResponse"
    "fixture/UploadPartCopyResponse.proto"
    s3
    (Proxy :: Proxy UploadPartCopy)

responsePutBucketACL :: PutBucketACLResponse -> TestTree
responsePutBucketACL = res
    "PutBucketACLResponse"
    "fixture/PutBucketACLResponse.proto"
    s3
    (Proxy :: Proxy PutBucketACL)
