{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.S3
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         , requestGetBucketMetricsConfiguration $
--             getBucketMetricsConfiguration
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
--         , requestGetBucketInventoryConfiguration $
--             getBucketInventoryConfiguration
--
--         , requestPutBucketInventoryConfiguration $
--             putBucketInventoryConfiguration
--
--         , requestGetBucketLocation $
--             getBucketLocation
--
--         , requestListBucketInventoryConfigurations $
--             listBucketInventoryConfigurations
--
--         , requestDeleteBucketInventoryConfiguration $
--             deleteBucketInventoryConfiguration
--
--         , requestGetBucketNotificationConfiguration $
--             getBucketNotificationConfiguration
--
--         , requestPutBucketAccelerateConfiguration $
--             putBucketAccelerateConfiguration
--
--         , requestPutBucketMetricsConfiguration $
--             putBucketMetricsConfiguration
--
--         , requestDeleteBucketMetricsConfiguration $
--             deleteBucketMetricsConfiguration
--
--         , requestListObjectsV2 $
--             listObjectsV2
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
--         , requestPutBucketAnalyticsConfiguration $
--             putBucketAnalyticsConfiguration
--
--         , requestListBucketAnalyticsConfigurations $
--             listBucketAnalyticsConfigurations
--
--         , requestDeleteBucketAnalyticsConfiguration $
--             deleteBucketAnalyticsConfiguration
--
--         , requestCreateMultipartUpload $
--             createMultipartUpload
--
--         , requestUploadPart $
--             uploadPart
--
--         , requestSelectObjectContent $
--             selectObjectContent
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
--         , requestGetBucketEncryption $
--             getBucketEncryption
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
--         , requestListBucketMetricsConfigurations $
--             listBucketMetricsConfigurations
--
--         , requestGetBucketPolicy $
--             getBucketPolicy
--
--         , requestPutBucketEncryption $
--             putBucketEncryption
--
--         , requestDeleteBucketEncryption $
--             deleteBucketEncryption
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
--         , requestGetBucketAnalyticsConfiguration $
--             getBucketAnalyticsConfiguration
--
--         , requestGetObjectTagging $
--             getObjectTagging
--
--         , requestListParts $
--             listParts
--
--         , requestDeleteObjectTagging $
--             deleteObjectTagging
--
--         , requestUploadPartCopy $
--             uploadPartCopy
--
--         , requestPutObjectTagging $
--             putObjectTagging
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
--         , responseGetBucketMetricsConfiguration $
--             getBucketMetricsConfigurationResponse
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
--         , responseGetBucketInventoryConfiguration $
--             getBucketInventoryConfigurationResponse
--
--         , responsePutBucketInventoryConfiguration $
--             putBucketInventoryConfigurationResponse
--
--         , responseGetBucketLocation $
--             getBucketLocationResponse
--
--         , responseListBucketInventoryConfigurations $
--             listBucketInventoryConfigurationsResponse
--
--         , responseDeleteBucketInventoryConfiguration $
--             deleteBucketInventoryConfigurationResponse
--
--         , responseGetBucketNotificationConfiguration $
--             notificationConfiguration
--
--         , responsePutBucketAccelerateConfiguration $
--             putBucketAccelerateConfigurationResponse
--
--         , responsePutBucketMetricsConfiguration $
--             putBucketMetricsConfigurationResponse
--
--         , responseDeleteBucketMetricsConfiguration $
--             deleteBucketMetricsConfigurationResponse
--
--         , responseListObjectsV2 $
--             listObjectsV2Response
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
--         , responsePutBucketAnalyticsConfiguration $
--             putBucketAnalyticsConfigurationResponse
--
--         , responseListBucketAnalyticsConfigurations $
--             listBucketAnalyticsConfigurationsResponse
--
--         , responseDeleteBucketAnalyticsConfiguration $
--             deleteBucketAnalyticsConfigurationResponse
--
--         , responseCreateMultipartUpload $
--             createMultipartUploadResponse
--
--         , responseUploadPart $
--             uploadPartResponse
--
--         , responseSelectObjectContent $
--             selectObjectContentResponse
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
--         , responseGetBucketEncryption $
--             getBucketEncryptionResponse
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
--         , responseListBucketMetricsConfigurations $
--             listBucketMetricsConfigurationsResponse
--
--         , responseGetBucketPolicy $
--             getBucketPolicyResponse
--
--         , responsePutBucketEncryption $
--             putBucketEncryptionResponse
--
--         , responseDeleteBucketEncryption $
--             deleteBucketEncryptionResponse
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
--         , responseGetBucketAnalyticsConfiguration $
--             getBucketAnalyticsConfigurationResponse
--
--         , responseGetObjectTagging $
--             getObjectTaggingResponse
--
--         , responseListParts $
--             listPartsResponse
--
--         , responseDeleteObjectTagging $
--             deleteObjectTaggingResponse
--
--         , responseUploadPartCopy $
--             uploadPartCopyResponse
--
--         , responsePutObjectTagging $
--             putObjectTaggingResponse
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

requestPutObjectACL :: PutObjectACL -> TestTree
requestPutObjectACL = req
    "PutObjectACL"
    "fixture/PutObjectACL.yaml"

requestPutBucketTagging :: PutBucketTagging -> TestTree
requestPutBucketTagging = req
    "PutBucketTagging"
    "fixture/PutBucketTagging.yaml"

requestGetBucketInventoryConfiguration :: GetBucketInventoryConfiguration -> TestTree
requestGetBucketInventoryConfiguration = req
    "GetBucketInventoryConfiguration"
    "fixture/GetBucketInventoryConfiguration.yaml"

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

requestDeleteBucketInventoryConfiguration :: DeleteBucketInventoryConfiguration -> TestTree
requestDeleteBucketInventoryConfiguration = req
    "DeleteBucketInventoryConfiguration"
    "fixture/DeleteBucketInventoryConfiguration.yaml"

requestGetBucketNotificationConfiguration :: GetBucketNotificationConfiguration -> TestTree
requestGetBucketNotificationConfiguration = req
    "GetBucketNotificationConfiguration"
    "fixture/GetBucketNotificationConfiguration.yaml"

requestPutBucketAccelerateConfiguration :: PutBucketAccelerateConfiguration -> TestTree
requestPutBucketAccelerateConfiguration = req
    "PutBucketAccelerateConfiguration"
    "fixture/PutBucketAccelerateConfiguration.yaml"

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

requestGetBucketACL :: GetBucketACL -> TestTree
requestGetBucketACL = req
    "GetBucketACL"
    "fixture/GetBucketACL.yaml"

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

responseGetBucketMetricsConfiguration :: GetBucketMetricsConfigurationResponse -> TestTree
responseGetBucketMetricsConfiguration = res
    "GetBucketMetricsConfigurationResponse"
    "fixture/GetBucketMetricsConfigurationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketMetricsConfiguration)

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

responseGetBucketInventoryConfiguration :: GetBucketInventoryConfigurationResponse -> TestTree
responseGetBucketInventoryConfiguration = res
    "GetBucketInventoryConfigurationResponse"
    "fixture/GetBucketInventoryConfigurationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketInventoryConfiguration)

responsePutBucketInventoryConfiguration :: PutBucketInventoryConfigurationResponse -> TestTree
responsePutBucketInventoryConfiguration = res
    "PutBucketInventoryConfigurationResponse"
    "fixture/PutBucketInventoryConfigurationResponse.proto"
    s3
    (Proxy :: Proxy PutBucketInventoryConfiguration)

responseGetBucketLocation :: GetBucketLocationResponse -> TestTree
responseGetBucketLocation = res
    "GetBucketLocationResponse"
    "fixture/GetBucketLocationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketLocation)

responseListBucketInventoryConfigurations :: ListBucketInventoryConfigurationsResponse -> TestTree
responseListBucketInventoryConfigurations = res
    "ListBucketInventoryConfigurationsResponse"
    "fixture/ListBucketInventoryConfigurationsResponse.proto"
    s3
    (Proxy :: Proxy ListBucketInventoryConfigurations)

responseDeleteBucketInventoryConfiguration :: DeleteBucketInventoryConfigurationResponse -> TestTree
responseDeleteBucketInventoryConfiguration = res
    "DeleteBucketInventoryConfigurationResponse"
    "fixture/DeleteBucketInventoryConfigurationResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketInventoryConfiguration)

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

responsePutBucketMetricsConfiguration :: PutBucketMetricsConfigurationResponse -> TestTree
responsePutBucketMetricsConfiguration = res
    "PutBucketMetricsConfigurationResponse"
    "fixture/PutBucketMetricsConfigurationResponse.proto"
    s3
    (Proxy :: Proxy PutBucketMetricsConfiguration)

responseDeleteBucketMetricsConfiguration :: DeleteBucketMetricsConfigurationResponse -> TestTree
responseDeleteBucketMetricsConfiguration = res
    "DeleteBucketMetricsConfigurationResponse"
    "fixture/DeleteBucketMetricsConfigurationResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketMetricsConfiguration)

responseListObjectsV2 :: ListObjectsV2Response -> TestTree
responseListObjectsV2 = res
    "ListObjectsV2Response"
    "fixture/ListObjectsV2Response.proto"
    s3
    (Proxy :: Proxy ListObjectsV2)

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

responsePutBucketAnalyticsConfiguration :: PutBucketAnalyticsConfigurationResponse -> TestTree
responsePutBucketAnalyticsConfiguration = res
    "PutBucketAnalyticsConfigurationResponse"
    "fixture/PutBucketAnalyticsConfigurationResponse.proto"
    s3
    (Proxy :: Proxy PutBucketAnalyticsConfiguration)

responseListBucketAnalyticsConfigurations :: ListBucketAnalyticsConfigurationsResponse -> TestTree
responseListBucketAnalyticsConfigurations = res
    "ListBucketAnalyticsConfigurationsResponse"
    "fixture/ListBucketAnalyticsConfigurationsResponse.proto"
    s3
    (Proxy :: Proxy ListBucketAnalyticsConfigurations)

responseDeleteBucketAnalyticsConfiguration :: DeleteBucketAnalyticsConfigurationResponse -> TestTree
responseDeleteBucketAnalyticsConfiguration = res
    "DeleteBucketAnalyticsConfigurationResponse"
    "fixture/DeleteBucketAnalyticsConfigurationResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketAnalyticsConfiguration)

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

responseSelectObjectContent :: SelectObjectContentResponse -> TestTree
responseSelectObjectContent = res
    "SelectObjectContentResponse"
    "fixture/SelectObjectContentResponse.proto"
    s3
    (Proxy :: Proxy SelectObjectContent)

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

responseGetBucketEncryption :: GetBucketEncryptionResponse -> TestTree
responseGetBucketEncryption = res
    "GetBucketEncryptionResponse"
    "fixture/GetBucketEncryptionResponse.proto"
    s3
    (Proxy :: Proxy GetBucketEncryption)

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

responseListBucketMetricsConfigurations :: ListBucketMetricsConfigurationsResponse -> TestTree
responseListBucketMetricsConfigurations = res
    "ListBucketMetricsConfigurationsResponse"
    "fixture/ListBucketMetricsConfigurationsResponse.proto"
    s3
    (Proxy :: Proxy ListBucketMetricsConfigurations)

responseGetBucketPolicy :: GetBucketPolicyResponse -> TestTree
responseGetBucketPolicy = res
    "GetBucketPolicyResponse"
    "fixture/GetBucketPolicyResponse.proto"
    s3
    (Proxy :: Proxy GetBucketPolicy)

responsePutBucketEncryption :: PutBucketEncryptionResponse -> TestTree
responsePutBucketEncryption = res
    "PutBucketEncryptionResponse"
    "fixture/PutBucketEncryptionResponse.proto"
    s3
    (Proxy :: Proxy PutBucketEncryption)

responseDeleteBucketEncryption :: DeleteBucketEncryptionResponse -> TestTree
responseDeleteBucketEncryption = res
    "DeleteBucketEncryptionResponse"
    "fixture/DeleteBucketEncryptionResponse.proto"
    s3
    (Proxy :: Proxy DeleteBucketEncryption)

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

responseGetBucketAnalyticsConfiguration :: GetBucketAnalyticsConfigurationResponse -> TestTree
responseGetBucketAnalyticsConfiguration = res
    "GetBucketAnalyticsConfigurationResponse"
    "fixture/GetBucketAnalyticsConfigurationResponse.proto"
    s3
    (Proxy :: Proxy GetBucketAnalyticsConfiguration)

responseGetObjectTagging :: GetObjectTaggingResponse -> TestTree
responseGetObjectTagging = res
    "GetObjectTaggingResponse"
    "fixture/GetObjectTaggingResponse.proto"
    s3
    (Proxy :: Proxy GetObjectTagging)

responseListParts :: ListPartsResponse -> TestTree
responseListParts = res
    "ListPartsResponse"
    "fixture/ListPartsResponse.proto"
    s3
    (Proxy :: Proxy ListParts)

responseDeleteObjectTagging :: DeleteObjectTaggingResponse -> TestTree
responseDeleteObjectTagging = res
    "DeleteObjectTaggingResponse"
    "fixture/DeleteObjectTaggingResponse.proto"
    s3
    (Proxy :: Proxy DeleteObjectTagging)

responseUploadPartCopy :: UploadPartCopyResponse -> TestTree
responseUploadPartCopy = res
    "UploadPartCopyResponse"
    "fixture/UploadPartCopyResponse.proto"
    s3
    (Proxy :: Proxy UploadPartCopy)

responsePutObjectTagging :: PutObjectTaggingResponse -> TestTree
responsePutObjectTagging = res
    "PutObjectTaggingResponse"
    "fixture/PutObjectTaggingResponse.proto"
    s3
    (Proxy :: Proxy PutObjectTagging)

responsePutBucketACL :: PutBucketACLResponse -> TestTree
responsePutBucketACL = res
    "PutBucketACLResponse"
    "fixture/PutBucketACLResponse.proto"
    s3
    (Proxy :: Proxy PutBucketACL)
