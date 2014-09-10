{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Storage Service is storage for the Internet. Amazon S3 has a
-- simple web services interface that you can use to store and retrieve any
-- amount of data, at any time, from anywhere on the web. It gives any
-- developer access to the same highly scalable, reliable, fast, inexpensive
-- data storage infrastructure that Amazon uses to run its own global network
-- of web sites. The service aims to maximize benefits of scale and to pass
-- those benefits on to developers.
--
-- This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.S3" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.S3
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.S3.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Network.AWS.S3.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
-- @
--
module Network.AWS.S3.Monadic
    (
    -- * AbortMultipartUpload
    -- $AbortMultipartUpload
      abortMultipartUpload
    , abortMultipartUploadCatch

    -- * CompleteMultipartUpload
    -- $CompleteMultipartUpload
    , completeMultipartUpload
    , completeMultipartUploadCatch

    -- * CopyObject
    -- $CopyObject
    , copyObject
    , copyObjectCatch

    -- * CreateBucket
    -- $CreateBucket
    , createBucket
    , createBucketCatch

    -- * CreateMultipartUpload
    -- $CreateMultipartUpload
    , createMultipartUpload
    , createMultipartUploadCatch

    -- * DeleteBucket
    -- $DeleteBucket
    , deleteBucket
    , deleteBucketCatch

    -- * DeleteBucketCors
    -- $DeleteBucketCors
    , deleteBucketCors
    , deleteBucketCorsCatch

    -- * DeleteBucketLifecycle
    -- $DeleteBucketLifecycle
    , deleteBucketLifecycle
    , deleteBucketLifecycleCatch

    -- * DeleteBucketPolicy
    -- $DeleteBucketPolicy
    , deleteBucketPolicy
    , deleteBucketPolicyCatch

    -- * DeleteBucketTagging
    -- $DeleteBucketTagging
    , deleteBucketTagging
    , deleteBucketTaggingCatch

    -- * DeleteBucketWebsite
    -- $DeleteBucketWebsite
    , deleteBucketWebsite
    , deleteBucketWebsiteCatch

    -- * DeleteObject
    -- $DeleteObject
    , deleteObject
    , deleteObjectCatch

    -- * DeleteObjects
    -- $DeleteObjects
    , deleteObjects
    , deleteObjectsCatch

    -- * GetBucketAcl
    -- $GetBucketAcl
    , getBucketAcl
    , getBucketAclCatch

    -- * GetBucketCors
    -- $GetBucketCors
    , getBucketCors
    , getBucketCorsCatch

    -- * GetBucketLifecycle
    -- $GetBucketLifecycle
    , getBucketLifecycle
    , getBucketLifecycleCatch

    -- * GetBucketLocation
    -- $GetBucketLocation
    , getBucketLocation
    , getBucketLocationCatch

    -- * GetBucketLogging
    -- $GetBucketLogging
    , getBucketLogging
    , getBucketLoggingCatch

    -- * GetBucketNotification
    -- $GetBucketNotification
    , getBucketNotification
    , getBucketNotificationCatch

    -- * GetBucketPolicy
    -- $GetBucketPolicy
    , getBucketPolicy
    , getBucketPolicyCatch

    -- * GetBucketRequestPayment
    -- $GetBucketRequestPayment
    , getBucketRequestPayment
    , getBucketRequestPaymentCatch

    -- * GetBucketTagging
    -- $GetBucketTagging
    , getBucketTagging
    , getBucketTaggingCatch

    -- * GetBucketVersioning
    -- $GetBucketVersioning
    , getBucketVersioning
    , getBucketVersioningCatch

    -- * GetBucketWebsite
    -- $GetBucketWebsite
    , getBucketWebsite
    , getBucketWebsiteCatch

    -- * GetObject
    -- $GetObject
    , getObject
    , getObjectCatch

    -- * GetObjectAcl
    -- $GetObjectAcl
    , getObjectAcl
    , getObjectAclCatch

    -- * GetObjectTorrent
    -- $GetObjectTorrent
    , getObjectTorrent
    , getObjectTorrentCatch

    -- * HeadBucket
    -- $HeadBucket
    , headBucket
    , headBucketCatch

    -- * HeadObject
    -- $HeadObject
    , headObject
    , headObjectCatch

    -- * ListBuckets
    -- $ListBuckets
    , listBuckets
    , listBucketsCatch

    -- * ListMultipartUploads
    -- $ListMultipartUploads
    , listMultipartUploads
    , listMultipartUploadsCatch

    -- * ListObjectVersions
    -- $ListObjectVersions
    , listObjectVersions
    , listObjectVersionsCatch

    -- * ListObjects
    -- $ListObjects
    , listObjects
    , listObjectsCatch

    -- * ListParts
    -- $ListParts
    , listParts
    , listPartsCatch

    -- * PutBucketAcl
    -- $PutBucketAcl
    , putBucketAcl
    , putBucketAclCatch

    -- * PutBucketCors
    -- $PutBucketCors
    , putBucketCors
    , putBucketCorsCatch

    -- * PutBucketLifecycle
    -- $PutBucketLifecycle
    , putBucketLifecycle
    , putBucketLifecycleCatch

    -- * PutBucketLogging
    -- $PutBucketLogging
    , putBucketLogging
    , putBucketLoggingCatch

    -- * PutBucketNotification
    -- $PutBucketNotification
    , putBucketNotification
    , putBucketNotificationCatch

    -- * PutBucketPolicy
    -- $PutBucketPolicy
    , putBucketPolicy
    , putBucketPolicyCatch

    -- * PutBucketRequestPayment
    -- $PutBucketRequestPayment
    , putBucketRequestPayment
    , putBucketRequestPaymentCatch

    -- * PutBucketTagging
    -- $PutBucketTagging
    , putBucketTagging
    , putBucketTaggingCatch

    -- * PutBucketVersioning
    -- $PutBucketVersioning
    , putBucketVersioning
    , putBucketVersioningCatch

    -- * PutBucketWebsite
    -- $PutBucketWebsite
    , putBucketWebsite
    , putBucketWebsiteCatch

    -- * PutObject
    -- $PutObject
    , putObject
    , putObjectCatch

    -- * PutObjectAcl
    -- $PutObjectAcl
    , putObjectAcl
    , putObjectAclCatch

    -- * RestoreObject
    -- $RestoreObject
    , restoreObject
    , restoreObjectCatch

    -- * UploadPart
    -- $UploadPart
    , uploadPart
    , uploadPartCatch

    -- * UploadPartCopy
    -- $UploadPartCopy
    , uploadPartCopy
    , uploadPartCopyCatch

    -- * Re-exported
    , module Network.AWS.S3

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.S3

type ServiceEr = Er S3

-- $AbortMultipartUpload
-- Aborts a multipart upload. To verify that all parts have been removed, so
-- you don't get charged for the part storage, you should call the List Parts
-- operation and ensure the parts list is empty.
--
-- See: 'Network.AWS.S3'

abortMultipartUpload :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => BucketName -- ^ 'amuBucket'
    -> ObjectKey -- ^ 'amuKey'
    -> Text -- ^ 'amuUploadId'
    -> State AbortMultipartUpload a
    -> m AbortMultipartUploadResponse
abortMultipartUpload p1 p2 p3 s =
    send $ (mkAbortMultipartUpload p1 p2 p3) &~ s

abortMultipartUploadCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => BucketName -- ^ 'amuBucket'
    -> ObjectKey -- ^ 'amuKey'
    -> Text -- ^ 'amuUploadId'
    -> State AbortMultipartUpload a
    -> m (Either ServiceEr AbortMultipartUploadResponse)
abortMultipartUploadCatch p1 p2 p3 s =
    sendCatch $ (mkAbortMultipartUpload p1 p2 p3) &~ s

-- $CompleteMultipartUpload
-- Completes a multipart upload by assembling previously uploaded parts.
--
-- See: 'Network.AWS.S3'

completeMultipartUpload :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => BucketName -- ^ 'cmuBucket'
    -> ObjectKey -- ^ 'cmuKey'
    -> Text -- ^ 'cmuUploadId'
    -> State CompleteMultipartUpload a
    -> m CompleteMultipartUploadResponse
completeMultipartUpload p1 p2 p4 s =
    send $ (mkCompleteMultipartUpload p1 p2 p4) &~ s

completeMultipartUploadCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => BucketName -- ^ 'cmuBucket'
    -> ObjectKey -- ^ 'cmuKey'
    -> Text -- ^ 'cmuUploadId'
    -> State CompleteMultipartUpload a
    -> m (Either ServiceEr CompleteMultipartUploadResponse)
completeMultipartUploadCatch p1 p2 p4 s =
    sendCatch $ (mkCompleteMultipartUpload p1 p2 p4) &~ s

-- $CopyObject
-- Creates a copy of an object that is already stored in Amazon S3.
--
-- See: 'Network.AWS.S3'

copyObject :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => ObjectKey -- ^ 'coKey'
    -> BucketName -- ^ 'coBucket'
    -> Text -- ^ 'coCopySource'
    -> State CopyObject a
    -> m CopyObjectResponse
copyObject p18 p2 p8 s =
    send $ (mkCopyObject p18 p2 p8) &~ s

copyObjectCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => ObjectKey -- ^ 'coKey'
    -> BucketName -- ^ 'coBucket'
    -> Text -- ^ 'coCopySource'
    -> State CopyObject a
    -> m (Either ServiceEr CopyObjectResponse)
copyObjectCatch p18 p2 p8 s =
    sendCatch $ (mkCopyObject p18 p2 p8) &~ s

-- $CreateBucket
-- Creates a new bucket.
--
-- See: 'Network.AWS.S3'

createBucket :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => BucketName -- ^ 'cbBucket'
    -> State CreateBucket a
    -> m CreateBucketResponse
createBucket p2 s =
    send $ (mkCreateBucket p2) &~ s

createBucketCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => BucketName -- ^ 'cbBucket'
    -> State CreateBucket a
    -> m (Either ServiceEr CreateBucketResponse)
createBucketCatch p2 s =
    sendCatch $ (mkCreateBucket p2) &~ s

-- $CreateMultipartUpload
-- Initiates a multipart upload and returns an upload ID. Note: After you
-- initiate multipart upload and upload one or more parts, you must either
-- complete or abort multipart upload in order to stop getting charged for
-- storage of the uploaded parts. Only after you either complete or abort
-- multipart upload, Amazon S3 frees up the parts storage and stops charging
-- you for the parts storage.
--
-- See: 'Network.AWS.S3'

createMultipartUpload :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => ObjectKey -- ^ 'cmu2Key'
    -> BucketName -- ^ 'cmu2Bucket'
    -> State CreateMultipartUpload a
    -> m CreateMultipartUploadResponse
createMultipartUpload p13 p2 s =
    send $ (mkCreateMultipartUpload p13 p2) &~ s

createMultipartUploadCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => ObjectKey -- ^ 'cmu2Key'
    -> BucketName -- ^ 'cmu2Bucket'
    -> State CreateMultipartUpload a
    -> m (Either ServiceEr CreateMultipartUploadResponse)
createMultipartUploadCatch p13 p2 s =
    sendCatch $ (mkCreateMultipartUpload p13 p2) &~ s

-- $DeleteBucket
-- Deletes the bucket. All objects (including all object versions and Delete
-- Markers) in the bucket must be deleted before the bucket itself can be
-- deleted.
--
-- See: 'Network.AWS.S3'

deleteBucket :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => BucketName -- ^ 'dbBucket'
    -> State DeleteBucket a
    -> m DeleteBucketResponse
deleteBucket p1 s =
    send $ (mkDeleteBucket p1) &~ s

deleteBucketCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => BucketName -- ^ 'dbBucket'
    -> State DeleteBucket a
    -> m (Either ServiceEr DeleteBucketResponse)
deleteBucketCatch p1 s =
    sendCatch $ (mkDeleteBucket p1) &~ s

-- $DeleteBucketCors
-- Deletes the cors configuration information set for the bucket.
--
-- See: 'Network.AWS.S3'

deleteBucketCors :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'dbcBucket'
    -> State DeleteBucketCors a
    -> m DeleteBucketCorsResponse
deleteBucketCors p1 s =
    send $ (mkDeleteBucketCors p1) &~ s

deleteBucketCorsCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'dbcBucket'
    -> State DeleteBucketCors a
    -> m (Either ServiceEr DeleteBucketCorsResponse)
deleteBucketCorsCatch p1 s =
    sendCatch $ (mkDeleteBucketCors p1) &~ s

-- $DeleteBucketLifecycle
-- Deletes the lifecycle configuration from the bucket.
--
-- See: 'Network.AWS.S3'

deleteBucketLifecycle :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'dblBucket'
    -> State DeleteBucketLifecycle a
    -> m DeleteBucketLifecycleResponse
deleteBucketLifecycle p1 s =
    send $ (mkDeleteBucketLifecycle p1) &~ s

deleteBucketLifecycleCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => BucketName -- ^ 'dblBucket'
    -> State DeleteBucketLifecycle a
    -> m (Either ServiceEr DeleteBucketLifecycleResponse)
deleteBucketLifecycleCatch p1 s =
    sendCatch $ (mkDeleteBucketLifecycle p1) &~ s

-- $DeleteBucketPolicy
-- Deletes the policy from the bucket.
--
-- See: 'Network.AWS.S3'

deleteBucketPolicy :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => BucketName -- ^ 'dbpBucket'
    -> State DeleteBucketPolicy a
    -> m DeleteBucketPolicyResponse
deleteBucketPolicy p1 s =
    send $ (mkDeleteBucketPolicy p1) &~ s

deleteBucketPolicyCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => BucketName -- ^ 'dbpBucket'
    -> State DeleteBucketPolicy a
    -> m (Either ServiceEr DeleteBucketPolicyResponse)
deleteBucketPolicyCatch p1 s =
    sendCatch $ (mkDeleteBucketPolicy p1) &~ s

-- $DeleteBucketTagging
-- Deletes the tags from the bucket.
--
-- See: 'Network.AWS.S3'

deleteBucketTagging :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => BucketName -- ^ 'dbtBucket'
    -> State DeleteBucketTagging a
    -> m DeleteBucketTaggingResponse
deleteBucketTagging p1 s =
    send $ (mkDeleteBucketTagging p1) &~ s

deleteBucketTaggingCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => BucketName -- ^ 'dbtBucket'
    -> State DeleteBucketTagging a
    -> m (Either ServiceEr DeleteBucketTaggingResponse)
deleteBucketTaggingCatch p1 s =
    sendCatch $ (mkDeleteBucketTagging p1) &~ s

-- $DeleteBucketWebsite
-- This operation removes the website configuration from the bucket.
--
-- See: 'Network.AWS.S3'

deleteBucketWebsite :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => BucketName -- ^ 'dbwBucket'
    -> State DeleteBucketWebsite a
    -> m DeleteBucketWebsiteResponse
deleteBucketWebsite p1 s =
    send $ (mkDeleteBucketWebsite p1) &~ s

deleteBucketWebsiteCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => BucketName -- ^ 'dbwBucket'
    -> State DeleteBucketWebsite a
    -> m (Either ServiceEr DeleteBucketWebsiteResponse)
deleteBucketWebsiteCatch p1 s =
    sendCatch $ (mkDeleteBucketWebsite p1) &~ s

-- $DeleteObject
-- Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object. If there
-- isn't a null version, Amazon S3 does not remove any objects.
--
-- See: 'Network.AWS.S3'

deleteObject :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => BucketName -- ^ 'doBucket'
    -> ObjectKey -- ^ 'doKey'
    -> State DeleteObject a
    -> m DeleteObjectResponse
deleteObject p1 p2 s =
    send $ (mkDeleteObject p1 p2) &~ s

deleteObjectCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => BucketName -- ^ 'doBucket'
    -> ObjectKey -- ^ 'doKey'
    -> State DeleteObject a
    -> m (Either ServiceEr DeleteObjectResponse)
deleteObjectCatch p1 p2 s =
    sendCatch $ (mkDeleteObject p1 p2) &~ s

-- $DeleteObjects
-- This operation enables you to delete multiple objects from a bucket using a
-- single HTTP request. You may specify up to 1000 keys.
--
-- See: 'Network.AWS.S3'

deleteObjects :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => BucketName -- ^ 'do1Bucket'
    -> Delete -- ^ 'do1Delete'
    -> State DeleteObjects a
    -> m DeleteObjectsResponse
deleteObjects p1 p2 s =
    send $ (mkDeleteObjects p1 p2) &~ s

deleteObjectsCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => BucketName -- ^ 'do1Bucket'
    -> Delete -- ^ 'do1Delete'
    -> State DeleteObjects a
    -> m (Either ServiceEr DeleteObjectsResponse)
deleteObjectsCatch p1 p2 s =
    sendCatch $ (mkDeleteObjects p1 p2) &~ s

-- $GetBucketAcl
-- Gets the access control policy for the bucket.
--
-- See: 'Network.AWS.S3'

getBucketAcl :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => BucketName -- ^ 'gbaBucket'
    -> State GetBucketAcl a
    -> m GetBucketAclResponse
getBucketAcl p1 s =
    send $ (mkGetBucketAcl p1) &~ s

getBucketAclCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => BucketName -- ^ 'gbaBucket'
    -> State GetBucketAcl a
    -> m (Either ServiceEr GetBucketAclResponse)
getBucketAclCatch p1 s =
    sendCatch $ (mkGetBucketAcl p1) &~ s

-- $GetBucketCors
-- Returns the cors configuration for the bucket.
--
-- See: 'Network.AWS.S3'

getBucketCors :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => BucketName -- ^ 'gbcBucket'
    -> State GetBucketCors a
    -> m GetBucketCorsResponse
getBucketCors p1 s =
    send $ (mkGetBucketCors p1) &~ s

getBucketCorsCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => BucketName -- ^ 'gbcBucket'
    -> State GetBucketCors a
    -> m (Either ServiceEr GetBucketCorsResponse)
getBucketCorsCatch p1 s =
    sendCatch $ (mkGetBucketCors p1) &~ s

-- $GetBucketLifecycle
-- Returns the lifecycle configuration information set on the bucket.
--
-- See: 'Network.AWS.S3'

getBucketLifecycle :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => BucketName -- ^ 'gblBucket'
    -> State GetBucketLifecycle a
    -> m GetBucketLifecycleResponse
getBucketLifecycle p1 s =
    send $ (mkGetBucketLifecycle p1) &~ s

getBucketLifecycleCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => BucketName -- ^ 'gblBucket'
    -> State GetBucketLifecycle a
    -> m (Either ServiceEr GetBucketLifecycleResponse)
getBucketLifecycleCatch p1 s =
    sendCatch $ (mkGetBucketLifecycle p1) &~ s

-- $GetBucketLocation
-- Returns the region the bucket resides in.
--
-- See: 'Network.AWS.S3'

getBucketLocation :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => BucketName -- ^ 'gbl1Bucket'
    -> State GetBucketLocation a
    -> m GetBucketLocationResponse
getBucketLocation p1 s =
    send $ (mkGetBucketLocation p1) &~ s

getBucketLocationCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => BucketName -- ^ 'gbl1Bucket'
    -> State GetBucketLocation a
    -> m (Either ServiceEr GetBucketLocationResponse)
getBucketLocationCatch p1 s =
    sendCatch $ (mkGetBucketLocation p1) &~ s

-- $GetBucketLogging
-- Returns the logging status of a bucket and the permissions users have to
-- view and modify that status. To use GET, you must be the bucket owner.
--
-- See: 'Network.AWS.S3'

getBucketLogging :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'gbl2Bucket'
    -> State GetBucketLogging a
    -> m GetBucketLoggingResponse
getBucketLogging p1 s =
    send $ (mkGetBucketLogging p1) &~ s

getBucketLoggingCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'gbl2Bucket'
    -> State GetBucketLogging a
    -> m (Either ServiceEr GetBucketLoggingResponse)
getBucketLoggingCatch p1 s =
    sendCatch $ (mkGetBucketLogging p1) &~ s

-- $GetBucketNotification
-- Return the notification configuration of a bucket.
--
-- See: 'Network.AWS.S3'

getBucketNotification :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'gbnBucket'
    -> State GetBucketNotification a
    -> m GetBucketNotificationResponse
getBucketNotification p1 s =
    send $ (mkGetBucketNotification p1) &~ s

getBucketNotificationCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => BucketName -- ^ 'gbnBucket'
    -> State GetBucketNotification a
    -> m (Either ServiceEr GetBucketNotificationResponse)
getBucketNotificationCatch p1 s =
    sendCatch $ (mkGetBucketNotification p1) &~ s

-- $GetBucketPolicy
-- Returns the policy of a specified bucket.
--
-- See: 'Network.AWS.S3'

getBucketPolicy :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => BucketName -- ^ 'gbpBucket'
    -> State GetBucketPolicy a
    -> m GetBucketPolicyResponse
getBucketPolicy p1 s =
    send $ (mkGetBucketPolicy p1) &~ s

getBucketPolicyCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => BucketName -- ^ 'gbpBucket'
    -> State GetBucketPolicy a
    -> m (Either ServiceEr GetBucketPolicyResponse)
getBucketPolicyCatch p1 s =
    sendCatch $ (mkGetBucketPolicy p1) &~ s

-- $GetBucketRequestPayment
-- Returns the request payment configuration of a bucket.
--
-- See: 'Network.AWS.S3'

getBucketRequestPayment :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => BucketName -- ^ 'gbrpBucket'
    -> State GetBucketRequestPayment a
    -> m GetBucketRequestPaymentResponse
getBucketRequestPayment p1 s =
    send $ (mkGetBucketRequestPayment p1) &~ s

getBucketRequestPaymentCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => BucketName -- ^ 'gbrpBucket'
    -> State GetBucketRequestPayment a
    -> m (Either ServiceEr GetBucketRequestPaymentResponse)
getBucketRequestPaymentCatch p1 s =
    sendCatch $ (mkGetBucketRequestPayment p1) &~ s

-- $GetBucketTagging
-- Returns the tag set associated with the bucket.
--
-- See: 'Network.AWS.S3'

getBucketTagging :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'gbtBucket'
    -> State GetBucketTagging a
    -> m GetBucketTaggingResponse
getBucketTagging p1 s =
    send $ (mkGetBucketTagging p1) &~ s

getBucketTaggingCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'gbtBucket'
    -> State GetBucketTagging a
    -> m (Either ServiceEr GetBucketTaggingResponse)
getBucketTaggingCatch p1 s =
    sendCatch $ (mkGetBucketTagging p1) &~ s

-- $GetBucketVersioning
-- Returns the versioning state of a bucket.
--
-- See: 'Network.AWS.S3'

getBucketVersioning :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => BucketName -- ^ 'gbvBucket'
    -> State GetBucketVersioning a
    -> m GetBucketVersioningResponse
getBucketVersioning p1 s =
    send $ (mkGetBucketVersioning p1) &~ s

getBucketVersioningCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => BucketName -- ^ 'gbvBucket'
    -> State GetBucketVersioning a
    -> m (Either ServiceEr GetBucketVersioningResponse)
getBucketVersioningCatch p1 s =
    sendCatch $ (mkGetBucketVersioning p1) &~ s

-- $GetBucketWebsite
-- Returns the website configuration for a bucket.
--
-- See: 'Network.AWS.S3'

getBucketWebsite :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'gbwBucket'
    -> State GetBucketWebsite a
    -> m GetBucketWebsiteResponse
getBucketWebsite p1 s =
    send $ (mkGetBucketWebsite p1) &~ s

getBucketWebsiteCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'gbwBucket'
    -> State GetBucketWebsite a
    -> m (Either ServiceEr GetBucketWebsiteResponse)
getBucketWebsiteCatch p1 s =
    sendCatch $ (mkGetBucketWebsite p1) &~ s

-- $GetObject
-- Retrieves objects from Amazon S3.
--
-- See: 'Network.AWS.S3'

getObject :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => BucketName -- ^ 'goBucket'
    -> ObjectKey -- ^ 'goKey'
    -> State GetObject a
    -> m GetObjectResponse
getObject p1 p6 s =
    send $ (mkGetObject p1 p6) &~ s

getObjectCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => BucketName -- ^ 'goBucket'
    -> ObjectKey -- ^ 'goKey'
    -> State GetObject a
    -> m (Either ServiceEr GetObjectResponse)
getObjectCatch p1 p6 s =
    sendCatch $ (mkGetObject p1 p6) &~ s

-- $GetObjectAcl
-- Returns the access control list (ACL) of an object.
--
-- See: 'Network.AWS.S3'

getObjectAcl :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => BucketName -- ^ 'goaBucket'
    -> ObjectKey -- ^ 'goaKey'
    -> State GetObjectAcl a
    -> m GetObjectAclResponse
getObjectAcl p1 p2 s =
    send $ (mkGetObjectAcl p1 p2) &~ s

getObjectAclCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => BucketName -- ^ 'goaBucket'
    -> ObjectKey -- ^ 'goaKey'
    -> State GetObjectAcl a
    -> m (Either ServiceEr GetObjectAclResponse)
getObjectAclCatch p1 p2 s =
    sendCatch $ (mkGetObjectAcl p1 p2) &~ s

-- $GetObjectTorrent
-- Return torrent files from a bucket.
--
-- See: 'Network.AWS.S3'

getObjectTorrent :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'gotBucket'
    -> ObjectKey -- ^ 'gotKey'
    -> State GetObjectTorrent a
    -> m GetObjectTorrentResponse
getObjectTorrent p1 p2 s =
    send $ (mkGetObjectTorrent p1 p2) &~ s

getObjectTorrentCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'gotBucket'
    -> ObjectKey -- ^ 'gotKey'
    -> State GetObjectTorrent a
    -> m (Either ServiceEr GetObjectTorrentResponse)
getObjectTorrentCatch p1 p2 s =
    sendCatch $ (mkGetObjectTorrent p1 p2) &~ s

-- $HeadBucket
-- This operation is useful to determine if a bucket exists and you have
-- permission to access it.
--
-- See: 'Network.AWS.S3'

headBucket :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => BucketName -- ^ 'hbBucket'
    -> State HeadBucket a
    -> m HeadBucketResponse
headBucket p1 s =
    send $ (mkHeadBucket p1) &~ s

headBucketCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => BucketName -- ^ 'hbBucket'
    -> State HeadBucket a
    -> m (Either ServiceEr HeadBucketResponse)
headBucketCatch p1 s =
    sendCatch $ (mkHeadBucket p1) &~ s

-- $HeadObject
-- The HEAD operation retrieves metadata from an object without returning the
-- object itself. This operation is useful if you're only interested in an
-- object's metadata. To use HEAD, you must have READ access to the object.
--
-- See: 'Network.AWS.S3'

headObject :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => BucketName -- ^ 'hoBucket'
    -> ObjectKey -- ^ 'hoKey'
    -> State HeadObject a
    -> m HeadObjectResponse
headObject p1 p6 s =
    send $ (mkHeadObject p1 p6) &~ s

headObjectCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => BucketName -- ^ 'hoBucket'
    -> ObjectKey -- ^ 'hoKey'
    -> State HeadObject a
    -> m (Either ServiceEr HeadObjectResponse)
headObjectCatch p1 p6 s =
    sendCatch $ (mkHeadObject p1 p6) &~ s

-- $ListBuckets
-- Returns a list of all buckets owned by the authenticated sender of the
-- request.
--
-- See: 'Network.AWS.S3'

listBuckets :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => State ListBuckets a
    -> m ListBucketsResponse
listBuckets s =
    send (mkListBuckets &~ s)

listBucketsCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => State ListBuckets a
    -> m (Either ServiceEr ListBucketsResponse)
listBucketsCatch s =
    sendCatch (mkListBuckets &~ s)

-- $ListMultipartUploads
-- This operation lists in-progress multipart uploads.
--
-- See: 'Network.AWS.S3'

listMultipartUploads :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env (ResumableSource m)
                        )
    => BucketName -- ^ 'lmuBucket'
    -> State ListMultipartUploads a
    -> ResumableSource m ListMultipartUploadsResponse
listMultipartUploads p1 s =
    paginate $ (mkListMultipartUploads p1) &~ s

listMultipartUploadsCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env (ResumableSource m)
                             )
    => BucketName -- ^ 'lmuBucket'
    -> State ListMultipartUploads a
    -> ResumableSource m (Either ServiceEr ListMultipartUploadsResponse)
listMultipartUploadsCatch p1 s =
    paginateCatch $ (mkListMultipartUploads p1) &~ s

-- $ListObjectVersions
-- Returns metadata about all of the versions of objects in a bucket.
--
-- See: 'Network.AWS.S3'

listObjectVersions :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env (ResumableSource m)
                      )
    => BucketName -- ^ 'lovBucket'
    -> State ListObjectVersions a
    -> ResumableSource m ListObjectVersionsResponse
listObjectVersions p1 s =
    paginate $ (mkListObjectVersions p1) &~ s

listObjectVersionsCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env (ResumableSource m)
                           )
    => BucketName -- ^ 'lovBucket'
    -> State ListObjectVersions a
    -> ResumableSource m (Either ServiceEr ListObjectVersionsResponse)
listObjectVersionsCatch p1 s =
    paginateCatch $ (mkListObjectVersions p1) &~ s

-- $ListObjects
-- Returns some or all (up to 1000) of the objects in a bucket. You can use
-- the request parameters as selection criteria to return a subset of the
-- objects in a bucket.
--
-- See: 'Network.AWS.S3'

listObjects :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env (ResumableSource m)
               )
    => BucketName -- ^ 'loBucket'
    -> State ListObjects a
    -> ResumableSource m ListObjectsResponse
listObjects p1 s =
    paginate $ (mkListObjects p1) &~ s

listObjectsCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env (ResumableSource m)
                    )
    => BucketName -- ^ 'loBucket'
    -> State ListObjects a
    -> ResumableSource m (Either ServiceEr ListObjectsResponse)
listObjectsCatch p1 s =
    paginateCatch $ (mkListObjects p1) &~ s

-- $ListParts
-- Lists the parts that have been uploaded for a specific multipart upload.
--
-- See: 'Network.AWS.S3'

listParts :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env (ResumableSource m)
             )
    => BucketName -- ^ 'lpBucket'
    -> ObjectKey -- ^ 'lpKey'
    -> Text -- ^ 'lpUploadId'
    -> State ListParts a
    -> ResumableSource m ListPartsResponse
listParts p1 p2 p5 s =
    paginate $ (mkListParts p1 p2 p5) &~ s

listPartsCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env (ResumableSource m)
                  )
    => BucketName -- ^ 'lpBucket'
    -> ObjectKey -- ^ 'lpKey'
    -> Text -- ^ 'lpUploadId'
    -> State ListParts a
    -> ResumableSource m (Either ServiceEr ListPartsResponse)
listPartsCatch p1 p2 p5 s =
    paginateCatch $ (mkListParts p1 p2 p5) &~ s

-- $PutBucketAcl
-- Sets the permissions on a bucket using access control lists (ACL).
--
-- See: 'Network.AWS.S3'

putBucketAcl :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => BucketName -- ^ 'pbaBucket'
    -> State PutBucketAcl a
    -> m PutBucketAclResponse
putBucketAcl p3 s =
    send $ (mkPutBucketAcl p3) &~ s

putBucketAclCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => BucketName -- ^ 'pbaBucket'
    -> State PutBucketAcl a
    -> m (Either ServiceEr PutBucketAclResponse)
putBucketAclCatch p3 s =
    sendCatch $ (mkPutBucketAcl p3) &~ s

-- $PutBucketCors
-- Sets the cors configuration for a bucket.
--
-- See: 'Network.AWS.S3'

putBucketCors :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => BucketName -- ^ 'pbcBucket'
    -> State PutBucketCors a
    -> m PutBucketCorsResponse
putBucketCors p1 s =
    send $ (mkPutBucketCors p1) &~ s

putBucketCorsCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => BucketName -- ^ 'pbcBucket'
    -> State PutBucketCors a
    -> m (Either ServiceEr PutBucketCorsResponse)
putBucketCorsCatch p1 s =
    sendCatch $ (mkPutBucketCors p1) &~ s

-- $PutBucketLifecycle
-- Sets lifecycle configuration for your bucket. If a lifecycle configuration
-- exists, it replaces it.
--
-- See: 'Network.AWS.S3'

putBucketLifecycle :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => BucketName -- ^ 'pblBucket'
    -> State PutBucketLifecycle a
    -> m PutBucketLifecycleResponse
putBucketLifecycle p1 s =
    send $ (mkPutBucketLifecycle p1) &~ s

putBucketLifecycleCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => BucketName -- ^ 'pblBucket'
    -> State PutBucketLifecycle a
    -> m (Either ServiceEr PutBucketLifecycleResponse)
putBucketLifecycleCatch p1 s =
    sendCatch $ (mkPutBucketLifecycle p1) &~ s

-- $PutBucketLogging
-- Set the logging parameters for a bucket and to specify permissions for who
-- can view and modify the logging parameters. To set the logging status of a
-- bucket, you must be the bucket owner.
--
-- See: 'Network.AWS.S3'

putBucketLogging :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'pbl1Bucket'
    -> BucketLoggingStatus -- ^ 'pbl1BucketLoggingStatus'
    -> State PutBucketLogging a
    -> m PutBucketLoggingResponse
putBucketLogging p1 p2 s =
    send $ (mkPutBucketLogging p1 p2) &~ s

putBucketLoggingCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'pbl1Bucket'
    -> BucketLoggingStatus -- ^ 'pbl1BucketLoggingStatus'
    -> State PutBucketLogging a
    -> m (Either ServiceEr PutBucketLoggingResponse)
putBucketLoggingCatch p1 p2 s =
    sendCatch $ (mkPutBucketLogging p1 p2) &~ s

-- $PutBucketNotification
-- Enables notifications of specified events for a bucket.
--
-- See: 'Network.AWS.S3'

putBucketNotification :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'pbnBucket'
    -> NotificationConfiguration -- ^ 'pbnNotificationConfiguration'
    -> State PutBucketNotification a
    -> m PutBucketNotificationResponse
putBucketNotification p1 p3 s =
    send $ (mkPutBucketNotification p1 p3) &~ s

putBucketNotificationCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => BucketName -- ^ 'pbnBucket'
    -> NotificationConfiguration -- ^ 'pbnNotificationConfiguration'
    -> State PutBucketNotification a
    -> m (Either ServiceEr PutBucketNotificationResponse)
putBucketNotificationCatch p1 p3 s =
    sendCatch $ (mkPutBucketNotification p1 p3) &~ s

-- $PutBucketPolicy
-- Replaces a policy on a bucket. If the bucket already has a policy, the one
-- in this request completely replaces it.
--
-- See: 'Network.AWS.S3'

putBucketPolicy :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => BucketName -- ^ 'pbpBucket'
    -> Text -- ^ 'pbpPolicy'
    -> State PutBucketPolicy a
    -> m PutBucketPolicyResponse
putBucketPolicy p1 p3 s =
    send $ (mkPutBucketPolicy p1 p3) &~ s

putBucketPolicyCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => BucketName -- ^ 'pbpBucket'
    -> Text -- ^ 'pbpPolicy'
    -> State PutBucketPolicy a
    -> m (Either ServiceEr PutBucketPolicyResponse)
putBucketPolicyCatch p1 p3 s =
    sendCatch $ (mkPutBucketPolicy p1 p3) &~ s

-- $PutBucketRequestPayment
-- Sets the request payment configuration for a bucket. By default, the bucket
-- owner pays for downloads from the bucket. This configuration parameter
-- enables the bucket owner (only) to specify that the person requesting the
-- download will be charged for the download.
--
-- See: 'Network.AWS.S3'

putBucketRequestPayment :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => BucketName -- ^ 'pbrpBucket'
    -> RequestPaymentConfiguration -- ^ 'pbrpRequestPaymentConfiguration'
    -> State PutBucketRequestPayment a
    -> m PutBucketRequestPaymentResponse
putBucketRequestPayment p1 p3 s =
    send $ (mkPutBucketRequestPayment p1 p3) &~ s

putBucketRequestPaymentCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => BucketName -- ^ 'pbrpBucket'
    -> RequestPaymentConfiguration -- ^ 'pbrpRequestPaymentConfiguration'
    -> State PutBucketRequestPayment a
    -> m (Either ServiceEr PutBucketRequestPaymentResponse)
putBucketRequestPaymentCatch p1 p3 s =
    sendCatch $ (mkPutBucketRequestPayment p1 p3) &~ s

-- $PutBucketTagging
-- Sets the tags for a bucket.
--
-- See: 'Network.AWS.S3'

putBucketTagging :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'pbtBucket'
    -> Tagging -- ^ 'pbtTagging'
    -> State PutBucketTagging a
    -> m PutBucketTaggingResponse
putBucketTagging p1 p3 s =
    send $ (mkPutBucketTagging p1 p3) &~ s

putBucketTaggingCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'pbtBucket'
    -> Tagging -- ^ 'pbtTagging'
    -> State PutBucketTagging a
    -> m (Either ServiceEr PutBucketTaggingResponse)
putBucketTaggingCatch p1 p3 s =
    sendCatch $ (mkPutBucketTagging p1 p3) &~ s

-- $PutBucketVersioning
-- Sets the versioning state of an existing bucket. To set the versioning
-- state, you must be the bucket owner.
--
-- See: 'Network.AWS.S3'

putBucketVersioning :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => BucketName -- ^ 'pbvBucket'
    -> VersioningConfiguration -- ^ 'pbvVersioningConfiguration'
    -> State PutBucketVersioning a
    -> m PutBucketVersioningResponse
putBucketVersioning p1 p4 s =
    send $ (mkPutBucketVersioning p1 p4) &~ s

putBucketVersioningCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => BucketName -- ^ 'pbvBucket'
    -> VersioningConfiguration -- ^ 'pbvVersioningConfiguration'
    -> State PutBucketVersioning a
    -> m (Either ServiceEr PutBucketVersioningResponse)
putBucketVersioningCatch p1 p4 s =
    sendCatch $ (mkPutBucketVersioning p1 p4) &~ s

-- $PutBucketWebsite
-- Set the website configuration for a bucket.
--
-- See: 'Network.AWS.S3'

putBucketWebsite :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'pbwBucket'
    -> WebsiteConfiguration -- ^ 'pbwWebsiteConfiguration'
    -> State PutBucketWebsite a
    -> m PutBucketWebsiteResponse
putBucketWebsite p1 p3 s =
    send $ (mkPutBucketWebsite p1 p3) &~ s

putBucketWebsiteCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'pbwBucket'
    -> WebsiteConfiguration -- ^ 'pbwWebsiteConfiguration'
    -> State PutBucketWebsite a
    -> m (Either ServiceEr PutBucketWebsiteResponse)
putBucketWebsiteCatch p1 p3 s =
    sendCatch $ (mkPutBucketWebsite p1 p3) &~ s

-- $PutObject
-- Adds an object to a bucket.
--
-- See: 'Network.AWS.S3'

putObject :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => ObjectKey -- ^ 'poKey'
    -> RqBody -- ^ 'poBody'
    -> BucketName -- ^ 'poBucket'
    -> State PutObject a
    -> m PutObjectResponse
putObject p16 p2 p3 s =
    send $ (mkPutObject p16 p2 p3) &~ s

putObjectCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => ObjectKey -- ^ 'poKey'
    -> RqBody -- ^ 'poBody'
    -> BucketName -- ^ 'poBucket'
    -> State PutObject a
    -> m (Either ServiceEr PutObjectResponse)
putObjectCatch p16 p2 p3 s =
    sendCatch $ (mkPutObject p16 p2 p3) &~ s

-- $PutObjectAcl
-- uses the acl subresource to set the access control list (ACL) permissions
-- for an object that already exists in a bucket.
--
-- See: 'Network.AWS.S3'

putObjectAcl :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => ObjectKey -- ^ 'poaKey'
    -> BucketName -- ^ 'poaBucket'
    -> State PutObjectAcl a
    -> m PutObjectAclResponse
putObjectAcl p10 p3 s =
    send $ (mkPutObjectAcl p10 p3) &~ s

putObjectAclCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => ObjectKey -- ^ 'poaKey'
    -> BucketName -- ^ 'poaBucket'
    -> State PutObjectAcl a
    -> m (Either ServiceEr PutObjectAclResponse)
putObjectAclCatch p10 p3 s =
    sendCatch $ (mkPutObjectAcl p10 p3) &~ s

-- $RestoreObject
-- Restores an archived copy of an object back into Amazon S3.
--
-- See: 'Network.AWS.S3'

restoreObject :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => BucketName -- ^ 'roBucket'
    -> ObjectKey -- ^ 'roKey'
    -> State RestoreObject a
    -> m RestoreObjectResponse
restoreObject p1 p2 s =
    send $ (mkRestoreObject p1 p2) &~ s

restoreObjectCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => BucketName -- ^ 'roBucket'
    -> ObjectKey -- ^ 'roKey'
    -> State RestoreObject a
    -> m (Either ServiceEr RestoreObjectResponse)
restoreObjectCatch p1 p2 s =
    sendCatch $ (mkRestoreObject p1 p2) &~ s

-- $UploadPart
-- Uploads a part in a multipart upload. Note: After you initiate multipart
-- upload and upload one or more parts, you must either complete or abort
-- multipart upload in order to stop getting charged for storage of the
-- uploaded parts. Only after you either complete or abort multipart upload,
-- Amazon S3 frees up the parts storage and stops charging you for the parts
-- storage.
--
-- See: 'Network.AWS.S3'

uploadPart :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => RqBody -- ^ 'upBody'
    -> BucketName -- ^ 'upBucket'
    -> ObjectKey -- ^ 'upKey'
    -> Integer -- ^ 'upPartNumber'
    -> Text -- ^ 'upUploadId'
    -> State UploadPart a
    -> m UploadPartResponse
uploadPart p1 p2 p5 p6 p7 s =
    send $ (mkUploadPart p1 p2 p5 p6 p7) &~ s

uploadPartCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => RqBody -- ^ 'upBody'
    -> BucketName -- ^ 'upBucket'
    -> ObjectKey -- ^ 'upKey'
    -> Integer -- ^ 'upPartNumber'
    -> Text -- ^ 'upUploadId'
    -> State UploadPart a
    -> m (Either ServiceEr UploadPartResponse)
uploadPartCatch p1 p2 p5 p6 p7 s =
    sendCatch $ (mkUploadPart p1 p2 p5 p6 p7) &~ s

-- $UploadPartCopy
-- Uploads a part by copying data from an existing object as data source.
--
-- See: 'Network.AWS.S3'

uploadPartCopy :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => BucketName -- ^ 'upcBucket'
    -> Text -- ^ 'upcUploadId'
    -> Text -- ^ 'upcCopySource'
    -> ObjectKey -- ^ 'upcKey'
    -> Integer -- ^ 'upcPartNumber'
    -> State UploadPartCopy a
    -> m UploadPartCopyResponse
uploadPartCopy p1 p10 p2 p8 p9 s =
    send $ (mkUploadPartCopy p1 p10 p2 p8 p9) &~ s

uploadPartCopyCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => BucketName -- ^ 'upcBucket'
    -> Text -- ^ 'upcUploadId'
    -> Text -- ^ 'upcCopySource'
    -> ObjectKey -- ^ 'upcKey'
    -> Integer -- ^ 'upcPartNumber'
    -> State UploadPartCopy a
    -> m (Either ServiceEr UploadPartCopyResponse)
uploadPartCopyCatch p1 p10 p2 p8 p9 s =
    sendCatch $ (mkUploadPartCopy p1 p10 p2 p8 p9) &~ s
