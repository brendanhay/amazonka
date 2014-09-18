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

-- | This module is provided for convenience. It offers an alternative to the
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
-- import Control.Applicative
-- import Network.AWS.S3.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
--
-- -- Or to void any additional parameters outside of those required using @return ()@:
-- operationName w x $ return ()
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


-- $AbortMultipartUpload
-- Aborts a multipart upload. To verify that all parts have been removed, so
-- you don't get charged for the part storage, you should call the List Parts
-- operation and ensure the parts list is empty.
--
-- See: 'Network.AWS.S3.AbortMultipartUpload'

abortMultipartUpload :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => BucketName -- ^ 'amuBucket'
    -> ObjectKey -- ^ 'amuKey'
    -> Text -- ^ 'amuUploadId'
    -> m AbortMultipartUploadResponse
abortMultipartUpload p1 p2 p3 =
    send (mkAbortMultipartUpload p1 p2 p3)

abortMultipartUploadCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => BucketName -- ^ 'amuBucket'
    -> ObjectKey -- ^ 'amuKey'
    -> Text -- ^ 'amuUploadId'
    -> m (Either S3Error AbortMultipartUploadResponse)
abortMultipartUploadCatch p1 p2 p3 =
    sendCatch (mkAbortMultipartUpload p1 p2 p3)

-- $CompleteMultipartUpload
-- Completes a multipart upload by assembling previously uploaded parts.
--
-- See: 'Network.AWS.S3.CompleteMultipartUpload'

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
    -> m (Either S3Error CompleteMultipartUploadResponse)
completeMultipartUploadCatch p1 p2 p4 s =
    sendCatch $ (mkCompleteMultipartUpload p1 p2 p4) &~ s

-- $CopyObject
-- Creates a copy of an object that is already stored in Amazon S3.
--
-- See: 'Network.AWS.S3.CopyObject'

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
    -> m (Either S3Error CopyObjectResponse)
copyObjectCatch p18 p2 p8 s =
    sendCatch $ (mkCopyObject p18 p2 p8) &~ s

-- $CreateBucket
-- Creates a new bucket.
--
-- See: 'Network.AWS.S3.CreateBucket'

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
    -> m (Either S3Error CreateBucketResponse)
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
-- See: 'Network.AWS.S3.CreateMultipartUpload'

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
    -> m (Either S3Error CreateMultipartUploadResponse)
createMultipartUploadCatch p13 p2 s =
    sendCatch $ (mkCreateMultipartUpload p13 p2) &~ s

-- $DeleteBucket
-- Deletes the bucket. All objects (including all object versions and Delete
-- Markers) in the bucket must be deleted before the bucket itself can be
-- deleted.
--
-- See: 'Network.AWS.S3.DeleteBucket'

deleteBucket :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => BucketName -- ^ 'dbBucket'
    -> m DeleteBucketResponse
deleteBucket p1 =
    send (mkDeleteBucket p1)

deleteBucketCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => BucketName -- ^ 'dbBucket'
    -> m (Either S3Error DeleteBucketResponse)
deleteBucketCatch p1 =
    sendCatch (mkDeleteBucket p1)

-- $DeleteBucketCors
-- Deletes the cors configuration information set for the bucket.
--
-- See: 'Network.AWS.S3.DeleteBucketCors'

deleteBucketCors :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'dbcBucket'
    -> m DeleteBucketCorsResponse
deleteBucketCors p1 =
    send (mkDeleteBucketCors p1)

deleteBucketCorsCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'dbcBucket'
    -> m (Either S3Error DeleteBucketCorsResponse)
deleteBucketCorsCatch p1 =
    sendCatch (mkDeleteBucketCors p1)

-- $DeleteBucketLifecycle
-- Deletes the lifecycle configuration from the bucket.
--
-- See: 'Network.AWS.S3.DeleteBucketLifecycle'

deleteBucketLifecycle :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'dblBucket'
    -> m DeleteBucketLifecycleResponse
deleteBucketLifecycle p1 =
    send (mkDeleteBucketLifecycle p1)

deleteBucketLifecycleCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => BucketName -- ^ 'dblBucket'
    -> m (Either S3Error DeleteBucketLifecycleResponse)
deleteBucketLifecycleCatch p1 =
    sendCatch (mkDeleteBucketLifecycle p1)

-- $DeleteBucketPolicy
-- Deletes the policy from the bucket.
--
-- See: 'Network.AWS.S3.DeleteBucketPolicy'

deleteBucketPolicy :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => BucketName -- ^ 'dbpBucket'
    -> m DeleteBucketPolicyResponse
deleteBucketPolicy p1 =
    send (mkDeleteBucketPolicy p1)

deleteBucketPolicyCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => BucketName -- ^ 'dbpBucket'
    -> m (Either S3Error DeleteBucketPolicyResponse)
deleteBucketPolicyCatch p1 =
    sendCatch (mkDeleteBucketPolicy p1)

-- $DeleteBucketTagging
-- Deletes the tags from the bucket.
--
-- See: 'Network.AWS.S3.DeleteBucketTagging'

deleteBucketTagging :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => BucketName -- ^ 'dbtBucket'
    -> m DeleteBucketTaggingResponse
deleteBucketTagging p1 =
    send (mkDeleteBucketTagging p1)

deleteBucketTaggingCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => BucketName -- ^ 'dbtBucket'
    -> m (Either S3Error DeleteBucketTaggingResponse)
deleteBucketTaggingCatch p1 =
    sendCatch (mkDeleteBucketTagging p1)

-- $DeleteBucketWebsite
-- This operation removes the website configuration from the bucket.
--
-- See: 'Network.AWS.S3.DeleteBucketWebsite'

deleteBucketWebsite :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => BucketName -- ^ 'dbwBucket'
    -> m DeleteBucketWebsiteResponse
deleteBucketWebsite p1 =
    send (mkDeleteBucketWebsite p1)

deleteBucketWebsiteCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => BucketName -- ^ 'dbwBucket'
    -> m (Either S3Error DeleteBucketWebsiteResponse)
deleteBucketWebsiteCatch p1 =
    sendCatch (mkDeleteBucketWebsite p1)

-- $DeleteObject
-- Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object. If there
-- isn't a null version, Amazon S3 does not remove any objects.
--
-- See: 'Network.AWS.S3.DeleteObject'

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
    -> m (Either S3Error DeleteObjectResponse)
deleteObjectCatch p1 p2 s =
    sendCatch $ (mkDeleteObject p1 p2) &~ s

-- $DeleteObjects
-- This operation enables you to delete multiple objects from a bucket using a
-- single HTTP request. You may specify up to 1000 keys.
--
-- See: 'Network.AWS.S3.DeleteObjects'

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
    -> m (Either S3Error DeleteObjectsResponse)
deleteObjectsCatch p1 p2 s =
    sendCatch $ (mkDeleteObjects p1 p2) &~ s

-- $GetBucketAcl
-- Gets the access control policy for the bucket.
--
-- See: 'Network.AWS.S3.GetBucketAcl'

getBucketAcl :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => BucketName -- ^ 'gbaBucket'
    -> m GetBucketAclResponse
getBucketAcl p1 =
    send (mkGetBucketAcl p1)

getBucketAclCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => BucketName -- ^ 'gbaBucket'
    -> m (Either S3Error GetBucketAclResponse)
getBucketAclCatch p1 =
    sendCatch (mkGetBucketAcl p1)

-- $GetBucketCors
-- Returns the cors configuration for the bucket.
--
-- See: 'Network.AWS.S3.GetBucketCors'

getBucketCors :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => BucketName -- ^ 'gbcBucket'
    -> m GetBucketCorsResponse
getBucketCors p1 =
    send (mkGetBucketCors p1)

getBucketCorsCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => BucketName -- ^ 'gbcBucket'
    -> m (Either S3Error GetBucketCorsResponse)
getBucketCorsCatch p1 =
    sendCatch (mkGetBucketCors p1)

-- $GetBucketLifecycle
-- Returns the lifecycle configuration information set on the bucket.
--
-- See: 'Network.AWS.S3.GetBucketLifecycle'

getBucketLifecycle :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => BucketName -- ^ 'gblBucket'
    -> m GetBucketLifecycleResponse
getBucketLifecycle p1 =
    send (mkGetBucketLifecycle p1)

getBucketLifecycleCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => BucketName -- ^ 'gblBucket'
    -> m (Either S3Error GetBucketLifecycleResponse)
getBucketLifecycleCatch p1 =
    sendCatch (mkGetBucketLifecycle p1)

-- $GetBucketLocation
-- Returns the region the bucket resides in.
--
-- See: 'Network.AWS.S3.GetBucketLocation'

getBucketLocation :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => BucketName -- ^ 'gbl1Bucket'
    -> m GetBucketLocationResponse
getBucketLocation p1 =
    send (mkGetBucketLocation p1)

getBucketLocationCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => BucketName -- ^ 'gbl1Bucket'
    -> m (Either S3Error GetBucketLocationResponse)
getBucketLocationCatch p1 =
    sendCatch (mkGetBucketLocation p1)

-- $GetBucketLogging
-- Returns the logging status of a bucket and the permissions users have to
-- view and modify that status. To use GET, you must be the bucket owner.
--
-- See: 'Network.AWS.S3.GetBucketLogging'

getBucketLogging :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'gbl2Bucket'
    -> m GetBucketLoggingResponse
getBucketLogging p1 =
    send (mkGetBucketLogging p1)

getBucketLoggingCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'gbl2Bucket'
    -> m (Either S3Error GetBucketLoggingResponse)
getBucketLoggingCatch p1 =
    sendCatch (mkGetBucketLogging p1)

-- $GetBucketNotification
-- Return the notification configuration of a bucket.
--
-- See: 'Network.AWS.S3.GetBucketNotification'

getBucketNotification :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'gbnBucket'
    -> m GetBucketNotificationResponse
getBucketNotification p1 =
    send (mkGetBucketNotification p1)

getBucketNotificationCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => BucketName -- ^ 'gbnBucket'
    -> m (Either S3Error GetBucketNotificationResponse)
getBucketNotificationCatch p1 =
    sendCatch (mkGetBucketNotification p1)

-- $GetBucketPolicy
-- Returns the policy of a specified bucket.
--
-- See: 'Network.AWS.S3.GetBucketPolicy'

getBucketPolicy :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => BucketName -- ^ 'gbpBucket'
    -> m GetBucketPolicyResponse
getBucketPolicy p1 =
    send (mkGetBucketPolicy p1)

getBucketPolicyCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => BucketName -- ^ 'gbpBucket'
    -> m (Either S3Error GetBucketPolicyResponse)
getBucketPolicyCatch p1 =
    sendCatch (mkGetBucketPolicy p1)

-- $GetBucketRequestPayment
-- Returns the request payment configuration of a bucket.
--
-- See: 'Network.AWS.S3.GetBucketRequestPayment'

getBucketRequestPayment :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => BucketName -- ^ 'gbrpBucket'
    -> m GetBucketRequestPaymentResponse
getBucketRequestPayment p1 =
    send (mkGetBucketRequestPayment p1)

getBucketRequestPaymentCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => BucketName -- ^ 'gbrpBucket'
    -> m (Either S3Error GetBucketRequestPaymentResponse)
getBucketRequestPaymentCatch p1 =
    sendCatch (mkGetBucketRequestPayment p1)

-- $GetBucketTagging
-- Returns the tag set associated with the bucket.
--
-- See: 'Network.AWS.S3.GetBucketTagging'

getBucketTagging :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'gbtBucket'
    -> m GetBucketTaggingResponse
getBucketTagging p1 =
    send (mkGetBucketTagging p1)

getBucketTaggingCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'gbtBucket'
    -> m (Either S3Error GetBucketTaggingResponse)
getBucketTaggingCatch p1 =
    sendCatch (mkGetBucketTagging p1)

-- $GetBucketVersioning
-- Returns the versioning state of a bucket.
--
-- See: 'Network.AWS.S3.GetBucketVersioning'

getBucketVersioning :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => BucketName -- ^ 'gbvBucket'
    -> m GetBucketVersioningResponse
getBucketVersioning p1 =
    send (mkGetBucketVersioning p1)

getBucketVersioningCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => BucketName -- ^ 'gbvBucket'
    -> m (Either S3Error GetBucketVersioningResponse)
getBucketVersioningCatch p1 =
    sendCatch (mkGetBucketVersioning p1)

-- $GetBucketWebsite
-- Returns the website configuration for a bucket.
--
-- See: 'Network.AWS.S3.GetBucketWebsite'

getBucketWebsite :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'gbwBucket'
    -> m GetBucketWebsiteResponse
getBucketWebsite p1 =
    send (mkGetBucketWebsite p1)

getBucketWebsiteCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'gbwBucket'
    -> m (Either S3Error GetBucketWebsiteResponse)
getBucketWebsiteCatch p1 =
    sendCatch (mkGetBucketWebsite p1)

-- $GetObject
-- Retrieves objects from Amazon S3.
--
-- See: 'Network.AWS.S3.GetObject'

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
    -> m (Either S3Error GetObjectResponse)
getObjectCatch p1 p6 s =
    sendCatch $ (mkGetObject p1 p6) &~ s

-- $GetObjectAcl
-- Returns the access control list (ACL) of an object.
--
-- See: 'Network.AWS.S3.GetObjectAcl'

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
    -> m (Either S3Error GetObjectAclResponse)
getObjectAclCatch p1 p2 s =
    sendCatch $ (mkGetObjectAcl p1 p2) &~ s

-- $GetObjectTorrent
-- Return torrent files from a bucket.
--
-- See: 'Network.AWS.S3.GetObjectTorrent'

getObjectTorrent :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'gotBucket'
    -> ObjectKey -- ^ 'gotKey'
    -> m GetObjectTorrentResponse
getObjectTorrent p1 p2 =
    send (mkGetObjectTorrent p1 p2)

getObjectTorrentCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => BucketName -- ^ 'gotBucket'
    -> ObjectKey -- ^ 'gotKey'
    -> m (Either S3Error GetObjectTorrentResponse)
getObjectTorrentCatch p1 p2 =
    sendCatch (mkGetObjectTorrent p1 p2)

-- $HeadBucket
-- This operation is useful to determine if a bucket exists and you have
-- permission to access it.
--
-- See: 'Network.AWS.S3.HeadBucket'

headBucket :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => BucketName -- ^ 'hbBucket'
    -> m HeadBucketResponse
headBucket p1 =
    send (mkHeadBucket p1)

headBucketCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => BucketName -- ^ 'hbBucket'
    -> m (Either S3Error HeadBucketResponse)
headBucketCatch p1 =
    sendCatch (mkHeadBucket p1)

-- $HeadObject
-- The HEAD operation retrieves metadata from an object without returning the
-- object itself. This operation is useful if you're only interested in an
-- object's metadata. To use HEAD, you must have READ access to the object.
--
-- See: 'Network.AWS.S3.HeadObject'

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
    -> m (Either S3Error HeadObjectResponse)
headObjectCatch p1 p6 s =
    sendCatch $ (mkHeadObject p1 p6) &~ s

-- $ListBuckets
-- Returns a list of all buckets owned by the authenticated sender of the
-- request.
--
-- See: 'Network.AWS.S3.ListBuckets'

listBuckets :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => m ListBucketsResponse
listBuckets =
    send (mkListBuckets)

listBucketsCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => m (Either S3Error ListBucketsResponse)
listBucketsCatch =
    sendCatch (mkListBuckets)

-- $ListMultipartUploads
-- This operation lists in-progress multipart uploads.
--
-- See: 'Network.AWS.S3.ListMultipartUploads'

listMultipartUploads :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => BucketName -- ^ 'lmuBucket'
    -> State ListMultipartUploads a
    -> Source m ListMultipartUploadsResponse
listMultipartUploads p1 s =
    paginate $ (mkListMultipartUploads p1) &~ s

listMultipartUploadsCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => BucketName -- ^ 'lmuBucket'
    -> State ListMultipartUploads a
    -> Source m (Either S3Error ListMultipartUploadsResponse)
listMultipartUploadsCatch p1 s =
    paginateCatch $ (mkListMultipartUploads p1) &~ s

-- $ListObjectVersions
-- Returns metadata about all of the versions of objects in a bucket.
--
-- See: 'Network.AWS.S3.ListObjectVersions'

listObjectVersions :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => BucketName -- ^ 'lovBucket'
    -> State ListObjectVersions a
    -> Source m ListObjectVersionsResponse
listObjectVersions p1 s =
    paginate $ (mkListObjectVersions p1) &~ s

listObjectVersionsCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => BucketName -- ^ 'lovBucket'
    -> State ListObjectVersions a
    -> Source m (Either S3Error ListObjectVersionsResponse)
listObjectVersionsCatch p1 s =
    paginateCatch $ (mkListObjectVersions p1) &~ s

-- $ListObjects
-- Returns some or all (up to 1000) of the objects in a bucket. You can use
-- the request parameters as selection criteria to return a subset of the
-- objects in a bucket.
--
-- See: 'Network.AWS.S3.ListObjects'

listObjects :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => BucketName -- ^ 'loBucket'
    -> State ListObjects a
    -> Source m ListObjectsResponse
listObjects p1 s =
    paginate $ (mkListObjects p1) &~ s

listObjectsCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => BucketName -- ^ 'loBucket'
    -> State ListObjects a
    -> Source m (Either S3Error ListObjectsResponse)
listObjectsCatch p1 s =
    paginateCatch $ (mkListObjects p1) &~ s

-- $ListParts
-- Lists the parts that have been uploaded for a specific multipart upload.
--
-- See: 'Network.AWS.S3.ListParts'

listParts :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => BucketName -- ^ 'lpBucket'
    -> ObjectKey -- ^ 'lpKey'
    -> Text -- ^ 'lpUploadId'
    -> State ListParts a
    -> Source m ListPartsResponse
listParts p1 p2 p5 s =
    paginate $ (mkListParts p1 p2 p5) &~ s

listPartsCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => BucketName -- ^ 'lpBucket'
    -> ObjectKey -- ^ 'lpKey'
    -> Text -- ^ 'lpUploadId'
    -> State ListParts a
    -> Source m (Either S3Error ListPartsResponse)
listPartsCatch p1 p2 p5 s =
    paginateCatch $ (mkListParts p1 p2 p5) &~ s

-- $PutBucketAcl
-- Sets the permissions on a bucket using access control lists (ACL).
--
-- See: 'Network.AWS.S3.PutBucketAcl'

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
    -> m (Either S3Error PutBucketAclResponse)
putBucketAclCatch p3 s =
    sendCatch $ (mkPutBucketAcl p3) &~ s

-- $PutBucketCors
-- Sets the cors configuration for a bucket.
--
-- See: 'Network.AWS.S3.PutBucketCors'

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
    -> m (Either S3Error PutBucketCorsResponse)
putBucketCorsCatch p1 s =
    sendCatch $ (mkPutBucketCors p1) &~ s

-- $PutBucketLifecycle
-- Sets lifecycle configuration for your bucket. If a lifecycle configuration
-- exists, it replaces it.
--
-- See: 'Network.AWS.S3.PutBucketLifecycle'

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
    -> m (Either S3Error PutBucketLifecycleResponse)
putBucketLifecycleCatch p1 s =
    sendCatch $ (mkPutBucketLifecycle p1) &~ s

-- $PutBucketLogging
-- Set the logging parameters for a bucket and to specify permissions for who
-- can view and modify the logging parameters. To set the logging status of a
-- bucket, you must be the bucket owner.
--
-- See: 'Network.AWS.S3.PutBucketLogging'

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
    -> m (Either S3Error PutBucketLoggingResponse)
putBucketLoggingCatch p1 p2 s =
    sendCatch $ (mkPutBucketLogging p1 p2) &~ s

-- $PutBucketNotification
-- Enables notifications of specified events for a bucket.
--
-- See: 'Network.AWS.S3.PutBucketNotification'

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
    -> m (Either S3Error PutBucketNotificationResponse)
putBucketNotificationCatch p1 p3 s =
    sendCatch $ (mkPutBucketNotification p1 p3) &~ s

-- $PutBucketPolicy
-- Replaces a policy on a bucket. If the bucket already has a policy, the one
-- in this request completely replaces it.
--
-- See: 'Network.AWS.S3.PutBucketPolicy'

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
    -> m (Either S3Error PutBucketPolicyResponse)
putBucketPolicyCatch p1 p3 s =
    sendCatch $ (mkPutBucketPolicy p1 p3) &~ s

-- $PutBucketRequestPayment
-- Sets the request payment configuration for a bucket. By default, the bucket
-- owner pays for downloads from the bucket. This configuration parameter
-- enables the bucket owner (only) to specify that the person requesting the
-- download will be charged for the download.
--
-- See: 'Network.AWS.S3.PutBucketRequestPayment'

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
    -> m (Either S3Error PutBucketRequestPaymentResponse)
putBucketRequestPaymentCatch p1 p3 s =
    sendCatch $ (mkPutBucketRequestPayment p1 p3) &~ s

-- $PutBucketTagging
-- Sets the tags for a bucket.
--
-- See: 'Network.AWS.S3.PutBucketTagging'

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
    -> m (Either S3Error PutBucketTaggingResponse)
putBucketTaggingCatch p1 p3 s =
    sendCatch $ (mkPutBucketTagging p1 p3) &~ s

-- $PutBucketVersioning
-- Sets the versioning state of an existing bucket. To set the versioning
-- state, you must be the bucket owner.
--
-- See: 'Network.AWS.S3.PutBucketVersioning'

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
    -> m (Either S3Error PutBucketVersioningResponse)
putBucketVersioningCatch p1 p4 s =
    sendCatch $ (mkPutBucketVersioning p1 p4) &~ s

-- $PutBucketWebsite
-- Set the website configuration for a bucket.
--
-- See: 'Network.AWS.S3.PutBucketWebsite'

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
    -> m (Either S3Error PutBucketWebsiteResponse)
putBucketWebsiteCatch p1 p3 s =
    sendCatch $ (mkPutBucketWebsite p1 p3) &~ s

-- $PutObject
-- Adds an object to a bucket.
--
-- See: 'Network.AWS.S3.PutObject'

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
    -> m (Either S3Error PutObjectResponse)
putObjectCatch p16 p2 p3 s =
    sendCatch $ (mkPutObject p16 p2 p3) &~ s

-- $PutObjectAcl
-- uses the acl subresource to set the access control list (ACL) permissions
-- for an object that already exists in a bucket.
--
-- See: 'Network.AWS.S3.PutObjectAcl'

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
    -> m (Either S3Error PutObjectAclResponse)
putObjectAclCatch p10 p3 s =
    sendCatch $ (mkPutObjectAcl p10 p3) &~ s

-- $RestoreObject
-- Restores an archived copy of an object back into Amazon S3.
--
-- See: 'Network.AWS.S3.RestoreObject'

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
    -> m (Either S3Error RestoreObjectResponse)
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
-- See: 'Network.AWS.S3.UploadPart'

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
    -> m (Either S3Error UploadPartResponse)
uploadPartCatch p1 p2 p5 p6 p7 s =
    sendCatch $ (mkUploadPart p1 p2 p5 p6 p7) &~ s

-- $UploadPartCopy
-- Uploads a part by copying data from an existing object as data source.
--
-- See: 'Network.AWS.S3.UploadPartCopy'

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
    -> m (Either S3Error UploadPartCopyResponse)
uploadPartCopyCatch p1 p10 p2 p8 p9 s =
    sendCatch $ (mkUploadPartCopy p1 p10 p2 p8 p9) &~ s
