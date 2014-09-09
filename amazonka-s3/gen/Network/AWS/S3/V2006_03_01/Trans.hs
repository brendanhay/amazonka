{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.S3.V2006_03_01.Trans
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
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.S3.V2006_03_01.Trans
    (
    -- * AbortMultipartUpload
    -- $AbortMultipartUpload
      abortMultipartUpload

    -- * CompleteMultipartUpload
    -- $CompleteMultipartUpload
    , completeMultipartUpload

    -- * CopyObject
    -- $CopyObject
    , copyObject

    -- * CreateBucket
    -- $CreateBucket
    , createBucket

    -- * CreateMultipartUpload
    -- $CreateMultipartUpload
    , createMultipartUpload

    -- * DeleteBucket
    -- $DeleteBucket
    , deleteBucket

    -- * DeleteBucketCors
    -- $DeleteBucketCors
    , deleteBucketCors

    -- * DeleteBucketLifecycle
    -- $DeleteBucketLifecycle
    , deleteBucketLifecycle

    -- * DeleteBucketPolicy
    -- $DeleteBucketPolicy
    , deleteBucketPolicy

    -- * DeleteBucketTagging
    -- $DeleteBucketTagging
    , deleteBucketTagging

    -- * DeleteBucketWebsite
    -- $DeleteBucketWebsite
    , deleteBucketWebsite

    -- * DeleteObject
    -- $DeleteObject
    , deleteObject

    -- * DeleteObjects
    -- $DeleteObjects
    , deleteObjects

    -- * GetBucketAcl
    -- $GetBucketAcl
    , getBucketAcl

    -- * GetBucketCors
    -- $GetBucketCors
    , getBucketCors

    -- * GetBucketLifecycle
    -- $GetBucketLifecycle
    , getBucketLifecycle

    -- * GetBucketLocation
    -- $GetBucketLocation
    , getBucketLocation

    -- * GetBucketLogging
    -- $GetBucketLogging
    , getBucketLogging

    -- * GetBucketNotification
    -- $GetBucketNotification
    , getBucketNotification

    -- * GetBucketPolicy
    -- $GetBucketPolicy
    , getBucketPolicy

    -- * GetBucketRequestPayment
    -- $GetBucketRequestPayment
    , getBucketRequestPayment

    -- * GetBucketTagging
    -- $GetBucketTagging
    , getBucketTagging

    -- * GetBucketVersioning
    -- $GetBucketVersioning
    , getBucketVersioning

    -- * GetBucketWebsite
    -- $GetBucketWebsite
    , getBucketWebsite

    -- * GetObject
    -- $GetObject
    , getObject

    -- * GetObjectAcl
    -- $GetObjectAcl
    , getObjectAcl

    -- * GetObjectTorrent
    -- $GetObjectTorrent
    , getObjectTorrent

    -- * HeadBucket
    -- $HeadBucket
    , headBucket

    -- * HeadObject
    -- $HeadObject
    , headObject

    -- * ListBuckets
    -- $ListBuckets
    , listBuckets

    -- * ListMultipartUploads
    -- $ListMultipartUploads
    , listMultipartUploads

    -- * ListObjectVersions
    -- $ListObjectVersions
    , listObjectVersions

    -- * ListObjects
    -- $ListObjects
    , listObjects

    -- * ListParts
    -- $ListParts
    , listParts

    -- * PutBucketAcl
    -- $PutBucketAcl
    , putBucketAcl

    -- * PutBucketCors
    -- $PutBucketCors
    , putBucketCors

    -- * PutBucketLifecycle
    -- $PutBucketLifecycle
    , putBucketLifecycle

    -- * PutBucketLogging
    -- $PutBucketLogging
    , putBucketLogging

    -- * PutBucketNotification
    -- $PutBucketNotification
    , putBucketNotification

    -- * PutBucketPolicy
    -- $PutBucketPolicy
    , putBucketPolicy

    -- * PutBucketRequestPayment
    -- $PutBucketRequestPayment
    , putBucketRequestPayment

    -- * PutBucketTagging
    -- $PutBucketTagging
    , putBucketTagging

    -- * PutBucketVersioning
    -- $PutBucketVersioning
    , putBucketVersioning

    -- * PutBucketWebsite
    -- $PutBucketWebsite
    , putBucketWebsite

    -- * PutObject
    -- $PutObject
    , putObject

    -- * PutObjectAcl
    -- $PutObjectAcl
    , putObjectAcl

    -- * RestoreObject
    -- $RestoreObject
    , restoreObject

    -- * UploadPart
    -- $UploadPart
    , uploadPart

    -- * UploadPartCopy
    -- $UploadPartCopy
    , uploadPartCopy

    -- * Re-exported
    , module AWS
    , module Network.AWS.S3.V2006_03_01
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.S3.V2006_03_01

-- $AbortMultipartUpload
-- Aborts a multipart upload. To verify that all parts have been removed, so
-- you don't get charged for the part storage, you should call the List Parts
-- operation and ensure the parts list is empty.
--
-- See: 'Network.AWS.S3.V2006_03_01.AbortMultipartUpload'

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

-- $CompleteMultipartUpload
-- Completes a multipart upload by assembling previously uploaded parts.
--
-- See: 'Network.AWS.S3.V2006_03_01.CompleteMultipartUpload'

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

-- $CopyObject
-- Creates a copy of an object that is already stored in Amazon S3.
--
-- See: 'Network.AWS.S3.V2006_03_01.CopyObject'

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

-- $CreateBucket
-- Creates a new bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.CreateBucket'

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

-- $CreateMultipartUpload
-- Initiates a multipart upload and returns an upload ID. Note: After you
-- initiate multipart upload and upload one or more parts, you must either
-- complete or abort multipart upload in order to stop getting charged for
-- storage of the uploaded parts. Only after you either complete or abort
-- multipart upload, Amazon S3 frees up the parts storage and stops charging
-- you for the parts storage.
--
-- See: 'Network.AWS.S3.V2006_03_01.CreateMultipartUpload'

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

-- $DeleteBucket
-- Deletes the bucket. All objects (including all object versions and Delete
-- Markers) in the bucket must be deleted before the bucket itself can be
-- deleted.
--
-- See: 'Network.AWS.S3.V2006_03_01.DeleteBucket'

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

-- $DeleteBucketCors
-- Deletes the cors configuration information set for the bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.DeleteBucketCors'

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

-- $DeleteBucketLifecycle
-- Deletes the lifecycle configuration from the bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.DeleteBucketLifecycle'

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

-- $DeleteBucketPolicy
-- Deletes the policy from the bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.DeleteBucketPolicy'

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

-- $DeleteBucketTagging
-- Deletes the tags from the bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.DeleteBucketTagging'

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

-- $DeleteBucketWebsite
-- This operation removes the website configuration from the bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.DeleteBucketWebsite'

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

-- $DeleteObject
-- Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object. If there
-- isn't a null version, Amazon S3 does not remove any objects.
--
-- See: 'Network.AWS.S3.V2006_03_01.DeleteObject'

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

-- $DeleteObjects
-- This operation enables you to delete multiple objects from a bucket using a
-- single HTTP request. You may specify up to 1000 keys.
--
-- See: 'Network.AWS.S3.V2006_03_01.DeleteObjects'

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

-- $GetBucketAcl
-- Gets the access control policy for the bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetBucketAcl'

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

-- $GetBucketCors
-- Returns the cors configuration for the bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetBucketCors'

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

-- $GetBucketLifecycle
-- Returns the lifecycle configuration information set on the bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetBucketLifecycle'

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

-- $GetBucketLocation
-- Returns the region the bucket resides in.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetBucketLocation'

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

-- $GetBucketLogging
-- Returns the logging status of a bucket and the permissions users have to
-- view and modify that status. To use GET, you must be the bucket owner.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetBucketLogging'

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

-- $GetBucketNotification
-- Return the notification configuration of a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetBucketNotification'

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

-- $GetBucketPolicy
-- Returns the policy of a specified bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetBucketPolicy'

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

-- $GetBucketRequestPayment
-- Returns the request payment configuration of a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetBucketRequestPayment'

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

-- $GetBucketTagging
-- Returns the tag set associated with the bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetBucketTagging'

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

-- $GetBucketVersioning
-- Returns the versioning state of a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetBucketVersioning'

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

-- $GetBucketWebsite
-- Returns the website configuration for a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetBucketWebsite'

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

-- $GetObject
-- Retrieves objects from Amazon S3.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetObject'

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

-- $GetObjectAcl
-- Returns the access control list (ACL) of an object.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetObjectAcl'

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

-- $GetObjectTorrent
-- Return torrent files from a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.GetObjectTorrent'

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

-- $HeadBucket
-- This operation is useful to determine if a bucket exists and you have
-- permission to access it.
--
-- See: 'Network.AWS.S3.V2006_03_01.HeadBucket'

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

-- $HeadObject
-- The HEAD operation retrieves metadata from an object without returning the
-- object itself. This operation is useful if you're only interested in an
-- object's metadata. To use HEAD, you must have READ access to the object.
--
-- See: 'Network.AWS.S3.V2006_03_01.HeadObject'

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

-- $ListBuckets
-- Returns a list of all buckets owned by the authenticated sender of the
-- request.
--
-- See: 'Network.AWS.S3.V2006_03_01.ListBuckets'

listBuckets :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
            => State ListBuckets a
            -> m ListBucketsResponse
listBuckets s =
    send (mkListBuckets &~ s)

-- $ListMultipartUploads
-- This operation lists in-progress multipart uploads.
--
-- See: 'Network.AWS.S3.V2006_03_01.ListMultipartUploads'

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

-- $ListObjectVersions
-- Returns metadata about all of the versions of objects in a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.ListObjectVersions'

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

-- $ListObjects
-- Returns some or all (up to 1000) of the objects in a bucket. You can use
-- the request parameters as selection criteria to return a subset of the
-- objects in a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.ListObjects'

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

-- $ListParts
-- Lists the parts that have been uploaded for a specific multipart upload.
--
-- See: 'Network.AWS.S3.V2006_03_01.ListParts'

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

-- $PutBucketAcl
-- Sets the permissions on a bucket using access control lists (ACL).
--
-- See: 'Network.AWS.S3.V2006_03_01.PutBucketAcl'

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

-- $PutBucketCors
-- Sets the cors configuration for a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.PutBucketCors'

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

-- $PutBucketLifecycle
-- Sets lifecycle configuration for your bucket. If a lifecycle configuration
-- exists, it replaces it.
--
-- See: 'Network.AWS.S3.V2006_03_01.PutBucketLifecycle'

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

-- $PutBucketLogging
-- Set the logging parameters for a bucket and to specify permissions for who
-- can view and modify the logging parameters. To set the logging status of a
-- bucket, you must be the bucket owner.
--
-- See: 'Network.AWS.S3.V2006_03_01.PutBucketLogging'

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

-- $PutBucketNotification
-- Enables notifications of specified events for a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.PutBucketNotification'

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

-- $PutBucketPolicy
-- Replaces a policy on a bucket. If the bucket already has a policy, the one
-- in this request completely replaces it.
--
-- See: 'Network.AWS.S3.V2006_03_01.PutBucketPolicy'

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

-- $PutBucketRequestPayment
-- Sets the request payment configuration for a bucket. By default, the bucket
-- owner pays for downloads from the bucket. This configuration parameter
-- enables the bucket owner (only) to specify that the person requesting the
-- download will be charged for the download.
--
-- See: 'Network.AWS.S3.V2006_03_01.PutBucketRequestPayment'

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

-- $PutBucketTagging
-- Sets the tags for a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.PutBucketTagging'

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

-- $PutBucketVersioning
-- Sets the versioning state of an existing bucket. To set the versioning
-- state, you must be the bucket owner.
--
-- See: 'Network.AWS.S3.V2006_03_01.PutBucketVersioning'

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

-- $PutBucketWebsite
-- Set the website configuration for a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.PutBucketWebsite'

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

-- $PutObject
-- Adds an object to a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.PutObject'

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

-- $PutObjectAcl
-- uses the acl subresource to set the access control list (ACL) permissions
-- for an object that already exists in a bucket.
--
-- See: 'Network.AWS.S3.V2006_03_01.PutObjectAcl'

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

-- $RestoreObject
-- Restores an archived copy of an object back into Amazon S3.
--
-- See: 'Network.AWS.S3.V2006_03_01.RestoreObject'

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

-- $UploadPart
-- Uploads a part in a multipart upload. Note: After you initiate multipart
-- upload and upload one or more parts, you must either complete or abort
-- multipart upload in order to stop getting charged for storage of the
-- uploaded parts. Only after you either complete or abort multipart upload,
-- Amazon S3 frees up the parts storage and stops charging you for the parts
-- storage.
--
-- See: 'Network.AWS.S3.V2006_03_01.UploadPart'

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

-- $UploadPartCopy
-- Uploads a part by copying data from an existing object as data source.
--
-- See: 'Network.AWS.S3.V2006_03_01.UploadPartCopy'

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
