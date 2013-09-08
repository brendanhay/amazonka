{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Storage Service (Amazon S3) is a web service that enables you
-- to store data in the cloud.
--
-- You can then download the data or use the data with other AWS services,
-- such as Amazon Elastic Compute Cloud (Amazon EC2).
module Network.AWS.S3
    (
    -- * S3 API Version
      s3Version

    -- * Operations on the Service
    -- ** GET Service
    , GetService                    (..)
    , GetServiceResult              (..)

    -- * Operations on Buckets
    -- **

    -- * Operations on Objects
    -- ** DELETE Object
    , DeleteObject                  (..)
    , DeleteObjectResult            (..)

    -- ** POST Delete Multiple Objects
    , DeleteMultipleObjects         (..)
    , DeleteMultipleObjectsResult   (..)

    -- ** GET Object
    , GetObject                     (..)
    , GetObjectResult               (..)

    -- ** GET Object ACL
    , GetObjectACL                  (..)
    , GetObjectACLResult            (..)

    -- ** GET Object Torrent
    , GetObjectTorrent              (..)
    , GetObjectTorrentResult        (..)

    -- ** HEAD Object
    , HeadObject                    (..)
    , HeadObjectResult              (..)

    -- ** OPTIONS Object
    , OptionsObject                 (..)
    , OptionsObjectResult           (..)

    -- ** POST Object
    , PostObject                    (..)
    , PostObjectResult              (..)

    -- ** POST Object Restore
    , PostObjectRestore             (..)
    , PostObjectRestoreResult       (..)

    -- ** PUT Object
    , PutObject                     (..)
    , PutObjectResult               (..)

    -- ** PUT Object ACL
    , PutObjectACL                  (..)
    , PutObjectACLResult            (..)

    -- ** PUT Object Copy
    , PutObjectCopy                 (..)
    , PutObjectCopyResult           (..)

    -- ** POST Initiate Multipart Upload
    , InitiateMultipartUpload       (..)
    , InitiateMultipartUploadResult (..)

    -- ** PUT Upload Part
    , UploadPart                    (..)
    , UploadPartResult              (..)

    -- ** PUT Upload Part Copy
    , UploadPartCopy                (..)
    , UploadPartCopyResult          (..)

    -- ** POST Complete Multipart Upload
    , CompleteMultipartUpload       (..)
    , CompleteMultipartUploadResult (..)

    -- ** DELETE Abort Multipart Upload
    , AbortMultipartUpload          (..)
    , AbortMultipartUploadResult    (..)

    -- ** GET List Parts
    , ListParts                     (..)
    , ListPartsResult               (..)

    -- * Data Types
    , module Network.AWS.S3.Types
    ) where

import Data.ByteString      (ByteString)
import Data.Monoid
import Data.String
import Data.Text            (Text)
import Network.AWS.Internal
import Network.AWS.S3.Types
import Network.Http.Client  (Method(..))

qry :: IsQuery a => Method -> ByteString -> a -> RawRequest
qry meth path = queryRequest s3Service meth (svcPath s3Service path)

--
-- Service
--

-- | Returns a list of all buckets owned by the requester.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTServiceGET.html>
data GetService = GetService deriving (Eq, Show, Generic)

instance IsQuery GetService

instance Rq GetService where
    type Rs GetService = Either S3ErrorResponse GetServiceResult
    request = qry GET undefined

data GetServiceResult = GetServiceResult
    { gsrOwner   :: !Owner
      -- ^ Information about the bucket owner.
    , gsrBuckets :: [Bucket]
      -- ^ A list of buckets for the service.
    } deriving (Eq, Show, Generic)

instance IsXML GetServiceResult where
    xmlPickler = withNS s3NS

--
-- Buckets
--

--
-- Objects
--

-- | Retrieves an object from Amazon S3.
--
-- You must have READ access to the object.
--
-- If you grant READ access to the anonymous user, you can return the object
-- without using an authorization header.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html>
data GetObject  = GetObject
    {} deriving (Eq, Show, Generic)

instance IsQuery GetObject

instance Rq GetObject where
    type Rs GetObject = Either S3ErrorResponse GetObjectResult
    request = qry GET undefined

data GetObjectResult = GetObjectResult
    {} deriving (Eq, Show, Generic)

instance IsXML GetObjectResult where
    xmlPickler = undefined

-- | Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object.
--
-- If there isn't a null version, Amazon S3 does not remove any objects.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectDELETE.html>
data DeleteObject  = DeleteObject
    {} deriving (Eq, Show, Generic)

instance IsQuery DeleteObject

instance Rq DeleteObject where
    type Rs DeleteObject = Either S3ErrorResponse DeleteObjectResult
    request = qry GET undefined

data DeleteObjectResult = DeleteObjectResult
    {} deriving (Eq, Show, Generic)

instance IsXML DeleteObjectResult where
    xmlPickler = undefined

-- | Delete multiple objects from a bucket using a single HTTP request.
--
-- If you know the object keys that you want to delete, then this operation
-- provides a suitable alternative to sending individual delete requests
-- (see 'DeleteObject'), reducing per-request overhead.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/multiobjectdeleteapi.html>
data DeleteMultipleObjects  = DeleteMultipleObjects
    {} deriving (Eq, Show, Generic)

instance IsQuery DeleteMultipleObjects

instance Rq DeleteMultipleObjects where
    type Rs DeleteMultipleObjects = Either S3ErrorResponse DeleteMultipleObjectsResult
    request = qry GET undefined

data DeleteMultipleObjectsResult = DeleteMultipleObjectsResult
    {} deriving (Eq, Show, Generic)

instance IsXML DeleteMultipleObjectsResult where
    xmlPickler = undefined

-- | Uses the ACL subresource to return the access control list (ACL) of an object.
--
-- You must have READ_ACP access to the object.
--
-- By default, returns ACL information about the latest version of an object.
-- To return ACL information about a different version, use the versionId subresource.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGETacl.html>
data GetObjectACL  = GetObjectACL
    {} deriving (Eq, Show, Generic)

instance IsQuery GetObjectACL

instance Rq GetObjectACL where
    type Rs GetObjectACL = Either S3ErrorResponse GetObjectACLResult
    request = qry GET undefined

data GetObjectACLResult = GetObjectACLResult
    {} deriving (Eq, Show, Generic)

instance IsXML GetObjectACLResult where
    xmlPickler = undefined

-- | Use the torrent subresource to return torrent files from a bucket.
--
-- You can get torrent only for objects that are less than 5 GB in size.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGETtorrent.html>
data GetObjectTorrent  = GetObjectTorrent
    {} deriving (Eq, Show, Generic)

instance IsQuery GetObjectTorrent

instance Rq GetObjectTorrent where
    type Rs GetObjectTorrent = Either S3ErrorResponse GetObjectTorrentResult
    request = qry GET undefined

data GetObjectTorrentResult = GetObjectTorrentResult
    {} deriving (Eq, Show, Generic)

instance IsXML GetObjectTorrentResult where
    xmlPickler = undefined

-- | Retrieves metadata from an object without returning the object itself.
--
-- You must have READ access to the object.
--
-- By default, the HEAD operation retrieves metadata from the latest version of an object. If the latest version is a delete marker, Amazon S3 behaves as if the object was deleted. To retrieve metadata from a different version, use the versionId subresource. 
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectHEAD.html>
data HeadObject  = HeadObject
    {} deriving (Eq, Show, Generic)

instance IsQuery HeadObject

instance Rq HeadObject where
    type Rs HeadObject = Either S3ErrorResponse HeadObjectResult
    request = qry GET undefined

data HeadObjectResult = HeadObjectResult
    {} deriving (Eq, Show, Generic)

instance IsXML HeadObjectResult where
    xmlPickler = undefined

-- | Preflight request to determine if an actual request can be sent with the
-- specific origin, HTTP method, and headers.
--
-- Amazon S3 supports cross-origin resource sharing (CORS) by enabling you to add
-- a cors subresource on a bucket.
--
-- When a browser sends this preflight request, Amazon S3 responds by evaluating
-- the rules that are defined in the cors configuration.
--
-- If cors is not enabled on the bucket, then Amazon S3 returns a 403 Forbidden response.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTOPTIONSobject.html>
data OptionsObject  = OptionsObject
    {} deriving (Eq, Show, Generic)

instance IsQuery OptionsObject

instance Rq OptionsObject where
    type Rs OptionsObject = Either S3ErrorResponse OptionsObjectResult
    request = qry GET undefined

data OptionsObjectResult = OptionsObjectResult
    {} deriving (Eq, Show, Generic)

instance IsXML OptionsObjectResult where
    xmlPickler = undefined

-- | Adds an object to a specified bucket using HTML forms.
--
-- An alternate form of 'PutObject' that enables browser-based uploads as a
-- way of putting objects in buckets.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPOST.html>
data PostObject  = PostObject
    {} deriving (Eq, Show, Generic)

instance IsQuery PostObject

instance Rq PostObject where
    type Rs PostObject = Either S3ErrorResponse PostObjectResult
    request = qry GET undefined

data PostObjectResult = PostObjectResult
    {} deriving (Eq, Show, Generic)

instance IsXML PostObjectResult where
    xmlPickler = undefined

-- | Restores a temporary copy of an archived object.
--
-- In the request, you specify the number of days that you want the restored
-- copy to exist. After the specified period, Amazon S3 deletes the temporary copy.
--
-- Note that the object remains archived; Amazon S3 deletes only the restored copy.
--
-- An object in the Glacier storage class is an archived object. To access the
-- object, you must first initiate a restore request, which restores a copy of
-- the archived object. Restore jobs typically complete in three to five hours.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPOSTrestore.html>
data PostObjectRestore  = PostObjectRestore
    {} deriving (Eq, Show, Generic)

instance IsQuery PostObjectRestore

instance Rq PostObjectRestore where
    type Rs PostObjectRestore = Either S3ErrorResponse PostObjectRestoreResult
    request = qry GET undefined

data PostObjectRestoreResult = PostObjectRestoreResult
    {} deriving (Eq, Show, Generic)

instance IsXML PostObjectRestoreResult where
    xmlPickler = undefined

-- | Add an object to a bucket.
--
-- You must have WRITE permissions on a bucket to add an object to it.
--
-- Amazon S3 never adds partial objects; if you receive a success response,
-- Amazon S3 added the entire object to the bucket.
--
-- Amazon S3 is a distributed system. If it receives multiple write requests
-- for the same object simultaneously, it overwrites all but the last object written.
--
-- Amazon S3 does not provide object locking; if you need this, make sure to
-- build it into your application layer or use versioning instead.
--
-- If you enable versioning for a bucket, Amazon S3 automatically generates a
-- unique version ID for the object being stored. Amazon S3 returns this ID in
-- the response using the x-amz-version-id response header.
--
-- If versioning is suspended, Amazon S3 always uses null as the version ID for
-- the object stored.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUT.html>
data PutObject  = PutObject
    {} deriving (Eq, Show, Generic)

instance IsQuery PutObject

instance Rq PutObject where
    type Rs PutObject = Either S3ErrorResponse PutObjectResult
    request = qry GET undefined

data PutObjectResult = PutObjectResult
    {} deriving (Eq, Show, Generic)

instance IsXML PutObjectResult where
    xmlPickler = undefined

-- | Set the access control list (ACL) permissions for an object that already
-- exists in a bucket.
--
-- You must have WRITE_ACP permission to set the ACL of an object.
--
-- You can use one of the following two ways to set an object's permissions:
--     * Specify the ACL in the request body, or
--     * Specify permissions using request headers
--
-- Depending on your application needs, you may choose to set the ACL on an
-- object using either the request body or the headers.
--
-- For example, if you have an existing application that updates an object ACL
-- using the request body, then you can continue to use that approach.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUTacl.html>
data PutObjectACL  = PutObjectACL
    {} deriving (Eq, Show, Generic)

instance IsQuery PutObjectACL

instance Rq PutObjectACL where
    type Rs PutObjectACL = Either S3ErrorResponse PutObjectACLResult
    request = qry GET undefined

data PutObjectACLResult = PutObjectACLResult
    {} deriving (Eq, Show, Generic)

instance IsXML PutObjectACLResult where
    xmlPickler = undefined

-- | Create a copy of an object that is already stored in Amazon S3.
--
-- A copy operation is the same as performing a GET and then a PUT.
--
-- Note You can store individual objects of up to 5 TB in Amazon S3.
--
-- You create a copy of your object up to 5 GB in size in a single atomic
-- operation using this API. However, for copying an object greater than 5 GB,
-- you must use the multipart upload API.
--
-- When copying an object, you can preserve most of the metadata (default) or
-- specify new metadata. However, the ACL is not preserved and is set to private
-- for the user making the request.
--
-- By default, x-amz-copy-source identifies the latest version of an object to
-- copy. (If the latest version is a Delete Marker, Amazon S3 behaves as if the
-- object was deleted.) To copy a different version, use the versionId subresource.
--
-- If you enable Versioning on the target bucket, Amazon S3 generates a unique
-- version ID for the object being copied. This version ID is different from
-- the version ID of the source object.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectCOPY.html>
data PutObjectCopy  = PutObjectCopy
    {} deriving (Eq, Show, Generic)

instance IsQuery PutObjectCopy

instance Rq PutObjectCopy where
    type Rs PutObjectCopy = Either S3ErrorResponse PutObjectCopyResult
    request = qry GET undefined

data PutObjectCopyResult = PutObjectCopyResult
    {} deriving (Eq, Show, Generic)

instance IsXML PutObjectCopyResult where
    xmlPickler = undefined

-- | Initiate a multipart upload and return an upload ID.
--
-- This upload ID is used to associate all the parts in the specific multipart
-- upload. You specify this upload ID in each of your subsequent upload part
-- requests (see 'UploadPart').
--
-- You also include this upload ID in the final request to either complete or
-- abort the multipart upload request.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadInitiate.html>
data InitiateMultipartUpload  = InitiateMultipartUpload
    {} deriving (Eq, Show, Generic)

instance IsQuery InitiateMultipartUpload

instance Rq InitiateMultipartUpload where
    type Rs InitiateMultipartUpload = Either S3ErrorResponse InitiateMultipartUploadResult
    request = qry GET undefined

data InitiateMultipartUploadResult = InitiateMultipartUploadResult
    {} deriving (Eq, Show, Generic)

instance IsXML InitiateMultipartUploadResult where
    xmlPickler = undefined

-- | Upload a part in a multipart upload.
--
-- Note In this operation you provide part data in your request. However, you
-- have an option to specify your existing Amazon S3 object as data source for
-- the part your are uploading.
--
-- To upload a part from an existing object you use the 'UploadPartCopy' operation.
--
-- You must initiate a multipart upload (see 'InitiateMultipartUpload') before
-- you can upload any part.
--
-- In response to your initiate request. Amazon S3 returns an upload ID,
-- a unique identifier, that you must include in your upload part request.
--
-- Part numbers can be any number from 1 to 10,000, inclusive. A part number
-- uniquely identifies a part and also defines its position within the object
-- being created.
--
-- If you upload a new part using the same part number that was used with a
-- previous part, the previously uploaded part is overwritten.
--
-- Each part must be at least 5 MB in size, except the last part.
--
-- There is no size limit on the last part of your multipart upload.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadUploadPart.html>
data UploadPart  = UploadPart
    {} deriving (Eq, Show, Generic)

instance IsQuery UploadPart

instance Rq UploadPart where
    type Rs UploadPart = Either S3ErrorResponse UploadPartResult
    request = qry GET undefined

data UploadPartResult = UploadPartResult
    {} deriving (Eq, Show, Generic)

instance IsXML UploadPartResult where
    xmlPickler = undefined

-- | Uploads a part by copying data from an existing object as data source.
--
-- You specify the data source by adding the request header x-amz-copy-source
-- in your request and a byte range by adding the request header x-amz-copy-source-range
-- in your request.
--
-- Note Instead of using an existing object as part data, you might use the
-- 'UploadPart' operation and provide data in your request. For more information, see Upload Part.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadUploadPartCopy.html>
data UploadPartCopy  = UploadPartCopy
    {} deriving (Eq, Show, Generic)

instance IsQuery UploadPartCopy

instance Rq UploadPartCopy where
    type Rs UploadPartCopy = Either S3ErrorResponse UploadPartCopyResult
    request = qry GET undefined

data UploadPartCopyResult = UploadPartCopyResult
    {} deriving (Eq, Show, Generic)

instance IsXML UploadPartCopyResult where
    xmlPickler = undefined

-- | Completes a multipart upload by assembling previously uploaded parts.
--
-- You first initiate the multipart upload and then upload all parts using
-- the 'UploadPart' operation.
--
-- After successfully uploading all relevant parts of an upload, you call this
-- operation to complete the upload.
--
-- Upon receiving this request, Amazon S3 concatenates all the parts in
-- ascending order by part number to create a new object.
--
-- In the 'CompleteMultipartUpload' request, you must provide the parts list.
--
-- You must ensure the parts list is complete, this operation concatenates the
-- parts you provide in the list.
--
-- For each part in the list, you must provide the part number and the ETag
-- header value, returned after that part was uploaded.
--
-- Processing of a Complete Multipart Upload request could take several minutes
-- to complete. After Amazon S3 begins processing the request, it sends an HTTP
-- response header that specifies a 200 OK response.
--
-- While processing is in progress, Amazon S3 periodically sends whitespace
-- characters to keep the connection from timing out. Because a request could
-- fail after the initial 200 OK response has been sent, it is important that
-- you check the response body to determine whether the request succeeded.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadComplete.html>
data CompleteMultipartUpload  = CompleteMultipartUpload
    {} deriving (Eq, Show, Generic)

instance IsQuery CompleteMultipartUpload

instance Rq CompleteMultipartUpload where
    type Rs CompleteMultipartUpload = Either S3ErrorResponse CompleteMultipartUploadResult
    request = qry GET undefined

data CompleteMultipartUploadResult = CompleteMultipartUploadResult
    {} deriving (Eq, Show, Generic)

instance IsXML CompleteMultipartUploadResult where
    xmlPickler = undefined

-- | Aborts a multipart upload.
--
-- After a multipart upload is aborted, no additional parts can be uploaded
-- using that upload ID.
--
-- The storage consumed by any previously uploaded parts will be freed.
--
-- However, if any part uploads are currently in progress, those part uploads
-- might or might not succeed. As a result, it might be necessary to abort a
-- given multipart upload multiple times in order to completely free all
-- storage consumed by all parts.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadAbort.html>
data AbortMultipartUpload  = AbortMultipartUpload
    {} deriving (Eq, Show, Generic)

instance IsQuery AbortMultipartUpload

instance Rq AbortMultipartUpload where
    type Rs AbortMultipartUpload = Either S3ErrorResponse AbortMultipartUploadResult
    request = qry GET undefined

data AbortMultipartUploadResult = AbortMultipartUploadResult
    {} deriving (Eq, Show, Generic)

instance IsXML AbortMultipartUploadResult where
    xmlPickler = undefined

-- | List the parts that have been uploaded for a specific multipart upload.
--
-- This operation must include the upload ID, which you obtain by sending
-- 'InitiateMultipartUpload'.
--
-- This returns a maximum of 1,000 uploaded parts and the default number of
-- parts returned is 1,000 parts.
--
-- You can restrict the number of parts returned by specifying the 'maxParts'
-- parameter.
--
-- If your multipart upload consists of more than 1,000 parts, the response
-- returns an 'IsTruncated' field with the value of 'True', and a
-- 'NextPartNumberMarker' element.
--
-- In subsequent List Parts requests you can include the part-number-marker
-- query string parameter and set its value to the NextPartNumberMarker field
-- value from the previous response.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadListParts.html>
data ListParts  = ListParts
    {} deriving (Eq, Show, Generic)

instance IsQuery ListParts

instance Rq ListParts where
    type Rs ListParts = Either S3ErrorResponse ListPartsResult
    request = qry GET undefined

data ListPartsResult = ListPartsResult
    {} deriving (Eq, Show, Generic)

instance IsXML ListPartsResult where
    xmlPickler = undefined
