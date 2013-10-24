{-# LANGUAGE GADTs             #-}
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
    -- -- * Operations on the Service
    -- -- ** GET Service
    --   GetService              (..)

    -- -- * Operations on Buckets
    -- -- **

    -- -- * Operations on Objects
    -- -- ** DELETE Object
    -- , DeleteObject            (..)

    -- -- ** POST Delete Multiple Objects
    -- , DeleteMultipleObjects   (..)

    -- -- ** GET Object
    -- , GetObject               (..)

    -- -- ** GET Object ACL
    -- , GetObjectACL            (..)

    -- -- ** GET Object Torrent
    -- , GetObjectTorrent        (..)

    -- -- ** HEAD Object
    -- , HeadObject              (..)

    -- -- ** OPTIONS Object
    -- , OptionsObject           (..)

    -- -- ** POST Object
    -- , PostObject              (..)

    -- -- ** POST Object Restore
    -- , PostObjectRestore       (..)

    -- ** PUT Object
     PutObject               (..)

    -- -- ** PUT Object ACL
    -- , PutObjectACL            (..)

    -- -- ** PUT Object Copy
    -- , PutObjectCopy           (..)

    -- -- ** POST Initiate Multipart Upload
    -- , InitiateMultipartUpload (..)

    -- -- ** PUT Upload Part
    -- , UploadPart              (..)

    -- -- ** PUT Upload Part Copy
    -- , UploadPartCopy          (..)

    -- -- ** POST Complete Multipart Upload
    -- , CompleteMultipartUpload (..)

    -- -- ** DELETE Abort Multipart Upload
    -- , AbortMultipartUpload    (..)

    -- -- ** GET List Parts
    -- , ListParts               (..)

    -- * Data Types
    , module Network.AWS.S3.Types

    -- * Common
    , module Network.AWS
    ) where

import           Data.ByteString      (ByteString)
import           Data.Monoid
import qualified Data.Text.Encoding   as Text
import           Network.AWS
import           Network.AWS.Headers
import           Network.AWS.Internal
import           Network.AWS.S3.Types
import           Network.Http.Client  (Method(..))
import           System.IO.Streams    (InputStream)

-- PUT /ObjectName HTTP/1.1
-- Host: BucketName.s3.amazonaws.com
-- Date: date
-- Authorization: signatureValue

obj :: Method -> Bucket -> Key -> [AnyHeader] -> Body -> AWS Request
obj meth (Bucket b) (Key k) hs =
    Request svc
    signer (versionS3 $ Text.encodeUtf8 b) svc meth path hs []
  where
    path = Text.encodeUtf8 k
    svc  = s3 { svcEndpoint = Global $ Text.encodeUtf8 b <> ".s3.amazonaws.com" }

hdrs :: (Monad m, Rs a ~ S3HeadersResponse)
     => a
     -> Response
     -> m (Either e (Either (Er a) (Rs a)))
hdrs _ Response{..} = return . Right . Right $ S3HeadersResponse rsHeaders

--
-- Service
--

-- -- | Returns a list of all buckets owned by the requester.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTServiceGET.html>
-- data GetService = GetService deriving (Eq, Show, Generic)

-- instance IsQuery GetService

-- instance Rq GetService where
--     request = qry GET undefined

-- type instance Er GetService = S3ErrorResponse
-- data instance Rs GetService = GetServiceResult
--     { gsrOwner   :: !Owner
--       -- ^ Information about the bucket owner.
--     , gsrBuckets :: [Bucket]
--       -- ^ A list of buckets for the service.
--     } deriving (Eq, Show, Generic)

-- instance IsXML (Rs GetService) where
--     xmlPickler = withNS s3NS

--
-- Buckets
--

--
-- Objects
--

-- GET /ObjectName HTTP/1.1
-- Host: BucketName.s3.amazonaws.com
-- Date: date
-- Authorization: signatureValue
-- Range:bytes=byte_range

-- -- | Retrieves an object from Amazon S3.
-- --
-- -- You must have READ access to the object.
-- --
-- -- If you grant READ access to the anonymous user, you can return the object
-- -- without using an authorization header.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html>
-- data GetObject  = GetObject
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery GetObject

-- instance Rq GetObject where
--     request = qry GET undefined

-- type instance Er GetObject = S3ErrorResponse
-- data instance Rs GetObject = GetObjectResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs GetObject) where
--     xmlPickler = undefined

-- -- | Removes the null version (if there is one) of an object and inserts a
-- -- delete marker, which becomes the latest version of the object.
-- --
-- -- If there isn't a null version, Amazon S3 does not remove any objects.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectDELETE.html>
-- data DeleteObject  = DeleteObject
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery DeleteObject

-- instance Rq DeleteObject where
--     request = qry GET undefined

-- type instance Er DeleteObject = S3ErrorResponse
-- data instance Rs DeleteObject = DeleteObjectResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs DeleteObject) where
--     xmlPickler = undefined

-- -- | Delete multiple objects from a bucket using a single HTTP request.
-- --
-- -- If you know the object keys that you want to delete, then this operation
-- -- provides a suitable alternative to sending individual delete requests
-- -- (see 'DeleteObject'), reducing per-request overhead.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/multiobjectdeleteapi.html>
-- data DeleteMultipleObjects  = DeleteMultipleObjects
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery DeleteMultipleObjects

-- instance Rq DeleteMultipleObjects where
--     request = qry GET undefined

-- type instance Er DeleteMultipleObjects = S3ErrorResponse
-- data instance Rs DeleteMultipleObjects = DeleteMultipleObjectsResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs DeleteMultipleObjects) where
--     xmlPickler = undefined

-- -- | Uses the ACL subresource to return the access control list (ACL) of an object.
-- --
-- -- You must have READ_ACP access to the object.
-- --
-- -- By default, returns ACL information about the latest version of an object.
-- -- To return ACL information about a different version, use the versionId subresource.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGETacl.html>
-- data GetObjectACL  = GetObjectACL
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery GetObjectACL

-- instance Rq GetObjectACL where
--     request = qry GET undefined

-- type instance Er GetObjectACL = S3ErrorResponse
-- data instance Rs GetObjectACL = GetObjectACLResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs GetObjectACL) where
--     xmlPickler = undefined

-- -- | Use the torrent subresource to return torrent files from a bucket.
-- --
-- -- You can get torrent only for objects that are less than 5 GB in size.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGETtorrent.html>
-- data GetObjectTorrent  = GetObjectTorrent
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery GetObjectTorrent

-- instance Rq GetObjectTorrent where
--     request = qry GET undefined

-- type instance Er GetObjectTorrent = S3ErrorResponse
-- data instance Rs GetObjectTorrent = GetObjectTorrentResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs GetObjectTorrent) where
--     xmlPickler = undefined

-- -- | Retrieves metadata from an object without returning the object itself.
-- --
-- -- You must have READ access to the object.
-- --
-- -- By default, the HEAD operation retrieves metadata from the latest version of an object. If the latest version is a delete marker, Amazon S3 behaves as if the object was deleted. To retrieve metadata from a different version, use the versionId subresource. 
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectHEAD.html>
-- data HeadObject  = HeadObject
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery HeadObject

-- instance Rq HeadObject where
--     request = qry GET undefined

-- type instance Er HeadObject = S3ErrorResponse
-- data instance Rs HeadObject = HeadObjectResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs HeadObject) where
--     xmlPickler = undefined

-- -- | Preflight request to determine if an actual request can be sent with the
-- -- specific origin, HTTP method, and headers.
-- --
-- -- Amazon S3 supports cross-origin resource sharing (CORS) by enabling you to add
-- -- a cors subresource on a bucket.
-- --
-- -- When a browser sends this preflight request, Amazon S3 responds by evaluating
-- -- the rules that are defined in the cors configuration.
-- --
-- -- If cors is not enabled on the bucket, then Amazon S3 returns a 403 Forbidden response.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTOPTIONSobject.html>
-- data OptionsObject  = OptionsObject
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery OptionsObject

-- instance Rq OptionsObject where
--     request = qry GET undefined

-- type instance Er OptionsObject = S3ErrorResponse
-- data instance Rs OptionsObject = OptionsObjectResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs OptionsObject) where
--     xmlPickler = undefined

-- -- | Adds an object to a specified bucket using HTML forms.
-- --
-- -- An alternate form of 'PutObject' that enables browser-based uploads as a
-- -- way of putting objects in buckets.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPOST.html>
-- data PostObject  = PostObject
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery PostObject

-- instance Rq PostObject where
--     request = qry GET undefined

-- type instance Er PostObject = S3ErrorResponse
-- data instance Rs PostObject = PostObjectResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs PostObject) where
--     xmlPickler = undefined

-- -- | Restores a temporary copy of an archived object.
-- --
-- -- In the request, you specify the number of days that you want the restored
-- -- copy to exist. After the specified period, Amazon S3 deletes the temporary copy.
-- --
-- -- Note that the object remains archived; Amazon S3 deletes only the restored copy.
-- --
-- -- An object in the Glacier storage class is an archived object. To access the
-- -- object, you must first initiate a restore request, which restores a copy of
-- -- the archived object. Restore jobs typically complete in three to five hours.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPOSTrestore.html>
-- data PostObjectRestore  = PostObjectRestore
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery PostObjectRestore

-- instance Rq PostObjectRestore where
--     request = qry GET undefined

-- type instance Er PostObjectRestore = S3ErrorResponse
-- data instance Rs PostObjectRestore = PostObjectRestoreResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs PostObjectRestore) where
--     xmlPickler = undefined

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
data PutObject = PutObject
    { poBucket  :: !Bucket
    , poKey     :: !Key
    , poHeaders :: [AnyHeader]
    , poBody    :: Body
    } deriving (Show, Generic)

instance ToHeaders PutObject

instance Rq PutObject where
    type Er PutObject = S3ErrorResponse
    type Rs PutObject = S3HeadersResponse
    request p@PutObject{..} = obj PUT poBucket poKey (toHeaders p) poBody
    response = hdrs

-- newtype PutObjectResult = PutObjectResult [(ByteString, ByteString)]
--     deriving (Eq, Show)

  -- "Expiration": {
  --                       "type": "timestamp",
  --                       "location": "header",
  --                       "location_name": "x-amz-expiration",
  --                       "documentation": "If the object expiration is configured, this will contain the expiration date (expiry-date) and rule ID (rule-id). The value of rule-id is URL encoded."
  --                   },
  --                   "ETag": {
  --                       "type": "string",
  --                       "location": "header",
  --                       "location_name": "ETag",
  --                       "documentation": "Entity tag for the uploaded object."
  --                   },
  --                   "ServerSideEncryption": {
  --                       "type": "string",
  --                       "location": "header",
  --                       "location_name": "x-amz-server-side-encryption",
  --                       "enum": [
  --                           "AES256"
  --                       ],
  --                       "documentation": "The Server-side encryption algorithm used when storing this object in S3."
  --                   },
  --                   "VersionId": {
  --                       "type": "string",
  --                       "location": "header",
  --                       "location_name": "x-amz-version-id",
  --                       "documentation": "Version of the object."
  --                   }

-- -- | Set the access control list (ACL) permissions for an object that already
-- -- exists in a bucket.
-- --
-- -- You must have WRITE_ACP permission to set the ACL of an object.
-- --
-- -- You can use one of the following two ways to set an object's permissions:
-- --     * Specify the ACL in the request body, or
-- --     * Specify permissions using request headers
-- --
-- -- Depending on your application needs, you may choose to set the ACL on an
-- -- object using either the request body or the headers.
-- --
-- -- For example, if you have an existing application that updates an object ACL
-- -- using the request body, then you can continue to use that approach.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUTacl.html>
-- data PutObjectACL  = PutObjectACL
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery PutObjectACL

-- instance Rq PutObjectACL where
--     request = qry GET undefined

-- type instance Er PutObjectACL = S3ErrorResponse
-- data instance Rs PutObjectACL = PutObjectACLResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs PutObjectACL) where
--     xmlPickler = undefined

-- -- | Create a copy of an object that is already stored in Amazon S3.
-- --
-- -- A copy operation is the same as performing a GET and then a PUT.
-- --
-- -- Note You can store individual objects of up to 5 TB in Amazon S3.
-- --
-- -- You create a copy of your object up to 5 GB in size in a single atomic
-- -- operation using this API. However, for copying an object greater than 5 GB,
-- -- you must use the multipart upload API.
-- --
-- -- When copying an object, you can preserve most of the metadata (default) or
-- -- specify new metadata. However, the ACL is not preserved and is set to private
-- -- for the user making the request.
-- --
-- -- By default, x-amz-copy-source identifies the latest version of an object to
-- -- copy. (If the latest version is a Delete Marker, Amazon S3 behaves as if the
-- -- object was deleted.) To copy a different version, use the versionId subresource.
-- --
-- -- If you enable Versioning on the target bucket, Amazon S3 generates a unique
-- -- version ID for the object being copied. This version ID is different from
-- -- the version ID of the source object.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectCOPY.html>
-- data PutObjectCopy  = PutObjectCopy
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery PutObjectCopy

-- instance Rq PutObjectCopy where
--     request = qry GET undefined

-- type instance Er PutObjectCopy = S3ErrorResponse
-- data instance Rs PutObjectCopy = PutObjectCopyResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs PutObjectCopy) where
--     xmlPickler = undefined

-- -- | Initiate a multipart upload and return an upload ID.
-- --
-- -- This upload ID is used to associate all the parts in the specific multipart
-- -- upload. You specify this upload ID in each of your subsequent upload part
-- -- requests (see 'UploadPart').
-- --
-- -- You also include this upload ID in the final request to either complete or
-- -- abort the multipart upload request.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadInitiate.html>
-- data InitiateMultipartUpload  = InitiateMultipartUpload
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery InitiateMultipartUpload

-- instance Rq InitiateMultipartUpload where
--     request = qry GET undefined

-- type instance Er InitiateMultipartUpload = S3ErrorResponse
-- data instance Rs InitiateMultipartUpload = InitiateMultipartUploadResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs InitiateMultipartUpload) where
--     xmlPickler = undefined

-- -- | Upload a part in a multipart upload.
-- --
-- -- Note In this operation you provide part data in your request. However, you
-- -- have an option to specify your existing Amazon S3 object as data source for
-- -- the part your are uploading.
-- --
-- -- To upload a part from an existing object you use the 'UploadPartCopy' operation.
-- --
-- -- You must initiate a multipart upload (see 'InitiateMultipartUpload') before
-- -- you can upload any part.
-- --
-- -- In response to your initiate request. Amazon S3 returns an upload ID,
-- -- a unique identifier, that you must include in your upload part request.
-- --
-- -- Part numbers can be any number from 1 to 10,000, inclusive. A part number
-- -- uniquely identifies a part and also defines its position within the object
-- -- being created.
-- --
-- -- If you upload a new part using the same part number that was used with a
-- -- previous part, the previously uploaded part is overwritten.
-- --
-- -- Each part must be at least 5 MB in size, except the last part.
-- --
-- -- There is no size limit on the last part of your multipart upload.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadUploadPart.html>
-- data UploadPart  = UploadPart
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery UploadPart

-- instance Rq UploadPart where
--     request = qry GET undefined

-- type instance Er UploadPart = S3ErrorResponse
-- data instance Rs UploadPart = UploadPartResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs UploadPart) where
--     xmlPickler = undefined

-- -- | Uploads a part by copying data from an existing object as data source.
-- --
-- -- You specify the data source by adding the request header x-amz-copy-source
-- -- in your request and a byte range by adding the request header x-amz-copy-source-range
-- -- in your request.
-- --
-- -- Note Instead of using an existing object as part data, you might use the
-- -- 'UploadPart' operation and provide data in your request. For more information, see Upload Part.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadUploadPartCopy.html>
-- data UploadPartCopy  = UploadPartCopy
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery UploadPartCopy

-- instance Rq UploadPartCopy where
--     request = qry GET undefined

-- type instance Er UploadPartCopy = S3ErrorResponse
-- data instance Rs UploadPartCopy = UploadPartCopyResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs UploadPartCopy) where
--     xmlPickler = undefined

-- -- | Completes a multipart upload by assembling previously uploaded parts.
-- --
-- -- You first initiate the multipart upload and then upload all parts using
-- -- the 'UploadPart' operation.
-- --
-- -- After successfully uploading all relevant parts of an upload, you call this
-- -- operation to complete the upload.
-- --
-- -- Upon receiving this request, Amazon S3 concatenates all the parts in
-- -- ascending order by part number to create a new object.
-- --
-- -- In the 'CompleteMultipartUpload' request, you must provide the parts list.
-- --
-- -- You must ensure the parts list is complete, this operation concatenates the
-- -- parts you provide in the list.
-- --
-- -- For each part in the list, you must provide the part number and the ETag
-- -- header value, returned after that part was uploaded.
-- --
-- -- Processing of a Complete Multipart Upload request could take several minutes
-- -- to complete. After Amazon S3 begins processing the request, it sends an HTTP
-- -- response header that specifies a 200 OK response.
-- --
-- -- While processing is in progress, Amazon S3 periodically sends whitespace
-- -- characters to keep the connection from timing out. Because a request could
-- -- fail after the initial 200 OK response has been sent, it is important that
-- -- you check the response body to determine whether the request succeeded.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadComplete.html>
-- data CompleteMultipartUpload  = CompleteMultipartUpload
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery CompleteMultipartUpload

-- instance Rq CompleteMultipartUpload where
--     type Er CompleteMultipartUpload = S3ErrorResponse
--     type Rs CompleteMultipartUpload = CompleteMultipartUploadResult
--     request = qry GET undefined

-- data CompleteMultipartUploadResponse = CompleteMultipartUploadResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML CompleteMultipartUploadResult

-- -- | Aborts a multipart upload.
-- --
-- -- After a multipart upload is aborted, no additional parts can be uploaded
-- -- using that upload ID.
-- --
-- -- The storage consumed by any previously uploaded parts will be freed.
-- --
-- -- However, if any part uploads are currently in progress, those part uploads
-- -- might or might not succeed. As a result, it might be necessary to abort a
-- -- given multipart upload multiple times in order to completely free all
-- -- storage consumed by all parts.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadAbort.html>
-- data AbortMultipartUpload  = AbortMultipartUpload
--     { amuBucket   :: !Bucket
--     , amuKey      :: !Key
--     , amuUploadId :: !UploadId
--     } deriving (Eq, Show, Generic)

-- instance IsQuery AbortMultipartUpload

-- instance Rq AbortMultipartUpload where
--     type Er AbortMultipartUpload = S3ErrorResponse
--     type Rs AbortMultipartUpload = AbortMultipartUploadResult
--     request = qry DELETE undefined
-- --  "uri": "/{Bucket}/{Key}?uploadId={UploadId}"

-- -- NoSuchUpload = 404
-- -- The specified multipart upload does not exist.
-- -- The upload ID might be invalid, or the multipart upload might have been aborted or completed.
-- -- empty response

-- data AbortMultipartUploadResponse = AbortMultipartUploadResult
--     deriving (Eq, Show, Generic)

-- instance IsXML AbortMultipartUploadResult

-- -- | List the parts that have been uploaded for a specific multipart upload.
-- --
-- -- This operation must include the upload ID, which you obtain by sending
-- -- 'InitiateMultipartUpload'.
-- --
-- -- This returns a maximum of 1,000 uploaded parts and the default number of
-- -- parts returned is 1,000 parts.
-- --
-- -- You can restrict the number of parts returned by specifying the 'maxParts'
-- -- parameter.
-- --
-- -- If your multipart upload consists of more than 1,000 parts, the response
-- -- returns an 'IsTruncated' field with the value of 'True', and a
-- -- 'NextPartNumberMarker' element.
-- --
-- -- In subsequent List Parts requests you can include the part-number-marker
-- -- query string parameter and set its value to the NextPartNumberMarker field
-- -- value from the previous response.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/mpUploadListParts.html>
-- data ListParts  = ListParts
--     {} deriving (Eq, Show, Generic)

-- instance IsQuery ListParts

-- instance Rq ListParts where
--     request = qry GET undefined

-- type instance Er ListParts = S3ErrorResponse
-- data instance Rs ListParts = ListPartsResult
--     {} deriving (Eq, Show, Generic)

-- instance IsXML (Rs ListParts) where
--     xmlPickler = undefined
