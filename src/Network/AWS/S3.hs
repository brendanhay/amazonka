{-# LANGUAGE GADTs              #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

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
    -- * Operations on the Service
    -- ** GET Service
    --   GetService                    (..)
    -- , GetServiceResponse            (..)

    -- * Operations on Buckets
    -- ** GET Bucket
      GetBucket (..)
    , GetBucketResponse (..)

    -- * Operations on Objects
    -- ** DELETE Object
    , DeleteObject                  (..)
    , DeleteObjectResponse

    -- -- ** POST Delete Multiple Objects
    -- , DeleteMultipleObjects         (..)
    -- , DeleteMultipleObjectsResponse (..)

    -- ** GET Object
    , GetObject                     (..)
    , GetObjectResponse

    -- -- ** GET Object ACL
    -- , GetObjectACL                  (..)
    -- , GetObjectACLResponse          (..)

    -- -- ** GET Object Torrent
    -- , GetObjectTorrent              (..)
    -- , GetObjectTorrentResponse

    -- ** HEAD Object
    , HeadObject                    (..)
    , HeadObjectResponse

    -- -- ** OPTIONS Object
    -- , OptionsObject                 (..)
    -- , OptionsObjectResponse

    -- -- ** POST Object Restore
    -- , PostObjectRestore             (..)
    -- , PostObjectRestoreResponse

    -- ** PUT Object
    , PutObject                     (..)
    , PutObjectResponse

    -- -- ** PUT Object ACL
    -- , PutObjectACL                  (..)
    -- , PutObjectACLResponse

    -- ** PUT Object Copy
    , PutObjectCopy                 (..)
    , PutObjectCopyResponse

    -- -- ** POST Initiate Multipart Upload
    -- , InitiateMultipartUpload       (..)
    -- , InitiateMultipartUploadResponse

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

import           Control.Monad.IO.Class
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary        as Conduit
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as Text
import           Network.AWS
import           Network.AWS.Internal       hiding (xml, query)
import           Network.AWS.S3.Types
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status

type S3Response = Response (ResumableSource AWS ByteString)

object :: StdMethod -> Text -> Text -> [Header] -> RequestBody -> Raw
object m b p hs = Raw s m (Text.encodeUtf8 p) [] hs
  where
    s = (s3 n) { svcEndpoint = Custom $ n <> ".s3.amazonaws.com" }
    n = Text.encodeUtf8 b

query :: IsQuery a => StdMethod -> Text -> Text -> a -> Raw
query m b p x = object m b p [] mempty .?. toQuery x

xml :: IsXML a => StdMethod -> Text -> Text -> a -> Raw
xml m b p = object m b p [] . RequestBodyBS . toXML

s3Response :: a
           -> S3Response
           -> AWS (Either AWSError (Either S3ErrorResponse S3Response))
s3Response _ rs
    | code >= 200 && code < 300 = return (Right plain)
    | code == 404               = return (Right notFound)
    | otherwise = do
        lbs <- responseBody rs $$+- Conduit.sinkLbs
        whenDebug . liftIO $ LBS.putStrLn lbs
        return . either Left (Right . Left) . parse $ LBS.toStrict lbs
  where
    parse :: ByteString -> Either AWSError S3ErrorResponse
    parse = fmapL toError . fromXML

    plain    = Right rs
    notFound = Left (S3ErrorResponse "Not Found.")

    code = statusCode (responseStatus rs)

--
-- Service
--

-- -- | Returns a list of all buckets owned by the requester.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTServiceGET.html>
-- data GetService = GetService deriving (Eq, Show)

-- instance Rq GetService where
--     type Er GetService = S3ErrorResponse
--     type Rs GetService = GetServiceResponse
--     request _ = empty GET "/"

-- data GetServiceResponse = GetServiceResponse
--     { gsrOwner   :: !Owner
--       -- ^ Information about the bucket owner.
--     , gsrBuckets :: [Bucket]
--       -- ^ A list of buckets for the service.
--     } deriving (Eq, Show, Generic)

-- instance IsXML GetServiceResponse where
--     xmlPickler = withRootNS s3NS "ListAllMyBucketsResult"

--
-- Buckets
--

-- | This implementation of the GET operation returns some or all (up to 1000) of
-- the objects in a bucket. You can use the request parameters as selection
-- criteria to return a subset of the objects in a bucket.
--
-- To use this implementation of the operation, you must have READ access
-- to the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGET.html>
data GetBucket = GetBucket
    { gbBucket    :: !Text
      -- ^ Target bucket.
    , gbDelimiter :: !Delimiter
      -- ^ A delimiter is a character you use to group keys.
      -- All keys that contain the same string between the prefix,
      -- if specified, and the first occurrence of the delimiter after
      -- the prefix are grouped under a single result element, CommonPrefixes.
    , gbPrefix    :: Maybe Text
      -- ^ Limits the response to keys that begin with the specified prefix.
      -- You can use prefixes to separate a bucket into different groupings of keys.
      -- (You can think of using prefix to make groups in the same way you'd
      -- use a folder in a file system.)
    , gbMaxKeys   :: !Int
      -- ^ Sets the maximum number of keys returned in the response body.
      -- The response might contain fewer keys but will never contain more.
      -- Use pagination to access additional matching keys.
    , gbMarker     :: Maybe Text
      -- ^ Specifies the key to start with when listing objects in a bucket.
      -- Amazon S3 lists objects in alphabetical order.
    } deriving (Eq, Show, Generic)

instance IsQuery GetBucket where
    queryPickler = genericQueryPickler $ defaultQueryOptions
        { queryFieldModifier = hyphenate
        }

instance Rq GetBucket where
    type Er GetBucket = S3ErrorResponse
    type Rs GetBucket = GetBucketResponse
    request gb@GetBucket{..} = query GET gbBucket "/" gb

instance Pg GetBucket where
    next gb@GetBucket{..} GetBucketResponse{..}
        | not gbrIsTruncated   = Nothing
        | isJust gbrNextMarker = Just $ gb { gbMarker = gbrNextMarker }
        | null gbrContents     = Nothing
        | otherwise            = Just $ gb { gbMarker = Just $ bcKey (last gbrContents) }

-- FIXME: consider the interaction between the nested list item pickler's root
-- and the parent using xpElemList vs xpList via generics/default
data GetBucketResponse = GetBucketResponse
    { gbrName        :: !Text
    , gbrPrefix      :: Maybe Text
    , gbrMarker      :: Maybe Text
    , gbrNextMarker  :: Maybe Text
      -- ^ When response is truncated (the IsTruncated element value in the
      -- response is true), you can use the key name in this field as marker in the
      -- subsequent request to get next set of objects. Amazon S3 lists objects in
      -- alphabetical order.  This element is returned only if you have delimiter
      -- request parameter specified. If response does not include the NextMaker and it
      -- is truncated, you can use the value of the last Key in the response as the
      -- marker in the subsequent request to get the next set of object keys.
    , gbrMaxKeys     :: !Int
    , gbrIsTruncated :: !Bool
    , gbrContents    :: [Contents]
    } deriving (Eq, Show, Generic)

instance IsXML GetBucketResponse where
    xmlPickler = pu { root = Just $ mkNName s3NS "ListBucketResult" }
      where
        pu = xpWrap (\(n, p, m, nm, k, t, c) -> GetBucketResponse n p m nm k t c,
                     \GetBucketResponse{..} -> (gbrName, gbrPrefix, gbrMarker, gbrNextMarker, gbrMaxKeys, gbrIsTruncated, gbrContents)) $
                 xp7Tuple (e "Name")
                          (xpOption $ e "Prefix")
                          (xpOption $ e "Marker")
                          (xpOption $ e "NextMarker")
                          (e "MaxKeys")
                          (e "IsTruncated")
                          (xpFindMatches $ xpElem (mkNName s3NS "Contents") xmlPickler)

        e n = xpElem (mkNName s3NS n) xmlPickler

-- | This implementation of the PUT operation creates a new bucket.
--
-- By default, the bucket is created in the US Standard region. You can
-- optionally specify a region in the request body.
-- You might choose a Region to optimize latency, minimize costs, or address
-- regulatory requirements.
--
-- For example, if you reside in Europe, you will probably find it advantageous
-- to create buckets in the EU (Ireland) Region.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUT.html>
data PutBucket = PutBucket
    { pbName     :: !Text
      -- ^ Target bucket.
    , pbLocation :: !Region
      -- ^ Specifies the region where the bucket will be created.
    , pbHeaders  :: [Header]
      -- ^ Common request headers.
    } deriving (Eq, Show)

instance Rq PutBucket where
    type Er PutBucket = S3ErrorResponse
    type Rs PutBucket = PutBucketResponse
    request PutBucket{..} =
        xml PUT pbName "/" (CreateBucketConfiguration pbLocation) .:. pbHeaders
    response = s3Response

type PutBucketResponse = S3Response

--
-- Objects
--

-- | Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object.
--
-- If there isn't a null version, Amazon S3 does not remove any objects.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectDELETE.html>
data DeleteObject = DeleteObject
    { doBucket  :: !Text
    , doKey     :: !Text
    , doHeaders :: [Header]
    } deriving (Eq, Show)

instance Rq DeleteObject where
    type Er DeleteObject = S3ErrorResponse
    type Rs DeleteObject = DeleteObjectResponse
    request DeleteObject{..} = object DELETE doBucket doKey doHeaders mempty
    response = s3Response

type DeleteObjectResponse = S3Response

-- -- | Delete multiple objects from a bucket using a single HTTP request.
-- --
-- -- If you know the object keys that you want to delete, then this operation
-- -- provides a suitable alternative to sending individual delete requests
-- -- (see 'DeleteObject'), reducing per-request overhead.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/multiobjectdeleteapi.html>
-- data DeleteMultipleObjects = DeleteMultipleObjects
--     { dmoBucket  :: !Text
--     , dmoMD5     :: !MD5
-- --    , dmoLength  :: !ContentLength
--     , dmoHeaders :: [AnyHeader]
--     , dmoObjects :: !DMObjects
--     }

-- deriving instance Show DeleteMultipleObjects

-- instance Rq DeleteMultipleObjects where
--     type Er DeleteMultipleObjects = S3ErrorResponse
--     type Rs DeleteMultipleObjects = DeleteMultipleObjectsResponse
--     request DeleteMultipleObjects{..} =
--         xml POST "/?delete" dmoBucket (hdr dmoMD5 : dmoHeaders) dmoObjects

-- data DeleteMultipleObjectsResponse = DeleteMultipleObjectsResponse
--     { dmorDeleted :: [DeletedObject]
--     , dmorError   :: [DeleteError]
--     } deriving (Eq, Show, Generic)

-- instance IsXML DeleteMultipleObjectsResponse where
--     xmlPickler = withRootNS s3NS "DeleteResult"

-- | Retrieves an object from Amazon S3.
--
-- You must have READ access to the object.
--
-- If you grant READ access to the anonymous user, you can return the object
-- without using an authorization header.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html>
data GetObject  = GetObject
    { goBucket  :: !Text
    , goKey     :: !Text
    , goHeaders :: [Header]
    } deriving (Eq, Show)

instance Rq GetObject where
    type Er GetObject = S3ErrorResponse
    type Rs GetObject = GetObjectResponse
    request GetObject{..} = object GET goBucket goKey goHeaders mempty
    response = s3Response

type GetObjectResponse = S3Response

-- -- | Uses the ACL subresource to return the access control list (ACL) of an object.
-- --
-- -- You must have READ_ACP access to the object.
-- --
-- -- By default, returns ACL information about the latest version of an object.
-- -- To return ACL information about a different version, use the versionId subresource.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGETacl.html>
-- data GetObjectACL = GetObjectACL
--     { goaclBucket  :: !Text
--     , goaclKey     :: !Text
--     , goaclHeaders :: [AnyHeader]
--     }

-- deriving instance Show GetObjectACL

-- instance Rq GetObjectACL where
--     type Er GetObjectACL = S3ErrorResponse
--     type Rs GetObjectACL = GetObjectACLResponse
--     request GetObjectACL {..} =
--         object GET goaclBucket goaclKey goaclHeaders Empty

--     response _ rs' = fmap (fmap (fmap f)) $ defaultResponse rs'
--       where
--         f x = x { goaclrHeaders = rsHeaders rs' }

-- data GetObjectACLResponse = GetObjectACLResponse
--     { goaclrHeaders :: [(ByteString, ByteString)]
--     , goaclrPolicy  :: !AccessControlPolicy
--     }

-- deriving instance Show GetObjectACLResponse

-- instance IsXML GetObjectACLResponse where
--     xmlPickler = xpWrap
--         (GetObjectACLResponse [], \(GetObjectACLResponse _ p) -> p)
--         (genericXMLPickler defaultXMLOptions)

-- -- | Use the torrent subresource to return torrent files from a bucket.
-- --
-- -- You can get torrent only for objects that are less than 5 GB in size.
-- --
-- -- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGETtorrent.html>
-- data GetObjectTorrent = GetObjectTorrent
--     { gotBucket :: !Text
--     , gotKey    :: !Text
--     , gotHeaders :: [AnyHeader]
--     }

-- deriving instance Show GetObjectTorrent

-- instance Rq GetObjectTorrent where
--     type Er GetObjectTorrent = S3ErrorResponse
--     type Rs GetObjectTorrent = GetObjectTorrentResponse
--     request GetObjectTorrent{..} =
--         object GET gotBucket (gotKey <> "?torrent") gotHeaders Empty

--     response = bodyRs

-- type GetObjectTorrentResponse = S3Response

-- | Retrieves metadata from an object without returning the object itself.
--
-- You must have READ access to the object.
--
-- By default, the HEAD operation retrieves metadata from the latest version of an object. If the latest version is a delete marker, Amazon S3 behaves as if the object was deleted. To retrieve metadata from a different version, use the versionId subresource. 
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectHEAD.html>
data HeadObject = HeadObject
    { hoBucket  :: !Text
    , hoKey     :: !Text
    , hoHeaders :: [Header]
    } deriving (Eq, Show)

instance Rq HeadObject where
    type Er HeadObject = S3ErrorResponse
    type Rs HeadObject = HeadObjectResponse
    request HeadObject{..} = object HEAD hoBucket hoKey hoHeaders mempty
    response = s3Response

type HeadObjectResponse = S3Response

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
--     { ooBucket         :: !Text
--     , ooKey            :: !Text
--     , ooOrigin         :: !Origin
--     , ooRequestMethod  :: !AccessControlRequestMethod
--     , ooRequestHeaders :: !AccessControlRequestHeaders
--     , ooHeaders        :: [AnyHeader]
--     }

-- deriving instance Show OptionsObject

-- instance Rq OptionsObject where
--     type Er OptionsObject = S3ErrorResponse
--     type Rs OptionsObject = OptionsObjectResponse
--     request OptionsObject{..} = object OPTIONS ooBucket ooKey hs Empty
--       where
--         hs = hdr ooOrigin
--            : hdr ooRequestMethod
--            : hdr ooRequestHeaders
--            : ooHeaders

--     response = headerRs

-- type OptionsObjectResponse = S3Response

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
--     { porBucket         :: !Text
--     , porKey            :: !Text
--     , porMD5            :: !MD5
--     , porRestoreRequest :: !RestoreRequest
--     }

-- deriving instance Show PostObjectRestore

-- instance Rq PostObjectRestore where
--     type Er PostObjectRestore = S3ErrorResponse
--     type Rs PostObjectRestore = PostObjectRestoreResponse
--     request PostObjectRestore{..} = sign (versionS3 name) $
--         Request svc POST path [hdr porMD5] [] (Strict $ toXML porRestoreRequest)
--       where
--         svc  = override (name <> ".s3.amazonaws.com") s3
--         name = Text.encodeUtf8 porBucket
--         path = addPrefix "/" porKey <> "?restore"

--     response = headerRs

-- type PostObjectRestoreResponse = S3Response

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
    { poBucket  :: !Text
    , poKey     :: !Text
    , poHeaders :: [Header]
    , poBody    :: !RequestBody
    }

instance Rq PutObject where
    type Er PutObject = S3ErrorResponse
    type Rs PutObject = PutObjectResponse
    request PutObject{..} = object PUT poBucket poKey poHeaders poBody
    response = s3Response

type PutObjectResponse = S3Response

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
-- data PutObjectACL = PutObjectACL
--     { poaclBucket  :: !Text
--     , poaclKey     :: !Text
--     , poaclPolicy  :: Maybe AccessControlPolicy
--     , poaclHeaders :: [AnyHeader]
--     }

-- deriving instance Show PutObjectACL

-- instance Rq PutObjectACL where
--     type Er PutObjectACL = S3ErrorResponse
--     type Rs PutObjectACL = PutObjectACLResponse
--     request PutObjectACL{..} =
--         object PUT poaclBucket (poaclKey <> "?acl") poaclHeaders Empty
--     response = headerRs

-- type PutObjectACLResponse = S3Response

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
data PutObjectCopy = PutObjectCopy
    { pocBucket    :: !Text
    , pocKey       :: !Text
    , pocSource    :: !Text
    , pocDirective :: !Directive
    , pocHeaders   :: [Header]
    }

instance Rq PutObjectCopy where
    type Er PutObjectCopy = S3ErrorResponse
    type Rs PutObjectCopy = PutObjectCopyResponse
    request PutObjectCopy{..} =
        object PUT pocBucket pocKey hs (RequestBodyBS "")
      where
        hs = [ ("X-AMZ-Copy-Source",        Text.encodeUtf8 pocSource)
             , ("X-AMZ-Metadata-Directive", BS.pack $ show pocDirective)
             ] ++ pocHeaders

    response = s3Response

type PutObjectCopyResponse = S3Response

-- x-amz-copy-source: /source_bucket/sourceObject
-- x-amz-metadata-directive: metadata_directive
-- x-amz-copy-source-if-match: etag
-- x-amz-copy-source-if-none-match: etag
-- x-amz-copy-source-if-unmodified-since: time_stamp
-- x-amz-copy-source-if-modified-since: time_stamp

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
-- data InitiateMultipartUpload = InitiateMultipartUpload
--     { imuBucket  :: !Text
--     , imuKey     :: !Text
--     , imuHeaders :: [AnyHeader]
--     }

-- deriving instance Show InitiateMultipartUpload

-- instance Rq InitiateMultipartUpload where
--     type Er InitiateMultipartUpload = S3ErrorResponse
--     type Rs InitiateMultipartUpload = InitiateMultipartUploadResponse
--     request InitiateMultipartUpload{..} =
--         object POST imuBucket (imuKey <> "?uploads") imuHeaders Empty

-- data InitiateMultipartUploadResponse = InitiateMultipartUploadResponse
--     { imurBucket   :: !Text
--     , imurKey      :: !Text
--     , imurUploadId :: !Text
--     } deriving (Eq, Show, Generic)

-- instance IsXML InitiateMultipartUploadResponse where
--     xmlPickler = withRootNS s3NS "InitiateMultipartUploadResult"

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
-- data UploadPart = UploadPart
--     { upBucket     :: !Text
--     , upKey        :: !Text
--     , upPartNumber :: !Text
--     , upUploadId   :: !Text
--     , upHeaders    :: [AnyHeader]
--     , upBody       :: !Body
--  -- Content Length ?
--     }

-- deriving instance Show UploadPart

-- instance Rq UploadPart where
--     type Er UploadPart = S3ErrorResponse
--     type Rs UploadPart = UploadPartResponse
--     request UploadPart{..} = object PUT upBucket path upHeaders upBody
--       where
--         path = Text.concat
--             [ upKey
--             , "?partNumber="
--             , upPartNumber
--             , "&uploadId="
--             , upUploadId
--             ]

--     response = headerRs

-- type UploadPartResponse = S3Response

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
--     { upcBucket     :: !Text
--     , upcKey        :: !Text
--     , upcSource     :: !CopySource
--     , upcPartNumber :: !Text
--     , upcUploadId  :: !Text
--     , upcHeaders    :: [AnyHeader]
--     }

-- deriving instance Show UploadPartCopy

-- instance Rq UploadPartCopy where
--     type Er UploadPartCopy = S3ErrorResponse
--     type Rs UploadPartCopy = UploadPartCopyResponse
--     request UploadPartCopy{..} =
--         object PUT upcBucket path (hdr upcSource : upcHeaders) Empty
--       where
--         path = Text.concat
--             [ upcKey
--             , "?partNumber="
--             , upcPartNumber
--             , "&uploadId="
--             , upcUploadId
--             ]

--     response = headerRs

-- type UploadPartCopyResponse = S3Response

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
-- data CompleteMultipartUpload = CompleteMultipartUpload
--     { cmuBucket   :: !Text
--     , cmuKey      :: !Text
--     , cmuUploadId :: !Text
-- --   Content Length?
--     , cmuHeaders  :: [AnyHeader]
--     , cmuParts    :: [Part]
--     }

-- deriving instance Show CompleteMultipartUpload

-- instance Rq CompleteMultipartUpload where
--     type Er CompleteMultipartUpload = S3ErrorResponse
--     type Rs CompleteMultipartUpload = CompleteMultipartUploadResponse
--     request CompleteMultipartUpload{..} = undefined
-- --        object POST cmuBucket path cmuHeaders . Strict $ toXML ""
--       where
--         path = Text.concat [cmuKey, "&uploadId=", cmuUploadId]

--     response = undefined

-- type CompleteMultipartUploadResponse = S3Response

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
-- data AbortMultipartUpload = AbortMultipartUpload
--     { amuBucket   :: !Text
--     , amuKey      :: !Text
--     , amuUploadId :: !Text -- UploadId
--     } deriving (Eq, Show, Generic)
-- --  "uri": "/{Bucket}/{Key}?uploadId={UploadId}"

-- instance Rq AbortMultipartUpload where
--     type Er AbortMultipartUpload = S3ErrorResponse
--     type Rs AbortMultipartUpload = AbortMultipartUploadResponse
--     request AbortMultipartUpload{..} = undefined
--     response = undefined

-- -- NoSuchUpload = 404
-- -- The specified multipart upload does not exist.
-- -- The upload ID might be invalid, or the multipart upload might have been aborted or completed.
-- -- empty response

-- data AbortMultipartUploadResponse = AbortMultipartUploadResponse
--     deriving (Eq, Show, Generic)

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
-- data ListParts = ListParts
--     {
--     }

-- deriving instance Show ListParts

-- instance Rq ListParts where
--     type Er ListParts = S3ErrorResponse
--     type Rs ListParts = ListPartsResponse
--     request ListParts{..} = undefined
--     response = undefined

-- type ListPartsResponse = S3Response
