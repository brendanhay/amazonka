{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.S3.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.S3.Types where

import Data.ByteString      (ByteString)
import Data.Monoid
import Data.Text            (Text)
import Data.Time
import Network.AWS.Headers
import Network.AWS.Internal
import Network.Http.Client  (Method)

-- | Currently supported version of the S3 service.
s3 :: Service
s3 = Global "s3" "2006-03-01"

data S3ErrorResponse = S3ErrorResponse { sssError :: !Text }
    deriving (Eq, Show, Generic)

instance ToError S3ErrorResponse where
    toError = Err . show

instance IsXML S3ErrorResponse

newtype S3HeadersResponse = S3HeadersResponse [(ByteString, ByteString)]
    deriving (Eq, Show)

data Bucket = Bucket
    { bName         :: !Text
      -- ^ Bucket's name.
    , bCreationDate :: !UTCTime
      -- ^ Date the bucket was created.
    } deriving (Eq, Show, Generic)

instance IsXML Bucket

--
-- DeleteMultipleObjects
--

data DMObject = DMObject
    { oKey     :: !Text
    , oVersion :: Maybe Text
    } deriving (Eq, Show, Generic)

instance IsXML DMObject

data DMObjects = DMObjects
    { oQuiet   :: !Bool
    , oObjects :: [DMObject]
    } deriving (Eq, Show)

instance IsXML DMObjects where
    xmlPickler = pu { root = Just $ mkAnNName "Delete" }
      where
        pu = xpWrap (\(q, os) -> DMObjects q os, \(DMObjects q os) -> (q, os)) $
             xpPair (xpElem (mkAnNName "Quiet") xmlPickler)
                    (xpElemList (mkAnNName "Object") xmlPickler)

--
-- DeleteMultipleObjectsResponse
--

data DeletedObject = DeletedObject
    { dobjKey                   :: !Text
    , dobjVersionId             :: Maybe Text
    , dobjDeleteMarker          :: Maybe Bool
    , dobjDeleteMarkerVersionId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance IsXML DeletedObject where
    xmlPickler = withRootNS s3NS "Deleted"

data DeleteError = DeleteError
    { deKey       :: !Text
    , deVersionId :: Maybe Text
    , deCode      :: !Text
    , deMessage   :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteError where
    xmlPickler = withRootNS s3NS "Error"

--
-- GetObjectACLResponse
--

data Owner = Owner
    { oID          :: !Text
      -- ^ Bucket owner's user ID.
    , oDisplayName :: !Text
      -- ^ Bucket owner's display name.
    } deriving (Eq, Show, Generic)

instance IsXML Owner

data Grantee = Grantee
    { gID          :: !Text
    , gDisplayName :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML Grantee

data Grant = Grant
    { gGrantee    :: !Grantee
    , gPermission :: !Text -- FULL_CONTROL | WRITE | READ_ACP
    } deriving (Eq, Show, Generic)

instance IsXML Grant

data AccessControlList = AccessControlList
    { aclGrant :: !Grant
    } deriving (Eq, Show, Generic)

instance IsXML AccessControlList

data AccessControlPolicy = AccessControlPolicy
    { acpOwner             :: !Owner
    , acpAccessControlList :: !AccessControlList
    } deriving (Eq, Show, Generic)

instance IsXML AccessControlPolicy

--
-- PostObjectRestore
--

data RestoreRequest = RestoreRequest
    { rrDays :: !Int
    } deriving (Eq, Show, Generic)

instance IsXML RestoreRequest where
    xmlPickler = withNS s3NS

-- type Bucket = T.Text

-- data BucketInfo
--     = BucketInfo {
--         bucketName         :: Bucket
--       , bucketCreationDate :: UTCTime
--       }
--     deriving (Show)

-- type Object = T.Text

-- data Object = Object
--     { objectKey          :: T.Text
--     , objectLastModified :: UTCTime
--     , objectETag         :: T.Text
--     , objectSize         :: Integer
--     , objectStorageClass :: StorageClass
--     , objectOwner        :: UserInfo
--     } deriving (Eq, Show, Generic)

data AES256 = AES256 deriving (Show)

instance IsHeader AES256 where
    encodeHeader = encodeHeader . show

data CannedACL
    = Private
    | PublicRead
    | PublicReadWrite
    | AuthenticatedRead
    | BucketOwnerRead
    | BucketOwnerFullControl
    | LogDeliveryWrite

instance Show CannedACL where
    show acl = case acl of
        Private                -> "private"
        PublicRead             -> "public-read"
        PublicReadWrite        -> "public-read-write"
        AuthenticatedRead      -> "authenticated-read"
        BucketOwnerRead        -> "bucket-owner-read"
        BucketOwnerFullControl -> "bucket-owner-full-control"
        LogDeliveryWrite       -> "log-delivery-write"

instance IsHeader CannedACL where
    encodeHeader = encodeHeader . show

data StorageClass
    = Standard
    | ReducedRedundancy

instance Show StorageClass where
    show Standard          = "STANDARD"
    show ReducedRedundancy = "REDUCED_REDUNDANCY"

instance IsHeader StorageClass where
    encodeHeader = encodeHeader . show

type Metadata = Header "x-amz-meta-" (Text, Text)

type Storage = Header "x-amz-storage-class" StorageClass

type ACL = Header "x-amz-acl" CannedACL

type GrantRead        = Header "x-amz-grant-read" Text
type GrantWrite       = Header "x-amz-grant-write" Text
type GrantReadACP     = Header "x-amz-grant-read-acp" Text
type GrantWriteACP    = Header "x-amz-grant-write-acp" Text
type GrantFullControl = Header "x-amz-grant-full-control" Text

-- | When a bucket is configured as a website, you can set this metadata on the
-- object so the website endpoint will evaluate the request for the object as
-- a 301 redirect to another object in the same bucket or an external URL.
type RedirectLocation = Header "x-amz-website-redirect-location" Text

-- | If the object is stored using server-side encryption, response includes
-- this header with value of the encryption algorithm used.
type Encryption = Header "x-amz-server-side-encryption" AES256

-- | Sets the Content-Type header of the response.
type ResponseContentType = Header "response-content-type" Text

-- | Sets the Content-Language header of the response.
type ResponseContentLanguage = Header "response-content-language" Text

-- | Sets the Expires header of the response.
type ResponseExpires = Header "response-expires" Text

-- | Sets the Cache-Control header of the response.
type ResponseCacheControl = Header "response-cache-control" Text

-- | Sets the Content-Disposition header of the response.
type ResponseContentDisposition = Header "response-content-disposition" Text

-- | Sets the Content-Encoding header of the response.
type ResponseContentEncoding = Header "response-content-encoding" Text

-- | Specifies whether the object retrieved was (true) or was not (false)
-- a Delete Marker.
--
-- If false, this response header does not appear in the response.
type DeleteMarker = Header "x-amz-delete-marker" Bool

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id key
-- value pairs providing object expiration information.
-- The value of the rule-id is URL encoded.
type Expiration = Header "x-amz-expiration" UTCTime

-- | Provides information about object restoration operation and expiration time
-- of the restored object copy.
type Restore = Header "x-amz-restore" Text

-- | Returns the version ID of the retrieved object if it has a unique version ID.
type VersionId = Header "x-amz-version-id" Text

-- | The value is the concatenation of the authentication device's serial number,
-- a space, and the value displayed on your authentication device.
--
-- Condition: Required to permanently delete a versioned object if versioning
-- is configured with MFA Delete enabled.
type MFA = Header "x-amz-mfa" Text

-- | Identifies the origin of the cross-origin request to Amazon S3.
--
-- For example, http://www.example.com.
type Origin = Header "origin" Text

-- | Identifies what HTTP method will be used in the actual request.
type AccessControlRequestMethod = Header "access-control-request-method" Method

-- | A comma-delimited list of HTTP headers that will be sent in the
-- actual request.
type AccessControlRequestHeaders = Header "access-control-request-headers" [Text]

-- | XML namespace to annotate S3 elements with.
s3NS :: ByteString
s3NS = "http://doc.s3.amazonaws.com/" <> svcVersion s3

-- | Helper to define S3 namespaced XML elements.
s3Elem :: ByteString -> NName ByteString
s3Elem = mkNName s3NS
