{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Data.ByteString      (ByteString)
import           Data.Monoid
import           Data.Text            (Text)
import           Data.Time
import           Network.AWS.Internal
import qualified Text.Read            as Read

-- | Currently supported version of the S3 service.
s3 :: ByteString -> Service
s3 b = Service Global (versionS3 b) "s3" "2006-03-01"

-- | XML namespace to annotate S3 elements with.
-- FIXME: Some S3 namespaces have the doc.* prefix?
s3NS :: ByteString
s3NS = "http://s3.amazonaws.com/doc/" <> svcVersion (s3 "") <> "/"

-- | Helper to define S3 namespaced XML elements.
s3Elem :: ByteString -> NName ByteString
s3Elem = mkNName s3NS

data S3ErrorResponse = S3ErrorResponse
    { serMessage  :: !Text
    , serResource :: !Text
    , serCode     :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML S3ErrorResponse where
    xmlPickler = (genericXMLPickler defaultXMLOptions)
        { root = Just $ mkAnNName "Error"
        }

instance ToError S3ErrorResponse where
    toError = Err . show

data Bucket = Bucket
    { bName         :: !Text
      -- ^ Bucket's name.
    , bCreationDate :: !UTCTime
      -- ^ Date the bucket was created.
    } deriving (Eq, Show, Generic)

instance IsXML Bucket

newtype Delimiter = Delimiter { unDelimiter :: Char }
    deriving (Eq, Show)

instance IsQuery Delimiter where
    queryPickler = (Delimiter, unDelimiter) `qpWrap` qpPrim

--
-- GetBucket
--

data Contents = Contents
    { bcKey          :: !Text
    , bcLastModified :: !UTCTime
    , bcETag         :: !ETag
    , bcSize         :: !Integer
    , bcStorageClass :: !StorageClass
    , bcVersionId    :: Maybe Text
--    , bcOwner        :: !Owner
    } deriving (Eq, Show, Generic)

instance IsXML Contents where
    xmlPickler = withRootNS s3NS "Contents"

--
-- GetVersions
--

data Version = Version
    { vKey          :: !Text
    , vVersionId    :: !Text
    , vIsLatest     :: !Bool
    , vLastModified :: !UTCTime
    , vETag         :: !ETag
    , vSize         :: !Integer
    , vStorageClass :: !StorageClass
    } deriving (Eq, Show, Generic)

instance IsXML Version where
    xmlPickler = withRootNS s3NS "Version"

--
-- PutBucket
--

data CreateBucketConfiguration = CreateBucketConfiguration
    { gbcLocationConstraint :: !Region
    } deriving (Eq, Show, Generic)

instance IsXML CreateBucketConfiguration where
    xmlPickler = withNS s3NS

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
        pu = xpWrap (uncurry DMObjects, \(DMObjects q os) -> (q, os)) $
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

-- FIXME: Grantee Values
-- You can specify the person (grantee) to whom you're assigning access rights (using request elements) in the following ways:

-- By the person's ID:

-- <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser"><ID>ID</ID><DisplayName>GranteesEmail</DisplayName>
-- </Grantee>
-- DisplayName is optional and ignored in the request.

-- By Email address:

-- <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="AmazonCustomerByEmail"><EmailAddress>Grantees@email.com</EmailAddress>lt;/Grantee>
-- The grantee is resolved to the CanonicalUser and, in a response to a GET Object acl request, appears as the CanonicalUser.

-- By URI:

-- <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group"><URI>http://acs.amazonaws.com/groups/global/AuthenticatedUsers</URI></Grantee>

data Grant = Grant
    { gGrantee    :: !Grantee
    , gPermission :: !Text -- FULL_CONTROL | WRITE | READ_ACP
    } deriving (Eq, Show, Generic)

instance IsXML Grant

-- <AccessControlPolicy>
--   <Owner>
--     <ID>75aa57f09aa0c8caeab4f8c24e99d10f8e7faeebf76c078efc7c6caea54ba06a</ID>
--     <DisplayName>mtd@amazon.com</DisplayName>
--   </Owner>
--   <AccessControlList>
--     <Grant>
--       <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser">
--         <ID>75aa57f09aa0c8caeab4f8c24e99d10f8e7faeebf76c078efc7c6caea54ba06a</ID>
--         <DisplayName>mtd@amazon.com</DisplayName>
--       </Grantee>
--       <Permission>FULL_CONTROL</Permission>
--     </Grant>
--   </AccessControlList>
-- </AccessControlPolicy>

data AccessControlList = AccessControlList
    { aclGrant :: [Grant]
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

--
-- CompleteMultipartUpload
--

data Part = Part
   { pPartNumber :: !Text
   , pETag       :: !ETag
   } deriving (Eq, Show, Generic)

instance IsXML Part

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

-- instance IsHeader AES256 where
--     encodeHeader = encodeHeader . show

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

data StorageClass
    = Standard
    | ReducedRedundancy
    | Glacier
      deriving (Eq)

instance Show StorageClass where
    show Standard          = "STANDARD"
    show ReducedRedundancy = "REDUCED_REDUNDANCY"
    show Glacier           = "GLACIER"

instance Read StorageClass where
    readPrec = readAssocList
        [ ("STANDARD",           Standard)
        , ("REDUCED_REDUNDANCY", ReducedRedundancy)
        , ("GLACIER",            Glacier)
        ]

instance IsXML StorageClass where
    xmlPickler = xpContent xpPrim

data Directive
    = Copy
    | Replace

data Source = Source
    { srcBucket :: !Text
    , srcKey    :: !Text
    } deriving (Eq, Show)

instance Show Directive where
    show Copy    = "COPY"
    show Replace = "REPLACE"

newtype ETag = ETag { unETag :: Text }
    deriving (Eq, Show)

instance IsXML ETag where
    xmlPickler = (ETag, unETag) `xpWrap` xmlPickler

-- instance IsHeader ETag where
--     encodeHeader (ETag t) = encodeHeader t

-- type Metadata = Header "x-amz-meta-" (Text, Text)

-- type Storage = Header "x-amz-storage-class" StorageClass

-- type ACL = Header "x-amz-acl" CannedACL

-- -- type GrantRead        = Header "x-amz-grant-read" Text
-- -- type GrantWrite       = Header "x-amz-grant-write" Text
-- -- type GrantReadACP     = Header "x-amz-grant-read-acp" Text
-- -- type GrantWriteACP    = Header "x-amz-grant-write-acp" Text
-- -- type GrantFullControl = Header "x-amz-grant-full-control" Text

-- -- FIXME:
-- -- For each of these headers, the value is a comma-separated list of one or more grantees. You specify each grantee as a type=value pair, where the type can be one of the following:

-- -- emailAddress — if value specified is the email address of an AWS account
-- -- id — if value specified is the canonical user ID of an AWS account
-- -- uri — if granting permission to a predefined group.

-- -- For example, the following x-amz-grant-read header grants list objects permission to the two AWS accounts identified by their email addresses.
-- -- x-amz-grant-read: emailAddress="xyz@amazon.com", emailAddress="abc@amazon.com"

-- -- | When a bucket is configured as a website, you can set this metadata on the
-- -- object so the website endpoint will evaluate the request for the object as
-- -- a 301 redirect to another object in the same bucket or an external URL.
-- type RedirectLocation = Header "x-amz-website-redirect-location" Text

-- -- | If the object is stored using server-side encryption, response includes
-- -- this header with value of the encryption algorithm used.
-- type Encryption = Header "x-amz-server-side-encryption" AES256

-- -- | Sets the Content-Type header of the response.
-- type ResponseContentType = Header "response-content-type" Text

-- -- | Sets the Content-Language header of the response.
-- type ResponseContentLanguage = Header "response-content-language" Text

-- -- | Sets the Expires header of the response.
-- type ResponseExpires = Header "response-expires" Text

-- -- | Sets the Cache-Control header of the response.
-- type ResponseCacheControl = Header "response-cache-control" Text

-- -- | Sets the Content-Disposition header of the response.
-- type ResponseContentDisposition = Header "response-content-disposition" Text

-- -- | Sets the Content-Encoding header of the response.
-- type ResponseContentEncoding = Header "response-content-encoding" Text

-- -- | Specifies whether the object retrieved was (true) or was not (false)
-- -- a Delete Marker.
-- --
-- -- If false, this response header does not appear in the response.
-- type DeleteMarker = Header "x-amz-delete-marker" Bool

-- -- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- -- response includes this header. It includes the expiry-date and rule-id key
-- -- value pairs providing object expiration information.
-- -- The value of the rule-id is URL encoded.
-- type Expiration = Header "x-amz-expiration" UTCTime

-- -- | Provides information about object restoration operation and expiration time
-- -- of the restored object copy.
-- type Restore = Header "x-amz-restore" Text

-- -- | Returns the version ID of the retrieved object if it has a unique version ID.
-- type VersionId = Header "x-amz-version-id" Text

-- -- | The value is the concatenation of the authentication device's serial number,
-- -- a space, and the value displayed on your authentication device.
-- --
-- -- Condition: Required to permanently delete a versioned object if versioning
-- -- is configured with MFA Delete enabled.
-- type MFA = Header "x-amz-mfa" Text

-- -- | Identifies the origin of the cross-origin request to Amazon S3.
-- --
-- -- For example, http://www.example.com.
-- type Origin = Header "origin" Text

-- -- | Identifies what HTTP method will be used in the actual request.
-- type AccessControlRequestMethod = Header "access-control-request-method" Method

-- -- | A comma-delimited list of HTTP headers that will be sent in the
-- -- actual request.
-- type AccessControlRequestHeaders = Header "access-control-request-headers" [Text]

-- -- | The name of the source bucket and key name of the source object,
-- -- separated by a slash (/).
-- --
-- -- This string must be URL-encoded. Additionally, the source bucket must be
-- -- valid and you must have READ access to the valid source object.
-- --
-- -- If the source object is archived in Amazon Glacier (storage class of the
-- -- object is GLACIER), you must first restore a temporary copy using the POST
-- -- Object restore. Otherwise, Amazon S3 returns the 403 ObjectNotInActiveTierError
-- -- error response.
-- type CopySource = Header "x-amz-copy-source" Source

-- -- | The range of bytes to copy from the source object.
-- --
-- -- The range value must use the form bytes=first-last, where the first and last
-- -- are the zero-based byte offsets to copy.
-- --
-- -- For example, bytes=0-9 indicates that you want to copy the first ten bytes
-- -- of the source.
-- type CopySourceRange = Header "x-amz-copy-source-range" Text

-- -- | Specifies whether the metadata is copied from the source object or replaced
-- -- with metadata provided in the request.
-- --
-- -- If copied, the metadata, except for the version ID, remains unchanged.
-- -- In addition, the server-side-encryption, storage-class, and
-- -- website-redirect-location metadata from the source is not copied.
-- -- If you specify this metadata explicitly in the copy request, Amazon S3
-- -- adds this metadata to the resulting object. If you specify headers in the
-- -- request specifying any user-defined metadata, Amazon S3 ignores these headers.
-- --
-- -- If replaced, all original metadata is replaced by the metadata you specify.
-- type MetadataDirective = Header "x-amz-metadata-directive" Directive

-- -- | Copies the object if its entity tag (ETag) matches the specified tag;
-- -- otherwise, the request returns a 412 HTTP status code error.
-- --
-- -- Constraints: This header can be used with x-amz-copy-source-if-unmodified-since,
-- -- but cannot be used with other conditional copy headers.
-- type CopySourceIfMatch = Header "x-amz-copy-source-if-match" ETag

-- -- | Copies the object if its entity tag (ETag) is different than the specified ETag;
-- -- otherwise, the request returns a 412 HTTP status code error.
-- --
-- -- Constraints: This header can be used with x-amz-copy-source-if-modified-since,
-- -- but cannot be used with other conditional copy headers.
-- type CopySourceIfNoneMatch = Header "x-amz-copy-source-if-none-match" ETag

-- -- | Copies the object if it hasn't been modified since the specified time;
-- -- otherwise, the request returns a 412 HTTP status code error.
-- type CopySourceIfUnmodifiedSince = Header "x-amz-copy-source-if-unmodified-since" UTCTime

-- -- | Copies the object if it has been modified since the specified time;
-- -- otherwise, the request returns a 412 HTTP status code error.
-- type CopySourceIfModifiedSince = Header "x-amz-copy-source-if-modified-since" UTCTime
