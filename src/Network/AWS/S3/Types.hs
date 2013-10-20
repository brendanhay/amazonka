{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
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

import           Network.AWS.S3.Headers
import           Control.Applicative    ((<$>))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.Monoid
import           Data.String
import           Data.Text              (Text)
import           Data.Text.Encoding
import           Data.Time
import           Network.AWS.Internal


s3Service :: Service
s3Service = Service "s3" s3Version SigningVersion2 $
    Global "s3.amazonaws.com"

-- | Currently supported version (2006-03-01) of the S3 service.
s3Version :: ServiceVersion
s3Version = "2006-03-01"

-- | XML namespace to annotate S3 elements with.
s3NS :: ByteString
s3NS = "http://s3.amazonaws.com/doc/" <> sPack s3Version <> "/"

-- | Helper to define S3 namespaced XML elements.
s3Elem :: ByteString -> NName ByteString
s3Elem = mkNName s3NS

data S3ErrorResponse = S3ErrorResponse { sssError :: !Text }
    deriving (Eq, Show, Generic)

instance ToError S3ErrorResponse where
    toError = Err . show

instance IsXML S3ErrorResponse



--
-- Service
--

data Owner = Owner
    { oDisplayName :: !Text
      -- ^ Bucket owner's display name.
    , oID          :: !Text
      -- ^ Bucket owner's user ID.
    } deriving (Eq, Show, Generic)

instance IsXML Owner where
    xmlPickler = withNS s3NS

data Bucket = Bucket
    { bName         :: !Text
      -- ^ Bucket's name.
    , bCreationDate :: !UTCTime
      -- ^ Date the bucket was created.
    } deriving (Eq, Show, Generic)

instance IsXML Bucket where
    xmlPickler = withNS s3NS

-- type Bucket = T.Text

-- data BucketInfo
--     = BucketInfo {
--         bucketName         :: Bucket
--       , bucketCreationDate :: UTCTime
--       }
--     deriving (Show)

-- type Object = T.Text

-- data ObjectId
--     = ObjectId {
--         oidBucket :: Bucket
--       , oidObject :: Object
--       , oidVersion :: Maybe T.Text
--       }
--     deriving (Show)

-- data ObjectInfo
--     = ObjectInfo {
--         objectKey          :: T.Text
--       , objectLastModified :: UTCTime
--       , objectETag         :: T.Text
--       , objectSize         :: Integer
--       , objectStorageClass :: StorageClass
--       , objectOwner        :: UserInfo
--       }
--     deriving (Show)

--
-- Buckets
--

--
-- Objects
--

--
-- Primitives
--

data AES256 = AES256 deriving (Show)

instance ToHeader AES256 where
    header = header . show

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

instance ToHeader CannedACL where
    header = header . show

data StorageClass
    = Standard
    | ReducedRedundancy

instance Show StorageClass where
    show Standard          = "STANDARD"
    show ReducedRedundancy = "REDUCED_REDUNDANCY"

instance ToHeader StorageClass where
    header = header . show

--
-- Headers
--

type ContentLength      = Hdr "Content-Length" Integer
type CacheControl       = Hdr "Cache-Control" Text
type ContentDisposition = Hdr "Content-Disposition" Text
type ContentEncoding    = Hdr "Content-Encoding" Text
type ContentMD5         = Hdr "Content-MD5" Text
type ContentType        = Hdr "Content-Type" Text
type Expect             = Hdr "Expect" Text
type Expires            = Hdr "Expires" Text
type Range              = Hdr "Range" Text
type IfModifiedSince    = Hdr "If-Modified-Since" Text
type IfUnmodifiedSince  = Hdr "If-Unmodified-Since" Text
type IfMatch            = Hdr "If-Match" Text
type IfNoneMatch        = Hdr "If-None-Match" Text
type Storage            = Hdr "x-amz-storage-class" StorageClass
type RedirectLocation   = Hdr "x-amz-website-redirect-location" Text
type Encryption         = Hdr "x-amz-server-side-encryption" AES256
type ACL                = Hdr "x-amz-acl" CannedACL
type GrantRead          = Hdr "x-amz-grant-read" Text
type GrantWrite         = Hdr "x-amz-grant-write" Text
type GrantReadACP       = Hdr "x-amz-grant-read-acp" Text
type GrantWriteACP      = Hdr "x-amz-grant-write-acp" Text
type GrantFullControl   = Hdr "x-amz-grant-full-control" Text
type Metadata           = Hdr "x-amz-meta-" (Text, Text)
