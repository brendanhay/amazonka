{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.CopyObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a copy of an object that is already stored in Amazon S3.
module Network.AWS.S3.V2006_03_01.CopyObject where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

type PutObjectCopy = CopyObject
type PutObjectCopyResponse = Rs CopyObject

-- | Default CopyObject request.
copyObject :: Text -- ^ 'corCopySource'
           -> BucketName -- ^ 'corBucket'
           -> ObjectKey -- ^ 'corKey'
           -> CopyObject
copyObject p1 p2 p3 = CopyObject
    { corCopySource = p1
    , corBucket = p2
    , corKey = p3
    , corMetadata = mempty
    , corCacheControl = Nothing
    , corContentDisposition = Nothing
    , corContentEncoding = Nothing
    , corContentLanguage = Nothing
    , corContentType = Nothing
    , corCopySourceIfMatch = Nothing
    , corCopySourceIfModifiedSince = Nothing
    , corCopySourceIfNoneMatch = Nothing
    , corCopySourceIfUnmodifiedSince = Nothing
    , corCopySourceSSECustomerAlgorithm = Nothing
    , corCopySourceSSECustomerKey = Nothing
    , corCopySourceSSECustomerKeyMD5 = Nothing
    , corExpires = Nothing
    , corGrantFullControl = Nothing
    , corGrantRead = Nothing
    , corGrantReadACP = Nothing
    , corGrantWriteACP = Nothing
    , corMetadataDirective = Nothing
    , corACL = Nothing
    , corSSECustomerAlgorithm = Nothing
    , corSSECustomerKey = Nothing
    , corSSECustomerKeyMD5 = Nothing
    , corServerSideEncryption = Nothing
    , corStorageClass = Nothing
    , corWebsiteRedirectLocation = Nothing
    }

data CopyObject = CopyObject
    { corCopySource :: Text
      -- ^ The name of the source bucket and key name of the source object,
      -- separated by a slash (/). Must be URL-encoded.
    , corBucket :: BucketName
    , corKey :: ObjectKey
    , corMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , corCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , corContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , corContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object
      -- and thus what decoding mechanisms must be applied to obtain the
      -- media-type referenced by the Content-Type header field.
    , corContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , corContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , corCopySourceIfMatch :: Maybe Text
      -- ^ Copies the object if its entity tag (ETag) matches the specified
      -- tag.
    , corCopySourceIfModifiedSince :: Maybe RFC822
      -- ^ Copies the object if it has been modified since the specified
      -- time.
    , corCopySourceIfNoneMatch :: Maybe Text
      -- ^ Copies the object if its entity tag (ETag) is different than the
      -- specified ETag.
    , corCopySourceIfUnmodifiedSince :: Maybe RFC822
      -- ^ Copies the object if it hasn't been modified since the specified
      -- time.
    , corCopySourceSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use when decrypting the source object
      -- (e.g., AES256).
    , corCopySourceSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use to decrypt the source object. The encryption key provided in
      -- this header must be one that was used when the source object was
      -- created.
    , corCopySourceSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , corExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , corGrantFullControl :: Maybe Text
      -- ^ Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on
      -- the object.
    , corGrantRead :: Maybe Text
      -- ^ Allows grantee to read the object data and its metadata.
    , corGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the object ACL.
    , corGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable object.
    , corMetadataDirective :: Maybe MetadataDirective
      -- ^ Specifies whether the metadata is copied from the source object
      -- or replaced with metadata provided in the request.
    , corACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    , corSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , corSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header.
    , corSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , corServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , corStorageClass :: Maybe StorageClass
      -- ^ The type of storage to use for the object. Defaults to
      -- 'STANDARD'.
    , corWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    } deriving (Eq, Show, Generic)

instance ToPath CopyObject where
    toPath CopyObject{..} = mconcat
        [ "/"
        , toBS corBucket
        , "/"
        , toBS corKey
        ]

instance ToQuery CopyObject

instance ToHeaders CopyObject where
    toHeaders CopyObject{..} = concat
        [ "x-amz-copy-source" =: corCopySource
        , "x-amz-meta-" =: corMetadata
        , "Cache-Control" =: corCacheControl
        , "Content-Disposition" =: corContentDisposition
        , "Content-Encoding" =: corContentEncoding
        , "Content-Language" =: corContentLanguage
        , "Content-Type" =: corContentType
        , "x-amz-copy-source-if-match" =: corCopySourceIfMatch
        , "x-amz-copy-source-if-modified-since" =: corCopySourceIfModifiedSince
        , "x-amz-copy-source-if-none-match" =: corCopySourceIfNoneMatch
        , "x-amz-copy-source-if-unmodified-since" =: corCopySourceIfUnmodifiedSince
        , "x-amz-copy-source-server-side-encryption-customer-algorithm" =: corCopySourceSSECustomerAlgorithm
        , "x-amz-copy-source-server-side-encryption-customer-key" =: corCopySourceSSECustomerKey
        , "x-amz-copy-source-server-side-encryption-customer-key-MD5" =: corCopySourceSSECustomerKeyMD5
        , "Expires" =: corExpires
        , "x-amz-grant-full-control" =: corGrantFullControl
        , "x-amz-grant-read" =: corGrantRead
        , "x-amz-grant-read-acp" =: corGrantReadACP
        , "x-amz-grant-write-acp" =: corGrantWriteACP
        , "x-amz-metadata-directive" =: corMetadataDirective
        , "x-amz-acl" =: corACL
        , "x-amz-server-side-encryption-customer-algorithm" =: corSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: corSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: corSSECustomerKeyMD5
        , "x-amz-server-side-encryption" =: corServerSideEncryption
        , "x-amz-storage-class" =: corStorageClass
        , "x-amz-website-redirect-location" =: corWebsiteRedirectLocation
        ]

instance ToBody CopyObject

instance AWSRequest CopyObject where
    type Sv CopyObject = S3

    request  = put
    response = xmlResponse

data instance Rs CopyObject = CopyObjectResponse
    { cooCopyObjectResult :: Maybe CopyObjectResult
    , cooCopySourceVersionId :: Maybe Text
    , cooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, the response includes
      -- this header.
    , cooSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , cooSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , cooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Eq, Show, Generic)
