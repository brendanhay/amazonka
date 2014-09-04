{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.S3.V2006_03_01.CopyObject
    (
    -- * Request
      CopyObject
    -- ** Request constructor
    , copyObject
    -- ** Request lenses
    , corCopySource
    , corBucket
    , corKey
    , corCacheControl
    , corContentDisposition
    , corContentEncoding
    , corContentLanguage
    , corContentType
    , corCopySourceIfMatch
    , corCopySourceIfModifiedSince
    , corCopySourceIfNoneMatch
    , corCopySourceIfUnmodifiedSince
    , corCopySourceSSECustomerAlgorithm
    , corCopySourceSSECustomerKey
    , corCopySourceSSECustomerKeyMD5
    , corExpires
    , corGrantFullControl
    , corGrantRead
    , corGrantReadACP
    , corGrantWriteACP
    , corMetadata
    , corMetadataDirective
    , corACL
    , corSSECustomerAlgorithm
    , corSSECustomerKey
    , corSSECustomerKeyMD5
    , corServerSideEncryption
    , corStorageClass
    , corWebsiteRedirectLocation

    -- * Response
    , CopyObjectResponse
    -- ** Response lenses
    , cooCopyObjectResult
    , cooCopySourceVersionId
    , cooExpiration
    , cooSSECustomerAlgorithm
    , cooSSECustomerKeyMD5
    , cooServerSideEncryption
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type PutObjectCopy = CopyObject

-- | Minimum specification for a 'CopyObject' request.
copyObject :: Text -- ^ 'corCopySource'
           -> BucketName -- ^ 'corBucket'
           -> ObjectKey -- ^ 'corKey'
           -> CopyObject
copyObject p1 p2 p3 = CopyObject
    { _corCopySource = p1
    , _corBucket = p2
    , _corKey = p3
    , _corCacheControl = Nothing
    , _corContentDisposition = Nothing
    , _corContentEncoding = Nothing
    , _corContentLanguage = Nothing
    , _corContentType = Nothing
    , _corCopySourceIfMatch = Nothing
    , _corCopySourceIfModifiedSince = Nothing
    , _corCopySourceIfNoneMatch = Nothing
    , _corCopySourceIfUnmodifiedSince = Nothing
    , _corCopySourceSSECustomerAlgorithm = Nothing
    , _corCopySourceSSECustomerKey = Nothing
    , _corCopySourceSSECustomerKeyMD5 = Nothing
    , _corExpires = Nothing
    , _corGrantFullControl = Nothing
    , _corGrantRead = Nothing
    , _corGrantReadACP = Nothing
    , _corGrantWriteACP = Nothing
    , _corMetadata = mempty
    , _corMetadataDirective = Nothing
    , _corACL = Nothing
    , _corSSECustomerAlgorithm = Nothing
    , _corSSECustomerKey = Nothing
    , _corSSECustomerKeyMD5 = Nothing
    , _corServerSideEncryption = Nothing
    , _corStorageClass = Nothing
    , _corWebsiteRedirectLocation = Nothing
    }
{-# INLINE copyObject #-}

data CopyObject = CopyObject
    { _corCopySource :: Text
      -- ^ The name of the source bucket and key name of the source object,
      -- separated by a slash (/). Must be URL-encoded.
    , _corBucket :: BucketName
    , _corKey :: ObjectKey
    , _corCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , _corContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , _corContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object
      -- and thus what decoding mechanisms must be applied to obtain the
      -- media-type referenced by the Content-Type header field.
    , _corContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , _corContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , _corCopySourceIfMatch :: Maybe Text
      -- ^ Copies the object if its entity tag (ETag) matches the specified
      -- tag.
    , _corCopySourceIfModifiedSince :: Maybe RFC822
      -- ^ Copies the object if it has been modified since the specified
      -- time.
    , _corCopySourceIfNoneMatch :: Maybe Text
      -- ^ Copies the object if its entity tag (ETag) is different than the
      -- specified ETag.
    , _corCopySourceIfUnmodifiedSince :: Maybe RFC822
      -- ^ Copies the object if it hasn't been modified since the specified
      -- time.
    , _corCopySourceSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use when decrypting the source object
      -- (e.g., AES256).
    , _corCopySourceSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use to decrypt the source object. The encryption key provided in
      -- this header must be one that was used when the source object was
      -- created.
    , _corCopySourceSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , _corExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , _corGrantFullControl :: Maybe Text
      -- ^ Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on
      -- the object.
    , _corGrantRead :: Maybe Text
      -- ^ Allows grantee to read the object data and its metadata.
    , _corGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the object ACL.
    , _corGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable object.
    , _corMetadata :: Map Text Text
      -- ^ A map of metadata to store with the object in S3.
    , _corMetadataDirective :: Maybe MetadataDirective
      -- ^ Specifies whether the metadata is copied from the source object
      -- or replaced with metadata provided in the request.
    , _corACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    , _corSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , _corSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header.
    , _corSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , _corServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _corStorageClass :: Maybe StorageClass
      -- ^ The type of storage to use for the object. Defaults to
      -- 'STANDARD'.
    , _corWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    } deriving (Show, Generic)

-- | The name of the source bucket and key name of the source object, separated
-- by a slash (/). Must be URL-encoded.
corCopySource :: Lens' CopyObject (Text)
corCopySource f x =
    f (_corCopySource x)
        <&> \y -> x { _corCopySource = y }
{-# INLINE corCopySource #-}

corBucket :: Lens' CopyObject (BucketName)
corBucket f x =
    f (_corBucket x)
        <&> \y -> x { _corBucket = y }
{-# INLINE corBucket #-}

corKey :: Lens' CopyObject (ObjectKey)
corKey f x =
    f (_corKey x)
        <&> \y -> x { _corKey = y }
{-# INLINE corKey #-}

-- | Specifies caching behavior along the request/reply chain.
corCacheControl :: Lens' CopyObject (Maybe Text)
corCacheControl f x =
    f (_corCacheControl x)
        <&> \y -> x { _corCacheControl = y }
{-# INLINE corCacheControl #-}

-- | Specifies presentational information for the object.
corContentDisposition :: Lens' CopyObject (Maybe Text)
corContentDisposition f x =
    f (_corContentDisposition x)
        <&> \y -> x { _corContentDisposition = y }
{-# INLINE corContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
corContentEncoding :: Lens' CopyObject (Maybe Text)
corContentEncoding f x =
    f (_corContentEncoding x)
        <&> \y -> x { _corContentEncoding = y }
{-# INLINE corContentEncoding #-}

-- | The language the content is in.
corContentLanguage :: Lens' CopyObject (Maybe Text)
corContentLanguage f x =
    f (_corContentLanguage x)
        <&> \y -> x { _corContentLanguage = y }
{-# INLINE corContentLanguage #-}

-- | A standard MIME type describing the format of the object data.
corContentType :: Lens' CopyObject (Maybe Text)
corContentType f x =
    f (_corContentType x)
        <&> \y -> x { _corContentType = y }
{-# INLINE corContentType #-}

-- | Copies the object if its entity tag (ETag) matches the specified tag.
corCopySourceIfMatch :: Lens' CopyObject (Maybe Text)
corCopySourceIfMatch f x =
    f (_corCopySourceIfMatch x)
        <&> \y -> x { _corCopySourceIfMatch = y }
{-# INLINE corCopySourceIfMatch #-}

-- | Copies the object if it has been modified since the specified time.
corCopySourceIfModifiedSince :: Lens' CopyObject (Maybe RFC822)
corCopySourceIfModifiedSince f x =
    f (_corCopySourceIfModifiedSince x)
        <&> \y -> x { _corCopySourceIfModifiedSince = y }
{-# INLINE corCopySourceIfModifiedSince #-}

-- | Copies the object if its entity tag (ETag) is different than the specified
-- ETag.
corCopySourceIfNoneMatch :: Lens' CopyObject (Maybe Text)
corCopySourceIfNoneMatch f x =
    f (_corCopySourceIfNoneMatch x)
        <&> \y -> x { _corCopySourceIfNoneMatch = y }
{-# INLINE corCopySourceIfNoneMatch #-}

-- | Copies the object if it hasn't been modified since the specified time.
corCopySourceIfUnmodifiedSince :: Lens' CopyObject (Maybe RFC822)
corCopySourceIfUnmodifiedSince f x =
    f (_corCopySourceIfUnmodifiedSince x)
        <&> \y -> x { _corCopySourceIfUnmodifiedSince = y }
{-# INLINE corCopySourceIfUnmodifiedSince #-}

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
corCopySourceSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
corCopySourceSSECustomerAlgorithm f x =
    f (_corCopySourceSSECustomerAlgorithm x)
        <&> \y -> x { _corCopySourceSSECustomerAlgorithm = y }
{-# INLINE corCopySourceSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header must
-- be one that was used when the source object was created.
corCopySourceSSECustomerKey :: Lens' CopyObject (Maybe Text)
corCopySourceSSECustomerKey f x =
    f (_corCopySourceSSECustomerKey x)
        <&> \y -> x { _corCopySourceSSECustomerKey = y }
{-# INLINE corCopySourceSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
corCopySourceSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
corCopySourceSSECustomerKeyMD5 f x =
    f (_corCopySourceSSECustomerKeyMD5 x)
        <&> \y -> x { _corCopySourceSSECustomerKeyMD5 = y }
{-# INLINE corCopySourceSSECustomerKeyMD5 #-}

-- | The date and time at which the object is no longer cacheable.
corExpires :: Lens' CopyObject (Maybe RFC822)
corExpires f x =
    f (_corExpires x)
        <&> \y -> x { _corExpires = y }
{-# INLINE corExpires #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
corGrantFullControl :: Lens' CopyObject (Maybe Text)
corGrantFullControl f x =
    f (_corGrantFullControl x)
        <&> \y -> x { _corGrantFullControl = y }
{-# INLINE corGrantFullControl #-}

-- | Allows grantee to read the object data and its metadata.
corGrantRead :: Lens' CopyObject (Maybe Text)
corGrantRead f x =
    f (_corGrantRead x)
        <&> \y -> x { _corGrantRead = y }
{-# INLINE corGrantRead #-}

-- | Allows grantee to read the object ACL.
corGrantReadACP :: Lens' CopyObject (Maybe Text)
corGrantReadACP f x =
    f (_corGrantReadACP x)
        <&> \y -> x { _corGrantReadACP = y }
{-# INLINE corGrantReadACP #-}

-- | Allows grantee to write the ACL for the applicable object.
corGrantWriteACP :: Lens' CopyObject (Maybe Text)
corGrantWriteACP f x =
    f (_corGrantWriteACP x)
        <&> \y -> x { _corGrantWriteACP = y }
{-# INLINE corGrantWriteACP #-}

-- | A map of metadata to store with the object in S3.
corMetadata :: Lens' CopyObject (Map Text Text)
corMetadata f x =
    f (_corMetadata x)
        <&> \y -> x { _corMetadata = y }
{-# INLINE corMetadata #-}

-- | Specifies whether the metadata is copied from the source object or replaced
-- with metadata provided in the request.
corMetadataDirective :: Lens' CopyObject (Maybe MetadataDirective)
corMetadataDirective f x =
    f (_corMetadataDirective x)
        <&> \y -> x { _corMetadataDirective = y }
{-# INLINE corMetadataDirective #-}

-- | The canned ACL to apply to the object.
corACL :: Lens' CopyObject (Maybe ObjectCannedACL)
corACL f x =
    f (_corACL x)
        <&> \y -> x { _corACL = y }
{-# INLINE corACL #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
corSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
corSSECustomerAlgorithm f x =
    f (_corSSECustomerAlgorithm x)
        <&> \y -> x { _corSSECustomerAlgorithm = y }
{-# INLINE corSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
corSSECustomerKey :: Lens' CopyObject (Maybe Text)
corSSECustomerKey f x =
    f (_corSSECustomerKey x)
        <&> \y -> x { _corSSECustomerKey = y }
{-# INLINE corSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
corSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
corSSECustomerKeyMD5 f x =
    f (_corSSECustomerKeyMD5 x)
        <&> \y -> x { _corSSECustomerKeyMD5 = y }
{-# INLINE corSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
corServerSideEncryption :: Lens' CopyObject (Maybe ServerSideEncryption)
corServerSideEncryption f x =
    f (_corServerSideEncryption x)
        <&> \y -> x { _corServerSideEncryption = y }
{-# INLINE corServerSideEncryption #-}

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
corStorageClass :: Lens' CopyObject (Maybe StorageClass)
corStorageClass f x =
    f (_corStorageClass x)
        <&> \y -> x { _corStorageClass = y }
{-# INLINE corStorageClass #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
corWebsiteRedirectLocation :: Lens' CopyObject (Maybe Text)
corWebsiteRedirectLocation f x =
    f (_corWebsiteRedirectLocation x)
        <&> \y -> x { _corWebsiteRedirectLocation = y }
{-# INLINE corWebsiteRedirectLocation #-}

instance ToPath CopyObject where
    toPath CopyObject{..} = mconcat
        [ "/"
        , toBS _corBucket
        , "/"
        , toBS _corKey
        ]

instance ToQuery CopyObject

instance ToHeaders CopyObject where
    toHeaders CopyObject{..} = concat
        [ "x-amz-copy-source" =: _corCopySource
        , "Cache-Control" =: _corCacheControl
        , "Content-Disposition" =: _corContentDisposition
        , "Content-Encoding" =: _corContentEncoding
        , "Content-Language" =: _corContentLanguage
        , "Content-Type" =: _corContentType
        , "x-amz-copy-source-if-match" =: _corCopySourceIfMatch
        , "x-amz-copy-source-if-modified-since" =: _corCopySourceIfModifiedSince
        , "x-amz-copy-source-if-none-match" =: _corCopySourceIfNoneMatch
        , "x-amz-copy-source-if-unmodified-since" =: _corCopySourceIfUnmodifiedSince
        , "x-amz-copy-source-server-side-encryption-customer-algorithm" =: _corCopySourceSSECustomerAlgorithm
        , "x-amz-copy-source-server-side-encryption-customer-key" =: _corCopySourceSSECustomerKey
        , "x-amz-copy-source-server-side-encryption-customer-key-MD5" =: _corCopySourceSSECustomerKeyMD5
        , "Expires" =: _corExpires
        , "x-amz-grant-full-control" =: _corGrantFullControl
        , "x-amz-grant-read" =: _corGrantRead
        , "x-amz-grant-read-acp" =: _corGrantReadACP
        , "x-amz-grant-write-acp" =: _corGrantWriteACP
        , "x-amz-meta-" =: _corMetadata
        , "x-amz-metadata-directive" =: _corMetadataDirective
        , "x-amz-acl" =: _corACL
        , "x-amz-server-side-encryption-customer-algorithm" =: _corSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _corSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _corSSECustomerKeyMD5
        , "x-amz-server-side-encryption" =: _corServerSideEncryption
        , "x-amz-storage-class" =: _corStorageClass
        , "x-amz-website-redirect-location" =: _corWebsiteRedirectLocation
        ]

instance ToBody CopyObject

data CopyObjectResponse = CopyObjectResponse
    { _cooCopyObjectResult :: Maybe CopyObjectResult
    , _cooCopySourceVersionId :: Maybe Text
    , _cooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, the response includes
      -- this header.
    , _cooSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _cooSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , _cooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Show, Generic)

cooCopyObjectResult :: Lens' CopyObjectResponse (Maybe CopyObjectResult)
cooCopyObjectResult f x =
    f (_cooCopyObjectResult x)
        <&> \y -> x { _cooCopyObjectResult = y }
{-# INLINE cooCopyObjectResult #-}

cooCopySourceVersionId :: Lens' CopyObjectResponse (Maybe Text)
cooCopySourceVersionId f x =
    f (_cooCopySourceVersionId x)
        <&> \y -> x { _cooCopySourceVersionId = y }
{-# INLINE cooCopySourceVersionId #-}

-- | If the object expiration is configured, the response includes this header.
cooExpiration :: Lens' CopyObjectResponse (Maybe RFC822)
cooExpiration f x =
    f (_cooExpiration x)
        <&> \y -> x { _cooExpiration = y }
{-# INLINE cooExpiration #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
cooSSECustomerAlgorithm :: Lens' CopyObjectResponse (Maybe Text)
cooSSECustomerAlgorithm f x =
    f (_cooSSECustomerAlgorithm x)
        <&> \y -> x { _cooSSECustomerAlgorithm = y }
{-# INLINE cooSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cooSSECustomerKeyMD5 :: Lens' CopyObjectResponse (Maybe Text)
cooSSECustomerKeyMD5 f x =
    f (_cooSSECustomerKeyMD5 x)
        <&> \y -> x { _cooSSECustomerKeyMD5 = y }
{-# INLINE cooSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cooServerSideEncryption :: Lens' CopyObjectResponse (Maybe ServerSideEncryption)
cooServerSideEncryption f x =
    f (_cooServerSideEncryption x)
        <&> \y -> x { _cooServerSideEncryption = y }
{-# INLINE cooServerSideEncryption #-}

instance AWSRequest CopyObject where
    type Sv CopyObject = S3
    type Rs CopyObject = CopyObjectResponse

    request = put
    response _ = cursorResponse $ \hs xml ->
        pure CopyObjectResponse
            <*> xml %|? "CopyObjectResult"
            <*> hs ~:? "x-amz-copy-source-version-id"
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
            <*> hs ~:? "x-amz-server-side-encryption"
