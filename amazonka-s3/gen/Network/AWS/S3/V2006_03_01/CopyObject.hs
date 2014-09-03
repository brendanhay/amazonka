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
corCopySource
    :: Functor f
    => (Text
    -> f (Text))
    -> CopyObject
    -> f CopyObject
corCopySource f x =
    (\y -> x { _corCopySource = y })
       <$> f (_corCopySource x)
{-# INLINE corCopySource #-}

corBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> CopyObject
    -> f CopyObject
corBucket f x =
    (\y -> x { _corBucket = y })
       <$> f (_corBucket x)
{-# INLINE corBucket #-}

corKey
    :: Functor f
    => (ObjectKey
    -> f (ObjectKey))
    -> CopyObject
    -> f CopyObject
corKey f x =
    (\y -> x { _corKey = y })
       <$> f (_corKey x)
{-# INLINE corKey #-}

-- | Specifies caching behavior along the request/reply chain.
corCacheControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corCacheControl f x =
    (\y -> x { _corCacheControl = y })
       <$> f (_corCacheControl x)
{-# INLINE corCacheControl #-}

-- | Specifies presentational information for the object.
corContentDisposition
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corContentDisposition f x =
    (\y -> x { _corContentDisposition = y })
       <$> f (_corContentDisposition x)
{-# INLINE corContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
corContentEncoding
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corContentEncoding f x =
    (\y -> x { _corContentEncoding = y })
       <$> f (_corContentEncoding x)
{-# INLINE corContentEncoding #-}

-- | The language the content is in.
corContentLanguage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corContentLanguage f x =
    (\y -> x { _corContentLanguage = y })
       <$> f (_corContentLanguage x)
{-# INLINE corContentLanguage #-}

-- | A standard MIME type describing the format of the object data.
corContentType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corContentType f x =
    (\y -> x { _corContentType = y })
       <$> f (_corContentType x)
{-# INLINE corContentType #-}

-- | Copies the object if its entity tag (ETag) matches the specified tag.
corCopySourceIfMatch
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corCopySourceIfMatch f x =
    (\y -> x { _corCopySourceIfMatch = y })
       <$> f (_corCopySourceIfMatch x)
{-# INLINE corCopySourceIfMatch #-}

-- | Copies the object if it has been modified since the specified time.
corCopySourceIfModifiedSince
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> CopyObject
    -> f CopyObject
corCopySourceIfModifiedSince f x =
    (\y -> x { _corCopySourceIfModifiedSince = y })
       <$> f (_corCopySourceIfModifiedSince x)
{-# INLINE corCopySourceIfModifiedSince #-}

-- | Copies the object if its entity tag (ETag) is different than the specified
-- ETag.
corCopySourceIfNoneMatch
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corCopySourceIfNoneMatch f x =
    (\y -> x { _corCopySourceIfNoneMatch = y })
       <$> f (_corCopySourceIfNoneMatch x)
{-# INLINE corCopySourceIfNoneMatch #-}

-- | Copies the object if it hasn't been modified since the specified time.
corCopySourceIfUnmodifiedSince
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> CopyObject
    -> f CopyObject
corCopySourceIfUnmodifiedSince f x =
    (\y -> x { _corCopySourceIfUnmodifiedSince = y })
       <$> f (_corCopySourceIfUnmodifiedSince x)
{-# INLINE corCopySourceIfUnmodifiedSince #-}

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
corCopySourceSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corCopySourceSSECustomerAlgorithm f x =
    (\y -> x { _corCopySourceSSECustomerAlgorithm = y })
       <$> f (_corCopySourceSSECustomerAlgorithm x)
{-# INLINE corCopySourceSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header must
-- be one that was used when the source object was created.
corCopySourceSSECustomerKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corCopySourceSSECustomerKey f x =
    (\y -> x { _corCopySourceSSECustomerKey = y })
       <$> f (_corCopySourceSSECustomerKey x)
{-# INLINE corCopySourceSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
corCopySourceSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corCopySourceSSECustomerKeyMD5 f x =
    (\y -> x { _corCopySourceSSECustomerKeyMD5 = y })
       <$> f (_corCopySourceSSECustomerKeyMD5 x)
{-# INLINE corCopySourceSSECustomerKeyMD5 #-}

-- | The date and time at which the object is no longer cacheable.
corExpires
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> CopyObject
    -> f CopyObject
corExpires f x =
    (\y -> x { _corExpires = y })
       <$> f (_corExpires x)
{-# INLINE corExpires #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
corGrantFullControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corGrantFullControl f x =
    (\y -> x { _corGrantFullControl = y })
       <$> f (_corGrantFullControl x)
{-# INLINE corGrantFullControl #-}

-- | Allows grantee to read the object data and its metadata.
corGrantRead
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corGrantRead f x =
    (\y -> x { _corGrantRead = y })
       <$> f (_corGrantRead x)
{-# INLINE corGrantRead #-}

-- | Allows grantee to read the object ACL.
corGrantReadACP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corGrantReadACP f x =
    (\y -> x { _corGrantReadACP = y })
       <$> f (_corGrantReadACP x)
{-# INLINE corGrantReadACP #-}

-- | Allows grantee to write the ACL for the applicable object.
corGrantWriteACP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corGrantWriteACP f x =
    (\y -> x { _corGrantWriteACP = y })
       <$> f (_corGrantWriteACP x)
{-# INLINE corGrantWriteACP #-}

-- | A map of metadata to store with the object in S3.
corMetadata
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> CopyObject
    -> f CopyObject
corMetadata f x =
    (\y -> x { _corMetadata = y })
       <$> f (_corMetadata x)
{-# INLINE corMetadata #-}

-- | Specifies whether the metadata is copied from the source object or replaced
-- with metadata provided in the request.
corMetadataDirective
    :: Functor f
    => (Maybe MetadataDirective
    -> f (Maybe MetadataDirective))
    -> CopyObject
    -> f CopyObject
corMetadataDirective f x =
    (\y -> x { _corMetadataDirective = y })
       <$> f (_corMetadataDirective x)
{-# INLINE corMetadataDirective #-}

-- | The canned ACL to apply to the object.
corACL
    :: Functor f
    => (Maybe ObjectCannedACL
    -> f (Maybe ObjectCannedACL))
    -> CopyObject
    -> f CopyObject
corACL f x =
    (\y -> x { _corACL = y })
       <$> f (_corACL x)
{-# INLINE corACL #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
corSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corSSECustomerAlgorithm f x =
    (\y -> x { _corSSECustomerAlgorithm = y })
       <$> f (_corSSECustomerAlgorithm x)
{-# INLINE corSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
corSSECustomerKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corSSECustomerKey f x =
    (\y -> x { _corSSECustomerKey = y })
       <$> f (_corSSECustomerKey x)
{-# INLINE corSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
corSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corSSECustomerKeyMD5 f x =
    (\y -> x { _corSSECustomerKeyMD5 = y })
       <$> f (_corSSECustomerKeyMD5 x)
{-# INLINE corSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
corServerSideEncryption
    :: Functor f
    => (Maybe ServerSideEncryption
    -> f (Maybe ServerSideEncryption))
    -> CopyObject
    -> f CopyObject
corServerSideEncryption f x =
    (\y -> x { _corServerSideEncryption = y })
       <$> f (_corServerSideEncryption x)
{-# INLINE corServerSideEncryption #-}

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
corStorageClass
    :: Functor f
    => (Maybe StorageClass
    -> f (Maybe StorageClass))
    -> CopyObject
    -> f CopyObject
corStorageClass f x =
    (\y -> x { _corStorageClass = y })
       <$> f (_corStorageClass x)
{-# INLINE corStorageClass #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
corWebsiteRedirectLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObject
    -> f CopyObject
corWebsiteRedirectLocation f x =
    (\y -> x { _corWebsiteRedirectLocation = y })
       <$> f (_corWebsiteRedirectLocation x)
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

cooCopyObjectResult
    :: Functor f
    => (Maybe CopyObjectResult
    -> f (Maybe CopyObjectResult))
    -> CopyObjectResponse
    -> f CopyObjectResponse
cooCopyObjectResult f x =
    (\y -> x { _cooCopyObjectResult = y })
       <$> f (_cooCopyObjectResult x)
{-# INLINE cooCopyObjectResult #-}

cooCopySourceVersionId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObjectResponse
    -> f CopyObjectResponse
cooCopySourceVersionId f x =
    (\y -> x { _cooCopySourceVersionId = y })
       <$> f (_cooCopySourceVersionId x)
{-# INLINE cooCopySourceVersionId #-}

-- | If the object expiration is configured, the response includes this header.
cooExpiration
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> CopyObjectResponse
    -> f CopyObjectResponse
cooExpiration f x =
    (\y -> x { _cooExpiration = y })
       <$> f (_cooExpiration x)
{-# INLINE cooExpiration #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
cooSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObjectResponse
    -> f CopyObjectResponse
cooSSECustomerAlgorithm f x =
    (\y -> x { _cooSSECustomerAlgorithm = y })
       <$> f (_cooSSECustomerAlgorithm x)
{-# INLINE cooSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cooSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyObjectResponse
    -> f CopyObjectResponse
cooSSECustomerKeyMD5 f x =
    (\y -> x { _cooSSECustomerKeyMD5 = y })
       <$> f (_cooSSECustomerKeyMD5 x)
{-# INLINE cooSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cooServerSideEncryption
    :: Functor f
    => (Maybe ServerSideEncryption
    -> f (Maybe ServerSideEncryption))
    -> CopyObjectResponse
    -> f CopyObjectResponse
cooServerSideEncryption f x =
    (\y -> x { _cooServerSideEncryption = y })
       <$> f (_cooServerSideEncryption x)
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
