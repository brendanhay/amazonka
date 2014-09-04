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
    -- ** Request alias
    , PutObjectCopy
    -- ** Request constructor
    , mkCopyObjectRequest
    -- ** Request lenses
    , corACL
    , corBucket
    , corCacheControl
    , corContentDisposition
    , corContentEncoding
    , corContentLanguage
    , corContentType
    , corCopySource
    , corCopySourceIfMatch
    , corCopySourceIfModifiedSince
    , corCopySourceIfNoneMatch
    , corCopySourceIfUnmodifiedSince
    , corExpires
    , corGrantFullControl
    , corGrantRead
    , corGrantReadACP
    , corGrantWriteACP
    , corKey
    , corMetadata
    , corMetadataDirective
    , corServerSideEncryption
    , corStorageClass
    , corWebsiteRedirectLocation
    , corSSECustomerAlgorithm
    , corSSECustomerKey
    , corSSECustomerKeyMD5
    , corCopySourceSSECustomerAlgorithm
    , corCopySourceSSECustomerKey
    , corCopySourceSSECustomerKeyMD5

    -- * Response
    , CopyObjectResponse
    -- ** Response lenses
    , cooCopyObjectResult
    , cooExpiration
    , cooCopySourceVersionId
    , cooServerSideEncryption
    , cooSSECustomerAlgorithm
    , cooSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type PutObjectCopy = CopyObject

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CopyObject' request.
mkCopyObjectRequest :: BucketName -- ^ 'corBucket'
                    -> Text -- ^ 'corCopySource'
                    -> ObjectKey -- ^ 'corKey'
                    -> CopyObject
mkCopyObjectRequest p1 p2 p3 = CopyObject
    { _corACL = Nothing
    , _corBucket = p2
    , _corCacheControl = Nothing
    , _corContentDisposition = Nothing
    , _corContentEncoding = Nothing
    , _corContentLanguage = Nothing
    , _corContentType = Nothing
    , _corCopySource = p8
    , _corCopySourceIfMatch = Nothing
    , _corCopySourceIfModifiedSince = Nothing
    , _corCopySourceIfNoneMatch = Nothing
    , _corCopySourceIfUnmodifiedSince = Nothing
    , _corExpires = Nothing
    , _corGrantFullControl = Nothing
    , _corGrantRead = Nothing
    , _corGrantReadACP = Nothing
    , _corGrantWriteACP = Nothing
    , _corKey = p18
    , _corMetadata = mempty
    , _corMetadataDirective = Nothing
    , _corServerSideEncryption = Nothing
    , _corStorageClass = Nothing
    , _corWebsiteRedirectLocation = Nothing
    , _corSSECustomerAlgorithm = Nothing
    , _corSSECustomerKey = Nothing
    , _corSSECustomerKeyMD5 = Nothing
    , _corCopySourceSSECustomerAlgorithm = Nothing
    , _corCopySourceSSECustomerKey = Nothing
    , _corCopySourceSSECustomerKeyMD5 = Nothing
    }
{-# INLINE mkCopyObjectRequest #-}

data CopyObject = CopyObject
    { _corACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    , _corBucket :: BucketName
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
    , _corCopySource :: Text
      -- ^ The name of the source bucket and key name of the source object,
      -- separated by a slash (/). Must be URL-encoded.
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
    , _corKey :: ObjectKey
    , _corMetadata :: Map Text Text
      -- ^ A map of metadata to store with the object in S3.
    , _corMetadataDirective :: Maybe MetadataDirective
      -- ^ Specifies whether the metadata is copied from the source object
      -- or replaced with metadata provided in the request.
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
    } deriving (Show, Generic)

-- | The canned ACL to apply to the object.
corACL :: Lens' CopyObject (Maybe ObjectCannedACL)
corACL = lens _corACL (\s a -> s { _corACL = a })
{-# INLINE corACL #-}

corBucket :: Lens' CopyObject (BucketName)
corBucket = lens _corBucket (\s a -> s { _corBucket = a })
{-# INLINE corBucket #-}

-- | Specifies caching behavior along the request/reply chain.
corCacheControl :: Lens' CopyObject (Maybe Text)
corCacheControl = lens _corCacheControl (\s a -> s { _corCacheControl = a })
{-# INLINE corCacheControl #-}

-- | Specifies presentational information for the object.
corContentDisposition :: Lens' CopyObject (Maybe Text)
corContentDisposition = lens _corContentDisposition (\s a -> s { _corContentDisposition = a })
{-# INLINE corContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
corContentEncoding :: Lens' CopyObject (Maybe Text)
corContentEncoding = lens _corContentEncoding (\s a -> s { _corContentEncoding = a })
{-# INLINE corContentEncoding #-}

-- | The language the content is in.
corContentLanguage :: Lens' CopyObject (Maybe Text)
corContentLanguage = lens _corContentLanguage (\s a -> s { _corContentLanguage = a })
{-# INLINE corContentLanguage #-}

-- | A standard MIME type describing the format of the object data.
corContentType :: Lens' CopyObject (Maybe Text)
corContentType = lens _corContentType (\s a -> s { _corContentType = a })
{-# INLINE corContentType #-}

-- | The name of the source bucket and key name of the source object, separated
-- by a slash (/). Must be URL-encoded.
corCopySource :: Lens' CopyObject (Text)
corCopySource = lens _corCopySource (\s a -> s { _corCopySource = a })
{-# INLINE corCopySource #-}

-- | Copies the object if its entity tag (ETag) matches the specified tag.
corCopySourceIfMatch :: Lens' CopyObject (Maybe Text)
corCopySourceIfMatch = lens _corCopySourceIfMatch (\s a -> s { _corCopySourceIfMatch = a })
{-# INLINE corCopySourceIfMatch #-}

-- | Copies the object if it has been modified since the specified time.
corCopySourceIfModifiedSince :: Lens' CopyObject (Maybe RFC822)
corCopySourceIfModifiedSince = lens _corCopySourceIfModifiedSince (\s a -> s { _corCopySourceIfModifiedSince = a })
{-# INLINE corCopySourceIfModifiedSince #-}

-- | Copies the object if its entity tag (ETag) is different than the specified
-- ETag.
corCopySourceIfNoneMatch :: Lens' CopyObject (Maybe Text)
corCopySourceIfNoneMatch = lens _corCopySourceIfNoneMatch (\s a -> s { _corCopySourceIfNoneMatch = a })
{-# INLINE corCopySourceIfNoneMatch #-}

-- | Copies the object if it hasn't been modified since the specified time.
corCopySourceIfUnmodifiedSince :: Lens' CopyObject (Maybe RFC822)
corCopySourceIfUnmodifiedSince = lens _corCopySourceIfUnmodifiedSince (\s a -> s { _corCopySourceIfUnmodifiedSince = a })
{-# INLINE corCopySourceIfUnmodifiedSince #-}

-- | The date and time at which the object is no longer cacheable.
corExpires :: Lens' CopyObject (Maybe RFC822)
corExpires = lens _corExpires (\s a -> s { _corExpires = a })
{-# INLINE corExpires #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
corGrantFullControl :: Lens' CopyObject (Maybe Text)
corGrantFullControl = lens _corGrantFullControl (\s a -> s { _corGrantFullControl = a })
{-# INLINE corGrantFullControl #-}

-- | Allows grantee to read the object data and its metadata.
corGrantRead :: Lens' CopyObject (Maybe Text)
corGrantRead = lens _corGrantRead (\s a -> s { _corGrantRead = a })
{-# INLINE corGrantRead #-}

-- | Allows grantee to read the object ACL.
corGrantReadACP :: Lens' CopyObject (Maybe Text)
corGrantReadACP = lens _corGrantReadACP (\s a -> s { _corGrantReadACP = a })
{-# INLINE corGrantReadACP #-}

-- | Allows grantee to write the ACL for the applicable object.
corGrantWriteACP :: Lens' CopyObject (Maybe Text)
corGrantWriteACP = lens _corGrantWriteACP (\s a -> s { _corGrantWriteACP = a })
{-# INLINE corGrantWriteACP #-}

corKey :: Lens' CopyObject (ObjectKey)
corKey = lens _corKey (\s a -> s { _corKey = a })
{-# INLINE corKey #-}

-- | A map of metadata to store with the object in S3.
corMetadata :: Lens' CopyObject (Map Text Text)
corMetadata = lens _corMetadata (\s a -> s { _corMetadata = a })
{-# INLINE corMetadata #-}

-- | Specifies whether the metadata is copied from the source object or replaced
-- with metadata provided in the request.
corMetadataDirective :: Lens' CopyObject (Maybe MetadataDirective)
corMetadataDirective = lens _corMetadataDirective (\s a -> s { _corMetadataDirective = a })
{-# INLINE corMetadataDirective #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
corServerSideEncryption :: Lens' CopyObject (Maybe ServerSideEncryption)
corServerSideEncryption = lens _corServerSideEncryption (\s a -> s { _corServerSideEncryption = a })
{-# INLINE corServerSideEncryption #-}

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
corStorageClass :: Lens' CopyObject (Maybe StorageClass)
corStorageClass = lens _corStorageClass (\s a -> s { _corStorageClass = a })
{-# INLINE corStorageClass #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
corWebsiteRedirectLocation :: Lens' CopyObject (Maybe Text)
corWebsiteRedirectLocation = lens _corWebsiteRedirectLocation (\s a -> s { _corWebsiteRedirectLocation = a })
{-# INLINE corWebsiteRedirectLocation #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
corSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
corSSECustomerAlgorithm = lens _corSSECustomerAlgorithm (\s a -> s { _corSSECustomerAlgorithm = a })
{-# INLINE corSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
corSSECustomerKey :: Lens' CopyObject (Maybe Text)
corSSECustomerKey = lens _corSSECustomerKey (\s a -> s { _corSSECustomerKey = a })
{-# INLINE corSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
corSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
corSSECustomerKeyMD5 = lens _corSSECustomerKeyMD5 (\s a -> s { _corSSECustomerKeyMD5 = a })
{-# INLINE corSSECustomerKeyMD5 #-}

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
corCopySourceSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
corCopySourceSSECustomerAlgorithm = lens _corCopySourceSSECustomerAlgorithm (\s a -> s { _corCopySourceSSECustomerAlgorithm = a })
{-# INLINE corCopySourceSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header must
-- be one that was used when the source object was created.
corCopySourceSSECustomerKey :: Lens' CopyObject (Maybe Text)
corCopySourceSSECustomerKey = lens _corCopySourceSSECustomerKey (\s a -> s { _corCopySourceSSECustomerKey = a })
{-# INLINE corCopySourceSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
corCopySourceSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
corCopySourceSSECustomerKeyMD5 = lens _corCopySourceSSECustomerKeyMD5 (\s a -> s { _corCopySourceSSECustomerKeyMD5 = a })
{-# INLINE corCopySourceSSECustomerKeyMD5 #-}

instance ToPath CopyObject where
    toPath CopyObject{..} = mconcat
        [ "/"
        , toBS _corBucket
        , "/"
        , toBS _corKey
        ]

instance ToQuery CopyObject

instance ToHeaders CopyObject

instance ToBody CopyObject

data CopyObjectResponse = CopyObjectResponse
    { _cooCopyObjectResult :: Maybe CopyObjectResult
    , _cooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, the response includes
      -- this header.
    , _cooCopySourceVersionId :: Maybe Text
    , _cooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _cooSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _cooSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    } deriving (Show, Generic)

cooCopyObjectResult :: Lens' CopyObjectResponse (Maybe CopyObjectResult)
cooCopyObjectResult = lens _cooCopyObjectResult (\s a -> s { _cooCopyObjectResult = a })
{-# INLINE cooCopyObjectResult #-}

-- | If the object expiration is configured, the response includes this header.
cooExpiration :: Lens' CopyObjectResponse (Maybe RFC822)
cooExpiration = lens _cooExpiration (\s a -> s { _cooExpiration = a })
{-# INLINE cooExpiration #-}

cooCopySourceVersionId :: Lens' CopyObjectResponse (Maybe Text)
cooCopySourceVersionId = lens _cooCopySourceVersionId (\s a -> s { _cooCopySourceVersionId = a })
{-# INLINE cooCopySourceVersionId #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cooServerSideEncryption :: Lens' CopyObjectResponse (Maybe ServerSideEncryption)
cooServerSideEncryption = lens _cooServerSideEncryption (\s a -> s { _cooServerSideEncryption = a })
{-# INLINE cooServerSideEncryption #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
cooSSECustomerAlgorithm :: Lens' CopyObjectResponse (Maybe Text)
cooSSECustomerAlgorithm = lens _cooSSECustomerAlgorithm (\s a -> s { _cooSSECustomerAlgorithm = a })
{-# INLINE cooSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cooSSECustomerKeyMD5 :: Lens' CopyObjectResponse (Maybe Text)
cooSSECustomerKeyMD5 = lens _cooSSECustomerKeyMD5 (\s a -> s { _cooSSECustomerKeyMD5 = a })
{-# INLINE cooSSECustomerKeyMD5 #-}

instance AWSRequest CopyObject where
    type Sv CopyObject = S3
    type Rs CopyObject = CopyObjectResponse

    request = put
    response _ = cursorResponse $ \hs xml ->
        pure CopyObjectResponse
            <*> xml %|? "CopyObjectResult"
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "x-amz-copy-source-version-id"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
