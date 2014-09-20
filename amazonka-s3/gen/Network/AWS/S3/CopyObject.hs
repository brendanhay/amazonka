{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.CopyObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a copy of an object that is already stored in Amazon S3.
module Network.AWS.S3.CopyObject
    (
    -- * Request
      CopyObject
    -- ** Request alias
    , PutObjectCopy
    -- ** Request constructor
    , copyObject
    -- ** Request lenses
    , coACL
    , coBucket
    , coCacheControl
    , coContentDisposition
    , coContentEncoding
    , coContentLanguage
    , coContentType
    , coCopySource
    , coCopySourceIfMatch
    , coCopySourceIfModifiedSince
    , coCopySourceIfNoneMatch
    , coCopySourceIfUnmodifiedSince
    , coExpires
    , coGrantFullControl
    , coGrantRead
    , coGrantReadACP
    , coGrantWriteACP
    , coKey
    , coMetadata
    , coMetadataDirective
    , coServerSideEncryption
    , coStorageClass
    , coWebsiteRedirectLocation
    , coSSECustomerAlgorithm
    , coSSECustomerKey
    , coSSECustomerKeyMD5
    , coCopySourceSSECustomerAlgorithm
    , coCopySourceSSECustomerKey
    , coCopySourceSSECustomerKeyMD5

    -- * Response
    , CopyObjectResponse
    -- ** Response constructor
    , copyObjectResponse
    -- ** Response lenses
    , corCopyObjectResult
    , corExpiration
    , corCopySourceVersionId
    , corServerSideEncryption
    , corSSECustomerAlgorithm
    , corSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

type PutObjectCopy = CopyObject

data CopyObject = CopyObject
    { _coACL :: Maybe ObjectCannedACL
    , _coBucket :: BucketName
    , _coCacheControl :: Maybe Text
    , _coContentDisposition :: Maybe Text
    , _coContentEncoding :: Maybe Text
    , _coContentLanguage :: Maybe Text
    , _coContentType :: Maybe Text
    , _coCopySource :: Text
    , _coCopySourceIfMatch :: Maybe Text
    , _coCopySourceIfModifiedSince :: Maybe RFC822
    , _coCopySourceIfNoneMatch :: Maybe Text
    , _coCopySourceIfUnmodifiedSince :: Maybe RFC822
    , _coExpires :: Maybe RFC822
    , _coGrantFullControl :: Maybe Text
    , _coGrantRead :: Maybe Text
    , _coGrantReadACP :: Maybe Text
    , _coGrantWriteACP :: Maybe Text
    , _coKey :: ObjectKey
    , _coMetadata :: Map Text Text
    , _coMetadataDirective :: Maybe MetadataDirective
    , _coServerSideEncryption :: Maybe ServerSideEncryption
    , _coStorageClass :: Maybe StorageClass
    , _coWebsiteRedirectLocation :: Maybe Text
    , _coSSECustomerAlgorithm :: Maybe Text
    , _coSSECustomerKey :: Maybe Text
    , _coSSECustomerKeyMD5 :: Maybe Text
    , _coCopySourceSSECustomerAlgorithm :: Maybe Text
    , _coCopySourceSSECustomerKey :: Maybe Text
    , _coCopySourceSSECustomerKeyMD5 :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CopyObject' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ACL ::@ @Maybe ObjectCannedACL@
--
-- * @Bucket ::@ @BucketName@
--
-- * @CacheControl ::@ @Maybe Text@
--
-- * @ContentDisposition ::@ @Maybe Text@
--
-- * @ContentEncoding ::@ @Maybe Text@
--
-- * @ContentLanguage ::@ @Maybe Text@
--
-- * @ContentType ::@ @Maybe Text@
--
-- * @CopySource ::@ @Text@
--
-- * @CopySourceIfMatch ::@ @Maybe Text@
--
-- * @CopySourceIfModifiedSince ::@ @Maybe RFC822@
--
-- * @CopySourceIfNoneMatch ::@ @Maybe Text@
--
-- * @CopySourceIfUnmodifiedSince ::@ @Maybe RFC822@
--
-- * @Expires ::@ @Maybe RFC822@
--
-- * @GrantFullControl ::@ @Maybe Text@
--
-- * @GrantRead ::@ @Maybe Text@
--
-- * @GrantReadACP ::@ @Maybe Text@
--
-- * @GrantWriteACP ::@ @Maybe Text@
--
-- * @Key ::@ @ObjectKey@
--
-- * @Metadata ::@ @Map Text Text@
--
-- * @MetadataDirective ::@ @Maybe MetadataDirective@
--
-- * @ServerSideEncryption ::@ @Maybe ServerSideEncryption@
--
-- * @StorageClass ::@ @Maybe StorageClass@
--
-- * @WebsiteRedirectLocation ::@ @Maybe Text@
--
-- * @SSECustomerAlgorithm ::@ @Maybe Text@
--
-- * @SSECustomerKey ::@ @Maybe Text@
--
-- * @SSECustomerKeyMD5 ::@ @Maybe Text@
--
-- * @CopySourceSSECustomerAlgorithm ::@ @Maybe Text@
--
-- * @CopySourceSSECustomerKey ::@ @Maybe Text@
--
-- * @CopySourceSSECustomerKeyMD5 ::@ @Maybe Text@
--
copyObject :: ObjectKey -- ^ 'coKey'
           -> BucketName -- ^ 'coBucket'
           -> Text -- ^ 'coCopySource'
           -> CopyObject
copyObject p18 p2 p8 = CopyObject
    { _coACL = Nothing
    , _coBucket = p2
    , _coCacheControl = Nothing
    , _coContentDisposition = Nothing
    , _coContentEncoding = Nothing
    , _coContentLanguage = Nothing
    , _coContentType = Nothing
    , _coCopySource = p8
    , _coCopySourceIfMatch = Nothing
    , _coCopySourceIfModifiedSince = Nothing
    , _coCopySourceIfNoneMatch = Nothing
    , _coCopySourceIfUnmodifiedSince = Nothing
    , _coExpires = Nothing
    , _coGrantFullControl = Nothing
    , _coGrantRead = Nothing
    , _coGrantReadACP = Nothing
    , _coGrantWriteACP = Nothing
    , _coKey = p18
    , _coMetadata = mempty
    , _coMetadataDirective = Nothing
    , _coServerSideEncryption = Nothing
    , _coStorageClass = Nothing
    , _coWebsiteRedirectLocation = Nothing
    , _coSSECustomerAlgorithm = Nothing
    , _coSSECustomerKey = Nothing
    , _coSSECustomerKeyMD5 = Nothing
    , _coCopySourceSSECustomerAlgorithm = Nothing
    , _coCopySourceSSECustomerKey = Nothing
    , _coCopySourceSSECustomerKeyMD5 = Nothing
    }

-- | The canned ACL to apply to the object.
coACL :: Lens' CopyObject (Maybe ObjectCannedACL)
coACL = lens _coACL (\s a -> s { _coACL = a })

coBucket :: Lens' CopyObject BucketName
coBucket = lens _coBucket (\s a -> s { _coBucket = a })

-- | Specifies caching behavior along the request/reply chain.
coCacheControl :: Lens' CopyObject (Maybe Text)
coCacheControl = lens _coCacheControl (\s a -> s { _coCacheControl = a })

-- | Specifies presentational information for the object.
coContentDisposition :: Lens' CopyObject (Maybe Text)
coContentDisposition =
    lens _coContentDisposition (\s a -> s { _coContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
coContentEncoding :: Lens' CopyObject (Maybe Text)
coContentEncoding =
    lens _coContentEncoding (\s a -> s { _coContentEncoding = a })

-- | The language the content is in.
coContentLanguage :: Lens' CopyObject (Maybe Text)
coContentLanguage =
    lens _coContentLanguage (\s a -> s { _coContentLanguage = a })

-- | A standard MIME type describing the format of the object data.
coContentType :: Lens' CopyObject (Maybe Text)
coContentType = lens _coContentType (\s a -> s { _coContentType = a })

-- | The name of the source bucket and key name of the source object, separated
-- by a slash (/). Must be URL-encoded.
coCopySource :: Lens' CopyObject Text
coCopySource = lens _coCopySource (\s a -> s { _coCopySource = a })

-- | Copies the object if its entity tag (ETag) matches the specified tag.
coCopySourceIfMatch :: Lens' CopyObject (Maybe Text)
coCopySourceIfMatch =
    lens _coCopySourceIfMatch (\s a -> s { _coCopySourceIfMatch = a })

-- | Copies the object if it has been modified since the specified time.
coCopySourceIfModifiedSince :: Lens' CopyObject (Maybe RFC822)
coCopySourceIfModifiedSince =
    lens _coCopySourceIfModifiedSince
         (\s a -> s { _coCopySourceIfModifiedSince = a })

-- | Copies the object if its entity tag (ETag) is different than the specified
-- ETag.
coCopySourceIfNoneMatch :: Lens' CopyObject (Maybe Text)
coCopySourceIfNoneMatch =
    lens _coCopySourceIfNoneMatch
         (\s a -> s { _coCopySourceIfNoneMatch = a })

-- | Copies the object if it hasn't been modified since the specified time.
coCopySourceIfUnmodifiedSince :: Lens' CopyObject (Maybe RFC822)
coCopySourceIfUnmodifiedSince =
    lens _coCopySourceIfUnmodifiedSince
         (\s a -> s { _coCopySourceIfUnmodifiedSince = a })

-- | The date and time at which the object is no longer cacheable.
coExpires :: Lens' CopyObject (Maybe RFC822)
coExpires = lens _coExpires (\s a -> s { _coExpires = a })

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
coGrantFullControl :: Lens' CopyObject (Maybe Text)
coGrantFullControl =
    lens _coGrantFullControl (\s a -> s { _coGrantFullControl = a })

-- | Allows grantee to read the object data and its metadata.
coGrantRead :: Lens' CopyObject (Maybe Text)
coGrantRead = lens _coGrantRead (\s a -> s { _coGrantRead = a })

-- | Allows grantee to read the object ACL.
coGrantReadACP :: Lens' CopyObject (Maybe Text)
coGrantReadACP = lens _coGrantReadACP (\s a -> s { _coGrantReadACP = a })

-- | Allows grantee to write the ACL for the applicable object.
coGrantWriteACP :: Lens' CopyObject (Maybe Text)
coGrantWriteACP = lens _coGrantWriteACP (\s a -> s { _coGrantWriteACP = a })

coKey :: Lens' CopyObject ObjectKey
coKey = lens _coKey (\s a -> s { _coKey = a })

-- | A map of metadata to store with the object in S3.
coMetadata :: Lens' CopyObject (Map Text Text)
coMetadata = lens _coMetadata (\s a -> s { _coMetadata = a })

-- | Specifies whether the metadata is copied from the source object or replaced
-- with metadata provided in the request.
coMetadataDirective :: Lens' CopyObject (Maybe MetadataDirective)
coMetadataDirective =
    lens _coMetadataDirective (\s a -> s { _coMetadataDirective = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
coServerSideEncryption :: Lens' CopyObject (Maybe ServerSideEncryption)
coServerSideEncryption =
    lens _coServerSideEncryption (\s a -> s { _coServerSideEncryption = a })

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
coStorageClass :: Lens' CopyObject (Maybe StorageClass)
coStorageClass = lens _coStorageClass (\s a -> s { _coStorageClass = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
coWebsiteRedirectLocation :: Lens' CopyObject (Maybe Text)
coWebsiteRedirectLocation =
    lens _coWebsiteRedirectLocation
         (\s a -> s { _coWebsiteRedirectLocation = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
coSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
coSSECustomerAlgorithm =
    lens _coSSECustomerAlgorithm (\s a -> s { _coSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
coSSECustomerKey :: Lens' CopyObject (Maybe Text)
coSSECustomerKey =
    lens _coSSECustomerKey (\s a -> s { _coSSECustomerKey = a })

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
coSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
coSSECustomerKeyMD5 =
    lens _coSSECustomerKeyMD5 (\s a -> s { _coSSECustomerKeyMD5 = a })

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
coCopySourceSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerAlgorithm =
    lens _coCopySourceSSECustomerAlgorithm
         (\s a -> s { _coCopySourceSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header must
-- be one that was used when the source object was created.
coCopySourceSSECustomerKey :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerKey =
    lens _coCopySourceSSECustomerKey
         (\s a -> s { _coCopySourceSSECustomerKey = a })

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
coCopySourceSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerKeyMD5 =
    lens _coCopySourceSSECustomerKeyMD5
         (\s a -> s { _coCopySourceSSECustomerKeyMD5 = a })

instance ToPath CopyObject

instance ToQuery CopyObject

instance ToHeaders CopyObject where
    toHeaders CopyObject{..} = concat
        [ "x-amz-acl" =: _coACL
        , "Cache-Control" =: _coCacheControl
        , "Content-Disposition" =: _coContentDisposition
        , "Content-Encoding" =: _coContentEncoding
        , "Content-Language" =: _coContentLanguage
        , "Content-Type" =: _coContentType
        , "x-amz-copy-source" =: _coCopySource
        , "x-amz-copy-source-if-match" =: _coCopySourceIfMatch
        , "x-amz-copy-source-if-modified-since" =: _coCopySourceIfModifiedSince
        , "x-amz-copy-source-if-none-match" =: _coCopySourceIfNoneMatch
        , "x-amz-copy-source-if-unmodified-since" =: _coCopySourceIfUnmodifiedSince
        , "Expires" =: _coExpires
        , "x-amz-grant-full-control" =: _coGrantFullControl
        , "x-amz-grant-read" =: _coGrantRead
        , "x-amz-grant-read-acp" =: _coGrantReadACP
        , "x-amz-grant-write-acp" =: _coGrantWriteACP
        , "x-amz-meta-" =: _coMetadata
        , "x-amz-metadata-directive" =: _coMetadataDirective
        , "x-amz-server-side-encryption" =: _coServerSideEncryption
        , "x-amz-storage-class" =: _coStorageClass
        , "x-amz-website-redirect-location" =: _coWebsiteRedirectLocation
        , "x-amz-server-side-encryption-customer-algorithm" =: _coSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _coSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _coSSECustomerKeyMD5
        , "x-amz-copy-source-server-side-encryption-customer-algorithm" =: _coCopySourceSSECustomerAlgorithm
        , "x-amz-copy-source-server-side-encryption-customer-key" =: _coCopySourceSSECustomerKey
        , "x-amz-copy-source-server-side-encryption-customer-key-MD5" =: _coCopySourceSSECustomerKeyMD5
        ]

instance ToBody CopyObject

data CopyObjectResponse = CopyObjectResponse
    { _corCopyObjectResult :: Maybe CopyObjectResult
    , _corExpiration :: Maybe RFC822
    , _corCopySourceVersionId :: Maybe Text
    , _corServerSideEncryption :: Maybe ServerSideEncryption
    , _corSSECustomerAlgorithm :: Maybe Text
    , _corSSECustomerKeyMD5 :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CopyObjectResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CopyObjectResult ::@ @Maybe CopyObjectResult@
--
-- * @Expiration ::@ @Maybe RFC822@
--
-- * @CopySourceVersionId ::@ @Maybe Text@
--
-- * @ServerSideEncryption ::@ @Maybe ServerSideEncryption@
--
-- * @SSECustomerAlgorithm ::@ @Maybe Text@
--
-- * @SSECustomerKeyMD5 ::@ @Maybe Text@
--
copyObjectResponse :: CopyObjectResponse
copyObjectResponse = CopyObjectResponse
    { _corCopyObjectResult = Nothing
    , _corExpiration = Nothing
    , _corCopySourceVersionId = Nothing
    , _corServerSideEncryption = Nothing
    , _corSSECustomerAlgorithm = Nothing
    , _corSSECustomerKeyMD5 = Nothing
    }

corCopyObjectResult :: Lens' CopyObjectResponse (Maybe CopyObjectResult)
corCopyObjectResult =
    lens _corCopyObjectResult (\s a -> s { _corCopyObjectResult = a })

-- | If the object expiration is configured, the response includes this header.
corExpiration :: Lens' CopyObjectResponse (Maybe RFC822)
corExpiration = lens _corExpiration (\s a -> s { _corExpiration = a })

corCopySourceVersionId :: Lens' CopyObjectResponse (Maybe Text)
corCopySourceVersionId =
    lens _corCopySourceVersionId (\s a -> s { _corCopySourceVersionId = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
corServerSideEncryption :: Lens' CopyObjectResponse (Maybe ServerSideEncryption)
corServerSideEncryption =
    lens _corServerSideEncryption
         (\s a -> s { _corServerSideEncryption = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
corSSECustomerAlgorithm :: Lens' CopyObjectResponse (Maybe Text)
corSSECustomerAlgorithm =
    lens _corSSECustomerAlgorithm
         (\s a -> s { _corSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
corSSECustomerKeyMD5 :: Lens' CopyObjectResponse (Maybe Text)
corSSECustomerKeyMD5 =
    lens _corSSECustomerKeyMD5 (\s a -> s { _corSSECustomerKeyMD5 = a })

instance AWSRequest CopyObject where
    type Sv CopyObject = S3
    type Rs CopyObject = CopyObjectResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure CopyObjectResponse
            <*> xml %|? "CopyObjectResult"
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "x-amz-copy-source-version-id"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
