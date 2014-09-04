{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds an object to a bucket.
module Network.AWS.S3.V2006_03_01.PutObject
    (
    -- * Request
      PutObject
    -- ** Request constructor
    , putObject
    -- ** Request lenses
    , porBucket
    , porKey
    , porBody
    , porCacheControl
    , porContentDisposition
    , porContentEncoding
    , porContentLanguage
    , porContentLength
    , porContentMD5
    , porContentType
    , porExpires
    , porGrantFullControl
    , porGrantRead
    , porGrantReadACP
    , porGrantWriteACP
    , porMetadata
    , porACL
    , porSSECustomerAlgorithm
    , porSSECustomerKey
    , porSSECustomerKeyMD5
    , porServerSideEncryption
    , porStorageClass
    , porWebsiteRedirectLocation

    -- * Response
    , PutObjectResponse
    -- ** Response lenses
    , pooETag
    , pooExpiration
    , pooVersionId
    , pooSSECustomerAlgorithm
    , pooSSECustomerKeyMD5
    , pooServerSideEncryption
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutObject' request.
putObject :: BucketName -- ^ 'porBucket'
          -> ObjectKey -- ^ 'porKey'
          -> RqBody -- ^ 'porBody'
          -> PutObject
putObject p1 p2 p3 = PutObject
    { _porBucket = p1
    , _porKey = p2
    , _porBody = p3
    , _porCacheControl = Nothing
    , _porContentDisposition = Nothing
    , _porContentEncoding = Nothing
    , _porContentLanguage = Nothing
    , _porContentLength = Nothing
    , _porContentMD5 = Nothing
    , _porContentType = Nothing
    , _porExpires = Nothing
    , _porGrantFullControl = Nothing
    , _porGrantRead = Nothing
    , _porGrantReadACP = Nothing
    , _porGrantWriteACP = Nothing
    , _porMetadata = mempty
    , _porACL = Nothing
    , _porSSECustomerAlgorithm = Nothing
    , _porSSECustomerKey = Nothing
    , _porSSECustomerKeyMD5 = Nothing
    , _porServerSideEncryption = Nothing
    , _porStorageClass = Nothing
    , _porWebsiteRedirectLocation = Nothing
    }
{-# INLINE putObject #-}

data PutObject = PutObject
    { _porBucket :: BucketName
    , _porKey :: ObjectKey
    , _porBody :: RqBody
    , _porCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , _porContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , _porContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object
      -- and thus what decoding mechanisms must be applied to obtain the
      -- media-type referenced by the Content-Type header field.
    , _porContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , _porContentLength :: Maybe Integer
      -- ^ Size of the body in bytes. This parameter is useful when the size
      -- of the body cannot be determined automatically.
    , _porContentMD5 :: Maybe Text
    , _porContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , _porExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , _porGrantFullControl :: Maybe Text
      -- ^ Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on
      -- the object.
    , _porGrantRead :: Maybe Text
      -- ^ Allows grantee to read the object data and its metadata.
    , _porGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the object ACL.
    , _porGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable object.
    , _porMetadata :: Map Text Text
      -- ^ A map of metadata to store with the object in S3.
    , _porACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    , _porSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , _porSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header.
    , _porSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , _porServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _porStorageClass :: Maybe StorageClass
      -- ^ The type of storage to use for the object. Defaults to
      -- 'STANDARD'.
    , _porWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    } deriving (Show, Generic)

porBucket :: Lens' PutObject (BucketName)
porBucket f x =
    f (_porBucket x)
        <&> \y -> x { _porBucket = y }
{-# INLINE porBucket #-}

porKey :: Lens' PutObject (ObjectKey)
porKey f x =
    f (_porKey x)
        <&> \y -> x { _porKey = y }
{-# INLINE porKey #-}

porBody :: Lens' PutObject (RqBody)
porBody f x =
    f (_porBody x)
        <&> \y -> x { _porBody = y }
{-# INLINE porBody #-}

-- | Specifies caching behavior along the request/reply chain.
porCacheControl :: Lens' PutObject (Maybe Text)
porCacheControl f x =
    f (_porCacheControl x)
        <&> \y -> x { _porCacheControl = y }
{-# INLINE porCacheControl #-}

-- | Specifies presentational information for the object.
porContentDisposition :: Lens' PutObject (Maybe Text)
porContentDisposition f x =
    f (_porContentDisposition x)
        <&> \y -> x { _porContentDisposition = y }
{-# INLINE porContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
porContentEncoding :: Lens' PutObject (Maybe Text)
porContentEncoding f x =
    f (_porContentEncoding x)
        <&> \y -> x { _porContentEncoding = y }
{-# INLINE porContentEncoding #-}

-- | The language the content is in.
porContentLanguage :: Lens' PutObject (Maybe Text)
porContentLanguage f x =
    f (_porContentLanguage x)
        <&> \y -> x { _porContentLanguage = y }
{-# INLINE porContentLanguage #-}

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
porContentLength :: Lens' PutObject (Maybe Integer)
porContentLength f x =
    f (_porContentLength x)
        <&> \y -> x { _porContentLength = y }
{-# INLINE porContentLength #-}

porContentMD5 :: Lens' PutObject (Maybe Text)
porContentMD5 f x =
    f (_porContentMD5 x)
        <&> \y -> x { _porContentMD5 = y }
{-# INLINE porContentMD5 #-}

-- | A standard MIME type describing the format of the object data.
porContentType :: Lens' PutObject (Maybe Text)
porContentType f x =
    f (_porContentType x)
        <&> \y -> x { _porContentType = y }
{-# INLINE porContentType #-}

-- | The date and time at which the object is no longer cacheable.
porExpires :: Lens' PutObject (Maybe RFC822)
porExpires f x =
    f (_porExpires x)
        <&> \y -> x { _porExpires = y }
{-# INLINE porExpires #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
porGrantFullControl :: Lens' PutObject (Maybe Text)
porGrantFullControl f x =
    f (_porGrantFullControl x)
        <&> \y -> x { _porGrantFullControl = y }
{-# INLINE porGrantFullControl #-}

-- | Allows grantee to read the object data and its metadata.
porGrantRead :: Lens' PutObject (Maybe Text)
porGrantRead f x =
    f (_porGrantRead x)
        <&> \y -> x { _porGrantRead = y }
{-# INLINE porGrantRead #-}

-- | Allows grantee to read the object ACL.
porGrantReadACP :: Lens' PutObject (Maybe Text)
porGrantReadACP f x =
    f (_porGrantReadACP x)
        <&> \y -> x { _porGrantReadACP = y }
{-# INLINE porGrantReadACP #-}

-- | Allows grantee to write the ACL for the applicable object.
porGrantWriteACP :: Lens' PutObject (Maybe Text)
porGrantWriteACP f x =
    f (_porGrantWriteACP x)
        <&> \y -> x { _porGrantWriteACP = y }
{-# INLINE porGrantWriteACP #-}

-- | A map of metadata to store with the object in S3.
porMetadata :: Lens' PutObject (Map Text Text)
porMetadata f x =
    f (_porMetadata x)
        <&> \y -> x { _porMetadata = y }
{-# INLINE porMetadata #-}

-- | The canned ACL to apply to the object.
porACL :: Lens' PutObject (Maybe ObjectCannedACL)
porACL f x =
    f (_porACL x)
        <&> \y -> x { _porACL = y }
{-# INLINE porACL #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
porSSECustomerAlgorithm :: Lens' PutObject (Maybe Text)
porSSECustomerAlgorithm f x =
    f (_porSSECustomerAlgorithm x)
        <&> \y -> x { _porSSECustomerAlgorithm = y }
{-# INLINE porSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
porSSECustomerKey :: Lens' PutObject (Maybe Text)
porSSECustomerKey f x =
    f (_porSSECustomerKey x)
        <&> \y -> x { _porSSECustomerKey = y }
{-# INLINE porSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
porSSECustomerKeyMD5 :: Lens' PutObject (Maybe Text)
porSSECustomerKeyMD5 f x =
    f (_porSSECustomerKeyMD5 x)
        <&> \y -> x { _porSSECustomerKeyMD5 = y }
{-# INLINE porSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
porServerSideEncryption :: Lens' PutObject (Maybe ServerSideEncryption)
porServerSideEncryption f x =
    f (_porServerSideEncryption x)
        <&> \y -> x { _porServerSideEncryption = y }
{-# INLINE porServerSideEncryption #-}

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
porStorageClass :: Lens' PutObject (Maybe StorageClass)
porStorageClass f x =
    f (_porStorageClass x)
        <&> \y -> x { _porStorageClass = y }
{-# INLINE porStorageClass #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
porWebsiteRedirectLocation :: Lens' PutObject (Maybe Text)
porWebsiteRedirectLocation f x =
    f (_porWebsiteRedirectLocation x)
        <&> \y -> x { _porWebsiteRedirectLocation = y }
{-# INLINE porWebsiteRedirectLocation #-}

instance ToPath PutObject where
    toPath PutObject{..} = mconcat
        [ "/"
        , toBS _porBucket
        , "/"
        , toBS _porKey
        ]

instance ToQuery PutObject

instance ToHeaders PutObject where
    toHeaders PutObject{..} = concat
        [ "Cache-Control" =: _porCacheControl
        , "Content-Disposition" =: _porContentDisposition
        , "Content-Encoding" =: _porContentEncoding
        , "Content-Language" =: _porContentLanguage
        , "Content-Length" =: _porContentLength
        , "Content-MD5" =: _porContentMD5
        , "Content-Type" =: _porContentType
        , "Expires" =: _porExpires
        , "x-amz-grant-full-control" =: _porGrantFullControl
        , "x-amz-grant-read" =: _porGrantRead
        , "x-amz-grant-read-acp" =: _porGrantReadACP
        , "x-amz-grant-write-acp" =: _porGrantWriteACP
        , "x-amz-meta-" =: _porMetadata
        , "x-amz-acl" =: _porACL
        , "x-amz-server-side-encryption-customer-algorithm" =: _porSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _porSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _porSSECustomerKeyMD5
        , "x-amz-server-side-encryption" =: _porServerSideEncryption
        , "x-amz-storage-class" =: _porStorageClass
        , "x-amz-website-redirect-location" =: _porWebsiteRedirectLocation
        ]

instance ToBody PutObject where
    toBody = toBody . _porBody

data PutObjectResponse = PutObjectResponse
    { _pooETag :: Maybe ETag
      -- ^ Entity tag for the uploaded object.
    , _pooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, this will contain the
      -- expiration date (expiry-date) and rule ID (rule-id). The value of
      -- rule-id is URL encoded.
    , _pooVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    , _pooSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _pooSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , _pooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Show, Generic)

-- | Entity tag for the uploaded object.
pooETag :: Lens' PutObjectResponse (Maybe ETag)
pooETag f x =
    f (_pooETag x)
        <&> \y -> x { _pooETag = y }
{-# INLINE pooETag #-}

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
pooExpiration :: Lens' PutObjectResponse (Maybe RFC822)
pooExpiration f x =
    f (_pooExpiration x)
        <&> \y -> x { _pooExpiration = y }
{-# INLINE pooExpiration #-}

-- | Version of the object.
pooVersionId :: Lens' PutObjectResponse (Maybe ObjectVersionId)
pooVersionId f x =
    f (_pooVersionId x)
        <&> \y -> x { _pooVersionId = y }
{-# INLINE pooVersionId #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
pooSSECustomerAlgorithm :: Lens' PutObjectResponse (Maybe Text)
pooSSECustomerAlgorithm f x =
    f (_pooSSECustomerAlgorithm x)
        <&> \y -> x { _pooSSECustomerAlgorithm = y }
{-# INLINE pooSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
pooSSECustomerKeyMD5 :: Lens' PutObjectResponse (Maybe Text)
pooSSECustomerKeyMD5 f x =
    f (_pooSSECustomerKeyMD5 x)
        <&> \y -> x { _pooSSECustomerKeyMD5 = y }
{-# INLINE pooSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
pooServerSideEncryption :: Lens' PutObjectResponse (Maybe ServerSideEncryption)
pooServerSideEncryption f x =
    f (_pooServerSideEncryption x)
        <&> \y -> x { _pooServerSideEncryption = y }
{-# INLINE pooServerSideEncryption #-}

instance AWSRequest PutObject where
    type Sv PutObject = S3
    type Rs PutObject = PutObjectResponse

    request = put
    response _ = headerResponse $ \hs ->
        pure PutObjectResponse
            <*> hs ~:? "ETag"
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "x-amz-version-id"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
            <*> hs ~:? "x-amz-server-side-encryption"
