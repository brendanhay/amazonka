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

porBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> PutObject
    -> f PutObject
porBucket f x =
    (\y -> x { _porBucket = y })
       <$> f (_porBucket x)
{-# INLINE porBucket #-}

porKey
    :: Functor f
    => (ObjectKey
    -> f (ObjectKey))
    -> PutObject
    -> f PutObject
porKey f x =
    (\y -> x { _porKey = y })
       <$> f (_porKey x)
{-# INLINE porKey #-}

porBody
    :: Functor f
    => (RqBody
    -> f (RqBody))
    -> PutObject
    -> f PutObject
porBody f x =
    (\y -> x { _porBody = y })
       <$> f (_porBody x)
{-# INLINE porBody #-}

-- | Specifies caching behavior along the request/reply chain.
porCacheControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porCacheControl f x =
    (\y -> x { _porCacheControl = y })
       <$> f (_porCacheControl x)
{-# INLINE porCacheControl #-}

-- | Specifies presentational information for the object.
porContentDisposition
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porContentDisposition f x =
    (\y -> x { _porContentDisposition = y })
       <$> f (_porContentDisposition x)
{-# INLINE porContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
porContentEncoding
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porContentEncoding f x =
    (\y -> x { _porContentEncoding = y })
       <$> f (_porContentEncoding x)
{-# INLINE porContentEncoding #-}

-- | The language the content is in.
porContentLanguage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porContentLanguage f x =
    (\y -> x { _porContentLanguage = y })
       <$> f (_porContentLanguage x)
{-# INLINE porContentLanguage #-}

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
porContentLength
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PutObject
    -> f PutObject
porContentLength f x =
    (\y -> x { _porContentLength = y })
       <$> f (_porContentLength x)
{-# INLINE porContentLength #-}

porContentMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porContentMD5 f x =
    (\y -> x { _porContentMD5 = y })
       <$> f (_porContentMD5 x)
{-# INLINE porContentMD5 #-}

-- | A standard MIME type describing the format of the object data.
porContentType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porContentType f x =
    (\y -> x { _porContentType = y })
       <$> f (_porContentType x)
{-# INLINE porContentType #-}

-- | The date and time at which the object is no longer cacheable.
porExpires
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> PutObject
    -> f PutObject
porExpires f x =
    (\y -> x { _porExpires = y })
       <$> f (_porExpires x)
{-# INLINE porExpires #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
porGrantFullControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porGrantFullControl f x =
    (\y -> x { _porGrantFullControl = y })
       <$> f (_porGrantFullControl x)
{-# INLINE porGrantFullControl #-}

-- | Allows grantee to read the object data and its metadata.
porGrantRead
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porGrantRead f x =
    (\y -> x { _porGrantRead = y })
       <$> f (_porGrantRead x)
{-# INLINE porGrantRead #-}

-- | Allows grantee to read the object ACL.
porGrantReadACP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porGrantReadACP f x =
    (\y -> x { _porGrantReadACP = y })
       <$> f (_porGrantReadACP x)
{-# INLINE porGrantReadACP #-}

-- | Allows grantee to write the ACL for the applicable object.
porGrantWriteACP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porGrantWriteACP f x =
    (\y -> x { _porGrantWriteACP = y })
       <$> f (_porGrantWriteACP x)
{-# INLINE porGrantWriteACP #-}

-- | A map of metadata to store with the object in S3.
porMetadata
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> PutObject
    -> f PutObject
porMetadata f x =
    (\y -> x { _porMetadata = y })
       <$> f (_porMetadata x)
{-# INLINE porMetadata #-}

-- | The canned ACL to apply to the object.
porACL
    :: Functor f
    => (Maybe ObjectCannedACL
    -> f (Maybe ObjectCannedACL))
    -> PutObject
    -> f PutObject
porACL f x =
    (\y -> x { _porACL = y })
       <$> f (_porACL x)
{-# INLINE porACL #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
porSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porSSECustomerAlgorithm f x =
    (\y -> x { _porSSECustomerAlgorithm = y })
       <$> f (_porSSECustomerAlgorithm x)
{-# INLINE porSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
porSSECustomerKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porSSECustomerKey f x =
    (\y -> x { _porSSECustomerKey = y })
       <$> f (_porSSECustomerKey x)
{-# INLINE porSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
porSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porSSECustomerKeyMD5 f x =
    (\y -> x { _porSSECustomerKeyMD5 = y })
       <$> f (_porSSECustomerKeyMD5 x)
{-# INLINE porSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
porServerSideEncryption
    :: Functor f
    => (Maybe ServerSideEncryption
    -> f (Maybe ServerSideEncryption))
    -> PutObject
    -> f PutObject
porServerSideEncryption f x =
    (\y -> x { _porServerSideEncryption = y })
       <$> f (_porServerSideEncryption x)
{-# INLINE porServerSideEncryption #-}

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
porStorageClass
    :: Functor f
    => (Maybe StorageClass
    -> f (Maybe StorageClass))
    -> PutObject
    -> f PutObject
porStorageClass f x =
    (\y -> x { _porStorageClass = y })
       <$> f (_porStorageClass x)
{-# INLINE porStorageClass #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
porWebsiteRedirectLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObject
    -> f PutObject
porWebsiteRedirectLocation f x =
    (\y -> x { _porWebsiteRedirectLocation = y })
       <$> f (_porWebsiteRedirectLocation x)
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
pooETag
    :: Functor f
    => (Maybe ETag
    -> f (Maybe ETag))
    -> PutObjectResponse
    -> f PutObjectResponse
pooETag f x =
    (\y -> x { _pooETag = y })
       <$> f (_pooETag x)
{-# INLINE pooETag #-}

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
pooExpiration
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> PutObjectResponse
    -> f PutObjectResponse
pooExpiration f x =
    (\y -> x { _pooExpiration = y })
       <$> f (_pooExpiration x)
{-# INLINE pooExpiration #-}

-- | Version of the object.
pooVersionId
    :: Functor f
    => (Maybe ObjectVersionId
    -> f (Maybe ObjectVersionId))
    -> PutObjectResponse
    -> f PutObjectResponse
pooVersionId f x =
    (\y -> x { _pooVersionId = y })
       <$> f (_pooVersionId x)
{-# INLINE pooVersionId #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
pooSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObjectResponse
    -> f PutObjectResponse
pooSSECustomerAlgorithm f x =
    (\y -> x { _pooSSECustomerAlgorithm = y })
       <$> f (_pooSSECustomerAlgorithm x)
{-# INLINE pooSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
pooSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutObjectResponse
    -> f PutObjectResponse
pooSSECustomerKeyMD5 f x =
    (\y -> x { _pooSSECustomerKeyMD5 = y })
       <$> f (_pooSSECustomerKeyMD5 x)
{-# INLINE pooSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
pooServerSideEncryption
    :: Functor f
    => (Maybe ServerSideEncryption
    -> f (Maybe ServerSideEncryption))
    -> PutObjectResponse
    -> f PutObjectResponse
pooServerSideEncryption f x =
    (\y -> x { _pooServerSideEncryption = y })
       <$> f (_pooServerSideEncryption x)
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
