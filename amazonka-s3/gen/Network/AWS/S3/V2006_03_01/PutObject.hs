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
    , mkPutObjectRequest
    -- ** Request lenses
    , porACL
    , porBody
    , porBucket
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
    , porKey
    , porMetadata
    , porServerSideEncryption
    , porStorageClass
    , porWebsiteRedirectLocation
    , porSSECustomerAlgorithm
    , porSSECustomerKey
    , porSSECustomerKeyMD5

    -- * Response
    , PutObjectResponse
    -- ** Response lenses
    , pooExpiration
    , pooETag
    , pooServerSideEncryption
    , pooVersionId
    , pooSSECustomerAlgorithm
    , pooSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutObject' request.
mkPutObjectRequest :: RqBody -- ^ 'porBody'
                   -> BucketName -- ^ 'porBucket'
                   -> ObjectKey -- ^ 'porKey'
                   -> PutObject
mkPutObjectRequest p1 p2 p3 = PutObject
    { _porACL = Nothing
    , _porBody = p2
    , _porBucket = p3
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
    , _porKey = p16
    , _porMetadata = mempty
    , _porServerSideEncryption = Nothing
    , _porStorageClass = Nothing
    , _porWebsiteRedirectLocation = Nothing
    , _porSSECustomerAlgorithm = Nothing
    , _porSSECustomerKey = Nothing
    , _porSSECustomerKeyMD5 = Nothing
    }
{-# INLINE mkPutObjectRequest #-}

data PutObject = PutObject
    { _porACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    , _porBody :: RqBody
    , _porBucket :: BucketName
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
    , _porKey :: ObjectKey
    , _porMetadata :: Map Text Text
      -- ^ A map of metadata to store with the object in S3.
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
    } deriving (Show, Generic)

-- | The canned ACL to apply to the object.
porACL :: Lens' PutObject (Maybe ObjectCannedACL)
porACL = lens _porACL (\s a -> s { _porACL = a })
{-# INLINE porACL #-}

porBody :: Lens' PutObject (RqBody)
porBody = lens _porBody (\s a -> s { _porBody = a })
{-# INLINE porBody #-}

porBucket :: Lens' PutObject (BucketName)
porBucket = lens _porBucket (\s a -> s { _porBucket = a })
{-# INLINE porBucket #-}

-- | Specifies caching behavior along the request/reply chain.
porCacheControl :: Lens' PutObject (Maybe Text)
porCacheControl = lens _porCacheControl (\s a -> s { _porCacheControl = a })
{-# INLINE porCacheControl #-}

-- | Specifies presentational information for the object.
porContentDisposition :: Lens' PutObject (Maybe Text)
porContentDisposition = lens _porContentDisposition (\s a -> s { _porContentDisposition = a })
{-# INLINE porContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
porContentEncoding :: Lens' PutObject (Maybe Text)
porContentEncoding = lens _porContentEncoding (\s a -> s { _porContentEncoding = a })
{-# INLINE porContentEncoding #-}

-- | The language the content is in.
porContentLanguage :: Lens' PutObject (Maybe Text)
porContentLanguage = lens _porContentLanguage (\s a -> s { _porContentLanguage = a })
{-# INLINE porContentLanguage #-}

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
porContentLength :: Lens' PutObject (Maybe Integer)
porContentLength = lens _porContentLength (\s a -> s { _porContentLength = a })
{-# INLINE porContentLength #-}

porContentMD5 :: Lens' PutObject (Maybe Text)
porContentMD5 = lens _porContentMD5 (\s a -> s { _porContentMD5 = a })
{-# INLINE porContentMD5 #-}

-- | A standard MIME type describing the format of the object data.
porContentType :: Lens' PutObject (Maybe Text)
porContentType = lens _porContentType (\s a -> s { _porContentType = a })
{-# INLINE porContentType #-}

-- | The date and time at which the object is no longer cacheable.
porExpires :: Lens' PutObject (Maybe RFC822)
porExpires = lens _porExpires (\s a -> s { _porExpires = a })
{-# INLINE porExpires #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
porGrantFullControl :: Lens' PutObject (Maybe Text)
porGrantFullControl = lens _porGrantFullControl (\s a -> s { _porGrantFullControl = a })
{-# INLINE porGrantFullControl #-}

-- | Allows grantee to read the object data and its metadata.
porGrantRead :: Lens' PutObject (Maybe Text)
porGrantRead = lens _porGrantRead (\s a -> s { _porGrantRead = a })
{-# INLINE porGrantRead #-}

-- | Allows grantee to read the object ACL.
porGrantReadACP :: Lens' PutObject (Maybe Text)
porGrantReadACP = lens _porGrantReadACP (\s a -> s { _porGrantReadACP = a })
{-# INLINE porGrantReadACP #-}

-- | Allows grantee to write the ACL for the applicable object.
porGrantWriteACP :: Lens' PutObject (Maybe Text)
porGrantWriteACP = lens _porGrantWriteACP (\s a -> s { _porGrantWriteACP = a })
{-# INLINE porGrantWriteACP #-}

porKey :: Lens' PutObject (ObjectKey)
porKey = lens _porKey (\s a -> s { _porKey = a })
{-# INLINE porKey #-}

-- | A map of metadata to store with the object in S3.
porMetadata :: Lens' PutObject (Map Text Text)
porMetadata = lens _porMetadata (\s a -> s { _porMetadata = a })
{-# INLINE porMetadata #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
porServerSideEncryption :: Lens' PutObject (Maybe ServerSideEncryption)
porServerSideEncryption = lens _porServerSideEncryption (\s a -> s { _porServerSideEncryption = a })
{-# INLINE porServerSideEncryption #-}

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
porStorageClass :: Lens' PutObject (Maybe StorageClass)
porStorageClass = lens _porStorageClass (\s a -> s { _porStorageClass = a })
{-# INLINE porStorageClass #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
porWebsiteRedirectLocation :: Lens' PutObject (Maybe Text)
porWebsiteRedirectLocation = lens _porWebsiteRedirectLocation (\s a -> s { _porWebsiteRedirectLocation = a })
{-# INLINE porWebsiteRedirectLocation #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
porSSECustomerAlgorithm :: Lens' PutObject (Maybe Text)
porSSECustomerAlgorithm = lens _porSSECustomerAlgorithm (\s a -> s { _porSSECustomerAlgorithm = a })
{-# INLINE porSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
porSSECustomerKey :: Lens' PutObject (Maybe Text)
porSSECustomerKey = lens _porSSECustomerKey (\s a -> s { _porSSECustomerKey = a })
{-# INLINE porSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
porSSECustomerKeyMD5 :: Lens' PutObject (Maybe Text)
porSSECustomerKeyMD5 = lens _porSSECustomerKeyMD5 (\s a -> s { _porSSECustomerKeyMD5 = a })
{-# INLINE porSSECustomerKeyMD5 #-}

instance ToPath PutObject where
    toPath PutObject{..} = mconcat
        [ "/"
        , toBS _porBucket
        , "/"
        , toBS _porKey
        ]

instance ToQuery PutObject

instance ToHeaders PutObject

instance ToBody PutObject

data PutObjectResponse = PutObjectResponse
    { _pooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, this will contain the
      -- expiration date (expiry-date) and rule ID (rule-id). The value of
      -- rule-id is URL encoded.
    , _pooETag :: Maybe ETag
      -- ^ Entity tag for the uploaded object.
    , _pooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
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
    } deriving (Show, Generic)

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
pooExpiration :: Lens' PutObjectResponse (Maybe RFC822)
pooExpiration = lens _pooExpiration (\s a -> s { _pooExpiration = a })
{-# INLINE pooExpiration #-}

-- | Entity tag for the uploaded object.
pooETag :: Lens' PutObjectResponse (Maybe ETag)
pooETag = lens _pooETag (\s a -> s { _pooETag = a })
{-# INLINE pooETag #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
pooServerSideEncryption :: Lens' PutObjectResponse (Maybe ServerSideEncryption)
pooServerSideEncryption = lens _pooServerSideEncryption (\s a -> s { _pooServerSideEncryption = a })
{-# INLINE pooServerSideEncryption #-}

-- | Version of the object.
pooVersionId :: Lens' PutObjectResponse (Maybe ObjectVersionId)
pooVersionId = lens _pooVersionId (\s a -> s { _pooVersionId = a })
{-# INLINE pooVersionId #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
pooSSECustomerAlgorithm :: Lens' PutObjectResponse (Maybe Text)
pooSSECustomerAlgorithm = lens _pooSSECustomerAlgorithm (\s a -> s { _pooSSECustomerAlgorithm = a })
{-# INLINE pooSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
pooSSECustomerKeyMD5 :: Lens' PutObjectResponse (Maybe Text)
pooSSECustomerKeyMD5 = lens _pooSSECustomerKeyMD5 (\s a -> s { _pooSSECustomerKeyMD5 = a })
{-# INLINE pooSSECustomerKeyMD5 #-}

instance AWSRequest PutObject where
    type Sv PutObject = S3
    type Rs PutObject = PutObjectResponse

    request = put
    response _ = headerResponse $ \hs ->
        pure PutObjectResponse
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "ETag"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "x-amz-version-id"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
