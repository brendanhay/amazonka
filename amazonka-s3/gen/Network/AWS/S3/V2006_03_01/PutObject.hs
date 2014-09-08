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
    , mkPutObject
    -- ** Request lenses
    , poACL
    , poBody
    , poBucket
    , poCacheControl
    , poContentDisposition
    , poContentEncoding
    , poContentLanguage
    , poContentLength
    , poContentMD5
    , poContentType
    , poExpires
    , poGrantFullControl
    , poGrantRead
    , poGrantReadACP
    , poGrantWriteACP
    , poKey
    , poMetadata
    , poServerSideEncryption
    , poStorageClass
    , poWebsiteRedirectLocation
    , poSSECustomerAlgorithm
    , poSSECustomerKey
    , poSSECustomerKeyMD5

    -- * Response
    , PutObjectResponse
    -- ** Response lenses
    , porExpiration
    , porETag
    , porServerSideEncryption
    , porVersionId
    , porSSECustomerAlgorithm
    , porSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data PutObject = PutObject
    { _poACL :: Maybe ObjectCannedACL
    , _poBody :: RqBody
    , _poBucket :: BucketName
    , _poCacheControl :: Maybe Text
    , _poContentDisposition :: Maybe Text
    , _poContentEncoding :: Maybe Text
    , _poContentLanguage :: Maybe Text
    , _poContentLength :: Maybe Integer
    , _poContentMD5 :: Maybe Text
    , _poContentType :: Maybe Text
    , _poExpires :: Maybe RFC822
    , _poGrantFullControl :: Maybe Text
    , _poGrantRead :: Maybe Text
    , _poGrantReadACP :: Maybe Text
    , _poGrantWriteACP :: Maybe Text
    , _poKey :: ObjectKey
    , _poMetadata :: Map Text Text
    , _poServerSideEncryption :: Maybe ServerSideEncryption
    , _poStorageClass :: Maybe StorageClass
    , _poWebsiteRedirectLocation :: Maybe Text
    , _poSSECustomerAlgorithm :: Maybe Text
    , _poSSECustomerKey :: Maybe Text
    , _poSSECustomerKeyMD5 :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutObject' request.
mkPutObject :: ObjectKey -- ^ 'poKey'
            -> RqBody -- ^ 'poBody'
            -> BucketName -- ^ 'poBucket'
            -> PutObject
mkPutObject p16 p2 p3 = PutObject
    { _poACL = Nothing
    , _poBody = p2
    , _poBucket = p3
    , _poCacheControl = Nothing
    , _poContentDisposition = Nothing
    , _poContentEncoding = Nothing
    , _poContentLanguage = Nothing
    , _poContentLength = Nothing
    , _poContentMD5 = Nothing
    , _poContentType = Nothing
    , _poExpires = Nothing
    , _poGrantFullControl = Nothing
    , _poGrantRead = Nothing
    , _poGrantReadACP = Nothing
    , _poGrantWriteACP = Nothing
    , _poKey = p16
    , _poMetadata = mempty
    , _poServerSideEncryption = Nothing
    , _poStorageClass = Nothing
    , _poWebsiteRedirectLocation = Nothing
    , _poSSECustomerAlgorithm = Nothing
    , _poSSECustomerKey = Nothing
    , _poSSECustomerKeyMD5 = Nothing
    }

-- | The canned ACL to apply to the object.
poACL :: Lens' PutObject (Maybe ObjectCannedACL)
poACL = lens _poACL (\s a -> s { _poACL = a })

poBody :: Lens' PutObject RqBody
poBody = lens _poBody (\s a -> s { _poBody = a })

poBucket :: Lens' PutObject BucketName
poBucket = lens _poBucket (\s a -> s { _poBucket = a })

-- | Specifies caching behavior along the request/reply chain.
poCacheControl :: Lens' PutObject (Maybe Text)
poCacheControl = lens _poCacheControl (\s a -> s { _poCacheControl = a })

-- | Specifies presentational information for the object.
poContentDisposition :: Lens' PutObject (Maybe Text)
poContentDisposition =
    lens _poContentDisposition (\s a -> s { _poContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
poContentEncoding :: Lens' PutObject (Maybe Text)
poContentEncoding =
    lens _poContentEncoding (\s a -> s { _poContentEncoding = a })

-- | The language the content is in.
poContentLanguage :: Lens' PutObject (Maybe Text)
poContentLanguage =
    lens _poContentLanguage (\s a -> s { _poContentLanguage = a })

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
poContentLength :: Lens' PutObject (Maybe Integer)
poContentLength = lens _poContentLength (\s a -> s { _poContentLength = a })

poContentMD5 :: Lens' PutObject (Maybe Text)
poContentMD5 = lens _poContentMD5 (\s a -> s { _poContentMD5 = a })

-- | A standard MIME type describing the format of the object data.
poContentType :: Lens' PutObject (Maybe Text)
poContentType = lens _poContentType (\s a -> s { _poContentType = a })

-- | The date and time at which the object is no longer cacheable.
poExpires :: Lens' PutObject (Maybe RFC822)
poExpires = lens _poExpires (\s a -> s { _poExpires = a })

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
poGrantFullControl :: Lens' PutObject (Maybe Text)
poGrantFullControl =
    lens _poGrantFullControl (\s a -> s { _poGrantFullControl = a })

-- | Allows grantee to read the object data and its metadata.
poGrantRead :: Lens' PutObject (Maybe Text)
poGrantRead = lens _poGrantRead (\s a -> s { _poGrantRead = a })

-- | Allows grantee to read the object ACL.
poGrantReadACP :: Lens' PutObject (Maybe Text)
poGrantReadACP = lens _poGrantReadACP (\s a -> s { _poGrantReadACP = a })

-- | Allows grantee to write the ACL for the applicable object.
poGrantWriteACP :: Lens' PutObject (Maybe Text)
poGrantWriteACP = lens _poGrantWriteACP (\s a -> s { _poGrantWriteACP = a })

poKey :: Lens' PutObject ObjectKey
poKey = lens _poKey (\s a -> s { _poKey = a })

-- | A map of metadata to store with the object in S3.
poMetadata :: Lens' PutObject (Map Text Text)
poMetadata = lens _poMetadata (\s a -> s { _poMetadata = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
poServerSideEncryption :: Lens' PutObject (Maybe ServerSideEncryption)
poServerSideEncryption =
    lens _poServerSideEncryption (\s a -> s { _poServerSideEncryption = a })

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
poStorageClass :: Lens' PutObject (Maybe StorageClass)
poStorageClass = lens _poStorageClass (\s a -> s { _poStorageClass = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
poWebsiteRedirectLocation :: Lens' PutObject (Maybe Text)
poWebsiteRedirectLocation =
    lens _poWebsiteRedirectLocation
         (\s a -> s { _poWebsiteRedirectLocation = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
poSSECustomerAlgorithm :: Lens' PutObject (Maybe Text)
poSSECustomerAlgorithm =
    lens _poSSECustomerAlgorithm (\s a -> s { _poSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
poSSECustomerKey :: Lens' PutObject (Maybe Text)
poSSECustomerKey =
    lens _poSSECustomerKey (\s a -> s { _poSSECustomerKey = a })

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
poSSECustomerKeyMD5 :: Lens' PutObject (Maybe Text)
poSSECustomerKeyMD5 =
    lens _poSSECustomerKeyMD5 (\s a -> s { _poSSECustomerKeyMD5 = a })

instance ToPath PutObject where
    toPath PutObject{..} = mconcat
        [ "/"
        , toBS _poBucket
        , "/"
        , toBS _poKey
        ]

instance ToQuery PutObject

instance ToHeaders PutObject where
    toHeaders PutObject{..} = concat
        [ "x-amz-acl" =: _poACL
        , "Cache-Control" =: _poCacheControl
        , "Content-Disposition" =: _poContentDisposition
        , "Content-Encoding" =: _poContentEncoding
        , "Content-Language" =: _poContentLanguage
        , "Content-Length" =: _poContentLength
        , "Content-MD5" =: _poContentMD5
        , "Content-Type" =: _poContentType
        , "Expires" =: _poExpires
        , "x-amz-grant-full-control" =: _poGrantFullControl
        , "x-amz-grant-read" =: _poGrantRead
        , "x-amz-grant-read-acp" =: _poGrantReadACP
        , "x-amz-grant-write-acp" =: _poGrantWriteACP
        , "x-amz-meta-" =: _poMetadata
        , "x-amz-server-side-encryption" =: _poServerSideEncryption
        , "x-amz-storage-class" =: _poStorageClass
        , "x-amz-website-redirect-location" =: _poWebsiteRedirectLocation
        , "x-amz-server-side-encryption-customer-algorithm" =: _poSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _poSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _poSSECustomerKeyMD5
        ]

instance ToBody PutObject where
    toBody = toBody . _poBody

data PutObjectResponse = PutObjectResponse
    { _porExpiration :: Maybe RFC822
    , _porETag :: Maybe ETag
    , _porServerSideEncryption :: Maybe ServerSideEncryption
    , _porVersionId :: Maybe ObjectVersionId
    , _porSSECustomerAlgorithm :: Maybe Text
    , _porSSECustomerKeyMD5 :: Maybe Text
    } deriving (Show, Generic)

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
porExpiration :: Lens' PutObjectResponse (Maybe RFC822)
porExpiration = lens _porExpiration (\s a -> s { _porExpiration = a })

-- | Entity tag for the uploaded object.
porETag :: Lens' PutObjectResponse (Maybe ETag)
porETag = lens _porETag (\s a -> s { _porETag = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
porServerSideEncryption :: Lens' PutObjectResponse (Maybe ServerSideEncryption)
porServerSideEncryption =
    lens _porServerSideEncryption
         (\s a -> s { _porServerSideEncryption = a })

-- | Version of the object.
porVersionId :: Lens' PutObjectResponse (Maybe ObjectVersionId)
porVersionId = lens _porVersionId (\s a -> s { _porVersionId = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
porSSECustomerAlgorithm :: Lens' PutObjectResponse (Maybe Text)
porSSECustomerAlgorithm =
    lens _porSSECustomerAlgorithm
         (\s a -> s { _porSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
porSSECustomerKeyMD5 :: Lens' PutObjectResponse (Maybe Text)
porSSECustomerKeyMD5 =
    lens _porSSECustomerKeyMD5 (\s a -> s { _porSSECustomerKeyMD5 = a })

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
