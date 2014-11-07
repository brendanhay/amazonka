{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds an object to a bucket.
module Network.AWS.S3.PutObject
    (
    -- * Request
      PutObject
    -- ** Request constructor
    , putObject
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
    , porSSECustomerAlgorithm
    , porSSECustomerKey
    , porSSECustomerKeyMD5
    , porServerSideEncryption
    , porStorageClass
    , porWebsiteRedirectLocation

    -- * Response
    , PutObjectOutput
    -- ** Response constructor
    , putObjectOutput
    -- ** Response lenses
    , pooETag
    , pooExpiration
    , pooSSECustomerAlgorithm
    , pooSSECustomerKeyMD5
    , pooServerSideEncryption
    , pooVersionId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data PutObject = PutObject
    { _porACL                     :: Maybe Text
    , _porBody                    :: RqBody
    , _porBucket                  :: Text
    , _porCacheControl            :: Maybe Text
    , _porContentDisposition      :: Maybe Text
    , _porContentEncoding         :: Maybe Text
    , _porContentLanguage         :: Maybe Text
    , _porContentLength           :: Maybe Int
    , _porContentMD5              :: Maybe Text
    , _porContentType             :: Maybe Text
    , _porExpires                 :: Maybe RFC822
    , _porGrantFullControl        :: Maybe Text
    , _porGrantRead               :: Maybe Text
    , _porGrantReadACP            :: Maybe Text
    , _porGrantWriteACP           :: Maybe Text
    , _porKey                     :: Text
    , _porMetadata                :: Map Text Text
    , _porSSECustomerAlgorithm    :: Maybe Text
    , _porSSECustomerKey          :: Maybe (Sensitive Text)
    , _porSSECustomerKeyMD5       :: Maybe Text
    , _porServerSideEncryption    :: Maybe Text
    , _porStorageClass            :: Maybe Text
    , _porWebsiteRedirectLocation :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'porACL' @::@ 'Maybe' 'Text'
--
-- * 'porBody' @::@ 'RqBody'
--
-- * 'porBucket' @::@ 'Text'
--
-- * 'porCacheControl' @::@ 'Maybe' 'Text'
--
-- * 'porContentDisposition' @::@ 'Maybe' 'Text'
--
-- * 'porContentEncoding' @::@ 'Maybe' 'Text'
--
-- * 'porContentLanguage' @::@ 'Maybe' 'Text'
--
-- * 'porContentLength' @::@ 'Maybe' 'Int'
--
-- * 'porContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'porContentType' @::@ 'Maybe' 'Text'
--
-- * 'porExpires' @::@ 'Maybe' 'UTCTime'
--
-- * 'porGrantFullControl' @::@ 'Maybe' 'Text'
--
-- * 'porGrantRead' @::@ 'Maybe' 'Text'
--
-- * 'porGrantReadACP' @::@ 'Maybe' 'Text'
--
-- * 'porGrantWriteACP' @::@ 'Maybe' 'Text'
--
-- * 'porKey' @::@ 'Text'
--
-- * 'porMetadata' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'porSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'porSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'porSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'porServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'porStorageClass' @::@ 'Maybe' 'Text'
--
-- * 'porWebsiteRedirectLocation' @::@ 'Maybe' 'Text'
--
putObject :: RqBody -- ^ 'porBody'
          -> Text -- ^ 'porBucket'
          -> Text -- ^ 'porKey'
          -> PutObject
putObject p1 p2 p3 = PutObject
    { _porBody                    = p1
    , _porBucket                  = p2
    , _porKey                     = p3
    , _porACL                     = Nothing
    , _porCacheControl            = Nothing
    , _porContentDisposition      = Nothing
    , _porContentEncoding         = Nothing
    , _porContentLanguage         = Nothing
    , _porContentLength           = Nothing
    , _porContentMD5              = Nothing
    , _porContentType             = Nothing
    , _porExpires                 = Nothing
    , _porGrantFullControl        = Nothing
    , _porGrantRead               = Nothing
    , _porGrantReadACP            = Nothing
    , _porGrantWriteACP           = Nothing
    , _porMetadata                = mempty
    , _porServerSideEncryption    = Nothing
    , _porStorageClass            = Nothing
    , _porWebsiteRedirectLocation = Nothing
    , _porSSECustomerAlgorithm    = Nothing
    , _porSSECustomerKey          = Nothing
    , _porSSECustomerKeyMD5       = Nothing
    }

-- | The canned ACL to apply to the object.
porACL :: Lens' PutObject (Maybe Text)
porACL = lens _porACL (\s a -> s { _porACL = a })

porBody :: Lens' PutObject RqBody
porBody = lens _porBody (\s a -> s { _porBody = a })

porBucket :: Lens' PutObject Text
porBucket = lens _porBucket (\s a -> s { _porBucket = a })

-- | Specifies caching behavior along the request/reply chain.
porCacheControl :: Lens' PutObject (Maybe Text)
porCacheControl = lens _porCacheControl (\s a -> s { _porCacheControl = a })

-- | Specifies presentational information for the object.
porContentDisposition :: Lens' PutObject (Maybe Text)
porContentDisposition =
    lens _porContentDisposition (\s a -> s { _porContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
porContentEncoding :: Lens' PutObject (Maybe Text)
porContentEncoding =
    lens _porContentEncoding (\s a -> s { _porContentEncoding = a })

-- | The language the content is in.
porContentLanguage :: Lens' PutObject (Maybe Text)
porContentLanguage =
    lens _porContentLanguage (\s a -> s { _porContentLanguage = a })

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
porContentLength :: Lens' PutObject (Maybe Int)
porContentLength = lens _porContentLength (\s a -> s { _porContentLength = a })

porContentMD5 :: Lens' PutObject (Maybe Text)
porContentMD5 = lens _porContentMD5 (\s a -> s { _porContentMD5 = a })

-- | A standard MIME type describing the format of the object data.
porContentType :: Lens' PutObject (Maybe Text)
porContentType = lens _porContentType (\s a -> s { _porContentType = a })

-- | The date and time at which the object is no longer cacheable.
porExpires :: Lens' PutObject (Maybe UTCTime)
porExpires = lens _porExpires (\s a -> s { _porExpires = a })
    . mapping _Time

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
porGrantFullControl :: Lens' PutObject (Maybe Text)
porGrantFullControl =
    lens _porGrantFullControl (\s a -> s { _porGrantFullControl = a })

-- | Allows grantee to read the object data and its metadata.
porGrantRead :: Lens' PutObject (Maybe Text)
porGrantRead = lens _porGrantRead (\s a -> s { _porGrantRead = a })

-- | Allows grantee to read the object ACL.
porGrantReadACP :: Lens' PutObject (Maybe Text)
porGrantReadACP = lens _porGrantReadACP (\s a -> s { _porGrantReadACP = a })

-- | Allows grantee to write the ACL for the applicable object.
porGrantWriteACP :: Lens' PutObject (Maybe Text)
porGrantWriteACP = lens _porGrantWriteACP (\s a -> s { _porGrantWriteACP = a })

porKey :: Lens' PutObject Text
porKey = lens _porKey (\s a -> s { _porKey = a })

-- | A map of metadata to store with the object in S3.
porMetadata :: Lens' PutObject (HashMap Text Text)
porMetadata = lens _porMetadata (\s a -> s { _porMetadata = a })
    . _Map

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
porSSECustomerAlgorithm :: Lens' PutObject (Maybe Text)
porSSECustomerAlgorithm =
    lens _porSSECustomerAlgorithm (\s a -> s { _porSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
porSSECustomerKey :: Lens' PutObject (Maybe Text)
porSSECustomerKey =
    lens _porSSECustomerKey (\s a -> s { _porSSECustomerKey = a })
        . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
porSSECustomerKeyMD5 :: Lens' PutObject (Maybe Text)
porSSECustomerKeyMD5 =
    lens _porSSECustomerKeyMD5 (\s a -> s { _porSSECustomerKeyMD5 = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
porServerSideEncryption :: Lens' PutObject (Maybe Text)
porServerSideEncryption =
    lens _porServerSideEncryption (\s a -> s { _porServerSideEncryption = a })

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
porStorageClass :: Lens' PutObject (Maybe Text)
porStorageClass = lens _porStorageClass (\s a -> s { _porStorageClass = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
porWebsiteRedirectLocation :: Lens' PutObject (Maybe Text)
porWebsiteRedirectLocation =
    lens _porWebsiteRedirectLocation
        (\s a -> s { _porWebsiteRedirectLocation = a })

instance ToPath PutObject where
    toPath PutObject{..} = mconcat
        [ "/"
        , toText _porBucket
        , "/"
        , toText _porKey
        ]

instance ToQuery PutObject

instance ToHeaders PutObject where
    toHeaders PutObject{..} = mconcat
        [ "x-amz-acl"                                       =: _porACL
        , "Cache-Control"                                   =: _porCacheControl
        , "Content-Disposition"                             =: _porContentDisposition
        , "Content-Encoding"                                =: _porContentEncoding
        , "Content-Language"                                =: _porContentLanguage
        , "Content-Length"                                  =: _porContentLength
        , "Content-MD5"                                     =: _porContentMD5
        , "Content-Type"                                    =: _porContentType
        , "Expires"                                         =: _porExpires
        , "x-amz-grant-full-control"                        =: _porGrantFullControl
        , "x-amz-grant-read"                                =: _porGrantRead
        , "x-amz-grant-read-acp"                            =: _porGrantReadACP
        , "x-amz-grant-write-acp"                           =: _porGrantWriteACP
        , "x-amz-meta-"                                     =: _porMetadata
        , "x-amz-server-side-encryption"                    =: _porServerSideEncryption
        , "x-amz-storage-class"                             =: _porStorageClass
        , "x-amz-website-redirect-location"                 =: _porWebsiteRedirectLocation
        , "x-amz-server-side-encryption-customer-algorithm" =: _porSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"       =: _porSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"   =: _porSSECustomerKeyMD5
        ]

instance ToBody PutObject where
    toBody = toBody . _porBody

data PutObjectOutput = PutObjectOutput
    { _pooETag                 :: Maybe Text
    , _pooExpiration           :: Maybe RFC822
    , _pooSSECustomerAlgorithm :: Maybe Text
    , _pooSSECustomerKeyMD5    :: Maybe Text
    , _pooServerSideEncryption :: Maybe Text
    , _pooVersionId            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutObjectOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pooETag' @::@ 'Maybe' 'Text'
--
-- * 'pooExpiration' @::@ 'Maybe' 'UTCTime'
--
-- * 'pooSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'pooSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'pooServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'pooVersionId' @::@ 'Maybe' 'Text'
--
putObjectOutput :: PutObjectOutput
putObjectOutput = PutObjectOutput
    { _pooExpiration           = Nothing
    , _pooETag                 = Nothing
    , _pooServerSideEncryption = Nothing
    , _pooVersionId            = Nothing
    , _pooSSECustomerAlgorithm = Nothing
    , _pooSSECustomerKeyMD5    = Nothing
    }

-- | Entity tag for the uploaded object.
pooETag :: Lens' PutObjectOutput (Maybe Text)
pooETag = lens _pooETag (\s a -> s { _pooETag = a })

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
pooExpiration :: Lens' PutObjectOutput (Maybe UTCTime)
pooExpiration = lens _pooExpiration (\s a -> s { _pooExpiration = a })
    . mapping _Time

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
pooSSECustomerAlgorithm :: Lens' PutObjectOutput (Maybe Text)
pooSSECustomerAlgorithm =
    lens _pooSSECustomerAlgorithm (\s a -> s { _pooSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
pooSSECustomerKeyMD5 :: Lens' PutObjectOutput (Maybe Text)
pooSSECustomerKeyMD5 =
    lens _pooSSECustomerKeyMD5 (\s a -> s { _pooSSECustomerKeyMD5 = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
pooServerSideEncryption :: Lens' PutObjectOutput (Maybe Text)
pooServerSideEncryption =
    lens _pooServerSideEncryption (\s a -> s { _pooServerSideEncryption = a })

-- | Version of the object.
pooVersionId :: Lens' PutObjectOutput (Maybe Text)
pooVersionId = lens _pooVersionId (\s a -> s { _pooVersionId = a })

instance AWSRequest PutObject where
    type Sv PutObject = S3
    type Rs PutObject = PutObjectOutput

    request  = put'
    response = const . xmlResponse $ \h x -> PutObjectOutput
        <$> h ~:? "ETag"
        <*> h ~:? "x-amz-expiration"
        <*> h ~:? "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~:? "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~:? "x-amz-server-side-encryption"
        <*> h ~:? "x-amz-version-id"
