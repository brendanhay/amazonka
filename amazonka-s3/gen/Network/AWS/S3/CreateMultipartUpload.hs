{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.CreateMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Initiates a multipart upload and returns an upload ID. Note: After you
-- initiate multipart upload and upload one or more parts, you must either
-- complete or abort multipart upload in order to stop getting charged for
-- storage of the uploaded parts. Only after you either complete or abort
-- multipart upload, Amazon S3 frees up the parts storage and stops charging
-- you for the parts storage.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/CreateMultipartUpload.html>
module Network.AWS.S3.CreateMultipartUpload
    (
    -- * Request
      CreateMultipartUpload
    -- ** Request constructor
    , createMultipartUpload
    -- ** Request lenses
    , cmuACL
    , cmuBucket
    , cmuCacheControl
    , cmuContentDisposition
    , cmuContentEncoding
    , cmuContentLanguage
    , cmuContentType
    , cmuExpires
    , cmuGrantFullControl
    , cmuGrantRead
    , cmuGrantReadACP
    , cmuGrantWriteACP
    , cmuKey
    , cmuMetadata
    , cmuSSECustomerAlgorithm
    , cmuSSECustomerKey
    , cmuSSECustomerKeyMD5
    , cmuSSEKMSKeyId
    , cmuServerSideEncryption
    , cmuStorageClass
    , cmuWebsiteRedirectLocation

    -- * Response
    , CreateMultipartUploadResponse
    -- ** Response constructor
    , createMultipartUploadResponse
    -- ** Response lenses
    , cmurBucket
    , cmurKey
    , cmurSSECustomerAlgorithm
    , cmurSSECustomerKeyMD5
    , cmurSSEKMSKeyId
    , cmurServerSideEncryption
    , cmurUploadId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data CreateMultipartUpload = CreateMultipartUpload
    { _cmuACL                     :: Maybe Text
    , _cmuBucket                  :: Text
    , _cmuCacheControl            :: Maybe Text
    , _cmuContentDisposition      :: Maybe Text
    , _cmuContentEncoding         :: Maybe Text
    , _cmuContentLanguage         :: Maybe Text
    , _cmuContentType             :: Maybe Text
    , _cmuExpires                 :: Maybe RFC822
    , _cmuGrantFullControl        :: Maybe Text
    , _cmuGrantRead               :: Maybe Text
    , _cmuGrantReadACP            :: Maybe Text
    , _cmuGrantWriteACP           :: Maybe Text
    , _cmuKey                     :: Text
    , _cmuMetadata                :: Map (CI Text) Text
    , _cmuSSECustomerAlgorithm    :: Maybe Text
    , _cmuSSECustomerKey          :: Maybe (Sensitive Text)
    , _cmuSSECustomerKeyMD5       :: Maybe Text
    , _cmuSSEKMSKeyId             :: Maybe (Sensitive Text)
    , _cmuServerSideEncryption    :: Maybe Text
    , _cmuStorageClass            :: Maybe Text
    , _cmuWebsiteRedirectLocation :: Maybe Text
    } deriving (Eq, Show)

-- | 'CreateMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmuACL' @::@ 'Maybe' 'Text'
--
-- * 'cmuBucket' @::@ 'Text'
--
-- * 'cmuCacheControl' @::@ 'Maybe' 'Text'
--
-- * 'cmuContentDisposition' @::@ 'Maybe' 'Text'
--
-- * 'cmuContentEncoding' @::@ 'Maybe' 'Text'
--
-- * 'cmuContentLanguage' @::@ 'Maybe' 'Text'
--
-- * 'cmuContentType' @::@ 'Maybe' 'Text'
--
-- * 'cmuExpires' @::@ 'Maybe' 'UTCTime'
--
-- * 'cmuGrantFullControl' @::@ 'Maybe' 'Text'
--
-- * 'cmuGrantRead' @::@ 'Maybe' 'Text'
--
-- * 'cmuGrantReadACP' @::@ 'Maybe' 'Text'
--
-- * 'cmuGrantWriteACP' @::@ 'Maybe' 'Text'
--
-- * 'cmuKey' @::@ 'Text'
--
-- * 'cmuMetadata' @::@ 'HashMap' ('CI' 'Text') 'Text'
--
-- * 'cmuSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'cmuSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'cmuSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'cmuSSEKMSKeyId' @::@ 'Maybe' 'Text'
--
-- * 'cmuServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'cmuStorageClass' @::@ 'Maybe' 'Text'
--
-- * 'cmuWebsiteRedirectLocation' @::@ 'Maybe' 'Text'
--
createMultipartUpload :: Text -- ^ 'cmuBucket'
                      -> Text -- ^ 'cmuKey'
                      -> CreateMultipartUpload
createMultipartUpload p1 p2 = CreateMultipartUpload
    { _cmuBucket                  = p1
    , _cmuKey                     = p2
    , _cmuACL                     = Nothing
    , _cmuCacheControl            = Nothing
    , _cmuContentDisposition      = Nothing
    , _cmuContentEncoding         = Nothing
    , _cmuContentLanguage         = Nothing
    , _cmuContentType             = Nothing
    , _cmuExpires                 = Nothing
    , _cmuGrantFullControl        = Nothing
    , _cmuGrantRead               = Nothing
    , _cmuGrantReadACP            = Nothing
    , _cmuGrantWriteACP           = Nothing
    , _cmuMetadata                = mempty
    , _cmuServerSideEncryption    = Nothing
    , _cmuStorageClass            = Nothing
    , _cmuWebsiteRedirectLocation = Nothing
    , _cmuSSECustomerAlgorithm    = Nothing
    , _cmuSSECustomerKey          = Nothing
    , _cmuSSECustomerKeyMD5       = Nothing
    , _cmuSSEKMSKeyId             = Nothing
    }

-- | The canned ACL to apply to the object.
cmuACL :: Lens' CreateMultipartUpload (Maybe Text)
cmuACL = lens _cmuACL (\s a -> s { _cmuACL = a })

cmuBucket :: Lens' CreateMultipartUpload Text
cmuBucket = lens _cmuBucket (\s a -> s { _cmuBucket = a })

-- | Specifies caching behavior along the request/reply chain.
cmuCacheControl :: Lens' CreateMultipartUpload (Maybe Text)
cmuCacheControl = lens _cmuCacheControl (\s a -> s { _cmuCacheControl = a })

-- | Specifies presentational information for the object.
cmuContentDisposition :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentDisposition =
    lens _cmuContentDisposition (\s a -> s { _cmuContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
cmuContentEncoding :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentEncoding =
    lens _cmuContentEncoding (\s a -> s { _cmuContentEncoding = a })

-- | The language the content is in.
cmuContentLanguage :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentLanguage =
    lens _cmuContentLanguage (\s a -> s { _cmuContentLanguage = a })

-- | A standard MIME type describing the format of the object data.
cmuContentType :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentType = lens _cmuContentType (\s a -> s { _cmuContentType = a })

-- | The date and time at which the object is no longer cacheable.
cmuExpires :: Lens' CreateMultipartUpload (Maybe UTCTime)
cmuExpires = lens _cmuExpires (\s a -> s { _cmuExpires = a }) . mapping _Time

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
cmuGrantFullControl :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantFullControl =
    lens _cmuGrantFullControl (\s a -> s { _cmuGrantFullControl = a })

-- | Allows grantee to read the object data and its metadata.
cmuGrantRead :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantRead = lens _cmuGrantRead (\s a -> s { _cmuGrantRead = a })

-- | Allows grantee to read the object ACL.
cmuGrantReadACP :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantReadACP = lens _cmuGrantReadACP (\s a -> s { _cmuGrantReadACP = a })

-- | Allows grantee to write the ACL for the applicable object.
cmuGrantWriteACP :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantWriteACP = lens _cmuGrantWriteACP (\s a -> s { _cmuGrantWriteACP = a })

cmuKey :: Lens' CreateMultipartUpload Text
cmuKey = lens _cmuKey (\s a -> s { _cmuKey = a })

-- | A map of metadata to store with the object in S3.
cmuMetadata :: Lens' CreateMultipartUpload (HashMap (CI Text) Text)
cmuMetadata = lens _cmuMetadata (\s a -> s { _cmuMetadata = a }) . _Map

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
cmuSSECustomerAlgorithm :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerAlgorithm =
    lens _cmuSSECustomerAlgorithm (\s a -> s { _cmuSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
cmuSSECustomerKey :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerKey =
    lens _cmuSSECustomerKey (\s a -> s { _cmuSSECustomerKey = a })
        . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
cmuSSECustomerKeyMD5 :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerKeyMD5 =
    lens _cmuSSECustomerKeyMD5 (\s a -> s { _cmuSSECustomerKeyMD5 = a })

-- | Specifies the AWS KMS key ID to use for object encryption.
cmuSSEKMSKeyId :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSEKMSKeyId = lens _cmuSSEKMSKeyId (\s a -> s { _cmuSSEKMSKeyId = a }) . mapping _Sensitive

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
cmuServerSideEncryption :: Lens' CreateMultipartUpload (Maybe Text)
cmuServerSideEncryption =
    lens _cmuServerSideEncryption (\s a -> s { _cmuServerSideEncryption = a })

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
cmuStorageClass :: Lens' CreateMultipartUpload (Maybe Text)
cmuStorageClass = lens _cmuStorageClass (\s a -> s { _cmuStorageClass = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
cmuWebsiteRedirectLocation :: Lens' CreateMultipartUpload (Maybe Text)
cmuWebsiteRedirectLocation =
    lens _cmuWebsiteRedirectLocation
        (\s a -> s { _cmuWebsiteRedirectLocation = a })

data CreateMultipartUploadResponse = CreateMultipartUploadResponse
    { _cmurBucket               :: Maybe Text
    , _cmurKey                  :: Maybe Text
    , _cmurSSECustomerAlgorithm :: Maybe Text
    , _cmurSSECustomerKeyMD5    :: Maybe Text
    , _cmurSSEKMSKeyId          :: Maybe (Sensitive Text)
    , _cmurServerSideEncryption :: Maybe Text
    , _cmurUploadId             :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'CreateMultipartUploadResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmurBucket' @::@ 'Maybe' 'Text'
--
-- * 'cmurKey' @::@ 'Maybe' 'Text'
--
-- * 'cmurSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'cmurSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'cmurSSEKMSKeyId' @::@ 'Maybe' 'Text'
--
-- * 'cmurServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'cmurUploadId' @::@ 'Maybe' 'Text'
--
createMultipartUploadResponse :: CreateMultipartUploadResponse
createMultipartUploadResponse = CreateMultipartUploadResponse
    { _cmurBucket               = Nothing
    , _cmurKey                  = Nothing
    , _cmurUploadId             = Nothing
    , _cmurServerSideEncryption = Nothing
    , _cmurSSECustomerAlgorithm = Nothing
    , _cmurSSECustomerKeyMD5    = Nothing
    , _cmurSSEKMSKeyId          = Nothing
    }

-- | Name of the bucket to which the multipart upload was initiated.
cmurBucket :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurBucket = lens _cmurBucket (\s a -> s { _cmurBucket = a })

-- | Object key for which the multipart upload was initiated.
cmurKey :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurKey = lens _cmurKey (\s a -> s { _cmurKey = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
cmurSSECustomerAlgorithm :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurSSECustomerAlgorithm =
    lens _cmurSSECustomerAlgorithm
        (\s a -> s { _cmurSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cmurSSECustomerKeyMD5 :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurSSECustomerKeyMD5 =
    lens _cmurSSECustomerKeyMD5 (\s a -> s { _cmurSSECustomerKeyMD5 = a })

-- | If present, specifies the AWS KMS key used to encrypt the object.
cmurSSEKMSKeyId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurSSEKMSKeyId = lens _cmurSSEKMSKeyId (\s a -> s { _cmurSSEKMSKeyId = a }) . mapping _Sensitive

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
cmurServerSideEncryption :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurServerSideEncryption =
    lens _cmurServerSideEncryption
        (\s a -> s { _cmurServerSideEncryption = a })

-- | ID for the initiated multipart upload.
cmurUploadId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurUploadId = lens _cmurUploadId (\s a -> s { _cmurUploadId = a })

instance ToPath CreateMultipartUpload where
    toPath CreateMultipartUpload{..} = mconcat
        [ "/"
        , toText _cmuBucket
        , "/"
        , toText _cmuKey
        ]

instance ToQuery CreateMultipartUpload where
    toQuery = const "uploads"

instance ToHeaders CreateMultipartUpload where
    toHeaders CreateMultipartUpload{..} = mconcat
        [ "x-amz-acl"                                       =: _cmuACL
        , "Cache-Control"                                   =: _cmuCacheControl
        , "Content-Disposition"                             =: _cmuContentDisposition
        , "Content-Encoding"                                =: _cmuContentEncoding
        , "Content-Language"                                =: _cmuContentLanguage
        , "Content-Type"                                    =: _cmuContentType
        , "Expires"                                         =: _cmuExpires
        , "x-amz-grant-full-control"                        =: _cmuGrantFullControl
        , "x-amz-grant-read"                                =: _cmuGrantRead
        , "x-amz-grant-read-acp"                            =: _cmuGrantReadACP
        , "x-amz-grant-write-acp"                           =: _cmuGrantWriteACP
        , "x-amz-meta-"                                     =: _cmuMetadata
        , "x-amz-server-side-encryption"                    =: _cmuServerSideEncryption
        , "x-amz-storage-class"                             =: _cmuStorageClass
        , "x-amz-website-redirect-location"                 =: _cmuWebsiteRedirectLocation
        , "x-amz-server-side-encryption-customer-algorithm" =: _cmuSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"       =: _cmuSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"   =: _cmuSSECustomerKeyMD5
        , "x-amz-server-side-encryption-aws-kms-key-id"     =: _cmuSSEKMSKeyId
        ]

instance ToXMLRoot CreateMultipartUpload where
    toXMLRoot = const (element "CreateMultipartUpload" [])

instance ToXML CreateMultipartUpload

instance AWSRequest CreateMultipartUpload where
    type Sv CreateMultipartUpload = S3
    type Rs CreateMultipartUpload = CreateMultipartUploadResponse

    request  = post
    response = xmlHeaderResponse $ \h x -> CreateMultipartUploadResponse
        <$> x .@? "Bucket"
        <*> x .@? "Key"
        <*> h ~:? "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~:? "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~:? "x-amz-server-side-encryption-aws-kms-key-id"
        <*> h ~:? "x-amz-server-side-encryption"
        <*> x .@? "UploadId"
