{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.S3.CreateMultipartUpload
    (
    -- * Request
      CreateMultipartUpload
    -- ** Request constructor
    , createMultipartUpload
    -- ** Request lenses
    , cmu1ACL
    , cmu1Bucket
    , cmu1CacheControl
    , cmu1ContentDisposition
    , cmu1ContentEncoding
    , cmu1ContentLanguage
    , cmu1ContentType
    , cmu1Expires
    , cmu1GrantFullControl
    , cmu1GrantRead
    , cmu1GrantReadACP
    , cmu1GrantWriteACP
    , cmu1Key
    , cmu1Metadata
    , cmu1SSECustomerAlgorithm
    , cmu1SSECustomerKey
    , cmu1SSECustomerKeyMD5
    , cmu1ServerSideEncryption
    , cmu1StorageClass
    , cmu1WebsiteRedirectLocation

    -- * Response
    , CreateMultipartUploadOutput
    -- ** Response constructor
    , createMultipartUploadOutput
    -- ** Response lenses
    , cmuo1Bucket
    , cmuo1Key
    , cmuo1SSECustomerAlgorithm
    , cmuo1SSECustomerKeyMD5
    , cmuo1ServerSideEncryption
    , cmuo1UploadId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data CreateMultipartUpload = CreateMultipartUpload
    { _cmu1ACL                     :: Maybe Text
    , _cmu1Bucket                  :: Text
    , _cmu1CacheControl            :: Maybe Text
    , _cmu1ContentDisposition      :: Maybe Text
    , _cmu1ContentEncoding         :: Maybe Text
    , _cmu1ContentLanguage         :: Maybe Text
    , _cmu1ContentType             :: Maybe Text
    , _cmu1Expires                 :: Maybe RFC822
    , _cmu1GrantFullControl        :: Maybe Text
    , _cmu1GrantRead               :: Maybe Text
    , _cmu1GrantReadACP            :: Maybe Text
    , _cmu1GrantWriteACP           :: Maybe Text
    , _cmu1Key                     :: Text
    , _cmu1Metadata                :: Map Text Text
    , _cmu1SSECustomerAlgorithm    :: Maybe Text
    , _cmu1SSECustomerKey          :: Maybe (Sensitive Text)
    , _cmu1SSECustomerKeyMD5       :: Maybe Text
    , _cmu1ServerSideEncryption    :: Maybe Text
    , _cmu1StorageClass            :: Maybe Text
    , _cmu1WebsiteRedirectLocation :: Maybe Text
    } (Eq, Show, Generic)

-- | 'CreateMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmu1ACL' @::@ 'Maybe' 'Text'
--
-- * 'cmu1Bucket' @::@ 'Text'
--
-- * 'cmu1CacheControl' @::@ 'Maybe' 'Text'
--
-- * 'cmu1ContentDisposition' @::@ 'Maybe' 'Text'
--
-- * 'cmu1ContentEncoding' @::@ 'Maybe' 'Text'
--
-- * 'cmu1ContentLanguage' @::@ 'Maybe' 'Text'
--
-- * 'cmu1ContentType' @::@ 'Maybe' 'Text'
--
-- * 'cmu1Expires' @::@ 'Maybe' 'UTCTime'
--
-- * 'cmu1GrantFullControl' @::@ 'Maybe' 'Text'
--
-- * 'cmu1GrantRead' @::@ 'Maybe' 'Text'
--
-- * 'cmu1GrantReadACP' @::@ 'Maybe' 'Text'
--
-- * 'cmu1GrantWriteACP' @::@ 'Maybe' 'Text'
--
-- * 'cmu1Key' @::@ 'Text'
--
-- * 'cmu1Metadata' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'cmu1SSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'cmu1SSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'cmu1SSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'cmu1ServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'cmu1StorageClass' @::@ 'Maybe' 'Text'
--
-- * 'cmu1WebsiteRedirectLocation' @::@ 'Maybe' 'Text'
--
createMultipartUpload :: Text -- ^ 'cmu1Bucket'
                      -> Text -- ^ 'cmu1Key'
                      -> CreateMultipartUpload
createMultipartUpload p1 p2 = CreateMultipartUpload
    { _cmu1Bucket                  = p1
    , _cmu1Key                     = p2
    , _cmu1ACL                     = Nothing
    , _cmu1CacheControl            = Nothing
    , _cmu1ContentDisposition      = Nothing
    , _cmu1ContentEncoding         = Nothing
    , _cmu1ContentLanguage         = Nothing
    , _cmu1ContentType             = Nothing
    , _cmu1Expires                 = Nothing
    , _cmu1GrantFullControl        = Nothing
    , _cmu1GrantRead               = Nothing
    , _cmu1GrantReadACP            = Nothing
    , _cmu1GrantWriteACP           = Nothing
    , _cmu1Metadata                = mempty
    , _cmu1ServerSideEncryption    = Nothing
    , _cmu1StorageClass            = Nothing
    , _cmu1WebsiteRedirectLocation = Nothing
    , _cmu1SSECustomerAlgorithm    = Nothing
    , _cmu1SSECustomerKey          = Nothing
    , _cmu1SSECustomerKeyMD5       = Nothing
    }

-- | The canned ACL to apply to the object.
cmu1ACL :: Lens' CreateMultipartUpload (Maybe Text)
cmu1ACL = lens _cmu1ACL (\s a -> s { _cmu1ACL = a })

cmu1Bucket :: Lens' CreateMultipartUpload Text
cmu1Bucket = lens _cmu1Bucket (\s a -> s { _cmu1Bucket = a })

-- | Specifies caching behavior along the request/reply chain.
cmu1CacheControl :: Lens' CreateMultipartUpload (Maybe Text)
cmu1CacheControl = lens _cmu1CacheControl (\s a -> s { _cmu1CacheControl = a })

-- | Specifies presentational information for the object.
cmu1ContentDisposition :: Lens' CreateMultipartUpload (Maybe Text)
cmu1ContentDisposition =
    lens _cmu1ContentDisposition (\s a -> s { _cmu1ContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
cmu1ContentEncoding :: Lens' CreateMultipartUpload (Maybe Text)
cmu1ContentEncoding =
    lens _cmu1ContentEncoding (\s a -> s { _cmu1ContentEncoding = a })

-- | The language the content is in.
cmu1ContentLanguage :: Lens' CreateMultipartUpload (Maybe Text)
cmu1ContentLanguage =
    lens _cmu1ContentLanguage (\s a -> s { _cmu1ContentLanguage = a })

-- | A standard MIME type describing the format of the object data.
cmu1ContentType :: Lens' CreateMultipartUpload (Maybe Text)
cmu1ContentType = lens _cmu1ContentType (\s a -> s { _cmu1ContentType = a })

-- | The date and time at which the object is no longer cacheable.
cmu1Expires :: Lens' CreateMultipartUpload (Maybe UTCTime)
cmu1Expires = lens _cmu1Expires (\s a -> s { _cmu1Expires = a })
    . mapping _Time

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
cmu1GrantFullControl :: Lens' CreateMultipartUpload (Maybe Text)
cmu1GrantFullControl =
    lens _cmu1GrantFullControl (\s a -> s { _cmu1GrantFullControl = a })

-- | Allows grantee to read the object data and its metadata.
cmu1GrantRead :: Lens' CreateMultipartUpload (Maybe Text)
cmu1GrantRead = lens _cmu1GrantRead (\s a -> s { _cmu1GrantRead = a })

-- | Allows grantee to read the object ACL.
cmu1GrantReadACP :: Lens' CreateMultipartUpload (Maybe Text)
cmu1GrantReadACP = lens _cmu1GrantReadACP (\s a -> s { _cmu1GrantReadACP = a })

-- | Allows grantee to write the ACL for the applicable object.
cmu1GrantWriteACP :: Lens' CreateMultipartUpload (Maybe Text)
cmu1GrantWriteACP =
    lens _cmu1GrantWriteACP (\s a -> s { _cmu1GrantWriteACP = a })

cmu1Key :: Lens' CreateMultipartUpload Text
cmu1Key = lens _cmu1Key (\s a -> s { _cmu1Key = a })

-- | A map of metadata to store with the object in S3.
cmu1Metadata :: Lens' CreateMultipartUpload (HashMap Text Text)
cmu1Metadata = lens _cmu1Metadata (\s a -> s { _cmu1Metadata = a })
    . _Map

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
cmu1SSECustomerAlgorithm :: Lens' CreateMultipartUpload (Maybe Text)
cmu1SSECustomerAlgorithm =
    lens _cmu1SSECustomerAlgorithm
        (\s a -> s { _cmu1SSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
cmu1SSECustomerKey :: Lens' CreateMultipartUpload (Maybe Text)
cmu1SSECustomerKey =
    lens _cmu1SSECustomerKey (\s a -> s { _cmu1SSECustomerKey = a })
        . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
cmu1SSECustomerKeyMD5 :: Lens' CreateMultipartUpload (Maybe Text)
cmu1SSECustomerKeyMD5 =
    lens _cmu1SSECustomerKeyMD5 (\s a -> s { _cmu1SSECustomerKeyMD5 = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
cmu1ServerSideEncryption :: Lens' CreateMultipartUpload (Maybe Text)
cmu1ServerSideEncryption =
    lens _cmu1ServerSideEncryption
        (\s a -> s { _cmu1ServerSideEncryption = a })

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
cmu1StorageClass :: Lens' CreateMultipartUpload (Maybe Text)
cmu1StorageClass = lens _cmu1StorageClass (\s a -> s { _cmu1StorageClass = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
cmu1WebsiteRedirectLocation :: Lens' CreateMultipartUpload (Maybe Text)
cmu1WebsiteRedirectLocation =
    lens _cmu1WebsiteRedirectLocation
        (\s a -> s { _cmu1WebsiteRedirectLocation = a })

instance ToPath CreateMultipartUpload where
    toPath CreateMultipartUpload{..} = mconcat
        [ "/"
        , toText _cmu1Bucket
        , "/"
        , toText _cmu1Key
        ]

instance ToQuery CreateMultipartUpload where
    toQuery = const "uploads"

instance ToHeaders CreateMultipartUpload where
    toHeaders CreateMultipartUpload{..} = mconcat
        [ "x-amz-acl"                                       =: _cmu1ACL
        , "Cache-Control"                                   =: _cmu1CacheControl
        , "Content-Disposition"                             =: _cmu1ContentDisposition
        , "Content-Encoding"                                =: _cmu1ContentEncoding
        , "Content-Language"                                =: _cmu1ContentLanguage
        , "Content-Type"                                    =: _cmu1ContentType
        , "Expires"                                         =: _cmu1Expires
        , "x-amz-grant-full-control"                        =: _cmu1GrantFullControl
        , "x-amz-grant-read"                                =: _cmu1GrantRead
        , "x-amz-grant-read-acp"                            =: _cmu1GrantReadACP
        , "x-amz-grant-write-acp"                           =: _cmu1GrantWriteACP
        , "x-amz-meta-"                                     =: _cmu1Metadata
        , "x-amz-server-side-encryption"                    =: _cmu1ServerSideEncryption
        , "x-amz-storage-class"                             =: _cmu1StorageClass
        , "x-amz-website-redirect-location"                 =: _cmu1WebsiteRedirectLocation
        , "x-amz-server-side-encryption-customer-algorithm" =: _cmu1SSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"       =: _cmu1SSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"   =: _cmu1SSECustomerKeyMD5
        ]

instance ToBody CreateMultipartUpload

data CreateMultipartUploadOutput = CreateMultipartUploadOutput
    { _cmuo1Bucket               :: Maybe Text
    , _cmuo1Key                  :: Maybe Text
    , _cmuo1SSECustomerAlgorithm :: Maybe Text
    , _cmuo1SSECustomerKeyMD5    :: Maybe Text
    , _cmuo1ServerSideEncryption :: Maybe Text
    , _cmuo1UploadId             :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'CreateMultipartUploadOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmuo1Bucket' @::@ 'Maybe' 'Text'
--
-- * 'cmuo1Key' @::@ 'Maybe' 'Text'
--
-- * 'cmuo1SSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'cmuo1SSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'cmuo1ServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'cmuo1UploadId' @::@ 'Maybe' 'Text'
--
createMultipartUploadOutput :: CreateMultipartUploadOutput
createMultipartUploadOutput = CreateMultipartUploadOutput
    { _cmuo1Bucket               = Nothing
    , _cmuo1Key                  = Nothing
    , _cmuo1UploadId             = Nothing
    , _cmuo1ServerSideEncryption = Nothing
    , _cmuo1SSECustomerAlgorithm = Nothing
    , _cmuo1SSECustomerKeyMD5    = Nothing
    }

-- | Name of the bucket to which the multipart upload was initiated.
cmuo1Bucket :: Lens' CreateMultipartUploadOutput (Maybe Text)
cmuo1Bucket = lens _cmuo1Bucket (\s a -> s { _cmuo1Bucket = a })

-- | Object key for which the multipart upload was initiated.
cmuo1Key :: Lens' CreateMultipartUploadOutput (Maybe Text)
cmuo1Key = lens _cmuo1Key (\s a -> s { _cmuo1Key = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
cmuo1SSECustomerAlgorithm :: Lens' CreateMultipartUploadOutput (Maybe Text)
cmuo1SSECustomerAlgorithm =
    lens _cmuo1SSECustomerAlgorithm
        (\s a -> s { _cmuo1SSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cmuo1SSECustomerKeyMD5 :: Lens' CreateMultipartUploadOutput (Maybe Text)
cmuo1SSECustomerKeyMD5 =
    lens _cmuo1SSECustomerKeyMD5 (\s a -> s { _cmuo1SSECustomerKeyMD5 = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
cmuo1ServerSideEncryption :: Lens' CreateMultipartUploadOutput (Maybe Text)
cmuo1ServerSideEncryption =
    lens _cmuo1ServerSideEncryption
        (\s a -> s { _cmuo1ServerSideEncryption = a })

-- | ID for the initiated multipart upload.
cmuo1UploadId :: Lens' CreateMultipartUploadOutput (Maybe Text)
cmuo1UploadId = lens _cmuo1UploadId (\s a -> s { _cmuo1UploadId = a })

instance FromXML CreateMultipartUploadOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateMultipartUploadOutput"
instance AWSRequest CreateMultipartUpload where
    type Sv CreateMultipartUpload = S3
    type Rs CreateMultipartUpload = CreateMultipartUploadOutput

    request  = post
    response = xmlResponse $ \h x -> CreateMultipartUploadOutput
        <$> x %| "Bucket"
        <*> x %| "Key"
        <*> h ~:? "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~:? "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~:? "x-amz-server-side-encryption"
        <*> x %| "UploadId"
