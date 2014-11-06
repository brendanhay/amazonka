{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , cmur1ACL
    , cmur1Bucket
    , cmur1CacheControl
    , cmur1ContentDisposition
    , cmur1ContentEncoding
    , cmur1ContentLanguage
    , cmur1ContentType
    , cmur1Expires
    , cmur1GrantFullControl
    , cmur1GrantRead
    , cmur1GrantReadACP
    , cmur1GrantWriteACP
    , cmur1Key
    , cmur1Metadata
    , cmur1SSECustomerAlgorithm
    , cmur1SSECustomerKey
    , cmur1SSECustomerKeyMD5
    , cmur1ServerSideEncryption
    , cmur1StorageClass
    , cmur1WebsiteRedirectLocation

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
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

data CreateMultipartUpload = CreateMultipartUpload
    { _cmur1ACL                     :: Maybe Text
    , _cmur1Bucket                  :: BucketName
    , _cmur1CacheControl            :: Maybe Text
    , _cmur1ContentDisposition      :: Maybe Text
    , _cmur1ContentEncoding         :: Maybe Text
    , _cmur1ContentLanguage         :: Maybe Text
    , _cmur1ContentType             :: Maybe Text
    , _cmur1Expires                 :: Maybe RFC822
    , _cmur1GrantFullControl        :: Maybe Text
    , _cmur1GrantRead               :: Maybe Text
    , _cmur1GrantReadACP            :: Maybe Text
    , _cmur1GrantWriteACP           :: Maybe Text
    , _cmur1Key                     :: ObjectKey
    , _cmur1Metadata                :: Map Text Text
    , _cmur1SSECustomerAlgorithm    :: Maybe Text
    , _cmur1SSECustomerKey          :: Maybe (Sensitive Text)
    , _cmur1SSECustomerKeyMD5       :: Maybe Text
    , _cmur1ServerSideEncryption    :: Maybe Text
    , _cmur1StorageClass            :: Maybe Text
    , _cmur1WebsiteRedirectLocation :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmur1ACL' @::@ 'Maybe' 'Text'
--
-- * 'cmur1Bucket' @::@ 'BucketName'
--
-- * 'cmur1CacheControl' @::@ 'Maybe' 'Text'
--
-- * 'cmur1ContentDisposition' @::@ 'Maybe' 'Text'
--
-- * 'cmur1ContentEncoding' @::@ 'Maybe' 'Text'
--
-- * 'cmur1ContentLanguage' @::@ 'Maybe' 'Text'
--
-- * 'cmur1ContentType' @::@ 'Maybe' 'Text'
--
-- * 'cmur1Expires' @::@ 'Maybe' 'UTCTime'
--
-- * 'cmur1GrantFullControl' @::@ 'Maybe' 'Text'
--
-- * 'cmur1GrantRead' @::@ 'Maybe' 'Text'
--
-- * 'cmur1GrantReadACP' @::@ 'Maybe' 'Text'
--
-- * 'cmur1GrantWriteACP' @::@ 'Maybe' 'Text'
--
-- * 'cmur1Key' @::@ 'ObjectKey'
--
-- * 'cmur1Metadata' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'cmur1SSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'cmur1SSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'cmur1SSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'cmur1ServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'cmur1StorageClass' @::@ 'Maybe' 'Text'
--
-- * 'cmur1WebsiteRedirectLocation' @::@ 'Maybe' 'Text'
--
createMultipartUpload :: BucketName -- ^ 'cmur1Bucket'
                      -> ObjectKey -- ^ 'cmur1Key'
                      -> CreateMultipartUpload
createMultipartUpload p1 p2 = CreateMultipartUpload
    { _cmur1Bucket                  = p1
    , _cmur1Key                     = p2
    , _cmur1ACL                     = Nothing
    , _cmur1CacheControl            = Nothing
    , _cmur1ContentDisposition      = Nothing
    , _cmur1ContentEncoding         = Nothing
    , _cmur1ContentLanguage         = Nothing
    , _cmur1ContentType             = Nothing
    , _cmur1Expires                 = Nothing
    , _cmur1GrantFullControl        = Nothing
    , _cmur1GrantRead               = Nothing
    , _cmur1GrantReadACP            = Nothing
    , _cmur1GrantWriteACP           = Nothing
    , _cmur1Metadata                = mempty
    , _cmur1ServerSideEncryption    = Nothing
    , _cmur1StorageClass            = Nothing
    , _cmur1WebsiteRedirectLocation = Nothing
    , _cmur1SSECustomerAlgorithm    = Nothing
    , _cmur1SSECustomerKey          = Nothing
    , _cmur1SSECustomerKeyMD5       = Nothing
    }

-- | The canned ACL to apply to the object.
cmur1ACL :: Lens' CreateMultipartUpload (Maybe Text)
cmur1ACL = lens _cmur1ACL (\s a -> s { _cmur1ACL = a })

cmur1Bucket :: Lens' CreateMultipartUpload BucketName
cmur1Bucket = lens _cmur1Bucket (\s a -> s { _cmur1Bucket = a })

-- | Specifies caching behavior along the request/reply chain.
cmur1CacheControl :: Lens' CreateMultipartUpload (Maybe Text)
cmur1CacheControl =
    lens _cmur1CacheControl (\s a -> s { _cmur1CacheControl = a })

-- | Specifies presentational information for the object.
cmur1ContentDisposition :: Lens' CreateMultipartUpload (Maybe Text)
cmur1ContentDisposition =
    lens _cmur1ContentDisposition (\s a -> s { _cmur1ContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
cmur1ContentEncoding :: Lens' CreateMultipartUpload (Maybe Text)
cmur1ContentEncoding =
    lens _cmur1ContentEncoding (\s a -> s { _cmur1ContentEncoding = a })

-- | The language the content is in.
cmur1ContentLanguage :: Lens' CreateMultipartUpload (Maybe Text)
cmur1ContentLanguage =
    lens _cmur1ContentLanguage (\s a -> s { _cmur1ContentLanguage = a })

-- | A standard MIME type describing the format of the object data.
cmur1ContentType :: Lens' CreateMultipartUpload (Maybe Text)
cmur1ContentType = lens _cmur1ContentType (\s a -> s { _cmur1ContentType = a })

-- | The date and time at which the object is no longer cacheable.
cmur1Expires :: Lens' CreateMultipartUpload (Maybe UTCTime)
cmur1Expires = lens _cmur1Expires (\s a -> s { _cmur1Expires = a })
    . mapping _Time

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
cmur1GrantFullControl :: Lens' CreateMultipartUpload (Maybe Text)
cmur1GrantFullControl =
    lens _cmur1GrantFullControl (\s a -> s { _cmur1GrantFullControl = a })

-- | Allows grantee to read the object data and its metadata.
cmur1GrantRead :: Lens' CreateMultipartUpload (Maybe Text)
cmur1GrantRead = lens _cmur1GrantRead (\s a -> s { _cmur1GrantRead = a })

-- | Allows grantee to read the object ACL.
cmur1GrantReadACP :: Lens' CreateMultipartUpload (Maybe Text)
cmur1GrantReadACP =
    lens _cmur1GrantReadACP (\s a -> s { _cmur1GrantReadACP = a })

-- | Allows grantee to write the ACL for the applicable object.
cmur1GrantWriteACP :: Lens' CreateMultipartUpload (Maybe Text)
cmur1GrantWriteACP =
    lens _cmur1GrantWriteACP (\s a -> s { _cmur1GrantWriteACP = a })

cmur1Key :: Lens' CreateMultipartUpload ObjectKey
cmur1Key = lens _cmur1Key (\s a -> s { _cmur1Key = a })

-- | A map of metadata to store with the object in S3.
cmur1Metadata :: Lens' CreateMultipartUpload (HashMap Text Text)
cmur1Metadata = lens _cmur1Metadata (\s a -> s { _cmur1Metadata = a })
    . _Map

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
cmur1SSECustomerAlgorithm :: Lens' CreateMultipartUpload (Maybe Text)
cmur1SSECustomerAlgorithm =
    lens _cmur1SSECustomerAlgorithm
        (\s a -> s { _cmur1SSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
cmur1SSECustomerKey :: Lens' CreateMultipartUpload (Maybe Text)
cmur1SSECustomerKey =
    lens _cmur1SSECustomerKey (\s a -> s { _cmur1SSECustomerKey = a })
        . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
cmur1SSECustomerKeyMD5 :: Lens' CreateMultipartUpload (Maybe Text)
cmur1SSECustomerKeyMD5 =
    lens _cmur1SSECustomerKeyMD5 (\s a -> s { _cmur1SSECustomerKeyMD5 = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
cmur1ServerSideEncryption :: Lens' CreateMultipartUpload (Maybe Text)
cmur1ServerSideEncryption =
    lens _cmur1ServerSideEncryption
        (\s a -> s { _cmur1ServerSideEncryption = a })

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
cmur1StorageClass :: Lens' CreateMultipartUpload (Maybe Text)
cmur1StorageClass =
    lens _cmur1StorageClass (\s a -> s { _cmur1StorageClass = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
cmur1WebsiteRedirectLocation :: Lens' CreateMultipartUpload (Maybe Text)
cmur1WebsiteRedirectLocation =
    lens _cmur1WebsiteRedirectLocation
        (\s a -> s { _cmur1WebsiteRedirectLocation = a })

instance ToPath CreateMultipartUpload where
    toPath CreateMultipartUpload{..} = mconcat
        [ "/"
        , toText _cmur1Bucket
        , "/"
        , toText _cmur1Key
        ]

instance ToQuery CreateMultipartUpload where
    toQuery = const "uploads"

instance ToHeaders CreateMultipartUpload where
    toHeaders CreateMultipartUpload{..} = mconcat
        [ "x-amz-acl"                                       =: _cmur1ACL
        , "Cache-Control"                                   =: _cmur1CacheControl
        , "Content-Disposition"                             =: _cmur1ContentDisposition
        , "Content-Encoding"                                =: _cmur1ContentEncoding
        , "Content-Language"                                =: _cmur1ContentLanguage
        , "Content-Type"                                    =: _cmur1ContentType
        , "Expires"                                         =: _cmur1Expires
        , "x-amz-grant-full-control"                        =: _cmur1GrantFullControl
        , "x-amz-grant-read"                                =: _cmur1GrantRead
        , "x-amz-grant-read-acp"                            =: _cmur1GrantReadACP
        , "x-amz-grant-write-acp"                           =: _cmur1GrantWriteACP
        , "x-amz-meta-"                                     =: _cmur1Metadata
        , "x-amz-server-side-encryption"                    =: _cmur1ServerSideEncryption
        , "x-amz-storage-class"                             =: _cmur1StorageClass
        , "x-amz-website-redirect-location"                 =: _cmur1WebsiteRedirectLocation
        , "x-amz-server-side-encryption-customer-algorithm" =: _cmur1SSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"       =: _cmur1SSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"   =: _cmur1SSECustomerKeyMD5
        ]

instance ToBody CreateMultipartUpload

data CreateMultipartUploadOutput = CreateMultipartUploadOutput
    { _cmuo1Bucket               :: Maybe BucketName
    , _cmuo1Key                  :: Maybe ObjectKey
    , _cmuo1SSECustomerAlgorithm :: Maybe Text
    , _cmuo1SSECustomerKeyMD5    :: Maybe Text
    , _cmuo1ServerSideEncryption :: Maybe Text
    , _cmuo1UploadId             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest CreateMultipartUpload where
    type Sv CreateMultipartUpload = S3
    type Rs CreateMultipartUpload = CreateMultipartUploadOutput

    request  = post
    response = const . xmlResponse $ \h x ->
        <$> x %| "Bucket"
        <*> x %| "Key"
        <*> h ~: "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~: "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~: "x-amz-server-side-encryption"
        <*> x %| "UploadId"
