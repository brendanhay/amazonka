{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
module Network.AWS.S3.CreateMultipartUpload
    (
    -- * Request
      CreateMultipartUpload
    -- ** Request alias
    , InitiateMultipartUpload
    -- ** Request constructor
    , createMultipartUpload
    -- ** Request lenses
    , cmu2ACL
    , cmu2Bucket
    , cmu2CacheControl
    , cmu2ContentDisposition
    , cmu2ContentEncoding
    , cmu2ContentLanguage
    , cmu2ContentType
    , cmu2Expires
    , cmu2GrantFullControl
    , cmu2GrantRead
    , cmu2GrantReadACP
    , cmu2GrantWriteACP
    , cmu2Key
    , cmu2Metadata
    , cmu2ServerSideEncryption
    , cmu2StorageClass
    , cmu2WebsiteRedirectLocation
    , cmu2SSECustomerAlgorithm
    , cmu2SSECustomerKey
    , cmu2SSECustomerKeyMD5

    -- * Response
    , CreateMultipartUploadResponse
    -- ** Response constructor
    , createMultipartUploadResponse
    -- ** Response lenses
    , cmurrBucket
    , cmurrKey
    , cmurrUploadId
    , cmurrServerSideEncryption
    , cmurrSSECustomerAlgorithm
    , cmurrSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

type InitiateMultipartUpload = CreateMultipartUpload

data CreateMultipartUpload = CreateMultipartUpload
    { _cmu2ACL :: Maybe ObjectCannedACL
    , _cmu2Bucket :: BucketName
    , _cmu2CacheControl :: Maybe Text
    , _cmu2ContentDisposition :: Maybe Text
    , _cmu2ContentEncoding :: Maybe Text
    , _cmu2ContentLanguage :: Maybe Text
    , _cmu2ContentType :: Maybe Text
    , _cmu2Expires :: Maybe RFC822
    , _cmu2GrantFullControl :: Maybe Text
    , _cmu2GrantRead :: Maybe Text
    , _cmu2GrantReadACP :: Maybe Text
    , _cmu2GrantWriteACP :: Maybe Text
    , _cmu2Key :: ObjectKey
    , _cmu2Metadata :: Map Text Text
    , _cmu2ServerSideEncryption :: Maybe ServerSideEncryption
    , _cmu2StorageClass :: Maybe StorageClass
    , _cmu2WebsiteRedirectLocation :: Maybe Text
    , _cmu2SSECustomerAlgorithm :: Maybe Text
    , _cmu2SSECustomerKey :: Maybe Text
    , _cmu2SSECustomerKeyMD5 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateMultipartUpload' request.
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
createMultipartUpload :: ObjectKey -- ^ 'cmu2Key'
                      -> BucketName -- ^ 'cmu2Bucket'
                      -> CreateMultipartUpload
createMultipartUpload p13 p2 = CreateMultipartUpload
    { _cmu2ACL = Nothing
    , _cmu2Bucket = p2
    , _cmu2CacheControl = Nothing
    , _cmu2ContentDisposition = Nothing
    , _cmu2ContentEncoding = Nothing
    , _cmu2ContentLanguage = Nothing
    , _cmu2ContentType = Nothing
    , _cmu2Expires = Nothing
    , _cmu2GrantFullControl = Nothing
    , _cmu2GrantRead = Nothing
    , _cmu2GrantReadACP = Nothing
    , _cmu2GrantWriteACP = Nothing
    , _cmu2Key = p13
    , _cmu2Metadata = mempty
    , _cmu2ServerSideEncryption = Nothing
    , _cmu2StorageClass = Nothing
    , _cmu2WebsiteRedirectLocation = Nothing
    , _cmu2SSECustomerAlgorithm = Nothing
    , _cmu2SSECustomerKey = Nothing
    , _cmu2SSECustomerKeyMD5 = Nothing
    }

-- | The canned ACL to apply to the object.
cmu2ACL :: Lens' CreateMultipartUpload (Maybe ObjectCannedACL)
cmu2ACL = lens _cmu2ACL (\s a -> s { _cmu2ACL = a })

cmu2Bucket :: Lens' CreateMultipartUpload BucketName
cmu2Bucket = lens _cmu2Bucket (\s a -> s { _cmu2Bucket = a })

-- | Specifies caching behavior along the request/reply chain.
cmu2CacheControl :: Lens' CreateMultipartUpload (Maybe Text)
cmu2CacheControl =
    lens _cmu2CacheControl (\s a -> s { _cmu2CacheControl = a })

-- | Specifies presentational information for the object.
cmu2ContentDisposition :: Lens' CreateMultipartUpload (Maybe Text)
cmu2ContentDisposition =
    lens _cmu2ContentDisposition (\s a -> s { _cmu2ContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
cmu2ContentEncoding :: Lens' CreateMultipartUpload (Maybe Text)
cmu2ContentEncoding =
    lens _cmu2ContentEncoding (\s a -> s { _cmu2ContentEncoding = a })

-- | The language the content is in.
cmu2ContentLanguage :: Lens' CreateMultipartUpload (Maybe Text)
cmu2ContentLanguage =
    lens _cmu2ContentLanguage (\s a -> s { _cmu2ContentLanguage = a })

-- | A standard MIME type describing the format of the object data.
cmu2ContentType :: Lens' CreateMultipartUpload (Maybe Text)
cmu2ContentType = lens _cmu2ContentType (\s a -> s { _cmu2ContentType = a })

-- | The date and time at which the object is no longer cacheable.
cmu2Expires :: Lens' CreateMultipartUpload (Maybe RFC822)
cmu2Expires = lens _cmu2Expires (\s a -> s { _cmu2Expires = a })

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
cmu2GrantFullControl :: Lens' CreateMultipartUpload (Maybe Text)
cmu2GrantFullControl =
    lens _cmu2GrantFullControl (\s a -> s { _cmu2GrantFullControl = a })

-- | Allows grantee to read the object data and its metadata.
cmu2GrantRead :: Lens' CreateMultipartUpload (Maybe Text)
cmu2GrantRead = lens _cmu2GrantRead (\s a -> s { _cmu2GrantRead = a })

-- | Allows grantee to read the object ACL.
cmu2GrantReadACP :: Lens' CreateMultipartUpload (Maybe Text)
cmu2GrantReadACP =
    lens _cmu2GrantReadACP (\s a -> s { _cmu2GrantReadACP = a })

-- | Allows grantee to write the ACL for the applicable object.
cmu2GrantWriteACP :: Lens' CreateMultipartUpload (Maybe Text)
cmu2GrantWriteACP =
    lens _cmu2GrantWriteACP (\s a -> s { _cmu2GrantWriteACP = a })

cmu2Key :: Lens' CreateMultipartUpload ObjectKey
cmu2Key = lens _cmu2Key (\s a -> s { _cmu2Key = a })

-- | A map of metadata to store with the object in S3.
cmu2Metadata :: Lens' CreateMultipartUpload (Map Text Text)
cmu2Metadata = lens _cmu2Metadata (\s a -> s { _cmu2Metadata = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
cmu2ServerSideEncryption :: Lens' CreateMultipartUpload (Maybe ServerSideEncryption)
cmu2ServerSideEncryption =
    lens _cmu2ServerSideEncryption
         (\s a -> s { _cmu2ServerSideEncryption = a })

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
cmu2StorageClass :: Lens' CreateMultipartUpload (Maybe StorageClass)
cmu2StorageClass =
    lens _cmu2StorageClass (\s a -> s { _cmu2StorageClass = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
cmu2WebsiteRedirectLocation :: Lens' CreateMultipartUpload (Maybe Text)
cmu2WebsiteRedirectLocation =
    lens _cmu2WebsiteRedirectLocation
         (\s a -> s { _cmu2WebsiteRedirectLocation = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
cmu2SSECustomerAlgorithm :: Lens' CreateMultipartUpload (Maybe Text)
cmu2SSECustomerAlgorithm =
    lens _cmu2SSECustomerAlgorithm
         (\s a -> s { _cmu2SSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
cmu2SSECustomerKey :: Lens' CreateMultipartUpload (Maybe Text)
cmu2SSECustomerKey =
    lens _cmu2SSECustomerKey (\s a -> s { _cmu2SSECustomerKey = a })

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
cmu2SSECustomerKeyMD5 :: Lens' CreateMultipartUpload (Maybe Text)
cmu2SSECustomerKeyMD5 =
    lens _cmu2SSECustomerKeyMD5 (\s a -> s { _cmu2SSECustomerKeyMD5 = a })

instance ToPath CreateMultipartUpload

instance ToQuery CreateMultipartUpload

instance ToHeaders CreateMultipartUpload where
    toHeaders CreateMultipartUpload{..} = concat
        [ "x-amz-acl" =: _cmu2ACL
        , "Cache-Control" =: _cmu2CacheControl
        , "Content-Disposition" =: _cmu2ContentDisposition
        , "Content-Encoding" =: _cmu2ContentEncoding
        , "Content-Language" =: _cmu2ContentLanguage
        , "Content-Type" =: _cmu2ContentType
        , "Expires" =: _cmu2Expires
        , "x-amz-grant-full-control" =: _cmu2GrantFullControl
        , "x-amz-grant-read" =: _cmu2GrantRead
        , "x-amz-grant-read-acp" =: _cmu2GrantReadACP
        , "x-amz-grant-write-acp" =: _cmu2GrantWriteACP
        , "x-amz-meta-" =: _cmu2Metadata
        , "x-amz-server-side-encryption" =: _cmu2ServerSideEncryption
        , "x-amz-storage-class" =: _cmu2StorageClass
        , "x-amz-website-redirect-location" =: _cmu2WebsiteRedirectLocation
        , "x-amz-server-side-encryption-customer-algorithm" =: _cmu2SSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _cmu2SSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _cmu2SSECustomerKeyMD5
        ]

instance ToBody CreateMultipartUpload

data CreateMultipartUploadResponse = CreateMultipartUploadResponse
    { _cmurrBucket :: Maybe BucketName
    , _cmurrKey :: Maybe ObjectKey
    , _cmurrUploadId :: Maybe Text
    , _cmurrServerSideEncryption :: Maybe ServerSideEncryption
    , _cmurrSSECustomerAlgorithm :: Maybe Text
    , _cmurrSSECustomerKeyMD5 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateMultipartUploadResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @Maybe BucketName@
--
-- * @Key ::@ @Maybe ObjectKey@
--
-- * @UploadId ::@ @Maybe Text@
--
-- * @ServerSideEncryption ::@ @Maybe ServerSideEncryption@
--
-- * @SSECustomerAlgorithm ::@ @Maybe Text@
--
-- * @SSECustomerKeyMD5 ::@ @Maybe Text@
--
createMultipartUploadResponse :: CreateMultipartUploadResponse
createMultipartUploadResponse = CreateMultipartUploadResponse
    { _cmurrBucket = Nothing
    , _cmurrKey = Nothing
    , _cmurrUploadId = Nothing
    , _cmurrServerSideEncryption = Nothing
    , _cmurrSSECustomerAlgorithm = Nothing
    , _cmurrSSECustomerKeyMD5 = Nothing
    }

-- | Name of the bucket to which the multipart upload was initiated.
cmurrBucket :: Lens' CreateMultipartUploadResponse (Maybe BucketName)
cmurrBucket = lens _cmurrBucket (\s a -> s { _cmurrBucket = a })

-- | Object key for which the multipart upload was initiated.
cmurrKey :: Lens' CreateMultipartUploadResponse (Maybe ObjectKey)
cmurrKey = lens _cmurrKey (\s a -> s { _cmurrKey = a })

-- | ID for the initiated multipart upload.
cmurrUploadId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurrUploadId = lens _cmurrUploadId (\s a -> s { _cmurrUploadId = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
cmurrServerSideEncryption :: Lens' CreateMultipartUploadResponse (Maybe ServerSideEncryption)
cmurrServerSideEncryption =
    lens _cmurrServerSideEncryption
         (\s a -> s { _cmurrServerSideEncryption = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
cmurrSSECustomerAlgorithm :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurrSSECustomerAlgorithm =
    lens _cmurrSSECustomerAlgorithm
         (\s a -> s { _cmurrSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cmurrSSECustomerKeyMD5 :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurrSSECustomerKeyMD5 =
    lens _cmurrSSECustomerKeyMD5 (\s a -> s { _cmurrSSECustomerKeyMD5 = a })

instance AWSRequest CreateMultipartUpload where
    type Sv CreateMultipartUpload = S3
    type Rs CreateMultipartUpload = CreateMultipartUploadResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure CreateMultipartUploadResponse
            <*> xml %|? "BucketName"
            <*> xml %|? "ObjectKey"
            <*> xml %|? "MultipartUploadId"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
