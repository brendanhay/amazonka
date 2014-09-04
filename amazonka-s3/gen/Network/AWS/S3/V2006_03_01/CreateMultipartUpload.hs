{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.CreateMultipartUpload
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
module Network.AWS.S3.V2006_03_01.CreateMultipartUpload
    (
    -- * Request
      CreateMultipartUpload
    -- ** Request alias
    , InitiateMultipartUpload
    -- ** Request constructor
    , mkCreateMultipartUploadRequest
    -- ** Request lenses
    , cmusACL
    , cmusBucket
    , cmusCacheControl
    , cmusContentDisposition
    , cmusContentEncoding
    , cmusContentLanguage
    , cmusContentType
    , cmusExpires
    , cmusGrantFullControl
    , cmusGrantRead
    , cmusGrantReadACP
    , cmusGrantWriteACP
    , cmusKey
    , cmusMetadata
    , cmusServerSideEncryption
    , cmusStorageClass
    , cmusWebsiteRedirectLocation
    , cmusSSECustomerAlgorithm
    , cmusSSECustomerKey
    , cmusSSECustomerKeyMD5

    -- * Response
    , CreateMultipartUploadResponse
    -- ** Response lenses
    , cmupBucket
    , cmupKey
    , cmupUploadId
    , cmupServerSideEncryption
    , cmupSSECustomerAlgorithm
    , cmupSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type InitiateMultipartUpload = CreateMultipartUpload

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateMultipartUpload' request.
mkCreateMultipartUploadRequest :: BucketName -- ^ 'cmusBucket'
                               -> ObjectKey -- ^ 'cmusKey'
                               -> CreateMultipartUpload
mkCreateMultipartUploadRequest p1 p2 = CreateMultipartUpload
    { _cmusACL = Nothing
    , _cmusBucket = p2
    , _cmusCacheControl = Nothing
    , _cmusContentDisposition = Nothing
    , _cmusContentEncoding = Nothing
    , _cmusContentLanguage = Nothing
    , _cmusContentType = Nothing
    , _cmusExpires = Nothing
    , _cmusGrantFullControl = Nothing
    , _cmusGrantRead = Nothing
    , _cmusGrantReadACP = Nothing
    , _cmusGrantWriteACP = Nothing
    , _cmusKey = p13
    , _cmusMetadata = mempty
    , _cmusServerSideEncryption = Nothing
    , _cmusStorageClass = Nothing
    , _cmusWebsiteRedirectLocation = Nothing
    , _cmusSSECustomerAlgorithm = Nothing
    , _cmusSSECustomerKey = Nothing
    , _cmusSSECustomerKeyMD5 = Nothing
    }
{-# INLINE mkCreateMultipartUploadRequest #-}

data CreateMultipartUpload = CreateMultipartUpload
    { _cmusACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    , _cmusBucket :: BucketName
    , _cmusCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , _cmusContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , _cmusContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object
      -- and thus what decoding mechanisms must be applied to obtain the
      -- media-type referenced by the Content-Type header field.
    , _cmusContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , _cmusContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , _cmusExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , _cmusGrantFullControl :: Maybe Text
      -- ^ Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on
      -- the object.
    , _cmusGrantRead :: Maybe Text
      -- ^ Allows grantee to read the object data and its metadata.
    , _cmusGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the object ACL.
    , _cmusGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable object.
    , _cmusKey :: ObjectKey
    , _cmusMetadata :: Map Text Text
      -- ^ A map of metadata to store with the object in S3.
    , _cmusServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _cmusStorageClass :: Maybe StorageClass
      -- ^ The type of storage to use for the object. Defaults to
      -- 'STANDARD'.
    , _cmusWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    , _cmusSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , _cmusSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header.
    , _cmusSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    } deriving (Show, Generic)

-- | The canned ACL to apply to the object.
cmusACL :: Lens' CreateMultipartUpload (Maybe ObjectCannedACL)
cmusACL = lens _cmusACL (\s a -> s { _cmusACL = a })
{-# INLINE cmusACL #-}

cmusBucket :: Lens' CreateMultipartUpload (BucketName)
cmusBucket = lens _cmusBucket (\s a -> s { _cmusBucket = a })
{-# INLINE cmusBucket #-}

-- | Specifies caching behavior along the request/reply chain.
cmusCacheControl :: Lens' CreateMultipartUpload (Maybe Text)
cmusCacheControl = lens _cmusCacheControl (\s a -> s { _cmusCacheControl = a })
{-# INLINE cmusCacheControl #-}

-- | Specifies presentational information for the object.
cmusContentDisposition :: Lens' CreateMultipartUpload (Maybe Text)
cmusContentDisposition = lens _cmusContentDisposition (\s a -> s { _cmusContentDisposition = a })
{-# INLINE cmusContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
cmusContentEncoding :: Lens' CreateMultipartUpload (Maybe Text)
cmusContentEncoding = lens _cmusContentEncoding (\s a -> s { _cmusContentEncoding = a })
{-# INLINE cmusContentEncoding #-}

-- | The language the content is in.
cmusContentLanguage :: Lens' CreateMultipartUpload (Maybe Text)
cmusContentLanguage = lens _cmusContentLanguage (\s a -> s { _cmusContentLanguage = a })
{-# INLINE cmusContentLanguage #-}

-- | A standard MIME type describing the format of the object data.
cmusContentType :: Lens' CreateMultipartUpload (Maybe Text)
cmusContentType = lens _cmusContentType (\s a -> s { _cmusContentType = a })
{-# INLINE cmusContentType #-}

-- | The date and time at which the object is no longer cacheable.
cmusExpires :: Lens' CreateMultipartUpload (Maybe RFC822)
cmusExpires = lens _cmusExpires (\s a -> s { _cmusExpires = a })
{-# INLINE cmusExpires #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
cmusGrantFullControl :: Lens' CreateMultipartUpload (Maybe Text)
cmusGrantFullControl = lens _cmusGrantFullControl (\s a -> s { _cmusGrantFullControl = a })
{-# INLINE cmusGrantFullControl #-}

-- | Allows grantee to read the object data and its metadata.
cmusGrantRead :: Lens' CreateMultipartUpload (Maybe Text)
cmusGrantRead = lens _cmusGrantRead (\s a -> s { _cmusGrantRead = a })
{-# INLINE cmusGrantRead #-}

-- | Allows grantee to read the object ACL.
cmusGrantReadACP :: Lens' CreateMultipartUpload (Maybe Text)
cmusGrantReadACP = lens _cmusGrantReadACP (\s a -> s { _cmusGrantReadACP = a })
{-# INLINE cmusGrantReadACP #-}

-- | Allows grantee to write the ACL for the applicable object.
cmusGrantWriteACP :: Lens' CreateMultipartUpload (Maybe Text)
cmusGrantWriteACP = lens _cmusGrantWriteACP (\s a -> s { _cmusGrantWriteACP = a })
{-# INLINE cmusGrantWriteACP #-}

cmusKey :: Lens' CreateMultipartUpload (ObjectKey)
cmusKey = lens _cmusKey (\s a -> s { _cmusKey = a })
{-# INLINE cmusKey #-}

-- | A map of metadata to store with the object in S3.
cmusMetadata :: Lens' CreateMultipartUpload (Map Text Text)
cmusMetadata = lens _cmusMetadata (\s a -> s { _cmusMetadata = a })
{-# INLINE cmusMetadata #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cmusServerSideEncryption :: Lens' CreateMultipartUpload (Maybe ServerSideEncryption)
cmusServerSideEncryption = lens _cmusServerSideEncryption (\s a -> s { _cmusServerSideEncryption = a })
{-# INLINE cmusServerSideEncryption #-}

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
cmusStorageClass :: Lens' CreateMultipartUpload (Maybe StorageClass)
cmusStorageClass = lens _cmusStorageClass (\s a -> s { _cmusStorageClass = a })
{-# INLINE cmusStorageClass #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
cmusWebsiteRedirectLocation :: Lens' CreateMultipartUpload (Maybe Text)
cmusWebsiteRedirectLocation = lens _cmusWebsiteRedirectLocation (\s a -> s { _cmusWebsiteRedirectLocation = a })
{-# INLINE cmusWebsiteRedirectLocation #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
cmusSSECustomerAlgorithm :: Lens' CreateMultipartUpload (Maybe Text)
cmusSSECustomerAlgorithm = lens _cmusSSECustomerAlgorithm (\s a -> s { _cmusSSECustomerAlgorithm = a })
{-# INLINE cmusSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
cmusSSECustomerKey :: Lens' CreateMultipartUpload (Maybe Text)
cmusSSECustomerKey = lens _cmusSSECustomerKey (\s a -> s { _cmusSSECustomerKey = a })
{-# INLINE cmusSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
cmusSSECustomerKeyMD5 :: Lens' CreateMultipartUpload (Maybe Text)
cmusSSECustomerKeyMD5 = lens _cmusSSECustomerKeyMD5 (\s a -> s { _cmusSSECustomerKeyMD5 = a })
{-# INLINE cmusSSECustomerKeyMD5 #-}

instance ToPath CreateMultipartUpload where
    toPath CreateMultipartUpload{..} = mconcat
        [ "/"
        , toBS _cmusBucket
        , "/"
        , toBS _cmusKey
        ]

instance ToQuery CreateMultipartUpload where
    toQuery CreateMultipartUpload{..} = mconcat
        [ "uploads"
        ]

instance ToHeaders CreateMultipartUpload

instance ToBody CreateMultipartUpload

data CreateMultipartUploadResponse = CreateMultipartUploadResponse
    { _cmupBucket :: Maybe BucketName
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , _cmupKey :: Maybe ObjectKey
      -- ^ Object key for which the multipart upload was initiated.
    , _cmupUploadId :: Maybe Text
      -- ^ ID for the initiated multipart upload.
    , _cmupServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _cmupSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _cmupSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    } deriving (Show, Generic)

-- | Name of the bucket to which the multipart upload was initiated.
cmupBucket :: Lens' CreateMultipartUploadResponse (Maybe BucketName)
cmupBucket = lens _cmupBucket (\s a -> s { _cmupBucket = a })
{-# INLINE cmupBucket #-}

-- | Object key for which the multipart upload was initiated.
cmupKey :: Lens' CreateMultipartUploadResponse (Maybe ObjectKey)
cmupKey = lens _cmupKey (\s a -> s { _cmupKey = a })
{-# INLINE cmupKey #-}

-- | ID for the initiated multipart upload.
cmupUploadId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmupUploadId = lens _cmupUploadId (\s a -> s { _cmupUploadId = a })
{-# INLINE cmupUploadId #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cmupServerSideEncryption :: Lens' CreateMultipartUploadResponse (Maybe ServerSideEncryption)
cmupServerSideEncryption = lens _cmupServerSideEncryption (\s a -> s { _cmupServerSideEncryption = a })
{-# INLINE cmupServerSideEncryption #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
cmupSSECustomerAlgorithm :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmupSSECustomerAlgorithm = lens _cmupSSECustomerAlgorithm (\s a -> s { _cmupSSECustomerAlgorithm = a })
{-# INLINE cmupSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cmupSSECustomerKeyMD5 :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmupSSECustomerKeyMD5 = lens _cmupSSECustomerKeyMD5 (\s a -> s { _cmupSSECustomerKeyMD5 = a })
{-# INLINE cmupSSECustomerKeyMD5 #-}

instance AWSRequest CreateMultipartUpload where
    type Sv CreateMultipartUpload = S3
    type Rs CreateMultipartUpload = CreateMultipartUploadResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateMultipartUploadResponse
            <*> xml %|? "BucketName"
            <*> xml %|? "ObjectKey"
            <*> xml %|? "MultipartUploadId"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
