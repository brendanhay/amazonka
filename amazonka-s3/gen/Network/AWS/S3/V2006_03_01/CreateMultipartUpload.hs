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
    -- ** Request constructor
    , createMultipartUpload
    -- ** Request lenses
    , cmusBucket
    , cmusKey
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
    , cmusMetadata
    , cmusACL
    , cmusSSECustomerAlgorithm
    , cmusSSECustomerKey
    , cmusSSECustomerKeyMD5
    , cmusServerSideEncryption
    , cmusStorageClass
    , cmusWebsiteRedirectLocation

    -- * Response
    , CreateMultipartUploadResponse
    -- ** Response lenses
    , cmupBucket
    , cmupUploadId
    , cmupKey
    , cmupSSECustomerAlgorithm
    , cmupSSECustomerKeyMD5
    , cmupServerSideEncryption
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type InitiateMultipartUpload = CreateMultipartUpload

-- | Minimum specification for a 'CreateMultipartUpload' request.
createMultipartUpload :: BucketName -- ^ 'cmusBucket'
                      -> ObjectKey -- ^ 'cmusKey'
                      -> CreateMultipartUpload
createMultipartUpload p1 p2 = CreateMultipartUpload
    { _cmusBucket = p1
    , _cmusKey = p2
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
    , _cmusMetadata = mempty
    , _cmusACL = Nothing
    , _cmusSSECustomerAlgorithm = Nothing
    , _cmusSSECustomerKey = Nothing
    , _cmusSSECustomerKeyMD5 = Nothing
    , _cmusServerSideEncryption = Nothing
    , _cmusStorageClass = Nothing
    , _cmusWebsiteRedirectLocation = Nothing
    }
{-# INLINE createMultipartUpload #-}

data CreateMultipartUpload = CreateMultipartUpload
    { _cmusBucket :: BucketName
    , _cmusKey :: ObjectKey
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
    , _cmusMetadata :: Map Text Text
      -- ^ A map of metadata to store with the object in S3.
    , _cmusACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
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
    } deriving (Show, Generic)

cmusBucket :: Lens' CreateMultipartUpload (BucketName)
cmusBucket f x =
    f (_cmusBucket x)
        <&> \y -> x { _cmusBucket = y }
{-# INLINE cmusBucket #-}

cmusKey :: Lens' CreateMultipartUpload (ObjectKey)
cmusKey f x =
    f (_cmusKey x)
        <&> \y -> x { _cmusKey = y }
{-# INLINE cmusKey #-}

-- | Specifies caching behavior along the request/reply chain.
cmusCacheControl :: Lens' CreateMultipartUpload (Maybe Text)
cmusCacheControl f x =
    f (_cmusCacheControl x)
        <&> \y -> x { _cmusCacheControl = y }
{-# INLINE cmusCacheControl #-}

-- | Specifies presentational information for the object.
cmusContentDisposition :: Lens' CreateMultipartUpload (Maybe Text)
cmusContentDisposition f x =
    f (_cmusContentDisposition x)
        <&> \y -> x { _cmusContentDisposition = y }
{-# INLINE cmusContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
cmusContentEncoding :: Lens' CreateMultipartUpload (Maybe Text)
cmusContentEncoding f x =
    f (_cmusContentEncoding x)
        <&> \y -> x { _cmusContentEncoding = y }
{-# INLINE cmusContentEncoding #-}

-- | The language the content is in.
cmusContentLanguage :: Lens' CreateMultipartUpload (Maybe Text)
cmusContentLanguage f x =
    f (_cmusContentLanguage x)
        <&> \y -> x { _cmusContentLanguage = y }
{-# INLINE cmusContentLanguage #-}

-- | A standard MIME type describing the format of the object data.
cmusContentType :: Lens' CreateMultipartUpload (Maybe Text)
cmusContentType f x =
    f (_cmusContentType x)
        <&> \y -> x { _cmusContentType = y }
{-# INLINE cmusContentType #-}

-- | The date and time at which the object is no longer cacheable.
cmusExpires :: Lens' CreateMultipartUpload (Maybe RFC822)
cmusExpires f x =
    f (_cmusExpires x)
        <&> \y -> x { _cmusExpires = y }
{-# INLINE cmusExpires #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
cmusGrantFullControl :: Lens' CreateMultipartUpload (Maybe Text)
cmusGrantFullControl f x =
    f (_cmusGrantFullControl x)
        <&> \y -> x { _cmusGrantFullControl = y }
{-# INLINE cmusGrantFullControl #-}

-- | Allows grantee to read the object data and its metadata.
cmusGrantRead :: Lens' CreateMultipartUpload (Maybe Text)
cmusGrantRead f x =
    f (_cmusGrantRead x)
        <&> \y -> x { _cmusGrantRead = y }
{-# INLINE cmusGrantRead #-}

-- | Allows grantee to read the object ACL.
cmusGrantReadACP :: Lens' CreateMultipartUpload (Maybe Text)
cmusGrantReadACP f x =
    f (_cmusGrantReadACP x)
        <&> \y -> x { _cmusGrantReadACP = y }
{-# INLINE cmusGrantReadACP #-}

-- | Allows grantee to write the ACL for the applicable object.
cmusGrantWriteACP :: Lens' CreateMultipartUpload (Maybe Text)
cmusGrantWriteACP f x =
    f (_cmusGrantWriteACP x)
        <&> \y -> x { _cmusGrantWriteACP = y }
{-# INLINE cmusGrantWriteACP #-}

-- | A map of metadata to store with the object in S3.
cmusMetadata :: Lens' CreateMultipartUpload (Map Text Text)
cmusMetadata f x =
    f (_cmusMetadata x)
        <&> \y -> x { _cmusMetadata = y }
{-# INLINE cmusMetadata #-}

-- | The canned ACL to apply to the object.
cmusACL :: Lens' CreateMultipartUpload (Maybe ObjectCannedACL)
cmusACL f x =
    f (_cmusACL x)
        <&> \y -> x { _cmusACL = y }
{-# INLINE cmusACL #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
cmusSSECustomerAlgorithm :: Lens' CreateMultipartUpload (Maybe Text)
cmusSSECustomerAlgorithm f x =
    f (_cmusSSECustomerAlgorithm x)
        <&> \y -> x { _cmusSSECustomerAlgorithm = y }
{-# INLINE cmusSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
cmusSSECustomerKey :: Lens' CreateMultipartUpload (Maybe Text)
cmusSSECustomerKey f x =
    f (_cmusSSECustomerKey x)
        <&> \y -> x { _cmusSSECustomerKey = y }
{-# INLINE cmusSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
cmusSSECustomerKeyMD5 :: Lens' CreateMultipartUpload (Maybe Text)
cmusSSECustomerKeyMD5 f x =
    f (_cmusSSECustomerKeyMD5 x)
        <&> \y -> x { _cmusSSECustomerKeyMD5 = y }
{-# INLINE cmusSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cmusServerSideEncryption :: Lens' CreateMultipartUpload (Maybe ServerSideEncryption)
cmusServerSideEncryption f x =
    f (_cmusServerSideEncryption x)
        <&> \y -> x { _cmusServerSideEncryption = y }
{-# INLINE cmusServerSideEncryption #-}

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
cmusStorageClass :: Lens' CreateMultipartUpload (Maybe StorageClass)
cmusStorageClass f x =
    f (_cmusStorageClass x)
        <&> \y -> x { _cmusStorageClass = y }
{-# INLINE cmusStorageClass #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
cmusWebsiteRedirectLocation :: Lens' CreateMultipartUpload (Maybe Text)
cmusWebsiteRedirectLocation f x =
    f (_cmusWebsiteRedirectLocation x)
        <&> \y -> x { _cmusWebsiteRedirectLocation = y }
{-# INLINE cmusWebsiteRedirectLocation #-}

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

instance ToHeaders CreateMultipartUpload where
    toHeaders CreateMultipartUpload{..} = concat
        [ "Cache-Control" =: _cmusCacheControl
        , "Content-Disposition" =: _cmusContentDisposition
        , "Content-Encoding" =: _cmusContentEncoding
        , "Content-Language" =: _cmusContentLanguage
        , "Content-Type" =: _cmusContentType
        , "Expires" =: _cmusExpires
        , "x-amz-grant-full-control" =: _cmusGrantFullControl
        , "x-amz-grant-read" =: _cmusGrantRead
        , "x-amz-grant-read-acp" =: _cmusGrantReadACP
        , "x-amz-grant-write-acp" =: _cmusGrantWriteACP
        , "x-amz-meta-" =: _cmusMetadata
        , "x-amz-acl" =: _cmusACL
        , "x-amz-server-side-encryption-customer-algorithm" =: _cmusSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _cmusSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _cmusSSECustomerKeyMD5
        , "x-amz-server-side-encryption" =: _cmusServerSideEncryption
        , "x-amz-storage-class" =: _cmusStorageClass
        , "x-amz-website-redirect-location" =: _cmusWebsiteRedirectLocation
        ]

instance ToBody CreateMultipartUpload

data CreateMultipartUploadResponse = CreateMultipartUploadResponse
    { _cmupBucket :: Maybe BucketName
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , _cmupUploadId :: Maybe Text
      -- ^ ID for the initiated multipart upload.
    , _cmupKey :: Maybe ObjectKey
      -- ^ Object key for which the multipart upload was initiated.
    , _cmupSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _cmupSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , _cmupServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Show, Generic)

-- | Name of the bucket to which the multipart upload was initiated.
cmupBucket :: Lens' CreateMultipartUploadResponse (Maybe BucketName)
cmupBucket f x =
    f (_cmupBucket x)
        <&> \y -> x { _cmupBucket = y }
{-# INLINE cmupBucket #-}

-- | ID for the initiated multipart upload.
cmupUploadId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmupUploadId f x =
    f (_cmupUploadId x)
        <&> \y -> x { _cmupUploadId = y }
{-# INLINE cmupUploadId #-}

-- | Object key for which the multipart upload was initiated.
cmupKey :: Lens' CreateMultipartUploadResponse (Maybe ObjectKey)
cmupKey f x =
    f (_cmupKey x)
        <&> \y -> x { _cmupKey = y }
{-# INLINE cmupKey #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
cmupSSECustomerAlgorithm :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmupSSECustomerAlgorithm f x =
    f (_cmupSSECustomerAlgorithm x)
        <&> \y -> x { _cmupSSECustomerAlgorithm = y }
{-# INLINE cmupSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cmupSSECustomerKeyMD5 :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmupSSECustomerKeyMD5 f x =
    f (_cmupSSECustomerKeyMD5 x)
        <&> \y -> x { _cmupSSECustomerKeyMD5 = y }
{-# INLINE cmupSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cmupServerSideEncryption :: Lens' CreateMultipartUploadResponse (Maybe ServerSideEncryption)
cmupServerSideEncryption f x =
    f (_cmupServerSideEncryption x)
        <&> \y -> x { _cmupServerSideEncryption = y }
{-# INLINE cmupServerSideEncryption #-}

instance AWSRequest CreateMultipartUpload where
    type Sv CreateMultipartUpload = S3
    type Rs CreateMultipartUpload = CreateMultipartUploadResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateMultipartUploadResponse
            <*> xml %|? "BucketName"
            <*> xml %|? "MultipartUploadId"
            <*> xml %|? "ObjectKey"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
            <*> hs ~:? "x-amz-server-side-encryption"
