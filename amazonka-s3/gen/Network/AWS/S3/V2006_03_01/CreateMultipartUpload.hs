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

cmusBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusBucket f x =
    (\y -> x { _cmusBucket = y })
       <$> f (_cmusBucket x)
{-# INLINE cmusBucket #-}

cmusKey
    :: Functor f
    => (ObjectKey
    -> f (ObjectKey))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusKey f x =
    (\y -> x { _cmusKey = y })
       <$> f (_cmusKey x)
{-# INLINE cmusKey #-}

-- | Specifies caching behavior along the request/reply chain.
cmusCacheControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusCacheControl f x =
    (\y -> x { _cmusCacheControl = y })
       <$> f (_cmusCacheControl x)
{-# INLINE cmusCacheControl #-}

-- | Specifies presentational information for the object.
cmusContentDisposition
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusContentDisposition f x =
    (\y -> x { _cmusContentDisposition = y })
       <$> f (_cmusContentDisposition x)
{-# INLINE cmusContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
cmusContentEncoding
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusContentEncoding f x =
    (\y -> x { _cmusContentEncoding = y })
       <$> f (_cmusContentEncoding x)
{-# INLINE cmusContentEncoding #-}

-- | The language the content is in.
cmusContentLanguage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusContentLanguage f x =
    (\y -> x { _cmusContentLanguage = y })
       <$> f (_cmusContentLanguage x)
{-# INLINE cmusContentLanguage #-}

-- | A standard MIME type describing the format of the object data.
cmusContentType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusContentType f x =
    (\y -> x { _cmusContentType = y })
       <$> f (_cmusContentType x)
{-# INLINE cmusContentType #-}

-- | The date and time at which the object is no longer cacheable.
cmusExpires
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusExpires f x =
    (\y -> x { _cmusExpires = y })
       <$> f (_cmusExpires x)
{-# INLINE cmusExpires #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
cmusGrantFullControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusGrantFullControl f x =
    (\y -> x { _cmusGrantFullControl = y })
       <$> f (_cmusGrantFullControl x)
{-# INLINE cmusGrantFullControl #-}

-- | Allows grantee to read the object data and its metadata.
cmusGrantRead
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusGrantRead f x =
    (\y -> x { _cmusGrantRead = y })
       <$> f (_cmusGrantRead x)
{-# INLINE cmusGrantRead #-}

-- | Allows grantee to read the object ACL.
cmusGrantReadACP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusGrantReadACP f x =
    (\y -> x { _cmusGrantReadACP = y })
       <$> f (_cmusGrantReadACP x)
{-# INLINE cmusGrantReadACP #-}

-- | Allows grantee to write the ACL for the applicable object.
cmusGrantWriteACP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusGrantWriteACP f x =
    (\y -> x { _cmusGrantWriteACP = y })
       <$> f (_cmusGrantWriteACP x)
{-# INLINE cmusGrantWriteACP #-}

-- | A map of metadata to store with the object in S3.
cmusMetadata
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusMetadata f x =
    (\y -> x { _cmusMetadata = y })
       <$> f (_cmusMetadata x)
{-# INLINE cmusMetadata #-}

-- | The canned ACL to apply to the object.
cmusACL
    :: Functor f
    => (Maybe ObjectCannedACL
    -> f (Maybe ObjectCannedACL))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusACL f x =
    (\y -> x { _cmusACL = y })
       <$> f (_cmusACL x)
{-# INLINE cmusACL #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
cmusSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusSSECustomerAlgorithm f x =
    (\y -> x { _cmusSSECustomerAlgorithm = y })
       <$> f (_cmusSSECustomerAlgorithm x)
{-# INLINE cmusSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
cmusSSECustomerKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusSSECustomerKey f x =
    (\y -> x { _cmusSSECustomerKey = y })
       <$> f (_cmusSSECustomerKey x)
{-# INLINE cmusSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
cmusSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusSSECustomerKeyMD5 f x =
    (\y -> x { _cmusSSECustomerKeyMD5 = y })
       <$> f (_cmusSSECustomerKeyMD5 x)
{-# INLINE cmusSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cmusServerSideEncryption
    :: Functor f
    => (Maybe ServerSideEncryption
    -> f (Maybe ServerSideEncryption))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusServerSideEncryption f x =
    (\y -> x { _cmusServerSideEncryption = y })
       <$> f (_cmusServerSideEncryption x)
{-# INLINE cmusServerSideEncryption #-}

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
cmusStorageClass
    :: Functor f
    => (Maybe StorageClass
    -> f (Maybe StorageClass))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusStorageClass f x =
    (\y -> x { _cmusStorageClass = y })
       <$> f (_cmusStorageClass x)
{-# INLINE cmusStorageClass #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
cmusWebsiteRedirectLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUpload
    -> f CreateMultipartUpload
cmusWebsiteRedirectLocation f x =
    (\y -> x { _cmusWebsiteRedirectLocation = y })
       <$> f (_cmusWebsiteRedirectLocation x)
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
cmupBucket
    :: Functor f
    => (Maybe BucketName
    -> f (Maybe BucketName))
    -> CreateMultipartUploadResponse
    -> f CreateMultipartUploadResponse
cmupBucket f x =
    (\y -> x { _cmupBucket = y })
       <$> f (_cmupBucket x)
{-# INLINE cmupBucket #-}

-- | ID for the initiated multipart upload.
cmupUploadId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUploadResponse
    -> f CreateMultipartUploadResponse
cmupUploadId f x =
    (\y -> x { _cmupUploadId = y })
       <$> f (_cmupUploadId x)
{-# INLINE cmupUploadId #-}

-- | Object key for which the multipart upload was initiated.
cmupKey
    :: Functor f
    => (Maybe ObjectKey
    -> f (Maybe ObjectKey))
    -> CreateMultipartUploadResponse
    -> f CreateMultipartUploadResponse
cmupKey f x =
    (\y -> x { _cmupKey = y })
       <$> f (_cmupKey x)
{-# INLINE cmupKey #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
cmupSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUploadResponse
    -> f CreateMultipartUploadResponse
cmupSSECustomerAlgorithm f x =
    (\y -> x { _cmupSSECustomerAlgorithm = y })
       <$> f (_cmupSSECustomerAlgorithm x)
{-# INLINE cmupSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cmupSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateMultipartUploadResponse
    -> f CreateMultipartUploadResponse
cmupSSECustomerKeyMD5 f x =
    (\y -> x { _cmupSSECustomerKeyMD5 = y })
       <$> f (_cmupSSECustomerKeyMD5 x)
{-# INLINE cmupSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cmupServerSideEncryption
    :: Functor f
    => (Maybe ServerSideEncryption
    -> f (Maybe ServerSideEncryption))
    -> CreateMultipartUploadResponse
    -> f CreateMultipartUploadResponse
cmupServerSideEncryption f x =
    (\y -> x { _cmupServerSideEncryption = y })
       <$> f (_cmupServerSideEncryption x)
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
