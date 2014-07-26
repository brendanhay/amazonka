{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.S3.V2006_03_01.CreateMultipartUpload where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

type InitiateMultipartUpload = CreateMultipartUpload
True

-- | Default CreateMultipartUpload request.
createMultipartUpload :: BucketName -- ^ '_cmurBucket'
                      -> ObjectKey -- ^ '_cmurKey'
                      -> CreateMultipartUpload
createMultipartUpload p1 p2 = CreateMultipartUpload
    { _cmurBucket = p1
    , _cmurKey = p2
    , _cmurMetadata = mempty
    , _cmurCacheControl = Nothing
    , _cmurContentDisposition = Nothing
    , _cmurContentEncoding = Nothing
    , _cmurContentLanguage = Nothing
    , _cmurContentType = Nothing
    , _cmurExpires = Nothing
    , _cmurGrantFullControl = Nothing
    , _cmurGrantRead = Nothing
    , _cmurGrantReadACP = Nothing
    , _cmurGrantWriteACP = Nothing
    , _cmurACL = Nothing
    , _cmurSSECustomerAlgorithm = Nothing
    , _cmurSSECustomerKey = Nothing
    , _cmurSSECustomerKeyMD5 = Nothing
    , _cmurServerSideEncryption = Nothing
    , _cmurStorageClass = Nothing
    , _cmurWebsiteRedirectLocation = Nothing
    }

data CreateMultipartUpload = CreateMultipartUpload
    { _cmurBucket :: BucketName
    , _cmurKey :: ObjectKey
    , _cmurMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , _cmurCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , _cmurContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , _cmurContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object
      -- and thus what decoding mechanisms must be applied to obtain the
      -- media-type referenced by the Content-Type header field.
    , _cmurContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , _cmurContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , _cmurExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , _cmurGrantFullControl :: Maybe Text
      -- ^ Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on
      -- the object.
    , _cmurGrantRead :: Maybe Text
      -- ^ Allows grantee to read the object data and its metadata.
    , _cmurGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the object ACL.
    , _cmurGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable object.
    , _cmurACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    , _cmurSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , _cmurSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header.
    , _cmurSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , _cmurServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _cmurStorageClass :: Maybe StorageClass
      -- ^ The type of storage to use for the object. Defaults to
      -- 'STANDARD'.
    , _cmurWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    } deriving (Generic)

instance ToPath CreateMultipartUpload where
    toPath CreateMultipartUpload{..} = mconcat
        [ "/"
        , toBS _cmurBucket
        , "/"
        , toBS _cmurKey
        ]

instance ToQuery CreateMultipartUpload

instance ToHeaders CreateMultipartUpload where
    toHeaders CreateMultipartUpload{..} = concat
        [ "x-amz-meta-" =: _cmurMetadata
        , "Cache-Control" =: _cmurCacheControl
        , "Content-Disposition" =: _cmurContentDisposition
        , "Content-Encoding" =: _cmurContentEncoding
        , "Content-Language" =: _cmurContentLanguage
        , "Content-Type" =: _cmurContentType
        , "Expires" =: _cmurExpires
        , "x-amz-grant-full-control" =: _cmurGrantFullControl
        , "x-amz-grant-read" =: _cmurGrantRead
        , "x-amz-grant-read-acp" =: _cmurGrantReadACP
        , "x-amz-grant-write-acp" =: _cmurGrantWriteACP
        , "x-amz-acl" =: _cmurACL
        , "x-amz-server-side-encryption-customer-algorithm" =: _cmurSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _cmurSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _cmurSSECustomerKeyMD5
        , "x-amz-server-side-encryption" =: _cmurServerSideEncryption
        , "x-amz-storage-class" =: _cmurStorageClass
        , "x-amz-website-redirect-location" =: _cmurWebsiteRedirectLocation
        ]

instance ToBody CreateMultipartUpload

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

data CreateMultipartUploadResponse = CreateMultipartUploadResponse
    { _cmuoBucket :: Maybe BucketName
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , _cmuoUploadId :: Maybe Text
      -- ^ ID for the initiated multipart upload.
    , _cmuoKey :: Maybe ObjectKey
      -- ^ Object key for which the multipart upload was initiated.
    , _cmuoSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _cmuoSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , _cmuoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Generic)
