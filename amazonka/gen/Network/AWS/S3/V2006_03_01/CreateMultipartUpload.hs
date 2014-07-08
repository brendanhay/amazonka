{-# LANGUAGE DeriveGeneric               #-}
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

import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

type InitiateMultipartUpload = CreateMultipartUpload
type InitiateMultipartUploadResponse = Rs CreateMultipartUpload

-- | Default CreateMultipartUpload request.
createMultipartUpload :: BucketName -- ^ 'cmurBucket'
                      -> ObjectKey -- ^ 'cmurKey'
                      -> CreateMultipartUpload
createMultipartUpload p1 p2 = CreateMultipartUpload
    { cmurBucket = p1
    , cmurKey = p2
    , cmurMetadata = mempty
    , cmurCacheControl = Nothing
    , cmurContentDisposition = Nothing
    , cmurContentEncoding = Nothing
    , cmurContentLanguage = Nothing
    , cmurContentType = Nothing
    , cmurExpires = Nothing
    , cmurGrantFullControl = Nothing
    , cmurGrantRead = Nothing
    , cmurGrantReadACP = Nothing
    , cmurGrantWriteACP = Nothing
    , cmurACL = Nothing
    , cmurSSECustomerAlgorithm = Nothing
    , cmurSSECustomerKey = Nothing
    , cmurSSECustomerKeyMD5 = Nothing
    , cmurServerSideEncryption = Nothing
    , cmurStorageClass = Nothing
    , cmurWebsiteRedirectLocation = Nothing
    }

data CreateMultipartUpload = CreateMultipartUpload
    { cmurBucket :: BucketName
    , cmurKey :: ObjectKey
    , cmurMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , cmurCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , cmurContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , cmurContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object
      -- and thus what decoding mechanisms must be applied to obtain the
      -- media-type referenced by the Content-Type header field.
    , cmurContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , cmurContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , cmurExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , cmurGrantFullControl :: Maybe Text
      -- ^ Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on
      -- the object.
    , cmurGrantRead :: Maybe Text
      -- ^ Allows grantee to read the object data and its metadata.
    , cmurGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the object ACL.
    , cmurGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable object.
    , cmurACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    , cmurSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , cmurSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header.
    , cmurSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , cmurServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , cmurStorageClass :: Maybe StorageClass
      -- ^ The type of storage to use for the object. Defaults to
      -- 'STANDARD'.
    , cmurWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    } deriving (Eq, Show, Generic)

instance ToPath CreateMultipartUpload where
    toPath CreateMultipartUpload{..} = mconcat
        [ "/"
        , toBS cmurBucket
        , "/"
        , toBS cmurKey
        ]

instance ToQuery CreateMultipartUpload

instance ToHeaders CreateMultipartUpload where
    toHeaders CreateMultipartUpload{..} = concat
        [ "x-amz-meta-" =: cmurMetadata
        , "Cache-Control" =: cmurCacheControl
        , "Content-Disposition" =: cmurContentDisposition
        , "Content-Encoding" =: cmurContentEncoding
        , "Content-Language" =: cmurContentLanguage
        , "Content-Type" =: cmurContentType
        , "Expires" =: cmurExpires
        , "x-amz-grant-full-control" =: cmurGrantFullControl
        , "x-amz-grant-read" =: cmurGrantRead
        , "x-amz-grant-read-acp" =: cmurGrantReadACP
        , "x-amz-grant-write-acp" =: cmurGrantWriteACP
        , "x-amz-acl" =: cmurACL
        , "x-amz-server-side-encryption-customer-algorithm" =: cmurSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: cmurSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: cmurSSECustomerKeyMD5
        , "x-amz-server-side-encryption" =: cmurServerSideEncryption
        , "x-amz-storage-class" =: cmurStorageClass
        , "x-amz-website-redirect-location" =: cmurWebsiteRedirectLocation
        ]

instance ToBody CreateMultipartUpload

instance AWSRequest CreateMultipartUpload where
    type Sv CreateMultipartUpload = S3

    request  = post
    response = response' $ \

data instance Rs CreateMultipartUpload = CreateMultipartUploadResponse
    { cmuoBucket :: Maybe BucketName
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , cmuoUploadId :: Maybe Text
      -- ^ ID for the initiated multipart upload.
    , cmuoKey :: Maybe ObjectKey
      -- ^ Object key for which the multipart upload was initiated.
    , cmuoSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , cmuoSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , cmuoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Eq, Show, Generic)
