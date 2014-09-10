{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Completes a multipart upload by assembling previously uploaded parts.
module Network.AWS.S3
    (
    -- * Request
      CompleteMultipartUpload
    -- ** Request constructor
    , mkCompleteMultipartUpload
    -- ** Request lenses
    , cmuBucket
    , cmuKey
    , cmuMultipartUpload
    , cmuUploadId

    -- * Response
    , CompleteMultipartUploadResponse
    -- ** Response constructor
    , mkCompleteMultipartUploadResponse
    -- ** Response lenses
    , cmurLocation
    , cmurBucket
    , cmurKey
    , cmurExpiration
    , cmurETag
    , cmurServerSideEncryption
    , cmurVersionId
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data CompleteMultipartUpload = CompleteMultipartUpload
    { _cmuBucket :: BucketName
    , _cmuKey :: ObjectKey
    , _cmuMultipartUpload :: Maybe CompletedMultipartUpload
    , _cmuUploadId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CompleteMultipartUpload' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @Key ::@ @ObjectKey@
--
-- * @MultipartUpload ::@ @Maybe CompletedMultipartUpload@
--
-- * @UploadId ::@ @Text@
--
mkCompleteMultipartUpload :: BucketName -- ^ 'cmuBucket'
                          -> ObjectKey -- ^ 'cmuKey'
                          -> Text -- ^ 'cmuUploadId'
                          -> CompleteMultipartUpload
mkCompleteMultipartUpload p1 p2 p4 = CompleteMultipartUpload
    { _cmuBucket = p1
    , _cmuKey = p2
    , _cmuMultipartUpload = Nothing
    , _cmuUploadId = p4
    }

cmuBucket :: Lens' CompleteMultipartUpload BucketName
cmuBucket = lens _cmuBucket (\s a -> s { _cmuBucket = a })

cmuKey :: Lens' CompleteMultipartUpload ObjectKey
cmuKey = lens _cmuKey (\s a -> s { _cmuKey = a })

cmuMultipartUpload :: Lens' CompleteMultipartUpload (Maybe CompletedMultipartUpload)
cmuMultipartUpload =
    lens _cmuMultipartUpload (\s a -> s { _cmuMultipartUpload = a })

cmuUploadId :: Lens' CompleteMultipartUpload Text
cmuUploadId = lens _cmuUploadId (\s a -> s { _cmuUploadId = a })

instance ToPath CompleteMultipartUpload

instance ToQuery CompleteMultipartUpload

instance ToHeaders CompleteMultipartUpload

instance ToBody CompleteMultipartUpload where
    toBody = toBody . encodeXML . _cmuMultipartUpload

data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse
    { _cmurLocation :: Maybe Text
    , _cmurBucket :: Maybe BucketName
    , _cmurKey :: Maybe ObjectKey
    , _cmurExpiration :: Maybe RFC822
    , _cmurETag :: Maybe ETag
    , _cmurServerSideEncryption :: Maybe ServerSideEncryption
    , _cmurVersionId :: Maybe ObjectVersionId
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CompleteMultipartUploadResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Location ::@ @Maybe Text@
--
-- * @Bucket ::@ @Maybe BucketName@
--
-- * @Key ::@ @Maybe ObjectKey@
--
-- * @Expiration ::@ @Maybe RFC822@
--
-- * @ETag ::@ @Maybe ETag@
--
-- * @ServerSideEncryption ::@ @Maybe ServerSideEncryption@
--
-- * @VersionId ::@ @Maybe ObjectVersionId@
--
mkCompleteMultipartUploadResponse :: CompleteMultipartUploadResponse
mkCompleteMultipartUploadResponse = CompleteMultipartUploadResponse
    { _cmurLocation = Nothing
    , _cmurBucket = Nothing
    , _cmurKey = Nothing
    , _cmurExpiration = Nothing
    , _cmurETag = Nothing
    , _cmurServerSideEncryption = Nothing
    , _cmurVersionId = Nothing
    }

cmurLocation :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cmurLocation = lens _cmurLocation (\s a -> s { _cmurLocation = a })

cmurBucket :: Lens' CompleteMultipartUploadResponse (Maybe BucketName)
cmurBucket = lens _cmurBucket (\s a -> s { _cmurBucket = a })

cmurKey :: Lens' CompleteMultipartUploadResponse (Maybe ObjectKey)
cmurKey = lens _cmurKey (\s a -> s { _cmurKey = a })

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
cmurExpiration :: Lens' CompleteMultipartUploadResponse (Maybe RFC822)
cmurExpiration = lens _cmurExpiration (\s a -> s { _cmurExpiration = a })

-- | Entity tag of the object.
cmurETag :: Lens' CompleteMultipartUploadResponse (Maybe ETag)
cmurETag = lens _cmurETag (\s a -> s { _cmurETag = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
cmurServerSideEncryption :: Lens' CompleteMultipartUploadResponse (Maybe ServerSideEncryption)
cmurServerSideEncryption =
    lens _cmurServerSideEncryption
         (\s a -> s { _cmurServerSideEncryption = a })

-- | Version of the object.
cmurVersionId :: Lens' CompleteMultipartUploadResponse (Maybe ObjectVersionId)
cmurVersionId = lens _cmurVersionId (\s a -> s { _cmurVersionId = a })

instance AWSRequest CompleteMultipartUpload where
    type Sv CompleteMultipartUpload = S3
    type Rs CompleteMultipartUpload = CompleteMultipartUploadResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure CompleteMultipartUploadResponse
            <*> xml %|? "Location"
            <*> xml %|? "BucketName"
            <*> xml %|? "ObjectKey"
            <*> hs ~:? "x-amz-expiration"
            <*> xml %|? "ETag"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "x-amz-version-id"
