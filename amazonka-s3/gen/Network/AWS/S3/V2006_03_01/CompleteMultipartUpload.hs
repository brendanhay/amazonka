{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.CompleteMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Completes a multipart upload by assembling previously uploaded parts.
module Network.AWS.S3.V2006_03_01.CompleteMultipartUpload
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
    -- ** Response lenses
    , cmursLocation
    , cmursBucket
    , cmursKey
    , cmursExpiration
    , cmursETag
    , cmursServerSideEncryption
    , cmursVersionId
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
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
{-# INLINE mkCompleteMultipartUpload #-}

cmuBucket :: Lens' CompleteMultipartUpload BucketName
cmuBucket = lens _cmuBucket (\s a -> s { _cmuBucket = a })
{-# INLINE cmuBucket #-}

cmuKey :: Lens' CompleteMultipartUpload ObjectKey
cmuKey = lens _cmuKey (\s a -> s { _cmuKey = a })
{-# INLINE cmuKey #-}

cmuMultipartUpload :: Lens' CompleteMultipartUpload (Maybe CompletedMultipartUpload)
cmuMultipartUpload =
    lens _cmuMultipartUpload (\s a -> s { _cmuMultipartUpload = a })
{-# INLINE cmuMultipartUpload #-}

cmuUploadId :: Lens' CompleteMultipartUpload Text
cmuUploadId = lens _cmuUploadId (\s a -> s { _cmuUploadId = a })
{-# INLINE cmuUploadId #-}

instance ToPath CompleteMultipartUpload where
    toPath CompleteMultipartUpload{..} = mconcat
        [ "/"
        , toBS _cmuBucket
        , "/"
        , toBS _cmuKey
        ]

instance ToQuery CompleteMultipartUpload where
    toQuery CompleteMultipartUpload{..} = mconcat
        [ "uploadId" =? _cmuUploadId
        ]

instance ToHeaders CompleteMultipartUpload

instance ToBody CompleteMultipartUpload where
    toBody = toBody . encodeXML . _cmuMultipartUpload

data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse
    { _cmursLocation :: Maybe Text
    , _cmursBucket :: Maybe BucketName
    , _cmursKey :: Maybe ObjectKey
    , _cmursExpiration :: Maybe RFC822
    , _cmursETag :: Maybe ETag
    , _cmursServerSideEncryption :: Maybe ServerSideEncryption
    , _cmursVersionId :: Maybe ObjectVersionId
    } deriving (Show, Generic)

cmursLocation :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cmursLocation = lens _cmursLocation (\s a -> s { _cmursLocation = a })
{-# INLINE cmursLocation #-}

cmursBucket :: Lens' CompleteMultipartUploadResponse (Maybe BucketName)
cmursBucket = lens _cmursBucket (\s a -> s { _cmursBucket = a })
{-# INLINE cmursBucket #-}

cmursKey :: Lens' CompleteMultipartUploadResponse (Maybe ObjectKey)
cmursKey = lens _cmursKey (\s a -> s { _cmursKey = a })
{-# INLINE cmursKey #-}

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
cmursExpiration :: Lens' CompleteMultipartUploadResponse (Maybe RFC822)
cmursExpiration = lens _cmursExpiration (\s a -> s { _cmursExpiration = a })
{-# INLINE cmursExpiration #-}

-- | Entity tag of the object.
cmursETag :: Lens' CompleteMultipartUploadResponse (Maybe ETag)
cmursETag = lens _cmursETag (\s a -> s { _cmursETag = a })
{-# INLINE cmursETag #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cmursServerSideEncryption :: Lens' CompleteMultipartUploadResponse (Maybe ServerSideEncryption)
cmursServerSideEncryption =
    lens _cmursServerSideEncryption
         (\s a -> s { _cmursServerSideEncryption = a })
{-# INLINE cmursServerSideEncryption #-}

-- | Version of the object.
cmursVersionId :: Lens' CompleteMultipartUploadResponse (Maybe ObjectVersionId)
cmursVersionId = lens _cmursVersionId (\s a -> s { _cmursVersionId = a })
{-# INLINE cmursVersionId #-}

instance AWSRequest CompleteMultipartUpload where
    type Sv CompleteMultipartUpload = S3
    type Rs CompleteMultipartUpload = CompleteMultipartUploadResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CompleteMultipartUploadResponse
            <*> xml %|? "Location"
            <*> xml %|? "BucketName"
            <*> xml %|? "ObjectKey"
            <*> hs ~:? "x-amz-expiration"
            <*> xml %|? "ETag"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "x-amz-version-id"
