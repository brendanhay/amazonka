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
    , mkCompleteMultipartUploadRequest
    -- ** Request lenses
    , cmurBucket
    , cmurKey
    , cmurMultipartUpload
    , cmurUploadId

    -- * Response
    , CompleteMultipartUploadResponse
    -- ** Response lenses
    , cmuoLocation
    , cmuoBucket
    , cmuoKey
    , cmuoExpiration
    , cmuoETag
    , cmuoServerSideEncryption
    , cmuoVersionId
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CompleteMultipartUpload' request.
mkCompleteMultipartUploadRequest :: BucketName -- ^ 'cmurBucket'
                                 -> ObjectKey -- ^ 'cmurKey'
                                 -> Text -- ^ 'cmurUploadId'
                                 -> CompleteMultipartUpload
mkCompleteMultipartUploadRequest p1 p2 p3 = CompleteMultipartUpload
    { _cmurBucket = p1
    , _cmurKey = p2
    , _cmurMultipartUpload = Nothing
    , _cmurUploadId = p4
    }
{-# INLINE mkCompleteMultipartUploadRequest #-}

data CompleteMultipartUpload = CompleteMultipartUpload
    { _cmurBucket :: BucketName
    , _cmurKey :: ObjectKey
    , _cmurMultipartUpload :: Maybe CompletedMultipartUpload
    , _cmurUploadId :: Text
    } deriving (Show, Generic)

cmurBucket :: Lens' CompleteMultipartUpload (BucketName)
cmurBucket = lens _cmurBucket (\s a -> s { _cmurBucket = a })
{-# INLINE cmurBucket #-}

cmurKey :: Lens' CompleteMultipartUpload (ObjectKey)
cmurKey = lens _cmurKey (\s a -> s { _cmurKey = a })
{-# INLINE cmurKey #-}

cmurMultipartUpload :: Lens' CompleteMultipartUpload (Maybe CompletedMultipartUpload)
cmurMultipartUpload = lens _cmurMultipartUpload (\s a -> s { _cmurMultipartUpload = a })
{-# INLINE cmurMultipartUpload #-}

cmurUploadId :: Lens' CompleteMultipartUpload (Text)
cmurUploadId = lens _cmurUploadId (\s a -> s { _cmurUploadId = a })
{-# INLINE cmurUploadId #-}

instance ToPath CompleteMultipartUpload where
    toPath CompleteMultipartUpload{..} = mconcat
        [ "/"
        , toBS _cmurBucket
        , "/"
        , toBS _cmurKey
        ]

instance ToQuery CompleteMultipartUpload where
    toQuery CompleteMultipartUpload{..} = mconcat
        [ "uploadId" =? _cmurUploadId
        ]

instance ToHeaders CompleteMultipartUpload

instance ToBody CompleteMultipartUpload

data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse
    { _cmuoLocation :: Maybe Text
    , _cmuoBucket :: Maybe BucketName
    , _cmuoKey :: Maybe ObjectKey
    , _cmuoExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, this will contain the
      -- expiration date (expiry-date) and rule ID (rule-id). The value of
      -- rule-id is URL encoded.
    , _cmuoETag :: Maybe ETag
      -- ^ Entity tag of the object.
    , _cmuoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _cmuoVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    } deriving (Show, Generic)

cmuoLocation :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cmuoLocation = lens _cmuoLocation (\s a -> s { _cmuoLocation = a })
{-# INLINE cmuoLocation #-}

cmuoBucket :: Lens' CompleteMultipartUploadResponse (Maybe BucketName)
cmuoBucket = lens _cmuoBucket (\s a -> s { _cmuoBucket = a })
{-# INLINE cmuoBucket #-}

cmuoKey :: Lens' CompleteMultipartUploadResponse (Maybe ObjectKey)
cmuoKey = lens _cmuoKey (\s a -> s { _cmuoKey = a })
{-# INLINE cmuoKey #-}

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
cmuoExpiration :: Lens' CompleteMultipartUploadResponse (Maybe RFC822)
cmuoExpiration = lens _cmuoExpiration (\s a -> s { _cmuoExpiration = a })
{-# INLINE cmuoExpiration #-}

-- | Entity tag of the object.
cmuoETag :: Lens' CompleteMultipartUploadResponse (Maybe ETag)
cmuoETag = lens _cmuoETag (\s a -> s { _cmuoETag = a })
{-# INLINE cmuoETag #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cmuoServerSideEncryption :: Lens' CompleteMultipartUploadResponse (Maybe ServerSideEncryption)
cmuoServerSideEncryption = lens _cmuoServerSideEncryption (\s a -> s { _cmuoServerSideEncryption = a })
{-# INLINE cmuoServerSideEncryption #-}

-- | Version of the object.
cmuoVersionId :: Lens' CompleteMultipartUploadResponse (Maybe ObjectVersionId)
cmuoVersionId = lens _cmuoVersionId (\s a -> s { _cmuoVersionId = a })
{-# INLINE cmuoVersionId #-}

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
