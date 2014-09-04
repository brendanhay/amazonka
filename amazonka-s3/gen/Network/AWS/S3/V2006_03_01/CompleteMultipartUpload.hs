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
    , completeMultipartUpload
    -- ** Request lenses
    , cmurBucket
    , cmurUploadId
    , cmurKey
    , cmurMultipartUpload

    -- * Response
    , CompleteMultipartUploadResponse
    -- ** Response lenses
    , cmuoBucket
    , cmuoETag
    , cmuoLocation
    , cmuoKey
    , cmuoExpiration
    , cmuoVersionId
    , cmuoServerSideEncryption
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CompleteMultipartUpload' request.
completeMultipartUpload :: BucketName -- ^ 'cmurBucket'
                        -> Text -- ^ 'cmurUploadId'
                        -> ObjectKey -- ^ 'cmurKey'
                        -> CompleteMultipartUpload
completeMultipartUpload p1 p2 p3 = CompleteMultipartUpload
    { _cmurBucket = p1
    , _cmurUploadId = p2
    , _cmurKey = p3
    , _cmurMultipartUpload = Nothing
    }
{-# INLINE completeMultipartUpload #-}

data CompleteMultipartUpload = CompleteMultipartUpload
    { _cmurBucket :: BucketName
    , _cmurUploadId :: Text
    , _cmurKey :: ObjectKey
    , _cmurMultipartUpload :: Maybe CompletedMultipartUpload
    } deriving (Show, Generic)

cmurBucket :: Lens' CompleteMultipartUpload (BucketName)
cmurBucket f x =
    f (_cmurBucket x)
        <&> \y -> x { _cmurBucket = y }
{-# INLINE cmurBucket #-}

cmurUploadId :: Lens' CompleteMultipartUpload (Text)
cmurUploadId f x =
    f (_cmurUploadId x)
        <&> \y -> x { _cmurUploadId = y }
{-# INLINE cmurUploadId #-}

cmurKey :: Lens' CompleteMultipartUpload (ObjectKey)
cmurKey f x =
    f (_cmurKey x)
        <&> \y -> x { _cmurKey = y }
{-# INLINE cmurKey #-}

cmurMultipartUpload :: Lens' CompleteMultipartUpload (Maybe CompletedMultipartUpload)
cmurMultipartUpload f x =
    f (_cmurMultipartUpload x)
        <&> \y -> x { _cmurMultipartUpload = y }
{-# INLINE cmurMultipartUpload #-}

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

instance ToBody CompleteMultipartUpload where
    toBody = toBody . encodeXML . _cmurMultipartUpload

data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse
    { _cmuoBucket :: Maybe BucketName
    , _cmuoETag :: Maybe ETag
      -- ^ Entity tag of the object.
    , _cmuoLocation :: Maybe Text
    , _cmuoKey :: Maybe ObjectKey
    , _cmuoExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, this will contain the
      -- expiration date (expiry-date) and rule ID (rule-id). The value of
      -- rule-id is URL encoded.
    , _cmuoVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    , _cmuoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Show, Generic)

cmuoBucket :: Lens' CompleteMultipartUploadResponse (Maybe BucketName)
cmuoBucket f x =
    f (_cmuoBucket x)
        <&> \y -> x { _cmuoBucket = y }
{-# INLINE cmuoBucket #-}

-- | Entity tag of the object.
cmuoETag :: Lens' CompleteMultipartUploadResponse (Maybe ETag)
cmuoETag f x =
    f (_cmuoETag x)
        <&> \y -> x { _cmuoETag = y }
{-# INLINE cmuoETag #-}

cmuoLocation :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cmuoLocation f x =
    f (_cmuoLocation x)
        <&> \y -> x { _cmuoLocation = y }
{-# INLINE cmuoLocation #-}

cmuoKey :: Lens' CompleteMultipartUploadResponse (Maybe ObjectKey)
cmuoKey f x =
    f (_cmuoKey x)
        <&> \y -> x { _cmuoKey = y }
{-# INLINE cmuoKey #-}

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
cmuoExpiration :: Lens' CompleteMultipartUploadResponse (Maybe RFC822)
cmuoExpiration f x =
    f (_cmuoExpiration x)
        <&> \y -> x { _cmuoExpiration = y }
{-# INLINE cmuoExpiration #-}

-- | Version of the object.
cmuoVersionId :: Lens' CompleteMultipartUploadResponse (Maybe ObjectVersionId)
cmuoVersionId f x =
    f (_cmuoVersionId x)
        <&> \y -> x { _cmuoVersionId = y }
{-# INLINE cmuoVersionId #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cmuoServerSideEncryption :: Lens' CompleteMultipartUploadResponse (Maybe ServerSideEncryption)
cmuoServerSideEncryption f x =
    f (_cmuoServerSideEncryption x)
        <&> \y -> x { _cmuoServerSideEncryption = y }
{-# INLINE cmuoServerSideEncryption #-}

instance AWSRequest CompleteMultipartUpload where
    type Sv CompleteMultipartUpload = S3
    type Rs CompleteMultipartUpload = CompleteMultipartUploadResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CompleteMultipartUploadResponse
            <*> xml %|? "BucketName"
            <*> xml %|? "ETag"
            <*> xml %|? "Location"
            <*> xml %|? "ObjectKey"
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "x-amz-version-id"
            <*> hs ~:? "x-amz-server-side-encryption"
