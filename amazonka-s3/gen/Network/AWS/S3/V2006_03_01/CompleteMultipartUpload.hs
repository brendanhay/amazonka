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

data CompleteMultipartUpload = CompleteMultipartUpload
    { _cmurBucket :: BucketName
    , _cmurUploadId :: Text
    , _cmurKey :: ObjectKey
    , _cmurMultipartUpload :: Maybe CompletedMultipartUpload
    } deriving (Show, Generic)

cmurBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> CompleteMultipartUpload
    -> f CompleteMultipartUpload
cmurBucket f x =
    (\y -> x { _cmurBucket = y })
       <$> f (_cmurBucket x)
{-# INLINE cmurBucket #-}

cmurUploadId
    :: Functor f
    => (Text
    -> f (Text))
    -> CompleteMultipartUpload
    -> f CompleteMultipartUpload
cmurUploadId f x =
    (\y -> x { _cmurUploadId = y })
       <$> f (_cmurUploadId x)
{-# INLINE cmurUploadId #-}

cmurKey
    :: Functor f
    => (ObjectKey
    -> f (ObjectKey))
    -> CompleteMultipartUpload
    -> f CompleteMultipartUpload
cmurKey f x =
    (\y -> x { _cmurKey = y })
       <$> f (_cmurKey x)
{-# INLINE cmurKey #-}

cmurMultipartUpload
    :: Functor f
    => (Maybe CompletedMultipartUpload
    -> f (Maybe CompletedMultipartUpload))
    -> CompleteMultipartUpload
    -> f CompleteMultipartUpload
cmurMultipartUpload f x =
    (\y -> x { _cmurMultipartUpload = y })
       <$> f (_cmurMultipartUpload x)
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

cmuoBucket
    :: Functor f
    => (Maybe BucketName
    -> f (Maybe BucketName))
    -> CompleteMultipartUploadResponse
    -> f CompleteMultipartUploadResponse
cmuoBucket f x =
    (\y -> x { _cmuoBucket = y })
       <$> f (_cmuoBucket x)
{-# INLINE cmuoBucket #-}

-- | Entity tag of the object.
cmuoETag
    :: Functor f
    => (Maybe ETag
    -> f (Maybe ETag))
    -> CompleteMultipartUploadResponse
    -> f CompleteMultipartUploadResponse
cmuoETag f x =
    (\y -> x { _cmuoETag = y })
       <$> f (_cmuoETag x)
{-# INLINE cmuoETag #-}

cmuoLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CompleteMultipartUploadResponse
    -> f CompleteMultipartUploadResponse
cmuoLocation f x =
    (\y -> x { _cmuoLocation = y })
       <$> f (_cmuoLocation x)
{-# INLINE cmuoLocation #-}

cmuoKey
    :: Functor f
    => (Maybe ObjectKey
    -> f (Maybe ObjectKey))
    -> CompleteMultipartUploadResponse
    -> f CompleteMultipartUploadResponse
cmuoKey f x =
    (\y -> x { _cmuoKey = y })
       <$> f (_cmuoKey x)
{-# INLINE cmuoKey #-}

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
cmuoExpiration
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> CompleteMultipartUploadResponse
    -> f CompleteMultipartUploadResponse
cmuoExpiration f x =
    (\y -> x { _cmuoExpiration = y })
       <$> f (_cmuoExpiration x)
{-# INLINE cmuoExpiration #-}

-- | Version of the object.
cmuoVersionId
    :: Functor f
    => (Maybe ObjectVersionId
    -> f (Maybe ObjectVersionId))
    -> CompleteMultipartUploadResponse
    -> f CompleteMultipartUploadResponse
cmuoVersionId f x =
    (\y -> x { _cmuoVersionId = y })
       <$> f (_cmuoVersionId x)
{-# INLINE cmuoVersionId #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
cmuoServerSideEncryption
    :: Functor f
    => (Maybe ServerSideEncryption
    -> f (Maybe ServerSideEncryption))
    -> CompleteMultipartUploadResponse
    -> f CompleteMultipartUploadResponse
cmuoServerSideEncryption f x =
    (\y -> x { _cmuoServerSideEncryption = y })
       <$> f (_cmuoServerSideEncryption x)
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
